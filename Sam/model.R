library(tidyverse)
library(nimble)
library(glmnet)
library(pscl)
library(rstanarm)
library(corrplot)

standardize <- function(v){
  (v-mean(v, na.rm=T))/sd(v, na.rm=T)
}

crime <- read_csv("crimes_cleaned.csv")

#Collapse all the violent crimes per hour
weather.covariates.base = colnames(crime)[15:26]
model.data <- crime %>% dplyr::select(DateTime, Date, Year, FullMoon, DayofWeek, Holiday,
                        hour, all_of(weather.covariates.base), 
                        Month, Unemployment) %>%
  distinct() %>% arrange(DateTime)

n.violent = crime %>% group_by(DateTime) %>%
  summarise (
    NumViolentCrimes = sum(Violent)
  )

model.data %>% left_join(n.violent) -> model.data

test.weather <- function(){
  weather.covariates <- setdiff(weather.covariates.base, c("hourly.apparent_temperature",
                                                           "hourly.direct_radiation_instant",
                                                           "hourly.wind_speed_10m"))
  X = as.matrix(model.data[weather.covariates] %>% dplyr::select(-hourly.is_day))
  X = apply(X, 2, standardize) %>% cbind(as.matrix(model.data[,c("hourly.is_day")]))
  y = as.matrix(model.data$NumViolentCrimes)
  
  model.zeroinf <- zeroinfl(y ~ X | 1, dist = "poisson", link = "logit")
  summary(model.zeroinf)
  coef(model.zeroinf) %>% abs() %>% sort()
  
  lasso.pois <- cv.glmnet(X, y, family = "poisson", alpha = 1)
  lambda.star <- lasso.pois$lambda.min
  lambda.star
  
  coefficients <- coef(lasso.pois, s = lambda.star)
  coefficients
  
  #Lasso regression implies that snow_depth, cloud_cover, and solar_radiation are not significant
  
  corrplot(cor(X))
}

#After looking at the effect of weather on crime, we will use this set of weather covariates to
#avoid some colinearity we had between some of the other weather covariates
weather.covariates <- setdiff(weather.covariates.base, c("hourly.apparent_temperature",
                                                         "hourly.direct_radiation_instant",
                                                         "hourly.wind_speed_10m",
                                                         "hourly.is_day"))

autoregressive <- function(){
  #Autoregressive plot to look at cumulative number of lags to include:
  deltas = c()
  alpha = 0.05
  significance <- c()
  index = 1
  model.data <- model.data %>% arrange(desc(DateTime))
  X = as.matrix(lag(model.data$NumViolentCrimes, index)[(index+1):nrow(model.data)])
  y = as.matrix(model.data$NumViolentCrimes[(index+1):nrow(model.data)])
  while(index <= 7*24){
    poisson.model = glm(y ~ X, family="poisson")
    deltas = c(deltas, coef(poisson.model)[1+index])
    summary(poisson.model) %>% print()
    significant = all(summary(poisson.model)$coefficients[(index+1),"Pr(>|z|)"] 
                      < alpha)
    if(!significant){
      significance <- c(significance, significant)
    } else {
      significance <- c()
    }
    index = index + 1
    x.new = as.matrix(lag(model.data$NumViolentCrimes, index)[(index+1):nrow(y)])
    X = cbind(X[(index+1):nrow(y),], x.new)
    y = as.matrix(model.data$NumViolentCrimes[(index+1):nrow(y)])
    colnames(X) <- 1:index
  }
  
  ggplot(mapping=aes(x=1:(index-1), y=deltas))+
    geom_point()+
    geom_line()+
    ylab(expression(psi[P]))+
    xlab("Cumulative (P) Lags on Number of Violent Crimes")+
    theme_minimal() 
}

#Weighted median of hour violent crime is committed
weighted.hours <- model.data %>% dplyr::select(hour, NumViolentCrimes) %>%
  group_by(hour) %>%
  summarise(
    weight = n()*sum(NumViolentCrimes)
  ) %>% ungroup() %>% arrange(weight) %>% uncount(weights=weight)
median.hour <- weighted.hours[round(0.5*nrow(weighted.hours)),] %>% pull(hour)

## START HERE ################################################################

#Factor Data
model.data <- model.data %>% arrange(DateTime) %>% mutate (
  #Center year so it's recoded as years from 2009 (creates a trend regression)
  #This makes it so 2010 is the lowest year in our data set and it will be centered at 1
  Year = Year-2009,
  Month = relevel(as.factor(Month), ref="January"),
  #9:00, this is the weighted median of the hour violent crime is committed. So we will use this as a base hour
  hour = relevel(as.factor(hour), ref=median.hour) ,
  Day = relevel(as.factor(DayofWeek), ref=1), #Monday,
  t = 1:nrow(model.data),
  Holiday = ifelse(!is.na(Holiday), Holiday, "None"),
  Holiday = relevel(as.factor(Holiday), ref="None")
) %>% arrange(desc(DateTime))

numeric.covariates = c("Unemployment", weather.covariates)
model.data[numeric.covariates] = apply(model.data[numeric.covariates], 2, standardize)
crime.covariates = c("FullMoon", "hourly.is_day", "Year", "t", "Month", "Day", "hour", "Holiday", 
                     numeric.covariates)
#Full data set for regression
model.data <- model.data[c(crime.covariates, "NumViolentCrimes")]

create_model <- function(zero.infl = F){
  model.formula <- "NumViolentCrimes ~ FullMoon*hourly.is_day + Unemployment + Holiday + Year + Year^2 + Year^3 + Month*Day*hour"
  #Weather covariates
  for(covariate in setdiff(weather.covariates, "hourly.is_day")){
    model.formula <- str_c(model.formula, str_c(" + ", covariate))
  }
  #Introduce auto regressive covariates for hour by day of the week
  for(i in 1:7){
    for(j in 1:24){
      model.formula <- str_c(model.formula, str_c(" + lag(NumViolentCrimes,", i*j, ")"))
    }
  }
  if(!zero.infl){
    model.formula %>% formula() 
  } else {
    str_c(model.formula, " | .") %>% formula()
  }
}
model = create_model()
zero.infl.model <- create_model(zero.infl = T)

run_mle <- function(){
  #Since this assumes a Normal likelihood, these estimates will be inefficient
  model.ols <- lm(model, data=model.data)
  summary(model.ols)
  
  yhat <- predict(model.ols)
  model.data$yhat.ols <- c(rep(NA_real_, 168), yhat)
  rmse.ols = sqrt(mean((model.data$NumViolentCrimes-model.data$yhat.ols)^2, na.rm=T))
  
  #Normal likelihood is bad!
  hist(yhat)
  
  #Zero inflated poisson with Maximum Likelihood
  model.zeroinf <- zeroinfl(zero.infl.model, data=model.data) 
}
