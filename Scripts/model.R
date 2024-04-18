library(tidyverse)
library(glmnet)
library(pscl)
library(corrplot)
library(arm)

#setwd(".../Scripts")

source("kfold.R")

standardize <- function(v){
  (v-mean(v, na.rm=T))/sd(v, na.rm=T)
}

#This data is created by Scripts/setup.R
crime <- read_csv("../Data/crimes_cleaned.csv")

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

#Run lasso poisson regressions for factor relevance
test.weather <- function(){
  weather.covariates <- setdiff(weather.covariates.base, c("hourly.apparent_temperature",
                                                           "hourly.direct_radiation_instant",
                                                           "hourly.wind_speed_10m"))
  X = as.matrix(model.data[weather.covariates] %>% dplyr::select(-hourly.is_day))
  X = apply(X, 2, standardize) %>% cbind(as.matrix(model.data[,c("hourly.is_day")]))
  
  y = as.matrix(model.data$NumViolentCrimes)
  
  model.data$Holiday = ifelse(is.na(model.data$Holiday), "Base", model.data$Holiday)
  X.holiday = model.matrix(lm(y ~ 0 + model.data$Holiday))
  
  model.zeroinf <- zeroinfl(y ~ X | 1, dist = "poisson", link = "logit")
  summary(model.zeroinf)
  coef(model.zeroinf) %>% abs() %>% sort()
  
  lasso.pois <- cv.glmnet(X, y, family = "poisson", alpha = 1)
  
  lasso.pois2 <- cv.glmnet(X.holiday, y, family = "poisson", alpha = 1)
  
  lambda.star <- lasso.pois2$lambda.min
  lambda.star
  
  alpha.star <- lasso.pois$lambda.min
  alpha.star
  
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

#Factor Data
model.data <- model.data %>% arrange(DateTime) %>% mutate (
  #Center year so it's recoded as years from 2009 (creates a trend regression)
  #This makes it so 2010 is the lowest year in our data set and it will be centered at 1
  Year = Year-2009,
  Month = relevel(as.factor(Month), ref="January"),
  #9:00, this is the weighted median of the hour violent crime is committed. So we will use this as a base hour
  hour = relevel(as.factor(hour), ref=21) ,
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
model.data <- model.data[c("DateTime", crime.covariates, "NumViolentCrimes")]
write_csv(model.data, "../Data/model_data.csv")

#Function to create model objects
create_model <- function(zero.infl = F){
  model.formula <- "NumViolentCrimes ~ FullMoon*hourly.is_day + Unemployment + Holiday + Year + I(Year^2) + I(Year^3) + Month + Day*hour"
  model.add <- ""
  #Weather covariates
  for(covariate in setdiff(weather.covariates, "hourly.is_day")){
    model.add <- str_c(model.add, str_c(" + ", covariate))
    model.formula <- str_c(model.formula, str_c(" + ", covariate))
  }
  #Introduce auto regressive covariates for hour by day of the week
  for(j in 1:24){
    model.add <- str_c(model.add, str_c(" + lag(NumViolentCrimes,", j, ")"))
    model.formula <- str_c(model.formula, str_c(" + lag(NumViolentCrimes,", j, ")"))
  }
  if(!zero.infl){
    model.formula %>% formula() 
  } else {
    #str_c(model.formula, " | FullMoon*hourly.is_day + Unemployment + Holiday + Year + I(Year^2) + I(Year^3) + Month + Day*hour", model.add) %>% formula()
    str_c(model.formula, " | 1") %>% formula()
  }
}
model = create_model()
zero.infl.model <- create_model(zero.infl = T)

#Mathematically predict expected value of crimes given MLE-calculated coefficients
zeroinfl.predict <- function(X.p, X, betas.p, beta){
  pi = invlogit(X.p%*%betas.p)
  (1-pi)*exp(X%*%beta)
}

run_mle <- function(){
  
  
  #Perform cross-validation on zero-inflated poisson model
  t.s <- train.split(model.data, seed=486)
  train <- t.s[[1]]
  test <- t.s[[2]]
  
  model.zeroinf.train <- zeroinfl(zero.infl.model, dist="poisson", data=train)
  model.zeroinf.test <- zeroinfl(zero.infl.model, dist="poisson", data=test)
  
  X.train = model.matrix(model.zeroinf.train); X.p.train = rep(1, nrow(X.train))
  X.test = model.matrix(model.zeroinf.test); X.p.test = rep(1, nrow(X.test))
  
  betas = model.zeroinf.train$coefficients$count %>% as.matrix()
  betas.p = model.zeroinf.train$coefficients$zero %>% as.matrix()
  yhat.zeroinf.train <- zeroinfl.predict(X.p.train, X.train, betas.p, betas)
  yhat.zeroinf.test <- zeroinfl.predict(X.p.test, X.test, betas.p, betas)
  
  mean((model.zeroinf.train$y-yhat.zeroinf.train)^2)
  #Out of sample MSE
  mean((model.zeroinf.test$y-yhat.zeroinf.test)^2)
  
  #Fit poisson model to find standard errors on coefficients
  model.pois <- glm(model, family=poisson(link="log"), data=model.data)
  predictions = predict(model.pois, type="response", se.fit = T)
  
  #Fit full zero-inflated poisson model
  model.zeroinf <- zeroinfl(zero.infl.model, dist="poisson", data=model.data)
  
  confidence_level <- 0.95  # Change as needed
  z_score <- qnorm((1 + confidence_level) / 2)
  lower_bound <- exp(log(model.zeroinf$fitted.values) - z_score * predictions$se.fit)
  upper_bound <- exp(log(model.zeroinf$fitted.values) + z_score * predictions$se.fit)
  
  # Create a dataframe with predictions and intervals
  prediction_intervals <- data.frame(
    DateTime = model.data[25:(nrow(model.data)),]$DateTime,
    NumViolentCrimes = model.data[25:(nrow(model.data)),]$NumViolentCrimes,
    estimate = model.zeroinf$fitted.values,
    lower_bound = lower_bound,
    upper_bound = upper_bound
  )
  
  #Bootstrap sampling of means to get predictive distribution
  bootstrap.intervals <- sapply(1:nrow(prediction_intervals), function(t){
    lambdas = seq(from = prediction_intervals[t,4], to = prediction_intervals[t,5], length.out = 100)
    counts = rpois(1000, sample(lambdas, replace=T)) %>% quantile(c(0.025, 0.975))
    zeros = rbinom(1000, 1, invlogit(model.zeroinf$coefficients$zero))
    counts[counts == 1] = 0
    counts
  }) %>% t()
 
  prediction_intervals <- cbind(prediction_intervals, bootstrap.intervals) %>% 
    as.data.frame()

  #Randomly select 2 weeks
  r.index = sample(1:(nrow(model.data)-24*14-100000), size=1)
  r.week = prediction_intervals[r.index:(r.index+(24*14)),]
  r.week %>%
    ggplot(aes(x=DateTime)) +
    geom_point(aes(y=NumViolentCrimes), size=1)+
    geom_jitter(aes(y=NumViolentCrimes), size=1, width=0.05, height=0.05)+
    labs(
      y = "Number of Violent Crimes",
      x = "Time (hr)"
    )+
    geom_line(aes(y=estimate), color="forestgreen", alpha=0.5, linewidth=1.2)+
    geom_ribbon(aes(ymin=lower_bound, ymax=upper_bound), 
                color=NA, fill="#14DD32", alpha=0.2)+
    geom_ribbon(aes(ymin=`2.5%`, ymax=`97.5%`), 
                color=NA, fill="forestgreen", alpha=0.1)+
    ggtitle(str_c("Violent Crimes per Hour in ", year(r.week$DateTime)))+
    theme_minimal()
}
