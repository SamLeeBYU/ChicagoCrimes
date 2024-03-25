#Load in libraries and data set for regression
source("model.R")

#Auto regressive lagged effects data
lagged.data <- c()
#for(i in 1:7){
  for(j in 1:24){
    if(length(lagged.data) <= 0){
      lagged.data <- as.matrix(lag(model.data$NumViolentCrimes, j))
    } else {
      lagged.data <- cbind(lagged.data, lag(model.data$NumViolentCrimes, j)) 
    }
  }
#}

library(caret)

#Split into training data and testing data
train_index <- createDataPartition(model.data$NumViolentCrimes, p = 0.01, list = FALSE)
train_set <- model.data[train_index, ]
train_lags <- lagged.data[train_index, ]
test_set <- model.data[-train_index, ]
test_lags <- lagged.data[-train_index, ]

dataToNimble <- function(model.df=model.data, lags){
  model.df <- model.df %>% arrange(desc(t))
  nimble.data <- list(
    #Response
    y = model.df$NumViolentCrimes,
    #Base Covariate data
    X = cbind(model.df$FullMoon, model.df$hourly.is_day, model.df$FullMoon*model.df$hourly.is_day,
              model.df$Unemployment, model.df$Year, model.df$Year^2, model.df$Year^3)
  )
  
  y = as.matrix(nimble.data$y)
  
  model.holiday.lm <- lm(NumViolentCrimes ~ Holiday, data=model.df)
  X = model.matrix(model.holiday.lm)
  # lasso.holiday <- cv.glmnet(0+X, y, family = "poisson", alpha = 1)
  # lambda.star <- lasso.holiday$lambda.min
  # 
  # holiday.coefficients <- coef(lasso.holiday, s = lambda.star) %>% as.matrix()
  
  #Lasso regression suggests that columbus day is largely irrelevant for the number of violent crimes
  
  holidays = model.df$Holiday %>% levels() %>% unique() %>% setdiff(c("None", "Columbus Day"))
  holiday.data = X[,sapply(colnames(X), function(colname) any(str_detect(colname, holidays)))]
  nimble.data$Holidays = holiday.data
  
  weather.covariates = c("hourly.temperature_2m", "hourly.relative_humidity_2m", "hourly.rain", "hourly.snowfall", "hourly.snow_depth", "hourly.cloud_cover", "hourly.wind_gusts_10m", "hourly.shortwave_radiation_instant")
  nimble.data$Weather = as.matrix(model.df[weather.covariates])
  
  #Time fixed effects
  model.hrs.lm <- lm(NumViolentCrimes ~ hour, data=model.df)
  X.hrs <- model.matrix(model.hrs.lm)[,2:24]
  nimble.data$Hours = X.hrs
  
  model.day.lm <- lm(NumViolentCrimes ~ Day, data=model.df)
  X.day <- model.matrix(model.day.lm)[,2:7]
  nimble.data$Days = X.day
  
  model.month.lm <- lm(NumViolentCrimes ~ Month, data=model.df)
  X.month <- model.matrix(model.month.lm)[,2:12]
  nimble.data$Months = X.month
  
  nimble.data$lag.y <- lags
  
  #Due to the introduction of the auto regressive variables, we lose 7*24=168 observations
  for(i in 1:length(nimble.data)){
    data = nimble.data[[i]] %>% as.matrix()
    data.adjusted <- data[(24+1):nrow(model.df),]
    nimble.data[[i]] <- data.adjusted %>% as.matrix()
  }
  
  nimble.consts <- list(
    N = model.df %>% nrow() - 24,
    H = holiday.data %>% ncol(),
    W = nimble.data$Weather %>% ncol(),
    Hrs = 23,
    D = 6,
    M = 11
  )

  return(list(
    data = nimble.data,
    consts = nimble.consts
  ))
}
formatted.data <- dataToNimble(model.data, lagged.data)
#formatted.data <- dataToNimble(train_set, train_lags)

model.code <- nimbleCode({
  #Priors for zero-inflated poisson distribution
  p ~ dunif(0,1)
  
  intercept ~ dunif(-5,15)
  # #Base regression x-matrix covariates
  for(i in 1:7){
    beta[i] ~ dnorm(mean=0, sd=5)
  }
  
  #Holiday dummy coefficient parameters
  for(i in 1:H){
    betaH[i] ~ dnorm(mean=0, sd=5)
  }

  #Weather Covariates
  for(i in 1:W){
    omega[i] ~ dnorm(mean=0, sd=5)
  }
  
  #Hour fixed effects
  for(i in 1:Hrs){
    eta[i] ~ dnorm(mean=0, sd=5)
  }
  
  #Day fixed effects
  for(i in 1:D){
    delta[i] ~ dnorm(mean=0, sd=5)
  }
  
  #Monthly fixed effects
  for(i in 1:M){
    mu[i] ~ dnorm(mean=0, sd=5)
  }
  
  #Two-way interaction coefficient parameters and preprocessing
  for(i in 1:Hrs){
    for(j in 1:D){
      hrs.d[1:N,D*(i-1)+j] <- Hours[1:N,i]*Days[1:N,j]
      eta.delta[D*(i-1)+j] ~ dnorm(mean=0, sd=5)
    }
  }
  
  #Auto regressive lagged effects
  for(j in 1:24){ #1:24
    psi[j] ~ dnorm(mean=0, sd=5)
  }
  
  for(t in 1:N){

    lambda[t] <-  exp(intercept + (X[t, 1:7] %*% beta[1:7])[1,1] + inprod(lag.y[t,1:24], psi[1:24]) + inprod(Weather[t,1:W],omega[1:W])+
                      inprod(Days[t,1:D],delta[1:D])+inprod(Months[t,1:M],mu[1:M])+inprod(Hours[t,1:Hrs],eta[1:Hrs])+
                      inprod(Holidays[t,1:H],betaH[1:H])+inprod(hrs.d[t,1:(Hrs*D)],eta.delta[1:(Hrs*D)])
                  )
    
    y[t,1] ~ dZIP(lambda=lambda[t], zeroProb=p)
  }
})

dZIP <- nimbleFunction(
  run = function(x = integer(), lambda = double(),
                 zeroProb = double(), log = logical(0, default=0)){
    returnType(double())
    if(x != 0){
      if (log) return(dpois(x, lambda, log=TRUE) + log(1-zeroProb))
      else return((1-zeroProb)*dpois(x, lambda, log=FALSE))
    }
    #Else x = 0
    totalProbZero <- zeroProb + (1 - zeroProb) * dpois(0, lambda, log=FALSE)
    if(log) return(log(totalProbZero))
    return(totalProbZero)
  }
)

rZIP <- nimbleFunction(
  run = function(n = integer(), lambda = double(), zeroProb = double()){
    returnType(integer())
    isStructuralZero <- rbinom(1, prob = zeroProb, size = 1)
    if(isStructuralZero) return(0)
    return(rpois(1, lambda))
  }
)

registerDistributions(list(
  dZIP = list(
    BUGSdist = "dZIP(lambda, zeroProb)",
    discrete = TRUE,
    range = c(0, Inf),
    types = c('value = integer()', 'lambda = double()', 'zeroProb = double()')
  )
))

#Run the model
ZIPmodel <- nimbleModel(model.code, constants = formatted.data$consts, calculate = F)
ZIPmodel$setData(formatted.data$data)
cZIPmodel <- compileNimble(ZIPmodel)
ZIPmcmc <- buildMCMC(ZIPmodel)
cZIPmcmc <- compileNimble(ZIPmcmc, project=ZIPmodel)
samples <- runMCMC(cZIPmcmc, niter=5000,
                   nburnin = 250, thin=10,
                   nchains = 1)
summary(samples)
plot(density(samples[,1]))
