#Load in libraries and data set for regression
source("model.R")
write_csv(model.data, "model_data.csv")

dataToNimble <- function(model.df=model.data){
  model.df <- model.df %>% arrange(desc(t))
  nimble.data <- list(
    #Response
    y = model.df$NumViolentCrimes,
    #Covariate data
    FullMoon = model.df$FullMoon,
    DayTime = model.df$hourly.is_day,
    Unemployment = model.df$Unemployment,
    Year = model.df$Year
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
  
  #This variable is loaded in with line 2
  weather.covariates
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
  
  #Auto regressive lagged effects data
  lagged.data <- c()
  for(i in 1:7){
    for(j in 1:24){
      if(length(lagged.data) <= 0){
        lagged.data <- as.matrix(lag(model.df$NumViolentCrimes, i*j))
      } else {
        lagged.data <- cbind(lagged.data, lag(model.df$NumViolentCrimes, i*j)) 
      }
    }
  }
  nimble.data$lag.y <- lagged.data
  
  #Due to the introduction of the auto regressive variables, we lose 7*24=168 observations
  for(i in 1:length(nimble.data)){
    data = nimble.data[[i]] %>% as.matrix()
    data.adjusted <- data[(168+1):nrow(model.df),]
    nimble.data[[i]] <- data.adjusted
  }
  
  nimble.consts <- list(
    N = model.df %>% nrow() - 168,
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
formatted.data <- dataToNimble(model.data)

model.code <- nimbleCode({
  #Priors for zero-inflated poisson distribution
  p ~ dunif(0,1)
  lambda ~ dunif(0,10)
  
  intercept ~ dunif(-2,10)
  #Base regression x-matrix covariates
  beta1 ~ dnorm(mean=0, sd=100)
  beta2 ~ dnorm(mean=0, sd=100)
  beta3 ~ dnorm(mean=0, sd=100)
  beta4 ~ dnorm(mean=0, sd=100)
  beta5 ~ dnorm(mean=0, sd=100)
  beta6 ~ dnorm(mean=0, sd=100)
  beta7 ~ dnorm(mean=0, sd=100)
  
  #Holiday dummy coefficient parameters
  for(i in 1:H){
    betaH[i] ~ dnorm(mean=0, sd=100)
  }
  
  #Weather Covariates
  for(i in 1:W){
    omega[i] ~ dnorm(mean=0, sd=100)
  }
  
  #Hour fixed effects
  for(i in 1:Hrs){
    eta[i] ~ dnorm(mean=0, sd=100)
  }
  
  #Day fixed effects
  for(i in 1:D){
    delta[i] ~ dnorm(mean=0, sd=100)
  }
  
  #Monthly fixed effects
  for(i in 1:M){
    mu[i] ~ dnorm(mean=0, sd=100)
  }
  
  #Two-way interaction coefficient parameters and preprocessing
  for(i in 1:Hrs){
    for(j in 1:D){
      hrs.d[1:N,i*j] <- Hours[1:N,i]*Days[1:N,j]
      eta.delta[i*j] ~ dnorm(mean=0, sd=100)
    }
  }
  
  for(i in 1:Hrs){
    for(j in 1:M){
      hrs.m[1:N,i*j] <- Hours[1:N,i]*Months[1:N,j]
      eta.mu[i*j] ~ dnorm(mean=0, sd=100)
    }
  }
  
  for(i in 1:D){
    for(j in 1:M){
      d.m[1:N,i*j] <- Days[1:N,i]*Months[1:N,j]
      delta.mu[i*j] ~ dnorm(mean=0, sd=100)
    }
  }
  
  #Three-way interaction
  for(i in 1:Hrs){
    for(j in 1:D){
      for(k in 1:M){
        hrs.d.m[1:N,i*j*k] <- Hours[1:N,i]*Days[1:N,j]*Months[1:N,k]
        eta.delta.mu[i*j*k] ~ dnorm(mean=0, sd=100)
      }
    }
  }
  
  #Auto regressive lagged effects
  for(j in 1:(Hrs+1)){ #1:24
    for(i in 1:(D+1)){ #1:7
      psi[i*j] ~ dnorm(mean=0, sd=100)
    }
  }
  
  for(t in 1:N){
    y[t] ~ dZIP(lambda, zeroProb = p)
    
    #We are particularly interested in the posterior distribution of beta1 (and by implication, beta3)
    lambda <- intercept+beta1*FullMoon[t,1]+beta2*DayTime[t,1]+beta3*FullMoon[t,1]*DayTime[t,1]+
      beta4*Unemployment[t,1] + beta5*Year[t,1] + beta6*Year[t,1]^2 + beta7*Year[t,1]^3 +
      t(Holidays[t,1:H])%*%betaH[1:H] + t(Weather[t,1:W])%*%omega[1:W] + 
      #Time fixed effects
      t(Hours[t,1:Hrs])%*%eta[1:Hrs]+ t(Days[t,1:D])%*%delta[1:D] + t(Months[t,1:M])%*%mu[1:M] + 
      t(hrs.d[t,1:(Hrs*D)])%*%eta.delta[1:(Hrs*D)] + t(hrs.m[t,1:(Hrs*M)])%*%eta.delta[1:(Hrs*M)]+
      t(d.m[t,1:(D*M)])%*%delta.mu[1:(D*M)]+t(hrs.d.m[t,1:(Hrs*D*M)])%*%eta.delta.mu[1:(Hrs*D*M)]+
      #Auto regressive Lagged effects
      t(lag.y[t,1:(7*24)])%*%psi[1:(7*24)]
  }
})

dZIP <- nimbleFunction(
  run = function(x = integer(), lambda = double(),
                 zeroProb = double(), log = logical(0, default=0)){
    returnType(double())
    if(x != 0){
      if (log) return(dpois(x, lambda, log=TRUE)) + log(1-zeroProb)
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
ZIPmodel <- nimbleModel(model.code, constants = formatted.data$consts)
ZIPmodel$setData(formatted.data$data)
cZIPmodel <- compileNimble(ZIPmodel)
ZIPmcmc <- compile(ZIPmcmc, project=ZIPmodel)

