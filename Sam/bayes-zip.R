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
  # model.df <- model.df %>% arrange(desc(t))
  # nimble.data <- list(
  #   #Response
  #   y = model.df$NumViolentCrimes,
  #   #Base Covariate data
  #   X = cbind(model.df$FullMoon, model.df$hourly.is_day, model.df$FullMoon*model.df$hourly.is_day,
  #             model.df$Unemployment, model.df$Year, model.df$Year^2, model.df$Year^3)
  # )
  # 
  # y = as.matrix(nimble.data$y)
  # 
  # model.holiday.lm <- lm(NumViolentCrimes ~ Holiday, data=model.df)
  # X = model.matrix(model.holiday.lm)
  # lasso.holiday <- cv.glmnet(0+X, y, family = "poisson", alpha = 1)
  # lambda.star <- lasso.holiday$lambda.min
  # 
  # holiday.coefficients <- coef(lasso.holiday, s = lambda.star) %>% as.matrix()
  
  #Lasso regression suggests that columbus day is largely irrelevant for the number of violent crimes
  
  # holidays = model.df$Holiday %>% levels() %>% unique() %>% setdiff(c("None", "Columbus Day"))
  # holiday.data = X[,sapply(colnames(X), function(colname) any(str_detect(colname, holidays)))]
  # nimble.data$Holidays = holiday.data
  
  weather.covariates = c("hourly.temperature_2m", "hourly.relative_humidity_2m", "hourly.rain", "hourly.snowfall", "hourly.snow_depth", "hourly.cloud_cover", "hourly.wind_gusts_10m", "hourly.shortwave_radiation_instant")
  # nimble.data$Weather = as.matrix(model.df[weather.covariates])
  # 
  # #Time fixed effects
  # model.hrs.lm <- lm(NumViolentCrimes ~ hour, data=model.df)
  # X.hrs <- model.matrix(model.hrs.lm)[,2:24]
  # nimble.data$Hours = X.hrs
  # 
  # model.day.lm <- lm(NumViolentCrimes ~ Day, data=model.df)
  # X.day <- model.matrix(model.day.lm)[,2:7]
  # nimble.data$Days = X.day
  # 
  # model.month.lm <- lm(NumViolentCrimes ~ Month, data=model.df)
  # X.month <- model.matrix(model.month.lm)[,2:12]
  # nimble.data$Months = X.month
  # 
  # nimble.data$lag.y <- lags
  # 
  # #Due to the introduction of the auto regressive variables, we lose 7*24=168 observations
  # for(i in 1:length(nimble.data)){
  #   data = nimble.data[[i]] %>% as.matrix()
  #   data.adjusted <- data[(24+1):nrow(model.df),]
  #   nimble.data[[i]] <- data.adjusted %>% as.matrix()
  # }
  # 
  # nimble.consts <- list(
  #   N = model.df %>% nrow() - 24,
  #   H = holiday.data %>% ncol(),
  #   W = nimble.data$Weather %>% ncol(),
  #   Hrs = 23,
  #   D = 6,
  #   M = 11
  # )
  # 
  # return(list(
  #   data = nimble.data,
  #   consts = nimble.consts
  # ))
  
  ### Poisson Anova -- Model each group of data (treatment group separately/independently--aside from the autoregressive effects)
  model.df$Holiday[model.df$Holiday == "Columbus Day"] = "None"
  holidays = model.matrix(lm(model.df$NumViolentCrimes~model.df$Holiday))
  holidays = holidays[,2:ncol(holidays)]
  model.df <- cbind(model.df, holidays)
  n.holidays = ncol(holidays)
    
  # L = model.df %>% group_by(Day, hour, Month) %>%
  #   summarize(
  #     n = n()
  #   )
  
  data <- c()
  
  Days = model.df$Day %>% unique() %>% sort()
  Hrs = model.df$hour %>% levels() %>% as.numeric() %>% sort()
  M = month.name
  
  iter = 1
  #There's a more efficient way to do this
  for(k in 1:12){
    m = M[k]
    for(i in 1:length(Days)){
      day = Days[i]
      for(j in 1:length(Hrs)){
        h = Hrs[j]
        
        
        print(iter)
        data.subset = model.df[model.df$hour == h &
                               model.df$Day == day &
                               model.df$Month == m,]
        x.subset = cbind(data.subset$FullMoon, data.subset$hourly.is_day, data.subset$FullMoon*data.subset$hourly.is_day,
                         data.subset$Unemployment, data.subset$Year, data.subset$Year^2, data.subset$Year^3)
        weather.subset = data.subset[weather.covariates] %>% as.matrix()
        holiday.subset = data.subset[(ncol(data.subset)-n.holidays+1):ncol(data.subset)] %>% as.matrix()
        lag.subset = lags[as.numeric(rownames(holiday.subset)),]
        
        x.subset = cbind(data.subset$NumViolentCrimes, x.subset, weather.subset, holiday.subset, lag.subset, iter) %>%
          as.data.frame()
        
        x.subset = x.subset[complete.cases(x.subset),] %>% as.matrix()
        
        
        if(length(c) <= 0){
          data <- x.subset
        } else {
          data <- rbind(data, x.subset)
        }
        
        iter = iter+1
      }
    }
  }
  return(list(
    data = list(
      X = data[,2:(ncol(data)-1)],
      y = data[,1] %>% as.vector()
    ),
    consts = list(
      N = nrow(data),
      tmt = as.vector(data[,ncol(data)])
    )
  ))
}
formatted.data <- dataToNimble(model.data, lagged.data)
#formatted.data <- dataToNimble(train_set, train_lags)

model.code.anova <- nimbleCode({
  
  for(i in 1:G){ #Number of hour/day/month treatment groups
    #Priors for zero-inflated poisson distribution
    p[i] ~ dunif(0,1)
    intercept[i] ~ dunif(-5,15)
    for(j in 1:53){ #Number of covariates/parameters we want to estimate
      beta[i,j] ~ dnorm(mean=0, sd=5)
    }
  }
  
  for(t in 1:N){
    lambda[tmt[t],t] <- exp(intercept[tmt[t]] + inprod(X[t,1:53], beta[tmt[t],1:53]))
    y[t] ~ dZIP(lambda=lambda[tmt[t],t], zeroProb=p[tmt[t]])
  }
})

#Run the model
#formatted.data$consts$G = formatted.data$consts$tmt %>% unique() %>% length()
sample.data <- formatted.data
sample.data$consts$G = 24 #Number of treatment groups we want
sample.subset = 1:max(which(sample.data$consts$tmt == sample.data$consts$G))
sample.data$data$X = sample.data$data$X[sample.subset,]
sample.data$data$y = sample.data$data$y[sample.subset]
sample.data$consts$N = length(sample.subset)

ZIPmodel.out <- nimbleMCMC(constants = sample.data$consts,
                           data = sample.data$data,
                           code = model.code.anova,
                           niter = 500,
                           nburnin = 10,
                           thin=5,
                           nchains=5,
                           samplesAsCodaMCMC = TRUE,
                           summary = TRUE,
                           WAIC = TRUE)

summary(as.matrix(ZIPmodel.out$samples))

# ZIPmodel <- nimbleModel(model.code.anova, constants = formatted.data$consts, calculate = F)
# ZIPmodel$setData(formatted.data$data)
# cZIPmodel <- compileNimble(ZIPmodel)
# ZIPmcmc <- buildMCMC(ZIPmodel)
# cZIPmcmc <- compileNimble(ZIPmcmc, project=ZIPmodel)
# samples <- runMCMC(cZIPmcmc, niter=100,
#                    nburnin = 5, thin=5,
#                    nchains = 1)
# summary(samples)

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
#Relative Effect of Full Moon on Number of Violent Crimes
plot(density(samples[,1]))

save(samples, file="Data/posterior_samples.RData")

#Create our X matrix
D = ncol(formatted.data$data$Days); Hrs = ncol(formatted.data$data$Hours)
interactions <- matrix(nrow=nrow(formatted.data$data$X), ncol=D*Hrs)
for(i in 1:Hrs){
  for(j in 1:D){
    interactions[,D*(i-1)+j] <- formatted.data$data$Hours[,i]*formatted.data$data$Days[,j]
  }
}

X = cbind(formatted.data$data$X, formatted.data$data$Holidays, formatted.data$data$Days,
          formatted.data$data$Hours, interactions, rep(1, nrow(formatted.data$data$X)),
          formatted.data$data$Months, formatted.data$data$Weather, formatted.data$data$lag.y)

betas = samples[,setdiff(colnames(samples), "p")]

#These should be the same
ncol(X) == ncol(betas)

Y.postpred <- sapply(1:nrow(samples), function(n){
  (1-samples[n,"p"])*exp(X%*%betas[n,])
})

point.estimates = Y.postpred %>% apply(1,mean)

#These should be similar
betas.est <- apply(betas, 2, mean)
post.est <- (1-mean(samples[,"p"]))*exp(X %*% betas.est)

posterior.intervals <- Y.postpred %>% apply(1, function(row){
  quantile(row, c(0.025, 0.975))})

posterior.estimates = data.frame(
  DateTime = model.data$DateTime[25:nrow(model.data)],
  NumViolentCrimes = model.data$NumViolentCrimes[25:nrow(model.data)],
  posterior.estimate = point.estimates,
  lowerq = posterior.intervals[1,],
  upperq = posterior.intervals[2,]
)

r.index = sample(nrow(posterior.estimates)-24*7, size=1)
r.week = posterior.estimates[r.index:(r.index+(24*7)),]

r.week %>% ggplot(aes(x=DateTime, y=NumViolentCrimes))+
  geom_point()+
  geom_jitter(width=0.1, height=0.1)+
  theme_minimal()+
  geom_line(aes(y=posterior.estimate))+
  geom_ribbon(aes(ymin=lowerq, ymax=upperq), 
              color=NA, fill="#14DD32", alpha=0.2)

param.intervals <- samples %>% apply(2, function(param){
  quantile(param, c(0.025, 0.5, 0.975))
}) %>% as.data.frame() %>%
  rownames_to_column(var="quantile") %>% pivot_longer(2:(1+ncol(samples)), names_to = "param",
                                                      values_to = "bound") %>%
  pivot_wider(names_from = quantile, values_from = bound) %>%
  mutate(
    Significant = ifelse(
      `97.5%` > abs(`97.5%`-`2.5%`) | `2.5%` < -abs(`97.5%`-`2.5%`),
      "Yes", "No"
    )
  ) %>% arrange(desc(`50%`)) %>% head(20)

param.intervals$param <- factor(param.intervals$param, levels = param.intervals$param)

param.intervals %>%
  ggplot(aes(x = param, y = `50%`, ymin = `2.5%`, ymax = `97.5%`, color = Significant)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange()+
  geom_point(size = 3) +
  geom_text(aes(label = Significant), vjust = -1, size = 3) +
  coord_flip() +
  labs(x = "Parameter", y="Relative Effect Size") +
  guides(color="none")+
  theme_minimal()
