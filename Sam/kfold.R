#Author: Sam Lee
#04/17/2024

#This is a custom function to prepare data for k-fold CV in R

k.fold.split <- function(data, k = 5, seed = NULL) {

  if (!is.null(seed)) set.seed(seed)
  n <- nrow(data)
  fold_size <- floor(n / k)
  
  index <- sample(n)
  
  folds <- list()
  for (i in 1:k) {
    start <- ((i - 1) * fold_size) + 1
    end <- min(i * fold_size, n)
    
    fold_indices <- index[start:end]
    
    fold <- data[fold_indices, ]
    
    folds[[i]] <- fold
  }
  
  return(folds)
}

train.split <- function(data, test=0.2, seed=NULL){
  if (!is.null(seed)) set.seed(seed)
  n <- nrow(data)
  
  train = floor((1-test)*n)
  
  index <- sample(1:n, train)
  
  return(
    list(data[index,], data[-index,])
  )
}
