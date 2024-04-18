#Author: Sam Lee
#04/17/2024

#I made this package so I wouldn't have to deal with the annoying nuances
#of trying to make summary statistics look nice for a report every time

sum.stats.table <- function(df, var.rownames=NULL){
  sum.stats <- summary(df, digits=3) 
  sum.stats <- data.frame(
    summary_stat = rownames(sum.stats),
    t(sum.stats)
  )
  sum.stats$Stat <- sapply(sum.stats$Freq, 
                           function(x) str_split(x, ":")[[1]])[1,] %>%
    trimws() %>% as.vector()
  sum.stats$Value <- sapply(sum.stats$Freq, 
                            function(x) str_split(x, ":")[[1]])[2,] %>% 
    trimws() %>% as.vector() %>% as.numeric()
  sum.stats <- sum.stats %>% dplyr::select(-c("summary_stat", "Var2", "Freq"))
  
  sum.stats <- sum.stats %>% 
    pivot_wider(names_from = Stat, values_from = Value) %>% as.data.frame()
  if(is.null(var.rownames)){
    rownames(sum.stats) <- sum.stats$Var1
  } else {
    rownames(sum.stats) <- var.rownames
  }
  sum.stats <- sum.stats %>% dplyr::select(-Var1)
  
  return(sum.stats)
}
