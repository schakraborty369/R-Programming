corr <- function(directory, thresold = 0) {
  
  setwd(paste0("C:/Users/Sony/Documents/",directory))
  comp <- complete("specdata", 1:332)
  setwd(paste0("C:/Users/Sony/Documents/",directory))
  if (thresold > max(comp$nobs)){c()}
  filtered <- comp[comp$nobs > thresold,]
  vec <- c()
  
  for (i in filtered$id)
  {
    foo <- paste0(formatC(i, width = 3, flag = "0"),".csv")
    x <- read.csv(file=foo, header = T) 
    cor <- cor(x[2], x[3], use="complete.obs")
    vec <- c(vec, cor)
  }
  setwd("C:/Users/Sony/Documents")
  vec
}