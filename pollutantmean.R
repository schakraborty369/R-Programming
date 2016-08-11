pollutantmean <- function(directory, pollutant, id = 1:332) {
              setwd(paste0("C:/Users/Sony/Documents/",directory))
              sum <- 0
              count <- 0
              
  for (i in id)
  {  
     foo <- paste0(formatC(i, width = 3, flag = "0"),".csv")
     x <- read.csv(file=foo, header = T) 
     poll <- x[[pollutant]]
     sum <- sum + sum(poll[!is.na(poll)])
     count <- count + length(poll[!is.na(poll)])
    }
  setwd("C:/Users/Sony/Documents")
  sum / count
}