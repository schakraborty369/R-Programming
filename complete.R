complete <- function(directory, id = 1:332) {
  
  setwd(paste0("C:/Users/Sony/Documents/",directory))
  nobs <- c()
  
  for (i in id)
      { 
                foo <- paste0(formatC(i, width = 3, flag = "0"),".csv")
                x <- read.csv(file=foo, header = T) 
                count <- min(sum(!is.na(x[2])),sum(!is.na(x[3])))
                nobs <- c(nobs,count)
      }
        setwd("C:/Users/Sony/Documents")
        as.data.frame(cbind(id,nobs))
        
  }