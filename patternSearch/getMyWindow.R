getMyWindow <- function(data, from, to){
  library(dplyr)
  
  data$tstime <- data$tstime %>% as.POSIXct()
  
  return(
    
    data %>%
      filter(tstime %>% as.POSIXct() <= to %>% as.POSIXct() 
             & 
             tstime %>% as.POSIXct() >= from %>% as.POSIXct())
    
  )
  
  
}