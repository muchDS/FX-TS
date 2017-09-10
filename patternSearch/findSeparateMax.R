findSeparateMax <- function(localTimeWindow){
  
  localMaxVect<- localTimeWindow %>% # localTimeWindow[5:length(localTimeWindow$tsval),]
                     mutate(maxIndicator = ifelse(tsval == max(tsval), 1, 0), maxRowNum = row_number()) %>% 
                     filter(maxIndicator == 1) %>%
                     select(maxRowNum) %>% unlist() %>% as.vector()
  # wektor odległości n-1 składników
  # pointsBetweenMax <- rep.int(0, times = length(localMaxVect) - 1)
  differentMaxSelector <- c()
  
  if(length(localMaxVect) == 1){
    
    return(localMaxVect)
    
  } else{
    for(i in 1:(length(localMaxVect) - 1)){
      
      if((localMaxVect[i + 1] - localMaxVect[i] ) > 4){
        differentMaxSelector <- c(differentMaxSelector, i, i+1)
      }
      
    }
    
  }
  
  return(
   localMaxVect[ differentMaxSelector %>% unique() %>% order()]
  )

}
