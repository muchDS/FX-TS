selectWindow <- function(localDF, earliestPOSIXltVal, latestPOSIXltVal){
  
  return(
    
    localDF %>% filter(tstime >= earliestPOSIXltVal, tstime <= latestPOSIXltVal)
    
  )
  
}