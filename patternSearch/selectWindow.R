selectWindow <- function(localDF, earliestPOSIXltVal, latestPOSIXltVal, minimalWindowSize){
  
  windowToReturn <- localDF %>%
                    filter(tstime >= earliestPOSIXltVal & tstime <= latestPOSIXltVal)
  
  print("time window created")
  
  # if(length(windowToReturn[,1]) == 0){
  #   print("resampling")
  #   
  #   while(length(windowToReturn[,1]) >= minimalWindowSize) {
  #     
  #     earliestPOSIXltVal <- earliestPOSIXltVal - minutes(5)
  #     windowToReturn <- localDF %>%
  #                       filter(tstime >= earliestPOSIXltVal & tstime <= latestPOSIXltVal)
  #     
  #   }
  # }
  
  return(
    
    windowToReturn
    
  )
  
}