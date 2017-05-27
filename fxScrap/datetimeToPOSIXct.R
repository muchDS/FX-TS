datetimeToPOSIXct <- function(datetimeToConvert){
library(lubridate)
  
# currMonth <- Sys.time() %>%
#              as.POSIXlt() %>%
#              month()
    
possibleDateFormats <- c("(^[[:digit:]]{2})\\/([[:digit:]]{2})\\/([[:digit:]]{2}$)",
                         "(^[[:digit:]]{2})\\/([[:digit:]]{2}$)",
                         "(^[[:digit:]]{1,2})\\:([[:digit:]]{2})\\:([[:digit:]]{2}$)")

for(dateRegex in 1:length(possibleDateFormats)){
  
  if(grepl(pattern = paste0(possibleDateFormats[dateRegex]), datetimeToConvert)){
    caseIndicator <- dateRegex %>% as.character()
  }
  
}

currDate <- Sys.Date() %>% as.character()
currYear <- year(currDate)

  return( 
    switch(
    EXPR = caseIndicator,
    
    "1" = tryCatch(
                gsub(pattern = paste0(possibleDateFormats[1]), 
                     replacement = "20\\3\\-\\1\\-\\2 23:59:59 EDT", 
                     datetimeToConvert),
                error = function(e){
                                    return(
                                            gsub(pattern = paste0(possibleDateFormats[1]), 
                                            replacement = "20\\3\\-\\2\\-\\1 23:59:59 EDT", 
                                            datetimeToConvert)
                )}
          ),
    "2" = gsub(pattern = paste0(possibleDateFormats[2]), replacement = "\\-\\2\\-\\1 23:59:59 EDT", datetimeToConvert) %>%
          paste0(currYear, .) ,
    "3" = paste0(currDate, " ", datetimeToConvert, " EDT")
   )
  )
}