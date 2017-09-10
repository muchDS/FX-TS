selectSingleSeries <- function(data, seriesName){
  
  # params
  # zakładam, że nazwa kolumny w postaci waluta1Waluta2_nazwa, np. eurusd_ask
  tsToAnalyze <- seriesName %>% 
    strsplit(split = "\\_") %>%
    unlist() %>% as.vector()
  
  
  # zbieram interesujące kolumny
  myData <- data[,grepl(pattern = paste0("rowid|recordTime|", 
                                           tsToAnalyze[1], "_", tsToAnalyze[2], "|", 
                                           tsToAnalyze[1], "_time"), 
                          names(data))
                   ]
  
  # normalizuję zbiór danych
  myDataUniBit <- myData %>%
    select(-grep(pattern = paste0(tsToAnalyze[1]), names(myData)))
  myDataSpecificValueBit <- myData %>%
    select(grep(pattern = paste0(tsToAnalyze[1], "_", tsToAnalyze[2]),
                names(myData))
    )
  myDataSpecificTimeBit <- myData %>%
    select(grep(pattern = paste0(tsToAnalyze[1], "_time"),
                names(myData))
    )
  
  myData <- cbind(myDataUniBit, myDataSpecificValueBit, myDataSpecificTimeBit)
  names(myData) <- c(names(myDataUniBit), "tsval", "tstime")
  
  myData$tstime <- myData$tstime %>% as.POSIXlt(., tz =" ")
  
  
  return(myData)
  
}