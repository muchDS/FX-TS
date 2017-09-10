

library(DBI)
library(RPostgreSQL)
library(readr)
library(dplyr)
library(lubridate)

# params
tsToAnalyze <- "eurusd_ask" %>% 
                strsplit(split = "\\_") %>%
                unlist() %>% as.vector()

source("connectMeToDB.R")
connObj <- connectMeToDB()

myData <- dbGetQuery(connObj, "SELECT * FROM finalfxtimeseriestable")

# czytanie danych z csv

# myData <- read_csv("dbBup.csv",n_max = 1)
# colTypes <- rep.int("n", length(colnames(myData)))
# colTypes[grep(pattern = "time", colnames(myData), ignore.case = TRUE)] <- "c"
# colTypes <- colTypes %>% as.list()
# myData <- read_csv("dbBup.csv",col_types = colTypes)

# zbieram interesujące kolumny
myData <- myData[,grepl(pattern = paste0("rowid|recordTime|", 
                                         tsToAnalyze[1], "_", tsToAnalyze[2], "|", 
                                         tsToAnalyze[1], "_time"), 
                        names(myData))
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

myData$tstime <- myData$tstime %>% as.POSIXlt()

return(myData)

# zakładam okienko 5min 
# przeszukuję przedziały od ostatniej obserwacji

if(("pointsToPlot" %>% exists(.))){
  remove(pointsToPlot)
  remove(windowLatestEntry)
  remove(finalDF)
  #remove(windowEarliesEntry)
}

source("selectWindow.R")
initialSpan <- 2  # szerokość przedziału przeszukiwania
windowEarliesEntry <- myData$tstime[length(myData$tstime)]

while(windowEarliesEntry >= myData$tstime[1]){
  
  leftSideEmpty <- FALSE
  rightSideEmpty <- FALSE
  
      if(!("windowLatestEntry" %>% exists(.))){
        
        windowEarliesEntry <- myData$tstime[length(myData$tstime)] - minutes(initialSpan)
        windowLatestEntry <- myData$tstime[length(myData$tstime)]
        
        print("ding1")
      }
  
  # myTimeWindow <- selectWindow(myData, windowEarliesEntry, windowLatestEntry, 30)
  myTimeWindow <- myData %>%
                    filter(tstime >= windowEarliesEntry & tstime <= windowLatestEntry)
  
  nextStart <- grep(pattern = paste0("^",
                                     myTimeWindow$tstime[1],
                                     "$"),
                    myData$tstime)
  
  windowLatestEntry <- myData$tstime[nextStart[length(nextStart)] - 1] 
  windowEarliesEntry <- windowLatestEntry %>% as.POSIXlt() - minutes(initialSpan)
  
  

      print(windowEarliesEntry)
      print(windowLatestEntry)

  # szukam lokalnego maksimum
  myTimeWindowMaxSplit <- myTimeWindow %>%
    filter(tsval == max(tsval)) %>% 
    select(rowid) %>% unlist() %>% as.vector()
  
  myTimeWindowLeft <- myTimeWindow %>%
                      filter(rowid < myTimeWindowMaxSplit[1])
  if(myTimeWindowLeft$tsval %>% length() > 0){
    leftMin <- myTimeWindowLeft %>%
               filter(tsval == min(tsval)) %>%
               arrange(desc(rowid))
  } else {
    leftSideEmpty <- TRUE
  }
  
  myTimeWindowRight <- myTimeWindow %>%
                       filter(rowid > myTimeWindowMaxSplit[length(myTimeWindowMaxSplit)])
  if(myTimeWindowRight$tsval %>% length()>0){
    rightMin <- myTimeWindowRight %>%
                filter(tsval == min(tsval)) %>%
                arrange(desc(rowid))
  } else {
    rightSideEmpty <- TRUE
  }
  
  maxRow <- myTimeWindow[myTimeWindow$rowid %in% myTimeWindowMaxSplit,] %>% 
            arrange(desc(rowid))
  
 if(!rightSideEmpty){
   pointsToPlot <- rbind(rightMin, maxRow)
 } else{
   pointsToPlot <- maxRow
 }
  
  if(!leftSideEmpty){
    pointsToPlot <- rbind(pointsToPlot, leftMin)
  }
  
  if(!("finalDF" %>% exists(.))){
    finalDF <- pointsToPlot
  } else {
    finalDF <- rbind(finalDF, pointsToPlot)
  }
  
}

plot(x = finalDF$tstime, y = finalDF$tsval, ylim = c(min(finalDF$tsval), max(finalDF$tsval)), pch = 20, cex = 0.6, type = "l",
     lty = 5, lwd =2)
par(new = TRUE)
plot(x = myData$tstime, y = myData$tsval,
     xlim = c(finalDF$tstime[finalDF$tstime %>% length()], finalDF$tstime[1]),
     axes = FALSE, xlab = "",
     ylim = c(min(finalDF$tsval), max(finalDF$tsval)),
     ylab = "" , type = "l", col="red")



# # żeby istniały jakiekolwiek szanse na formację musząpojawić isę jeszcze dwa maksima, zaraz po minimach
# # szukam minimów po lewej i prawej stronie maksimum przedziału
# 
# tsvalnext <- myTimeWindow$tsval[2:length(myTimeWindow$tsval)]
# tstimenext <- myTimeWindow$tstime[2:length(myTimeWindow$tsval)]
# myTimeWindow <- cbind(myTimeWindow[-length(myTimeWindow$tsval),], tsvalnext, tstimenext)
# 
# myTimeWindow <- myTimeWindow %>%
#                         mutate(avgtsval = (tsvalnext + tsval)/2, avgtstime = (tstime %>% as.POSIXlt() + 
#                                                                                 (tstimenext %>% as.POSIXlt() -
#                                                                                    tstime %>% as.POSIXlt())/2))
# 
# myTimeWindow$tsval <- myTimeWindow$avgtsval
# myTimeWindow$tstime <- myTimeWindow$avgtstime
# 
# 
# tsvalnext <- myTimeWindow$tsval[2:length(myTimeWindow$tsval)]
# tstimenext <- myTimeWindow$tstime[2:length(myTimeWindow$tsval)]
# myTimeWindow <- myTimeWindow[-length(myTimeWindow$tsval),]
# myTimeWindow$tsvalnext <- tsvalnext
# myTimeWindow$tstimenext <- tstimenext
# 
# myTimeWindow <- myTimeWindow %>%
#   mutate(avgtsval = (tsvalnext + tsval)/2, avgtstime = (tstime %>% as.POSIXlt() + 
#                                                           (tstimenext %>% as.POSIXlt() -
#                                                              tstime %>% as.POSIXlt())/2))
# 
# 
# timeDiff <- myData$recordTime %>% as.POSIXlt() - myData$eurusd_time %>% as.POSIXlt()

# for(i in grep(pattern = "\\_time$", colnames(myData))){
# 
#   timeToCheck <- myData[, i] %>% unlist() %>% as.vector()  %>% as.POSIXlt()
#   timeChangeCheck <- ((myData$recordTime %>% as.POSIXlt() - 
#                      timeToCheck) %>% round(., digits = 0)) == -18
#   transferVect <- myData[, i] %>% unlist() %>% as.vector() %>% as.POSIXlt()
#   transferVect[timeChangeCheck] <- (transferVect[timeChangeCheck] - days(1)) 
#   transferVect <- transferVect %>% as.vector() %>% as.character()
#   myData[, i] <- transferVect
#   
# }

plot(y = myTimeWindow$tsval, 
     x  = myTimeWindow$tstime ,
     type = "l")



