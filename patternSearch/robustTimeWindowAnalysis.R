

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


# zakładam okienko 5min 
# przeszukuję przedziały od ostatniej obserwacji
source("selectWindow.R")
initialSpan <- 5  # szerokość przedziału przeszukiwania

while(windowEarliesEntry >= myData$tstime[1]){
  
if(!("windowLatestEntry" %>% exists(.))){
  myTimeWindow <- selectWindow(myData, 
                               myData$tstime[length(myData$tstime)] - minutes(initialSpan), 
                               myData$tstime[length(myData$tstime)])
}else{
  myTimeWindow <- selectWindow(myData, windowEarliesEntry, windowLatestEntry)
}
  
  
  
  
  windowLatestEntry <- myData$tstime[length(myData$tstime)]
  windowEarliesEntry <- windowLatestEntry - minutes(initialSpan)

  # szukam lokalnego maksimum
  myTimeWindowMaxSplit <- myTimeWindow %>%
    filter(tsval == max(tsval)) %>%
    select(rowid) %>% unlist() %>% as.vector()
  # przedział przed maksimum
  leftMin <- myTimeWindow %>%
    filter(rowid <= myTimeWindowMaxSplit) %>%
    filter(tsval == min(tsval))
  # przedział po maksimum
  rightMin <- myTimeWindow %>%
    filter(rowid >= myTimeWindowMaxSplit) %>%
    filter(tsval == min(tsval))
  
}





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



