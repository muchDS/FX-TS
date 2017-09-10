library(DBI)
library(RPostgreSQL)
library(dplyr)
library(rvest)
library(httr)
# # # #  SOURCING # # # #
# # # # FUNCTIONS # # # #

source("scrapVectorFromTable.R")
source("scrapSingleTable.R")
source("connectMeToDB.R")
source("normalizeMyString.R")
source("loopedScrapingFunction.R")

# # # # SCRIPT # # # # # 

#loop for scraping
#initial dt = 10 sec
scrapMe <- TRUE

FXCssTagsVector <- c(".bgCurrencyFormatting .left", ".bgBid", ".bgAsk", ".bgAsk+ td", "#rates td:nth-child(5)", ".bgTimestamp")
# Seting CSS tags for MI scraping
MICssTagsVector <- c(".plusIconTd", ".plusIconTd+ td", "#cr_12 td:nth-child(4)", 
                     "#cr_12 td:nth-child(5)", ".bold:nth-child(6)", ".bold:nth-child(7)", "td:nth-child(8)")

connectionObject <- connectMeToDB()
rowId <- dbGetQuery(connectionObject, "SELECT MAX(rowid) FROM fxtimeseriestable")
# to handle proxy server change
ignitionTime <- Sys.time() %>% as.POSIXlt()
# reading proxy server list
library(readr)
proxyDF <- read_csv("proxys.csv")
initialProxyIP <- proxyDF$ip[1]
initialProxyPort <- proxyDF$port[1]
newProxyList <- list()

while(scrapMe){
  
  currTime <- Sys.time() %>% as.POSIXlt()
  if(abs(currTime$hour - ignitionTime$hour) > 0){
    tryCatch(
      function(){
        dbDisconnect(connectionObject);
        connectionObject <<- connectMeToDB();
        ignitionTime <<- Sys.time() %>% as.POSIXlt()
        print("connection refreshed")
      }
      ,
      error = function(){
        print("Connection problem - trying to reconnect");
        Sys.sleep(60)
        
      }
      
    )

  }
  
  rowId <- rowId + 1
  
  tryCatch(newProxyList <<- loopedScrapingFunction(connectionObject, 
                                  FXCssTagsVector, 
                                  MICssTagsVector, 
                                  rowId, proxyDF,
                                  initialProxyIP, 
                                  initialProxyPort) , 
           error = function(e){print("waiting for connection");Sys.sleep(30);rowId <<- rowId - 1; print(e)})
  
  if(!is.null(newProxyList)){
    initialProxyIP <- newProxyList$ip
    initialProxyPort <- newProxyList$port
  }


}



  
