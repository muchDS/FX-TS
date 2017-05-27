library(rvest)
library(RPostgreSQL)
library(DBI)
library(dplyr)

address <- "23185.p.tld.pl"
dbName <- "pg23185_muchdsdb"
dbUser <- "pg23185_muchdsdb"
pass <- "!Muchdsdbpass"

connection <- dbConnect(PostgreSQL(),
                        dbname = dbName,
                        user = dbUser,
                        password = pass,
                        host = address)

#q <- dbGetQuery(connection, "SELECT * FROM fxtimeseriestable")

MarketIdxNamesPage <- read_html("https://www.investing.com/indices/major-indices")

MarketIdxNames <- html_nodes(MarketIdxNamesPage, css=".plusIconTd") %>%
                html_text() %>%
                gsub(pattern = "[[:space:][:punct:]]", replacement = "", .) %>%
                tolower()

FXRatesPage <- read_html("http://www.marketwatch.com/investing/currencies/tools")

FXPairsNames <- html_nodes(FXRatesPage, css=".bgCurrencyFormatting .left") %>%
  html_text() %>% 
  gsub(pattern = "[[:punct:]]quotes.*sampled$", replacement = "", .) %>%
  tolower()

dbColnames <- rep(NA, length(MarketIdxNames)*6 + length(FXPairsNames)*5+1 ) 
suffixVectFX <- c("bid", "ask", "high", "low", "time")
suffixVectMarket <- c("last", "high", "low", "chgv", "chgp", "time") 

dbColnames[1] <- "recordTime"  
counter <- 1
  
for (q in FXPairsNames){
  
  for(suffix in suffixVectFX){
    
    counter <- counter + 1
    dbColnames[counter] <- paste0(q, "_", suffix)
    
  }
  
}
 
  
  for (q in MarketIdxNames){
    
    for(suffix in suffixVectMarket){
      
      counter <- counter + 1
      dbColnames[counter] <- paste0(q, "_", suffix)
      
    }
    
  }



  initialTable <- matrix(0, nrow = 1, ncol = length(dbColnames))

  colnames(initialTable) <- dbColnames

  
  FXTimeSeriesTable <- initialTable %>% as.data.frame()
  FXTimeSeriesTable <- FXTimeSeriesTable %>%
                       mutate(recordTime = Sys.time() %>% as.character())
  
  FXTimeSeriesTable[1, grepl(pattern = "time$", colnames(FXTimeSeriesTable))] <- c(Sys.time())
  
  for(c in 1:length(colnames(FXTimeSeriesTable))){
    
    if(
        grepl(pattern = "_time$", colnames(FXTimeSeriesTable)[c], ignore.case = TRUE)
        ){
          FXTimeSeriesTable[,c] <- FXTimeSeriesTable[,c] %>% 
                                   as.POSIXct(origin = Sys.time()) %>% as.character()
        }
    
  }
  
  rowid <- 1
  FXTimeSeriesTable <- data.frame(rowid, FXTimeSeriesTable)
dbWriteTable(connection, "fxtimeseriestable",FXTimeSeriesTable)