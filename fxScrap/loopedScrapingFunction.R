loopedScrapingFunction <- function(connectionObject, FXCssTagsVector, MICssTagsVector, rowId, localProxyDF, localInitProxyIP, localInitProxyPort){

  source("readPage.R")
  startTime <- Sys.time()
  
    tryCatch(dbGetInfo(connectionObject), error = function(e){print("reconnecting");Sys.time();connectionObject <<- connectMeToDB();Sys.time()} )
  
  
  recordTime <- as.POSIXlt(Sys.time())
  recordTimeEDT <- recordTime
  recordTimeEDT$hour <- recordTimeEDT$hour - 6
  
  if(recordTimeEDT$wday == 5 && recordTimeEDT$wday == 16){scrapMe <<- FALSE}
  
  recordTimeEDT <- paste0(recordTimeEDT, " EDT")
  #MIRatesPage <- read_html("https://www.investing.com/indices/major-indices")
  iterKillCommand <- FALSE
  tryCatch(
    MIRatesPage <- readPage("https://www.investing.com/indices/major-indices", localProxyDF ,localInitProxyIP, localInitProxyPort),
    error = function(e){print("MIError");iterKillCommand <<- TRUE}
  )
  tryCatch(
    FXRatesPage <- readPage("http://www.marketwatch.com/investing/currencies/tools", localProxyDF, MIRatesPage$ip, MIRatesPage$port),
    error = function(e){print("FXError");iterKillCommand <<- TRUE}
    
  )

  #print(paste0(iterKillCommand, " ", MIRatesPage$ip, " ", FXRatesPage$ip, " ",is.null(MIRatesPage$page), " ",is.null(FXRatesPage$page)))
  
  
  
  if(iterKillCommand ||
     length(MIRatesPage$ip) == 0 || 
     length(FXRatesPage$ip) == 0 || 
     is.null(MIRatesPage$page) || 
     is.null(FXRatesPage$page)){print("empty iter")
                                return(NULL)
                               }
  
  listToReturn <- list(FXRatesPage$ip, FXRatesPage$port)
  names(listToReturn) <- c("ip", "port")
  
  insertString <- paste0(rowId, ", '", recordTime, " CEST'",
                         ", ",
                         scrapSingleTable(FXRatesPage$page, FXCssTagsVector, "#rates th", "FX"),
                         ", ",
                         scrapSingleTable(MIRatesPage$page, MICssTagsVector, "#cr_12 th", "MI"),
                         ", '", recordTimeEDT,"'")
  dbGetQuery(connectionObject, paste0("INSERT INTO fxtimeseriestable VALUES (", insertString,")"))
  
  
  waitTime <- ifelse(10 - (Sys.time() - startTime) < 0, 0, 10 - (Sys.time() - startTime))
  Sys.sleep(waitTime)
  return(listToReturn)
}