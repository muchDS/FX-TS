library(dplyr)
library(RPostgreSQL)
library(DBI)

source("connectMeToDB.R")
source("stockActiveTimesSelector.R")
connObj <- connectMeToDB()

# ściągam tabelke z netu
results <- dbGetQuery(connObj, "SELECT * FROM fxtimeseriestable") %>%
           arrange(rowid)
# resultsBUp <- dbGetQuery(connObj, "SELECT * FROM fxtimeseriestable") 
# kasuję puste wiersze (scrapowane gdy giełda nie działała jeszcze)
goodResults <- stockActiveTimesSelector(results)
# tworzę nową tabelę zawierającą jejdynie dobre wpisy -> 
# finalfxtimeseriestable

if("finalfxtimeseriestable" %in% dbListTables(connObj)){
  print("flushing data...")
  for(i in 2:length(goodResults[,1])){
    
    insertString <- paste0(goodResults[i,], collapse = ", ") %>% 
           gsub(pattern = "([[:digit:]]{4}\\-[[:digit:]]{2}\\-[[:digit:]]{2} [[:digit:]]{2}\\:[[:digit:]]{2}\\:[[:digit:]]{2}[[:space:]]{0,1}[[:upper:]]{0,4})", 
           replacement = "'\\1'", .)
    
    dbGetQuery(connObj, paste0("INSERT INTO finalfxtimeseriestable VALUES (",insertString,")"))
  }

  dbGetQuery(connObj, "DELETE FROM fxtimeseriestable")
  print("done")
  
} else {
  print("flushing data into new table...")
  dbWriteTable(connObj, "finalfxtimeseriestable", goodResults)
  print("done")
}
