stockActiveTimesSelector <- function(stockDF){
  stockDF <- stockDF[2:length(stockDF[,1]),]
  stockDF <- stockDF %>%
             arrange(rowid)
  
  stockDF <- stockDF[-grep(pattern = ".*23:59:59.*", stockDF$eurusd_time),]
 # stockStartTime <- stockDF[1, "eurusd_time"]
 # stockStopTime <- stockDF[length(stockDF[,1]), "eurusd_time"]
  
  firsValidID <- stockDF %>%
                  filter(eurusd_time <= stockDF[1, "eurusd_time"]) %>% 
                  select(rowid) %>% 
                  filter(row_number() == n()) %>%
                  unlist() %>% as.vector()
  lastValidID <- stockDF %>%
                 filter(eurusd_time == stockDF[length(stockDF[,1]), "eurusd_time"]) %>% 
                 select(rowid) %>% 
                 filter(row_number() == 1) %>%
                 unlist() %>% as.vector()
  
  
  stockDF <- stockDF %>%
             filter(rowid >= firsValidID & rowid <= lastValidID)
  # repeating smapling in order to delete remaining crap
  firsValidID <- stockDF %>%
    filter(eurusd_time <= stockDF[1, "eurusd_time"]) %>% 
    select(rowid) %>% 
    filter(row_number() == n()) %>%
    unlist() %>% as.vector()
  
  if(firsValidID - stockDF[1,1] > 0){
    stockDF <- stockDF %>%
      filter(rowid >= firsValidID)
  }
  
  lastValidID <- stockDF %>%
    filter(eurusd_time == stockDF[length(stockDF[,1])-1, "eurusd_time"]) %>% 
    select(rowid) %>% 
    filter(row_number() == 1) %>%
    unlist() %>% as.vector()

  if(stockDF[length(stockDF[,1]), 1] - lastValidID > 1){
    stockDF <- stockDF %>%
      filter(rowid <= lastValidID) 
  }
  
  return(
 stockDF 
  )
  
}


