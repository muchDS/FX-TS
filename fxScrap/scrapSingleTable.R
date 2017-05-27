scrapSingleTable <- function(page, CssTagsVector, headerTag, localCaseTag){
  
  library(DBI)
  library(RPostgreSQL)
  library(dplyr)
  library(rvest)
  # # # #  SOURCING # # # #
  # # # # FUNCTIONS # # # #
  
  source("scrapVectorFromTable.R")
  source("connectMeToDB.R")
  source("normalizeMyString.R")
    
  #Setting FX tags which should be normalized
  TagsToNormalize <- CssTagsVector[1]
  #Setiing FX tags which should be turned into datetime
  TagsToDatetime <- ifelse(localCaseTag == "FX", CssTagsVector[6], CssTagsVector[7])
  # Setting FX tags which should be returned as numbers
  
  if(localCaseTag == "FX"){
    TagsToNumeric <- CssTagsVector[2:5]
  }else{
    TagsToNumeric <- CssTagsVector[2:6]
  }
  
  # extracting FX table's header 
  TableHeader <- scrapVectorFromTable(headerTag, page, TagsToNormalize, TagsToDatetime, TagsToNumeric, localCaseTag)
  
  if(localCaseTag == "MI"){
    TableHeader <- TableHeader[grep(pattern = ".*[[:alnum:]]{1,}.*", TableHeader)] %>%
      gsub(pattern = "\\.$", replacement = "v", .) %>%
      gsub(pattern = "(\\. \\%)", replacement = "p",.) %>%
      tolower()
  }
  
  Readings <- lapply(CssTagsVector, scrapVectorFromTable, page, TagsToNormalize, TagsToDatetime, TagsToNumeric, localCaseTag)
  names(Readings) <- TableHeader
  Readings <- Readings %>% as.data.frame()
  
  
 for(i in 1:length(Readings[,1])) {
   firstPiece <- Readings[i, 2:(length(CssTagsVector)-1)] %>% as.character() %>% paste0(., collapse = ", ")
   timePiece <- Readings[i, length(CssTagsVector)]  %>% as.character()
   
   if(i == 1){
     singleRowReading <- paste0(firstPiece,", '", timePiece, "'")
   }else{
     singleRowReading <- paste0(singleRowReading, ", ", firstPiece,", '", timePiece, "'")
   }
   
 }
 
    return(singleRowReading)
  
}