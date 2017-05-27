timeDiff <- rep(NA, 300)

while(scrapMe)  {
  #reading pages to be scraped
  MIRatesPage <- read_html("https://www.investing.com/indices/major-indices")
  FXRatesPage <- read_html("http://www.marketwatch.com/investing/currencies/tools")
  
  
}

# Setting CSS tags for FX scraping
FXCssTagsVector <- c(".bgCurrencyFormatting .left", ".bgBid", ".bgAsk", ".bgAsk+ td", "#rates td:nth-child(5)", ".bgTimestamp")

q <- scrapSingleTable(MIRatesPage, MICssTagsVector, "#cr_12 th", "MI")
p <- scrapSingleTable(FXRatesPage, FXCssTagsVector, "#rates th", "FX")

diffVect <- c()


#Setting FX tags which should be normalized
FXTagsToNormalize <- FXCssTagsVector[1]
#Setiing FX tags which should be turned into datetime
FXTagsToDatetime <- FXCssTagsVector[6]
# Setting FX tags which should be returned as numbers
FXTagsToNumeric <- FXCssTagsVector[2:5]

# extracting FX table's header 
FXTableHeader <- scrapVectorFromTable("#rates th", FXRatesPage, FXTagsToNormalize, FXTagsToDatetime, FXTagsToNumeric, "FX")

FXReadings <- lapply(FXCssTagsVector, scrapVectorFromTable, FXRatesPage, FXTagsToNormalize, FXTagsToDatetime, FXTagsToNumeric, "FX")
names(FXReadings) <- FXTableHeader
FXReadings <- FXReadings %>% as.data.frame()

# Seting CSS tags for MI scraping
MICssTagsVector <- c(".plusIconTd", ".plusIconTd+ td", "#cr_12 td:nth-child(4)", 
                     "#cr_12 td:nth-child(5)", ".bold:nth-child(6)", ".bold:nth-child(7)", "td:nth-child(8)")

#Setting FX tags which should be normalized
MITagsToNormalize <- MICssTagsVector[1]
#Setiing FX tags which should be turned into datetime
MITagsToDatetime <- MICssTagsVector[7]
# Setting FX tags which should be returned as numbers
MITagsToNumeric <- MICssTagsVector[2:7]

# extracting FX table's header 
MIHeaderCss <- "#cr_12 th"
MITableHeader <- scrapVectorFromTable(paste0(MIHeaderCss), MIRatesPage, MITagsToNormalize, MITagsToDatetime, MITagsToNumeric, "MI") 
MITableHeader <- MITableHeader[grep(pattern = ".*[[:alnum:]]{1,}.*", MITableHeader)] %>%
  gsub(pattern = "\\.$", replacement = "v", .) %>%
  gsub(pattern = "(\\. \\%)", replacement = "p",.) %>%
  tolower()

MIReadings <- lapply(MICssTagsVector, scrapVectorFromTable, MIRatesPage, MITagsToNormalize, MITagsToDatetime, MITagsToNumeric, "MI")
names(MIReadings) <- MITableHeader
MIReadings <- MIReadings %>% as.data.frame()