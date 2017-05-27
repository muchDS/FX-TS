scrapVectorFromTable <- function(cssTag, page, tagsForNormalization, tagsForDatetime, tagsForNumeric, caseTag){
  
 
  # print(cssTag)
  # print(tagsForNormalization)
  # print(tagsForDatetime)
  # print(tagsForNumeric)
  # print(caseTag)
  # 
  source("normalizeMyString.R")
  source("datetimeToPOSIXct.R")
  
  if(grepl(pattern = paste0(cssTag), tagsForDatetime,fixed = TRUE)%>% sum()>0){
   
      return(
        html_nodes(page, css = paste0(cssTag)) %>%
                html_text() %>%
                sapply(., datetimeToPOSIXct)  %>% 
                 as.POSIXct()
      )
    
    
  #   if(grepl(pattern = "^[[:digit:]]{2}\\/[[:digit:]]{2}\\/[[:digit:]]{2}$", timeVect) %>% sum() > 0 && caseTag == "FX"){
  #      return( 
  #            gsub(pattern = "(^[[:digit:]]{2})\\/([[:digit:]]{2})\\/([[:digit:]]{2}$)",
  #            replacement = "20\\3\\-\\1\\-\\2 23:59 EST", 
  #            timeVect) %>% 
  #            as.POSIXct(., origin = Sys.time(), tz = "EST")
  #             )
  # } else {
  #     return(as.POSIXct(timeVect, origin = Sys.time(), tz = "EST"))
  #   }
    
  }
  
  if(grepl(pattern = paste0(cssTag), tagsForNormalization, fixed = TRUE)%>% sum()>0){
    # print("normalizing")
    return(
      html_nodes(page, css = paste0(cssTag)) %>%
        html_text() %>%
        normalizeMyString()
    )
  }else if(grepl(pattern = paste0(cssTag), tagsForNumeric, fixed = TRUE) %>% sum()>0 && caseTag == "FX"){
    # print("FX numeric")
    return(
      html_nodes(page, css = paste0(cssTag)) %>%
        html_text() %>%
        as.numeric()
    )
  } else if(grepl(pattern = paste0(cssTag), tagsForNumeric, fixed = TRUE) %>% sum()>0 && caseTag == "MI"){
    # print("MI numeric")
    return(
      html_nodes(page, css = paste0(cssTag)) %>%
        html_text() %>%
        gsub(pattern = "\\,|\\+|\\%", replacement = "", .) %>%
        as.numeric()
    )
  } else{
    # print("plaint vect return")
    return(
      html_nodes(page, css = paste0(cssTag)) %>%
        html_text() 
    )
  }
  
  
}