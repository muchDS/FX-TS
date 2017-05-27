normalizeMyString <- function(inputVector){
  
  return(
      inputVector %>% 
        gsub(pattern = "[[:punct:]]", replacement = "", .) %>%
        gsub(pattern = "quotes.*sampled$", replacement = "", .) %>%
        tolower() %>%
        gsub(pattern = "[[:space:]]", replacement = "")
    )
 
  

  
}