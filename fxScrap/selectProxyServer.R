selectProxyServer <- function(url, localProxyDF){
  
  firstAvalProxy <- 0
  
  for(i in 1:length(localProxyDF$ip)){
    pageToReturn <<- NULL
    pageToReturn <<- tryCatch(GET(
                                  paste0(url), use_proxy(paste0(localProxyDF$ip[i]), 
                                  localProxyDF$port[i], 
                                  auth = "basic"),
                                  timeout(4)
    ),
    error = function(e) {}
    
    )
    if(!is.null(pageToReturn)){
      firstAvalProxy <- i
      break
      }
    
  }
  
  return(firstAvalProxy)
  
}