readPage <- function(url, localProxyDF, proxyIP, proxyPort){
newProxyIdx <- 0
    
  source("selectProxyServer.R")
  pageToReturn <- NULL
  
  tryCatch(
    pageToReturn <- read_html(paste0(url)),
    error = function(e){
    print("read_html failed")
      tryCatch(
               pageToReturn <<- GET(paste0(url), use_proxy(paste0(proxyIP), 
                            proxyPort, 
                             auth = "basic")) %>% read_html(),
        error = function(e){
          print("going for new proxy")
          newProxyIdx <<- selectProxyServer(url, localProxyDF)
                     proxyIP <<- localProxyDF$ip[newProxyIdx]
                     proxyPort <<- localProxyDF$port[newProxyIdx]
                     
                     if(newProxyIdx == 0){
                       print("proxy timeout")
                     }else{
                       print(paste0("using:", proxyIP, " ",proxyPort))
                     }
                     
                     pageToReturn <<- GET(
                                           paste0(url), use_proxy(paste0(proxyIP), 
                                            proxyPort , 
                                              auth = "basic"))%>% read_html()
        }
        
      )
      
    },
    finally = {
      
      listToReturn <- list(pageToReturn, proxyIP, proxyPort)
      names(listToReturn) <- c("page", "ip", "port")
      return(listToReturn)
    }
  )
}