library(ggplot2)

#Code for finding local maximum, left side local minimum, right side local maximum for given part of whole time series

setwd("C:/Users/support/Dropbox/R programmin/dsMuch")


######################
##  Real data 
#####################
#realdata <- read.csv2('dbBup.CSV',sep=',', stringsAsFactors = F)
#saveRDS(realdata,'realdata.RDS')

realdata <- readRDS('realdata.RDS')

## select columns

x <- realdata[,c('recordTimeEDT','eurusd_bid','eurusd_ask'),]
x$eurusd_bid <- as.numeric(x$eurusd_bid)
x$eurusd_ask <- as.numeric(x$eurusd_ask)

x$rw <- x$eurusd_bid/x$eurusd_ask
x$pts <- NA                                     ## Flag for local minimum/maximum
x$value <- NA                                   ## Saving local min/max values in additional column (easier) (mostly for plotting)
x$index <- 1:nrow(x)    

### parameters:

  n <- 1000           ## Width interval (number of observations in one range)
  
## Comment: Width of interval should the a function in future

  
#####################
## FAKE DATA  
#   
# ### Random Walk simulation 
#  n <- 400           ## Width interval (number of observations in one range)  
#  rw.steps <- 10000  ## Steps taken by random walker
#   rw  <- cumsum(sample(c(-1,1),rw.steps, TRUE))
#   plot(rw,type='l')
# 
# 
# ### Data frame: random walk values
# 
#   x <- as.data.frame(rw, stringsAsFactors = F)    ## random walk values
#   x$pts <- NA                                     ## Flag for local minimum/maximum
#   x$value <- NA                                   ## Saving local min/max values in additional column (easier) (mostly for plotting)
#   x$index <- 1:nrow(x)                            ## index of steps
#   
#########################
  
  
  
  k <- round(nrow(x)/n)   ## number of ranges
  ks <- cumsum(rep(n,k))  ## coordinates for ranges (for plotting only)
  
### iterator for each range
  i <- 1

  for (i in 1:k){
    
    ## max observation number
    kni <- n*i
    
    ## find local maximum in range
    
    v <- ((kni-n)+1):kni
    
    maxs <- unique(sort(x$rw[v], decreasing = T))[1]
    
    x[which(x$rw %in% maxs & x$index %in% v),'pts'] <- 'local.max'
    
    max.index <- x[which(x$rw %in% maxs & x$index %in% v, x$pts %in%  c('local.max')),'index']
    
    
    ## for each maximum (when we got more than one global maximum (same value) in a range (quite rare))
    m <- 1
    for (m in 1:length(max.index)){
      
      #find left local minimum
      vl <- ((kni-n)+1):max.index[m]
      lmins <- unique(sort(x$rw[vl], decreasing = F))[1]
      x[which(x$rw %in% lmins & x$index %in% vl),'pts'] <- 'left.local.min'
      
      #find right local minimum
      vr <- max.index[m]:kni
      rmins <- unique(sort(x$rw[vr], decreasing = F))[1]
      x[which(x$rw %in% rmins & x$index %in% vr),'pts'] <- 'right.local.min'
      
    }
  }  
  
  ## save local values for plotting
  x$value <- ifelse(is.na(x$pts) == F, x$rw, x$value)  
  
  ggplot(data = x, aes(x=seq(1,nrow(x)), y= rw, group=1))+
    geom_line(stat='identity')+
    geom_vline(xintercept = ks, color = 'slategray2')+
    geom_point(aes(y=value,color=pts), na.rm=T)+
    theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
    scale_color_discrete(name = 'Point',breaks=c("left.local.min","local.max","right.local.min")) +
    xlab("Index") +
    ylab("Random Walk value") 
  
  
  
  
  
  
  
  
####################################
#     approximation
####################################  
  
  
  
  
apprx <-   x[is.na(x$pts) == F,]
apprx$index <- 1:nrow(apprx)
  
  ggplot(data = apprx, aes(x=seq(1,nrow(apprx)), y= rw, group=1))+
    geom_line(stat='identity')+
    theme(panel.background = element_rect(fill = 'white', colour = 'black'))+
    scale_color_discrete(name = 'Point',breaks=c("left.local.min","local.max","right.local.min")) +
    xlab("Index") +
    ylab("Random Walk value") 
  
  
  
  
  
####################################
#     Slope
####################################   
#   
#   
#   ind <- apprx[which(apprx$rw == max(apprx$rw)),'index']
#   vll <- apprx[which(apprx$rw == max(apprx$rw)),'value']
#   
#   
# ### found high point
# ### go from this point to the left
#   
#   gr <- 1
#   gr.list <- list()
#   for (gr in 1:(ind-1))
#   {
#     ind2 <- apprx[gr,c('index')]
#     vll2 <- apprx[gr,c('value')]
#     
#    gr.list[gr] <- (vll-vll2)/ (ind-ind2)  
#     
#   }  
# 
# ### higher sloper -  big difference
#   
#   
#   plot(unlist(gr.list))
#   
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  