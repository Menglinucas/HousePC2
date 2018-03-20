# this function is not used
# convert sp to dataframe

segpiece<-function(qu){
  
  # source("fortify-spatial.r",encoding = 'UTF-8')
  
  qu<-plyr::ldply(qu@polygons,fortify)
  
  natab<-data.frame("long"=NA,"lat"=NA,"order"=NA,
                    "hole"=NA,"piece"=NA,"id"=NA,"group"=NA)
  
  qu2<-subset(qu,piece==1)
  
  if (length(unique(qu$piece))>=2){
    for (i in 2:length(unique(qu$piece)))
    {
      qu2<-rbind(qu2,natab)
      qu2<-rbind(qu2,subset(qu,piece==i))
    }
  }
  
  row.names(qu2)<-c(1:nrow(qu2))
  
  qu<-qu2
  
  return(qu)
  
}
