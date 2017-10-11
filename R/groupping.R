
# grouping by year

group<-function(result,year){
  
  result$year<-NA
  
  names(result)[ncol(result)]<-year
  
  gcol<-which(substr(names(result[1:ncol(result)-1]),1,4)==year)
  
  for (i in (1:nrow(result)))
  {
    gsub<-subset(result,row.names(result)==i)[gcol]
    gsub2<-gsub[!is.na(gsub)]
    if (length(gsub2)>0)
    {
      result[i,ncol(result)]<-sum(gsub2)/length(gsub2)
    }
  }
  
  return(result)
  
}