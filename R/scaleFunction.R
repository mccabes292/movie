

scaleFunction=function(vec,full,scaleType="SD",scaleFold=TRUE){

  corTemp=cor(vec,full,use="complete.obs")
  #print(corTemp)
  if(corTemp<0){
    vec=vec*(-1)
  }
  
  if(scaleFold==FALSE){
    return(vec)
  }
  mu=mean(vec)
  if(scaleType=="SD"){
    madVal=sd(vec)
  }
  if(scaleType=="MAD"){
    madVal=mad(vec)
  }

  madFinal=ifelse(madVal==0,1,madVal)
  return((vec-mu)/madFinal)
}
