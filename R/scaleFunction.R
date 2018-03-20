

scaleFunction=function(vec,full){
  corTemp=cor(vec,full,use="complete.obs")
  if(corTemp<0){
    vec=vec*(-1)
  }
  mu=mean(vec)
  madVal=mad(vec)
  return((vec-mu)/madVal)
}