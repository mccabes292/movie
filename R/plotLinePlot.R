
plotLinePlot=function(t1,leg=TRUE){
  numData=length(t1$fullContributions)
  numComp=choose(numData,2)
  corrMat=data.frame(matrix(rep(0,numComp*2),ncol=1))
  colnames(corrMat)=c("Corr")
  k=1
  for(i in 1:(numData-1)){
    for(j in (i+1):numData){
      full1=t1$fullContributions[[i]]
      full2=t1$fullContributions[[j]]
      corrMat[k,1]=cor(full1,full2)
      k=k+1
      cv1=t1$cvContributions[[i]]
      cv2=t1$cvContributions[[j]]
      corrMat[k,1]=cor(cv1,cv2)
      k=k+1
    }
    
  }
  if(numData==2){
    corrMat$comp=c(1,1)
    corrMat$full=factor(c("Full","CV"),levels=c("Full","CV"))
  }else{
    corrMat$comp=rep(1:numData,each=2)
    corrMat$full=factor(rep(c("Full","CV"),numData),levels=c("Full","CV"))
  }
  
  datName=names(t1$cvContributions)
  nameList=NULL
  k=1
  for(i in 1:(length(datName)-1)){
    for(j in (i+1):length(datName)){
      nameList[k]=paste(datName[i]," & ",datName[j],sep="" )
      k=k+1
    }
  }
  
  p1=ggplot(data=corrMat,aes(x=full,y=Corr,group=comp)  )+geom_line(aes(linetype=factor(comp,labels = nameList)))+xlab("")+ylab("Correlation")+ylim(-0.1,1)
  p1=p1+theme_bw()+theme(text=element_text(size=50),axis.text.x=element_text(size=50),axis.text.y=element_text(size=30),axis.title.y=element_text(size=25),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="right",legend.text=element_text(size=10),legend.title=element_blank(),panel.border = element_blank(), axis.line = element_line())+geom_point()+scale_linetype_manual(values=c("longdash", "dotted","solid"))
  if(leg==FALSE){
    p1=p1+theme(legend.position = "none")
  }
  
  return(p1)
}

