
plotMoviePlots=function(movieObject,plotType,xAxisPlot,yAxisPlot,colorVar=NULL,colorVarLabel=NULL,leg=TRUE){
  if(class(movieObject)!="movie"){
    stop("Not a 'movie' object")
  }
  if(!(plotType %in% c("CV","Full","Comparison","SideBySide"))){
    stop("Invalid plot type")
  }
 
  if(!(xAxisPlot%in%1:length(movieObject[[1]]))){
    stop("xAxisPlot not in range.")
  }
  if(!(yAxisPlot%in%1:length(movieObject[[1]]))){
    stop("yAxisPlot not in range.")
  }
  if(xAxisPlot==yAxisPlot){
    
  }

  scaledFull=movieObject[[1]]
  scaledCVScores=movieObject[[2]]
  idMem=movieObject[[3]]
  if(plotType=="CV"){
    ###Code for CV contribution plots
    corTemp=cor(scaledCVScores[[xAxisPlot]],scaledCVScores[[yAxisPlot]],use="complete.obs")
    p=ggplot(data.frame("t1"=scaledCVScores[[xAxisPlot]],"t2"=scaledCVScores[[yAxisPlot]]),aes(t1,t2))+xlab(paste(names(scaledCVScores)[xAxisPlot]," (CV)",sep=""))+ylab(paste(names(scaledCVScores)[yAxisPlot]," (CV)",sep=""))+coord_fixed(ratio=1)+geom_text(aes(size=10,x=Inf,y=-Inf,hjust=1.5,vjust=-1,label=paste("Cor: ",round(corTemp,4),sep="")))+theme(text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
    if(!is.null(colorVar)){
      p=p+geom_point(aes(color=colorVar))+labs(color=colorVarLabel)+theme(text = element_text(size=20))+guides(size=FALSE)
    }else{
      p=p+geom_point()+theme(text = element_text(size=20),legend.position="none")
    }
    print(p)
  }
  if(plotType=="Full"){
    corTemp=cor(scaledFull[[xAxisPlot]],scaledFull[[yAxisPlot]],use="complete.obs")
    p=ggplot(data.frame("t1"=scaledFull[[xAxisPlot]],"t2"=scaledFull[[yAxisPlot]]),aes(t1,t2))+xlab(paste(names(scaledFull)[xAxisPlot]," (Full)",sep=""))+ylab(paste(names(scaledFull)[yAxisPlot]," (Full)",sep=""))+coord_fixed(ratio=1)+geom_text(aes(size=10,x=Inf,y=-Inf,hjust=1.5,vjust=-1,label=paste("Cor: ",round(corTemp,4),sep="")))
    if(!is.null(colorVar)){
      p=p+geom_point(aes(color=(colorVar)))+labs(color=colorVarLabel)+theme(text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+guides(size=FALSE)
    }else{
      p=p+geom_point()+theme(text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position="none")
    }
    print(p)
  }
  if(plotType=="Comparison"){
    ###Code for CV vs Full Scores Plot
    
    corTemp=cor(scaledCVScores[[xAxisPlot]],scaledFull[[xAxisPlot]],use="complete.obs" )
    p=ggplot(data.frame("t1"=scaledCVScores[[xAxisPlot]],"t2"=scaledFull[[xAxisPlot]]),aes(t1,t2))+xlab(paste(names(scaledCVScores)[xAxisPlot]," (CV)",sep=""))+ylab(paste(names(scaledFull)[xAxisPlot]," (Full)",sep=""))+coord_fixed(ratio=1)+geom_text(aes(size=10,x=Inf,y=-Inf,hjust=1.5,vjust=-1,label=paste("Cor: ",round(corTemp,4),sep="")))
    if(!is.null(colorVar)){
      p=p+geom_point(aes(color=(colorVar)))+labs(color=colorVarLabel)+theme(text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+guides(size=FALSE)
    }else{
      p=p+geom_point()+theme(legend.position="none",text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    print(p)
  }
  if(plotType=="SideBySide"){
    corTemp1=cor(scaledFull[[xAxisPlot]],scaledFull[[yAxisPlot]],use="complete.obs")
    p1=ggplot(data.frame("t1"=scaledFull[[xAxisPlot]],"t2"=scaledFull[[yAxisPlot]]),aes(t1,t2))+xlab(paste(names(scaledFull)[xAxisPlot]," (Full)",sep=""))+ylab(paste(names(scaledFull)[yAxisPlot]," (Full)",sep=""))+coord_fixed(ratio=1)+geom_text(aes(size=10,x=Inf,y=-Inf,hjust=1.5,vjust=-1,label=paste("Cor: ",round(corTemp1,4),sep="")))
    if(!is.null(colorVar)){
      p1=p1+geom_point(aes(color=(colorVar)))+labs(color=colorVarLabel)+theme(text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
      leg=get_legend(p1)
      p1=p1+theme(legend.position="none")
    }else{
      p1=p1+geom_point()+theme(legend.position="none")+theme(text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
    }
    ###Code for CV contribution plots
    corTemp2=cor(scaledCVScores[[xAxisPlot]],scaledCVScores[[yAxisPlot]],use="complete.obs")
    p2=ggplot(data.frame("t1"=scaledCVScores[[xAxisPlot]],"t2"=scaledCVScores[[yAxisPlot]]),aes(t1,t2))+xlab(paste(names(scaledCVScores)[xAxisPlot]," (CV)",sep=""))+ylab(paste(names(scaledCVScores)[yAxisPlot]," (CV)",sep=""))+coord_fixed(ratio=1)+geom_text(aes(size=10,x=Inf,y=-Inf,hjust=1.5,vjust=-1,label=paste("Cor: ",round(corTemp2,4),sep="")))
    if(!is.null(colorVar)){
      p2=p2+geom_point(aes(color=(colorVar)))+labs(color=colorVarLabel)+theme(legend.position="none",text = element_text(size=20))+theme(text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
      m1=plot_grid(p1,p2,ncol=2,align="h")
      m2=plot_grid(m1,leg,ncol=2,rel_widths = c(6,.5))
      print(m2)
    }else{
      p2=p2+geom_point()+theme(legend.position="none",text = element_text(size=20),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))+guides(size=FALSE)
      m1=plot_grid(p1,p2,ncol=2,align="h")
      print(m1)
    }
  }
  
  

}
