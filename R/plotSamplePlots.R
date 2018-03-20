#' Create Sample Plots
#' 
#' To create an individual plot from the grid produced by \code{plotSamplePlots()}
#' 
#' @param sampleObject Object generated from \code{makeSamplePlotObject()}
#' @param plotNum See plotSamplePlots()
#' @param xAxisPlot Index for which matrix should lie on the x-axis
#' @param yAxisPlot Index for which matrix should lie on the y-axis
#' @param colorVar Variable by which points can be colored by. If NA then no color will be given
#' 
#' @return Specified plot.
#' 
#' @examples plotSamplePlots(sampleObject,plotNum,xAxisPlot,colorVar)
plotSamplePlots=function(sampleObject,plotNum,xAxisPlot,yAxisPlot,colorVar=NULL){
  if(length(sampleObject)!=3){
    stop("Invalid sample object")
  }
  if(!(plotNum %in% c(1,2,3,4))){
    stop("Invalid plot number")
  }
  scaledFull=sampleObject[[1]]
  scaledCVScores=sampleObject[[2]]
  idMem=sampleObject[[3]]
  if(plotNum==1){
    ###Code for CV Sample plots
    corTemp=cor(scaledCVScores[[xAxisPlot]],scaledCVScores[[yAxisPlot]],use="complete.obs")
    p=ggplot(data.frame("t1"=scaledCVScores[[xAxisPlot]],"t2"=scaledCVScores[[yAxisPlot]]),aes(t1,t2))+xlab(paste("CV Score ",xAxisPlot,sep=""))+ylab(paste("CV Score ",yAxisPlot,sep=""))+coord_fixed(ratio=1)+ggtitle(paste("Cor: ",round(corTemp,4),sep=""))
    if(!is.null(colorVar)){
      p=p+geom_point(aes(color=(colorVar)))
    }else{
      p=p+geom_point()
    }
    print(p)
  }
  if(plotNum==2){
    corTemp=cor(scaledFull[[xAxisPlot]],scaledFull[[yAxisPlot]],use="complete.obs")
    p=ggplot(data.frame("t1"=scaledFull[[xAxisPlot]],"t2"=scaledFull[[yAxisPlot]]),aes(t1,t2))+xlab(paste("Full Score ",xAxisPlot,sep=""))+ylab(paste("Full Score ",yAxisPlot,sep=""))+coord_fixed(ratio=1)+ggtitle(paste("Cor: ",round(corTemp,4),sep=""))
    if(!is.null(colorVar)){
      p=p+geom_point(aes(color=(colorVar)))
    }else{
      p=p+geom_point()
    }
    print(p)
  }
  if(plotNum==3){
    ###Code for CV vs Full Scores Plot
    
    corTemp=cor(scaledCVScores[[xAxisPlot]],scaledFull[[xAxisPlot]],use="complete.obs" )
    p=ggplot(data.frame("t1"=scaledCVScores[[xAxisPlot]],"t2"=scaledFull[[xAxisPlot]]),aes(t1,t2))+xlab(paste("CV Score ",xAxisPlot,sep=""))+ylab(paste("Full Score ",xAxisPlot,sep=""))+coord_fixed(ratio=1)+ggtitle(paste("Cor: ",round(corTemp,4),sep=""))
    if(!is.null(colorVar)){
      p=p+geom_point(aes(color=(colorVar)))
    }else{
      p=p+geom_point()
    }
    print(p)
  }
  if(plotNum==4){
    corTemp=cor(scaledFull[[xAxisPlot]],scaledFull[[yAxisPlot]],use="complete.obs")
    p1=ggplot(data.frame("t1"=scaledFull[[xAxisPlot]],"t2"=scaledFull[[yAxisPlot]]),aes(t1,t2))+xlab(paste("Full Score ",xAxisPlot,sep=""))+ylab(paste("Full Score ",yAxisPlot,sep=""))+coord_fixed(ratio=1)+ggtitle(paste("Cor: ",round(corTemp,4),sep=""))
    if(!is.null(colorVar)){
      p1=p1+geom_point(aes(color=(colorVar)))
    }else{
      p1=p1+geom_point()
    }
    ###Code for CV Sample plots
    corTemp=cor(scaledCVScores[[xAxisPlot]],scaledCVScores[[yAxisPlot]],use="complete.obs")
    p2=ggplot(data.frame("t1"=scaledCVScores[[xAxisPlot]],"t2"=scaledCVScores[[yAxisPlot]]),aes(t1,t2))+xlab(paste("CV Score ",xAxisPlot,sep=""))+ylab(paste("CV Score ",yAxisPlot,sep=""))+coord_fixed(ratio=1)+ggtitle(paste("Cor: ",round(corTemp,4),sep=""))
    if(!is.null(colorVar)){
      p2=p2+geom_point(aes(color=(colorVar)))
    }else{
      p2=p2+geom_point()
    }
    m1=plot_grid(p1,p2,ncol=2)
    print(m1)
  }
}