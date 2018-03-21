#' Create Sample Plots
#' 
#' To create an individual plot from the grid produced by \code{plotSamplePlots()}
#' 
#' @param sampleObject Object generated from \code{makeSamplePlotObject()}
#' @param plotType Specifies the type of plot to be generated
#' \code{"CV"} - CV Sample/Score Plot - (k-1) X (k-1) upper triangular grid where $plot_{i,j}$ Corresponds to the sample/score plot for matrix j+1 (x-axis) and i (y-axis)\cr\cr
#' \code{"Full"} - Full Sample/Score Plot - Same gridded plot as above but now for the full scores i.e. the scores from the full analysis\cr\cr
#' \code{"Comparison"} - Matrix Specific Score Plots - k plots (k is the number of matrices in the analysis) which plot the scores for the CV analysis vs. the scores for the full analysis\cr\cr
#' \code{"SideBySide"} - Side by side plot of the CV and Full Plots
#'
#' @param xAxisPlot Index for which matrix should lie on the x-axis
#' @param yAxisPlot Index for which matrix should lie on the y-axis
#' @param colorVar Variable by which points can be colored by. If NA then no color will be given
#' 
#' @return Specified plot.
#' 
#' @examples plotSamplePlots(sampleObject,plotType,xAxisPlot,colorVar)
plotSamplePlots=function(sampleObject,plotType,xAxisPlot,yAxisPlot,colorVar=NULL){
  if(length(sampleObject)!=3){
    stop("Invalid sample object")
  }
  if(!(plotType %in% c("CV","Full","Comparison","SideBySide"))){
    stop("Invalid plot type")
  }
  scaledFull=sampleObject[[1]]
  scaledCVScores=sampleObject[[2]]
  idMem=sampleObject[[3]]
  if(plotType=="CV"){
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
  if(plotType=="Full"){
    corTemp=cor(scaledFull[[xAxisPlot]],scaledFull[[yAxisPlot]],use="complete.obs")
    p=ggplot(data.frame("t1"=scaledFull[[xAxisPlot]],"t2"=scaledFull[[yAxisPlot]]),aes(t1,t2))+xlab(paste("Full Score ",xAxisPlot,sep=""))+ylab(paste("Full Score ",yAxisPlot,sep=""))+coord_fixed(ratio=1)+ggtitle(paste("Cor: ",round(corTemp,4),sep=""))
    if(!is.null(colorVar)){
      p=p+geom_point(aes(color=(colorVar)))
    }else{
      p=p+geom_point()
    }
    print(p)
  }
  if(plotType=="Comparison"){
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
  if(plotType=="SideBySide"){
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