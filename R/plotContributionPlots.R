#' Create contribution Plots
#' 
#' To create an individual plot from the grid produced by \code{plotcontributionPlots()}
#' 
#' @param contributionObject Object generated from \code{makecontributionPlotObject()}
#' @param plotType Specifies the type of plot to be generated
#' \code{"CV"} - CV contribution/Score Plot - (k-1) X (k-1) upper triangular grid where $plot_{i,j}$ Corresponds to the contribution/score plot for matrix j+1 (x-axis) and i (y-axis)\cr\cr
#' \code{"Full"} - Full contribution/Score Plot - Same gridded plot as above but now for the full scores i.e. the scores from the full analysis\cr\cr
#' \code{"Comparison"} - Matrix Specific Score Plots - k plots (k is the number of matrices in the analysis) which plot the scores for the CV analysis vs. the scores for the full analysis\cr\cr
#' \code{"SideBySide"} - Side by side plot of the CV and Full Plots
#'
#' @param xAxisPlot Index for which matrix should lie on the x-axis
#' @param yAxisPlot Index for which matrix should lie on the y-axis
#' @param colorVar Variable by which points can be colored by. If NA then no color will be given
#' 
#' @return Specified plot.
#' 
#' @examples plotContributionPlots(contributionObject,plotType,xAxisPlot,colorVar)
plotContributionPlots=function(contributionObject,plotType,xAxisPlot,yAxisPlot,colorVar=NULL){
  if(length(contributionObject)!=3){
    stop("Invalid contribution object")
  }
  if(!(plotType %in% c("CV","Full","Comparison","SideBySide"))){
    stop("Invalid plot type")
  }
  if(!is.null(colorVar)&is.null(names(colorVar)) ){
    warning("Color variable does not have a label. Legend will not have a label.")
  }
  scaledFull=contributionObject[[1]]
  scaledCVScores=contributionObject[[2]]
  idMem=contributionObject[[3]]
  if(plotType=="CV"){
    ###Code for CV contribution plots
    corTemp=cor(scaledCVScores[[xAxisPlot]],scaledCVScores[[yAxisPlot]],use="complete.obs")
    p=ggplot(data.frame("t1"=scaledCVScores[[xAxisPlot]],"t2"=scaledCVScores[[yAxisPlot]]),aes(t1,t2))+xlab(paste(names(scaledCVScores)[xAxisPlot]," (CV)",sep=""))+ylab(paste(names(scaledCVScores)[yAxisPlot]," (CV)",sep=""))+coord_fixed(ratio=1)+ggtitle(paste("Cor: ",round(corTemp,4),sep=""))
    if(!is.null(colorVar)){
      p=p+geom_point(aes(color=(colorVar)))+labs(color=names(colorVar))
    }else{
      p=p+geom_point()
    }
    print(p)
  }
  if(plotType=="Full"){
    corTemp=cor(scaledFull[[xAxisPlot]],scaledFull[[yAxisPlot]],use="complete.obs")
    p=ggplot(data.frame("t1"=scaledFull[[xAxisPlot]],"t2"=scaledFull[[yAxisPlot]]),aes(t1,t2))+xlab(paste(names(scaledFull)[xAxisPlot]," (Full)",sep=""))+ylab(paste(names(scaledFull)[yAxisPlot]," (Full)",sep=""))+coord_fixed(ratio=1)+ggtitle(paste("Cor: ",round(corTemp,4),sep=""))
    if(!is.null(colorVar)){
      p=p+geom_point(aes(color=(colorVar)))+labs(color=names(colorVar))
    }else{
      p=p+geom_point()
    }
    print(p)
  }
  if(plotType=="Comparison"){
    ###Code for CV vs Full Scores Plot
    
    corTemp=cor(scaledCVScores[[xAxisPlot]],scaledFull[[xAxisPlot]],use="complete.obs" )
    p=ggplot(data.frame("t1"=scaledCVScores[[xAxisPlot]],"t2"=scaledFull[[xAxisPlot]]),aes(t1,t2))+xlab(paste(names(scaledCVScores)[xAxisPlot]," (CV)",sep=""))+ylab(paste(names(scaledFull)[xAxisPlot]," (Full)",sep=""))+coord_fixed(ratio=1)+ggtitle(paste("Cor: ",round(corTemp,4),sep=""))
    if(!is.null(colorVar)){
      p=p+geom_point(aes(color=(colorVar)))+labs(color=names(colorVar))
    }else{
      p=p+geom_point()
    }
    print(p)
  }
  if(plotType=="SideBySide"){
    corTemp=cor(scaledFull[[xAxisPlot]],scaledFull[[yAxisPlot]],use="complete.obs")
    p1=ggplot(data.frame("t1"=scaledFull[[xAxisPlot]],"t2"=scaledFull[[yAxisPlot]]),aes(t1,t2))+xlab(paste(names(scaledFull)[xAxisPlot]," (Full)",sep=""))+ylab(paste(names(scaledFull)[yAxisPlot]," (Full)",sep=""))+coord_fixed(ratio=1)+ggtitle(paste("Cor: ",round(corTemp,4),sep=""))
    if(!is.null(colorVar)){
      p1=p1+geom_point(aes(color=(colorVar)))+labs(color=names(colorVar))
    }else{
      p1=p1+geom_point()
    }
    ###Code for CV contribution plots
    corTemp=cor(scaledCVScores[[xAxisPlot]],scaledCVScores[[yAxisPlot]],use="complete.obs")
    p2=ggplot(data.frame("t1"=scaledCVScores[[xAxisPlot]],"t2"=scaledCVScores[[yAxisPlot]]),aes(t1,t2))+xlab(paste(names(scaledCVScores)[xAxisPlot]," (CV)",sep=""))+ylab(paste(names(scaledCVScores)[yAxisPlot]," (CV)",sep=""))+coord_fixed(ratio=1)+ggtitle(paste("Cor: ",round(corTemp,4),sep=""))
    if(!is.null(colorVar)){
      p2=p2+geom_point(aes(color=(colorVar)))+labs(color=names(colorVar))
    }else{
      p2=p2+geom_point()
    }
    m1=plot_grid(p1,p2,ncol=2)
    print(m1)
  }
}