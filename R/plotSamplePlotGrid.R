#'Grid of Sample Plots
#'
#'This function creates a grid of plots to help validate the effectiveness and the extent of over fitting for multi-omics methods.
#'
#'@param sampleObject Output of makeSamplePlotObject()
#'@param plotType  Specifies which plots to make \cr\cr
#' \code{"CV"} - CV Sample/Score Plot - (k-1) X (k-1) upper triangular grid where $plot_{i,j}$ Corresponds to the sample/score plot for matrix j+1 (x-axis) and i (y-axis)\cr\cr
#' \code{"Full"} - Full Sample/Score Plot - Same gridded plot as above but now for the full scores i.e. the scores from the full analysis\cr\cr
#' \code{"Comparison"} - Matrix Specific Score Plots - k plots (k is the number of matrices in the analysis) which plot the scores for the CV analysis vs. the scores for the full analysis\cr\cr
#'
#'
#'
plotSamplePlotGrid=function(sampleObject,plotType){
  if(length(sampleObject)!=3){
    stop("Invalid sample object")
  }
  if(!(plotType %in% c("CV","Full","Comparison"))){
    stop("Invalid plot type")
  }
  scaledFull=sampleObject[[1]]
  scaledCVScores=sampleObject[[2]]
  idMem=sampleObject[[3]]
  if(plotType=="CV"){
    ###Code for CV Sample plots
    par(mfrow=c(length(scaledCVScores)-1,length(scaledCVScores)-1))
    for(i in 1:(length(scaledCVScores)-1)){
      replicate(i-1,plot.new())
      for(j in (i+1):length(scaledCVScores)){
        
        plot(scaledCVScores[[j]],scaledCVScores[[i]],xlab=paste("CV Score ",j,sep=""),ylab=paste("CV Score ",i,sep=""),asp=1)
      }
      
    }
  }
  if(plotType=="Full"){
    par(mfrow=c(length(scaledFull)-1,length(scaledFull)-1))
    for(i in 1:(length(scaledFull)-1)){
      replicate(i-1,plot.new())
      for(j in (i+1):length(scaledFull)){
        
        plot(scaledFull[[j]],scaledFull[[i]],xlab=paste("Full Score ",j,sep=""),ylab=paste("Full Score ",i,sep=""),asp=1)
      }
      
    }
  }
  if(plotType=="Comparison"){
    ###Code for CV vs Full Scores Plot
    low=floor(sqrt(length(scaledCVScores)))
    high=ceiling(sqrt(length(scaledCVScores)))
    par(mfrow=c(low,high))
    for(i in 1:length(scaledCVScores)){
      plot(scaledCVScores[[i]],scaledFull[[i]],xlab="CV Scores",ylab="Full Scores",main=paste("Matrix ",i,sep=""),asp=1)
    }
  }
}
