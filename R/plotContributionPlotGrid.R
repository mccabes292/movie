#'Grid of contribution Plots
#'
#'This function creates a grid of plots to help validate the effectiveness and the extent of over fitting for multi-omics methods.
#'
#'@param contributionObject Output of makeContributionPlotObject()
#'@param plotType  Specifies which plots to make \cr\cr
#' \code{"CV"} - CV contribution/Score Plot - (k-1) X (k-1) upper triangular grid where $plot_{i,j}$ Corresponds to the contribution/score plot for matrix j+1 (x-axis) and i (y-axis)\cr\cr
#' \code{"Full"} - Full contribution/Score Plot - Same gridded plot as above but now for the full scores i.e. the scores from the full analysis\cr\cr
#' \code{"Comparison"} - Matrix Specific Score Plots - k plots (k is the number of matrices in the analysis) which plot the scores for the CV analysis vs. the scores for the full analysis\cr\cr
#'
#'
#'
plotContributionPlotGrid=function(contributionObject,plotType){
 
  if(length(contributionObject)!=3){
    stop("Invalid contribution object")
  }
  if(!(plotType %in% c("CV","Full","Comparison"))){
    stop("Invalid plot type")
  }
  scaledFull=contributionObject[[1]]
  scaledCVScores=contributionObject[[2]]
  idMem=contributionObject[[3]]
  if(plotType=="CV"){
    ###Code for CV contribution plots
    par(mfrow=c(length(scaledCVScores)-1,length(scaledCVScores)-1))
    for(i in 1:(length(scaledCVScores)-1)){
      replicate(i-1,plot.new())
      for(j in (i+1):length(scaledCVScores)){
        corTemp=cor(scaledCVScores[[j]],scaledCVScores[[i]],use="complete.obs")
        plot(scaledCVScores[[j]],scaledCVScores[[i]],xlab=paste(names(scaledCVScores)[j]," (CV)",sep=""),ylab=paste(names(scaledCVScores)[i]," (CV)",sep=""),main=paste("Cor: ",round(corTemp,4),sep=""),asp=1)
      }
      
    }
  }
  
  if(plotType=="Full"){
    par(mfrow=c(length(scaledFull)-1,length(scaledFull)-1))
    for(i in 1:(length(scaledFull)-1)){
      replicate(i-1,plot.new())
      for(j in (i+1):length(scaledFull)){
        corTemp=cor(scaledFull[[j]],scaledFull[[i]],use="complete.obs")
        plot(scaledFull[[j]],scaledFull[[i]],xlab=paste(names(scaledFull)[j]," (Full)",sep=""),ylab=paste(names(scaledFull)[i]," (Full)",sep=""),main=paste("Cor: ",round(corTemp,4),sep=""),asp=1)
      }
      
    }
  }
  if(plotType=="Comparison"){
    ###Code for CV vs Full Scores Plot
    low=floor(sqrt(length(scaledCVScores)))
    print(low)
    high=ceiling(sqrt(length(scaledCVScores)))
    print(high)
    par(mfrow=c(high,high))
    
    for(i in 1:length(scaledCVScores)){
      corTemp=cor(scaledCVScores[[i]],scaledFull[[i]],use="complete.obs")
      plot(scaledCVScores[[i]],scaledFull[[i]],xlab=paste(names(scaledCVScores)[i]," (CV)",sep=""),ylab=paste(names(scaledCVScores)[i]," (Full)",sep=""),main=paste("Cor:  ",round(corTemp,4),sep=""),asp=1)
    }
  }
}


