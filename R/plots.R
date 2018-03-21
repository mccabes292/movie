#' Make Contribution Plot
#' 
#' Function utilizing plot which makes a contribution plot
#' 
#' @param sampleObject Object of class samplePlotObject
#' @param plotType Specifies the type of plot to be generated
#' \code{"CV"} - CV Sample/Score Plot - (k-1) X (k-1) upper triangular grid where $plot_{i,j}$ Corresponds to the sample/score plot for matrix j+1 (x-axis) and i (y-axis)\cr\cr
#' \code{"Full"} - Full Sample/Score Plot - Same gridded plot as above but now for the full scores i.e. the scores from the full analysis\cr\cr
#' \code{"Comparison"} - Matrix Specific Score Plots - k plots (k is the number of matrices in the analysis) which plot the scores for the CV analysis vs. the scores for the full analysis\cr\cr
#' \code{"SideBySide"} - Side by side plot of the CV and Full Plots
#'
#' @param xAxisPlot Index for which matrix should lie on the x-axis.  This is only used if \code{Grid=F}
#' @param yAxisPlot Index for which matrix should lie on the y-axis.  This is only used if \code{Grid=F}
#' @param colorVar Variable by which points can be colored by. If NA then no color will be given. This is only used if \code{Grid=F}
#'

plot.samplePlotObject=function(sampleObject,plotType,xAxisPlot,yAxisPlot,colorVar=NULL,grid){
  if(grid==T){
    plotSamplePlotGrid(sampleObject,plotType)  
  }else{
    plotSamplePlots(sampleObject,plotType,xAxisPlot,yAxisPlot,colorVar)
  }
}