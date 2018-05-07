#' Make Contribution Plot
#' 
#' Function utilizing plot which makes a contribution plot
#' 
#' @param movieObject Object of class `movie`
#' @param plotType Specifies the type of plot to be generated
#' \code{"CV"} - CV Analysis Contribution Plot \cr\cr
#' \code{"Full"} - Full Analysis Contribution Plot \cr\cr
#' \code{"Comparison"} - Matrix Specific Score Plots \cr\cr
#' \code{"SideBySide"} - Side by side plot of the CV and Full Plots - Not available if \code{Grid=T}.
#'
#' @param xAxisPlot Index for which matrix should lie on the x-axis.  This is only used if \code{Grid=F}.
#' @param yAxisPlot Index for which matrix should lie on the y-axis.  This is only used if \code{Grid=F}.
#' @param grid  T or F corresponding if you want a grid of plots or an individual plot.
#' @param colorVar Variable by which points can be colored by. If NA then no color will be given. This is only used if \code{Grid=F}
#' @param colorVarLabel String to label the legend.

plot.movie=function(movieObject,plotType,xAxisPlot,yAxisPlot,colorVar=NULL,grid,colorVarLabel=NULL){
  if(grid==T){
    plotMoviePlotGrid(movieObject,plotType)  
  }else{
    plotMoviePlots(movieObject,plotType,xAxisPlot,yAxisPlot,colorVar,colorVarLabel)
  }
}