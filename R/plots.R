#' Make Contribution Plot
#' 
#' Function utilizing plot which makes a contribution plot
#' 
#' @param x Object of class `movie`
#' @param plotType Specifies the type of plot to be generated
#' \code{"CV"} - CV Analysis Contribution Plot \cr\cr
#' \code{"Full"} - Full Analysis Contribution Plot \cr\cr
#' \code{"Comparison"} - Matrix Specific Score Plots \cr\cr
#' \code{"SideBySide"} - Side by side plot of the CV and Full Plots - Not available if \code{Grid=T}.
#' \code{"Line"} - Plots correlations in Full and CV analysis for all pairwise datatype comparisons. 
#' @param xAxisPlot Index for which matrix should lie on the x-axis.  This is only used if \code{Grid=F}.
#' @param yAxisPlot Index for which matrix should lie on the y-axis.  This is only used if \code{Grid=F}.
#' @param grid  T or F corresponding if you want a grid of plots or an individual plot.
#' @param colorVar Variable by which points can be colored by. If NA then no color will be given. This is only used if \code{Grid=F}
#' @param colorVarLabel String to label the legend.
#' @param leg T or F designating whether or legend will be displayed for the "Line" plot
#' @param ... other parameters to be passed through to plotting functions.
#' @export

plot.movie=function(x,plotType,xAxisPlot,yAxisPlot,colorVar=NULL,grid,colorVarLabel=NULL,leg=TRUE,...){
  movieObject=x
  
  if((plotType=="SideBySide")&(missing(grid)) ){
    grid=FALSE
  }
  if((plotType=="Comparison")&(missing(yAxisPlot)) ){
   yAxisPlot=1 
  }
  if(plotType=="Line"){
    plotLinePlot(movieObject,leg=leg)
  }else if(grid==TRUE){
    plotMoviePlotGrid(movieObject,plotType)  
  }else{
    plotMoviePlots(movieObject,plotType,xAxisPlot,yAxisPlot,colorVar,colorVarLabel,leg=leg)
  }
}
