#' Make Contribution Plot
#' 
#' Function utilizing plot which makes a contribution plot
#' 
#' @param sampleObject Object of class samplePlotObject
#' @param plotNum Number corresponding to which plot to make

plot.samplePlotObject=function(sampleObject,plotNum,xAxisPlot,yAxisPlot,colorVar=NULL,grid){
  if(grid==T){
    plotSamplePlotGrid(sampleObject,plotNum,colorVar)  
  }else{
    plotSamplePlots(sampleObject,plotNum,xAxisPlot,yAxisPlot,colorVar)
  }
}