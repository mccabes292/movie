## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=F-----------------------------------------------------------
library(movie)

data(mccares)
fullContribution_MCCA=list("CNV"=fullContribution_CNV_MCCA,"RNA"=fullContribution_RNA_MCCA,"miRNA"=fullContribution_miRNA_MCCA)
cvContribution_MCCA=list("CNV"=cvContribution_CNV_MCCA,"RNA"=cvContribution_RNA_MCCA,"miRNA"=cvContribution_miRNA_MCCA)

## ------------------------------------------------------------------------
movieOb=makeMovieObject(fullContributions =fullContribution_MCCA,cvContributions = cvContribution_MCCA,foldMem = foldMem)

## ------------------------------------------------------------------------
plot(movieOb,plotType = "SideBySide",xAxisPlot=1,yAxisPlot=2,colorVar = as.factor(foldMem),grid=FALSE,colorVarLabel="Fold")



## ------------------------------------------------------------------------
plot(movieOb,plotType = "Comparison",grid=TRUE)

plot(movieOb,plotType = "Comparison",xAxisPlot = 1,yAxisPlot = 2,grid=FALSE,colorVar=as.factor(foldMem),colorVarLabel="Fold")

## ------------------------------------------------------------------------
plot(movieOb,plotType = "Full",grid=TRUE)

plot(movieOb,plotType = "Full",xAxisPlot = 1,yAxisPlot = 2,grid=FALSE,colorVar=as.factor(foldMem),colorVarLabel="Fold")

## ------------------------------------------------------------------------
plot(movieOb,plotType = "CV",grid=TRUE)

plot(movieOb,plotType = "CV",xAxisPlot = 1,yAxisPlot = 2,grid=FALSE,colorVar=as.factor(foldMem),colorVarLabel="Fold")

