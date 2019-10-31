
plotMoviePlotGrid=function(movieObject,plotType){
 
  if(length(movieObject)!=3){
    stop("Invalid Movie object")
  }
  if(!(plotType %in% c("CV","Full","Comparison"))){
    stop("Invalid plot type")
  }
  scaledFull=movieObject[[1]]
  scaledCV=movieObject[[2]]
  idMem=movieObject[[3]]
  if(plotType=="CV"){
    ###Code for CV contribution plots
    par(mfrow=c(length(scaledCV)-1,length(scaledCV)-1))
    for(i in 1:(length(scaledCV)-1)){
      replicate(i-1,plot.new())
      for(j in (i+1):length(scaledCV)){
        corTemp=cor(scaledCV[[j]],scaledCV[[i]],use="complete.obs")
        plot(scaledCV[[j]],scaledCV[[i]],xlab=paste(names(scaledCV)[j]," (CV)",sep=""),bty="L",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,ylab=paste(names(scaledCV)[i]," (CV)",sep=""),main=paste("Cor: ",round(corTemp,4),sep=""),asp=1)
      }
      
    }
  }
  
  if(plotType=="Full"){
    par(mfrow=c(length(scaledFull)-1,length(scaledFull)-1))
    for(i in 1:(length(scaledFull)-1)){
      replicate(i-1,plot.new())
      for(j in (i+1):length(scaledFull)){
        corTemp=cor(scaledFull[[j]],scaledFull[[i]],use="complete.obs")
        plot(scaledFull[[j]],scaledFull[[i]],xlab=paste(names(scaledFull)[j]," (Full)",sep=""),bty="L",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,ylab=paste(names(scaledFull)[i]," (Full)",sep=""),main=paste("Cor: ",round(corTemp,4),sep=""),asp=1)
      }
      
    }
  }
  if(plotType=="Comparison"){
    ###Code for CV vs Full Scores Plot
    low=round(sqrt(length(scaledCV)))
    high=ceiling(sqrt(length(scaledCV)))
    par(mfrow=c(low,high))
    
    for(i in 1:length(scaledCV)){
      corTemp=cor(scaledCV[[i]],scaledFull[[i]],use="complete.obs")
      plot(scaledCV[[i]],scaledFull[[i]],xlab=paste(names(scaledCV)[i]," (CV)",sep=""),bty="L",pch=16,cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,ylab=paste(names(scaledCV)[i]," (Full)",sep=""),main=paste("Cor:  ",round(corTemp,4),sep=""),asp=1)
    }
  }
}


