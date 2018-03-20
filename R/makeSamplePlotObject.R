#' To standardize and adjust "scores" from a cross validation multi-omics analysis where the "scores" need to be standardized and weighted to have similar results with the full analysis.
#' @param fullScores List of unscaled "score vectors" for the analysis using all subjects.  The length of this list should be equivalent to the number of matrices that were used in the analysis and the length of each element should be n. 
#' @param cvScores List of unscaled "score vectors" for the cross validation analysis.  The length of this list should be equivalent to the number of matrices that were used in the analysis and the length of each element should be n.
#' @param idMem Vector of length n indicating in which fold the subjects had the scores calculated
#' @return An object which contains standardized scores for both the full and CV analysis as well as the fold membership
#' @examples 
#' makeSamplePlotObject(fullScores,cvScores,idMem)
makeSamplePlotObject=function(fullScores,cvScores,idMem){
  if(length(fullScores)!=length(cvScores)){stop("Length of Full and CV Scores are not equal")}
  ##Scale scores by folds
  scaledCVScores=NULL
  scaledFull=NULL
  for(i in 1:length(cvScores)){  
    scaledCVScores[[i]]=(data.frame("score"=cvScores[[i]],"fullScores"=fullScores[[i]],idMem) %>% dplyr::group_by(idMem) %>% dplyr::mutate(scaled=scaleFunction(score,fullScores)))$scaled
    scaledFull[[i]]=scaleFunction(fullScores[[i]],fullScores[[i]])
  }
  objList=list("fullScores"=scaledFull,"cvScores"=scaledCVScores,"membership"=idMem)
  class(objList)<-"samplePlotObject"
  return(objList)
}