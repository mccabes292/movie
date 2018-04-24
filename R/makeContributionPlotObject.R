#' Create Contribution Plot Object
#' 
#' To standardize and adjust contributions from a cross validation multi-omics analysis where the contributions need to be standardized and weighted to have similar results with the full analysis.
#' @param fullContributions List of unscaled "score vectors" for the analysis using all subjects.  The length of this list should be equivalent to the number of matrices that were used in the analysis and the length of each element should be n. 
#' @param cvContributions List of unscaled "score vectors" for the cross validation analysis.  The length of this list should be equivalent to the number of matrices that were used in the analysis and the length of each element should be n.
#' @param foldMem Vector of length n indicating in which fold the subjects had the scores calculated
#' @param scaleType Value of the type of scaling desired.  If the contributions are not clustered together, then "MAD" should be used instead of the default "SD".  See vignette for more details. \cr
#' "SD" - Standard Deviation \cr
#' "MAD" - Median Absolute Deviation#' 
#' @return An object which contains standardized scores for both the full and CV analysis as well as the fold membership
#' @examples 
#' makecontributionPlotObject(fullContributions,cvContributions,foldMem)
makeContributionPlotObject=function(fullContributions,cvContributions,foldMem,scaleType="SD"){
  if(length(fullContributions)!=length(cvContributions)){stop("Length of Full and CV Scores are not equal")}
  if(is.null(names(fullContributions))){stop("fullContributions does not have a label")}
  if(is.null(names(cvContributions))){stop("cvContributions does not have a label")}
  if(!(scaleType%in%c("SD","MAD"))){
    stop("Scaling type not 'SD' or 'MAD'")
  }
  ##Scale scores by folds
  scaledcvContributions=NULL
  scaledFull=NULL
  for(i in 1:length(cvContributions)){  
    d1=(data.frame("contribution"=cvContributions[[i]],"fullContributions"=fullContributions[[i]],foldMem))
    names(d1)=c("contribution","fullContributions","foldMem")
    scaledcvContributions[[i]]=(d1 %>% dplyr::group_by(foldMem) %>% dplyr::mutate(scaled=scaleFunction(contribution,fullContributions,scaleType=scaleType)))$scaled
    scaledFull[[i]]=scaleFunction(fullContributions[[i]],fullContributions[[i]],scaleType=scaleType)
  }
  names(scaledcvContributions)=names(cvContributions) 
  names(scaledFull)=names(fullContributions)
  objList=list("fullContributions"=scaledFull,"cvContributions"=scaledcvContributions,"membership"=foldMem)
  class(objList)<-"contributionPlotObject"
  return(objList)
}