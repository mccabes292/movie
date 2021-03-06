#' Create MOVIE Object
#' 
#' To scale and flip contributions from a cross validation multi-omics analysis in order to be align with the contributions from the full analysis.
#' @param fullContributions List of unscaled contributions for the analysis using all subjects.  The length of this list should be equivalent to the number of matrices that were used in the analysis and the length of each element within the list should be the number of samples. 
#' @param cvContributions List of unscaled contributions for the cross validation analysis.  The dimensions of this list should be the same as the `fullContributions`.
#' @param foldMem Vector of length equal to the number of samples, indicating in which fold the subjects had their contributions calculated
#' @param scaleType Value of the type of scaling desired.  If the contributions are not clustered together, then "MAD" should be used instead of the default "SD".  See vignette for more details. \cr\cr
#' "SD" - Standard Deviation \cr
#' "MAD" - Median Absolute Deviation#' 
#' @param scaleFold TRUE or FALSE on whether or not scaling should be done within Fold. Default is FALSE
#' @return A `movie` object which contains the standardized contributions for both the full and CV analysis as well as the fold membership which will be used for plotting.
#' @importFrom graphics par plot plot.new
#' @importFrom stats cor mad sd
#' @importFrom utils read.csv
#' @importFrom magrittr %>%
#' @importFrom cowplot plot_grid get_legend
#' @importFrom ggplot2 ggplot aes xlab ylab ylim scale_linetype_manual theme_bw
#'  ggtitle geom_point geom_line coord_fixed labs theme element_text element_blank element_line geom_text guides
#' @importFrom dplyr group_by
#' 
#' @export
makeMovieObject=function(fullContributions,cvContributions,foldMem,scaleType="SD",scaleFold=FALSE){
  if(length(fullContributions)!=length(cvContributions)){stop("Length of Full and CV Scores are not equal")}
  if(is.null(names(fullContributions))){stop("fullContributions does not have a label")}
  if(is.null(names(cvContributions))){stop("cvContributions does not have a label")}
  if(!(scaleType%in%c("SD","MAD"))){
    stop("Scaling type not 'SD'or 'MAD'")
  }
  ##Scale scores by folds
  scaledcvContributions=NULL
  scaledFull=NULL
  for(i in 1:length(cvContributions)){  
    d1=(data.frame("contribution"=cvContributions[[i]],"fullContributions"=fullContributions[[i]],"foldMem"=foldMem))
    names(d1)=c("contribution","fullContributions","foldMem")
    scaledcvContributions[[i]]=(d1 %>% dplyr::group_by(foldMem) %>% dplyr::mutate(scaled=scaleFunction(contribution,fullContributions,scaleType=scaleType,scaleFold=scaleFold)))$scaled
    if(scaleFold==FALSE){
      
      scaledcvContributions[[i]]=scaleFunction(scaledcvContributions[[i]],fullContributions[[i]],scaleType=scaleType)
    }
    scaledFull[[i]]=scaleFunction(fullContributions[[i]],fullContributions[[i]],scaleType=scaleType)
  }
  names(scaledcvContributions)=names(cvContributions) 
  names(scaledFull)=names(fullContributions)
  objList=list("fullContributions"=scaledFull,"cvContributions"=scaledcvContributions,"membership"=foldMem)
  class(objList)<-"movie"
  return(objList)
}