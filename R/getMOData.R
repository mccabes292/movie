#'Load Multi Omics Data
#'
#'Load data for a specified cancer and selected data types
#'
#'@param dataTypes As many as desired from "Clinical","CNV","Gene","miRNA","Module","Mutation","OS","Protein"
#'@param cancerType Desired Cancer type from "BLCA","BRCA","COAD","HNSC","KIRC","LUAD","LUSC"
#'
#'@return Returns a list of data matrices in the order specified by \code{dataTypes} with common subjects having the specified \code{cancerType}
#'
#'
getExampleMOData=function(dataTypes,cancerType){
  dataVec=c("Clinical","CNV","Gene","miRNA","Module","Mutation","OS","Protein")
  cancerVec=c("BLCA","BRCA","COAD","HNSC","KIRC","LUAD","LUSC")
  cancerVec2=c(3,4,5,6,7,8,9)
  if(!all(dataTypes%in%dataVec)  ){
    stop("Invalid Data Types")
  }
  if(!(cancerType%in%cancerVec)  ){
    stop("Invalid Cancer Type")
  }
  if(length(cancerType)!=1  ){
    stop("More than one Cancer type requested")
  }
  
  cancerIndex=cancerVec2[cancerType==cancerVec]
  
  
  splitFunc=function(index){
    t1=unlist(strsplit(as.character(data1$X[index]),"_"))[1]
    return(t1)
  }

  
  
  
  filePath="C:/Sean/UNC/Mike Love/pccca/AlexData/TCGA_8cancer_rmmis_"
  fullData=NULL
  keepIndex=NULL
  for(i in 1:6){
    data1=read.csv(paste(filePath,i,".csv",sep=""))
    t1=sapply(1:nrow(data1),splitFunc)
    if(i==1){
      keepIndex=as.vector(t(data1[cancerIndex,-1]==1))
    }
    data2=data.frame("dataType"=t1[t1%in%dataTypes],data1[t1%in%dataTypes, c(TRUE,keepIndex)])
    fullData=rbind(fullData,data2)
  }
  
  finalData=NULL
  for(i in 1:length(dataTypes)){
    finalData[[i]]=fullData[fullData$dataType==dataTypes[i],-c(1,2) ]
    rownames(finalData[[i]])=(fullData$X)[fullData$dataType==dataTypes[i]]
  }
  names(finalData)=dataTypes
  return(finalData)
  
}