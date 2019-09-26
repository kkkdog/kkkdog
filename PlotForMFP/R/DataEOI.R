#' To obtain hat values, studenlized residual, and Cook distance
#'
#' This function returns a data.frame contains hat values, Studentized Residuals,  Cook distance, and sample number
#'
#' @inheritParams DataEOI
#' @return A dataframe
#' @param data results from lm() or mfp()
#' @param SampleName which variable in the original data is considered to be sample labels, defaulted as the row name of the data
#' @seealso
#' @export
#' @examples
#'
#' library(mfp)
#' library(PlotForMFP)
#' re<-mfp(y ~ fp(x1,df=4) + fp(x3,df=4) + fp(x5,df=4) + fp(x6,df=4) + fp(x7,df=4) + fp(x10,df=4)
#'         + x9  + x2 +x4a  + x4b + x8, data=DataMFP,select = 0.05,rescale = FALSE)
#' View(DataEOI(re))
#' PlotEOI(re)
#'
#'

DataEOI<-function(data,SampleName=""){
  CookD<-cooks.distance(data)
  LeValue<-hatvalues(data)
  RS<-rstudent(data)
  if("dev.lin" %in% names(data)){data=data$fit$data}else{data=data$model}
  if(SampleName==""){SampleName=rownames(data)}else{SampleName=data[,"SampleName"]}
  EOI<-data.frame(CookD=CookD,Leverage=LeValue,RStudent=RS,SampleName=SampleName)
  return(EOI)
}
