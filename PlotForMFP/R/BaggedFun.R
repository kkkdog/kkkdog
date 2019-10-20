resampler<-function(data){
  n<-nrow(data)
  RowSample<-sample(n,n,replace=TRUE)
  return(data[RowSample,])
}

FitInBoot<-function(data,formula,B,select=select,alpha=alpha,family=family){
rpk<-list()

Return<-list()
for (i in 1:B){
Udata<-resampler(data)
k<-mfp::mfp(formula=formula,data=Udata,select=select,alpha=alpha,family=family)
pk<-PlotForMFP::Patrial(k)$PatrialData

for(i in names(pk)){
rpk[[i]]<-c(rpk[[i]],pk[[i]]$pa)
}}

for(j in names(rpk)){
  rpk[[j]]<-matrix(rpk[[j]],nrow=nrow(data))
}

k<-mfp::mfp(formula=formula,data=data,select=select,alpha=alpha,family=family)
pk<-PlotForMFP::Patrial(k)$PatrialData
Return[["ref"]]<-pk
Return[["fit"]]<-rpk
return(Return)
}


#' To evaluate the instability of the curve
#'
#' This function return a list contains the bagged fitted values, variance, conditional variance, and deviation.
#'
#' @inheritParams CurveInstability
#' @return a list contains the bagged fitted values, variance, conditional variance, and deviation.
#' @param data data set
#' @param formula, get from as.formula()
#' @param B bootstrap times
#' @param select,alpha same as mpf funciton,default=0.05
#' @param family default as gaussian, alternatively, cox
#' @seealso mfp() Patrial()
#' @examples
#' library(mfp)
#' formula<-as.formula(y ~ fp(x1,df=4) + fp(x3,df=4) + fp(x5,df=4) + fp(x6,df=4) + fp(x7,df=4) + fp(x10,df=4)
#' +x9+ x2 +x4a + x4b + x8)
#' BaggedFun(data=data,B=10,formula=formula)
#'
#' @export
BaggedFun<-function(data,formula,B,select=0.05,alpha=0.05,family=gaussian){
  re<-FitInBoot(data=data,formula=formula,B=B,select=select,alpha=alpha,family=family)
  out<-list()
  for( i in names(re$fit)){
    R<-ncol(re$fit[[i]])
    fit<-as.data.frame(re$fit[[i]])
    out$bag[[i]]<-apply(fit,1,function(x) sum(x)/B)
    fun_temp<-function(x){(x-sum(x)/B)^2+(B-R)*(sum(x)/B)^2}
    out$V[[i]]<-sum(apply(fit,1,fun_temp))/B
    out$D[[i]]<-sum((out$bag[[i]]-re$ref[[i]]$pa)^2)
    q<-R/B
    out$VconM[[i]]<-sum(apply(fit,1,function(x) (x-1/q*(sum(x)/B))^2)) / B
    temp<-c(names(out[["R"]]),i)
    out[["R"]]<-c(out[["R"]],R)
    names(out[["R"]])<-temp
   }
  return(out)
}





