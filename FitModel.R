resampler<-function(data,x,y){
  n<-nrow(data)
  RowSample<-sample(n,n,replace=TRUE)
  return(data[RowSample,c(x,y)])
}

SplineEstimator<-function(data,X,x,y){
  reTemp<-smooth.spline(data[,x],data[,y],cv=TRUE)
  PreTemp<-predict(reTemp,X)
  return(PreTemp$y)
}

BootCI<-function(data,x,y,B,m,alpha){
  PreX<-seq(from=min(data[,x]),to=max(data[,x]),length.out = m)
  MeanSpline<-SplineEstimator(data,X=PreX,x,y)
  ReBoot<-replicate(B,SplineEstimator(resampler(data,x,y),X=PreX,x,y))  ### logical error
  Upper<-apply(ReBoot,1,quantile,probs=1-alpha/2)
  Lower<-apply(ReBoot,1,quantile,probs=alpha/2)
  return(list(Mean=MeanSpline,Lower=Lower,Upper=Upper,x=PreX,xin=data[,x],yin=data[,y],LabelX=x,LabelY=y))
}

####CI with functional form
FunCI<-function(data,x,y,alpha,cv){
  re<-smooth.spline(x=data[,x],y=data[,y],cv=TRUE)
  res<-(re$y-re$yin)/(1-re$lev)
  sigma<-sqrt(var(res))
  Upper<-re$y + qnorm(1-alpha/2)*sigma*sqrt(re$lev)
  Lower<-re$y - qnorm(1-alpha/2)*sigma*sqrt(re$lev)
  return(list(Mean=re$y,Lower=Lower,Upper=Upper,x=re$x,xin=data[,x],yin=data[,y],LabelX=x,LabelY=y))
}

LoessCI<-function(data,x,y,alpha,span){
  re<-predict(loess(formula(paste0(y,"~",x)),data=data,span = span),se=TRUE)
  Upper<-re$fit + qnorm(1-alpha/2)*re$se.fit
  Lower<-re$fit - qnorm(1-alpha/2)*re$se.fit
  order=order(data[,x],decreasing = FALSE)
  return(list(Mean=re$fit[order],Lower=Lower[order],Upper=Upper[order],x=data[order,x],xin=data[order,x],yin=data[order,y],LabelX=x,LabelY=y))
}

#' Fiited the splines.
#'
#' This function fits the spline using three methods, namely, weighted,  boostrap, and loess. Actually, the first two methods are the same
#' cubic splines. The difference is that weighted method estimates confidence interval using fuctional form, while the boostrap method estimates
#' confidence interval using bootstrap method. FitModel returns a list contains initial data, fitted values, and confidence intervals, which can
#' be applied to make spline plot, see PlotSpline().
#'
#' @return A list contains initial data, fitted values, 95% confidence intervals
#' @param data input data,default as a data.frame
#' @param x,y  variable and reponse
#' @param alpha  range of confidence intervals, default as 0.05
#' @param span  special for loess method, values to be used.
#' @param B  times of bootstrap, default as 100
#' @param m  special for loess method, the number of predicted values to draw the plot
#' @param method default as loess, weighted and bootstrap are options.
#' @seealso PlotSpline
#' @examples
#' x<-rnorm(100,5,2)
#' y=x^2+rnorm(100,0,5)-50
#' data<-data.frame(x=x,y=y)
#' temp<-FitModel(data,x="x",y="y")
#' PlotSpline(temp,col.CI=rgb(70,130,180,maxColorValue = 255),xlab="X",ylab="y")
#' @export
FitModel<-function(data,x,y,alpha=0.05,cv=TRUE,span=0.75,B=100,m=200,method="loess"){
  if(method=="loess"){return(LoessCI(data,x,y,alpha,span))}else
    if(method=="weighted"){
      return(FunCI(data,x,y,alpha,cv))
    }else
      if(method=="bootstrap"){
        return(BootCI(data,x,y,B,m,alpha))
      }
}
