#' To Plot residuals for MPF model or general linear model
#'
#' This function returns a nice residual plot with confidence interval. The only input data could be results from mfp() or lm(), and a variable.
#'
#' @inheritParams PlotSpline
#' @return A smoothed plot with confidence interval
#' @param Data data from mfp() or lm(), or FitSpline()
#' @param x the variable to make residual plot
#' @param xlab,ylab the label of axis.
#' @param log which axis need a log scale, e.g. "x","y","xy"
#' @param points logical, should be points be plotted, default as FALSE
#' @param col.CI colour of confidence interval,default as "royalblue4"
#' @param xlim,ylim limits of axis, default as the range of y
#' @param shadow logical, should be shadow be plotted?, default as TRUE
#' @param col.shadow color of the shadow
#' @param tick logical, should be ticks be plotted? default as TRUE
#' @param tick.length the length of ticks,defaults as 0.03.
#' @seealso FitSpline(),PlotSpline()
#' @export
#' @examples
#' x<-rnorm(100,5,2)
#' y=x^2+rnorm(100,0,5)-50
#' data<-data.frame(x=x,y=y)
#' temp<-FitModel(data,x="x",y="y")
#' PlotSpline(temp,col.CI=rgb(70,130,180,maxColorValue = 255),xlab="X",ylab="y")

PlotResiduals<-function(data,x,alpha=0.05,cv=TRUE,span=0.75,B=100,m=200,method="loess",cex=1,xlab="",ylab="",log="",points=FALSE,col.CI="royalblue4",xlim="",
                     ylim="",col.shadow=rgb(215,215,215,maxColorValue = 255),shadow=TRUE,tick.length=0.03,tick=FALSE){

  if("PlotForMPF" %in% names(data)){data=data}else if("dev.lin" %in% names(data)){
    data=data.frame(tempx=data$X[,x],
                    tempy=data$residuals)
     colnames(data)<-c(x,"Residuals")
     data=FitSpline(data=data,x=x,y="Residuals",alpha=alpha,cv=cv,span=span,B=B,m=m,method=method)}else{
       data=data.frame(tempx=data$model[,x],
                       tempy=data$residuals)
       colnames(data)<-c(x,"Residuals")
     data=FitSpline(data=data,x=x,y="Residuals",alpha=alpha,cv=cv,span=span,B=B,m=m,method=method)}
 
  if(xlab==""){xlab=data$LabelX}
  if(ylab==""){ylab=data$LabelY}
  if(xlim==""){xlim=range(data$xin)}
  if(ylim==""){ylim=range(data$yin)}
  
  plot.new()
  plot.window(xlim=xlim,ylim=ylim,log=log)
  axis(1)
  axis(2)
  box()
  if(shadow==TRUE){rect(quantile(data$xin,probs=0.025),ylim[1], quantile(data$xin,probs=0.975), ylim[2], col=col.shadow,density=-80,border = NA)}
  title(xlab=xlab,ylab=ylab,line=2)
  polygon(c(data$x,rev(data$x)),c(data$Lower,rev(data$Upper)),col=col.CI,border=FALSE)
  lines(data$Mean~data$x)
  abline(h=0,lty="dashed")
  if(points==TRUE){points(data$yin~data$xin,cex=cex)}
  if(tick==TRUE){
    for(i in data$xin){
      tempx<-c(i,i)
      tempy<-c(ylim[1],ylim[1]+(ylim[2]-ylim[1])*tick.length)
      lines(tempy~tempx,lwd=0.3)
    }
  }
}
