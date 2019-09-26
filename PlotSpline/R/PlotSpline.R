#' To make a spline plot
#'
#' This function returns a spline plot with confidence interval. This function is based on the basic plot, so a lot of parameters
#' can be changed. Be aware that not all elements are needed. The priciple of graphic is as simple as possible.
#'
#' @inheritParams PlotSpline
#' @return A smoothed plot with confidence interval
#' @param SpList list from FiitModel
#' @param xlab,ylab the label of axis.
#' @param log which axis need a log scale, e.g. "x","y","xy"
#' @param points logical, should be points be plotted, default as FALSE
#' @param col.CI colour of confidence interval,default as "royalblue4"
#' @param xlim,ylim limits of axis, default as the range of y
#' @param shadow logical, should be shadow be plotted?, default as TRUE
#' @param col.shadow color of the shadow
#' @param tick logical, should be ticks be plotted? default as TRUE
#' @param tick.length the length of ticks,defaults as 0.03.
#' @seealso
#' @export
#' @examples
#' x<-rnorm(100,5,2)
#' y=x^2+rnorm(100,0,5)-50
#' data<-data.frame(x=x,y=y)
#' temp<-FitModel(data,x="x",y="y")
#' PlotSpline(temp,col.CI=rgb(70,130,180,maxColorValue = 255),xlab="X",ylab="y")

PlotSpline<-function(SpList,cex=1,xlab=SpList$LabelX,ylab=SpList$LabelY,log="",points=TRUE,col.CI="royalblue4",xlim=range(SpList$xin),
                     ylim=range(SpList$yin),col.shadow=rgb(215,215,215,maxColorValue = 255),shadow=FALSE,tick.length=0.03,tick=TRUE){
  plot.new()
  plot.window(xlim=xlim,ylim=ylim,log=log)
  axis(1)
  axis(2)
  box()
  if(shadow==TRUE){rect(quantile(SpList$xin,probs=0.025),ylim[1], quantile(SpList$xin,probs=0.975), ylim[2], col=col.shadow,density=-80,border = NA)}
  title(xlab=xlab,ylab=ylab,line=2)
  polygon(c(SpList$x,rev(SpList$x)),c(SpList$Lower,rev(SpList$Upper)),col=col.CI,border=FALSE)
  lines(SpList$Mean~SpList$x)
  abline(h=0,lty="dashed")
  if(points==TRUE){points(SpList$yin~SpList$xin,cex=cex)}
  if(tick==TRUE){
    for(i in SpList$xin){
      tempx<-c(i,i)
      tempy<-c(ylim[1],ylim[1]+(ylim[2]-ylim[1])*tick.length)
      lines(tempy~tempx,lwd=0.3)
    }
  }
}
