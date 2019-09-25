#' To male patrial plot for MFP
#'
#' This function returns with a nice patrial plot with confidence interval using the results from RePatrial()
#'
#' @return A patrial plot with confidence interval
#' @param data results obtained from the RePatrial()
#' @param xlim,ylim ranges of x axis and y axis, defaulted as the range of x and y.
#' @param log, which axis needs the log scale, defaulted as "", if "x", the x axis scaled; if "xy", both scaled
#' @param shadow, logical, whether is 95% sample areas should be maked as a shadowed rectangle
#' @param shadow.col the color of the shadow, defaulted as rgb(215,215,215,maxColorValue = 255)
#' @param p.pch character of the points, defaulted as 1
#' @param p.col color of the points, defaulted as "black"
#' @param p.cex size of the points, defaulted as 1
#' @param l.lty type of the line, defaulted as "solid"
#' @param l.col color of the line, defaulted as "black"
#' @param l.cex size of the line, defaulted as 1
#' @param CI.border logical, whether the border should be drawn for the CI, default as NA
#' @param CI.col the color for the CI band, defaulted as "grey"
#' @seealso PlotPatrial()
#' @examples
#' x<-runif(100,0,10)
#' y<-runif(100,0,10)
#' z=log(x)+0.2*y^2 + rnorm(100,0,2)
#'
#' library(mfp)
#' re<-mfp(z~fp(x,4)+fp(y,4))
#' re0<-RePatrial("y",re)
#' PlotPatrial(re0,shadow=FALSE)
#'
#' @export
PlotPatrial<-function(data, xlim=range(data$PatrialData$x), ylim=range(data$PatrialData$pa),log="",
                      shadow=TRUE,shadow.col=rgb(215,215,215,maxColorValue = 255),
                      p.pch=1,p.col="black",p.cex=1,
                      l.lty="solid",l.col="black",l.cex=1,
                      CI.border=NA,CI.col="grey"){

  DataPlot<-data$PatrialData

  if(data$YesOrNO){
    plot.new()
    plot.window(xlim=xlim,ylim=ylim,log=log)
    axis(1)
    axis(2)
    box()
    if(shadow==TRUE){rect(quantile(DataPlot$x,probs=0.025),ylim[1],quantile(DataPlot$x,probs=0.975), ylim[2], col=shadow.col,density=-80,border = NA)}
    title(xlab=data$var,ylab="Patrial Predictor",line=2)
    polygon(c(DataPlot$x,rev(DataPlot$x)),c(DataPlot$pa_p_lower,rev(DataPlot$pa_p_upper)),col=CI.col,border =CI.border)
    lines(pa_p~x,data=DataPlot,lty=l.lty,cex=l.cex,col=l.col)
    points(pa~x,cex=p.cex,data=DataPlot,pch=p.pch,col=p.col)}else{
      print("Patrial plot is not available")
    }
}
