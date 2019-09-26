#' To male patrial plot for MFP
#'
#' This function returns with a nice patrial plot with confidence interval using the results from RePatrial()
#'
#' @return A patrial plot with confidence interval
#' @param data results obtained from the RePatrial()
#' @param xlim,ylim a list, defaulted as the range of x and y. to set them, using ylim=list(var1=c(1,10),...), where var1 should be the variables in the final function
#' @param log, a list, which axis needs the log scale, defaulted as "", if "x", the x axis scaled; if "xy", both scaled. To set it, using log=list(var1="xy",var2="x",...)
#' @param xlab,ylab, a list, the labels of axis. To set it, using xlab=list(var1="name1",var2="name2",...)
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
#'
#' x<-runif(100,0,10)
#' y<-runif(100,0,10)
#' z=log(x)+0.2*y^2 + rnorm(100,0,2)
#'
#' library(mfp)
#' re<-mfp(z~fp(x,4)+fp(y,4))
#' re0<-RePatrial("y",re)
#' PlotPatrial(re0)
#' PlotPatrial(re,xlab=list(x="X in R"))
#' PlotPatrial(re,"x")
#'
#' @export
PlotPatrial<-function(data,var="",xlim=data$xlim, ylim=data$ylim,xlab=data$xlab,ylab=data$ylab,log=data$log,
                      shadow=FALSE,shadow.col=rgb(215,215,215,maxColorValue = 255),
                      p.pch=1,p.col="black",p.cex=1,
                      l.lty="solid",l.col="black",l.cex=1,
                      CI.border=NA,CI.col="grey"){

if("PlotOrNot" %in% names(data)){data=data}else{data=RePatrial(data)}
if(var==""){varALL=data$ConVar}else{varALL=var}

if(length(xlim)==length(data$xlim)){xlim=data$xlim}else{xlim=anchors::replace.list(data$xlim,xlim)}
if(length(ylim)==length(data$ylim)){ylim=data$ylim}else{ylim=anchors::replace.list(data$ylim,ylim)}
if(length(xlab)==length(data$xlab)){xlab=data$xlab}else{xlab=anchors::replace.list(data$xlab,xlab)}
if(length(ylab)==length(data$ylab)){ylab=data$ylab}else{ylab=anchors::replace.list(data$ylab,ylab)}
if(length(log)!=length(data$log)){log=data$log}else{log=anchors::replace.list(data$log,log)}

for(iVar in varALL){
DataPlot<-data$PatrialData[[iVar]]
plot.new()
plot.window(xlim=xlim[[iVar]],ylim=ylim[[iVar]],log=log[[iVar]])
axis(1)
axis(2)
box()
if(shadow==TRUE){rect(quantile(DataPlot$x,probs=0.025),ylim[1],quantile(DataPlot$x,probs=0.975), ylim[2], col=shadow.col,density=-80,border = NA)}
title(xlab=xlab[[iVar]],ylab=ylab[[iVar]],line=2)
polygon(c(DataPlot$x,rev(DataPlot$x)),c(DataPlot$pa_p_lower,rev(DataPlot$pa_p_upper)),col=CI.col,border =CI.border)
lines(pa_p~x,data=DataPlot,lty=l.lty,cex=l.cex,col=l.col)
points(pa~x,cex=p.cex,data=DataPlot,pch=p.pch,col=p.col)}
}





