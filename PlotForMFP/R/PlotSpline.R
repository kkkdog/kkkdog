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


PlotSpline<-function(data,x,y,alpha=0.05,cv=TRUE,span=0.75,B=100,m=200,method="loess",point=TRUE,shadow=TRUE,tick=TRUE,
                     window.p=list(),rect.p=list(),title.p=list(),polygon.p=list(),line.p=list(),abline.p=list(),point.p=list(),
                     tick.p=list()){


  if("PlotForMPF" %in% names(data)){data=data}else{data=FitSpline(data,x,y,alpha=0.05,cv=TRUE,span=0.75,B=100,m=200,method="loess")}

  xlab=data$LabelX
  ylab=data$LabelY
  xlim=range(data$xin)
  ylim=range(data$yin)


  plot.new()

  window0<-list(xlim=xlim,ylim=ylim)
  window1<-anchors::replace.list(window0,window.p)
  do.call(plot.window,window1)

  axis(1)
  axis(2)
  box()

  ylim<-window1$ylim
  xlim<-window1$xlim
  RectP<-list(xleft=quantile(data$xin,probs=0.025),ybottom=ylim[1], xright=quantile(data$xin,probs=0.975), ytop=ylim[2],col="blue")
  RectP<-anchors::replace.list(RectP,rect.p)
  if(shadow==TRUE){do.call(rect,RectP)}

  titleP<-list(line=2,xlab=xlab,ylab=ylab)
  titleP<-anchors::replace.list(titleP,title.p)
  do.call(title,titleP)

  polygonP<-list(x=c(data$x,rev(data$x)),y=c(data$Lower,rev(data$Upper)),col="red",border=FALSE)
  polygonP<-anchors::replace.list(polygonP,polygon.p)
  do.call(polygon,polygonP)

  lineP<-list(x=data$Mean,y=data$x)
  lineP<-anchors::replace.list(lineP,line.p)
  do.call(lines,lineP)

  ablineP<-list(h=0,lty="dashed")
  ablineP<-anchors::replace.list(ablineP,abline.p)
  do.call(abline,ablineP)

  pointP<-list(x=data$xin,y=data$yin,type="p")
  pointP<-anchors::replace.list(pointP,point.p)
  if(point==TRUE){do.call(points,pointP)}

  if(tick==TRUE){for(i in data$xin){
      tick.length=0.03
      tempx<-c(i,i)
      tempy<-c(ylim[1],ylim[1]+(ylim[2]-ylim[1])*tick.length)
      tempP<-list(x=tempx,y=tempy,lwd=0.3)
      tempP<-anchors::replace.list(tempP,tick.p)
      do.call(lines,tempP)
      }
  }
}
