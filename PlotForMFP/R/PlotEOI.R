#' To get a plot to reflect the hat values, Studentized Residuals,  Cook distance
#'
#' EOI is short for extreme values, outliers, and influencial values. This function returns a nice plot contains these information
#'
#' @inheritParams PlotEOI
#' @return A plot
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
PlotEOI<-function(data,SampleName="",ylim=c(),xlim=c(),CI.border=NA,CI.color="grey",
                  CutOff.Lev="",Lev.lty="dashed",Lev.col="black",Lev.cex=1,
                  CutOff.RS=3,RS.lty="dashed",RS.col="black",RS.cex=1,
                  CutOff.CookD="",
                  CookD.col="red"){

  DataPlot<-DataEOI(data,SampleName=SampleName)

  if(length(ylim)==0){ylim=range(DataPlot$RStudent)}else{ylim=ylim}
  if(length(xlim)==0){xlim=range(DataPlot$Leverage)}else{xlim=xlim}
  if(CutOff.CookD==""){CutOff.CookD=3*mean(DataPlot$CookD)}else{CutOff.CookD=CutOff.CookD}
  if(CutOff.Lev==""){CutOff.Lev=3*(length(coef(data))/length(data$residuals))}else{CutOff.Lev=CutOff.Lev}

  plot.new()
  plot.window(xlim=xlim,ylim=ylim)
  axis(1)
  axis(2)
  box()
  points(RStudent~Leverage,data=DataPlot,ylim=ylim,xlim=xlim)
  title(xlab="Hat values",ylab="Studentized Residuals",line=2,cex.main = 2,cex.sub = 0.75)
  ##add smooth spline with interval
  temp<-predict(loess(RStudent~Leverage, data=DataPlot), se=T)
  temp<-data.frame(Leverage=DataPlot$Leverage,fit=temp$fit,Upper=temp$fit+2*temp$se.fit,Lower=temp$fit-2*temp$se.fit)
  temp<-temp[order(temp$Leverage),]
  polygon(c(temp$Leverage,rev(temp$Leverage)),c(temp$Upper,rev(temp$Lower)),col=CI.color,border = CI.border)
  lines(temp$Leverage,temp$fit,col="black")
  points(RStudent~Leverage,data=DataPlot)

  #temp<-smooth.spline(EsQua$LeValue,EsQua$RS,cv=TRUE)

  #lines(temp)
  abline(h=0,lty="dashed")
  abline(v=CutOff.Lev,lty=Lev.lty,col=Lev.col,cex=Lev.cex)
  abline(h=CutOff.RS,lty=RS.lty,col=RS.col,cex=RS.cex)
  abline(h=-CutOff.RS,lty=RS.lty,col=RS.col,cex=RS.cex)
  mtext(text=CutOff.Lev,side=1,line=1,at=c(CutOff.Lev),cex=0.5)
  mtext(text=CutOff.RS,side=2,line=1,at=c(CutOff.RS),cex=0.5)
  mtext(text=paste0("-",CutOff.RS),side=2,line=1,at=c(-CutOff.RS),cex=0.5)

  #large hat values
  if(any(DataPlot$Leverage>CutOff.Lev)){
  points(RStudent~Leverage,data=DataPlot[DataPlot$Leverage>CutOff.Lev,])
  text(RStudent~Leverage,labels=SampleName,data=DataPlot[DataPlot$Leverage>CutOff.Lev,],pos=4,cex=0.7,offset=0.3)}
  #large studentized residuals
  if(any(abs(DataPlot$RStudent)>CutOff.RS)){
  points(RStudent~Leverage,data=DataPlot[abs(DataPlot$RStudent)>CutOff.RS,])
  text(RStudent~Leverage,labels=SampleName,data=DataPlot[abs(DataPlot$RStudent)>CutOff.RS,],pos=4,cex=0.7,offset=0.3)}
  ##cooke's distance
  if(any(DataPlot$CookD>CutOff.CookD)){
  points(RStudent~Leverage,data=DataPlot[which(DataPlot$CookD>CutOff.CookD),],col="red")
  text(RStudent~Leverage,labels=SampleName,data=DataPlot[which(DataPlot$CookD>CutOff.CookD),],cex=0.7,pos=4,offset=0.3)
  }
  ##
  legend("topright",inset=0.03,legend=c("Influential observations"),pch=1,col=c("red"),cex=1,bg="white")
}
