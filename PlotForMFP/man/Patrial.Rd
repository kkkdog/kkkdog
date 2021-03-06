% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Patrial.R
\name{Patrial}
\alias{Patrial}
\title{To get partial predictor for MFP}
\usage{
Patrial(data)
}
\arguments{
\item{data, }{results obtained from mfp function (see mfp packages)}

\item{var, }{the variable we need to obtain patrial predictor and make patrial plot}
}
\value{
A smoothed plot with confidence interval
}
\description{
This function returns partial predictors with 95% confidence interval. The returned result will be adopted to PlotPatrial function
}
\examples{

x<-runif(100,0,10)
y<-runif(100,0,10)
z=log(x)+0.2*y^2 + rnorm(100,0,2)

library(mfp)
re<-mfp(z~fp(x,4)+fp(y,4))
re0<-Patrial("y",re)
PlotPatrial(re0)
PlotPatrial(re,xlab=list(x="X in R"))
PlotPatrial(re,"x")

}
\seealso{
PlotPatrial()
}
