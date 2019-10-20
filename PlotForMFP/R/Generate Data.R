#' To generate matrix, can be applied to GenData
#'
#' This function generate matrix, expecially correlation matrix, which can be applied to GenData function
#'
#' @inheritParams GenMatrix
#' @return A matrix
#' @param n the dimension of matrix
#' @param rlist a list to add values to matrix
#' @param a, default values of the matrix, defaulted as 0
#' @param b, default values of the diag of the matrix, defaulted as 1
#' @seealso GenData
#' @export
#' @examples
#'
#' rlist=list(c(1,5,0.7),c(1,10,0.5),c(2,6,0.5),c(4,8,-0.7),c(7,8,0.3),c(11,12,0.7),c(9,13,0.5),c(7,14,0.5))
#' rmatrix<-GenMatrix(15,rlist)
#' D<-GenData(100,rmatrix,mu=0,se=1.25)

GenMatrix<-function(n,rlist=list(),a=0,b=1){
    ma<-matrix(a,n,n)+diag(n)*b
    for(i in rlist){
      ma[i[1],i[2]]<-ma[i[2],i[1]]<-i[3]
    }
    return(ma)
  }

#' To generate simulated data
#'
#' This function generate random simulated data, with certain correlation between variables,
#' @inheritParams GenData
#' @return a matrix
#' @param n the sample size
#' @param rmatrix correlation matrix, obtained from GenMatrix
#' @param mu, mean of the variabels, default as 0, also can can set as vector,e.g. c(1,2,3,4)
#' @param se, se of the variabels, default as 1, also can can set as vector,e.g. c(1,2,3,4)
#' @seealso GenMatrix
#' @export
#' @examples
#'
#' rlist=list(c(1,5,0.7),c(1,10,0.5),c(2,6,0.5),c(4,8,-0.7),c(7,8,0.3),c(11,12,0.7),c(9,13,0.5),c(7,14,0.5))
#' rmatrix<-GenMatrix(15,rlist)
#' D<-GenData(100,rmatrix,mu=0,se=1.25)

GenData<-function(n,rmatrix,mu=0,se=1){

    if(length(mu)==1){mu=rep(mu,nrow(rmatrix))}
    if(length(se)==1){se=rep(se,nrow(rmatrix))}
    if(length(mu)!=nrow(rmatrix)) stop('mu has improper length')
    if(length(se)!=nrow(rmatrix)) stop('se has improper length')

     d<-MASS::mvrnorm(n,mu=mu,Sigma=rmatrix)
     d=t(se*t(d))
     colnames(d)<-paste0("x",1:ncol(rmatrix))
     return(d)
}


#' To generate bootstap sample with equal sample size
#'
#' This function generate bootstap sample with equal sample size
#' @inheritParams
#' @return a bootstrap sample
#' @param data original data
#' @seealso
#' @export
#' @examples
#' library(PlotForMFP)
#' resampler(DataMFP)
#'
resampler<-function(data){
  n<-nrow(data)
  RowSample<-sample(n,n,replace=TRUE)
  return(data[RowSample,])
}



