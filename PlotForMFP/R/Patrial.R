VarB_C<-function(vars,data){
  varB=c()
  for(i in vars){
    if(dim(table(data[,i]))<=5){
      varB<-c(varB,i)
    }
  }
  varC<-setdiff(vars,varB)
  return(list(C=varC,B=varB))
}

FormulaGot<-function(data){
  terms<-rownames(data$R)[-1]

  # grops the continuous variables and catogarical variables
  vars<-colnames(data$X)[-1]
  DataX<-as.data.frame(data$X)[,-1]
  B_C<-VarB_C(vars,DataX)
  # end

  # get mean of terms, catogracal terms, and continuous terms
  VarMean<-as.data.frame(t(as.data.frame(apply(DataX,2,mean))))
  VarMeanR<-c()
  for(i in terms){
    VarMeanR<-c(VarMeanR,transform(VarMean,temp=eval(parse(text=i)))$temp)
  }

  termsB<-terms[terms %in% B_C$B]
  termsC<-terms[!(terms %in% B_C$B)]
  termsC<-gsub("^I","",termsC)
  VarMeanC<-VarMeanR[!(terms %in% B_C$B)]
  #end

  # form formula
  FormuB<-paste0(termsB,collapse = "+")
  FormuC<-paste0("I(",termsC,"-",VarMeanC,")")
  FormuC<-paste0(FormuC,collapse="+")
  termy<-as.character(data$formula)[2]
  Formu0<-as.formula(paste0(termy,"~",FormuB,"+",FormuC))
  return(list(formula=Formu0,B_C=B_C))
}

# prepare the data for lm fitting

Datalm<-function(data){
  DataMo<-as.data.frame(cbind(data$y,data$X))
  colnames(DataMo)[1]<-as.character(data$formula)[2]
  return(DataMo)
}

#' To get partial predictor for MFP
#'
#' This function returns partial predictors with 95% confidence interval. The returned result will be adopted to PlotPatrial function
#'
#' @return A smoothed plot with confidence interval
#' @param data, results obtained from mfp function (see mfp packages)
#' @param var, the variable we need to obtain patrial predictor and make patrial plot
#' @seealso PlotPatrial()
#' @examples
#'
#' x<-runif(100,0,10)
#' y<-runif(100,0,10)
#' z=log(x)+0.2*y^2 + rnorm(100,0,2)
#'
#' library(mfp)
#' re<-mfp(z~fp(x,4)+fp(y,4))
#' re0<-Patrial("y",re)
#' PlotPatrial(re0)
#' PlotPatrial(re,xlab=list(x="X in R"))
#' PlotPatrial(re,"x")
#'
#' @export
  Patrial<-function(data){
  Formu0<-FormulaGot(data=data)
  DataMo<-Datalm(data)
  re<-lm(Formu0$formula,DataMo)
  sig.var<-rownames(data$trafo)[!is.na(data$trafo)]
  if(length(sig.var)==0){return("We can not return the patrial result, because there is no continuous variable or no continuous variable included in the final model")}else{

  ConVar=sig.var[sig.var %in% Formu0$B_C$C]
  Re<-list()
  xlim=list()
  ylim=list()
  log=list()
  xlab=list()
  ylab=list()
  for (iVar in ConVar){
    DataRe<-as.data.frame(re$model)
    temp<-coef(re)[-1]
    temp<-t(temp*t(DataRe[,names(temp)]))
    Pacom=coef(re)[1]+residuals(re)
    M<-c(grep(paste0(iVar,"/"),colnames(temp)),grep(paste0(iVar,")"),colnames(temp)),grep(paste0(iVar," +"),colnames(temp)))
    pa=Pacom+apply(as.data.frame(temp[,M]),1,sum)
    pa_p=coef(re)[1]+apply(as.data.frame(temp[,M]),1,sum)
    N=M+1
    cov=vcov(re)
    if(length(N)==2){CI=sqrt(cov[1,1]+cov[N[1],N[1]]*DataRe[,N[1]]^2+cov[N[2],N[2]]*DataRe[,N[2]]^2+
                               2*cov[N[1],N[2]]*DataRe[,N[1]]*DataRe[,N[2]]+
                               2*cov[1,N[1]]*DataRe[,N[1]]+
                               2*cov[1,N[2]]*DataRe[,N[2]])}else if(length(N)==1){
                                 CI=sqrt(cov[1,1]+cov[N[1],N[1]]*DataRe[,N[1]]^2+
                                           2*cov[1,N[1]]*DataRe[,N[1]])}
    pa_p_lower=pa_p-1.96*CI
    pa_p_upper=pa_p+1.96*CI
    DataX<-data$X[,iVar]
    ord=order(DataX,decreasing = FALSE)
    Re[[iVar]]=data.frame(pa=pa[ord],pa_p=pa_p[ord],pa_p_lower=pa_p_lower[ord],pa_p_upper=pa_p_upper[ord],x=DataX[ord])
    xlim[[iVar]]=range(DataX)
    ylim[[iVar]]=range(pa)
    log[[iVar]]=c("")
    xlab[[iVar]]=iVar
    ylab[[iVar]]="Patrial Predictor"
  }
  PlotOrNot<- ( length(ConVar) > 0 )
  return(list(PlotOrNot=PlotOrNot,Con=Formu0$B_C$C,ConVar=ConVar,PatrialData=Re,xlim=xlim,ylim=ylim,log=log,xlab=xlab,ylab=ylab,lm=re))
 }
}



