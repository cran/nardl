#' Indian yearly data of inflation rate and percentage food import to total import
#'
#' \itemize{
#' \item{food}{percentage food import to total import}
#' \item{inf}{inflation rate}
#' \item{year}{the year}
#' }
#'
#' @docType data
#' @name  fod
#' @usage data(fod)
#' @format A data frame with 54 rows and 2 variables
NULL

trimr <- function(x,rb,re) {
  x <- cbind(x)
  n <- nrow(x)
  if ((rb+re) >= n) {
    stop('Attempting to trim too much')
  }
  z <- x[(rb+1):(n-re),]
  return(z)
}

lagm <- function(m, nLags) {
  nargin <- length(as.list(match.call())) - 1
  if (nargin != 2) {
    stop('Check function inputs')
  }
  lagM <- c()
  for(i in seq(nLags)) {
    for(j in seq(ncol(m))) {
      tmp <- c(rep(NA, i), trimr(m[,j], 0, i))
      #colnames(tmp)<-colnames(m)

      lagM <- cbind(lagM, tmp)

    }
  }
  colnames(lagM)<-paste(colnames(m)[1:2],c(1:ncol(lagM)),sep = "_")
  #seq(nLags*ncol(m))
  return(lagM)
}


#'@importFrom car linearHypothesis
#'@importFrom stats lm
nreg<-function(x,y,p,q){
  dy<-diff(as.matrix(y))
  dx<-diff(x)
  n<-nrow(dx)
  cl<-ncol(dx)
  pos<-dx[,1:cl]>=0
  dxp<-as.numeric(pos)*dx[,1:cl]
  dxn<-(1-as.numeric(pos))*dx[,1:cl]
  if(ncol(dx)>=2){
    xp<-apply(dxp,2,cumsum)
    xn<-apply(dxn,2,cumsum)
  }
  else{
    xp<-cumsum(dxp)
    xn<-cumsum(dxn)
  }
  lagy<-lagm(as.matrix(y),1)
  lxp<-lagm(as.matrix(xp),1)
  lxn<-lagm(as.matrix(xn),1)
  ldy<-lagm(dy,c(1:p))
  ldxp<-lagm(as.matrix(dxp),c(0:q))
  ldxn<-lagm(as.matrix(dxn),c(0:q))
  lagy<-na.omit(lagy)
  fit<-lm(dy~lagy+lxp+lxn+ldy+ldxp+ldxn )
  coeff1<-summary(fit)$coefficients
  #model selection
  sell<-model.select(fit,sig = 0.1,verbose=F)
  coeff<-summary(sell)$coefficients
  nlvars<-length(coeff[,1])
  lvars<-coeff[3:nlvars,1]
  coof<- -lvars/coeff[[2]]
  cof<- matrix(coof, length(lvars),1)
  fcof<-matrix(coeff[1:2,1],length(coeff[1:2,1]),1)
  lcof<-rbind(fcof,cof)
  rownames(lcof)<-rownames(coeff)
  colnames(lcof)<-"Long-run coef"
  #cointegration test
  l<-c(paste(names(coeff[-1,1]),"=0"))
  coin<-linearHypothesis(sell,l,test="F")
  # asymmetry test
  ll<-c(paste(names(coof[1]),"=",names(coof[2]) ))
  asym<-linearHypothesis(sell,ll,test="F")
  #results
  fstat<-coin$F[2]
  k<-ncol(x)
  #first ardl model
  s1<-summary(fit)
  s2<-summary(sell)
  tasym<-asym$F[2]
  pasym<-asym$`Pr(>F)`[2]
  list( coeff1=coeff1, coeff=coeff, lcof=lcof,fstat=fstat,s1=s1,s2=s2,tasym=tasym, pasym=pasym,k=k)
}

#' method
#'
#' @author Zaghdoudi Taha
#' @param x a numeric design matrix for the model.
#' @param ... not used
#' @export
nardl<- function(x,...){UseMethod("nardl") }

nardl.default <- function(x,y,p,q,...)
{
 est <- nreg(x,y,p,q,...)
  est$call <- match.call()
  class(est) <- "nardl"
  est

}

#print.nardl <- function(x,...)
#{
 # cat("Call:\n")
  #print(x$call)
 # cat("\nCoefficients:\n")
 # print(x$coefficients)
#}

#' Summary
#'
#' @param object is the object of the function
#' @param ... not used
#' @importFrom stats pchisq printCoefmat pt
#' @export
summary.nardl<-function(object,...)
{
  cat("==============================================================\n")
  cat("\nNARDL model:\n")
  print(object$s1)
  cat("==============================================================\n")
  cat("\n Selected NARDL model:\n")
  print(object$s2)
  cat("==============================================================\n")
  cat("Asymmetric Cointegration test\n")
  print( coint<-bounds.test(3,object$k,object$fstat))
  cat("==============================================================\n")
  cat("\nLong-run coefficients\n")
  print(object$lcof)
  cat("==============================================================\n")
  cat("Asymmetry statistics\n")
  cat("\nWald F-statistic: ",object$tasym,"Pvalue: ",object$pasym,"\n")

}
#' formula
#'
#' @param formula food~inf
#' @param p scalar, autoregressive order for dependent variable
#' @param q scalar, autoregressive order for dependent variable
#' @param data the dataframe
#' @param ... not used
#' @importFrom stats model.frame model.matrix model.response  update
#' @export
nardl.formula <-function(formula,p,q,data=list(),...)
{
  mff <- update(formula, ~ . -1)
  mf <- model.frame(mff, data=data)
  x <- model.matrix(attr(mf, "terms"), data=mf)
  y <- model.response(mf)
  est <- nardl.default(x,y,p,q,...)
  est$call <- match.call()
  est$equa <- formula
  est
}


