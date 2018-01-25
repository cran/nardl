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
  if(ncol(m)>=2){
    colnames(lagM)<-paste(colnames(m)[1:2],c(1:ncol(lagM)),sep = "_")
  }
  else{
    colnames(lagM)<-paste(colnames(m),c(1:ncol(lagM)),sep = "_")
  }
  #seq(nLags*ncol(m))
  return(lagM)
}

ArchTest <- function (x, lags=12, demean = FALSE)
{
  # Capture name of x for documentation in the output
  xName <- deparse(substitute(x))
  #
  x <- as.vector(x)
  if(demean) x <- scale(x, center = TRUE, scale = FALSE)
  #
  lags <- lags + 1
  mat <- embed(x^2, lags)
  arch.lm <- summary(lm(mat[, 1] ~ mat[, -1]))
  STATISTIC <- arch.lm$r.squared * length(resid(arch.lm))
  names(STATISTIC) <- "Chi-squared"
  PARAMETER <- lags - 1
  names(PARAMETER) <- "df"
  PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
  METHOD <- "ARCH LM-test;  Null hypothesis:  no ARCH effects"
  result <- list(statistic = STATISTIC, parameter = PARAMETER,
                 p.value = PVAL, method = METHOD, data.name =
                   xName)
  class(result) <- "htest"
  return(result)
}


#'Nonlinear ARDL function
#'
#'@param formula food~inf or food~inf|I(inf^2)
#'@param data the dataframe
#'@param p max lags of dependent variable
#'@param q max lags of independent variables
#'@param ic : c("aic","bic","R2") criteria model selection
#'@param graph 1 to show stability tests plot
#'@importFrom car linearHypothesis
#'@importFrom stats lm AIC BIC as.formula model.frame model.matrix model.response na.omit sd update vcov embed resid
#'@importFrom graphics abline legend lines par plot
#'@importFrom strucchange recresid
#'@importFrom tseries jarque.bera.test
#'@importFrom lmtest bgtest
#'@import Formula
#'@import matlab
#'@import qpcR
#'@export
nardl<-function(formula,data,p,q,ic=c("aic","bic","R2"),graph){
  case<-3
  f<-formula
  a<-unlist(strsplit(as.character(f),"[|]"))
  #core<-paste(un[[2]],"~",un[[3]],"+",un[[4]])
  if(length(a)==4){
    lhs   <- a[[2]]
    core  <- a[[3]]
    suffix <- a[[4]]
    fm <- paste(lhs,"~",core,"|",suffix)
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action"), names(mf), 0)
    mf <- mf[c(1, m)]
    ffm <- Formula::Formula(as.formula(fm))
    mf[[1]] <- as.name("model.frame")
    mf$formula <- ffm
    fffm<-update(ffm, ~ . -1|~ . -1)
    mf <- eval(mf, parent.frame())
    x <- model.matrix(fffm, data = mf, rhs = 1)
    h <- model.matrix(fffm, data = mf, rhs = 2)
    y <- model.response(mf)
    dy<-diff(as.matrix(y))
    dx<-diff(x)
    dh<-diff(h)
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
    lagh<-lagm(as.matrix(h),1)
    lxp<-lagm(as.matrix(xp),1)
    lxn<-lagm(as.matrix(xn),1)
    #fit<-lm(dy~na.omit(lagy)+lxp+lxn)
    #lag order selection
    R2 = NULL
    AK = NULL
    SC = NULL
    ordmax<-max(p,q)
    for (i in 1:ordmax){
      #lagmat = cbind(lagmat[-i,],x[(1):(l1-i)]) # lagged matrix
      # armod <- lm(x[(i+1):l1]~lagmat)
      ldy<-lagm(dy,c(1:i))
      ldh<-lagm(as.matrix(dh),c(1:i))
      ldxp<-lagm(as.matrix(dxp),c(1:i))
      ldxn<-lagm(as.matrix(dxn),c(1:i))
      lagy<-na.omit(lagy)
      lagh<-na.omit(lagh)
      fit<-lm(dy~lagy+lxp+lxn+ldy+ldxp+ldxn+lagh+ldh)
      R2[i] = qpcR::Rsq(fit)
      AK[i] = AIC(fit)
      SC[i] = BIC(fit)
    }
    np<-0
    if(ic=="aic") np<-which.min(AK)
    if(ic=="bic") np<-which.min(SC)
    if(ic=="R2") np<-which.min(R2)
    ldy<-lagm(dy,c(1:np))
    ldh<-lagm(as.matrix(dh),c(1:np))
    ldxp<-lagm(as.matrix(dxp),c(1:np))
    ldxn<-lagm(as.matrix(dxn),c(1:np))
    lagy<-na.omit(lagy)
    lagh<-na.omit(lagh)
    fit<-lm(dy~lagy+lxp+lxn+ldy+ldxp+ldxn+lagh+ldh)
    fitcoef<-summary(fit)

  }else{
    lhs   <- a[[2]]
    core  <- a[[3]]
    fm <- paste(lhs,"~",core)
    ffm<-as.formula(fm)
    fffm<-update(ffm, ~ . -1)
    mf <- model.frame(formula=fffm, data=data)
    x <- model.matrix(attr(mf, "terms"), data=mf)
    y <- model.response(mf)
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
    #lag order selection
    R2 = NULL
    AK = NULL
    SC = NULL
    ordmax<-max(p,q)
    for (i in 1:ordmax){
      ldy<-lagm(dy,c(1:i))
      ldxp<-lagm(as.matrix(dxp),c(1:i))
      ldxn<-lagm(as.matrix(dxn),c(1:i))
      lagy<-na.omit(lagy)
      fit<-lm(dy~lagy+lxp+lxn+ldy+ldxp+ldxn)
      R2[i] = qpcR::Rsq(fit)
      AK[i] = AIC(fit)
      SC[i] = BIC(fit)
    }
    np<-0
    if(ic=="aic") np<-which.min(AK)
    if(ic=="bic") np<-which.min(SC)
    if(ic=="R2") np<-which.min(R2)
    ldy<-lagm(dy,c(1:np))
    ldxp<-lagm(as.matrix(dxp),c(1:np))
    ldxn<-lagm(as.matrix(dxn),c(1:np))
    lagy<-na.omit(lagy)
    fit<-lm(dy~lagy+lxp+lxn+ldy+ldxp+ldxn)
    fitcoef<-summary(fit)
  }

  #long run coeff
  sel<-summary(fit)
  residu<-sel$residuals
  #jarque berra test for residual normallity
  jbtest<-tseries::jarque.bera.test(residu)
  #lmtest and ARCH tests
  lmtest<-lmtest::bgtest(fit,order = np)
  arch<-ArchTest(residu,np)

  coeff<-sel$coefficients
  nlvars<-length(coeff[,1])
  lvars<-coeff[3:nlvars,1]
  seldata<-data.matrix(coeff)
  coof<- -lvars/coeff[[2]]
  cof<- matrix(coof, length(lvars),1)
  #-----SE by delta method
  vc<-vcov(sel)
  vcc<-vc[2:nrow(vc),2:ncol(vc)]
  nsel<-length(sel$coefficients[,1])
  fb1<-lvars/coeff[[2]]^2
  fb2<-(-coeff[[2]])*matlab::eye(nrow(as.matrix(fb1)))
  fb<-cbind(as.matrix(fb1),fb2)
  lrse<-sqrt(diag(fb%*%vcc%*%t(fb)))
  lrt<-coof/lrse
  lrpv<-2*pt(-abs(lrt), sel$df[2])
  lres<-cbind(cof,lrse,lrt,lrpv)
  lres
  rownames(lres)<-names(lvars)
  colnames(lres)<-c("Estimate", "Std. Error", "t value", "Pr(>|t|)" )
  #---------------------------------------------------
  #cointegration test
  l<-c(paste(names(coeff[-1,1]),"=0"))
  coin<-car::linearHypothesis(fit,l,test="F")
  fstat<-coin$F[2]
  k<-ncol(x)
  #asymetry test for xn vs xp
  ll2<-c(paste(names(coof[1]),"=",names(coof[2]) ))
  asym<-car::linearHypothesis(fit,ll2,test="F")
  tasym<-asym$F
  pasym<-asym$`Pr(>F)`
  #get recursive residuals
  rece<-strucchange::recresid(fit)
 # plot cumsum and cumsumq
  if(graph==1){
    bbst<-sel$coefficients[,1]
    kt<-length(bbst[-1])
    n<-length(rece)
    cusum(rece,kt)
    cumsq(rece,kt,n)
  }

 out<-list(fstat=fstat, fit=fit,fitcoef=fitcoef, sel=sel, cof=cof,coof=coof,
       k=k,case=case,lvars=lvars,vcc=vcc,fb=fb,fb1=fb1,fb2=fb2,
       lrse=lrse,lrt=lrt,lrpv=lrpv,lres=lres,selresidu=residu,
       tasym=tasym,pasym=pasym,jbtest=jbtest,lmtest=lmtest,arch=arch,np=np)
 class(out) <- "nardl"
 # Return results.
 return(out)



}

cumsq<-function(e,k,n){
  w<-as.matrix(na.omit(e))
  w=cumsum(w^2)/sum(w^2)
  m=abs(0.5*(n-k)-1) #abs to avoid negative log
  c=0.74191-0.17459*log(m)-0.26526*(1/m)+0.0029985*m-0.000010943*m^2
  w2=c+(seqa(k,1,length(w))-k)/(n-k)
  w1=-c+(seqa(k,1,length(w))-k)/(n-k)
  x<-seqa(k,1,length(w))
  w1<-matrix(w1,length(w1),1)
  w<-matrix(w,length(w),1)
  w2<-matrix(w2,length(w2),1)
  grange<-range(w1,w2)
  par(mar = c(5,4,4,8))
  plot(x,w,main="CUSUM of Squares Test",type = "l",ylim = grange,xlab ="",ylab = "Emperical fluctuation process",col="blue")
  lines(x,w1,col="red")
  lines(x,w2,col="red")
  abline(h=0,lty=2)
  legend(par("usr")[2],par("usr")[4],
         xpd = TRUE ,
         bty = "n",
         c("CUSUM of squares","5% significance"),
         lty = c(1, 1),
         cex=0.8,
         col=c("blue","red") )

}

cusum<-function(e,k){
  w<-as.matrix(na.omit(e))
  n<-length(e)
  w=cumsum(w/apply(w, 2, sd))
  c=0.984
  w2=seqa(c*sqrt(n-k),(2*c*sqrt(n-k))/length(w),length(w))
  w1=seqa(-c*sqrt(n-k),(-2*c*sqrt(n-k))/length(w),length(w))
  x<-seqa(k,1,length(w))
  w1<-matrix(w1,length(w1),1)
  w<-matrix(w,length(w),1)
  w2<-matrix(w2,length(w2),1)
  grange<-range(w1,w2)
  par(mar = c(5,4,4,8))
  plot(x,w,main="CUSUM Test",type = "l",ylim = grange,xlab = "",ylab = "Emperical fluctuation process",col="blue")
  lines(x,w1,col="red")
  lines(x,w2,col="red")
  abline(h=0,lty=2)
  legend(par("usr")[2],par("usr")[4],
         xpd = TRUE ,
        bty = "n",
         c("CUSUM ","5% significance"),
         lty = c(1, 1),
         cex=0.8,
         col=c("blue","red") )

}

seqa<-function(a,b,c){
  #seq=(a:b:(a+b*(c-1)))';
  se<-seq(a,(a+b*(c-1)),by=b)
  return(t(se))
}



#' summary
#'
#' @param object is the object of the function
#' @param ... not used
#' @importFrom stats pchisq printCoefmat pt
#' @export
summary.nardl<-function(object,...)
{

  cat("==============================================================\n")
  cat("\n NARDL model:\n")
  print(object$sel)
  cat("---------------------------------\n")
  cat("\n model diagnostic tests:\n----------\n")
  cat(" JB test:\n","JB:",object$jbtest$statistic[[1]],"Pvalue",object$jbtest$p.value[[1]],"\n----------\n")
  cat(" LM test for serial correlation:\n","LM(",object$np,"):",object$lmtest$statistic[[1]],"Pvalue",object$lmtest$p.value[[1]],"\n----------\n")
  cat(" ARCH test:\n","ARCH(",object$np,"):",object$arch$statistic[[1]],"Pvalue",object$arch$p.value[[1]],"\n----------\n")
  cat("==============================================================\n")
  cat("Asymmetric Cointegration test\n")
  print( bounds.test(3,object$k,object$fstat))
  cat("==============================================================\n")
  cat("\nLong-run coefficients\n")
  printCoefmat(object$lres,has.Pvalue = TRUE,signif.stars = TRUE)
  cat("==============================================================\n")
  cat(" Long Run Asymmety test\n","F-stat:",object$tasym[2],"Pvalue:",object$pasym[2],"\n")
  cat("==============================================================\n")

}



