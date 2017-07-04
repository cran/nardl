bounds.test <- function(case,k,Fstat ){

  #DEBUG <- TRUE
  tables="pss"
  append=FALSE
  file=NULL

  val <- NULL
  val <- rbind(val,	c(2.44,3.28,3.15,4.11,3.88,4.92,4.81,6.02)	)
  val <- rbind(val,	c(2.17,3.19,2.72,3.83,3.22,4.5,3.88,5.3)	)
  val <- rbind(val,	c(2.01,3.1,2.45,3.63,2.87,4.16,3.42,4.84)	)
  val <- rbind(val,	c(1.9,3.01,2.26,3.48,2.62,3.9,3.07,4.44)	)
  val <- rbind(val,	c(1.81,2.93,2.14,3.34,2.44,3.71,2.82,4.21)	)
  val <- rbind(val,	c(1.75,2.87,2.04,3.24,2.32,3.59,2.66,4.05)	)
  val <- rbind(val,	c(1.7,2.83,1.97,3.18,2.22,3.49,2.54,3.91)	)
  val <- rbind(val,	c(1.66,2.79,1.91,3.11,2.15,3.4,2.45,3.79)	)
  val <- rbind(val,	c(1.63,2.75,1.86,3.05,2.08,3.33,2.34,3.68)	)
  val <- rbind(val,	c(1.6,2.72,1.82,2.99,2.02,3.27,2.26,3.6)	)
  case1 <- data.frame( k=1:10, value=matrix(val,nrow=10,ncol=8))
  colnames(case1)=c("K","90.0","90.1","95.0","95.1","97.0","97.1","99.0","99.1")

  val <- NULL
  val <- rbind(val,	c(3.02,3.51,3.62,4.16,4.18,4.79,4.94,5.58)	)
  val <- rbind(val,	c(2.63,3.35,3.1,3.87,3.55,4.38,4.13,5)	)
  val <- rbind(val,	c(2.37,3.2,2.79,3.67,3.15,4.08,3.65,4.66)	)
  val <- rbind(val,	c(2.2,3.09,2.56,3.49,2.88,3.87,3.29,4.37)	)
  val <- rbind(val,	c(2.08,3,2.39,3.38,2.7,3.73,3.06,4.15)	)
  val <- rbind(val,	c(1.99,2.94,2.27,3.28,2.55,3.61,2.88,3.99)	)
  val <- rbind(val,	c(1.92,2.89,2.17,3.21,2.43,3.51,2.73,3.9)	)
  val <- rbind(val,	c(1.85,2.85,2.11,3.15,2.33,3.42,2.62,3.77)	)
  val <- rbind(val,	c(1.8,2.8,2.04,3.08,2.24,3.35,2.5,3.68)	)
  val <- rbind(val,	c(1.76,2.77,1.98,3.04,2.18,3.28,2.41,3.61)	)
  case2 <- data.frame( k=1:10, value=matrix(val,nrow=10,ncol=8))
  colnames(case2)=c("K","90.0","90.1","95.0","95.1","97.0","97.1","99.0","99.1")

  val <- NULL
  val <- rbind(val,	c(4.04,4.78,4.94,5.73,5.77,6.68,6.84,7.84)	)
  val <- rbind(val,	c(3.17,4.14,3.79,4.85,4.41,5.52,5.15,6.36)	)
  val <- rbind(val,	c(2.72,3.77,3.23,4.35,3.69,4.89,4.29,5.61)	)
  val <- rbind(val,	c(2.45,3.52,2.86,4.01,3.25,4.49,3.74,5.06)	)
  val <- rbind(val,	c(2.26,3.35,2.62,3.79,2.96,4.18,3.41,4.68)	)
  val <- rbind(val,	c(2.12,3.23,2.45,3.61,2.75,3.99,3.15,4.43)	)
  val <- rbind(val,	c(2.03,3.13,2.32,3.5,2.6,3.84,2.96,4.26)	)
  val <- rbind(val,	c(1.95,3.06,2.22,3.39,2.48,3.7,2.79,4.1)	)
  val <- rbind(val,	c(1.88,2.99,2.14,3.3,2.37,3.6,2.65,3.97)	)
  val <- rbind(val,	c(1.83,2.94,2.06,3.24,2.28,3.5,2.54,3.86)	)
  case3 <- data.frame( k=1:10, value=matrix(val,nrow=10,ncol=8))
  colnames(case3)=c("K","90.0","90.1","95.0","95.1","97.0","97.1","99.0","99.1")

  val <- NULL
  val <- rbind(val,	c(4.05,4.49,4.68,5.15,5.3,5.83,6.1,6.73)	)
  val <- rbind(val,	c(3.38,4.02,3.88,4.61,4.37,5.16,4.99,5.85)	)
  val <- rbind(val,	c(2.97,3.74,3.38,4.23,3.8,4.68,4.3,5.23)	)
  val <- rbind(val,	c(2.68,3.53,3.05,3.97,3.4,4.36,3.81,4.92)	)
  val <- rbind(val,	c(2.49,3.38,2.81,3.76,3.11,4.13,3.5,4.63)	)
  val <- rbind(val,	c(2.33,3.25,2.63,3.62,2.9,3.94,3.27,4.39)	)
  val <- rbind(val,	c(2.22,3.17,2.5,3.5,2.76,3.81,3.07,4.23)	)
  val <- rbind(val,	c(2.13,3.09,2.38,3.41,2.62,3.7,2.93,4.06)	)
  val <- rbind(val,	c(2.05,3.02,2.3,3.33,2.52,3.6,2.79,3.93)	)
  val <- rbind(val,	c(1.98,2.97,2.21,3.25,2.42,3.52,2.68,3.84)	)
  case4 <- data.frame( k=1:10, value=matrix(val,nrow=10,ncol=8))
  colnames(case4)=c("K","90.0","90.1","95.0","95.1","97.0","97.1","99.0","99.1")

  val <- NULL
  val <- rbind(val,	c(5.59,6.26,6.56,7.3,7.46,8.27,8.74,9.63)	)
  val <- rbind(val,	c(4.19,5.06,4.87,5.85,5.49,6.59,6.34,7.52)	)
  val <- rbind(val,	c(3.47,4.45,4.01,5.07,4.52,5.62,5.17,6.36)	)
  val <- rbind(val,	c(3.03,4.06,3.47,4.57,3.89,5.07,4.4,5.72)	)
  val <- rbind(val,	c(2.75,3.79,3.12,4.25,3.47,4.67,3.93,5.23)	)
  val <- rbind(val,	c(2.53,3.59,2.87,4,3.19,4.38,3.6,4.9)	)
  val <- rbind(val,	c(2.38,3.45,2.69,3.83,2.98,4.16,3.34,4.63)	)
  val <- rbind(val,	c(2.26,3.34,2.55,3.68,2.82,4.02,3.15,4.43)	)
  val <- rbind(val,	c(2.16,3.24,2.43,3.56,2.67,3.87,2.97,4.24)	)
  val <- rbind(val,	c(2.07,3.16,2.33,3.46,2.56,3.76,2.84,4.1)	)
  case5 <- data.frame( k=1:10, value=matrix(val,nrow=10,ncol=8))
  colnames(case5)=c("k","90.0","90.1","95.0","95.1","97.0","97.1","99.0","99.1")

  ## -------------------------------------------------
  # validate
  match.arg( tables,c("pss") )  # c("pss","narayan")
  if (tables=="pss")
    table <- switch( case, case1, case2, case3, case4, case5 )
  #else
  #table <- switch( case, n_case1, n_case2, n_case3, n_case4, n_case5 )

  #if (!inherits(obj,"ardl")) stop("Class of the argument must be ardl.")

  #K <- length(obj$variableTerms)
  if (k<1 || k>10) stop("Number of regressors must be between 1 and 10")

  case_desc <- switch(case, "no intercept, no trend",
                      "restricted intercert, no trend (not supported)",
                      "unrestricted intercert, no trend",
                      "unrestricted intercept, restricted trend (not supported)",
                      "unrestricted intercept, unrestricted trend")


  cat("\nBounds Test:\n")
  #cat(deparse(formula(obj)),"\n")
  cat("\nPSS case",case," (",case_desc,")")
  cat("\nRegressors (K)",k," \n\n")

  ## document the null hypothesis or NO LR relation
  cat("d(y_t) = alpha + pi (y_t-1,x_t)' + phi (d(y_t),d(x_t))' + epsilon_t \n")
  cat("Null hypothesis (H0): No long-run relation exist, ie H0:pi=0\n\n")

  cat(sprintf("         I(0)   I(1)\n"))
  cat(sprintf("  10%%   %3.2f  %3.2f\n", table[k,"90.0"],table[k,"90.1"] ))
  cat(sprintf("   5%%   %3.2f  %3.2f\n", table[k,"95.0"],table[k,"95.1"] ))
  cat(sprintf(" 2.5%%   %3.2f  %3.2f\n", table[k,"97.0"],table[k,"97.1"] ))
  cat(sprintf("   1%%   %3.2f  %3.2f\n", table[k,"99.0"],table[k,"99.1"] ))
  cat("\nF statistic ",Fstat,"\n\n")
  diagn <- "Existence of a Long Term relation is"
  if (Fstat>table[k,"95.1"])
    cat(diagn,"not rejected at 5%")
  if (Fstat<table[k,"95.0"])
    cat(diagn,"rejected at 5% (even assumming all regressors I(0))")
  if (Fstat<=table[k,"95.1"] && Fstat>=table[k,"95.0"])
    cat(diagn,"rejected at 5% with I(1) regressors but not with I(0) regressors ")
}
