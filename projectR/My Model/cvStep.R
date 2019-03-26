# Leave one out CV to calculate MSPE
# ... thinBy, times, g

crossValidate<-function(variable, maxTheta_s=100, maxTheta_r=1000, order=0, ...){
  nSites<-length(variable$data)
  
  summaryTable<-matrix(NA, nrow=nSites, ncol=12)
  
  for (i in 1:nSites){
    
    # fit model using construction set
    # find cont and A
    contAndA<-getContrastAndA(nSites, variable, order=order, leaveOut=i)
    cont<-contAndA[[1]]
    A<-contAndA[[2]]
  
    # estimate parameters
    objChain<-getObjChain(cont, A, maxTheta_s, maxTheta_r, leaveOut=i, ...)
    optimVals<-objChain[objChain[,3]==min(objChain[,3]),]
    theta_s<-optimVals[1]
    theta_r<-optimVals[2]
    theta_star <- getTheta_star(cont, A, psiMat(theta_s, theta_r, nSites=nSites, leaveOut=i, ...))
    
    psiFull <- psiMat(theta_s, theta_r, nSites=nSites, leaveOut = 0, ...)
    sigmaFull <- theta_star*psiFull
   
    cvC<-sigmaFull[i,][-i]
    cvSigma<-sigmaFull[-i,-i]
    
    if (order==0){
      lambda<-getLambdaOrd(cvC, cvSigma)
      predVar<-ordKrigVar(lambda, cvC, cvSigma)
    }
    if (order==1){
      x<-c(1,variable$coords[i,])
      X<-cbind(1,variable$coords[-i, ])
      lambda<-getLambdaUni(cvC, cvSigma, x, X)
      predVar<-uniKrigVar(lambda, cvC, cvSigma)
    }
    cvZ<-variable$data[-i]
    pred<-t(lambda) %*% cvZ
    
    dat<-variable$data[i]
      
    SPE<-(dat-pred)^2
    
    lb.75 <- pred-qnorm(0.875)*sqrt(predVar)
    ub.75 <- pred+qnorm(0.875)*sqrt(predVar)
    lb.85 <- pred-qnorm(0.925)*sqrt(predVar)
    ub.85 <- pred+qnorm(0.925)*sqrt(predVar)
    lb.95 <- pred-qnorm(0.975)*sqrt(predVar)
    ub.95 <- pred+qnorm(0.975)*sqrt(predVar)
    
    in.75 <- ((dat >= lb.75) && (dat <= ub.75))
    in.85 <- ((dat >= lb.85) && (dat <= ub.85))
    in.95 <- ((dat >= lb.95) && (dat <= ub.95))
    
    summaryVector<-c(i, SPE, predVar, lb.75, ub.75, in.75, lb.85, ub.85, in.85, lb.95, ub.95, in.95)
    
    summaryTable[i,]<-summaryVector
    
    print("i")
  }
  
  MSPE<-mean(summaryTable[,2])
  percent.75 <- sum(summaryTable[,6])/nSites
  percent.85 <- sum(summaryTable[,9])/nSites
  percent.95 <- sum(summaryTable[,12])/nSites
  
  cvSummary <- c(MSPE=MSPE, percent.75=percent.75, percent.85=percent.85, percent.95=percent.95)
  colnames(summaryTable)=c("leaveOut", "SPE", "krigeVar", "lb.75", "ub.75", "in.75", "lb.85", "up.85", "in.85", "lb.95", "up.95", "in.95")
  
  return(list(summaryTable, cvSummary))
}


