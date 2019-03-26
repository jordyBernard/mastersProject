get.Sig <- function(dist.mat, sig.sq, phi){
  n.sites <- nrow(dist.mat)
  Sig <- matrix(NA, nrow=n.sites, ncol=n.sites)
  for(i in 1:n.sites){
    for(j in 1:n.sites){
      dist <- dist.mat[i,j]
      sig_ij <- sig.sq-exp.variog(dist, sig.sq, phi)
      Sig[i,j] <- sig_ij
    }
  }
  return(Sig)
}

swd.CV <- function(dist.mat, variable, init.param){
  data <- variable$data
  sig.sq.init <- init.param[1]
  phi.init <- init.param[2]
  summaryTable <- matrix(NA, nrow=length(data), ncol=12)
  for(leave.out in 1:length(data)){
    dist.mat.lo <- dist.mat[-leave.out, -leave.out]
    data.lo <- data[-leave.out]
    swd.data.lo <- swd.data(dist.mat.lo, data.lo)
    Q.variable <- function(params){
      sig.sq<-params[1]
      phi<-params[2]
      return(Q(sig.sq, phi, swd.data.lo))
    } 
    
    fit <- optim(c(sig.sq.init, phi.init), lower=c(0,0), fn=Q.variable, method ="L-BFGS-B")
    sig.sq.lo <- fit$par[1]
    phi.lo <- fit$par[2]
    Sig.full <- get.Sig(dist.mat, sig.sq.lo, phi.lo)
    Sig.lo<-Sig.full[-leave.out, -leave.out]
    c.lo <- as.matrix(Sig.full[leave.out,][-leave.out])
    lambda <- getLambdaOrd(c.lo, Sig.lo)
    predVar<-krigVarOrd(lambda, c.lo, Sig.lo)
    
    z <- variable$data[-leave.out]
    pred<-t(lambda) %*% z
    
    dat<-variable$data[leave.out]
    
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
    
    summaryVector<-c(leave.out, SPE, predVar, lb.75, ub.75, in.75, lb.85, ub.85, in.85, lb.95, ub.95, in.95)
    
    summaryTable[leave.out,]<-summaryVector
  }
  nSites <- nrow(dist.mat)
  
  MSPE<-mean(summaryTable[,2])
  percent.75 <- sum(summaryTable[,6])/nSites
  percent.85 <- sum(summaryTable[,9])/nSites
  percent.95 <- sum(summaryTable[,12])/nSites
  
  cvSummary <- c(MSPE=MSPE, percent.75=percent.75, percent.85=percent.85, percent.95=percent.95)
  colnames(summaryTable)=c("leaveOut", "SPE", "krigeVar", "lb.75", "ub.75", "in.75", "lb.85", "up.85", "in.85", "lb.95", "up.95", "in.95")
  
  return(list(summaryTable, cvSummary))
}

swd.results <- swd.CV(dist.mat, aveChlor, c(.25, 25))
