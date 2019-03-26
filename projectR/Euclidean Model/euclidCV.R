# There's probably a much cleaner implementation, but this will do for now

# init.cov.pars specs:
# aveChlor.euc trend=1st sigsq=0.16, phi=24

euclidCv <- function(geodata, ini.cov.pars, trend="cte", max.dist=75){
  
  n.sites <- length(geodata$data)
  
  summaryTable.exp <- matrix(NA, nrow=n.sites, ncol=12)
  summaryTable.gauss <- matrix(NA, nrow=n.sites, ncol=12)
  summaryTable.wave <- matrix(NA, nrow=n.sites, ncol=12)
  
  for(i in 1:n.sites){
    leave.out.coords <- geodata$coords[i,]
    leave.out.data <- geodata$data[i]
    geodata.construct <- as.geodata(data.frame(geodata$coords[-i,1], geodata$coords[-i,2], geodata$data[-i]))
    
    variog.est <- variog(geodata.construct, trend=trend, estimator.type="modulus", max.dist=max.dist)
    
    exp.fit <- variofit(variog.est, 
                        ini.cov.pars=ini.cov.pars,
                        cov.model="exponential", 
                        fix.nugget=T,
                        max.dist = max.dist)
    gauss.fit <- variofit(variog.est, 
                         ini.cov.pars=ini.cov.pars,
                         cov.model="gaussian", 
                         fix.nugget=T,
                         max.dist = max.dist)
  
    wave.fit <- variofit(variog.est, 
                         ini.cov.pars=ini.cov.pars,
                         cov.model="wave", 
                         fix.nugget=T,
                         max.dist = max.dist)
    
    kr.obj.exp <- krige.control(type.krige = "OK",
                                trend.d=trend,
                                trend.l=trend,
                                cov.model="exponential",
                                cov.pars=exp.fit$cov.pars,
                                nugget=0)
    
    kr.obj.gauss <- krige.control(type.krige = "OK",
                                trend.d=trend,
                                trend.l=trend,
                                cov.model="gaussian",
                                cov.pars=gauss.fit$cov.pars,
                                nugget=0)
    
    kr.obj.wave <- krige.control(type.krige = "OK",
                                  trend.d=trend,
                                  trend.l=trend,
                                  cov.model="wave",
                                  cov.pars=wave.fit$cov.pars,
                                  nugget=0)
    
    kr.results.exp <- krige.conv(geodata.construct, krige=kr.obj.exp, locations=leave.out.coords)
    kr.results.gauss <- krige.conv(geodata.construct, krige=kr.obj.gauss, locations=leave.out.coords)
    kr.results.wave <- krige.conv(geodata.construct, krige=kr.obj.wave, locations=leave.out.coords)
    
    SPE.exp <- (kr.results.exp$predict-leave.out.data)^2
    SPE.gauss <- (kr.results.gauss$predict-leave.out.data)^2
    SPE.wave <- (kr.results.wave$predict-leave.out.data)^2
    
    predVar.exp <- kr.results.exp$krige.var
    predVar.gauss <- kr.results.gauss$krige.var
    predVar.wave <- kr.results.wave$krige.var
    
    lb.75.exp <- kr.results.exp$predict-qnorm(0.875)*sqrt(predVar.exp)
    ub.75.exp <- kr.results.exp$predict+qnorm(0.875)*sqrt(predVar.exp)
    lb.85.exp <- kr.results.exp$predict-qnorm(0.925)*sqrt(predVar.exp)
    ub.85.exp <- kr.results.exp$predict+qnorm(0.925)*sqrt(predVar.exp)
    lb.95.exp <- kr.results.exp$predict-qnorm(0.975)*sqrt(predVar.exp)
    ub.95.exp <- kr.results.exp$predict+qnorm(0.975)*sqrt(predVar.exp)
    
    lb.75.gauss <- kr.results.gauss$predict-qnorm(0.875)*sqrt(predVar.gauss)
    ub.75.gauss <- kr.results.gauss$predict+qnorm(0.875)*sqrt(predVar.gauss)
    lb.85.gauss <- kr.results.gauss$predict-qnorm(0.925)*sqrt(predVar.gauss)
    ub.85.gauss <- kr.results.gauss$predict+qnorm(0.925)*sqrt(predVar.gauss)
    lb.95.gauss <- kr.results.gauss$predict-qnorm(0.975)*sqrt(predVar.gauss)
    ub.95.gauss <- kr.results.gauss$predict+qnorm(0.975)*sqrt(predVar.gauss)
    
    lb.75.wave <- kr.results.wave$predict-qnorm(0.875)*sqrt(predVar.wave)
    ub.75.wave <- kr.results.wave$predict+qnorm(0.875)*sqrt(predVar.wave)
    lb.85.wave <- kr.results.wave$predict-qnorm(0.925)*sqrt(predVar.wave)
    ub.85.wave <- kr.results.wave$predict+qnorm(0.925)*sqrt(predVar.wave)
    lb.95.wave <- kr.results.wave$predict-qnorm(0.975)*sqrt(predVar.wave)
    ub.95.wave <- kr.results.wave$predict+qnorm(0.975)*sqrt(predVar.wave)
    
    in.75.exp <- ((leave.out.data >= lb.75.exp) && (leave.out.data <= ub.75.exp))
    in.85.exp <- ((leave.out.data >= lb.85.exp) && (leave.out.data <= ub.85.exp))
    in.95.exp <- ((leave.out.data >= lb.95.exp) && (leave.out.data <= ub.95.exp))
    
    in.75.gauss <- ((leave.out.data >= lb.75.gauss) && (leave.out.data <= ub.75.gauss))
    in.85.gauss <- ((leave.out.data >= lb.85.gauss) && (leave.out.data <= ub.85.gauss))
    in.95.gauss <- ((leave.out.data >= lb.95.gauss) && (leave.out.data <= ub.95.gauss))
    
    in.75.wave <- ((leave.out.data >= lb.75.wave) && (leave.out.data <= ub.75.wave))
    in.85.wave <- ((leave.out.data >= lb.85.wave) && (leave.out.data <= ub.85.wave))
    in.95.wave <- ((leave.out.data >= lb.95.wave) && (leave.out.data <= ub.95.wave))
      
    summaryVector.exp <- c(i, SPE.exp, predVar.exp, lb.75.exp, ub.75.exp, in.75.exp, lb.85.exp, ub.85.exp, in.85.exp, lb.95.exp, ub.95.exp, in.95.exp)
    summaryVector.gauss <- c(i, SPE.gauss, predVar.gauss, lb.75.gauss, ub.75.gauss, in.75.gauss, lb.85.gauss, ub.85.gauss, in.85.gauss, lb.95.gauss, ub.95.gauss, in.95.gauss)
    summaryVector.wave <- c(i, SPE.wave, predVar.wave, lb.75.wave, ub.75.wave, in.75.wave, lb.85.wave, ub.85.wave, in.85.wave, lb.95.wave, ub.95.wave, in.95.wave)
  
    summaryTable.exp[i,] <- summaryVector.exp
    summaryTable.gauss[i,] <- summaryVector.gauss
    summaryTable.wave[i,] <- summaryVector.wave
  }
  
  
  colnames(summaryTable.exp)=c("leaveOut", "SPE", "krigeVar", "lb.75", "ub.75", "in.75", "lb.85", "up.85", "in.85", "lb.95", "up.95", "in.95")
  colnames(summaryTable.gauss)=c("leaveOut", "SPE", "krigeVar", "lb.75", "ub.75", "in.75", "lb.85", "up.85", "in.85", "lb.95", "up.95", "in.95")
  colnames(summaryTable.wave)=c("leaveOut", "SPE", "krigeVar", "lb.75", "ub.75", "in.75", "lb.85", "up.85", "in.85", "lb.95", "up.95", "in.95")
  
  summaryTableList<-list(exp=summaryTable.exp, gauss=summaryTable.gauss, wave=summaryTable.wave)
  
  MSPE.exp <- mean(summaryTable.exp[,2])
  MSPE.gauss <- mean(summaryTable.gauss[,2])
  MSPE.wave <- mean(summaryTable.wave[,2])
  
  percent.75.exp <- sum(summaryTable.exp[,6])/n.sites
  percent.85.exp <- sum(summaryTable.exp[,9])/n.sites
  percent.95.exp <- sum(summaryTable.exp[,12])/n.sites
  
  percent.75.gauss <- sum(summaryTable.gauss[,6])/n.sites
  percent.85.gauss <- sum(summaryTable.gauss[,9])/n.sites
  percent.95.gauss <- sum(summaryTable.gauss[,12])/n.sites
  
  percent.75.wave <- sum(summaryTable.wave[,6])/n.sites
  percent.85.wave <- sum(summaryTable.wave[,9])/n.sites
  percent.95.wave <- sum(summaryTable.wave[,12])/n.sites
  
  cvSummary.exp <- c(MSPE=MSPE.exp, percent.75=percent.75.exp, percent.85=percent.85.exp, percent.95=percent.95.exp)
  cvSummary.gauss <- c(MSPE=MSPE.gauss, percent.75=percent.75.gauss, percent.85=percent.85.gauss, percent.95=percent.95.gauss)
  cvSummary.wave <- c(MSPE=MSPE.wave, percent.75=percent.75.wave, percent.85=percent.85.wave, percent.95=percent.95.wave)

  cvSummaryList<-list(exp=cvSummary.exp, gauss=cvSummary.gauss, wave=cvSummary.wave)
  
  return(list(summaryTableList, cvSummaryList))
}

euclid_results <- euclidCv(aveChlor.euc, ini.cov.pars=c(0.17, 24), trend="1st", max.dist=75)

