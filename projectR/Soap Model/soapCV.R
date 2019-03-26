soap.CV <- function(data, boundary, knots){
  n.sites <- nrow(data)
  summaryTable <- matrix(NA, nrow=n.sites, ncol=12)
  for (leave.out in 1:n.sites){
    data.lo <- data[-leave.out,]
    p.data <- list(x.coord=data[leave.out,1], y.coord=data[leave.out,2])
    soap.mod <- gam(data[,3] ~ s(x.coord, y.coord, bs = "so", xt = list(bnd = boundary)),
                    data = data, method = "REML", knots = knots)
    # MSPE
    prediction <- predict(soap.mod, newdata = p.data)
    spe <- (data[leave.out,3] - prediction)^2
    
    # Pred Bounds
    pred.SE <- sqrt(soap.mod$sig2)
    
    lb.75 <- prediction-qnorm(0.875)*pred.SE
    ub.75 <- prediction+qnorm(0.875)*pred.SE
    lb.85 <- prediction-qnorm(0.925)*pred.SE
    ub.85 <- prediction+qnorm(0.925)*pred.SE
    lb.95 <- prediction-qnorm(0.975)*pred.SE
    ub.95 <- prediction+qnorm(0.975)*pred.SE
    
    leave.out.data <- data[leave.out, 3]
    
    in.75 <- ((leave.out.data >= lb.75) && (leave.out.data <= ub.75))
    in.85 <- ((leave.out.data >= lb.85) && (leave.out.data <= ub.85))
    in.95 <- ((leave.out.data >= lb.95) && (leave.out.data <= ub.95))
    
    summaryVector <- c(leave.out, spe, pred.SE, lb.75, ub.75, in.75, lb.85, ub.85, in.85, lb.95,
                           ub.95, in.95)
    summaryTable[leave.out,] <- summaryVector
  }
  
  MSPE <- mean(summaryTable[,2])
  
  percent.75<- sum(summaryTable[,6])/n.sites
  percent.85 <- sum(summaryTable[,9])/n.sites
  percent.95 <- sum(summaryTable[,12])/n.sites
  
  cvSummary <- c(MSPE=MSPE, percent.75=percent.75, percent.85=percent.85, percent.95=percent.95)
  
  return(list(summaryTable , cvSummary))
}

soap.CV(chlor.df, boundary, knots)

