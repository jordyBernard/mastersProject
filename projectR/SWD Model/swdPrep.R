# Module estimates parameters for swd model

Q <- function(sig.sq, phi, swd.data){
  emp.var <- robust.variog.est(swd.data)
  n_k <- emp.var$n.k
  est <- emp.var$est
  h_k <- emp.var$bin.center
  Q <- 0
  for(i in 1:length(n_k)){
    Q <- Q + n_k[i]*(est[i]-exp.variog(h_k[i], sig.sq, phi))^2 
  }
  return(Q)
}
  
exp.variog <- function(h, sig.sq, phi){
  return(sig.sq*(1-exp(-h/phi)))
}

robust.variog.est <- function(swd.data, max.dist=max(dist.mat), num.bins=13){
  bin.width <- max.dist/num.bins
  bin.divs <- rep(0,(num.bins+1))
  bin.center <- rep(0,num.bins)
  for(j in 2:(num.bins+1)){
    bin.divs[j]<-bin.divs[j-1] + bin.width
    bin.center[j-1]<-(bin.divs[j-1]+bin.divs[j])/2 
  }
  n.k <- rep(NA, num.bins)
  est <- rep(NA, num.bins)
  for (i in 1:num.bins){
    lb <- bin.divs[i]
    ub <- bin.divs[i+1]
    in.bin <- as.numeric(swd.data[,1] > lb)+ as.numeric(swd.data[,1] <= ub)==2
    n.k[i] <- sum(in.bin)
    diffs<-swd.data[in.bin, 2]
    est[i] <- (1/n.k[i]*sum(abs(diffs)^0.5))^4 / (0.457+0.494/n.k[i])
  }
  return(list(bin.center=bin.center, n.k=n.k, est=est))
}

swd.data <- function(dist.mat, data){
  n.locs <- nrow(dist.mat)
  u.length <- 0
  for(i in 1:(n.locs-1)){
    u.length<-u.length+i
  }
  u<-rep(NA, u.length)
  k<-1
  for(i in 1:(n.locs-1)){
    u[k:(k+n.locs-i-1)]<-dist.mat[(i+1):n.locs, i]
    k<-k+(n.locs-i)
  }
  k<-1
  diffs<-rep(NA, u.length)
  for(i in 1:(n.locs-1)){
    for(j in (i+1):n.locs){
      diffs[k] <- data[i]-data[j] 
      k<-k+1
    }
  }
  return(data.frame(u=u, diffs=diffs))
}


## Don't need what's below for CV ##

swd.data.chlor <- swd.data(dist.mat, aveChlor$data)

Q.chlor <- function(params){
  sig.sq<-params[1]
  phi<-params[2]
  return(Q(sig.sq, phi, swd.data.chlor))
}  

# Get initial values
est <- robust.variog.est(swd.data.chlor)
plot(est$bin.center, est$est, cex=est$n.k/sum(est$n.k)*20) # guess sig.sq=0.4 , phi=100

# Optimize
optim(c(0.4, 100), lower=c(0,0), fn=Q.chlor, method ="L-BFGS-B")


# See if it's reasonable
x<-seq(0,200,length=200)
y<-exp.variog(x,0.2422197,24.8406104)
lines(x,y)

optim(c(5, 30), lower=c(0,0), fn=Q.chlor, method ="L-BFGS-B")
