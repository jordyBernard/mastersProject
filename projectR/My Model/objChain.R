# Module to estimate parameters
is.invertable <- function(m){
  class(try(solve(m),silent=T))=="matrix"
} 

objective <- function(c, A, psi){
  if (is.invertable(t(A)%*%psi%*%A)){
    n.minus.r<-length(c)
    rVal<-(n.minus.r)*log(t(c)%*%solve(t(A)%*%psi%*%A)%*%c)+log(det(t(A)%*%psi%*%A))
    return(rVal)
  }else{
    print("fuck")
    return(99999)
  }
}

getObjChain<-function(cont, A, maxTheta_s=100, maxTheta_r=1000, thinBy=1, times=rep(0,23), g=g_box, leaveOut=0){
  a<-Sys.time()
  chainLength<-(floor(maxTheta_s/thinBy)+1)*floor(maxTheta_r/thinBy)
  objChain<-matrix(NA, nrow=chainLength, ncol=3)
  k<-1
  nObs<-length(times)
  for(theta_r in seq(thinBy, maxTheta_r, by=thinBy)){
    for (theta_s in seq(0, maxTheta_s, by=thinBy)){
      
      objChain[k,]<-cbind(theta_s, theta_r, objective(cont, A, psiMat(theta_s, theta_r, g, thinBy, nObs, times, leaveOut=leaveOut)))
      if (k%%10==0){
        print(k/chainLength)
      }
      k<-k+1
    }
  }
  b<-Sys.time()
  print(b-a)
  return(objChain)
}

