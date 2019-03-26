# Get ordinary and universal kriging weights
getLambdaOrd<-function(c, sigma){
  one<-matrix(1, nrow=length(c), ncol=1)
  return(t(t(c+one%*%((1-t(one)%*%solve(sigma)%*%c)/(t(one)%*%solve(sigma)%*%one)))%*%solve(sigma)))
}

getLambdaUni<-function(c, sigma, x, X){
  return(t(t(c+X%*%solve(t(X)%*%solve(sigma)%*%X)%*%(x-t(X)%*%solve(sigma)%*%c))%*%solve(sigma)))
}


# Calculate kriging variance
krigVarOrd<-function(lambda, c, sigma){
  one<-matrix(1, nrow=length(c), ncol=1)
  m<-(1-t(one)%*%solve(sigma)%*%c)/(t(one)%*%solve(sigma)%*%one)
  return(sigma[1,1]-t(lambda)%*%c+m)
}

krigVarUni<-function(lambda, c, sigma){
  temp<-0
  for (i in 1:length(c)){
    for (j in 1:length(c)){
      temp<-temp+lambda[i]*lambda[j]*sigma[i,j]
    }
  }
  return(sigma[1,1]-2*t(lambda)%*%c+temp)
}
