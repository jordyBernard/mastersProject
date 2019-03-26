# Function returns error contrasts and matrix A for REML step

getContrastAndA<-function(numSites, variable, order=0, leaveOut=0){
  if (leaveOut==0){
    I<-diag(rep(1, numSites))
    if (order==0){
      X<-rep(1,numSites)
      P<-X%*%solve(t(X)%*%X)%*%t(X)
    }
    if (order==1){
      X<-cbind(1, variable$coords)
      P<-X%*%solve(t(X)%*%X)%*%t(X)
    }
    contrast<-((I-P)%*%variable$data)[findLinIndRowIndexes(I-P)]
    A<-t((I-P)[findLinIndRowIndexes(I-P),])
  }else if(leaveOut>0){
    I<-diag(rep(1, numSites-1))
    if (order==0){
      X<-rep(1,numSites-1)
      P<-X%*%solve(t(X)%*%X)%*%t(X)
    }
    if (order==1){
      X<-cbind(1, variable$coords[-leaveOut,])
      P<-X%*%solve(t(X)%*%X)%*%t(X)
    }
    contrast<-((I-P)%*%variable$data[-leaveOut])[findLinIndRowIndexes(I-P)]
    A<-t((I-P)[findLinIndRowIndexes(I-P),])
  }
  return(list(contrast, A))
}

