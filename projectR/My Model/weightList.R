getWeightList<-function(maxTransitions, thinBy=1){
  a<-Sys.time()
  numSites<-length(siteBox)
  weights<-matrix(0, nrow=numGridBoxes, ncol=numSites)
  for (i in 1:numSites){
    weights[siteBox[i],i]<-1
  }
  weights<-as(weights, "sparseMatrix")
  tranProbMat<-as(tranProbMat, "sparseMatrix")
  weightList<-list(weights)
  k<-2
  for (i in 2:(maxTransitions+1)){
    weights<-tranProbMat%*%weights
    if (((i-1)%%thinBy)==0){
      weightList[[k]]<-weights
      k<-k+1
    }
    if (i%%1000==0){
      print(i/maxTransitions)
    }
  }
  b<-Sys.time()
  print(b-a)
  return(weightList)
}

weightList<-getWeightList(8000)


