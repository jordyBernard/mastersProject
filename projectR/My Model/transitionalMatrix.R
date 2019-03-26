# Modeule creates transitional matrix T

## Choose parameter values
M<-0.5
maxQ_i<-8

tranProbMat<-Matrix(0, nrow=numGridBoxes, ncol=numGridBoxes) # Instantiate Transitional Probability Matrix T

## Fill off diagonal entries of transitional probability matrix T
offDiagProbFunction<-function(M, maxQ_i){
  return(M*(1/maxQ_i))
}
offDiagProb<-offDiagProbFunction(M, maxQ_i)
tranProbMat<-offDiagProb*neighborMatrix

## Fill diagonal entries of transitional probability matrix T
q_i<-rep(NA, numGridBoxes)
for(i in 1:numGridBoxes){
  q_i[i]<-sum(neighborMatrix[i,])-1
}
diagTranProbFunction<-function(M, maxQ_i){
  return(1-M*(q_i/maxQ_i))
}
diagTranProb<-diagTranProbFunction(M, maxQ_i)
diag(tranProbMat)<-diagTranProb

