# Given a matrix, find a set of linearly independent rows
# Probably a cleaner way to do it, but module finds a set of linearly independent rows for REML step. 

# library(Matrix)
findLinIndRowIndexes<-function(M){
  ranks<-c(1, rep(NA,(nrow(M)-1)))
  for (i in 2:nrow(M)){
    A<-M[1:i,]
    ranks[i]<-rankMatrix(A, method="maybeGrad")
  }
  keeperIndexes<-c(1)
  k<-2
  for (i in 2:length(ranks)){
    if (ranks[i]!=ranks[i-1]){
      keeperIndexes[k]<-i
      k<-k+1
    }
  }
  return(keeperIndexes)
}
