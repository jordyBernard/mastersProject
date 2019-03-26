# Module assigns neighbor relationships and plots a pretty picture

library("Matrix")

## Create neighbor relationships
neighborMatrix<-Matrix(0, nrow=numGridBoxes, ncol=numGridBoxes)
for (k_1 in 1:numGridBoxes){
  for (k_2 in 1:numGridBoxes){
    if (abs(B_ij[[k_1]][1]-B_ij[[k_2]][1])<=1){
      if(abs(B_ij[[k_1]][2]-B_ij[[k_2]][2])<=1){
        neighborMatrix[k_1, k_2]<-1
      }
    }
  }
  if (k_1%%1000==0){
    print(k_1)
  }
}

## Verify neighbor relationships with a pretty plot
plot(pugetSpatPoly, asp=1/0.6817, col=c("blue","green","green","green","green","green","green","green","green","green","green","green","green"))
for (i in 1:length(gridBoxCenterCoords)){
  coord<-gridBoxCenterCoords[[i]]
  points(coord[1], coord[2], pch=1, col="red", cex=.5)
}
for (k_1 in 1:numGridBoxes){
  for (k_2 in 1:numGridBoxes){
    if (abs(B_ij[[k_1]][1]-B_ij[[k_2]][1])<=1){
      if(abs(B_ij[[k_1]][2]-B_ij[[k_2]][2])<=1){
        x_1<-gridBoxCenterCoords[[k_1]][1]
        x_2<-gridBoxCenterCoords[[k_2]][1]
        y_1<-gridBoxCenterCoords[[k_1]][2]
        y_2<-gridBoxCenterCoords[[k_2]][2]
        lines(c(x_1,x_2), c(y_1,y_2), col="orange")
      }
    }
  }
}

