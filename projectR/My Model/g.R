# Module to construct moving average functions

g_box<-function(x, theta_s, theta_r){
  return(rep(1,length(x)))
}

g_dl<-function(x, theta_s, theta_r){
  return(1-1/theta_r*(x-(theta_s)))
}
  
g_ul<-function(x, theta_s, theta_r){
  return(1/theta_r*(x-(theta_s-1)))
}
  
g_hoodoo<-function(x, theta_s, theta_r){
  y<-rep(0,length(x))
  if ((theta_r %% 2) == 0){
    for(i in 1:length(x)){
      if (x[i] <= (theta_s+theta_r/2-1)){
        y[i]<-2/theta_r*(x[i]-theta_s+1)
      }
      if (x[i] > (theta_s+theta_r/2-1)){
        y[i]<-1-2/theta_r*(x[i]-(theta_s+theta_r/2))
      }
    }
    return(y)
  }
  for(i in 1:length(x)){
    if (x[i] <= (theta_s+(theta_r-1)/2)){
      y[i]<-2/(theta_r+1)*(x[i]-theta_s+1)
    }
    if (x[i] > (theta_s+(theta_r-1)/2)){
      y[i]<-1-2/(theta_r+1)*(x[i]-(theta_s+(theta_r-1)/2))
    }
  }
  return(y)
}

## Some plots
par(mfrow=c(2,2))
theta_s <- 3
theta_r<- 5
x<-theta_s:(theta_s+theta_r-1)
y_box<-g_box(x, theta_s, theta_r)
y_dl<-g_dl(x, theta_s, theta_r)
y_ul<-g_ul(x, theta_s, theta_r)
y_hoodoo<-g_hoodoo(x, theta_s, theta_r)
plot(x,y_box, xlim=c(0,(theta_s+theta_r)), ylim=c(0,2), col="black", xlab="", ylab="",axes=F, cex=3,pch=19, main="Box")
lines(c(0,3),c(0.1,0.1), col="green",lwd=4)
lines(c(3,7),c(0.1,0.1), col="blue", lwd=4)
lines(c(3,3),c(0,1),col="red",lwd=4)
lines(c(0,8),c(0,0), lwd=4)
plot(x,y_dl, xlim=c(0,(theta_s+theta_r)), ylim=c(0,2), col="black",xlab="",ylab="",axes=F,cex=3, pch=19, main="Downward Linear")
lines(c(0,3),c(0.1,0.1), col="green",lwd=4)
lines(c(3,7),c(0.1,0.1), col="blue", lwd=4)
lines(c(3,3),c(0,1),col="red",lwd=4)
lines(c(0,8),c(0,0), lwd=4)
plot(x,y_ul, xlim=c(0,(theta_s+theta_r)), ylim=c(0,2), col="black",xlab="",ylab="",axes=F,cex=3, pch=19, main="Upward Linear")
lines(c(0,3),c(0.1,0.1), col="green",lwd=4)
lines(c(3,7),c(0.1,0.1), col="blue", lwd=4)
lines(c(7,7),c(0,1),col="red",lwd=4)
lines(c(0,8),c(0,0), lwd=4)
plot(x, y_hoodoo, xlim=c(0,(theta_s+theta_r)), ylim=c(0,2), col="black",xlab="",ylab="",axes=F,cex=3, pch=19, main="Hoodoo")
lines(c(0,3),c(0.1,0.1), col="green",lwd=4)
lines(c(3,7),c(0.1,0.1), col="blue", lwd=4)
lines(c(5,5),c(0,1),col="red",lwd=4)
lines(c(0,8),c(0,0), lwd=4)
