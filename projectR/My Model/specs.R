getSpecChains <- function(mod.num){
  if(mod.num==1){
    g <- g_box
    order <- 0
  }else if (mod.num==2){
    g <- g_ul
    order <- 0
  }else if (mod.num==3){
    g <- g_dl
    order <- 0
  }else if (mod.num==4){
    g <- g_hoodoo
    order <- 0
  }else if (mod.num==5){
    g <- g_box
    order <- 1
  }else if (mod.num==6){
    g <- g_ul
    order <- 1
  }else if (mod.num==7){
    g <- g_dl
    order <- 1
  }else{
    g <- g_hoodoo
    order <- 1
  }
  
  contAndA <- getContrastAndA(22, aveChlor, order=order, leaveOut=0)
  cont <- contAndA[[1]]
  A <- contAndA[[2]]
  objChain <- getObjChain(cont, A, maxTheta_s=2000, maxTheta_r=8000, thinBy=400, times=rep(0,22), g=g, leaveOut=0)
  return(objChain)
}

library(parallel)
no.cores <- detectCores()-1
cluster <- makeCluster(no.cores, type="FORK")
specChains <- parLapply(cluster, 1:8, getSpecChains)
stopCluster(cluster)

write.table(specChains[[1]], file="~/Desktop/gBox0", sep=" ")
write.table(specChains[[2]], file="~/Desktop/gUl0", sep=" ")
write.table(specChains[[3]], file="~/Desktop/gDl0", sep=" ")
write.table(specChains[[4]], file="~/Desktop/gHoodoo0", sep=" ")
write.table(specChains[[5]], file="~/Desktop/gBox1", sep=" ")
write.table(specChains[[6]], file="~/Desktop/gUl1", sep=" ")
write.table(specChains[[7]], file="~/Desktop/gDl1", sep=" ")
write.table(specChains[[8]], file="~/Desktop/gHoodoo1", sep=" ")

# graphs and shit
chain <- specChains[[1]] # XX pick the chain you want here XX 
order <- 0 # will need to change order here
g <- g_box # change g here
theta_s <- chain[which(chain[,3]==min(chain[,3])), 1]
theta_r <- chain[which(chain[,3]==min(chain[,3])), 2]
contAndA <- getContrastAndA(22, aveChlor, order=order, leaveOut=0) 
cont <- contAndA[[1]]
A <- contAndA[[2]]
getTheta_star<-function(c, A, psi){
  n.minus.r<-length(c)
  return(1/(n.minus.r)*t(c)%*%solve(t(A)%*%psi%*%A)%*%c)
}
psiMat <- getPsiMat(theta_s, theta_r, g, thinBy = 400, nSites=22, times=rep(0,22))
theta_star <- getTheta_star(cont, A, psiMat) 
sigma<-function(theta_star, phi){
  return(theta_star[1,1]*phi)
}
sigma <- sigma(theta_star, psiMat)


# Pretty plots
library(scatterplot3d)
cols<-rep("red",nrow(chain))
minVal<-min(chain[,3])
third<-(max(chain[,3])-min(chain[,3]))/3
for (i in 1:nrow(chain)){
  if (chain[i,3]<=(minVal+2*third)){
    cols[i]<-"orange"
  }
  if (chain[i,3]<=(minVal+third)){
    cols[i]<-"yellow"
  }
  if (chain[i,3]==minVal){
    cols[i]<-"blue"
  }
}

scatterplot3d(chain[,1], chain[,2], chain[,3], col.axis="blue", col.grid = "lightblue",pch=20,highlight.3d=TRUE, color=cols, xlab=expression(theta[s]), ylab=expression(theta[r]), zlab=expression(obj))
scatterplot3d(chain[,2], chain[,1], chain[,3], col.axis="blue", col.grid = "lightblue",pch=20, color=cols, xlab=expression(theta[s]), ylab=expression(theta[r]), zlab=expression(obj), type="h")

# everything looks good with the exception of g_dl
r.mat <- matrix(NA, nrow=8, ncol=2)
rownames(r.mat) <- c("box.0","ul.0","dl.0","hoodoo.0","box.1","ul.1","dl.1","hoodoo.1")
colnames(r.mat) <- c("theta_s", "theta_r")
for (i in 1:8){
  theta_s <- specChains[[i]][which(specChains[[i]][,3]==min(specChains[[i]][,3])), 1]
  theta_r <- specChains[[i]][which(specChains[[i]][,3]==min(specChains[[i]][,3])), 2]
  r.mat[i,1] <- theta_s
  r.mat[i,2] <- theta_r
}
r.mat

# use max.theta_r=1000 and max.theta_s=10000


