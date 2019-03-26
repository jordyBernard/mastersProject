# Function to fill in entries of psiMat       
psiFun<-function(site_1, site_2, theta_s, t_1=0, t_2=0, theta_r=1, g=g_box, thinBy=1){
  rVal<-0
  for(t_3 in max((t_1+theta_s), (t_2+theta_s)):min((t_1+theta_s+theta_r-1),(t_2+theta_s+theta_r-1))){
    if(((t_3-t_1)%%thinBy)==0 && ((t_3-t_2)%%thinBy)==0){
      g_1<-g((t_3-t_1), theta_s, theta_r)
      g_2<-g((t_3-t_2), theta_s, theta_r)
      w_1<-weightList[[(t_3-t_1)/thinBy+1]][,site_1]
      w_2<-weightList[[(t_3-t_2)/thinBy+1]][,site_2]
      rVal<-rVal+g_1*g_2*(sqrt(w_1)%*%sqrt(w_2))
    }
  }
  return(rVal)
}

# function to compute psiMat
psiMat<-function(theta_s, theta_r=1, g=g_box, thinBy=1, nSites=23, times=rep(0,23), leaveOut=0){
  psiMat<-matrix(NA, nrow=nSites, ncol=nSites)
  for (j in 1:nSites){
    for (k in 1:nSites){
      psiMat[j,k]<-psiFun(j, k, theta_s, t_1=times[j], t_2=times[k], theta_r=theta_r, g=g, thinBy=thinBy)
    }
  }
  if (leaveOut > 0){
    psiMat<-psiMat[-leaveOut, -leaveOut]
  }
  return(psiMat)
}
