# Module to fit euclidean models and justify choices for variograms

# Plot variogram clouds as well as loess curve
variog.cloud.plot <- function(geodata, trend="cte"){
  geodata.variog <- variog(geodata, trend=trend, option="cloud")
  plot(geodata.variog)
  u<-geodata.variog$u
  v<-geodata.variog$v
  geodata.loess <- loess(v~u)
  new_u <- seq(from=min(u), max(u), length=100)
  fitted <- predict(geodata.loess, data.frame(u = new_u))
  lines(new_u, fitted)
} 

variog.cloud.plot(aveChlor.euc)

###################################################################################


# return and plot robust estimate of variogram
# ... trend="cte", estimator.type="classical", max.dist=largest dist between sites
variog.est <- function(geodata, ...){
  variog.estimate <- variog(geodata, ...)
  plot(variog.estimate, pts.range=c(1,3), cex.lab=1.6, cex.axis=1.5, cex.main=1.5, type="b")
  return(variog.estimate)
}

# A Ron Question -- where to clip -- i.e. max.dist argument
# I think we will ditch aveTemp and aveSal
variog.est(aveChlor.euc, trend="cte", estimator.type="modulus")



###################################################################################


# Fit variogram using WLS for aveChlor.euclid

library(tcltk) # For eyefit

aveChlor.euc.var.est <- variog.est(aveChlor.euc, trend="cte", estimator.type="modulus", max.dist=75) # Emperical variogram

# eyefit(aveChlor.euc.var.est) #sigsq=0.17, phi=24

aveChlor.euc.exp <- variofit(aveChlor.euc.var.est, 
                                ini.cov.pars=c(0.17, 24),
                                cov.model="exponential", 
                                fix.nugget=T,
                                max.dist = 75)

aveChlor.euc.gauss <- variofit(aveChlor.euc.var.est, 
                                  ini.cov.pars=c(0.17, 24),
                                  cov.model="gaussian", 
                                  fix.nugget=T,
                                  max.dist = 75)


aveChlor.euc.wave <- variofit(aveChlor.euc.var.est, 
                                  ini.cov.pars=c(0.17, 24),
                                  cov.model="wave", 
                                  fix.nugget=T,
                                  max.dist = 75)


par(mfrow=c(1,1))

#1
plot(variog(aveChlor.euc, option="cloud")$u, variog(aveChlor.euc, option="cloud")$v,  cex=1, pch=19, col="red", xlab="distance", ylab="semivariance")
#2
variog.est(aveChlor.euc, trend="cte", estimator.type="modulus", max.dist=75, col="red") # Emperical variogram
points(variog(aveChlor.euc, option="cloud")$u, variog(aveChlor.euc, option="cloud")$v,  cex=1, pch=19, col="red")
#3
variog.est(aveChlor.euc, trend="cte", estimator.type="modulus", max.dist=75, col="red") # Emperical variogram
lines(aveChlor.euc.exp, col="red", lwd=8)
lines(aveChlor.euc.gauss, lty=2, col="blue",lwd=8)
lines(aveChlor.euc.wave, lty=3, col="green", lwd=9)
legend(-2.832349, 0.2262061, legend=c("Exponential", "Guassian", "Wave"),
       col=c("red", "blue", "green"), lty=1:3, lwd=8, cex=0.8)
#4
plot(1, 1, col="white", xlim=c(0,70), ylim=c(0,.21), xlab="distance", ylab="semivariance")
lines(aveChlor.euc.exp, col="black", lwd=8)
lines(c(-10, 80), c(0,0), col="red", lwd=8)
lines(c(64.43902, 64.43902), c(0,0.1414), col="green", lwd=8)
lines(c(-10, 80), c( 0.1414, 0.1414), col="blue", lwd=8)
legend(-2.832349, 0.2180749, legend=c("Nugget = 0", "Sill = 0.14", "Asymptotic Range = 64.4"),
       col=c("red", "blue", "green"), lty=1,lwd=8,cex=0.8)




plot(1, 1, col="white", xlim=c(0,70), ylim=c(0,.21), xlab="distance", ylab="semivariance")
lines(aveChlor.euc.exp, col="black", lwd=4)
lines(c(-10, 80), c(0,0), col="red", lwd=4)
lines(c(64.43902, 64.43902), c(0,0.1414), col="green", lwd=4)
lines(c(-10, 80), c( 0.1414, 0.1414), col="blue", lwd=4)
legend(-2.832349, 0.2262061, legend=c("Nugget = 0", "Sill = 0.14", "Asymptotic Range = 64.4"),
       col=c("red", "blue", "green"), lty=1,cex=0.8)

variog.est(aveChlor.euc, trend="cte", estimator.type="modulus", max.dist=75, col="red") # Emperical variogram
points(variog(aveChlor.euc, option="cloud")$u, variog(aveChlor.euc, option="cloud")$v,  cex=1, pch=19, col="red")
plot(variog(aveChlor.euc, option="cloud")$u, variog(aveChlor.euc, option="cloud")$v,  cex=1, pch=19, col="red", xlab="distance", ylab="semivariance")


###################################################################################


# Create a pretty smoothed map for aveChlor.euclid
lonmin <- 0
lonmax <- 111*ratio*(-122.1+123.3)
latmin<- 0
latmax <- 111*(49-47)
dxdy=1

ogs <- list(lonmin=lonmin, lonmax=lonmax, latmin=latmin, latmax=latmax, dxdy=dxdy)

kr.grid <- pred_grid(c(lonmin, lonmax), c(latmin, latmax), by=dxdy)

kr.obj <- krige.control(type.krige="OK",
                        trend.d = "cte",
                        cov.model="exponential", 
                        cov.pars=aveChlor.euc.exp$cov.pars,
                        nugget=0)

kr.results <- krige.conv(aveChlor.euc, krige=kr.obj, locations=kr.grid)

my_grays <- gray(seq(25/63, 59/63, length=40)) 
my_grays[length(my_grays)] <- "white"

par(mfrow=c(1,1))

kr.pred<-kr.results$predict
im<-my_plot_results(kr.pred, ogs, aveChlor.euc$borders, grays, "Predictive Surface")
my_overlay_data_locs(aveChlor.euc$coords, 10, col="red")

kr.sd<-sqrt(kr.results$krige.var)
my_plot_results(2*1.96*kr.sd, ogs, aveChlor.euc$borders, grays, "PI Width")
my_overlay_data_locs(aveChlor.euc$coords, 10, col="red")

my_plot_results(kr.pred-1.96*kr.sd, ogs, aveChlor.euc$borders, grays, "Low 2.5")
my_overlay_data_locs(aveChlor.euc$coords, 10, col="red")

my_plot_results(kr.pred+1.96*kr.sd, ogs, aveChlor.euc$borders, grays, "High 97.5" )
my_overlay_data_locs(aveChlor.euc$coords, 10, col="red")

