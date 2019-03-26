# Module approximates shortest in water distances (distances are stored in dist.mat)

library(raster)
library(gdistance)
library(geosphere)

x.min <- -123.3
x.max <- -122.1
y.min <- 47
y.max <- 49

x.part <- 100 # Control pixel size here
del.x <- (x.max-x.min)/x.part

y.part <- floor(((y.max-y.min)/ratio)/del.x) # To give square pixels
del.y <- (y.max-y.min)/y.part

#
r <- raster(nrows=y.part, ncols=x.part, xmn=x.min, ymn=y.min, xmx=x.max, ymx=y.max)

#
coord.x.not <- x.min+del.x/2
coord.y <- y.max-del.y/2
k<-1
for(i in 1:y.part){
  coord.x <- coord.x.not
  for(j in 1:x.part){
    if (in.water(coord.x, coord.y)){
      r[k] <- 1
    }else{
      r[k] <- 9999999 
    }
    coord.x <- coord.x+del.x
    k<-k+1  
  }
  coord.y<-coord.y-del.y
}

image(r, asp=1/0.6817, col=c("blue", "green"), xlab="", ylab="", axes=F)

tran <- transition(r, function(x) 1/mean(x), "8") 
tran<-geoCorrection(tran)

coords <- aveChlor$coords
n.sites<-nrow(coords)
dist.mat<-matrix(0,nrow=n.sites, ncol=n.sites)

for (i in 1:n.sites){
  c.1<-as.numeric(coords[i,])
  for (j in 1:n.sites){
    c.2<-as.numeric(coords[j,])
    if (c.1[1]!=c.2[1] && c.1[2]!=c.2[2]){
      sp <- shortestPath(tran, c.1, c.2, output="SpatialLines")
      dist <- SpatialLinesLengths(sp, longlat=T)
      dist.mat[i,j]<-dist
    }
  }
}

c.1<-as.numeric(coords[4,])
c.2<-as.numeric(coords[14,])
sp <-shortestPath(tran, c.1, c.2, output="SpatialLines")
lines(sp, col="red", lwd=4)
points(c(c.1[1],c.2[1]), c(c.1[2],c.2[2]), col="red", cex = 3.5, pch=19)
points(c(c.1[1],c.2[1]), c(c.1[2],c.2[2]), col="black", cex = 2.75, pch=19)
points(c(c.1[1],c.2[1]), c(c.1[2],c.2[2]), col="red", cex = 2, pch=4)

