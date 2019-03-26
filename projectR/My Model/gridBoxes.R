# Module fills puget sound with grid boxes and make a few pretty pictures

## Fill Puget with grid boxes
## Note: In this implementation, the region is considered to be the grid boxes with centers lying on either water or land water boundaries

n_x<-100 # number of divisions in the latitudinal direction
base_x<-(pugetExtent[2]-pugetExtent[1])/n_x # the size of the base of each grid box
base_y<-base_x*(ratio) # to correct for latitiude
n_y<-(pugetExtent[4]-pugetExtent[3])/base_y # number of divisions in the longitudinal direction
gridBoxCenterCoords<-list()
gridBoxVerticesCoords<-list()
B_ij<-list() # this will be used to determine neighbor relationships
siteBox<-rep(NA, nrow(coords)) # to determine which gridbox a site is located in
xCoord_0<-pugetExtent[1]-base_x/2
yCoord<-pugetExtent[3]-base_y
k<-1
for (j in 1:n_y){
  xCoord<-xCoord_0
  yCoord<-yCoord+base_y
  for (i in 1:n_x){
    xCoord<-xCoord+base_x
    if(in.water(xCoord, yCoord)){
      B_ij[[k]]<-c(i,j)
      gridBoxCenterCoords[[k]]<-c(xCoord, yCoord)
      gridBoxVerticesCoords[[k]]<-rbind(c(xCoord-base_x/2, yCoord-base_y/2), c(xCoord+base_x/2, yCoord-base_y/2), c(xCoord+base_x/2, yCoord+base_y/2), c(xCoord-base_x/2, yCoord+base_y/2), c(xCoord-base_x/2, yCoord-base_y/2))
      for (l in 1:nrow(coords)){
        if(point.in.polygon(coords[l,1], coords[l,2], gridBoxVerticesCoords[[k]][,1], gridBoxVerticesCoords[[k]][,2])!=0){
          siteBox[l]<-k
        }
      }
      k<-k+1
    }
  }
}


## Make plot of center points of grid boxes
numGridBoxes<-length(gridBoxCenterCoords)
plot(pugetSpatPoly, asp=1/ratio, col=c("blue","green","green","green","green","green","green","green","green","green","green","green","green"))
for (i in 1:numGridBoxes){
  coord<-gridBoxCenterCoords[[i]]
  points(coord[1], coord[2], col="red", cex=.25)
}

## Make a plot of the grid boxes
plot(pugetSpatPoly,asp=1/ratio, col=c("blue","green","green","green","green","green","green","green","green","green","green","green","green")) # base plot for nice dimensions
polygon(regionCoords, col="green") # remove water
plotGridBoxes<-function(gridBoxCoords){
  for (i in 1:numGridBoxes){
    polygon(gridBoxCoords[[i]][,1],gridBoxCoords[[i]][,2], col = "blue")
  }
}
plotGridBoxes(gridBoxVerticesCoords)





