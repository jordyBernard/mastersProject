# Compute latitudinal correction
lat.1<-47
lon.1<--123.3
lat.2<-49
lon.2<--122.1
earth.rad<-3440
earth.circ<-21639
rad<-earth.rad*cos(mean(lat.1,lat.2)*pi/180)
circ<-2*pi*rad
ratio<-circ/earth.circ
