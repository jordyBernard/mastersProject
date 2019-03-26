# Create basic plot of puget sound and plot sites where water quality measurements were made

## Convert coastline coordinate data into spatial polygon object
coastPolys<-Polygons(list(Polygon(coastCoords)), ID="coastline")
landPolys<-Polygons(list(Polygon(landCoords)), ID="landline")
vashonPolys<-Polygons(list(Polygon(vashonCoords)), ID="vashon")
bainbridgePolys<-Polygons(list(Polygon(bainbridgeCoords)), ID="bainbridge")
morrowstonePolys<-Polygons(list(Polygon(marrowstoneCoords)), ID="marrowstone")
whidbeyPolys<-Polygons(list(Polygon(whidbeyCoords)), ID="whidbey")
lopezPolys<-Polygons(list(Polygon(lopezCoords)), ID="lopez")
guemesPolys<-Polygons(list(Polygon(guemesCoords)), ID="guemes")
sanJuanPolys<-Polygons(list(Polygon(sanJuanCoords)), ID="san Juan")
orcasPolys<-Polygons(list(Polygon(orcasCoords)), ID="orcas")
lumaPolys<-Polygons(list(Polygon(lumaCoords)), ID="luma")
andersonPolys<-Polygons(list(Polygon(andersonCoords)), ID="anderson")
horstinePolys<-Polygons(list(Polygon(horstineCoords)), ID="horstine")
pugetSpatPoly<-SpatialPolygons(list(coastPolys,landPolys,vashonPolys, bainbridgePolys, morrowstonePolys, whidbeyPolys, lopezPolys, guemesPolys, sanJuanPolys, orcasPolys, lumaPolys, andersonPolys, horstinePolys))

## Plot coastlines
plot(pugetSpatPoly, asp=1/0.6817, col=c("blue","green","green","green","green","green","green","green","green","green","green","green","green"))

## Add jan 2015 sites
points(jan2015$lon, jan2015$lat, pch=8,col = "red", cex = 2)

