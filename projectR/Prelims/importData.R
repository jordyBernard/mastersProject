##### Module imports puget sound water and coastline data #####

# import puget sound january 2015 water data
library(readxl)
jan2015 <- read_excel("/Users/jordy/Desktop/projectR/Data/jan2015.xlsx", sheet = "Main", na = "NA")

# import coastline data

## Load packages
library(rgdal)
library(raster)
library(rgeos)
library(sp)

## Get my coastlines from the interweb
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_coastline.zip", destfile="coastlines.zip")
unzip(zipfile="coastlines.zip", exdir="ne-coastlines-10m")
coastlines<-readOGR("ne-coastlines-10m/ne_10m_coastline.shp")
pugetExtent<-extent(-123.3, -122.1, 47, 49)
pugetCoastlines<-crop(coastlines, pugetExtent)

## Extract boundary coordinate data
regionCoords<-rbind(c(-123.3,47),c(-122.1,47),c(-122.1,49),c(-123.3,49),c(-123.3,47)) 
coastlineCoords<-coordinates(pugetCoastlines)[[1]][1][[1]]
coastCoords<-rbind(coastlineCoords, c(-123.3, 49), c(-123.3, 48.11692))
landCoords<-rbind(coastlineCoords, c(-122.1, 49), c(-122.1, 47), c(-123.3, 47), c(-123.3, 48.11692))
vashonCoords<-coordinates(pugetCoastlines)[[2]][[1]]
bainbridgeCoords<-coordinates(pugetCoastlines)[[3]][[1]]
marrowstoneCoords<-coordinates(pugetCoastlines)[[4]][[1]]
whidbeyCoords<-coordinates(pugetCoastlines)[[5]][[1]]
lopezCoords<-coordinates(pugetCoastlines)[[6]][[1]]
guemesCoords<-coordinates(pugetCoastlines)[[7]][[1]]
sanJuanCoords<-coordinates(pugetCoastlines)[[8]][[1]]
orcasCoords<-coordinates(pugetCoastlines)[[10]][[1]]
lumaCoords<-coordinates(pugetCoastlines)[[11]][[1]]
andersonCoords<-coordinates(pugetCoastlines)[[13]][[1]]
horstineCoords<-coordinates(pugetCoastlines)[[14]][[1]]

