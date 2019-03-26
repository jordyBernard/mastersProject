# basic diagnostic plots

## load packages
library(geoR)

## use geoR's plot function to help diagnose if we want to use a spatial model and include trend terms

### create data frame for coordinates
coords<-data.frame(jan2015$lon, jan2015$lat)

### for surface temperature
surTempDF<-data.frame(jan2015$lon, jan2015$lat, jan2015$surTemp)
surTemp<-as.geodata(surTempDF)
surTemp$borders<-coastCoords
plot(surTemp)

### for average temperature
aveTempDF<-data.frame(jan2015$lon, jan2015$lat, jan2015$aveTemp)
aveTemp<-as.geodata(aveTempDF)
aveTemp$borders<-coastCoords
plot(aveTemp)

### for bottom temperature
botTempDF<-data.frame(jan2015$lon, jan2015$lat, jan2015$botTemp)
botTemp<-as.geodata(botTempDF)
botTemp$borders<-coastCoords
plot(botTemp)

### for surface salinity
surSalDF<-data.frame(jan2015$lon, jan2015$lat, jan2015$surSal)
surSal<-as.geodata(surSalDF)
surSal$borders<-coastCoords
plot(surSal)

### for average salinity
aveSalDF<-data.frame(jan2015$lon, jan2015$lat, jan2015$aveSal)
aveSal<-as.geodata(aveSalDF)
aveSal$borders<-coastCoords
plot(aveSal)

### for bottom salinity
botSalDF<-data.frame(jan2015$lon, jan2015$lat, jan2015$botSal)
botSal<-as.geodata(botSalDF)
botSal$borders<-coastCoords
plot(botSal)

### for surface chlorophyll
surChlorDF<-data.frame(jan2015$lon, jan2015$lat, jan2015$surChlor)[-2,]
surChlor<-as.geodata(surChlorDF)
surChlor$borders<-coastCoords
plot(surChlor)

### for average chlorophyll
aveChlorDF<-data.frame(jan2015$lon, jan2015$lat, jan2015$aveChlor)[-2,]
aveChlor<-as.geodata(aveChlorDF)
aveChlor$borders<-coastCoords
plot(aveChlor)


### for bottom chlorophyll
botChlorDF<-data.frame(jan2015$lon, jan2015$lat, jan2015$botChlor)[-2,]
botChlor<-as.geodata(botChlorDF)
botChlor$borders<-coastCoords
plot(botChlor)

### for trimmed average chlorophyll
aveChlorTrimDF<-data.frame(jan2015$lon, jan2015$lat, jan2015$aveChlorTrim)[-2,]
aveChlorTrim<-as.geodata(aveChlorTrimDF)
aveChlorTrim$borders<-coastCoords
plot(aveChlorTrim)

### for surface dissolved oxygen
surDisOxyDF<-data.frame(jan2015$lon, jan2015$lat, jan2015$surDisOxy)
surDisOxy<-as.geodata(surDisOxyDF)
surDisOxy$borders<-coastCoords
plot(surDisOxy)

### for average dissolved oxygen
aveDisOxyDF<-data.frame(jan2015$lon, jan2015$lat, jan2015$aveDisOxy)
aveDisOxy<-as.geodata(aveDisOxyDF)
aveDisOxy$borders<-coastCoords
plot(aveDisOxy)

### for bottom dissolved oxygen
botDisOxyDF<-data.frame(jan2015$lon, jan2015$lat, jan2015$botDisOxy)
botDisOxy<-as.geodata(botDisOxyDF)
botDisOxy$borders<-coastCoords
plot(botDisOxy)

### for surface adjusted dissolved oxygen
surAdjDisOxyDF<-data.frame(jan2015$lon, jan2015$lat, jan2015$surAdjDisOxy)[-2,]
surAdjDisOxy<-as.geodata(surAdjDisOxyDF)
surAdjDisOxy$borders<-coastCoords
plot(surAdjDisOxy)

### for average adjusted dissolved oxygen
aveAdjDisOxyDF<-data.frame(jan2015$lon, jan2015$lat, jan2015$aveAdjDisOxy)[-2,]
aveAdjDisOxy<-as.geodata(aveAdjDisOxyDF)
aveAdjDisOxy$borders<-coastCoords
plot(aveAdjDisOxy)

### for bottom adjusted dissolved oxygen
botAdjDisOxyDF<-data.frame(jan2015$lon, jan2015$lat, jan2015$botAdjDisOxy)[-2,]
botAdjDisOxy<-as.geodata(botAdjDisOxyDF)
botAdjDisOxy$borders<-coastCoords
plot(botAdjDisOxy)



## Compare the emperical variogram to the emperical variograms associated with shuffle data to determine if we need a (standard) spatial model.

### Function to plot emperical variograms
needSpatMod<-function(geoData, lambda, trend){
  geoDataVariog<-variog(geoData, lambda=lambda, trend=trend, estimator.type="modulus")
  plot(geoDataVariog, type="l")
  n<-nrow(geoData$coords)
  for (i in 1:25){
    newOrder<-sample(1:n, n, replace=F)
    reordered<-geoData$data[newOrder]
    reorderedGeoData<-as.geodata(cbind(geoData$coords, reordered))
    newVariog<-variog(reorderedGeoData, lambda=lambda, trend=trend, estimator.type="modulus")
    lines(newVariog, col="gray")
  }
  lines(geoDataVariog, lwd=3 )
}

### Make plots for each variable

#### for surface temperature
needSpatMod(surTemp, 1, "cte")
needSpatMod(surTemp, 1, "1st")

#### for average temperature
needSpatMod(aveTemp, 1, "cte")
needSpatMod(aveTemp, 1, "1st")

### for bottom temperature
needSpatMod(botTemp, 1, "cte")
needSpatMod(botTemp, 1, "1st")

#### for bottom temperature
needSpatMod(botSal, 1, "cte")
needSpatMod(botSal, 1, "1st")

#### for surface salinity
needSpatMod(surSal, 1, "cte")
needSpatMod(surSal, 1, "1st")

#### for average salinity
needSpatMod(aveSal, 1, "cte")
needSpatMod(aveSal, 1, "1st")

#### for bottom salinity
needSpatMod(botSal, 1, "cte")
needSpatMod(botSal, 1, "1st")

#### for surface chlorophyll
needSpatMod(surChlor, 1, "cte")
needSpatMod(surChlor, 1, "1st")

#### for average chlorophyll
needSpatMod(aveChlor, 1, "cte")
needSpatMod(aveChlor, 1, "1st")

#### for bottom chlorophyll
needSpatMod(botChlor, 1, "cte")
needSpatMod(botChlor, 1, "1st")

#### for trimmed average chlorophyll
needSpatMod(aveChlorTrim, 1, "cte")
needSpatMod(aveChlorTrim, 1, "1st")

#### for surface dissolved oxygen
needSpatMod(surDisOxy, 1, "cte")
needSpatMod(surDisOxy, 1, "1st")

#### for average dissolved oxygen
needSpatMod(aveDisOxy, 1, "cte")
needSpatMod(aveDisOxy, 1, "1st")

#### for bottom dissolved oxygen
needSpatMod(botDisOxy, 1, "cte")
needSpatMod(botDisOxy, 1, "1st")

#### for surface adjusted dissolved oxygen
needSpatMod(surAdjDisOxy, 1, "cte")
needSpatMod(surAdjDisOxy, 1, "1st")

#### for average adjusted dissolved oxygen
needSpatMod(aveAdjDisOxy, 1, "cte")
needSpatMod(aveAdjDisOxy, 1, "1st")

#### for bottom adjusted dissolved oxygen
needSpatMod(botAdjDisOxy, 1, "cte")
needSpatMod(botAdjDisOxy, 1, "1st")

