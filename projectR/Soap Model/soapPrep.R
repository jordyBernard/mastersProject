# The model has more coefficients than data when we include islands, so I'm only using coastline as a boundary. 
coastCoords.conv <- coord.convert(coastCoords)
boundary <- list(list(x=coastCoords.conv[,1], y=coastCoords.conv[,2]))
names(boundary[[1]]) <- c("x.coord", "y.coord")

# When using a gridded approach to picking knots, those close to the boundaries don't work.
# Also with a grid approach, few to no knots on the southern end of puget sound.
# We'll play with the number that we end up using
plot(boundary[[1]]$x.coord, boundary[[1]]$y.coord, ty="l")
knots <- locator()
names(knots) <- c("x.coord","y.coord")

#Prepare aveChlor data#
chlor.coords <- coord.convert(aveChlor$coords)
chlor.df <- data.frame(chlor.coords[,1], chlor.coords[,2], aveChlor$data)
names(chlor.df) <- c("x.coord","y.coord","ave.chlor")

## Fit model
library(mgcv)
soap.mod <- gam(ave.chlor ~ s(x.coord, y.coord, bs = "so", xt = list(bnd = boundary)),
          data = chlor.df, method = "REML", knots = knots)

plot(soap.mod, asp = 1, xlim = c(0,91), ylim = c(0,225), se = FALSE, scheme = 2, main = "")








