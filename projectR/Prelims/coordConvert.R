# I bet there's a package to do this, but this module does some coordinate conversion so that we're looking at a euclidean plane

coord.convert <- function(coord.mat){
  return.coords <- matrix(NA, nrow=nrow(coord.mat), ncol=2)
  return.coords[,1] <- coord.mat[,1] + 123.3
  return.coords[,2] <- coord.mat[,2] - 47
  return.coords[,1] <- ratio * return.coords[,1]
  return.coords <- 111 * return.coords
  return(return.coords)
}

new.coords <- coord.convert(aveChlor$coords)
aveChlor.df <- data.frame(new.coords[,1], new.coords[,2], aveChlor$data)
aveChlor.euc <- as.geodata(aveChlor.df)
new.borders <- coord.convert(aveChlor$borders)
aveChlor.euc$borders <- new.borders


