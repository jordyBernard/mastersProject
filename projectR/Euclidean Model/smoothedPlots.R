# Module to create smoothed plots
my_plot_results <- function(values, ogs, borders, grays, name){
  my_lons <- seq(ogs$lonmin, ogs$lonmax, by=ogs$dxdy)
  my_lats <- seq(ogs$latmin, ogs$latmax, by=ogs$dxdy)
  my_grid <- pred_grid( c(ogs$lonmin,ogs$lonmax), c(ogs$latmin,ogs$latmax), by=ogs$dxdy )
  my_in_border <- point.in.polygon( my_grid[,1], my_grid[,2], borders[,1], borders[,2])
  my_results <- matrix(NA, nrow=length(my_lons), ncol=length(my_lats)) 
  my_results[my_in_border!=0] <- values
  image(my_results, col=my_grays, asp=2/0.6817, axes = F, main=name)
  contour(my_results, add=TRUE, labcex=1.2)
}

my_overlay_data_locs <- function(lonlats,mypch,col="black"){
  points( lonlats[,1],lonlats[,2], pch=mypch, col=col) 
}
