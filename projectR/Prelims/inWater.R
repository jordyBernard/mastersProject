in.water<-function(xCoord, yCoord){
  in.water<-F
  if((point.in.polygon(xCoord, yCoord, regionCoords[,1], regionCoords[,2])!=0)&&
  (point.in.polygon(xCoord, yCoord, landCoords[,1], landCoords[,2])==0)&&
  (point.in.polygon(xCoord, yCoord, vashonCoords[,1], vashonCoords[,2])==0)&&
  (point.in.polygon(xCoord, yCoord, bainbridgeCoords[,1], bainbridgeCoords[,2])==0)&&
  (point.in.polygon(xCoord, yCoord, marrowstoneCoords[,1], marrowstoneCoords[,2])==0)&&
  (point.in.polygon(xCoord, yCoord, whidbeyCoords[,1], whidbeyCoords[,2])==0)&&
  (point.in.polygon(xCoord, yCoord, lopezCoords[,1], lopezCoords[,2])==0)&&
  (point.in.polygon(xCoord, yCoord, guemesCoords[,1], guemesCoords[,2])==0)&&
  (point.in.polygon(xCoord, yCoord, sanJuanCoords[,1], sanJuanCoords[,2])==0)&&
  (point.in.polygon(xCoord, yCoord, orcasCoords[,1], orcasCoords[,2])==0)&&
  (point.in.polygon(xCoord, yCoord, lumaCoords[,1], lumaCoords[,2])==0)&&
  (point.in.polygon(xCoord, yCoord, andersonCoords[,1], andersonCoords[,2])==0)&&
  (point.in.polygon(xCoord, yCoord, horstineCoords[,1], horstineCoords[,2])==0)){
    in.water<-T
  }
  return(in.water)
}
