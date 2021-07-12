library(raster)
library(rgdal)
library(rgeos)
library(stringr)

list = list.files(path = "~/module_results/bfast/",pattern = glob2rx("bfast_outputs_*.vrt"),recursive = T)
dev.off()
par(mfrow = c(6,7))
par(mar=c(0,0,0,0))
for(vrt in list){
  vrt_basename <- str_split_fixed(vrt,"/",2)[1]
  print(vrt_basename)
  plot(raster(paste0("~/module_results/bfast/",vrt)),axes=F,legend=F)
  title(main=vrt_basename,font.main=1,cex.main=1,line=-3,adj=0.05)