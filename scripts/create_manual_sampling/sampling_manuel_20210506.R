####################################################################################################
################################################## PACKAGES & OPTIONS
options(stringsAsFactors = F)

packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}
## Packages for geospatial data handling
packages(raster)
packages(rgeos)
packages(rgdal)

packages(snow)
packages(htmltools)
packages(devtools)
packages(gdalUtils)


## Packages for data table handling
packages(xtable)
packages(DT)
packages(dismo)
packages(stringr)
packages(plyr)

## Packages for graphics and interactive maps
packages(ggplot2)
packages(leaflet)
packages(RColorBrewer)
packages(rclipboard)

####################################################################################################
datadir <- "/home/diaf/diaf_aa_2000_2010_2014/data/cartes/"
setwd(datadir)

map_name <- paste0(datadir,"diaf_00_18_provinces_20210505.tif")
rp_name  <- paste0(datadir,"sae_design_diaf_00_18_provinces_20210505/manual_sampling.csv")

map <- raster(map_name)
rp  <- read.csv(rp_name)

rand_sample <-   data.frame(sampleRandom(map, 10, xy = TRUE))

names(rand_sample) <- c("x_coord", "y_coord", "map_code")
rand_sample$id     <- row(rand_sample)[, 1]
rp2 <-
  merge(
    rp,
    data.frame(table(rand_sample$map_code)),
    by.x = "map_code",
    by.y = "Var1",
    all.x = T
  )
rp2[is.na(rp2)] <- 0


############### Create the list of classes that need to be specifically sampled
to_rtp <- rp2[rp2$Freq <  rp2$final, ]$map_code
to_spl <- rp2[rp2$Freq >= rp2$final, ]$map_code

############### Create the list of classes that are enough represented in the random sampling
#to_rtp <- rp$map_code 

final <- list()

# ############### Loop into the well represented classes, sample and append
# if (length(to_spl) > 0) {
#   for (i in 1:length(to_spl)) {
#     tmp <- rand_sample[rand_sample$id
#                        %in%
#                          sample(rand_sample[rand_sample$map_code == to_spl[i], ]$id,
#                                 rp2[rp2$map_code == to_spl[i], ]$final), ]
#     final <- rbind(final, tmp)
#   }
# }

############### Loop into the subrepresented classes, raster_to_point then append
if (length(to_rtp) > 0) {
  for (i in 1:length(to_rtp)) {
    print(i)
    tmp_rtp <- as.data.frame(rasterToPoints(map,fun = function(rast) {rast == to_rtp[i]}))
    
    names(tmp_rtp) <- c("x_coord", "y_coord", "map_code")
    tmp_rtp$id     <- row(tmp_rtp)[, 1]
    sampling       <- min(rp2[rp2$map_code == to_rtp[i], ]$final,rp2[rp2$map_code == to_rtp[i], ]$map_area)
    
    tmp            <- tmp_rtp[tmp_rtp$id %in% sample(tmp_rtp[tmp_rtp$map_code == to_rtp[i], ]$id, sampling),] 
    final          <- rbind(final, tmp)
  }
  }

names(final) <- c("xcoord","ycoord","map_code", "PLOTID")
final <- final[,c(1,2,4,3)]
table(final$map_code,useNA = "always")
final$PLOTID <- row(final)[,1]
length(unique(final$PLOTID))

proj <- proj4string(map)

spdf <- SpatialPointsDataFrame(final[,c("xcoord","ycoord")],data = final,proj4string = CRS(proj))
spdf_geo <- spTransform(spdf,CRS("+init=epsg:4326"))

plot(spdf)
head(spdf)
df <- spdf_geo@data
df$LON  <- spdf_geo@coords[,1]
df$LAT  <- spdf_geo@coords[,2]

plot(df$LON,df$LAT)
head(df)

df <- df[,c("LON","LAT","PLOTID","map_code","xcoord","ycoord")]

write.csv(df,paste0(datadir,"sae_design_diaf_00_18_provinces_20210505/manual_selection_20210510.csv"),row.names = F)
write.csv(df[,c("LON","LAT","PLOTID")],paste0(datadir,"sae_design_diaf_00_18_provinces_20210505/ceo_manual_selection_20210510.csv"),row.names = F)
