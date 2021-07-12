####################################################################################
####### Object:  Extraction des valeurs de la carte 00-18     
####### Author:  amelie.arquero@fao.org                             
####### Update:  2021/04/14                                    
####################################################################################


######################################################################################################### 
##################### PARTIE III : EXPORTER LE FICHIER DE POINTS
######################################################################################################### 

options(stringsAsFactors = F)
library(sp)
library(dplyr)
library(foreign)
library(plyr)
library(rgeos)
library(rgdal)
library(raster)
library(ggplot2)
library(tidyr)
##################### LIRE LES POINTS et lire la carte

bdd_dir <- "/home/diaf/diaf_aa_2000_2010_2014/data/bdd/"
map_dir <- "/home/diaf/diaf_aa_2000_2010_2014/data/cartes/"
res_dir <- "/home/diaf/diaf_aa_2000_2010_2014/data/results/"
dir.create(res_dir)

df <- read.csv2(paste0(bdd_dir,"bdd_14_16_20200429.csv"))
#map_1618 <- paste0(my_dir,"dd_00_10_14_16_18.tif")
map_1618 <- paste0(map_dir,"diaf_00_18_provinces.tif")
head(df)

##################### TRANSFORMER LES POINTS EN FICHIER SPATIAL

#mettre les coordonnées en format numérique
df$location_x <- as.numeric(df$location_x)
df$location_y <- as.numeric(df$location_y)

spdf_geo <- SpatialPointsDataFrame(
  coords = df[,c("location_x","location_y")],
  data   = df,
  proj4string=CRS("+init=epsg:4326")
)

##################### EXTRAIRE LES VALEURS DES POINTS
spdf_geo$map_0018 <- raster::extract(raster(map_1618),spdf_geo)

table(spdf_geo$map_0018)
df1 <- spdf_geo@data
df1$code_0018   <- substr(df1$map_0018,nchar(df1$map_0018)-1,nchar(df1$map_0018))
df1$province <- substr(df1$map_0018,0,nchar(df1$map_0018)-2)


names(df)
head(df1)
table(df1$code_0018,df1$ce_transition,df1$province)
nf <- df1[df1$ce_transition==1 & df1$code_0018==10,]
dd <- spdf_geo@data
table(dd$map_0018,dd$ce_transition)

df3 <- table(dd$map_0018,dd$ce_transition)
head(df3)
write.csv2(df3,paste0(bdd_dir,"bdd_0018_vs_carte0018.csv"),row.names = F)

df3 <- read.csv2(paste0(bdd_dir,"bdd_0018_vs_carte0018.csv"))
table(dd$map_0018,dd$map_transition)
