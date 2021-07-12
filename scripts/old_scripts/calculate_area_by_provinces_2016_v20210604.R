####################################################################################
####### Object:  calculer les superficies par provinces           
####### Author:  amelie.arquero@fao.org                              
####### Update:  2021/04/15                                
####################################################################################

# installer mes packages 


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

# Lire mes fichiers 

my_dir <-"/home/diaf/diaf_aa_2000_2010_2014/"
setwd(my_dir)

ma_fusion <- paste0(my_dir,"data/cartes/dd_00_10_14_2016_v20210604.tif")

##################### RASTERISER LE SHAPEFILE DES PROVINCES SUR LA CARTE 00_18
system(sprintf("oft-rasterize_attr.py -v %s -i %s -o %s  -a %s",
               paste0(my_dir,"data/shp_drc/RDC_Province_26.shp"),
               ma_fusion,
               paste0(my_dir,"rdc_provinces.tif"),
               "ID_SEPAL"
))

##################### PASSER LES PROVINCES EN 16Bit
system(sprintf("gdal_translate -ot Int16 -co COMPRESS=LZW  %s %s",
               paste0(my_dir,"rdc_provinces.tif"),
               paste0(my_dir,"rdc_provinces_16b.tif")
))

##################### PASSER LA CARTE 00-18 16Bit
system(sprintf("gdal_translate -ot Int16 -co COMPRESS=LZW  %s %s",
               ma_fusion,
               paste0(my_dir,"tmp_diaf_00_18_16b.tif")
))

##################### COMBINER LES DEUX CARTES
system(sprintf("gdal_calc.py -A %s -B %s --type=Int16 --co=\"COMPRESS=LZW\" --outfile=%s --calc=\"%s\"",
               paste0(my_dir,"rdc_provinces_16b.tif"),
               paste0(my_dir,"tmp_diaf_00_18_16b.tif"),
               paste0(my_dir,"diaf_00_2016_provinces.tif"),
               "A*100+B"
))

# supprime la version 16 b 
#system(sprintf("rm %s",
#              (paste0(my_dir,"diaf_00_18_16b.tif"))
#))

##################### IDENTIFIER TOUTES LES COMBINAISONS VALIDES
#classes <- as.numeric(as.vector(outer(1:26,paste, sep="")))
classes <- as.numeric(as.vector(outer(1:26,c(10,20,24,31,32,33,34,50),paste, sep="")))
#classes <- c(10,20,24,31,32,33,34,50)
classes <- classes[order(classes)]
classes

##################### CALCULER LE COMPTAGE DE PIXEL
system(sprintf("oft-stat %s %s %s ",
               paste0(my_dir,"diaf_00_2016_provinces.tif"),
               paste0(my_dir,"diaf_00_2016_provinces.tif"),
               paste0(my_dir,"stats_00_2016.txt")
))

######################################################################################################### 
##################### PARTIE II : EXPORTER LE FICHIER DE SUPERFICIES
######################################################################################################### 



##################### EXTRAIRE LA RESOLUTION
  pix <- res(raster(paste0(my_dir,"diaf_00_2016_provinces.tif")))[1]

##################### LIRE LA TABLE ET CALCULER LES SUPERFICIES
df <- read.table(paste0(my_dir,"stats_00_2016.txt"))[,1:2]
names(df) <- c("class","pixel")
df$area_ha <- df$pixel * pix*pix/10000
sum(df$area_ha)
table(df$class)
##################### SELECTIONNER UNIQUEMENT LES CLASSES VALIDES
df1 <- df[df$class %in% classes,]

##################### EXTRAIRE LES CODES PROVINCES ET DE CHANGEMENT
df1$change   <- substr(df1$class,nchar(df1$class)-1,nchar(df1$class))
df1$province <- substr(df1$class,0,nchar(df1$class)-2)
head(df1)
##################### FUSIONNER AVEC LES NOMS DES PROVINCES
codes <- read.dbf(paste0(my_dir,"data/shp_drc/RDC_Province_26.dbf"))[,c("NOM","ID_SEPAL")]
df2   <- merge(df1,codes,by.y="ID_SEPAL",by.x="province",all.x=T)
df2   <- arrange(df2,class)[,c("class","NOM","province","change","pixel","area_ha")]

##################### VERIFIER LES TOTAUX
sum(df2$area_ha)
tapply(df2$area_ha,df2$NOM,sum)

##################### EXPORTER LE FICHIER
write.csv2(df2,paste0(my_dir,"areas_00_2016_v20210604.csv"),row.names = F)

### ON EST DES GROS BOULETS ET ON COPIE A LA MAIN DE MY_DIR VERS CARTES
