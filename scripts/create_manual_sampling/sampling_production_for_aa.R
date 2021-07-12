####################################################################################
####### Object:  Production des échantillons 2016-2018       
####### Author:  amelie.arquero@fao.org                               
####### Update:  2021/04/22                                    
######################################################################

# charger les packages (sortir ceux qui ne sont pas nécessaires)
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

bdd_dir <-"/home/diaf/diaf_aa_2000_2010_2014/data/bdd/"
map_dir <- "/home/diaf/diaf_aa_2000_2010_2014/data/cartes/"
res_dir <- "/home/diaf/diaf_aa_2000_2010_2014/data/sampling/"
dir.create(res_dir)

df <- read.csv2(paste0(bdd_dir,"bdd_14_16_20200429.csv"))
#map_1618 <- paste0(my_dir,"dd_00_10_14_16_18.tif")
map_1618 <- paste0(map_dir,"diaf_00_18_provinces.tif")
head(df)

#compute (almost) proportionnal sampling


a2$proportional  <- floor(a2$wi * a2$overallsample)

a2$max   <- apply(cbind(a2$proportional,120), 1, max)
a2$diff  <- a2$max - a2$proportional
a2$final <- 0

a4       <- a2[0,]

for(province in unique(a2$province)){
  
  a3          <- a2[a2$province == province,]
  
  a3$adjusted  <- a3$area/sum(a3$area)*(a3$overallsample-3*120)+120