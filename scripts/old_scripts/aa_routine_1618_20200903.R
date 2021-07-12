####################################################################################
####### Object:  Procédure générique pour analyse de la précision          
####### Author:  amelie.arquero@fao.org                              
####### Update:  2020/09/03                                    
####################################################################################

options(stringsAsFactors = F)

library(foreign)
library(plyr)
library(rgeos)
library(rgdal)
library(raster)
library(ggplot2)


setwd("~/diaf_aa_2000_2010_2014/data/bdd_mai_ndombe/")

# vérifier l'encodage regional et fusionner les csv

list <- list.files(".",pattern = "res_")
seps <- sapply(list,function(x){substr(readLines(x)[1],3,3)})

df <- read.csv2(list[1],sep=";")
for(i in 2:length(list)){
  tmp<-read.csv(list[i],sep=";")
  df<-rbind(df,tmp)
}
#write.table(df,"maindombe_1618.csv",sep=";",row.names = F)

# vérifier les informations de la bd (dates, transitions)


# vérifier les ID uniques par rapport à la BD d'origine 


# extraction code carte 


# extraction code province 


