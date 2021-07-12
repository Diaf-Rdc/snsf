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
ceo_dir <- "/home/diaf/diaf_aa_2000_2010_2014/data/bdd/bdd_ceo_sae_2016_2018/"
sae_dir <- "/home/diaf/diaf_aa_2000_2010_2014/data/cartes/sae_design_diaf_00_18_provinces_20210505/"

date    <- "2021-06-07"
######################################## Paramètres
ceo_prefix <- "ceo-DRC_Collect_Reference_Data_2016_2018"
ceo_type   <- "sample-data"

template   <- paste0(ceo_prefix,"_*-",ceo_type,"-",date,".csv")
template


######################################## Liste de fichier et initialisation de la table
list_ceo <- list.files(ceo_dir,pattern = glob2rx(template))
tmp      <- read.csv(paste0(ceo_dir,list_ceo[1]),encoding = "UTF-8")
d0       <- tmp[0]


######################################## Lire et Fusionner les fichiers dans DF
for(file in list_ceo){
  tmp <- read.csv(paste0(ceo_dir,file),encoding = "UTF-8")
  tmp$group <- substr(file,42,nchar(file)-27)
  #print(ncol(tmp))
  d0 <- rbind(d0,tmp)
}

length(unique(d0$pl_plotid))
table(d0$email,useNA = "always")

all(
  names(d0) ==
  c("plot_id",  "sample_id",  "lon",  "lat",  "email",  "flagged",  "collection_time",  "analysis_duration",  "imagery_title",  "imagery_attributions",  "sample_geom",
    "pl_plotid",
    "Primary.Land.Cover",  "Secondary.Land.Cover",  "Tertiary.Land.Cover",  "Quaternary.Land.Cover",
    "Secondary.Land.Cover..NF.",  "Tertiary.Land.Cover..Sa.",  "Secondary.Land.Cover..Cu.",  "Tertiary.Land.Cover..PC.",  "Tertiary.Land.Cover..PF.",
    "CC.Time.1.....of.Tree.Cover",     "Quality.of.Point...Sensor",  "Confidence",  "Percentage.of.Cloud.Cover",
    "Year.of.Analysis...Time.2",
    "Primary.Land.Cover...Time.2",
    "Secondary.Land.Cover..F.",    "Tertiary.Land.Cover..F.Nat.",  "Quaternary.Land.Cover..FDH.",  
    "Tertiary.Land.Cover..PF_2.",  "Secondary.Land.Cover..NF_T2.",
    "Tertiary.Land.Cover..Sa_T2.",  "Tertiary.Land.Cover..Cu_T2.",  "Quaternary.Land.Cover..Per.cul_T2.",  
    "CC.Time.2.....of.Tree.Cover",    "Quality.of.Point...Sensor...T2",  "Confidence...T2",  "Percentage.of.Cloud.Cover...T2",
    "Changes",  "Year.of.Analysis...Time.1",
    "group")
)

names(d0)<-
  c("plot_id",  "sample_id",  "lon",  "lat",  "email",  "flagged",  "collection_time",  "analysis_duration",  "imagery_title",  "imagery_attributions",  "sample_geom",
    "pl_plotid",
    "lc_t1_n1",  "lc_t1_n2_F",  "lc_t1_n3_F",  "lc_t1_n4_F",
    "lc_t1_n2_NF",  "lc_t1_n3_SA",  "lc_t1_n3_CU",  "lc_t1_n4_PC",  "lc_t1_n3_PF",
    "tc_time1",  "image_t1",  "confidence_t1",  "cloud_t1",
    "year_analysis_t2",
    "lc_t2_n1",
    "lc_t2_n2_F",   "lc_t2_n3_F",  "lc_t2_n4_F",  
    "lc_t2_n3_PF",  "lc_t2_n2_NF",
    "lc_t2_n3_SA",  "lc_t2_n3_CU",  "lc_t2_n4_PC",
    "tc_time2",  "image_t2",  "confidence_t2",  "cloud_t2",
    "change",  "year_analysis_t1",
    "groupe")

d0[d0$lc_t1_n1 == "",]


table(d0$lc_t1_n2_F,d0$lc_t1_n3_PF)
table(d0$lc_t2_n2_F,d0$lc_t2_n3_PF)

table(d0$lc_t1_n2_NF,d0$lc_t1_n3_SA)
table(d0$lc_t1_n2_NF,d0$lc_t1_n3_CU)

df <- d0[!(d0$flagged == "true" & d0$pl_plotid == 434),]
table(df$email,df$groupe)

table(df$lc_t1_n1,df$lc_t2_n1,df$change)
table(df$change)

df$groupe    <- as.numeric(df$groupe)
df$cluster   <- 0
df[df$groupe <= 12 ,"cluster"] <- 3
df[df$groupe <=  8 ,"cluster"] <- 2
df[df$groupe <=  4 ,"cluster"] <- 1

table(df$cluster)

d1 <- read.csv(paste0(ceo_dir,"manual_selection_20210510.csv"))

d1$province   <- as.numeric(substr(d1$map_code,1,nchar(d1$map_code)-2))
d1$map_change <- as.numeric(substr(d1$map_code,nchar(d1$map_code)-1,nchar(d1$map_code)))

head(d1)

bd <- merge(d1,df[df$cluster ==1,c("pl_plotid","change")],by.x="PLOTID",by.y="pl_plotid",all.x=T)
names(bd)[ncol(bd)] <- "change_1"

bd <- merge(bd,df[df$cluster ==2,c("pl_plotid","change")],by.x="PLOTID",by.y="pl_plotid",all.x=T)
names(bd)[ncol(bd)] <- "change_2"

bd <- merge(bd,df[df$cluster ==3,c("pl_plotid","change")],by.x="PLOTID",by.y="pl_plotid",all.x=T)
names(bd)[ncol(bd)] <- "change_3"

bd$ag_12 <- 0
bd$ag_23 <- 0
bd$ag_31 <- 0
head(df[df$cluster ==1,c("pl_plotid","change")])

bd[bd$change_1 == bd$change_2,"ag_12"] <- 1
bd[bd$change_2 == bd$change_3,"ag_23"] <- 1
bd[bd$change_3 == bd$change_1,"ag_31"] <- 1

bd$agreement <- bd$ag_12+bd$ag_23+bd$ag_31
table(bd$agreement)
head(bd[bd$agreement == 1,])

bd$change_ag <- "No Agreement"
bd[bd$agreement == 3,"change_ag"] <- bd[bd$agreement == 3,"change_1"]
table(bd$change_ag)
bd[bd$agreement == 1 & bd$ag_12 == 1 ,"change_ag"] <- bd[bd$agreement == 1 & bd$ag_12 == 1,"change_1"]
bd[bd$agreement == 1 & bd$ag_23 == 1 ,"change_ag"] <- bd[bd$agreement == 1 & bd$ag_23 == 1,"change_2"]
bd[bd$agreement == 1 & bd$ag_31 == 1 ,"change_ag"] <- bd[bd$agreement == 1 & bd$ag_31 == 1,"change_3"]
table(bd$map_change,bd$change_ag,bd$agreement)

b0 <- bd[bd$agreement ==1 & bd$change_ag == "Stable forest",]

table(b0$change_1,b0$change_2,b0$change_3)

write.csv2(bd,
           "/home/diaf/diaf_aa_2000_2010_2014/data/bdd/bdd_ceo_sae_2016_2018/bdd_sae_2016_2018.csv",
           row.names = F)

write.csv2(df,
           "/home/diaf/diaf_aa_2000_2010_2014/data/bdd/bdd_ceo_sae_2016_2018/bdd_ceo_interpretation_2016_2018.csv",
           row.names = F)
