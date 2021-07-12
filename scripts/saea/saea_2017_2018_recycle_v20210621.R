bdd_dir <- "/home/diaf/diaf_aa_2000_2010_2014/data/bdd/bdd_ceo_sae_2016_2018/"
sae_dir <- "/home/diaf/diaf_aa_2000_2010_2014/data/cartes/sae_design_diaf_00_18_provinces_20210505/"
shp_dir <- "/home/diaf/diaf_aa_2000_2010_2014/data/shp_drc/"


##### BDD 2016-2018: points recyclés 14-16 + points interprétés-croisés 16-18 de changement 
bdd      <- paste0(bdd_dir,"bdd_sae_conv_2016_2018_completed_v20210615.csv")

##### Controle qualité automatique 2014-2016 (points d'omissions via CUMSUM Andreas)
cumsum   <- paste0("/home/diaf/rdc_extract_st/change_analysis_complete.csv")

##### Controle qualité 2016-2018 (2 accords sur 3, Stable F ou NF >> reverifiés)
bdd_cq1  <- paste0(bdd_dir,"sae_cq_ceo-CQ_2016_2018_1-sample-data-2021-06-20.csv")
bdd_cq2  <- paste0(bdd_dir,"sae_cq_ceo-CQ_2016_2018_2-sample-data-2021-06-20.csv")
bdd_cq3  <- paste0(bdd_dir,"sae_cq_ceo-CQ_2016_2018_3-sample-data-2021-06-20.csv")

##### Controle qualité manuel 2014-2016 (reverifies dans CEO)
bdd_cq4  <- paste0(bdd_dir,"sae_cq_ceo-CQ_2016_2018_4_omission-sample-data-2021-06-20.csv")
bdd_cq5  <- paste0(bdd_dir,"sae_cq_ceo-CQ_2016_2018_5_omission-sample-data-2021-06-21.csv")

##### Lire les BDD
df     <- read.csv(bdd)
df_cs  <- read.csv(cumsum)

df_cq1 <- read.csv(bdd_cq1)
df_cq2 <- read.csv(bdd_cq2)
df_cq3 <- read.csv(bdd_cq3)
df_cq4 <- read.csv(bdd_cq4)
df_cq5 <- read.csv(bdd_cq5)

df_cq1$group_cq <- "cq1"
df_cq2$group_cq <- "cq2"
df_cq3$group_cq <- "cq3"
df_cq4$group_cq <- "cq4"
df_cq5$group_cq <- "cq5"

##### Assembler les fichiers de controle qualité
df_cq5$flagged <- as.character(df_cq5$flagged)
df_cq  <- bind_rows(df_cq1,df_cq2,df_cq3,df_cq4,df_cq5)

table(df_cq$flagged)
df_cq$redoublon <- duplicated(df_cq$pl_plotid)
table(df_cq$redoublon,df_cq$group_cq)
df_cq[df_cq$pl_plotid %in% df_cq[df_cq$redoublon ==T,"pl_plotid"],]

df_cq <- df_cq[df_cq$redoublon ==F,]

##### Convert CEO codes into BDD code, pour le CQ
to_ref_change <- function(df,in_code,out_code){
  df[,out_code] <- 0
df[df[,in_code] == "non foret stable",out_code] <- 10
df[df[,in_code] == "foret stable",out_code]     <- 20
df[df[,in_code] == "Degradation",out_code]       <- 24
df[df[,in_code] == "Deforestation",out_code]     <- 34
df[df[,in_code] == "gains",out_code]             <- 50
return(df)
}

dq <- to_ref_change(df_cq,"Type.de.changement","ref_change_CQ")
bd <- df_cs

df$status <- "none"
df[!(df$PLOTID %in% bd$plot_id),"status"] <- "miss"
df[df$PLOTID %in% bd$plot_id,"status"] <- "hit"

#### SPATIALISATION
spdf    <- SpatialPointsDataFrame(df[,c("location_x","location_y")],
                                  df,
                                  proj4string = CRS("+init=epsg:4326"))

table(df$status)

plot(spdf)

bd$doublon <- duplicated(bd$plot_id)
b0 <- bd[bd$doublon ==F,]

d0 <- merge(df,b0,by.x="PLOTID",by.y="plot_id",all.x=T)



##### Verification coherence
table(dq$ref_change_CQ,dq$group_cq)
length(unique(df_cq$pl_plotid))
length(unique(df$PLOTID))
length(unique(df_cs$id))

all(df_cq$pl_plotid %in% df$PLOTID)
all(df_cs$plot_id %in% df$PLOTID)
all(df_cq4$pl_plotid %in% df_cs$plot_id)

hist(df_cs$ts_images)
names(df)
names(df_cq)
table(dq$email,dq$ref_change_CQ)

hist(d0$confidence)

##### Fusion DF et DQ
d1 <- merge(d0,dq,all.x=T,by.x="PLOTID",by.y="pl_plotid")

d1$ref_change_edit <- d1$ref_change

visual_check_ids <- d1[!is.na(d1$ref_change_CQ) & d1$ref_change_CQ >0,"PLOTID"]
d1[d1$PLOTID %in% visual_check_ids,"ref_change_edit"] <- d1[d1$PLOTID %in% visual_check_ids,"ref_change_CQ"]
table(d1$ref_change,d1$ref_change_edit)

tier3 <- d1[d1$PLOTID %in% df_cq5$pl_plotid,]

table(tier3$ref_change_edit,tier3$ref_change)
hist(tier3$confidence)


tier2 <- d1[!is.na(d1$confidence) & d1$confidence > 0.55 & d1$map_change == 20 & d1$ref_change ==20,]
summary(d1$confidence)

deg_t2_ids <- sample(tier2$PLOTID,size = nrow(tier3[tier3$ref_change_edit == 24,])/nrow(tier3)*nrow(tier2))
def_t2_ids <- sample(tier2$PLOTID[!(tier2$PLOTID %in% deg_t2_ids)],size = nrow(tier3[tier3$ref_change_edit == 34,])/nrow(tier3)*nrow(tier2))

d1[d1$PLOTID %in% deg_t2_ids,"ref_change_edit"] <- 24
d1[d1$PLOTID %in% def_t2_ids,"ref_change_edit"] <- 34

#check <- d1[d1$PLOTID %in% c(deg_t2_ids[1:10],def_t2_ids[1:10]),c("location_x","location_y","PLOTID")]
#names(check) <- c("LON","LAT","PLOTID")
#write.csv(check,paste0("/home/diaf/rdc_extract_st/check_20pts_omission_055.csv"),row.names = F)

##### Matrice de confusion avant CQ (attention : prends en compte les 52 points du premier CQ)
table(df$map_change,df$ref_change)

##### Matrice de confusion apres CQ
table(d1$map_change,d1$ref_change_edit)


head(d1)







##### FICHIER DES SUPERFICIES
tmp    <- read.csv(paste0(sae_dir,"areas_00_18_v20210504.csv"))
tmpfdd <- tmp[tmp$change %in% c(20,24,34,50),]
tmp_nf <- tmp[tmp$change %in% c(10,31,32,33),]
ar_nf  <- tmp[tmp$change %in% c(10),]

sum(tmpfdd$area_ha)+sum(tmp_nf$area_ha)

tmp_nf$change <- 10
tmp_nf$class <- paste0(tmp_nf$province,tmp_nf$change)

tmp_pix <- tmp_nf %>% group_by(class) %>% summarize(sum_pixel = sum(pixel)) 
tmp_pix$class <- as.numeric(tmp_pix$class)
tmp_pix <- arrange(tmp_pix,class)
all(ar_nf$class == tmp_pix$class)
ar_nf$pixel <- tmp_pix$sum_pixel
ar_nf$area_ha <- ar_nf$pixel*0.09

areas <- rbind(tmpfdd,ar_nf)
areas <- arrange(areas,class)
sum(areas$area_ha)
areas <- areas[,c("class","area_ha")]
names(areas) <- c("class","area")



##### SAE calculation
legend           <- sort(unique(areas[,"class"]))
d1$ref_code_edit <- paste0(d1$province,d1$ref_change_edit)

map_code <- "map_code"
ref_code <- "ref_code_edit"

s <- saea(d1,0.9,areas,legend,map_code,ref_code)

s$class_code <- as.numeric(substr(s$class,nchar(s$class)-1,nchar(s$class)))
s$province   <- as.numeric(substr(s$class,0,nchar(s$class)-2))

codes <- read.dbf(paste0(shp_dir,"RDC_Province_26.dbf"))[,c("NOM","ID_SEPAL")]
names(codes) <- c("province_name","ID_SEPAL")

s1  <- merge(s,codes,by.y="ID_SEPAL",by.x="province",all.x=T)

#write.csv(s1,paste0(sae_dir,"sae_stats_conv_2016_2018_L5_v20210621.csv"),row.names = F)  
#write.csv(d1,paste0(bdd_dir,"bdd_sae_conv_2016_2018_completed_L5_v20210621.csv"),row.names = F)  

tapply(s1$strRS_area_estimate,s1$class_code,sum)
tapply(s1$map_pixel_count,s1$class_code,sum)
