bdd_dir <- "/home/diaf/diaf_aa_2000_2010_2014/data/bdd/bdd_ceo_sae_2016_2018/"
sae_dir <- "/home/diaf/diaf_aa_2000_2010_2014/data/cartes/sae_design_diaf_00_18_provinces_20210505/"
shp_dir <- "/home/diaf/diaf_aa_2000_2010_2014/data/shp_drc/"

bdd_14_16_recycle <- paste0(bdd_dir,"bdd_1416_vs_carte0018.csv")
bdd_17_18_diaf    <- paste0(bdd_dir,"bdd_sae_2016_2018.csv")
bdd_17_18_amelie  <- paste0(bdd_dir,"bdd_sae_2017_2018_v20210615_edit_amelie.csv")
#bdd_17_18_wilfred <- paste0(bdd_dir,"bdd_sae_2017_2018_v20210615_edit_wilfred.csv")

df     <- read.csv2(bdd_17_18_diaf)
df_rec <- read.csv(bdd_14_16_recycle)
df_aml <- read.csv(bdd_17_18_amelie)

df_rec$PLOTID <- 10000+1:nrow(df_rec)
table(df_rec$code_0018,df_rec$ce_transition)

dr <- df_rec[(df_rec$code_0018 == 10 & df_rec$ce_transition == 1) | 
             (df_rec$code_0018 == 20 & df_rec$ce_transition == 2),]

dr$ref_change <- dr$code_0018

to_ref_change <- function(df,ce_class){
df$ref_change <- 0
df[df[,ce_class] == "Stable non forest","ref_change"] <- 10
df[df[,ce_class] == "Stable forest","ref_change"]     <- 20
df[df[,ce_class] == "Degradation","ref_change"]       <- 24
df[df[,ce_class] == "Deforestation","ref_change"]     <- 34
df[df[,ce_class] == "Gains","ref_change"]             <- 50
return(df)
}

df <- to_ref_change(df    ,"change_ag")
da <- to_ref_change(df_aml,"RES_CQ")

ids_am <- da[da$RES_CQ != "","PLOTID"]

# table(df$map_change,df$ref_change,df$agreement)
# table(df$ref_change,df$change_ag)
# table(df$change_ag,df$agreement)
# table(df_aml$map_change,df_aml$RES_CQ)
# 
# names(df)
# names(df_rec)
# head(df_rec)
# head(df_aml)
# table(df_aml$ref_change)
# summary(df_rec$map_province)
# all(df_rec$map_province == df_rec$map_province)


d1 <- df[!(df$PLOTID %in% ids_am),c("PLOTID","LON","LAT","map_code","province","map_change","ref_change")]
d2 <- da[ (da$PLOTID %in% ids_am),c("PLOTID","LON","LAT","map_code","province","map_change","ref_change")]
d3 <- dr[,c("PLOTID","location_x","location_y","map_0018","province","code_0018","ref_change")]

names(d1) <- names(d2) <- names(d3) <- c("PLOTID","location_x","location_y","map_code","province","map_change","ref_change")
bd <- bind_rows(d1,d2,d3)
bd$ref_code <- paste0(bd$province,bd$ref_change)
bd <- bd[bd$ref_change >0 ,]
table(bd$map_change,bd$ref_change)
table(bd$province,bd$ref_change)
table(bd$province,bd$map_change)


tmp <- read.csv(paste0(sae_dir,"areas_00_18_v20210504.csv"))
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

legend  <- sort(unique(areas[,"class"]))
map_code <- "map_code"
ref_code <- "ref_code"

s <- saea(bd,0.9,areas,legend,map_code,ref_code)

s$class_code <- as.numeric(substr(s$class,nchar(s$class)-1,nchar(s$class)))
s$province   <- as.numeric(substr(s$class,0,nchar(s$class)-2))

codes <- read.dbf(paste0(shp_dir,"RDC_Province_26.dbf"))[,c("NOM","ID_SEPAL")]
names(codes) <- c("province_name","ID_SEPAL")

s1  <- merge(s,codes,by.y="ID_SEPAL",by.x="province",all.x=T)

write.csv(s1,paste0(sae_dir,"sae_stats_conv_2016_2018_v20210615.csv"),row.names = F)  
write.csv(bd,paste0(bdd_dir,"bdd_sae_conv_2016_2018_completed_v20210615.csv"),row.names = F)  
length(unique(bd$PLOTID))
summary(bd$PLOTID)
plot(bd$PLOTID)
head(bd)
