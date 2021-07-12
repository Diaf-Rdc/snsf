####################################################################################
####### Object:  ESTIMATION OF EMISSIONS FOR RENF 2010-2014        
####### Author:  remi.dannunzio@fao.org                               
####### Update:  2020/04/23                               
####################################################################################
setwd(bdd_dir)

df <- read.csv2(paste0(bdd_dir,"Bd_14_16_20200410_V3.csv"))

names(df)

table(df$secondary_land_cover_label.1,df$secondary_land_cover1_label)
df <- df[,names(df) != "secondary_land_cover_label.1"]

convert_date <- function(x){paste0(substr(x,2,2),"_",substr(x,5,5),"_",substr(x,10,10))}

df$tertiary_land_cover  <- convert_date(df$tertiary_land_cover)
df$tertiary_land_cover1 <- convert_date(df$tertiary_land_cover1)

df[df$tertiary_land_cover == "__","tertiary_land_cover"] <- NA
df[df$tertiary_land_cover1 == "__","tertiary_land_cover1"] <- NA

df[df$tertiary_land_cover_label == "Forest_Dense_Humid"  & !is.na(df$tertiary_land_cover_label), "tertiary_land_cover_label"]  <- "Forest, Dense, Humid"
df[df$tertiary_land_cover1_label == "Forest_Dense_Humid" & !is.na(df$tertiary_land_cover1_label),"tertiary_land_cover1_label"] <- "Forest, Dense, Humid"

table(df$tertiary_land_cover_label,df$tertiary_land_cover,useNA = "always")

table_tier     <- function(x){
  table(df[,paste0(x,"_land_cover")],
        df[,paste0(x,"_land_cover_label")])
}

correspondance <- function(x){
  sapply(1:ncol(table_tier(x)),function(y){which.max(table_tier(x)[,y])})
  }

codes          <- function(x){
  d <- data.frame(cbind(names(correspondance(x)),
        colnames(table_tier(x))))
  names(d) <- c("code",paste0("class_",x))
  d
  }

levels <- c("primary","secondary","tertiary","quarternary")

sapply(levels,table_tier)
lapply(levels,codes)

x  <- levels[1]
tt <- merge(df,codes(x),by.x=paste0(x,"_land_cover"),by.y="code",all.x=T)
tt[,paste0(x,"_land_cover_label")] <- tt[,paste0("class_",x)]

x  <- levels[2]
tt2 <- merge(tt,codes(x),by.x=paste0(x,"_land_cover"),by.y="code",all.x=T)
tt2[,paste0(x,"_land_cover_label")] <- tt2[,paste0("class_",x)]

## A FINIR
x  <- levels[3]
tt3 <- merge(x=tt2,y=codes(x),by.x=paste0(x,"_land_cover"),by.y="code",all.x=T,useNA="no")
tt3[,paste0(x,"_land_cover_label")] <- tt3[,paste0("class_",x)]
names(table(tt2$tertiary_land_cover,useNA = "always")) %in% codes(x)$code
?table
names(tt3)
str(tt2)
tail(tt)
x  <- levels[4]
tt <- merge(tt,codes(x),by.x=paste0(x,"_land_cover"),by.y="code",all.x=T)
tt[,paste0(x,"_land_cover_label")] <- tt[,paste0("class_",x)]

# sapply(levels,function(x){table(tt[,paste0("class_",x)] == tt[,paste0(x,"_land_cover_label")],useNA = "always")})
# 
# dd <- tt[,names(df)]
# tt$terti
# table(df$primary_land_cover,df$primary_land_cover_label,useNA = "always")
# table(dd$primary_land_cover,dd$primary_land_cover_label,useNA = "always")
# 
# table(df$quarternary_land_cover,df$quarternary_land_cover_label,useNA = "always")
# table(dd$quarternary_land_cover,dd$quarternary_land_cover_label,useNA = "always")
# 
# table(df$quarternary_land_cover,df$quarternary_land_cover_label,useNA = "always")
# table(df$quarternary_land_cover,df$quarternary_land_cover_label,useNA = "always")
# 
# 
# write.csv2(df,paste0(bdd_dir,"bdd_14_16_20200429.csv"),row.names = F)
# summary(df)
# head(df[is.na(df$tertiary_land_cover_label),])
# head(df)
