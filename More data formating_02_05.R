
#BRE 2022 GIS work, distance to nearest road, fragmentation, site etc


library(readr)
library(tidyverse)
library(spatstat)
library(sp)
library(rgdal)
library(maptools)
library(raster)
library(rgeos)


#load the data 
setwd("~/github/BRE_2022/BRE_2022")
df <- read_csv("BRE_2022_clean_02_03.csv")
View(df)

df$survey_date<-as.Date(df$survey_date)#transform to Date class

# #get unique sites
# t2<-df%>%
#   dplyr::select(c(Site_num,Long,Lat))
# 
# t3<-unique(t2)#



library(maps)
library(mapdata)
library(ggmap)

# #convert to UTM
# #Function
# LongLatToUTM<-function(x,y,zone){
#   xy <- data.frame(ID = 1:length(x), X = x, Y = y)
#   coordinates(xy) <- c("X", "Y")
#   proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
#   res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
#   return(as.data.frame(res))
# }
# 
# dist<-LongLatToUTM(t3$Long,t3$Lat,17)
# 
# t4<-cbind(dist,t3)
# 
# coordinates(t4) = ~X+Y
# plot(t4)
# 
# writeOGR(t4,"/Volumes/Seagate Expansion Drive/GIS/USA/NC/Vector features/BRE 2022 shapefiles/Surveys","BRE_2022_nndist", driver = "ESRI Shapefile")

setwd("/Volumes/Seagate Expansion Drive/GIS/USA/NC/Vector features/BRE 2022 shapefiles/Surveys")
nnd<-readOGR(".","BRE_2022_nndist")
nnd  <- as(nnd, "ppp")

plot(nnd %mark% nndist(nnd,k=1), markscale=1)

nnd$marks$sep<-NULL

nnd$marks$nndist<-nndist(nnd,k=1)

nnd1<-as.data.frame(nnd$marks)#finnaly correct! 


nnwhich(nnd,k=1)

too_close<-nnd1%>%
  filter(nndist<=10)

test<-too_close%>%
  group_by(nndist)%>%
  filter(n()==1)

test$Flag<-NA

test2<-too_close%>%
  group_by(nndist)%>%
  filter(n()>1)%>%
  mutate(Flag=cur_group_id())%>%
  ungroup()

test3<-rbind(test,test2)

yep<-test3%>%
  mutate(new_flag=case_when(Site_num %in% c(69,438,443)~15,
                            Site_num %in% c(160,161,162)~14,
                            Site_num %in% c(172,2692,2691)~18,
                            Site_num %in% c(470,1906,103)~8,
                            Site_num %in% c(472,473,506)~6,
                            Site_num %in% c(512,513,514)~2,
                            Site_num %in% c(764,765,766)~12,
                            Site_num %in% c(832,834,833)~48,
                            Site_num %in% c(913,912,1417)~33,
                            Site_num %in% c(942,944,2335)~23,
                            Site_num %in% c(951,685,686)~34,
                            Site_num %in% c(1262,2534,2533)~40,
                            Site_num %in% c(2441,2445,2444,2443)~35,
                            TRUE~as.numeric(as.character(Flag))
                            ))


yep$new_flag1<-yep$new_flag+5000

yep<-as.data.frame(yep)

grpd<-yep%>%
  dplyr::select(-c(Flag,new_flag))

nnd1$new_flag1<-nnd1$Site_num

anti<-unique(yep$Site_num)

nnd2<-nnd1%>%
  filter(!Site_num %in% anti)

nnd3<-rbind(nnd2,grpd)


tm<-nnd3%>%
  dplyr::select(Site_num,nndist,new_flag1)


tdf<-merge(df,tm,by=("Site_num"))


tdf$new_flag1<-as.numeric(tdf$new_flag1)


#fix unknown age columns

utf<-tdf

age<-utf%>%
  filter(grepl("age",notes))

#seperate out PLME unk age 

PLME_c<-age%>%
  filter(grepl("PLME",notes))

#1 additional ANEA unk caught 
PLME_d<-PLME_c%>%
  filter(grepl("Rock 1st found w/ 1 lg female and 1 unk age ANAE, 1 PLME",notes))
#cut out others 
PLME_e<-PLME_c%>%
  filter(!grepl("Rock 1st found w/ 1 lg female and 1 unk age ANAE, 1 PLME",notes))

#add unkown col
PLME_d$num_unk_ANAE<-1
PLME_e$num_unk_ANAE<-NA

#recobine 
PLME_f<-rbind(PLME_d,PLME_e)

#temporarily cut out fixed PLME records 
age_a<-age%>%
  filter(!Survey_ID %in% PLME_f$Survey_ID)

age_a$num_unk_ANAE<-1

#write_csv(age_a,"age_a_temp.csv")#write csv to manually edit numbers 

age_a_temp <- read_csv("age_a_temp.csv")
age_a_temp$survey_date<-as.Date(age_a_temp$survey_date)

age_b<-rbind(age_a_temp,PLME_f)

n_age<-utf%>%
  filter(!grepl("age",notes))

n_age$num_unk_ANAE<-NA

n_age_a<-n_age%>%
  filter(grepl("unid",notes))

#write_csv(n_age_a,"n_age_a.csv")
n_age_a<-read_csv("n_age_a.csv")

#replace non edited data 
n_age_b<-n_age%>%
  filter(!grepl("unid",notes))

n_age_c<-rbind(n_age_a,n_age_b)

names(PLME_f)
names(n_age_c)

df_1<-rbind(n_age_c,age_b)#bind it all together 



fm<-anti_join(utf,df_1,by=c("survey_date","notes"))#check to see if I missed anything 

df_1<-df_1%>%
  relocate(num_unk_ANAE,.after = num_adult)

df_2<-df_1%>%
  dplyr::select(!total_ANAE)

#make total green column

df_2<-df_2%>%
  rowwise()%>%
  mutate(total_ANAE=sum(c(num_adult,
                          num_unk_ANAE,
                          num_juv_subad_yrling,
                          num_hatchling),na.rm=TRUE))


df_2<-df_2%>%
  relocate(total_ANAE, .after =num_eggs )

#write_csv(df_2,"df_2_temp.csv")

df_2<-read_csv("df_2_temp.csv")#re load edited data

df_2$total_ANAE<-NULL

df_2<-df_2%>%
  rowwise()%>%
  mutate(total_ANAE=sum(c(num_adult,
                          num_unk_ANAE,
                          num_juv_subad_yrling,
                          num_hatchling),na.rm=TRUE))

df_2<-df_2%>%
  relocate(total_ANAE, .after =num_eggs )


#write updated dataset 


#write_csv(df_2,"BRE_2022_02_05.csv")



# exp<-tdf%>%
#   filter(new_flag1=="5015")
# setwd("~/github/BRE_2022/BRE_2022/Data")
# 
# write_csv(exp,"grouping_example.csv")

# #load as a shapefile 
# dist <- readOGR(".","HNG_site_dist")  # Don't add the .shp extension
# dist   <- as(dist, "ppp")
# 
# x<-data.frame(nndist(dist,k=1))
# 
# ggplot(x,aes(x$nndist.dist..k...1.))+geom_density()
# 
# #get mean, sd and range of distance
# median(nndist(dist,k=1))
# mean(nndist(dist,k=1))
# sd(nndist(dist,k=1))
# range(nndist(dist,k=1))

# #add to location dataframe
# dizy<-dist#rename
# dizy$sep<-nndist(dist,k=1)#add seperation column
# 
# #combine for site names
# diz<-cbind(dizy,coords)




setwd("~/Documents/NCWRC/Green Salamander EOY")
nnk <- read_csv("Sites_For_Dist_group.csv")
nnk <-plyr::rename(nnk ,c("SITE"="Site"))
setwd("~/Documents/NCWRC/Green Salamander EOY")

f_m<-df[,c(1,15)]

nnk<-merge(nnk,f_m,by="Site")


nnk<-unique(nnk)


coordinates(nnk) = ~UTM_E+UTM_N
plot(nnk)

writeOGR(nnk,"~/Documents/NCWRC/Data/Terrestrial Surveys/Rock locations/Shapefiles","nnk", driver = "ESRI Shapefile")

setwd("~/Documents/NCWRC/Data/Terrestrial Surveys/Rock locations/Shapefiles")

s  <- readOGR(".","nnk")  # Don't add the .shp extension
poi   <- as(s, "ppp")

unique(nnk@data$Spatial_Area)

DPSF<-nnk[nnk@data$Spatial_Area=="DuPont",]
HNG<-nnk[nnk@data$Spatial_Area=="HNG",]
BRE<-nnk[nnk@data$Spatial_Area=="BRE",]

writeOGR(DPSF,"~/Documents/NCWRC/Data/Terrestrial Surveys/Rock locations/Shapefiles","DPSF", driver = "ESRI Shapefile")
writeOGR(HNG,"~/Documents/NCWRC/Data/Terrestrial Surveys/Rock locations/Shapefiles","HNG", driver = "ESRI Shapefile")
writeOGR(BRE,"~/Documents/NCWRC/Data/Terrestrial Surveys/Rock locations/Shapefiles","BRE", driver = "ESRI Shapefile")

setwd("~/Documents/NCWRC/Data/Terrestrial Surveys/Rock locations/Shapefiles")
DPSF <- readOGR(".","DPSF")  # Don't add the .shp extension
DPSF   <- as(DPSF, "ppp")

HNG <- readOGR(".","HNG")  # Don't add the .shp extension
HNG   <- as(HNG, "ppp")

BRE <- readOGR(".","BRE")  # Don't add the .shp extension
BRE   <- as(BRE, "ppp")



mean(nndist(DPSF,k=1))
sd(nndist(DPSF,k=1))
range(nndist(DPSF,k=1))

du<-nndist(DPSF,k=1)
sum(du<100)

mean(nndist(HNG,k=1))
sd(nndist(HNG,k=1))
range(nndist(HNG,k=1))

hg<-nndist(HNG,k=1)
hg
sum(hg<100)

mean(nndist(BRE,k=1))
sd(nndist(BRE,k=1))
range(nndist(BRE,k=1))

brb<-nndist(BRE,k=1)
brb
sum(brb<100)


# Do some extraction of raster values for various buffer sizes and vairbles
setwd("~/Documents/NCWRC/Green Salamander EOY/GIS files")

#read in the buffered site polygon
buf_10<- readOGR(".","Occ_Sites_10m_buff_UTM")


#read in the canopy raster 
setwd("/Volumes/Seagate Expansion Drive/GIS/Landcover Data/NC/nlcd2016release/canopy")
canopy<-raster("NC_NLCD2011_UTM_canopy_analytical.tif")

plot(canopy)#check to make sure everything is ok

mask_can<-mask(x=canopy,mask=buf_10)
plot(mask_can)

crop_can<-crop(x=mask_can,y=extent(buf_10))
plot(crop_can)








