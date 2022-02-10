#nearest road and fragmentation metrics

#load observation covs and presence absence data
setwd("~/github/BRE_2022/BRE_2022/Data")

### BRE population trends 2022 

#import data
library(readr)
library(tidyverse)
df <- read_csv("BRE_2022_02_05.csv")

df$survey_date<-as.Date(df$survey_date)#transform to Date class



library(geosphere)
library(spatstat)
library(sp)
library(sf)
library(rgdal)
library(maptools)
library(raster)
library(rgeos)

setwd("/Volumes/Seagate Expansion Drive/GIS/USA/NC/Vector features/BRE 2022 shapefiles/Primary Roads/TIGER line all roads")

#load the roads 
rds<-readOGR(".","BRE TIGER line roads")

crs(rds)


#get unique sites
site<-df%>%
  distinct(Long,Lat)

#add projected coords
LongLatToUTM<-function(x,y,zone){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=",zone," ellps=WGS84",sep='')))
  return(as.data.frame(res))
}

dist<-LongLatToUTM(sites$Long,sites$Lat,17)

siteR<-cbind(sites,dist)#combine 

coordinates(site) = ~Long+Lat

crs(site)<-CRS("+init=epsg:4326")

crs()<-CRS("+init=epsg:32617")

crs(dist)

rds2<-spTransform(rds, CRS("+init=epsg:32617"))

rds_ll<-spTransform(rds2, CRS("+init=epsg:4326"))
dist_ll<-spTransform(dist, CRS("+init=epsg:4326"))

d<-dist2Line(dist_ll,rds_ll)

dd<-as.data.frame(d)

setwd("~/github/BRE_2022/BRE_2022/Data")
write_csv(dd,"road_dist.csv")

#load the points 
#get unique sites
sites<-df%>%
  distinct(Long,Lat)

sitr<-SpatialPoints(sites,CRS("+init=epsg:4326"))

coordinates(sitr) = ~Long+lat
plot(sitr)

spTransform(rds, CRS("+init=epsg:4326"))

rds2<-spTransform(rds, CRS("+init=epsg:4326"))

dist2Line(disty,rds)




crs(disty)



#summary stats



#get unique sites
dsd<-df%>%
  distinct(Site_num,nndist)

mean(dsd$nndist)
sd(dsd$nndist)
range(dsd$nndist)


library(ggplot2)
library(ggpubr)

fivenum(dsd$nndist)

ggplot(data=dsd,aes(nndist))+
  geom_density()+
  xlim(0,200)+
  ylab("Density")+
  xlab("Distance to nearest neighbor(m)")+
  annotate("text", x = 41, y = 0.016, label = "Median 41.2m",fontface =2)+
  annotate("text", x = 190, y = 0.0075, label = "max 5657m >",fontface =2)+
  theme_pubr()



#try aggregating counts for new flags

g_d<-df%>%
  filter(new_flag1>=5000)

g_d1<-g_d %>% 
  group_by(new_flag1,survey_date) %>%
  summarise(across(c(num_adult,num_unk_ANAE,num_juv_subad_yrling,
                     num_hatchling,num_eggs,total_ANAE,total_ANAE_or_PLME), sum))

a<-names(g_d)

g_da<-g_d[-c(14:20)]


g_d2<-merge(g_da,g_d1,by=c("survey_date","new_flag1"))

g_d2<-g_d2%>%
  relocate(a,.after=species)

#remove old counts from original data 

ng_d<-df%>%
  filter(new_flag1<5000)


6655+2244#check to make sure no data was missed

#combine data 

g_d3<-rbind(ng_d,g_d2)#now there are doubles... need to fix

#need to make a new dataframe for combined counts

z<-df


z1<- z %>% 
  group_by(new_flag1,survey_date) %>%
  summarise(across(c(num_adult,num_unk_ANAE,num_juv_subad_yrling,
                     num_hatchling,num_eggs,total_ANAE,total_ANAE_or_PLME), sum))

#need to cut out the multiple surveys per day from the observer detection probability survey 

zx<-z%>%
  group_by(new_flag1,survey_date)%>%
  filter(n()>1)

zoc<-zx%>%
  filter(grepl("occ det study",notes)|
         grepl("Occ det study",notes)|
         grepl("Occ-det study",notes)|
         grepl("occ",notes))

zoc1<-zoc%>%
  dplyr::select(-c(Site_num:prop_ownership)&
                  -c(data_source:species)&
                  -c(nest:nndist))

#change NAs to 0 to see if that will belp?

zoc1<-zoc1%>%
  replace(is.na(.),0)

#it did 
zoc2<-zoc1%>%
  group_by(new_flag1,survey_date) %>%
  summarise_all(max)


#now get other non occupancy dups 
noc<-zx%>%
filter(!grepl("occ det study",notes)&
       !grepl("Occ det study",notes)&
       !grepl("Occ-det study",notes)&
       !grepl("occ",notes))


noc1<-noc%>%
  dplyr::select(-c(Site_num:prop_ownership)&
                  -c(data_source:species)&
                  -c(nest:nndist))

#change NAs to 0 to see if that will belp?

noc1<-noc1%>%
  replace(is.na(.),0)

#it did 
noc2<-noc1%>%
  group_by(new_flag1,survey_date) %>%
  summarise_all(sum)

#add together 
comb<-rbind(noc2,zoc2)

#looks good 
sort(unique(comb$new_flag1))
sort(unique(zx$new_flag1))

#now add back together 
x<-z%>%
  group_by(new_flag1,survey_date)%>%
  filter(n()==1)

x1<-x%>%
  dplyr::select(-c(Site_num:prop_ownership)&
                  -c(data_source:species)&
                  -c(nest:nndist))

xo<-x1%>%
  replace(is.na(.),0)



cox<-rbind(comb,xo)#final 

#add site_num col back in for merges 

sn_c<-z%>%
  dplyr::select(Site_num,new_flag1,Long,Lat)

nfl<-sn_c%>%
  distinct(new_flag1,Site_num,Long,Lat)

nfl2<-nfl%>%
  group_by(new_flag1)%>%
  slice(which.min(Site_num))



cox1<-merge(cox,nfl2,by=c("new_flag1"))

#relocate 

cox1<-cox1%>%
  relocate(Site_num,Long,Lat,.after=survey_date)
# 
# setwd("~/github/BRE_2022/BRE_2022/Data")
# 
# write_csv(cox1,"BRE_2022_02_09_y.csv")






