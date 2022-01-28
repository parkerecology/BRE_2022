
#Extract GIS related varibles


#load observation covs and presence absence data
setwd("~/github/BRE_2022/BRE_2022")


### BRE population trends 2022 

#import data
library(readr)
df <- read_csv("Data/BRE surveys_22.csv")
View(df)

#cut out PLME 
unique(df$species)#get exact spelling 

df<-subset(df,df$species!="Southern Gray-cheeked Salamander")#subset everything that is NOT PLME

#change to date format
df$survey_date<-as.Date(df$survey_date, format = "%m/%d/%y")#change survey date to date format

#get R to not turn the 2 digit years to future dates, e.g. 1/01/48 is not 1/01/2048, but 1/01/1948
df$survey_date<-as.Date(ifelse(df$survey_date > "2021-12-31", format(df$survey_date, "19%y-%m-%d"), format(df$survey_date)))

#make a year column 
df$year<-format(df$survey_date,format="%Y")#extract year 
df$year<-as.numeric(df$year)#change to numeric


#load the tidyverse for pipes etc...
library(tidyverse)

#aggregate by year to get number of surveys per year

ef<-
  df%>%
  group_by(year)%>%
  summarise(surveys=n())

#make a plot of effort over time 

library(ggplot2)
library(ggpubr)

ggplot(data = ef,aes(year,surveys))+
  geom_line()+
  geom_hline(yintercept=20, linetype="dashed", color = "red")+
  annotate("text", x = 2020, y = 48, label = "20 Surveys",fontface =2)+
  labs(x="Year", y="Surveys")+
  theme_pubr()



#aggregate by year, site number to get number of surveys per year

sef<-
  df%>%
  group_by(year,Site_num)%>%
  summarise(surveys=n())

#get total number of surveys per site
ssf<-
  sef%>%
  group_by(Site_num)%>%
  summarise(surveys=sum(surveys))


#get coords for quick map

xy<-df[c(1,4,5)]

mef<-merge(ssf,xy,by=c("Site_num"))

mef[!complete.cases(mef), ]#site_num 2334 no lat long (drop it)

mef<-subset(mef,mef$Site_num!="2334")

coords <- data.frame(mef[,c(4,3)])

coords<-coords%>%
  distinct()

a<-mef%>%
  distinct(Site_num,.keep_all = TRUE)


a$ind<-duplicated(a[,c(3,4)])

a[ind,]

#load raster packages 
library(rasterVis)
library(raster)
library(rgdal)
library(terra)



# MODIS MOD44B.006 VCF percent tree cover ---------------------------------

#avialible here: https://lpdaac.usgs.gov/products/mod44bv006/

#set working directory to netcdf location 
setwd("/Volumes/Seagate Expansion Drive/GIS/USA/NC/MODIS")
r_brick<-brick("MOD44B.006_250m_Greens.nc",varname="Percent_Tree_Cover")

r_brick[r_brick>100] <- NA


gplot(r_brick[[1]])+
  geom_raster(aes(fill=value))+
  geom_point(
    data=coords,
    aes(x=Long,y=Lat),col="red")+
  coord_equal()

values <- data.frame(extract(r_brick, coords))
names(values) <- gsub(pattern = "X", replacement = "cc_250m_", x = names(values))
names(values) <- substring(names(values),1,12)

s.no<-data.frame(mef[,c(1,4,3)])

gf <- cbind.data.frame(s.no,values)

cc_250m <- gather(gf,year,cc_250m,cc_250m_2000:cc_250m_2020, factor_key=TRUE)

# setwd("~/github/BRE_2022/BRE_2022/Data")
# 
# write_csv(cc_250m,"Yearly_canopy_250m.csv")


# DAYMET data download and formatting monthly precip, max temp and min temp ----------------------------------------------------

library(tidyverse)
library(patchwork)
library(raster)
library(daymetr)
library(rasterVis)
library(rgdal)
library(ggplot2)
library(ggpubr)
library(rcartocolor)
library(viridis)



#download the daymet netCDFs subset to western NC
# download_daymet_ncss(location = c(37, -84, 34.5, -81.5),
#                      start = 1999,
#                      end = 2020,
#                      frequency = "monthly",
#                      param = c("tmin","tmax","prcp"),
#                      path = "/Volumes/Seagate Expansion Drive/GIS/USA/NC/Daymet/Monthly aggregate",
#                      silent = TRUE)



# DAYMET max temp --------------------------------------------

setwd("/Volumes/Seagate Expansion Drive/GIS/USA/NC/Daymet/Monthly aggregate")


tmax_99<-brick("tmax_monavg_1999_ncss.nc")
tmax_00<-brick("tmax_monavg_2000_ncss.nc")
tmax_01<-brick("tmax_monavg_2001_ncss.nc")
tmax_02<-brick("tmax_monavg_2002_ncss.nc")
tmax_03<-brick("tmax_monavg_2003_ncss.nc")
tmax_04<-brick("tmax_monavg_2004_ncss.nc")
tmax_05<-brick("tmax_monavg_2005_ncss.nc")
tmax_06<-brick("tmax_monavg_2006_ncss.nc")
tmax_07<-brick("tmax_monavg_2007_ncss.nc")
tmax_08<-brick("tmax_monavg_2008_ncss.nc")
tmax_09<-brick("tmax_monavg_2009_ncss.nc")
tmax_10<-brick("tmax_monavg_2010_ncss.nc")
tmax_11<-brick("tmax_monavg_2011_ncss.nc")
tmax_12<-brick("tmax_monavg_2012_ncss.nc")
tmax_13<-brick("tmax_monavg_2013_ncss.nc")
tmax_14<-brick("tmax_monavg_2014_ncss.nc")
tmax_15<-brick("tmax_monavg_2015_ncss.nc")
tmax_16<-brick("tmax_monavg_2016_ncss.nc")
tmax_17<-brick("tmax_monavg_2017_ncss.nc")
tmax_18<-brick("tmax_monavg_2018_ncss.nc")
tmax_19<-brick("tmax_monavg_2019_ncss.nc")
tmax_20<-brick("tmax_monavg_2020_ncss.nc")



# to set the correct projection
raster::projection(tmax_99) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_00) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_01) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_02) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_03) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_04) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_05) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_06) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_07) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_08) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_09) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_10) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_11) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_12) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_13) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_14) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_15) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_16) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_17) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_18) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_19) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmax_20) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"



# re-project to lat lon
tmax_99 <- raster::projectRaster(tmax_99, crs = "+init=epsg:4326")
tmax_00 <- raster::projectRaster(tmax_00, crs = "+init=epsg:4326")
tmax_01 <- raster::projectRaster(tmax_01, crs = "+init=epsg:4326")
tmax_02 <- raster::projectRaster(tmax_02, crs = "+init=epsg:4326")
tmax_03 <- raster::projectRaster(tmax_03, crs = "+init=epsg:4326")
tmax_04 <- raster::projectRaster(tmax_04, crs = "+init=epsg:4326")
tmax_05 <- raster::projectRaster(tmax_05, crs = "+init=epsg:4326")
tmax_06 <- raster::projectRaster(tmax_06, crs = "+init=epsg:4326")
tmax_07 <- raster::projectRaster(tmax_07, crs = "+init=epsg:4326")
tmax_08 <- raster::projectRaster(tmax_08, crs = "+init=epsg:4326")
tmax_09 <- raster::projectRaster(tmax_09, crs = "+init=epsg:4326")
tmax_10 <- raster::projectRaster(tmax_10, crs = "+init=epsg:4326")
tmax_11 <- raster::projectRaster(tmax_11, crs = "+init=epsg:4326")
tmax_12 <- raster::projectRaster(tmax_12, crs = "+init=epsg:4326")
tmax_13 <- raster::projectRaster(tmax_13, crs = "+init=epsg:4326")
tmax_14 <- raster::projectRaster(tmax_14, crs = "+init=epsg:4326")
tmax_15 <- raster::projectRaster(tmax_15, crs = "+init=epsg:4326")
tmax_16 <- raster::projectRaster(tmax_16, crs = "+init=epsg:4326")
tmax_17 <- raster::projectRaster(tmax_17, crs = "+init=epsg:4326")
tmax_18 <- raster::projectRaster(tmax_18, crs = "+init=epsg:4326")
tmax_19 <- raster::projectRaster(tmax_19, crs = "+init=epsg:4326")
tmax_20 <- raster::projectRaster(tmax_20, crs = "+init=epsg:4326")


#check coords for crs match 
gplot(tmax_20[[5]])+
  geom_raster(aes(fill=value))+
  geom_point(
    data=coords,
    aes(x=Long,y=Lat),col="blue",fill="turquoise1",size=0.5)+
  scale_fill_carto_c(palette = "Geyser",na.value="white")+
  coord_equal()+
  theme_pubr()


#one row missing values, which row? (fixed) 

library(leaflet)

leaflet(data = mef) %>% 
  addProviderTiles(providers$Stamen.Toner) %>%
  addCircleMarkers(~Long, ~Lat,
                   radius = ~log(surveys),
                   color=c("#B3EE3A"),
                   fillOpacity = 0.5
  )





library(maps)
v_tmax_99 <- data.frame(terra::extract(tmax_99, coords))
v_tmax_00 <- data.frame(terra::extract(tmax_00, coords))
v_tmax_01 <- data.frame(terra::extract(tmax_01, coords))
v_tmax_02 <- data.frame(terra::extract(tmax_02, coords))
v_tmax_03 <- data.frame(terra::extract(tmax_03, coords))
v_tmax_04 <- data.frame(terra::extract(tmax_04, coords))
v_tmax_05 <- data.frame(terra::extract(tmax_05, coords))
v_tmax_06 <- data.frame(terra::extract(tmax_06, coords))
v_tmax_07 <- data.frame(terra::extract(tmax_07, coords))
v_tmax_08 <- data.frame(terra::extract(tmax_08, coords))
v_tmax_09 <- data.frame(terra::extract(tmax_09, coords))
v_tmax_10 <- data.frame(terra::extract(tmax_10, coords))
v_tmax_11 <- data.frame(terra::extract(tmax_11, coords))
v_tmax_12 <- data.frame(terra::extract(tmax_12, coords))
v_tmax_13 <- data.frame(terra::extract(tmax_13, coords))
v_tmax_14 <- data.frame(terra::extract(tmax_14, coords))
v_tmax_15 <- data.frame(terra::extract(tmax_15, coords))
v_tmax_16 <- data.frame(terra::extract(tmax_16, coords))
v_tmax_17 <- data.frame(terra::extract(tmax_17, coords))
v_tmax_18 <- data.frame(terra::extract(tmax_18, coords))
v_tmax_19 <- data.frame(terra::extract(tmax_19, coords))
v_tmax_20 <- data.frame(terra::extract(tmax_20, coords))


#combine into a single dataframe 

tmax_all<-cbind(v_tmax_99,
                v_tmax_00,
                v_tmax_01,
                v_tmax_02,
                v_tmax_03,
                v_tmax_04,
                v_tmax_05,
                v_tmax_06,
                v_tmax_07,
                v_tmax_08,
                v_tmax_09,
                v_tmax_10,
                v_tmax_11,
                v_tmax_12,
                v_tmax_13,
                v_tmax_14,
                v_tmax_15,
                v_tmax_16,
                v_tmax_17,
                v_tmax_18,
                v_tmax_19,
                v_tmax_20)

tmax_all$Site_num<-unique(mef$Site_num)

#Check for NAs
tmax_all[!complete.cases(tmax_all), ]#no NAs

#rename
names(tmax_all) <- gsub(pattern = "X", replacement = "tmax_", x = names(tmax_all))
names(tmax_all) <- substring(names(tmax_all),1,12)

tmax_all <- tmax_all%>%
  relocate("Site_num")

#make a copy to convert to previous year's tmax
tmax_all.1<-tmax_all

names(tmax_all.1[2:265])<-str_replace(names(tmax_all.1[2:265]), substring(names(tmax_all.1[2:265]),6,9), function(x) as.numeric(x) + 1)

names(tmax_all.1)

library(stringr)
tmax.1_bh<-
  tmax_all.1%>%
  select_if(stringr::str_detect(names(.),".06") | 
              stringr::str_detect(names(.),".07") |
              stringr::str_detect(names(.),".08") |
              stringr::str_detect(names(.),".09") |
              stringr::str_detect(names(.),".10") |
              stringr::str_detect(names(.),"site_code"))



#gather vars into long format
tmax_l <- gather(tmax_all,time,tmax,tmax_1999.01:tmax_2020.12, factor_key=TRUE)

#gather previous year vars into long format
tmax.1_l <- gather(tmax_all.1,time,tmax,tmax_2000.01:tmax_2021.12, factor_key=TRUE)


#add new cols for month and year
tmax_l$Year<-substring(tmax_l$time,6,9)
tmax_l$month<-substring(tmax_l$time,11,12)

#some formatting
#tmax_l$Year_d<-as.Date(tmax_l$Year,format="%Y")
tmax_l$Year_n<-as.numeric(tmax_l$Year)

#change month to numeric 
tmax_l$month<-as.numeric(tmax_l$month)

#previous year

#add new cols for month and year
tmax.1_l$Year<-substring(tmax.1_l$time,6,9)
tmax.1_l$month<-substring(tmax.1_l$time,11,12)

#some formatting
#tmax.1_l$Year_d<-as.Date(tmax.1_l$Year,format="%Y")
tmax.1_l$Year_n<-as.numeric(tmax.1_l$Year)

#change month to numeric 
tmax.1_l$month<-as.numeric(tmax.1_l$month)






# DAYMET min temp ---------------------------------------------------------

setwd("/Volumes/Seagate Expansion Drive/GIS/USA/NC/Daymet/Monthly aggregate")


tmin_99<-brick("tmin_monavg_1999_ncss.nc")
tmin_00<-brick("tmin_monavg_2000_ncss.nc")
tmin_01<-brick("tmin_monavg_2001_ncss.nc")
tmin_02<-brick("tmin_monavg_2002_ncss.nc")
tmin_03<-brick("tmin_monavg_2003_ncss.nc")
tmin_04<-brick("tmin_monavg_2004_ncss.nc")
tmin_05<-brick("tmin_monavg_2005_ncss.nc")
tmin_06<-brick("tmin_monavg_2006_ncss.nc")
tmin_07<-brick("tmin_monavg_2007_ncss.nc")
tmin_08<-brick("tmin_monavg_2008_ncss.nc")
tmin_09<-brick("tmin_monavg_2009_ncss.nc")
tmin_10<-brick("tmin_monavg_2010_ncss.nc")
tmin_11<-brick("tmin_monavg_2011_ncss.nc")
tmin_12<-brick("tmin_monavg_2012_ncss.nc")
tmin_13<-brick("tmin_monavg_2013_ncss.nc")
tmin_14<-brick("tmin_monavg_2014_ncss.nc")
tmin_15<-brick("tmin_monavg_2015_ncss.nc")
tmin_16<-brick("tmin_monavg_2016_ncss.nc")
tmin_17<-brick("tmin_monavg_2017_ncss.nc")
tmin_18<-brick("tmin_monavg_2018_ncss.nc")
tmin_19<-brick("tmin_monavg_2019_ncss.nc")
tmin_20<-brick("tmin_monavg_2020_ncss.nc")



# to set the correct projection
raster::projection(tmin_99) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_00) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_01) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_02) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_03) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_04) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_05) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_06) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_07) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_08) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_09) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_10) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_11) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_12) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_13) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_14) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_15) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_16) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_17) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_18) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_19) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"
raster::projection(tmin_20) <- "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=km +no_defs"



# re-project to lat lon
tmin_99 <- raster::projectRaster(tmin_99, crs = "+init=epsg:4326")
tmin_00 <- raster::projectRaster(tmin_00, crs = "+init=epsg:4326")
tmin_01 <- raster::projectRaster(tmin_01, crs = "+init=epsg:4326")
tmin_02 <- raster::projectRaster(tmin_02, crs = "+init=epsg:4326")
tmin_03 <- raster::projectRaster(tmin_03, crs = "+init=epsg:4326")
tmin_04 <- raster::projectRaster(tmin_04, crs = "+init=epsg:4326")
tmin_05 <- raster::projectRaster(tmin_05, crs = "+init=epsg:4326")
tmin_06 <- raster::projectRaster(tmin_06, crs = "+init=epsg:4326")
tmin_07 <- raster::projectRaster(tmin_07, crs = "+init=epsg:4326")
tmin_08 <- raster::projectRaster(tmin_08, crs = "+init=epsg:4326")
tmin_09 <- raster::projectRaster(tmin_09, crs = "+init=epsg:4326")
tmin_10 <- raster::projectRaster(tmin_10, crs = "+init=epsg:4326")
tmin_11 <- raster::projectRaster(tmin_11, crs = "+init=epsg:4326")
tmin_12 <- raster::projectRaster(tmin_12, crs = "+init=epsg:4326")
tmin_13 <- raster::projectRaster(tmin_13, crs = "+init=epsg:4326")
tmin_14 <- raster::projectRaster(tmin_14, crs = "+init=epsg:4326")
tmin_15 <- raster::projectRaster(tmin_15, crs = "+init=epsg:4326")
tmin_16 <- raster::projectRaster(tmin_16, crs = "+init=epsg:4326")
tmin_17 <- raster::projectRaster(tmin_17, crs = "+init=epsg:4326")
tmin_18 <- raster::projectRaster(tmin_18, crs = "+init=epsg:4326")
tmin_19 <- raster::projectRaster(tmin_19, crs = "+init=epsg:4326")
tmin_20 <- raster::projectRaster(tmin_20, crs = "+init=epsg:4326")


#check coords for crs match 
gplot(tmin_20[[5]])+
  geom_raster(aes(fill=value))+
  geom_point(
    data=coords,
    aes(x=Long,y=Lat),col="blue",fill="turquoise1",size=0.5)+
  scale_fill_carto_c(palette = "Geyser",na.value="white")+
  coord_equal()+
  theme_pubr()


#one row missing values, which row? (fixed) 

library(leaflet)

leaflet(data = mef) %>% 
  addProviderTiles(providers$Stamen.Toner) %>%
  addCircleMarkers(~Long, ~Lat,
                   radius = ~log(surveys),
                   color=c("#B3EE3A"),
                   fillOpacity = 0.5
  )





library(maps)
v_tmin_99 <- data.frame(terra::extract(tmin_99, coords))
v_tmin_00 <- data.frame(terra::extract(tmin_00, coords))
v_tmin_01 <- data.frame(terra::extract(tmin_01, coords))
v_tmin_02 <- data.frame(terra::extract(tmin_02, coords))
v_tmin_03 <- data.frame(terra::extract(tmin_03, coords))
v_tmin_04 <- data.frame(terra::extract(tmin_04, coords))
v_tmin_05 <- data.frame(terra::extract(tmin_05, coords))
v_tmin_06 <- data.frame(terra::extract(tmin_06, coords))
v_tmin_07 <- data.frame(terra::extract(tmin_07, coords))
v_tmin_08 <- data.frame(terra::extract(tmin_08, coords))
v_tmin_09 <- data.frame(terra::extract(tmin_09, coords))
v_tmin_10 <- data.frame(terra::extract(tmin_10, coords))
v_tmin_11 <- data.frame(terra::extract(tmin_11, coords))
v_tmin_12 <- data.frame(terra::extract(tmin_12, coords))
v_tmin_13 <- data.frame(terra::extract(tmin_13, coords))
v_tmin_14 <- data.frame(terra::extract(tmin_14, coords))
v_tmin_15 <- data.frame(terra::extract(tmin_15, coords))
v_tmin_16 <- data.frame(terra::extract(tmin_16, coords))
v_tmin_17 <- data.frame(terra::extract(tmin_17, coords))
v_tmin_18 <- data.frame(terra::extract(tmin_18, coords))
v_tmin_19 <- data.frame(terra::extract(tmin_19, coords))
v_tmin_20 <- data.frame(terra::extract(tmin_20, coords))


#combine into a single dataframe 

tmin_all<-cbind(v_tmin_99,
                v_tmin_00,
                v_tmin_01,
                v_tmin_02,
                v_tmin_03,
                v_tmin_04,
                v_tmin_05,
                v_tmin_06,
                v_tmin_07,
                v_tmin_08,
                v_tmin_09,
                v_tmin_10,
                v_tmin_11,
                v_tmin_12,
                v_tmin_13,
                v_tmin_14,
                v_tmin_15,
                v_tmin_16,
                v_tmin_17,
                v_tmin_18,
                v_tmin_19,
                v_tmin_20)

tmin_all$Site_num<-unique(mef$Site_num)

#Check for NAs
tmin_all[!complete.cases(tmin_all), ]#no NAs

#rename
names(tmin_all) <- gsub(pattern = "X", replacement = "tmin_", x = names(tmin_all))
names(tmin_all) <- substring(names(tmin_all),1,12)

tmin_all <- tmin_all%>%
  relocate("Site_num")




# TerraClimate PDSI -------------------------------------------------------

setwd("/Volumes/Seagate Expansion Drive/GIS/USA/NC/Terraclimate")
r_brick<-brick("TerraClimate_PDSI_2020.nc")

r_brick[r_brick>100] <- NA


gplot(r_brick[[1]])+
  geom_raster(aes(fill=value))+
  geom_point(
    data=coords,
    aes(x=Long,y=Lat),col="red")+
  coord_equal()

