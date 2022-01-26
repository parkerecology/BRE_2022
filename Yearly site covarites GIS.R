
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

coords <- data.frame(mef[,c(4,3)])

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


# DAYMET data monthly max temp and min temp ----------------------------------------------------

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
download_daymet_ncss(location = c(37, -84, 34.5, -81.5),
                     start = 2000,
                     end = 2018,
                     frequency = "monthly",
                     param = c("tmin","tmax","prcp"),
                     path = "/Volumes/Seagate Expansion Drive/GIS/USA/NC/Daymet/Monthly aggregate",
                     silent = TRUE)

setwd("/Volumes/Seagate Expansion Drive/GIS/USA/NC/Daymet/Monthly aggregate")


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



# reproject to lat lon
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
gplot(tmax_20[[4]])+
  geom_raster(aes(fill=value))+
  geom_point(
    data=coords,
    aes(x=Long,y=Lat),col="blue",fill="turquoise1",size=0.5)+
  scale_fill_carto_c(palette = "Geyser",na.value="white")+
  coord_equal()+
  theme_pubr()

