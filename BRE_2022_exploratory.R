

### BRE population trends 2022 
setwd("~/github/BRE_2022/BRE_2022")


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

library(leaflet)

leaflet(data = mef) %>% 
  addProviderTiles(providers$Stamen.Toner) %>%
addCircleMarkers(~Long, ~Lat,
  radius = ~log(surveys),
  color=c("#B3EE3A"),
  fillOpacity = 0.5
)


