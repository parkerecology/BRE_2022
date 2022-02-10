#data formating for unmarked 


#load observation covs and presence absence data
setwd("~/github/BRE_2022/BRE_2022/Data")

### BRE population trends 2022 

#import data
library(readr)
library(tidyverse)
df <- read_csv("BRE_2022_02_05.csv")

df$survey_date<-as.Date(df$survey_date)#transform to Date class


# #figure out how to deal with shared PLME cols 
# 
# dfp<-df%>%
#   filter(species=="Southern Gray-cheeked Salamander")#filter out PLME
# 
# test<-df%>%
#   filter(Survey_ID %in% dfp$Survey_ID)#get matching survey IDs to get the doubled rows (1423)
# 
# a<-test%>%
#   filter(!grepl("ANAE",notes))#filter out rows without ANAE in the notes column (777)
#   
#  b<-test%>%
#    filter(!Survey_ID %in% a$Survey_ID)#find rows that don't match in a (664)
# 
# #make a copy of dataframe a  
# c<-a
# 
# c$species<-"Green Salamander"#change species to Greens
# c$num_adult<-0#change counts to zero 
# c[c(15:17)]<-NA#change all the green columns to NA 
# c$total_ANAE<-0#change the total_ANAE to 0
# 
# #add back to main dataframe
# 
# dff<-as.data.frame(rbind(df,c))
# 
# df<-dff%>%
#   filter(species!="Southern Gray-cheeked Salamander")#filter out PLME

#write to csv

#write_csv(df,"BRE_2022_clean_02_03.csv")


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

sef2<-sef%>%
  filter(year>=2000)

#make a plot of surveys over time by site and year 
ggplot(data = sef2,aes(year,surveys))+
  geom_line(aes(color=as.factor(Site_num)))+
  labs(x="Year", y="Surveys")+
  theme_pubr()+
  theme(legend.position="none")

#make zero survey dataframe 
nd<-data.frame(expand.grid(
  Site_num=unique(sef2$Site_num),
  year=unique(sef2$year)))

#merge with other data 

tot_eff<-merge(sef2,nd,by=c("Site_num","year"),all.y=TRUE)

tot_eff$surveys[is.na(tot_eff$surveys)]<-0

tot_eff$Site_num<-as.factor(tot_eff$Site_num)

na.value = "transparent"

#make a count of observations surveys

tt<-tot_eff%>%
  dplyr::group_by(year,Site_num)%>%
  summarise(counts = sum(surveys > 0, na.rm = TRUE))
  
gt<-tt%>%
  dplyr::group_by(Site_num)%>%
  summarise(years_surv = sum(counts > 0, na.rm = TRUE))


#make a plot of surveys over time by site and year 
tot_eff<-merge(sef2,nd,by=c("Site_num","year"),all.y=TRUE)

up<-ggplot(data = tot_eff,aes(year,Site_num,fill=surveys))+
  geom_raster()+
  labs(x="Year", y="Site num")+
  scale_fill_binned(type = "viridis",na.value = "transparent",)+
  theme_pubr()+
  theme(legend.position="none",
        axis.text.y = element_blank())

#make a plotly version to identify sites 

library(plotly)
ggplotly(up)

#get total number of surveys per site
ssf<-
  sef%>%
  group_by(Site_num)%>%
  summarise(surveys=sum(surveys))


#get max number of visits 
names(df)

gm<-df%>%
  group_by(Site_num) %>%
  mutate(n = n_distinct(survey_date))

max(gm$n)#581

#get max number of visits post 2000

df_2k<-df%>%
  filter(year>=2000)

df_2k<-df_2k%>%
  group_by(Site_num) %>%
  mutate(n = n_distinct(survey_date))

max(df_2k$n)#



m210<-df%>%
  filter(Site_num==210)


y15<-gt%>%
  filter(years_surv>=15)

y10<-gt%>%
  filter(years_surv>=10)

ys_g15<-df%>%
  filter(Site_num %in% y15$Site_num)%>%
  filter(year>=2000)

ys_g10<-df%>%
  filter(Site_num %in% y10$Site_num)%>%
  filter(year>=2000)

ys_g15$Site_num<-as.factor(ys_g15$Site_num)


ggplot(data = ys_g15,aes(year,total_ANAE))+
  geom_jitter(size=1,alpha=0.5)+
  geom_smooth(color="darkred",method ="glm",formula = y ~ x + I(x^2))+
  labs(x="Year", y="ANAE/survey")+
  facet_wrap(~Site_num,ncol = 2)+
  ylim(-10,35)+
  ggtitle("Sites with >15 years of surveys")+
  theme_pubr()+
  theme(legend.position="none")

ggplot(data = ys_g15,aes(year,total_ANAE))+
  geom_boxplot(aes(group=year))+
  geom_smooth(color="darkred",method ="glm",formula = y ~ x + I(x^2))+
  labs(x="Year", y="ANAE/survey")+
  ggtitle("Sites with >15 years of surveys")+
  theme_pubr()+
  theme(legend.position="none")

ggplot(data = ys_g15,aes(year,total_ANAE))+
  geom_boxplot(aes(group=year))+
  geom_smooth(color="darkred",method ="glm",formula = y ~ x + I(x^2))+
  labs(x="Year", y="ANAE/survey")+
  #facet_wrap(~Site_num,ncol = 2)+
  ggtitle("Sites with >15 years of surveys")+
  theme_pubr()+
  theme(legend.position="none")

ggplot(data = ys_g10,aes(year,total_ANAE))+
  geom_jitter(size=1,alpha=0.5)+
  geom_smooth(color="darkred",method = "gam", formula = y ~ s(log(x)))+
  labs(x="Year", y="ANAE/survey")+
  #facet_wrap(~Site_num,ncol = 2)+
  scale_color_manual(values= wes_palette("BottleRocket1", n = 7))+
  ggtitle("Sites with >10 years of surveys")+
  theme_pubr()+
  theme(legend.position="none")

library(tidyverse)
library(ggpubr)


#subset out sites with more than 100 surveys 

df100<-subset(gm,gm$n>=100)

ggplot(data = m210,aes(year,total_ANAE))+
  geom_jitter()+
  geom_smooth()+
  labs(x="Year", y="ANAE/survey")+
  ggtitle("Site 210")+
  theme_pubr()

ggplot(data = df100,aes(year,total_ANAE))+
  geom_jitter()+
  geom_smooth()+
  labs(x="Year", y="ANAE/survey")+
  ggtitle("Site with >100 Surveys")+
  theme_pubr()


ggplot(data = df,aes(year,total_ANAE))+
  geom_jitter()+
  geom_smooth()+
  labs(x="Year", y="ANAE/survey")+
  ggtitle("all time")+
  theme_pubr()

ggplot(data = df,aes(year,total_ANAE))+
  geom_boxplot(aes(group=year))+
  labs(x="Year", y="ANAE/survey")+
  theme_pubr()

ggplot(data = df,aes(year,total_ANAE))+
  geom_boxplot(aes(group=year))+
  ylim(0,20)+
  geom_vline(xintercept=2000, linetype="dashed", color = "red")+
  annotate("text", x = 1980, y = 18, label = "34 surveys > 20",fontface =2,color="red")+
  labs(x="Year", y="ANAE/survey")+
  theme_pubr()


