# clean up the site covariates

library(readr)
library(tidyverse)
df <- read_csv("BRE_2022_02_09_y.csv",col_types = cols(survey_date = col_date(format = "%Y-%m-%d")))

#make julian date column

df$J_date<-format(df$survey_date,format="%j")
df$J_date<-as.numeric(df$J_date)
View(df)

#old database for nest or no
nf <- read_csv("Data/BRE_2022_02_05.csv")


# Add some site covarites -------------------------------------------------





#history of reproduction 

df_hor<-nf %>%
  group_by(Site_num) %>% 
  summarise(HOR = sum(nest,na.rm=TRUE))


nnf<-unique(merge(df_hor,nf[c(1,25)],by=c("Site_num")))

nnf$HOR_l<-ifelse(nnf$HOR>=1,1,0)


#now aggregate into any recorded HOR

n1<-nnf[c(3,4)]

n2<-n1%>%
  group_by(new_flag1)%>%
  summarise(es_hor=sum(HOR_l))

n2$es_hor<-ifelse(n2$es_hor>=1,1,0)

n2<-plyr::rename(n2,c("es_hor"="HOR"))


#bd swabs

library(readxl)
bd <- read_excel("Data/Final_all BRE green Bd swabs_for Kevin.xlsx", 
                 col_types = c("skip", "skip", "skip", 
                               "date", "skip", "text", "numeric", 
                               "skip"))


bd$Bd_result_n<-ifelse(grepl("Not",bd$Bd_result),0,1)

bd1<-merge(bd,nnf[c(1,3)],by=c("Site_num"))

bd2<-bd1%>%
  group_by(new_flag1)%>%
  summarise(bd_sum=sum(Bd_result_n))

nfg<-data.frame(new_flag1=unique(df$new_flag1))

bd3<-merge(bd2,nfg,by=c("new_flag1"),all.y=TRUE)

bd3<-plyr::rename(bd3,c("bd_sum"="bd_result"))


sc<-merge(bd3,n2,by=c("new_flag1"))


# format YSCs -------------------------------------------------------------

#current year max temp
tmax_c <- read_csv("Data/tmax_current_year_ysc_L.csv")
View(tmax_c)


#make a new dataframe with new_flag designations 

flags<-df%>%
  distinct(new_flag1,Site_num)

tmax_c<-merge(tmax_c,flags,by=c("Site_num"))

#make a quick plot of number of eggs over time
library(ggplot2)
library(ggpubr)


ggplot(data=df,aes(J_date,num_eggs))+
  geom_point()+
  labs(x="Julian date",y="Number of eggs")+
  annotate("text", x = 200, y = 75, label = "July 20 peak",fontface =2,color="red")+
  theme_pubr()


#get july max temp
tmax_c_July<-tmax_c%>%
  filter(month==7)

#Subset out needed rows
tmax_c_July<-tmax_c_July%>%
  select(new_flag1,Year,tmax)

#turn to wide dataframe by year
#data_wide <- spread(olddata_long, condition, measurement)
tmax_c_July<-spread(tmax_c_July,Year,tmax)

tmax_c_July<-tmax_c_July%>%
  rename_with(~paste0(.,"_tmax_c_July"), -new_flag1)


#get August max temp
tmax_c_August<-tmax_c%>%
  filter(month==8)

#Subset out needed rows
tmax_c_August<-tmax_c_August%>%
  select(new_flag1,Year,tmax)

#turn to wide dataframe by year
#data_wide <- spread(olddata_long, condition, measurement)
tmax_c_August<-spread(tmax_c_August,Year,tmax)

tmax_c_August<-tmax_c_August%>%
  rename_with(~paste0(.,"_tmax_c_August"), -new_flag1)


#previous year max temp 

tmax_p <- read_csv("Data/tmax_past_year_ysc_L.csv")

#merge with new dataframe with new_flag designations 

tmax_p<-merge(tmax_p,flags,by=c("Site_num"))


#get july max temp
tmax_p_July<-tmax_p%>%
  filter(month==7)

#Subset out needed rows
tmax_p_July<-tmax_p_July%>%
  select(new_flag1,Year,tmax)

#turn to wide dataframe by year
#data_wide <- spread(olddata_long, condition, measurement)
tmax_p_July<-spread(tmax_p_July,Year,tmax)

tmax_p_July<-tmax_p_July%>%
  rename_with(~paste0(.,"_tmax_p_July"), -new_flag1)



#get August max temp
tmax_p_August<-tmax_p%>%
  filter(month==8)

#Subset out needed rows
tmax_p_August<-tmax_p_August%>%
  select(new_flag1,Year,tmax)

#turn to wide dataframe by year
#data_wide <- spread(olddata_long, condition, measurement)
tmax_p_August<-spread(tmax_p_August,Year,tmax)

tmax_p_August<-tmax_p_August%>%
  rename_with(~paste0(.,"_tmax_p_August"), -new_flag1)



#minimum temperature current year
tmin_c <- read_csv("Data/tmin_current_year_ysc_L.csv")

#merge with new dataframe with new_flag designations 
tmin_c<-merge(tmin_c,flags,by=c("Site_num"))


#get Dec max temp
tmin_c_Dec<-tmin_c%>%
  filter(month==12)

#Subset out needed rows
tmin_c_Dec<-tmin_c_Dec%>%
  select(new_flag1,Year,tmin)

#turn to wide dataframe by year
#data_wide <- spread(olddata_long, condition, measurement)
tmin_c_Dec<-spread(tmin_c_Dec,Year,tmin)

tmin_c_Dec<-tmin_c_Dec%>%
  rename_with(~paste0(.,"_tmin_c_Dec"), -new_flag1)



#get Jan max temp
tmin_c_Jan<-tmin_c%>%
  filter(month==1)

#Subset out needed rows
tmin_c_Jan<-tmin_c_Jan%>%
  select(new_flag1,Year,tmin)

#turn to wide dataframe by year
#data_wide <- spread(olddata_long, condition, measurement)
tmin_c_Jan<-spread(tmin_c_Jan,Year,tmin)

tmin_c_Jan<-tmin_c_Jan%>%
  rename_with(~paste0(.,"_tmin_c_Jan"), -new_flag1)


#minimum temperature previous year
tmin_p <- read_csv("Data/tmin_past_year_ysc_L.csv")

#merge with new dataframe with new_flag designations 
tmin_p<-merge(tmin_p,flags,by=c("Site_num"))


#get Dec max temp
tmin_p_Dec<-tmin_p%>%
  filter(month==12)

#Subset out needed rows
tmin_p_Dec<-tmin_p_Dec%>%
  select(new_flag1,Year,tmin)

#turn to wide dataframe by year
#data_wide <- spread(olddata_long, condition, measurement)
tmin_p_Dec<-spread(tmin_p_Dec,Year,tmin)

tmin_p_Dec<-tmin_p_Dec%>%
  rename_with(~paste0(.,"_tmin_p_Dec"), -new_flag1)



#get Jan max temp
tmin_p_Jan<-tmin_p%>%
  filter(month==1)

#Subset out needed rows
tmin_p_Jan<-tmin_p_Jan%>%
  select(new_flag1,Year,tmin)

#turn to wide dataframe by year
#data_wide <- spread(olddata_long, condition, measurement)
tmin_p_Jan<-spread(tmin_p_Jan,Year,tmin)

tmin_p_Jan<-tmin_p_Jan%>%
rename_with(~paste0(.,"_tmin_p_Jan"), -new_flag1)


#

# PDSI TerraClimate -------------------------------------------------------


pdsi <- read_csv("Data/BRE_2022_PDSI.csv",col_types = cols(`system:index` = col_skip()))


pdsi<-merge(pdsi,flags,by=c("Site_num"))


pdsi<-pdsi%>%
  dplyr::select(-c(.geo,Site_num))%>%
  relocate(new_flag1)
  
pdsi_n<-pdsi[-c(1)]

names(pdsi_n)<-gsub("^(.{4})(.*)$","\\1.\\2", names(pdsi_n))

pdsi<-cbind(pdsi[c(1)],pdsi_n)


pdsi_c_July<-
  pdsi%>%
  select_if(stringr::str_detect(names(.),".07_") |
            stringr::str_detect(names(.),"new_flag1"))



pdsi_c_August<-
  pdsi%>%
  select_if(stringr::str_detect(names(.),".08_") |
              stringr::str_detect(names(.),"new_flag1"))


#make previous year pdsi


pdsi_p<-pdsi[-c(1)]

names(pdsi_p) <- str_replace(names(pdsi_p), substring(names(pdsi_p),1,4), function(x) as.numeric(x) + 1)

pdsi_p<- cbind(pdsi[c(1)],pdsi_p)

pdsi_p<-pdsi_p%>%
rename_with(~paste0(.,"_p"), -new_flag1)


pdsi_p_July<-
  pdsi_p%>%
  select_if(stringr::str_detect(names(.),".07_") |
              stringr::str_detect(names(.),"new_flag1"))



pdsi_p_August<-
  pdsi_p%>%
  select_if(stringr::str_detect(names(.),".08_") |
              stringr::str_detect(names(.),"new_flag1"))





#merge 

ysc<-pdsi_c_August%>%
  inner_join(pdsi_c_July,by=c("new_flag1"))%>%
  inner_join(pdsi_p_July,by=c("new_flag1"))%>%
  inner_join(pdsi_p_August,by=c("new_flag1"))%>%
  inner_join(tmax_c_July,by=c("new_flag1"))%>%
  inner_join(tmax_p_July,by=c("new_flag1"))%>%
  inner_join(tmax_c_August,by=c("new_flag1"))%>%
  inner_join(tmax_p_August,by=c("new_flag1"))%>%
  inner_join(tmin_c_Dec,by=c("new_flag1"))%>%
  inner_join(tmin_c_Jan,by=c("new_flag1"))%>%
  inner_join(tmin_p_Dec,by=c("new_flag1"))%>%
  inner_join(tmin_p_Jan,by=c("new_flag1"))
  


a<-names(ysc)

ysc<-
  ysc%>%
  select_if(!stringr::str_detect(names(.),"1999")&
              !stringr::str_detect(names(.),"2021"))


b<-names(ysc)

setdiff(a,b)#all good 



# detection covariates ----------------------------------------------------

#make a year column
df$year<-format(df$survey_date,format="%Y")#extract year
df$year<-as.numeric(df$year)#change to numeric


#add season 
df$Season<-cut(df$J_date,
               breaks=c(-Inf,91,160,275,330,365),
               labels=c("Winter","Spring","Summer","Fall","Winter"))


#format observations for unmarked dataframe
df2<-subset(df,df$survey_date>=as.Date("2000-01-01"))

df2<-
  df2%>%
  dplyr::arrange(survey_date)%>%
  dplyr::group_by(year,new_flag1) %>%
  dplyr::mutate(Count = 1:n())


#new dataset for formating umf
nd<-data.frame(expand.grid(
  year=seq(2000,2020),
  Count=seq(1,72),
  new_flag1=unique(df2$new_flag1)))

#merge to fill spaces, makes NA for unmarked
df2_na<-merge(df2,nd,by=c("new_flag1","year", "Count"),all=TRUE)

length(unique(df2_na$new_flag1))
length(unique(df2$new_flag1))

mean(max_visits$max_C)

#change to wide format
library(data.table)
df2_w<-data.table::dcast(setDT(df2_na),new_flag1~year+Count, value.var=c("total_ANAE","J_date","Season"))

length(unique(df2_w$new_flag1))

# setwd("~/github/BRE_2022/BRE_2022/Data")
# write_csv(df2_w,"BRE_2022_wide_Y_obsCovs.csv")


# Filter out sites before 2000 in YSC and SC -----------------------------


sc<-sc%>%
  filter(new_flag1 %in% df2_w$new_flag1)

ysc<-ysc%>%
  filter(new_flag1 %in% df2_w$new_flag1)
  
# setwd("~/github/BRE_2022/BRE_2022/Data")
# write_csv(ysc,"YSC_2022_02_11.csv")
# write_csv(sc,"BRE_2022_siteCovs.csv")

