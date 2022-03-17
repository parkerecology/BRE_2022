#### Unmarked Dail-Madsen N-Mixture models Aneides aeneus 2000-2020

options(max.print =60000)


library(readr)
library(tidyverse)

setwd("~/github/BRE_2022/BRE_2022/Data")
# Load counts and observation covariates
y_obs <- read_csv("BRE_2022_wide_Y_obsCovs.csv")

#change format to numeric 
y_obs<-y_obs%>%
mutate(across(where(is.logical), as.numeric))


# #change season to factor
# J_date=as.data.frame(y_obs[,c(1543:3054)])
# Season=as.data.frame(y_obs[,c(3084:4595)])
# 
# Season<-Season%>%
#   mutate(across(where(is.numeric), as.factor))

#add empty columns for the rest of 2021

nam<-paste0("Season_2021_",30:72)
tot<-paste0("total_ANAE_2021_",30:72)
jdat<-paste0("J_date_2021_",30:72)

y_obs[nam]<-NA#add placeholder NA cols for Season

y_obs[tot]<-NA#add placeholder NA cols for counts

y_obs[jdat]<-NA#add placeholder NA cols for J_date

#move them behind others
y_obs<-y_obs%>%
  relocate(any_of(tot),.after = total_ANAE_2021_29)

#move them behind others
y_obs<-y_obs%>%
  relocate(any_of(jdat),.after = J_date_2021_29)

#load yearly site covariates
ysc <- read_csv("YSC_2022_02_11.csv")


#add placeholder NA cols in the YSC

ysc<-ysc%>%
  mutate(`2021.08_pdsi`=NA,.after=`2020.08_pdsi`)%>%
  mutate(`2021.07_pdsi`=NA,.after=`2020.07_pdsi`)%>%
  mutate(`2021.07_pdsi_p`=NA,.after=`2020.07_pdsi_p`)%>%
  mutate(`2021.08_pdsi_p`=NA,.after=`2020.08_pdsi_p`)%>%
  mutate(`2021_tmax_c_July`=NA,.after=`2020_tmax_c_July`)%>%
  mutate(`2021_tmax_p_July`=NA,.after=`2020_tmax_p_July`)%>%
  mutate(`2021_tmax_c_August`=NA,.after=`2020_tmax_c_August`)%>%
  mutate(`2021_tmax_p_August`=NA,.after=`2020_tmax_p_August`)%>%
  mutate(`2021_tmin_c_Dec`=NA,.after=`2020_tmin_c_Dec`)%>%
  mutate(`2021_tmin_c_Jan`=NA,.after=`2020_tmin_c_Jan`)%>%
  mutate(`2021_tmin_p_Dec`=NA,.after=`2020_tmin_p_Dec`)%>%
  mutate(`2021_tmin_p_Jan`=NA,.after=`2020_tmin_p_Jan`)




#load site covariates 
sc <- read_csv("BRE_2022_siteCovs.csv")

# load new site covariates 
sites <- read_csv("sites_agg_02_22.csv")

sc<-merge(sc,sites,by=("new_flag1"))

sc<-sc%>%
  dplyr::select(new_flag1,bd_result,HOR,nndist,DuPont,private)

#make unmarked dataframe

library(unmarked)


umf_bre <- unmarkedFramePCO(y=y_obs[,c(2:1585)],
                            siteCovs =sc,
                            obsCovs=list(
                            J_date=as.data.frame(y_obs[,1586:3169]),
                            Season=as.data.frame(y_obs[,3170:4753])),
                            yearlySiteCovs = list(
                              pdsi_aug_c=as.data.frame(ysc[,c(2:23)]),
                              pdsi_july_c=as.data.frame(ysc[,c(24:45)]),
                              pdsi_july_p=as.data.frame(ysc[,c(46:67)]),
                              pdsi_aug_p=as.data.frame(ysc[,c(68:89)]),
                              tmax_july_c=as.data.frame(ysc[,c(90:111)]),
                              tmax_july_p=as.data.frame(ysc[,c(112:133)]),
                              tmax_aug_c=as.data.frame(ysc[,c(134:155)]),
                              tmax_aug_p=as.data.frame(ysc[,c(156:177)]),
                              tmin_dec_c=as.data.frame(ysc[,c(178:199)]),
                              tmin_jan_c=as.data.frame(ysc[,c(200:221)]),
                              tmin_dec_p=as.data.frame(ysc[,c(222:243)]),
                              tmin_jan_p=as.data.frame(ysc[,c(244:265)])),
                            numPrimary = 22)


summary(umf_bre)
plot(umf_bre)

#set factor levels
umf_bre@obsCovs$Season<-factor(umf_bre@obsCovs$Season,levels=c("Spring","Summer","Fall","Winter"))



# Detection models  -------------------------------------------------------
library(beepr)

#Null model
system.time(
  m.null <- pcountOpen(~1, ~1, ~1, ~1, umf_bre, K=150)
  )
summary(m.null)
beep(8)

#Season detection model
system.time(
  p.season <- pcountOpen(~1, ~1, ~1, ~Season, umf_bre, K=150)
  )
summary(p.season)
beep(8)
         



# Abundance models --------------------------------------------------------


#Season detection model
system.time(
  l.HOR <- pcountOpen(~HOR, ~1, ~1, ~Season, umf_bre, K=150)
)
summary(l.HOR)
beep(8)



# omega models (survival probability) -------------------------------------


#previous year's drought conditions during july 
system.time(
  o.pdsi_july_p <- pcountOpen(~HOR, ~1, ~scale(pdsi_july_p), ~Season, umf_bre, K=150)
)
summary(o.pdsi_july_p)
beep(8)


#current year's drought conditions during july 
system.time(
  o.pdsi_july_c <- pcountOpen(~HOR, ~1, ~scale(pdsi_july_c), ~Season, umf_bre, K=150)
)
summary(o.pdsi_july_c)
beep(8)

#previous year's drought conditions during August
system.time(
  o.pdsi_aug_p <- pcountOpen(~HOR, ~1, ~scale(pdsi_aug_p), ~Season, umf_bre, K=150)
)
summary(o.pdsi_aug_p)
beep(8)



# gamma models (recruitment rate) -------------------------------------


#previous year's drought conditions during july 
system.time(
  g.pdsi_july_p <- pcountOpen(~HOR, ~scale(pdsi_july_p), ~1, ~Season, umf_bre, K=150)
)
summary(g.pdsi_july_p)
beep(8)

#previous year's drought conditions during july exp
system.time(
  g.pdsi_july_p_exp <- pcountOpen(~HOR, ~scale(pdsi_july_p),~1, ~Season, umf_bre, K=150,dynamics = c("trend"))
)
summary(g.pdsi_july_p_exp)
beep(8)


exp(coef(g.pdsi_july_p_exp,type="gamma"))


#previous year's drought conditions during aug exp
system.time(
  g.pdsi_aug_p_exp <- pcountOpen(~HOR, ~scale(pdsi_aug_p),~1, ~Season, umf_bre, K=150,dynamics = c("trend"))
)
summary(g.pdsi_aug_p_exp)
beep(8)

#previous year's max temps during july exp
system.time(
  g.tmax_july_p_exp <- pcountOpen(~HOR, ~scale(tmax_july_p), ~1, ~Season, umf_bre, K=150,dynamics = c("trend"))
)
summary(g.tmax_july_p_exp)
beep(8)



#previous year's max temps during aug exp
system.time(
  g.tmax_aug_p_exp <- pcountOpen(~HOR, ~scale(tmax_aug_p),~1, ~Season, umf_bre, K=150,dynamics = c("trend"))
)
summary(g.tmax_aug_p_exp)
beep(8)

#current year's max temps during july exp
system.time(
  g.tmax_july_c_exp <- pcountOpen(~HOR, ~scale(tmax_july_c), ~1, ~Season, umf_bre, K=150,dynamics = c("trend"))
)
summary(g.tmax_july_c_exp)
beep(8)

#Current year's max temps during aug exp
system.time(
  g.tmax_aug_c_exp <- pcountOpen(~HOR, ~scale(tmax_aug_c),~1, ~Season, umf_bre, K=150,dynamics = c("trend"))
)
summary(g.tmax_aug_c_exp)
beep(8)


#previous year's min temps during dec exp
system.time(
  g.tmin_dec_p_exp <- pcountOpen(~HOR, ~scale(tmin_dec_p), ~1, ~Season, umf_bre, K=150,dynamics = c("trend"))
)
summary(g.tmin_dec_p_exp)
beep(8)

#previous year's min temps during jan exp
system.time(
  g.tmin_jan_p_exp <- pcountOpen(~HOR, ~scale(tmin_jan_p),~1, ~Season, umf_bre, K=150,dynamics = c("trend"))
)
summary(g.tmin_jan_p_exp)
beep(8)

#current year's min temps during dec exp
system.time(
  g.tmin_dec_c_exp <- pcountOpen(~HOR, ~scale(tmin_dec_c), ~1, ~Season, umf_bre, K=150,dynamics = c("trend"))
)
summary(g.tmin_dec_c_exp)
beep(8)

#Current year's min temps during jan exp
system.time(
  g.tmin_jan_c_exp <- pcountOpen(~HOR, ~scale(tmin_jan_c),~1, ~Season, umf_bre, K=150,dynamics = c("trend"))
)
summary(g.tmin_jan_c_exp)
beep(8)



#previous year's min temps during jan exp
system.time(
  g.tmin_jan_pdsi_july_p_exp <- pcountOpen(~HOR, ~scale(tmin_jan_p)+scale(pdsi_july_p),~1, ~Season, umf_bre, K=150,dynamics = c("trend"))
)
summary(g.tmin_jan_pdsi_july_p_exp)
beep(8)



#rank by AICc too
cand.set<-c(
  "lam(.)gam(.)omg(.)p(.)"                                        = m.null,
  "lam(.)gam(.)omg(.)p(Season)"                                   = p.season,
  "lam(HOR)gam(.)omg(.)p(Season)"                                 = l.HOR,
  "lam(HOR)gam(.)omg(PDSI July y-1)p(Season)"                     = o.pdsi_july_p,
  "lam(HOR)gam(PDSI July y-1)omg(.)p(Season)"                     = g.pdsi_july_p,
  "lam(HOR)gam(PDSI July y-1)omg(.)p(Season)_exp"                 = g.pdsi_july_p_exp,
  "lam(HOR)gam(PDSI August y-1)omg(.)p(Season)_exp"               = g.pdsi_aug_p_exp,
  "lam(HOR)gam()omg(PDSI July)p(Season)"                          = o.pdsi_july_c,
  "lam(HOR)gam(.)omg(PDSI Aug y-1)p(Season)"                      = o.pdsi_aug_p,
  "lam(HOR)gam(tmax July y-1)omg(.)p(Season)_exp"                 = g.tmax_july_p_exp ,
  "lam(HOR)gam(tmax July)omg(.)p(Season)_exp"                     = g.tmax_july_c_exp,
  "lam(HOR)gam(tmax aug y-1)omg(.)p(Season)_exp"                  = g.tmax_aug_p_exp ,
  "lam(HOR)gam(tmax aug)omg(.)p(Season)_exp"                      = g.tmax_aug_c_exp,
  "lam(HOR)gam(tmin dec y-1)omg(.)p(Season)_exp"                  = g.tmin_dec_p_exp,
  "lam(HOR)gam(tmin dec)omg(.)p(Season)_exp"                      = g.tmin_dec_c_exp,
  "lam(HOR)gam(tmin jan y-1)omg(.)p(Season)_exp"                  = g.tmin_jan_p_exp,
  "lam(HOR)gam(tmin jan)omg(.)p(Season)_exp"                      = g.tmin_jan_c_exp,
  "lam(HOR)gam(pdsi july y-1 + tmin dec y-1)omg(.)p(Season)_exp"  = l.HOR
  )

library(AICcmodavg)
(mods_C <- aictab(cand.set,second.ord = FALSE))


top_m<-g.pdsi_july_p_exp

#check out best model
# system.time(goff<-Nmix.gof.test(top_m,nsim=1000, plot.hist = FALSE))
# 
# beep(2)

#goodness of fit
#check out season detection model c-hat = 0, p=0.37
# system.time(gofn<-Nmix.gof.test(m2_null,nsim=1000, plot.hist = FALSE))
# pbPost("note", "She done", "Wow you so smart.\nGood job." )




#quick plot of pop growth 

#quick look at plot of data
nd<-data.frame(pdsi_july_p=seq(min(na.omit(umf_bre@yearlySiteCovs$pdsi_july_p)),max(na.omit(umf_bre@yearlySiteCovs$pdsi_july_p)),length.out =50))

ep<-predict(g.pdsi_july_p_exp, type="gamma" , newdata=nd,appendData=TRUE)

pd <- position_dodge(0.2) # move them .05 to the left and right

library(ggpubr)

#quick plot
ggplot(ep,aes(pdsi_july_p,Predicted))+
  geom_line()+
  geom_ribbon(aes(ymin=lower, ymax=upper),alpha=0.5)+
  ylab("r")+
  xlab("PDSI July T-1 scaled")+
  theme_pubr()+
  theme(axis.title.y = element_text(face = "italic"))



