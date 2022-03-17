#### Unmarked Dail-Madsen N-Mixture models Aneides aeneus 2000-2020

options(max.print = 60000)
options(scipen = 20)

library(readr)
library(tidyverse)

setwd("~/github/BRE_2022/BRE_2022/Data")
# Load counts and observation covariates
y_obs <- read_csv("BRE_2022_wide_Y_obsCovs.csv")


# change format to numeric
y_obs <- y_obs %>%
  mutate(across(where(is.logical), as.numeric))


# #change season to factor
# J_date=as.data.frame(y_obs[,c(1543:3054)])
# Season=as.data.frame(y_obs[,c(3084:4595)])
#
# Season<-Season%>%
#   mutate(across(where(is.numeric), as.factor))

# add empty columns for the rest of 2021

nam <- paste0("Season_2021_", 30:72)
tot <- paste0("total_ANAE_2021_", 30:72)
jdat <- paste0("J_date_2021_", 30:72)

y_obs[nam] <- NA # add placeholder NA cols for Season

y_obs[tot] <- NA # add placeholder NA cols for counts

y_obs[jdat] <- NA # add placeholder NA cols for J_date

# move them behind others
y_obs <- y_obs %>%
  relocate(any_of(tot), .after = total_ANAE_2021_29)

# move them behind others
y_obs <- y_obs %>%
  relocate(any_of(jdat), .after = J_date_2021_29)

#pivot longer
y_obs<-y_obs%>%
  mutate_if(is.numeric,as.character)


y1<-y_obs%>%
  pivot_longer(!new_flag1,
               names_to = c(".value","year"),
               names_pattern = '(.*?)(\\d+)')

y1<-y1%>%
  rename("J_date"="J_date_")%>%
  rename("total_ANAE"="total_ANAE_")%>%
  rename("Season"="Season_")
  
y1$J_date<-as.numeric(y1$J_date)

sy1<-y1%>%
  #dplyr::group_by(year)%>%
  arrange(J_date)%>%
  dplyr::group_by(year,Season,new_flag1)%>%
  slice_max(!is.na(total_ANAE),n=3,with_ties = F)
  

#try with datatable
library(data.table)
dt<-as.data.table(y1,key = c("new_flag1","year","Season"))

lk<-dt[, tail(.SD, 3), by=c("new_flag1","year","Season")]


#format observations for unmarked dataframe
sy1$J_date<-as.numeric(sy1$J_date)
sy1$year<-as.numeric(sy1$year)

df2<-
  lk%>%
  dplyr::arrange(J_date)%>%
  dplyr::group_by(year,new_flag1) %>%
  dplyr::mutate(Count = 1:n())

df3<-df2%>%
  filter(Count<13)

length(unique(df3$new_flag1))#still 1181 sites

#new dataset for formating umf
nd<-data.frame(expand.grid(
  year=seq(2000,2021),
  Count=seq(1,12),
  new_flag1=unique(df2$new_flag1)))

#merge to fill spaces, makes NA for unmarked
df2_na<-merge(df3,nd,by=c("new_flag1","year", "Count"),all=TRUE)

df2_na$new_flag1<-as.factor(df2_na$new_flag1)

length(unique(sy1$new_flag1))

# load yearly site covariates
ysc <- read_csv("YSC_2022_02_11.csv")


# add placeholder NA cols in the YSC

ysc <- ysc %>%
  mutate(`2021.08_pdsi` = NA, .after = `2020.08_pdsi`) %>%
  mutate(`2021.07_pdsi` = NA, .after = `2020.07_pdsi`) %>%
  mutate(`2021.07_pdsi_p` = NA, .after = `2020.07_pdsi_p`) %>%
  mutate(`2021.08_pdsi_p` = NA, .after = `2020.08_pdsi_p`) %>%
  mutate(`2021_tmax_c_July` = NA, .after = `2020_tmax_c_July`) %>%
  mutate(`2021_tmax_p_July` = NA, .after = `2020_tmax_p_July`) %>%
  mutate(`2021_tmax_c_August` = NA, .after = `2020_tmax_c_August`) %>%
  mutate(`2021_tmax_p_August` = NA, .after = `2020_tmax_p_August`) %>%
  mutate(`2021_tmin_c_Dec` = NA, .after = `2020_tmin_c_Dec`) %>%
  mutate(`2021_tmin_c_Jan` = NA, .after = `2020_tmin_c_Jan`) %>%
  mutate(`2021_tmin_p_Dec` = NA, .after = `2020_tmin_p_Dec`) %>%
  mutate(`2021_tmin_p_Jan` = NA, .after = `2020_tmin_p_Jan`)


#Load Gridmet drought YSCs 


gm_spei2y <- read_csv("BRE_2022_GM_spei2y.csv", 
                               col_types = cols(`system:index` = col_skip(), 
                                                .geo = col_skip()))


gm_spei1y <- read_csv("BRE_2022_GM_spei1y.csv", 
                               col_types = cols(`system:index` = col_skip(), 
                                                .geo = col_skip()))

gm_spei180d <- read_csv("BRE_2022_GM_spei180d.csv", 
                                 col_types = cols(`system:index` = col_skip(), 
                                                  .geo = col_skip()))

test<-ysc%>%
  left_join(gm_spei180d,by="new_flag1")%>%
  left_join(gm_spei1y,by="new_flag1")%>%
  left_join(gm_spei2y,by=c("new_flag1"))

names(test)#all good 

ysc<-ysc%>%
  left_join(gm_spei180d,by="new_flag1")%>%
  left_join(gm_spei1y,by="new_flag1")%>%
  left_join(gm_spei2y,by=c("new_flag1"))



# load site covariates
sc <- read_csv("BRE_2022_siteCovs.csv")

# load new site covariates
sites <- read_csv("sites_agg_02_22.csv")

sc <- merge(sc, sites, by = ("new_flag1"))

sc <- sc %>%
  dplyr::select(new_flag1, bd_result, HOR, nndist, DuPont, private)

# make unmarked dataframe

library(unmarked)


umf_bre <- unmarkedFramePCO(
  y = y_obs[, c(2:1585)],
  siteCovs = sc,
  obsCovs = list(
    J_date = as.data.frame(y_obs[, 1586:3169]),
    Season = as.data.frame(y_obs[, 3170:4753])),
  yearlySiteCovs = list(
    pdsi_aug_c = as.data.frame(ysc[, c(2:23)]),
    pdsi_july_c = as.data.frame(ysc[, c(24:45)]),
    pdsi_july_p = as.data.frame(ysc[, c(46:67)]),
    pdsi_aug_p = as.data.frame(ysc[, c(68:89)]),
    tmax_july_c = as.data.frame(ysc[, c(90:111)]),
    tmax_july_p = as.data.frame(ysc[, c(112:133)]),
    tmax_aug_c = as.data.frame(ysc[, c(134:155)]),
    tmax_aug_p = as.data.frame(ysc[, c(156:177)]),
    tmin_dec_c = as.data.frame(ysc[, c(178:199)]),
    tmin_jan_c = as.data.frame(ysc[, c(200:221)]),
    tmin_dec_p = as.data.frame(ysc[, c(222:243)]),
    tmin_jan_p = as.data.frame(ysc[, c(244:265)]),
    gm_spei180d = as.data.frame(ysc[, c(266:287)]),
    gm_spei1y = as.data.frame(ysc[, c(288:309)]),
    gm_spei2y = as.data.frame(ysc[, c(310:331)])
  ),
  numPrimary = 22
)


summary(umf_bre)
#plot(umf_bre)

# set factor levels
umf_bre@obsCovs$Season <- factor(umf_bre@obsCovs$Season, levels = c("Spring", "Summer", "Fall", "Winter"))


# scale yearly site covarites

umf_bre@yearlySiteCovs <-
  umf_bre@yearlySiteCovs %>%
  mutate(across(!gm_spei180d & !gm_spei1y & !gm_spei2y, scale))



# Detection models  -------------------------------------------------------
library(beepr)

kmax <- 150 # set K value


# Exponential null model ART 
system.time(
  m.null_exp <-
    pcountOpen(
      lambda = ~1,
      gamma  = ~1,
      omega  = ~1,
      p      = ~1,
      data = umf_bre,
      K=kmax,
      se= FALSE,
      dynamics = "trend"
    )
)

summary(m.null_exp)
beep(8)

# Ricker logisitic null model ART ~17 mins
system.time(
  m.null_r <-
    pcountOpen(
      lambda = ~1,
      gamma  = ~1,
      omega  = ~1,
      p      = ~1,
      data = umf_bre,
      K=kmax,
      se= FALSE,
      dynamics = "ricker"
    )
)

summary(m.null_r)
beep(8)



# Season detection model
system.time(p.season <-
  pcountOpen(
    lambda = ~1,
    gamma  = ~1,
    omega  = ~1,
    p      = ~Season,
    umf_bre,
    K = kmax,
    se= FALSE,
    dynamics = "gompertz"
  ))
summary(p.season)
beep(8)




# Abundance models --------------------------------------------------------


# History of reproduction ricker

system.time(l.HOR_r <-
              pcountOpen(
                lambda = ~HOR,
                gamma  = ~1,
                omega  = ~1,
                p      = ~Season,
                umf_bre,
                K = kmax,
                se= FALSE,
                dynamics = "ricker"
              ))
summary(l.HOR_r)
beep(8)


# History of reproduction trend ART ~23 mins
system.time(l.HOR_exp <-
              pcountOpen(
                lambda = ~HOR,
                gamma  = ~1, 
                omega  = ~1,
                p      = ~Season,
                umf_bre,
                K = kmax,
                se= FALSE,
                dynamics = "trend"
              ))
summary(l.HOR_exp)
beep(8)

# History of reproduction gompertz ART ~77.9 mins no se
system.time(l.HOR_g <-
              pcountOpen(
                lambda = ~HOR,
                gamma  = ~1, 
                omega  = ~1,
                p      = ~Season,
                umf_bre,
                K = kmax,
                se= FALSE,
                dynamics = "gompertz"
              ))
summary(l.HOR_g)
beep(8)





# omega models (survival probability or carrying capacity) -------------------------------------


# previous year's drought conditions during july ricker
system.time(o.pdsi_july_p <-
              pcountOpen(
                lambda = ~HOR,
                gamma  = ~1,
                omega  = ~pdsi_july_p,
                p      = ~Season,
                umf_bre,
                K = kmax,
                se= FALSE,
                dynamics = "ricker"
              ))
summary(pdsi_july_p)
beep(8)

# previous year's cumulative SPEI drought conditions to july ART ~61 mins
tmp <- coef(l.HOR_g)
inits <- c(tmp[1:4], 0,tmp[5:8])


system.time(o.spei1y_g <-
              pcountOpen(
                lambda = ~HOR,
                gamma  = ~1,
                omega  = ~gm_spei1y,
                p      = ~Season,
                umf_bre,
                K = kmax,
                #se= FALSE,
                starts = inits,
                dynamics = "gompertz"
              ))
summary(o.spei1y_g)
beep(1)



# current year's drought conditions during july

system.time(o.pdsi_july_c <-
              pcountOpen(
                lambda = ~HOR,
                gamma  = ~1,
                omega  = ~pdsi_july_c,
                p      = ~Season,
                umf_bre,
                K = kmax,
                se= FALSE,
                dynamics = "ricker"
              ))
summary(pdsi_july_c)
beep(8)



# previous year's drought conditions during August

system.time(o.pdsi_aug_p <-
              pcountOpen(
                lambda = ~HOR,
                gamma  = ~1,
                omega  = ~pdsi_aug_p,
                p      = ~Season,
                umf_bre,
                K = kmax,
                se= FALSE,
                dynamics = "ricker"
              ))
summary(pdsi_aug_p)
beep(8)




# gamma models (recruitment rate or population growth rate) -------------------------------------


# previous year's drought conditions during july ART ~64 mins

system.time(g.pdsi_july_p_r <-
              pcountOpen(
                lambda = ~HOR,
                gamma  = ~pdsi_july_p,
                omega  = ~1,
                p      = ~Season,
                umf_bre,
                K = kmax,
                se= FALSE,
                dynamics = "ricker"
              ))
summary(g.pdsi_july_p_r)
beep(8)



# previous year's drought conditions during july exp ART ~25 mins (no SE and starts)

tmp <- coef(l.HOR_exp)
inits <- c(tmp[1:3], 0,tmp[4:7])

system.time(g.pdsi_july_p_exp <-
              pcountOpen(
                lambda = ~HOR,
                gamma  = ~pdsi_july_p,
                omega  = ~1,
                p      = ~Season,
                umf_bre,
                K = kmax,
                se= FALSE,
                starts = inits,
                dynamics = "trend"
              ))
summary(g.pdsi_july_p_exp)
beep(8)

# previous year's drought conditions during july exp ART ~95 mins (no SE)

tmp <- coef(l.HOR_g)
inits <- c(tmp[1:3], 0,tmp[4:8])

system.time(g.pdsi_july_p_g <-
              pcountOpen(
                lambda = ~HOR,
                gamma  = ~pdsi_july_p,
                omega  = ~1,
                p      = ~Season,
                umf_bre,
                K = kmax,
                #se= FALSE,
                starts = inits,
                dynamics = "gompertz"
              ))
summary(g.pdsi_july_p_g)
beep(1)



# previous 180 day cumulative SPEI drought conditions to july ART ~100 mins

system.time(g.spei180d_g <-
              pcountOpen(
                lambda = ~HOR,
                gamma  = ~gm_spei180d,
                omega  = ~1,
                p      = ~Season,
                umf_bre,
                K = kmax,
                se= FALSE,
                #starts = inits,
                dynamics = "gompertz"
              ))
summary(g.spei180d_g)
beep(1)



# previous year's cumulative SPEI drought conditions to July quadratic ART 

system.time(g.spei180d2_g <-
              pcountOpen(
                lambda = ~HOR,
                gamma  = ~gm_spei180d + I(gm_spei180d^2),
                omega  = ~1,
                p      = ~Season,
                umf_bre,
                K = kmax,
                #se= FALSE,
                #starts = inits,
                dynamics = "gompertz"
              ))
summary(g.spei180d2_g)
beep(1)





# previous year's cumulative SPEI drought conditions to july ART ~100 mins

system.time(g.spei1y_g <-
              pcountOpen(
                lambda = ~HOR,
                gamma  = ~gm_spei1y,
                omega  = ~1,
                p      = ~Season,
                umf_bre,
                K = kmax,
                #se= FALSE,
                #starts = inits,
                dynamics = "gompertz"
              ))
summary(g.spei1y_g)
beep(1)



# previous year's cumulative SPEI drought conditions to July quadratic ART 

system.time(g.spei1y2_g <-
              pcountOpen(
                lambda = ~HOR,
                gamma  = ~gm_spei1y + I(gm_spei1y^2),
                omega  = ~1,
                p      = ~Season,
                umf_bre,
                K = kmax,
                #se= FALSE,
                #starts = inits,
                dynamics = "gompertz"
              ))
summary(g.spei1y2_g)
beep(1)


# previous year's cumulative SPEI drought conditions to july ART ~100 mins

tmp <- coef(l.HOR_g)
inits <- c(tmp[1:3], 0,tmp[4:8])


system.time(g.spei2y_g <-
              pcountOpen(
                lambda = ~HOR,
                gamma  = ~gm_spei2y,
                omega  = ~1,
                p      = ~Season,
                umf_bre,
                K = kmax,
                #se= FALSE,
                starts = inits,
                dynamics = "gompertz"
              ))
summary(g.spei2y_g)
beep(1)



# previous year's cumulative SPEI drought conditions to July quadratic ART 

system.time(g.spei2y2_g <-
              pcountOpen(
                lambda = ~HOR,
                gamma  = ~gm_spei2y + I(gm_spei2y^2),
                omega  = ~1,
                p      = ~Season,
                umf_bre,
                K = kmax,
                #se= FALSE,
                #starts = inits,
                dynamics = "gompertz"
              ))
summary(g.spei2y2_g)
beep(1)





# previous year's drought conditions during aug exp

system.time(g.pdsi_aug_p_r <-
              pcountOpen(
                lambda = ~HOR,
                gamma  = ~pdsi_aug_p,
                omega  = ~1,
                p      = ~Season,
                umf_bre,
                K = kmax,
                dynamics = "ricker"
              ))
summary(g.pdsi_aug_p_r)
beep(8)



# previous year's max temps during july exp


system.time(g.tmax_july_p_r <-
              pcountOpen(
                lambda = ~HOR,
                gamma  = ~tmax_july_p,
                omega  = ~1,
                p      = ~Season,
                umf_bre,
                K = kmax,
                dynamics = "ricker"
              ))
summary(g.tmax_july_p_r)
beep(8)




# previous year's max temps during aug exp

system.time(g.tmax_aug_p_r <-
              pcountOpen(
                lambda = ~HOR,
                gamma  = ~tmax_aug_p,
                omega  = ~1,
                p      = ~Season,
                umf_bre,
                K = kmax,
                dynamics = "ricker"
              ))
summary(g.tmax_aug_p_r)
beep(8)



# current year's max temps during july exp

system.time(g.tmax_july_c_r <-
              pcountOpen(
                lambda = ~HOR,
                gamma  = ~tmax_july_c,
                omega  = ~1,
                p      = ~Season,
                umf_bre,
                K = kmax,
                dynamics = "ricker"
              ))
summary(g.tmax_july_c_r)
beep(8)


# Current year's max temps during aug exp
system.time(g.tmax_aug_c_exp <- 
              pcountOpen(
                lambda = ~HOR, 
                gamma  = ~tmax_aug_c, 
                omega  = ~1, 
                p      = ~Season, 
                umf_bre, 
                K = kmax, 
                dynamics = c("trend"))
)
summary(g.tmax_aug_c_exp)
beep(8)

system.time(g.tmax_july_c_r <-
              pcountOpen(
                lambda = ~HOR,
                gamma  = ~tmax_july_c,
                omega  = ~1,
                p      = ~Season,
                umf_bre,
                K = kmax,
                se=FALSE,
                dynamics = "ricker"
              ))
summary(g.tmax_july_c_r)
beep(8)



# previous year's min temps during dec exp
system.time( g.tmin_dec_p_exp <- 
               pcountOpen(
                 lambda = ~ HOR, 
                 gamma  = ~ tmin_dec_p, 
                 omega  = ~ 1, 
                 p      = ~ Season, 
                 umf_bre, 
                 K = kmax,
                 se=FALSE,
                 dynamics = c("trend"))
)
summary(g.tmin_dec_p_exp)
beep(8)

# previous year's min temps during jan exp
system.time(g.tmin_jan_p_exp <- 
              pcountOpen(
                lambda = ~HOR, 
                gamma  = ~tmin_jan_p, 
                omega  = ~1,
                p      = ~Season, 
                umf_bre,
                K = kmax,
                se=FALSE,
                dynamics = c("trend"))
)
summary(g.tmin_jan_p_exp)
beep(8)

# current year's min temps during dec exp
system.time(g.tmin_dec_c_exp <- 
              pcountOpen(
                lambda = ~HOR, 
                gamma  = ~tmin_dec_c, 
                omega  = ~1, 
                p      = ~Season,
                umf_bre, 
                K = kmax,
                se=FALSE,
                dynamics = c("trend"))
)
summary(g.tmin_dec_c_exp)
beep(8)

# Current year's min temps during jan exp
system.time(g.tmin_jan_c_exp <-
              pcountOpen(
                lambda = ~HOR, 
                gamma  = ~tmin_jan_c,
                omega  = ~1, 
                p      = ~Season, 
                umf_bre, 
                K = kmax,
                se=FALSE,
                dynamics = c("trend"))
)
summary(g.tmin_jan_c_exp)
beep(8)



# previous year's min temps during jan exp
system.time(g.tmin_jan_pdsi_july_p_exp <- 
              pcountOpen(
                lambda = ~HOR, 
                gamma  = ~tmin_jan_p + pdsi_july_p, 
                omega  = ~1, 
                p      = ~Season, 
                umf_bre, 
                K = kmax,
                se=FALSE,
                dynamics = c("trend"))
)
summary(g.tmin_jan_pdsi_july_p_exp)
beep(8)



# rank by AICc too
cand.set <- c(
  "lam(.)gam(.)omg(.)p(.)"                                        = m.null,
  "lam(.)gam(.)omg(.)p(Season)"                                   = p.season,
  "lam(HOR)gam(.)omg(.)p(Season)"                                 = l.HOR,
  "lam(HOR)gam(.)omg(PDSI July y-1)p(Season)"                     = o.pdsi_july_p,
  "lam(HOR)gam(PDSI July y-1)omg(.)p(Season)"                     = g.pdsi_july_p,
  "lam(HOR)gam(PDSI July y-1)omg(.)p(Season)_exp"                 = g.pdsi_july_p_exp,
  "lam(HOR)gam(PDSI July y-1)omg(.)p(Season)_r"                   = g.pdsi_july_p_r,
  "lam(HOR)gam(PDSI July y-1)omg(.)p(Season)_g"                   = g.pdsi_july_p_g,
  "lam(HOR)gam(PDSI August y-1)omg(.)p(Season)_exp"               = g.pdsi_aug_p_exp,
  "lam(HOR)gam()omg(PDSI July)p(Season)"                          = o.pdsi_july_c,
  "lam(HOR)gam(.)omg(PDSI Aug y-1)p(Season)"                      = o.pdsi_aug_p,
  "lam(HOR)gam(tmax July y-1)omg(.)p(Season)_exp"                 = g.tmax_july_p_exp,
  "lam(HOR)gam(tmax July)omg(.)p(Season)_exp"                     = g.tmax_july_c_exp,
  "lam(HOR)gam(tmax aug y-1)omg(.)p(Season)_exp"                  = g.tmax_aug_p_exp,
  "lam(HOR)gam(tmax aug)omg(.)p(Season)_exp"                      = g.tmax_aug_c_exp,
  "lam(HOR)gam(tmin dec y-1)omg(.)p(Season)_exp"                  = g.tmin_dec_p_exp,
  "lam(HOR)gam(tmin dec)omg(.)p(Season)_exp"                      = g.tmin_dec_c_exp,
  "lam(HOR)gam(tmin jan y-1)omg(.)p(Season)_exp"                  = g.tmin_jan_p_exp,
  "lam(HOR)gam(tmin jan)omg(.)p(Season)_exp"                      = g.tmin_jan_c_exp,
  "lam(HOR)gam(pdsi july y-1 + tmin dec y-1)omg(.)p(Season)_exp"  = l.HOR
)

library(AICcmodavg)
(mods_C <- aictab(cand.set, second.ord = FALSE))


top_m <- g.pdsi_july_p_exp



# rank by AICc too
cand.set <- c(
  "lam(.)gam(.)omg(.)p(.)"                                        = m.null,
  #"lam(.)gam(.)omg(.)p(Season)"                                   = p.season,
  "lam(HOR)gam(.)omg(.)p(Season)"                                 = l.HOR_g,
  #"lam(HOR)gam(.)omg(PDSI July y-1)p(Season)"                     = o.pdsi_july_p,
  #"lam(HOR)gam(PDSI July y-1)omg(.)p(Season)"                     = g.pdsi_july_p,
  "lam(HOR)gam(SPEI July T-180d)omg(.)p(Season)_g"                   = g.spei180d_g,
  "lam(HOR)gam(PDSI July y-1)omg(.)p(Season)_exp"                 = g.pdsi_july_p_exp,
  "lam(HOR)gam(PDSI July y-1)omg(.)p(Season)_r"                   = g.pdsi_july_p_r,
  "lam(HOR)gam(SPEI July y-1)omg(.)p(Season)_g"                   = g.spei1y_g,
  "lam(HOR)gam(.)omg(SPEI July y-1)p(Season)_g"                   = o.spei1y_g,
  "lam(HOR)gam(SPEI^2 July y-1)omg(.)p(Season)_g"                   = g.spei1y2_g
  #"lam(HOR)gam(PDSI August y-1)omg(.)p(Season)_exp"               = g.pdsi_aug_p_exp,
  #"lam(HOR)gam()omg(PDSI July)p(Season)"                          = o.pdsi_july_c,
  #"lam(HOR)gam(.)omg(PDSI Aug y-1)p(Season)"                      = o.pdsi_aug_p,
  #"lam(HOR)gam(tmax July y-1)omg(.)p(Season)_exp"                 = g.tmax_july_p_exp,
  #"lam(HOR)gam(tmax July)omg(.)p(Season)_exp"                     = g.tmax_july_c_exp,
  #"lam(HOR)gam(tmax aug y-1)omg(.)p(Season)_exp"                  = g.tmax_aug_p_exp,
  #"lam(HOR)gam(tmax aug)omg(.)p(Season)_exp"                      = g.tmax_aug_c_exp,
  #"lam(HOR)gam(tmin dec y-1)omg(.)p(Season)_exp"                  = g.tmin_dec_p_exp,
  #"lam(HOR)gam(tmin dec)omg(.)p(Season)_exp"                      = g.tmin_dec_c_exp,
  #"lam(HOR)gam(tmin jan y-1)omg(.)p(Season)_exp"                  = g.tmin_jan_p_exp,
  #"lam(HOR)gam(tmin jan)omg(.)p(Season)_exp"                      = g.tmin_jan_c_exp,
  #"lam(HOR)gam(pdsi july y-1 + tmin dec y-1)omg(.)p(Season)_exp"  = l.HOR
)

library(AICcmodavg)
(mods_C <- aictab(cand.set, second.ord = FALSE))


# check out best model
# system.time(goff<-Nmix.gof.test(top_m,nsim=1000, plot.hist = FALSE))
#
# beep(2)

# goodness of fit
# check out season detection model c-hat = 0, p=0.37
# system.time(gofn<-Nmix.gof.test(m2_null,nsim=1000, plot.hist = FALSE))
# pbPost("note", "She done", "Wow you so smart.\nGood job." )




# quick plot of pop growth

unscale <- function(z, center = attr(z, "scaled:center"), scale = attr(z, "scaled:scale")) {
  if(!is.null(scale))  z <- sweep(z, 2, scale, `*`)
  if(!is.null(center)) z <- sweep(z, 2, center, `+`)
  structure(z,
            "scaled:center"   = NULL,
            "scaled:scale"    = NULL,
            "unscaled:center" = center,
            "unscaled:scale"  = scale
  )
}

uns_pdsi_july_p<-unscale(umf_bre@yearlySiteCovs$pdsi_july_p)

# quick look at plot of data
nd <- data.frame(pdsi_july_p = seq(
  min(na.omit(umf_bre@yearlySiteCovs$pdsi_july_p)), 
  max(na.omit(umf_bre@yearlySiteCovs$pdsi_july_p)), 
  length.out = 50))


# quick look at plot of data SPEI 1y
nd <- data.frame( gm_spei1y= seq(
  min(na.omit(umf_bre@yearlySiteCovs$gm_spei1y)), 
  max(na.omit(umf_bre@yearlySiteCovs$gm_spei1y)), 
  length.out = 50))

# # quick look at plot of data
# nd <- data.frame(pdsi_july_p = seq(
#   min(na.omit(uns_pdsi_july_p)), 
#   max(na.omit(uns_pdsi_july_p)), 
#   length.out = 50))
# 

ep <- predict(g.spei1y_g, type = "gamma", newdata = nd, appendData = TRUE)

pd <- position_dodge(0.2) # move them .05 to the left and right

library(ggpubr)



aff <- data.frame(x1 = -1.45, x2 = 2, y1 =-0.05, y2 = -0.05)

# quick plot
ggplot(ep, aes(gm_spei1y, Predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  ylab("Population growth rate (r)") +
  xlab("SPEI 1 Year") +
  annotate("text", x = -1.75, y = -0.05,fontface = 4, label = "Drought",alpha=0.5) +
  annotate("text", x = 2.2, y = -0.05,fontface = 4, label = "Wet",alpha=0.5) +
  geom_segment(aes(x=x1,xend=x2, y=y1, yend=y2),data = aff,size=0.5,alpha=0.5,
               arrow = arrow(length = unit(0.35, "cm"),ends = "both"))+
  ggtitle("Gompertz model")+
  theme_pubr() +
  theme(axis.title.y = element_text(face = "italic"))




#quick pop estimate 


#Richard Chandler's method 


library(unmarked)

#Rename model of interest 

m2_t<-g.spei1y2_g

## Draw posterior samples of annual abundance

## You could modify this to compute any function of the posterior


#re <- ranef(m2)#no trends
system.time(re1<-ranef(m2_t))# lambda trends ART ~32 mins

postPredN <- function(ranefOut, nSims=100) {
  
  post <- ranefOut@post
  
  postDim <- dim(post)
  
  nSites <- postDim[1]
  
  nProbs <- postDim[2]
  
  possibleN <- 0:(nProbs-1)
  
  nYears <- postDim[3]
  
  NsitePostSamples <- array(NA_integer_,
                            
                            c(nSites, nYears, nSims))
  
  for(i in 1:nSites) {
    
    for(t in 1:nYears) {
      
      NsitePostSamples[i,t,] <- sample(
        
        possibleN, size=nSims,
        
        replace=TRUE, prob=post[i,,t])
      
    }
    
  }
  
  NpostSamples <- apply(NsitePostSamples, c(2,3), sum)
  
  return(NpostSamples)
  
}


Npost <- postPredN(re1, nSims=1000)

rowMeans(Npost)             ## Posterior mean of annual abundance
pl<-data.frame(rowMeans(Npost),Year=seq(2000,2021,1))
apply(Npost, 1, median)     ## Posterior median

a<-data.frame(apply(Npost, 1, quantile,
                    
                    prob=c(0.025, 0.975)))## 95% CI


a$rows<-row.names(a)

a1<-gather(a,var,level,"X1":"X22")

a1$Year<-rep(2000:2021,each=2)

a1$var<-NULL

a1<-spread(a1,rows,level)

pl<-cbind(pl,a1)

pl<-pl[c(-2)]

pl<-plyr::rename(pl,c("rowMeans.Npost."="mean"))


#pl<-data.frame(rowMeans(Npost),Year=seq(2009,2019,1))
pl$Year<-as.character(pl$Year)
pl$Year<-as.Date(pl$Year,format=c("%Y"))
# data.frame(apply(Npost, 1, quantile,
#       
#       prob=c(0.025, 0.975)))## 95% CI


ggplot(data=pl,aes(Year,mean))+
  geom_line(color="black",linetype=2,alpha=0.5)+
  geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),alpha=0.2)+
  theme_pubr()+
  ylab(expression(hat(italic("N"))))+
  xlab("")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))


min(pl$mean)/max(pl$mean)*100



# make a plot of drought conditions over time 
gm_spei1y_l<-gm_spei1y%>%
pivot_longer(!new_flag1,names_to = "Year",values_to = "SPEI_1y")


gm_spei1y_l$Year<-substr(gm_spei1y_l$Year,1,4)
gm_spei1y_l$Year<-as.numeric(gm_spei1y_l$Year)

#make a plot 

ggplot(data=gm_spei1y_l,aes(Year,SPEI_1y,fill=new_flag1))+
  geom_line(aes(group=new_flag1),alpha=0.01)+
  geom_smooth()+
  annotate("text", x = 2000, y = 2,fontface = 4, label ="Wet",alpha=0.5) +
  annotate("text", x = 2000, y = -2,fontface = 4, label ="Drought",alpha=0.5) +
  geom_hline(yintercept = 0,linetype=2,color="red",alpha=0.5)+
  ylab(expression(bolditalic("SPEI 1 year")))+
  xlab("")+
  theme_pubr()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))
  

# make a plot of drought conditions over time 
gm_spei180d_l<-gm_spei180d%>%
  pivot_longer(!new_flag1,names_to = "Year",values_to = "SPEI_180d")


gm_spei180d_l$Year<-substr(gm_spei180d_l$Year,1,4)
gm_spei180d_l$Year<-as.numeric(gm_spei180d_l$Year)

#make a plot 

ggplot(data=gm_spei180d_l,aes(Year,SPEI_180d,fill=new_flag1))+
  geom_line(aes(group=new_flag1),alpha=0.01)+
  geom_smooth()+
  annotate("text", x = 2000, y = 2,fontface = 4, label ="Wet",alpha=0.5) +
  annotate("text", x = 2000, y = -2,fontface = 4, label ="Drought",alpha=0.5) +
  geom_hline(yintercept = 0,linetype=2,color="red",alpha=0.5)+
  ylab(expression(bolditalic("SPEI 180 days")))+
  xlab("")+
  theme_pubr()+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))



#rate of increase 

pl$roi<-log(pl$mean)

ggplot(data=pl,aes(Year,roi))+
  geom_line(color="black",linetype=2,alpha=0.5)+
  #geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),alpha=0.2)+
  theme_pubr()+
  ylab(expression(hat(italic("N"))))+
  xlab("")+
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))

-2.536 +(-1.06632653* 0.912)

