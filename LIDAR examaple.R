
#working with lIDAR data in R 

library(lidR)

#load las file 
setwd("/Volumes/Seagate Expansion Drive/GIS/USA/NC/LIDAR")#use the Sessions menu>set working directory 
las <- readLAS("117389_1.las")
plot(las)

#make a canopy height model
thr <- c(0,2,5,10,15)
edg <- c(0, 1.5)
chm <- grid_canopy(las, 1, pitfree(thr, edg))#extract the elevation at canopy top


library(rasterVis)
library(ggplot2)
library(ggpubr)

gplot(chm) + 
  geom_tile(aes(fill = value)) +
  scale_fill_gradientn(colours = rev(terrain.colors(225))) +
  coord_equal()+
  labs(fill = "Canopy top elevation")+
  theme_pubr()
  




plot(chm)
par(mar = c(3, 2, 0.1, 0.1))  
plot(chm)


dtm_tin <- grid_terrain(las, res = 1, algorithm = tin())
plot_dtm3d(dtm_tin, bg = "white") 

plot(dtm_tin)


CH<-chm-dtm_tin

plot(CH)

gplot(CH) + 
  geom_raster(aes(fill = value)) +
  scale_fill_gradientn(colours = height.colors(50)) +
  coord_equal()+
  labs(fill = "Canopy height(ft)")+
  scale_alpha_manual(values=c(0,1),guide="none")+
  theme_pubr()

library(viridis)

gplot(CH) + 
  geom_raster(aes(fill = value)) +
  scale_fill_gradientn(colours=c("white","white", .colors(50)),values=c(0,0.1,seq(0.1001,1,length.out=10)),na.value = "transparent") +
  coord_equal()+
  labs(fill = "Canopy height(ft)")+
  theme_pubr()


colours=c("red","red",topo.colors(6)),
values=c(0,0.1,seq(0.1001,1,length.out=7)),na.value = "transparent")


#find individual treetops (not working as of now)
ttops <- find_trees(chm, lmf(ws = 30))
plot(chm, col = height.colors(50))
plot(ttops, add = TRUE)

