##################### Figure 1. 
# Map of mean annual temperature of Córdoba (1992-2021) based on ERA5-Land

#Read .nc with daily mean temperature
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(sp)
library(sf)
#open tmean
tmean_brick<-brick('/home/julius/ERA5/era5land_cba_1992_2021_dmtemp.nc',varname="t2m")
#stack
tmean_stack<-stack(tmean_brick)
plot(tmean_stack$X1992.01.01)
tmean_stack_c<-tmean_stack - 273.15
plot(tmean_stack_c$X1992.07.04)
# writeRaster(tmean_stack_c, 'daily_tmean.tiff')
# Use stack function to read in all bands
#tmean_stack_c2<-stack('/home/julius/ERA5/daily_tmean.tiff')
#plot(tmean_stack_c2$daily_tmean.1)
#calculate a mean of raster stack
  Q50 <- calc(tmean_stack_c , fun = function(x) {quantile(x,probs = 0.5,na.rm=TRUE)} )

r_mean <- calc(tmean_stack_c,  fun = function(x) {mean(x,na.rm=TRUE)} )
plot(r_mean)
#plot(Q50)
plot(r_mean)
r_mean<- disaggregate(r_mean, fact=3)
res(r_mean)
plot(r_mean)
setwd("home/julius/ERA5")
getwd()
writeRaster(r_mean, 'tma_cba.tiff',format='GTiff',overwrite=TRUE) #write raster

###########################################
##read raster of mean annual temperature
###########################################
r_mean<-raster('/home/julius/tma_cba.tiff')

cba_limites<-sf::read_sf("/home/julius/cordoba/cba_polygon_wgs84.shp") #mask wit sf
cba_sp <- as_Spatial(cba_limites)
proj4string(cba_sp) <- CRS("+init=epsg:4326")
borde_sf<-st_as_sf(cba_limites)
borde2 = st_transform(borde_sf, st_crs(r_mean))
#you need a sp object to mask (asSpatial..)
borde3<-as_Spatial (borde2)
r_mean_mask<-mask(r_mean, borde3)
plot(r_mean_mask)
writeRaster(r_mean_mask, 'tma_cba_mask.tiff',format='GTiff',overwrite=TRUE) #write raster

#############################################
#figure 1.  map of mean temperatures
#############################################
library(tmap)
library(sp)
library(sf)
path2<-"/home/julius/cordoba/cba_dptos_polygon_wgs84.shp" ##read vector of cordoba
cba_dpto<-st_read(path2) 
crs(r)<-crs(cba_dpto) #crs equal to raster
tm_shape(r_mean_mask)+
  tm_raster(palette="Spectral",
            title="mean annual temperature (1992-2021)")+
  
  tm_shape(cba_dpto) +
  tm_borders() +
  tmap::tm_layout(, 
                  inner.margins=c(0,0,.1,0), 
                  title.size=.8, 
                  title.position = c('center', 'TOP')) + 
  tmap::tm_layout(legend.only = FALSE, 
                  legend.outside.position = c('center', 'TOP'), 
                  legend.title.size=1.5,
                  legend.position = c('right','bottom'),
                  legend.title.color = 'black'
  )

#tmean<-stack(nc_tmean)
#map for Argencon
library(stars)
library(tmap)
#Figure1 

#############################################
#figure 2.  a) map of maximum temperatures (mean-max)
#############################################

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library("rasterVis")
library("RColorBrewer")
library('raster')

#read era stack temperature in centigrades
era_stack_c<- stack ("/home/julius/ERA5/era_stack_c.tif")
dt3<-seq(as.Date("1992-01-01"), as.Date("2021-12-31"), by="days")
era_stack_c<-setZ(era_stack_c,dt3,"time")
getZ(era_stack_c)
r_tmm <- calc(tmean_stack_c,  fun = function(x) {mean(x,na.rm=TRUE)} )
#use dissagregatte
r_tmm<- disaggregate(r_tmm, fact=3)
plot(r_tmm)

#mask wit sf
cba_limites<-sf::read_sf("/home/julius/cordoba/cba_polygon_wgs84.shp") 
cba_sp <- as_Spatial(cba_limites)
proj4string(cba_sp) <- CRS("+init=epsg:4326")
map_tmmax <- mask(r_tmm,cba_sp)
plot(map_tmmax)

###########################################################
#                          Figure 2b) Percentile 90
###########################################################
##read p90
##need cba_sp
path1<-("/home/julius/ERA5/raster/p90_om_c.tiff")

p90<-raster(path1)
plot(p90)
par("mar")#check par
par(mar=c(1,1,1,1))#correcting par
p90<- disaggregate(p90, fact=3)
map_p90<- mask(p90,cba_sp)
plot(map_p90)

###############################################
#            FIGURE 2
###############################################
#2a
#needs cba dpto
path2<-"/home/julius/cordoba/cba_dptos_polygon_wgs84.shp" ##read vector of cordoba
cba_dpto<-st_read(path2) 

library(tmap)
paleta<- c('#83b2d0','#95dab6','#f2e6b1','#ffcc5c' ,'#dc8580')

figure2a<-tm_shape(map_tmmax)+
  tm_raster(palette=paleta,
            title="temperature (°C)")+
  
  tm_shape(cba_dpto) +
  tm_borders() +
  tmap::tm_layout(, 
                  inner.margins=c(0,0,.1,0), 
                  title.size=.8, 
                  title.position = c('center', 'TOP')) + 
  tmap::tm_layout(legend.only = FALSE, 
                  legend.outside.position = c('center', 'TOP'), 
                  legend.title.size=1.5,
                  legend.position = c('right','bottom'),
                  legend.title.color = 'black'
  )

figure2a

########################################Figure2b

paleta<- c('#83b2d0','#95dab6','#f2e6b1','#ffcc5c' ,'#dc8580')

figure2b<-tm_shape(map_p90)+
  tm_raster(palette=paleta,
            title="temperature (°C)")+
  
  tm_shape(cba_dpto) +
  tm_borders() +
  #title
  tmap::tm_layout(inner.margins=c(0,0,.1,0), 
                  title.size=.8, 
                  title.position = c('center', 'TOP')) + 
  #legend
  tmap::tm_layout(
                  legend.title.size = 0.5,
                  legend.text.size = 0.5,
                  legend.width=0.5,
                   legend.only = FALSE, 
                  #legend.outside.position = c('center', 'TOP'), 
                  legend.position = c('right','bottom'),
                  legend.title.color = 'black'
  )

figure2b
#################################COMPOSITE FIGURE 2}
current.mode <- tmap_mode("plot")
figure2<-tmap_arrange(
  figure2a,figure2b,
  ncol = 2,
  nrow = 1,
  #widths = NA,
  #heights = NA,
  #sync = FALSE,
  #asp = 0,
  outer.margins = 0.02)

tmap_save(
  tm = figure2,
  filename = "figure_2.svg",
  units = "cm",
  width = 10,
  height = 5,
  dpi = 600,
  scale = NA)

###############################################
#                   FIGURE 3
###############################################

#raster of hw count 2013
##need to reed raster with stars. not working with original raster .nc
#r <- read_stars("hwcount.tif")
library(sp)
getwd()
setwd('/home/julius/era5')
#path2<- "/home/julius/ERA5/raster/heat_count/heatcount_2013.tiff" #heat wave count
path2<- "/home/julius/ERA5/raster/heat_count/heatcount_2013.tiff"

r<-raster(path2 , package="raster")
plot(r)
#resampling raster
#use dissagregatte
r2<- disaggregate(r, fact=3)
#res(meuse.raster.disaggregate)
#############################################mask raster
#mask wit sf
cba_limites<-sf::read_sf("/home/julius/cordoba/cba_polygon_wgs84.shp") 
cba_sp <- as_Spatial(cba_limites)
proj4string(cba_sp) <- CRS("+init=epsg:4326")
borde_sf<-st_as_sf(cba_limites)
borde2 = st_transform(borde_sf, st_crs(r2))
#you need a sp object to mask (asSpatial..)
borde3<-as_Spatial (borde2)
hw_mask <- mask(r2, borde3)
plot(hw_mask)


#raster of hw count 2013 corrected p90 32
path2r<- "/home/julius/ERA5/raster/heat_count32/heatcount32_2013.tiff"
rr<-raster(path2r , package="raster")
plot(rr)
#resampling raster
#use dissagregatte
rr2<- disaggregate(rr, fact=3)
hw_maskr <- mask(rr2, borde3)
plot(hw_maskr)
##########################################/mask raster
##read vector of cordoba
#Dptos of cba
path2<-"/home/julius/cordoba/cba_dptos_polygon_wgs84.shp"
cba_dpto<-st_read(path2) 
crs(r)<-crs(cba_dpto)
# Add fill and border layers to nz shape
#tmaptools::palette_explorer()

###################################### making map with tmap
library(tmap)

##need masked hwcount with normal p90 and 32 corrected p90

paleta<- c('#83b2d0','#95dab6','#f2e6b1','#ffcc5c' ,'#dc8580')

figure3a<-tm_shape(hw_mask)+
  tm_raster(palette=paleta,
            title="heat-waves (n° events)")+
  
  tm_shape(cba_dpto) +
  tm_borders() +
  tmap::tm_layout(, 
                  inner.margins=c(0,0,.1,0), 
                  title.size=.8, 
                  title.position = c('center', 'TOP')) + 
  tmap::tm_layout(legend.only = FALSE, 
                  legend.outside.position = c('center', 'TOP'), 
                  legend.title.size=1.5,
                  legend.position = c('right','bottom'),
                  legend.title.color = 'black'
  )
figure3a

#figure3b heatwaves for 2015 corrected P90 and 35°C


library(tmap)
paleta<- c('#83b2d0','#95dab6','#f2e6b1','#ffcc5c' ,'#dc8580')

figure3b<-tm_shape(hw_maskr)+
  tm_raster(palette=paleta,
            title="heat-waves (n° events)")+
  
  tm_shape(cba_dpto) +
  tm_borders() +
  tmap::tm_layout(, 
                  inner.margins=c(0,0,.1,0), 
                  title.size=.8, 
                  title.position = c('center', 'TOP')) + 
  tmap::tm_layout(legend.only = FALSE, 
                  legend.outside.position = c('center', 'TOP'), 
                  legend.title.size=1.5,
                  legend.position = c('right','bottom'),
                  legend.title.color = 'black'
  )
figure3b

#Figure 3 c
figure3c<-tm_shape(g30_hc_y2015)+
  tm_raster(palette=paleta,
            title="heat-waves (n° events)")+
  
  tm_shape(cba_dpto) +
  tm_borders() +
  tmap::tm_layout(, 
                  inner.margins=c(0,0,.1,0), 
                  title.size=.8, 
                  title.position = c('center', 'TOP')) + 
  tmap::tm_layout(legend.only = FALSE, 
                  legend.outside.position = c('center', 'TOP'), 
                  legend.title.size=1.5,
                  legend.position = c('right','bottom'),
                  legend.title.color = 'black'
  )
figure3c


#################################COMPOSITE FIGURE 3
current.mode <- tmap_mode("plot")
figure3<-tmap_arrange(
  figure3a,figure3b,
  ncol = 2,
  nrow = 1,
  #widths = NA,
  #heights = NA,
  #sync = FALSE,
  #asp = 0,
  outer.margins = 0.02)

tmap_save(
  tm = figure3,
  filename = "/home/julius/ERA5/images/figure3.pdf",
  units = "cm",
  width = 10,
  height = 5,
  dpi = 600,
  scale = NA)

#'Heat-waves cummulative event, period (1992-2021)'


################################################
#####                           Figure 4
################################################
require(raster)
SISdm<-brick("/home/julius/ERA5/raster/anomalies/anom_2013.tiff")
#dates2013<-getZ(an_2013)
#write.table(dates2013, "/home/julius/ERA5/raster/anomalies/anom_2013.txt")
dates2013_2<-read.table("/home/julius/ERA5/raster/anomalies/anom_2013.txt")
setZ(SISdm, dates2013_2, name='time')
hovmoller(an_2013)


#####################################
# Table 1
#####################################

#'
######################################
##Table 1
######################################

#Now agregatting
library(raster)
#read heat-count
  count_hw<- raster("/home/julius/ERA5/hwcount.tif")

#Dptos of cba
path2<-"/home/julius/cordoba/cba_dptos_polygon_wgs84.shp"
cba_dpto<-st_read(path2) 
crs(count_hw)<-crs(cba_dpto)
library(sf)
library(stars)
library(ggplot2)

##need to reed raster with stars. not working with original raster .nc
hwcount2 <- read_stars("/home/julius/ERA5/hwcount.tif")
hw_by_dpto <- aggregate(hwcount2 ,cba_dpto, mean)#https://datacarpentry.org/semester-biology/materials/spatial-data-polygon-aggregation-R/
plot(hw_by_dpto, col=heat.colors(10))

df_dptos<- as.data.frame(cba_dpto)

df_hw_by_dpto<-as.data.frame(hw_by_dpto) #get df of counts
df_hw_by_dpto$dpto<-df_dptos$fna
df_hw_by_dpto$geometry<-NULL
write.table(df_hw_by_dpto, "table1_heatcount_by_dpto2.txt", sep = ";" )
#as_Spatial(hw_by_dpto)
#rasterToPoints(hw_by_dpto)
#So, we are interested in variability in heat-wave count

# harv_soils <- st_read("data/HARV/harv_soils.shp")

ggplot() +
  geom_stars(data = harv_dtm) +
  geom_sf(data = harv_soils, alpha = 0)


#################################################################
#          Figure 4. Temporal dynamic of days under heat-waves
#################################################################
# read table
hotdays<-read.table ("/home/julius/ERA5/dataframe/table_dayscount_by_dpto_master_1992_2021.txt"
           , sep = ","
           , header = TRUE
           , comment.char = ""  )

library(ggplot2)
names(hotdays)
#delete the first unusefull column
hotdays$X.<-NULL
dim(hotdays)

features <- c("Department",sprintf("%02d", seq(1992,2021)))
colnames(hotdays)<-features

names(hotdays)
hotdays
#columns to rows
hotdays2<-hotdays %>%
  pivot_longer(!Department, names_to = "year", values_to = "heat_waves_days")
hotdays2
hotdays2$year<-as.Date(hotdays2$year)
str(hotdays2)
create time series plot
p <- ggplot(hotdays2, aes(x=year, y=heat_waves_days)) +
  geom_line()

#display time series plot
p
