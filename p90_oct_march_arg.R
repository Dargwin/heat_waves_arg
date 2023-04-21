#percentile90 using only warm months (oct to march)
####
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library("rasterVis")
library("RColorBrewer")
library('raster')


#period of time 30 years
files_e<-list.files(path = '/home/jdp/MEGA/CONAE/Olas de calor/Prueba Arg',
                    pattern = ".nc",
                    full.names = TRUE,
                    ignore.case = TRUE)
era_stack5<-stack(files_e ,varname="t2m")
era_stack5
  getZ(era_stack5)
plot(era_stack5$X1992.06.19)
getwd()
setwd("/home/jdp/MEGA/CONAE/Olas de calor/Prueba Arg")
writeRaster(era_stack5$X1992.01.01, "stack_arg_ver.tiff",format="GTiff")
#Put in a date format (exclude the format to avoid problems)
dt3<-seq(as.Date("1992-01-01"), as.Date("2022-12-31"), by="days")
#asignar las fechas como Z variable de nombre time
era_stack5<-setZ(era_stack5,dt3,"time")
getZ(era_stack5)
###################################
#defininf object a
a<-era_stack5
##############initiate loop to subset and split raster by dates
#el total de las bandas del raster stack comienza el 1/01/1992
years<-seq(from = 1992, to = 2022, by=1)
#inicializar en 0 todas las variables antes de empezar el loop
orden=NULL; aux=NULL; r=NULL; rast.list=NULL
rast.list<-list()
for (j in 1:31){
  aux<- a[[which(getZ(a) >= as.Date(paste0(j+1991,'-10-01')) & getZ(a) < as.Date(paste0(j+1992,'-04-01'))) ]]
  rast.list[[j]]<-aux
  out <- paste0("This is my output of iteration ", j, "for year ", j+1991, ".")  # Some output
  print(out)                                                # Using print function
}

#veo la lista donde cada posicion contiene un raster stack desde octubre a marzo 
rast.list
era_oct_march<-stack(rast.list)

#So, calculate q90 for each cell grid on raster
p90_om <- calc(era_oct_march , fun = function(x) {quantile(x,probs = 0.9,na.rm=TRUE)} )
#el primer resultado esta en grados kelvin que es el original de ERA5
#paso a centigrados
    p90_om_c<-p90_om-273.15
plot(p90_om_c)

writeRaster(p90_om_c, "p90_om_c_ARG.tiff",format="GTiff")
