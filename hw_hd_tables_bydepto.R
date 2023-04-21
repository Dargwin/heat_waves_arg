#Conteo de olas de calor por depto y por año
library(raster)

raster_files<-list.files(path = "/home/jdp/MEGA/CONAE/Olas de calor/heat_waves",
                         pattern = ".tif",
                         full.names = TRUE,
                         ignore.case = TRUE)
raster_files

hwcount <- stack(raster_files)
hwcount <- brick(hwcount)
hwcount

#Agregarlas por depto
library(sf)

path2<-"/home/jdp/MEGA/CONAE/Olas de calor/mail sonia/Deptos/Departamentos.shp"
arg_dpto<-st_read(path2)
crs(hwcount)<-crs(arg_dpto)
getwd()
setwd("/home/jdp/MEGA/CONAE/Olas de calor")

hw_by_depto_max <-extract(hwcount, arg_dpto, fun = max, sp= TRUE)
write.table(hw_by_depto_max, "table_heatcount_by_dpto_max.csv", sep = ";", na = "0")

hw_by_depto_mean <-extract(hwcount, arg_dpto, fun = mean, sp= TRUE)
write.table(hw_by_depto_mean, "table_heatcount_by_dpto_mean.csv", sep = ";", na = "0")

hw_by_depto_median <-extract(hwcount, arg_dpto, fun = median, sp= TRUE)
write.table(hw_by_depto_median, "table_heatcount_by_dpto_median.csv", sep = ";", na = "0")

#Conteo de días bajo olas de calor por depto y por año

raster_files2<-list.files(path = "/home/jdp/MEGA/CONAE/Olas de calor/heat_wave_days",
                         pattern = ".tif",
                         full.names = TRUE,
                         ignore.case = TRUE)
raster_files2

hdcount <- stack(raster_files2)
hdcount <- brick(hdcount)
hdcount

#Agregarlas por depto
crs(hdcount)<-crs(arg_dpto)
hd_by_depto_max <-extract(hdcount, arg_dpto, fun = max, sp= TRUE)
write.table(hd_by_depto_max, "table_heatdays_by_dpto_max.csv", sep = ";", na = "0")

hd_by_depto_mean <-extract(hdcount, arg_dpto, fun = mean, sp= TRUE)
write.table(hd_by_depto_mean, "table_heatdays_by_dpto_mean.csv", sep = ";", na = "0")

hd_by_depto_median <-extract(hdcount, arg_dpto, fun = median, sp= TRUE)
write.table(hd_by_depto_median, "table_heatdays_by_dpto_median.csv", sep = ";", na = "0")


#Exportar para graficar
library(rgdal)

#To export a shapefile use thewriteOGR function. The first argument is the
#spatial object produced in R. dsn and layer are the same as above.
#The obligatory 4. argument is the driver used to generate the shapefile.
#The function ogrDrivers() lists all available drivers.
#If you want to export a shapfile to ArcGis or QGis you could use 
#driver = "ESRI Shapefile".

writeOGR(hd_by_depto_mean, dsn = "/home/jdp/MEGA/CONAE/Olas de calor", layer = "hd_bydepto_mean", driver = "ESRI Shapefile" )
writeOGR(hd_by_depto_max, dsn = "/home/jdp/MEGA/CONAE/Olas de calor", layer = "hd_bydepto_max", driver = "ESRI Shapefile" )
writeOGR(hd_by_depto_median, dsn = "/home/jdp/MEGA/CONAE/Olas de calor", layer = "hd_bydepto_median", driver = "ESRI Shapefile" )

writeOGR(hw_by_depto_mean, dsn = "/home/jdp/MEGA/CONAE/Olas de calor", layer = "hw_bydepto_mean", driver = "ESRI Shapefile" )
writeOGR(hw_by_depto_max, dsn = "/home/jdp/MEGA/CONAE/Olas de calor", layer = "hw_bydepto_max", driver = "ESRI Shapefile" )
writeOGR(hw_by_depto_median, dsn = "/home/jdp/MEGA/CONAE/Olas de calor", layer = "hw_bydepto_median", driver = "ESRI Shapefile" )


hd_by_depto_mean
