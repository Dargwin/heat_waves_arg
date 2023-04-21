library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library("rasterVis")
library("RColorBrewer")
library('raster')
  
##############################################################
###### reading files of ERA5 daily min  temp for each year (in .netcdf and  kelvin)
###############################################################
    
    
raster_files_min<-list.files(path = "/home/jdp/MEGA/CONAE/Olas de calor/con_minimas",
                        pattern = ".nc",
                        full.names = TRUE,
                        ignore.case = TRUE)
raster_files_min
    
############################################################################
# Here we take netcdf files list and create annual raster brick with temperatures
##########################################################################
rList_min <- list() # Create empty list to save raster values
    #statList <- list() # to save data.frame with statistics
    for(i in 1:length(raster_files_min)){
      temp <- stack(raster_files_min[i],varname="t2m")#le aclaras que chupe la variable "t2m"
      dates_era<-getZ(temp)
      temp2<-temp-273.15
      temp2<-setZ(temp2, dates_era, "time")
      rList_min[[i]] <- temp2 # extract values for each raster
      out <- paste0("This is my output of iteration ", i, "")  # Some output
      print(out)       
                                    }
rList_min #tengo una lista con las temperaturas


#################################################################################################
#Calculate and read p90s for Argentina
#################################################################################################
p90_max<-raster("/home/jdp/MEGA/CONAE/Olas de calor/Prueba Arg/p90_om_c_ARG.tif")
p90_min <-raster("/home/jdp/MEGA/CONAE/Olas de calor/con_minimas/p90_min_arg.tif")
plot(p90_max)
plot(p90_min)

# list of rasters
#rList_min
#rList2_min <-unlist(rList)
#rList2_min


#####################################################################################################
#calculate anomalies with minimum dayly temp (Tdaily-P90)
#########################################################################################################
#set objects to null before you run loop
orden=NULL; hc=NULL; r=NULL; rast.list2=NULL
rast.list_min<-list()
rList_min
##need raster list with ERA5 daily max temperatures
#i) for each day calculate difference between daily temp and P90. 
#ii) only retain positive values and set null the another ones
#AUMENTAR MEMORIA SWAP EN LINUX!!
#swapon -s
#fallocate -l 6G /swapfile


output<-for (j in 1:31) {
  dates9<-getZ(rList_min[[j]])
  gg30_c_a <- overlay(x=rList_min[[j]],y=p90_min,fun=function(x,y){x-y}) #hago Tmax - TP90
  gg30_c_ap<-gg30_c_a > 0 #put null in the others
  gg30_c_ap<-setZ(gg30_c_ap, dates9, "time")
  rast.list_min[[j]]<- gg30_c_ap
  out <- paste0("This is my output of iteration ", j, "for year ", j+1991, ".")  # Some output
  print(out)                                                # Using print function
}
rast.list_min #tengo un raster brick con los dias con anomalias en minimas
class(rast.list_min)


###########################################################################################################33
#plot(rast.list_min[[24]])
#an_2015<-output[[24]]
#plot(output[[22]])
#an_2013<-output[[22]]
#setwd("/home/julius/ERA5/anomalias/warm_semester/")
# #save anomalies 2013


#writeRaster(rast.list2[[1]]
#            , "/home/jdp/MEGA/CONAE/Olas de calor/Anomalias ARG/year"
#            , format="GTiff" 
#            , overwrite=TRUE
#            ,bandorder='BIL')

#####################################################################################################
#calculate anomalies with maximum dayly temp (Tdaily-P90)
#########################################################################################################

##############################################################
###### reading files of ERA5 daily max  temp for each year (in .netcdf and  kelvin)
###############################################################

raster_files_max<-list.files(path = "/home/jdp/MEGA/CONAE/Olas de calor/Prueba Arg",
                         pattern = ".nc",
                         full.names = TRUE,
                         ignore.case = TRUE)
raster_files_max

#ncpath <- "/home/jdp/MEGA/CONAE/Olas de calor/Prueba Arg/"
#ncname <- "fd0cf272-7a54-40e3-9c75-fcedbbe1ae22"  
#ncfname <- paste(ncpath, ncname, ".nc", sep="")
#dname <- "tmp"  # note: tmp means temperature (not temporary)
#ncin <- nc_open(ncfname)
#print(ncin)


############################################################################
# Here we take netcdf files list and create annual raster brick with temperatures
##########################################################################

rList_max <- list() # Create empty list to save raster values
#statList <- list() # to save data.frame with statistics
for(i in 1:length(raster_files_max)){
  temp <- stack(raster_files_max[i],varname="t2m")#le aclaras que chupe la variable "t2m"
  dates_era<-getZ(temp)
  temp2<-temp-273.15
  temp2<-setZ(temp2, dates_era, "time")
  rList_max[[i]] <- temp2 # extract values for each raster
  out <- paste0("This is my output of iteration ", i, "")  # Some output
  print(out)       
}
rList_max #tengo una lista con las temperaturas


#rList2_max<-unlist(rList)
#rList2_max



#####################################################################################################
#calculate anomalies with maximum dayly temp (Tdaily-P90)
#########################################################################################################
#set objects to null before you run loop
orden=NULL; hc=NULL; r=NULL; rast.list2=NULL
rast.list_max<-list()
##need raster list with ERA5 daily max temperatures
#i) for each day calculate difference between daily temp and P90. 
#ii) only retain positive values and set null the another ones
#AUMENTAR MEMORIA SWAP EN LINUX!!
#swapon -s
#fallocate -l 6G /swapfile


output2<-for (j in 1:31) {
  dates9<-getZ(rList_max[[j]])
  gg30_c_a2 <- overlay(x=rList_max[[j]],y=p90_max,fun=function(x,y){x-y}) #hago Tmax - TP90
  gg30_c_ap2<-gg30_c_a2 > 0 #put null in the others
  gg30_c_ap2<-setZ(gg30_c_ap2, dates9, "time")
  rast.list_max[[j]]<- gg30_c_ap2
  out <- paste0("This is my output of iteration ", j, "for year ", j+1991, ".")  # Some output
  print(out)                                                # Using print function
}

rast.list_max


#Conteo de olas de calor

pb <- txtProgressBar(min = 0, max = length(rast.list_max), style = 3)

suma_rasters <- function(r1, r2){
  return(r1 + r2)
}


heat_days <- mapply(suma_rasters, rast.list_max, rast.list_min)



##################################
####### count heat waves
####################################

rast.list3<-list()
j=NULL;gg30_c_a=NULL;gg30_c_ap<-NULL; gg30_c_ap=NULL
y=NULL;z=NULL; z2=NULL; suma1=NULL; z4=NULL
for(j in 1:length(heat_days)){
  hc<-calc(heat_days[[j]], 
           forceapply=TRUE,
           fun= function(x){
             rle<- rle(x)
             rle$v_filter<-as.integer(rle$values>1) #me quedo con los valores mayores a 1
             rle$l_filter<-as.integer(rle$lengths>2)#me quedo con largos de 3 dias o mas
             product<-rle$v_filter*rle$l_filter#aca anula sea por pocos dias o por sin anomalia
             sumprod<-sum(product)
             return(sumprod)
           })
  rast.list3[[j]]<- hc
  out <- paste0("This is my output of iteration ", j, "for year ", j+1991, ".")  # Some output
  print(out)                                                # Using print function
}

rast.list3

#write heat count

i=NULL
for (i in 1:length(rast.list3)){
  writeRaster(rast.list3[[i]], filename = paste0("/home/jdp/MEGA/CONAE/Olas de calor/heat_waves/heatwaves_",i+1991,".tiff"), format = "GTiff", overwrite = TRUE)
  out <- paste0("This is my output of iteration ", i, "for year ", i+1991, ".")  # Some output
  print(out)                                                # Using print function
}

##################################
####### count days under heat waves
####################################
rast.list4<-list()
j=NULL;gg30_c_a=NULL;gg30_c_ap<-NULL; gg30_c_ap=NULL
y=NULL;z=NULL; z2=NULL; suma1=NULL; z4=NULL
for(j in 1:length(heat_days)){
  hc<-calc(heat_days[[j]], 
           forceapply=TRUE,
           fun= function(x){
             rle<- rle(x)
             rle$v_filter<-as.integer(rle$values>1)
             rle$l_filter<-as.integer(rle$lengths>2)
             product<-rle$v_filter*rle$l_filter*rle$lengths
             sumprod<-sum(product)
             return(sumprod)
           })
  rast.list4[[j]]<- hc
  out <- paste0("This is my output of iteration ", j, " for year ", j+1991, ".")  # Some output
  print(out)                                                # Using print function
}

rast.list4

i=NULL
for (i in 1:length(rast.list4)){
  writeRaster(rast.list4[[i]], filename = paste0("/home/jdp/MEGA/CONAE/Olas de calor/heat_wave_days/heat_wave_days_",i+1991,".tiff"), format = "GTiff", overwrite = TRUE)
  out <- paste0("This is my output of iteration ", i, "for year ", i+1991, ".")  # Some output
  print(out)                                                # Using print function
}

