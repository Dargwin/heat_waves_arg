library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library("rasterVis")
library("RColorBrewer")
library('raster')
     
    
    
#mascara <- shapefile("/home/jdp/Modelado de Nicho/Shapes/ARG_adm0.shp")
#plot(mascara)
    
    
    
##############################################################3
###### reading files of ERA5 daily max  temp for each year (in .netcdf and  kelvin)
###############################################################
    
    
raster_files<-list.files(path = "/home/jdp/MEGA/CONAE/Olas de calor/Prueba Arg",
                        pattern = ".nc",
                        full.names = TRUE,
                        ignore.case = TRUE)
raster_files
    
#for {for i in 1: lenght(files_e)}
 #r_name <- list.files(mypath,full.names = F)
    
############################################################################
# Here we take netcdf files list and create annual raster brick with temperatures
##########################################################################
rList <- list() # Create empty listto save raster values
    #statList <- list() # to save data.frame with statistics
    for(i in 1:length(raster_files)){
      temp <- stack(raster_files[i],varname="t2m")#le aclaras que chupe la variable "t2m"
      dates_era<-getZ(temp)
      temp2<-temp-273.15
      temp2<-setZ(temp2, dates_era, "time")
      rList[[i]] <- temp2 # extract values for each raster
      out <- paste0("This is my output of iteration ", i, "")  # Some output
       print(out)       
                                    }
rList

########################################
#my process
########################################
#https://www.blasbenito.com/post/02_parallelizing_loops_with_r/

#################################################################################################
#Calculate and read p90 for Argentina
#################################################################################################
p90<-raster("/home/jdp/MEGA/CONAE/Olas de calor/Prueba Arg/p90_om_c_ARG.tif")
plot(p90)
# list of rasters
#rList
rList2<-unlist(rList)
#plot(rList2[[1]])
#plot(rList[[3]])
#reclasify p90
#p90g30<-p90
# #assumong minimum temperature of 32°C
#  p90g30 <- reclassify(p90,
#                       c(-Inf, 32, 32))
# plot(p90g30)
#years<-seq(from = 2000, to = 2021, by=1)

#####################################################################################################
#calculate anomalies (Tdaily-P90)
#########################################################################################################
#set objects to null before you run loop
orden=NULL; hc=NULL; r=NULL; rast.list2=NULL
rast.list2<-list()
##need raster list with ERA5 daily max temperatures
#i) for each day calculate difference between daily temp and P90. 
#ii) only retain positive values and set null the another ones
#AUMENTAR MEMORIA SWAP EN LINUX!!
#swapon -s
#fallocate -l 6G /swapfile
#

#gc() #libera memoria en r

output<-for (j in 1:31) {
  dates9<-getZ(rList[[j]])
  gg30_c_a <- overlay(x=rList[[j]],y=p90,fun=function(x,y){x-y}) #hago Tmax - TP90
  gg30_c_ap<-gg30_c_a > 0 #put null in the others
  gg30_c_ap<-setZ(gg30_c_ap, dates9, "time")
  rast.list2[[j]]<- gg30_c_ap
  out <- paste0("This is my output of iteration ", j, "for year ", j+1991, ".")  # Some output
  print(out)                                                # Using print function
}
rast.list2
class(rast.list2)


###########################################################################################################33
plot(rast.list2[[24]])
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


##################################
####### count heat waves
####################################
rast.list3<-list()
j=NULL;gg30_c_a=NULL;gg30_c_ap<-NULL; gg30_c_ap=NULL
y=NULL;z=NULL; z2=NULL; suma1=NULL; z4=NULL
for(j in 1:length(rast.list2)){
  hc<-calc(rast.list2[[j]], 
           forceapply=TRUE,
           fun= function(x){
             rle<- rle(x)
             rle$l_filter<-as.integer(rle$lengths>2)#me quedo con largos de 3 dias o mas
             rle$v_filter<-as.integer(rle$values>0)
             product<-rle$v_filter*rle$l_filter#aca anula sea por pocos dias o por sin anomalia
             sumprod<-sum(product)
             return(sumprod)
           })
  rast.list3[[j]]<- hc
  out <- paste0("This is my output of iteration ", j, "for year ", j+1991, ".")  # Some output
  print(out)                                                # Using print function
                          }

rast.list3
plot(rast.list3[[2]])
##plot all years by each one
     for(i in 1:length(rast.list3)){
  plot(rast.list3[[i]], main=paste0("year ",1991+i))
                              }

#hc2015<-rast.list3[[24]]
#plot(hc2015)
#writeRaster(hc2015, "/home/julius/ERA5/raster/heat_count/hc2015.tiff", format="GTiff")
#https://stackoverflow.com/questions/47591678/how-to-batch-process-geotiffs-in-r-with-lapply
#write anomalies for each year in warm semester


#write heat count

i=NULL
for (i in 1:length(rast.list3)){
  writeRaster(rast.list3[[i]], filename = paste0("/home/jdp/MEGA/CONAE/Olas de calor/heat_count/heatcount_",i+1991,".tiff"), format = "GTiff", overwrite = TRUE)
  out <- paste0("This is my output of iteration ", i, "for year ", i+1991, ".")  # Some output
  print(out)                                                # Using print function
}


##################################
####### count days under heat waves
####################################
rast.list4<-list()
j=NULL;gg30_c_a=NULL;gg30_c_ap<-NULL; gg30_c_ap=NULL
y=NULL;z=NULL; z2=NULL; suma1=NULL; z4=NULL
for(j in 1:length(rast.list2)){
  hc<-calc(rast.list2[[j]], 
           forceapply=TRUE,
           fun= function(x){
             rle<- rle(x)
             rle$l_filter<-as.integer(rle$lengths>2)
             rle$v_filter<-as.integer(rle$values>0)
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
  writeRaster(rast.list4[[i]], filename = paste0("/home/jdp/MEGA/CONAE/Olas de calor/heat_days/heatdays_",i+1991,".tiff"), format = "GTiff", overwrite = TRUE)
  out <- paste0("This is my output of iteration ", i, "for year ", i+1991, ".")  # Some output
  print(out)                                                # Using print function
}



#plot(rast.list4[[2]])
##plot all years by each one
#for(i in 1:length(rast.list4)){
#  plot(rast.list4[[i]], main=paste0("year ",1991+i))
#}
