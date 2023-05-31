library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library("rasterVis")
library("RColorBrewer")
library('raster')


##############################################################3
###### reading files of ERA5 daily max  temp for each year
###############################################################
raster_files<-list.files(path = '/home/julius/ERA5/years',
                    pattern = ".nc",
                    full.names = TRUE,
                    ignore.case = TRUE)
#for {for i in 1: lenght(files_e)}
r_name <- list.files(mypath,full.names = F)
rList <- list() # to save raster values
#statList <- list() # to save data.frame with statistics


for(i in 1:length(raster_files)){
  temp <- stack(raster_files[i],varname="t2m")
  dates_era<-getZ(temp)
  temp2<-temp-273.15
  temp2<-setZ(temp2, dates_era, "time")
  rList[[i]] <- temp2 # extract values for each raster
  out <- paste0("This is my output of iteration ", i, "")  # Some output
   print(out)       
                                }
rList
##########################################################
###                code for paralellization
##########################################################
list.of.packages <- c(
  "foreach",
  "doParallel",
  "ranger",
  "palmerpenguins",
  "tidyverse",
  "kableExtra"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages) > 0){
  install.packages(new.packages, dep=TRUE)
}

#loading packages
for(package.i in list.of.packages){
  suppressPackageStartupMessages(
    library(
      package.i, 
      character.only = TRUE
    )
  )
}
parallel::detectCores()
n.cores <- parallel::detectCores() - 1
#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "FORK"
)
#check cluster definition (optional)
print(my.cluster)
#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)
#check if it is registered (optional)
foreach::getDoParRegistered()
#how many workers are available? (optional)
foreach::getDoParWorkers()

########################################
#my process
########################################
#https://www.blasbenito.com/post/02_parallelizing_loops_with_r/

#p90<-raster("/home/julius/ERA5/p90.tif")
p90<-raster("/home/julius/ERA5/raster/p90_om_c.tiff")

# list of rasters
rList
rList2<-unlist(rList)
plot(rList2[[1]])
#plot(rList[[3]])

#reclasify p90
#p90g30<-p90
# #assumong minimum temperature of 32°C
  p90g30 <- reclassify(p90,
                      c(-Inf, 32, 32))
# plot(p90g30)
#years<-seq(from = 2000, to = 2021, by=1)
orden=NULL; hc=NULL; r=NULL; rast.list2=NULL
rast.list2<-list()
rList
##need raster list with ERA5 daily max temperatures
output<-foreach (
  j=1:30
) %dopar% {
  dates9<-getZ(rList[[j]])
  gg30_c_a <- overlay(x=rList[[j]],y=p90g30,fun=function(x,y){x-y})
  gg30_c_ap<-gg30_c_a > 0
  gg30_c_ap<-setZ(gg30_c_ap, dates9, "time")
  rast.list2[[j]]<- gg30_c_ap
  #out <- paste0("This is my output of iteration ", j, "for year ", j+1999, ".")  # Some output
  #print(out)                                                # Using print function
}

output
plot(output[[24]])
an_2015<-output[[24]]
setwd("/home/julius/ERA5/anomalias/warm_semester/")
# #save anomalies 2015
# writeRaster(an_2015, "/home/julius/ERA5/raster/anomalies/anom_2015.tiff", format="GTiff")

parallel::stopCluster(cl = my.cluster)

##################################
####### count heat waves
####################################
rast.list3<-list()
j=NULL;gg30_c_a=NULL;gg30_c_ap<-NULL; gg30_c_ap=NULL
y=NULL;z=NULL; z2=NULL; suma1=NULL; z4=NULL
for(j in 1:length(output)){
  hc<-calc(output[[j]], 
           forceapply=TRUE,
           fun= function(x){
             rle<- rle(x)
             rle$l_filter<-as.integer(rle$lengths>2)
             rle$v_filter<-as.integer(rle$values>0)
             product<-rle$v_filter*rle$l_filter
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

#average map

#write heat count

i=NULL
for (i in 1:length(rast.list3)){
  writeRaster(rast.list3[[i]], filename = paste0("/home/julius/ERA5/raster/heat_count32/heatcount32_",i+1991,".tiff"), format = "GTiff", overwrite = TRUE)
  out <- paste0("This is my output of iteration ", i, "for year ", i+1991, ".")  # Some output
  print(out)                                                # Using print function
                                }
