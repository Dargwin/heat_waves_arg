####################################################################
# Script to process and analyze heat waves time series by means of
# time series clustering
#
# Author: Juan Diego Pinotti
# Last modified: April 2023
####################################################################


#
# Load libraries
#

library(zoo) # used to interpolate
library(dtw)
library(dtwclust)
library(ISOweek)
library(tidyr)
library(ggplot2)

setwd("/home/jdp/MEGA/CONAE/Olas de calor")

#
# Data preparation
#


# read heatwaves data (heatwaves of Argentina per year 1992-2022)
#heatwaves in columns 8:38

heat_days <- read.csv("table_heatcount_by_dpto_median.csv", 
                      header = TRUE, sep = ",")
class(heat_days)
dim(heat_days)
head(heat_days)


heat_days2 <- heat_days[,8:38]
head(heat_days2)

class(heat_days2)

names_dto <- heat_days[,4]
names_dto
names_dto <- as.factor(names_dto)
head(names_dto)
heat_days3 <- apply(heat_days2, 2, as.numeric)
head(heat_days3)
heat_days3 <- cbind(names_dto,heat_days3)

class(heat_days3)
head(heat_days3)

#
# Data curation
#


dim(heat_days3)
heat_days_ts <- tslist(na.approx(heat_days3[,2:32]), simplify = T)
heat_days_ts
head(heat_days3)
head(heat_days_ts)
str(heat_days_ts)


# Config DTW_basic + DBA + preprocess

cfg_dtw_pp <- 
  compare_clusterings_configs(
    types = "partitional",
    k = 3:10, 
    controls = list(partitional = 
                      partitional_control(iter.max = 100L, 
                                          nrep = 10L)),
    preprocs = pdc_configs("preproc",
                           zscore = list(center = c(FALSE, TRUE))),
    distances = pdc_configs("distance", 
                            partitional = list(
                              dtw_basic = list(
                                window.size = seq(from = 1L, 
                                                  to = 5L, 
                                                  by = 1L),
                                norm = c("L1", "L2"))
                            )),
    centroids = pdc_configs("centroid", 
                            share.config = c("p"), 
                            dba = list(
                              window.size = seq(from = 1L, 
                                                to = 5L, 
                                                by = 1L),
                              norm = c("L1", "L2"))
    ),
    no.expand = c("window.size", "norm")
  )



#
# Print configs
#

cfg_dtw_pp

num_configs = sapply(cfg_dtw_pp, attr, which = "num.configs")
cat("\nTotal # of configs ", sum(num_configs), "\n")



# define evaluation indices, score and pick
evaluators <- cvi_evaluators("internal")
score_fun <- evaluators$score
pick_fun <- evaluators$pick


#
# Clustering comparisons
# 

data <- heat_days_ts

# dtw-dba
comp_dtw_pp <- 
  compare_clusterings(data, 
                      types = "partitional",
                      configs = cfg_dtw_pp, 
                      seed = 7L,
                      trace = TRUE,
                      score.clus = evaluators$score, 
                      pick.clus = evaluators$pick,
                      return.objects = TRUE)

# info of the best rep
best_dtw_pp_config <- comp_dtw_pp$pick$config
id_best <- comp_dtw_pp$pick$config$config_id
best_dtw_pp_config
id_best
# re-run the best
best_dtw_pp <- 
  repeat_clustering(series = data, 
                    clusterings = comp_dtw_pp, 
                    config_id = id_best,
                    tsclust = list(seed = 7L, 
                                   trace = TRUE, 
                                   control = partitional_control(
                                     iter.max = 100L, 
                                     nrep = 10L)))

str(best_dtw_pp)

# plot members and centroids separately
plot(best_dtw_pp, type = "series")
plot(best_dtw_pp, type = "centroids", lty=1, col="red")

best_dtw_pp@centroids

# Obtener los centroides de best_dtw_pp
centroids <- best_dtw_pp@centroids

# Crear una función para trazar la línea de regresión y mostrar los coeficientes y valores p
plot_regression_line <- function(x, y) {
  lm_model <- lm(y ~ x)
  coef <- coef(lm_model)[2]
  p_value <- summary(lm_model)$coefficients[2, 4]
  
  abline(lm_model, col = "black", lwd = 2)
  
  cat("Coeficiente:", round(coef, 4), "\n")
  cat("Valor p:", format(p_value, scientific = TRUE, digits = 4), "\n\n")
}

# Graficar los centroides con línea de regresión y mostrar los coeficientes y valores p
par(mfrow = c(1, 3))

for (i in 1:3) {
  plot(best_dtw_pp@centroids[[i]], type = "l",lwd = 2,col = "red", main = paste("Centroide", i))
  plot_regression_line(1:length(best_dtw_pp@centroids[[i]]), best_dtw_pp@centroids[[i]])
}



#get labels for each row
id_cluster <- matrix(best_dtw_pp@cluster, ncol = 1L, byrow = TRUE)
id_cluster


# Carga del shapefile de departamentos
library(rgdal)
departamentos <- readOGR("/home/jdp/MEGA/CONAE/Olas de calor/mail sonia/Deptos/Departamentos.shp")

# Carga del dataframe con la información de los clusters
id_cluster <-as.data.frame(id_cluster)

id_cluster <- cbind(heat_days$OBJECTID, id_cluster)
colnames(id_cluster) <- c("OBJECTID", "Cluster")
head(id_cluster)

# Join espacial
departamentos_clusters <- sp::merge(departamentos, id_cluster, by = "OBJECTID")

# Exportar shapefile
writeOGR(obj = departamentos_clusters,
         dsn = "/home/jdp/MEGA/CONAE/Olas de calor",
         layer = "Clusters_olas_prueba", driver = "ESRI Shapefile",
         overwrite_layer = TRUE)

