## this analysis follows the below tutorials

#http://remote-sensing.org/unsupervised-classification-with-r/
#https://www.r-exercises.com/2018/02/28/advanced-techniques-with-raster-data-part-1-unsupervised-classification/?__cf_chl_captcha_tk__=95093339d521789be882d0b5b3ab77f12debcf39-1609698000-0-AWqv_DC4nb9vdENGfsl9H-zfUjYkvB_MYG1F7eGd5_wbbLMYmiRCS7O4I-SO2gJ-vh0VHQjYruVihL_i6Yva7OXsHZSx6uy1ao-BCuTjPCDNdNI0rrL8rJZjC_-w-uTovyTYlZtZBqGyLMuMJAc1jkeYRoKKT-smd9quKJkufCXUuaKPOY0gOgKr3HEvnJ85RCLHlu5yR0nwwxE4Yby2OXixVxBC-zfs6KcIjSX_D0YPtFFcQJ6zeHLGvrKubUdgvy-QgK-yibPsf1SME3D3GOSrbjKBlXp1zAKXRZixFdGgwPdR7gsovlOSZYRvjp8z6HkMFazNcHzDLR7wxf2AnvQpJpUvssrPSz96xRF37PmvdIwIlebYRemHwAaNRT_4zC-CFczGncHctZ4WThw0TrUNHpDT-3Z_IXzO1MG5y0wUBDxiF9GDoVoBRPthwXgYTgXugtHjt4Hw3DdeR7p-ATTmjbnGihYy0TPMJVxztb5mjx5_cxKDpS1aTllEtIQbDHfaayxl1nq3VPTts82w_TkhHvYyknqewe5c1eVZiUxYrNhJSnL1MfvCjaJOY3mLcYkAxuMSzkMY7zm3V73jEtIUlItGap3rTI_ZbC45vMxPQnF_CpI6EyBTcuxrvINgckLvYknpcFAzl2k6J3N_Mm5DjzswE080byrC3FSD-G_i


# loading libraries

library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(dplyr)
library(stringr)
library(readr)
library(rgdal)
library(tmap)
library(janitor)
library(ggplot2)
library(raster)
library(fpc)
library(dbscan)
library(tidyverse)
library(tidyr)
library(fs)
library(RStoolbox)
library(raster)
library(cluster)

# load the log image rationing 

stack_after_before_log<-
  stack(here::here("MAXAR_data","image_differencing","stack_after_before_log.tif"))

# Extract all values from the raster into a data frame
rstDF <- values(stack_after_before_log)

head(rstDF)

# Check NA's in the data
idx <- complete.cases(rstDF)
head(idx)

# Initiate the raster datasets that will hold all clustering solutions 
# from 2 groups/clusters up to 12
rstKM <- raster(stack_after_before_log[[1]])
rstCLARA <- raster(stack_after_before_log[[1]])

# increase the memory limit to handle the processing
memory.size(40000)

for(nClust in 2:12){
  
  cat("-> Clustering data for nClust =",nClust,"......")
  
  # Perform K-means clustering
  km <- kmeans(rstDF[idx,], centers = nClust, iter.max = 50)
  
  # Perform CLARA's clustering (using manhattan distance)
  cla <- cluster::clara(rstDF[idx, ], k = nClust, metric = "manhattan")
  
  # Create a temporary integer vector for holding cluster numbers
  kmClust <- vector(mode = "integer", length = ncell(stack_after_before_log))
  claClust <- vector(mode = "integer", length = ncell(stack_after_before_log))
  
  # Generate the temporary clustering vector for K-means (keeps track of NA's)
  kmClust[!idx] <- NA
  kmClust[idx] <- km$cluster
  
  # Generate the temporary clustering vector for CLARA (keeps track of NA's too ;-)
  claClust[!idx] <- NA
  claClust[idx] <- cla$clustering
  
  # Create a temporary raster for holding the new clustering solution
  # K-means
  tmpRstKM <- raster(stack_after_before_log[[1]])
  # CLARA
  tmpRstCLARA <- raster(stack_after_before_log[[1]])
  
  # Set raster values with the cluster vector
  # K-means
  values(tmpRstKM) <- kmClust
  # CLARA
  values(tmpRstCLARA) <- claClust
  
  # Stack the temporary rasters onto the final ones
  if(nClust==2){
    rstKM    <- tmpRstKM
    rstCLARA <- tmpRstCLARA
  }else{
    rstKM    <- stack(rstKM, tmpRstKM)
    rstCLARA <- stack(rstCLARA, tmpRstCLARA)
  }
  
  cat(" done!\n\n")
}


# Write the clustering solutions for each algorithm

save_kmean<-
  writeRaster(rstKM,
              filename=file.path(here::here("MAXAR_data","image_differencing"),"raster_kmean.tif"))
save_clara<-
  writeRaster(rstCLARA,
              filename=file.path(here::here("MAXAR_data","image_differencing"),"raster_clara.tif"))


#Evaluating Unsupervised Classification/Clustering Performance

library(clusterCrit)

# Start a data frame that will store all silhouette values
# for k-means and CLARA 

clustPerfSI <- data.frame(nClust = 2:12, SI_KM = NA, SI_CLARA = NA)

for(i in 1:nlayers(rstKM)){ # Iterate through each layer
  
  cat("-> Evaluating clustering performance for nClust =",(2:12)[i],"......")
  
  # Extract random cell samples stratified by cluster
  cellIdx_RstKM <- sampleStratified(rstKM[[i]], size = 2000)
  cellIdx_rstCLARA <- sampleStratified(rstCLARA[[i]], size = 2000)
  
  # Get cell values from the Stratified Random Sample from the raster 
  # data frame object (rstDF)
  rstDFStRS_KM <- rstDF[cellIdx_RstKM[,1], ]
  rstDFStRS_CLARA <- rstDF[cellIdx_rstCLARA[,1], ]
  
  # Make sure all columns are numeric (intCriteria function is picky on this)
  rstDFStRS_KM[] <- sapply(rstDFStRS_KM, as.numeric)
  rstDFStRS_CLARA[] <- sapply(rstDFStRS_CLARA, as.numeric)
  
  # Compute the sample-based Silhouette index for: 
  
  # K-means
  clCritKM <- intCriteria(traj = rstDFStRS_KM, 
                          part = as.integer(cellIdx_RstKM[,2]), 
                          crit = "Silhouette")
  # and CLARA
  clCritCLARA <- intCriteria(traj = rstDFStRS_CLARA, 
                             part = as.integer(cellIdx_rstCLARA[,2]), 
                             crit = "Silhouette")
  
  # Write the silhouette index value to clustPerfSI data frame holding 
  # all results
  clustPerfSI[i, "SI_KM"]    <- clCritKM[[1]][1]
  clustPerfSI[i, "SI_CLARA"] <- clCritCLARA[[1]][1]
  
  cat(" done!\n\n")
  
}

# save the results in a csv file

save_silhouette<-
  write.csv(clustPerfSI, file = here::here("MAXAR_data","image_differencing","silhouette_clustPerfSI.csv"), row.names = FALSE)

# print out a table with the silhouette index results for comparing each clustering solution:
  
silhouette_table<-
  knitr::kable(clustPerfSI, 
               digits = 3, align = "c", 
               col.names = c("#clusters","Avg. Silhouette (k-means)","Avg. Silhouette (CLARA)"))

silhouette_table

# make a plot for comparing the two algorithms

plot(clustPerfSI[,1], clustPerfSI[,2], 
     xlim = c(1,13), ylim = range(clustPerfSI[,2:3]), type = "n", 
     ylab="Avg. Silhouette Index", xlab="# of clusters",
     main="Silhouette index by # of clusters")

# Plot Avg Silhouette values across # of clusters for K-means
lines(clustPerfSI[,1], clustPerfSI[,2], col="red")
# Plot Avg Silhouette values across # of clusters for CLARA
lines(clustPerfSI[,1], clustPerfSI[,3], col="blue")

# Grid lines
abline(v = 1:13, lty=2, col="light grey")
abline(h = seq(0.30,0.44,0.02), lty=2, col="light grey")

legend("topright", legend=c("K-means","CLARA"), col=c("red","blue"), lty=1, lwd=1)

rstKM<-
  stack(here::here("MAXAR_data","image_differencing","raster_kmean.tif"))

# the highest coefficient corresponds to cluser n =3 in the k-mean method
plot(rstKM[[3]])

#let's plot the clustered result with 3 clusters 

cuts=c(0,1,2,3) #set breaks
pal <- colorRampPalette(c("white","red","blue"))

plot(rstKM[[3]], breaks=cuts, col = pal(4)) #plot with defined breaks

# plot reclassified data

plot(rstKM[[3]],
     legend = FALSE,
     col = c( "yellow", "navyblue","lightsteelblue4"), axes = FALSE,  
     main = "Classified Damage \n severe, medium, little to no damage",cex.main=0.7,box=FALSE)

legend("bottomleft",
       legend = c("severe damage","medium damage","little to no damage" ),
       fill = c("navyblue", "lightsteelblue4", "yellow"),
       border = FALSE,
       bty = "n",
       cex = 0.5) # turn off legend border


## unsupervised randomForest classification using kmeans
# this was not explanatory so it was not mentioned in the analysis

library("randomForest")

v <- na.omit(rstDF)

vx<-v[sample(nrow(v), 500),]
rf = randomForest(vx)
rf_prox <- randomForest(vx,ntree = 1000, proximity = TRUE)$proximity

E_rf <- kmeans(rf_prox, 12, iter.max = 100, nstart = 10)
rf <- randomForest(vx,as.factor(E_rf$cluster),ntree = 500)
rf_raster<- predict(stack_after_before_log,rf)

save_randomforest<-
  writeRaster(rf_raster,
              filename=file.path(here::here("MAXAR_data","image_differencing"),"raster_randomforest.tif"))


plot(rf_raster)

plot(rf_raster,
     legend = FALSE,
     col = c("yellow", "red", "beige"), axes = FALSE,
     main = "Classified Damage \n severe, medium, little to no damage")

legend("bottomleft",
       legend = c("severe trees", "medium damage", "little to no damage"),
       fill = c("red", "pink", "white"),
       border = FALSE,
       bty = "n") # turn off legend border


plot(rf_raster, col=c("white", 
                      "white","orange", "red","red"))



library(rasterVis)
library(viridis)


# Density Map per zone
# the raster::extract did not work as the file is huge
# the shapefile Raster_cell_count_per_zones.shp was obtained from QGIS through the zonal statistics
# the image differencing tiff layer:  stack_after_before_log and the beirut operational zones shapefile were used 

Raster_cell_count_per_zones <- 
  st_read(here::here("MAXAR_data","image_differencing", "Raster_cell_count_per_zones.shp")) %>% 
  st_transform(., 22770)

# plot each map
tmap_mode("plot")

# set the breaks
# for our mapped data


density<-Raster_cell_count_per_zones%>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per ward
  mutate(density_raster=X_countfina/area)%>%
  #select density and some other variables 
  dplyr::select(density_raster, zone_numbe, Cadaster_1, X_countfina)

# quick plot
density_map = tm_shape(density) +
  tm_polygons("density_raster",
              style="jenks",
              palette="PuOr",
              midpoint=0,
              popup.vars=c("Cadaster_1", "density"),
              title="Damage Reported Density")

density_map
