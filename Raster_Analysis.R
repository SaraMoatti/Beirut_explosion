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

# load beirut operartional zones shapefile

Bei_Exp_Zones <- 
  st_read(here::here("data", "beirut_port_explosions_operational_zones_139", "beirut_port_explosions_operational_zones_139.shp"))

# load the Maxar data rasters before the imagery from before the explosion
# rasters have different extents

#try to bring the rgb stack
r1_stack<-
  stack(here::here("MAXAR_data","Raster tif before and after the blast","Before","10300500A5F95600.tif"))

#explore the raster
r1_stack@layers
r1_stack[[2]]

#unify the extents 
r1_stack<- r1_stack%>% 
  crop(.,Bei_Exp_Zones) %>% 
  mask(.,Bei_Exp_Zones)

r1_stack_df  <- as.data.frame(r1_stack, xy = TRUE)

#remove infinite values and NA

r1_stack_df <- r1_stack_df[is.finite(rowSums(r1_stack_df)),]

str(r1_stack_df)

#plot histogram of first band

ggplot() +
  geom_histogram(data = r1_stack_df, aes(X10300500A5F95600.1))

# plot the stack
plotRGB(r1_stack)

# repeat the process with images after the explosion
# after

r1_after_stack<-
  stack(here::here("MAXAR_data","Raster tif before and after the blast","After","10300500A6F8AA00.tif"))

r1_after_stack@layers
r1_after_stack[[2]]

# crop and mask to Beirut operational zones

r1_after_stack<- r1_after_stack%>% 
  crop(.,Bei_Exp_Zones) %>% 
  mask(.,Bei_Exp_Zones)

# transform to dataframe
r1_after_stack_df  <- as.data.frame(r1_after_stack, xy = TRUE)

#remove infinite values and NA

r1_after_stack_df <- r1_after_stack_df[is.finite(rowSums(r1_after_stack_df)),]

str(r1_after_stack_df)

#plot histogram of first band

ggplot() +
  geom_histogram(data = r1_after_stack_df, aes(X10300500A6F8AA00.1))

# plot the 
plotRGB(r1_after_stack)

#check if both have same extent

extent(r1_after_stack)
extent(r1_stack)

r1_after_stack<-r1_after_stack %>% 
  resample(.,r1_stack,
           method = "ngb")

# image differencing 
# trial with I2-I1

stack_after_before <- overlay(r1_after_stack,
                              r1_stack,
                        fun = function(r1, r2) { return( r1 - r2) })

stack_after_before_df  <- as.data.frame(stack_after_before[[1]], xy = TRUE)

plot(stack_after_before$layer.3 , main = "Blue")

savetofolder_stack_after_before<- 
  writeRaster(stack_after_before,
              filename=file.path(here::here("MAXAR_data","image_differencing"),"stack_after_before.tif"))

# increasing the memory to handle the process

memory.limit()

memory.limit(size=40000)

# log rationing difference
# log10(r1/r2) 

stack_after_before_log <- overlay(r1_after_stack,
                              r1_stack,
                              fun = function(r1, r2) { return(log10(r1/r2))})
# save the outcome

savetofolder_stack_after_before_log<- 
  writeRaster(stack_after_before_log,
              filename=file.path(here::here("MAXAR_data","image_differencing"),"stack_after_before_log.tif"))

# another trial
# abs log difference

abs_stack_after_before_log <- overlay(r1_after_stack,
                                      r1_stack,
                                      fun = function(r1, r2) { return(abs(log10(r1/r2)))})
plot(abs_stack_after_before_log$layer.1)

savetofolder_abs_stack_after_before_log<- 
  writeRaster(abs_stack_after_before_log,
              filename=file.path(here::here("MAXAR_data","image_differencing"),"abs_stack_after_before_log.tif"))

stack_after_before_df  <- as.data.frame(stack_after_before[[1]], xy = TRUE)


# transform image differencing rasterstack to dataframe
# takes ages

stack_after_before_log_df  <- as.data.frame(stack_after_before_log, xy = TRUE)


plot(stack_after_before_log_df)

# look at the result
plotRGB(stack_after_before_log,axes=TRUE, stretch="lin")
