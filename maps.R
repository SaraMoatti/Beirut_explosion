# specify the colour palette

tmaptools::palette_explorer()

##### MAXAR SAR IMAGERY ANALYSIS #####

### damage density _raster based data

Raster_cell_count_per_zones <- 
  st_read(here::here("MAXAR_data","image_differencing", "Raster_cell_count_per_zones.shp")) %>% 
  st_transform(., 22770)

# prepare the data
# get the density

density<-Raster_cell_count_per_zones%>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per ward
  mutate(density_raster=X_countfina/area)%>%
  #select density and some other variables 
  dplyr::select(density_raster, zone_numbe, Cadaster_1, X_countfina)


tmap_mode("plot")
breaks = c(4, 4.15, 4.3, 4.45, 4.6,4.75) 

dm1 <- tm_shape(density) + 
  tm_borders("white")+
  tm_polygons("density_raster",
              breaks=breaks,
              palette="-cividis",
              alpha = 0.7)+
  tm_scale_bar(position=c(0.01,0.1),text.size=0.5,color.dark = "grey46")+
  tm_compass(north=0, color.dark = "grey46",position=c("left","0.2"), size = 0.5)+
  tm_legend(show=FALSE)+
  tm_layout(title = "Damage assessment_SAR imagery based",title.size = 2,frame=FALSE)
  

dm1_legend <- tm_shape(density) +
  tm_polygons("density_raster",
              title = "Damage Density",
              breaks=breaks,
              palette="-cividis",
              alpha = 0.7,
              border.col = "white") +
  tm_layout(legend.only=TRUE, legend.title.size = 1,legend.position=c(0.01,0.25),asp=0.1)

density_map1=tmap_arrange(dm1,dm1_legend)
density_map1





##### Kmean and CLARA silhouette analysis

# get the silhouette coerfficient saved in the table earlier
clustPerfSI<-
  read.csv(here::here("MAXAR_data","image_differencing","silhouette_clustPerfSI.csv"))

clustPerfSI

#call the table where the solhouette coefficeitn are stored

knitr::kable(clustPerfSI, digits = 3, align = "c", 
             col.names = c("#clusters","Avg. Silhouette (k-means)","Avg. Silhouette (CLARA)"))

# set the layout
plot(clustPerfSI[,1], clustPerfSI[,2],
     xlim = c(1,13), ylim = range(clustPerfSI[,2:3]), type = "n", 
     ylab="Avg. Silhouette Coeff.", xlab="Number of clusters",
     main="Silhouette Coeff. by number of clusters")


# Plot Avg Silhouette values across # of clusters for K-means
lines(clustPerfSI[,1], clustPerfSI[,2], col="navyblue",lwd=2)
# Plot Avg Silhouette values across # of clusters for CLARA
lines(clustPerfSI[,1], clustPerfSI[,3], col="lightsteelblue4",lwd=2)

# Grid lines
abline(v = 1:13, lty=2, col="light grey")
abline(h = seq(0.30,0.44,0.02), lty=2, col="light grey")

legend("topright", legend=c("K-means","CLARA"), col=c("navyblue","lightsteelblue4"), lty=1, lwd=2)





####let's plot the clustered result with 3 clusters 

cuts=c(0,1,2,3) #set breaks

# plot reclassified data
plot(rstKM[[3]],
     legend = FALSE,
     col = c( "yellow", "navyblue","lightsteelblue4"), axes = FALSE,  
     main = "Classified Damage \n severe, medium, little to no damage",cex.main=1.2,box=FALSE)

legend("bottomleft",
       legend = c("severe damage","medium damage","little to no damage" ),
       fill = c("navyblue", "lightsteelblue4", "yellow"),
       border = FALSE,
       bty = "n",
       cex = 0.7) 


##### POINT BASED ANALYSIS ######

##### density map _observations based
breaks2 = c(0,0.3, 0.6, 0.9,1.2,1.5,1.8,2.1) 

tmap_mode("plot")

dm2 <- tm_shape(points_sf_joined) + 
  tm_borders("white")+
  tm_polygons("density_obs",
              breaks = breaks2,
              palette="-cividis",
              alpha = 0.7)+
  tm_scale_bar(position=c(0.01,0.1),text.size=0.5,color.dark = "grey46")+
  tm_compass(north=0, color.dark = "grey46",position=c("left","0.2"), size = 0.5)+
  tm_legend(show=FALSE)+
  tm_layout(title = "Damage assessment_Observations based",title.size = 2,frame=FALSE)


dm_legend2 <- tm_shape(points_sf_joined) +
  tm_polygons("density_obs",
              breaks= breaks2,
              title = "Damage Density",
              palette="-cividis",
              alpha = 0.7,
              border.col = "white") +
  tm_layout(legend.only=TRUE, legend.title.size = 1,legend.position=c(0.01,0.25),asp=0.1)

density_map2=tmap_arrange(dm2,dm_legend2)
density_map2


###plot all observations with relative metadata

tmap_mode("view")

t1=tm_shape(Bei_Exp_Zones) +
  tm_polygons(col = "navyblue", alpha = 0.3,border.col = "white") +
  tm_shape(Bei_Zones_Buildings) +
  tm_polygons(border.alpha = 0,col = "beige", alpha = 0.5) +
  tm_shape(volun_circ_spatial) +
  tm_dots(col = "navyblue", border.alpha = 0,size=0.008)

t2=tm_shape(mysay_spatial)+
  tm_dots(col = "yellow2", border.alpha = 0, size=0.008) 


t3=tm_shape(FrontLineEngineers_spatial) +
  tm_dots(col = "red4", border.alpha = 0, size=0.008) 

t4=tm_shape(BebWShebek_spatial) +
  tm_dots(col = "sienna2", border.alpha = 0, size=0.008) 

t5=tm_shape(Basecamp_spatial) +
  tm_dots(col = "palegreen3", border.alpha = 0, size=0.008) 

t1+t2+t3+t4+t5


# datasets sources

# Simple Bar Plot

# prepare the data

volun_circ_new_hist<-volun_circ_new %>% 
  mutate(NGO = "The Volunteer Circle")
mysay_new_hist<-mysay_new %>% 
  mutate(NGO = "My Say")
FrontLineEngineers_new_hist<-FrontLineEngineers_new %>% 
  mutate(NGO = "Front Line Engineers")
BebWShebek_new_hist<-BebWShebek_new %>% 
  mutate(NGO = "BeB W Shebbek")
Basecamp_new_hist<-Basecamp_new %>% 
  mutate(NGO="Basecamp")

combined_datasets_hist<- bind_rows(volun_circ_new_hist,
                              mysay_new_hist,
                              FrontLineEngineers_new_hist,
                              BebWShebek_new_hist,
                              Basecamp_new_hist ) %>% 
  add_count(NGO) %>% 
  dplyr::select(NGO, n)

p<-ggplot (combined_datasets_hist, aes (x= NGO, y=-n , fill = as.factor(n))) +         
  geom_bar (position = position_dodge(), stat = "identity",show.legend = FALSE) + 
  scale_fill_manual(values = alpha(c( "red", "#FFFF66","#66CC66","#003366", "#FF9966"), 0.006))+
  coord_flip () + 
  scale_x_discrete(name = "", position = "top") +     
  scale_y_continuous(name = "Number of reported damage",
                     breaks = seq(0, -1000, by = -200),  
                     labels = seq(0,  1000, by = 200))  

p




# DBSCAN

# Ripley's k analysis 

K <- combined_datasets_spatial_sub.ppp %>%
  Kest(., correction="border") %>%
  plot(col = c("black"))

## DBSCAN

# prepare the data
combined_datasets_spatial_sub_points <- combined_datasets_spatial_sub %>%
  coordinates(.)%>%
  as.data.frame()

# double check the data type
class(combined_datasets_spatial_sub_points)

#now run the dbscan analysis
db <- combined_datasets_spatial_sub_points%>%
  fpc::dbscan(.,eps = 100, MinPts = 30)

#reset the layout to default
par(mfrow=c(1,1))

#now plot the results
plot(db, combined_datasets_spatial_sub_points, main = "DBSCAN Output", frame = F)
plot(Bei_Exp_Zones$geometry, add=T)

db$cluster

combined_datasets_spatial_sub_points<- combined_datasets_spatial_sub_points %>%
  mutate(dbcluster=db$cluster)

chulls <- combined_datasets_spatial_sub_points %>%
  group_by(dbcluster) %>%
  dplyr::mutate(hull = 1:n(),
                hull = factor(hull, chull(coords.x1, coords.x2)))%>%
  arrange(hull)

chulls <- chulls %>%
  filter(dbcluster >=1)

dbplot <- ggplot(data=combined_datasets_spatial_sub_points, 
                 aes(coords.x1, coords.x2, colour=dbcluster, fill=dbcluster)) 
#add the points in
dbplot <- dbplot + geom_point()
#now the convex hulls
dbplot <- dbplot + geom_polygon(data = chulls, 
                                aes(coords.x1, coords.x2, group=dbcluster), 
                                alpha = 0.5) 

# plot 

dbplot + theme_bw() + coord_equal()

#add a basemap
#get the bbox for Beirut

BeirutWGSbb <- Bei_Exp_Zones%>%
  st_transform(., 4326)%>%
  st_bbox()

BeirutWGSbb

library(OpenStreetMap)

# choose the esri-topo style

basemap <- OpenStreetMap::openmap(c(33.86149,35.49469),c(33.91946,35.55270),
                                  zoom=NULL,
                                  "esri-topo")
plot.OpenStreetMap(basemap) 

# convert the basemap to local grid

basemap_bng <- openproj(basemap, projection="+init=epsg:22770" , alpha=0.2)

autoplot.OpenStreetMap(basemap_bng) + 
  geom_point(data=combined_datasets_spatial_sub_points, 
             aes(coords.x1,coords.x2, 
                 colour=dbcluster, 
                 fill=dbcluster)) + 
  geom_polygon(data = chulls, 
               aes(coords.x1,coords.x2, 
                   group=dbcluster,
                   fill=dbcluster), 
               alpha = 0.5)  




##### UNHABITAT socio-economic classification #####

tmap_mode("plot")

dm3 <- tm_shape(UN_habitat_shp) + 
  tm_borders("white")+
  tm_polygons("socio_economic_classification",
              #breaks=breaks,
              palette="PuOr",
              alpha = 0.7)+
  tm_scale_bar(position=c(0.01,0.1),text.size=0.5,color.dark = "grey46")+
  tm_compass(north=0, color.dark = "grey46",position=c("left","0.2"), size = 0.5)+
  tm_legend(show=FALSE)+
  tm_layout(title = "Socio-Economic Classification",title.size = 2,frame=FALSE)


dm3_legend <- tm_shape(UN_habitat_shp) +
  tm_polygons("socio_economic_classification",
              title = "",
              #breaks=breaks,
              palette="PuOr",
              alpha = 0.7,
              border.col = "white") +
  tm_layout(legend.only=TRUE, legend.title.size = 1,legend.position=c(0.01,0.25),asp=0.1)

density_map3=tmap_arrange(dm3,dm3_legend)
density_map3
