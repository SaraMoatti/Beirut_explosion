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

# first, get the Beirut Explosion operational zones
# operational zones reflects the damaged zones in Beirut (ref)

Bei_Exp_Zones <- 
  st_read(here::here("data", "beirut_port_explosions_operational_zones_139", "beirut_port_explosions_operational_zones_139.shp")) %>% 
# transform the coordinate reference system to Dei EL Zor that covers Lebanon
  st_transform(., 22770)

qtm(Bei_Exp_Zones)

# get the beirut buildings geojson to reflect on the urban area

Beirut_Buildings <- 
  st_read("https://opendata.arcgis.com/datasets/d4d43fbe781145d4b11f9eac3f5dc5a1_0.geojson") %>% 
  st_transform(., 22770)

qtm(Beirut_Buildings)

# crop the dataframe with buildings to the operational zones extent

Bei_Zones_Buildings<- Beirut_Buildings[Bei_Exp_Zones,]

qtm(Bei_Zones_Buildings)

# get the UNHABITAT soscio-economic classification of operational zones  

UN_habitat_shp <- 
  st_read(here::here("data","unhabitat_zone_data_batch2_2020_08_19", "UNHABITAT_Zone_Data_batch2_2020_08_19.shp")) %>% 
  st_transform(., 22770)

# rename the variable 

UN_habitat_shp<-UN_habitat_shp %>% 
  dplyr::rename( socio_economic_classification = UNHABITAT1 )

# plot the UN map
tmap_mode("plot")

# set the breaks
# for our mapped data
breaks = c(0, 5, 12, 26, 57, 286) 

tm1<- tm_shape(UN_habitat_shp) + 
  tm_polygons("socio_economic_classification", 
              breaks=breaks,
              palette="PuBu")+
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(a)", position=c(0,0.85), size=1.5)

legend <- tm_shape(UN_habitat_shp) +
  tm_polygons("socio_economic_classification",
              palette="PuBu") +
  tm_scale_bar(position=c(0.2,0.04), text.size=0.6)+
  tm_compass(north=0, position=c(0.65,0.6))+
  tm_layout(legend.only = TRUE, legend.position=c(0.2,0.25),asp=0.1)+
  tm_credits("(c) ref and ref", position=c(0.0,0.0))

t=tmap_arrange(tm1,legend)
t

# loading the damage assessment data collected from different sources/NGOs
# point based geolocated damages data 

# "The Volunteer Circle" NGO data
# ref: https://openmaplebanon.org/open-data 

volun_circ_csv <- read_csv("C:/Users/SaraMoatti/Desktop/UCL/Year 2/GIS/Assignment/DBScan_Clustering_Attempt/data/OML Consolidated Public Sheet - TheVolunteerCircle.csv",      
                           locale = locale(encoding = "UTF-8"),
                           na = "NA")

# select relevant variables and clean data

volun_circ_csv_contains<-volun_circ_csv %>% 
  dplyr::select(contains("Building"), 
                contains("Lat"),
                contains("Long"),
                contains("Zone"),
                contains("Neighborhood")) %>% 
  clean_names()

# rename columns names as they are causing problem with the arabic font

volun_circ_csv_contains <- volun_circ_csv_contains %>%
  dplyr::rename(building_exterior_assessment=1,neighbrhood=5)
  

# remove NA values

volun_circ_new <- na.omit(volun_circ_csv_contains)

# "Rebuild Beirut" NGO data
# ref: https://openmaplebanon.org/open-data

Reb_Bei_csv <- read_csv("C:/Users/SaraMoatti/Desktop/UCL/Year 2/GIS/Assignment/DBScan_Clustering_Attempt/data/OML Consolidated Public Sheet - RebuildBeirut.csv",      
                        locale = locale(encoding = "UTF-8"),
                        na = "NA")

# select relevant variables and clean data

Reb_Bei_csv_contains<-Reb_Bei_csv %>% 
  dplyr::select(contains("Lat"),
                contains("Long")) %>% 
  clean_names()

Reb_Bei_new <- na.omit(Reb_Bei_csv_contains)

#the dataset was dropped as it is not fully geolocated

# "Nusaned" NGO data
# ref: https://openmaplebanon.org/open-data

nusanad_csv <- read_csv("C:/Users/SaraMoatti/Desktop/UCL/Year 2/GIS/Assignment/DBScan_Clustering_Attempt/data/OML Consolidated Public Sheet - nusanad.csv",      
                        locale = locale(encoding = "UTF-8"),
                        na = "NA")

# select relevant variables and clean data

nusanad_csv_contains<-nusanad_csv %>% 
  dplyr::select(contains("Lat"),
                contains("Long"),
                contains("Damage Level")) %>% 
  clean_names()

# remove NA values

nusanad_new <- na.omit(nusanad_csv_contains)

#the dataset was dropped as the majority is not geolocated

# "MySay" NGO data
# ref: https://openmaplebanon.org/open-data

mysay_csv <- read_csv("C:/Users/SaraMoatti/Desktop/UCL/Year 2/GIS/Assignment/DBScan_Clustering_Attempt/data/OML Consolidated Public Sheet - mysay.csv",      
                      locale = locale(encoding = "UTF-8"),
                      na = "NA")

# select relevant variables and clean data

mysay_csv_contains<-mysay_csv %>% 
  dplyr::select(contains("Lat"),
                contains("Long"),
                contains("House damage")) %>% 
  clean_names()

# remove NA values

mysay_new <- na.omit(mysay_csv_contains)

# clean the data: extract the damage points only

mysay_new <- mysay_new %>% 
  filter(`house_damage`=="Total destruction" | `house_damage`=="Some damage" )

# "FrontLineEngineers" NGO data
# ref: https://openmaplebanon.org/open-data

FrontLineEngineers_csv <- read_csv("C:/Users/SaraMoatti/Desktop/UCL/Year 2/GIS/Assignment/DBScan_Clustering_Attempt/data/OML Consolidated Public Sheet - FrontLineEngineers.csv",      
                                   locale = locale(encoding = "UTF-8"),
                                   na = "NA")

# select relevant variables and clean data

FrontLineEngineers_csv_contains<-FrontLineEngineers_csv %>% 
  dplyr::select("Lat",
                "Long",
                "Building condition") %>% 
  clean_names()

FrontLineEngineers_new <- na.omit(FrontLineEngineers_csv_contains)

# clean the data: exclude the non damage points

FrontLineEngineers_new  <- FrontLineEngineers_new  %>% 
  filter(`building_condition`!="No Damage, No evacuation" )

#remove rows with empty building condition that were not dropped as NA

FrontLineEngineers_new<-
  FrontLineEngineers_new[FrontLineEngineers_new$'building_condition' != "", ]

# "BebWShebek" NGO data
# ref: https://openmaplebanon.org/open-data

BebWShebek_csv <- read_csv("C:/Users/SaraMoatti/Desktop/UCL/Year 2/GIS/Assignment/DBScan_Clustering_Attempt/data/OML Consolidated Public Sheet - BebWShebek.csv",      
                           locale = locale(encoding = "UTF-8"),
                           na = "NA")

#select relevant variables - there was no other useful metadata
BebWShebek_csv_contains<-BebWShebek_csv %>% 
  dplyr::select("Lat",
                "Long") %>% 
  clean_names()

BebWShebek_new <- na.omit(BebWShebek_csv_contains)

# "Basecamp" NGO data
# ref: https://openmaplebanon.org/open-data

Basecamp_csv <- read_csv("C:/Users/SaraMoatti/Desktop/UCL/Year 2/GIS/Assignment/DBScan_Clustering_Attempt/data/OML Consolidated Public Sheet - Basecamp.csv",      
                         locale = locale(encoding = "UTF-8"),
                         na = "NA")

# resolve data types issues
Basecamp_csv <- Basecamp_csv %>% 
  mutate_at(c("Lat","Long"), as.double)


Basecamp_csv_contains<-Basecamp_csv %>% 
  dplyr::select("Lat",
                "Long") %>% 
  clean_names()

Basecamp_new <- na.omit(Basecamp_csv_contains)

#"lebaneseRedCross" NGO data
# ref: https://openmaplebanon.org/open-data

leb_red_cross_csv <- read_csv("C:/Users/SaraMoatti/Desktop/UCL/Year 2/GIS/Assignment/DBScan_Clustering_Attempt/data/OML Consolidated Public Sheet - LebaneseRedCross.csv",      
                              locale = locale(encoding = "UTF-8"),
                              na = "NA")

# Select relevant variables
# geolocation method might be a problem in this dataset
# damages are geolocated/grouped by zones midpoint coordinates not individual coordinates


leb_red_cross_csv_contains<-leb_red_cross_csv %>% 
  dplyr::select(contains("Lat"),
                contains("Long"),
                contains("Zone"),
                contains("OML UID")) %>% 
  clean_names()

leb_red_cross_UID <- na.omit(leb_red_cross_csv_contains)

leb_red_cross_new1 <- leb_red_cross_UID %>%
  group_by(zone)%>%
  summarise(count=n())

# plot the datasets to verify
# transform the dataframe to sf to prepare it for plotting

volun_circ_spatial<- volun_circ_new %>%
  st_as_sf(., coords = c("long", "lat"), 
           crs = 4326) %>%
  st_transform(., 22770) %>% 
  .[Bei_Exp_Zones,]

mysay_spatial<- mysay_new %>% 
  st_as_sf(., coords = c("long", "lat"), 
           crs = 4326) %>%
  st_transform(., 22770)%>% 
  .[Bei_Exp_Zones,]

FrontLineEngineers_spatial<- FrontLineEngineers_new %>%
  st_as_sf(., coords = c("long", "lat"), 
           crs = 4326) %>%
  st_transform(., 22770)%>% 
  .[Bei_Exp_Zones,]

BebWShebek_spatial<- BebWShebek_new %>%
  st_as_sf(., coords = c("long", "lat"), 
           crs = 4326) %>%
  st_transform(., 22770)%>% 
  .[Bei_Exp_Zones,]

Basecamp_spatial <- Basecamp_new %>%
  st_as_sf(., coords = c("long", "lat"), 
           crs = 4326) %>%
  st_transform(., 22770)%>% 
  .[Bei_Exp_Zones,]

tmap_mode("view")

t1=tm_shape(Bei_Exp_Zones) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(volun_circ_spatial) +
  tm_dots(col = "blue") 

t2=tm_shape(Bei_Exp_Zones) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(mysay_spatial) +
  tm_dots(col = "yellow") 

t3= tm_shape(Bei_Exp_Zones) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(FrontLineEngineers_spatial) +
  tm_dots(col = "red") 

t4= tm_shape(Bei_Exp_Zones) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BebWShebek_spatial) +
  tm_dots(col = "green") 

t5= tm_shape(Bei_Exp_Zones) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(Basecamp_spatial) +
  tm_dots(col = "violet") 

#check how to add legend
#check how to make them overlap without covering each

t1+t2+t3+t4+t5

# let's combine the datasets excluding the lebanese red cross dataset

combined_datasets = bind_rows(volun_circ_new,
                            mysay_new,
                            FrontLineEngineers_new,
                            BebWShebek_new,
                            Basecamp_new) 

combined_datasets_spatial<- combined_datasets %>%
  st_as_sf(., coords = c("long", "lat"), 
           crs = 4326) %>%
  st_transform(., 22770) %>% 
  .[Bei_Exp_Zones,]
  
tm_shape(Bei_Exp_Zones) +
  tm_polygons(col = NA, alpha = 0.2) +
  tm_shape(combined_datasets_spatial) +
  tm_dots(col = "violet")

# Density Map 

points_sf_joined <- Bei_Exp_Zones%>%
  st_join(combined_datasets_spatial)%>%
  add_count(zone_numbe)%>%
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per ward
  mutate(density_obs=n/area*1000)%>%
  #select density and some other variables 
  dplyr::select(density_obs, zone_numbe, cadaster_1, n)


points_sf_joined<- points_sf_joined %>%                    
  group_by(zone_numbe) %>%         
  summarise(density_obs = first(density_obs),
            
            zone_names= first(cadaster_1),
            damagecount= first(n))

tm_shape(points_sf_joined) +
  tm_polygons("density_obs",
              style="jenks",
              palette="PuOr",
              midpoint=0,
              popup.vars=c("zone_names", "density_obs"),
              title="Damage Reported Density")


# perform a dbscan on the dataset

combined_datasets_spatial_sub <- combined_datasets_spatial[Bei_Exp_Zones,]

# transform sf to sp
combined_datasets_spatial_sub <- combined_datasets_spatial_sub  %>%
  as(., 'Spatial')


window <- as.owin(Bei_Exp_Zones)
plot(window)

combined_datasets_spatial_sub.ppp <- ppp(x=combined_datasets_spatial_sub@coords[,1],
                                y=combined_datasets_spatial_sub@coords[,2],
                                window=window)

combined_datasets_spatial_sub@coords[,1]

combined_datasets_spatial_sub.ppp %>%
  density(., sigma=100) %>%
  plot(.,pch=16,cex=0.5,
       main="Reported Damages")

# Ripley's k analysis 

K <- combined_datasets_spatial_sub.ppp %>%
  Kest(., correction="border") %>%
  plot(col = c("black"))


## DBSCAN

combined_datasets_spatial_sub_points <- combined_datasets_spatial_sub %>%
  coordinates(.)%>%
  as.data.frame()

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

#now plot, setting the coordinates to scale correctly and as a black and white plot 
#(just for the hell of it)...
dbplot + theme_bw() + coord_equal()


#add a basemap
#get the bbox in lat long for Harrow
BeirutWGSbb <- Bei_Exp_Zones%>%
  st_transform(., 4326)%>%
  st_bbox()

BeirutWGSbb

library(OpenStreetMap)

basemap <- OpenStreetMap::openmap(c(33.86149,35.49469),c(33.91946,35.55270),
                                  zoom=NULL,
                                  "stamen-toner")
plot.OpenStreetMap(basemap) 

# convert the basemap to local Grid
basemap_bng <- openproj(basemap, projection="+init=epsg:22770")

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

