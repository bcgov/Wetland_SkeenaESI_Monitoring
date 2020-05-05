# Copyright 2020 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

source('header.R')

#Subset data to make exploration easier
ws_AOI <- readRDS(file = 'tmp/ws') %>%
  filter(SUB_SUB_DRAINAGE_AREA_NAME == "Bulkley")
#or use full ESI area
ESI<-readRDS(file = 'tmp/ESI')
#select AOI
AOI<-ESI
#Clip ESI boundary
#ESI <- ESI %>%
#  st_intersection(AOI)

#will clip wetlands used by AOI and by a 500m buffer around roads - since need to be physically accessible.
#Read in ESI road layer with updated Kispiox roads
RoadKisp <- readRDS(file = 'tmp/RoadKisp') %>%
  st_intersection(AOI)
#roads_sfin2 <- readRDS(file = 'tmp/roads_sf') %>%
#  st_intersection(AOI)
roads_sfin<-RoadKisp

#ROAD_CLASS and ROAD_SURFACE read in as factors convert to character
roads_sfin$ROAD_CLASS <- as.character(roads_sfin$ROAD_CLASS)
roads_sfin$ROAD_SURFACE <- as.character(roads_sfin$ROAD_CLASS)

#Surface missing for non-DRA roads - pulling BCGW_SOURCE to identify non-DRA
roads_sf <- roads_sfin %>%
  mutate(RoadClass = ifelse(is.na(ROAD_CLASS), Road_Class_Descr, ROAD_CLASS)) %>%
  mutate(RoadClass = ifelse(is.na(RoadClass), BCGW_SOURCE, RoadClass)) %>%
  mutate(SurfType = ifelse(ROAD_SURFACE %in% c(NA, '',' ','   ') , CONDITION, ROAD_SURFACE)) %>%
  mutate(SurfType = ifelse(is.na(SurfType) , BCGW_SOURCE, SurfType))

#Check Class and Surface Type
roads.class.sum <- roads_sf %>%
  st_drop_geometry() %>%
  group_by(RoadClass) %>%
  dplyr::summarise(count = n())
roads.class.sum

roads.surf.sum <- roads_sf %>%
  st_drop_geometry() %>%
  group_by(SurfType) %>%
  dplyr::summarise(count = n())
roads.surf.sum

#Surface missing for non-DRA roads - suggest loose since most are gravel roads
# eliminate all non-roads - first check what road class and types
unique(roads_sf$RoadClass)
unique(roads_sf$SurfType)

#get rid of roads that are impassible, trails, etc and overgrown or DIFFICULT
roads_keep <- roads_sf %>%
  filter(!RoadClass %in% c("IMPASSIBLE_RD","IMPASSABLE_RD","driveway","collector","ramp","restricted","trail",
                          "arterial","strata")) %>%
  filter(!SurfType %in% c("overgrown","IMPASSABLE","DIFFICULT"))

#st_geometry(roads_keep) <- "geometry"
saveRDS(roads_keep, file = 'tmp/AOI/roads_keep')
#roads_keep<-readRDS(file = 'tmp/AOI/roads_keep')

#1km buffer on roads to use for determing if a wetland can be sampled
#get small data set to test
#ws_AOI <- readRDS(file = 'tmp/ws') %>%
#  +     filter(SUB_SUB_DRAINAGE_AREA_NAME == "Bulkley")
roads_keep<-  roads_keep %>%
  st_intersection(AOI)

road_aoi <- st_buffer(roads_keep, dist = 500) %>%
  st_union() %>%
  st_sf() %>%
  mutate(kmRd=1)

saveRDS(road_aoi, file = 'tmp/AOI/road_aoi')
#road_aoi<-readRDS(file = 'tmp/AOI/road_aoi')

#Clip wetlands by AOI
#Need to set wet_id to link back to data after st_intersects
Wetlands <- readRDS(file = 'tmp/Wetlands') %>%
  st_buffer(0) %>%
  st_intersection(AOI)

Wetlands <- Wetlands %>%
  mutate(wet_id=as.numeric(rownames(Wetlands)))
saveRDS(Wetlands, file = 'tmp/AOI/Wetlands')
#Wetlands<-readRDS(file = 'tmp/AOI/Wetlands')

#wetlands close to road
wetlandRoad <-st_intersection(Wetlands, road_aoi)
wetlandRoad<- wetlandRoad %>%
  mutate(wet_id=as.numeric(rownames(wetlandRoad)))

write_sf(wetlandRoad, file.path(spatialOutDir,"wetlandRoad.gpkg"))
#wetlandRoad<-st_read(file.path(spatialOutDir,"wetlandRoad.gpkg"))
#all wetlands but with 1km buffer
wetRoadDat<-wetlandRoad %>%
  st_drop_geometry() %>%
  dplyr::select(kmRd, Wetland_Co)

Wetlands <- Wetlands %>%
  left_join(wetRoadDat)
Wetlands$kmRd[is.na(Wetlands$kmRd)]<-0
saveRDS(Wetlands, file = 'tmp/AOI/Wetlands')

#Generate an ESI Wetlands point coverage
wetlandsXY <- st_centroid(Wetlands)
wetpt <- st_coordinates(wetlandsXY)
wetpt <- wetlandsXY %>%
  cbind(wetpt) %>%
  st_drop_geometry()

waterpt <- st_as_sf(wetpt, coords= c("X","Y"), crs = 3005) %>%
  st_intersection(AOI)
waterpt <- waterpt %>%
  mutate(wet_id=as.numeric(rownames(waterpt)))
#st_crs(waterpt)<-3005
write_sf(waterpt, file.path(spatialOutDir,"waterpt.gpkg"))

#mapview(roads_sf)+mapview(road_aoi)+mapview(Wetlands)

vri <- readRDS(file = 'tmp/vri') %>%
  st_buffer(0) %>%
  st_intersection(AOI)
saveRDS(vri, file = 'tmp/AOI/vri')
vri<-readRDS(file = 'tmp/AOI/vri')
#ws <- readRDS(file = 'tmp/ws') %>%
#  st_intersection(AOI)

waterbodies <- readRDS(file = 'tmp/waterbodies') %>%
  st_intersection(AOI)
saveRDS(waterbodies, file = 'tmp/AOI/waterbodies')

#Streams <- readRDS(file = 'tmp/Streams') %>%
#  st_intersection(AOI)

Rivers <- readRDS(file = 'tmp/Rivers') %>%
  st_intersection(AOI)
saveRDS(Rivers, file = 'tmp/AOI/Rivers')

bec_sf <- readRDS(file= 'tmp/bec_sf') %>%
  st_intersection(AOI)
saveRDS(bec_sf, file = 'tmp/AOI/bec_sf')

LForm <-   raster(file.path('/Users/darkbabine/Dropbox (BVRC)/_dev/Bears/GB_Data/data/Landform',"LForm.tif")) %>%
  crop(AOI)

LForm_LUT <- readRDS(file= 'tmp/LForm_LUT')

LandCover <- raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','LandCover.tif')) %>%
  crop(AOI)
saveRDS(LandCover, file = 'tmp/AOI/LandCover')

LandCoverAndAge<-readRDS(file = 'tmp/LandCoverAndAge') %>%
  crop(ESI)
saveRDS(LandCoverAndAge, file = 'tmp/AOI/LandCoverAndAge')

Age <- raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','Age.tif')) %>%
  crop(AOI)
saveRDS(Age, file = 'tmp/AOI/Age')

LogYear<-
  raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','LogYear.tif')) %>%
  crop(AOI)
saveRDS(LogYear, file = 'tmp/AOI/LogYear')

DEM<-  raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','DEM.tif')) %>%
  crop(AOI)
saveRDS(DEM, file = 'tmp/AOI/DEM')

#Raster rail
RailRoads<-
  raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','RailRoads.tif')) %>%
  crop(AOI)
saveRDS(RailRoads, file = 'tmp/AOI/RailRoads')

#Raster pipeline
Pipe<-
  raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','Pipe_PrinceRupertGasTransmissionLtd.tif'))  %>%
  crop(AOI)
saveRDS(Pipe, file = 'tmp/AOI/Pipe')

#Raster HydroTransmission
HydroTransmission<-
  raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','HydroTransmission.tif')) %>%
  crop(AOI)
saveRDS(HydroTransmission, file = 'tmp/AOI/HydroTransmission')

#Raster roads
RoadType<-
  raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','roadType.tif')) %>%
  crop(AOI)
saveRDS(RoadType, file = 'tmp/AOI/RoadType')

RoadType_LUT <- read_csv(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/LUT','RoadType_LUT.csv'))

ExtensiveFootprint<-
  raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','ExtensiveFootprint.tif')) %>%
  crop(AOI)
saveRDS(ExtensiveFootprint, file = 'tmp/AOI/ExtensiveFootprint')

Wildfire_Historical<-readRDS('tmp/Wildfire_Historical') %>%
  st_intersection(AOI)
saveRDS(Wildfire_Historical, file = 'tmp/AOI/Wildfire_Historical')

Wildfire_2018<-readRDS('tmp/Wildfire_2018') %>%
  st_intersection(AOI)
saveRDS(Wildfire_2018, file = 'tmp/AOI/Wildfire_2018')

BurnSeverity_2017<-readRDS('tmp/BurnSeverity_2017') %>%
  st_buffer(0) %>%
  st_intersection(AOI)
saveRDS(BurnSeverity_2017, file = 'tmp/AOI/BurnSeverity_2017')

ESI_DEM <- readRDS(file = 'tmp/ESI_DEM')
ESI_OW <-readRDS(file='tmp/ESI_OW') %>%
  st_set_crs(3005) %>%
  st_intersection(AOI)
ESI_LBN <-readRDS(file='tmp/ESI_LBN') %>%
  st_set_crs(3005) %>%
  st_intersection(AOI)
ESI_Gitxsan <-readRDS(file='tmp/ESI_Gitxsan') %>%
  st_set_crs(3005) %>%
  st_intersection(AOI)
ESI_Gitanyow <-readRDS(file='tmp/ESI_Gitanyow') %>%
  st_set_crs(3005) %>%
  st_intersection(AOI)


gc()
