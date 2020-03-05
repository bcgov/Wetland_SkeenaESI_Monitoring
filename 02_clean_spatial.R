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
AOI <- ws %>%
  filter(SUB_SUB_DRAINAGE_AREA_NAME == "Bulkley")

raster(file.path('/Users/darkbabine/Dropbox (BVRC)/_dev/Bears/GB_Data/data/Landform',"LForm.tif")) %>%
  mask(ESI)
saveRDS(LForm, file = 'tmp/LForm')

vri <- readRDS(file = 'tmp/vri') %>%
  st_buffer(0) %>%
  vri2<-vri %>%
  st_intersection(AOI)

ws <- readRDS(file = 'tmp/ws') %>%
  st_intersection(AOI)

Wetlands <- readRDS(file = 'tmp/Wetlands') %>%
  st_intersection(AOI)

waterbodies <- readRDS(file = 'tmp/waterbodies') %>%
  st_intersection(AOI)

Streams <- readRDS(file = 'tmp/Streams') %>%
  st_intersection(AOI)

bec_sf <- readRDS(file= 'tmp/bec_sf') %>%
  st_intersection(AOI)

ESI <- readRDS(file = 'tmp/ESI') %>%
  st_intersection(AOI)

roads_sfin <- readRDS(file = 'tmp/roads_sf') %>%
  st_intersection(AOI)

#Surface missing for non-DRA roads - pulling BCGW_SOURCE to identify non-DRA
roads_sf <- roads_sfin %>%
  mutate(RoadClass = ifelse(is.na(ROAD_CLASS), BCGW_SOURCE, ROAD_CLASS)) %>%
  mutate(SurfType = ifelse(is.na(ROAD_SURFACE), BCGW_SOURCE, ROAD_SURFACE))

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
Surface_LUT<-data.frame(
          SurfType=c("loose", "overgrown", "paved", "rough", "unknown",
          "WHSE_BASEMAPPING.TRIM_TRANSPORTATION_LINES", "WHSE_FOREST_TENURE.ABR_ROAD_SECTION_LINE",
         "WHSE_FOREST_TENURE.FTEN_ROAD_SECTION_LINES_SVW", "WHSE_FOREST_VEGETATION.RSLT_FOREST_COVER_INV_SVW",
         "WHSE_MINERAL_TENURE.OG_PETRLM_ACCESS_ROADS_PUB_SP"),
         Surface=c("loose", "overgrown", "paved", "rough", "unknown", "loose",
                   "loose", "loose", "loose", "loose"))
roads_sf <- roads_sf %>%
  left_join(Surface_LUT)

LForm <-   raster(file.path('/Users/darkbabine/Dropbox (BVRC)/_dev/Bears/GB_Data/data/Landform',"LForm.tif")) %>%
  mask(AOI)

LForm_LUT <- readRDS(file= 'tmp/LForm_LUT')

LandCover <- raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','LandCover.tif')) %>%
  mask(AOI)
LandCover_LUT <- readRDS(file= 'tmp/LandCover_LUT')

Age <- raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','Age.tif')) %>%
  mask(AOI)

ESI_DEM <- readRDS(file = 'tmp/ESI_DEM')
ESI_OW <-readRDS(file='tmp/ESI_OW') %>%
  st_intersection(AOI)
ESI_LBN <-readRDS(file='tmp/ESI_LBN') %>%
  st_intersection(AOI)
ESI_Gitxsan <-readRDS(file='tmp/ESI_Gitxsan') %>%
  st_intersection(AOI)
ESI_Gitanyow <-readRDS(file='tmp/ESI_Gitanyow') %>%
  st_intersection(AOI)

