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

#read in watersheds from Watershed_SkeenaESI_Monitoring project
#identify which watersheds have been selected for sampling
Watersheds<-st_read(file.path('../Watershed_SkeenaESI_Monitoring/out/spatial','WSamplePoolS2.gpkg')) %>%
  mutate(WatershedID=as.character(WATERSHED_FEATURE_ID)) %>%
  dplyr::select(WatershedID, SampleType)

#Read in clean wetlands
WetlandsW<-readRDS(file = 'tmp/AOI/Wetlands1')
wetptW<-st_read(file.path(spatialOutDir,"waterpt.gpkg"))

#Point in polygon to assign watershed to wetland
Wshd_pts <-st_intersection(wetptW, Watersheds) %>%
  st_drop_geometry() %>%
  dplyr::select('Wetland_Co', WatershedID, Wshd_Sample_Type=SampleType)

#Join point coverage back to main wetland file
Wetlands2<-WetlandsW %>%
  #st_drop_geometry %>%
  left_join(Wshd_pts, by='Wetland_Co')

#Set NULL to 0
Wetlands2[is.na(Wetlands2)] <- 0

saveRDS(Wetlands2, file = 'tmp/AOI/Wetlands2')
#write to geo package to check
write_sf(Wetlands2, file.path(spatialOutDir,"Wetlands2.gpkg"))

