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

#read in FREP blocks
FREPblocks<-readRDS(file='tmp/AOI/FREPblocks')

#identify which FREP blocks are adjacent to wetlands, first buffer FREP blocks
FREPbuf<- st_buffer(FREPblocks, 100)

#Read in clean wetlands
Wetlands<-readRDS(file = 'tmp/AOI/Wetlands2')
wetptW<-st_read(file.path(spatialOutDir,"waterpt.gpkg"))

#Point in polygon to assign FREP block to wetland
FREPbuf_pts <-st_intersection(wetptW, FREPbuf) %>%
  st_drop_geometry() %>%
  dplyr::select(Wetland_Co,OPENING_ID, DISTRICT_NAME, OPENING_GROSS_AREA, DISTURBANCE_END_DATE)
length(unique(FREPbuf_pts$Wetland_Co)) #429
length(unique(FREPbuf_pts$OPENING_ID)) #185

FREP_pts <-st_intersection(wetptW, FREPblocks) %>%
  st_drop_geometry() %>%
  dplyr::select(Wetland_Co,FREP_OPENING_ID=OPENING_ID, FREP_DISTRICT_NAME=DISTRICT_NAME, FREP_OPENING_GROSS_AREA=OPENING_GROSS_AREA, FREP_DISTURBANCE_END_DATE=DISTURBANCE_END_DATE)
length(unique(FREP_pts$Wetland_Co))#117
length(unique(FREP_pts$FREP_OPENING_ID)) #63

#Join point coverage back to main wetland file
Wetlands3<-Wetlands %>%
  #st_drop_geometry %>%
  left_join(FREP_pts, by='Wetland_Co')

#Set NULL to 0
Wetlands3[is.na(Wetlands3)] <- 0

saveRDS(Wetlands3, file = 'tmp/AOI/Wetlands3')
saveRDS(Wetlands3, file = 'tmp/AOI/Wetlands')
#write to geo package to check
write_sf(Wetlands3, file.path(spatialOutDir,"Wetlands3.gpkg"))

