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

#read in CGL points
CGLwetsD<-read_excel(file.path(CGLDir,'List of Unsurveyed Wetlands in Wetsuweten territory by CGL.xlsx')) %>%
  mutate(CGLstart=substr(gsub("[^0-9.]", "",  KP_Start),1,3))
CGLpoints <-readRDS(file='tmp/AOI/CGLpoints') %>%
  mutate(CGLstart=substr(gsub("[^0-9.]", "",  Name),1,3)) %>%
  dplyr::filter(CGLstart %in% unique(CGLwetsD$CGLstart))

CGLbuf<-CGLpoints %>%
  st_buffer(500)

#Read in clean wetlands
Wetlands3<-readRDS(file = 'tmp/AOI/Wetlands3')

#Point in polygon to assign FREP block to wetland
CGLwetsS <-st_intersection(Wetlands3, CGLbuf) %>%
  st_drop_geometry() %>%
  dplyr::select(Wetland_Co)

CGLwets<-Wetlands3 %>%
  dplyr::filter(Wetland_Co %in% CGLwetsS$Wetland_Co)

saveRDS(CGLwets, file = 'tmp/AOI/CGLwets')
#write to geo package to check
write_sf(CGLwets, file.path(spatialOutDir,"CGLwets.gpkg"))


