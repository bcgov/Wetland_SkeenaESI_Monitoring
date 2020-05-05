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

# Analysis generates a Land Type strata
# first, it modifies forested portion and designates as conifer, mixed, deciduous/shrub
# second, first it uses the LandCoverAge coverage and nibbles into water/wetland features and
# assigns to largest neighbourhood

source ('header.R')
ESI<-readRDS(file = 'tmp/ESI')

AOI<-ESI

#Load admin units - Gitanyow Wilps for now
ESI_Gitanyow <-readRDS(file='tmp/ESI_Gitanyow')
waterpt<-st_read(file.path(spatialOutDir,"waterpt.gpkg"))

#join to wetlands
Git_pts <- st_intersection(waterpt, ESI_Gitanyow) %>%
  #write_sf(bec_pts, file.path(spatialOutDir,"bec_pts.gpkg"))
  st_drop_geometry() %>%
  dplyr::select(Wetland_Co, House_Name)

WriteXLS(Git_pts,file.path(dataOutDir,paste('Git_pts.xlsx',sep='')))




gc()
