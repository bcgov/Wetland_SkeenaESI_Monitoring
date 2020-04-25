# Copyright 2019 Province of British Columbia
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


source("header.R")
ESI<-readRDS(file = 'tmp/ESI')
AOI<-ESI

#Make map of lakes+rivers
#waterbodies<-readRDS(file = 'tmp/AOI/waterbodies')  %>%
#  st_intersection(AOI)
#saveRDS(waterbodies, file = 'tmp/AOI/waterbodies')
waterbodies<-readRDS(file = 'tmp/AOI/waterbodies')
#write_sf(waterbodies, file.path(spatialOutDir,"waterbodies.gpkg"))
waterbodiesB <- st_buffer(waterbodies, 100)

#Rivers<-readRDS(file = 'tmp/AOI/Rivers') %>%
#st_intersection(AOI)
#saveRDS(Rivers, file = 'tmp/AOI/Rivers')
#write_sf(Rivers, file.path(spatialOutDir,"Rivers.gpkg"))
Rivers<-readRDS(file = 'tmp/AOI/Rivers')
RiversB <- st_buffer(Rivers, 100)

#union seems to have a memory leak in R studio - alterative method used
#LakesRivers <- st_union(waterbodiesB, RiversB)
#write_sf(LakesRivers, file.path(spatialOutDir,"LakesRivers.gpkg"))
#LakesRiversB <- st_buffer(LakesRivers, 100)

#Wetlands<-read_sf(file.path(spatialOutDir,"wetlandroad.gpkg"))
Wetlands<-readRDS(file = 'tmp/AOI/Wetlands')
#wet_id wasnt joining properly so will define after read
Wetlands <- Wetlands %>%
  mutate(wet_id2=as.numeric(rownames(Wetlands)))

LakeNearBy<-waterbodiesB %>%
  #st_sf() %>%
  #st_intersection(Wetlands)
  st_intersects(Wetlands) %>%
  data.frame()
colnames(LakeNearBy)<-c('Water','wet_id2')
LakeNearBy <- LakeNearBy %>%
  mutate(Water=ifelse(Water>0, 1, 0))

RiverNearBy<-RiversB %>%
  #st_sf() %>%
  #st_intersection(Wetlands)
  st_intersects(Wetlands) %>%
  data.frame()
colnames(RiverNearBy)<-c('Water','wet_id2')
RiverNearBy <- RiverNearBy %>%
  mutate(Water=ifelse(Water>0, 1, 0))

#join the 2 data frames with 1 record per wetland
WaterNearBy <- LakeNearBy %>%
  left_join(RiverNearBy, by='wet_id2') %>%
  group_by(wet_id2) %>%
  dplyr::summarise(nWaterNear=n()) %>%
  mutate(Water=ifelse(nWaterNear>0, 1, 0))

WetlandsN<-Wetlands %>%
  left_join(WaterNearBy, by='wet_id2')
WetlandsN$Water[is.na(WetlandsN$Water)]<-0

write_sf(WetlandsN, file.path(spatialOutDir,"WetlandsN.gpkg"))
WetlandsN<-st_read(file.path(spatialOutDir,"WetlandsN.gpkg"))
#Organize flow related variables for classifying wetlands
WetFlow<-WetlandsN %>%
  #st_drop_geometry() %>%
  dplyr::select("wet_id", "Wetland_Co","lake_intersect", "mmwb_intersect", "river_intersect",
                "split_by_stream", "stream_end", "stream_start", "mean_slope_pct",
                "max_stream_order", "Verticalflow", "Bidirectional", "Throughflow",
                "Outflow", "Inflow", "Water") %>%
  mutate(ConnectedWet=ifelse(Throughflow=="Yes" | Bidirectional=="Yes", 2, 0)) %>%
  mutate(UnconnectedWet=ifelse(Verticalflow=="Yes" | Outflow=="Yes" | Inflow=="Yes",1, 0)) %>%
  mutate(AdjacentWaterWet=ifelse(Water>0, 2, 0)) %>%
  #mutate(AdjacentWaterWet=ifelse(Water>0, 3, 0)) %>%
  mutate(FlowCode=pmax(ConnectedWet,UnconnectedWet,AdjacentWaterWet, na.rm=FALSE))
#if 0 ie no cases from connected, unconnected or adjacent water then assign to unconnected
WetFlow$FlowCode[WetFlow$FlowCode==0]<-1

#unique(WetFlow$FlowCode)
write_sf(WetFlow, file.path(spatialOutDir,"WetFlow.gpkg"))


#evaluate prevelance of each category
# Generate a breakdown of how many and % wetlands in each flow group
flow.site <- WetFlow %>%
  st_drop_geometry() %>%
  group_by(FlowCode)%>%
  dplyr::summarise(no.pts = n()) %>%
  mutate(perc = ceiling(no.pts / sum(no.pts)*100))

WriteXLS(st_drop_geometry(WetFlow),file.path(dataOutDir,paste('WetFlow.xlsx',sep='')))
WriteXLS(flow.site,file.path(dataOutDir,paste('ESIFlowxWetland.xlsx',sep='')))

