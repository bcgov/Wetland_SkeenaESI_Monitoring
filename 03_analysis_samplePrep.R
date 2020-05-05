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

source ('header.R')

Wetlands<-readRDS(file = 'tmp/AOI/Wetlands')

#Assemble wetland data with each of the strata as a single field
# bec_pts - wet_id, BECgroup2
bec_pts<-read_xlsx(file.path(dataOutDir,paste('bec_pts.xlsx',sep='')))
# WetFlow - wet_id, FlowCode
WetFlow <- read_xlsx(file.path(dataOutDir,paste('WetFlow.xlsx',sep='')))
# WetFlow - wet_id, FlowCode
LandT <- read_xlsx(file.path(dataOutDir,paste('ltls.xlsx',sep='')))
# WetFlow - wet_id, FlowCode
Disturb <- read_xlsx(file.path(dataOutDir,paste('disturb.ls.xlsx',sep='')))
#Gitanyow
Gitanyow <- read_xlsx(file.path(dataOutDir,paste('Git_pts.xlsx',sep='')))

#Join strata and select criteria attributes data back to wetlands
SampleStrata<-Wetlands %>%
  st_drop_geometry() %>%
  left_join(Gitanyow, by='Wetland_Co') %>%
  mutate(House_Name = replace_na(House_Name, 'Non-Gitanyow')) %>%
  left_join(WetFlow, by='Wetland_Co') %>%
  left_join(bec_pts, by='Wetland_Co') %>%
  left_join(LandT, by='Wetland_Co') %>%
  left_join(Disturb, by='Wetland_Co') %>%
  mutate(StrataGroup=group_indices(.,BEC,FlowCode)) %>%
  #Drop any wetlands that are NA for BEC - 6 cases for some reason
  dplyr::filter(!is.na(BEC)) %>%
  #Drop Landcover NAs - 64 cases? all wetlands should be assigned properly? need to check
  dplyr::filter(!is.na(LanCoverLabel)) %>%
  mutate(Sampled=0) %>%
  dplyr::select(Wetland_Co, Sampled, kmRd, StrataGroup, House_Name, Dist_to_Road, BEC,
                FlowCode, Verticalflow=Verticalflow.y, Bidirectional=Bidirectional.y,
                Throughflow=Throughflow.y, Outflow=Outflow.y, Inflow=Inflow.y,
                LanCoverLabel, DisturbType)

saveRDS(SampleStrata, file = 'tmp/AOI/SampleStrata')

#Load in sits previously surveyed
#Read in cleaned wetland plot data
site_data <- read_excel(file.path(dataOutDir,'WetPlots.xlsx'), 1)
plot_data <- read_excel(file.path(dataOutDir,'WetPlots.xlsx'), 2)
veg_data <- read_excel(file.path(dataOutDir,'WetPlots.xlsx'), 4)

#Join wetland spatial with plot data to spatially identify wetlands
#some issues with GPS locations in field card missing or not neccessarily correct UTM issues, miss record
#Pull only wetlands that have plots
wet_site2019<- SampleStrata %>%
  mutate(newid = Wetland_Co) %>%
  inner_join(site_data) %>%
  mutate(Sampled=1) %>%
  dplyr::select(Wetland_Co, Sampled, kmRd, StrataGroup, House_Name, Dist_to_Road, BEC,
                FlowCode, Verticalflow, Bidirectional,Throughflow, Outflow, Inflow,
                LanCoverLabel, DisturbType)
saveRDS(wet_site2019, file = 'tmp/AOI/wet_site2019')

StrataGroup<-SampleStrata %>%
  group_by(StrataGroup, BEC, FlowCode) %>%
  dplyr::summarise(nWetlands=n())

saveRDS(StrataGroup, file = 'tmp/AOI/StrataGroup')

