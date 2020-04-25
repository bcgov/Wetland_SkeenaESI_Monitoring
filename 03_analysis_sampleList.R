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

#Assemble wetland data with each of the strata as a single field
# bec_pts - wet_id, BECgroup2
bec_pts<-read_xlsx(file.path(dataOutDir,paste('bec_pts.xlsx',sep='')))
# WetFlow - wet_id, FlowCode
WetFlow <- read_xlsx(file.path(dataOutDir,paste('WetFlow.xlsx',sep='')))

#Join bec and flow data back to wetlands
SampleStrata<-Wetlands %>%
  #st_drop_geometry() %>%
  left_join(WetFlow, by='Wetland_Co') %>%
  left_join(bec_pts, by='Wetland_Co') %>%
  #Drop any wetlands that are NA for BEC - 6 cases for some reason
  dplyr::filter(!is.na(BEC)) %>%
  dplyr::select(Wetland_Co, kmRd, Dist_to_Road, BEC, FlowCode)
#Data check
#tt<-subset(SampleStrata, is.na(BEC))

#Check data by re-connecting to wetlands and inspecting
write_sf(SampleStrata, file.path(spatialOutDir,"SampleStrata.gpkg"))

#dplyr approach to calculating how many wetlands to do in each strata
Wet_dt<-data.frame(Wetland_Co=SampleStrata$Wetland_Co, BEC=SampleStrata$BEC,
                   FlowCode=SampleStrata$FlowCode, NearRd=SampleStrata$kmRd)

#Calculate the number of groups, based on all the BEC flow combinations
NSampGroups<- Wet_dt %>%
  group_by(BEC,FlowCode) %>%
  n_groups() #6

#Initial approach was to sample according to #wetlands in each strata
#Changed to sample evenly in each strata
#To get 69 samples - sample a fraction of each of the n_groups (31) + ensure list of at
#First calculate how many samples required for each strata
#sampRequired <- Wet_dt %>%
  #mutate(Group=group_indices(.,BEC,FlowCode)) %>%
#  group_by(BEC,FlowCode) %>%
  #sample_n(15)
#  sample_frac(.00185) %>%
#  group_by(BEC, FlowCode) %>%
#  dplyr::summarise(nToSample=n())
#sum(sampRequired$nToSample)

#Revised approach sampling evenly across strata
sampRequired <- Wet_dt %>%
  #mutate(Group=group_indices(.,BEC,FlowCode)) %>%
  group_by(BEC, FlowCode) %>%
  dplyr::summarise(nToSample=round(100/NSampGroups,0))
sum(sampRequired$nToSample)

#Second need at least twice the sample size to accomodate unaccessible sites.
#Initial method
#samp2020 <- Wet_dt %>%
#  mutate(Group=group_indices(.,BEC,FlowCode)) %>%
#  group_by(BEC,FlowCode) %>%
#  sample_frac(.006) %>%
#  dplyr::filter(NearRd==1) #generates 158 cases

#Identify 2xs as needed in each strata
samp2020 <- Wet_dt %>%
  mutate(Group=group_indices(.,BEC,FlowCode)) %>%
  dplyr::filter(NearRd==1) %>%
  group_by(BEC,FlowCode) %>%
  sample_n(sampRequired$nToSample)

#map them to see where they fall
WetSamples<- Wetlands %>%
  right_join(samp2020) %>%
  st_as_sf()
write_sf(WetSamples, file.path(spatialOutDir,"WetSamples.gpkg"))

#pull in sites already sampled
#Read in cleaned wetland plot data
site_data <- read_excel(file.path(dataOutDir,'WetPlots.xlsx'), 1)
plot_data <- read_excel(file.path(dataOutDir,'WetPlots.xlsx'), 2)
veg_data <- read_excel(file.path(dataOutDir,'WetPlots.xlsx'), 4)

#Join wetland spatial with plot data to spatially identify wetlands
#some issues with GPS locations in field card missing or not neccessarily correct UTM issues, miss record
#Pull only wetlands that have plots
wet_site<- SampleStrata %>%
  st_drop_geometry() %>%
  mutate(newid = Wetland_Co) %>%
  inner_join(site_data) %>%
  mutate(sampled=1) %>%
  dplyr::select(Wetland_Co, sampled, BEC, FlowCode)

#Calculate number already sampled in each strata
Grp_wet_site <- wet_site %>%
  group_by(BEC, FlowCode) %>%
  dplyr::summarise(nSampled=n())

#Summarise how much to sample in each strata - knowing some will be dropped
Grp_samp <- samp2020 %>%
  group_by(BEC, FlowCode) %>%
  dplyr::summarise(nToOverSample=n())

#Join with sampled to see how many 'left' to sample - some units sampled to many times
#according to strata
Grp_sampJ <- Grp_samp %>%
  full_join(Grp_wet_site) %>%
  mutate(nSampled = replace_na(nSampled, 0)) %>%
  mutate(sampOver2020=nToOverSample-nSampled)

#Calculate number of wetlands in each group and number to sample in 2020
nWetsPerGroups<- Wet_dt %>%
  mutate(Group=group_indices(.,BEC,FlowCode)) %>%
  group_by(BEC,FlowCode) %>%
  dplyr::summarise(nWet=n()) %>%
  left_join(Grp_sampJ, by=c('BEC','FlowCode')) %>%
  left_join(sampRequired, by=c('BEC','FlowCode')) %>%
  mutate(nToSample = replace_na(nToSample, 0)) %>%
  mutate(nSampled = replace_na(nSampled, 0)) %>%
  mutate(nToSample2020=nToSample-nSampled) %>%
  dplyr::select(BEC, FlowCode, nWetlands=nWet, nToSample, nSampled2019=nSampled, nToSample2020)

#Data checking
sum(nWetsPerGroups$nToSample,na.rm=TRUE)
sum(nWetsPerGroups$nToSample2020,na.rm=TRUE)
sum(nWetsPerGroups$nSampled2019,na.rm=TRUE)
nWetsPerGroups[is.na(nWetsPerGroups)] <- 0

WriteXLS(nWetsPerGroups,file.path(dataOutDir,paste('nWetsPerGroups.xlsx',sep='')),SheetNames='nWetsPerGroups')


########
#Explore approaches to generating lists of wetlands for sampling based on the 4 strata
# 1 use the splitstackshape pkg's stratified function
library(splitstackshape)
#Do a draw for an excess of sites so a list
#or do only samples required, then a second tier draw to pick up back up sites and those
#far from roads
#samples .0015 from each group to generate approximately 71 sites total
samp<-stratified(Wet_dt, c("BEC","FlowCode"),.0015)
#samples small percentage of each combination, however smaller groups dont get sampled
#may want to sample across each strata combination?
#may want to simplify strata so have more samples/strata
#how many groups?
#map them to see where they fall
WetSamples<- Wetlands %>%
  right_join(samp) %>%
  st_as_sf()
write_sf(WetSamples, file.path(spatialOutDir,"WetSamples.gpkg"))

