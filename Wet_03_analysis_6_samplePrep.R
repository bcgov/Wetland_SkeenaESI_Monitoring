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

#Generates SampleStrata file

source ('header.R')

Wetlands<-readRDS(file = 'tmp/AOI/Wetlands3')

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
#Gitxsan
GitxsanWshd_pts<- read_xlsx(file.path(dataOutDir,paste('GitxsanWshd_pts.xlsx',sep='')))
#OW
Wetsuweten_pts<-read_xlsx(file.path(dataOutDir,paste('Wetsuweten_pts.xlsx',sep='')))
#LBN
LBN_pts<-read_xlsx(file.path(dataOutDir,paste('LBN_pts.xlsx',sep='')))
#WFN
WFN_pts<-read_xlsx(file.path(dataOutDir,paste('WFN_pts.xlsx',sep='')))

#Join strata and select criteria attributes data back to wetlands
SampleStrata<-Wetlands %>%
  st_drop_geometry() %>%
  left_join(Gitanyow, by='Wetland_Co') %>%
  left_join(GitxsanWshd_pts, by='Wetland_Co') %>%
  left_join(Wetsuweten_pts, by='Wetland_Co') %>%
  left_join(LBN_pts, by='Wetland_Co') %>%
  left_join(WFN_pts, by='Wetland_Co') %>%
  unite('Nat', c('Nation.x','Nation.y','Nation.x.x','Nation.y.y','Nation'), na.rm = TRUE, remove = FALSE,sep='') %>%
  mutate(House_Name = Nat) %>%
  #dplyr::filter(House_Name != "") %>%
  mutate(House_Name = replace_na(House_Name, 'Non-Gitanyow')) %>%
  left_join(WetFlow, by='Wetland_Co') %>%
  left_join(bec_pts, by='Wetland_Co') %>%
  left_join(LandT, by='Wetland_Co') %>%
  left_join(Disturb, by='Wetland_Co') %>%
  mutate(StrataGroup=as.character(group_indices(.,BEC,FlowCode))) %>%
  #Drop any wetlands that are NA for BEC - 6 cases for some reason
  dplyr::filter(!is.na(BEC)) %>%
  #Drop Landcover NAs - 64 cases? all wetlands should be assigned properly? need to check
  dplyr::filter(!is.na(LanCoverLabel)) %>%
  mutate(Sampled=0) %>%
  mutate(SampleType=0) %>%
  dplyr::select(Wetland_Co, Sampled, SampleType, kmRd, StrataGroup, House_Name, Dist_to_Road, BEC,
                FlowCode, Verticalflow=Verticalflow.y, Bidirectional=Bidirectional.y,
                Throughflow=Throughflow.y, Outflow=Outflow.y, Inflow=Inflow.y,
                LanCoverLabel, DisturbType, Wshd_Sample_Type,WatershedID,Wet_plotLink,
                FREP_OPENING_ID,FREP_DISTRICT_NAME,FREP_OPENING_GROSS_AREA,FREP_DISTURBANCE_END_DATE)

#Load in sits previously surveyed
#Read in cleaned wetland plot data - from 02_clean_plot_data.R
site_data <- read_excel(file.path(dataOutDir,'WetPlots.xlsx'), 1)
plot_data <- read_excel(file.path(dataOutDir,'WetPlots.xlsx'), 2)
veg_data <- read_excel(file.path(dataOutDir,'WetPlots.xlsx'), 4)

#Join wetland spatial with plot data to spatially identify wetlands
#some issues with GPS locations in field card missing or not necessarily correct UTM issues, miss record
#Pull only wetlands that have plots
wet_site2019<- SampleStrata %>%
  mutate(newid = Wetland_Co) %>%
  inner_join(site_data, by=c('Wet_plotLink'='newid')) %>%
  mutate(Sampled=1) %>%
  mutate(SampleType=1) %>%
  dplyr::select(Wetland_Co, Sampled, SampleType,kmRd, StrataGroup, House_Name, Dist_to_Road, BEC,
                FlowCode, Verticalflow, Bidirectional,Throughflow, Outflow, Inflow,
                LanCoverLabel, DisturbType, Wshd_Sample_Type,WatershedID,
                FREP_OPENING_ID,FREP_DISTRICT_NAME,FREP_OPENING_GROSS_AREA,FREP_DISTURBANCE_END_DATE)

SampleStrata20191<-SampleStrata %>%
  dplyr::select(!Wet_plotLink)

#Set Sampled in SampleStrata where it has been sampled and set others to 0
SampleStrata20191$Sampled <- wet_site2019[match(SampleStrata20191$Wetland_Co, wet_site2019$Wetland_Co),2]
SampleStrata20191$SampleType <- wet_site2019[match(SampleStrata20191$Wetland_Co, wet_site2019$Wetland_Co),3]
SampleStrata20191[is.na(SampleStrata20191)] <- 0

#Filter out wetlands that dont have assinged Nation unless they were sampled
SampleStrata20191 <- SampleStrata20191 %>%
  dplyr::filter(House_Name!="" | Sampled==1)

saveRDS(SampleStrata20191, file = 'tmp/AOI/SampleStrata20191')

#FREP wetlands

#DSS FREP Wetlands:
#Prioritize  wetlands associated with OPENING_IDs in priority watersheds
#1685607 1685204 1686836
#OPENING_ID 1685607 - Gitanyow Wetland candidates (40860, 40864, 40870, 40884) - 40884
#Wetlands 40876, 40902, 40898 also in block but outside of priority watershed
#OPENING_ID - Gitxsan-Babine - 1685204 - Wetland candidates (41130, 58148) - 41130? or both since doing FREP?
#OPENING_ID - Gitxsan-Kispiox - 1686836 - Wetland candidates (41215, 41216) - 41216?
#OPENING_ID - OW - 1685492 - wetland 6673 - wetland overlaps priority watershed, but not totally within
#Second tier FREP wetlands - Buhr interest in 2-3 blocks in Gitanyow - but no more blocks with wetlands
#Add FREP wetland sites to sample frame
#How many are in the watersheds?
nrow(subset(SampleStrata20191, FREP_OPENING_ID>0)) #117
nrow(subset(SampleStrata20191, Wshd_Sample_Type>0 & FREP_OPENING_ID>0)) #8
length(unique(SampleStrata20191$FREP_OPENING_ID)) #3 blocks
tt<-subset(SampleStrata20191, Wshd_Sample_Type>0 & FREP_OPENING_ID>0)
length(unique(tt$FREP_OPENING_ID)) #3 blocks -

FREPtargetDSS<-c(40884,41130,41216,6673)

#DND FREP wetlands
NadinaFREPWetlands<-subset(SampleStrata, FREP_OPENING_ID>0 &
                             FREP_DISTRICT_NAME=='Nadina Natural Resource District' &
                             House_Name != "" &
                             kmRd==1)
length(unique(NadinaFREPWetlands$FREP_OPENING_ID)) #35

#write out for inspection
#write_sf(NadinaFREPWetlands, file.path(spatialOutDir,"NadinaFREPWetlands.gpkg"))
#mapview(NadinaFREPWetlands)+mapview(ESI_LBN)+mapview(ESI_OW)+mapview(ESI)
#select 4 random wetlands from the Nadina FREP blocks
#DNDrandomFREPblocks<-sample(NadinaFREPWetlands$FREP_OPENING_ID,4)
FREPtargetDND<-sample(NadinaFREPWetlands$Wetland_Co,4)

FREPsites<- SampleStrata20191 %>%
  dplyr::filter(Wetland_Co %in% FREPtargetDND | Wetland_Co %in% FREPtargetDSS) %>%
  dplyr::select(Wetland_Co, Sampled, SampleType,kmRd, StrataGroup, House_Name, Dist_to_Road, BEC,
                FlowCode, Verticalflow, Bidirectional,Throughflow, Outflow, Inflow,
                LanCoverLabel, DisturbType, Wshd_Sample_Type,WatershedID,
                FREP_OPENING_ID,FREP_DISTRICT_NAME,FREP_OPENING_GROSS_AREA,FREP_DISTURBANCE_END_DATE)

SampleStrata20192<-SampleStrata20191 %>%
  mutate(Sampled=ifelse((Wetland_Co %in% FREPsites$Wetland_Co), 1, Sampled)) %>%
  mutate(SampleType=ifelse((Wetland_Co %in% FREPsites$Wetland_Co), 2, SampleType)) %>%
  mutate(DisturbType=ifelse((Wetland_Co %in% FREPsites$Wetland_Co), "Logging", DisturbType))
SampleStrata20192[is.na(SampleStrata20192)] <- 0


#Set sample SampleType=2 if a FREP wetland and to 3 if a CGL wetland
CGLtarget<-c(30878, 30889, 31153, 31225, 4679)
CGLsites<- SampleStrata20192 %>%
  dplyr::filter(Wetland_Co %in% CGLtarget) %>%
  dplyr::select(Wetland_Co, Sampled, SampleType,kmRd, StrataGroup, House_Name, Dist_to_Road, BEC,
                FlowCode, Verticalflow, Bidirectional,Throughflow, Outflow, Inflow,
                LanCoverLabel, DisturbType, Wshd_Sample_Type,WatershedID,
                FREP_OPENING_ID,FREP_DISTRICT_NAME,FREP_OPENING_GROSS_AREA,FREP_DISTURBANCE_END_DATE)

SampleStrata20193<-SampleStrata20192 %>%
  mutate(Sampled=ifelse((Wetland_Co %in% CGLsites$Wetland_Co), 1, Sampled)) %>%
  mutate(SampleType=ifelse((Wetland_Co %in% CGLsites$Wetland_Co), 3, SampleType)) %>%
  mutate(DisturbType=ifelse((Wetland_Co %in% CGLsites$Wetland_Co), "Pipeline", DisturbType))

SampleStrata20193[is.na(SampleStrata20193)] <- 0
saveRDS(SampleStrata20193, file = 'tmp/AOI/SampleStrata2019Full')

#data check, How many sampled wetlands in priority watersheds
tt<-subset(SampleStrata20193, Wetland_Co %in% CGLtarget | Wetland_Co %in% FREPtargetDND | Wetland_Co %in% FREPtargetDSS)
nrow(SampleStrata20193)#53109
table(SampleStrata20193$SampleType)
nrow(subset(SampleStrata20193, Sampled>0)) #30
nrow(subset(SampleStrata20193, Sampled>0 & Wshd_Sample_Type>0)) #5
nrow(subset(SampleStrata20193, Sampled==0 & Wshd_Sample_Type>0)) #0
nrow(subset(SampleStrata20193, Sampled>0 | Wshd_Sample_Type>0)) #1609

#Filter into new df
#Filter out suspect polygons and the northern Gitxsan Watersheds
#unique(SampleStrata20193$House_Name)

HouseDrop<-c("Gitxsan_ Sustut","Gitxsan_ Nass" ,"Gitxsan_ Upper Skeena" )
WetMappingErros<-c(40237)
SampleStrata2019<-SampleStrata20193 %>%
  dplyr::filter(!(Wetland_Co %in% WetMappingErros)) %>%
  dplyr::filter(!(House_Name %in% HouseDrop)) %>%
  dplyr::filter(Wshd_Sample_Type>0 | Sampled>0)
saveRDS(SampleStrata2019, file = 'tmp/AOI/SampleStrata2019')

wet_site2019<-SampleStrata2019 %>%
  dplyr::filter(Sampled>0)
saveRDS(wet_site2019, file = 'tmp/AOI/wet_site2019')
WriteXLS(wet_site2019,file.path(dataOutDir,paste('wet_site2019.xlsx',sep='')),SheetNames='wet_site2019')

StrataGroup<-SampleStrata %>%
  group_by(StrataGroup, BEC, FlowCode) %>%
  dplyr::summarise(nWetlands=n())

saveRDS(StrataGroup, file = 'tmp/AOI/StrataGroup')

tt<-(subset(SampleStrata2019, Sampled==1))


