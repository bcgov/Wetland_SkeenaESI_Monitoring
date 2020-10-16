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


# For dropping sample records and re-picking so requirements still met

#Read ins 2020 samples
SampleStrataS<-readRDS(file='tmp/AOI/SampleStrataS')
SampleStrata2019Full<-readRDS(file = 'tmp/AOI/SampleStrata2019Full')

nrow(subset(SampleStrataS, Sampled==1))
table(SampleStrataS$SampleType)

#Read in sites completed and dropped
WetPlotFnSheets<- excel_sheets(file.path(Tier2Dir,'2020FieldData/Sampling2020_3Sept.xlsx'))
Wet2020Complete<-read_excel(file.path(Tier2Dir,'2020FieldData/Sampling2020_3Sept.xlsx'),
                            sheet = WetPlotFnSheets[2]) %>%
                            mutate(Sampled=1) %>%
                            mutate(SampleType=8)

Wet2020Drop<-read_excel(file.path(Tier2Dir,'2020FieldData/Sampling2020_3Sept.xlsx'),
                            sheet = WetPlotFnSheets[3]) %>%
                            mutate(Sampled=0) %>%
                            mutate(SampleType=99)
# note ‘Lower Skeena’ = ‘Kitwanga’ = ‘Gitwangak’
AdminsToDrop<-c("Gitxsan_ Suskwa", "Gitxsan_ Kitwanga")

#Drop wetlands from sample pool for those in pool that were dropped
SampleStrataS <- SampleStrataS %>%
  dplyr::filter(!Wetland_Co %in% Wet2020Drop$Wetland_Co) %>%
  dplyr::filter(!House_Name %in% AdminsToDrop)

#Update wetlands sampled that are in the sample pool
SampleStrataS2 <- SampleStrataS %>%
  mutate(SampleType=ifelse(Wetland_Co %in% Wet2020Complete$Wetland_Co, 8, SampleType)) %>%
  mutate(Sampled=ifelse(Wetland_Co %in% Wet2020Complete$Wetland_Co, 1, Sampled))
#Add wetlands that were sampled but not in sample pool
nrow(subset(Wet2020Complete, !Wetland_Co %in% SampleStrataS2$Wetland_Co))
AddComp<-Wet2020Complete %>%
  dplyr::filter(!Wetland_Co %in% SampleStrataS2$Wetland_Co) %>%
  dplyr::select(Wetland_Co)

WetlandsToAdd<-SampleStrata2019Full  %>%
  right_join(AddComp, by='Wetland_Co') %>%
  mutate(Sampled=1) %>%
  mutate(SampleType=8) %>%
  dplyr::select(Wetland_Co, Sampled, SampleType, kmRd, StrataGroup, House_Name, Dist_to_Road, BEC,
                FlowCode, Verticalflow, Bidirectional,Throughflow, Outflow, Inflow,
                LanCoverLabel, DisturbType,Wshd_Sample_Type,WatershedID,
                FREP_OPENING_ID,FREP_DISTRICT_NAME,FREP_OPENING_GROSS_AREA,FREP_DISTURBANCE_END_DATE)

SampleStrataM <- rbind(SampleStrataS2,WetlandsToAdd)

#WFN priority wetlands
WFNSheets<- excel_sheets(file.path(DataDir,'Wetlands2020_Wetlands3_WFNSamplePlan_06Sep2020.xlsx'))
WFNpriority<-read_excel(file.path(DataDir,'Wetlands2020_Wetlands3_WFNSamplePlan_06Sep2020.xlsx'),
                            sheet = WFNSheets[1])
nrow(WFNpriority) #25
#Check if any WFN wetlands are in sample frame
#tt<-subset(SampleStrataM, Wetland_Co %in% WFNpriority$Wetland_Co)
nrow(subset(SampleStrataM, House_Name == 'Wetsuweten')) #791
nrow(subset(SampleStrataM, Sampled==1 & House_Name == 'Wetsuweten'))#56
WetSites<-SampleStrataM %>%
  dplyr::filter(SampleType>0 & House_Name == 'Wetsuweten')

table(WetSites$SampleType)
#1  2  3  4  5  8
#9  5  3  5 28  6

WetSitesDrop<-WetSites %>%
  dplyr::filter(!SampleType %in% c(1,8)) #41
table(WetSitesDrop$SampleType)

#Drop 28 administrative sites from Wetsuweten and replace with WFN sites.
AddWFN<-WFNpriority %>%
  dplyr::filter(!Wetland_Co %in% SampleStrataM$Wetland_Co) %>%
  dplyr::select(Wetland_Co)

WFNToAdd<-SampleStrata2019Full  %>%
  right_join(AddWFN, by='Wetland_Co') %>%
  mutate(Sampled=1) %>%
  mutate(SampleType=6) %>%
  mutate(SampleTDescrip='WFN priority') %>%
  dplyr::select(Wetland_Co, Sampled, SampleType, kmRd, StrataGroup, House_Name, Dist_to_Road, BEC,
                FlowCode, Verticalflow, Bidirectional,Throughflow, Outflow, Inflow,
                LanCoverLabel, DisturbType,Wshd_Sample_Type,WatershedID,
                FREP_OPENING_ID,FREP_DISTRICT_NAME,FREP_OPENING_GROSS_AREA,FREP_DISTURBANCE_END_DATE)

SampleStrataM <- SampleStrataM %>%
  rbind(WFNToAdd) %>%
  dplyr::filter(!(SampleType==5 & House_Name == 'Wetsuweten'))


#Gitxsan priority wetlands
#Read in boundaries to identify wetlands
FN_house_layers <- st_layers(file.path(SyncDir, "BC Gov Internal/FN_Territory.gpkg"))
Gitxsan_housesIn <- st_read(file.path(SyncDir, "BC Gov Internal/FN_Territory.gpkg"), layer='gitxsan_houses_v6_191007') %>%
    st_transform(st_crs(ESI))

Gitxsan_house_pts <-st_intersection(wetptW, Gitxsan_housesIn) %>%
  st_drop_geometry()
Gitxsan_houses <- Gitxsan_house_pts %>%
  dplyr::select(Wetland_Co,LAXWIIYIP,PDEEK,SIMGIIGYET,WATERSHED_) %>%
  dplyr::filter((LAXWIIYIP=='Xsi Git Gat Gaitin' & PDEEK=='Giskaast' &
                   SIMGIIGYET=='Gwii Yeehl' & WATERSHED_=='Kispiox') |
                  (LAXWIIYIP=='Sgan Snaat' & PDEEK=='Lax Gibuu' &
                     SIMGIIGYET=='Wii Mugulsxw' & WATERSHED_=='Kispiox'))

nrow(Gitxsan_houses) #142
nrow(subset(SampleStrataM, Wetland_Co %in% Gitxsan_houses$Wetland_Co)) #22

tt<- SampleStrataM %>%
  dplyr::filter(Wetland_Co %in% Gitxsan_houses$Wetland_Co)

#41216 and 41235
tttt<- Gitxsan_house_pts %>%
  dplyr::filter(Wetland_Co %in% c(41216, 41235)) %>%
  dplyr::select(Wetland_Co,LAXWIIYIP,PDEEK,SIMGIIGYET,WATERSHED_)

#Join point coverage back to main wetland file
Wetlands3<-Wetlands %>%
  #st_drop_geometry %>%
  left_join(FREP_pts, by='Wetland_Co')





#Save revised output
saveRDS(SampleStrataM , file = 'tmp/AOI/SampleStrataM')

tt<-subset(SampleStrataM, Sampled==1)


