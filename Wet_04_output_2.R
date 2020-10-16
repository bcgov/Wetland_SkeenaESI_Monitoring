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

#Add desciptor for each SampleType
SampleTypeD<-data.frame(SampleType=c(1,2,3,4,5,6,8),
                        SampleTDescrip=c('2019Sample','FREP','CGL','Biological','Administrative','WFN priority','2020Sample'))

#Write out updated 2020 sampling data by Nation
SampleStrataM<-readRDS(file = 'tmp/AOI/SampleStrataM') %>%
  dplyr::filter(Sampled==1) %>%
  mutate(Sampled=ifelse(!SampleType %in% c(1,8), 0,1)) %>%
  mutate(Admin=substr(House_Name, 1,30)) %>%
  left_join(SampleTypeD) %>%
  dplyr::select(Wetland_Co, Sampled, SampleType, SampleTDescrip, kmRd, StrataGroup, House_Name, Dist_to_Road, BEC,
                FlowCode, Verticalflow, Bidirectional,Throughflow, Outflow, Inflow,
                LanCoverLabel, DisturbType,Wshd_Sample_Type,WatershedID,
                FREP_OPENING_ID,FREP_DISTRICT_NAME,FREP_OPENING_GROSS_AREA,FREP_DISTURBANCE_END_DATE)

#Break data.frame into a list of data.frames of each Nation and get data.frame list member name
table(SampleStrataM$House_Name)
HouseSampleL<-split(SampleStrataM, SampleStrataM$House_Name)

AdminNames<-lapply(HouseSampleL, function(i){substr(i$House_Name[1],1,30)})
WriteXLS(HouseSampleL,file.path(dataOutDir,paste('WetsByHouse.xlsx',sep='')),SheetNames=AdminNames)


#Write out 2020 wetland spatial
WetlandsToSampleData <- SampleStrataM %>%
  dplyr::select(Wetland_Co, Sampled, SampleType, House_Name, BEC, FlowCode,
                LanCoverLabel,DisturbType)

#Write out spatial - all samples
WetlandsToSampleA<-Wetlands  %>%
  right_join(WetlandsToSampleData, by='Wetland_Co')

WetlandsToSampleA[is.na(WetlandsToSampleA)] <- 0
saveRDS(WetlandsToSampleA, file = 'tmp/AOI/WetlandsToSampleA')
write_sf(WetlandsToSampleA, file.path(spatialOutDir,"WetlandsToSampleA.gpkg"))

#Write out spatial - samples remaining
WetlandsToSampleData <- SampleStrataM %>%
  dplyr::filter(Sampled==0) %>%
  dplyr::select(Wetland_Co, Sampled, SampleType, House_Name, BEC, FlowCode,
                LanCoverLabel,DisturbType)

#Write out spatial
WetlandsToSampleLeft<-Wetlands  %>%
  right_join(WetlandsToSampleData, by='Wetland_Co')

WetlandsToSampleLeft[is.na(WetlandsToSampleLeft)] <- 0
saveRDS(WetlandsToSampleLeft, file = 'tmp/AOI/WetlandsToSampleLeft')
write_sf(WetlandsToSampleLeft, file.path(spatialOutDir,"WetlandsToSampleLeft.gpkg"))

WriteXLS(ScoreCard2020A,file.path(dataOutDir,paste('ScoreCard2020A.xlsx',sep='')))





