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

#Write out updated 2020 sampling data by Nation
SampleStrataM<-readRDS(file = 'tmp/AOI/SampleStrataM') %>%
  dplyr::filter(Sampled==1)

#Break data.frame into a list of data.frames of each Nation
table(SampleStrataM$House_Name)
HouseSampleL<-split(SampleStrataM, SampleStrataM$House_Name)

HouseDataNames<-unique(SampleStrataM$House_Name)
WriteXLS(HouseSampleL,file.path(dataOutDir,paste('WetsByHouse.xlsx',sep='')),SheetNames=HouseDataNames)

#Write out 2019 wetland data
WetData<-list(wet_site2019,ScoreCard2019,StrataGroup)
WetDataNames<-c('wet_site2019','ScoreCard2019','StrataGroup')
WriteXLS(WetData,file.path(dataOutDir,paste('WetPlots2019.xlsx',sep='')),SheetNames=WetDataNames)

#Write out 2020 wetland data-
WetData<-list(Wet_sampledS,ScoreCard2020,StrataGroup)
WetDataNames<-c('wet_site2020','ScoreCard2020','StrataGroup')
WriteXLS(WetData,file.path(dataOutDir,paste('WetPlots2020.xlsx',sep='')),SheetNames=WetDataNames)

#Write out 2020 wetland spatial
WetlandsToSampleData <- Wet_sampledS %>%
  dplyr::select(Wetland_Co, Sampled, SampleType, House_Name, BEC, FlowCode,
                LanCoverLabel,DisturbType)

WetlandsToSample<-Wetlands  %>%
  right_join(WetlandsToSampleData, by='Wetland_Co')

WetlandsToSample[is.na(WetlandsToSample)] <- 0
saveRDS(WetlandsToSample, file = 'tmp/AOI/WetlandsToSample')
write_sf(WetlandsToSample, file.path(spatialOutDir,"WetlandsToSample.gpkg"))

#Write out full wetland spatial
WetlandsData <- SampleStrataS %>%
  dplyr::select(Wetland_Co, Sampled, SampleType, House_Name, BEC, FlowCode,
                LanCoverLabel,DisturbType)

WetlandsProcessed<-Wetlands  %>%
  right_join(WetlandsData, by='Wetland_Co')

WetlandsProcessed[is.na(WetlandsProcessed)] <- 0
saveRDS(WetlandsProcessed, file = 'tmp/AOI/WetlandsProcessed')
write_sf(WetlandsProcessed, file.path(spatialOutDir,"WetlandsProcessed.gpkg"))

write_sf(tt, file.path(spatialOutDir,"Inflow.gpkg"))

#Write out watershed sample pool
WriteXLS(wshedTroutGroup,file.path(dataOutDir,paste('wshedTroutGroup.xlsx',sep='')))

st_write(VHwshed2, file.path(spatialOutDir,"VHwshedNew.kml"), driver='kml', delete_layer=TRUE, append=FALSE)
st_write(VHwshed1, file.path(spatialOutDir,"VHwshedOrig.kml"), driver='kml', delete_layer=TRUE, append=FALSE)

write_sf(VHwshed1, file.path(spatialOutDir,"VHwshedOrig.gpkg"))
write_sf(VHwshed2, file.path(spatialOutDir,"VHwshedNew.gpkg"))

#Save spatial version of wetlands with Tier 1.5 attributes
write_sf(Wetlands_ClimateLandForm, file.path(spatialOutDir,"Wetlands_ClimateLandForm.gpkg"))

#Save csv version of data
Wetlands_ClimateLandForm_dat <- Wetlands_ClimateLandForm %>%
  st_drop_geometry()

#write file to csv - change line feed from unix style to windows otherwise ClimateBC errors
write_csv(Wetlands_ClimateLandForm_dat, path=file.path(dataOutDir, 'Wetlands_ClimateLandForm_dat.csv'))



