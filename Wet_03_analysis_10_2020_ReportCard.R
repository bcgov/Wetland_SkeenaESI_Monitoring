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

#Generates 2020 report card from data 03_Wet_09_sampleAddStrata.R

SampleStrataS<-readRDS(file='tmp/AOI/SampleStrataS')

#Use a function to get #categories, #wets
RequireFn <- function(dataset, RequireNIn){
  dataset %>%
    group_by_(.dots=requs[RequireNIn,2]) %>%
    dplyr::summarise(nSampled=sum(Sampled), nWets=n()) %>%
    dplyr::rename(setNames(requs[RequireNIn,2], 'Requirement')) %>%
    dplyr::mutate(ReqGroup=RequireNIn) %>%
    mutate(ReqGroupName=requs[RequireNIn,2])%>%
    dplyr::select(ReqGroup, ReqGroupName, Requirement,nWets, nSampled)
}

#Make a list of what attributes to populate the score card and feed funtion
requs<-data.frame(ReqN=c(1,2,3,4,5,6,7,8,9,10),
                  Req=c('StrataGroup','WatershedID','House_Name','Verticalflow',
                        'Bidirectional','Throughflow', 'Outflow', 'Inflow',
                        'LanCoverLabel', 'DisturbType'))

df<-lapply(requs[,1], function(i) RequireFn(SampleStrataS, i))
ScoreCard2020<-ldply(df,data.frame)

saveRDS(ScoreCard2020, file = 'tmp/AOI/ScoreCard2020')

