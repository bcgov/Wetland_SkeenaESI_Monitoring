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
SampleStrata<-readRDS(file='tmp/AOI/SampleStrata')
SampleStrata2019<-SampleStrata
#Set Sampled in SampleStrata where it has been sampled and set others to 0
SampleStrata2019$Sampled <- wet_site2019[match(SampleStrata2019$Wetland_Co, wet_site2019$Wetland_Co),2]
SampleStrata2019[is.na(SampleStrata2019)] <- 0
saveRDS(SampleStrata2019, file = 'tmp/AOI/SampleStrata2019')

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
requs<-data.frame(ReqN=c(1,2,3,4,5,6,7,8,9),
                  Req=c('StrataGroup','House_Name','Verticalflow',
                        'Bidirectional','Throughflow', 'Outflow', 'Inflow',
                        'LanCoverLabel', 'DisturbType'))

df<-lapply(requs[,1], function(i) RequireFn(SampleStrata2019, i))
ScoreCard2019<-ldply(df,data.frame)

saveRDS(ScoreCard2019, file = 'tmp/AOI/ScoreCard2019')
