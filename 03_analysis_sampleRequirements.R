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
requs<-data.frame(ReqN=c(1,2,3,4,5,6,7,8),
                  Req=c('House_Name','Verticalflow',
                        'Bidirectional','Throughflow', 'Outflow', 'Inflow',
                        'LanCoverLabel', 'DisturbType'))
#### Start Loop  ####
#Set variables
j<-1
NWetsToSample<-100
Wet_sampledR<-wet_site2019
SampleStrataR<-SampleStrataS
NReplicates<-3
minSampled<-1
#Initialize a score card listing what requirement has been sampled
df<-lapply(requs[,1], function(i) RequireFn(SampleStrataR, i))
ScoreCardR<-ldply(df,data.frame)

#Loop through till NReplicates is met for all requirements
while (minSampled < NReplicates) {
  #for (j in 1:NWetsToSample) {

  #Remove wetlands already sampled from the SampleStrata pool
  #and those far from roads
  SampleStrataPool <- SampleStrataR %>%
    filter(Sampled < NReplicates) %>%
    filter(kmRd==1)

  #Take least common attribure of score card that hasnt been sampled at least 2 times for sampling
  NewSampIn <- ScoreCardR %>%
    filter(nSampled < NReplicates) %>%
    filter(rank(nWets,ties.method="first")==1)
  #Now join with SampleStrata to get all the attributes
  NewSample<-NewSampIn %>%
    left_join(SampleStrataPool, by = setNames(requs[NewSampIn$ReqGroup,2], 'Requirement')) %>%
    dplyr::rename(setNames('Requirement',requs[NewSampIn$ReqGroup,2])) %>%
    dplyr::sample_n(1) %>%
    mutate(Sampled=1) %>%
    #dplyr::select(Wetland_Co, Sampled, kmRd, StrataGroup, House_Name, Dist_to_Road, BEC, FlowCode, Verticalflow, LanCoverLabel, DisturbType)
  dplyr::select(Wetland_Co, Sampled, kmRd, StrataGroup, House_Name, Dist_to_Road, BEC,
                FlowCode, Verticalflow, Bidirectional,Throughflow, Outflow, Inflow,
                LanCoverLabel, DisturbType)
  #Add to already selected wetlands
  Wet_sampledR <- rbind(Wet_sampledR,NewSample)
  SampleStrataR$Sampled <- Wet_sampledR[match(SampleStrataR$Wetland_Co, Wet_sampledR$Wetland_Co),2]
  SampleStrataR[is.na(SampleStrataR)] <- 0

  #Regenerate the score card
  df<-lapply(requs[,1], function(i) RequireFn(SampleStrataR, i))
  ScoreCardR<-ldply(df,data.frame)

  minSampled<-min(ScoreCardR$nSampled)
#get another wetland to sample
}

#data check
tt<-subset(SampleStrataR, Sampled==1)
