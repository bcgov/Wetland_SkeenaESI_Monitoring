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

#Generates SampleStrataS file
#builds off 03_Wet_08_sampleRequirements.R - which meets basic requirments
#this adds additional sites - up to the 70 required that fill in and
#meet the statistical strata requirement

source ('header.R')

Wet_sampledS<-readRDS(file='tmp/AOI/Wet_sampledR')
SampleStrataS<-readRDS(file='tmp/AOI/SampleStrataR')

#Calculate the number of groups, based on all the BEC flow combinations
NSampGroups<- SampleStrataS %>%
  group_by(BEC, FlowCode) %>%
  n_groups() #6

#sampRequired <- Wet_dt %>%
#  group_by(BEC, FlowCode) %>%
#  dplyr::summarise(nToSample=round(100/NSampGroups,0))
#sum(sampRequired$nToSample)
#Need 17 min in each strata - set to 20

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

#Make a list of what attributes to populate the score card and feed function
requs<-data.frame(ReqN=c(1,2,3),
                  Req=c('StrataGroup','WatershedID','House_Name'))

#### Start Loop  ####
#Set variables
j<-1
NWetsToSample<-100
NReplicates<-2
minSampled<-1
NoSamples<-list()

#Initialize a score card listing what requirement has been sampled
df<-lapply(requs[,1], function(i) RequireFn(SampleStrataS, i))
ScoreCardS<-ldply(df,data.frame)

#Loop through till NReplicates is met for all requirements
while (minSampled < NReplicates) {
  #for (j in 1:NWetsToSample) {

  #Remove wetlands already sampled from the SampleStrata pool
  #and those far from roads
  SampleStrataPool <- SampleStrataS %>%
    filter(Sampled ==0) %>%
    filter(Sampled < NReplicates) %>%
    filter(kmRd==1)

  #Take least common attribure of score card that hasnt been sampled at least 2 times for sampling
  NewSampIn <- ScoreCardS %>%
    filter(nSampled < NReplicates) %>%
    filter(rank(nWets,ties.method="first")==1)

  NewSampleTest<-NewSampIn %>%
    left_join(SampleStrataPool, by = setNames(requs[NewSampIn$ReqGroup,2], 'Requirement')) %>%
    filter(rank(nWets,ties.method="random")==1)

  if (!is.na(NewSampleTest$Wetland_Co)) {

  #Now join with SampleStrata to get all the attributes
  NewSample<-NewSampIn %>%
    left_join(SampleStrataPool, by = setNames(requs[NewSampIn$ReqGroup,2], 'Requirement')) %>%
    dplyr::rename(setNames('Requirement',requs[NewSampIn$ReqGroup,2])) %>%
    dplyr::sample_n(1) %>%
    mutate(SampleType=5) %>%
    mutate(Sampled=1) %>%
    #dplyr::select(Wetland_Co, Sampled, kmRd, StrataGroup, House_Name, Dist_to_Road, BEC, FlowCode, Verticalflow, LanCoverLabel, DisturbType)
  dplyr::select(Wetland_Co, Sampled, SampleType, kmRd, StrataGroup, House_Name, Dist_to_Road, BEC,
                FlowCode, Verticalflow, Bidirectional,Throughflow, Outflow, Inflow,
                LanCoverLabel, DisturbType,Wshd_Sample_Type,WatershedID,
                FREP_OPENING_ID,FREP_DISTRICT_NAME,FREP_OPENING_GROSS_AREA,FREP_DISTURBANCE_END_DATE)

  #Add to already selected wetlands
  Wet_sampledS <- rbind(Wet_sampledS,NewSample)
  SampleStrataS$Sampled <- Wet_sampledS[match(SampleStrataS$Wetland_Co, Wet_sampledS$Wetland_Co),2]
  SampleStrataS$SampleType <- Wet_sampledS[match(SampleStrataS$Wetland_Co, Wet_sampledS$Wetland_Co),3]
  SampleStrataS[is.na(SampleStrataS)] <- 0

  #Regenerate the score card
  df<-lapply(requs[,1], function(i) RequireFn(SampleStrataS, i))
  ScoreCardS<-ldply(df,data.frame)

  minSampled<-min(ScoreCardS$nSampled)
  #get another wetland to sample
} else {

  #If not samples then track and remove from score card
  NoSamples<-rbind(NoSamples, NewSampIn$Requirement)
  ScoreCardS<-ScoreCardS %>%
    dplyr::filter(!Requirement %in% NoSamples)
    minSampled<-min(ScoreCardS$nSampled)
}
#get another wetland to sample
}

saveRDS(SampleStrataS, file = 'tmp/AOI/SampleStrataS')
saveRDS(Wet_sampledS, file = 'tmp/AOI/Wet_sampledS')
saveRDS(ScoreCardS, file = 'tmp/AOI/ScoreCardS')

#data check
tt<-subset(SampleStrataS, Sampled==1)

Ag<-subset(SampleStrataS, DisturbType=='Moderate_Agriculture')
Fire<-subset(SampleStrataS, DisturbType=='Fire')
Urban<-subset(SampleStrataS, DisturbType=='Severe_Urban_roads')


