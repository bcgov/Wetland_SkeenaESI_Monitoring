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

# Generates SampleStrataR file and updated score card
# has wetlands for sampling meeting requirements

source ('header.R')
#SampleStrata<-readRDS(file='tmp/AOI/SampleStrata')
SampleStrata2019<-readRDS(file='tmp/AOI/SampleStrata2019')
#wet_site2019<-readRDS(file='tmp/AOI/wet_site2019')
#SampleStrataR<-readRDS(file='tmp/AOI/SampleStrataR')

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

########
setNames(requs[1,2], 'Requirement')
#######

#Make a list of what attributes to populate the score card and feed function
requs<-data.frame(ReqN=c(1,2,3,4,5,6,7),
                  Req=c('Verticalflow',
                        'Bidirectional','Throughflow', 'Outflow', 'Inflow',
                        'LanCoverLabel', 'DisturbType'))
#### Start Loop  ####
#Set variables
j<-1
NWetsToSample<-100
Wet_sampledR<-wet_site2019
#SampleStrataR<-SampleStrata
SampleStrataR<-SampleStrata2019

#Updated sampled with what has been sampled and set rest to 0
#SampleStrataR$Sampled <- Wet_sampledR[match(SampleStrataR$Wetland_Co, Wet_sampledR$Wetland_Co),2]
#SampleStrataR[is.na(SampleStrataR)] <- 0

nrow(subset(SampleStrataR, Sampled==1))

#How many replicates for each requirement?
NReplicates<-3
minSampled<-1
NoSamples<-list()
#Initialize a score card listing what requirement has been sampled
df<-lapply(requs[,1], function(i) RequireFn(SampleStrataR, i))
ScoreCardR<-ldply(df,data.frame)

#Loop through till NReplicates is met for all requirements
while (minSampled < NReplicates) {
  #for (j in 1:NWetsToSample) {

  #Remove wetlands already sampled from the SampleStrata pool
  #and those far from roads
  SampleStrataPool <- SampleStrataR %>%
    filter(Sampled == 0) %>%
    #filter(Sampled < NReplicates) %>%#Not sure what this is doing?
    filter(kmRd==1)

  #Take least common attribute of scorecard that hasn't been sampled at least 2 times for sampling
  NewSampIn <- ScoreCardR %>%
    filter(nSampled < NReplicates) %>%
    filter(rank(nWets,ties.method="first")==1)

  NewSampleTest<-NewSampIn %>%
    left_join(SampleStrataPool, by = setNames(requs[NewSampIn$ReqGroup,2], 'Requirement')) %>%
    filter(rank(nWets,ties.method="random")==1)

  if (!is.na(NewSampleTest$Wetland_Co)) {

  #Now join with SampleStrata to get all the attributes
  #NewSample<-NewSamp %>%
  #  left_join(SampleStrataPool, by = setNames(requs[NewSampIn$ReqGroup,2], 'Requirement')) %>%
  #  filter(rank(nWets,ties.method="first")==1) %>%
  NewSample<-NewSampleTest %>%
    dplyr::rename(setNames('Requirement',requs[NewSampIn$ReqGroup,2])) %>%
    dplyr::sample_n(1) %>%
    mutate(Sampled=1) %>%
    mutate(SampleType=4) %>%
    #dplyr::select(Wetland_Co, Sampled, kmRd, StrataGroup, House_Name, Dist_to_Road, BEC, FlowCode, Verticalflow, LanCoverLabel, DisturbType)
  dplyr::select(Wetland_Co, Sampled, SampleType, kmRd, StrataGroup, House_Name, Dist_to_Road, BEC,
                FlowCode, Verticalflow, Bidirectional,Throughflow, Outflow, Inflow,
                LanCoverLabel, DisturbType,Wshd_Sample_Type, WatershedID,
                FREP_OPENING_ID,FREP_DISTRICT_NAME,FREP_OPENING_GROSS_AREA,FREP_DISTURBANCE_END_DATE)
  #Add to already selected wetlands
  Wet_sampledR <- rbind(Wet_sampledR,NewSample)
  SampleStrataR$Sampled <- Wet_sampledR[match(SampleStrataR$Wetland_Co, Wet_sampledR$Wetland_Co),2]
  SampleStrataR$SampleType <- Wet_sampledR[match(SampleStrataR$Wetland_Co, Wet_sampledR$Wetland_Co),3]
  SampleStrataR[is.na(SampleStrataR)] <- 0

  #Regenerate the score card
  df<-lapply(requs[,1], function(i) RequireFn(SampleStrataR, i))
  ScoreCardR<-ldply(df,data.frame) %>%
    dplyr::filter(!Requirement %in% NoSamples)

  minSampled<-min(ScoreCardR$nSampled)
  #minSampled<-sum((ScoreCardR$Target-ScoreCardR$nSampled)[(ScoreCardR$Target-ScoreCardR$nSampled)>=0])

  } else {

#If not samples then track and remove from score card
    NoSamples<-rbind(NoSamples, NewSampIn$Requirement)
    ScoreCardR<-ScoreCardR %>%
      dplyr::filter(!Requirement %in% NoSamples)

  }
#get another wetland to sample
}
Wet_sampledS <- SampleStrataR %>%
  dplyr::filter(Sampled==1)

tt<-(subset(SampleStrataR, Sampled==1))

#data check
nrow(subset(SampleStrataR, Sampled==1))
tt<-(subset(SampleStrataR, Sampled==1))
saveRDS(Wet_sampledR, file = 'tmp/AOI/Wet_sampledR')
saveRDS(SampleStrataR, file = 'tmp/AOI/SampleStrataR')


