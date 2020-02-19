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

source('header.R')

#Make an unlist for column names, clean up so no spaces and puncutation
colName<-PlotInfo[,1] %>%
  mutate_all(funs(gsub("[[:punct:]]", "", .))) %>%
  mutate_all(funs(gsub(" ", "_", .))) %>%
  unlist()

#Peel off top of file which has general wetland info
#First record contains headers
WetInfo<-PlotInfo[1:8,-1] %>%
  t() %>%
  as.data.frame()

colnames(WetInfo)<-colName[1:8]
WetInfo <- tibble::rownames_to_column(WetInfo, "FID")

#split sampled wetlands into in wetland plots ie data.frame is by plots not wetlands
# 5 possible sub-plots
#First Transpose entire data set
WetPlots<-PlotInfo[9:79,-1] %>%
  t() %>%
  as.data.frame()

#Make a list of column names for plots
colnames(WetPlots)<-colName[9:79]
WetPlots <- WetPlots %>%
  tibble::rownames_to_column("FID")
  #mutate()

#Function to select plots
WetExtractF <- function(WP,i) {
  WP %>% dplyr::select(FID,starts_with(paste("P",i,sep=''))) %>%
  setNames(wetPlotColNames)
}

#Make a column name list
  wetPlotColNames <- WetPlots %>%
    dplyr::select(FID,starts_with("P1")) %>%
    colnames() %>%
    str_remove("P1_")

#Make a list of plots
WetPList<-list()
for (j in 1:5) {
  WetPList[[j]]<-WetExtractF(WetPlots,j)
}

#Re-assemble as a data.frame by plots
WetPlots<-bind_rows(WetPList, .id = "Wetland_Plot_Num")

#Peel off wildlife obervations
WetWildlife<-PlotInfo[80:142,-1] %>%
  t() %>%
  as.data.frame()

colnames(WetWildlife)<-colName[80:142]
WetWildlife <- tibble::rownames_to_column(WetWildlife, "FID")



