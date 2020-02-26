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


#install.packages("janitor")
#install.packages("tibble")

library(lubridate)
library(tibble)
library(janitor)


source('header.R')

#Make a list for column names, clean up so no spaces and puncutation
colName<-PlotInfo[,1] %>%
  mutate_all(funs(gsub("[[:punct:]]", "", .))) %>%
  mutate_all(funs(gsub(" ", "_", .)))  %>%
  unlist()

#Peel off top of file which has general wetland info and transform to data.frame
#First record contains headers, will add layer
WetInfo <-PlotInfo[1:8,-1] %>%
  t() %>%
  as.data.frame()

colnames(WetInfo)<-colName[1:8]
WetInfo <- WetInfo %>%
  tibble::rownames_to_column("FID") %>%
  dplyr::rename(NewID=New_PolygonID_Layer_January_2020___Merged_wetland_Polygons__ObjectID)

WetInfo <- WetInfo %>%
  mutate(comments = WPT)

WetInfo <- WetInfo %>%
  mutate(WPT = as.numeric(as.character(WPT)),
         WPT = ifelse( FID == "190975", 142, WPT),
         NewID = as.numeric(as.character(NewID)))

# Fix dates
WetInfo <- WetInfo %>%
    mutate(Date2 = case_when(
                  FID == "111563" ~  mdy(as.character(Date)))) %>%
    mutate(Date1 = excel_numeric_to_date(
      as.numeric(as.character(WetInfo$Date)), date_system = "modern"))

WetInfo <- WetInfo %>%
  rowwise() %>%
   mutate(Date = max(Date2, Date1, na.rm = TRUE) )









#split sampled wetlands into in wetland plots ie data.frame is by plots not wetlands
#5 possible sub-plots
#First Transpose entire data set, except header row
WetInfoPlots<-PlotInfo[c(1,9:79),-1] %>%
  t() %>%
  as.data.frame()

#Make a list of column names for plots - from colName done above
colnames(WetInfoPlots)<-colName[c(1,9:79)]
WetInfoPlots <- WetInfoPlots %>%
  tibble::rownames_to_column("FID") %>%
  dplyr::rename(NewID=New_PolygonID_Layer_January_2020___Merged_wetland_Polygons__ObjectID)

###Option do within plot samples as seperate records or as attributes - stop as above
#Function to select plots
WetExtractF <- function(WP,i) {
  WP %>% dplyr::select(FID,starts_with(paste("P",i,sep=''))) %>%
  setNames(wetPlotColNames)
}

#Make a generic column name list ie peel off P#_ from each record
  wetPlotColNames <- WetInfoPlots %>%
    dplyr::select(FID,starts_with("P1")) %>%
    colnames() %>%
    str_remove("P1_")

#Make a list of plots
  WetVPList<-list()
for (j in 1:5) {
  WetVPList[[j]]<-WetExtractF(WetInfoPlots,j)
}

#Re-assemble as a data.frame by plots
WetInfoPlots2<-bind_rows(WetVPList, .id = "Wetland_Plot_Num")

#Peel off wildlife obervations
WetWildlife<-PlotInfo[c(1,80:142),-1] %>%
  t() %>%
  as.data.frame()

colnames(WetWildlife)<-colName[c(1,80:142)]
WetWildlife <- tibble::rownames_to_column(WetWildlife, "FID")

##############
# Plot Veg surveys - WetList
# Aggregate plots id'd by wetland id and plot number
WetVegPlots_DF<- WetList %>%
  ldply(data.frame) %>%
  mutate(NewID=gsub("Veg_FID","",.id)) %>%
  dplyr::select(NewID, Plot.ID, Species.Name, Latin,
                pc_cover=X..Cover, gt_3m=X.3.m, eq_1to3=X1.3.m, lt_1m=X..1m)

#Clean up Plot Function Data
#remove unnecessary columns and transform
WetPlotFnData <- WetPlotFnDataIn %>%
  dplyr::select(8:39) %>%
  t() %>%
  as.data.frame()

#assign column names
colName<- WetPlotFnDataIn %>%
  dplyr::select(1)
colnames(WetPlotFnData)<-unlist(as.list(colName))
#Clean up names
WetPlotFnData <- WetPlotFnData[ !duplicated(names(WetPlotFnData)) ]
#Drop un-needed columns, take first row and id all NA then drop those columns
ColsToDrop <- WetPlotFnData[1,]
WetPlotFnData <- WetPlotFnData %>%
  dplyr::select(-c(colnames(ColsToDrop)[colSums(is.na(ColsToDrop)) > 0]))
#Break function data into 2 data.frames so can write to xlsx (otherwise over 256 columns)
WetPlotFnData1<- WetPlotFnData[,1:187] # to F39 questions
WetPlotFnData2<- WetPlotFnData[,c(1:3,188:ncol(WetPlotFnData))] # F40 questions and greater


#clean up stressor sheet - WetPlotFnStressor
#Get plot numbers and remove (1/2) designation
colName<- colnames(WetPlotFnStressor)#[1,7:29])
colName<-gsub("\\(1/2)","",colName)
colnames(WetPlotFnStressor)<-colName

#Read data
WetStress<-WetPlotFnStressor %>% #[7:29]
  t() %>%
  as.data.frame()

#Assign colnames to transposed data and select cells that have data
colnames(WetStress)<-unlist(as.list(WetStress[1,]))
WetStress<-WetStress[8:nrow(WetStress),2:ncol(WetStress)] %>%
  tibble::rownames_to_column("NFID")

#Join WetlandFnData to spatial wetland data on 'NewID' and 'Wetland_Co'
WetPlotFnData$Wetland_Co<-as.numeric(levels(WetPlotFnData$NewID))[WetPlotFnData$NewID]

Wetland_Fn<-Wetlands %>%
  left_join(WetPlotFnData, by=c('Wetland_Co')) %>%
  dplyr::filter(!is.na(NewID))

#mapview(Wetland_Fn)

## write out data
#Take processed data make alist and write to multi-tab spreadsheet for use in R course
WetData<-list(WetInfo,WetInfoPlots,WetVegPlots_DF,WetPlotFnData1,WetPlotFnData2,WetStress)
WetDataNames<-c('WetlandInfo','WetlandVeg','WetVegPlots_DF,','WetlandFunction1','WetlandFunction2','WetlandStressors')

WriteXLS(WetData,file.path(dataOutDir,paste('WetPlots.xls',sep='')),SheetNames=WetDataNames)

##############

#Data explore
Wetland_Fntest<-Wetland_data %>%
  left_join(WetPlotFnData, by=c('Wetland_Co')) %>%
  dplyr::select(Wetland_Co,NewID, Shape_Area, MAP_LABEL)





