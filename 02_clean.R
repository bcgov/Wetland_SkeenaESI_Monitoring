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

#Make a list for column names, clean up so no spaces and puncutation
colName<-PlotInfo[,1] %>%
  mutate_all(funs(gsub("[[:punct:]]", "", .))) %>%
  mutate_all(funs(gsub(" ", "_", .)))  %>%
  mutate_all(funs(gsub("__", "_", .)))  %>%
  mutate_all(funs(gsub("___", "_", .)))  %>%
  unlist()




## Wetland site information  - Sheet 1:

#Peel off top of file which has general wetland info and transform to data.frame
#First record contains headers, will add layer
WetInfo <-PlotInfo[1:8,-1] %>%
  t() %>%
  as.data.frame()

colnames(WetInfo)<-colName[1:8]

WetInfo <- WetInfo %>%
  tibble::rownames_to_column("FID") %>%
  dplyr::rename(NewID = New_PolygonID_Layer_January_2020__Merged_wetland_Polygons_ObjectID,
                slope_pc = Slope_) %>%
  rename_all(tolower) %>%
  mutate(comments = wpt,
         wpt = as.numeric(as.character(wpt)),
         wpt = ifelse(fid == "190975", 142, wpt),
         newid = as.numeric(as.character(newid)),
         date2 = case_when(fid == "111563" ~  mdy(as.character(date))),
         date1 = excel_numeric_to_date(
           as.numeric(as.character(date)), date_system = "modern")) %>%
  rowwise() %>%
  mutate(date = max(date2, date1, na.rm = TRUE)) %>%
  select(- c(date2, date1))

#Make a LUT for newid since not in all forms
newid_LUT<-WetInfo %>%
  dplyr::select(fid,newid)

## Wetland plot information  - Sheet 2:

#split sampled wetlands into in wetland plots ie data.frame is by plots not wetlands
#5 possible sub-plots
#First Transpose entire data set, except header row
WetInfoPlots<-PlotInfo[c(1,9:79),-1 ] %>%
  t() %>%
  as.data.frame()

#Make a list of column names for plots - from colName done above
colnames(WetInfoPlots)<-colName[c(1,9:79)]

WetInfoPlots <- WetInfoPlots %>%
  tibble::rownames_to_column("fid") %>%
  dplyr::rename(newid=New_PolygonID_Layer_January_2020__Merged_wetland_Polygons_ObjectID) %>%
  rename_all(tolower)

###Option do within plot samples as seperate records or as attributes - stop as above
#Function to select plots
WetExtractF <- function(WP,i) {
  WP %>% dplyr::select(fid, starts_with(paste("p",i,sep=''))) %>%
  setNames(wetPlotColNames)
}

#Make a generic column name list ie peel off P#_ from each record
  wetPlotColNames <- WetInfoPlots %>%
    dplyr::select(fid,starts_with("p1")) %>%
    colnames() %>%
    str_remove("p1_")

#Make a list of plots
  WetVPList<-list()
for (j in 1:5) {
  WetVPList[[j]]<-WetExtractF(WetInfoPlots,j)
}

#Re-assemble as a data.frame by plots
WetInfoPlots2<-bind_rows(WetVPList, .id = "plot.id")
w <-bind_rows(WetVPList, .id = "plot.id")
w <- w[,-16]

WetInfoPlots2 <- w %>%
  mutate(plot.id = as.numeric(plot.id),
          easting = as.numeric(easting),
          northing = as.numeric(northing),
          water_ph = as.numeric(water_ph)) %>%
  right_join(newid_LUT)

# 1) We can see geographic co-ordinates and some NAs - so we will remove these
WetInfoPlots3 <- WetInfoPlots2 %>%
  filter(!is.na(utm_zone))

# 2) The coordinates are in two UTM zones (9 and 10)
unique(WetInfoPlots3$utm_zone)

# we can convert both of these to a common projection (i.e WGS84).
# We can build a function to do this ourselves.

convert_utm <- function(data, utm, input_crs, output_crs = 4236) {
  tdata <- data %>%
    filter(utm_zone == utm) %>%
    st_as_sf(coords = c("easting", "northing"), crs = input_crs) %>%
    st_transform(crs = output_crs)

  cbind(tdata, st_coordinates(tdata)) %>%
    st_drop_geometry()
}

# now we can use the function with wrote to convert the two parts of the data
# and join them back together into a single table. Note we now have an X, Y columns
# with out WGS coordinates.

utm9 <- convert_utm(data = WetInfoPlots3, utm = "9U" , input_crs =26909,  output_crs = 4236)
utm10 <- convert_utm(data = WetInfoPlots3, utm = "10U" , input_crs =26910,  output_crs = 4236)

plot_wgs <- bind_rows(utm9, utm10)

# the plot_wgs is a data frame but to plot this on a map we want to convert
# this to a spatial object. There are two main packages to do this (sp) an
# older package and (sf) a newer and more robust package. Lets convert firstly
# to an sf package.

plot_sf <- st_as_sf(plot_wgs, coords = c("X","Y"),  crs = 4236) %>%
  st_transform(3005)

# we can check this in an interactive map using the package mapview
mapview(plot_sf)

# wetland animal sightings # sheet 3.
#Peel off wildlife obervations
WetWildlife<-PlotInfo[c(1,80:142),-1] %>%
  t() %>%
  as.data.frame()

colnames(WetWildlife)<-colName[c(1,80:142)]
WetWildlife <- tibble::rownames_to_column(WetWildlife, "fid")
WetWildlife <- dplyr::rename(WetWildlife , newid =
                               New_PolygonID_Layer_January_2020__Merged_wetland_Polygons_ObjectID)





# wetland Plot Veg Surveys # sheet 4
# Aggregate plots id'd by wetland id and plot number

WetVegPlots_DF<- WetList %>%
  ldply(data.frame) %>%
  mutate(fid = gsub("Veg_FID","",.id)) %>%
  right_join(newid_LUT) %>%
  dplyr::select(fid, newid, Plot.ID, Species.Name, Latin,
                pc_cover=X..Cover, gt_3m=X.3.m, eq_1to3=X1.3.m, lt_1m=X..1m) %>%
  rename_all(tolower) %>%
  mutate(plot.id = as.numeric(gsub("P","", plot.id))) %>%
  mutate(latin = tolower(trimws(latin)),
         latin = gsub("sp.","sp",latin)) %>%
mutate(pc_cover = ifelse(pc_cover == "T", 1, pc_cover),
       pc_cover = as.numeric(pc_cover),
       latin_family = word(latin, 1)) %>%
  filter(!is.na(species.name),
         !is.na(pc_cover))




#Wetland Function Data - sheets 5 and 6
#Clean up Plot Function Data
#remove unnecessary columns and transform
WetPlotFnData <- WetPlotFnDataIn %>%
  dplyr::select(8:39) %>%
  t() %>%
  as.data.frame()

#assign column names
colName <- WetPlotFnDataIn %>%
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




#Wetland Stressor Data - sheet 7
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
  tibble::rownames_to_column("fid") %>%
  right_join(newid_LUT)








## write out data
#Take processed data make alist and write to multi-tab spreadsheet for use in R course
WetData<-list(WetInfo,WetInfoPlots2,WetWildlife,WetVegPlots_DF,WetPlotFnData1,WetPlotFnData2,WetStress)
WetDataNames<-c('WetlandInfo','Wetland_plot','WetWildlife','WetVegPlots_DF','WetlandFunction1','WetlandFunction2','WetlandStressors')

WriteXLS(WetData,file.path(dataOutDir,paste('WetPlots.xlsx',sep='')),SheetNames=WetDataNames)

