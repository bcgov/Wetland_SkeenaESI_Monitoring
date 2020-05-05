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

#Identify all fires that are <20 years old
Wildfire_Historical<-readRDS(file = 'tmp/AOI/Wildfire_Historical')
Wildfire_2018<-readRDS(file = 'tmp/AOI/Wildfire_2018')
ProvRast<-readRDS(file = 'tmp/ProvRast')
ESI<-readRDS(file = 'tmp/ESI')
AOI<-ESI
Fire20Year<-Wildfire_Historical %>%
  dplyr::filter(FIRE_YEAR>1999) %>% #icludes
  st_union(Wildfire_2018) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(fire=1)
#write_sf(Fire10Year, file.path(spatialOutDir,"Fire10Year.gpkg"))
Fire20YearR<- fasterize(Fire20Year,ProvRast,field='fire') %>%
 crop(AOI)
Fire20YearR[is.na(Fire20YearR)]<-0
writeRaster(Fire20YearR,filename=file.path(spatialOutDir,"Fire20YearR.tif"), format="GTiff", overwrite=TRUE)

#Set Log year to flag cutblocks <20 years old as human disturbance
LogYear<-readRDS(file = 'tmp/AOI/LogYear')
recLY<-c(2018-20,2018,2, 1915,2018-20,0)
LogYearClassed<-reclassify(LogYear, rcl=matrix(recLY,ncol=3,byrow=TRUE), right=FALSE, include.lowest=TRUE) %>%
  crop(AOI)
writeRaster(LogYearClassed, filename=file.path(spatialOutDir,paste("LogYearClassed",sep="")), format="GTiff",overwrite=TRUE)

#Process Linear features and put a 100m buffer on them
#First rasterize roads
roads_keep<-readRDS(file = 'tmp/AOI/roads_keep')
road_aoi1001 <- st_buffer(roads_keep, dist = 100)
saveRDS(road_aoi1001, file = 'tmp/AOI/road_aoi1001')
#write_sf(road_aoi1001, file.path(spatialOutDir,"road_aoi1001.gpkg"))
road_aoi100 <- road_aoi1001 %>%
  st_as_sf() %>%
  st_cast("MULTIPOLYGON")
saveRDS(road_aoi100, file = 'tmp/AOI/road_aoi100')
RoadR<-fasterize(road_aoi100, ProvRast, background = 0)
writeRaster(RoadR, filename=file.path(spatialOutDir,paste("RoadR",sep="")), format="GTiff",overwrite=TRUE)

RailRoads<-readRDS(file = 'tmp/AOI/RailRoads')
HydroTransmission<-readRDS(file = 'tmp/AOI/HydroTransmission')
Pipe<-readRDS(file = 'tmp/AOI/Pipe')
LinearF<- Reduce("+",list(RoadR, RailRoads, HydroTransmission, Pipe))

LinearF[LinearF >= 1]<-4
writeRaster(LinearF, filename=file.path(spatialOutDir,paste("LinearF",sep="")), format="GTiff",overwrite=TRUE)
#Use gridDistance function to create a surface of distance from road in meters
LinearF_gd<-gridDistance(LinearF,origin=1)
#reclass all within 100m to
#LinearF_gd[LinearF_gd >=100]<-0
linear_rcl <- c(0,100,3,100,1000000,0)
LinearDisturb <-reclassify(LinearF_gd, rcl=  matrix(linear_rcl,ncol=3,byrow=TRUE)
                           , right=FALSE, include.lowest=TRUE)  %>%
  crop(AOI)

writeRaster(LinearDisturb, filename=file.path(spatialOutDir,paste("LinearDisturb",sep="")), format="GTiff",overwrite=TRUE)

#Human Disturbance - set to 1 where present
ExtensiveFootprint<-readRDS(file = 'tmp/AOI/ExtensiveFootprint')
#divide up into fully allienated (-10 to -5 and -1 - urban, etc = 2), or
#partially (-4 to -2, agriculture, recreation=1)
ExtensiveFootprint[(ExtensiveFootprint >=-10 & ExtensiveFootprint<=-5) |  ExtensiveFootprint==-1] <-4
ExtensiveFootprint[(ExtensiveFootprint >=-4 & ExtensiveFootprint<=-2) ] <-3

#Combine human disturbance + linear buffer + fires <20 + logggin disturbance <20
#human disturbed is 1 from extensive footprint or from linear
#2 for agriculture and rural
#3 for Logging <20
#4 for Fire <20
LandDisturb<-max(ExtensiveFootprint, LinearDisturb, LogYearClassed, Fire20YearR, na.rm=TRUE)

#pull in wetlands and assign to nearest neighbour.
#Wetlands<-readRDS(file = 'tmp/AOI/Wetlands')
#WetlandsR<- fasterize(st_collection_extract(Wetlands, "POLYGON"),ProvRast,background=0) %>%
#  crop(AOI)
#WetlandsR[WetlandsR==1]<-NA
#writeRaster(WetlandsR, filename=file.path(spatialOutDir,"WetlandsR.tif"), format="GTiff", overwrite=TRUE)

#1 buffer wetland approach
Wetlands<-readRDS(file = 'tmp/AOI/Wetlands')

WetlandsB<-st_buffer(Wetlands, dist=200) %>%
  st_collection_extract("POLYGON")
write_sf(WetlandsB, file.path(spatialOutDir,"WetlandsB200.gpkg"))

#Take max disturbance and assign to wetland, such if any urban then urban disturbance, etc
#alternative is to take most common - but likley larger effect if more severe disturbance
#WetlandsE1 <- raster::extract(LandDisturb, WetlandsB, sp=TRUE)
#WetlandsE3 <- exact_extract(LandDisturb, WetlandsB2) - returns propotion of each
Wetlands_E <- data.frame(DisturbCode=exact_extract(LandDisturb, WetlandsB, 'max'))
Wetlands_E$wet_id <-as.numeric(rownames(Wetlands_E))
Wetlands_LD <- Wetlands %>%
  mutate(wet_id=as.numeric(rownames(WetlandsB))) %>%
  left_join(Wetlands_E)
write_sf(Wetlands_LD, file.path(spatialOutDir,"Wetlands_LD.gpkg"))

# Put wetlands into the disturbance layer as NA for filling
#LandDisturbToFill <- max(LandDisturb,WetlandsR)
#writeRaster(LandDisturbToFill, filename=file.path(spatialOutDir,"LandDisturbToFill.tif"), format="GTiff", overwrite=TRUE)
#LandDisturbToFill<-raster(file.path(spatialOutDir,"LandDisturbToFill.tif"))

#Originally did modal of neighbourhood - most common
#changed to any disturbance adjacent to wetland
#fill.na <- function(x, i=13) {
#  if( is.na(x)[i] ) {
#    #return( modal(x, ties='highest',na.rm=TRUE))
#    return( sum(x, na.rm=TRUE))
#  } else {
#    return( round(x[i],0) )
#  }
#}
#Pass the fill.na function to raster::focal and check results.
#The pad argument creates virtual rows/columns of NA values to keep the
#vector length constant along the edges of the raster.
#This is why we can always expect the fifth value of the vector to be
#the focal value in a 3x3 window thus, the index i=5 in the fill.na function.
#Do the fill twice to nibble into large lakes sufficient to assign areas
#where wetlands may occur to their largest neighbour

#LandDisturbFilled2 <- focal(LandDisturbToFill, w = matrix(1,5,5), fun = fill.na,
#                           pad = TRUE, na.rm = FALSE )
#writeRaster(LandDisturbFilled2, filename=file.path(spatialOutDir,"LandDisturbFilled2.tif"), format="GTiff", overwrite=TRUE)
#LandDisturbFilled<-raster(file.path(spatialOutDir,"LandDisturbFilled2.tif"))
####
#Read in the point coverage of wetland centroids
#waterpt<-st_read(file.path(spatialOutDir,"waterptRoad.gpkg"))
#waterpt<-st_read(file.path(spatialOutDir,"waterpt.gpkg"))

#extract the raster value from the Land cover map
#disturb_pts <- raster::extract(LandDisturbFilled, waterpt, sp=TRUE) %>%
# st_as_sf() %>%
#  dplyr::rename(DisturbCode=LandDisturbFilled)

#Look up the values in a table and make a list
LandDisturb_LUT <- data.frame(DisturbCode=c(0,1,2,3,4),
                    DisturbType=c('undisturbed','Fire','Logging','Moderate_Agriculture','Severe_Urban_roads'))

# make a list of unique land types
disturb.ls <- Wetlands_LD %>%
  st_drop_geometry() %>%
  left_join(LandDisturb_LUT) %>%
  dplyr::select(Wetland_Co, wet_id, DisturbCode, DisturbType)
#unique(disturb.ls$DisturbType)
WriteXLS(disturb.ls,file.path(dataOutDir,paste('disturb.ls.xlsx',sep='')))

# If we want to sample 10 sites first we need to calculate the proportion of sites
# to sample within each variant

prop.site <- disturb.ls %>%
  group_by(DisturbType)%>%
  dplyr::summarise(no.pts = n()) %>%
  #st_drop_geometry() %>%
  mutate(perc = ceiling(no.pts / sum(no.pts)*100))

#WriteXLS(prop.site,file.path(dataOutDir,paste('ESILandDisturbxWetland.xlsx',sep='')))
WriteXLS(prop.site,file.path(dataOutDir,paste('ESI_Wetland_Strata_Disturb.xlsx',sep='')),SheetNames='Disturbance')

gc()

