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

# Analysis generates a Land Type strata
# first, it modifies forested portion and designates as conifer, mixed, deciduous/shrub
# second, first it uses the LandCoverAge coverage and nibbles into water/wetland features and
# assigns to largest neighbourhood

source ('header.R')
ESI<-readRDS(file = 'tmp/ESI')
AOI<-ESI

#### First part ####
#Read the VRI layer and fasterize the VRI$BCLCS_LEVEL_4 the selected attributes
#TC = Treed - Coniferous
#TB =  Treed - Broadleaf
#TM =  Treed - Mixed

#Need to figure out decid, confi, or mix from VRI - bclcs_level_4
Tree_LUT<-data.frame(Tree=c('TC','TB','TM'),Tcode=c(1,2,3))

vri<-readRDS(file = 'tmp/AOI/vri')
Trees<- vri %>%
  filter(BCLCS_LEVEL_4 %in% c('TC','TB','TM')) %>%
  dplyr::select(Tree=BCLCS_LEVEL_4) %>%
  left_join(Tree_LUT) %>%
  st_cast("MULTIPOLYGON")
#write_sf(Trees, file.path(spatialOutDir,"Trees.gpkg"))

ProvRast<-readRDS(file='tmp/ProvRast')
TreesR<- fasterize(Trees,ProvRast,field='Tcode') %>%
  crop(ESI)
TreesR[is.na(TreesR)] <- 0

writeRaster(TreesR, filename=file.path(spatialOutDir,"TreesR.tif"), format="GTiff", overwrite=TRUE)

# Do a similar draw pulling the other land cover veg attributes from BCLCS_LEVEL_4
# fasterize
Veg_LUT=data.frame(Veg=c('ST','SL','HE','HF','HG','BY','BM','BL'),Vcode=c(4,5,6,7,8,9,10,11))

Veg<- vri %>%
  filter(BCLCS_LEVEL_4 %in% c('ST','SL','HE','HF','HG','BY','BM','BL')) %>%
  dplyr::select(Veg=BCLCS_LEVEL_4) %>%
  left_join(Veg_LUT) %>%
  st_cast("MULTIPOLYGON")
write_sf(Veg, file.path(spatialOutDir,"Veg.gpkg"))

VegR<- fasterize(Veg,ProvRast,field='Vcode') %>%
 crop(ESI)
VegR[is.na(VegR)] <- 0
writeRaster(VegR, filename=file.path(spatialOutDir,"VegR.tif"), format="GTiff", overwrite=TRUE)

#Join trees and veg making non-tree deciduous
VegR[VegR>0]<-3
VegTree<-VegR+TreesR
writeRaster(VegTree, filename=file.path(spatialOutDir,"VegTree.tif"), format="GTiff", overwrite=TRUE)

#### Second Part #####
#Set all water to NA so can be filled
LandCoverAndAge<-readRDS(file = 'tmp/AOI/LandCoverAndAge')
LandCoverAndAge[LandCoverAndAge %in% c(-28,-27,-23,-22,-21,-20)] <- NA

# Join the landcover, trees and veg together - replacing the >0 raster values
# in the LandCoverAndAge raster with the TreesR and VegR rasters
LandCoverAndAge[LandCoverAndAge>0 | VegTree>0] <- 0
LCstrata<-sum(VegTree, LandCoverAndAge)

#Fill in water with larget neighbour
#Set all water to NA so can be filled
LCstrata[LCstrata %in% c(-28,-27,-23,-22,-21,-20)] <- NA

#Function to nibble into water features to ensure wetlands land in a non-water
#dominant adjacent feature - inspired by this link https://stackoverflow.com/questions/36960974/how-to-replace-raster-values-less-than-0-to-na-in-r-code/49159943
#Fill with highest value - will be oldest forest or if non-forest then human disturbed, then shrubs
fill.na <- function(x, i=5) {
  if( is.na(x)[i] ) {
    return( modal(x, ties='highest',na.rm=TRUE))
  } else {
    return( round(x[i],0) )
  }
}
#Pass the fill.na function to raster::focal and check results.
#The pad argument creates virtual rows/columns of NA values to keep the
#vector length constant along the edges of the raster.
#This is why we can always expect the fifth value of the vector to be
#the focal value in a 3x3 window thus, the index i=5 in the fill.na function.
#Do the fill twice to nibble into large lakes sufficient to assign areas
#where wetlands may occur to their largest neighbour

LCstrataFilled <- focal(LCstrata, w = matrix(1,3,3), fun = fill.na,
                               pad = TRUE, na.rm = FALSE ) %>%
  focal(w = matrix(1,3,3), fun = fill.na,pad = TRUE, na.rm = FALSE )
writeRaster(LCstrataFilled, filename=file.path(spatialOutDir,"LCstrataFilled.tif"), format="GTiff", overwrite=TRUE)

#Read in the point coverage of wetland centroids
#waterpt<-st_read(file.path(spatialOutDir,"waterptRoad.gpkg"))
waterpt<-st_read(file.path(spatialOutDir,"waterpt.gpkg"))

#extract the raster value from the Land cover map
lt_pts <- raster::extract(LCstrataFilled, waterpt, sp=TRUE) %>%
  st_as_sf() %>%
  dplyr::rename(LandCoverCode=layer)

#Look up the values in a table and make a list
LandCover_LUT <- read_excel(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/LUT',
                 'LandCoverLookUp_LUT.xlsx'),sheet=1)[1:21,] %>%
                  rbind(data.frame(LanCoverLabel=c('UForest','Conifer','Broad Leaf','Mixed'), LandCoverCode=c(0,1,2,3)))

# make a list of unique land types
lt.ls <- lt_pts %>%
  st_drop_geometry() %>%
  left_join(LandCover_LUT) %>%
  dplyr::select(Wetland_Co, wet_id, LandCoverCode, LanCoverLabel)

WriteXLS(lt.ls,file.path(dataOutDir,paste('ltls.xlsx',sep='')))

## generate a list summarizing land types, and the number and % of wetlands, then save
unique(lt.ls$LanCoverLabel)
prop.site <- lt.ls %>%
  group_by(LanCoverLabel)%>%
  dplyr::summarise(no.pts = n()) %>%
  #st_drop_geometry() %>%
  mutate(perc = ceiling(no.pts / sum(no.pts)*100))

#WriteXLS(prop.site,file.path(dataOutDir,paste('ESILandTypexWetland.xlsx',sep='')))
WriteXLS(prop.site,file.path(dataOutDir,paste('ESI_Wetland_Strata_LT.xlsx',sep='')),SheetNames='LandType')

gc()
