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

# Requires layers - bec_sf, waterpt - clean clips to AOI

#Read in BEC_LUT to cast becs as groups from Doug Lewis/BEC program lookup table
BECgroupSheets<- excel_sheets(file.path(DataDir,'BECv11_SubzoneVariant_GroupsVESI_V3.xlsx'))
BECgroupSheetsIn<-read_excel(file.path(DataDir,'BECv11_SubzoneVariant_GroupsVESI_V3.xlsx'),
                             sheet = BECgroupSheets[2])

BECGroup_LUT<-data.frame(VARns=BECgroupSheetsIn$`BEC Unit`,
                         BECgroup=BECgroupSheetsIn$GROUP, stringsAsFactors = FALSE)

#Read in bec layer and join LUT
bec_sf<-readRDS(file = 'tmp/AOI/bec_sf')

BECg<- bec_sf %>%
  mutate(VARns=MAP_LABEL) %>%
  left_join(BECGroup_LUT)

#Reduce/aggregate to bec groups
bec_g <- BECg %>%
  mutate(sumbec=1) %>%
  dplyr::group_by(BECgroup) %>%
  dplyr::summarise(nbecs = sum(sumbec))

#Data checking -
#plot(bec_g[1])
#ubecs<-unique(bec_g$BECgroup)

#Based on inspection, aggregate some of the rarer groups to make a LUT
#re-join and generate a second version of bec groups
#BECGroup_LUT2<-data.frame(BECgroup=ubecs,
##                   BECgroup2=c(ubecs[1:7],ubecs[7],ubecs[6],ubecs[3],ubecs[11:12],ubecs[12],ubecs[4]))

#bec_g2<-bec_g %>%
#  left_join(BECGroup_LUT2) %>%
#  mutate(sumbec2=1) %>%
#  dplyr::group_by(BECgroup2) %>%
#  dplyr::summarise(nbecs = sum(sumbec2))
#plot(bec_g2[1])
#bec_g<-bec_g2

#save the aggregated becs
write_sf(bec_g, file.path(spatialOutDir,"bec_g.gpkg"))

# join the bec and waterpts and drop geometry
#Read in the point coverage of wetland centroids first
waterpt<-st_read(file.path(spatialOutDir,"waterpt.gpkg"))
#waterpt<-st_read(file.path(spatialOutDir,"waterptRoad.gpkg"))

#bec_pts2 <- st_intersects(waterpt, bec_g) %>%
bec_pts <- st_intersection(waterpt, bec_g) %>%
#write_sf(bec_pts, file.path(spatialOutDir,"bec_pts.gpkg"))
  st_drop_geometry() %>%
  dplyr::select(Wetland_Co, BEC=BECgroup)

WriteXLS(bec_pts,file.path(dataOutDir,paste('bec_pts.xlsx',sep='')))

# make a list of unique bec variants
bgc.ls <- as.list(unique(bec_pts$BEC))

# generate a list summarizing bec groups, and the number and % of wetlands, then save
prop.site <- bec_pts %>%
  group_by(BEC)%>%
  dplyr::summarise(no.pts = n()) %>%
  mutate(perc = ceiling(no.pts / sum(no.pts)*100))

WriteXLS(prop.site,file.path(dataOutDir,paste('ESI_Wetland_Strata_BEC.xlsx',sep='')),SheetNames='BEC')

gc()
