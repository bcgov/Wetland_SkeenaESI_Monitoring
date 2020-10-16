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

#ClimateBC
# ASC file type methods
#Write a smaller sub set as a test
ws_AOI <- readRDS(file = 'tmp/ws') %>%
  filter(SUB_SUB_DRAINAGE_AREA_NAME == "Bulkley")

DEM_cBC <-DEM %>%
  crop(ws_AOI) %>%
  projectRaster(crs=CRS("+init=epsg:4326"))

rgdal::writeGDAL(as(DEM_cBC, "SpatialGridDataFrame"),
                 file.path(spatialOutDir,"DEMt_cBC.asc"),
                 drivername = "AAIGrid", mvFlag=-9999)

#Method 2 make a csv of the lat, lons of each wetland
#Get the centroid of the wetlands
wetlandsXY <- st_centroid(Wetlands)

#ClimateBC wants elevation so extract from DEM at wetland centroids
#and transform geometry to the crs expected by ClimateBC
wetlandsXYDEM <- DEM %>%
  raster::extract(wetlandsXY) %>%
  cbind(wetlandsXY) %>%
  st_transform(crs=4326)

#Get x, y coordinates - long and lat
wetpt <- st_coordinates(wetlandsXYDEM)
wetpt <- wetlandsXYDEM %>%
  cbind(wetpt) %>%
  st_drop_geometry() %>%
  dplyr::select(Id1=Wetland_Co,Id2=wet_id, long=Y, lat=X, elev='.')

#write file to csv - change line feed from unix style to windows otherwise ClimateBC errors
write_csv(wetpt, path=file.path(dataOutDir, 'ESIwetpt.csv'))

#Process file on Windows machine using ClimateBC and read back in
#Annual Variables
ESIclimate<-read_csv(file=file.path(DataDir,'ESIwetpt_Normal_1961_1990Y.csv')) %>%
  dplyr::rename(Wetland_Co=Id1) %>%
  dplyr::select(Wetland_Co, Latitude, Longitude, Elevation, CMD, DD5)
#Seasonal Variables
ESIclimateS<-read_csv(file=file.path(DataDir,'ESIwetpt_Normal_1961_1990S.csv')) %>%
  dplyr::rename(Wetland_Co=Id1) %>%
  mutate(Rad=Rad_sp+Rad_sm) %>%
  dplyr::select(Wetland_Co, Latitude, Longitude, Elevation, Rad_sp, Rad_sm, Rad)

#CMD		Hargreaves climatic moisture deficit (mm)
#DD>5		degree-days above 5°C, growing degree-days
#sum:
#RAD_sp	spring solar radiation (MJ m‐2 d‐1)
#RAD_sm	summer solar radiation (MJ m‐2 d‐1)

#VRI data to check - site_position_meso
wetXY<-st_join(wetlandsXY, vri, join=st_intersects) %>%
  dplyr::select(Wetland_Co, vri_FEATURE_ID=FEATURE_ID, SITE_POSITION_MESO)

#pull landform to wetland and compare
wetlandsXYLF <- LForm %>%
  raster::extract(wetXY) %>%
  cbind(wetXY) %>%
  dplyr::rename(LFcode = '.') %>%
  left_join(LForm_LUT) %>%
  dplyr::select(Wetland_Co, vri_FEATURE_ID, SITE_POSITION_MESO, LFcode, Landform)

wetCompare <- wetlandsXYLF %>%
  mutate(Group=group_indices(.,Landform, SITE_POSITION_MESO)) %>%
  group_by(Landform, SITE_POSITION_MESO) %>%
  dplyr::summarise(nWet=n())

table.df <- wetlandsXYLF %>%
  st_drop_geometry() %>%
  tabyl(Landform, SITE_POSITION_MESO)

#C Crest - The generally convex uppermost portion of a hill (meso scale). It is usually convex in all directions and generally has no distinct aspect. The term "crest" may also be applied to a ridge.
#U Upper slope- The generally convex, upper portion of the slope of a hill (meso scale) immediately below the crest. It has a convex surface profile with a specific aspect.
#M Middle slope- The area of the slope of a hill between the upper and lower slope, where the slope profile is not generally concave or convex. It has a straight or somewhat sigmoid surface profile with a specific aspect.
#L Lower slope - The area toward the base of the slope of the hill. It generally has a concave surface profile with a specific aspect.
#T Toe - The area differentiated from the lower slope by an abrupt decrease in slope gradient. It is often characterized by seepage.
#D Depression - Any area that is concave in all directions. It is generally at the foot of a meso scale hill or in a generally level area.
#F Flat (Level) - Any level area not immediately adjacent to a meso scale hill (or toe). The surface profile is generally horizontal with no significant aspect.
#  Decent correlation, but 11993 cases are NA - 23%

#Combine ClimateBC variables, Landform and SITE_POSITION_MESO and output gpkg and csv
Wetlands_ClimateLandForm <- wetlandsXYLF %>%
  left_join(ESIclimate, by = "Wetland_Co") %>%
  left_join(ESIclimateS, by = "Wetland_Co") %>%
  dplyr::select(Wetland_Co, Elevation.x, Rad_sp, Rad_sm, Rad, CMD, DD5,
                vri_FEATURE_ID, SITE_POSITION_MESO, LFcode, Landform)
gc()
