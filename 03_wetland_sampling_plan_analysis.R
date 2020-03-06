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

# Original written by genevieve perkins (genevieve.perkins@gov.bc.ca)
# modified by Don Morgan (don.morgan@gov.bc.ca) to apply to ESI wetland program

# requires : WetPlots.xlsx

#Add using existing to meet sample criteria ie identify gaps not sampled
#Add other strata
#Add lake adjacency

source('header.R')

#Read in cleaned wetland plot data
site_data <- read_excel(file.path(dataOutDir,'WetPlots.xlsx'), 1)
plot_data <- read_excel(file.path(dataOutDir,'WetPlots.xlsx'), 2)
veg_data <- read_excel(file.path(dataOutDir,'WetPlots.xlsx'), 4)

#Join wetland spatial with plot data to spatially identify wetland
#some issues with GPS locations in field card missing or not neccessarily correct UTM issues, miss record
#Pull only wetlands that have plots
wet_site<- Wetlands %>%
  mutate(newid = Wetland_Co) %>%
  inner_join(site_data)

#join to site data but pull all wetlands
wet_site_all<- Wetlands %>%
  mutate(newid = Wetland_Co) %>%
  left_join(site_data)

#####Not sure if I need this section:
# subset ws data set to include only the drainages in which the wetland plots
# occur. First we can use an intersect to see what is the name of the polgons
# (sub drainages) in which the points fall.
#study_area_drain <- ws %>% st_intersection(plot)

# note this is point data with a new column added
# we can use this column to then subset our drainage polygon
#ws_gps <- ws %>%
#  filter(SUB_SUB_DRAINAGE_AREA_NAME %in%
#          study_area_drain$SUB_SUB_DRAINAGE_AREA_NAME)



# lets keep only the theoretically drivable
roads <- roads_sf %>%
 filter(ROAD_SURFACE %in% c("loose", "paved", "rough"))

## see what this looks like
# mapview(roads_keep)
# plot(st_geometry(roads_keep))

mapview(waterbodies) +
mapview(Streams) +
mapview(Wetlands)

# lets select only the smallest lakes
waterbodies <- waterbodies %>%
  select(c(WATERBODY_POLY_ID, WATERBODY_TYPE, AREA_HA, GNIS_NAME_1)) %>%
  filter(AREA_HA < 0.5)

wetlands <- wetlands %>% # lakes
  select(c(WATERBODY_POLY_ID, WATERBODY_TYPE, AREA_HA, GNIS_NAME_1))


# lets get the centroid of each of the waterbodies and wetlands
waterbodiesXY <- st_centroid(waterbodies)
wbpt <- st_coordinates(waterbodiesXY)
wbpt <- waterbodiesXY %>%
  cbind(wbpt)%>%
  st_drop_geometry()

wetlandsXY <- st_centroid(Wetlands)
wetpt <- st_coordinates(wetlandsXY)
wetpt <- wetlandsXY %>%
  cbind(wetpt) %>%
  st_drop_geometry()

#Not sure if neeed lakes???
#waterpt <- bind_rows(wbpt, wetpt )
waterpt <- bind_rows(wetpt)
waterpt <- st_as_sf(waterpt, coords= c("X","Y"), crs = 3005)

#Make points for already sampled plots
wetlandsSXY <- st_centroid(wet_site)
wetptS <- st_coordinates(wetlandsSXY)
wetptS <- wetlandsSXY %>%
  cbind(wetptS) %>%
  st_drop_geometry()

wetSpt <- st_as_sf(wetptS, coords= c("X","Y"), crs = 3005)


mapview(waterpt)

# Part 2: Stratified random sampling of wetlands  ---------------------------------------------

# lets now create a random sample of wetlands
# the criteria we want to work with
#       - wetlands must be within X distance to a road
#       - create random points within the bec zones ( )
#       - dont want to sample where we already have data


# lets create a buffer around the roads and then determine which points fall within the
# buffer

#mapview(roads)

roads_b1000 <- st_buffer(roads, dist = 1000) %>% st_union()

roads_b50 <- st_buffer(roads, dist = 50) %>% st_union()

mapview(roads) + mapview(roads_b1000)+ mapview(roads_b50)

# now lets get the area more than 50 m and within 1000km
road_aoi <- st_difference(roads_b1000,  roads_b50)

mapview(road_aoi) + mapview(waterpt)

# now lets keep only the wetlands that fall within our given distance from the road
# this might take some time (as it is a large data set)

if(file.exists(file.path("tmp/waterpt.rds"))) {
  waterpt <- readRDS(file = file.path("tmp/waterpt.rds"))
} else {
  waterpt <-st_intersection(waterpt, road_aoi)

  saveRDS(waterpt, file = "tmp/waterpt.rds")
}

waterpt

# We now have 1842 wetlands!


# we can now eliminate any points that have already been sampled - lets buffer out sites by 500m
# to ensure no overlap with new points

plot_exclude <- st_buffer(wetSpt, dist = 500)

overlap_pts <- st_intersection(waterpt, plot_exclude)

waterpt <- waterpt %>%
  filter(!Wetland_Co %in% overlap_pts$Wetland_Co)


# We can now randomly select using the bec types to stratify first let us intersect the
# point to add the bec zone name and id

bec_pts <- st_intersection(waterpt, bec_sf)

# make a list of unique bec variants
bgc.ls <- as.list(unique(bec_pts$MAP_LABEL))


# If we want to sample 10 sites first we need to calculate the proportion of sites
# to sample within each variant

prop.site <- bec_pts %>%
  group_by(MAP_LABEL)%>%
  dplyr::summarise(no.pts = n()) %>%
  st_drop_geometry() %>%
  mutate(perc = ceiling(no.pts / sum(no.pts)*100))


out <- lapply(bgc.ls, function(x) {
  no.pts <- prop.site %>%
    filter(MAP_LABEL == x) %>%
    select(perc) %>%
    pull
  sdata <- bec_pts  %>%  filter(MAP_LABEL == x)
  sample_n(sdata, no.pts)
})

out <- do.call("rbind", out)

mapview(out) + mapview(bec_sf, zcol='MAP_LABEL') + mapview(Wetlands)



# write out as csv

write.csv(out, file = file.path(dataOutDir,"wetland_sample.csv"))


# Write out a KML file ----------------------------------------------------

# Now we can write out a KML file so we can
# view this is google earth. First we need to convert to a sp object.

out_sp <- as(out, "Spatial")

# write out a kml
kml(out_sp,
    file.name    = file.path(spatialOutDir,"wetland_points.kml"),
    points_names = out_sp$MAP_LABEL,
    colour    = "#FF0000",
    alpha     = 0.6,
    size      = 1,
    shape     = "http://maps.google.com/mapfiles/kml/pal2/icon18.png")



#####Other Data
#Join WetlandFnData to spatial wetland data on 'NewID' and 'Wetland_Co'
WetPlotFnData$Wetland_Co<-as.numeric(levels(WetPlotFnData$NewID))[WetPlotFnData$NewID]

Wetland_Fn<-Wetlands %>%
  left_join(WetPlotFnData, by=c('Wetland_Co')) %>%
  dplyr::filter(!is.na(NewID))


