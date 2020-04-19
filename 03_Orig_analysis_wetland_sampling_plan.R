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

source('header.R')

#Read in cleaned wetland plot data
site <- read_excel(file.path(dataOutDir,'WetPlots.xlsx'), 1)
plot <- read_excel(file.path(dataOutDir,'WetPlots.xlsx'), 2)
veg <- read_excel(file.path(dataOutDir,'WetPlots.xlsx'), 3)

# check the data
site
plot
veg

# subset ws data set to include only the drainages in which the wetland plots
# occur. First we can use an intersect to see what is the name of the polgons
# (sub drainages) in which the points fall.
study_area_drain <- ws %>% st_intersection(plot)

# note this is point data with a new column added
# we can use this column to then subset our drainage polygon
ws_gps <- ws %>%
  filter(SUB_SUB_DRAINAGE_AREA_NAME %in%
          study_area_drain$SUB_SUB_DRAINAGE_AREA_NAME)

# For testing will use one of the sub sub drainage areas for
# faster processing. This will be our area of interest (AOI)

AOI <- ws %>%
  filter(SUB_SUB_DRAINAGE_AREA_NAME == "Bulkley")

# we only want to keep the columns with road class and surface type
roads <- roads_sf %>%
  select(c(ROAD_CLASS, ROAD_SURFACE, FILE_TYPE_DESCRIPTION))

roads.Descrip.sum <- roads %>%
  #filter(!is.na(ROAD_CLASS)) %>%
  st_drop_geometry() %>%
  group_by(FILE_TYPE_DESCRIPTION) %>%
  dplyr::summarise(count = n(), na.rm=TRUE)

# lets see what types of roads we have
roads.class.sum <- roads %>%
  #filter(!is.na(ROAD_CLASS)) %>%
  st_drop_geometry() %>%
  group_by(ROAD_CLASS) %>%
  dplyr::summarise(count = n(), na.rm=TRUE)

# lets check the road surface types
roads.surf.sum <- roads %>%
  st_drop_geometry() %>%
  group_by(ROAD_SURFACE) %>%
  dplyr::summarise(count = n())

roads_NA <- roads %>%
  filter(is.na(ROAD_CLASS))

# lets keep only the main types of roads and not include overgrown surface
roads_keep <- roads %>%
  filter(ROAD_CLASS %in% c("highway", "local", "unclassifed","resource"))%>%
  filter(ROAD_SURFACE != "overgrown")

## see what this looks like
# mapview(roads)
# plot(st_geometry(roads))

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

wetlandsXY <- st_centroid(wetlands)
wetpt <- st_coordinates(wetlandsXY)
wetpt <- wetlandsXY %>%
  cbind(wetpt) %>%
  st_drop_geometry()

waterpt <- bind_rows(wbpt, wetpt )
waterpt <- st_as_sf(waterpt, coords= c("X","Y"), crs = 3005)

mapview(waterpt)


# Part 2: Stratified random sampling of wetlands  ---------------------------------------------

# lets now create a random sample of wetlands
# the criteria we want to work with
#       - wetlands must be within X distance to a road
#       - create random points within the bec zones ( )
#       - dont want to sample where we already have data


# lets create a buffer around the roads and then determine which points fall within the
# buffer

mapview(roads)

roads_b1000 <- st_buffer(roads, dist = 1000) %>% st_union()

roads_b50 <- st_buffer(roads, dist = 50) %>% st_union()

mapview(roads) + mapview(roads_b1000)+ mapview(roads_b50)

# now lets get the area more than 50 m and within 1000km

road_aoi <- st_difference(roads_b1000,  roads_b50)

mapview(road_aoi) + mapview(waterpt)

# now lets keep only the wetlands that fall within our given distance from the road
# this might take some time (as it is a large data set)

if(file.exists(file.path("demo","waterpt.rds"))) {
  waterpt <- readRDS(file = file.path("demo","waterpt.rds"))

} else {

  waterpt <-st_intersection(waterpt, road_aoi)

  saveRDS(waterpt, file = "demo/waterpt.rds")
}

waterpt

# We now have 1842 wetlands!


# we can now eliminate any points that have already been sampled - lets buffer out sites by 500m
# to ensure no overlap with new points

plot_exclude <- st_buffer(plot, dist = 500)

overlap_pts <- st_intersection(waterpt, plot_exclude)

waterpt <- waterpt %>%
  filter(!WATERBODY_POLY_ID %in% overlap_pts$WATERBODY_POLY_ID)


# We can now randomly select using the bec types to stratify first let us intersect the
# point to add the bec zone name and id

bec_pts <- st_intersection(waterpt, bec_sf)

# make a list of unique bec variants
bgc.ls <- as.list(unique(bec_pts$MAP_LABEL))


# If we want to sample 100 sites first we need to calculate the proportion of sites
# to sample within each variant

prop.site <- bec_pts %>%
  group_by(MAP_LABEL)%>%
  summarise(no.pts = n()) %>%
  st_drop_geometry() %>%
  mutate(perc = ceiling(no.pts / sum(no.pts)*100))


out <- lapply(bgc.ls, function(x) {
  no.pts <- prop.sites %>%
    filter(MAP_LABEL == x) %>%
    select(perc) %>%
    pull
  sdata <- bec_pts  %>%  filter(MAP_LABEL == x)
  sample_n(sdata, no.pts)
})

out <- do.call("rbind", out)

mapview(out) + mapview(plot)



# write out as csv

write.csv(out, file = "demo/wetland_sample.csv")


# Write out a KML file ----------------------------------------------------

# Now we can write out a KML file so we can
# view this is google earth. First we need to convert to a sp object.

out_sp <- as(out, "Spatial")

# write out a kml
kml(out_sp,
    file.name    = file.path("demo/wetland_points.kml"),
    points_names = out_sp$MAP_LABEL,
    colour    = "#FF0000",
    alpha     = 0.6,
    size      = 1,
    shape     = "http://maps.google.com/mapfiles/kml/pal2/icon18.png")


