## Demo 1.

# determine sampling points for a fish monitoring survey

source('header.R')

# bring in BC boundary
bc <- bcmaps::bc_bound()

ESI_file <- file.path("tmp/ESI")
if (!file.exists(ESI_file)) {
#Load ESI boundary
ESIin <- read_sf(file.path(ESIDir,'Data/Skeena_ESI_Boundary'), layer = "ESI_Skeena_Study_Area_Nov2017") %>%
  st_transform(3005)
ESI <- st_cast(ESIin, "MULTIPOLYGON")
saveRDS(ESI, file = ESI_file)

#For data download use filtered ws as a subset
study_area <- ESI

#Load ESI supporting wetland data
Wet_gdb <-file.path(WetspatialDir,'Wetland_Assessment_Level1_InputData.gdb')
wet_list <- st_layers(Wet_gdb)

#Load ESI Wetlands
WetW_gdb <-file.path(WetspatialDir,'Wetland_T1','Skeena_ESI_T1_Wetland_20191219.gdb')
#wet_list <- st_layers(Wet_gdb)
Wetlands <- readOGR(dsn=WetW_gdb, layer = "Skeena_ESI_T1_Wetland_20191219") %>%
  as('sf')
st_crs(ws)<-3005
saveRDS(Wetlands, file = 'tmp/Wetlands')


#Make a version that is data only
Wetland_data<- Wetlands
st_geometry(Wetland_data) <- NULL

# 1. Water Drainage data
#  read in the water drainage data set to set up and AOI:
# https://catalogue.data.gov.bc.ca/dataset/water-survey-of-canada-sub-sub-drainage-areas

# download the water drainage and use as an area of interest using the bcdata package
#ws <- get_layer("wsc_drainages", class = "sf") %>%
#  select(SUB_DRAINAGE_AREA_NAME, SUB_SUB_DRAINAGE_AREA_NAME) %>%
#  filter(SUB_DRAINAGE_AREA_NAME %in% c("Nechako", "Skeena - Coast"))

ws <- get_layer("wsc_drainages", class = "sf") %>%
  select(SUB_DRAINAGE_AREA_NAME, SUB_SUB_DRAINAGE_AREA_NAME) %>%
  st_intersection(study_area)
st_crs(ws)<-3005
saveRDS(ws, file = 'tmp/ws')

#mapview(ws) + mapview(ESI)

## plot the data to verify locations
#plot(ws["SUB_DRAINAGE_AREA_NAME"], key.pos = NULL)
#mapview::mapview(ws)

#FWA_Streams
Streams <- read_sf(Wet_gdb, layer = "FWA_Streams")
st_crs(Streams) <- 3005
saveRDS(Streams, file = 'tmp/Streams')

#FWA_Lakes
waterbodies <- read_sf(Wet_gdb, layer = "FWA_Lakes")
st_crs(waterbodies) <- 3005
saveRDS(waterbodies, file = 'tmp/waterbodies')

# read in the VRI data
vri <- read_sf(Wet_gdb, layer = 'VRI_LYRR1_181128')
st_crs(vri) <- 3005
saveRDS(vri, file = 'tmp/vri')

#First Nation house groups/wilps/yinta
ESI_OW <- read_sf(file.path(ESIDir,'Data/Library/FN'), layer = "Wetsuweten_House") %>%
  st_transform(3005)
saveRDS(ESI_OW, file='tmp/ESI_OW')
ESI_LBN <- read_sf(file.path(ESIDir,'Data/Library/FN'), layer = "LBN_subwatershed") %>%
  st_transform(3005)
saveRDS(ESI_LBN, file='tmp/ESI_LBN')
ESI_Gitxsan <- read_sf(file.path(ESIDir,'Data/Library/FN'), layer = "gitxsan_houses_v5") %>%
  st_transform(3005)
saveRDS(ESI_Gitxsan, file='tmp/ESI_Gitxsan')
ESI_Gitanyow <- read_sf(file.path(ESIDir,'Data/Library/FN'), layer = "Gitanyow_Houses") %>%
  st_transform(3005)
saveRDS(ESI_Gitanyow, file='tmp/ESI_Gitanyow')

# Download BEC - # Gets bec_sf zone shape and filters the desired subzones
bec_sf <- bec(class = "sf") %>%
  st_intersection(study_area) %>%
  select(ZONE, MAP_LABEL, ZONE_NAME ) %>%
  st_cast("MULTIPOLYGON")
saveRDS(bec_sf, file='tmp/bec_sf')


#Roads - use latest CE roads
Rd_gdb <- list.files(file.path(RoadDir, "CE_Roads/2017"), pattern = ".gdb", full.names = TRUE)[1]
fc_list <- st_layers(Rd_gdb)

# Read as sf and calculate road lengths
roads_sf <- read_sf(Rd_gdb, layer = "integrated_roads") %>%
 st_intersection(study_area) %>%
  mutate(rd_len = st_length(.))
st_crs(roads_sf) <- 3005
saveRDS(roads_sf, file='tmp/roads_sf')

#Read DEM
ESI_DEM <- raster(file.path(ESIDir,'Data/Library/DEM25.tif'))
saveRDS(ESI_DEM, file = 'tmp/ESI_DEM')

#Read in Landform file and mask to ESI area
LForm<-
  #raster(file.path('../GB_Data/data/Landform',"Landform_BCAlbs.tif")) %>%
  raster(file.path('/Users/darkbabine/Dropbox (BVRC)/_dev/Bears/GB_Data/data/Landform',"LForm.tif")) %>%
  mask(ESI)
saveRDS(LForm, file = 'tmp/LForm')
#mapview(LForm, maxpixels =  271048704)

#LFormFlat[!(LFormFlat[] %in% c(1000,5000,6000,7000,8000))]<-NA

#      ID	Landform	colour
#   1000	 Valley	 #358017
#   2000	 Hilltop in Valley	 #f07f21
#   3000	 Headwaters	 #7dadc3
#   4000	 Ridges and Peaks	 #ebebf1
#   5000	 Plains	 #c9de8d
#   6000	 Local Ridge in Plain	 #f0b88a
#   7000	 Local Valley in Plain	 #4cad25
#   8000	 Gentle Slopes	 #bbbbc0
#   9000	 Steep Slopes	 #8d8d91

LForm_LUT <- data.frame(LFcode = c(1000,2000,3000,4000,5000,6000,7000,8000,9000),
                        Landform = c('Valley','Hilltop in Valley','Headwaters','Ridges and Peaks',
                                     'Plains','Local Ridge in Plain','Local Valley in Plain',
                                     'Gentle Slopes','Steep Slopes'),
                        colourC = c('#358017','#f07f21','#7dadc3','#ebebf1','#c9de8d','#f0b88a',
                                    '#4cad25','#bbbbc0','#8d8d91'))
saveRDS(LForm_LUT, file = 'tmp/LForm_LUT')

LandCover<-
  raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','LandCover.tif')) %>%
  mask(ESI)
LandCover_LUT <- read_excel(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/LUT','LandCoverLookUp_LUT.xlsx'),sheet=1)
saveRDS(LandCover, file = 'tmp/LandCover_LUT')

Age<-
  raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','Age.tif')) %>%
  mask(ESI)
saveRDS(Age, file = 'tmp/Age')

#bcdata::filter(INTERSECTS(study_area)) %>%
#  collect()
} else {
  vri <- readRDS(file = 'tmp/vri')
  ws <- readRDS(file = 'tmp/ws')
  Wetlands <- readRDS(file = 'tmp/Wetlands')
  waterbodies <- readRDS(file = 'tmp/waterbodies')
  Streams <- readRDS(file = 'tmp/Streams')
  bec_sf <- readRDS(file= 'tmp/bec_sf')
  ESI <- readRDS(file = 'tmp/ESI')
  roads_sf <- readRDS(file = 'tmp/roads_sf')
  LForm <- readRDS(file = 'tmp/LForm')
  LForm_LUT <- readRDS(file= 'tmp/LForm_LUT')
  LandCover <- readRDS(file= 'tmp/LandCover')
  LandCover_LUT <- readRDS(file= 'tmp/LandCover_LUT')
  Age <- readRDS(file= 'tmp/Age')
  ESI_DEM <- readRDS(file = 'tmp/ESI_DEM')
  ESI_OW <-readRDS(file='tmp/ESI_OW')
  ESI_LBN <-readRDS(file='tmp/ESI_LBN')
  ESI_Gitxsan <-readRDS(file='tmp/ESI_Gitxsan')
  ESI_Gitanyow <-readRDS(file='tmp/ESI_Gitanyow')
}

#Subset data to make exploration easier
AOI <- ws %>%
  filter(SUB_SUB_DRAINAGE_AREA_NAME == "Bulkley")


#######################
waterbodies <- study_area[0, ] # creates an empty sf dataframe
waterbodies <- bcdc_query_geodata("cb1e3aba-d3fe-4de1-a2d4-b8b6650fb1f6", crs = epsg) %>% # lakes
  bcdata::filter(INTERSECTS(study_area)) %>%
  collect() %>% {if(nrow(.) > 0){rbind(., waterbodies)} else waterbodies}
waterbodies <- bcdc_query_geodata("f7dac054-efbf-402f-ab62-6fc4b32a619e", crs = epsg) %>% # rivers
  bcdata::filter(INTERSECTS(study_area)) %>%
  collect() %>% {if(nrow(.) > 0){rbind(., waterbodies)} else waterbodies}
waterbodies <- bcdc_query_geodata("93b413d8-1840-4770-9629-641d74bd1cc6", crs = epsg) %>% # wetlands
  bcdata::filter(INTERSECTS(study_area)) %>%
  collect() %>% {if(nrow(.) > 0){rbind(., waterbodies)} else waterbodies}





