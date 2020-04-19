## Demo 1.

# determine sampling points for a fish monitoring survey

source('header.R')

# bring in BC boundary
bc <- bcmaps::bc_bound()

ProvRast<-raster(nrows=15744, ncols=17216, xmn=159587.5, xmx=1881187.5,
                 ymn=173787.5, ymx=1748187.5,
                 res = c(100,100), vals = 1)

crs(ProvRast)<-crs(bc)
saveRDS(ProvRast,file='tmp/ProvRast')

ESI_file <- file.path("tmp/ESI")
if (!file.exists(ESI_file)) {
#Load ESI boundary
ESIin <- read_sf(file.path(ESIDir,'Data/Skeena_ESI_Boundary'), layer = "ESI_Skeena_Study_Area_Nov2017") %>%
  st_transform(3005)
ESI <- st_cast(ESIin, "MULTIPOLYGON")
saveRDS(ESI, file = ESI_file)

#Load ESI supporting wetland data
Wet_gdb <-file.path(WetspatialDir,'Wetland_Assessment_Level1_InputData.gdb')
wet_list <- st_layers(Wet_gdb)

#Load ESI Wetlands
WetW_gdb <-file.path(WetspatialDir,'Wetland_T1','Skeena_ESI_T1_Wetland_20191219.gdb')
#wet_list <- st_layers(WetW_gdb)
Wetlands <- readOGR(dsn=WetW_gdb, layer = "Skeena_ESI_T1_Wetland_20191219") %>%
  as('sf')
Wetlands <- Wetlands %>%
  mutate(wet_id=as.numeric(rownames(Wetlands)))
st_crs(Wetlands)<-3005
saveRDS(Wetlands, file = 'tmp/Wetlands')

#Read in all the fire layers
WetInData<-file.path(WetspatialDir,'Wetland_Assessment_Level1_InputData.gdb')
wetin_list <- st_layers(WetInData)

Wildfire_2018 <- readOGR(dsn=WetInData, layer = "Wildfire_2018_181128")  %>%
  as('sf')
st_crs(Wildfire_2018)<-3005
saveRDS(Wildfire_2018,'tmp/Wildfire_2018')

Wildfire_Historical <- readOGR(dsn=WetInData, layer = "Wildfire_Historical_181128")   %>%
  as('sf')
st_crs(Wildfire_Historical)<-3005
saveRDS(Wildfire_Historical,'tmp/Wildfire_Historical')

BurnSeverity_2018 <- readOGR(dsn=WetInData, layer = "BurnSeverity_2018_181128")   %>%
  as('sf')
st_crs(BurnSeverity_2018)<-3005
saveRDS(BurnSeverity_2018,'tmp/BurnSeverity_2018')

BurnSeverity_2017 <- readOGR(dsn=WetInData, layer = "BurnSeverity_2017_181128")   %>%
  as('sf')
st_crs(BurnSeverity_2017)<-3005
saveRDS(BurnSeverity_2017,'tmp/BurnSeverity_2017')

# 1. Water Drainage data
#  read in the water drainage data set to set up and AOI:
# https://catalogue.data.gov.bc.ca/dataset/water-survey-of-canada-sub-sub-drainage-areas

# download the water drainage and use as an area of interest using the bcdata package
#ws <- get_layer("wsc_drainages", class = "sf") %>%
#  select(SUB_DRAINAGE_AREA_NAME, SUB_SUB_DRAINAGE_AREA_NAME) %>%
#  filter(SUB_DRAINAGE_AREA_NAME %in% c("Nechako", "Skeena - Coast"))

ws <- get_layer("wsc_drainages", class = "sf") %>%
  select(SUB_DRAINAGE_AREA_NAME, SUB_SUB_DRAINAGE_AREA_NAME)
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

#FWA_Rivers
Rivers <- read_sf(Wet_gdb, layer = "FWA_Rivers")
st_crs(Rivers) <- 3005
saveRDS(Rivers, file = 'tmp/Rivers')

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
  select(ZONE, MAP_LABEL, ZONE_NAME ) %>%
  st_cast("MULTIPOLYGON")
saveRDS(bec_sf, file='tmp/bec_sf')

#Roads - use latest CE roads
Rd_gdb <- list.files(file.path(RoadDir, "CE_Roads/2017"), pattern = ".gdb", full.names = TRUE)[1]
fc_list <- st_layers(Rd_gdb)

# Read as sf and calculate road lengths
roads_sf <- read_sf(Rd_gdb, layer = "integrated_roads") %>%
  mutate(rd_len = st_length(.))
st_crs(roads_sf) <- 3005
saveRDS(roads_sf, file='tmp/roads_sf')

#Read DEM
ESI_DEM <- raster(file.path(ESIDir,'Data/Library/DEM25.tif'))
saveRDS(ESI_DEM, file = 'tmp/ESI_DEM')

#Read in Landform file and mask to ESI area
LForm<-
  #raster(file.path('../GB_Data/data/Landform',"Landform_BCAlbs.tif")) %>%
  raster(file.path('/Users/darkbabine/Dropbox (BVRC)/_dev/Bears/GB_Data/data/Landform',"LForm.tif"))
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
  raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','LandCover.tif'))
saveRDS(LandCover, file = 'tmp/LandCover')
LandCover_LUT <- read_excel(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/LUT','LandCoverLookUp_LUT.xlsx'),sheet=1)
saveRDS(LandCover_LUT, file = 'tmp/LandCover_LUT')

LandCoverAndAge<-
  raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','LandCoverAndAge.tif'))
saveRDS(LandCoverAndAge, file = 'tmp/LandCoverAndAge')

Age<-
  raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','Age.tif'))
saveRDS(Age, file = 'tmp/Age')

LogYear<-
  raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','LogYear.tif'))
saveRDS(LogYear, file = 'tmp/LogYear')

#Raster rail
RailRoads<-
  raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','RailRoads.tif'))
saveRDS(RailRoads, file = 'tmp/RailRoads')

#Raster pipeline
Pipe<-
  raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','Pipe_PrinceRupertGasTransmissionLtd.tif'))
saveRDS(Pipe, file = 'tmp/Pipe')

#Raster HydroTransmission
HydroTransmission<-
  raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','HydroTransmission.tif'))
saveRDS(HydroTransmission, file = 'tmp/HydroTransmission')

#Raster roads
RoadType<-
  raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','roadType.tif'))
saveRDS(RoadType, file = 'tmp/roadType')

RoadType_LUT <- read_csv(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/LUT','RoadType_LUT.csv'))
saveRDS(RoadType_LUT, file = 'tmp/RoadType_LUT')

ExtensiveFootprint<-
  raster(file.path(ESIDir,'Data/DataScience/SkeenaESI_LandCover_Age_Human_Footprint/OutRaster','ExtensiveFootprint.tif'))
saveRDS(ExtensiveFootprint, file = 'tmp/ExtensiveFootprint')

#Load ESI Wetlands
ESI_gdb <-file.path(ESIData,'ESI_Data.gdb')
#wet_list <- st_layers(ESI_gdb)
RoadKisp <- readOGR(dsn=ESI_gdb, layer = "SSAF_Ext_Clip_ConsRd_inclKispBulk_DSS_190918") %>%
  as('sf')
st_crs(RoadKisp)<-3005
saveRDS(RoadKisp, file = 'tmp/RoadKisp')

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
  LandCoverAndAge <- readRDS(file= 'tmp/LandCoverAndAge')
  ExtensiveFootprint <- readRDS(file= 'tmp/ExtensiveFootprint')
  RoadType <- readRDS(file= 'tmp/RoadType')
  HydroTransmission <- readRDS(file= 'tmp/HydroTransmission')
  Pipe <- readRDS(file= 'tmp/Pipe')
  RailRoads <- readRDS(file= 'tmp/RailRoads')
  LogYear <- readRDS(file= 'tmp/LogYear')
  Wildfire_2018 <- readRDS(file= 'tmp/Wildfire_2018')
  Wildfire_Historical <- readRDS(file= 'tmp/Wildfire_Historical')
  BurnSeverity_2018 <- readRDS(file= 'tmp/BurnSeverity_2018')
  BurnSeverity_2017 <- readRDS(file= 'tmp/BurnSeverity_2017')
  RoadKisp <- readRDS(file= 'tmp/RoadKisp')
  ESI_DEM <- readRDS(file = 'tmp/ESI_DEM')
  ESI_OW <-readRDS(file='tmp/ESI_OW')
  ESI_LBN <-readRDS(file='tmp/ESI_LBN')
  ESI_Gitxsan <-readRDS(file='tmp/ESI_Gitxsan')
  ESI_Gitanyow <-readRDS(file='tmp/ESI_Gitanyow')
}

