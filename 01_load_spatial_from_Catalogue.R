## Demo 1.

# determine sampling points for a fish monitoring survey

source('header.R')

# bring in BC boundary
bc <- bcmaps::bc_bound()

#Load ESI boundary
ESIin <- read_sf(file.path(ESIDir,'Data/Skeena_ESI_Boundary'), layer = "ESI_Skeena_Study_Area_Nov2017") %>%
  st_transform(3005)
ESI <- st_cast(ESIin, "MULTIPOLYGON")
saveRDS(ESI, file = 'tmp/ESI')

#Load ESI supporting wetland data
Wet1_gdb <-file.path(WetspatialDir,'Wetland_Assessment_Level1_InputData.gdb')
wet_list <- st_layers(Wet1_gdb)

#Load ESI Wetlands
Wet_gdb <-file.path(WetspatialDir,'Wetland_T1','Skeena_ESI_T1_Wetland_20191219.gdb')
#wet_list <- st_layers(Wet_gdb)
Wetlands <- readOGR(dsn=Wet_gdb, layer = "Skeena_ESI_T1_Wetland_20191219") %>%
  as('sf')
st_crs(Wetlands)=3005

#Make a version that is data only
Wetland_data<- Wetlands
st_geometry(Wetland_data) <- NULL

# 1. Water Drainage data
#  read in the water drainage data set to set up and AOI:
# https://catalogue.data.gov.bc.ca/dataset/water-survey-of-canada-sub-sub-drainage-areas

# download the water drainage and use as an area of interest using the bcdata package
ws <- get_layer("wsc_drainages", class = "sf") %>%
  select(SUB_DRAINAGE_AREA_NAME, SUB_SUB_DRAINAGE_AREA_NAME) %>%
  filter(SUB_DRAINAGE_AREA_NAME %in% c("Nechako", "Skeena - Coast"))

## plot the data to verify locations
#plot(ws["SUB_DRAINAGE_AREA_NAME"], key.pos = NULL)
#mapview::mapview(ws)

#For data download use ESI boundary ws as a subset
study_area <- ESI
#If wanted to use a different boundary
  #ws %>%
  #filter(SUB_SUB_DRAINAGE_AREA_NAME == "Babine") %>%
  #st_union()

# read in the VRI data - struggles with access...
vri <- bcdc_query_geodata("2ebb35d8-c82f-4a17-9c96-612ac3532d55") %>%
  bcdata::filter(INTERSECTS(study_area)) %>%
  bcdata::select(c("BCLCS_LEVEL_2", "HARVEST_DATE")) %>% # Treed sites
  collect()

# Download BEC - # Gets bec_sf zone shape and filters the desired subzones
bec_sf <- bec(class = "sf") %>%
  st_intersection(study_area)

if(!is.null(subzones)){
  bec_sf <- dplyr::filter(bec_sf, as.character(MAP_LABEL) %in% subzones)
  study_area <- st_intersection(study_area, bec_sf) %>% # The AOI is trimmed according to the bec_sf zones included
    summarise()
  st_write(study_area, file.path(res_folder, "AOI.gpkg"), delete_layer = TRUE)

# Road processing
roads <- bcdc_query_geodata("bb060417-b6e6-4548-b837-f9060d94743e") %>%
  bcdata::filter(INTERSECTS(study_area)) %>%
  collect()

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





