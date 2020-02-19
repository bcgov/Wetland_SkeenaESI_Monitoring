library(sf)
library(rgdal)
library(dplyr)
library(plyr)
library(readr)
library(raster)
library(bcmaps)
library(fasterize)
library(tidyr)
library(rio)
library(WriteXLS)
library(ggplot2)
library(readxl)
library(stringr)

require(gdata)
require(reshape2)


MonitoringSeason<-"2019"

OutDir <- 'out'
dataOutDir <- file.path(OutDir,'data')
StrataOutDir <- file.path(dataOutDir,'Strata')
figsOutDir <- file.path(OutDir,'figures')
SpatialDir <- file.path('data','spatial')
DataDir <- file.path('data',MonitoringSeason)
spatialOutDir <- file.path(OutDir,'spatial')

GBspatialDir <- file.path('../GB_Data/out/spatial')
GBdataOutDir <- file.path('../GB_Data/out/data')
GBPDir <-file.path('../GB_Data/data/Population/Bear_Density_2018')

dir.create(file.path(OutDir), showWarnings = FALSE)
dir.create(file.path(dataOutDir), showWarnings = FALSE)
dir.create(file.path(StrataOutDir), showWarnings = FALSE)
dir.create(file.path(figsOutDir), showWarnings = FALSE)
dir.create(file.path(spatialOutDir), showWarnings = FALSE)
dir.create(DataDir, showWarnings = FALSE)
dir.create("tmp", showWarnings = FALSE)

#Location of Monitoring spreadsheet
WetMonDir<-file.path('../../../Projects/ESI/Wetlands/Monitoring/2019FieldData')
#system(paste('ls ',WetMonDir,sep=''))

