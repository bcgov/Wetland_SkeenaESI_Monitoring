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

source('header.R')


WriteXLS(prop.site,file.path(dataOutDir,paste('ESI_Wetland_Strata_Disturb.xlsx',sep='')),SheetNames='Disturbance')

WriteXLS(WetStrata,file.path(dataOutDir,paste('WetPlotFnData.xls',sep='')))

#WriteXLS(WetPlotFnData,file.path(OutDir,paste('WetPlotFnData.xls',sep='')))


# Now we can write out a KML file so we can
# view this is google earth. First we need to convert to a sp object.
out_sp <- as(WetSamples, "Spatial")
# write out a kml
kml(out_sp,
    file.name    = file.path(spatialOutDir,"wetland_points.kml"),
    points_names = out_sp$BEC.y,
    colour    = "#FF0000",
    alpha     = 0.6,
    size      = 1,
    shape     = "http://maps.google.com/mapfiles/kml/pal2/icon18.png")

