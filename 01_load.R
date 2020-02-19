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

source("header.R")

#Read spreadsheet sheets from Tier2Data_FieldForms_PlotAndVeg_DoneMA.xlsx
WetPlots<- excel_sheets(file.path(WetMonDir,'Tier2Data_FieldForms_PlotAndVeg_DoneMA.xlsx'))
#First sheet is the plot info - has to make up column  names for some throwing a warning
PlotInfo<-read_excel(file.path(WetMonDir,'Tier2Data_FieldForms_PlotAndVeg_DoneMA.xlsx'),
                     sheet = WetPlots[1],.name_repair)
#Subsequent sheets are veg plots
x<-WetPlots[2:length(WetPlots)]
WetList<-lapply(x,function(x) {
  read_excel(file.path(WetMonDir,'Tier2Data_FieldForms_PlotAndVeg_DoneMA.xlsx'), sheet=x)
  })

