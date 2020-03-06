# Copyright 2019 Province of British Columbia
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

#Buffer lakes and put lake id in wetland
# then join lake attriburtes to wetland - may be more than one?

LakesB <- st_buffer(Lakes, 100)
intLakes <- as_tibble(st_intersection(Wetlands, LakesB))


intBEC$area <- st_area(intBEC$geometry)
saveRDS(intBEC, file = 'tmp/intBEC')
} else {
  intBEC <- readRDS(file = 'tmp/intBEC')
}
#Select largest BEC poly in Wetland
MajBEC <- intBEC %>%
  group_by(OBJECTID) %>%
  dplyr::summarise(max_BEC = BGC_LABEL[which.max(area)],BEC_area = max(area))

#Join largest BEC to wetland map
MajBEC_table <- data.frame(MajBEC)
MajBEC_table <- dplyr::select(MajBEC_table, OBJECTID, max_BEC, BEC_area)
Wetland_BEC <- left_join(Wetlands,MajBEC_table)

