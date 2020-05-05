
[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

# Wetland\_SkeenaESI\_Monitoring

This repository presents 3 analysis; 1) Set of scripts that read in the
Skeena East Environmental Stewardship Initiative’s (ESI) Wetland
Ecosystem Services Protocol (WESP) field cards, cleanup and summarise;
2) Generate a list of potential 2020 wetlands to sample, stratified on
Biogeoclimatic Ecosystem Classification (BEC) groups and two wetland
flow characterizations - connected to water or unconnected; and 3)
compiles Tier 1.5 wetland Climate BC data for wetlands and landform
position.

### Data

Field data was collected in 2019 by Skeena East ESI crews. Paper forms
were transfered into excel spreadsheets.

Data on wetlands was collated by Jesse Fraser using provincial
inventory. Skeena-Stikine FLNRORD District office supplied an updated
roads layer.

ClimateBC variables - <http://climatebc.ca>

Landform data from Adaptwest’s landfacets
<https://adaptwest.databasin.org/pages/adaptwest-landfacets>

### Usage

There are three sets of scripts that are contained in the repo, they
need to be run in order, note there is some duplication between sets:

Plot Data processing:

  - 01\_load\_plot\_data.R
  - 02\_clean\_plot\_data.R
  - 03\_analysis\_samplePrep.R
  - 03\_analysis\_sample2019.R

Sample selection:

  - 01\_load\_spatial.R
  - 02\_clean\_spatial.R
  - 03\_analysis\_BECStrata.R
  - 03\_analysis\_FlowStrata.R
  - 03\_analysis\_LandTypeStrataN.R
  - 03\_analysis\_DisturbanceStrataN.R
  - 03\_analysis\_samplePrep.R
  - 03\_analysis\_sampleAdminUnits.R
  - 03\_analysis\_sampleRequirements.R
  - 03\_analysis\_sampleAddStrata.R
  - 03\_analysis\_sample2020Summary.R
  - 04\_output.R

Tier 1.5 wetalnd data:

  - 01\_load\_spatial.R
  - 02\_clean\_spatial.R
  - 03\_analysis\_15Data.R

### Project Status

This project is part of Skeena East ESI a collaboration of Skeena First
Nations and the Provincial Government.

### Getting Help or Reporting an Issue

To report bugs/issues/feature requests, please file an
[issue](https://github.com/bcgov/Wetland_SkeenaESI_Monitoring/issues/).

### How to Contribute

If you would like to contribute, please see our
[CONTRIBUTING](CONTRIBUTING.md) guidelines.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.

### License

    Copyright 2020 Province of British Columbia
    
    Licensed under the Apache License, Version 2.0 (the &quot;License&quot;);
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at
    
    http://www.apache.org/licenses/LICENSE-2.0
    
    Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an &quot;AS IS&quot; BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

-----

This repository is maintained by
[ENVEcosystems](https://github.com/orgs/bcgov/teams/envecosystems/members).
