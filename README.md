# Carceral-Proximity-Analysis

## How to Access

You can access a working version of this application [here](http://critical-data-analysis.org/shiny/proximity/proximity-app/).

## About

## Definitions

* Airports: 
* Brownfields: To gather brownfields in the Carceral Proximity App,  we downloaded the EPA's [FRS dataset](https://www.epa.gov/frs/frs-data-resources) as a CSV file and then filtered it to those in which the SITE_TYPE_NAME was equal to "BROWNFIELDS SITE". We then performed a [spatial join](https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/issues/13) with the [Census Bureau's 2019 Census Tract Shapefiles](https://www2.census.gov/geo/tiger/TIGER2019/TRACT/) to append census tract information to the brownfields dataset.
* Census Tracts: Census tracts were aggregated from the US Census Bureau's [2019 TIGER/Line Shapefiles](https://www2.census.gov/geo/tiger/TIGER2019/TRACT/).
* Military Bases: 
* Prisons: Prison boundary data was sourced from the Department of Homeland Security's [HIFLD database](https://hifld-geoplatform.opendata.arcgis.com/datasets/prison-boundaries/data?geometry=97.022%2C-3.069%2C-116.728%2C75.954) this data is more than a decade more up-to-date than the most recent DOJ prison survey. We then calculated the centroid of each prison boundary and performed a [spatial join](https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/issues/13) with the [Census Bureau's 2019 Census Tract Shapefiles](https://www2.census.gov/geo/tiger/TIGER2019/TRACT/) to append census tract information to the prisons dataset.
* Superfund sites: In the Carceral Proximity App, Superfund sites are any EPA facilities in the Facility Registry System (FRS) that are associated with SEMS, Superfund's IT system. To gather these facilities, we downloaded the EPA's [FRS dataset](https://www.epa.gov/frs/frs-data-resources) as a CSV file and then filtered it to those in which "SEMS" was detected in the PGM_SYS_ACRNMS variable. This currently includes National Priorities List (NPL) and non-NPL sites. 

## Aims

## Timeline

## How to Contribute

1. File an issue via this repo's [issue queue](https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/issues).

2. Write code to fix issues or to create new features. When contributing code, please be sure to:

  * Fork this repository, modify the code (changing only one thing at a time), and then issue a pull request for each change.
  * Follow the project's coding style (using K&R-style indentation and bracketing, commenting above each feature, and using snake case for variables).
  * Test your code locally before issuing a pull request.
  * Clearly state the purpose of your change in the description field for each commit.

## Architecture

The code for the proximity app is stored entirely in [app.R](https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/proximity-app/app.R) in this repo. All code has been commented. 

## Change Log

### V0.2

### V0.1
* [4](https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/issues/4) Replaced Contaminated Sites with Superfund Sites
* [7](https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/issues/7) Added Census Tracts as Map Layer
* [6](https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/issues/6) Allow users to toggle between different basemaps

## Copyrights
Please see [license](https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/LICENSE) file for details.

## Have Questions?
Contact [hack-for-california@ucdavis.edu](mailto:hack-for-california@ucdavis.edu)
