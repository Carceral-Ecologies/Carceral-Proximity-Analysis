# Carceral EJ Mapper

## About

The Carceral EJ Mapper is a tool for examining the proximity of carceral facilities in the United States to various environmental hazards. Within the context of a US state, users may apply filter conditions to determine which carceral facilities have at least a certain number of superfund sites, brownfields, and other sites of toxic pollution within a given proximity to the facility. The tool is useful for identifying carceral facilities and incarcerated people on the frontlines of environmental injustice in the US.

## Cite As

Deckard Barnes, Brittany Bates, Ben Millam, Priyanshi Nigam, Lindsay Poirier, Savannah Ramirez, Michelle Servin, & Nicholas Shapiro. (2021, March 31). Carceral EJ Mapper (Version 1.1.2). Zenodo. [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4667080.svg)](https://doi.org/10.5281/zenodo.4667080)

## Contributors
<!-- ALL-CONTRIBUTORS-LIST:START -->
| Contributions | Name (alpha order) |
| ----: | :---- |
| [🔢](# "Content") [💻](# "Code") [🤔](# "Ideas and Planning") | [Deckard Barnes](https://github.com/djbarnes24601) |
| [🔢](# "Content") [💻](# "Code") [🤔](# "Ideas and Planning") | [Brittany Bates](https://github.com/bee-49) |
| [💻](# "Code") [🚇](# "Infrastructure") [🤔](# "Ideas and Planning") | [Ben Millam](https://github.com/benmillam) |
| [🔢](# "Content") [🤔](# "Ideas and Planning") | [Priyanshi Nigam](#) |
| [🔢](# "Content") [📋](# "Organizer")[💻](# "Code") [🚇](# "Infrastructure") [🤔](# "Ideas and Planning") | [Lindsay Poirier](https://github.com/lindsaypoirier) |
| [🔢](# "Content") [📋](# "Organizer") [🐛](# "Bug Reports") [🤔](# "Ideas and Planning") | [Savannah Ramirez](https://github.com/savannahramirez2) |
| [🔢](# "Content") [🤔](# "Ideas and Planning") | [Michelle Servin](https://github.com/mservin310) |
| [🔢](# "Content") [📋](# "Organizer") [🤔](# "Ideas and Planning") | [Nick Shapiro](https://github.com/shapironick) |

<!-- ALL-CONTRIBUTORS-LIST:END -->

(For a key to the contribution emoji or more info on this format, check out [“All Contributors.”](https://allcontributors.org/docs/en/emoji-key))

## How to Access

You can access a working version of this application [here](http://critical-data-analysis.org/shiny/proximity/proximity-app/).

## How to Provide Feedback

Questions, bug reports, and feature requests can be submitted to this repo's [issue queue](https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/issues).

## Copyrights
All code in this repo is licensed with a GNU General Public License 3.0. Please see the [license](https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/blob/master/LICENSE) file for details.

All written materials are licensed with a Creative Commons Attribution-ShareAlike 3.0 Unported (CC BY-SA 3.0). Please see this [license](https://creativecommons.org/licenses/by-sa/3.0/) for details.

## Have Questions?
Contact [hack-for-california@ucdavis.edu](mailto:hack-for-california@ucdavis.edu)

## Definitions and Data Sources

* Airports: Airports were sourced from the Department of Homeland Security's HIFLD database in a dataset called [Aircraft Landing Facilities](https://hifld-geoplatform.opendata.arcgis.com/datasets/aircraft-landing-facilities). This data was filtered to those in which the fac_type was equal to "AIRPORT". It is up to date as of July 13, 2018.
* Brownfields: Brownfields were sourced from the EPA's [Facility Registry Service (FRS)](https://www.epa.gov/frs/frs-data-resources). We downloaded this dataset as a CSV file and then filtered it to those in which the SITE_TYPE_NAME was equal to "BROWNFIELDS SITE". We then performed a [spatial join](https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/issues/13) with the [Census Bureau's 2019 Census Tract Shapefiles](https://www2.census.gov/geo/tiger/TIGER2019/TRACT/) to append census tract information to the brownfields dataset.
* Census Tracts: Census tracts were aggregated from the US Census Bureau's [2019 TIGER/Line Shapefiles](https://www2.census.gov/geo/tiger/TIGER2019/TRACT/).
* Military Bases: Military bases were sourced from the Office of the Assistant Secretary of Defense's [Defense Installations Spatial Data Infrastructure](https://www.acq.osd.mil/eie/BSI/BEI_DISDI.html). This data represents point locations and boundaries of US Military Installations, Ranges, and Training Areas as of May 2019. (See file called Geospatial Information for U.S. Military Installations, Ranges, and Training Areas). 8 polygons in the boundaries file in this zip had self-intersecting polygons, which were fixed in QGIS using the Fix Geometries feature. 
* Carceral Facilities: Carceral facilities boundary data was sourced from the Department of Homeland Security's [HIFLD database](https://hifld-geoplatform.opendata.arcgis.com/datasets/prison-boundaries/data?geometry=97.022%2C-3.069%2C-116.728%2C75.954) While this data was collected in 2018, it is more than a decade more up-to-date than the most recent DOJ prison survey. We then calculated the centroid of each prison boundary and performed a [spatial join](https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/issues/13) with the [Census Bureau's 2019 Census Tract Shapefiles](https://www2.census.gov/geo/tiger/TIGER2019/TRACT/) to append census tract information to the prisons dataset.
* Superfund sites: Superfund sites are any EPA facilities in the [Facility Registry System (FRS)](https://www.epa.gov/frs/frs-data-resources) that are associated with SEMS, Superfund's IT system. To gather these facilities, we downloaded the EPA's FRS dataset as a CSV file and then filtered it to those in which "SEMS" was detected in the PGM_SYS_ACRNMS variable. This currently includes both National Priorities List (NPL) and non-NPL sites. 
* TRI Facilities: TRI facilities include any facilities that reported emissions in the [2018 EPA Toxic Release Inventory](https://www.epa.gov/toxics-release-inventory-tri-program/tri-basic-data-files-calendar-years-1987-2018?). TRI facilities are US industrial facilities with 10 or more employees that emit above a certain threshold of TRI-regulated chemicals in a given year. For more specific information on how the EPA defines a TRI Facility, see: https://www.epa.gov/toxics-release-inventory-tri-program/basics-tri-reporting

## Data Collection and Update Process

In the Fall of 2019, contributors to this project began identifying environmental hazards that, when sited in proximity to carceral facilities, may indicate high degrees of exposure to toxicants amongst incarcerated people. The team researched open government geospatial datasets indicating the location of these hazards, and the above listed data sources represent the results of that research. 

In the coming months, we intend to add PFAS sites and glyphosate use to the map. 

We intend to replace the map's source data files on at least a bi-annual basis, contingent on the data producer's update process.

## Repo Architecture

The code for the proximity app is stored entirely in [proximity-app/app.R](proximity-app/app.R) of this repo. All code has been commented. 

Original data files are stored in [proximity-app/data-original](proximity-app/data-original), and the cleaned data files (the files used on the map) are stored in [proximity-app/data-clean](proximity-app/data-clean). 

Scripts for cleaning each of the data files are stored in the [proximity-app/cleaning-scripts](proximity-app/cleaning-scripts) directory. Anything ending in -data-load-and-clean.R is a file designed to load and filter a source dataset, add census tract information, and then output the cleaned data. Perhaps most importantly, the filter functions in app.R rely on a dataset produced by a file in cleaning-scripts called [find_objects-proximate_prisons.R](proximity-app/cleaning-scripts/find_objects_proximate_prisons.R). This file calculates the distance of every carceral facility to every environmental hazard within 5000 meters of that facility and stores the results in a data file. This file outputs [proximity-app/data-clean/prisons_with_facility_distances.csv](proximity-app/data-clean/prisons_with_facility_distances.csv), which app.R references when a user filters the map to carceral facilities with x number of x hazards within x meters of the facility. 

## How to Contribute

1. File an issue via this repo's [issue queue](https://github.com/Carceral-Ecologies/Carceral-Proximity-Analysis/issues).

2. Write code to fix issues or to create new features. When contributing code, please be sure to:

  * Review the [Hack for California Documentation Guidelines](https://docs.google.com/document/d/1f3isQQS7uho_vto2Bf5HvbZBtUA0Pe0f7NlR_ARjTQ8/edit?usp=sharing).
  * Fork this repository, create a remote to this repo, and ensure your forked repo is constantly consistent with this repo. 
  * Modify the code, following this project's coding style (using Ratliffe indentation style; snake_case for variable, function names, and file names; dash-case for directory names, and commenting above each function).
  * Commit code often and follow the recommendations in the Documentation Guidelines for formatting commit messages. 
  * Test your code locally before issuing a pull request.
  * Issue a pull request for each change.




