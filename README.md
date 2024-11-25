# ScotPHO Online Profiles Tool

In this repository you can find the code used to produce [ScotPHO Online Profiles Tool](https://scotland.shinyapps.io/ScotPHO_profiles_tool/).

October 2024 : A new version of the ScotPHO profile tool design was launched. The redesign involved significant re-factoring of app code and creation of this new Git repository.
The historic profiles tool repo can be found here: [Historic ScotPHO Profile Tool repo](https://github.com/Public-Health-Scotland/scotpho-profiles-tool)

The changes to app design: 
-content changes introducing information presented by different demographic splits (e.g. age/sex/gender/ethnicity)
-app navigation changes - introducing topic specific theme buttons and global filtering
-technical/architectural transformations such as the use of the bslib package to specify app layout and increased use of shiny modules

## Code

This repo does not contain any datafiles only the scripts that generate the shiny app. Data can be downloaded from the app either through individual visualisation panels or bulk download through data download tab.

## data files 
- main_dataset : populates visualisations in summary, trend, rank navpanels
- popgroup_dataset: populates visualisations in popgroup navpanel
- simd_dataset:  populates visualisations deprivation navpanel
- techdoc: contains indicator metadata for all indicators within the tool
- geo_lookup: lookup matching geography identification codes to geography full descriptions
- main_data_geonodes: contains heirachical information linking smaller geographies to parent areas
- shapefiles: simplified geographical boundary information that allow map creation within rank navpanel

## 'data preparation'
Within this folder is a master script '1_ScotPHO_profiles_data_prepartion' that generates several data files that are required for the app to run. These scripts will only run for those with sufficient internal network access permissions.

## shiny_app
Within this folder are scripts require for functioning of the profiles tool shiny app
