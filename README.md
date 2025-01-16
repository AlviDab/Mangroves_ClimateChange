# Protecting climate-resilient mangroves requires a small increase in the global protected area

In this repository is reported the code to run the prioritisation and produce the figures of the study "Protecting climate-resilient mangroves requires a small increase in the global protected area".

The code should be run sequentially, starting from "01_data_processing" to "04_patchwork_figures"

Data to run the code in "Code/01_data_processing" can be downloaded at:

- Global Mangrove Watch (https://www.globalmangrovewatch.org/)
- IUCN red list - mangrove species (https://www.iucnredlist.org/resources/spatial-data-download)
- Global biophysical typology of mangroves (https://zenodo.org/records/8340259)
- Probability of mangrove loss (https://github.com/cabuelow/mangrove-network-models?tab=readme-ov-file#probabilistic-forecasts-of-the-direction-of-future-change-in-mangrove-extent-using-network-models)

Cloning the content of the branch "demo" it is possible to run a demo of the content of the folder "Code/01_data_processing" using a smaller dataset to make it less computationally intensive. Running all the data processing code from the folder "Code/01_data_processing" from the branch "master" could take multiple days (or weeks, depending on the computing power of your machine).

Data to run the code starting from "Code/07_CPA_selection.R" is provided as RDS. 

## System requirements
The code was written and tested in R version 4.4.0.

## Installation guide
Download and install R (https://cran.r-project.org/bin/windows/base/) and RStudio (https://posit.co/download/rstudio-desktop/)

Install the following packages in R to run the code and reproduce results.

`install.packages(c('tidyverse', 'sf', 'prioritizr', 'terra', 'devtools', 'patchwork', 'sf', 'purrr', 'furrr', 'wdpar', 
                   'parallel', 'tmap', 'openxlsx', 'ggeffects', 'ggstats', 'ggrepel', 'biscale', 'irr', 'ggnewscale', 
                   'MetBrewer', 'spatstat', 'collapse', 'modelr'))`
                   
`devtools::install_github("https://github.com/MathMarEcol/spatialplanr")`

`devtools::install_github("emlab-ucsb/spatialgridr")`

`devtools::install_github("BlakeRMills/MoMAColors")`

## Instruction for use
1. Download or clone the github repo in the branch 'master' and double click the Mangroves_CC.Rproj file to open RStudio. To run the data in the demo use
2. The data to run the analysis starting from the code in "Code/02_prioritisation" is available.
3. To run the analysis and generate the figures, start with the first script in "Code/02_prioritisation" and run the rest in sequential order including those in "03_figures" and "04_patchwork_figures".
