## Overview
This repository allows to reproduce the results obtained in "Abunijela et al. (2025) - Wastewater-based Surveillance as a tool for monitoring and estimating COVID-19 incidence and trends: Insights from Germany, 2022-2024". The analyses were conducted using R 4.4.1 (64 bit, Windows). You can recreate the project environment by using the `renv` package (https://rstudio.github.io/renv/articles/renv). To do this, make sure that you have the same R version installed and that you run "install.packages("renv")" and then "renv::restore()".

## Folders in this repository
* `data`: This folder contains the datasets needed for the analyses. 
* `R`: This folders contains the R scripts alled by `main.R` (located in the main directory) needed to replicate the results.
* `output`: This folder contains the figures and tables of the manuscript which can be reproduced by running `main.R`.
* `renv`: This folder contains files to recreate the project environment and need not be called directly, see https://rstudio.github.io/renv/articles/renv.

## How to reproduce the results
If you use RStudio, open the `R` project 
`WBS_for_COVID19_monitoring.Rproj` in the main directory first, then no further adjustment should be necessary. If you do not use RStudio, you have to set your working directories accordingly by replacing  "here()" by your main directory  (e.g. "C:/WBS_for_COVID19_monitoring") and "R" by the appropiate folder (e.g. "C:/WBS_for_COVID19_monitoring/R") in the script `main.R`. To reproduce the results which are then stored in the `output` folder and its subfolders, just run the script `main.R` which calls all R scripts stored in the subfolder `R`. The processing may take some minutes.