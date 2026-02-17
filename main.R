# clean workspace
rm(list = ls())

# Function to install and load packages
install_and_load <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE)
  }
}

# install (if not done yet) and load renv package
# install.packages("renv")
# library(renv)

# restore project environment. works if you have same R version (4.5.1) installed.
# renv::restore()

# install (if not done yet) and load here package
install_and_load("here")

# define paths where to store results and where to load the data from
results_here_tables <- here("output/tables")
results_here_plots <- here("output/plots")
data_here <- here("data")

# run all self-written functions and install required packages
source(here("R", "load_packages_and_functions.R"), encoding = "UTF-8")

# prepare data and results that can be used to generate results tables and figures
source(here("R", "prepare_data_for_analyses.R"), encoding = "UTF-8")

# generate results tables and figures
source(here("R", "generate_plots_and_tables.R"), encoding = "UTF-8")
