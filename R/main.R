# clean workspace
rm(list = ls())

# in case not installed, install pacman package
if (!require("pacman"))
  install.packages("pacman")
# install (if not done yet) and load here package
pacman::p_load(here)

# install (if not done yet) and load renv package
# pacman::p_load(renv)

# restore project environment. works if you have same R version (4.4.1) installed.
# renv::restore()

# define paths where to store results and where to load the data from
results_here_tables <- here(here(), "output/tables")
results_here_plots <- here(here(), "output/plots")
data_here <- here(here(), "data")

# run all self-written functions and install required packages
source(here("R", "load_packages_and_functions.R"), encoding = "UTF-8")

# prepare data and results that can be used to generate results tables and figures
source(here("R", "prepare_data_for_analyses.R"), encoding = "UTF-8")

# generate results tables and figures
source(here("R", "generate_plots_and_tables.R"), encoding = "UTF-8")
