# this script just enables you to run all relevant scripts in one shot

# clean workspace
rm(list = ls())

# in case not installed, install pacman package
if (!require("pacman"))
  install.packages("pacman")
# install (if not done yet) and load required packages and read in self-written
# functions
pacman::p_load(here)

source(here("R_new", "load_packages_and_functions.R"), encoding = "UTF-8")
#source(here("R_new", "import_data.R"), encoding = "UTF-8") #imports data
source(here("R_new", "prepare_data_for_analyses.R"), encoding = "UTF-8")
source(here("R_new", "generate_plots_and_tables.R"), encoding = "UTF-8")
