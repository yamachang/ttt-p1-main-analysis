# ---------------------------------------------------------------------------- #
# Run VAR and ML-VAR Network Analyses ----
# Authors: Josip Razum, Sebastian Castro-Alvarez, Laura F. Bringmann
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running script, restart R (CTRL+SHIFT+F10 on Windows) and set working 
# directory to parent folder

# Note: Use "bin_no_adj" (created in "further_clean_data_align_obs.R") as the time
# variable to be included in the models as a covariate (to detrend the time series)

# ---------------------------------------------------------------------------- #
# Store working directory, check correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("./02_networks/code/01_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# TODO: Load packages (Josip to load required packages)





# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./02_networks/data/final_clean/dat_anlys.RData")

# ---------------------------------------------------------------------------- #
# Run VAR analyses ----
# ---------------------------------------------------------------------------- #

# TODO: Josip to insert adapted code to analyze all 8 nodes and include "bin_no_adj"
# as a covariate





# ---------------------------------------------------------------------------- #
# Run ML-VAR analyses ----
# ---------------------------------------------------------------------------- #

# TODO: Josip to insert adapted code to analyze all 8 nodes and include "bin_no_adj"
# as a covariate




