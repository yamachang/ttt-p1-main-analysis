# ---------------------------------------------------------------------------- #
# Run VAR and ML-VAR Network Analyses ----
# Authors: Josip Razum, Sebastian Castro-Alvarez, Laura F. Bringmann, Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running script, restart R (CTRL+SHIFT+F10 on Windows) and set working 
# directory to parent folder

# The present script runs idiographic and multilevel VAR models on data that have 
# already been aligned to ensure equal time intervals (mimicking Mplus's TINTERVAL);
# thus, here we set "tinterval" to 1. The data have also already been detrended. See
# "02_further_clean_data_align_obs.R" for the alignment and detrending steps.

# Note: The "var2Mplus" and "mlvar2Mplus" functions (along with other functions)
# are defined in the folder "./02_networks/code/from_sebastian_edited_by_josip/R/".
# All functions in that folder were emailed from Sebastian to Jeremy on 9/5/23.
# Josip removed the random effects covariances from the "write.mlvar" function in 
# the script "write.mlvar_without_random_effects_covariance.R" (which Josip emailed 
# to Jeremy on 10/17/24) but did not change Sebastian's other scripts in that folder.

# ---------------------------------------------------------------------------- #
# Store working directory, check correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("./02_networks/code/01_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# Load packages (note: Josip is unsure whether this script uses the "esmpack" 
# package below, but we load it just in case)

pkgs <- c("remotes", "MplusAutomation")
groundhog.library(pkgs, groundhog_day)

# Manually load "esmpack", which is not available on CRAN (install from latest GitHub 
# commit "0e76781" ("Merge branch 'wviechtb:master' into master") as of 3/16/23

packages <- rownames(installed.packages())
if (!"esmpack" %in% packages) {
  remotes::install_github("secastroal/esmpack", ref = "0e76781")
}
rm(packages)

library("esmpack")

# ---------------------------------------------------------------------------- #
# Import data ----
# ---------------------------------------------------------------------------- #

load("./02_networks/data/final_clean/data_var.RDS")

# ---------------------------------------------------------------------------- #
# Create vector to store unique participant IDs ----
# ---------------------------------------------------------------------------- #

ids <- unique(data_var$lifepak_id)

# ---------------------------------------------------------------------------- #
# Run idiographic VAR models using all 8 nodes ----
# ---------------------------------------------------------------------------- #

varfit <- list()

mplus_var_path <- "./02_networks/results/mplus_var/"

if (!(dir.exists(mplus_var_path))) {dir.create(mplus_var_path)}

setwd(mplus_var_path)

for (i in 1:length(ids)) {
  tmp <- data_var[data_var$lifepak_id == ids[i], 
                  c("bin_no_adj", "bad_d", "control_d", "energy_d", "focus_d", "fun_d", "interest_d", "movement_d", "sad_d")]
  tmp <- na.omit(tmp)
  names(tmp) <- c("time", "bad", "cont", "ener", "foc", "fun", "int", "move", "sad")
  
  varfit[[i]] <- var2Mplus(y = c("bad", "cont", "ener", "foc", "fun", "int", "move", "sad"), data = tmp,
                           filename = paste0("id", ids[i], ".dat"),
                           variable_options = list(timevar = "time", tinterval = 1),
                           output_options = list(standardized = TRUE),
                           analysis_options = list(biterations.min = 5000, chains = 3),
                           runmodel = !file.exists(paste0("id", ids[i], ".out")))
}
rm(tmp)

setwd(wd_dir)

saveRDS(varfit, file = "./02_networks/results/varfit.RDS")

# ---------------------------------------------------------------------------- #
# Run idiographic VAR models using 7 nodes (including "control"; excluding "fun") ----
# ---------------------------------------------------------------------------- #

varfit_control <- list()

mplus_var_control_path <- "./02_networks/results/mplus_var_control/"

if (!(dir.exists(mplus_var_control_path))) {dir.create(mplus_var_control_path)}

setwd(mplus_var_control_path)

if (!(dir.exists(mplus_var_path))) {dir.create(mplus_var_path)}

setwd(mplus_var_path)

for (i in 1:length(ids)) {
  tmp <- data_var[data_var$lifepak_id == ids[i], 
                  c("bin_no_adj", "bad_d", "control_d", "energy_d", "focus_d", "interest_d", "movement_d", "sad_d")]
  tmp <- na.omit(tmp)
  names(tmp) <- c("time", "bad", "cont", "ener", "foc", "int", "move", "sad")
  
  varfit_control[[i]] <- var2Mplus(y = c("bad", "cont", "ener", "foc", "int", "move", "sad"), data = tmp,
                                   filename = paste0("id", ids[i], ".dat"),
                                   variable_options = list(timevar = "time", tinterval = 1),
                                   output_options = list(standardized = TRUE),
                                   analysis_options = list(biterations.min = 5000, chains = 3),
                                   runmodel = !file.exists(paste0("id", ids[i], ".out")))
}
rm(tmp)

setwd(wd_dir)

saveRDS(varfit_control, file = "./02_networks/results/varfit_control.RDS")

# ---------------------------------------------------------------------------- #
# Run idiographic VAR models using 7 nodes (including "fun"; excluding "control") ----
# ---------------------------------------------------------------------------- #

varfit_fun <- list()

mplus_var_fun_path <- "./02_networks/results/mplus_var_fun/"

if (!(dir.exists(mplus_var_fun_path))) {dir.create(mplus_var_fun_path)}

setwd(mplus_var_fun_path)

for (i in 1:length(ids)) {
  tmp <- data_var[data_var$lifepak_id == ids[i], 
                  c("bin_no_adj", "bad_d", "energy_d", "focus_d","fun_d", "interest_d", "movement_d", "sad_d")]
  tmp <- na.omit(tmp)
  names(tmp) <- c("time", "bad", "ener", "foc", "fun", "int", "move", "sad")
  
  varfit_fun[[i]] <- var2Mplus(y = c("bad", "ener", "foc", "fun", "int", "move", "sad"), data = tmp,
                               filename = paste0("id", ids[i], ".dat"),
                               variable_options = list(timevar = "time", tinterval = 1),
                               output_options = list(standardized = TRUE),
                               analysis_options = list(biterations.min = 5000, chains = 3),
                               runmodel = !file.exists(paste0("id", ids[i], ".out")))
}
rm(tmp)

setwd(wd_dir)

saveRDS(varfit_fun, file = "./02_networks/results/varfit_fun.RDS")

# ---------------------------------------------------------------------------- #
# Run ML-VAR model using all 8 nodes ----
# ---------------------------------------------------------------------------- #

varfit <- list()

mplus_mlvar_path <- "./02_networks/results/mplus_mlvar/"

if (!(dir.exists(mplus_mlvar_path))) {dir.create(mplus_mlvar_path)}

setwd(mplus_mlvar_path)

tmp <- data_var[, c("lifepak_id", "bin_no_adj", "bad_d", "control_d", "energy_d", "focus_d", "fun_d", "interest_d", "movement_d", "sad_d")]
tmp <- na.omit(tmp)
names(tmp) <- c("id", "time", "bad", "cont", "ener", "foc", "fun", "int", "move", "sad")

# Note: random.effects arguments are Sebastian's defaults

mlvarfit <- mlvar2Mplus(y = c("bad", "cont", "ener", "foc", "fun", "int", "move", "sad"), id = "id", data = tmp,
                        random.effects = list(lagged = TRUE, slopes = FALSE,
                                              trend = TRUE, rvar = FALSE),
                        variable_options = list(timevar = "time", tinterval = 1),
                        output_options = list(standardized = TRUE),
                        analysis_options = list(biterations.min = 5000, chains = 3),
                        filename = "mlvar_tinterval.dat",
                        runmodel = !file.exists("mlvar_tinterval.out"))
rm(tmp)

setwd(wd_dir)

saveRDS(mlvarfit, file = "./02_networks/results/mlvarfit.RDS")

# ---------------------------------------------------------------------------- #
# Run ML-VAR model using 7 nodes (including "control"; excluding "fun") ----
# ---------------------------------------------------------------------------- #

varfit <- list()

mplus_mlvar_control_path <- "./02_networks/results/mplus_mlvar_control/"

if (!(dir.exists(mplus_mlvar_control_path))) {dir.create(mplus_mlvar_control_path)}

setwd(mplus_mlvar_control_path)

tmp <- data_var[, c("lifepak_id", "bin_no_adj", "bad_d", "control_d", "energy_d", "focus_d", "interest_d", "movement_d", "sad_d")]
tmp <- na.omit(tmp)
names(tmp) <- c("id", "time", "bad", "cont", "ener", "foc", "int", "move", "sad")

mlvarfit_control <- mlvar2Mplus(y = c("bad", "cont", "ener", "foc", "int", "move", "sad"), id = "id", data = tmp,
                                random.effects = list(lagged = TRUE, slopes = FALSE,
                                                      trend = TRUE, rvar = FALSE),
                                variable_options = list(timevar = "time", tinterval = 1),
                                output_options = list(standardized = TRUE),
                                analysis_options = list(biterations.min = 5000, chains = 3),
                                filename = "mlvar_tinterval.dat",
                                runmodel = !file.exists("mlvar_tinterval.out"))
rm(tmp)

setwd(wd_dir)

saveRDS(mlvarfit_control, file = "./02_networks/results/mlvarfit_control.RDS")

# ---------------------------------------------------------------------------- #
# Run ML-VAR model using 7 nodes (including "fun"; excluding "control") ----
# ---------------------------------------------------------------------------- #

varfit <- list()

mplus_mlvar_fun_path <- "./02_networks/results/mplus_mlvar_fun/"

if (!(dir.exists(mplus_mlvar_fun_path))) {dir.create(mplus_mlvar_fun_path)}

setwd(mplus_mlvar_fun_path)

tmp <- data_var[, c("lifepak_id", "bin_no_adj", "bad_d", "energy_d", "focus_d", "fun_d", "interest_d", "movement_d", "sad_d")]
tmp <- na.omit(tmp)
names(tmp) <- c("id", "time", "bad", "ener", "foc", "fun", "int", "move", "sad")

mlvarfit_fun <- mlvar2Mplus(y = c("bad", "ener", "foc", "fun", "int", "move", "sad"), id = "id", data = tmp,
                            random.effects = list(lagged = TRUE, slopes = FALSE,
                                                  trend = TRUE, rvar = FALSE),
                            variable_options = list(timevar = "time", tinterval = 1),
                            output_options = list(standardized = TRUE),
                            analysis_options = list(biterations.min = 5000, chains = 3),
                            filename = "mlvar_tinterval.dat",
                            runmodel = !file.exists("mlvar_tinterval.out"))
rm(tmp)

setwd(wd_dir)

saveRDS(mlvarfit_fun, file = "./02_networks/results/mlvarfit_fun.RDS")