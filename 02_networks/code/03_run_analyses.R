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

# TODO (JE to review Josip's code below): Load packages

pkgs_analysis <- c("remotes","MplusAutomation","dplyr","tidyr","Hmisc","networktools","netcontrol")
groundhog.library(pkgs_analysis, groundhog_day)

#groundhog day reports that the package "esmpack" is not available on CRAN.
#code below installs it if not already installed and activates it.
packages <- rownames(installed.packages())
if (!"esmpack" %in% packages) {
  remotes::install_github("secastroal/esmpack")}
rm(packages)
library("esmpack")

# ---------------------------------------------------------------------------- #
# TODO (JE to review Josip's code below): Import data ----
# ---------------------------------------------------------------------------- #

load("./02_networks/data/final_clean/data_var.RDS")

# ---------------------------------------------------------------------------- #
# TODO (JE to review Josip's code below): Run VAR analyses ----
# ---------------------------------------------------------------------------- #

#we have already previously detrended the data, and now we run the analyses with our own tinterval-transformed time variable,
#with tinterval set to 1.

#---------------------------------------------------------------------
#Run DSEM per each person in Mplus (person-specific VAR)----
#---------------------------------------------------------------------

#---------------------------------------------------------------------#
#Computing the 8-node person-specific network######
#---------------------------------------------------------------------#

#run the analysis

varfit <- list()

if (!(dir.exists("./02_networks/results/Mplus_var"))) {dir.create("./02_networks/results/Mplus_var")}

setwd("./02_networks/results/Mplus_var/")

for (i in 1:length(ids)) {
  tmp <- data_var[data_var$lifepak_id == ids[i], 
                  c("bin_no_adj", "bad_d", "control_d", "energy_d", "focus_d", "fun_d", "interest_d", "movement_d", "sad_d")]
  tmp <- na.omit(tmp)
  names(tmp) <- c("time", "bad", "cont", "ener", "foc", "fun", "int", "move", "sad")
  
  varfit[[i]] <- var2Mplus(y = c("bad", "cont", "ener", "foc", "fun", "int", "move", "sad"), data = tmp,
                           filename = paste0("id", ids[i], ".dat"),
                           variable_options = list(timevar = "time", tinterval = 1),
                           output_options = list(standardized = TRUE),
                           analysis_options = list(biterations.min = 5000,
                                                   chains = 3),
                           runmodel = !file.exists(paste0("id", ids[i], ".out")))
}
rm(tmp)

setwd(wd_dir)

saveRDS(varfit, file="./02_networks/results/varfit.RDS")

#--------------------------------------------
#Computing the 7-node network with control
#-------------------------------------------- 

varfit_control <- list()

if (!(dir.exists("./02_networks/results/Mplus_control"))) {dir.create("./02_networks/results/Mplus_control")}

setwd("./02_networks/results/Mplus_control/")

for (i in 1:length(ids)) {
  tmp <- data_var[data_var$lifepak_id == ids[i], 
                  c("bin_no_adj", "bad_d", "control_d", "energy_d", "focus_d", "interest_d", "movement_d", "sad_d")]
  tmp <- na.omit(tmp)
  names(tmp) <- c("time", "bad", "cont", "ener", "foc", "int", "move", "sad")
  
  varfit_control[[i]] <- var2Mplus(y = c("bad", "cont", "ener", "foc", "int", "move", "sad"), data = tmp,
                                   filename = paste0("id", ids[i], ".dat"),
                                   variable_options = list(timevar = "time", tinterval = 1),
                                   output_options = list(standardized = TRUE),
                                   analysis_options = list(biterations.min = 5000,
                                                           chains = 3),
                                   runmodel = !file.exists(paste0("id", ids[i], ".out")))
}
rm(tmp)

setwd(wd_dir)


#save as RDS file
saveRDS(varfit_control, file="./02_networks/results/varfit_control.RDS")



#---------------------------------------------------
# Computing the 7-node network with fun######
#---------------------------------------------------

varfit_fun <- list()

if (!(dir.exists("./02_networks/results/Mplus_fun"))) {dir.create("./02_networks/results/Mplus_fun")}

setwd("./02_networks/results/Mplus_fun/")

for (i in 1:length(ids)) {
  tmp <- data_var[data_var$lifepak_id == ids[i], 
                  c("bin_no_adj", "bad_d", "energy_d", "focus_d","fun_d", "interest_d", "movement_d", "sad_d")]
  tmp <- na.omit(tmp)
  names(tmp) <- c("time", "bad", "ener", "foc", "fun", "int", "move", "sad")
  
  varfit_fun[[i]] <- var2Mplus(y = c("bad", "ener", "foc", "fun", "int", "move", "sad"), data = tmp,
                               filename = paste0("id", ids[i], ".dat"),
                               variable_options = list(timevar = "time", tinterval = 1),
                               output_options = list(standardized = TRUE),
                               analysis_options = list(biterations.min = 5000,
                                                       chains = 3),
                               runmodel = !file.exists(paste0("id", ids[i], ".out")))
}
rm(tmp)

setwd(wd_dir)

#save as RDS file
saveRDS(varfit_fun, file="./02_networks/results/varfit_fun.RDS")





# ---------------------------------------------------------------------------- #
# TODO (JE to review Josip's code below): Run ML-VAR analyses ----
# ---------------------------------------------------------------------------- #

#-----------------------------------------------------------------------------#
# Computing the 8-node ML-VAR network#####
#-----------------------------------------------------------------------------#


varfit <- list()

if (!(dir.exists("./02_networks/results/Mplus_mlvar"))) {dir.create("./02_networks/results/Mplus_mlvar")}

setwd("./02_networks/results/Mplus_mlvar/")

tmp <- data_var [, c("lifepak_id","bin_no_adj", "bad_d", "control_d", "energy_d", "focus_d", "fun_d", "interest_d", "movement_d", "sad_d")]
tmp <- na.omit(tmp)
names(tmp) <- c("id","time", "bad", "cont", "ener", "foc", "fun", "int", "move", "sad")

#runmodel = TRUE
mlvarfit <- mlvar2Mplus(y = c("bad", "cont", "ener", "foc", "fun", "int", "move", "sad"), id = "id", data= tmp,
                        random.effects = list(lagged = TRUE, slopes = FALSE,
                                              trend = TRUE, rvar = FALSE),
                        variable_options = list(timevar = "time", tinterval = 1),
                        output_options = list(standardized = TRUE),
                        analysis_options = list(biterations.min = 5000,
                                                chains = 3),
                        filename = "mlvar_tinterval.dat",
                        runmodel = !file.exists("mlvar_tinterval.out"))
rm(tmp)


#5000 iterations is enough

setwd(wd_dir)

#save as RDS file
saveRDS(mlvarfit, file="./02_networks/results/mlvarfit.RDS")


#------------------------------------------------------------------------------#                                                                          
# Computing the 7-node ML-VAR network with control#####
#------------------------------------------------------------------------------#

varfit <- list()

if (!(dir.exists("./02_networks/results/Mplus_mlvar_control"))) {dir.create("./02_networks/results/Mplus_mlvar_control")}

setwd("./02_networks/results/Mplus_mlvar_control/")

tmp <- data_var [, c("lifepak_id","bin_no_adj", "bad_d", "control_d", "energy_d", "focus_d", "interest_d", "movement_d", "sad_d")]
tmp <- na.omit(tmp)
names(tmp) <- c("id","time", "bad", "cont", "ener", "foc", "int", "move", "sad")

#runmodel = TRUE
mlvarfit_control <- mlvar2Mplus(y = c("bad", "cont", "ener", "foc", "int", "move", "sad"), id = "id", data= tmp,
                                random.effects = list(lagged = TRUE, slopes = FALSE,
                                                      trend = TRUE, rvar = FALSE),
                                variable_options = list(timevar = "time", tinterval = 1),
                                output_options = list(standardized = TRUE),
                                analysis_options = list(biterations.min = 5000,
                                                        chains = 3),
                                filename = "mlvar_tinterval.dat",
                                runmodel = !file.exists("mlvar_tinterval.out"))
rm(tmp)

setwd(wd_dir)

saveRDS(mlvarfit_control, file="./02_networks/results/mlvarfit_control.RDS")

#------------------------------------------------------------------------------#                                                                          
# Computing the 7-node ML-VAR network with fun#####
#------------------------------------------------------------------------------#


varfit <- list()

if (!(dir.exists("./02_networks/results/Mplus_mlvar_fun"))) {dir.create("./02_networks/results/Mplus_mlvar_fun")}

setwd("./02_networks/results/Mplus_mlvar_fun/")

tmp <- data_var [, c("lifepak_id","bin_no_adj", "bad_d", "energy_d", "focus_d", "fun_d", "interest_d", "movement_d", "sad_d")]
tmp <- na.omit(tmp)
names(tmp) <- c("id","time", "bad", "ener", "foc", "fun", "int", "move", "sad")

#runmodel = TRUE
mlvarfit_fun <- mlvar2Mplus(y = c("bad", "ener", "foc", "fun", "int", "move", "sad"), id = "id", data= tmp,
                            random.effects = list(lagged = TRUE, slopes = FALSE,
                                                  trend = TRUE, rvar = FALSE),
                            variable_options = list(timevar = "time", tinterval = 1),
                            output_options = list(standardized = TRUE),
                            analysis_options = list(biterations.min = 5000,
                                                    chains = 3),
                            filename = "mlvar_tinterval.dat",
                            runmodel = !file.exists("mlvar_tinterval.out"))
rm(tmp)

setwd(wd_dir)

saveRDS(mlvarfit_fun, file="./02_networks/results/mlvarfit_fun.RDS")





