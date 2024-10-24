# ---------------------------------------------------------------------------- #
# Extract Person-Specific Temporal Network Results -----
# Authors: Josip Razum, Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running script, restart R (CTRL+SHIFT+F10 on Windows) and set working 
# directory to parent folder

# ---------------------------------------------------------------------------- #
# Store working directory, check correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("./02_networks/code/01_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# Load packages

pkgs <- c("dplyr", "tidyr", "Hmisc", "networktools", "netcontrol") # TODO: Determine what packages are needed

groundhog.library(pkgs, groundhog_day)

# ---------------------------------------------------------------------------- #
# Import results ----
# ---------------------------------------------------------------------------- #

varfit           <- readRDS(file="./02_networks/results/varfit.RDS")
varfit_control   <- readRDS(file="./02_networks/results/varfit_control.RDS")
varfit_fun       <- readRDS(file="./02_networks/results/varfit_fun.RDS")

mlvarfit         <- readRDS(file="./02_networks/results/mlvarfit.RDS")
mlvarfit_control <- readRDS(file="./02_networks/results/mlvarfit_control.RDS")
mlvarfit_fun     <- readRDS(file="./02_networks/results/mlvarfit_fun.RDS")

# Load Lifepak IDs

load("./02_networks/data/final_clean/ids.RDS")

# ---------------------------------------------------------------------------- #
# Label idiographic VAR fit list elements by "lifepak_id" ----
# ---------------------------------------------------------------------------- #

# Define function to label idiographic VAR fit list elements by "lifepak_id"

name_var_fit_elements <- function(fit, lifepak_ids) {
  if (length(fit) == length(lifepak_ids)) {
    names(fit) <- lifepak_ids
  } else {
    stop("Length of VAR fit list differs from length of Lifepak IDs")
  }
  
  return(fit)
}

# Run function for VAR fit list elements

varfit         <- name_var_fit_elements(varfit,         ids)
varfit_control <- name_var_fit_elements(varfit_control, ids)
varfit_fun     <- name_var_fit_elements(varfit_fun,     ids)

# ---------------------------------------------------------------------------- #
# TODO: Check that ML-VAR clusters correspond to Lifepak IDs ----
# ---------------------------------------------------------------------------- #

# TODO: Although all ML-VAR clusters reflect Lifepak IDs, the order differs

length(intersect(ids, unique(mlvarfit$parameters$wilevel.standardized$stdyx.standardized$cluster))) == length(ids)

ids
unique(mlvarfit$parameters$wilevel.standardized$stdyx.standardized$cluster)





# ---------------------------------------------------------------------------- #
# 1. Compute network parameters from 8-node VAR model ----
# ---------------------------------------------------------------------------- #

# Extract autoregressive and cross-lagged temporal model coefficients (parameters 
# 1-64, whereas parameters 65-92 are contemporaneous relations, parameters 93-100
# are intercepts, and parameters 101-108 are residual variances)

  # TODO: Ideally extract rows and columns by name below

parameters <- paste0("par", 1:64)
results <- lapply(varfit, function(x) {
  out <- cbind(parameters, 
               x$parameters$stdyx.standardized[c(1:64),     # TODO: Rows where "paramHeader" contains ".ON"
                                               c("est", "lower_2.5ci", "upper_2.5ci")])
  
  return(out)
})

# Stack results for all participants in one data frame

results <- do.call(rbind, results)

# Create a column that signifies to which participant each set of coefficients belongs

  # TODO: Avoid hard-coding with 64

num_participants <- nrow(results) / 64

  # TODO: Label "varfit" list by participant ID and then use that to add participant IDs here instead
  # (and change column name to lowercase)

participant_labels <- rep(paste0("Participant ", 1:num_participants), each = 64,
                          length.out = nrow(results))

results$Participant <- participant_labels

results <- results[, c("Participant", "parameters", "est", "lower_2.5ci", "upper_2.5ci")]

# TODO (avoid hard-coding with order of names): Create column with criterion variable names

variables <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")

repeated_variables <- rep(rep(variables, each = 8), length.out = nrow(results))

results$Criterion <- repeated_variables # TODO: Change column name to lowercase

# Create column with predictor variable names

repeated_predictor_variables <- rep(variables, length.out = nrow(results))

results$Predictor <- repeated_predictor_variables # TODO: Change column name to lowercase

# Replace nonsignificant edges with zeroes

results$est <- ifelse((results$lower_2.5ci > 0 & results$upper_2.5ci > 0) |
                        (results$lower_2.5ci < 0 & results$upper_2.5ci < 0), results$est, 0)

# Define function to create directed adjacency matrix of autoregressive and 
# cross-lagged coefficients for a given participant, where each row is the 
# predictor and each column is the criterion)

create_adjacency_matrix <- function(participant_data) {
  variables <- unique(c(participant_data$Criterion, participant_data$Predictor))
  adj_matrix <- matrix(NA, nrow = length(variables), ncol = length(variables)) # TODO: Initialized with NA instead of 0
  rownames(adj_matrix) <- variables
  colnames(adj_matrix) <- variables
  for (i in 1:nrow(participant_data)) {
    predictor <- participant_data$Predictor[i]
    criterion <- participant_data$Criterion[i]
    relationship <- participant_data$est[i]
    adj_matrix[predictor, criterion] <- relationship
  }
  return(adj_matrix)
}

# Group data by participant and create adjacency matrices

  # TODO: Have to convert Participant to factor to preserve order. Otherwise,
  # group_split() will sort Participant alphabetically (e.g., "Participant 1",
  # then "Participant 10"). But use actual participant IDs instead of this.

results$Participant <- factor(results$Participant, levels = unique(results$Participant))

adjacency_matrices <- results %>%
  group_by(Participant) %>%
  group_split() %>%
  lapply(create_adjacency_matrix)

# Name list elements by Participant IDs

names(adjacency_matrices) <- unique(results$Participant)

# TODO: If Participant is not converted to factor in Line, 129 above, results will
# match for first element but not for others

adjacency_matrices[[1]]
View(varfit[[1]]$parameters$stdyx.standardized)

adjacency_matrices[[3]]
View(varfit[[3]]$parameters$stdyx.standardized)

# TODO: Separate extraction of model coefficients and creation of adjacency
# matrices from computation of network parameters. Also, condense code using
# custom functions rather than repeating code across several models.





# ---------------------------------------------------------------------------- #
# Computing the person-specific parameters in a 7-node VAR network with control ----
# ---------------------------------------------------------------------------- #

#extracting the relevant parameters
parameters_control <- paste0("par", 1:49)
results_control <- lapply(varfit_control, function(x) {
  out <- cbind(parameters_control, 
               x$parameters$stdyx.standardized[c(1:49),
                                               c(3, 6, 7)])
  
  return(out)
})

results_control <- do.call(rbind, results_control)

# ---------------------------------------------------------------------------- #
# Computing the parameters in a 7-node VAR network with control ----
# ---------------------------------------------------------------------------- #

#create a column in the dataframe that signifies to which participant do the given set of coefficients belong. 
# Calculate the number of participants
num_participants <- ceiling(nrow(results_control) / 49)

# Create the participant labels
participant_labels <- rep(paste0("Participant ", 1:num_participants), each = 49, length.out = nrow(results_control))

results_control$Participant <- participant_labels

results_control <- results_control[, c("Participant", "parameters_control", "est", "lower_2.5ci", "upper_2.5ci")]

#create the column with criterion variable names

variables_control <- c("bad", "control", "energy", "focus", "interest", "movement", "sad")

repeated_variables <- rep(rep(variables_control, each = 7), length.out = nrow(results_control))

results_control$Criterion <- repeated_variables

#create the column with predictor variable names
repeated_predictor_variables <- rep(variables_control, length.out = nrow(results_control))

results_control$Predictor <- repeated_predictor_variables

#replacing non-significant edges with zeroes

results_control$est <- ifelse((results_control$lower_2.5ci > 0 & results_control$upper_2.5ci > 0) |
                                (results_control$lower_2.5ci < 0 & results_control$upper_2.5ci < 0), results_control$est, 0)

#create an adjacency matrix of auto-regressive and cross-regressive coefficients
#create a directed adjacency matrix for each participant

# Function to create adjacency matrix for a given participant's data
create_adjacency_matrix <- function(participant_data) {
  variables_control <- unique(c(participant_data$Criterion, participant_data$Predictor))
  adj_matrix <- matrix(0, nrow = length(variables_control), ncol = length(variables_control))
  rownames(adj_matrix) <- variables_control
  colnames(adj_matrix) <- variables_control
  for (i in 1:nrow(participant_data)) {
    predictor <- participant_data$Predictor[i]
    criterion <- participant_data$Criterion[i]
    relationship <- participant_data$est[i]
    adj_matrix[predictor, criterion] <- relationship
  }
  return(adj_matrix)
}

# Group data by participant and create adjacency matrices
adjacency_matrices_control <- results_control %>%
  group_by(Participant) %>%
  group_split() %>%
  lapply(create_adjacency_matrix)

# Naming the list elements by Participant IDs 
names(adjacency_matrices_control) <- unique(results_control$Participant)

# ---------------------------------------------------------------------------- #
# Computing the parameters in a 7-node VAR network with fun ----
# ---------------------------------------------------------------------------- #

#extracting the relevant parameters
parameters_fun <- paste0("par", 1:49)
results_fun <- lapply(varfit_fun, function(x) {
  out <- cbind(parameters_fun, 
               x$parameters$stdyx.standardized[c(1:49),
                                               c(3, 6, 7)])
  
  return(out)
})

results_fun <- do.call(rbind, results_fun)
rm(parameters_fun)

#create a column in the dataframe that signifies to which participant do the given set of coefficients belong. 
# Calculate the number of participants
num_participants <- ceiling(nrow(results_fun) / 49)

#create participant labels
participant_labels <- rep(paste0("Participant ", 1:num_participants), each = 49, length.out = nrow(results_fun))

results_fun$Participant <- participant_labels

results_fun <- results_fun[, c("Participant", "parameters_fun", "est", "lower_2.5ci", "upper_2.5ci")]

#create the column with criterion variable names

variables_fun <- c("bad", "energy", "focus", "fun", "interest", "movement", "sad")

repeated_variables <- rep(rep(variables_fun, each = 7), length.out = nrow(results_fun))

results_fun$Criterion <- repeated_variables

#create the column with predictor variable names
repeated_predictor_variables <- rep(variables_fun, length.out = nrow(results_fun))

results_fun$Predictor <- repeated_predictor_variables

#replacing non-significant edges with zeroes

results_fun$est <- ifelse((results_fun$lower_2.5ci > 0 & results_fun$upper_2.5ci > 0) |
                            (results_fun$lower_2.5ci < 0 & results_fun$upper_2.5ci < 0), results_fun$est, 0)

#create an adjacency matrix of auto-regressive and cross-regressive coefficients
#create a directed adjacency matrix for each participant

# function to create adjacency matrix for a given participant's data
create_adjacency_matrix <- function(participant_data) {
  variables_fun <- unique(c(participant_data$Criterion, participant_data$Predictor))
  adj_matrix <- matrix(0, nrow = length(variables_fun), ncol = length(variables_fun))
  rownames(adj_matrix) <- variables_fun
  colnames(adj_matrix) <- variables_fun
  for (i in 1:nrow(participant_data)) {
    predictor <- participant_data$Predictor[i]
    criterion <- participant_data$Criterion[i]
    relationship <- participant_data$est[i]
    adj_matrix[predictor, criterion] <- relationship
  }
  return(adj_matrix)
}

# Group data by participant and create adjacency matrices
adjacency_matrices_fun <- results_fun %>%
  group_by(Participant) %>%
  group_split() %>%
  lapply(create_adjacency_matrix)

# Naming the list elements by Participant IDs 
names(adjacency_matrices_fun) <- unique(results_fun$Participant)

# ---------------------------------------------------------------------------- #
# Extracting the person-specific parameters from the 8-node ML-VAR network ----
# ---------------------------------------------------------------------------- #

parameters_mlvar_solution <- mlvarfit$parameters$wilevel.standardized$stdyx.standardized

# ---------------------------------------------------------------------------- #
# Creating the adjacency matrix for the 8-node ML-VAR network parameters ----
# ---------------------------------------------------------------------------- #

results_mlvar <- parameters_mlvar_solution

# Subset the dataframe to keep only the desired rows
results_mlvar <- results_mlvar %>%
  group_by(cluster) %>%
  slice_head(n = 64) %>%
  ungroup()

#create a column in the dataframe that signifies to which participant do the given set of coefficients belong. 
# calculate the number of participants
num_participants <- ceiling(nrow(results_mlvar) / 64)

# create participant labels
participant_labels <- rep(paste0("Participant ", 1:num_participants), each = 64, length.out = nrow(results_mlvar))

# add the new column to the dataframe
results_mlvar$Participant <- participant_labels

results_mlvar <- results_mlvar[, c("Participant", "paramHeader", "param", "est", "lower_2.5ci", "upper_2.5ci", "sig", "cluster")]

#create the column with criterion variable names

variables <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")

repeated_variables <- rep(rep(variables, each = 8), length.out = nrow(results_mlvar))

results_mlvar$Criterion <- repeated_variables

#create the column with predictor variable names
repeated_predictor_variables <- rep(variables, length.out = nrow(results_mlvar))

results_mlvar$Predictor <- repeated_predictor_variables

#replacing non-significant edges with zeroes

results_mlvar$est <- ifelse((results_mlvar$lower_2.5ci > 0 & results_mlvar$upper_2.5ci > 0) |
                              (results_mlvar$lower_2.5ci < 0 & results_mlvar$upper_2.5ci < 0), results_mlvar$est, 0)

#create an adjacency matrix of auto-regressive and cross-regressive coefficients
#create a directed adjacency matrix for each participant

# function to create adjacency matrix for a given participant's data
create_adjacency_matrix <- function(participant_data) {
  variables <- unique(c(participant_data$Criterion, participant_data$Predictor))
  adj_matrix <- matrix(0, nrow = length(variables), ncol = length(variables))
  rownames(adj_matrix) <- variables
  colnames(adj_matrix) <- variables
  for (i in 1:nrow(participant_data)) {
    predictor <- participant_data$Predictor[i]
    criterion <- participant_data$Criterion[i]
    relationship <- participant_data$est[i]
    adj_matrix[predictor, criterion] <- relationship
  }
  return(adj_matrix)
}

# group data by participant and create adjacency matrices
adjacency_matrices_mlvar <- results_mlvar %>%
  group_by(Participant) %>%
  group_split() %>%
  lapply(create_adjacency_matrix)

# naming the list elements by Participant IDs 
names(adjacency_matrices_mlvar) <- unique(results_mlvar$Participant)

# ---------------------------------------------------------------------------- #
# Computing the person-specific parameters for the 7-node ML-VAR network with control ----
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Extracting the person-specific parameters for the 7-node ML-VAR network with control ----
# ---------------------------------------------------------------------------- #

parameters_mlvar_solution_control <- mlvarfit_control$parameters$wilevel.standardized$stdyx.standardized

results_mlvar_control <- parameters_mlvar_solution_control

# Subset the dataframe to keep only the desired rows
results_mlvar_control <- results_mlvar_control %>%
  group_by(cluster) %>%
  slice_head(n = 49) %>%
  ungroup()

#create a column in the dataframe that signifies to which participant do the given set of coefficients belong. 
# Calculate the number of participants
num_participants <- ceiling(nrow(results_mlvar_control) / 49)

# Create the participant labels
participant_labels <- rep(paste0("Participant ", 1:num_participants), each = 49, length.out = nrow(results_mlvar_control))

# Add the new column to the dataframe
results_mlvar_control$Participant <- participant_labels

results_mlvar_control <- results_mlvar_control[, c("Participant", "paramHeader", "param", "est", "lower_2.5ci", "upper_2.5ci", "sig", "cluster")]

#create the column with criterion variable names

variables <- c("bad", "control", "energy", "focus", "interest", "movement", "sad")

repeated_variables <- rep(rep(variables, each = 7), length.out = nrow(results_mlvar_control))

results_mlvar_control$Criterion <- repeated_variables

#create the column with predictor variable names
repeated_predictor_variables <- rep(variables, length.out = nrow(results_mlvar_control))

results_mlvar_control$Predictor <- repeated_predictor_variables

#replacing non-significant edges with zeroes

results_mlvar_control$est <- ifelse((results_mlvar_control$lower_2.5ci > 0 & results_mlvar_control$upper_2.5ci > 0) |
                              (results_mlvar_control$lower_2.5ci < 0 & results_mlvar_control$upper_2.5ci < 0), results_mlvar_control$est, 0)

# ---------------------------------------------------------------------------- #
# Creating the adjacency matrix from ML-VAR parameters for the 7-node ML-VAR network with control ----
# ---------------------------------------------------------------------------- #

#create an adjacency matrix of auto-regressive and cross-regressive coefficients
#create a directed adjacency matrix for each participant

# Function to create adjacency matrix for a given participant's data
create_adjacency_matrix <- function(participant_data) {
  variables_mlvar_control <- unique(c(participant_data$Criterion, participant_data$Predictor))
  adj_matrix <- matrix(0, nrow = length(variables_mlvar_control), ncol = length(variables_mlvar_control))
  rownames(adj_matrix) <- variables_mlvar_control
  colnames(adj_matrix) <- variables_mlvar_control
  for (i in 1:nrow(participant_data)) {
    predictor <- participant_data$Predictor[i]
    criterion <- participant_data$Criterion[i]
    relationship <- participant_data$est[i]
    adj_matrix[predictor, criterion] <- relationship
  }
  return(adj_matrix)
}

# Group data by participant and create adjacency matrices
adjacency_matrices_mlvar_control <- results_mlvar_control %>%
  group_by(Participant) %>%
  group_split() %>%
  lapply(create_adjacency_matrix)

# Naming the list elements by Participant IDs 
names(adjacency_matrices_mlvar_control) <- unique(results_mlvar_control$Participant)

# ---------------------------------------------------------------------------- #
# Computing the person-specific parameters for the 7-node ML-VAR network with fun ----
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Extracting the person-specific parameters for the 7-node ML-VAR network with fun ----
# ---------------------------------------------------------------------------- #

parameters_mlvar_solution_fun <- mlvarfit_fun$parameters$wilevel.standardized$stdyx.standardized

# ---------------------------------------------------------------------------- #
# Creating the adjacency matrix from ML-VAR parameters for the 7-node network with fun ----
# ---------------------------------------------------------------------------- #

mlvarfit_fun <- readRDS(file="./02_networks/results/mlvarfit_fun.RDS")

results_mlvar_fun <- parameters_mlvar_solution_fun

# Subset the dataframe to keep only the desired rows
results_mlvar_fun <- results_mlvar_fun %>%
  group_by(cluster) %>%
  slice_head(n = 49) %>%
  ungroup()

#create a column in the dataframe that signifies to which participant do the given set of coefficients belong. 
# Calculate the number of participants
num_participants <- ceiling(nrow(results_mlvar_fun) / 49)

# Create the participant labels
participant_labels <- rep(paste0("Participant ", 1:num_participants), each = 49, length.out = nrow(results_mlvar_fun))

# Add the new column to the dataframe
results_mlvar_fun$Participant <- participant_labels

results_mlvar_fun <- results_mlvar_fun[, c("Participant", "paramHeader", "param", "est", "lower_2.5ci", "upper_2.5ci", "sig", "cluster")]

#create the column with criterion variable names

variables <- c("bad", "energy", "focus", "fun", "interest", "movement", "sad")

repeated_variables <- rep(rep(variables, each = 7), length.out = nrow(results_mlvar_fun))

results_mlvar_fun$Criterion <- repeated_variables

#create the column with predictor variable names
repeated_predictor_variables <- rep(variables, length.out = nrow(results_mlvar_fun))

results_mlvar_fun$Predictor <- repeated_predictor_variables

#replacing non-significant edges with zeroes

results_mlvar_fun$est <- ifelse((results_mlvar_fun$lower_2.5ci > 0 & results_mlvar_fun$upper_2.5ci > 0) |
                                      (results_mlvar_fun$lower_2.5ci < 0 & results_mlvar_fun$upper_2.5ci < 0), results_mlvar_fun$est, 0)

# ---------------------------------------------------------------------------- #
# Creating the adjacency matrix from ML-VAR parameters for the 7-node network with fun ----
# ---------------------------------------------------------------------------- #

#create an adjacency matrix of auto-regressive and cross-regressive coefficients
#create a directed adjacency matrix for each participant

# Function to create adjacency matrix for a given participant's data
create_adjacency_matrix <- function(participant_data) {
  variables_mlvar_fun <- unique(c(participant_data$Criterion, participant_data$Predictor))
  adj_matrix <- matrix(0, nrow = length(variables_mlvar_fun), ncol = length(variables_mlvar_fun))
  rownames(adj_matrix) <- variables_mlvar_fun
  colnames(adj_matrix) <- variables_mlvar_fun
  for (i in 1:nrow(participant_data)) {
    predictor <- participant_data$Predictor[i]
    criterion <- participant_data$Criterion[i]
    relationship <- participant_data$est[i]
    adj_matrix[predictor, criterion] <- relationship
  }
  return(adj_matrix)
}

# Group data by participant and create adjacency matrices
adjacency_matrices_mlvar_fun <- results_mlvar_fun %>%
  group_by(Participant) %>%
  group_split() %>%
  lapply(create_adjacency_matrix)

# Naming the list elements by Participant IDs 
names(adjacency_matrices_mlvar_fun) <- unique(results_mlvar_fun$Participant)

# ---------------------------------------------------------------------------- #
# TODO: Export temporal network results ----
# ---------------------------------------------------------------------------- #




