# ---------------------------------------------------------------------------- #
# Compute Person-Specific Network Parameters -----
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

pkgs <- c("dplyr", "tidyr", "Hmisc", "networktools", "netcontrol")

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
                                               c(3, 6, 7)]) # TODO: Columns "est", "lower_2.5ci", "upper_2.5ci"
  
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
# Computing expected influence for the 8-node VAR network ----
# ---------------------------------------------------------------------------- #

#Computing expected influence for participant 1
expectedInf(adjacency_matrices[["Participant 1"]], step = c("both", 1, 2), directed = TRUE)

#Computing expected influence for all participants
apply_expectedInf <- function(matrix) {
  expectedInf(matrix, step = c("both", 1, 2), directed = TRUE)
}

results_exp_inf <- lapply(adjacency_matrices, apply_expectedInf)

#transform this list into a data frame

expected_influence <- data.frame()

for (participant in names(results_exp_inf)) {
  participant_data <- results_exp_inf[[participant]]
  
  
  for (step in names(participant_data)) {
    step_data <- participant_data[[step]]
    
    temp_df <- data.frame(
      Participant = participant,
      Step = step,
      t(as.data.frame(step_data))
    )
    
    expected_influence <- rbind(expected_influence, temp_df)
  }
}

# Reset row names
rownames(expected_influence) <- NULL


expected_influence_wide <- expected_influence %>%
  pivot_wider(
    names_from = Step, 
    values_from = c(bad, control, energy, focus, fun, interest, movement, sad),
    names_sep = "_"
  ) 

expected_influence_wide <- expected_influence_wide[, c("Participant", 
                                                       "bad_step1", "control_step1", "energy_step1", "focus_step1", "fun_step1", "interest_step1", "movement_step1", "sad_step1",
                                                       "bad_step2", "control_step2", "energy_step2", "focus_step2", "fun_step2", "interest_step2", "movement_step2", "sad_step2")]

all_person_spec_parameters <- expected_influence_wide

#compute mean expected influence of the 2 core depression symptoms (sad and interest)
all_person_spec_parameters <- all_person_spec_parameters %>%
  rowwise() %>%
  mutate(m_ei_s1_sad_int = mean(c(sad_step1, interest_step1), na.rm = TRUE),
         m_ei_s2_sad_int = mean(c(sad_step2, interest_step2), na.rm = TRUE)) %>%
  ungroup()

# ---------------------------------------------------------------------------- #
# Computing sum of signed outgoing edges connecting perceived agency ("control") to the 2 core depression symptoms ("sad", "interest") for the 8-node VAR network ----
# ---------------------------------------------------------------------------- #

sum_outgoing <- results %>%
  filter(Predictor == "control" & (Criterion == "interest" | Criterion == "sad")) %>% 
  group_by(Participant) %>%
  summarise(sum_cont_to_sadint = sum(est, na.rm = TRUE)) %>%
  mutate(Participant_number = as.numeric(gsub(".*[^0-9]", "", Participant))) %>% #have to sort the "Participant" column according to the numeric suffix
  arrange(Participant_number) %>%
  select(-Participant_number)

#compute sum of signed outgoing edges connecting positive activity engagement ("fun") to the 2 core depression symptoms ("sad", "interest")

sum_outgoing_1 <- results %>%
  filter(Predictor == "fun" & (Criterion == "interest" | Criterion == "sad")) %>% 
  group_by(Participant) %>%
  summarise(sum_fun_to_sadint = sum(est, na.rm = TRUE)) %>%
  mutate(Participant_number = as.numeric(gsub(".*[^0-9]", "", Participant))) %>% #have to sort the "Participant" column according to the numeric suffix
  arrange(Participant_number) %>%
  select(-Participant_number)

#join these two dataframes with the sum of signed outgoing edges with the data frame with the rest of parameters

all_person_spec_parameters <- all_person_spec_parameters %>%
  left_join(sum_outgoing, by = "Participant") %>%
  left_join(sum_outgoing_1, by = "Participant")

# ---------------------------------------------------------------------------- #
# Computing density (inter-node, intra-node and overall connectivity - 3 parameters) for the 8-node VAR network ----
# ---------------------------------------------------------------------------- #

#inter-node connectivity 
sum_off_diag <- function(mat) {
  sum(mat) - sum(diag(mat))
}

inter_node_connectivity <- sapply(adjacency_matrices, sum_off_diag)

all_person_spec_parameters$`inter-node connectivity` <- inter_node_connectivity

#intra-node connectivity
sum_diag <- function(mat) {
  sum(diag(mat))
}

all_person_spec_parameters$intra_node_connectivity <- sapply(adjacency_matrices, sum_diag)

#overall connectivity

all_person_spec_parameters$overall_connectivity <- sapply(adjacency_matrices, function(mat) sum(mat))

# ---------------------------------------------------------------------------- #
# Computing controllability centrality for the 8-node VAR network ----
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Computing average controllability centrality for the 8-node VAR network ----
# ---------------------------------------------------------------------------- #

#Computing average control centrality for all participants and transforming the resulting list into a dataframe

results_ave_cont_centrality <- lapply(adjacency_matrices, function(matrix) ave_control_centrality(matrix))

results_ave_cont_centrality <- do.call(rbind, results_ave_cont_centrality)

results_ave_cont_centrality <- as.data.frame(results_ave_cont_centrality)

results_ave_cont_centrality$Participant <- rownames(results_ave_cont_centrality)

rownames(results_ave_cont_centrality) <- NULL

results_ave_cont_centrality <- results_ave_cont_centrality %>% relocate(Participant, .before = V1)

colnames(results_ave_cont_centrality) <- c("Participant", "bad_acc", "control_acc", "energy_acc", "focus_acc", 
                                           "fun_acc", "interest_acc", "movement_acc", "sad_acc")

#compute mean average controllability centrality of the 2 core depression symptoms (sad and interest)
results_ave_cont_centrality <- results_ave_cont_centrality %>%
  rowwise() %>%
  mutate(m_acc_sad_int = mean(c(sad_acc, interest_acc), na.rm = TRUE)) %>%
  ungroup()

# ---------------------------------------------------------------------------- #
# Computing modal controllability centrality for the 8-node VAR network ----
# ---------------------------------------------------------------------------- #

results_modal_cont_centrality <- lapply(adjacency_matrices, function(matrix) modal_control_centrality(matrix))

results_modal_cont_centrality <- do.call(rbind, results_modal_cont_centrality)

results_modal_cont_centrality <- as.data.frame(results_modal_cont_centrality)

results_modal_cont_centrality$Participant <- rownames(results_modal_cont_centrality)

rownames(results_modal_cont_centrality) <- NULL

results_modal_cont_centrality <- results_modal_cont_centrality %>% relocate(Participant, .before = V1)

colnames(results_modal_cont_centrality) <- c("Participant", "bad_mcc", "control_mcc", "energy_mcc", "focus_mcc", 
                                             "fun_mcc", "interest_mcc", "movement_mcc", "sad_mcc")

#compute mean modal controllability centrality of the 2 core depression symptoms (sad and interest)
results_modal_cont_centrality <- results_modal_cont_centrality %>%
  rowwise() %>%
  mutate(m_mcc_sad_int = mean(c(sad_mcc, interest_mcc), na.rm = TRUE)) %>%
  ungroup()

#merge the data frame with expected influence & other parameters with data frames with average control centrality 
#and modal control centrality in a single data frame
all_person_spec_parameters <- all_person_spec_parameters %>%
  left_join(results_ave_cont_centrality, by = "Participant") %>%
  left_join(results_modal_cont_centrality, by = "Participant")

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
# Computing expected influence for all participants - for the 7-node VAR network with control ----
# ---------------------------------------------------------------------------- #

results_exp_inf_control <- lapply(adjacency_matrices_control, apply_expectedInf)

#transform this list into a data frame

expected_influence_control <- data.frame()

for (participant in names(results_exp_inf_control)) {
  participant_data <- results_exp_inf_control[[participant]]
  
  for (step in names(participant_data)) {
    step_data <- participant_data[[step]]
    
    temp_df <- data.frame(
      Participant = participant,
      Step = step,
      t(as.data.frame(step_data))
    )
    
    expected_influence_control <- rbind(expected_influence_control, temp_df)
  }
}

rownames(expected_influence_control) <- NULL

expected_influence_control_wide <- expected_influence_control %>%
  pivot_wider(
    names_from = Step, 
    values_from = c(bad, control, energy, focus, interest, movement, sad),
    names_sep = "_"
  ) 

expected_influence_control_wide <- expected_influence_control_wide [, c("Participant", 
                                                                        "bad_step1", "control_step1", "energy_step1", "focus_step1", "interest_step1", "movement_step1", "sad_step1",
                                                                        "bad_step2", "control_step2", "energy_step2", "focus_step2", "interest_step2", "movement_step2", "sad_step2")]

expected_influence_just_control <- expected_influence_control_wide [, c("Participant", "control_step1", "control_step2")]

colnames(expected_influence_just_control) <- c("Participant", "control_step1_7", "control_step2_7")

# ---------------------------------------------------------------------------- #
# Computing average controllability centrality for the 7-node VAR network with control ----
# ---------------------------------------------------------------------------- #

#ave_control_centrality function

#computing average control centrality for all participants and transforming the resulting list into a dataframe

results_ave_cont_centrality_control <- lapply(adjacency_matrices_control, function(matrix) ave_control_centrality(matrix))

results_ave_cont_centrality_control <- do.call(rbind, results_ave_cont_centrality_control)

results_ave_cont_centrality_control <- as.data.frame(results_ave_cont_centrality_control)

results_ave_cont_centrality_control$Participant <- rownames(results_ave_cont_centrality_control)

rownames(results_ave_cont_centrality_control) <- NULL

results_ave_cont_centrality_control <- results_ave_cont_centrality_control %>% relocate(Participant, .before = V1)

colnames(results_ave_cont_centrality_control) <- c("Participant", "bad_acc", "control_acc", "energy_acc", "focus_acc", 
                                                   "interest_acc", "movement_acc", "sad_acc")

results_ave_cont_centrality_just_control <- results_ave_cont_centrality_control [, c("Participant", "control_acc")]

colnames(results_ave_cont_centrality_just_control) <- c("Participant", "control_acc_7")

# ---------------------------------------------------------------------------- #
# Computing modal controllability centrality for the 7-node VAR network with control ----
# ---------------------------------------------------------------------------- #

results_modal_cont_centrality_control <- lapply(adjacency_matrices_control, function(matrix) modal_control_centrality(matrix))

results_modal_cont_centrality_control <- do.call(rbind, results_modal_cont_centrality_control)

results_modal_cont_centrality_control <- as.data.frame(results_modal_cont_centrality_control)

results_modal_cont_centrality_control$Participant <- rownames(results_modal_cont_centrality_control)

rownames(results_modal_cont_centrality_control) <- NULL

results_modal_cont_centrality_control <- results_modal_cont_centrality_control %>% relocate(Participant, .before = V1)

colnames(results_modal_cont_centrality_control) <- c("Participant", "bad_mcc", "control_mcc", "energy_mcc", "focus_mcc", 
                                                     "interest_mcc", "movement_mcc", "sad_mcc")

results_modal_cont_centrality_just_control <- results_modal_cont_centrality_control[, c("Participant", "control_mcc")]

colnames(results_modal_cont_centrality_just_control) <- c("Participant", "control_mcc_7")

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

#computing expected influence for all participants

results_exp_inf_fun <- lapply(adjacency_matrices_fun, apply_expectedInf)

#transform this list into a data frame

expected_influence_fun <- data.frame()

for (participant in names(results_exp_inf_fun)) {
  participant_data <- results_exp_inf_fun[[participant]]
  
  for (step in names(participant_data)) {
    step_data <- participant_data[[step]]
    
    temp_df <- data.frame(
      Participant = participant,
      Step = step,
      t(as.data.frame(step_data))
    )
    
    expected_influence_fun <- rbind(expected_influence_fun, temp_df)
  }
}

rownames(expected_influence_fun) <- NULL

expected_influence_fun_wide <- expected_influence_fun %>%
  pivot_wider(
    names_from = Step, 
    values_from = c(bad, fun, energy, focus, interest, movement, sad),
    names_sep = "_"
  ) 

expected_influence_fun_wide <- expected_influence_fun_wide [, c("Participant", 
                                                                "bad_step1", "energy_step1", "focus_step1", "fun_step1", "interest_step1", "movement_step1", "sad_step1",
                                                                "bad_step2", "energy_step2", "focus_step2", "fun_step2", "interest_step2", "movement_step2", "sad_step2")]

expected_influence_just_fun <- expected_influence_fun_wide [, c("Participant", "fun_step1", "fun_step2")]

colnames (expected_influence_just_fun) <- c("Participant", "fun_step1_7", "fun_step2_7")

# ---------------------------------------------------------------------------- #
# Computing average contolability centrality for the 7-node VAR network with fun ----
# ---------------------------------------------------------------------------- #

#ave_fun_centrality function

#computing average control centrality for all participants and transforming the resulting list into a dataframe

results_ave_cont_centrality_fun <- lapply(adjacency_matrices_fun, function(matrix) ave_control_centrality(matrix))

results_ave_cont_centrality_fun <- do.call(rbind, results_ave_cont_centrality_fun)

results_ave_cont_centrality_fun <- as.data.frame(results_ave_cont_centrality_fun)

results_ave_cont_centrality_fun$Participant <- rownames(results_ave_cont_centrality_fun)

rownames(results_ave_cont_centrality_fun) <- NULL

results_ave_cont_centrality_fun <- results_ave_cont_centrality_fun %>% relocate(Participant, .before = V1)

colnames(results_ave_cont_centrality_fun) <- c("Participant", "bad_acc", "energy_acc", "focus_acc", 
                                               "fun_acc", "interest_acc", "movement_acc", "sad_acc")

results_ave_cont_centrality_just_fun <- results_ave_cont_centrality_fun [, c("Participant", "fun_acc")]

colnames(results_ave_cont_centrality_just_fun) <- c("Participant", "fun_acc_7")

# ---------------------------------------------------------------------------- #
# Computing modal controlability centrality for the 7-node VAR network with fun ----
# ---------------------------------------------------------------------------- #

results_modal_cont_centrality_fun <- lapply(adjacency_matrices_fun, function(matrix) modal_control_centrality(matrix))

results_modal_cont_centrality_fun <- do.call(rbind, results_modal_cont_centrality_fun)

results_modal_cont_centrality_fun <- as.data.frame(results_modal_cont_centrality_fun)

results_modal_cont_centrality_fun$Participant <- rownames(results_modal_cont_centrality_fun)

rownames(results_modal_cont_centrality_fun) <- NULL

results_modal_cont_centrality_fun <- results_modal_cont_centrality_fun %>% relocate(Participant, .before = V1)

colnames(results_modal_cont_centrality_fun) <- c("Participant", "bad_mcc", "energy_mcc", "focus_mcc", 
                                                 "fun_mcc", "interest_mcc", "movement_mcc", "sad_mcc")

results_modal_cont_centrality_just_fun <- results_modal_cont_centrality_fun[, c("Participant", "fun_mcc")]

colnames(results_modal_cont_centrality_just_fun) <- c("Participant", "fun_mcc_7")

# ---------------------------------------------------------------------------- #
# Merging the entire data frame with person-specific VAR parameters ----
# ---------------------------------------------------------------------------- #

all_person_spec_parameters <- all_person_spec_parameters %>%
  left_join(expected_influence_just_fun, by = "Participant") %>%
  left_join(expected_influence_just_control, by = "Participant") %>%
  left_join(results_ave_cont_centrality_just_control, by = "Participant") %>%
  left_join(results_modal_cont_centrality_just_control, by = "Participant") %>%
  left_join(results_ave_cont_centrality_just_fun, by = "Participant") %>%
  left_join(results_modal_cont_centrality_just_fun, by = "Participant")

# ---------------------------------------------------------------------------- #
# Labeling columns for person-specific VAR parameters ----
# ---------------------------------------------------------------------------- #

all_person_spec_parameters <- all_person_spec_parameters %>%
  mutate_at(vars(bad_step1, control_step1, energy_step1, focus_step1, fun_step1, interest_step1, movement_step1, sad_step1), ~ {
    label(.) <- "One-step expected influence for a given node"
    .
  })

all_person_spec_parameters <- all_person_spec_parameters %>%
  mutate_at(vars(bad_step2, control_step2, energy_step2, focus_step2, fun_step2, interest_step2, movement_step2, sad_step2), ~ {
    label(.) <- "Two-step expected influence for a given node"
    .
  })

label(all_person_spec_parameters$m_ei_s1_sad_int)    <- "Mean one-step expected influence of sad and interest"
label(all_person_spec_parameters$m_ei_s2_sad_int)    <- "Mean two-step expected influence of sad and interest"
label(all_person_spec_parameters$sum_cont_to_sadint) <- "Sum of outgoing edges connecting control to sad and interest"
label(all_person_spec_parameters$sum_fun_to_sadint)  <- "Sum of outgoing edges connecting fun to sad and interest"

all_person_spec_parameters <- all_person_spec_parameters %>%
  mutate_at(vars(bad_acc, control_acc, energy_acc, focus_acc, 
                 fun_acc, interest_acc, movement_acc, sad_acc), ~ {
                   label(.) <- "Average controllability centrality for a given node"
                   .
                 })

label(all_person_spec_parameters$m_acc_sad_int) <- "Mean average controllability centrality of sad and interest"

all_person_spec_parameters <- all_person_spec_parameters %>%
  mutate_at(vars(bad_mcc, control_mcc, energy_mcc, focus_mcc, 
                 fun_mcc, interest_mcc, movement_mcc, sad_mcc), ~ {
                   label(.) <- "Modal controllability centrality for a given node"
                   .
                 })

label(all_person_spec_parameters$m_mcc_sad_int)   <- "Mean modal controllability centrality of sad and interest"
label(all_person_spec_parameters$fun_step1_7)     <- "One-step expected influence of fun in 7-node network"
label(all_person_spec_parameters$fun_step2_7)     <- "Two-step expected influence of fun in 7-node network"
label(all_person_spec_parameters$control_step1_7) <- "One-step expected influence of control in 7-node network"
label(all_person_spec_parameters$control_step2_7) <- "Two-step expected influence of control in 7-node network"
label(all_person_spec_parameters$control_acc_7)   <- "Average controllability centrality of control in a 7-node network"
label(all_person_spec_parameters$fun_acc_7)       <- "Average controllability centrality of fun in a 7-node network"
label(all_person_spec_parameters$control_mcc_7)   <- "Modal controllability centrality of control in a 7-node network"
label(all_person_spec_parameters$fun_mcc_7)       <- "Modal controllability centrality of fun in a 7-node network"

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

#Computing expected influence for all participants
apply_expectedInf <- function(matrix) {
  expectedInf(matrix, step = c("both", 1, 2), directed = TRUE)
}

#Computing expected influence for all participants

results_exp_inf_mlvar <- lapply(adjacency_matrices_mlvar, apply_expectedInf)

#transform this list into a data frame

expected_influence_mlvar <- data.frame()

for (participant in names(results_exp_inf_mlvar)) {
  participant_data <- results_exp_inf_mlvar[[participant]]
  
  for (step in names(participant_data)) {
    step_data <- participant_data[[step]]
    
    temp_df <- data.frame(
      Participant = participant,
      Step = step,
      t(as.data.frame(step_data))
    )
    
    expected_influence_mlvar <- rbind(expected_influence_mlvar, temp_df)
  }
}

rownames(expected_influence_mlvar) <- NULL

expected_influence_wide_mlvar <- expected_influence_mlvar %>%
  pivot_wider(
    names_from = Step, 
    values_from = c(bad, control, energy, focus, fun, interest, movement, sad),
    names_sep = "_"
  ) 

expected_influence_wide_mlvar <- expected_influence_wide_mlvar [, c("Participant", 
                                                                    "bad_step1", "control_step1", "energy_step1", "focus_step1", "fun_step1", "interest_step1", "movement_step1", "sad_step1", 
                                                                    "bad_step2", "control_step2", "energy_step2", "focus_step2", "fun_step2", "interest_step2", "movement_step2", "sad_step2")]

all_person_spec_parameters_mlvar <- expected_influence_wide_mlvar

#compute mean expected influence of the 2 core depression symptoms (sad and interest)
all_person_spec_parameters_mlvar <- all_person_spec_parameters_mlvar %>%
  rowwise() %>%
  mutate(m_ei_s1_sad_int = mean(c(sad_step1, interest_step1),na.rm = TRUE),
         m_ei_s2_sad_int = mean(c(sad_step2, interest_step2), na.rm = TRUE)) %>%
  ungroup()

# ---------------------------------------------------------------------------- #
# Computing sum of signed outgoing edges connecting perceived agency ("control") to the 2 core depression symptoms ("sad", "interest") for the 8-node ML-VAR network ----
# ---------------------------------------------------------------------------- #

sum_outgoing_mlvar <- results_mlvar %>%
  filter(Predictor == "control" & (Criterion == "interest" | Criterion == "sad")) %>% 
  group_by(Participant) %>%
  summarise(sum_cont_to_sadint = sum(est, na.rm = TRUE)) %>%
  mutate(Participant_number = as.numeric(gsub(".*[^0-9]", "", Participant))) %>% #have to sort the "Participant" column according to the numeric suffix
  arrange(Participant_number) %>%
  select(-Participant_number)

#compute sum of signed outgoing edges connecting positive activity engagement ("fun") to the 2 core depression symptoms ("sad", "interest")

sum_outgoing_1_mlvar <- results_mlvar %>%
  filter(Predictor == "fun" & (Criterion == "interest" | Criterion == "sad")) %>% 
  group_by(Participant) %>%
  summarise(sum_fun_to_sadint = sum(est, na.rm = TRUE)) %>%
  mutate(Participant_number = as.numeric(gsub(".*[^0-9]", "", Participant))) %>% #have to sort the "Participant" column according to the numeric suffix
  arrange(Participant_number) %>%
  select(-Participant_number)

#join these two dataframes with the sum of signed outgoind edges with the data frame with the rest of parameters

all_person_spec_parameters_mlvar <- all_person_spec_parameters_mlvar %>%
  left_join(sum_outgoing_mlvar, by = "Participant") %>%
  left_join(sum_outgoing_1_mlvar, by = "Participant")

# ---------------------------------------------------------------------------- #
# Computing ML-VAR density (inter-node, intra-node and overall connectivity - 3 parameters) for the 8-node ML-VAR network ----
# ---------------------------------------------------------------------------- #

#inter-node connectivity 
sum_off_diag <- function(mat) {
  sum(mat) - sum(diag(mat))
}

inter_node_connectivity_mlvar <- sapply(adjacency_matrices_mlvar, sum_off_diag)

all_person_spec_parameters_mlvar$`inter-node connectivity` <- inter_node_connectivity_mlvar

#intra-node connectivity
sum_diag <- function(mat) {
  sum(diag(mat))
}

all_person_spec_parameters_mlvar$intra_node_connectivity <- sapply(adjacency_matrices_mlvar, sum_diag)

#overall connectivity

all_person_spec_parameters_mlvar$overall_connectivity <- sapply(adjacency_matrices_mlvar, function(mat) sum(mat))

# ---------------------------------------------------------------------------- #
# Computing ML-VAR controllability centrality for the 8-node ML-VAR network ----
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Computing average control centrality for the 8-node ML-VAR network ----
# ---------------------------------------------------------------------------- #

results_ave_cont_centrality_mlvar <- lapply(adjacency_matrices_mlvar, function(matrix) ave_control_centrality(matrix))

results_ave_cont_centrality_mlvar <- do.call(rbind, results_ave_cont_centrality_mlvar)

results_ave_cont_centrality_mlvar <- as.data.frame(results_ave_cont_centrality_mlvar)

results_ave_cont_centrality_mlvar$Participant <- rownames(results_ave_cont_centrality_mlvar)

rownames(results_ave_cont_centrality_mlvar) <- NULL

results_ave_cont_centrality_mlvar <- results_ave_cont_centrality_mlvar %>% relocate(Participant, .before = V1)

colnames(results_ave_cont_centrality_mlvar) <- c("Participant", "bad_acc", "control_acc", "energy_acc", "focus_acc", 
                                                  "fun_acc", "interest_acc", "movement_acc", "sad_acc")

#compute mean average controllability centrality of the 2 core depression symptoms (sad and interest)
results_ave_cont_centrality_mlvar <- results_ave_cont_centrality_mlvar %>%
  rowwise() %>%
  mutate(m_acc_sad_int = mean(c(sad_acc, interest_acc), na.rm = TRUE)) %>%
  ungroup()

# ---------------------------------------------------------------------------- #
# Computing ML-VAR modal controllability centrality for the 8-node ML-VAR network ----
# ---------------------------------------------------------------------------- #

results_modal_cont_centrality_mlvar <- lapply(adjacency_matrices_mlvar, function(matrix) modal_control_centrality(matrix))

results_modal_cont_centrality_mlvar <- do.call(rbind, results_modal_cont_centrality_mlvar)

results_modal_cont_centrality_mlvar <- as.data.frame(results_modal_cont_centrality_mlvar)

results_modal_cont_centrality_mlvar$Participant <- rownames(results_modal_cont_centrality_mlvar)

rownames(results_modal_cont_centrality_mlvar) <- NULL

results_modal_cont_centrality_mlvar <- results_modal_cont_centrality_mlvar %>% relocate(Participant, .before = V1)

colnames(results_modal_cont_centrality_mlvar) <- c("Participant", "bad_mcc", "control_mcc", "energy_mcc", "focus_mcc", 
                                                   "fun_mcc", "interest_mcc", "movement_mcc", "sad_mcc")

#compute mean modal controllability centrality of the 2 core depression symptoms (sad and interest)
results_modal_cont_centrality_mlvar <- results_modal_cont_centrality_mlvar %>%
  rowwise() %>%
  mutate(m_mcc_sad_int = mean(c(sad_mcc, interest_mcc), na.rm = TRUE)) %>%
  ungroup()

#merge the data frame with expected influence & other parameters with data frames with average control centrality and 
#modal control centrality in a single data frame
all_person_spec_parameters_mlvar <- all_person_spec_parameters_mlvar %>%
  left_join(results_ave_cont_centrality_mlvar, by = "Participant") %>%
  left_join(results_modal_cont_centrality_mlvar, by = "Participant")

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
# Computing expected influence for all participants for the ML-VAR 7-node network with control ----
# ---------------------------------------------------------------------------- #

results_exp_inf_mlvar_control <- lapply(adjacency_matrices_mlvar_control,
                                        function(matrix) expectedInf(matrix, step = c("both", 1, 2), directed = TRUE))

#transform this list into a data frame

expected_influence_mlvar_control <- data.frame()

for (participant in names(results_exp_inf_mlvar_control)) {
  participant_data <- results_exp_inf_mlvar_control[[participant]]

  for (step in names(participant_data)) {
    step_data <- participant_data[[step]]

    temp_df <- data.frame(
      Participant = participant,
      Step = step,
      t(as.data.frame(step_data))
    )

    expected_influence_mlvar_control <- rbind(expected_influence_mlvar_control, temp_df)
  }
}

rownames(expected_influence_mlvar_control) <- NULL

expected_influence_mlvar_control_wide <- expected_influence_mlvar_control %>%
  pivot_wider(
    names_from = Step, 
    values_from = c(bad, control, energy, focus, interest, movement, sad),
    names_sep = "_"
  ) 

expected_influence_mlvar_control_wide <- expected_influence_mlvar_control_wide [, c("Participant", 
                                                                                    "bad_step1", "control_step1", "energy_step1", "focus_step1", "interest_step1", "movement_step1", "sad_step1", 
                                                                                    "bad_step2", "control_step2", "energy_step2", "focus_step2", "interest_step2", "movement_step2", "sad_step2")]

expected_influence_just_mlvar_control <- expected_influence_mlvar_control_wide [, c("Participant", "control_step1", "control_step2")]

colnames(expected_influence_just_mlvar_control) <- c("Participant", "control_step1_7", "control_step2_7")

# ---------------------------------------------------------------------------- #
# Computing average controllability centrality for the 7-node ML-VAR network with control ----
# ---------------------------------------------------------------------------- #

#ave_mlvar_control_centrality function

#Computing average control centrality for all participants and transforming the resulting list into a dataframe

results_ave_cont_centrality_mlvar_control <- lapply(adjacency_matrices_mlvar_control, function(matrix) ave_control_centrality(matrix))

results_ave_cont_centrality_mlvar_control <- do.call(rbind, results_ave_cont_centrality_mlvar_control)

results_ave_cont_centrality_mlvar_control <- as.data.frame(results_ave_cont_centrality_mlvar_control)

results_ave_cont_centrality_mlvar_control$Participant <- rownames(results_ave_cont_centrality_mlvar_control)

rownames(results_ave_cont_centrality_mlvar_control) <- NULL

results_ave_cont_centrality_mlvar_control <- results_ave_cont_centrality_mlvar_control %>% relocate(Participant, .before = V1)

colnames(results_ave_cont_centrality_mlvar_control) <- c("Participant", "bad_acc", "control_acc", "energy_acc", "focus_acc", 
                                                         "interest_acc", "movement_acc", "sad_acc")

results_ave_cont_centrality_just_mlvar_control <- results_ave_cont_centrality_mlvar_control [, c("Participant", "control_acc")]

colnames(results_ave_cont_centrality_just_mlvar_control) <- c("Participant", "control_acc_7")

# ---------------------------------------------------------------------------- #
# Computing modal controllability centrality for the ML-VAR 7-node network with control ----
# ---------------------------------------------------------------------------- #

results_modal_cont_centrality_mlvar_control <- lapply(adjacency_matrices_mlvar_control, function(matrix) modal_control_centrality(matrix))

results_modal_cont_centrality_mlvar_control <- do.call(rbind, results_modal_cont_centrality_mlvar_control)

results_modal_cont_centrality_mlvar_control <- as.data.frame(results_modal_cont_centrality_mlvar_control)

results_modal_cont_centrality_mlvar_control$Participant <- rownames(results_modal_cont_centrality_mlvar_control)

rownames(results_modal_cont_centrality_mlvar_control) <- NULL

results_modal_cont_centrality_mlvar_control <- results_modal_cont_centrality_mlvar_control %>% relocate(Participant, .before = V1)

colnames(results_modal_cont_centrality_mlvar_control) <- c("Participant", "bad_mcc", "control_mcc", "energy_mcc", "focus_mcc", 
                                                           "interest_mcc", "movement_mcc", "sad_mcc")

results_modal_cont_centrality_just_mlvar_control <- results_modal_cont_centrality_mlvar_control[, c("Participant", "control_mcc")]

colnames(results_modal_cont_centrality_just_mlvar_control) <- c("Participant", "control_mcc_7")

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
# Computing expected influence for all participants for the ML-VAR 7-node network with fun ----
# ---------------------------------------------------------------------------- #

results_exp_inf_mlvar_fun <- lapply(adjacency_matrices_mlvar_fun,
                                        function(matrix) expectedInf(matrix, step = c("both", 1, 2), directed = TRUE))

#transform this list into a data frame

expected_influence_mlvar_fun <- data.frame()

for (participant in names(results_exp_inf_mlvar_fun)) {
  participant_data <- results_exp_inf_mlvar_fun[[participant]]
  
  for (step in names(participant_data)) {
    step_data <- participant_data[[step]]
    
    temp_df <- data.frame(
      Participant = participant,
      Step = step,
      t(as.data.frame(step_data))
    )
    
    expected_influence_mlvar_fun <- rbind(expected_influence_mlvar_fun, temp_df)
  }
}

rownames(expected_influence_mlvar_fun) <- NULL

expected_influence_mlvar_fun_wide <- expected_influence_mlvar_fun %>%
  pivot_wider(
    names_from = Step, 
    values_from = c(bad, energy, focus, fun, interest, movement, sad),
    names_sep = "_"
  ) 

expected_influence_mlvar_fun_wide <- expected_influence_mlvar_fun_wide [, c("Participant", 
                                                                            "bad_step1", "energy_step1", "focus_step1", "fun_step1", "interest_step1", "movement_step1", "sad_step1", 
                                                                            "bad_step2", "energy_step2", "focus_step2", "fun_step2", "interest_step2", "movement_step2", "sad_step2")]

expected_influence_just_mlvar_fun <- expected_influence_mlvar_fun_wide [, c("Participant", "fun_step1", "fun_step2")]

colnames(expected_influence_just_mlvar_fun) <- c("Participant", "fun_step1_7", "fun_step2_7")

# ---------------------------------------------------------------------------- #
# Computing average controllability centrality for the ML-VAR 7-node network with fun ----
# ---------------------------------------------------------------------------- #

#ave_mlvar_fun_centrality function

#Computing average control centrality for all participants and transforming the resulting list into a dataframe

results_ave_cont_centrality_mlvar_fun <- lapply(adjacency_matrices_mlvar_fun, function(matrix) ave_control_centrality(matrix))

results_ave_cont_centrality_mlvar_fun <- do.call(rbind, results_ave_cont_centrality_mlvar_fun)

results_ave_cont_centrality_mlvar_fun <- as.data.frame(results_ave_cont_centrality_mlvar_fun)

results_ave_cont_centrality_mlvar_fun$Participant <- rownames(results_ave_cont_centrality_mlvar_fun)

rownames(results_ave_cont_centrality_mlvar_fun) <- NULL

results_ave_cont_centrality_mlvar_fun <- results_ave_cont_centrality_mlvar_fun %>% relocate(Participant, .before = V1)

colnames(results_ave_cont_centrality_mlvar_fun) <- c("Participant", "bad_acc", "energy_acc", "focus_acc", "fun_acc",
                                                     "interest_acc", "movement_acc", "sad_acc")

results_ave_cont_centrality_just_mlvar_fun <- results_ave_cont_centrality_mlvar_fun [, c("Participant", "fun_acc")]

colnames(results_ave_cont_centrality_just_mlvar_fun) <- c("Participant", "fun_acc_7")

# ---------------------------------------------------------------------------- #
# Computing modal controllability centrality for the ML-VAR 7-node network with fun ----
# ---------------------------------------------------------------------------- #

results_modal_cont_centrality_mlvar_fun <- lapply(adjacency_matrices_mlvar_fun, function(matrix) modal_control_centrality(matrix))

results_modal_cont_centrality_mlvar_fun <- do.call(rbind, results_modal_cont_centrality_mlvar_fun)

results_modal_cont_centrality_mlvar_fun <- as.data.frame(results_modal_cont_centrality_mlvar_fun)

results_modal_cont_centrality_mlvar_fun$Participant <- rownames(results_modal_cont_centrality_mlvar_fun)

rownames(results_modal_cont_centrality_mlvar_fun) <- NULL

results_modal_cont_centrality_mlvar_fun <- results_modal_cont_centrality_mlvar_fun %>% relocate(Participant, .before = V1)

colnames(results_modal_cont_centrality_mlvar_fun) <- c("Participant", "bad_mcc", "energy_mcc", "focus_mcc", "fun_mcc",
                                                       "interest_mcc", "movement_mcc", "sad_mcc")

results_modal_cont_centrality_just_mlvar_fun <- results_modal_cont_centrality_mlvar_fun[, c("Participant", "fun_mcc")]

colnames(results_modal_cont_centrality_just_mlvar_fun) <- c("Participant", "fun_mcc_7")

# ---------------------------------------------------------------------------- #
# Merging the entire data frame with person-specific parameters from ML-VAR ----
# ---------------------------------------------------------------------------- #

all_person_spec_parameters_mlvar <- all_person_spec_parameters_mlvar %>%
  left_join(expected_influence_just_mlvar_fun, by = "Participant") %>%
  left_join(expected_influence_just_mlvar_control, by = "Participant") %>%
  left_join(results_ave_cont_centrality_just_mlvar_control, by = "Participant") %>%
  left_join(results_modal_cont_centrality_just_mlvar_control, by = "Participant") %>%
  left_join(results_ave_cont_centrality_just_mlvar_fun, by = "Participant") %>%
  left_join(results_modal_cont_centrality_just_mlvar_fun, by = "Participant")

# ---------------------------------------------------------------------------- #
# Labeling the columns for person-specific parameters from ML-VAR ----
# ---------------------------------------------------------------------------- #

all_person_spec_parameters_mlvar <- all_person_spec_parameters_mlvar %>%
  mutate_at(vars(bad_step1, control_step1, energy_step1, focus_step1, fun_step1, interest_step1, movement_step1, sad_step1), ~ {
    label(.) <- "ML_VAR One-step expected influence for a given node"
    .
  })

all_person_spec_parameters_mlvar <- all_person_spec_parameters_mlvar %>%
  mutate_at(vars(bad_step2, control_step2, energy_step2, focus_step2, fun_step2, interest_step2, movement_step2, sad_step2), ~ {
    label(.) <- "ML_VAR Two-step expected influence for a given node"
    .
  })

label(all_person_spec_parameters_mlvar$m_ei_s1_sad_int)    <- "ML_VAR Mean one-step expected influence of sad and interest"
label (all_person_spec_parameters_mlvar$m_ei_s2_sad_int)   <- "ML_VAR Mean two-step expected influence of sad and interest"
label(all_person_spec_parameters_mlvar$sum_cont_to_sadint) <- "ML_VAR Sum of outgoing edges connecting control to sad and interest"
label(all_person_spec_parameters_mlvar$sum_fun_to_sadint)  <- "ML_VAR Sum of outgoing edges connecting fun to sad and interest"

all_person_spec_parameters_mlvar <- all_person_spec_parameters_mlvar %>%
  mutate_at(vars(bad_acc, control_acc, energy_acc, focus_acc, 
                 fun_acc, interest_acc, movement_acc, sad_acc), ~ {
                   label(.) <- "ML_VAR Average controllability centrality for a given node"
                   .
                 })

label(all_person_spec_parameters_mlvar$m_acc_sad_int) <- "ML_VAR Mean average controllability centrality of sad and interest"

all_person_spec_parameters_mlvar <- all_person_spec_parameters_mlvar %>%
  mutate_at(vars(bad_mcc, control_mcc, energy_mcc, focus_mcc, 
                 fun_mcc, interest_mcc, movement_mcc, sad_mcc), ~ {
                   label(.) <- "ML_VAR Modal controllability centrality for a given node"
                   .
                 })

label(all_person_spec_parameters_mlvar$m_mcc_sad_int)   <- "ML_VAR Mean modal controllability centrality of sad and interest"
label(all_person_spec_parameters_mlvar$fun_step1_7)     <- "ML_VAR One-step expected influence of fun in 7-node network"
label(all_person_spec_parameters_mlvar$fun_step2_7)     <- "ML_VAR Two-step expected influence of fun in 7-node network"
label(all_person_spec_parameters_mlvar$control_step1_7) <- "ML_VAR One-step expected influence of control in 7-node network"
label(all_person_spec_parameters_mlvar$control_step2_7) <- "ML_VAR Two-step expected influence of control in 7-node network"
label(all_person_spec_parameters_mlvar$control_acc_7)   <- "ML_VAR Average controllability centrality of control in a 7-node network"
label(all_person_spec_parameters_mlvar$fun_acc_7)       <- "ML_VAR Average controllability centrality of fun in a 7-node network"
label(all_person_spec_parameters_mlvar$control_mcc_7)   <- "ML_VAR Modal controllability centrality of control in a 7-node network"
label(all_person_spec_parameters_mlvar$fun_mcc_7)       <- "ML_VAR Modal controllability centrality of fun in a 7-node network"

# ---------------------------------------------------------------------------- #
# Merging the data frames with person-specific parameters from VAR and person-specific parameters extracted from ML-VAR ----
# ---------------------------------------------------------------------------- #

#changing the names of all columns in the ML-VAR data frame so that they end with _mlvar
colnames(all_person_spec_parameters_mlvar) <- paste0(colnames(all_person_spec_parameters_mlvar), "_mlvar")

colnames(all_person_spec_parameters_mlvar)[colnames(all_person_spec_parameters_mlvar) == "Participant_mlvar"] <- "Participant"

#merging the two data frames with person-specific parameters

all_person_spec_parameters_var_mlvar <- all_person_spec_parameters %>%
  left_join(all_person_spec_parameters_mlvar, by = "Participant")

# TODO: Save parameters as RDS file