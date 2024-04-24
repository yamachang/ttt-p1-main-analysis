# ---------------------------------------------------------------------------- #
# Further Clean Data and Align Observations in Time
# Authors: Jeremy W. Eberle, Josip Razum, Sebastian Castro-Alvarez
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

pkgs <- c("dplyr", "tidyr")
groundhog.library(pkgs, groundhog_day)

# ---------------------------------------------------------------------------- #
# Import intermediately cleaned data ----
# ---------------------------------------------------------------------------- #

# Load data for all participants

dat <- read.csv2("./02_networks/data/intermediate_clean/deid_cleaned_lifepak_ttt_phase_1.csv",
                 header = TRUE, sep = ",")

# Load test data (before and after applying Mplus TINTERVAL for "lifepak_id" 26232)

df_befor_tint <- read.table("./02_networks/data/test_mplus_tinterval/id26232_before_tinterval.dat", na.strings = "*",
                            col.names = c("time", "int", "sad"))
df_after_tint <- read.table("./02_networks/data/test_mplus_tinterval/id26232_after_tinterval.dat", na.strings = "*",
                            col.names = c("int", "sad", "int_and_1", "sad_and_1", "time", "newtime", "bint"))

# ---------------------------------------------------------------------------- #
# Further clean data ----
# ---------------------------------------------------------------------------- #

# Order by "lifepak_id"

dat <- dat[order(dat$lifepak_id), ]

# Convert time variables to POSIXct

dat$notification_time <- as.POSIXct(gsub("T", "", dat$notification_time), tz = "GMT")
dat$response_time     <- as.POSIXct(gsub("T", "", dat$response_time),     tz = "GMT")

# ---------------------------------------------------------------------------- #
# Inspect "notification_time" ----
# ---------------------------------------------------------------------------- #

# Obtain range of first "notification_time" across participants. Participant 34516 
# has only one row (so will be excluded from analysis due to too few observations), 
# whose "notification_time" is "22:07:24 GMT". After excluding this participant,
# the earliest of the first notifications was at "07:31:15 GMT" and the latest of
# the first notifications was at "11:00:58 GMT".

dat_split <- split(dat, dat$lifepak_id)

# View(dat[dat$lifepak_id == 34516, ])

range(unlist(lapply(dat_split, function(x) {
  format(min(x$notification_time), format = "%H:%M:%S %Z")
}))) == c("07:31:15 GMT", "22:07:24 GMT")

range(unlist(lapply(dat_split[names(dat_split) != 34516], function(x) {
  format(min(x$notification_time), format = "%H:%M:%S %Z")
}))) == c("07:31:15 GMT", "11:00:58 GMT")

  # Not sure why participant 815120's first notification was at "11:00:58 GMT"; 
  # by the next day, their notifications began coming closer to 7:30 am. Only two 
  # other participants' first notifications came after 10:30 am (546191, 908905),
  # and their first notifications the next day were closer to 7:30 am too. After
  # ignoring these participants, the latest first notification is at "10:26:34 GMT".

# View(dat[dat$lifepak_id == 815120, ])
# View(dat[dat$lifepak_id == 546191, ])
# View(dat[dat$lifepak_id == 908905, ])

range(unlist(lapply(dat_split[!(names(dat_split) %in% 
                                  c(34516, 815120, 546191, 908905))], function(x) {
  format(min(x$notification_time), format = "%H:%M:%S %Z")
}))) == c("07:31:15 GMT", "10:26:34 GMT")

# Obtain earliest "notification_time" across participants. After excluding this 
# participant, all participants have earliest notification times that are plausible 
# to have occurred within the first 2-3 hr window, which started at 7:30 am (per the
# preregistration; https://doi.org/10.17605/OSF.IO/35W8R).

range(unlist(lapply(dat_split, function(x) {
  min(format((x$notification_time), format = "%H:%M:%S %Z"))
}))) == c("07:30:01 GMT", "22:07:24 GMT")

range(unlist(lapply(dat_split[names(dat_split) != 34516], function(x) {
  min(format((x$notification_time), format = "%H:%M:%S %Z"))
}))) == c("07:30:01 GMT", "08:42:41 GMT")

# ---------------------------------------------------------------------------- #
# Compute "hr_since_start" ----
# ---------------------------------------------------------------------------- #

# Compute difference in hours between 7:30 am on the date of each participant's 
# first notification the current response_time

dat <- dat %>%
  group_by(lifepak_id) %>%
  mutate(first_notification_date = as.Date(min(notification_time)),
         study_start_time = as.POSIXct(paste(first_notification_date, "07:30:00"), 
                                       format = "%Y-%m-%d %H:%M:%S", tz= "GMT")) %>%
  ungroup()

dat$first_notification_date <- NULL

dat <- dat %>%
  mutate(hr_since_start = as.numeric(difftime(response_time, study_start_time, units = "hours")))

# ---------------------------------------------------------------------------- #
# Compute alternative versions of "hr_since_start" ----
# ---------------------------------------------------------------------------- #

# (Not used) For comparison to Mplus's TINTERVAL below (at least when applied to one 
# participant), also compute response time in hours since each participant's first 
# notification

# dat <- dat %>%
#   group_by(lifepak_id) %>%
#   mutate(first_notification_time = min(notification_time)) %>%
#   ungroup() %>%
#   mutate(hr_since_start_alt = as.numeric(difftime(response_time, first_notification_time, units = "hours")))
# 
# dat$first_notification_time <- NULL
# 
# dat$hr_since_start_alt <- NULL # Don't use this variable

# (Not used) In original analyses, Sebastian turned response times into hours
# and started counting for each individual on the first day that they have a valid 
# observation. The hours correspond to the number of hours since the 00:00 hours 
# of that first day. Therefore, if a person completed the first esm questionnaire 
# at 9:30, the response time is 9.5. Also, if the completed another esm questionnaire 
# the next day at 8:00, then the response time is 32.

# tmp_time <- tapply(dat$response_time, 
#                    dat$lifepak_id, 
#                    FUN = function(x) {
#                      as.numeric(x) - as.numeric(min(x[which(!is.na(x))])) %/% 
#                        86400 * 86400
#                    }, 
#                    simplify = TRUE)
# 
# dat$hr_since_start_seb <- unlist(tmp_time)/3600
# 
# dat$hr_since_start_seb <- NULL # Don't use this variable

# ---------------------------------------------------------------------------- #
# Remove irrelevant rows ----
# ---------------------------------------------------------------------------- #

# Check that "response_no" is always ascending consecutive integers

dat_split <- split(dat, dat$lifepak_id)

all(unlist(lapply(dat_split, function(x) {
  all(x$response_no == 1:length(x$response_no))
})))

# Remove rows with repeated notification times and no responses and then recode
# "response_no" into "response_no_cln" as ascending consecutive integers

tapply(dat$notification_time,
       dat$lifepak_id,
       FUN = function(x) {
         sum(duplicated(x))
       })

# View(dat[dat$lifepak_id == 816917, ]) # Repeated: 2020-10-11 18:10:49
# View(dat[dat$lifepak_id == 905207, ]) # Repeated: 2020-11-28 09:24:24

node_vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")

dat_split <- split(dat, dat$lifepak_id)

dat_split <- lapply(dat_split, function(x) {
  dup_times <- unique(x$notification_time[duplicated(x$notification_time)])
  
  x <- x[!(x$notification_time %in% dup_times & 
             rowSums(is.na(x[, node_vars])) == length(node_vars)), ]
  
  x$response_no_cln <- NA
  x$response_no_cln <- 1:nrow(x)
  
  return(x)
})

dat <- do.call(rbind, dat_split)

# Remove 177 rows after the last expected beep (105 expected). All of these
# have no responses on the variables of interest, and their notification times
# are all 8:00:00 (suggesting these rows are not for the typical EMA beeps).

5*21 == 105 # Expected number of beeps for everyone (5 beeps per day*21 days)

nrow(dat[dat$response_no_cln > 105, ]) == 177

nrow(dat[dat$response_no_cln > 105 &
           rowSums(is.na(dat[, node_vars])) == length(node_vars), ]) == 177

dat <- dat[!(dat$response_no_cln > 105 &
               rowSums(is.na(dat[, node_vars])) == length(node_vars)), ]

# ---------------------------------------------------------------------------- #
# Inspect missing beeps ----
# ---------------------------------------------------------------------------- #

# Note: 7 participants have fewer than 105 beeps. Laura Jans indicated on 4/1/24
# that (a) 5 (IDs 60988, 546191, 558692, 659070, 697540) likely deleted the app 
# or got a new phone without informing the study team; (b) 1 (ID 155884) withdrew 
# from study because they were no longer interested; and (c) 1 (ID 34516) accidentally 
# deleted the app and got a new phone and informed study team and thus has data under 
# both ID 21441-034516 (1 row from "3T_P1_V2_NIS_21200_958251_Download2.csv") and
# ID 21476-878753 (20 rows from "3T_P1_V2_NIS_21200_958251_Download3.csv"), although
# the clean data retains only the row from "3T_P1_V2_NIS_21200_958251_Download2.csv"
# (in any case, this participant will be excluded due to having too few rows).

table(dat$response_no_cln, useNA = "always")

dat_split <- split(dat, dat$lifepak_id)

missing_beep_ids <- names(which(unlist(lapply(dat_split, function(x) {
  max(x$response_no_cln) < 105
}))))

# sink(file = "2024.04.01_ids_with_missing_beeps.txt")
# lapply(dat_split[missing_beep_ids], function(x) {
#   cat("lifepak_id:", as.character(unique(x$lifepak_id)), "\n")
#   cat("number of beeps:", max(x$response_no_cln), "\n")
#   cat("notification time of first and last beep:", "\n")
#   print(range(x$notification_time))
#   cat("\n\n")
# })
# sink()

# ---------------------------------------------------------------------------- #
# Remove rows outside beep completion window ----
# ---------------------------------------------------------------------------- #

# Compute time to respond after notification and check whether it is ever more 
# than 2 hr (or 60*60*2 = 7200 secs). This occurred for 23 observations across
# 17 participants, when participants started to respond within the 2-hr window 
# but did not complete their response within the 2 hr.

dat$not_res_time_diff <- NA
dat$not_res_time_diff <- dat$response_time - dat$notification_time

max(dat$not_res_time_diff, na.rm = TRUE) == 43264   # Seconds
round(43264 / 60, 2)                     == 721.07  # Minutes
round(721.07 / 60, 2)                    == 12.02   # Hours

sum(dat$not_res_time_diff > 60*60*2, na.rm = TRUE) == 23 # Observations

dat_split <- split(dat, dat$lifepak_id)

sum(unlist(lapply(dat_split, function(x) {
  any(x$not_res_time_diff > 60*60*2, na.rm = TRUE)
})))                                               == 17  # Participants

# Remove these observations by changing "completed_session" to 0 and making
# most of response-related columns NA

target_vars <- names(dat)[!(names(dat) %in% c("lifepak_id", "response_no",
                                              "response_no_cln", "notification_time"))]

for (i in 1:nrow(dat)) {
  if (!is.na(dat$not_res_time_diff[i]) & dat$not_res_time_diff[i] > 60*60*2) {
    dat[i, target_vars] <- NA
    dat$completed_session[i] <- 0
  }
}

# ---------------------------------------------------------------------------- #
# Further clean data ----
# ---------------------------------------------------------------------------- #

# Check whether "response_time" is ascending (i.e., is not unsorted)

dat_split <- split(dat, dat$lifepak_id)

sum(unlist(lapply(dat_split, function(x) {
  is.unsorted(x$response_time, na.rm = TRUE, strictly = TRUE)
}))) == 0

# Check if "response_time" for a row is ever greater than "notification_time" of 
# next row

no_issues <- 0

for (lifepak_id in unique(dat$lifepak_id)) {
  tmp <- dat[dat$lifepak_id == lifepak_id, ]
  
  if (any((tmp$response_time > lead(tmp$notification_time)) & 
          !is.na(lead(tmp$response_time)), na.rm = TRUE)) {
    print("response_time for a given row is greater than notification_time for next row for lifepak_id: ")
    print(unique(tmp$lifepak_id))
  } else {
    no_issues <- no_issues + 1
  }
}

no_issues == length(unique(dat$lifepak_id))

# ---------------------------------------------------------------------------- #
# Remove rows with negative scores ----
# ---------------------------------------------------------------------------- #

# TODO: Josip to insert code for excluding negative scores (e.g., -1, -2)

#This shows that the emotions were measured based on a visual analogue scale (VAS) from 0 to 100. For these analyses, we focus 
#on the symptoms *interest*,*sad*, *bad*,*energy*,*focus*,*movement*, as well as *control* and *positive activity engagement*. 
#Notice, that there are `r sum(mlvardata[, "interest"] < 0, na.rm = TRUE)` observations with negative scores on the variable *interest*. 
#As this should not be the case, we further explore these observations.
#First, we create a subset of the data that only includes these observations and the variables *ID*, *interest*, *sad*,*bad*,*energy*,*focus*,*movement*,
#*control*,*positive activity engagement*:

dat_minus0 <- dat[dat[, "interest"] < 0 & 
                                !is.na(dat[, "interest"]), 
                              c("lifepak_id", "response_time", 
                                "interest", "sad","bad","energy","focus","movement","control","fun")]

#The negative scores either -1 or -2 and come from `r nsub(mlvardata_minus0$lifepak_id)` subjects. 
#The complete data (not just those data points with negative values on interest) of these subjects is stored in the following data.frame: 

dat_neg <- dat[dat$lifepak_id %in% 
                             unique(dat_minus0$lifepak_id), 
                           c("lifepak_id", "response_time", 
                             "interest", "sad","bad","energy","focus","movement","control","fun")]

#As the data from these subjects are not reliable, we decide to exclude them from the analyses. 
#Thus, we create another data.frame without their data.

dat02 <- dat[!(dat$lifepak_id %in% 
                             unique(dat_minus0$lifepak_id)), ]

rm(dat_neg, dat_minus0)

# TODO: Josip to compute sample size at this point

n_distinct(dat02$lifepak_id)

#sample size is n=105 at this point.

# ---------------------------------------------------------------------------- #
# Exclude participants with too few observations ----
# ---------------------------------------------------------------------------- #

# TODO: Josip to insert code for excluding participants who do not have at least 
# 60 observations across all nodes

#first we need to load Sebastian's package "esmpack". We can move this at the beginning of the script later.
packages <- rownames(installed.packages())
if (!"esmpack" %in% packages) {
  remotes::install_github("secastroal/esmpack")
}
library(esmpack)

# Create indicator variable of valid scores on all 8 variables
dat02$ind_nomiss <- ifelse(is.na(dat02$interest) &
                                   is.na(dat02$sad) & is.na(dat02$bad)& is.na(dat02$energy) & is.na(dat02$focus)& is.na(dat02$movement) & is.na(dat02$control)& is.na(dat02$fun) ,NA, 1)

# Get compliance on all variables per person
comp_all <- calc.nomiss(ind_nomiss, lifepak_id, dat02)

# Get IDs of subjects with 60 or more responses on all variables
id_include <- as.numeric(names(which(comp_all >= 60)))

# Exclude subjects with less than 60 responses on both variables
dat03 <- dat02[dat02$lifepak_id %in% id_include, ]

# Remove indicator variable in final subset
dat03 <- dat03[, 1:15]


# TODO: Josip to compute sample size at this point

n_distinct(dat03$lifepak_id)

#sample size is n=64 at this point.

# ---------------------------------------------------------------------------- #
# Exclude participants with too little variation ----
# ---------------------------------------------------------------------------- #

# TODO: Josip to insert code to exclude participants who do not have enough 
# variation on their time series

sd_interest <- tapply(dat03$interest, dat03$lifepak_id, sd, na.rm = TRUE) 
sd_sad <- tapply(dat03$sad, dat03$lifepak_id, sd, na.rm = TRUE) 

sd_bad <- tapply(dat03$energy, dat03$lifepak_id, sd, na.rm = TRUE) 
sd_energy <- tapply(dat03$energy, dat03$lifepak_id, sd, na.rm = TRUE) 

sd_focus <- tapply(dat03$focus, dat03$lifepak_id, sd, na.rm = TRUE) 
sd_movement <- tapply(dat03$movement, dat03$lifepak_id, sd, na.rm = TRUE) 

sd_control <- tapply(dat03$control, dat03$lifepak_id, sd, na.rm = TRUE) 
sd_fun <- tapply(dat03$fun, dat03$lifepak_id, sd, na.rm = TRUE) 


sd_lower <- ifelse(sd_interest < 10 | sd_sad < 10 | sd_bad < 10 | sd_energy < 10 | sd_focus < 10 | sd_movement < 10 |  sd_control < 10 |  sd_fun < 10, TRUE, FALSE)
sd_exclude <- as.numeric(names(which(sd_lower)))

dat_sd10 <- dat03[!(dat03$lifepak_id %in% sd_exclude), ]

n_distinct(dat_sd10$lifepak_id)

#sample size is n=48 at this point. 

# TODO: Josip to compute sample size at this point if we require SD of at least 1,
# 2, 3, 4, 5, 6, 7, 8, 9, or 10 (so we can make a final decision)

sd_lower9.5 <- ifelse(sd_interest < 9.5 | sd_sad < 9.5 | sd_bad < 9.5 | sd_energy < 9.5 | sd_focus < 9.5 | sd_movement < 9.5 |  sd_control < 9.5 |  sd_fun < 9.5, TRUE, FALSE)
sd_exclude9.5 <- as.numeric(names(which(sd_lower9.5)))

dat_sd09.5 <- dat03[!(dat03$lifepak_id %in% sd_exclude9.5), ]

n_distinct(dat_sd09.5$lifepak_id)

#n=51 with sd=9.5.

sd_lower9 <- ifelse(sd_interest < 9 | sd_sad < 9 | sd_bad < 9 | sd_energy < 9 | sd_focus < 9 | sd_movement < 9 |  sd_control < 9 |  sd_fun < 9, TRUE, FALSE)
sd_exclude9 <- as.numeric(names(which(sd_lower9)))

dat_sd09 <- dat03[!(dat03$lifepak_id %in% sd_exclude9), ]

n_distinct(dat_sd09$lifepak_id)


#n=51 with sd=9.Notice that this is the same as with sd=9.5 that Sebastian used.

sd_lower8 <- ifelse(sd_interest < 8 | sd_sad < 8 | sd_bad < 8 | sd_energy < 8 | sd_focus < 8 | sd_movement < 8 |  sd_control < 8 |  sd_fun < 8, TRUE, FALSE)
sd_exclude8 <- as.numeric(names(which(sd_lower8)))

dat_sd08 <- dat03[!(dat03$lifepak_id %in% sd_exclude8), ]

n_distinct(dat_sd08$lifepak_id)

#n=53 with sd=8.

sd_lower7 <- ifelse(sd_interest < 7 | sd_sad < 7 | sd_bad < 7 | sd_energy < 7 | sd_focus < 7 | sd_movement < 7 |  sd_control < 7 |  sd_fun < 7, TRUE, FALSE)
sd_exclude7 <- as.numeric(names(which(sd_lower7)))

dat_sd07 <- dat03[!(dat03$lifepak_id %in% sd_exclude7), ]

n_distinct(dat_sd07$lifepak_id)

#n=53 with sd=7.

sd_lower6 <- ifelse(sd_interest < 6 | sd_sad < 6 | sd_bad < 6 | sd_energy < 6 | sd_focus < 6 | sd_movement < 6 |  sd_control < 6 |  sd_fun < 6, TRUE, FALSE)
sd_exclude6 <- as.numeric(names(which(sd_lower6)))

dat_sd06 <- dat03[!(dat03$lifepak_id %in% sd_exclude6), ]

n_distinct(dat_sd06$lifepak_id)

#n=54 with sd=6.

sd_lower5 <- ifelse(sd_interest < 5 | sd_sad < 5 | sd_bad < 5 | sd_energy < 5 | sd_focus < 5 | sd_movement < 5 |  sd_control < 5 |  sd_fun < 5, TRUE, FALSE)
sd_exclude5 <- as.numeric(names(which(sd_lower5)))

dat_sd05 <- dat03[!(dat03$lifepak_id %in% sd_exclude5), ]

n_distinct(dat_sd05$lifepak_id)

#n=56 with sd=5.

sd_lower4 <- ifelse(sd_interest < 4 | sd_sad < 4 | sd_bad < 4 | sd_energy < 4 | sd_focus < 4 | sd_movement < 4 |  sd_control < 4 |  sd_fun < 4, TRUE, FALSE)
sd_exclude4 <- as.numeric(names(which(sd_lower4)))

dat_sd04 <- dat03[!(dat03$lifepak_id %in% sd_exclude4), ]

n_distinct(dat_sd04$lifepak_id)

#n=57 with sd=4.

sd_lower3 <- ifelse(sd_interest < 3 | sd_sad < 3 | sd_bad < 3 | sd_energy < 3 | sd_focus < 3 | sd_movement < 3 |  sd_control < 3 |  sd_fun < 3, TRUE, FALSE)
sd_exclude3 <- as.numeric(names(which(sd_lower3)))

dat_sd03 <- dat03[!(dat03$lifepak_id %in% sd_exclude3), ]

n_distinct(dat_sd03$lifepak_id)

#n=57 with sd=3.

sd_lower2 <- ifelse(sd_interest < 2 | sd_sad < 2 | sd_bad < 2 | sd_energy < 2 | sd_focus < 2 | sd_movement < 2 |  sd_control < 2 |  sd_fun < 2, TRUE, FALSE)
sd_exclude2 <- as.numeric(names(which(sd_lower2)))

dat_sd02 <- dat03[!(dat03$lifepak_id %in% sd_exclude2), ]

n_distinct(dat_sd02$lifepak_id)

#n=59 with sd=2.

sd_lower1 <- ifelse(sd_interest < 1 | sd_sad < 1 | sd_bad < 1 | sd_energy < 1 | sd_focus < 1 | sd_movement < 1 |  sd_control < 1 |  sd_fun < 1, TRUE, FALSE)
sd_exclude1 <- as.numeric(names(which(sd_lower1)))

dat_sd01 <- dat03[!(dat03$lifepak_id %in% sd_exclude1), ]

n_distinct(dat_sd01$lifepak_id)

#n=60 with sd=1.

# ---------------------------------------------------------------------------- #
# Align observations to ensure equal time intervals (mimic Mplus's TINTERVAL) ----
# ---------------------------------------------------------------------------- #

dat_bin <- dat

# Remove rows with no "hr_since_start" value

dat_bin <- dat_bin[!is.na(dat_bin$hr_since_start), ]

# Note: One row has "hr_since_start" value but NAs on all nodes. Laura Jans indicated 
# on 4/3/24 that "prefer not to answer" was not an option for node variables and that 
# participants could not just click "yes" to skip the question without moving the slider. 
# Thus, it is unclear why these responses are NA.

nrow(dat_bin[rowSums(is.na(dat_bin[, node_vars])) == length(node_vars), ]) == 1

# View(dat_bin[rowSums(is.na(dat_bin[, node_vars])) == length(node_vars), ])
# View(dat_bin[dat_bin$lifepak_id == 806721, ]) # "response_no" 14 with "notification_time" "2020-09-22 18:04:51"

# Bin "hr_since_start"

max_hr_since_start <- ceiling(max(dat_bin$hr_since_start))
time_interval <- 2.5

breaks <- seq(0, max_hr_since_start + 2, time_interval)

dat_bin$bin_range <- cut(dat_bin$hr_since_start, breaks = breaks, right = FALSE, dig.lab = 4)
dat_bin$bin_no <- as.integer(dat_bin$bin_range)

# Check for multiple observations in same bin

tapply(dat_bin$bin_no,
       dat_bin$lifepak_id,
       FUN = function(x) {
         sum(duplicated(x))
       })

# Adjust bin number to prevent multiple observations in same bin

dat_bin_split <- split(dat_bin, dat_bin$lifepak_id)

adjust_bin_no <- function(df) {
  df <- df[order(df$hr_since_start), ]
  
  df$bin_no_adj <- df$bin_no
  
  if (nrow(df) > 1) {
    for (j in 2:nrow(df)) {
      if (df$bin_no_adj[j] == df$bin_no_adj[j - 1]) {
        if (j == 2) {
          if (df$bin_no_adj[j - 1] != 1) {
            df$bin_no_adj[j - 1] <- df$bin_no_adj[j] - 1
          } else if (df$bin_no_adj[j - 1] == 1 & 
                       df$bin_no_adj[j] != df$bin_no_adj[j + 1]) {
            df$bin_no_adj[j] <- df$bin_no_adj[j] + 1
          }
        } else if (j > 2) {
          if (df$bin_no_adj[j - 1] - 1 != df$bin_no_adj[j - 2]) {
            df$bin_no_adj[j - 1] <- df$bin_no_adj[j] - 1
          } else if (df$bin_no_adj[j] != df$bin_no_adj[j + 1]) {
            df$bin_no_adj[j] <- df$bin_no_adj[j] + 1
          } else {
            warning("No empty bin above or below present bin.")
          }
        }
      }
    }
  }
  
  # Add adjusted bin range (first ensure that "bin_no_adj" does not surpass last 
  # available level in "bin_range")
  
  bin_range_levels <- levels(df$bin_range)
  
  if (max(df$bin_no_adj) > length(bin_range_levels)) {
    stop("bin_no_adj surpasses last available level of bin_range")
  }
  
  df$bin_range_adj <- bin_range_levels[df$bin_no_adj]
  
  # Indicate what new bin number was used
  
  df$bin_no_diff <- ifelse(df$bin_no == df$bin_no_adj, NA, df$bin_no_adj)
  
  return(df)
}

dat_bin_split <- lapply(dat_bin_split, adjust_bin_no)

dat_bin <- do.call(rbind, dat_bin_split)

# Inspect four test cases (works as expected)

test1 <- dat_bin[dat_bin$lifepak_id == 206517, ]
test2 <- dat_bin[dat_bin$lifepak_id == 302021, ]
test3 <- dat_bin[dat_bin$lifepak_id == 479327, ]
test4 <- dat_bin[dat_bin$lifepak_id == 877140, ]

# View(test1)
# View(test2)
# View(test3)
# View(test4)

# Check for multiple observations in adjusted bin number

sum(tapply(dat_bin$bin_no_adj,
           dat_bin$lifepak_id,
           FUN = function(x) {
             sum(duplicated(x))
           }) != 0) == 0

# Add missing rows to reflect all possible bins (use "bin_no_adj" as time variable in analysis)

length_bin_range_levels <- length(levels(dat_bin$bin_range))

dat_bin <- dat_bin %>%
  group_by(lifepak_id) %>%
  complete(bin_no_adj = 1:length_bin_range_levels)

range(dat_bin$bin_no_adj) == c(1, 199)

# Rearrange columns

dat_bin <- dat_bin[, c("lifepak_id",
                       "response_no", "response_no_cln", 
                       "study_start_time", "notification_time", "response_time",
                       "hr_since_start", "bin_no", "bin_range", 
                       "bin_no_adj", "bin_range_adj", "bin_no_diff", 
                       "time_diff", "not_res_time_diff", 
                       "completed_session", node_vars)]

# ---------------------------------------------------------------------------- #
# Compare aligned data with test data from Mplus's TINTERVAL for one participant ----
# ---------------------------------------------------------------------------- #

# Compare to test data from Mplus for lifepak_id 26232 for node "interest" (or "int")
# as example when using "hr_since_bin_alt" (which is what Mplus uses for the original
# time variable, but we ultimately do not use it above). Note that in "df_after_tint", 
# column "newtime" is likely the midpoint of the range for a given bin and "bint" likely 
# stands for "bin time" and is the time variable used in analysis. The vectors are the 
# same except for the one from Mplus lacks an NA at the end (due to our basing 
# "max_hr_since_start" on all participants) and observations 

# test5 <- dat_bin[dat_bin$lifepak_id == 26232, ]
# 
# View(test5)
# View(df_after_tint)
# 
# test5$interest <- as.numeric(test5$interest)
# df_after_tint_int_fill <- c(df_after_tint$int, NA)
# 
# identical(test5$interest[1:136], df_after_tint_int_fill[1:136])
# identical(test5$interest[139:199], df_after_tint_int_fill[139:199])
# 
# test5$interest[137:138]         # NA 0
# df_after_tint_int_fill[137:138] # 0  NA

# ---------------------------------------------------------------------------- #
# Remove unneeded columns ----
# ---------------------------------------------------------------------------- #

dat_anlys <- dat_bin[, c("lifepak_id",
                         "hr_since_start", "bin_no_adj", "bin_range_adj",
                         node_vars)]

# ---------------------------------------------------------------------------- #
# Export data ----
# ---------------------------------------------------------------------------- #

dir.create("./02_networks/data/final_clean")

save(dat_bin,   file = "./02_networks/data/final_clean/dat_bin.RData")
save(dat_anlys, file = "./02_networks/data/final_clean/dat_anlys.RData")