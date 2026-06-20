#### D2: Simulated Mean WTP ###############
# Function: Simulate mean WTP draws for all classes, insects, and wellbeing levels
# Author: Dr Peter King (p.king1@leeds.ac.uk)
# Last Edited: 20/06/2026
# Changes:
## - WARNING: Very compute-intensive (hours on a standard PC; N_Reps_KrinskyRobb=10000)
## - Fixed model path from V1 to V3 (accepted published model)
## - Removed plotting code (moved to XX_Druid_Model_SimulatedMeansPlot.R)
## - Table C1 is now built by 17_Druid_Table_SimulatedMeanWTP.R
## - Adopted script 07 Simulator internals: single mvrnorm matrix, matrix pre-allocation,
##   future_lapply parallel execution, fixed setcolorder bug
## - NOTE: Re-running this script will produce results close but not bitwise identical to
##   the deposited Wide CSV. The deposited file was generated as a side effect of script 07's
##   estimation run (different RNG path). Means agree well within Monte Carlo noise.

# **********************************************************************************
#### Section 0: Replication Information ####
# **********************************************************************************

# here() = "C:/Users/earpkin/OneDrive - University of Leeds/DRUID/D2/Analysis/D2Backup"
# ─ Session info ─────────────────────────────────────
# setting  value
# version  R version 4.5.0 (2025-04-11 ucrt)
# os       Windows 11 x64 (build 22631)
# system   x86_64, mingw32
# ui       RStudio
# language (EN)
# collate  English_United Kingdom.utf8
# ctype    English_United Kingdom.utf8
# tz       Europe/London
# date     2025-05-20
# rstudio  2023.06.2+561 Mountain Hydrangea (desktop)
# pandoc   NA
#
# ─ Packages ─────────────────────────────────────────
# package      * version    date (UTC) lib source
# apollo       * 0.3.5      2025-03-12 [1] CRAN (R 4.5.0)
# bgw            0.1.3      2024-03-29 [1] CRAN (R 4.5.0)
# cli            3.6.5      2025-04-23 [1] CRAN (R 4.5.0)
# coda           0.19-4.1   2024-01-31 [1] CRAN (R 4.5.0)
# data.table   * 1.17.2     2025-05-12 [1] CRAN (R 4.5.0)
# dichromat      2.0-0.1    2022-05-02 [1] CRAN (R 4.5.0)
# digest         0.6.37     2024-08-19 [1] CRAN (R 4.5.0)
# dplyr        * 1.1.4      2023-11-17 [1] CRAN (R 4.5.0)
# farver         2.1.2      2024-05-13 [1] CRAN (R 4.5.0)
# forcats      * 1.0.0      2023-01-29 [1] CRAN (R 4.5.0)
# future       * 1.49.0     2025-04-01 [1] CRAN (R 4.5.0)
# future.apply * 1.11.3     2025-04-01 [1] CRAN (R 4.5.0)
# generics       0.1.4      2025-05-09 [1] CRAN (R 4.5.0)
# ggplot2      * 3.5.2      2025-04-09 [1] CRAN (R 4.5.0)
# ggridges     * 0.5.6      2024-01-23 [1] CRAN (R 4.5.0)
# glue           1.8.0      2024-09-30 [1] CRAN (R 4.5.0)
# gtable         0.3.6      2024-10-25 [1] CRAN (R 4.5.0)
# here         * 1.0.1      2020-12-13 [1] CRAN (R 4.5.0)
# hms            1.1.3      2023-03-21 [1] CRAN (R 4.5.0)
# janitor      * 2.2.1      2024-12-22 [1] CRAN (R 4.5.0)
# lattice        0.22-7     2025-04-02 [1] CRAN (R 4.5.0)
# lifecycle      1.0.4      2023-11-07 [1] CRAN (R 4.5.0)
# lubridate    * 1.9.4      2024-12-08 [1] CRAN (R 4.5.0)
# magrittr     * 2.0.3      2022-03-30 [1] CRAN (R 4.5.0)
# MASS         * 7.3-65     2025-02-28 [1] CRAN (R 4.5.0)
# Matrix         1.7-3      2025-03-11 [1] CRAN (R 4.5.0)
# MatrixModels   0.5-4      2025-03-26 [1] CRAN (R 4.5.0)
# matrixStats    1.5.0      2025-01-07 [1] CRAN (R 4.5.0)
# maxLik         1.5-2.1    2024-03-24 [1] CRAN (R 4.5.0)
# mcmc           0.9-8      2023-11-16 [1] CRAN (R 4.5.0)
# MCMCpack       1.7-1      2024-08-27 [1] CRAN (R 4.5.0)
# miscTools      0.6-28     2023-05-03 [1] CRAN (R 4.5.0)
# mnormt         2.1.1      2022-09-26 [1] CRAN (R 4.5.0)
# mvtnorm        1.3-3      2025-01-10 [1] CRAN (R 4.5.0)
# numDeriv       2016.8-1.1 2019-06-06 [1] CRAN (R 4.5.0)
# pillar         1.10.2     2025-04-05 [1] CRAN (R 4.5.0)
# pkgconfig      2.0.3      2019-09-22 [1] CRAN (R 4.5.0)
# plyr           1.8.9      2023-10-02 [1] CRAN (R 4.5.0)
# purrr        * 1.0.4      2025-02-05 [1] CRAN (R 4.5.0)
# quantreg       6.1        2025-03-10 [1] CRAN (R 4.5.0)
# R6             2.6.1      2025-02-15 [1] CRAN (R 4.5.0)
# randtoolbox    2.0.5      2024-10-18 [1] CRAN (R 4.5.0)
# RColorBrewer   1.1-3      2022-04-03 [1] CRAN (R 4.5.0)
# Rcpp           1.0.14     2025-01-12 [1] CRAN (R 4.5.0)
# readr        * 2.1.5      2024-01-10 [1] CRAN (R 4.5.0)
# reshape2     * 1.4.4      2020-04-09 [1] CRAN (R 4.5.0)
# rlang          1.1.6      2025-04-11 [1] CRAN (R 4.5.0)
# rngWELL        0.10-10    2024-10-17 [1] CRAN (R 4.5.0)
# rprojroot      2.0.4      2023-11-05 [1] CRAN (R 4.5.0)
# RSGHB          1.2.2      2019-07-03 [1] CRAN (R 4.5.0)
# Rsolnp         1.16       2015-12-28 [1] CRAN (R 4.5.0)
# rstudioapi     0.17.1     2024-10-22 [1] CRAN (R 4.5.0)
# sandwich       3.1-1      2024-09-15 [1] CRAN (R 4.5.0)
# scales         1.4.0      2025-04-24 [1] CRAN (R 4.5.0)
# sessioninfo  * 1.2.3      2025-02-05 [1] CRAN (R 4.5.0)
# snakecase      0.11.1     2023-08-27 [1] CRAN (R 4.5.0)
# SparseM        1.84-2     2024-07-17 [1] CRAN (R 4.5.0)
# stringi        1.8.7      2025-03-27 [1] CRAN (R 4.5.0)
# stringr      * 1.5.1      2023-11-14 [1] CRAN (R 4.5.0)
# survival       3.8-3      2024-12-17 [1] CRAN (R 4.5.0)
# tibble       * 3.2.1      2023-03-20 [1] CRAN (R 4.5.0)
# tidyr        * 1.3.1      2024-01-24 [1] CRAN (R 4.5.0)
# tidyselect     1.2.1      2024-03-11 [1] CRAN (R 4.5.0)
# tidyverse    * 2.0.0      2023-02-22 [1] CRAN (R 4.5.0)
# timechange     0.3.0      2024-01-18 [1] CRAN (R 4.5.0)
# truncnorm      1.0-9      2023-03-20 [1] CRAN (R 4.5.0)
# tzdb           0.5.0      2025-03-15 [1] CRAN (R 4.5.0)
# utf8           1.2.5      2025-05-01 [1] CRAN (R 4.5.0)
# vctrs          0.6.5      2023-12-01 [1] CRAN (R 4.5.0)
# withr          3.0.2      2024-10-28 [1] CRAN (R 4.5.0)
# zoo            1.8-14     2025-04-10 [1] CRAN (R 4.5.0)
#
# [1] C:/Users/earpkin/AppData/Local/Programs/R/R-4.5.0/library
# * ── Packages attached to the search path.

options(scipen = 90)
library(tidyverse)
library(data.table)
library(mvtnorm)
library(apollo)
library(future)
library(future.apply)
library(here)


# **********************************************************************************
#### Section 1: Import model ####
# **********************************************************************************

## V3 is the accepted published model (output of 07_Druid_Model_TruncatedLC3C.R).
Model <- readRDS(here("CEOutput/Main/LCM",
                      "D2_Truncated_LC_3C_MXL_NoDR_V3_model.rds"))


# **********************************************************************************
#### Section 2: Define simulator function ####
# **********************************************************************************

## Krinsky-Robb simulation: draw N_Reps_KrinskyRobb parameter vectors from the
## multivariate normal approximation to the MLE (robvarcov), then integrate over
## N_Reps_Draws random taste draws to obtain mean WTP per draw. The distribution
## across N_Reps_KrinskyRobb draws gives the confidence interval.
##
## Draw strategy: generate all taste draws as a single matrix up front (N_Reps_Draws ×
## n_params) rather than separate rnorm calls per parameter. Pre-allocate result_mat
## to avoid repeated list copies inside the loop.
##
## RNG: seeded via L'Ecuyer-CMRG + future.seed=TRUE (set before plan() below).
## Each parallel worker gets an independent reproducible RNG stream — no explicit
## set.seed() calls inside this function.
Simulator <- function(Model, Class, insect, wellbeing) {

  N_Reps_KrinskyRobb <- 10000
  N_Reps_Draws       <- 10000

  param_names <- c("Encounter_Medium", "Encounter_High",
                   "Existence_Medium", "Existence_High",
                   "Bequest_Medium",   "Bequest_High")

  all_params <- c(param_names,
                  paste0("Int_",
                         rep(param_names, each = 2),
                         "_",
                         rep(c("Bee", "Wasp"), times = length(param_names))))

  ## Draw all taste shocks at once: one row per respondent draw, one col per param.
  all_draws <- matrix(rnorm(N_Reps_Draws * length(all_params)),
                      nrow = N_Reps_Draws,
                      ncol = length(all_params))
  colnames(all_draws) <- all_params

  ## Draw KrinskyRobb parameter vectors from MVN approximation to MLE.
  mean_v      <- Model$estimate
  covar       <- as.matrix(Model$robvarcov)
  model_draws <- mvtnorm::rmvnorm(n = N_Reps_KrinskyRobb, mean = mean_v, sigma = covar)

  ## Pre-allocate: one row per KrinskyRobb draw, one col per attribute × level.
  result_mat <- matrix(NA_real_,
                       nrow = N_Reps_KrinskyRobb,
                       ncol = length(param_names))
  colnames(result_mat) <- param_names

  for (i in seq_len(N_Reps_KrinskyRobb)) {
    for (j in seq_along(param_names)) {
      param    <- param_names[j]
      mu_name  <- paste0("mu_",  param, "_Class", Class)
      sig_name <- paste0("sig_", param, "_Class", Class)

      lv_prefix <- ifelse(insect == "Beetle",
                          "Int_LV_Beetle_",
                          paste0("Int_LV_", insect, "_"))
      lv_name <- paste0(lv_prefix, param, "_Class", Class)

      base_wtp <- model_draws[i, mu_name] +
        model_draws[i, sig_name] * all_draws[, param] +
        model_draws[i, lv_name]  * wellbeing

      if (insect != "Beetle") {
        int_mu_name  <- paste0("mu_Int_",  param, "_", insect, "_Class", Class)
        int_sig_name <- paste0("sig_Int_", param, "_", insect, "_Class", Class)
        insect_effect <- model_draws[i, int_mu_name] +
          model_draws[i, int_sig_name] * all_draws[, paste0("Int_", param, "_", insect)]
        total_wtp <- base_wtp + insect_effect
      } else {
        total_wtp <- base_wtp
      }

      result_mat[i, j] <- mean(total_wtp)
    }
  }

  Output         <- as.data.table(result_mat)
  Output[, insect := insect]
  return(Output)
}


# **********************************************************************************
#### Section 3: Run simulator for all combinations ####
# **********************************************************************************

Classes          <- c(1, 2, 3)
Insects          <- c("Beetle", "Bee", "Wasp")
wellbeing_levels <- c(-2, -1, 0, 1, 2)

param_grid <- expand.grid(
  Class     = Classes,
  insect    = Insects,
  wellbeing = wellbeing_levels
)

## Set up reproducible parallel RNG (L'Ecuyer-CMRG gives independent streams per worker).
## This must be called before plan() so the seed propagates to workers.
RNGkind("L'Ecuyer-CMRG")
set.seed(123)
plan(multisession, workers = parallel::detectCores() - 1)

results <- future_lapply(seq_len(nrow(param_grid)), function(i) {
  sim_result           <- Simulator(Model     = Model,
                                    Class     = param_grid$Class[i],
                                    insect    = param_grid$insect[i],
                                    wellbeing = param_grid$wellbeing[i])
  sim_result[, Class     := param_grid$Class[i]]
  sim_result[, wellbeing := param_grid$wellbeing[i]]
  return(sim_result)
}, future.seed = TRUE)

## Reset to sequential after parallel work is done.
plan(sequential)

all_results <- rbindlist(results, fill = TRUE)

## Round numeric columns to 4 d.p. — keeps file size manageable.
num_cols <- setdiff(names(all_results), c("Class", "insect", "wellbeing"))
all_results[, (num_cols) := lapply(.SD, round, 4), .SDcols = num_cols]

setcolorder(all_results, c("Class", "insect", "wellbeing", num_cols))


# **********************************************************************************
#### Section 4: Export ####
# **********************************************************************************

fwrite(all_results,
       here("CEOutput/Main/LCM",
            "D2_Truncated_LC_3C_MXL_NoDR_V3_SimulatedMeans_Wide.csv"),
       sep   = ",",
       quote = FALSE)


# End Of Script **************************************************************
