#### D2: Insects survey ####
## Function: Assign class membership from LCM and export covariate data
## Author: Dr Peter King (p.king1@leeds.ac.uk)
## Last change: 23/04/2026
## Notes:
# - Depends on 03_Druid_Setup_MergeCE.R (Data_Covariates) and 06_Druid_Model_TruncatedLC3C.R (Pi values)
# - Outputs Data_Covariates_Step6.csv used by all downstream figure/table scripts


## ****************************************************************************
#### Stage Zero: Replication and libraries ####
## ****************************************************************************

## Replication Information:
# here() = "C:/Users/earpkin/OneDrive - University of Leeds/DRUID/D2/Analysis/D2Backup"
# version  R version 4.5.0 (2025-04-11 ucrt)
# os       Windows 11 x64 (build 22631)
# See 00_D2_Replicator.R for full package list

library(data.table)
library(magrittr)
library(dplyr)
library(here)


## ****************************************************************************
#### Stage One: Load data ####
## ****************************************************************************


## Respondent data in long format
database <- here("Data/Main", "database_Step3.csv") %>% fread() %>% data.frame()

## Respondent data in wide format
# Data_Covariates <- here("Data/Main",
#                         "Data_Covariates_Spatial_Step5_old.csv") %>% fread() %>% data.frame()

Data_Covariates <- here("Data/Main",
                        "Data_Covariates_Step5.csv") %>% fread() %>% data.frame()


## ****************************************************************************
#### Stage Two: Assign class membership ####
## ****************************************************************************


## Easier to grab SerialSQ from long-format database and append to wide
Data_Covariates$SerialSQ <- database %>%
  distinct(Respondent, SerialSQ) %>%
  .$SerialSQ

## Drop protest bids, serial non-participants, and always-none respondents
Data_Covariates <- Data_Covariates %>% dplyr::filter(SerialSQ != 1 &
                                                       Protest_True == 1 &
                                                       CE_ANA_None != 1)

## Load posterior class membership probabilities from final model
D2_Truncated_LC_3C_MXL_NoDR_V3_model_PiValues <- here("CEOutput/Main/LCM",
                                                  "D2_Truncated_LC_3C_MXL_NoDR_V3_model_PiValues.rds") %>%
  readRDS()

## Assign best-guess class as the class with highest posterior probability
ClassMembership <- apply(do.call(cbind, D2_Truncated_LC_3C_MXL_NoDR_V3_model_PiValues), 1, which.max)
Data_Covariates$ClassMembership <- ClassMembership


## ****************************************************************************
#### Stage Three: Export ####
## ****************************************************************************


# Data_Covariates %>%
#   data.frame() %>%
#   fwrite(
#     sep = ",",
#     here(
#       "Data/Main",
#       "Data_Covariates_Spatial_Step6_old.csv"
#     )
#   )
# 




Data_Covariates %>%
  data.frame() %>%
  fwrite(
    sep = ",",
    here(
      "Data/Main",
      "Data_Covariates_Step6.csv"
    )
  )

#### End of script
## ****************************************************************************
