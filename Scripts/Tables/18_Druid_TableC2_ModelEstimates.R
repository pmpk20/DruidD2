#### D2: Table C2 ###############
# Function: Build Table C2 — model estimates structured by attribute, level, class, and insect
# Author: Dr Peter King (p.king1@leeds.ac.uk)
# Last Edited: 20/06/2026
# Changes:
## - New script (table formatting extracted from 07_Druid_Model_TruncatedLC3C.R)
## - Reads _estimates.csv (written automatically by apollo_saveOutput in script 07)
## - Restructures flat parameter list into SI Table C2 layout
## - Columns: Attribute × Level × Class × Value (Mean/SD) → Bee / Beetle / Wasp
## - Beetle column = base attribute parameters; Bee/Wasp = insect-specific interaction terms

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
# data.table   * 1.17.2     2025-05-12 [1] CRAN (R 4.5.0)
# here         * 1.0.1      2020-12-13 [1] CRAN (R 4.5.0)
#
# [1] C:/Users/earpkin/AppData/Local/Programs/R/R-4.5.0/library
# * ── Packages attached to the search path.

library(data.table)
library(here)


# **********************************************************************************
#### Section 1: Import data ####
# **********************************************************************************

## apollo_saveOutput() writes _estimates.csv with an unnamed first column (parameter
## names). fread() auto-names this column "V1".
est <- fread(here("CEOutput/Main/LCM",
                  "D2_Truncated_LC_3C_MXL_NoDR_V3_estimates.csv"))

## Robustly locate SE and p-value columns (column names include trailing dots
## depending on how the CSV was written).
se_col <- grep("Rob.*std.*err", names(est), value = TRUE)[1]
pv_col <- grep("Rob.*p.*val",   names(est), value = TRUE)[1]

## Model object for diagnostics reported in the Table C2 caption.
Model <- readRDS(here("CEOutput/Main/LCM",
                      "D2_Truncated_LC_3C_MXL_NoDR_V3_model.rds"))


# **********************************************************************************
#### Section 2: Format and parse ####
# **********************************************************************************

## Significance stars using robust p-value.
add_stars <- function(p) {
  fifelse(p < 0.01, "***", fifelse(p < 0.05, "**", fifelse(p < 0.10, "*", "")))
}

## Format each estimate as "value*** (SE)" — matches SI Table C2 style.
est[, cell := paste0(
  sprintf("%.2f", Estimate),
  add_stars(get(pv_col)),
  " (",
  sprintf("%.2f", get(se_col)),
  ")"
)]

## Table C2 covers only the attribute-level mu/sig parameters.
## Excluded: tax (beta_Tax), ASC (asc_C), class allocation (delta_/ClassAllocation_),
## and wellbeing LV interactions (Int_LV_*) which appear in Table C3.
##
## Beetle base:  mu_[Attr]_[Level]_Class[c]
## Bee/Wasp int: mu_Int_[Attr]_[Level]_[Insect]_Class[c]  (same for sig_)

beetle_pat <- "^(mu|sig)_(Encounter|Existence|Bequest)_(Medium|High)_Class[123]$"
int_pat    <- "^(mu|sig)_Int_(Encounter|Existence|Bequest)_(Medium|High)_(Bee|Wasp)_Class[123]$"

est_c2 <- est[grepl(beetle_pat, V1) | grepl(int_pat, V1)]

## Parse Beetle base parameters.
est_c2[grepl(beetle_pat, V1), `:=`(
  Value     = fifelse(grepl("^mu_", V1), "Mean", "SD"),
  Attribute = sub("^(?:mu|sig)_(Encounter|Existence|Bequest)_.*", "\\1", V1),
  Level     = fifelse(grepl("_High_Class", V1), "Large", "Small"),
  Class     = as.integer(sub(".*_Class([123])$", "\\1", V1)),
  Insect    = "Beetle"
)]

## Parse Bee/Wasp interaction parameters.
est_c2[grepl(int_pat, V1), `:=`(
  Value     = fifelse(grepl("^mu_", V1), "Mean", "SD"),
  Attribute = sub("^(?:mu|sig)_Int_(Encounter|Existence|Bequest)_.*", "\\1", V1),
  Level     = fifelse(grepl("_High_", V1), "Large", "Small"),
  Class     = as.integer(sub(".*_Class([123])$", "\\1", V1)),
  Insect    = sub("^.*_(Bee|Wasp)_Class[123]$", "\\1", V1)
)]

class_label_map <- c("1" = "1: Pro-insect",
                     "2" = "2: Insect-averse",
                     "3" = "3: Ambivalent")
est_c2[, Class_label := class_label_map[as.character(Class)]]
est_c2[, Class_order := Class]


# **********************************************************************************
#### Section 3: Pivot and order ####
# **********************************************************************************

## Pivot Insect to columns → Bee / Beetle / Wasp.
TableC2 <- dcast(est_c2,
                 Attribute + Level + Class_order + Class_label + Value ~ Insect,
                 value.var = "cell")
setDT(TableC2)

## Display order: Encounter → Existence → Bequest; Large → Small; Class 1–3; Mean before SD.
TableC2[, attr_order  := match(Attribute, c("Encounter", "Existence", "Bequest"))]
TableC2[, level_order := match(Level,     c("Large", "Small"))]
TableC2[, value_order := match(Value,     c("Mean", "SD"))]
setorder(TableC2, attr_order, level_order, Class_order, value_order)
TableC2[, c("attr_order", "level_order", "value_order", "Class_order") := NULL]
setnames(TableC2, "Class_label", "Class")
setcolorder(TableC2, c("Attribute", "Level", "Class", "Value", "Bee", "Beetle", "Wasp"))


# **********************************************************************************
#### Section 4: Append diagnostics and export ####
# **********************************************************************************

## Diagnostic measures reported in the Table C2 caption.
diag_rows <- data.table(
  Attribute = "Diagnostics",
  Level     = "",
  Class     = "",
  Value     = c("N", "AIC", "BIC", "Adj.R2", "LogLik"),
  Bee       = c(
    as.character(Model$nIndivs),
    sprintf("%.3f", Model$AIC),
    sprintf("%.3f", Model$BIC),
    sprintf("%.3f", Model$adjRho2_C),
    sprintf("%.3f", as.numeric(Model$LLout["model"]))
  ),
  Beetle = "",
  Wasp   = ""
)

TableC2_full <- rbind(TableC2, diag_rows, fill = TRUE)

fwrite(TableC2_full,
       here("OtherOutput/Tables", "TableC2_ModelEstimates.csv"),
       sep   = ",",
       quote = TRUE)


# End Of Script **************************************************************
