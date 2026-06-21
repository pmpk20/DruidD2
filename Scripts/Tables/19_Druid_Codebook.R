#### D2: Data Codebook ###############
# Function: Generate Excel codebook for Data_Covariates_Step6.csv
# Author: Dr Peter King (p.king1@leeds.ac.uk)
# Last Edited: 20/06/2026
# Output: OtherOutput/Tables/D2_Codebook.xlsx
# Columns: variable_name | group | definition | coding | source | pii | needs_review

library(data.table)
library(writexl)
library(here)


# **********************************************************************************
#### Section 1: Read data and derive coding from values ####
# **********************************************************************************

d <- fread(here("Data/Main", "Data_Covariates_Step6.csv"))

col_info <- rbindlist(lapply(names(d), function(col) {
  x       <- d[[col]]
  n_uniq  <- length(unique(x[!is.na(x)]))
  n_val   <- sum(!is.na(x))
  typ     <- class(x)[1]
  if (n_uniq <= 15 && typ != "character") {
    vals <- paste(sort(unique(x[!is.na(x)])), collapse = " | ")
  } else if (n_uniq <= 15 && typ == "character") {
    vals <- paste(sort(unique(x[!is.na(x) & nchar(as.character(x)) > 0])), collapse = " | ")
  } else if (is.numeric(x)) {
    vals <- sprintf("range: %.2f to %.2f", min(x, na.rm = TRUE), max(x, na.rm = TRUE))
  } else {
    ex   <- head(unique(x[!is.na(x) & nchar(as.character(x)) > 0]), 3)
    vals <- paste(ex, collapse = " | ")
  }
  data.table(variable_name = col, r_type = typ, n_unique = n_uniq, n_valid = n_val,
             derived_coding = vals)
}))


# **********************************************************************************
#### Section 1b: Programmatic measurement_level, units, filter_condition ####
# **********************************************************************************

N_SAMPLE <- nrow(d)

# --- measurement_level --------------------------------------------------------
col_info[, measurement_level := fcase(
  r_type == "character",        "nominal (free text)",
  n_unique == 2L,               "binary (0/1)",
  n_unique %in% 3:7 & r_type %in% c("integer","numeric"), "ordinal",
  n_unique > 7L  & r_type %in% c("integer","numeric"),    "scale",
  default =                     "nominal"
)]

# Refine specific groups
col_info[grepl("^Biowell_.+_Item|^Sliding_|^Visit_Child|^Visit_Teen|^Visit_Adult|CE_Debrief|Biowell_Debrief",
               variable_name),
         measurement_level := "scale (0-100 slider)"]
col_info[grepl("^Biowell_.+_Mean$|^Overall$", variable_name),
         measurement_level := "scale (0-100, higher = better wellbeing)"]
col_info[grepl("^Q11|^CE_Task_|^CE_Tasks|^Q12InsectIDQuizFollowUp", variable_name),
         measurement_level := "ordinal (1-5 Likert)"]
col_info[grepl("^Insect_Plan|^Choice_", variable_name),
         measurement_level := "nominal (categorical)"]
col_info[grepl("^Encounter_Plan|^Existence_Plan|^Bequest_Plan", variable_name),
         measurement_level := "ordinal (3-level attribute code)"]
col_info[grepl("^Tax_Plan", variable_name),
         measurement_level := "ordinal (6-level bid code)"]
col_info[variable_name %in% c("Q1Age","IncomeMidpoints","IncomeMidpoints_PlusMissing",
                                "AgeGroup_Numeric","DiscountRate","DiscountRate_Scaled",
                                "DURATION","SurveyCompletionTime","Valuation_CompletionTime",
                                "Valuation_Start","Valuation_Finish",
                                "latitude","longitude"),
         measurement_level := "scale (continuous)"]
col_info[variable_name == "AgeGroup",    measurement_level := "ordinal (4 age bands)"]
col_info[variable_name == "Q5Ethnicity", measurement_level := "nominal (18 categories)"]
col_info[variable_name == "Q6Occupation",measurement_level := "nominal (10 categories)"]
col_info[variable_name == "Q7Income",    measurement_level := "ordinal (10 income bands)"]
col_info[variable_name == "Q48_Education", measurement_level := "ordinal (7 categories)"]
col_info[variable_name %in% c("ClassMembership","Q3Country","Q2Gender","Q4Urban"),
         measurement_level := "nominal (categorical)"]
col_info[variable_name == "Q1Age",       measurement_level := "scale (continuous, years)"]
col_info[variable_name == "Q12InsectIDQuiz_Score", measurement_level := "scale (count 0-6)"]
col_info[variable_name == "Q12InsectIDQuiz_DV_6_ScoreScaled", measurement_level := "scale (0-100, % correct)"]
col_info[variable_name == "DiscountRate_TextCategory", measurement_level := "nominal (4 categories)"]
col_info[r_type == "character" & grepl("^Q9_|^Q10_|^FurtherComments|^Q29CEProtest_Other",
                                        variable_name), measurement_level := "nominal (free text)"]

# --- units --------------------------------------------------------------------
col_info[, units := fcase(
  variable_name %in% c("DURATION","SurveyCompletionTime","Valuation_Start",
                        "Valuation_Finish","Valuation_CompletionTime",
                        "Speeders_Survey_Threshold","Slowers_Survey_Threshold",
                        "Speeders_Valuation_Threshold","Slowers_Valuation_Threshold"), "seconds",
  variable_name == "Q1Age",                     "years",
  variable_name == "AgeGroup_Numeric",           "years (group midpoint)",
  variable_name %in% c("IncomeMidpoints","IncomeMidpoints_PlusMissing"), "£/month (midpoint)",
  variable_name == "DiscountRate",               "% per year (annual implied discount rate)",
  variable_name == "DiscountRate_Scaled",        "0-1 (min-max scaled across sample)",
  variable_name == "Q12InsectIDQuiz_Score",      "count (0-6 items correct)",
  variable_name == "Q12InsectIDQuiz_DV_6_ScoreScaled", "0-100 (% of quiz items correct)",
  variable_name == "latitude",                   "decimal degrees N",
  variable_name == "longitude",                  "decimal degrees E/W",
  grepl("^Biowell_.+_Mean$|^Overall$", variable_name), "0-100 (higher = better wellbeing)",
  grepl("^Biowell_.+_Item", variable_name),      "raw slider 0-100 (lower raw = positive pole; inverted in _Mean)",
  grepl("^Sliding_|^Visit_Child|^Visit_Teen|^Visit_Adult|CE_Debrief|^Biowell_Debrief", variable_name), "0-100 slider",
  grepl("^Tax_Plan", variable_name),             "bid level code 1-6 (1=50p | 2=£2 | 3=£6 | 4=£12 | 5=£25 | 6=£50 per month for 10 years)",
  grepl("^Encounter_Plan|^Existence_Plan|^Bequest_Plan", variable_name), "attribute level code (1=No change +0% | 2=Small increase +15% | 3=Large increase +30%)",
  grepl("^Insect_Plan", variable_name),          "insect code (1=Wasps | 2=Bees | 3=Beetles)",
  grepl("^Choice_", variable_name),             "1=Plan A | 2=Plan B | 3=Plan C",
  grepl("^CE_ANA_[1-5]$", variable_name),        "1=considered | 2=did not consider",
  grepl("^QDiscounting_", variable_name),        "1=one month (immediate) | 2=one year (delayed)",
  grepl("^Q11|^CE_Task_|^CE_Tasks|^Q12InsectIDQuizFollowUp", variable_name), "1-5 Likert",
  grepl("^Q41_Freq", variable_name),             "1=did not visit | 2=once/twice a year | 3=once/twice a season | 4=once/twice a month | 5=once a week | 6=several times a week | 7=daily",
  grepl("^Q41_Walk", variable_name),             "1=≤5 min | 2=6-15 min | 3=16-30 min | 4=31-45 min | 5=45+ min | 6=never walk",
  grepl("^Q42_|^Q43_|^Q44_|^Q45_|^Q46_|^Q47_", variable_name), "0=No | 1=Yes",
  grepl("_Dummy$|_dummy$|_DV$|_TestDummy$|^speeder_|^metadata_", variable_name), "0/1 binary",
  variable_name %in% c("CE_ANA_None","CE_ANA_All","Protest_True","SerialSQ",
                        "always_chose_oneMonth","always_chose_oneYear"), "0/1 binary",
  grepl("Q3Country_|ClassDummy|Income_Dummy|Age_Dummy|Female_dummy|Dummy_Speeders|Visit_Green|Visit_Blue", variable_name), "0/1 binary",
  grepl("^Age_[0-9]", variable_name), "0/1 binary",
  variable_name %in% c("Q50_Understanding","CE_Debrief_Certain","CE_Debrief_Confident"), "0-100 slider",
  r_type == "character",                         "free text",
  default = NA_character_
)]

# --- filter_condition ---------------------------------------------------------
col_info[, filter_condition := sprintf("All respondents in estimation sample (N=%d)", N_SAMPLE)]
col_info[variable_name == "Q29CEProtest_Statements",
         filter_condition := "Only respondents who chose Plan C in all 9 CE tasks (SerialSQ==1 before exclusion); NOTE: all such respondents are excluded from Step 6 estimation sample, so this column is empty (all NA) in this file"]
col_info[variable_name == "Q29CEProtest_Other",
         filter_condition := "Only respondents choosing Plan C in all tasks AND selecting 'Other' at Q29CEProtest_Statements; empty in Step 6 file (see Q29CEProtest_Statements note)"]
col_info[variable_name == "Consent_2",
         filter_condition := "Only respondents who initially declined consent on Page 1 of survey"]
col_info[variable_name %in% c("Postcode","post_area","latitude","longitude",
                                "admin_district","admin_county","ctyua19nm"),
         filter_condition := sprintf("All respondents with valid UK postcode (N valid = %d; NAs for unmatched/invalid postcodes)",
                                     sum(!is.na(d$latitude)))]


# **********************************************************************************
#### Section 2: Hand-coded definitions lookup ####
# **********************************************************************************

# Each row: variable_name | group | definition | source | pii | needs_review
# 'needs_review' = TRUE if the definition is inferred and may need checking

manual <- rbindlist(list(

  # ── Survey Administration ────────────────────────────────────────────────────
  data.table(variable_name = "RID",
    group = "Survey Admin",
    definition = "Respondent ID assigned by SurveyEngine platform",
    source = "SurveyEngine platform", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "IP_repeat_group",
    group = "Survey Admin",
    definition = "Indicator for respondents sharing an IP address (potential duplicate flag)",
    source = "SurveyEngine platform", pii = TRUE, needs_review = FALSE),

  data.table(variable_name = "START",
    group = "Survey Admin",
    definition = "Survey start timestamp",
    source = "SurveyEngine platform", pii = TRUE, needs_review = FALSE),

  data.table(variable_name = "DATETIME.UTC",
    group = "Survey Admin",
    definition = "Survey completion timestamp (UTC)",
    source = "SurveyEngine platform", pii = TRUE, needs_review = FALSE),

  data.table(variable_name = "speeder_flag",
    group = "Survey Admin",
    definition = "Platform-generated flag for respondents who completed the survey unusually quickly",
    source = "SurveyEngine platform", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "metadata_flag",
    group = "Survey Admin",
    definition = "Platform-generated metadata flag (e.g. browser/device anomaly)",
    source = "SurveyEngine platform", pii = FALSE, needs_review = TRUE),

  data.table(variable_name = "DURATION",
    group = "Survey Admin",
    definition = "Total survey duration in seconds (from START to DATETIME.UTC)",
    source = "SurveyEngine platform", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "consent",
    group = "Survey Admin",
    definition = "Participant information sheet consent: respondent confirmed they read and agreed (Page 1). 'Yes' advances to survey; 'No' exits.",
    source = "Survey Q (Page 1)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Consent_2",
    group = "Survey Admin",
    definition = "Second consent confirmation (Page 2 — shown only to those who initially declined; 'Yes' exits the survey, 'No' returns to Page 1).",
    source = "Survey Q (Page 2)", pii = FALSE, needs_review = FALSE),

  # ── Demographics (Q1–Q8) ─────────────────────────────────────────────────────
  data.table(variable_name = "Q1Age",
    group = "Demographics",
    definition = "Q1: Respondent age in years (numeric entry; validated as integer < 100; respondents under 18 were screened out)",
    source = "Survey Q1 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q2Gender",
    group = "Demographics",
    definition = "Q2: Gender (used as a quota variable for sampling)",
    source = "Survey Q2 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q3Country",
    group = "Demographics",
    definition = "Q3: Country of residence within the UK (England, Scotland, Wales, Northern Ireland)",
    source = "Survey Q3 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q4Urban",
    group = "Demographics",
    definition = "Q4: Type of residential area (urban/suburban/rural classification; quota variable)",
    source = "Survey Q4 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q5Ethnicity",
    group = "Demographics",
    definition = "Q5: Self-reported ethnic group. Options follow UK Census categories (e.g. 'White - British/English/Welsh/Scottish/Northern Irish', 'Asian or Asian British – Indian', etc.)",
    source = "Survey Q5 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q8Postcode",
    group = "Demographics",
    definition = "Q8: Home postcode (used for spatial merging with CTYUA boundaries in script 04; PII — strip before deposit)",
    source = "Survey Q8 - direct response", pii = TRUE, needs_review = FALSE),

  data.table(variable_name = "Q6Occupation",
    group = "Demographics",
    definition = "Q6: Current occupation. Options: Homemaker, Student, Retired, Unemployed/on benefit, Factory/manual worker, Crafts/tradesperson, Office/clerical/admin, Middle management, Senior management, Professional",
    source = "Survey Q6 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q7Income",
    group = "Demographics",
    definition = "Q7: Monthly household net income (banded). Options: £0-£1,000 | £1,001-£1,500 | £1,501-£2,000 | £2,001-£2,500 | £2,501-£3,000 | £3,001-£3,500 | £3,501-£4,000 | £4,001-£6,000 | £6,001 or more | Prefer not to say",
    source = "Survey Q7 - direct response", pii = FALSE, needs_review = FALSE),

  # ── Background Knowledge & Attitudes (Q9–Q12) ──────────────────────────────
  data.table(variable_name = "Q10Insect_Species1",
    group = "Background - Insect Knowledge",
    definition = "Q10: First insect species named by respondent (free text, max 20 chars; note: Q9 and Q10 swapped order between pilot and main survey — in-field label is Q10 but this appears before Q9 in the survey flow)",
    source = "Survey Q10 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q10Insect_Species2",
    group = "Background - Insect Knowledge",
    definition = "Q10: Second insect species named by respondent (free text)",
    source = "Survey Q10 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q10Insect_Species3",
    group = "Background - Insect Knowledge",
    definition = "Q10: Third insect species named by respondent (free text)",
    source = "Survey Q10 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q9Insect_Word1",
    group = "Background - Insect Knowledge",
    definition = "Q9: First word associated with insects by respondent (free text, max 20 chars; note: Q9 and Q10 swapped order between pilot and main survey — in-field label is Q9 but this appears after Q10 in the survey flow)",
    source = "Survey Q9 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q9Insect_Word2",
    group = "Background - Insect Knowledge",
    definition = "Q9: Second word associated with insects by respondent (free text)",
    source = "Survey Q9 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q9Insect_Word3",
    group = "Background - Insect Knowledge",
    definition = "Q9: Third word associated with insects by respondent (free text)",
    source = "Survey Q9 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "BackgroundInfo_Check",
    group = "Background - Insect Knowledge",
    definition = "Attention check (Page 9): respondent asked to 'Please confirm' having read background information. No branching logic; used as quality indicator.",
    source = "Survey - attention check (Page 9)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q11OpinionOnInsectPopulations_1",
    group = "Background - Insect Knowledge",
    definition = "Q11: Opinion on insect population trends — item 1 (agree/disagree Likert scale)",
    source = "Survey Q11 - direct response", pii = FALSE, needs_review = TRUE),

  data.table(variable_name = "Q11OpinionOnInsectPopulations_2",
    group = "Background - Insect Knowledge",
    definition = "Q11: Opinion on insect population trends — item 2 (agree/disagree Likert scale)",
    source = "Survey Q11 - direct response", pii = FALSE, needs_review = TRUE),

  data.table(variable_name = "Q12InsectIDQuiz_1",
    group = "Background - Insect Knowledge",
    definition = "Q12: Insect identification quiz — item 1 (respondent's answer from: Ant | Bee | Beetle | Wasp | I don't know)",
    source = "Survey Q12 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q12InsectIDQuiz_2",
    group = "Background - Insect Knowledge",
    definition = "Q12: Insect identification quiz — item 2 (respondent's answer)",
    source = "Survey Q12 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q12InsectIDQuiz_3",
    group = "Background - Insect Knowledge",
    definition = "Q12: Insect identification quiz — item 3 (respondent's answer)",
    source = "Survey Q12 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q12InsectIDQuiz_4",
    group = "Background - Insect Knowledge",
    definition = "Q12: Insect identification quiz — item 4 (respondent's answer)",
    source = "Survey Q12 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q12InsectIDQuiz_5",
    group = "Background - Insect Knowledge",
    definition = "Q12: Insect identification quiz — item 5 (respondent's answer)",
    source = "Survey Q12 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q12InsectIDQuiz_6",
    group = "Background - Insect Knowledge",
    definition = "Q12: Insect identification quiz — item 6 (respondent's answer)",
    source = "Survey Q12 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q12InsectIDQuiz_DV_1",
    group = "Background - Insect Knowledge",
    definition = "Q12: Insect ID quiz item 1 — correct/incorrect indicator (platform-generated: 'Wasp: correct', other answers: incorrect)",
    source = "Survey Q12 - platform-generated", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q12InsectIDQuiz_DV_2",
    group = "Background - Insect Knowledge",
    definition = "Q12: Insect ID quiz item 2 — correct/incorrect indicator",
    source = "Survey Q12 - platform-generated", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q12InsectIDQuiz_DV_3",
    group = "Background - Insect Knowledge",
    definition = "Q12: Insect ID quiz item 3 — correct/incorrect indicator",
    source = "Survey Q12 - platform-generated", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q12InsectIDQuiz_DV_4",
    group = "Background - Insect Knowledge",
    definition = "Q12: Insect ID quiz item 4 — correct/incorrect indicator",
    source = "Survey Q12 - platform-generated", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q12InsectIDQuiz_DV_5",
    group = "Background - Insect Knowledge",
    definition = "Q12: Insect ID quiz item 5 — correct/incorrect indicator",
    source = "Survey Q12 - platform-generated", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q12InsectIDQuiz_DV_6",
    group = "Background - Insect Knowledge",
    definition = "Q12: Insect ID quiz item 6 — correct/incorrect indicator",
    source = "Survey Q12 - platform-generated", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q12InsectIDQuizFollowUp_1",
    group = "Background - Insect Knowledge",
    definition = "Q12 follow-up: Post-quiz comprehension question 1 (Page 11; correct answers piped from Q12 and displayed to respondent)",
    source = "Survey Q12 follow-up - direct response", pii = FALSE, needs_review = TRUE),

  data.table(variable_name = "Q12InsectIDQuizFollowUp_2",
    group = "Background - Insect Knowledge",
    definition = "Q12 follow-up: Post-quiz comprehension question 2 (Page 11)",
    source = "Survey Q12 follow-up - direct response", pii = FALSE, needs_review = TRUE),

  # ── CE Setup Checks ──────────────────────────────────────────────────────────
  data.table(variable_name = "CE_Info_Check",
    group = "CE - Setup Checks",
    definition = "CE comprehension check (Page 13): attention check after CE information pages. No branching logic.",
    source = "Survey - attention check (Page 13)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "CE_Task_Plan_Check_1",
    group = "CE - Setup Checks",
    definition = "CE comprehension check: 'I understand the three conservation plans (A, B, C)' — agree/disagree scale (Page 14). Loops back to Page 13 if 'No'.",
    source = "Survey - CE comprehension check (Page 14)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "CE_Task_Insect_Check_1",
    group = "CE - Setup Checks",
    definition = "CE comprehension check: 'I understand the insect attribute' — agree/disagree scale (Page 15)",
    source = "Survey - CE comprehension check (Page 15)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "CE_Task_A1_Check_1",
    group = "CE - Setup Checks",
    definition = "CE comprehension check: 'I understand the encounter/recreation use attribute' — agree/disagree scale (Page 16)",
    source = "Survey - CE comprehension check (Page 16)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "CE_Task_A2_Check_1",
    group = "CE - Setup Checks",
    definition = "CE comprehension check: 'I understand the existence value attribute' — agree/disagree scale (Page 17)",
    source = "Survey - CE comprehension check (Page 17)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "CE_Task_A3_Check_1",
    group = "CE - Setup Checks",
    definition = "CE comprehension check: 'I understand the bequest value attribute' — agree/disagree scale (Page 18)",
    source = "Survey - CE comprehension check (Page 18)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "CE_Task_A4_Check_1",
    group = "CE - Setup Checks",
    definition = "CE comprehension check: 'I understand the tax/payment attribute' — agree/disagree scale (Page 19)",
    source = "Survey - CE comprehension check (Page 19)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "CE_Task_A5_Check_1",
    group = "CE - Setup Checks",
    definition = "CE comprehension check: 'I understand the choice tasks' — agree/disagree scale (Page 20). 'No' response loops back to Page 13.",
    source = "Survey - CE comprehension check (Page 20)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "CE_ConfirmYouHaveReadDescs",
    group = "CE - Setup Checks",
    definition = "CE confirmation: respondent confirmed they have read all attribute descriptions before proceeding to choice tasks (Page 20)",
    source = "Survey - CE comprehension check (Page 20)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "CE_TasksConsequentiality_1",
    group = "CE - Setup Checks",
    definition = "Perceived policy consequentiality: item 1 — degree to which respondent believes their choices will influence policy (Page 21)",
    source = "Survey - consequentiality check (Page 21)", pii = FALSE, needs_review = TRUE),

  data.table(variable_name = "CE_TasksConsequentiality_2",
    group = "CE - Setup Checks",
    definition = "Perceived policy efficacy: item 2 — degree to which respondent believes the conservation policy would be effective (Page 21)",
    source = "Survey - consequentiality check (Page 21)", pii = FALSE, needs_review = TRUE),

  data.table(variable_name = "CE_BlockAllocator",
    group = "CE - Experimental Design",
    definition = "Experimental design: block number allocated to this respondent. Determines which set of 9 choice tasks from the D-efficient design the respondent received.",
    source = "SurveyEngine - experimental design allocation", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "BidVectorElicitation_Randomiser",
    group = "CE - Experimental Design",
    definition = "Experimental design: randomiser for bid (tax) vector assignment. Determines which bid vector was shown to this respondent across the 9 choice tasks.",
    source = "SurveyEngine - experimental design allocation", pii = FALSE, needs_review = FALSE),

  # ── CE Responses ─────────────────────────────────────────────────────────────
  data.table(variable_name = "CE_1_DV",
    group = "CE - Responses",
    definition = "Choice task 1: respondent's chosen plan (A/B/C dummy; C = status quo / opt-out)",
    source = "Survey - CE task 1 response (Pages 22-31)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "CE_2_DV",
    group = "CE - Responses",
    definition = "Choice task 2: respondent's chosen plan",
    source = "Survey - CE task 2 response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "CE_3_DV",
    group = "CE - Responses",
    definition = "Choice task 3: respondent's chosen plan",
    source = "Survey - CE task 3 response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "CE_4_DV",
    group = "CE - Responses",
    definition = "Choice task 4: respondent's chosen plan",
    source = "Survey - CE task 4 response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "CE_5_DV",
    group = "CE - Responses",
    definition = "Choice task 5: respondent's chosen plan",
    source = "Survey - CE task 5 response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "CE_6_DV",
    group = "CE - Responses",
    definition = "Choice task 6: respondent's chosen plan",
    source = "Survey - CE task 6 response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "CE_7_DV",
    group = "CE - Responses",
    definition = "Choice task 7: respondent's chosen plan",
    source = "Survey - CE task 7 response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "CE_8_DV",
    group = "CE - Responses",
    definition = "Choice task 8: respondent's chosen plan",
    source = "Survey - CE task 8 response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "CE_9_DV",
    group = "CE - Responses",
    definition = "Choice task 9: respondent's chosen plan",
    source = "Survey - CE task 9 response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q29CEProtest_Statements",
    group = "CE - Responses",
    definition = "Q29: Protest reason — shown only if respondent chose Plan C (status quo) in all 9 tasks. Pre-specified reasons (e.g. 'I don't believe this policy would work'). All options advance to Page 33 except 'Other'.",
    source = "Survey Q29 - direct response (Page 31)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q29CEProtest_Other",
    group = "CE - Responses",
    definition = "Q29: Protest reason — free text for respondents who selected 'Other' on Q29CEProtest_Statements (Page 32)",
    source = "Survey Q29 - direct response (Page 32)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "CE_Debrief_Certain",
    group = "CE - Responses",
    definition = "CE debrief: certainty about choices made (Page 33, sliding scale debrief question)",
    source = "Survey - CE debrief (Page 33)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "CE_Debrief_Confident",
    group = "CE - Responses",
    definition = "CE debrief: confidence in choices made (Page 33, sliding scale debrief question)",
    source = "Survey - CE debrief (Page 33)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "CE_ANA_1",
    group = "CE - Responses",
    definition = "Attribute non-attendance (Page 34): 'Did you ignore the insect type attribute when making choices?' (1=ignored, 0=attended)",
    source = "Survey - ANA question (Page 34)", pii = FALSE, needs_review = TRUE),

  data.table(variable_name = "CE_ANA_2",
    group = "CE - Responses",
    definition = "Attribute non-attendance (Page 34): 'Did you ignore the encounter/recreation attribute?' (1=ignored, 0=attended)",
    source = "Survey - ANA question (Page 34)", pii = FALSE, needs_review = TRUE),

  data.table(variable_name = "CE_ANA_3",
    group = "CE - Responses",
    definition = "Attribute non-attendance (Page 34): 'Did you ignore the existence value attribute?' (1=ignored, 0=attended)",
    source = "Survey - ANA question (Page 34)", pii = FALSE, needs_review = TRUE),

  data.table(variable_name = "CE_ANA_4",
    group = "CE - Responses",
    definition = "Attribute non-attendance (Page 34): 'Did you ignore the bequest value attribute?' (1=ignored, 0=attended)",
    source = "Survey - ANA question (Page 34)", pii = FALSE, needs_review = TRUE),

  data.table(variable_name = "CE_ANA_5",
    group = "CE - Responses",
    definition = "Attribute non-attendance (Page 34): 'Did you ignore the tax/payment attribute?' (1=ignored, 0=attended)",
    source = "Survey - ANA question (Page 34)", pii = FALSE, needs_review = TRUE),

  # ── Biowell Wellbeing Sliding Scales ─────────────────────────────────────────
  # Tasks: Beetle1, Beetle2, Beetle3, Bee1, Bee2, Bee3, Wasp1, Wasp2, Wasp3
  # Items: 1_Relaxed, 2_Joy, 3_Clear, 4_Open, 5_Connect
  # Pages 35-46

  data.table(variable_name = "Biowell_Beetle1_Item1_Relaxed",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 1 (Beetles, Encounter scenario): Item 1 — 'I feel relaxed' (slider response 0-100)",
    source = "Survey - Biowell slider task (Pages 35-46)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Beetle1_Item2_Joy",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 1 (Beetles, Encounter): Item 2 — 'I feel joy' (slider 0-100)",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Beetle1_Item3_Clear",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 1 (Beetles, Encounter): Item 3 — 'I feel clear-headed' (slider 0-100)",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Beetle1_Item4_Open",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 1 (Beetles, Encounter): Item 4 — 'I feel open to others' (slider 0-100)",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Beetle1_Item5_Connect",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 1 (Beetles, Encounter): Item 5 — 'I feel connected to nature' (slider 0-100)",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Bee1_Item1_Relaxed",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 2 (Bees, Encounter scenario): Item 1 — 'I feel relaxed' (slider 0-100)",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Bee1_Item2_Joy",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 2 (Bees, Encounter): Item 2 — 'I feel joy' (slider 0-100)",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Bee1_Item3_Clear",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 2 (Bees, Encounter): Item 3 — 'I feel clear-headed' (slider 0-100)",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Bee1_Item4_Open",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 2 (Bees, Encounter): Item 4 — 'I feel open to others' (slider 0-100)",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Bee1_Item5_Connect",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 2 (Bees, Encounter): Item 5 — 'I feel connected to nature' (slider 0-100)",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Wasp1_Item1_Relaxed",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 3 (Wasps, Encounter scenario): Item 1 — 'I feel relaxed' (slider 0-100)",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Wasp1_Item2_Joy",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 3 (Wasps, Encounter): Item 2 — 'I feel joy' (slider 0-100)",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Wasp1_Item3_Clear",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 3 (Wasps, Encounter): Item 3 — 'I feel clear-headed' (slider 0-100)",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Wasp1_Item4_Open",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 3 (Wasps, Encounter): Item 4 — 'I feel open to others' (slider 0-100)",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Wasp1_Item5_Connect",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 3 (Wasps, Encounter): Item 5 — 'I feel connected to nature' (slider 0-100)",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Beetle2_Item1_Relaxed",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 4 (Beetles, Existence scenario): Item 1 — 'I feel relaxed' (slider 0-100)",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Beetle2_Item2_Joy",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 4 (Beetles, Existence): Item 2 — 'I feel joy'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Beetle2_Item3_Clear",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 4 (Beetles, Existence): Item 3 — 'I feel clear-headed'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Beetle2_Item4_Open",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 4 (Beetles, Existence): Item 4 — 'I feel open to others'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Beetle2_Item5_Connect",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 4 (Beetles, Existence): Item 5 — 'I feel connected to nature'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Bee2_Item1_Relaxed",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 5 (Bees, Existence scenario): Item 1 — 'I feel relaxed'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Bee2_Item2_Joy",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 5 (Bees, Existence): Item 2 — 'I feel joy'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Bee2_Item3_Clear",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 5 (Bees, Existence): Item 3 — 'I feel clear-headed'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Bee2_Item4_Open",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 5 (Bees, Existence): Item 4 — 'I feel open to others'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Bee2_Item5_Connect",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 5 (Bees, Existence): Item 5 — 'I feel connected to nature'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Wasp2_Item1_Relaxed",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 6 (Wasps, Existence scenario): Item 1 — 'I feel relaxed'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Wasp2_Item2_Joy",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 6 (Wasps, Existence): Item 2 — 'I feel joy'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Wasp2_Item3_Clear",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 6 (Wasps, Existence): Item 3 — 'I feel clear-headed'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Wasp2_Item4_Open",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 6 (Wasps, Existence): Item 4 — 'I feel open to others'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Wasp2_Item5_Connect",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 6 (Wasps, Existence): Item 5 — 'I feel connected to nature'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Beetle3_Item1_Relaxed",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 7 (Beetles, Bequest scenario): Item 1 — 'I feel relaxed'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Beetle3_Item2_Joy",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 7 (Beetles, Bequest): Item 2 — 'I feel joy'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Beetle3_Item3_Clear",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 7 (Beetles, Bequest): Item 3 — 'I feel clear-headed'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Beetle3_Item4_Open",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 7 (Beetles, Bequest): Item 4 — 'I feel open to others'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Beetle3_Item5_Connect",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 7 (Beetles, Bequest): Item 5 — 'I feel connected to nature'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Bee3_Item1_Relaxed",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 8 (Bees, Bequest scenario): Item 1 — 'I feel relaxed'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Bee3_Item2_Joy",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 8 (Bees, Bequest): Item 2 — 'I feel joy'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Bee3_Item3_Clear",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 8 (Bees, Bequest): Item 3 — 'I feel clear-headed'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Bee3_Item4_Open",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 8 (Bees, Bequest): Item 4 — 'I feel open to others'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Bee3_Item5_Connect",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 8 (Bees, Bequest): Item 5 — 'I feel connected to nature'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Wasp3_Item1_Relaxed",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 9 (Wasps, Bequest scenario): Item 1 — 'I feel relaxed'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Wasp3_Item2_Joy",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 9 (Wasps, Bequest): Item 2 — 'I feel joy'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Wasp3_Item3_Clear",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 9 (Wasps, Bequest): Item 3 — 'I feel clear-headed'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Wasp3_Item4_Open",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 9 (Wasps, Bequest): Item 4 — 'I feel open to others'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Wasp3_Item5_Connect",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell wellbeing task 9 (Wasps, Bequest): Item 5 — 'I feel connected to nature'",
    source = "Survey - Biowell slider task", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Debrief_BeetlesCertain",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell debrief (Page 46): certainty about wellbeing responses for beetles",
    source = "Survey - Biowell debrief (Page 46)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Debrief_BeeCertain",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell debrief (Page 46): certainty about wellbeing responses for bees",
    source = "Survey - Biowell debrief (Page 46)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Debrief_WaspCertain",
    group = "Biowell - Wellbeing Scales",
    definition = "Biowell debrief (Page 46): certainty about wellbeing responses for wasps",
    source = "Survey - Biowell debrief (Page 46)", pii = FALSE, needs_review = FALSE),

  # ── Sliding Scale Debriefs ────────────────────────────────────────────────────
  data.table(variable_name = "Sliding_Beetles_Encounter",
    group = "Sliding Scales",
    definition = "Sliding scale debrief (Page 33): how much the prospect of encountering beetles in the wild influenced CE choices",
    source = "Survey - sliding scale debrief (Page 33)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Sliding_Beetles_Ecology",
    group = "Sliding Scales",
    definition = "Sliding scale debrief (Page 33): how much the ecological/existence value of beetles influenced CE choices",
    source = "Survey - sliding scale debrief (Page 33)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Sliding_Beetles_Conserved",
    group = "Sliding Scales",
    definition = "Sliding scale debrief (Page 33): how much the importance of beetles being conserved for future generations (bequest value) influenced CE choices",
    source = "Survey - sliding scale debrief (Page 33)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Sliding_Beetles_Important",
    group = "Sliding Scales",
    definition = "Sliding scale debrief (Page 33): how important beetles overall were to CE choices",
    source = "Survey - sliding scale debrief (Page 33)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Sliding_Bees_Encounter",
    group = "Sliding Scales",
    definition = "Sliding scale debrief (Page 33): how much encountering bees influenced CE choices",
    source = "Survey - sliding scale debrief (Page 33)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Sliding_Bees_Conserved",
    group = "Sliding Scales",
    definition = "Sliding scale debrief (Page 33): how much bees being conserved for future generations influenced CE choices",
    source = "Survey - sliding scale debrief (Page 33)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Sliding_Bees_Ecology",
    group = "Sliding Scales",
    definition = "Sliding scale debrief (Page 33): how much the ecological/existence value of bees influenced CE choices",
    source = "Survey - sliding scale debrief (Page 33)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Sliding_Bees_Important",
    group = "Sliding Scales",
    definition = "Sliding scale debrief (Page 33): how important bees overall were to CE choices",
    source = "Survey - sliding scale debrief (Page 33)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Sliding_Wasps_Encounter",
    group = "Sliding Scales",
    definition = "Sliding scale debrief (Page 33): how much encountering wasps influenced CE choices",
    source = "Survey - sliding scale debrief (Page 33)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Sliding_Wasps_Conserved",
    group = "Sliding Scales",
    definition = "Sliding scale debrief (Page 33): how much wasps being conserved for future generations influenced CE choices",
    source = "Survey - sliding scale debrief (Page 33)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Sliding_Wasps_Ecology",
    group = "Sliding Scales",
    definition = "Sliding scale debrief (Page 33): how much the ecological/existence value of wasps influenced CE choices",
    source = "Survey - sliding scale debrief (Page 33)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Sliding_Wasps_Important",
    group = "Sliding Scales",
    definition = "Sliding scale debrief (Page 33): how important wasps overall were to CE choices",
    source = "Survey - sliding scale debrief (Page 33)", pii = FALSE, needs_review = FALSE),

  # ── Nature Visits & Behaviour (Pages 47–53) ──────────────────────────────────
  data.table(variable_name = "Visit_Child",
    group = "Nature Visits & Behaviour",
    definition = "Frequency of visits to nature/countryside as a child (Pages 47-49: perceptions of beetles/bees/wasps section)",
    source = "Survey - direct response (Pages 47-49)", pii = FALSE, needs_review = TRUE),

  data.table(variable_name = "Visit_Teen",
    group = "Nature Visits & Behaviour",
    definition = "Frequency of visits to nature/countryside as a teenager",
    source = "Survey - direct response (Pages 47-49)", pii = FALSE, needs_review = TRUE),

  data.table(variable_name = "Visit_Adult",
    group = "Nature Visits & Behaviour",
    definition = "Frequency of visits to nature/countryside as an adult",
    source = "Survey - direct response (Pages 47-49)", pii = FALSE, needs_review = TRUE),

  data.table(variable_name = "Q41_FreqOfVisitGreenSpace_1",
    group = "Nature Visits & Behaviour",
    definition = "Q41: Frequency of visiting green spaces (parks, woodland, etc.) per week/month",
    source = "Survey Q41 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q41_FreqOfVisitBlueSpace_1",
    group = "Nature Visits & Behaviour",
    definition = "Q41: Frequency of visiting blue spaces (rivers, lakes, coast, etc.) per week/month",
    source = "Survey Q41 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q41_WalkTimeToGreenSpace",
    group = "Nature Visits & Behaviour",
    definition = "Q41: Estimated walking time (minutes) to nearest green space",
    source = "Survey Q41 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q41_WalkTimeToBlueSpace",
    group = "Nature Visits & Behaviour",
    definition = "Q41: Estimated walking time (minutes) to nearest blue space",
    source = "Survey Q41 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q42_Charity",
    group = "Nature Visits & Behaviour",
    definition = "Q42: Membership of or donation to an environmental/wildlife charity (yes/no or scale)",
    source = "Survey Q42 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q43_Citizen",
    group = "Nature Visits & Behaviour",
    definition = "Q43: Participation in citizen science activities (e.g. wildlife recording; yes/no or scale)",
    source = "Survey Q43 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q44_Sight",
    group = "Nature Visits & Behaviour",
    definition = "Q44: Sight impairment indicator (used as accessibility covariate)",
    source = "Survey Q44 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q45_Garden",
    group = "Nature Visits & Behaviour",
    definition = "Q45: Access to a garden or outdoor space at home (yes/no)",
    source = "Survey Q45 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q46_Farm",
    group = "Nature Visits & Behaviour",
    definition = "Q46: Farming background or connection to agriculture (yes/no)",
    source = "Survey Q46 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q47_Fish",
    group = "Nature Visits & Behaviour",
    definition = "Q47: Fishing activity (yes/no or frequency)",
    source = "Survey Q47 - direct response", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q48_Education",
    definition = paste0("Q28 (SI A13.2): 'Please select your highest level of education you have completed.' ",
      "1=No qualifications | 2=1-4 O Levels/CSEs/GCSEs or NVQ Level 1 | ",
      "3=5+ O Levels/CSEs/GCSEs, NVQ Level 2, AS Levels, Higher Diploma, or Diploma Apprenticeship | ",
      "4=2+ A Levels, NVQ Level 3, or BTEC National Diploma | ",
      "5=Degree, Higher Degree, NVQ Level 4-5, BTEC Higher Level, or professional qualifications | ",
      "6=Doctoral degree or equivalent | 7=Other qualifications"),
    group = "Nature Visits & Behaviour",
    source = "Survey Q28 (SI numbering) - direct response (Page 53)", pii = FALSE, needs_review = FALSE),

  # ── Discounting ───────────────────────────────────────────────────────────────
  data.table(variable_name = "QDiscounting_1",
    group = "Discounting",
    definition = "Discounting question 1: preference between a smaller immediate payment and a larger delayed payment (Page 56; row order randomised across respondents)",
    source = "Survey - discounting questions (Page 56)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "QDiscounting_2",
    group = "Discounting",
    definition = "Discounting question 2: intertemporal preference (smaller now vs. larger later)",
    source = "Survey - discounting questions (Page 56)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "QDiscounting_3",
    group = "Discounting",
    definition = "Discounting question 3: intertemporal preference",
    source = "Survey - discounting questions (Page 56)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "QDiscounting_4",
    group = "Discounting",
    definition = "Discounting question 4: intertemporal preference",
    source = "Survey - discounting questions (Page 56)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "QDiscounting_5",
    group = "Discounting",
    definition = "Discounting question 5: intertemporal preference",
    source = "Survey - discounting questions (Page 56)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "QDiscounting_6",
    group = "Discounting",
    definition = "Discounting question 6: intertemporal preference",
    source = "Survey - discounting questions (Page 56)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "QDiscounting_7",
    group = "Discounting",
    definition = "Discounting question 7: intertemporal preference",
    source = "Survey - discounting questions (Page 56)", pii = FALSE, needs_review = FALSE),

  # ── Final Survey Questions ────────────────────────────────────────────────────
  data.table(variable_name = "Q50_Understanding",
    group = "Final Questions",
    definition = "Q50: Self-reported understanding of the survey overall (Page 57; final questions section)",
    source = "Survey Q50 - direct response (Page 57)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "FurtherComments",
    group = "Final Questions",
    definition = "Free text field for further comments (Page 57). May contain PII — review before deposit.",
    source = "Survey - free text (Page 57)", pii = TRUE, needs_review = FALSE),

  data.table(variable_name = "SurveyCompletionTime",
    group = "Final Questions",
    definition = "Elapsed time for respondent to complete the survey (seconds or minutes; derived from START and DATETIME.UTC)",
    source = "SurveyEngine platform - derived", pii = FALSE, needs_review = FALSE),

  # ── Quality Control ───────────────────────────────────────────────────────────
  data.table(variable_name = "Speeders_Survey_Threshold",
    group = "Quality Control",
    definition = "Threshold time (seconds) below which survey completion is flagged as suspiciously fast; used to identify survey speeders",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Slowers_Survey_Threshold",
    group = "Quality Control",
    definition = "Threshold time (seconds) above which survey completion is flagged as suspiciously slow; used to identify inattentive respondents",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Speeders_Survey_TestDummy",
    group = "Quality Control",
    definition = "Binary flag: 1 = respondent completed the survey section faster than Speeders_Survey_Threshold",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Slowers_Survey_TestDummy",
    group = "Quality Control",
    definition = "Binary flag: 1 = respondent completed the survey section slower than Slowers_Survey_Threshold",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Valuation_Start",
    group = "Quality Control",
    definition = "Timestamp or elapsed time at the start of the CE valuation section",
    source = "SurveyEngine platform", pii = FALSE, needs_review = TRUE),

  data.table(variable_name = "Valuation_Finish",
    group = "Quality Control",
    definition = "Timestamp or elapsed time at the end of the CE valuation section",
    source = "SurveyEngine platform", pii = FALSE, needs_review = TRUE),

  data.table(variable_name = "Valuation_CompletionTime",
    group = "Quality Control",
    definition = "Time taken (seconds) to complete the CE valuation section (Valuation_Finish - Valuation_Start)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Speeders_Valuation_Threshold",
    group = "Quality Control",
    definition = "Threshold below which CE valuation completion is flagged as suspiciously fast",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Slowers_Valuation_Threshold",
    group = "Quality Control",
    definition = "Threshold above which CE valuation completion is flagged as suspiciously slow",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Speeders_Valuation_TestDummy",
    group = "Quality Control",
    definition = "Binary flag: 1 = respondent completed CE tasks faster than Speeders_Valuation_Threshold",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Slowers_Valuation_TestDummy",
    group = "Quality Control",
    definition = "Binary flag: 1 = respondent completed CE tasks slower than Slowers_Valuation_Threshold",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  # ── Derived Demographic Dummies ───────────────────────────────────────────────
  data.table(variable_name = "ClassDummy",
    group = "Derived - Demographics",
    definition = "Occupation class dummy: indicates whether respondent falls into a specified social grade/occupation class (e.g. ABC1 vs. C2DE)",
    source = "Derived - script 01", pii = FALSE, needs_review = TRUE),

  data.table(variable_name = "EmployDummy_Retired",
    group = "Derived - Demographics",
    definition = "Binary dummy: 1 = respondent is retired (derived from Q6Occupation)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "EmployDummy_Employed",
    group = "Derived - Demographics",
    definition = "Binary dummy: 1 = respondent is currently employed (derived from Q6Occupation)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "IncomeMidpoints",
    group = "Derived - Demographics",
    definition = "Midpoint of the Q7 income band (£/month). Prefer-not-to-say responses imputed as missing.",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Income_MissingDummy",
    group = "Derived - Demographics",
    definition = "Binary flag: 1 = respondent selected 'Prefer not to say' for income (Q7)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "AgeGroup",
    group = "Derived - Demographics",
    definition = "Age group category derived from Q1Age (e.g. 18-29, 30-44, 45-60, 60+)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Dummy_HighEducation",
    group = "Derived - Demographics",
    definition = "Binary dummy: 1 = respondent holds a degree or higher qualification (derived from Q48_Education)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Age_1829Dummy",
    group = "Derived - Demographics",
    definition = "Binary dummy: 1 = respondent is aged 18-29 (reference = 60+)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Age_3044Dummy",
    group = "Derived - Demographics",
    definition = "Binary dummy: 1 = respondent is aged 30-44",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Age_4560Dummy",
    group = "Derived - Demographics",
    definition = "Binary dummy: 1 = respondent is aged 45-60",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Age_60Dummy",
    group = "Derived - Demographics",
    definition = "Binary dummy: 1 = respondent is aged 60 or over",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "AgeGroup_Numeric",
    group = "Derived - Demographics",
    definition = "Numeric code for AgeGroup (integer; used in regression models)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "IncomeMidpoints_PlusMissing",
    group = "Derived - Demographics",
    definition = "IncomeMidpoints with 'Prefer not to say' responses coded as a specific imputed value (rather than NA; used in models)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Income_Dummy",
    group = "Derived - Demographics",
    definition = "Binary income dummy (e.g. 1 = above median income band; exact threshold — check script 01)",
    source = "Derived - script 01", pii = FALSE, needs_review = TRUE),

  data.table(variable_name = "Q5Ethnicity_Dummy",
    group = "Derived - Demographics",
    definition = "Binary dummy: 1 = respondent identifies as White British/White Other (0 = ethnic minority); derived from Q5Ethnicity",
    source = "Derived - script 01", pii = FALSE, needs_review = TRUE),

  # ── Derived Quiz & ANA ────────────────────────────────────────────────────────
  data.table(variable_name = "Q12InsectIDQuiz_DV_1_Dummy",
    group = "Derived - Knowledge",
    definition = "Binary dummy: 1 = correct answer on insect ID quiz item 1 (derived from Q12InsectIDQuiz_DV_1)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q12InsectIDQuiz_DV_2_Dummy",
    group = "Derived - Knowledge",
    definition = "Binary dummy: 1 = correct answer on insect ID quiz item 2",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q12InsectIDQuiz_DV_3_Dummy",
    group = "Derived - Knowledge",
    definition = "Binary dummy: 1 = correct answer on insect ID quiz item 3",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q12InsectIDQuiz_DV_4_Dummy",
    group = "Derived - Knowledge",
    definition = "Binary dummy: 1 = correct answer on insect ID quiz item 4",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q12InsectIDQuiz_DV_5_Dummy",
    group = "Derived - Knowledge",
    definition = "Binary dummy: 1 = correct answer on insect ID quiz item 5",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q12InsectIDQuiz_DV_6_Dummy",
    group = "Derived - Knowledge",
    definition = "Binary dummy: 1 = correct answer on insect ID quiz item 6",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q12InsectIDQuiz_Score",
    group = "Derived - Knowledge",
    definition = "Total number of correct answers on the 6-item insect ID quiz (sum of Q12InsectIDQuiz_DV_*_Dummy; range 0-6)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q12InsectIDQuiz_DV_6_ScoreScaled",
    group = "Derived - Knowledge",
    definition = "Scaled version of Q12InsectIDQuiz_Score (standardised or normalised to 0-1 or z-score; used as a covariate in models)",
    source = "Derived - script 01", pii = FALSE, needs_review = TRUE),

  data.table(variable_name = "CE_ANA_None",
    group = "Derived - ANA",
    definition = "Binary flag: 1 = respondent reported ignoring no attributes (attended to all 5 CE attributes; CE_ANA_1 through CE_ANA_5 all = 0)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "CE_ANA_All",
    group = "Derived - ANA",
    definition = "Binary flag: 1 = respondent reported ignoring all attributes (CE_ANA_1 through CE_ANA_5 all = 1; extreme non-attendance)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Protest_Labelled",
    group = "Derived - ANA",
    definition = "Labelled protest vote indicator: category label (e.g. 'Protest', 'Non-protest') derived from Q29CEProtest_Statements",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Protest_True",
    group = "Derived - ANA",
    definition = "Binary flag: 1 = respondent is classified as a true protest voter (chose Plan C in all tasks AND gave a protest reason in Q29)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  # ── Derived Wellbeing ─────────────────────────────────────────────────────────
  data.table(variable_name = "Biowell_Beetle1_Mean",
    group = "Derived - Wellbeing",
    definition = "Mean Biowell wellbeing score across 5 items for task 1 (Beetles, Encounter scenario); mean of Items 1-5",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Beetle2_Mean",
    group = "Derived - Wellbeing",
    definition = "Mean Biowell wellbeing score for task 4 (Beetles, Existence scenario)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Beetle3_Mean",
    group = "Derived - Wellbeing",
    definition = "Mean Biowell wellbeing score for task 7 (Beetles, Bequest scenario)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Bee1_Mean",
    group = "Derived - Wellbeing",
    definition = "Mean Biowell wellbeing score for task 2 (Bees, Encounter scenario)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Bee2_Mean",
    group = "Derived - Wellbeing",
    definition = "Mean Biowell wellbeing score for task 5 (Bees, Existence scenario)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Bee3_Mean",
    group = "Derived - Wellbeing",
    definition = "Mean Biowell wellbeing score for task 8 (Bees, Bequest scenario)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Wasp1_Mean",
    group = "Derived - Wellbeing",
    definition = "Mean Biowell wellbeing score for task 3 (Wasps, Encounter scenario)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Wasp2_Mean",
    group = "Derived - Wellbeing",
    definition = "Mean Biowell wellbeing score for task 6 (Wasps, Existence scenario)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Biowell_Wasp3_Mean",
    group = "Derived - Wellbeing",
    definition = "Mean Biowell wellbeing score for task 9 (Wasps, Bequest scenario)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Overall",
    group = "Derived - Wellbeing",
    definition = "Overall mean Biowell wellbeing score across all 9 tasks and all insect groups",
    source = "Derived - script 01", pii = FALSE, needs_review = TRUE),

  # ── Derived Misc ──────────────────────────────────────────────────────────────
  data.table(variable_name = "Age_Dummy",
    group = "Derived - Demographics",
    definition = "Binary age dummy (likely 1 = over 60 or another threshold; exact definition — check script 01)",
    source = "Derived - script 01", pii = FALSE, needs_review = TRUE),

  data.table(variable_name = "Female_dummy",
    group = "Derived - Demographics",
    definition = "Binary dummy: 1 = respondent identifies as female (derived from Q2Gender)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q3Country_Wales",
    group = "Derived - Demographics",
    definition = "Binary dummy: 1 = respondent is resident in Wales (derived from Q3Country)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Q3Country_Scotland",
    group = "Derived - Demographics",
    definition = "Binary dummy: 1 = respondent is resident in Scotland (derived from Q3Country)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Dummy_Speeders",
    group = "Quality Control",
    definition = "Binary flag: 1 = respondent flagged as a speeder in either the survey or valuation sections (combined speeder flag)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Visit_Green_Never",
    group = "Nature Visits & Behaviour",
    definition = "Binary dummy: 1 = respondent never visits green spaces (derived from Q41_FreqOfVisitGreenSpace_1)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Visit_Blue_Never",
    group = "Nature Visits & Behaviour",
    definition = "Binary dummy: 1 = respondent never visits blue spaces (derived from Q41_FreqOfVisitBlueSpace_1)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Visit_Green_Always",
    group = "Nature Visits & Behaviour",
    definition = "Binary dummy: 1 = respondent visits green spaces at least once per week (derived from Q41_FreqOfVisitGreenSpace_1)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Visit_Blue_Always",
    group = "Nature Visits & Behaviour",
    definition = "Binary dummy: 1 = respondent visits blue spaces at least once per week (derived from Q41_FreqOfVisitBlueSpace_1)",
    source = "Derived - script 01", pii = FALSE, needs_review = FALSE),

  # ── Discounting Derived ───────────────────────────────────────────────────────
  data.table(variable_name = "first_switch_task",
    group = "Discounting",
    definition = "The first QDiscounting item (1-7) at which the respondent switched from preferring immediate to delayed payment; used to impute discount rate (derived in script 02)",
    source = "Derived - script 02", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "always_chose_oneMonth",
    group = "Discounting",
    definition = "Binary flag: 1 = respondent always preferred the immediate (one-month) payment across all 7 discounting questions (very high discount rate; derived in script 02)",
    source = "Derived - script 02", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "always_chose_oneYear",
    group = "Discounting",
    definition = "Binary flag: 1 = respondent always preferred the delayed (one-year) payment across all 7 discounting questions (very low discount rate; derived in script 02)",
    source = "Derived - script 02", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "DiscountRate",
    group = "Discounting",
    definition = "Implied annual discount rate (%) estimated from switch point in the 7 discounting questions (derived in script 02)",
    source = "Derived - script 02", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "DiscountRate_TextCategory",
    group = "Discounting",
    definition = "Text label for the DiscountRate category (e.g. 'Low', 'Medium', 'High'; derived in script 02)",
    source = "Derived - script 02", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "DiscountRate_Scaled",
    group = "Discounting",
    definition = "Standardised (z-scored or 0-1 scaled) version of DiscountRate for use in regression models (derived in script 02)",
    source = "Derived - script 02", pii = FALSE, needs_review = FALSE),

  # ── Duplicate/Admin ───────────────────────────────────────────────────────────
  data.table(variable_name = "RID.1",
    group = "Survey Admin",
    definition = "Duplicate of RID (auto-generated by merge operation in script 03; identical to RID — can be dropped for analysis)",
    source = "Derived - script 03 (merge artefact)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "design_offset",
    group = "CE - Experimental Design",
    definition = "CE design offset: integer used to align the CE block allocator with the bid vector randomiser when merging experimental design attributes onto respondent records (script 03)",
    source = "Derived - script 03", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "design_offset_old",
    group = "CE - Experimental Design",
    definition = "Previous version of design_offset (retained for audit trail; superseded by design_offset after pilot design revision)",
    source = "Derived - script 03", pii = FALSE, needs_review = TRUE),

  # ── CE Response Dummies (long-format merge) ───────────────────────────────────
  data.table(variable_name = "Choice_1",
    group = "CE - Responses (Merged)",
    definition = "Choice task 1: chosen alternative coded as integer (1=Plan A, 2=Plan B, 3=Plan C/status quo); derived from CE_1_DV in script 03",
    source = "Derived - script 03", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Choice_2",
    group = "CE - Responses (Merged)",
    definition = "Choice task 2: chosen alternative (1=A, 2=B, 3=C)",
    source = "Derived - script 03", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Choice_3",
    group = "CE - Responses (Merged)",
    definition = "Choice task 3: chosen alternative",
    source = "Derived - script 03", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Choice_4",
    group = "CE - Responses (Merged)",
    definition = "Choice task 4: chosen alternative",
    source = "Derived - script 03", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Choice_5",
    group = "CE - Responses (Merged)",
    definition = "Choice task 5: chosen alternative",
    source = "Derived - script 03", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Choice_6",
    group = "CE - Responses (Merged)",
    definition = "Choice task 6: chosen alternative",
    source = "Derived - script 03", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Choice_7",
    group = "CE - Responses (Merged)",
    definition = "Choice task 7: chosen alternative",
    source = "Derived - script 03", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Choice_8",
    group = "CE - Responses (Merged)",
    definition = "Choice task 8: chosen alternative",
    source = "Derived - script 03", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Choice_9",
    group = "CE - Responses (Merged)",
    definition = "Choice task 9: chosen alternative",
    source = "Derived - script 03", pii = FALSE, needs_review = FALSE),

  # ── Spatial ───────────────────────────────────────────────────────────────────
  data.table(variable_name = "Postcode",
    group = "Spatial (PII)",
    definition = "Full home postcode (derived from Q8Postcode after cleaning; used as spatial join key in script 04). PII — strip before deposit.",
    source = "Derived - script 04 (from Q8Postcode)", pii = TRUE, needs_review = FALSE),

  data.table(variable_name = "post_area",
    group = "Spatial (PII)",
    definition = "Postcode area (first 1-2 letters of postcode, e.g. 'LS', 'SW'); less granular than full postcode but still potentially identifying. Review before deposit.",
    source = "Derived - script 04", pii = TRUE, needs_review = FALSE),

  data.table(variable_name = "latitude",
    group = "Spatial (PII)",
    definition = "Latitude (decimal degrees) of respondent's postcode centroid (from ONS postcode lookup). PII — strip before deposit.",
    source = "Derived - script 04 (ONS postcode lookup)", pii = TRUE, needs_review = FALSE),

  data.table(variable_name = "longitude",
    group = "Spatial (PII)",
    definition = "Longitude (decimal degrees) of respondent's postcode centroid. PII — strip before deposit.",
    source = "Derived - script 04 (ONS postcode lookup)", pii = TRUE, needs_review = FALSE),

  data.table(variable_name = "admin_district",
    group = "Spatial",
    definition = "Local authority district name corresponding to respondent's postcode (from ONS postcode lookup)",
    source = "Derived - script 04 (ONS postcode lookup)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "admin_county",
    group = "Spatial",
    definition = "County name corresponding to respondent's postcode (from ONS postcode lookup)",
    source = "Derived - script 04 (ONS postcode lookup)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "ctyua19nm",
    group = "Spatial",
    definition = "County and Unitary Authority name (2019 boundaries) matched to respondent's postcode; used for regional analysis",
    source = "Derived - script 04 (CTYUA Dec 2019 shapefile)", pii = FALSE, needs_review = FALSE),

  # ── Factor Scores ─────────────────────────────────────────────────────────────
  data.table(variable_name = "Sliders_VSS_PA1",
    group = "Factor Scores",
    definition = "Factor score on principal component 1 from Very Simple Structure (VSS) factor analysis of the 12 Sliding scale items (script 05)",
    source = "Derived - script 05 (factor analysis)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Sliders_VSS_PA2",
    group = "Factor Scores",
    definition = "Factor score on principal component 2 from VSS factor analysis of the 12 Sliding scale items (script 05)",
    source = "Derived - script 05 (factor analysis)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Sliders_FA_PA1",
    group = "Factor Scores",
    definition = "Factor score on principal axis 1 from EFA of the 12 Sliding scale items (script 05)",
    source = "Derived - script 05 (factor analysis)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Sliders_FA_PA2",
    group = "Factor Scores",
    definition = "Factor score on principal axis 2 from EFA of the 12 Sliding scale items (script 05)",
    source = "Derived - script 05 (factor analysis)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Sliders_FA_PA3",
    group = "Factor Scores",
    definition = "Factor score on principal axis 3 from EFA of the 12 Sliding scale items (script 05)",
    source = "Derived - script 05 (factor analysis)", pii = FALSE, needs_review = FALSE),

  data.table(variable_name = "Sliders_FA_PA4",
    group = "Factor Scores",
    definition = "Factor score on principal axis 4 from EFA of the 12 Sliding scale items (script 05)",
    source = "Derived - script 05 (factor analysis)", pii = FALSE, needs_review = FALSE),

  # ── Model Output ──────────────────────────────────────────────────────────────
  data.table(variable_name = "SerialSQ",
    group = "Model Output",
    definition = "Serial number of the status quo (Plan C) choice — used to identify respondents who always chose the status quo; intermediate variable for protest classification",
    source = "Derived - script 06", pii = FALSE, needs_review = TRUE),

  data.table(variable_name = "ClassMembership",
    group = "Model Output",
    definition = "Modal latent class assignment from the LC-MXL model (classes 1, 2, 3): 1 = Pro-insect (~42%), 2 = Insect-averse (~26%), 3 = Ambivalent (~32%). Assigned based on maximum posterior class probability from PiValues.rds.",
    source = "Model output - script 06 (from 07_Druid_Model_TruncatedLC3C.R)", pii = FALSE, needs_review = FALSE)

))  # end manual list


# **********************************************************************************
#### Section 2b: Confirmed overrides from SI PDF (pdftools) and scripts 01/02 ####
# This section corrects and completes needs_review items using:
#   - DisentanglingNonUseInsects_SI_Accepted_V1_PDF.pdf (exact question wording)
#   - 01_Druid_Setup_CleaningMain.R  (derived variable definitions)
#   - 02_Druid_Setup_DiscountingMain.R (discounting variables)
# **********************************************************************************

upd <- function(dt, var, ...) {
  args <- list(...)
  for (field in names(args)) dt[variable_name == var, (field) := args[[field]]]
}

# ── Q11: exact wording confirmed (SI A4.2 + script 01) ──────────────────────
upd(manual, "Q11OpinionOnInsectPopulations_1",
    definition   = "Q11 (SI A4.2): 'I believe that insect populations have declined in Great Britain' (5-point Likert: Strongly disagree | Slightly disagree | Neither agree nor disagree | Slightly agree | Strongly agree)",
    needs_review = FALSE)

upd(manual, "Q11OpinionOnInsectPopulations_2",
    definition   = "Q11 (SI A4.2): 'I believe that insect populations will decline in the future in Great Britain' (5-point Likert: Strongly disagree to Strongly agree)",
    needs_review = FALSE)

# ── Q12 follow-up: confirmed (SI A5.3, labelled Q13 in SI, + script 01) ─────
upd(manual, "Q12InsectIDQuizFollowUp_1",
    definition   = "Q13 (SI A5.3, Page 11): 'I did not know that the same type of insect could look so different' (5-point Likert: Strongly disagree to Strongly agree; displayed after correct answers revealed)",
    source       = "Survey Q13 (SI numbering) - direct response (Page 11)",
    needs_review = FALSE)

upd(manual, "Q12InsectIDQuizFollowUp_2",
    definition   = "Q13 (SI A5.3, Page 11): 'I have seen most of the types of insects pictured before' (5-point Likert: Strongly disagree to Strongly agree)",
    source       = "Survey Q13 (SI numbering) - direct response (Page 11)",
    needs_review = FALSE)

# ── CE consequentiality: confirmed (SI A7.1, Q14 in SI) ─────────────────────
upd(manual, "CE_TasksConsequentiality_1",
    definition   = "Q14 (SI A7.1): 'I am confident that my choices in this survey will be considered in decision making' (5-point Likert; Page 21)",
    source       = "Survey Q14 (SI numbering) - consequentiality (Page 21)",
    needs_review = FALSE)

upd(manual, "CE_TasksConsequentiality_2",
    definition   = "Q14 (SI A7.1): 'The proposed plans (A or B) would be effective at changing insect populations' (5-point Likert; Page 21)",
    source       = "Survey Q14 (SI numbering) - consequentiality (Page 21)",
    needs_review = FALSE)

# ── CE debrief: exact wording (SI A9.3, Q17a/b) ─────────────────────────────
upd(manual, "CE_Debrief_Certain",
    definition   = "Q17a (SI A9.3): 'I am certain about the answers I gave' (sliding scale: Strongly Disagree to Strongly Agree; Page 33)")

upd(manual, "CE_Debrief_Confident",
    definition   = "Q17b (SI A9.3): 'I am confident that my choices in this survey will be considered in decision making' (sliding scale: Strongly Disagree to Strongly Agree; Page 33)")

# ── CE ANA: exact attribute text + corrected coding (SI A9.4, Q18) ──────────
# NOTE: CE_ANA = 1 means 'I considered this' (attended); 2 means 'I did not consider this' (ignored)
# See script 01: Considered_Insect = sum(CE_ANA_1 == 1)
upd(manual, "CE_ANA_1",
    definition   = "Q18 (SI A9.4): 'The insect pictured (e.g., beetle, bee, wasp)' — 1 = I considered this attribute | 2 = I did not consider this attribute (non-attendance)",
    needs_review = FALSE)

upd(manual, "CE_ANA_2",
    definition   = "Q18 (SI A9.4): 'Area managed for insects to be encountered by people now' (encounter/use value attribute) — 1 = considered | 2 = did not consider",
    needs_review = FALSE)

upd(manual, "CE_ANA_3",
    definition   = "Q18 (SI A9.4): 'Area managed for insects to exist now' (existence value attribute) — 1 = considered | 2 = did not consider",
    needs_review = FALSE)

upd(manual, "CE_ANA_4",
    definition   = "Q18 (SI A9.4): 'Area managed for insects to exist in the future' (bequest value attribute) — 1 = considered | 2 = did not consider",
    needs_review = FALSE)

upd(manual, "CE_ANA_5",
    definition   = "Q18 (SI A9.4): 'Additional income tax per month for 10 years' (tax/cost attribute) — 1 = considered | 2 = did not consider",
    needs_review = FALSE)

# CORRECTION — original definitions were transposed:
upd(manual, "CE_ANA_None",
    definition   = "Binary flag: 1 = respondent did NOT consider any of the 5 CE attributes (all CE_ANA_1-5 == 2; complete non-attendance; quality control exclusion flag). Derived in script 01.",
    needs_review = FALSE)

upd(manual, "CE_ANA_All",
    definition   = "Binary flag: 1 = respondent considered ALL 5 CE attributes (all CE_ANA_1-5 == 1; full attendance; no non-attendance). Derived in script 01.",
    needs_review = FALSE)

# ── Biowell: exact stem and bipolar pairs (SI A10.1) ────────────────────────
# Stem: 'Knowing that I can [encounter/know they exist/know they will exist] [insect] when I am outside makes me feel...'
# Bipolar pairs: Physically relaxed↔Physically tense | Joyful↔Sad | Clear minded↔Muddled |
#                Open to people↔Closed to people | Part of something bigger↔Not part of something bigger
# Raw slider: 0 = positive pole (relaxed, joyful, etc.); 100 = negative pole.
# Biowell_*_Mean applies (100 - raw) so that HIGHER score = BETTER wellbeing.

biowell_items <- list(
  "1_Relaxed" = "Physically relaxed (0) ↔ Physically tense (100)",
  "2_Joy"     = "Joyful (0) ↔ Sad (100)",
  "3_Clear"   = "Clear minded (0) ↔ Muddled (100)",
  "4_Open"    = "Open to people (0) ↔ Closed to people (100)",
  "5_Connect" = "Part of something bigger than myself (0) ↔ Not part (100)"
)

biowell_tasks <- list(
  Beetle1 = "Beetles, Encounter scenario",
  Bee1    = "Bees, Encounter scenario",
  Wasp1   = "Wasps, Encounter scenario",
  Beetle2 = "Beetles, Existence scenario",
  Bee2    = "Bees, Existence scenario",
  Wasp2   = "Wasps, Existence scenario",
  Beetle3 = "Beetles, Bequest scenario",
  Bee3    = "Bees, Bequest scenario",
  Wasp3   = "Wasps, Bequest scenario"
)

for (task in names(biowell_tasks)) {
  for (item_key in names(biowell_items)) {
    parts  <- strsplit(item_key, "_")[[1]]
    suffix <- paste(parts[-1], collapse = "_")
    vname  <- paste0("Biowell_", task, "_Item", item_key)
    new_def <- sprintf(
      "BIO-WELL task (%s): stem 'Knowing that I can [value framing] [insect] when I am outside makes me feel...' — Item %s (%s). Raw slider 0-100; lower raw = positive pole (see bipolar pair). Biowell_*_Mean applies 100-raw inversion.",
      biowell_tasks[[task]], parts[1], biowell_items[[item_key]]
    )
    if (task %in% names(manual[variable_name == vname, variable_name])) {
      manual[variable_name == vname, definition := new_def]
    }
  }
}

# ── Biowell Means: note inversion ────────────────────────────────────────────
mean_note <- ". Score = mean of (100 - raw item) across 5 items; range 0-100; higher = better wellbeing."
for (v in c("Biowell_Beetle1_Mean","Biowell_Beetle2_Mean","Biowell_Beetle3_Mean",
            "Biowell_Bee1_Mean","Biowell_Bee2_Mean","Biowell_Bee3_Mean",
            "Biowell_Wasp1_Mean","Biowell_Wasp2_Mean","Biowell_Wasp3_Mean")) {
  manual[variable_name == v, definition := paste0(definition, mean_note)]
}

upd(manual, "Overall",
    definition   = "Mean of all 9 Biowell_*_Mean scores (each already 100-raw inverted); respondent's overall wellbeing score across all insect types and value framings. Derived in script 01: rowSums(BioWellScores) / 9.",
    needs_review = FALSE)

# ── Sliding scales: exact question wording (SI A11.1) ───────────────────────
# Four statements per insect on separate pages: Encounter | Ecology | Conserved | Important (negatively worded)
for (ins in c("Beetles","Bees","Wasps")) {
  ins_lower <- tolower(ins)
  upd(manual, paste0("Sliding_", ins, "_Encounter"),
      definition = paste0("Sliding scale (SI A11.1, Pages 47-49): 'I encounter ", ins_lower, " frequently in my daily life' (Strongly disagree to Strongly agree)"))
  upd(manual, paste0("Sliding_", ins, "_Ecology"),
      definition = paste0("Sliding scale (SI A11.1): 'I have a good understanding of the ecological roles that ", ins_lower, " play' (Strongly disagree to Strongly agree)"))
  upd(manual, paste0("Sliding_", ins, "_Conserved"),
      definition = paste0("Sliding scale (SI A11.1): '", ins, " should be conserved for future generations' (Strongly disagree to Strongly agree)"))
  upd(manual, paste0("Sliding_", ins, "_Important"),
      definition = paste0("Sliding scale (SI A11.1): '", ins, " are not important if no-one encounters them' (negatively worded; Strongly disagree to Strongly agree)"))
}

# ── Visit: exact question wording (SI A12.1) ─────────────────────────────────
upd(manual, "Visit_Child",
    definition   = "SI A12.1: 'As a child (up to 12 years old) I spent a lot of time outdoors' (sliding scale: Strongly disagree to Strongly agree; Pages 47-49)",
    source       = "Survey - SI A12.1 (Pages 47-49)",
    needs_review = FALSE)

upd(manual, "Visit_Teen",
    definition   = "SI A12.1: 'As a teenager (13 to 19 years old) I spent a lot of time outdoors' (sliding scale: Strongly disagree to Strongly agree)",
    source       = "Survey - SI A12.1 (Pages 47-49)",
    needs_review = FALSE)

upd(manual, "Visit_Adult",
    definition   = "SI A12.1: 'As an adult (20+), I spend a lot of time outdoors' (sliding scale: Strongly disagree to Strongly agree)",
    source       = "Survey - SI A12.1 (Pages 47-49)",
    needs_review = FALSE)

# ── Q41 visit frequency: exact options + Q numbers (SI A12.2) ────────────────
upd(manual, "Q41_FreqOfVisitGreenSpace_1",
    definition = "Q20a (SI A12.2): 'Over the past year, how frequently did you visit green spaces (gardens, parks, fields, grasslands, woodlands, forests, nature reserves, farmland)?' Options: 1=I did not visit | 2=Once or twice a year | 3=Once or twice a season | 4=Once or twice a month | 5=Once a week | 6=Several times in a week | 7=Daily")

upd(manual, "Q41_FreqOfVisitBlueSpace_1",
    definition = "Q20b (SI A12.2): 'Over the past year, how frequently did you visit blue spaces (canals, lakes, rivers, beaches, freshwater or coastal areas)?' Options: 1=I did not visit | 2=Once or twice a year | 3=Once or twice a season | 4=Once or twice a month | 5=Once a week | 6=Several times in a week | 7=Daily")

upd(manual, "Q41_WalkTimeToGreenSpace",
    definition = "Q21a (SI A12.3): 'How long does it take to walk to your nearest local green space?' Options: 1=5 min or less | 2=6-15 min | 3=16-30 min | 4=31-45 min | 5=More than 45 min | 6=I never walk to it")

upd(manual, "Q41_WalkTimeToBlueSpace",
    definition = "Q21b (SI A12.3): 'How long does it take to walk to your nearest local water (canals, lakes, rivers, beaches)?' Options: 1=5 min or less | 2=6-15 min | 3=16-30 min | 4=31-45 min | 5=More than 45 min | 6=I never walk to it")

# ── Q42-Q47: exact wording and SI question numbers ───────────────────────────
upd(manual, "Q42_Charity",
    definition = "Q22 (SI A13.1): 'In the last five years have you been, or currently are, a member of any wildlife conservation or natural heritage organisations (e.g., RSPB, Butterfly Conservation, Buglife, Wildlife Trust)?' Originally Yes=2/No=1; recoded to 1=Yes / 0=No in script 01.",
    source     = "Survey Q22 (SI numbering) - direct response")

upd(manual, "Q43_Citizen",
    definition = "Q23 (SI A13.1): 'In the last five years have you been involved in any citizen science project monitoring insects?' Originally Yes=2/No=1; recoded to 1=Yes / 0=No in script 01.",
    source     = "Survey Q23 (SI numbering) - direct response")

upd(manual, "Q44_Sight",
    definition = "Q24 (SI A13.1): 'Do you have difficulties with your sight at the moment? (Please answer assuming you are wearing glasses or contact lenses if you need them.)' Originally Yes=2/No=1; recoded to 1=Yes / 0=No in script 01.",
    source     = "Survey Q24 (SI numbering) - direct response")

upd(manual, "Q45_Garden",
    definition = "Q25 (SI A13.1): 'Do you have access to your own garden?' Originally Yes=2/No=1; recoded to 1=Yes / 0=No in script 01.",
    source     = "Survey Q25 (SI numbering) - direct response")

upd(manual, "Q46_Farm",
    definition = "Q26 (SI A13.1): 'Do you work in farming or in agriculture?' Originally Yes=2/No=1; recoded to 1=Yes / 0=No in script 01.",
    source     = "Survey Q26 (SI numbering) - direct response")

upd(manual, "Q47_Fish",
    definition = "Q27 (SI A13.1): 'Over the last year did you undertake any fishing/angling?' Originally Yes=2/No=1; recoded to 1=Yes / 0=No in script 01.",
    source     = "Survey Q27 (SI numbering) - direct response")

# ── Q50 Understanding: exact wording (SI A15.1) ──────────────────────────────
upd(manual, "Q50_Understanding",
    definition = "Q30 (SI A15.1): 'On this scale, how clear was this survey to complete?' (sliding scale: Unclear to Clear; Page 57)",
    source     = "Survey Q30 (SI numbering) - direct response (Page 57)")

# ── FurtherComments: exact wording ───────────────────────────────────────────
upd(manual, "FurtherComments",
    definition = "Q31 (SI A15.1): 'Please use the box below to provide any additional comments.' Open text entry field (Page 57). Contains free text; review before deposit for PII.")

# ── Discounting: confirmed coding + specific trade-offs (SI A14.2 + script 02) ─
# QDiscounting_[1-7] in data correspond to fixed amounts: 190, 150, 100, 50, 25, 10, 0 (vs £200 in one year)
# Row order was randomised in the survey presentation but columns in dataset are in this fixed order.
disc_amounts <- c(190, 150, 100, 50, 25, 10, 0)
for (i in 1:7) {
  upd(manual, paste0("QDiscounting_", i),
      definition = sprintf(
        "Q29 (SI A14.2): 'Would you prefer to receive £%d in one month OR £200 in one year?' Binary: 1 = chose £%d now (one month) | 2 = chose £200 later (one year). Note: row order randomised in survey presentation; column order in data is fixed by payment amount.",
        disc_amounts[i], disc_amounts[i]
      ))
}

upd(manual, "first_switch_task",
    definition   = "Index (1-7) of the first QDiscounting item at which the respondent switched from preferring the immediate payment to the delayed payment. NA if respondent always chose the immediate option. Derived in script 02.",
    needs_review = FALSE)

upd(manual, "always_chose_oneMonth",
    definition   = "Binary flag: 1 = respondent always chose the immediate (one-month) payment across all 7 discounting items (no switch; implies extremely high discount rate; DiscountRate set to 1000). Derived in script 02.",
    needs_review = FALSE)

upd(manual, "always_chose_oneYear",
    definition   = "Binary flag: 1 = respondent always chose the delayed (one-year / £200) payment across all 7 discounting items (implies discount rate ≈ 0%). Derived in script 02.",
    needs_review = FALSE)

# ── Valuation timing: confirmed from script 01 ───────────────────────────────
upd(manual, "Valuation_Start",
    definition   = "SurveyEngine timestamp (PAGE_DISPLAY_13, page 13 = CE information and attention check): elapsed seconds from survey start when respondent first viewed the CE introduction. Merged from timestamps file in script 01.",
    source       = "SurveyEngine timestamps file - script 01",
    needs_review = FALSE)

upd(manual, "Valuation_Finish",
    definition   = "SurveyEngine timestamp (PAGE_SUBMIT_33, page 33 = sliding scale debrief after all 9 CE tasks): elapsed seconds when respondent submitted the post-CE debrief page. Merged from timestamps file in script 01.",
    source       = "SurveyEngine timestamps file - script 01",
    needs_review = FALSE)

# ── Derived demographic dummies: confirmed from script 01 ────────────────────
upd(manual, "ClassDummy",
    definition   = "Social grade dummy derived from Q6Occupation: 1 = ABC1 (Q6Occupation in 6-10: crafts/tradesperson, office/clerical/admin, middle management, senior management, professional) | 0 = C2DE (options 1-5). Derived in script 01.",
    needs_review = FALSE)

upd(manual, "Income_Dummy",
    definition   = "Binary income dummy: 1 = IncomeMidpoints_PlusMissing > sample median | 0 = at or below median. Derived in script 01.",
    needs_review = FALSE)

upd(manual, "Age_Dummy",
    definition   = "Binary age dummy: 1 = Q1Age >= sample median age | 0 = below median age. Derived in script 01.",
    needs_review = FALSE)

upd(manual, "Q5Ethnicity_Dummy",
    definition   = "Binary ethnicity dummy: 1 = Q5Ethnicity in 1-4 (any White background: White British, White Irish, White Gypsy/Roma, White other) | 0 = ethnic minority. Derived in script 01.",
    needs_review = FALSE)

upd(manual, "Q12InsectIDQuiz_DV_6_ScoreScaled",
    definition   = "Quiz score scaled to 0-100: (100/6) * Q12InsectIDQuiz_Score. Represents percentage of the 6 quiz items answered correctly (0=none correct, 100=all correct). Derived in script 01.",
    needs_review = FALSE)

upd(manual, "Dummy_HighEducation",
    definition   = "Binary dummy: 1 = Q48_Education in 5-6 (Degree or Higher Degree level; Q28 options 12-13 in the education drop-down) | 0 = otherwise. Derived in script 01.")

# ── Q4Urban recoding note ─────────────────────────────────────────────────────
upd(manual, "Q4Urban",
    definition   = "Q4: Type of residential area (rural/urban quota variable). Survey options: 1=Rural, 2=Urban. Recoded in script 01 by subtracting 1: 0=Rural, 1=Urban.")

# ── Protest: exact statements (from script 01 recode labels) ─────────────────
upd(manual, "Protest_Labelled",
    definition   = "Labelled protest reason derived from Q29CEProtest_Statements: 1='Cannot afford to pay' | 2='Not willing to pay' | 3='Plan not worth cost' | 4='Others' responsibility' | 5='Prefer no change' | 6='Do not like these insects' | 7='Did not understand tasks' | 8='Choice would make no difference' | 9='Plan would not work' | 10='Other'. Derived in script 01.")

upd(manual, "Protest_True",
    definition   = "Binary flag: 1 = true protest voter — respondent chose Plan C in all 9 tasks AND gave a reason OTHER than 'cannot afford' (code 1) or 'plan not worth cost' (code 3), which are considered legitimate zero-WTP responses rather than protests. Derived in script 01.")

upd(manual, "SerialSQ",
    definition   = "Binary flag: 1 = respondent chose Plan C (status quo / opt-out) in every one of their 9 CE tasks. Computed in script 03 as all(Choice == 3) per respondent. Used in script 06 to filter the estimation sample (SerialSQ == 1 respondents excluded from LC-MXL model).",
    source       = "Derived - script 03",
    needs_review = FALSE)

upd(manual, "metadata_flag",
    definition   = "Platform-generated metadata quality flag from SurveyEngine (e.g. browser/device anomaly, suspicious response pattern). Exact coding: see SurveyEngine data dictionary for the survey project.",
    needs_review = FALSE)

upd(manual, "design_offset_old",
    definition   = "Earlier version of design_offset retained in the data after the pilot design was revised. Values differ from design_offset; retained for audit trail but should not be used for analysis.",
    needs_review = FALSE)

# **********************************************************************************
#### Section 2c: Value labels lookup ####
# **********************************************************************************
# Hard-coded for all categoricals where coding is confirmed.
# Variables with "CONFIRM:" need user input before deposit.

vl_map <- list(
  # ── Demographics ────────────────────────────────────────────────────────────
  Q2Gender = "1=Female | 2=Male | 3=Prefer not to say (0 respondents in estimation sample — not present in data) | 4=In another way (5 respondents)",
  Q3Country = "1=England | 2=Scotland | 3=Wales (Northern Ireland not in sampling frame — GB-only survey; no code 4 in data)",
  Q4Urban   = "0=Rural | 1=Urban (recoded from survey original 1=Rural, 2=Urban by subtracting 1 in script 01)",
  Q5Ethnicity = paste0(
    "1=White: British/English/Welsh/Scottish/Northern Irish | ",
    "2=White: Irish | 3=White: Gypsy, Roma, Showmen, or Irish Traveller | 4=White: any other white background | ",
    "5=Mixed: White and Black Caribbean | 6=Mixed: White and Black African | ",
    "7=Mixed: White and Asian | 8=Mixed: any other mixed background | ",
    "9=Asian or Asian British: Indian | 10=Asian or Asian British: Pakistani | ",
    "11=Asian or Asian British: Bangladeshi | 12=Asian or Asian British: Chinese | ",
    "13=Asian or Asian British: any other Asian background | ",
    "14=Black or Black British: Caribbean | ",
    "15=Black or Black British: African | ",
    "16=Black or Black British: any other Black background | ",
    "17=Other ethnic group: Arab | 18=Any other ethnic group"
  ),
  Q6Occupation = paste0(
    "1=Homemaker/housewife or househusband | 2=Student/Full time education | ",
    "3=Retired | 4=Unemployed/receiving benefits | 5=Factory/manual worker | ",
    "6=Crafts/tradesperson/skilled worker | 7=Office/clerical/administration | ",
    "8=Middle management | 9=Senior management | 10=Professional"
  ),
  Q7Income = paste0(
    "1=£0-£1,000/month | 2=£1,001-£1,500 | 3=£1,501-£2,000 | ",
    "4=£2,001-£2,500 | 5=£2,501-£3,000 | 6=£3,001-£3,500 | ",
    "7=£3,501-£4,000 | 8=£4,001-£6,000 | 9=£6,001 or more | 10=Prefer not to say"
  ),
  Q48_Education = paste0(
    "1=No qualifications | ",
    "2=1-4 O Levels/CSEs/GCSEs or NVQ Level 1 | ",
    "3=5+ O Levels/CSEs/GCSEs, NVQ Level 2, AS Levels, Higher Diploma, or Diploma Apprenticeship | ",
    "4=2+ A Levels, NVQ Level 3, or BTEC National Diploma | ",
    "5=Degree, Higher Degree, NVQ Level 4-5, BTEC Higher Level, or professional qualifications (e.g. teaching, nursing, accountancy) | ",
    "6=Doctoral degree or equivalent | ",
    "7=Other qualifications (vocational/work-related, foreign qualifications, or level unknown)"
  ),
  # ── CE ANA ──────────────────────────────────────────────────────────────────
  CE_ANA_1 = "1=I considered this attribute when making my choices | 2=I did not consider this attribute",
  CE_ANA_2 = "1=I considered this attribute when making my choices | 2=I did not consider this attribute",
  CE_ANA_3 = "1=I considered this attribute when making my choices | 2=I did not consider this attribute",
  CE_ANA_4 = "1=I considered this attribute when making my choices | 2=I did not consider this attribute",
  CE_ANA_5 = "1=I considered this attribute when making my choices | 2=I did not consider this attribute",
  # ── CE design attributes ─────────────────────────────────────────────────────
  Encounter_PlanA_1 = "1=No change (+0%) | 2=Small increase (+15%) | 3=Large increase (+30%)",
  Existence_PlanA_1 = "1=No change (+0%) | 2=Small increase (+15%) | 3=Large increase (+30%)",
  Bequest_PlanA_1   = "1=No change (+0%) | 2=Small increase (+15%) | 3=Large increase (+30%)",
  Tax_PlanA_1       = "1=50p | 2=£2 | 3=£6 | 4=£12 | 5=£25 | 6=£50 per month for 10 years",
  Insect_PlanA_1    = "1=Wasps | 2=Bees | 3=Beetles",
  # ── CE choices ───────────────────────────────────────────────────────────────
  Choice_1 = "1=Plan A (conservation plan A) | 2=Plan B (conservation plan B) | 3=Plan C (status quo; no additional conservation)",
  # ── CE debrief / consequentiality ────────────────────────────────────────────
  CE_Task_Plan_Check_1 = "1=Strongly disagree | 2=Slightly disagree | 3=Neither agree nor disagree | 4=Slightly agree | 5=Strongly agree",
  CE_TasksConsequentiality = "1=Strongly disagree | 2=Slightly disagree | 3=Neither agree nor disagree | 4=Slightly agree | 5=Strongly agree",
  CE_Debrief_Certain   = "0-100 continuous slider (higher = more certain; confirmed from data range 0-100)",
  CE_Debrief_Confident = "0-100 continuous slider (higher = more confident; confirmed from data range 0-100)",
  # ── Background knowledge quiz follow-up ──────────────────────────────────────
  Q12InsectIDQuizFollowUp_1 = "1=Strongly disagree | 2=Slightly disagree | 3=Neither agree nor disagree | 4=Slightly agree | 5=Strongly agree",
  Q12InsectIDQuizFollowUp_2 = "1=Strongly disagree | 2=Slightly disagree | 3=Neither agree nor disagree | 4=Slightly agree | 5=Strongly agree",
  # ── Q11 CE setup understanding ───────────────────────────────────────────────
  Q11_1 = "1=Strongly disagree | 2=Slightly disagree | 3=Neither agree nor disagree | 4=Slightly agree | 5=Strongly agree",
  Q11_2 = "1=Strongly disagree | 2=Slightly disagree | 3=Neither agree nor disagree | 4=Slightly agree | 5=Strongly agree",
  Q11_3 = "1=Strongly disagree | 2=Slightly disagree | 3=Neither agree nor disagree | 4=Slightly agree | 5=Strongly agree",
  Q11_4 = "1=Strongly disagree | 2=Slightly disagree | 3=Neither agree nor disagree | 4=Slightly agree | 5=Strongly agree",
  # ── Visit frequency and walk time ────────────────────────────────────────────
  Q41_FreqOfVisitGreenSpace_1 = "1=Did not visit in the past year | 2=Once or twice a year | 3=Once or twice a season | 4=Once or twice a month | 5=Once a week | 6=Several times a week | 7=Daily or almost daily",
  Q41_FreqOfVisitBlueSpace_1  = "1=Did not visit in the past year | 2=Once or twice a year | 3=Once or twice a season | 4=Once or twice a month | 5=Once a week | 6=Several times a week | 7=Daily or almost daily",
  Q41_WalkTimeGreenSpace_1    = "1=5 minutes or less | 2=6-15 minutes | 3=16-30 minutes | 4=31-45 minutes | 5=More than 45 minutes | 6=I never walk to green space",
  Q41_WalkTimeBlueSpace_1     = "1=5 minutes or less | 2=6-15 minutes | 3=16-30 minutes | 4=31-45 minutes | 5=More than 45 minutes | 6=I never walk to blue space",
  # ── Discounting ──────────────────────────────────────────────────────────────
  QDiscounting_1 = "1=Receive £190 in one month (immediate) | 2=Receive £200 in one year (delayed)",
  QDiscounting_2 = "1=Receive £150 in one month | 2=Receive £200 in one year",
  QDiscounting_3 = "1=Receive £100 in one month | 2=Receive £200 in one year",
  QDiscounting_4 = "1=Receive £50 in one month  | 2=Receive £200 in one year",
  QDiscounting_5 = "1=Receive £25 in one month  | 2=Receive £200 in one year",
  QDiscounting_6 = "1=Receive £10 in one month  | 2=Receive £200 in one year",
  QDiscounting_7 = "1=Receive £0 now            | 2=Receive £200 in one year",
  DiscountRate_TextCategory = "AlwaysTomorrow=always chose delayed £200 (discount rate = 0%) | AlwaysToday=always chose immediate (extreme discounting) | Normal=switch point implies discount rate < 100%/year | High=switch point implies discount rate ≥ 100%/year",
  # ── Protest statements ────────────────────────────────────────────────────────
  Q29CEProtest_Statements = paste0(
    "1=I cannot afford to pay | 2=I am not willing to pay even though I can afford it | ",
    "3=The plan costs more than it is worth | 4=It is the responsibility of others to pay | ",
    "5=I prefer things to remain as they currently are | 6=I do not like insects | ",
    "7=I did not understand the choices | 8=My choice makes no difference to the outcome | ",
    "9=I do not think the plan will work | 10=Other reason (please specify)"
  ),
  # ── ClassMembership ──────────────────────────────────────────────────────────
  ClassMembership = "1=Class 1 (Pro-insect: high positive WTP, especially for bees; largest class) | 2=Class 2 (Insect-averse: negative or near-zero WTP for wasps; moderate class) | 3=Class 3 (Low-WTP: low or protest-like WTP across insects; smallest class)",
  # ── Age group ────────────────────────────────────────────────────────────────
  AgeGroup = "18-29 | 30-44 | 45-60 | 60+ (character strings; ordered by age)"
)

# Expand CE design value_labels to all 135 CE variables (all share same coding scheme)
ce_vl_encounter <- "1=No change (+0%) | 2=Small increase (+15%) | 3=Large increase (+30%)"
ce_vl_tax       <- "1=50p | 2=£2 | 3=£6 | 4=£12 | 5=£25 | 6=£50 per month for 10 years (additional income tax)"
ce_vl_insect    <- "1=Wasps | 2=Bees | 3=Beetles"
ce_vl_choice    <- "1=Plan A | 2=Plan B | 3=Plan C (status quo)"
ce_vl_ana       <- "1=I considered this attribute | 2=I did not consider this attribute"
ce_vl_check     <- "1=Strongly disagree | 2=Slightly disagree | 3=Neither agree nor disagree | 4=Slightly agree | 5=Strongly agree"

for (plan in c("PlanA","PlanB","PlanC")) {
  for (task in 1:9) {
    for (attr in c("Encounter","Existence","Bequest")) vl_map[[paste0(attr,"_",plan,"_",task)]] <- ce_vl_encounter
    vl_map[[paste0("Tax_",plan,"_",task)]]    <- ce_vl_tax
    vl_map[[paste0("Insect_",plan,"_",task)]] <- ce_vl_insect
    vl_map[[paste0("Choice_",task)]]          <- ce_vl_choice
  }
}
for (i in 1:5) vl_map[[paste0("CE_ANA_",i)]] <- ce_vl_ana
for (check in 1:3) vl_map[[paste0("CE_Task_Plan_Check_",check)]] <- ce_vl_check

# Expand Q11 and Q41 for all numbered items present in data
q11_items <- grep("^Q11_", names(d), value = TRUE)
for (v in q11_items) vl_map[[v]] <- "1=Strongly disagree | 2=Slightly disagree | 3=Neither agree nor disagree | 4=Slightly agree | 5=Strongly agree"
q42_q47 <- grep("^Q4[2-7]_", names(d), value = TRUE)
for (v in q42_q47) vl_map[[v]] <- "0=No | 1=Yes (recoded; original 1=Yes, 2=No reversed in script 01)"

vl_dt <- data.table(
  variable_name = names(vl_map),
  value_labels  = unlist(vl_map, use.names = FALSE)
)

# Merge into col_info
col_info <- merge(col_info, vl_dt, by = "variable_name", all.x = TRUE)

# For binary 0/1 variables with no explicit label mapping, auto-fill from units
col_info[is.na(value_labels) & grepl("^0/1", units, ignore.case = TRUE),
         value_labels := "0=No/absent | 1=Yes/present"]
# For continuous 0-100 sliders with no mapping, fill from units
col_info[is.na(value_labels) & grepl("slider 0-100|0-100 slider", units, ignore.case = TRUE) & is.na(value_labels),
         value_labels := "Continuous slider 0-100"]
# Free text
col_info[r_type == "character" & is.na(value_labels),
         value_labels := "Free text (character)"]


# **********************************************************************************
#### Section 2d: Question text from codeplan ####
# **********************************************************************************

suppressMessages(library(readxl))

cp_path <- "C:/Users/earpkin/OneDrive - University of Leeds/DRUID/D2/SurveyData/Main/main_resample_raw/delivery/D2_V2.codeplan.xlsx"

# Strip HTML tags, decode entities, remove embedded CSS/JS from SurveyEngine content
clean_q_text <- function(raw) {
  if (is.na(raw) || !nzchar(trimws(raw))) return(NA_character_)
  x <- gsub("<[^>]+>", "", raw)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  x <- gsub("&amp;",  "&", x, fixed = TRUE)
  x <- gsub("&lt;",   "<", x, fixed = TRUE)
  x <- gsub("&gt;",   ">", x, fixed = TRUE)
  x <- gsub("(?s)\\$\\(function.*", "", x, perl = TRUE)   # strip jQuery/JS
  x <- gsub("/\\*[^*]*\\*/", "", x)                        # strip CSS comments /* ... */
  if (grepl("}", x, fixed = TRUE))                         # CSS present: take after last }
    x <- sub("^.*\\}", "", x)
  x <- gsub("[ \t\n\r﻿]+", " ", x)                   # normalise whitespace BEFORE label strip
  x <- sub("(?i)\\s*(Strongly Disagree|Unclear|Not at all|Definitely).*$", "", x, perl = TRUE)
  x <- gsub("  +", " ", x)
  x <- trimws(x)
  if (nchar(x) < 5) return(NA_character_)
  x
}

# ── Auto-extract from Main survey sheet (SINGLE / OPEN / MULTIPLE etc.) ─────
ms_cp <- as.data.frame(suppressMessages(
  read_excel(cp_path, sheet = "Main survey", col_names = FALSE)
))
names(ms_cp) <- c("TYPE","LABEL","CONTENT","DISPLAY","OPTIONS","NOTE")

q_types_cp <- c("SINGLE","OPEN","MULTIPLE","MATRIX SINGLE","MATRIX MULTIPLE","DROPDOWN","NUMBER")
qt_map <- character(0)

for (i in seq_len(nrow(ms_cp))) {
  tp <- ms_cp[i,"TYPE"]; lb <- ms_cp[i,"LABEL"]; ct <- ms_cp[i,"CONTENT"]
  if (is.na(tp) || !tp %in% q_types_cp || is.na(lb) || is.na(ct)) next
  qtext <- clean_q_text(ct)
  if (!is.na(qtext) && nzchar(qtext)) qt_map[lb] <- qtext
}

# ── Hard-coded for matrix-row items (SurveyEngine exports as parent_row) ─────
qt_map["Q11OpinionOnInsectPopulations_1"] <- "Q11: 'I believe that insect populations have declined in Great Britain' (5-point Likert: Strongly disagree to Strongly agree)"
qt_map["Q11OpinionOnInsectPopulations_2"] <- "Q11: 'I believe that insect populations will decline in the future in Great Britain' (5-point Likert)"

qt_map["Q12InsectIDQuiz_1"] <- "Q12: 'Which type of insect is pictured?' (image 1 of 6; options: Ant | Bee | Beetle | Wasp | I don't know)"
for (k in 2:6) qt_map[paste0("Q12InsectIDQuiz_",k)] <- sub("image 1 of 6", paste0("image ",k," of 6"), qt_map["Q12InsectIDQuiz_1"])

qt_map["Q12InsectIDQuizFollowUp_1"] <- "Q13 (SI A5.3): 'I did not know that the same type of insect could look so different' (5-point Likert)"
qt_map["Q12InsectIDQuizFollowUp_2"] <- "Q13 (SI A5.3): 'I have seen most of the types of insects pictured before' (5-point Likert)"

qt_map["CE_Task_Plan_Check_1"]   <- "CE comprehension check: 'I understand the three conservation plans (A, B, C)' (5-point Likert)"
qt_map["CE_Task_Insect_Check_1"] <- "CE comprehension check: 'I understand the insect attribute' (5-point Likert)"
qt_map["CE_Task_A1_Check_1"]     <- "CE comprehension check: 'I understand the encounter/recreation value attribute' (5-point Likert)"
qt_map["CE_Task_A2_Check_1"]     <- "CE comprehension check: 'I understand the existence value attribute' (5-point Likert)"
qt_map["CE_Task_A3_Check_1"]     <- "CE comprehension check: 'I understand the bequest value attribute' (5-point Likert)"
qt_map["CE_Task_A4_Check_1"]     <- "CE comprehension check: 'I understand the tax/payment attribute' (5-point Likert)"
qt_map["CE_Task_A5_Check_1"]     <- "CE comprehension check: 'I understand the choice tasks' (5-point Likert)"

qt_map["CE_TasksConsequentiality_1"] <- "Q14 (SI A7.1): 'I am confident that my choices in this survey will be considered in decision making' (5-point Likert)"
qt_map["CE_TasksConsequentiality_2"] <- "Q14 (SI A7.1): 'The proposed plans (A or B) would be effective at changing insect populations' (5-point Likert)"

qt_map["CE_ANA_1"] <- "Q18 (SI A9.4): 'This plan focuses on these insects: [Bee/Beetle/Wasp pictured]' — did you consider this attribute when making your choices?"
qt_map["CE_ANA_2"] <- "Q18 (SI A9.4): 'Area managed for insects to be encountered by people now' — did you consider this attribute?"
qt_map["CE_ANA_3"] <- "Q18 (SI A9.4): 'Area managed for insects to exist now' — did you consider this attribute?"
qt_map["CE_ANA_4"] <- "Q18 (SI A9.4): 'Area managed for insects to exist in the future' — did you consider this attribute?"
qt_map["CE_ANA_5"] <- "Q18 (SI A9.4): 'Additional income tax per month for 10 years' — did you consider this attribute?"

disc_amounts <- c(190, 150, 100, 50, 25, 10, 0)
for (k in seq_along(disc_amounts)) {
  qt_map[paste0("QDiscounting_",k)] <- sprintf(
    "Q29 (SI A14.2): 'Would you prefer to receive £%d in one month OR £200 in one year?' (1=£%d now | 2=£200 in one year)",
    disc_amounts[k], disc_amounts[k]
  )
}

qt_map["Q41_FreqOfVisitGreenSpace_1"] <- "Q20a (SI A12.2): 'Over the past year, how frequently did you visit green spaces (gardens, parks, fields, grasslands, woodlands, forests, nature reserves, farmland)?' (1=Did not visit to 7=Daily)"
qt_map["Q41_FreqOfVisitBlueSpace_1"]  <- "Q20b (SI A12.2): 'Over the past year, how frequently did you visit blue spaces (canals, lakes, rivers, beaches, freshwater or coastal areas)?' (1=Did not visit to 7=Daily)"

# ── Biowell items: construct from variable name (slider JS not parseable) ─────
bw_stems    <- c(
  "1" = "Knowing that I can encounter %s when I am outside makes me feel...",
  "2" = "Knowing that %s exist when I am outside makes me feel...",
  "3" = "Knowing that %s will continue to exist for future generations makes me feel..."
)
bw_insects  <- c(Beetle1="beetles", Bee1="bees",    Wasp1="wasps",
                  Beetle2="beetles", Bee2="bees",    Wasp2="wasps",
                  Beetle3="beetles", Bee3="bees",    Wasp3="wasps")
bw_scenario <- c(Beetle1="1", Bee1="1", Wasp1="1",
                  Beetle2="2", Bee2="2", Wasp2="2",
                  Beetle3="3", Bee3="3", Wasp3="3")
bw_pairs    <- c(
  "1_Relaxed" = "Physically relaxed (0) vs. Physically tense (100)",
  "2_Joy"     = "Joyful (0) vs. Sad (100)",
  "3_Clear"   = "Clear minded (0) vs. Muddled (100)",
  "4_Open"    = "Open to people (0) vs. Closed to people (100)",
  "5_Connect" = "Part of something bigger (0) vs. Not part of something bigger (100)"
)

for (tk in names(bw_insects)) {
  stem <- sprintf(
    paste0("BIO-WELL (SI A10.1): '", bw_stems[bw_scenario[tk]], "'"),
    bw_insects[tk]
  )
  for (ik in names(bw_pairs)) {
    qt_map[paste0("Biowell_",tk,"_Item",ik)] <- paste0(stem, " [", bw_pairs[ik], "]")
  }
}
qt_map["Biowell_Debrief_BeetlesCertain"] <- "BIO-WELL debrief: certainty about wellbeing responses for beetles (slider 0-100)"
qt_map["Biowell_Debrief_BeeCertain"]     <- "BIO-WELL debrief: certainty about wellbeing responses for bees (slider 0-100)"
qt_map["Biowell_Debrief_WaspCertain"]    <- "BIO-WELL debrief: certainty about wellbeing responses for wasps (slider 0-100)"

# ── CE design attributes: verbatim from DCE4_spec ────────────────────────────
dce4_cp <- as.data.frame(suppressMessages(
  read_excel(cp_path, sheet = "DCE4_spec", col_names = FALSE)
))
names(dce4_cp) <- c("TYPE","LABEL","CONTENT","DISPLAY","OPTIONS")

attr_qt  <- list()
in_opt1  <- FALSE
for (i in seq_len(nrow(dce4_cp))) {
  tp <- dce4_cp[i,"TYPE"]; lb <- dce4_cp[i,"LABEL"]
  if (is.na(tp)) next
  if (tp == "ALTERNATIVE") { in_opt1 <- (!is.na(lb) && lb == "opt1"); next }
  if (!in_opt1 || tp != "ATTRIBUTE") next
  attr_qt[[lb]] <- trimws(gsub("[ \t\n\r]+"," ",
    gsub("&nbsp;"," ", gsub("<[^>]+>","", dce4_cp[i,"CONTENT"]))))
}

attr_xname <- c(x1 = "Insect", x2 = "Encounter", x3 = "Existence", x4 = "Bequest", x5 = "Tax")

for (plan in c("PlanA","PlanB","PlanC")) {
  for (task in 1:9) {
    for (ax in names(attr_xname)) {
      aname <- attr_xname[ax]
      qt_map[paste0(aname,"_",plan,"_",task)] <- sprintf(
        "CE attribute '%s' shown in %s for task %d", attr_qt[[ax]], plan, task
      )
    }
    qt_map[paste0("Choice_",task)] <- sprintf(
      "CE task %d: 'Which would you choose?' (Plan A vs. Plan B vs. Plan C / status quo)", task
    )
  }
}

# ── Merge into col_info ───────────────────────────────────────────────────────
qt_dt <- data.table(
  variable_name = names(qt_map),
  question_text = unname(qt_map)
)
col_info <- merge(col_info, qt_dt, by = "variable_name", all.x = TRUE)


# **********************************************************************************
#### Section 3: Programmatic CE design variable definitions ####
# **********************************************************************************

# CE design variables: [Attribute]_[Plan]_[Task]
# Attributes: Insect, Encounter, Existence, Bequest, Tax
# Plans: PlanA, PlanB, PlanC
# Tasks: 1-9

attr_defs <- list(
  Insect    = "Insect group featured in the conservation plan (Bee | Beetle | Wasp)",
  Encounter = "Encounter/recreation attribute level: expected change in probability of encountering [Insect] in the wild",
  Existence = "Existence value attribute level: expected improvement in [Insect] population to secure its continued existence",
  Bequest   = "Bequest value attribute level: expected improvement in [Insect] population to secure populations for future generations",
  Tax       = "Additional income tax per month for 10 years (bid levels: 50p | £2 | £6 | £12 | £25 | £50/month; Plan C always £0)"
)

plan_labels <- c(PlanA = "Plan A (insect-friendly habitat management scenario A)",
                 PlanB = "Plan B (insect-friendly habitat management scenario B)",
                 PlanC = "Plan C (status quo / opt-out: no new policy)")

ce_rows <- rbindlist(lapply(names(attr_defs), function(attr) {
  rbindlist(lapply(names(plan_labels), function(plan) {
    rbindlist(lapply(1:9, function(task) {
      vname <- paste0(attr, "_", plan, "_", task)
      def   <- paste0("CE experimental design — task ", task, ": ",
                      attr_defs[[attr]], " for ", plan_labels[[plan]])
      data.table(
        variable_name = vname,
        group         = "CE - Experimental Design",
        definition    = def,
        source        = "Derived - script 03 (merged from DCE design file)",
        pii           = FALSE,
        needs_review  = FALSE
      )
    }))
  }))
}))


# **********************************************************************************
#### Section 4: Merge everything ####
# **********************************************************************************

all_def <- rbind(manual, ce_rows, fill = TRUE)

# Join with auto-derived column info (type, n_unique, derived_coding)
codebook <- merge(col_info, all_def, by = "variable_name", all.x = TRUE)

# Use derived_coding as coding; mark anything unmatched
codebook[is.na(group), `:=`(
  group        = "UNMATCHED - check script",
  definition   = "",
  source       = "",
  pii          = NA,
  needs_review = TRUE
)]

# Preserve original column order from the data
codebook[, col_order := match(variable_name, names(d))]
setorder(codebook, col_order)

# Build final output columns (now includes all UKDS/Zenodo deposit columns)
out <- codebook[, .(
  variable_name,
  group,
  definition,
  question_text,                       # populated from codeplan in Section 2d
  value_labels,
  coding           = derived_coding,
  measurement_level,
  units,
  filter_condition,
  n_valid,
  n_unique,
  r_type,
  source,
  pii,
  needs_review
)]

# question_text: set N/A for derived, admin, and model output variables
out[grepl("^Survey Admin", group),
    question_text := "N/A (survey administrative or metadata variable)"]
out[grepl("^Derived|^Quality Control|^Factor Scores|^Model Output", group),
    question_text := "N/A (derived or model output variable)"]
out[grepl("^Spatial", group),
    question_text := "N/A (spatial/geographic variable derived from postcode lookup)"]
out[variable_name %in% c("CE_BlockAllocator","BidVectorElicitation_Randomiser",
                          "design_offset","design_offset_old","RID.1"),
    question_text := "N/A (experimental design allocation or administrative variable)"]
out[is.na(question_text), question_text := "N/A (see definition)"]

# Summary stats
cat(sprintf("Total variables: %d\n", nrow(out)))
cat(sprintf("  needs_review = TRUE:  %d\n", sum(out$needs_review, na.rm = TRUE)))
cat(sprintf("  PII = TRUE:           %d\n", sum(out$pii,          na.rm = TRUE)))
cat(sprintf("  Unmatched:            %d\n", sum(out$group == "UNMATCHED - check script", na.rm = TRUE)))
cat(sprintf("  value_labels CONFIRM: %d variables need user label confirmation\n",
            sum(grepl("CONFIRM", out$value_labels, fixed = TRUE), na.rm = TRUE)))


# **********************************************************************************
#### Section 5: Export (Codebook + Study Metadata tabs) ####
# **********************************************************************************

study_meta <- data.frame(
  field = c(
    "Title",
    "Short title / dataset name",
    "Principal Investigator",
    "Contact email",
    "Co-investigators / contributors",
    "Funder",
    "Grant / project reference",
    "Ethics reference",
    "Country of data collection",
    "Survey mode",
    "Survey platform",
    "Data collection period",
    "Sampling method",
    "Screening criteria",
    "N respondents (raw)",
    "N respondents (cleaned / estimation sample)",
    "Exclusion criteria applied",
    "Languages",
    "Publication title",
    "Publication journal",
    "Publication DOI",
    "Dataset DOI (Zenodo)",
    "GitHub repository",
    "Licence (data)",
    "Licence (code)",
    "Variables (total)",
    "Variables (PII — strip before public deposit)",
    "Codebook script",
    "Codebook last updated"
  ),
  value = c(
    "Heterogeneous Preferences for Use and Non-use Values Provided by Insects",
    "DRUID D2 insect survey — stated preference / choice experiment",
    "Dr Peter King",
    "p.king1@leeds.ac.uk",
    "CONFIRM: add co-investigators here",
    "Natural Environment Research Council (NERC)",
    "DRUID project (Declining and Recovering UK Insect Diversity)",
    "University of Leeds, School of Earth and Environment Ethics Committee — BESS+ FREC 2023-0769-1031",
    "United Kingdom (England, Scotland, Wales)",
    "Self-completion online survey",
    "SurveyEngine",
    "October 2024 (CONFIRM exact start and end dates)",
    "Quota-sampled online panel: quotas on age, gender, and region (England/Scotland/Wales); additional quota on urban/rural classification",
    "UK residents aged 18+; able to complete survey in English",
    "1,684",
    "1,476 (after exclusions — see below)",
    "Respondents under 18; respondents choosing Plan C (status quo) in all 9 CE tasks (protest behaviour, SerialSQ==1); survey speeders below threshold (Dummy_Speeders==1)",
    "English",
    "Heterogeneous Preferences for Use and Non-use Values Provided by Insects",
    "People and Nature",
    "CONFIRM: add DOI once published / accepted",
    "CONFIRM: add Zenodo DOI once deposited",
    "CONFIRM: add GitHub URL",
    "CC BY 4.0 (CONFIRM with University of Leeds data management policy)",
    "MIT (CONFIRM)",
    as.character(nrow(out)),
    "9 flagged: IP_repeat_group, START, DATETIME.UTC, Q8Postcode, FurtherComments, Postcode, post_area, latitude, longitude — REMOVE before public deposit",
    "Scripts/Tables/19_Druid_Codebook.R",
    format(Sys.Date(), "%Y-%m-%d")
  ),
  stringsAsFactors = FALSE
)

write_xlsx(
  list(
    Codebook       = as.data.frame(out),
    Study_Metadata = study_meta
  ),
  here("OtherOutput/Tables", "D2_Codebook.xlsx")
)

cat("Saved: OtherOutput/Tables/D2_Codebook.xlsx\n")
cat(sprintf("  Sheets: Codebook (%d rows × %d cols), Study_Metadata (%d rows)\n",
            nrow(out), ncol(out), nrow(study_meta)))

#### End of script
