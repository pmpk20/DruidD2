Author: Dr Peter King ([p.king1\@leeds.ac.uk](mailto:p.king1@leeds.ac.uk){.email})

Last Change: 23/04/2026

## DRUID Insect survey

### Here you will find the survey design, experimental design, R code, raw responses, and model outputs.

### PLEASE NOTE: Data are publicly available in this repository and will be assigned a permanent DOI via Zenodo.

#### Description:

-   `/Data/Main/` contains anonymised survey data for N = 1,684 online UK respondents from October 2024. Raw SurveyEngine export files (covariates xlsx, timestamps xlsx) are withheld; the anonymised input to the pipeline is `Data_Covariates_Step0.csv`. See the commented block at the top of `01_Druid_Setup_CleaningMain.R` for details of what was withheld and why.
-   `/Survey/` has the full survey reproduced and explained alongside pictures used therein.
-   `/Biowell4/` is a backup for the Shiny app [here](https://pmpk20.shinyapps.io/biowell4/) demonstrating wellbeing scores from insects.
-   Run `00_D2_Replicator.R` to execute all replication scripts in order. Scripts are numbered 01--15 by execution sequence.
-   Data are publicly available in this repository. A Zenodo DOI will be linked here upon deposit.

#### Directory structure (replication files only):

```         
D2Backup/
├── 00_D2_Replicator.R
├── Data/
│   └── Main/
│       ├── DRUID_resampling_DCE_d2_test2_2024-10-31.xlsx              # raw DCE choice responses (resampling wave)
│       ├── Data_Covariates_Step0.csv                                  # [input to 01] anonymised covariates + valuation timing; 1,684 × 158
│       ├── Data_Covariates_Step1.csv                                  # [output of 01] wide-format covariates; 1,684 × 214
│       ├── Data_Covariates_Step2.csv                                  # [output of 02] wide-format covariates; 1,684 × 220
│       ├── Data_Covariates_Step3.csv                                  # [output of 03] wide-format covariates; 1,684 × 365
│       ├── database_Step3.csv                                         # [output of 05] long-format CE data; 15,156 × 266
│       ├── Data_Covariates_Step5.csv                                  # [output of 05] covariates with wellbeing factor scores; 1,684 × 378
│       └── Data_Covariates_Step6.csv                                  # [output of 06] estimation sample with class membership; 1,476 × 380
├── Scripts/
│   ├── Setup/
│   │   ├── 01_Druid_Setup_CleaningMain.R
│   │   ├── 02_Druid_Setup_DiscountingMain.R
│   │   ├── 03_Druid_Setup_MergeCE.R
│   │   ├── 04_Druid_Setup_Postcodes.R                                  # diagnostic only — not run in replicator
│   │   ├── 05_Druid_Setup_SlidersFactorAnalysis.R
│   │   └── 06_Druid_Setup_ClassMembership.R
│   ├── CEModelling/
│   │   ├── 07_Druid_Model_TruncatedLC3C.R                        # → Table C2 (estimates.csv)
│   │   └── 08_Druid_Model_SimulatedMeanWTP.R                     # → Table C1 (SimulatedMeans_Wide.csv)
│   ├── Tables/
│   │   ├── 09_Druid_Table1_ClassAllocation.R                     # Table 1
│   │   ├── 14_Druid_TableB2_SampleVsPopulation.R                 # Table B2
│   │   ├── 16_Druid_TableC3_WellbeingLVs.R                       # Table C3
│   │   ├── 17_Druid_TableC1_SimulatedMeanWTP.R                   # Table C1
│   │   ├── 18_Druid_TableC2_ModelEstimates.R                     # Table C2
│   │   └── 19_Druid_Codebook.R                                   # Data codebook
│   └── Figures/
│       ├── 10_Druid_Figure2_WTPClasses.R                         # Figure 2
│       ├── 11_Druid_Figure3_WellbeingDistribution.R              # Figure 3
│       ├── 12_Druid_Figure4_WellbeingWTP.R                       # Figure 4
│       ├── 13_Druid_FigureB1_CEDebrief.R                         # Figure B1
│       └── 15_Druid_FigureC1_WTPClassesDistribution.R            # Figure C1
├── Survey/
│   └── Figure1_ChoiceCard.png                                         # Figure 1 (static image, not script-generated)
├── CEOutput/
│   └── Main/
│       └── LCM/
│           ├── D2_Truncated_LC_3C_MXL_NoDR_V3_model.rds          # [output of 07] fitted model object
│           ├── D2_Truncated_LC_3C_MXL_NoDR_V3_model_PiValues.rds # [output of 07] posterior class probabilities
│           ├── D2_Truncated_LC_3C_MXL_NoDR_V3_estimates.csv       # [output of 07] parameter estimates
│           ├── D2_Truncated_LC_3C_MXL_NoDR_V3_output.txt          # [output of 07] full model output
│           └── D2_Truncated_LC_3C_MXL_NoDR_V3_SimulatedMeans_Wide.csv  # [output of 08] simulated WTP
└── OtherOutput/
    ├── Figures/
    │   ├── D2_Figure2_WTPClasses.jpg                              # [output of 10] Figure 2
    │   ├── D2_Figure3_WellbeingDistributions.jpg                  # [output of 11] Figure 3
    │   ├── D2_Figure4_WellbeingWTP.jpg                            # [output of 12] Figure 4
    │   ├── D2_FigureB1_CEDebrief.jpg                              # [output of 13] Figure B1
    │   └── D2_FigureC1_WTPClassesDistribution.jpg                 # [output of 15] Figure C1
    └── Tables/
        ├── TableB2_SampleVsQuota.txt                              # [output of 14] Table B2
        ├── TableC1_SimulatedMeanWTP.csv                           # [output of 17] Table C1
        ├── TableC2_ModelEstimates.csv                             # [output of 18] Table C2
        ├── TableC3_WellbeingLVs.csv                               # [output of 16] Table C3
        └── D2_Codebook.xlsx                                       # [output of 19] data codebook (380 variables)
```

#### Acknowledgements:

-   Data provided by [SurveyEngine](https://surveyengine.com/).
-   Using RStudio and [Apollo](https://apollochoicemodelling.com/) (Hess and Palma, 2019).
-   This work was funded by the Natural Environment Research Council (NERC) through the [DRUID](https://druidproject.org.uk/) project.
-   Ethical approval for data collection was granted by the School of Earth and Environment Ethics Committee, University of Leeds (Ref: BESS+ FREC 2023-0769-1031).
-   Acknowledgement: Using specialist and High-Performance Computing systems provided by Information Services at the University of Kent, and by the Research Computing Team at the University of Leeds.
