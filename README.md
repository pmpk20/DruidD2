[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20799844.svg)](https://doi.org/10.5281/zenodo.20799844)
------------------------------------------------------------------------

#### Title: *Heterogeneous Preferences for Use and Non-use Values Provided by Insects: supporting dataset*

#### Keywords: Bequest, Choice Modelling, Cultural Ecosystem Services, Encountering, Existence, Insects, Non-use Values.

#### Paper authors: Peter King (Leeds), Tom Breeze (Reading), Theresa Robinson (JNCC), Martin Dallimer (Imperial). Correspondence: p.king1\@leeds.ac.uk

#### Status: Published in ***People and Nature*** --- doi: [forthcoming](forthcoming)

#### Last change: 02/07/2026

#### Acknowledgements:  
-   This work was supported by the Natural Environment Research Council Grant/Award Number: NE/V006916/1 This work is part of task D2 of work package D for the DRUID project: <https://environment.leeds.ac.uk/geography-research-river-basin-processes-management/dir-record/research-projects/1656/drivers-and-repercussions-of-uk-insect-declines-druid>
-   This work was partly undertaken on the Aire HPC system at the University of Leeds, UK and using specialist and High-Performance computing systems provided by Information Services at the University of Kent. 
-   Data provided by [SurveyEngine](https://surveyengine.com/).
-   Using RStudio and [Apollo](https://apollochoicemodelling.com/) (Hess and Palma, 2019).
-   Ethical approval for data collection was granted by the School of Earth and Environment Ethics Committee, University of Leeds (Ref: BESS+ FREC 2023-0769-1031).

#### Description:
-   `/CEOutput/Main/LCM/`: Outputs of just the modelling used in-text. If you want to know about specification search just ask!
-   `/OtherOutput/`: All tables + figures in the manuscript.
-   `/Scripts/`: All cleaning, modelling, and reporting R scripts.
-   `/Data/Main/` contains anonymised survey data for N = 1,684 online UK respondents from October 2024. The anonymised input to the pipeline is `Data_Covariates_Step0.csv`. See the commented block at the top of `01_Druid_Setup_CleaningMain.R` for details of what was withheld and why.
-   `/Survey/` has the full survey reproduced and explained alongside pictures used therein, and the codebook explaining each variable and coding.
-   Run `00_D2_Replicator.R` to execute all replication scripts in order. Scripts are numbered 01--19 by execution sequence.
-   Data are publicly available in this repository. Zenodo DOI: [https://doi.org/10.5281/zenodo.20799845](https://doi.org/10.5281/zenodo.20799844)

  #### Requirements:
  - R version 4.5.0 or later
  - Key packages: apollo (0.3.5), data.table (1.17.2), tidyverse (2.0.0),
    ggplot2 (3.5.2), ggdist (3.3.2), writexl, readxl, here, janitor, psych
  - See 00_D2_Replicator.R for the full sessionInfo()

------------------------------------------------------------------------

## Interactive Shiny App

**Live app: <[https://pmpk20.shinyapps.io/biowell4/](https://pmpk20.shinyapps.io/biowell4/)/>**

The app lets readers interactively explore how their wellbeing from the encountering, existence, and bequest values from bees, beetles, and wasps compares to our sample. Try it to find out whether you are in our 'pro-insect', 'insect-averse', or 'ambivalent' latent classes!

------------------------------------------------------------------------
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
│   ├── Figure1_ChoiceCard.png                                         # Figure 1 (static image, not script-generated)
│   ├── Figure1_FinalCE.html                                           # interactive CE choice card (full survey version)
│   ├── Figure1_FinalCE.png                                            # static export of the above
│   ├── CE_Explainer_A1.jpg ... CE_Explainer_Plan.jpg                  # choice experiment explainer slides (7 images)
│   ├── D2_V2.codeplan.xlsx                                            # SurveyEngine codeplan (question text source for script 19)
│   └── D2_Codebook.xlsx                                               # [output of 19] data codebook (380 variables)
├── CEOutput/
│   └── Main/
│       └── LCM/
│           ├── D2_Truncated_LC_3C_MXL_NoDR_V3_model.rds          # [output of 07] fitted model object
│           ├── D2_Truncated_LC_3C_MXL_NoDR_V3_model_PiValues.rds # [output of 07] posterior class probabilities
│           ├── D2_Truncated_LC_3C_MXL_NoDR_V3_estimates.csv       # [output of 07] parameter estimates
│           ├── D2_Truncated_LC_3C_MXL_NoDR_V3_output.txt          # [output of 07] full model output
│           ├── D2_Truncated_LC_3C_MXL_NoDR_V3_additional_output.txt  # [output of 07] Apollo session report
│           └── D2_Truncated_LC_3C_MXL_NoDR_V3_SimulatedMeans_Wide.csv  # [output of 08] simulated WTP
└── OtherOutput/
    ├── Figures/
    │   ├── D2_Figure2_WTPClasses.jpg                              # [output of 10] Figure 2
    │   ├── D2_Figure3_WellbeingDistributions.jpg                  # [output of 11] Figure 3
    │   ├── D2_Figure4_WellbeingWTP.jpg                            # [output of 12] Figure 4
    │   ├── D2_FigureB1_CEDebrief.jpg                              # [output of 13] Figure B1
    │   └── D2_FigureC1_WTPClassesDistribution.jpg                 # [output of 15] Figure C1
    └── Tables/
        ├── Table1.txt                                             # [output of 09] Table 1 (WTP estimates)
        ├── Table1_ClassAllocation.txt                             # [output of 09] class allocation counts
        ├── TableB2_SampleVsQuota.txt                              # [output of 14] Table B2
        ├── TableC1_SimulatedMeanWTP.csv                           # [output of 17] Table C1
        ├── TableC2_ModelEstimates.csv                             # [output of 18] Table C2
        └── TableC3_WellbeingLVs.csv                               # [output of 16] Table C3
```
