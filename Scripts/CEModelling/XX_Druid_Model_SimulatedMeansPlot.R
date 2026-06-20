#### D2: Simulated Means Plots (exploratory — not in paper) ###############
# Function: Plot WTP as a function of wellbeing level by class, insect, and attribute
# Author: Dr Peter King (p.king1@leeds.ac.uk)
# Last Edited: 20/06/2026
# Changes:
## - Extracted from 08_Druid_Model_SimulatedMeanWTP.R
## - Not referenced in replicator; saved for future use

# **********************************************************************************
#### Section 0: Replication Information ####
# **********************************************************************************

# here() = "C:/Users/earpkin/OneDrive - University of Leeds/DRUID/D2/Analysis/D2Backup"

library(tidyverse)
library(data.table)
library(RColorBrewer)
library(here)


# **********************************************************************************
#### Section 1: Import data ####
# **********************************************************************************

all_results <- here("CEOutput/Main/LCM",
                    "D2_Truncated_LC_3C_MXL_NoDR_V3_SimulatedMeans_Wide.csv") %>%
  fread() %>% data.frame()

Classes          <- c(1, 2, 3)
Insects          <- c("Beetle", "Bee", "Wasp")
wellbeing_levels <- c(-2, -1, 0, 1, 2)

TextSize  <- 18
TextSetup <- element_text(size   = TextSize,
                          colour = "black",
                          face   = "plain",
                          family = "serif")

custom_colors <- c(
  RColorBrewer::brewer.pal(9, "Blues")[6],
  RColorBrewer::brewer.pal(9, "Reds")[6]
)


# **********************************************************************************
#### Section 2: Reshape to long format ####
# **********************************************************************************

all_results_long <- all_results %>%
  pivot_longer(
    cols      = c(Encounter_Medium, Encounter_High,
                  Existence_Medium, Existence_High,
                  Bequest_Medium,   Bequest_High),
    names_to  = "attribute_level",
    values_to = "wtp"
  ) %>%
  separate(attribute_level, into = c("attribute", "level"), sep = "_") %>%
  mutate(
    wellbeing = factor(wellbeing, levels = wellbeing_levels),
    insect    = factor(insect),
    Class     = factor(Class),
    level     = case_when(
      level == "Medium" ~ "Small",
      level == "High"   ~ "Large"
    )
  )


# **********************************************************************************
#### Section 3: V1 — Points with error bars (all wellbeing levels) ####
# **********************************************************************************

D2_SimulatedMeans_V1_Points <-
  all_results_long %>%
  mutate(Class = case_when(
    Class == 1 ~ "Class 1 (Pro-insect)",
    Class == 2 ~ "Class 2 (Insect-averse)",
    Class == 3 ~ "Class 3 (Ambivalent)"
  )) %>%
  ggplot(aes(x = wellbeing, y = wtp, colour = level, group = level)) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  facet_grid(Class ~ insect + attribute, scales = "free_y") +
  labs(x = "Wellbeing Level", y = "Willingness to Pay (WTP)", colour = "Level") +
  scale_colour_manual(values = custom_colors) +
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_vline(xintercept = 3, alpha = 0.5) +
  theme_bw() +
  theme(
    legend.position        = "bottom",
    strip.background       = element_rect(fill = "white"),
    strip.text.x           = element_text(size = TextSize, colour = "black", family = "serif", face = "bold"),
    strip.text.y           = element_text(size = TextSize, colour = "black", family = "serif", face = "bold"),
    axis.text.x            = element_text(size = TextSize, colour = "black", family = "serif", angle = 55, hjust = 1),
    legend.background      = element_blank(),
    panel.grid.major.x     = element_blank(),
    panel.grid.minor.x     = element_blank(),
    panel.grid.major.y     = element_blank(),
    panel.grid.minor.y     = element_blank(),
    axis.text.y            = TextSetup,
    axis.title.y           = TextSetup,
    axis.title.x           = TextSetup,
    legend.text            = TextSetup,
    legend.title           = TextSetup
  )

ggsave(
  D2_SimulatedMeans_V1_Points,
  device   = "jpeg",
  filename = here("OtherOutput/Figures", "D2_SimulatedMeans_V1_Points.jpg"),
  width    = 30, height = 25, units = "cm", dpi = 500
)


# **********************************************************************************
#### Section 4: V2 — Ribbon plot (wellbeing -2, 0, +2 only) ####
# **********************************************************************************

D2_SimulatedMeans_V2_Ribbon <-
  all_results_long %>%
  dplyr::filter(wellbeing %in% c(-2, 0, 2)) %>%
  group_by(Class, insect, attribute, wellbeing, level) %>%
  summarise(
    mean_wtp = mean(wtp),
    sd_wtp   = sd(wtp),
    lower    = mean_wtp - sd_wtp,
    upper    = mean_wtp + sd_wtp,
    .groups  = "drop"
  ) %>%
  mutate(Class = case_when(
    Class == 1 ~ "Class 1 (Pro-insect)",
    Class == 2 ~ "Class 2 (Insect-averse)",
    Class == 3 ~ "Class 3 (Ambivalent)"
  )) %>%
  ggplot(aes(x = wellbeing, y = mean_wtp, colour = level, fill = level, group = level)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), colour = NA, alpha = 0.25) +
  geom_line(size = 0.8) +
  geom_point(size = 2) +
  facet_grid(Class ~ insect + attribute, scales = "free_y") +
  labs(x = "Wellbeing Level", y = "Willingness to Pay (WTP)", colour = "Level", fill = "Level") +
  scale_colour_manual(values = rev(RColorBrewer::brewer.pal(9, "Blues")[c(5, 9)])) +
  geom_hline(yintercept = 0, alpha = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 2, alpha = 0.5, linetype = "dashed") +
  theme_bw() +
  theme(
    legend.position        = "bottom",
    strip.background       = element_rect(fill = "white"),
    strip.text.x           = element_text(size = TextSize, colour = "black", family = "serif", face = "bold"),
    strip.text.y           = element_text(size = TextSize, colour = "black", family = "serif", face = "bold"),
    axis.text.x            = element_text(size = TextSize, colour = "black", family = "serif", hjust = 1),
    legend.background      = element_blank(),
    panel.grid.major.x     = element_blank(),
    panel.grid.minor.x     = element_blank(),
    panel.grid.major.y     = element_blank(),
    panel.grid.minor.y     = element_blank(),
    axis.text.y            = TextSetup,
    axis.title.y           = TextSetup,
    axis.title.x           = TextSetup,
    legend.text            = TextSetup,
    legend.title           = TextSetup
  )

ggsave(
  D2_SimulatedMeans_V2_Ribbon,
  device   = "jpeg",
  filename = here("OtherOutput/Figures", "D2_SimulatedMeans_V2_Ribbon.jpg"),
  width    = 30, height = 25, units = "cm", dpi = 500
)


# End Of Script **************************************************************
