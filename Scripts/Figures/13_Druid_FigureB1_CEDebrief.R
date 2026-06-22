#### D2: Debriefing Descriptives  ###############
# Script author: Peter King (p.king1@leeds.ac.uk)
# Last Edited: 12/02/2026
# COMMENTS: CE debriefing scale by measure + class



# **********************************************************************************
#### Section 0: Setup environment and replication information ####
# **********************************************************************************


# **********************************************************************************
#### Replication Information:

# here() = "C:/Users/earpkin/OneDrive - University of Leeds/DRUID/D2/Analysis/D2Backup"
# version  R version 4.5.0 (2025-04-11 ucrt)
# os       Windows 11 x64 (build 22631)
# See 00_D2_Replicator.R for full package list


# **********************************************************************************
#### Libraries:
options(scipen=90)
library(tidyverse)
library(dplyr)
library(magrittr)
library(mded)
library(here)
library(data.table)
library(Rfast)
library(matrixStats)
library(ggplot2)
library(ggridges)
library(ggdist)
library(RColorBrewer)


# **********************************************************************************
#### Section 1: Import ####
# **********************************************************************************


# Using fread() from data.table as significantly faster than read.csv()
# Store data in Data subfolder so referencing it using here() package
Data <-
  here("Data/Main", "Data_Covariates_Step6.csv") %>% fread() %>% data.frame()



## Group here
PlotData <- cbind(
  "Plan_Check" = Data$CE_Task_Plan_Check_1,
  "Insect_Check" = Data$CE_Task_Insect_Check_1,
  "A1_Check" = Data$CE_Task_A1_Check_1,
  "A2_Check" = Data$CE_Task_A2_Check_1,
  "A3_Check" = Data$CE_Task_A3_Check_1,
  "A4_Check" = Data$CE_Task_A4_Check_1,
  "A5_Check" = Data$CE_Task_A5_Check_1,
  "Class" = Data$ClassMembership
) %>%
  data.frame() %>%
  pivot_longer(cols = 1:7,
               names_to = "name",
               values_to = "value") %>% 
  mutate(
    Class = factor(Class, 
                   levels = c("1", "2", "3"),
                   labels = c("Class 1 (Pro-insect)",
                              "Class 2 (Insect-averse)", 
                              "Class 3 (Ambivalent)"))
  )


# **********************************************************************************
#### Section 2: Setup plot ####
# **********************************************************************************


# Custom mapping for y-axis labels
prefix_labels <- c(
  "Plan_Check" = "Plan A, B, C",
  "Insect_Check" = "Insect",
  "A1_Check" = "Encountering",
  "A2_Check" = "Existence",
  "A3_Check" = "Bequest",
  "A4_Check" = "Tax",
  "A5_Check" = "Choice"
)


## Specify per-insect here
custom_colors <- c(
  RColorBrewer::brewer.pal(n = 9, name = "Purples")[c(3, 5, 7)],
  RColorBrewer::brewer.pal(n = 9, name = "Greys")[c(4, 6)],
  RColorBrewer::brewer.pal(n = 9, name = "Reds")[c(6, 9)])


TextSize <- 16


TextSetup <- element_text(size = TextSize,
                          colour = "black",
                          family = "serif")


# **********************************************************************************
#### Section 3: Plot ####
# **********************************************************************************


# Plotting
FigureB1 <- 
  PlotData %>%
  ggplot(aes(
    x = value %>% as.factor(),
    y = name,
    group = name,
    fill = name
  )) +
  ggdist::stat_histinterval(outline_bars = TRUE,
                            point_interval = "mean_qi",
                            position = "dodge",
                            slab_colour = "black",
                            aes(
                              point_size = 1.5
                            )) +
  
  facet_grid( ~ Class,
             labeller = labeller(Class = label_wrap_gen(width = 15))) +
  
  # Custom x-axis labels
  scale_x_discrete(
    name = "I understand the information presented here\n1/5 (Strongly disagree) - 5/5 (strongly agree)"
  ) +
  
  # Custom y-axis labels
  scale_y_discrete(name = "Part of the CE", 
                   labels = prefix_labels) +
  
  # Adding vertical line at 50
  geom_vline(xintercept = 3, 
             alpha = 0.5) +
  
  theme_bw() + 
  
  # Custom fill colors
  scale_fill_manual(
    name = "Response", 
    values = custom_colors,
    labels = prefix_labels
  ) +
  
  scale_point_size_continuous(guide = NULL) +
  scale_slab_colour_discrete(guide = NULL) +
  # Remove black rectangle around legend and adjust layout
  theme(
    legend.position = "bottom",
    legend.text = TextSetup,
    legend.title = TextSetup,
    axis.text.x = TextSetup,
    axis.text.y = TextSetup,
    axis.title.y = TextSetup,
    strip.background = element_rect(fill = "white"),
    strip.text = TextSetup,
    legend.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank()
  )

# **********************************************************************************
#### Section 3: Plot ####
# **********************************************************************************



# Date <- gsub(pattern = "-",replacement = "_",Sys.Date())
## Save output in highest DPI
ggsave(
  FigureB1,
  device = "jpeg",
  filename = here("OtherOutput/Figures", "D2_FigureB1_CEDebrief.jpg"),
  width = 20,
  height = 25,
  units = "cm",
  dpi = 500
)


# **********************************************************************************
#### Section Y: Sliders but facet and Y axis swapped ####
# **********************************************************************************

# PlotData %>%
#   ggplot(aes(
#     x = value,
#     y = Insect,
#     group = name,
#     fill = name
#   )) +
#   ggdist::stat_histinterval(outline_bars = TRUE,
#                             position = "dodge",
#                             slab_colour = "black",
#                             aes(
#                               point_size = 1.5
#                             )) +
#   
#   # Custom x-axis labels
#   scale_x_continuous(
#     name = "Response\n(Strongly disagree - strongly agree)",
#     limits = c(0, 100),
#     breaks = seq(0, 100, 10),
#     labels = seq(0, 100, 10)
#   ) +
#   
#   # Custom y-axis labels
#   scale_y_discrete(name = "Variable", 
#                    labels = PlotData$Insect %>% unique()) +
#   
#   # Adding vertical line at 50
#   geom_vline(xintercept = 50, 
#              alpha = 0.5) +
#   
#   theme_bw() + 
#   
#   facet_grid( ~ Statement) +
#   
#   # Custom fill colors
#   scale_fill_manual(
#     name = "Response", 
#     values = c(
#       RColorBrewer::brewer.pal(n = 4, name = "Purples"),
#       RColorBrewer::brewer.pal(n = 4, name = "Greys"),
#       RColorBrewer::brewer.pal(n = 4, name = "Reds")),
#     labels = prefix_labels,
#     guide = guide_legend(nrow = 4, 
#                          ncol = 3, 
#                          reverse = TRUE)
#   ) +
#   
#   scale_point_size_continuous(guide = NULL) +
#   scale_slab_colour_discrete(guide = NULL) +
#   # Remove black rectangle around legend and adjust layout
#   theme(
#     legend.position = "bottom",
#     legend.text = TextSetup,
#     legend.title = TextSetup,
#     axis.text.x = TextSetup,
#     axis.text.y = TextSetup,
#     axis.title.y = TextSetup,
#     strip.background = element_rect(fill = "white"),
#     strip.text = TextSetup,
#     legend.background = element_blank(),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank(),
#     panel.grid.major.y = element_blank()
#   )
