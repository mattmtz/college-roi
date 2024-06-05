###---------------------------------------------------------------------------#
### Project : COLLEGE SCORECARD ROI
### Purpose : EARNINGS BY COHORT
### Author  : MATT MARTINEZ
### Audited : NO
###---------------------------------------------------------------------------#

rm(list = ls())
options(scipen = 999)

# Set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")

# Load packages
library(data.table)
library(tidyverse)
library(magrittr)
library(lubridate)
library(stringr)
library(janitor)
library(openxlsx)

# Define Graph theme
GraphTheme = theme_classic() + 
  theme(
    axis.line = element_line(linewidth = 1, colour = "black"),
    panel.grid.major.y = element_line(linewidth = 0.4, color = "grey89"),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 13),
    axis.text.y = element_text(size = 12, color = "black"),
    #legend.title = element_blank(),
    legend.key.size = unit(0.8, 'cm'),
    legend.background = element_blank())

# Read in data
graphdat <- fread("../intermediate/full_data.csv")

#### (1) MEDIAN EARNINGS BY SURVEY YEAR ---------------------------------------

# Set up data
fig1 <- full_data_cps %>%
  group_by(year) %>%
  summarize('6-Year' = median(md_earn_wne_p6_23_dollars, na.rm=T)/1000,
            '8-Year' = median(md_earn_wne_p8_23_dollars, na.rm = T)/1000,
            '10-Year'= median(md_earn_wne_p10_23_dollars, na.rm = T)/1000) %>%
  ungroup() %>%
  pivot_longer(!year, names_to = "cohort", values_to = "earnings") %>%
  mutate(cohort = factor(cohort, levels = c("6-Year", "8-Year", "10-Year"))) %>%
  filter(!is.na(earnings))

# Create plot
ggplot(fig1, aes(x = year, y = earnings, group = cohort, color = cohort)) +
  geom_point(size= 2.5) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c('cornflowerblue', 'darkorange1', 
                                'forestgreen'),
                     breaks = c("10-Year", "8-Year", "6-Year")) +
  GraphTheme +
  theme(legend.position = c(0.8,0.83),
        legend.text = element_text(size = 13),
        legend.title= element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1)) +
  labs(color = "Years After Enrollment",
       x = "College Scorecard Survey Vintage",
       y = "Medians of Median Earnings ($, 2023)") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(25, 52),
                     labels = scales::dollar_format(prefix = "$", 
                                                    suffix = "K"))
# Save output
ggsave("output/earnings_by_survey_yr.png", 
       width = 3500, height = 2750, units = "px")