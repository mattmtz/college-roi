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

#### (0) SETUP ----------------------------------------------------------------

# List years of interest
KEYYRS <- c(2003,2005,2007,2009,2011:2014,2018,2019)

# List key files
KEYFILES <- c(paste0("MERGED",KEYYRS,"_", 
                     str_sub(as.character(KEYYRS+1),-2),"_PP.csv"),
              "Most-Recent-Cohorts-Institution.csv")

# Key variables
KEYVARS <- c(paste0("md_earn_wne_p",c(6,8,10)))

# Cohort crosswalk
cohorts <- fread("input/cohort_crosswalk.csv")

# Inflation data
cps <- read.xlsx("input/r-cpi-u-rs-alllessfe.xlsx", sheet = 1,
                 startRow = 6) %>%
  clean_names() %>%
  select(year, avg) %>%
  rename(cps_yr = year,
         cps_deflator = avg)

#### (1) KEY VARIABLE EXTRACTION ----------------------------------------------

# Container dataset
allyrs <- data.frame()

# Function for processing individual datasets
CLN_YRS <- function(x) {
  
  # Download file
  individ_yr <- fread(paste0("../raw_data/",KEYFILES[x])) %>%
    clean_names() %>%
    select(unitid, opeid, opeid6, preddeg, control, st_fips,
           ccugprof, ccsizset, ug, costt4_a, costt4_p, all_of(KEYVARS)) %>%
    # Add years & file name
    mutate(year = paste0(KEYYRS[x],"_",str_sub(as.character(KEYYRS[x]+1),-2)),
           file_name = KEYFILES[x]) %>%
    # Clean earnings variable
    mutate(across(all_of(KEYVARS), as.numeric)) %>%
    # Set all other variables to character
    mutate(across(!all_of(KEYVARS), as.character))
  
  # Add to overall dataset
  allyrs <- bind_rows(allyrs, individ_yr)
  
  return(allyrs)
}

# Apply function
combined_data <- lapply(1:length(KEYFILES), CLN_YRS) %>%
  bind_rows() %>%
  # clean year and add cohorts
  mutate(year = ifelse(year == "NA_NA", "2019_20", year))

rm(allyrs)

#### (2) CREATE DATASET -------------------------------------------------------

# Identify cohorts
full_data <- combined_data %>%
  left_join(cohorts) %>%
  mutate(across(c(p6_cohort, p8_cohort, p10_cohort), 
                ~factor(.x, 
                        levels = c("96-97/97-98", "98-99/99-00", "00-01/01-02",
                                   "01-02/02-03", "02-03/03-04", "03-04/04-05",
                                   "04-05/05-06", "05-06/06-07", "06-07/07-08",
                                   "07-08/08-09", "08-09/09-10", "09-10/10-11",
                                   "10-11/11-12", "11-12/12-13", "12-13/13-14"))))

# 2023 CPS avg
Y2023 <- cps[nrow(cps),2]

# Merge into full dataset
full_data_cps <- full_data %>%
  mutate(cps_yr = case_when(
    year == "2012_13" ~ 2015,
    year == "2013_14" ~ 2016,
    year == "2014_15" ~ 2017,
    year == "2018_19" ~ 2020,
    year == "2019_20" ~ 2021,
    T                 ~ 2014)) %>%
  left_join(cps) %>%
  mutate(across(all_of(KEYVARS), ~.x*Y2023/cps_deflator,
                .names = "{.col}_23_dollars"))

# export data
fwrite(full_data_cps, "../intermediate/combined_scorecard_data.csv")

# clean environment
rm(combined_data, full_data); gc()



#### (4) MEDIAN EARNINGS BY COHORT --------------------------------------------

# Create list of unique cohorts
cohort_list_int <- full_data_cps %>%
  select(year, ends_with("_cohort")) %>%
  pivot_longer(!year, names_to = "drop", values_to = "cohort") %>%
  filter(cohort!= "")

cohort_list = unique(cohort_list_int$cohort)

# Create data for each cohort
COHORT <- function(x) {
  
  indiv_cohort <- full_data_cps %>%
    filter(p6_cohort == x | p8_cohort == x | p10_cohort == x) %>%
    mutate(cohort = x,
           cohort_earnings = case_when(
             p6_cohort == x  ~ md_earn_wne_p6_23_dollars,
             p8_cohort == x  ~ md_earn_wne_p8_23_dollars,
             p10_cohort == x ~ md_earn_wne_p10_23_dollars)) %>%
    mutate(cohort_yr = case_when(
      p6_cohort == cohort  ~ "P6",
      p8_cohort == cohort  ~ "P8",
      p10_cohort == cohort ~ "P10")) %>%
    select(year, cohort, cohort_yr, cohort_earnings) %>%
    filter(!is.na(cohort_earnings))
  
  return(indiv_cohort)
}

# Create cohort data
fig2 <- lapply(cohort_list, COHORT) %>% bind_rows() %>%
  group_by(year, cohort, cohort_yr) %>%
  summarize(earnings = median(cohort_earnings)/1000) %>%
  ungroup() %>%
  mutate(cohort = 
           factor(cohort,
                  levels = c("96-97/97-98", "98-99/99-00", "00-01/01-02",
                             "01-02/02-03", "02-03/03-04", "03-04/04-05",
                             "04-05/05-06", "05-06/06-07", "06-07/07-08",
                             "07-08/08-09", "08-09/09-10", "09-10/10-11",
                             "10-11/11-12", "11-12/12-13", "12-13/13-14"))) %>%
  group_by(cohort) %>%
  mutate(count = n()) %>%
  ungroup()

# Graph data
ggplot(fig2 %>% filter(count>1), aes(x = year, y = earnings,
                                     group = cohort, color = cohort)) +
  geom_point(size= 2.5) +
  geom_text(aes(x = year, y = earnings, label = cohort_yr), 
            vjust =-0.5, hjust = 0.85, show.legend = F) +
  guides(color = guide_legend(nrow =2, title = "Cohort:"))+
  geom_line(linewidth = 1.2) +
  GraphTheme +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 11),
        legend.title=element_text(size=14),
        axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1)) +
  labs(color = "Cohort: ",
       x = "College Scorecard Survey Vintage",
       y = "Medians of Median Earnings ($, 2023)") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(25, 52),
                     labels = scales::dollar_format(prefix = "$", 
                                                    suffix = "K"))

# Save output
ggsave("output/earnings_by_cohort.png", 
       width = 3750, height = 2750, units = "px")