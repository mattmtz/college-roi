###---------------------------------------------------------------------------#
### Project : COLLEGE SCORECARD ROI
### Purpose : SUMMARIZE VARIABLES
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

# Identify years with earnings data
KEYYRS <- c(2003,2005,2007,2009,2011:2014,2018,2019)

# List key files
KEYFILES <- c(paste0("MERGED",KEYYRS,"_", 
                     str_sub(as.character(KEYYRS+1),-2),"_PP.csv"),
              "Most-Recent-Cohorts-Institution.csv")

# Identify variables by group
IDVARS <- c("UNITID", "OPEID", "OPEID6", "INSTNM", "CONTROL", "STABBR",
            "ST_FIPS", "REGION", "PREDDEG", "CCBASIC","LOCALE", 
            "ICLEVEL", "CCUGPROF", "CCSIZSET", "MENONLY", "WOMENONLY", 
            "AGE_ENTRY", "OPENADMP", "RELAFFIL","NUMBRANCH", "UGDS",
            "HBCU", "PBI", "ANNHI", "TRIBAL", "AANAPII", "HSI", "NANTI", 
            "ADM_RATE", "MAIN", "PCTPELL", "UGDS_MEN", "UGDS_WOMEN")

DEBTVARS <- c("NPT4_PUB", "NPT4_PRIV", "NPT4_PROG", "NPT4_OTHER", "DEBT_MDN",
              "RPY_7YR_RT")

EARNVARS <- c("MD_EARN_WNE_P6" ,"MD_EARN_WNE_P8", "MD_EARN_WNE_P10",
              "GT_THRESHOLD_P10")

GRADVARS <- c("C150_L4_POOLED_SUPP", "C150_4_POOLED_SUPP")

ALLVARS <- c(IDVARS, DEBTVARS, EARNVARS, GRADVARS)

# Identify min. required variables for analysis
KEYVARS <- c("UNITID", "OPEID6", "INSTNM", "STABBR", "PREDDEG", EARNVARS[1:3],
             DEBTVARS[1:4])

#### (1) COMBINE INDIVIDUAL FILES ---------------------------------------------

# Container dataset
allyrs <- data.frame()

# Function for processing individual datasets
CLN_YRS <- function(x) {
  
  # Download file
  individ_yr <- fread(paste0("../raw_data/",KEYFILES[x])) %>%
    select(any_of(ALLVARS)) %>%
    clean_names() %>%
    # Add years & file name
    mutate(year = paste0(KEYYRS[x],"_",str_sub(as.character(KEYYRS[x]+1),-2)),
           file_name = KEYFILES[x]) %>%
    mutate(across(everything(), as.character))
  
  # Add to overall dataset
  allyrs <- bind_rows(allyrs, individ_yr)
  
  return(allyrs)
}

# Select relevant variables
raw_combination <- lapply(1:length(KEYFILES), CLN_YRS) %>% bind_rows() 

#### (2) SUMMARIZE UNUSABLE DATA BY FILE --------------------------------------

# Count obs in each data file
filecounts <- raw_combination %>% group_by(file_name) %>% count()

# Find which files have missing variables
missingflags <- raw_combination %>%
  group_by(file_name) %>%
  summarise(across(tolower(ALLVARS),  ~sum(is.na(.)))) %>%
  ungroup() %>%
  left_join(filecounts) %>%
  select(file_name, n, everything()) %>%
  mutate(across(tolower(ALLVARS[1]):tolower(ALLVARS[length(ALLVARS)]),
                ~ ifelse(n-.x == 0, 1, 0)))

# Export missing variable summary
missing_int <- missingflags %>%
  mutate(across(tolower(ALLVARS[1]):tolower(ALLVARS[length(ALLVARS)]),
                ~ ifelse(.x == 1, "NO DATA", ""))) %>%
  select(-c(file_name, n))

missing_data_summary <- as.data.frame(t(missing_int)) %>%
  rownames_to_column()
names(missing_data_summary) <- c("variable", KEYFILES)

# Find number of unusable observations based on earnings/price variables
unusable <- raw_combination %>%
  select(file_name, all_of(tolower(KEYVARS))) %>%
  rename(p6 = md_earn_wne_p6,
         p8 = md_earn_wne_p8,
         p10= md_earn_wne_p10) %>%
  mutate(unusable_earn = ifelse(is.na(p6) | is.na(p8) | is.na(p10), 1, 0),
         unusable_cost = ifelse(is.na(npt4_pub) & is.na(npt4_priv) &
                                  is.na(npt4_other), 1, 0)) %>%
  mutate(ovl_unusable = ifelse(unusable_earn + unusable_cost == 0, 0, 1)) %>%
  group_by(file_name) %>%
  summarize(tot_unusable = sum(ovl_unusable)) %>%
  ungroup() %>%
  left_join(filecounts) %>%
  mutate(share_unusable = tot_unusable / n) %>%
  select(file_name, n, tot_unusable, share_unusable)

# Export findings
fwrite(missing_data_summary, paste0("output/missing_data_summaries/",
                                    "missing_variables_summary.csv"))
fwrite(unusable, paste0("output/missing_data_summaries/",
                        "missing_earnings_or_price_summary.csv"))

# Clean workspace
rm(missingflags, missing_int, missing_data_summary, unusable); gc()

#### (3) DUPLICATION/NAMING ISSUES --------------------------------------------

# Remove unusable observations
test_dat <- raw_combination %>%
  rename(p6 = md_earn_wne_p6,
         p8 = md_earn_wne_p8,
         p10= md_earn_wne_p10) %>%
  mutate(unusable_earn = ifelse(is.na(p6) | is.na(p8) | is.na(p10), 1, 0),
         unusable_cost = ifelse(is.na(npt4_pub) & is.na(npt4_priv) &
                                  is.na(npt4_other), 1, 0)) %>%
  filter(unusable_earn == 0 & unusable_cost == 0) %>%
  select(-starts_with("unusable"))

# Institution name by file_name and OPEID6
same_name <- test_dat %>%
  group_by(file_name, opeid6) %>%
  count(instnm) %>%
  ungroup() %>%
  filter(n > 1)

# 56 institutions have multiple observations with the same opeid6/name
# in the same file
test_dat %>%
  filter(instnm %in% same_name$instnm) %>%
  distinct(instnm) %>% count()

# NOTE: the "main" variable deals with this completely
test_dat %>%
  filter(instnm %in% same_name$instnm) %>%
  filter(main == 1) %>%
  group_by(file_name, opeid6) %>%
  count(instnm) %>%
  ungroup() %>%
  filter(n > 1) %>% count()

# Institutions do not change names over time
test_dat %>% group_by(opeid6) %>% 
  summarize(n = n_distinct(instnm)) %>% 
  ungroup() %>% 
  filter(n>1)
