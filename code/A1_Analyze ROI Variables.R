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
library(FinCal)

#### (0) SETUP ----------------------------------------------------------------

# Define constants
r = 0.02
FEDRATE = 0.0499

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
KEYVARS <- c("UNITID", "OPEID6", "INSTNM", "STABBR", "ST_FIPS",
             "MD_EARN_WNE_P6", "MD_EARN_WNE_P8", "MD_EARN_WNE_P10",
             )

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

#### (2) SUMMARIZE MISSING VARIABLES BY FILE ----------------------------------

# Count obs in each data file
filecounts <- raw_combination %>% group_by(file_name) %>% count()

# Find which files have missing data
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

fwrite(missing_data_summary, "output/missing_variables_summary.csv")

