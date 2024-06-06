###---------------------------------------------------------------------------#
### Project : COLLEGE SCORECARD ROI
### Purpose : CREATE DATASET 
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

# Identify years with earnings data
KEYYRS <- c(2003,2005,2007,2009,2011:2014,2018,2019)

# School characteristics - only available in most recent datafile
IDVARS <- c("UNITID", "OPEID6", "INSTNM", "CCBASIC", "LOCALE", "CCUGPROF", 
            "CCSIZSET", "MENONLY", "WOMENONLY", "RELAFFIL", "HBCU", "PBI", 
            "ANNHI", "TRIBAL", "AANAPII", "HSI", "NANTI")

# Key variables for analysis
KEYVARS <- c("UNITID", "OPEID", "OPEID6", "INSTNM", "CONTROL", "STABBR", 
             "REGION", "PREDDEG", "ICLEVEL", "ADM_RATE", "OPENADMP", "UGDS", 
             "NUMBRANCH", "MD_EARN_WNE_P6", "MD_EARN_WNE_P8", 
             "MD_EARN_WNE_P10", "NPT4_PUB", "NPT4_PRIV", "NPT4_PROG",
             "NPT4_OTHER", "DEBT_MDN")

# List variables to convert to strings
CHARVARS <- c("UNITID", "OPEID", "OPEID6", "INSTNM", "STABBR")

# NOTE: KEYVARS only includes variables with data in all years, except:
# ST_FIPS, NUMBRANCH, UGDS_MEN, UGDS_WOMEN, PCT_PELL

# List key files
KEYFILES <- c(paste0("MERGED",KEYYRS,"_", 
                     str_sub(as.character(KEYYRS+1),-2),"_PP.csv"),
              "Most-Recent-Cohorts-Institution.csv")

# Inflation data
cps <- read.xlsx("input/r-cpi-u-rs-alllessfe.xlsx", sheet = 1,
                 startRow = 6) %>%
  clean_names() %>%
  select(year, avg) %>%
  rename(cps_yr = year,
         cps_deflator = avg)

# Cohort crosswalk
cohorts <- fread("input/cohort_xwalk.csv")

# Load ccbasic crosswalk
xwalk <- fread("input/ccbasic_xwalk.csv") %>%
  mutate(across(everything(), as.character))

#### (1) COMBINE INDIVIDUAL FILES ---------------------------------------------

# Function for processing individual datasets
allyrs <- data.frame()
CLN_YRS <- function(x) {
  
  # Download file
  individ_yr <- fread(paste0("../raw_data/",KEYFILES[x])) %>%
    select(any_of(KEYVARS)) %>%
    mutate(across(c(CHARVARS), as.character)) %>%
    mutate(across(!c(CHARVARS), as.numeric)) %>%
    clean_names() %>%
    # Add years & file name
    mutate(year = paste0(KEYYRS[x],"_",str_sub(as.character(KEYYRS[x]+1),-2)),
           file_name = KEYFILES[x])
  
  # Add to overall dataset
  allyrs <- bind_rows(allyrs, individ_yr)
  
  return(allyrs)
}

allyrs <- lapply(1:length(KEYFILES), CLN_YRS) %>% bind_rows()

# Get university characteristics
desc <- fread(paste0("../raw_data/",KEYFILES[length(KEYFILES)])) %>%
  select(all_of(IDVARS)) %>%
  mutate(across(any_of(c(CHARVARS)), as.character)) %>%
  clean_names()

#### (2) CLEAN DATASET --------------------------------------------------------

# Create usable data
filtered_df <- allyrs %>%
  rename(p6 = md_earn_wne_p6,
         p8 = md_earn_wne_p8,
         p10= md_earn_wne_p10) %>%
  mutate(unusable_earn = ifelse(is.na(p6) | is.na(p8) | is.na(p10), 1, 0),
         unusable_cost = ifelse(is.na(npt4_pub) & is.na(npt4_priv) &
                                  is.na(npt4_other), 1, 0)) %>%
  filter(unusable_earn == 0 & unusable_cost == 0) %>%
  select(-starts_with("unusable")) %>%
  left_join(desc)

# De-code relevant variables
decoded_data <- filtered_df %>%
  mutate(
    iclevel = factor(iclevel, levels = c(1,2,3),
                     labels=c("4-year","2-year","Less than 2-year")),
    control = factor(control, levels = c(1,2,3), 
                     labels=c("Public","Private nonprofit",
                              "Private for-profit")),
    preddeg = factor(preddeg, levels = seq(0,4), 
                     labels = c("Not classified", "Certificate", "Associate's",
                                "Bachelor's", "Graduate")),
    region = factor(region, levels = seq(0,9),
                    labels = c("U.S. Service Schools", 
                               "New England (CT, ME, MA, NH, RI, VT)", 
                               "Mid East (DE, DC, MD, NJ, NY, PA)", 
                               "Great Lakes (IL, IN, MI, OH, WI)",
                               "Plains (IA, KS, MN, MO, NE, ND, SD)",
                               "Southeast (AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA, WV)",
                               "Southwest (AZ, NM, OK, TX)",
                               "Rocky Mountains (CO, ID, MT, UT, WY)",
                               "Far West (AK, CA, HI, NV, OR, WA)",
                               "Outlying Areas (AS, FM, GU, MH, MP, PR, PW, VI)"))) %>%
  droplevels()

# Create enrollment categories, create flag for institutions reporting together
# & drop graduate institutions
cln_decoded_data <- decoded_data %>%
  mutate(ugdssize = forcats::fct_na_value_to_level(
    cut(ugds, breaks = c(0,250,500,1e7), 
        labels=c("<=250","251-500",">500"), include.lowest=TRUE)),
    report_as_group = ifelse(numbranch>1,1,0)) %>%
  filter(!preddeg %in% c(0,4))

# De-duplicate
deduped_data <- cln_decoded_data %>%
  distinct(instnm,stabbr, .keep_all = T)

# Add in characteristics
full_dat <- deduped_data %>%
  left_join(xwalk) %>%
  select(-ccbasic) %>%
  rename(ccbasic = cln_ccbasic)
