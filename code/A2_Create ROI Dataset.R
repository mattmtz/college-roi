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

# School characteristics - only available in most recent datafile
IDVARS <- c("UNITID", "OPEID6", "INSTNM", "CCBASIC", "LOCALE", "CCUGPROF", 
            "CCSIZSET", "MENONLY", "WOMENONLY", "RELAFFIL", "HBCU", "PBI", 
            "ANNHI", "TRIBAL", "AANAPII", "HSI", "NANTI")

# Key variables for analysis
KEYVARS <- c("UNITID", "OPEID", "OPEID6", "INSTNM", "CONTROL", "STABBR", 
             "REGION", "PREDDEG", "ICLEVEL", "ADM_RATE", "OPENADMP", "UGDS", 
             "C150_4", "C150_L4", "MD_EARN_WNE_P6", "MD_EARN_WNE_P8", 
             "MD_EARN_WNE_P10", "NPT4_PUB", "NPT4_PRIV", "NPT4_PROG",
             "NPT4_OTHER", "DEBT_MDN")

# List variables to convert to strings
CHARVARS <- c("UNITID", "OPEID", "OPEID6", "INSTNM", "STABBR")

# NOTE: KEYVARS only includes variables with data in all years, except:
# ST_FIPS, NUMBRANCH, UGDS_MEN, UGDS_WOMEN, PCT_PELL

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
xwalk <- fread("input/ccbasic_xwalk.csv")

#### (1) FIND USABLE FILES ----------------------------------------------------

# Get list of relevant files
ALLFILES <- c(list.files(path = "../raw_data/", pattern = "^(MERGED)"),
              "Most-Recent-Cohorts-Institution.csv")

# Select files with ROI variables defined
filedecisions <- data.frame()

FILECHECK <- function(x) {
  
  # Download file
  individ_file <- fread(paste0("../raw_data/", ALLFILES[x])) %>%
    mutate(file_name = ALLFILES[x]) %>%
    select(file_name, any_of(KEYVARS[6:length(KEYVARS)])) %>%
    clean_names() %>%
    rename(p6 = md_earn_wne_p6,
           p8 = md_earn_wne_p8,
           p10= md_earn_wne_p10)
  
  # Count total observations
  obscount <- individ_file %>% group_by(file_name) %>% count() %>% ungroup()
  
  # Find number of missing observations
  missings <- individ_file %>%
    group_by(file_name) %>%
    summarize(across(everything(), ~sum(is.na(.)))) %>%
    ungroup() %>%
    mutate(across(where(is.numeric), ~ifelse(obscount$n - .x == 0, 1, 0)))
  
  # Decide whether file is usable or not
  file_decision <- missings %>%
    mutate(usable_earn = ifelse(p6 + p8 + p10 != 0, 0, 1),
           usable_debt = ifelse(npt4_pub + npt4_priv + npt4_prog + npt4_other
                                == 4, 0, 1)) %>%
    mutate(usable_file = ifelse(usable_earn + usable_debt < 2, 0, 1)) %>%
    select(file_name, usable_file)
  
  filedecisions <- bind_rows(filedecisions, file_decision)
  return(filedecisions)
}

KEYFILES <- lapply(1:length(ALLFILES), FILECHECK) %>% bind_rows() %>%
  filter(usable_file == 1)

KEYFILES <- KEYFILES$file_name
rm(filedecisions, ALLFILES); gc()

#### (2) COMBINE USABLE FILES -------------------------------------------------

# Function for processing individual datasets
allyrs <- data.frame()
CLN_YRS <- function(x) {
  
  # Download file
  individ_yr <- fread(paste0("../raw_data/",KEYFILES[x])) %>%
    select(any_of(KEYVARS)) %>%
    mutate(across(all_of(CHARVARS), as.character)) %>%
    mutate(across(!all_of(CHARVARS), as.numeric)) %>%
    clean_names() %>%
    # Add years & file name
    mutate(year = str_extract(KEYFILES[x], "[:digit:]+_[:digit:]"),
           file_name = KEYFILES[x])
  
  # Add to overall dataset
  allyrs <- bind_rows(allyrs, individ_yr)
  
  return(allyrs)
}

allyrs <- lapply(1:length(KEYFILES), CLN_YRS) %>% bind_rows()

# Get university characteristics from latest data file
desc <- fread(paste0("../raw_data/",KEYFILES[length(KEYFILES)])) %>%
  select(all_of(IDVARS)) %>%
  mutate(across(any_of(c(CHARVARS)), as.character)) %>%
  clean_names()

#### (3) CLEAN DATASET --------------------------------------------------------

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
    adm_rate = ifelse(is.na(adm_rate) & openadmp == "Yes", 1, adm_rate),
    grad_rate = ifelse(is.na(c150_l4), c150_4, c150_l4),
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

# Create enrollment categories, drop graduate institutions, & clean
# graduation rate
cln_decoded_data <- decoded_data %>%
  mutate(ugdssize = forcats::fct_na_value_to_level(
    cut(ugds, breaks = c(0,250,500,1e7), 
        labels=c("<=250","251-500",">500"), include.lowest=TRUE))) %>%
  filter(!preddeg %in% c(0,4))

# De-duplicate
deduped_data <- cln_decoded_data %>%
  distinct(instnm,stabbr, file_name, .keep_all = T)

# Add in crosswalks
full_dat <- deduped_data %>%
  left_join(cohorts) %>%
  left_join(xwalk) %>%
  select(-ccbasic) %>%
  rename(ccbasic = ccbasic_decode)

#### (4) CREATE ROI DATASET ---------------------------------------------------

roi_data <- full_dat %>%
  # create cost measure
  rowwise() %>%
  mutate(netprice = mean(c(npt4_pub, npt4_priv, npt4_prog, npt4_other),
                         na.rm = T)) %>%
  ungroup() %>%
  filter(netprice >=0) %>%
  # Create earnings measures
  mutate(d8_6 = p8 - p6,
         d10_8 = p10 - p8) %>%
  mutate(avg_earn = abs((d10_8 + d8_6)/4)) %>%
  mutate(p7 = p6 + d8_6/2,
         p9 = p8 + d10_8/2) %>%
  mutate(p5 = p6 - avg_earn) %>%
  mutate(p4 = p5 - avg_earn) %>%
  mutate(p3 = p4 - avg_earn) %>%
  mutate(p2 = p3 - avg_earn) %>%
  # other items
  mutate(debt_mdn = as.numeric(debt_mdn)) %>%
  mutate(rop = p10/netprice - 1,
         debt_roi = p10/debt_mdn - 1) %>%
  # rankings
  mutate(rank_rop = min_rank(-round(rop,2)),
         rank_debt_roi = min_rank(-debt_roi),
         rank_netprice = min_rank(-round(netprice,2)),
         rank_earn = min_rank(-p10),
         rank_debt = min_rank(-debt_mdn),
         rank_grad_rate = min_rank(-grad_rate))
