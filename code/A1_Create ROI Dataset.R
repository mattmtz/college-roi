###---------------------------------------------------------------------------#
### Project : COLLEGE SCORECARD ROI
### Purpose : CALCULATE NPVS
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

# Variables that need deflating
DEFL_VARS <- c(paste0("md_earn_wne_p",c(6,8,10)), "net_price")

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
xwalk <- fread("input/ccbasic_xwalk.csv")

#### (1) COMBINE INDIVIDUAL FILES ---------------------------------------------

# Identify key variables
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

# Find variables missing in over half of files
missing_int <- missingflags %>%
  select(-c(file_name, n)) %>%
  summarize(across(everything(), ~ sum(.x)))

missing_totals <- as.data.frame(t(missing_int)) %>%
  rownames_to_column() %>%
  filter(V1 > length(KEYFILES)/2) %>%
  # NPT4_OTHER & NPT4_PROG were only used in 2011/2012
  filter(!rowname %in% c("npt4_other", "npt4_prog"))

#### (3) 

################# TO INCORPORATE LATER ########################################

#### (2) CLEAN DATASET --------------------------------------------------------

# De-code relevant variables
decoded_data <- filtered_data %>%
  left_join(xwalk) %>%
  select(-ccbasic) %>%
  rename(ccbasic = cln_ccbasic) %>%
  mutate(
    debt_mdn = as.numeric(debt_mdn),
    iclevel = factor(iclevel, levels = c(1,2,3),
                     labels=c("4-year","2-year","Less than 2-year")),
    control = factor(control, levels = c(1,2,3), 
                     labels=c("Public","Private nonprofit",
                              "Private for-profit")),
    preddeg = factor(preddeg, levels = seq(0,4), 
                     labels = c("Not classified", "Certificate", "Associate's",
                                "Bachelor's", "Graduate")),
    openadmp = case_when(
      openadmp == 1 ~ "Yes",
      openadmp == 2 ~ "No",
      T             ~ "Does not admit first time students"),
    adm_rate = ifelse(is.na(adm_rate) & openadmp == "Yes", 1, adm_rate),
    grad_rate = ifelse(is.na(c150_l4_pooled_supp), c150_4_pooled_supp,
                       c150_l4_pooled_supp),
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
  mutate(grad_rate = ifelse(grad_rate == "PS", NA, grad_rate)) %>%
  mutate(grad_rate = as.numeric(grad_rate)) %>%
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

# Remove institutions with no program data
program_data <- raw_fos %>%
  clean_names() %>%
  filter(!is.na(unitid), !is.na(opeid6)) %>%
  distinct(unitid, opeid6)

scorecard <- inner_join(deduped_data, program_data)