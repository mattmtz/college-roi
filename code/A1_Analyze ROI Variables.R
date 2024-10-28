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

# Identify variables by group
IDVARS <- c("UNITID", "OPEID", "OPEID6", "INSTNM", "CONTROL", "STABBR",
            "ST_FIPS", "REGION", "PREDDEG", "CCBASIC","LOCALE", 
            "ICLEVEL", "CCUGPROF", "CCSIZSET", "MENONLY", "WOMENONLY", 
            "AGE_ENTRY", "OPENADMP", "RELAFFIL","NUMBRANCH", "UGDS",
            "HBCU", "PBI", "ANNHI", "TRIBAL", "AANAPII", "HSI", "NANTI", 
            "ADM_RATE", "MAIN", "PCTPELL", "UGDS_MEN", "UGDS_WOMEN", "PCTFLOAN")

PRICEVARS <- c("NPT4_PUB", "NPT4_PRIV", "NPT4_PROG", "NPT4_OTHER", "DEBT_MDN",
              "RPY_7YR_RT")

EARNVARS <- c("MD_EARN_WNE_P6", "MD_EARN_WNE_P8", "MD_EARN_WNE_P10",
              "MD_EARN_WNE_P7", "MD_EARN_WNE_P9", "MD_EARN_WNE_P11",
              "GT_THRESHOLD_P10")

GRADVARS <- c("C150_L4_POOLED_SUPP", "C150_4_POOLED_SUPP", "C150_L4", "C150_4")

STEMVARS <- c(paste0(rep("PCIP", 7), c("01","03","04","11","14","15","26",
                                       "27", "40", "41", "42")))

# Identify min. required variables for analysis
KEYVARS <- c("UNITID", "OPEID6", "INSTNM", "STABBR", "PREDDEG", EARNVARS[1:6],
             PRICEVARS[1:4])

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
    rename(p6 = md_earn_wne_p6, p7 = md_earn_wne_p7, p8 = md_earn_wne_p8,
           p9 = md_earn_wne_p9, p10= md_earn_wne_p10, p11 = md_earn_wne_p11)
  
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
    mutate(usable_earn1 = ifelse(p6 + p8 + p10 != 0, 0, 1),
           usable_earn2 = ifelse(p7 + p9 + p11 != 0, 0, 1),
           usable_debt = ifelse(npt4_pub + npt4_priv + npt4_prog + npt4_other
                                == 4, 0, 1)) %>%
    mutate(usable_earn = ifelse(usable_earn1 + usable_earn2 == 0, 0, 1)) %>%
    mutate(usable_file = ifelse(usable_earn + usable_debt < 2, 0, 1)) %>%
    select(file_name, usable_file)
  
  filedecisions <- bind_rows(filedecisions, file_decision)
  return(filedecisions)
}

KEYFILES <- lapply(1:length(ALLFILES), FILECHECK) %>% bind_rows() %>%
  filter(usable_file == 1)

KEYFILES <- KEYFILES$file_name

# Output list of key files
keyfiles_out <- data.frame("file_name" = KEYFILES)
fwrite(keyfiles_out, "intermediate/relevant_files.csv")

#### (2) READ IN KEY FILES ----------------------------------------------------

# Function for processing individual datasets
allyrs <- data.frame()

CLN_YRS <- function(x) {
  
  # Download file
  individ_yr <- fread(paste0("../raw_data/",KEYFILES[x])) %>%
    select(any_of(c(IDVARS, PRICEVARS, EARNVARS, GRADVARS)), 
           starts_with("PCIP")) %>%
    clean_names() %>%
    rename(p6 = md_earn_wne_p6, p7 = md_earn_wne_p7, p8 = md_earn_wne_p8,
           p9 = md_earn_wne_p9, p10= md_earn_wne_p10, p11= md_earn_wne_p11) %>%
    # Add years & file name
    mutate(year = str_extract(KEYFILES[x], "[:digit:]+_[:digit:]+"),
           file_name = KEYFILES[x]) %>%
    mutate(across(everything(), as.character))
  
  # Add to overall dataset
  allyrs <- bind_rows(allyrs, individ_yr)
  
  return(allyrs)
}

# Select relevant variables
raw_combination <- lapply(1:length(KEYFILES), CLN_YRS) %>%
  bind_rows() %>%
  mutate(year = ifelse(is.na(year), "most_recent", year))

ALLVARS <- names(raw_combination)[1:(length(names(raw_combination))-2)]

#### (3) SUMMARIZE UNUSABLE DATA BY FILE --------------------------------------

# Filter data to non-missing ROI observations
filtered_data <- raw_combination %>%
  mutate(unusable_earn1 = ifelse(is.na(p6) | is.na(p8) | is.na(p10), 1, 0),
         unusable_earn2 = ifelse(is.na(p7) | is.na(p9) | is.na(p11), 1, 0),
         unusable_cost = ifelse(is.na(npt4_pub) & is.na(npt4_priv) &
                                  is.na(npt4_other), 1, 0)) %>%
  mutate(unusable_earn = ifelse(unusable_earn1 + unusable_earn2 == 2, 1, 0)) %>%
  mutate(ovl_unusable = ifelse(unusable_earn + unusable_cost == 0, 
                               "usable", "unusable"))

# Count obs in each data file
filecounts <- filtered_data %>%
  group_by(file_name, ovl_unusable) %>% 
  count() %>%
  ungroup() %>%
  pivot_wider(id_cols = file_name, 
              names_from = ovl_unusable, values_from = n) %>%
  mutate(usable = ifelse(is.na(usable), 0, usable)) %>%
  mutate(n = usable + unusable)

# Count missing obs in each data file
missings <- filtered_data %>%
  group_by(file_name) %>%
  filter(ovl_unusable == "usable") %>%
  summarise(across(all_of(ALLVARS),  ~sum(is.na(.)))) %>%
  ungroup() %>%
  left_join(filecounts) %>%
  select(file_name, n, usable, unusable, everything())

# Find % of variables missing each year
pct_missings <- missings %>%
  mutate(across(any_of(ALLVARS), ~ ifelse(.x/usable == 0 | .x/usable == 1, 
                                          .x/usable,
                                          format(round(.x/usable, 3), 
                                                 nsmall = 2)))) %>%
  select(-n)

# Find which files have missing variables
missingflags <- missings %>%
  mutate(across(ALLVARS[1]:ALLVARS[length(ALLVARS)],
                ~ ifelse(usable-.x == 0, 1, 0)))

# Export missing variable summary
missing_int <- missingflags %>%
  mutate(across(ALLVARS[1]:ALLVARS[length(ALLVARS)],
                ~ ifelse(.x == 1, "NO DATA", ""))) %>%
  select(-c(file_name, n, usable, unusable))

missing_data_summary <- as.data.frame(t(missing_int)) %>%
  rownames_to_column()
names(missing_data_summary) <- c("variable", KEYFILES)

# Check for full PCIP measures
pcip_dat <- filtered_data %>%
  filter(ovl_unusable == "usable") %>%
  select(file_name, starts_with("pcip")) %>%
  mutate(across(starts_with("pcip"), as.numeric)) %>%
  rowwise() %>%
  mutate(tot_pcip = sum(across(starts_with("pcip")), na.rm = T)) %>%
  ungroup() %>%
  select(-starts_with("pcip")) %>%
  group_by(file_name) %>%
  summarize(mn_pcp_fill = mean(tot_pcip)) %>%
  ungroup()

# Export findings
fwrite(pct_missings, paste0("output/missing_data_summaries/",
                            "pct_missing_observations_by_variable.csv"))
fwrite(missing_data_summary, paste0("output/missing_data_summaries/",
                                    "missing_variables_summary.csv"))
fwrite(filecounts %>% mutate(share_unusable = unusable / n),
       paste0("output/missing_data_summaries/",
              "missing_earnings_or_price_summary.csv"))

fwrite(pcip_dat, paste0("output/missing_data_summaries/",
                        "missing_pcip_summary.csv"))

# Clean workspace
rm(missingflags, missing_int, missing_data_summary); gc()

#### (4) DUPLICATION/NAMING ISSUES --------------------------------------------

# Remove unusable observations
test_dat <- filtered_data %>% filter(ovl_unusable == "usable")

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
test_dat %>%
  select(unitid, instnm) %>%
  distinct() %>%
  group_by(unitid) %>%
  count(instnm) %>%
  ungroup() %>%
  filter(n>1)
