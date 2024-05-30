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

# Define constants
r = 0.02
FEDRATE = 0.0499

# Load raw data
raw_inst <- fread("input/raw_data/Most-Recent-Cohorts-Institution.csv")
raw_fos <- fread("input/raw_data/Most-Recent-Cohorts-Field-of-Study.csv")

# Load ccbasic crosswalk
xwalk <- fread("input/ccbasic_xwalk.csv")

#### (1) PREPARE DATASET  -----------------------------------------------------

# Identify key variables
IDVARS <- c("UNITID", "OPEID", "OPEID6", "INSTNM", "CONTROL", "STABBR",
            "ST_FIPS", "REGION", "PREDDEG", "CCBASIC","LOCALE", "LOCALE2", 
            "ICLEVEL", "CCUGPROF", "CCSIZSET", "MENONLY", "WOMENONLY", 
            "AGE_ENTRY", "OPENADMP", "RELAFFIL","NUMBRANCH", "UGDS", "UG", 
            "HBCU", "PBI", "ANNHI", "TRIBAL", "AANAPII", "HSI", "NANTI", 
            "ADM_RATE", "MAIN", "PCTPELL", "UGDS_MEN", "UGDS_WOMEN")

DEBTVARS <- c("NPT4_PUB", "NPT4_PRIV", "NPT4_PROG", "NPT4_OTHER", "DEBT_MDN",
              "RPY_7YR_RT")

EARNVARS <- c("MD_EARN_WNE_P6" ,"MD_EARN_WNE_P8", "MD_EARN_WNE_P10",
              "GT_THRESHOLD_P10")

GRADVARS <- c("C150_L4_POOLED_SUPP", "C150_4_POOLED_SUPP")

# Select relevant variables
filtered_data <- raw_inst %>%
  select(all_of(c(IDVARS, DEBTVARS, EARNVARS, GRADVARS))) %>%
  clean_names()

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

#### (2) CREATE NPV MEASURES --------------------------------------------------

# clean workspace
rm(cln_decoded_data, decoded_data, deduped_data, filtered_data,
   program_data, raw_fos, raw_inst, xwalk, DEBTVARS, EARNVARS,
   IDVARS, GRADVARS); gc()

# Build cost/earnings measures
npv_data <- scorecard %>%
  # create cost measure
  rowwise() %>%
  mutate(netprice = mean(c(npt4_pub, npt4_priv, npt4_prog, npt4_other),
                         na.rm = T)) %>%
  ungroup() %>%
  filter(netprice >=0 & !is.na(netprice)) %>%
  # drop records with missing data
  drop_na(c(md_earn_wne_p6, md_earn_wne_p8, md_earn_wne_p10)) %>%
  # create earnings measures
  rename(p6  = md_earn_wne_p6,
         p8  = md_earn_wne_p8,
         p10 = md_earn_wne_p10) %>%
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
  mutate(grad_rate = as.numeric(grad_rate),
         debt_mdn = as.numeric(debt_mdn)) %>%
  mutate(rop = p10/netprice - 1,
         debt_roi = p10/debt_mdn - 1) %>%
  # rankings
  mutate(rank_rop = min_rank(-round(rop,2)),
         rank_debt_roi = min_rank(-debt_roi),
         rank_netprice = min_rank(-round(netprice,2)),
         rank_earn = min_rank(-p10),
         rank_debt = min_rank(-debt_mdn),
         rank_grad_rate = min_rank(-grad_rate))

# Create NPV function
NPVFUN <- function(x) {
  
  npv_calc <- npv_data %>%
    select(unitid, opeid6, netprice, iclevel, starts_with("p")) %>%
    select(-c(pbi,pctpell)) %>%
    rowwise() %>%
    mutate(
      baNPV = npv(r=r, cf = c(rep(-netprice,5), c(p6, p7, p8, p9, p10,
                                                  rep(p10, x-10)))),
      aaNPV = npv(r=r, cf = c(rep(-netprice,3), c(p4, p5, p6, p7, p8, p9,
                                                  p10, rep(p10, x-10)))),
      crNPV = npv(r=r, cf = c(-netprice, p2, p3, p4, p5, p6, p7, p8, p9,
                              p10, rep(p10, x-10)))) %>%
    ungroup() %>%
    mutate(npv_int = case_when(
      preddeg == "Bachelor's"          ~ baNPV,
      preddeg == "Associate's"         ~ aaNPV,
      preddeg == "Certificate" & 
        iclevel == "Less than 2-year"  ~ crNPV, 
      preddeg == "Certificate" & 
        iclevel == "4-year"            ~ baNPV, 
      T                                ~ aaNPV)) %>%
    select(unitid, opeid6, npv_int)
  
  npv_data <- left_join(npv_data, npv_calc)
  return(npv_data)
}

# Calculate NPVs
npv_data <- NPVFUN(10) %>% rename(npv10 = npv_int)
npv_data <- NPVFUN(15) %>% rename(npv15 = npv_int)
npv_data <- NPVFUN(20) %>% rename(npv20 = npv_int)
npv_data <- NPVFUN(30) %>% rename(npv30 = npv_int)
npv_data <- NPVFUN(40) %>% rename(npv40 = npv_int)

# Add NPV stats
full_npv_data <- npv_data %>%
  mutate(rank_npv10 = min_rank(-npv10),
         rank_npv15 = min_rank(-npv15),
         rank_npv20 = min_rank(-npv20), 
         rank_npv30 = min_rank(-npv30),
         rank_npv40 = min_rank(-npv40),
         pctile_npv10 = ntile(npv10, 100),
         pctile_npv20 = ntile(npv20, 100),
         pctile_npv30 = ntile(npv30, 100),
         pctile_npv40 = ntile(npv40, 100))
         
#### (3) EXPORT DATA ----------------------------------------------------------

output <- full_npv_data %>%
  select(instnm, stabbr, iclevel, preddeg, control, rank_npv10, npv10,
         rank_npv15, npv15, rank_npv20, npv20, rank_npv30, npv30, rank_npv40,
         npv40, rank_rop, rop, rank_debt_roi, debt_roi, rank_debt, debt_mdn,
         rank_earn, p10, rank_netprice, netprice, rank_grad_rate, grad_rate,
         rpy_7yr_rt, hbcu, pbi, annhi, tribal, aanapii, hsi, nanti, ccbasic,
         ccugprof, ccsizset, menonly, womenonly, age_entry, report_as_group,
         gt_threshold_p10) %>%
  arrange(stabbr, instnm)

names(output) = c("Institution", "State", "Level", "Predominant degree", 
                  "Control", "10-year NPV rank", "10-year NPV", 
                  "15-year NPV rank", "15-year NPV", "20-year NPV rank", 
                  "20-year NPV", "30-year NPV rank", "30-year NPV", 
                  "40-year NPV rank", "40-year NPV", 
                  "Earnings-price return rank", "Earnings-price return", 
                  "Earnings-debt return rank", "Earnings-debt return", 
                  "Debt rank", "Median debt", "10-year earnings rank",
                  "Median 10-yr earnings", "Net price rank", "Net price", 
                  "Graduation rate rank", "Graduation rate",
                  "7-year repayment rate","HBCU", "PBI", "ANNHI", "TRIBAL", 
                  "AANAPII", "HSI", "NANTI", "CCBASIC", "CCUGPROF", "CCSIZSET", 
                  "MENONLY", "WOMENONLY", "AGE_ENTRY", "Reporting as a group", 
                  "Share earning more than high school graduates 10-years after enrolling")