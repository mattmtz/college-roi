library(dplyr)

gradsonthego = haven::read_dta("D://Projects/Scorecard/grads-on-the-go/Data/InstitutionFile.dta") %>%
  mutate(gotgFlag = 1)
edge_geocode = openxlsx::read.xlsx("D://Projects/Scorecard/Data/EDGE_GEOCODE_POSTSECSCH_2122.xlsx")
edge_data = edge_geocode %>%
  select(UNITID, CNTY, NMCNTY) %>%
  mutate(UNITID = as.integer(UNITID))
scorecard_orig = read.csv("../Most-Recent-Cohorts-Institution.csv", stringsAsFactors = FALSE)
names(scorecard_orig)[1] = "UNITID"
# Gather vars
id_vars = toupper(c("unitid", "opeid", "opeid6", "instnm", "control", "stabbr", "st_fips", "region", "latitude", "longitude", "preddeg", 
                    "ccbasic","locale", "locale2", "iclevel", "ccugprof", "ccsizset", "menonly", "womenonly", "age_entry", "openadmp",  
                    "relaffil","numbranch", "ugds", "ug", "hbcu", "pbi", "annhi", "tribal", "aanapii", "hsi", "nanti", "adm_rate", "main",
                    "pctpell", "UGDS_MEN", "UGDS_WOMEN"))
idVars = select(scorecard_orig, all_of(id_vars))
idVars = idVars %>%
  left_join(edge_data)
incVars = select(scorecard_orig, UNITID, starts_with("INC_PCT"))
debtVars = select(scorecard_orig, UNITID, CDR2, CDR3, PCTFLOAN, starts_with("DEBT"), starts_with("GRAD_DEBT"),
                  starts_with("FEMALE_DEBT"), starts_with("MALE_DEBT"),
                  starts_with("CUML_DEBT"), starts_with("PLUS_"), starts_with("PPLUS"), starts_with("FTFTPCT"), 
                  starts_with("LPSTAFFORD"), starts_with("LPPPLUS"), starts_with("LPGPLUS")) %>% select(-ends_with("_SUPP"))
earnVars = select(scorecard_orig, UNITID, COUNT_WNE_3YR, CNTOVER150_3YR, starts_with("MD_EARN_WNE_P"),
                  starts_with("PCT10_EARN_WNE_P"), starts_with("PCT25_EARN_WNE_P"), 
                  starts_with("MN_EARN_WNE_P"), starts_with("SD_EARN_WNE_P"), starts_with("GT_25K_P"), starts_with("GT_28K_P"),
                  starts_with("MN_EARN_WNE_INC"), starts_with("PCT75_EARN_WNE_P"), starts_with("PCT90_EARN_WNE_P"), 
                  starts_with("COUNT_WNE_P"), starts_with("COUNT_WNE_INC"), starts_with("COUNT_NWNE_"),
                  starts_with("COUNT_WNE_MALE"), starts_with("MN_EARN_WNE_MALE"), starts_with("GT_THRESHOLD"))
rpyVars = select(scorecard_orig, UNITID, starts_with("RPY_"), starts_with("FEMALE_RPY_"), starts_with("MALE_RPY_"),
                 starts_with("DBRR"), starts_with("BBRR")) 
gradVars = select(scorecard_orig, UNITID, C150_4_POOLED_SUPP, C150_L4_POOLED_SUPP, C150_4_PELL, C150_L4_PELL)
costVars = select(scorecard_orig, UNITID, starts_with("NPT4_P"), NPT4_OTHER, "COSTT4_A", "COSTT4_P", starts_with("NPT41_P"),
                  NPT41_OTHER, starts_with("NPT42_P"), NPT42_OTHER, starts_with("NPT43_P"), NPT43_OTHER, starts_with("NPT44_P"),
                  NPT44_OTHER, starts_with("NPT45_P"), NPT45_OTHER)
ipedsCountVars = select(scorecard_orig, UNITID, starts_with("NUM4"))
contextVars = select(scorecard_orig, UNITID, FAMINC, FAMINC_IND, MD_FAMINC, PCT_WHITE, PCT_BLACK, PCT_ASIAN, PCT_HISPANIC,
                     PCT_BA, PCT_GRAD_PROF, MEDIAN_HH_INC, POVERTY_RATE, UNEMP_RATE)
instVars = select(scorecard_orig, UNITID, AVGFACSAL, PFTFAC, INEXPFTE)

scorecard_orig = inner_join(idVars, incVars) %>% inner_join(debtVars) %>% inner_join(earnVars) %>% inner_join(rpyVars) %>%
  inner_join(gradVars) %>% inner_join(costVars) %>% inner_join(ipedsCountVars) %>% inner_join(contextVars) %>% 
  inner_join(instVars)

scorecard_orig = inner_join(mutate(select(scorecard_orig, UNITID, INSTNM, STABBR, NMCNTY), UNITID = as.numeric(UNITID)), 
                            mutate_all(select(scorecard_orig, -c(INSTNM, STABBR, NMCNTY)), as.numeric))
scorecard_orig = mutate(scorecard_orig, STABBR = factor(STABBR))
scorecard_orig = mutate(scorecard_orig, iclevel = factor(ICLEVEL, levels = c(1,2,3), 
                                                         labels=c("4-year","2-year","Less than 2-year")))
scorecard_orig = mutate(scorecard_orig, control = factor(CONTROL, levels = c(1,2,3), 
                                                         labels=c("Public","Private nonprofit","Private for-profit")))
scorecard_orig = mutate(scorecard_orig, 
                        preddeg = factor(PREDDEG, levels = seq(0,4), 
                                         labels = c("Not classified", "Certificate", "Associate's","Bachelor's", "Graduate")))
scorecard_orig = mutate(scorecard_orig, 
                        OPENADMP = factor(OPENADMP, levels=c(1,2,3), labels=c("Yes","No","Does not admit first time students")))
scorecard_orig = mutate(scorecard_orig, AdmRate = if_else(is.na(ADM_RATE) & OPENADMP == "Yes", 1, ADM_RATE))
scorecard_orig = mutate(scorecard_orig, ccbasic = factor(CCBASIC, levels = seq(1,33), labels=c(
  "Associate's Colleges: High Transfer-High Traditional",
  "Associate's Colleges: High Transfer-Mixed Traditional/Nontraditional",
  "Associate's Colleges: High Transfer-High Nontraditional",
  "Associate's Colleges: Mixed Transfer/Vocational & Technical-High Traditional",
  "Associate's Colleges: Mixed Transfer/Vocational & Technical-Mixed Traditional/Nontraditional",
  "Associate's Colleges: Mixed Transfer/Vocational & Technical-High Nontraditional",
  "Associate's Colleges: High Vocational & Technical-High Traditional",
  "Associate's Colleges: High Vocational & Technical-Mixed Traditional/Nontraditional",
  "Associate's Colleges: High Vocational & Technical-High Nontraditional",
  "Special Focus Two-Year: Health Professions",
  "Special Focus Two-Year: Technical Professions",
  "Special Focus Two-Year: Arts & Design",
  "Special Focus Two-Year: Other Fields",
  "Baccalaureate/Associate's Colleges: Associate's Dominant",
  "Doctoral Universities: Highest Research Activity",
  "Doctoral Universities: Higher Research Activity",
  "Doctoral Universities: Moderate Research Activity",
  "Master's Colleges & Universities: Larger Programs",
  "Master's Colleges & Universities: Medium Programs",
  "Master's Colleges & Universities: Small Programs",
  "Baccalaureate Colleges: Arts & Sciences Focus",
  "Baccalaureate Colleges: Diverse Fields",
  "Baccalaureate/Associate's Colleges: Mixed Baccalaureate/Associate's",
  "Special Focus Four-Year: Faith-Related Institutions",
  "Special Focus Four-Year: Medical Schools & Centers",
  "Special Focus Four-Year: Other Health Professions Schools",
  "Special Focus Four-Year: Engineering Schools",
  "Special Focus Four-Year: Other Technology-Related Schools",
  "Special Focus Four-Year: Business & Management Schools",
  "Special Focus Four-Year: Arts, Music & Design Schools",
  "Special Focus Four-Year: Law Schools",
  "Special Focus Four-Year: Other Special Focus Institutions",
  "Tribal Colleges")))
scorecard_orig = mutate(scorecard_orig, region = factor(REGION, levels = seq(0,9), labels=c(
  "U.S. Service Schools", 
  "New England (CT, ME, MA, NH, RI, VT)", 
  "Mid East (DE, DC, MD, NJ, NY, PA)", 
  "Great Lakes (IL, IN, MI, OH, WI)",
  "Plains (IA, KS, MN, MO, NE, ND, SD)",
  "Southeast (AL, AR, FL, GA, KY, LA, MS, NC, SC, TN, VA, WV)",
  "Southwest (AZ, NM, OK, TX)","Rocky Mountains (CO, ID, MT, UT, WY)",
  "Far West (AK, CA, HI, NV, OR, WA)",
  "Outlying Areas (AS, FM, GU, MH, MP, PR, PW, VI)")))

scorecard_orig = mutate(scorecard_orig, GradRate = if_else(is.na(C150_L4_POOLED_SUPP), C150_4_POOLED_SUPP, C150_L4_POOLED_SUPP))
scorecard = distinct(scorecard_orig, INSTNM, STABBR, .keep_all = TRUE)
# Remove predominantly graduate institutions
scorecard = filter(scorecard, preddeg != "Graduate", preddeg != "Not classified")
scorecard = droplevels(scorecard)
scorecard = mutate(scorecard, 
                   ugsize = forcats::fct_na_value_to_level(cut(UG, breaks = c(0,250,500,1e7), 
                                                         labels=c("<=250","251-500",">500"), include.lowest=TRUE)))
scorecard = mutate(scorecard, 
                   ugdssize = forcats::fct_na_value_to_level(cut(UGDS, breaks = c(0,250,500,1e7), 
                                                           labels=c("<=250","251-500",">500"), include.lowest=TRUE)))

# Flag institutions reporting together
scorecard = scorecard %>% mutate(reportAsGroup = if_else(NUMBRANCH > 1, 1, 0))
saveRDS(scorecard, file="scorecard_0423.rds")

# Remove institutions with no program data
programData = read.csv("../Most-Recent-Cohorts-Field-of-Study.csv", stringsAsFactors = FALSE)
names(programData)[1] = "UNITID"
programData = filter(programData, !is.na(UNITID), !is.na(OPEID6))
programData = distinct(programData, UNITID, OPEID6)
programData = mutate(programData, UNITID = as.numeric(UNITID))
scorecard = inner_join(scorecard, programData)

saveRDS(scorecard, file="scorecard_0423_mergedWithProgram.rds")

# academies = c(164155, 128328, 197036, 130624, 197027)
# ac = scorecard %>%
#   filter(UNITID %in% academies) %>%
#   select(INSTNM, NPT4_PUB, MD_EARN_WNE_P10, MD_EARN_WNE_P6, MD_EARN_WNE_P8)

forCF = scorecard %>% rowwise() %>% mutate(netPrice = mean(c(NPT4_PUB, NPT4_PRIV, NPT4_PROG, NPT4_OTHER), na.rm=TRUE))
forCF = tidyr::drop_na(forCF, c(MD_EARN_WNE_P6 ,MD_EARN_WNE_P8, MD_EARN_WNE_P10, netPrice))
forCF = filter(forCF, netPrice >= 0) 
forCF = forCF %>% mutate(diffEarn6_8 = MD_EARN_WNE_P8 - MD_EARN_WNE_P6, diffEarn8_10 = MD_EARN_WNE_P10 - MD_EARN_WNE_P8) 
forCF = forCF %>% mutate(avgEarnChg = (diffEarn6_8 + diffEarn8_10)/4) %>% 
  mutate(MD_EARN_WNE_P7 = MD_EARN_WNE_P6 + (diffEarn6_8/2), MD_EARN_WNE_P9 = MD_EARN_WNE_P8 + (diffEarn8_10/2))
# Use absolute value to build up to 6-year earnings
forCF = forCF %>% mutate(avgEarn = abs(avgEarnChg))
forCF = forCF %>% mutate(MD_EARN_WNE_P5 = MD_EARN_WNE_P6 - avgEarn, 
                         MD_EARN_WNE_P4 = MD_EARN_WNE_P5 - avgEarn, 
                         MD_EARN_WNE_P3 = MD_EARN_WNE_P4 - avgEarn, 
                         MD_EARN_WNE_P2 = MD_EARN_WNE_P3 - avgEarn)

r = 0.02
fedrate = 0.0499
forCF = forCF %>% rowwise() %>% 
  mutate(baNPV10 = FinCal::npv(r=r, cf=c(rep(-netPrice,5), c(MD_EARN_WNE_P6, MD_EARN_WNE_P7, MD_EARN_WNE_P8, MD_EARN_WNE_P9,
                                                             MD_EARN_WNE_P10))), 
         aaNPV10 = FinCal::npv(r=r, cf=c(rep(-netPrice,3), c(MD_EARN_WNE_P4, MD_EARN_WNE_P5, MD_EARN_WNE_P6, MD_EARN_WNE_P7,
                                                             MD_EARN_WNE_P8, MD_EARN_WNE_P9, MD_EARN_WNE_P10))), 
         certNPV10 = FinCal::npv(r=r, cf=c(-netPrice, c(MD_EARN_WNE_P2, MD_EARN_WNE_P3, MD_EARN_WNE_P4, MD_EARN_WNE_P5,
                                                        MD_EARN_WNE_P6, MD_EARN_WNE_P7, MD_EARN_WNE_P8, MD_EARN_WNE_P9,
                                                        MD_EARN_WNE_P10))))

# NPV for 15-years, no growth
forCF = forCF %>% rowwise() %>% 
  mutate(baNPV15 = FinCal::npv(r=r, cf=c(rep(-netPrice,5), c(MD_EARN_WNE_P6, MD_EARN_WNE_P7, MD_EARN_WNE_P8, MD_EARN_WNE_P9,
                                                             MD_EARN_WNE_P10, rep(MD_EARN_WNE_P10, 5)))),
         aaNPV15 = FinCal::npv(r=r, cf=c(rep(-netPrice,3), c(MD_EARN_WNE_P4, MD_EARN_WNE_P5, MD_EARN_WNE_P6, MD_EARN_WNE_P7,
                                                             MD_EARN_WNE_P8, MD_EARN_WNE_P9, MD_EARN_WNE_P10, 
                                                             rep(MD_EARN_WNE_P10, 5)))), 
         certNPV15 = FinCal::npv(r=r, cf=c(-netPrice, c(MD_EARN_WNE_P2, MD_EARN_WNE_P3, MD_EARN_WNE_P4, MD_EARN_WNE_P5,
                                                        MD_EARN_WNE_P6, MD_EARN_WNE_P7, MD_EARN_WNE_P8, MD_EARN_WNE_P9,
                                                        MD_EARN_WNE_P10, rep(MD_EARN_WNE_P10, 5)))))

# NPV for 20-years, no growth
forCF = forCF %>% rowwise() %>%
  mutate(baNPV20 = FinCal::npv(r=r, cf=c(rep(-netPrice,5), c(MD_EARN_WNE_P6, MD_EARN_WNE_P7, MD_EARN_WNE_P8, MD_EARN_WNE_P9,
                                                             MD_EARN_WNE_P10, rep(MD_EARN_WNE_P10, 10)))), 
         aaNPV20 = FinCal::npv(r=r, cf=c(rep(-netPrice,3), c(MD_EARN_WNE_P4, MD_EARN_WNE_P5, MD_EARN_WNE_P6, MD_EARN_WNE_P7,
                                                             MD_EARN_WNE_P8, MD_EARN_WNE_P9, MD_EARN_WNE_P10,
                                                             rep(MD_EARN_WNE_P10, 10)))), 
         certNPV20 = FinCal::npv(r=r, cf=c(-netPrice, c(MD_EARN_WNE_P2, MD_EARN_WNE_P3, MD_EARN_WNE_P4, MD_EARN_WNE_P5,
                                                        MD_EARN_WNE_P6, MD_EARN_WNE_P7, MD_EARN_WNE_P8, MD_EARN_WNE_P9,
                                                        MD_EARN_WNE_P10, rep(MD_EARN_WNE_P10, 10)))))

# NPV for 30-years, no growth
forCF = forCF %>% rowwise() %>%
  mutate(baNPV30 = FinCal::npv(r=r, cf=c(rep(-netPrice,5), c(MD_EARN_WNE_P6, MD_EARN_WNE_P7, MD_EARN_WNE_P8, MD_EARN_WNE_P9,
                                                             MD_EARN_WNE_P10, rep(MD_EARN_WNE_P10, 20)))), 
         aaNPV30 = FinCal::npv(r=r, cf=c(rep(-netPrice,3), c(MD_EARN_WNE_P4, MD_EARN_WNE_P5, MD_EARN_WNE_P6, MD_EARN_WNE_P7,
                                                             MD_EARN_WNE_P8, MD_EARN_WNE_P9, MD_EARN_WNE_P10, 
                                                             rep(MD_EARN_WNE_P10, 20)))), 
         certNPV30 = FinCal::npv(r=r, cf=c(-netPrice, c(MD_EARN_WNE_P2, MD_EARN_WNE_P3, MD_EARN_WNE_P4, MD_EARN_WNE_P5,
                                                        MD_EARN_WNE_P6, MD_EARN_WNE_P7, MD_EARN_WNE_P8, MD_EARN_WNE_P9,
                                                        MD_EARN_WNE_P10, rep(MD_EARN_WNE_P10, 20)))))

# NPV for 40-years, no growth
forCF = forCF %>% rowwise() %>% 
  mutate(baNPV40 = FinCal::npv(r=r, cf=c(rep(-netPrice,5), c(MD_EARN_WNE_P6, MD_EARN_WNE_P7, MD_EARN_WNE_P8, MD_EARN_WNE_P9,
                                                             MD_EARN_WNE_P10, rep(MD_EARN_WNE_P10, 30)))), 
         aaNPV40 = FinCal::npv(r=r, cf=c(rep(-netPrice,3), c(MD_EARN_WNE_P4, MD_EARN_WNE_P5, MD_EARN_WNE_P6, MD_EARN_WNE_P7,
                                                             MD_EARN_WNE_P8, MD_EARN_WNE_P9, MD_EARN_WNE_P10,
                                                             rep(MD_EARN_WNE_P10, 30)))), 
         certNPV40 = FinCal::npv(r=r, cf=c(-netPrice, c(MD_EARN_WNE_P2, MD_EARN_WNE_P3, MD_EARN_WNE_P4, MD_EARN_WNE_P5,
                                                        MD_EARN_WNE_P6, MD_EARN_WNE_P7, MD_EARN_WNE_P8, MD_EARN_WNE_P9,
                                                        MD_EARN_WNE_P10, rep(MD_EARN_WNE_P10, 30)))))

# NPV assignment
forCF = forCF %>% ungroup()
forCF = mutate(forCF,
               NPV10 = if_else(preddeg=="Bachelor's", baNPV10, 
                               if_else(preddeg=="Associate's", aaNPV10, 
                                       if_else(preddeg=="Certificate" & ICLEVEL == 3, certNPV10,
                                               if_else(preddeg=="Certificate" & ICLEVEL == 1, baNPV10, aaNPV10)))))
forCF = mutate(forCF, 
               NPV15 = if_else(preddeg=="Bachelor's", baNPV15, 
                               if_else(preddeg=="Associate's", aaNPV15, 
                                       if_else(preddeg=="Certificate" & ICLEVEL == 3, certNPV15, 
                                               if_else(preddeg=="Certificate" & ICLEVEL == 1, baNPV15, aaNPV15)))))
forCF = mutate(forCF, 
               NPV20 = if_else(preddeg=="Bachelor's", baNPV20, 
                               if_else(preddeg=="Associate's", aaNPV20, 
                                       if_else(preddeg=="Certificate" & ICLEVEL == 3, certNPV20, 
                                               if_else(preddeg=="Certificate" & ICLEVEL == 1, baNPV20, aaNPV20)))))
forCF = mutate(forCF, 
               NPV30 = if_else(preddeg=="Bachelor's", baNPV30, 
                               if_else(preddeg=="Associate's", aaNPV30, 
                                       if_else(preddeg=="Certificate" & ICLEVEL == 3, certNPV30, 
                                               if_else(preddeg=="Certificate" & ICLEVEL == 1, baNPV30, aaNPV30)))))
forCF = mutate(forCF, 
               NPV40 = if_else(preddeg=="Bachelor's", baNPV40, 
                               if_else(preddeg=="Associate's", aaNPV40, 
                                       if_else(preddeg=="Certificate" & ICLEVEL == 3, certNPV40,
                                               if_else(preddeg=="Certificate" & ICLEVEL == 1, baNPV40, aaNPV40)))))
# Compute debt-earnings ratio 
forCF = mutate(forCF, debtROI = MD_EARN_WNE_P10/DEBT_MDN - 1)

forCF = mutate(forCF, rankNPV10 = min_rank(-NPV10), rankNPV15 = min_rank(-NPV15), rankNPV20 = min_rank(-NPV20), 
               rankNPV30 = min_rank(-NPV30), rankNPV40 = min_rank(-NPV40))

forCF = mutate(forCF, rankDebtROI = min_rank(-debtROI), rankEarn = min_rank(-MD_EARN_WNE_P10), 
               rankDebt = min_rank(-DEBT_MDN), rankGradRate = min_rank(-GradRate))

forCF = mutate(forCF, pctileNPV40 = ntile(NPV40, 100), pctileNPV20 = ntile(NPV20, 100),
               pctileNPV30 = ntile(NPV30, 100), pctileNPV10 = ntile(NPV10, 100))

#compute return on net price
forCF = forCF %>% mutate(ROP = MD_EARN_WNE_P10/netPrice - 1)
forCF = forCF %>% mutate(rankROP = min_rank(-round(ROP,2)), rankNetPrice = min_rank(-round(netPrice,2)))

# Financial Calculation for debt payment
forCF = mutate(forCF, debtPayment = FinCal::pmt(fedrate/12, n=120, pv=DEBT_MDN, fv=0)*-12)
forCF = mutate(forCF, paymentEarnings = debtPayment/MD_EARN_WNE_P10)

forCF = forCF %>% left_join(gradsonthego, by=c("UNITID"= "unitid"))
saveRDS(forCF, file="roi0423.rds")

# Output
showtable = select(forCF, 
                   INSTNM, STABBR, iclevel, preddeg, control, rankNPV40, NPV40, rankNPV10, NPV10, rankROP, ROP, rankDebtROI,
                   debtROI, rankDebt, DEBT_MDN, rankEarn, MD_EARN_WNE_P10, rankNetPrice, netPrice)
names(showtable) = c("Institution", "State", "Level", "Predominant degree", "Control", "40-year NPV rank", "40-year NPV", 
                     "10-year NPV rank", "10-year NPV", "Earnings-price rank", "Earnings-price return", 
                     "Earnings-debt return rank", "Earnings-debt return", "Debt rank", "Median debt", 
                     "Earnings rank", "Median 10-yr earnings", "Net price rank", "Net price")
showtable = arrange(showtable, Institution, State)

forWeb = select(forCF, INSTNM, STABBR, iclevel, preddeg, control, rankNPV10, NPV10, rankNPV15, NPV15, rankNPV20, NPV20, 
                rankNPV30, NPV30, rankNPV40, NPV40, rankROP, ROP, rankDebtROI, debtROI, rankDebt, DEBT_MDN, 
                rankEarn, MD_EARN_WNE_P10, rankNetPrice, netPrice, rankGradRate, GradRate, RPY_7YR_RT, 
                HBCU, PBI, ANNHI, TRIBAL, AANAPII, HSI, NANTI, CCBASIC, CCUGPROF, CCSIZSET, MENONLY, WOMENONLY, AGE_ENTRY,
                reportAsGroup, GT_THRESHOLD_P10)
forWeb = arrange(forWeb, STABBR, INSTNM)
names(forWeb) = c("Institution", "State", "Level", "Predominant degree", "Control", "10-year NPV rank", "10-year NPV", 
                  "15-year NPV rank", "15-year NPV", "20-year NPV rank", "20-year NPV", "30-year NPV rank", "30-year NPV", 
                  "40-year NPV rank", "40-year NPV", "Earnings-price return rank", "Earnings-price return", 
                  "Earnings-debt return rank", "Earnings-debt return", "Debt rank", "Median debt", "10-year earnings rank",
                  "Median 10-yr earnings", "Net price rank", "Net price", "Graduation rate rank", "Graduation rate", 
                  "7-year repayment rate","HBCU", "PBI", "ANNHI", "TRIBAL", "AANAPII", "HSI", "NANTI", "CCBASIC", 
                  "CCUGPROF", "CCSIZSET", "MENONLY", "WOMENONLY", "AGE_ENTRY",
                  "Reporting as a group", "Share earning more than high school graduates 10-years after enrolling")

write.csv(forWeb, "ROIforWeb0423.csv", row.names = FALSE)
