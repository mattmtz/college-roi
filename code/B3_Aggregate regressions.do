/******************************
*** NAME:    MATT MARTINEZ
*** PROJECT: UNDEREMPLOYMENT
*** PURPOSE: SETUP 
*** DATE:    06/02/2024
******************************/

clear
capture log close
macro drop _all
set more off, perm
set rmsg on
ssc install estout, replace
ssc install outreg2, replace

cd "C:/Users/mattm/Desktop/ROI/college-roi/"

import delimited using "intermediate/roi_data.csv", clear

*******************
*** SET UP DATA ***
*******************

keep if year == ""

** ENCODE KEY VARIABLES **
encode control, gen(n_control)
encode region, gen(n_region)
encode preddeg, gen(n_preddeg)
encode ugdssize, gen(n_ugdssize)

** CREATE AGGREGATE CCBASIC CATEGORIES **
gen ba_ccbasic = ""
	replace ba_ccbasic = "ba_colleges" if strpos(ccbasic, "Baccalaureate")
	replace ba_ccbasic = "dctrl_univs" if strpos(ccbasic, "Doctoral Univ")
	replace ba_ccbasic = "ma_colleges" if strpos(ccbasic, "Master's Coll")
	replace ba_ccbasic = "other" if mi(ba_ccbasic) & n_preddeg == 2
	
encode ba_ccbasic, gen(n_ba_ccbasic)

** CREATE LOGGED VALUES OF DEPVARS **
gen ln_p10 = ln(p10)
gen ln_npv10 = ln(npv10)
gen ln_npv40 = ln(npv40)

gen ugds_thousands = ugds / 1000

********************************
*** AGGREGATE-LEVEL ANALYSIS ***
********************************

estimates clear
eststo: regress ln_p10 adm_rate stem_pct ugds grad_rate pctpell pctfloan ///
 netprice ib2.n_preddeg ib3.n_control, r

eststo: regress ln_npv10 adm_rate stem_pct ugds grad_rate pctpell pctfloan ///
 ib2.n_preddeg ib3.n_control, r

eststo: regress ln_npv40 adm_rate stem_pct ugds grad_rate pctpell pctfloan ///
 ib2.n_preddeg ib3.n_control, r
 
esttab est1 est2 est3 using "output/_LaTeX/agg_regs.tex", label b(2) se(2) r2 ///
 nogaps star(* 0.10 ** 0.05 *** 0.01) booktabs title("Aggregate Regressions") ///
 nobase replace
 
***************************
*** ANALYSIS BY PREDDEG ***
***************************

estimates clear
** ASSOCIATES **
eststo: regress ln_p10 adm_rate stem_pct ugds grad_rate pctpell pctfloan ///
 netprice ib3.n_control if n_preddeg == 1, r

eststo: regress ln_npv10 adm_rate stem_pct ugds grad_rate pctpell pctfloan ///
 ib3.n_control if n_preddeg == 1, r

eststo: regress ln_npv40 adm_rate stem_pct ugds grad_rate pctpell pctfloan ///
 ib3.n_control if n_preddeg == 1, r

** BACHELORS **
eststo: regress ln_p10 adm_rate stem_pct ugds grad_rate pctpell pctfloan ///
 netprice ib3.n_control if n_preddeg == 2, r

eststo: regress ln_npv10 adm_rate stem_pct ugds grad_rate pctpell pctfloan ///
 ib3.n_control if n_preddeg == 2, r

eststo: regress ln_npv40 adm_rate stem_pct ugds grad_rate pctpell pctfloan ///
 ib3.n_control if n_preddeg == 2, r
 
** CERTIFICATE **
eststo: regress ln_p10 adm_rate stem_pct ugds grad_rate pctpell pctfloan ///
 netprice ib3.n_control if n_preddeg == 3, r

eststo: regress ln_npv10 adm_rate stem_pct ugds grad_rate pctpell pctfloan ///
 ib3.n_control if n_preddeg == 3, r

eststo: regress ln_npv40 adm_rate stem_pct ugds grad_rate pctpell pctfloan ///
 ib3.n_control if n_preddeg == 3, r
 
** EXPORT **
esttab using "output/_LaTeX/preddeg_regs.tex", label b(2) se(2) r2 nogaps nobase ///
 star(* 0.10 ** 0.05 *** 0.01) booktabs title("Aggregate Regressions") ///
 mgroups("Associate's" "Bachelor's" "Certificate", pattern(1 0 0 1 0 0 1 0 0) ///
 prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))
