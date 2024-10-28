/******************************
*** NAME:    MATT MARTINEZ
*** PROJECT: UNDEREMPLOYMENT
*** PURPOSE: SETUP 
*** DATE:    06/02/2024
******************************/

clear
capture log close
macro drop _all
program drop _all
set more off, perm
set rmsg on
ssc install estout, replace

cd "C:/Users/mattm/Desktop/ROI/college-roi/"

import delimited using "intermediate/roi_data.csv", clear

***********************************
*** DEFINE XTSUM OUTPUT PROGRAM ***
***********************************

program define xtsum2, eclass

syntax varlist

foreach var of local varlist {
    xtsum `var'

    tempname mat_`var'
    matrix mat_`var' = J(3, 3, .)
    matrix mat_`var'[1,1] = (`r(mean)', `r(sd)', `r(N)')
    matrix mat_`var'[2,1] = (., `r(sd_b)',  `r(n)')
    matrix mat_`var'[3,1] = (., `r(sd_w)',  `r(Tbar)')
    matrix colnames mat_`var'= Mean "Std. Dev." "N/n/T-bar"
    matrix rownames mat_`var'= `var' " " " "

    local matall `matall' mat_`var'
    local obw `obw' overall between within
}


if `= wordcount("`varlist'")' > 1 {
    local matall = subinstr("`matall'", " ", " \ ",.)
    matrix allmat = (`matall')
    ereturn matrix mat_all = allmat
}
else ereturn matrix mat_all = mat_`varlist'
ereturn local obw = "`obw'"

end

********************
*** PREPARE DATA ***
********************
 
** ENCODE KEY VARIABLES **
replace year = "MR" if mi(year)
encode year, gen(n_year)
encode iclevel, gen(n_iclevel)
encode control, gen(n_control)
encode region, gen(n_region)
encode preddeg, gen(n_preddeg)

**************************
*** PANEL DATA SUMMARY ***
**************************

preserve
	drop if year == "MR"
	bysort unitid: gen N = _N
	histogram N, percent discrete ytitle("Share of overall dataset") ///
	xtitle("unitid appearances in dataset") color(navy) blcolor(white) ///
	xlabel(0/8) ylabel(0 "0%" 20 "20%" 40 "40%" 60 "60%")
	graph export "output/_LaTeX/panel_balance_summary.png", ///
	 width(1500) height(1100) replace
restore	

**********************************
*** DEPENDENT VARIABLE SUMMARY ***
**********************************

** CORRELATION MATRICES **
estpost correlate p10 npv10 npv15 npv20 npv30 npv40 if year!= "MR", matrix
esttab . using "output/_LaTeX/depvar_correls_all.tex", unstack b(2) not noobs ///
 nostar nonumber compress note("Note: Most recent file excluded.") ///
 title("ROI variable correlations (individual year files)") replace 
 
estpost correlate p10 npv10 npv15 npv20 npv30 npv40 if year== "MR", matrix
esttab . using "output/_LaTeX/depvar_correls_mr.tex", unstack b(2) not noobs ///
 nostar title("ROI Variables Correlations") ///
 addnote("Most recent file only.") nonumber compress replace
 
** DISTRIBUTIONS **
kdensity p10 if year=="MR",  xtitle("10-year post-enrollment earnings") ///
 xlabel(0 "$0" 50000 "$50k" 100000 "$100k" 150000 "$150k") title("10-year earnings") ///
 note("Most recent file only. Kernel = epanechnikov.") name(kdens_p10, replace)
 
kdensity npv10 if year=="MR",  xtitle("10-year post-enrollment NPV") ///
 xlabel(-200000 "-$200k" 0 "$0" 200000 "$200k" 400000 "$400k" 600000 "$600k") ///
 title("10-year NPV") note("Most recent file only. Kernel = epanechnikov.") ///
 ylabel(, format("%4.0g")) ytitle("") name(kdens_npv10, replace)

kdensity npv40 if year=="MR",  xtitle("40-year post-enrollment NPV") ///
 xlabel(0 "$0" 1000000 "$1m" 2000000 "$2m" 3000000 "$3m" 4000000 "$4m" 5000000 "$5m") ///
 ylabel(, format("%4.0g")) ytitle("") title("40-year NPV") ///
 note("Most recent file only. Kernel = epanechnikov.") name(kdens_npv40, replace)
 
graph combine kdens_p10 kdens_npv10 kdens_npv40, r(1)
graph export "output/_LaTeX/full_depvar_kdensity_mr.png", width(2500) height(1100) replace
graph close _all

** 10-YEAR EARNINGS VS. NPV SCATTERPLOTS **
preserve
keep if year == "MR"
twoway (scatter p10 npv10 if n_preddeg==3) (scatter p10 npv10 if n_preddeg==2) ///
 (scatter p10 npv10 if n_preddeg==1), legend(order(1 "Certificate" 2 "Bachelors" ///
 3 "Associates") r(1)) xlabel(-200000 "-$200k" 0 "$0" 200000 "$200k" 400000 ///
 "$400k" 600000 "$600k" 800000 "$800k") ytitle("10-year earnings") ///
 ylabel(0 "$0" 50000 "$50k" 100000 "$100k" 150000 "$150k") xtitle("10-year NPV") ///
 yscale(titlegap(*6)) graphregion(margin(r=1)) name(left, replace)

twoway (scatter p10 npv20 if n_preddeg==3) (scatter p10 npv20 if n_preddeg==2) ///
 (scatter p10 npv20 if n_preddeg==1), legend(order(1 "Certificate" 2 "Bachelors" ///
 3 "Associates") r(1)) xlabel(0 "$0" 500000 "$0.5M" 1000000 "$1.0M" 1500000 ///
 "$1.5M" 2000000 "$2.0M") xtitle("20-year NPV") ytitle("10-year earnings") ///
 yscale(off) graphregion(margin(r=10)) name(right, replace)

graph combine left right, ycommon title("10-year earnings vs. NPV", color(black)) ///
 subtitle("by time horizon and institution primary degree", color(black))
 
graph export "output/_LaTeX/p10_npv_correlations_by_preddeg_mr.png", ///
 width(2700) height(2000) replace 

graph drop _all
restore
************************************
*** INDEPENDENT VARIABLE SUMMARY ***
************************************

** SET AS PANEL DATA **
isid unitid n_year
xtset unitid n_year

** HOW OFTEN DOES A CHANGE HAPPEN FOR NOMINALLY TIME-INVARIANT VARIABLES? **
foreach var of varlist control region preddeg iclevel ccbasic {
	di "`var'"
	preserve
		count if mi(`var')
		keep unitid `var'
		duplicates drop
		bysort unitid: gen N = _N
		tab N
	restore
}

** KEY INSTITUTION TYPE BREAKDOWNS **
foreach var of varlist control preddeg iclevel {
tab `var' if year == "MR"
}

** CHECK PREDDEG ICLEVEL **
tab preddeg iclevel if year != "MR", row
tab iclevel preddeg if year == "MR", row

table year n_iclevel if n_preddeg==1, format(%9.0fc) c(p50 p10) 
table year n_iclevel if n_preddeg==3, format(%9.0fc) c(p50 p10) 

** CHECK CCBASIC **
gen mi_ccbasic_flag = (ccbasic == "")
tab year mi_ccbasic_flag, row
drop mi_ccbasic_flag

** CHECK ADM_PCT **
tab preddeg openadmp, row
tab openadmp preddeg, row
tab control openadmp, row 
tab openadmp control, row 

** CHECK STEM_PCT **
gen stem0 = (stem_pct == 0)

tab preddeg stem0, row
tab stem0 preddeg, row
tab control stem0, row 
tab stem0 control, row

** CHECK PCTPELL/PCTFLOAN **
preserve
keep if year == "MR"

* By CONTROL*
twoway (kdensity pctpell if n_control == 3) (kdensity pctpell if n_control == 2) ///
 (kdensity pctpell if n_control == 1), legend(order(1 "Public" 2 "Nonprofit" ///
 3 "For-profit") r(1)) xtitle("Undergraduate Pell Percentage") ytitle("") ///
 name(u_left, replace)

twoway (kdensity pctfloan if n_control == 3) (kdensity pctfloan if n_control == 2) ///
 (kdensity pctfloan if n_control == 1), legend(order(1 "Public" 2 "Nonprofit" ///
 3 "For-profit") r(1)) xtitle("Undergraduate Federal Loan Percentage") ///
 ytitle("") name(u_right, replace)
 
graph combine u_left u_right, ycommon name(top, replace) ///
 title("Pell Grant/Federal Loan Percentages by Institution Control")
 
* By PREDDEG*
twoway (kdensity pctpell if n_preddeg == 1) (kdensity pctpell if n_preddeg == 2) ///
 (kdensity pctpell if n_preddeg == 3), legend(order(1 "Associates" 2 "Bachelors" ///
 3 "Certificate") r(1)) xtitle("Undergraduate Pell Percentage") ytitle("") ///
 name(b_left, replace)

twoway (kdensity pctfloan if n_preddeg == 1) (kdensity pctfloan if n_preddeg == 2) ///
 (kdensity pctfloan if n_preddeg == 3), legend(order(1 "Associates" 2 "Bachelors" ///
 3 "Certificate") r(1)) xtitle("Undergraduate Federal Loan Percentage") ///
 ytitle("") name(b_right, replace)
 
graph combine b_left b_right, ycommon name(bottom, replace) ///
 title("Pell Grant/Federal Loan Percentages by Institution Primary Degree")

graph combine top bottom, cols(1) imargin(r=5)
graph export "output/_LaTeX/aid_pct_kdensity_mr.png", width(2750) height(1900) replace
graph close _all

restore
** CHECK WITHIN-INSTITUTION VARIABILITY (RATE VARS) **
preserve
	drop if year== "MR"
	xtsum2 adm_rate grad_rate stem_pct pctpell pctfloan
	matrix define RATE_STATS = e(mat_all)
	esttab mat(RATE_STATS, fmt(%12.3fc)) using "output/_LaTeX/rate_vars_sumstats.tex", ///
	 mlabels(none) labcol2(`e(obw)') varlabels(r2 " " r3 " ")  ///
	 title("Rate Variable Summary Statistics") replace
restore
 
** CHECK WITHIN-INSTITUTION VARIABILITY (VALUE VARS) **
preserve
	drop if year== "MR"
	xtsum2 p10 npv10 npv40
	matrix define RATE_STATS = e(mat_all)
	esttab mat(RATE_STATS, fmt(%9.0fc)) using "output/_LaTeX/depvars_sumstats.tex", ///
	 mlabels(none) labcol2(`e(obw)') varlabels(r2 " " r3 " ")  ///
	 title("Dependent Variable Summary Statistics") replace
restore
