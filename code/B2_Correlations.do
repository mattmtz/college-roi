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
* findit grc1leg
* NOTE: NEED TO INSTALL grc1leg for scatterplots

cd "C:/Users/mattm/Desktop/ROI/college-roi/"

import delimited using "intermediate/roi_data.csv", clear

*******************
*** SET UP DATA ***
*******************

keep if year == ""

** ENCODE KEY VARIABLES **
encode iclevel, gen(n_iclevel)
encode control, gen(n_control)
encode region, gen(n_region)
encode preddeg, gen(n_preddeg)

global KEYVARS p10 npv10 npv40 adm_rate stem_pct ugds grad_rate pctpell pctfloan netprice

****************************
*** OVERALL CORRELATIONS ***
****************************

estpost correlate $KEYVARS , matrix
esttab . using "output/_LaTeX/agg_correls_mr.tex", unstack b(2) not noobs ///
 nostar nonumber compress f replace 

*******************************
*** CORRELATIONS BY PREDDEG ***
*******************************
tab n_preddeg
local PREDDEG "associates bachelors certificate"
local i = 1
foreach x of local PREDDEG {

	estimates clear
	matrix drop _all

	estpost correlate $KEYVARS if n_preddeg == `i', matrix
 	
	matrix define RAWMAT = e(b)
	matrix define MAT = RAWMAT[.,1..27]
	estadd matrix MAT = MAT
	ereturn post MAT
	
	esttab . using `"output/_LaTeX/`x'_correls_mr.tex"', uns b(2) not noobs ///
	 nonumber compress f replace	
 
	local ++i
}

*****************************************
*** CORRELATIONS BY PREDDEG & CONTROL ***
*****************************************
tab n_control
local CONTROL "forprofit nonprofit public"
local PREDDEG "associates bachelors certificate"
local i = 1

foreach x of local CONTROL {
	di "`x'"
	local j = 1
	foreach z of local PREDDEG {
	
		di "`z'"
		
		estimates clear
		matrix drop _all
		
		estpost correlate $KEYVARS if n_preddeg==`j' & n_control==`i', matrix
		
		matrix define RAWMAT = e(b)
		matrix define MAT = RAWMAT[.,1..27]
		estadd matrix MAT = MAT
		ereturn post MAT
	
		esttab . using `"output/_LaTeX/`x'_`z'_correls_mr.tex"', uns b(2) not ///
		noobs nonumber compress f replace	
 
		local ++j
	}
	local ++i
}

********************
*** SCATTERPLOTS ***
********************


** PCTPELL **
twoway (scatter p10 pctpell if n_preddeg==1 & n_control == 3, msize(small)) ///
 (scatter p10 pctpell if n_preddeg==1 & n_control == 2, msize(small)) ///
 (scatter p10 pctpell if n_preddeg==1 & n_control == 1, msize(small)), ///
 legend(order(1 "Public" 2 "Nonprofit" 3 "For-profit") r(1)) xscale(range(0 1.03)) ///
 xlabel(0 "0%" .1 "10%" .2 "20%" .3 "30%" .4 "40%" .5 "50%" .6 "60%" .7 ///
 "70%" .8 "80%" .9 "90%" 1 "100%") ytitle("10-year earnings")  ///
 ylabel(0 "$0" 50000 "$50k" 100000 "$100k" 150000 "$150k") ///
 xtitle("Undergraduate Pell Grant share") yscale(range(0 155000) titlegap(*6)) ///
 graphregion(margin(r=1)) title("Associate's degree-granting") ///
 name(uleft, replace)

twoway (scatter p10 pctpell if n_preddeg==2 & n_control == 3, msize(small)) ///
 (scatter p10 pctpell if n_preddeg==2 & n_control == 2, msize(small)) ///
 (scatter p10 pctpell if n_preddeg==2 & n_control == 1, msize(small)), ///
 legend(order(1 "Public" 2 "Nonprofit" 3 "For-profit") r(1)) yscale(off) ///
 xlabel(0 "0%" .1 "10%" .2 "20%" .3 "30%" .4 "40%" .5 "50%" .6 "60%" .7 ///
 "70%" .8 "80%" .9 "90%" 1 "100%") xtitle("Undergraduate Pell Grant share") ///
 graphregion(margin(r=5 l=7)) title("Bachelor's degree-granting") ///
 xscale(range(0 1.03)) name(umid, replace)
 
twoway (scatter p10 pctpell if n_preddeg==3 & n_control == 3, msize(small)) ///
 (scatter p10 pctpell if n_preddeg==3 & n_control == 2, msize(small)) ///
 (scatter p10 pctpell if n_preddeg==3 & n_control == 1, msize(small)), ///
 legend(order(1 "Public" 2 "Nonprofit" 3 "For-profit") r(1)) yscale(off) ///
 xlabel(0 "0%" .1 "10%" .2 "20%" .3 "30%" .4 "40%" .5 "50%" .6 "60%" .7 ///
 "70%" .8 "80%" .9 "90%" 1 "100%") xtitle("Undergraduate Pell Grant share") ///
 graphregion(margin(r=10)) title("Certificate-granting") xscale(range(0 1.03)) ///
 name(uright, replace)
 
grc1leg uleft umid uright, ycommon legendfrom(uleft) rows(1) ///
 title("10-year earnings vs. undergraduate Pell Grant share", color(black)) ///
 subtitle("by institution primary degree and control", color(black))
 
graph export "output/p10_pctpell_correlations_by_preddeg_mr.png", ///
 width(3600) height(1400) replace 

graph close _all
 
** Admissions Rate **
twoway (scatter p10 adm_rate if n_preddeg==1 & n_control == 3, msize(small)) ///
 (scatter p10 adm_rate if n_preddeg==1 & n_control == 2, msize(small)) ///
 (scatter p10 adm_rate if n_preddeg==1 & n_control == 1, msize(small)), ///
 legend(order(1 "Public" 2 "Nonprofit" 3 "For-profit") r(1)) xscale(range(0 1.03)) ///
 xlabel(0 "0%" .1 "10%" .2 "20%" .3 "30%" .4 "40%" .5 "50%" .6 "60%" .7 ///
 "70%" .8 "80%" .9 "90%" 1 "100%") ytitle("10-year earnings")  ///
 ylabel(0 "$0" 50000 "$50k" 100000 "$100k" 150000 "$150k") ///
 xtitle("Undergraduate admissions rate") yscale(range(0 155000) titlegap(*6)) ///
 graphregion(margin(r=1)) title("Associate's degree-granting") ///
 name(uleft, replace)

twoway (scatter p10 adm_rate if n_preddeg==2 & n_control == 3, msize(small)) ///
 (scatter p10 adm_rate if n_preddeg==2 & n_control == 2, msize(small)) ///
 (scatter p10 adm_rate if n_preddeg==2 & n_control == 1, msize(small)), ///
 legend(order(1 "Public" 2 "Nonprofit" 3 "For-profit") r(1)) yscale(off) ///
 xlabel(0 "0%" .1 "10%" .2 "20%" .3 "30%" .4 "40%" .5 "50%" .6 "60%" .7 ///
 "70%" .8 "80%" .9 "90%" 1 "100%") xtitle("Undergraduate admissions rate") ///
 graphregion(margin(r=5 l=7)) title("Bachelor's degree-granting") ///
 xscale(range(0 1.03)) name(umid, replace)
 
twoway (scatter p10 adm_rate if n_preddeg==3 & n_control == 3, msize(small)) ///
 (scatter p10 adm_rate if n_preddeg==3 & n_control == 2, msize(small)) ///
 (scatter p10 adm_rate if n_preddeg==3 & n_control == 1, msize(small)), ///
 legend(order(1 "Public" 2 "Nonprofit" 3 "For-profit") r(1)) yscale(off) ///
 xlabel(0 "0%" .1 "10%" .2 "20%" .3 "30%" .4 "40%" .5 "50%" .6 "60%" .7 ///
 "70%" .8 "80%" .9 "90%" 1 "100%") xtitle("Undergraduate admissions rate") ///
 graphregion(margin(r=10)) title("Certificate-granting") xscale(range(0 1.03)) ///
 name(uright, replace)
 
grc1leg uleft umid uright, ycommon legendfrom(uleft) rows(1) ///
 title("10-year earnings vs. undergraduate admissions rate", color(black)) ///
 subtitle("by institution primary degree and control", color(black))
 
graph export "output/p10_admrate_correlations_by_preddeg_mr.png", ///
 width(3600) height(1400) replace 

graph close _all

** Graduation Rate **
twoway (scatter p10 grad_rate if n_preddeg==1 & n_control == 3, msize(small)) ///
 (scatter p10 grad_rate if n_preddeg==1 & n_control == 2, msize(small)) ///
 (scatter p10 grad_rate if n_preddeg==1 & n_control == 1, msize(small)), ///
 legend(order(1 "Public" 2 "Nonprofit" 3 "For-profit") r(1)) xscale(range(0 1.03)) ///
 xlabel(0 "0%" .1 "10%" .2 "20%" .3 "30%" .4 "40%" .5 "50%" .6 "60%" .7 ///
 "70%" .8 "80%" .9 "90%" 1 "100%") ytitle("10-year earnings")  ///
 ylabel(0 "$0" 50000 "$50k" 100000 "$100k" 150000 "$150k") ///
 xtitle("Undergraduate graduation rate") yscale(range(0 155000) titlegap(*6)) ///
 graphregion(margin(r=1)) title("Associate's degree-granting") ///
 name(uleft, replace)

twoway (scatter p10 grad_rate if n_preddeg==2 & n_control == 3, msize(small)) ///
 (scatter p10 grad_rate if n_preddeg==2 & n_control == 2, msize(small)) ///
 (scatter p10 grad_rate if n_preddeg==2 & n_control == 1, msize(small)), ///
 legend(order(1 "Public" 2 "Nonprofit" 3 "For-profit") r(1)) yscale(off) ///
 xlabel(0 "0%" .1 "10%" .2 "20%" .3 "30%" .4 "40%" .5 "50%" .6 "60%" .7 ///
 "70%" .8 "80%" .9 "90%" 1 "100%") xtitle("Undergraduate graduation rate") ///
 graphregion(margin(r=5 l=7)) title("Bachelor's degree-granting") ///
 xscale(range(0 1.03)) name(umid, replace)
 
twoway (scatter p10 grad_rate if n_preddeg==3 & n_control == 3, msize(small)) ///
 (scatter p10 grad_rate if n_preddeg==3 & n_control == 2, msize(small)) ///
 (scatter p10 grad_rate if n_preddeg==3 & n_control == 1, msize(small)), ///
 legend(order(1 "Public" 2 "Nonprofit" 3 "For-profit") r(1)) yscale(off) ///
 xlabel(0 "0%" .1 "10%" .2 "20%" .3 "30%" .4 "40%" .5 "50%" .6 "60%" .7 ///
 "70%" .8 "80%" .9 "90%" 1 "100%") xtitle("Undergraduate graduation rate") ///
 graphregion(margin(r=10)) title("Certificate-granting") xscale(range(0 1.03)) ///
 name(uright, replace)
 
grc1leg uleft umid uright, ycommon legendfrom(uleft) rows(1) ///
 title("10-year earnings vs. undergraduate graduation rate", color(black)) ///
 subtitle("by institution primary degree and control", color(black))
 
graph export "output/p10_gradrate_correlations_by_preddeg_mr.png", ///
 width(3600) height(1400) replace 

graph close _all
