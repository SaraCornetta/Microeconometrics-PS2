*****************************************
******** Microeconometrics 20295*********
************PROBLEM SET 2****************
*****************************************
** Authors: Jacopo Battista (3172719), Sara Cornetta (3159903), Giulia Rita Putrino (3149673)
** Group 8

/* Gets user name */
local user = c(username)
display "`user'"

/* Stores filepath conditionally */
if ("`user'" == "erick") {
    global filepath "/home/erick/TEMP/"
}

if ("`user'" == "jacop") {
    global filepath ""
}

if ("`user'" == "sarac") {
    global filepath "C:\Users\sarac\OneDrive\Desktop\PS2\files"
}

if ("`user'" == "samsung") {
    global filepath "C:\Users\samsung\Downloads\PS2 MICROECONOMETRICS"
}

cd "$filepath"

***ssc install diff***


*----------------------------------------*
***************Exercise 1*****************
*----------------------------------------*
*# 1.(a)*

import delimited "$filepath\pset_4.csv", clear

/*The variable stpop reports the US state population. This variable is key to weighting descriptive output and analysis by state population.

STATA allows four alternative weighting methods: frequency weights, analytic weights, sampling weights and importance weights. As the divorce rates are state level averages, the analytic weights (aweight) are to be used. Indeed, these type of weighting weights observations as if each observation is a mean computed from a sample size n, where n is the weight variable. So analytic weights should be used in samples where each observation is a group mean. 
*/

*# 1.(b)*

*Graph 1*
tab lfdivlaw
codebook lfdivlaw

gen TREATED = (lfdivlaw >= 1968 & lfdivlaw <=1988)

gen Y_T= div_rate if TREATED==1
gen Y_C=div_rate if TREATED==0

preserve

collapse Y_T Y_C, by(year)
gen diff= Y_T - Y_C
twoway (line Y_T year) (line Y_C year) (line diff year, lpattern(dash)), ///
title(DIVORCE RATE: REFORM STATES AND CONTROLS) ///
ytitle(Divorces per 1000 people) xtitle(Year) legend(order(1 "Change the divorce law during 1968-1988" 2 "Friedberg's sample" 3 "Difference"))

restore

drop TREATED Y_T Y_C

*Graph 2*

gen TREATED = (lfdivlaw >= 1969 & lfdivlaw<=1973)
gen TREATED2 = (lfdivlaw==2000)

gen Y_T= div_rate if TREATED==1
gen Y_C=div_rate if TREATED2==1

preserve
collapse Y_T Y_C, by(year)
gen diff= Y_T - Y_C
twoway (line Y_T year) (line Y_C year) ///
(line diff year, lpattern(dash)) if year<=1978, ///
xline(1968.5) ///
title(DIVORCE RATE: REFORM STATES IN 1969-1973 AND 2000) ///
ytitle(Divorces per 1000 people) xtitle(Year) legend(order(1 "Change the divorce law between 1969 and 1973" 2 "Change the divorce law in 2000" 3 "Difference"))
restore

/*Yes, the parallel pre-trends assumption can be supported. In fact, the graph lines remains relatively consistent throughout the years.
*/

*#1.(c)*
preserve 
keep if year == 1968 | year == 1978
keep if (lfdivlaw >=1969 & lfdivlaw<=1973) | (lfdivlaw==2000)

gen UNILATERAL = (lfdivlaw >=1969 & lfdivlaw<=1973)
gen POST = (year==1978)
gen POST_UNILATERAL = (POST*UNILATERAL==1)

*regression (i)*
reg div_rate POST_UNILATERAL POST [aweight = stpop], vce(robust)

*regression (ii)*
reg div_rate POST_UNILATERAL POST UNILATERAL [aweight = stpop], vce(robust)

reg div_rate i.POST##i.UNILATERAL [aweight = stpop], vce(robust)

diff div_rate,  t(UNILATERAL) p(POST)

*#1.(d)*

matrix Table_1 = J(3, 3, .)

qui sum  div_rate if UNILATERAL==1 & POST==1 [aw=stpop]
matrix Table_1[1,1]=round(r(mean),.001)

qui sum  div_rate if UNILATERAL==0 & POST==1 [aw=stpop]
matrix Table_1[1,2]=round(r(mean),.001)

qui sum  div_rate if UNILATERAL==1 & POST==0 [aw=stpop]
matrix Table_1[2,1]=round(r(mean),.001)

qui sum  div_rate if UNILATERAL==0 & POST==0 [aw=stpop]
matrix Table_1[2,2]=round(r(mean),.001)

matrix Table_1[1,3]=Table_1[1,1]-Table_1[1,2]
matrix Table_1[2,3]=Table_1[2,1]-Table_1[2,2]
matrix Table_1[3,1]=Table_1[1,1]-Table_1[2,1]
matrix Table_1[3,2]=Table_1[1,2]-Table_1[2,2]
matrix Table_1[3,3]=Table_1[3,1]-Table_1[3,2]

matrix colnames Table_1= "UNILATERAL=1" "UNILATERAL=0" "Difference 2"
matrix rownames Table_1= "POST=1" "POST=0" "Difference 1"
	
putexcel set "$filepath/table_1.xlsx", replace
putexcel A1=matrix(Table_1) , names nformat(number_d2)
putexcel (A1:A4), overwr bold border(right thick) 
putexcel (A1:D1), overwr bold border(bottom thick) 
putexcel (C1:C4), border(right thick) 
putexcel (A4:D4), border(top thick)	
	
restore




