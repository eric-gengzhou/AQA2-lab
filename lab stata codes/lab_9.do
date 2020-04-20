/** MISSING VALUES **/

* load data
clear all
set seed 54321

set maxvar 10000
use GSS_lab_9.dta, replace


* look at missing patterns
misstable patterns happy lninc educ female polviews

* take snapshot of data
snapshot save, label("original data")

* generate dummies for missing rows
misstable sum happy lninc educ female polviews, gen(m_)

* use only complete observations
keep if m_happy!=1 & m_lninc!=1 & m_educ!=1 & m_polviews!=1

* check results
misstable patterns happy lninc educ female polviews




* run TRUE regression and save coefficients
reg happy lninc educ female polviews 
matrix true_coef = e(b)


* take snapshot of complete data set
snapshot save, label("complete data")


/* generate 400 missing values under MCAR */

*if you want to ensure replicability use setseed
//set seed 123 

* generate random number from U(0,1)
gen u = runiform()
* sort by this number (effect: randomly reordering rows)
sort u
* create dummy for missing rows
gen misrow = 1 in 1/400  /// [_n]

* next, we create a random integer between 1 and 5 
* that indicates which column is missing
gen miscol = floor(5*runiform() + 1) in 1/400

* Note: we have selected the rows and columns "completely" at random.
*       the missing values should be correlated with "nothing".
*       this is what is meant by missing completely at random (MCAR)

local vlist "happy lninc educ female polviews"
forvalues i=1/400 {
	
	local which_col = miscol in `i' //loop through miscol through obs 1-400
	local which_var : word `which_col' of `vlist' // 5 variables in vlist and this will take each varname  out by its order in that list
	replace `which_var'=. in `i' //the the variable in vlist is called, replace it to a missing value at that "i", i.e., observation
	
}

* browse data
bro

* run a regression under MCAR
reg happy lninc educ female polviews 
matrix noimp_mcar = e(b)

* compare results
mat list true_coef
mat list noimp_mcar

* drop created variables
drop  misrow miscol


/* missing under MAR */

* restore complete data
snapshot restore 2

* MAR basically states that we can predict, probabilistically,
* the missing pattern with a set of observed variables 

* probability of missing
gen pm_lninc = invlogit(.2*educ + .9*female - .5*polviews + invnorm(uniform()))
gen pm_educ = invlogit(.6*lninc - .95*female - 1*polviews+ invnorm(uniform()))
gen pm_female = invlogit(-.5*lninc + .61*educ - .5*polviews+ invnorm(uniform()))
gen pm_polviews = invlogit(.2*lninc - .6*educ - 1.2*female+ invnorm(uniform()))
* as the total number of observations with missings is set to 400
* we create 100 missings for each variable

local vlist = "lninc educ female polviews"
foreach v of local vlist {
	gsort -pm_`v'
	replace `v'=. in 1/100
}
sort happy	
drop pm_*

* we first run a regression by list-wise deletion
reg happy lninc educ female polviews 
mat noimp_mar = e(b)
mat list noimp_mar
mat list true_coef


/* Multiple imputation */

mi set flong
mi register imputed lninc educ female polviews 
mi register regular happy

* start imputation
#delimit;
mi impute chained (logit) female 
				  (regress) lninc 
				  (truncreg, ll(0)) educ 
				  (ologit) polviews = happy, 
	add(10) rseed (53421) dots burnin(20)
	savetrace(trace,replace)
;

snapshot save, label("imputed_mar")


/* Check Convergence of Chains */
use trace, clear
desc
reshape wide *mean *sd, i(iter) j(m)
tsset iter

graph drop _all
local vlist = "lninc educ female polviews"

foreach v of local vlist  {
	tsline `v'_mean*, legend(off) nodraw ///
	ytitle(`v') scheme(s2color) name(g_`v')
}

graph combine g_lninc g_educ g_female g_polviews, ///
	title("Traceplot of Imputed Means")
	
snapshot restore 3

/* Fit regression with imputed values */
mi set flong
mi register imputed lninc educ female polviews 
mi register regular happy

mi estimate: reg happy lninc educ female polviews 
mat imp_mar = e(b_mi)

* compare results, true coefs, multiple imputated coefs of mar data, and no MI coefs of mar data
mat list true_coef
mat list imp_mar
mat list noimp_mar

/* missing values under MNAR */

* restore to the complete data
snapshot list // this tells you what snapshot data you have stored in the memory
// snapshot erase _all //this will erase all the snapshot data we have in the memory
snapshot restore 2 

* generate a variable that is correlated with income
* but not a function of other variables in the data

qui reg lninc educ female happy polviews // remember this is also called the decomposition or residualization
predict tmp1, resid
gsort -tmp1
replace lninc=. in 1/400 // now since we generate missing values based on the value of lninc, the missingness would now be correlated with lninc

qui reg polviews educ female happy lninc
predict tmp2, resid
gsort -tmp2
replace polviews=. in 1/400 //same as above but with polviews

* drop temporary variable and check correlations
drop tmp*

* note that because "tmp1" and "tmp2" are residuals,
* they are likely not explained by the 
* sort on "somevar" and take the first 300 observations as missing
* whether income or polviews is missing is determined by a coinflip

* run regression
reg happy lninc educ female polviews 
matrix noimp_mnar = e(b)

mat list noimp_mnar //ignoring missingness when it's mnar would bias the results
mat list true_coef 


* impute missing values

mi set flong
mi register imputed lninc polviews 
mi register regular happy educ female

* start imputation
#delimit;
mi impute chained (regress) lninc 
				  (ologit) polviews = happy educ female, 
	add(10) rseed (53421) dots burnin(20)
	savetrace(trace,replace) //this option will save the traceplot as a data file in your current directory
;

snapshot save, label("imputed_mnar")


* Again, check convergence
use trace, clear
desc
reshape wide *mean *sd, i(iter) j(m) // for each iteration and variable 
tsset iter //plot the graph as a time-series object

graph drop _all
local vlist = "lninc polviews"
foreach v of local vlist  {
	tsline `v'_mean*, legend(off) nodraw ///
	ytitle(`v') scheme(s2color) name(g_`v')
}
graph combine g_lninc g_polviews, ///
	title("Traceplot of Imputed Means")
	
snapshot restore 4

* estimate model

mi estimate: reg happy lninc educ female polviews 
mat imp_mnar = e(b_mi)


* compare results
matrix comb = true_coef \ noimp_mcar \ noimp_mar \ ///
			  imp_mar \ noimp_mnar \ imp_mnar
mat rownames comb = TRUE MCAR_n MAR_n MAR_i MNAR_n MNAR_i
mat list comb
