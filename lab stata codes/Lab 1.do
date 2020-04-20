**Lab codes for AQA-II 2020
**Lab session 1


* clear everything
clear all
* setting maxvar
set maxvar 30000

* set working directory
cd 
* Load data set 
use GSS_lab_1.dta

* Save under different name
//save "GSS_lab_1.dta", replace
use GSS_lab_1.dta, clear

* Browsing dataset
bro

** SETTING UP THE DADA------------------------------------

* search for religious preferences and religion rased in
lookfor relig
lookfor raised

/** Religion **/

* a look at religion
tab relig
tab year relig
tab relig, miss
* without labels
tab relig, nol miss

* Dummy for individuals with no religion
gen none=relig==4
* Check recoding
tab relig none
tab relig none, miss
* assign missings
replace none=. if relig==.
tab relig none, mis
* try again
replace none=. if missing(relig)
* Check again
tab relig none, mis


/** Religion Raised in **/

tab relig16
tab relig16, nol miss
gen rone=relig16==4 if !missing(relig16)
tab relig16 rone, mis

* drop it
drop rone
recode relig16 (4=1) (else=0), gen(rone)
tab relig16 rone, mis

* drop it again
drop rone
tab relig16,nol
recode relig16 (1/3=0) (4=1) (5/13=0) (else=.), gen(rone)
tab relig16 rone, mis


/** Keep only year 2014 **/

keep if year==2014
* equivalently you might use "drop if year !=2014"

* check crosstab
tab rone none, mis
tab rone none, col chi lrchi
tab rone none, row chi lrchi

/** Labeling **/
* look at what we have 
codebook rone
* assign label to variable
lab var rone "religion raised in"
* check
codebook rone
* define label for None
lab def none_lab 0 "some" 1 "none"
* check
codebook rone
* assign label
lab val rone none_lab
* check, again..
codebook rone
tab rone

* what labels are available?
lab list
* assign same value label to None
lab var none "current religion"
lab val none none_lab

tab rone none

** Multiway Cross-Tabulation --------------------------------

* generate cohort variable
gen coh75=cohort>1974 if !missing(cohort)
lab var coh75 "year of birth"
lab def coh75 0 "pre-1975" 1 "1975 or later"
lab val coh75 coh75
tab cohort coh75, mis

* generate female variable
recode sex (1=0) (2=1) (else=.),gen(female)
lab var female "gender"
lab def fem 1 "female" 0 "male"
lab val female fem

* tabulate one variable at a time
tab1 rone none sex coh75

* tabulate together: 3- and 4-way
tab rone none, summarize(female) mean
table rone none female
table rone none female, c(mean coh75)
table rone none coh75, by(female) row col

tab rone none if female==0 & coh75==0, row
tab rone none if female==0 & coh75==1, row
tab rone none if female==1 & coh75==0, row
tab rone none if female==1 & coh75==1, row

table rone if female==0 & coh75==0, c(mean none)
table rone if female==0 & coh75==1, c(mean none)
table rone if female==1 & coh75==0, c(mean none)
table rone if female==1 & coh75==1, c(mean none)


** Regressions & Tables --------------------------------

/** Two Predictors **/

** Let us first focus on Rone
reg none rone
table rone, c(mean none)

* next is rone and female together
reg none rone female, nohead
table rone female, c(mean none)
** Why are the proportions different??

reg none i.rone##i.female , nohead
table rone female, c(mean none)

/** Plot the results **/

* take a snapshot of the data
snapshot save, label("before collapsing")
* check
snapshot list

* collapsing data
collapse (mean) none if !missing(rone) & !missing(female), by(rone female)
*if you would like to creat a graph of yearly trend of somethings, collapse data by year
* look at data
bro

separate none, by(female)
rename none0 none_m
lab var none_m "male"
rename none1 none_f
lab var none_f "female"

* line graph of nones by gender 
twoway (line none_m none_f rone)
* look at data
list
twoway (line none_m none_f rone, lp(l -) lc(blue red)), ///
	ylab(.1334 .2133 .5895 .6667, grid glp(shortdash) glc(gray) glw(vthin)) ///
	xlab(0 1, val grid glp(shortdash) glc(gray) glw(vthing)) ///
	xscale(range(-.25 1.25)) ///
	ytitle(Proportion of Nones)

* restore original data
snapshot restore 1	

/** Three Predictors **/

reg none rone##female##coh75
**this regression reminds us interaction might not always be a good way, consider stratified analysis
table rone femal, c(mean none) by(coh75)

/** Plot the results **/

* collapsing data
collapse (mean) none if !missing(rone) & !missing(coh75), by(rone female coh75)
* look at data
bro

separate none, by(female)
rename none0 none_m
lab var none_m "male"
rename none1 none_f
lab var none_f "female"

* line graph of nones by gender if born before 75
twoway (line none_m none_f rone if coh75==0)

* add second set of lines for (coh75==1), this is also not an ideal way of showing the relationship
twoway (line none_m none_f rone if coh75==0) (line none_m none_f rone if coh75==1), ///
	scheme(s2gcolor)

* separate plots into two for each cohort
twoway (line none_m none_f rone), ///
	scheme(s2gcolor) ///
	by(coh75)

* make lines a little bit thicker
twoway (line none_m none_f rone, lw(medthick medthick)), ///
	scheme(s2gcolor) ///
	by(coh75) 

* change color
twoway (line none_m none_f rone, lw(medthick medthick) lc(black gray)), ///
	scheme(s2gcolor) ///
	by(coh75) 

* bring graphs closer together
twoway (line none_m none_f rone, lw(medthick medthick) lc(black gray)), ///
	scheme(s2gcolor) ///
	by(coh75, compact) 

* delete note on the bottom
twoway (line none_m none_f rone, lw(medthick medthick) lc(black gray)), /// 
	scheme(s2gcolor) ///
	by(coh75, compact note("")) 

* change position of legend to 3 o'clock
twoway (line none_m none_f rone, lw(medthick medthick) lc(black gray)), /// 
	scheme(s1color) ///
	by(coh75, compact note("") legend(pos(3))) 

* put legend into two rows
twoway (line none_m none_f rone, lw(medthick medthick) lc(black gray)), /// 
	scheme(s2gcolor) ///
	by(coh75, compact note("") legend(pos(3))) ///
	legend(rows(2))

* show only 0 and 1 on x-axis (there exists no rone of .5!)
twoway (line none_m none_f rone, lw(medthick medthick) lc(black gray)), /// 
	scheme(s2gcolor) ///
	by(coh75, compact note("") legend(pos(3))) ///
	legend(rows(2)) ///
	xlab(0(1)1)

* add value labels to 0 and 1 on the x-axis
twoway (line none_m none_f rone, lw(medthick medthick) lc(black gray)), /// 
	scheme(s2gcolor) ///
	by(coh75, compact note("") legend(pos(3))) ///
	legend(rows(2)) ///
	xlab(0(1)1, val)

* make x-axis's scal a little bit wider so that the labels can be seen
twoway (line none_m none_f rone, lw(medthick medthick) lc(black gray)), /// 
	scheme(s2gcolor) ///
	by(coh75, compact note("") legend(pos(3))) ///
	legend(rows(2)) ///
	xlab(0(1)1, val) ///
	xscale(range(-.1 1.1))

* change ticks on the y-axis
twoway (line none_m none_f rone, lw(medthick medthick) lc(black gray)), /// 
	scheme(s2gcolor) ///
	by(coh75, compact note("") legend(pos(3))) ///
	legend(rows(2)) ///
	xlab(0(1)1, val) ///
	xscale(range(-.1 1.1)) ///
	ylab(0(.25)1)

* change line 1) pattern, 2) width, and 3) color of horizontal (grid) lines
twoway (line none_m none_f rone, lw(medthick medthick) lc(black gray)), /// 
	scheme(s2gcolor) ///
	by(coh75, compact note("") legend(pos(3))) ///
	legend(rows(2)) ///
	xlab(0(1)1, val) ///
	xscale(range(-.1 1.1)) ///
	ylab(0(.25)1, glp(-) glw(thin) glc(grey))

* finally, add title on the y-axis
twoway (line none_m none_f rone, lw(medthick medthick) lc(black gray)), /// 
	scheme(s2gcolor) ///
	by(coh75, compact note("") legend(pos(3))) ///
	legend(rows(2)) ///
	xlab(0(1)1, val) ///
	xscale(range(-.1 1.1)) ///
	ylab(0(.25)1, glp(-) glw(thin) glc(grey)) ///
	ytitle(No religious preference (%))

* maybe changing the scheme...?
twoway (line none_m none_f rone, lw(medthick medthick) lc(black gray)), /// 
	scheme(s1mono) ///
	by(coh75, compact note("") legend(pos(3))) ///
	legend(rows(2)) ///
	xlab(0(1)1, val) ///
	xscale(range(-.1 1.1)) ///
	ylab(0(.25)1, glp(-) glw(thin) glc(grey)) ///
	ytitle(No religious preference (%))

* Another way to make a similar graph is to store them separately
twoway (line none_m none_f rone if coh75==0), title(pre-1975) name(p1)

twoway (line none_m none_f rone if coh75==1), ///
title(1975 or later) name(p2)
graph combine p1 p2

* notice that the y-axes are different, which is confusing 
graph drop _all
twoway (line none_m none_f rone if coh75==0), ///
title(pre-1975) ylab(0(.25)1) name(p1)
twoway (line none_m none_f rone if coh75==1), ///
title(1975 or later) ylab(0(.25)1) name(p2)
graph combine p1 p2


* restore original data
snapshot restore 1
	
