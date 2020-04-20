

**AQA-II, spring 2020
**do file for lab 2

* clear everything
clear all

* setting maxvar
set maxvar 30000

* set working directory
cd "your directory"

* Save under different name
*save "gss7214.dta"
use GSS_lab_2, clear

* keep only post 2009 years
keep if year>= 2010

* generate dummy for support for abortion & check
gen Abdum = abany==1 if abany < .
tab Abdum abany, mis
* label variable
lab var Abdum "Support for Abortion (all circumstanes)"
lab def yn 0 "No" 1 "Yes"
lab val Abdum yn

* generate dummy for gender
gen Female = sex == 2 if sex <.
tab Female sex, mis
lab var Female "Gender"
lab def G 0 "Male" 1 "Female"
lab val Female G

* generate political views var.
gen Ideo = polviews if polviews <.
tab Ideo polviews, mis
lab var Ideo "Ideological Self-placement"
lab def ide 1 "EL" 2 "L" 3 "SL" 4 "M" 5 "SC" 6 "C" 7 "EC"
lab val Ideo ide

* show proportion of support for all combinations across Ideo and sex
table Female Ideo , c(mean Abdum)
tab Femal Ideo, sum(Abdum) mean

* take snapshot
snapshot save, label("before collapse")

* collapse data
collapse (mean) Abdum if Female < . & Ideo < ., by(Female Ideo)
* check dataset
bro
* label new outcome
lab var Abdum "Support (%)"

* plot
tw (connected Abdum Ideo if Female==0, lc(blue) m(oh) mc(blue)) ///
(connected Abdum Ideo if Female==1, lc(red) m(X) mc(red)), ///
xlab(1(1)7, val) xscale(range(.5 7.5)) ///
legend(label(1 Male) label(2 Female))
* seems like there's an interaction!

drop if Ideo >= 8

tw (connected Abdum Ideo if Female==0, lc(blue) m(oh) mc(blue)) ///
(connected Abdum Ideo if Female==1, lc(red) m(X) mc(red)), ///
xlab(1(1)7, val) xscale(range(.5 7.5)) ///
legend(label(1 Male) label(2 Female))

* restore previous data
snapshot restore 1
bro

/** Logistic Regression **/

* run interaction model (treating Ideo as continuous)
* note: 1) the "c." prefix tells STATA that the variable should be treated as continuous
*       2) the "i." prefix stands for categorical variables


logit Abdum c.Ideo##i.Female //what is the difference between this one and the one below?
logistic Abdum c.Ideo##i.Female 


* odds ratios? 
* note: 1) if no outcome and predictors are specified, STATA will run the latest model again
*       2) the "nohead" options says that STATA should not print the header in the results

logit, or nohead //ln(odss ratio) = coef.
	
* centering self-placement variable at 4 (which is "Moderate")
gen IdeoC = Ideo - 4
tab IdeoC Ideo, mis

* run logit with centered variable, would you expect any changes?
logit Abdum c.IdeoC##i.Female

* compare results (what do you expect? which coefficients are the same? which are different?)
* note: 1) the "quietly" at the start tells STATA not to produce any output
*       2) the "estimates store" command saves estimation results in the objects "m1" and "m2"
quietly logit Abdum c.Ideo##i.Female
estimates store raw
quietly logit Abdum c.IdeoC##i.Female
estimates store centered
* tabulate stored results, (options "b se" tells STATA to print coefficients and standard errors)
estimates table raw centered, b se


//** Generate Predicted Probabilities **//

* predict probabilities
* note: "if e(sample)" says that a the command should be executed only for those observation that 
*       were used in the last estimation procedure (here logistic regression)
quietly logit Abdum c.IdeoC##i.Female
predict Yhat if e(sample)

* check results? 
* note: 1) nolog tells STATA not to show iterations of maximization procedure
logit Abdum c.IdeoC##i.Female if IdeoC <4, nohead nolog 
tab Yhat if IdeoC==0 & Female==0
di 1/(1+exp(-(-.8415046 ))) 
tab Yhat if IdeoC==-2 & Female==1
di 1/(1+exp(-(-.8415046 - .0965176*(1) -.3142825*(-2) -.0704282*(-2 *1))))

* plot results
tw (connected Yhat Ideo if Female==0, sort lc(red) mc(red)) ///
(connected Yhat Ideo if Female==1, sort lc(blue) mc(blue)) ///
(scatter Abdum Ideo, m(oh) mcol(gray*.25) jitter(5)), ///
xlab(1(1)7, val) xscale(range(.5 7.5)) ///
legend(label(1 "Male")) legend(label(2 "Female")) ///
legend(label(3 "Observed (Jittered)")) ///
ytitle("Predicted Probabilities")

* adding observed proportions (note the i. pefix on Ideo!)
quietly reg Abdum i.Ideo##i.Female
predict RegPr if e(sample)
egen Comb1 = tag(Ideo Female Abdum)


tw (scatter RegPr Ideo if Female==0 & Comb1==1, m(O) mc(red*.25) msize(medium)) ///
(scatter RegPr Ideo if Female==1 & Comb1==1, m(O) mc(blue*.25) msize(medium)) ///
(connected Yhat Ideo if Female==0, sort lc(red) mc(red) m(O)) ///
(connected Yhat Ideo if Female==1, sort lc(blue) mc(blue) m(O)), ///
xlab(1(1)7, val  angle(horizontal)) xscale(range(.5 7.5)) ///
ylab(0(.25)1) ///
legend(order(3 4 1 2)) ///
legend(label(3 "Male")) legend(label(4 "Female")) ///
legend(label(1 "Observed (Male, %)")) ///
legend(label(2 "Observed (Female, %)")) ///
legend(rows(2)) ///
ytitle("Predicted Probabilities")


/** Labor Force Participation **/

**    Create LFP variable
gen LFP=wrkstat<5 //binary
replace LFP=wrkstat if wrkstat>=.
lab var LFP "Labor force participation"
lab def LFP 0 "Out" 1 "In"
lab val LFP LFP


**   Polynomial Model 
gen Age18=age-18
lab var Age18 "Age (centered)"

* quadratic
logit LFP c.Age18##c.Age18
predict Yhat1
tw (line Yhat1 Age18, sort)
* WHY DOES THE CURVE NOT LOOK QUADRATIC??
predict Xb1, xb
tw (line Xb1 Age18, sort), ytitle("Logit")

* add interactions
logit LFP Age18 c.Age18#c.Age18 c.Age18#c.Age18 /// age terms
	i.Female /// gender
	i.Female#c.Age18 i.Female#c.Age18#c.Age18, /// interactions
	nolog nohead
* ALL SIGNIFICANTLY DIFFERENT FROM ZERO, WHAT CAN WE INFER FROM THIS??

*** test for joint significance ***
* Wald test
testparm i.Female#c.Age18 i.Female#c.Age18#c.Age18
* LR test
quietly logit LFP Age18 c.Age18#c.Age18 c.Age18#c.Age18 ///
	i.Female ///
	i.Female#c.Age18 i.Female#c.Age18#c.Age18
estimates store full
quietly logit LFP Age18 c.Age18#c.Age18 c.Age18#c.Age18 ///
	i.Female 
estimates store restricted
lrtest full restricted, stats


* predict probability
qui logit LFP Age18 c.Age18#c.Age18 c.Age18#c.Age18 /// age terms
	i.Female /// gender
	i.Female#c.Age18 i.Female#c.Age18#c.Age18, /// interactions
	nolog nohead

predict Yhat2
* separate by gender & rename
separate Yhat2, by(Female)
rename Yhat20 Yhat2m
rename Yhat21 Yhat2f
lab var Yhat2m "Predicted Probability, Male"
lab var Yhat2f "Predicted Probability, Female"

* plot
tw (line Yhat2m Age18, sort) (line Yhat2f Age18, sort)
* add color
tw (line Yhat2m Age18,sort lc(blue)) (line Yhat2f Age18, sort lc(red))

* Observed Proportions?
reg LFP i.Age18  /// age terms
        i.Female /// gender
        i.Female#i.Age18, /// interactions
		nohead
* WHY ARE NOT USING ANY SQUARED TERMS???

* get proportions
predict Obspr
* These are just the observed proportions! (to see why, check ...)
bysort Age18 Female: egen Obscheck = mean(LFP)
scatter Obspr Obscheck
drop Obscheck

* get unique combinations of LFP Age18 and Female
egen CombLFP = tag(LFP Age18 Female)

* add observed proportions to plot (note that scatter comes first!)
tw (scatter Obspr Age18 if Female==0 & CombLFP==1, mc(blue) m(oh)) ///
   (scatter Obspr Age18 if Female==1 & CombLFP==1, mc(red) m(oh)) ///
   (line Yhat2m Age18,sort lc(blue)) (line Yhat2f Age18, sort lc(red))


 * add legend 
tw (scatter Obspr Age18 if Female==0 & CombLFP==1, mc(blue) m(oh)) ///
   (scatter Obspr Age18 if Female==1 & CombLFP==1, mc(red) m(oh)) ///
   (line Yhat2m Age18,sort lc(blue)) (line Yhat2f Age18, sort lc(red)), ///
   legend(label(1 "Observed(%), Male")) legend(label(2 "Observed(%), Female"))

 
* change legend order
tw (scatter Obspr Age18 if Female==0 & CombLFP==1, mc(blue) m(oh)) ///
   (scatter Obspr Age18 if Female==1 & CombLFP==1, mc(red) m(oh)) ///
   (line Yhat2m Age18,sort lc(blue)) (line Yhat2f Age18, sort lc(red)), ///
   legend(label(1 "Observed Proportion, Male")) legend(label(2 "Observed Proportion, Female")) ///
   legend(order(1 3 2 4))
   
* change size of legend and add title
tw (scatter Obspr Age18 if Female==0 & CombLFP==1, mc(blue) m(oh)) ///
   (scatter Obspr Age18 if Female==1 & CombLFP==1, mc(red) m(oh)) ///
   (line Yhat2m Age18,sort lc(blue)) (line Yhat2f Age18, sort lc(red)), ///
   legend(label(1 "Observed Proportion, Male")) legend(label(2 "Observed Proportion, Female")) ///
   legend(order(1 3 2 4)) ///
   legend(pos(3) size(small) subtitle({bf: Gender}))
   
* add subtitles to legend
tw (scatter Obspr Age18 if Female==0 & CombLFP==1, mc(blue) m(oh)) ///
   (scatter Obspr Age18 if Female==1 & CombLFP==1, mc(red) m(oh)) ///
   (line Yhat2m Age18,sort lc(blue)) (line Yhat2f Age18, sort lc(red)), ///
   legend(label(1 "Observed Proportion, Male")) legend(label(2 "Observed Proportion, Female")) ///
   legend(size(small) subtitle({bf: Gender}, size(small)))  ///
   legend(order(- "{bf: Male}" 1 3 - "{bf: Female}" 2 4))

* Now we can shorten the labels
tw (scatter Obspr Age18 if Female==0 & CombLFP==1, mc(blue) m(oh)) ///
   (scatter Obspr Age18 if Female==1 & CombLFP==1, mc(red) m(oh)) ///
   (line Yhat2m Age18,sort lc(blue)) (line Yhat2f Age18, sort lc(red)), ///
   legend(label(1 "Observed Proportion")) legend(label(2 "Observed Proportion")) ///
   legend(label(3 "Predicted Probability")) legend(label(4 "Predicted Probability")) /// !!!
   legend( size(small) subtitle({bf: Gender}, size(small)))  ///
   legend(order(- "{bf: Male}" 1 3 - "{bf: Female}" 2 4))

 * Lastly, lets change the angle of the xlabs
tw (scatter Obspr Age18 if Female==0 & CombLFP==1, mc(blue) m(oh)) ///
   (scatter Obspr Age18 if Female==1 & CombLFP==1, mc(red) m(oh)) ///
   (line Yhat2m Age18,sort lc(blue)) (line Yhat2f Age18, sort lc(red)), ///
   legend(label(1 "Observed Proportion")) legend(label(2 "Observed Proportion")) ///
   legend(label(3 "Predicted Probability")) legend(label(4 "Predicted Probability")) /// 
   legend( size(small) subtitle({bf: Gender}, size(small)))  ///
   legend(order(- "{bf: Male}" 1 3 - "{bf: Female}" 2 4)) ///
   xlab(,angle(horizontal)) ///
   xtitle("Age") ytitle("Prob. Labor Force Participation")


   
/** How to use MARGINS **/

* run logistic regression
logit LFP c.age i.Female
* predicted probabilities
predict pred

tw (line pred age) // we see zig-zagged lines, why?
tw (scatter pred age) 

logit LFP c.age i.Female c.educ
predict pred2 
tw (scatter pred2 age)

logit LFP c.age i.Female c.educ 
margins
predict pred3 if e(sample)
sum pred3

margins, at(age=(18(1)89) educ=12 Female=1)
marginsplot

* change xlabes
marginsplot, xlabel(20(10)90)
* change titles
marginsplot, ///
	xlabel(20(10)90) ///
	ytitle("Predicted Probabilities") ///
	xtitle("Age") ///
	title("Labor Force Participation and Age") ///
	note("Calcuated Probability for women with 12 years of education")

* change pointwise boxplots to lines
marginsplot, ///
	xlabel(20(10)90) ///
	ytitle("Predicted Probabilities") ///
	xtitle("Age") ///
	title("Labor Force Participation and Age") ///
	note("Calcuated Probability for women with 12 years of education") ///
	recast(line) 

* change pointwise boxplots to area plot
marginsplot, ///
	xlabel(20(10)90) ///
	ytitle("Predicted Probabilities") ///
	xtitle("Age") ///
	title("Labor Force Participation and Age") ///
	note("Calcuated Probability for women with 12 years of education") ///
	recast(line) ///
	recastci(rline)

* change line pattern of ci
marginsplot, ///
	xlabel(20(10)90, angle(0)) ///
	ytitle("Predicted Probabilities") ///
	xtitle("Age") ///
	title("Labor Force Participation and Age") ///
	note("Calcuated Probability for women with 12 years of education") ///
	recast(line) ///
	recastci(rline) ciopts(lc(gray) lp(-) lw(vthin))
	

/** Use MARGINS to generate same plot as Mike's **/
qui logit LFP age c.age#c.age c.age#c.age /// age terms
	i.Female /// gender
	i.Female#c.age i.Female#c.age#c.age, /// interactions
	nolog nohead

margins, at(age=(18(1)89) Female=(0 1)) vsquish
* show plot
marginsplot
* show without estimated CI
marginsplot, noci
* change colors and add title
marginsplot, noci recast(line) ///
	plot1opts(lc(blue)) ///
	plot2opts(lc(red) lp(l)) ///
	xtitle(Age) ytitle(Predicted Probability) title(Labor Force Participation)

* change labels & position of legend
marginsplot, noci recast(line) ///
	plot1opts(lc(blue)) ///
	plot2opts(lc(red) lp(l)) ///
	xtitle(Age) ytitle(Predicted Probability) title(Labor Force Participation) ///
	xlabel(20(10)90, angle(horizontal)) legend(pos(3) subtitle("Gender") size(small))

* Lastly, add observed proportions!

marginsplot, noci recast(line) ///
	plot1opts(lc(blue)) ///
	plot2opts(lc(red) lp(l)) ///
	xtitle(Age) ytitle(Predicted Probability) title(Labor Force Participation) ///
	xlabel(20(10)90, angle(horizontal)) legend(pos(3) subtitle("Gender") size(small)) ///
	addplot((scatter Obspr age if Female==0 & CombLFP==1, mc(blue) m(oh)) ///
   (scatter Obspr age if Female==1 & CombLFP==1, mc(red) m(oh)))

   
/** Plot difference in predicted probability between Male and Female **/

qui logit LFP Age18 c.Age18#c.Age18 c.Age18#c.Age18 /// age terms
	i.Female /// gender
	i.Female#c.Age18 i.Female#c.Age18#c.Age18, /// interactions
	nolog nohead

* calculate margins
margins, dydx(Female) over(Age18)
* plot
marginsplot, yline(0, lc(red) lp(l))
* change line 
marginsplot, yline(0, lc(red) lp(l)) ///
	recast(line) recastci(rline) ///
	ciopts(lc(gray) lp(-) lw(vthin))
	
	
