*lab 5
** SETTING UP THE SESSION -------------------------------

* clear everything and load the gss again
clear all
use GSS_lab_5.dta

* look at outcome
tab happy   
recode happy 1=3 2=2 3=1 else = ., gen(rhappy)
lab var rhappy "happiness"
lab def happylab 1 "not so happy" 2 "pretty happy" 3 "very happy"
lab val rhappy happylab
tab rhappy, m

* generate log(base2)-income (constant $)
gen lbinc = log(income)/log(2)
* note: 
* the "log" and "ln" function in STATA are both the natural logarithm,
* i.e., the logarithm with base e, where e is the Euler constant (this is just a number as "pi').
* To transform from log base e to log base 2, we note the following:
*
* Let lb(x) be the log with base 2 of x and ln(x) the natural logarithm of x. 
* Consider the following identity;
* 			x = 2^(lb(x)).
* As both sides are non-negative, take the natural logarithm of both sides, to obtain
* 		ln(x) = ln(2^(lb(x)) = lb(x) * ln(2)
* Dividing both sides by ln(2), we get
*       lb(x) = ln(x)/ln(2)
* as desired.

gen Female = sex==2
lab var Female "Gender"
lab def F 0 "Male" 1 "Female"
lab val Female F

gen mar = marital==1 if !missing(marital)
lab var mar "Marital Status"
lab def mar 0 "Not Married" 1 "Married"
lab val mar mar

* keep only 2006-2012
keep if year >= 2006 & year <= 2012


/** Ordered Logistic Regression Without Predictors **/

* Let us start with simplest model of happiness (no predictors at all)
ologit rhappy
* note that we have no estimated "regression coefficients", not even a CONSTANT
* on the other hand, we have two estimated cutpoints.

* so what are these cutpoints? First, we store the cutpoints into a matrix
mat b = e(b)
* note: 
* matrix objects are stores separately in STATA, so you have to use "matrix" commands to access them
* For example, to see the matrix you can use
mat list b
* to access an element you might use b[1,1], b[1,2], etc.
display b[1,1]
* you can look into the available commands that pertain to matrix objects by typing
help matrix

* Now, notice that "no constant" means practically that we are assuming that the constant is equal to zero
* To understand the cutpoints, we first look at the standard Logistic distribution
* (The standard Logistic distribution is just a distribution like any other. The standard Logistic distribution
*  has a mean of zero and a variance of pi^2/3. 
*  You don't have to understand the equations, I just want to show how it looks like)
tw function y=exp(-x)/((1+exp(-x))^2), range(-10 10)
* This looks a lot like a Normal distribution, but the the tails are thicker.
tw (function y=exp(-x)/((1+exp(-x))^2), range(-10 10)) ///
   (function y=normalden(x), range(-10 10) lc(red)), ///
   legend(label(1 "Logistic distribution") label(2 "Normal distribution")) //this shows the shape of logit/probit error 

* let us plot the histogram of a logistically distributed random variable, together with the cutpoints
scalar c1 = b[1,1]
scalar c2 = b[1,2]
tw (function y=exp(-x)/((1+exp(-x))^2), range(-10 10)), xline(`=scalar(c1)' `=scalar(c2)', lc(red))

* Note that the density under the curve in the three areas defined by the cutpoints
* is roughly similar to the proportion of individuals who are,
* in order, "not so happy", "pretty happy", and "very happy"
tab rhappy

* Actually, they are not only roughly similar but exactly equal
di invlogit(b[1,1]) // density below the first cutpoint
di invlogit(b[1,2])-invlogit(b[1,1]) // density between the second and first cutpoint
di 1 - invlogit(b[1,2]) // density above the second cutpoint
* the invlogit function is simply invlogit(x) = 1/(1+exp(-x))


/** Ordered Logistic Regression with Single (continuous) Predictor **/

* I think now we are ready to use income as a regressor
ologit rhappy lbinc
predict xb if e(sample), xb
predict p1 p2 p3 if e(sample)

* save coefficients & plot
mat b = e(b)
scalar c1 = b[1,2]
scalar c2 = b[1,3]
tw (connected xb lbinc, mc(red) msize(large)), yline(`=scalar(c1)' `=scalar(c2)', lc(blue) lp(l)) 
* Note that the logistic distribution, as the Normal distribution, is symmetric
* Thus, the plot indicates that even for the lowest income level, less then 50% say they are "not so happy",
* and for respondents with the highest income, less than 50% say they are "very happy"
* Does this mean that the majority of respondents at each income level are predicted to have responded "pretty happy"?
* No! why not?

* Let's look at the predicted probabilities directly
tw (connected p1 p2 p3 lbinc, sort mc(red*.5 blue*.5 green*.5) lc(red*.5 blue*.5 green*.5)), ///
	yscale(range(0,.75)) ylabel(0(.25).75) 
* note:
* by multiplying the colors by numbers,	we can control how "strong" the colors look

* look how this looks likes on the original income scale! Much of the action is at the lower end!
tw (connected p1 p2 p3 income, sort mc(red*.5 blue*.5 green*.5) lc(red*.5 blue*.5 green*.5)), ///
	yscale(range(0,.75)) ylabel(0(.25).75) 
	
* let's add observed responses (we have to do this for all response categories!)
forvalues v=1/3 {
	bysort income: egen O`v' = mean(rhappy==`v') if e(sample)
}
* this is called a "loop" as the local variable v will first take on value 1, then 2, and then 3
* and for each of these iterations, STATA executes the commands within the curly brackets
* Note that if you refer to the local variable v in the loop, you have to use `v' not just v
* The result of the loop is the same as
* bysort coninc: egen O1 = mean(rhappy==1)
* bysort coninc: egen O2 = mean(rhappy==2)
* bysort coninc: egen O3 = mean(rhappy==3)

tw (line p1 p2 p3 income, sort lw(thick thick thick) lc(red*.5 blue*.5 green*.5)) ///
	(scatter O1 O2 O3 income, m(oh dh x) msize(small small small) mc(red*.5 blue*.5 green*.5)), ///
	yscale(range(0,.75)) ylabel(0(.25).75) //

tw (line p1 p2 p3 lbinc, sort lw(thick thick thick) lc(red*.5 blue*.5 green*.5)) ///
	(scatter O1 O2 O3 lb, m(oh dh x) msize(small small small) mc(red*.5 blue*.5 green*.5)), ///
	yscale(range(0,.75)) ylabel(0(.25).75) 

drop p1 p2 p3 O1 O2 O3


/** Multiple Ordered Logistic Regression **/

* finally we are here!
* control for gender, year, marital status, and education
ologit rhappy lbinc i.Female i.mar educ i.year 
* predict probabilities
predict p1 p2 p3 if e(sample)
* we have the same problem as always when trying to plot the results
tw (scatter p1 p2 p3 lbinc, sort ///
	mc(blue red green) msize(small small small))
drop p1 p2 p3 // Doesn't look good ;(

* FOR STATA VER < 14 MARGINS CALCULATES ONLY ONE CATEOGRY ;(
* so we have to install and introduce new commands
findit spost13_ado

* again we first run the model
ologit rhappy lbinc i.Female i.mar educ i.year
* the spost13_ado package has very nice features
* two of them are "mtable" and "mgen"
* Both of these commands use the "margins" routine to 
* generate predictions. However, the package makes these 
* predictions more "user friendly" 
* (note that this implies that you will have less control
* over the options of the margins command)
*
* We will focus especially on the mgen command, but just to show you
* what mtable does, we might run the following:

mtable, at(lbinc=(9(2)17) Female=0 mar=1 year=2012 educ=12) //to make this work, you will have to fill in the parameters
//you need to make good choices

* as you can see, it predicts the "default" outcome (here the predicted probabilities)
* and shows you them in table
* The options are largely the same as in "margins"

* The "mgen" command generates new variables that you can plot
* By specifying the stub(XX) option, we are asking STATA to
* put a XX in front of all variables that are generated by mgen
* This is useful when deleting the variables later on

mgen, at(lbinc=(9(1)17) Female=0 year=2012 educ=12) stub(pred_) //same as the command above, but variables are generated
//so you could easily plot them as you like, the marginal effect of income from 9 - 17 when female, year, educ are fixed 
//at certain points

* the outcome shows that the generated variables, their labels, and some stats

* Now, using these variables, we can generate plots 
tw line pred_pr1 pred_pr2 pred_pr3 pred_lbinc, lc(red blue green)
* As mgen generates also confidence intervals,
* we might plot them, too

#delimit
tw (line pred_pr1 pred_ll1 pred_ul1 
	pred_pr2 pred_ll2 pred_ul2
	pred_pr3 pred_ll3 pred_ul3 pred_lbinc, 
	lc(red red red blue blue blue green green green)
	lp(l - - l - - l - -)
	lw(medium vthin vthin medium vthin vthin medium vthin vthin))
;

* we have used a "delimiter". 
* By starting with "#delimit" we are telling STATA that the
* following command ends when an ";" appears. 
* (Note the comment "delimiter now ;" on the screen which notices you)
* This is convenient when commands become very long
* and you don't want to repeatedly use "///"
* Note that you'll have to run the whole command at once.

* The legend looks repetitive and unnecessarily long
* We can use the "order" option to retain only a few of them
#delimit
tw (line pred_pr1 pred_ll1 pred_ul1 
	pred_pr2 pred_ll2 pred_ul2
	pred_pr3 pred_ll3 pred_ul3 pred_lbinc, 
	lc(red red red blue blue blue green green green)
	lp(l - - l - - l - -)
	lw(medium vthin vthin medium vthin vthin medium vthin vthin))
	,
	legend(label(1 "Not so happy")
		   label(4 "Pretty happy")
		   label(7 "Very happy")
		   order(1 4 7)
		   row(1))
	xscale(range(8.5 17.5)) xlabel(8.5(1.5)17.5)
	note("Dashed lines are 95% pointwise CIs for predictions")
;
* drop the new variables 
drop pred_*
* note:
* When you use "drop pred_*" in STATA, it will drop all variables that
* start with "pred_", so be careful when using "*"!

* We can also let both income and year vary
mgen, at(lbinc=(9(.5)17) mar=0 Female=0 year=(2006 2008 2010 2012) educ=12) stub(pred_) //allow this marginal effect to
//vary across year 2006 - 2012

* Let us plot them
# delimit
tw (line pred_pr1 pred_pr2 pred_pr3 pred_lbinc if pred_year==2006, 
		lc(blue*.5 purple*.5 red*.5) lp(l l l)) 
	(line pred_pr1 pred_pr2 pred_pr3 pred_lbinc if pred_year==2008, 
		lc(blue*.5 purple*.5 red*.5) lp(- - -)) 
	(line pred_pr1 pred_pr2 pred_pr3 pred_lbinc if pred_year==2010, 
		lc(blue*.5 purple*.5 red*.5) lp(dot dot dot)) 
	(line pred_pr1 pred_pr2 pred_pr3 pred_lbinc if pred_year==2012, 
		lc(blue*.5 purple*.5 red*.5) lp(_ _ _))
;
	

* change legend size and position
# delimit ;
tw (line pred_pr1 pred_pr2 pred_pr3 pred_lbinc if pred_year==2006, 
		lc(blue*.5 purple*.5 red*.5) lp(l l l)) 
	(line pred_pr1 pred_pr2 pred_pr3 pred_lbinc if pred_year==2008, 
		lc(blue*.5 purple*.5 red*.5) lp(- - -)) 
	(line pred_pr1 pred_pr2 pred_pr3 pred_lbinc if pred_year==2010, 
		lc(blue*.5 purple*.5 red*.5) lp(dot dot dot)) 
	(line pred_pr1 pred_pr2 pred_pr3 pred_lbinc if pred_year==2012, 
		lc(blue*.5 purple*.5 red*.5) lp(_ _ _))
	, 
	legend(
		size(small)
	)
;
			
* change label of the legends
# delimit ;
tw (line pred_pr1 pred_pr2 pred_pr3 pred_lbinc if pred_year==2006, 
		lc(blue*.5 purple*.5 red*.5) lp(l l l)) 
	(line pred_pr1 pred_pr2 pred_pr3 pred_lbinc if pred_year==2008, 
		lc(blue*.5 purple*.5 red*.5) lp(- - -)) 
	(line pred_pr1 pred_pr2 pred_pr3 pred_lbinc if pred_year==2010, 
		lc(blue*.5 purple*.5 red*.5) lp(dot dot dot)) 
	(line pred_pr1 pred_pr2 pred_pr3 pred_lbinc if pred_year==2012, 
		lc(blue*.5 purple*.5 red*.5) lp(_ _ _))
	,
	legend(
		size(small)
		label(1 2006)
		label(2 2006)
		label(3 2006)
		label(4 2008)
		label(5 2008)
		label(6 2008)
		label(7 2010)
		label(8 2010)
		label(9 2010)
		label(10 2012)
		label(11 2012)
		label(12 2012)
	)
;

* change order of legends
# delimit ;
tw (line pred_pr1 pred_pr2 pred_pr3 pred_lbinc if pred_year==2006, 
		lc(blue*.5 purple*.5 red*.5) lp(l l l)) 
	(line pred_pr1 pred_pr2 pred_pr3 pred_lbinc if pred_year==2008, 
		lc(blue*.5 purple*.5 red*.5) lp(- - -)) 
	(line pred_pr1 pred_pr2 pred_pr3 pred_lbinc if pred_year==2010, 
		lc(blue*.5 purple*.5 red*.5) lp(dot dot dot)) 
	(line pred_pr1 pred_pr2 pred_pr3 pred_lbinc if pred_year==2012, 
		lc(blue*.5 purple*.5 red*.5) lp(_ _ _))
	,
	legend(
		size(small) pos(3)
		label(1 2006) label(2 2006) label(3 2006)
		label(4 2008) label(5 2008) label(6 2008)
		label(7 2010) label(8 2010) label(9 2010)
		label(10 2012) label(11 2012) label(12 2012)
		order(- "{it: Not so Happy}" 1 4 7 10 
			  - "{it: Pretty Happy}" 2 5 8 11 
			  - "{it: Very Happy}" 3 6 9 12)
		subtitle("{bf: Happiness}")
	)
; //this graph would actually look nice if you could make the size of label smaller

* add titles
gen pred_lbinc_exp = 2^(pred_lbinc)/1000

# delimit ;
tw (line pred_pr1 pred_pr2 pred_pr3 pred_lbinc_exp if pred_year==2006, 
		lc(blue*.5 purple*.5 red*.5) lp(l l l)) 
	(line pred_pr1 pred_pr2 pred_pr3 pred_lbinc_exp if pred_year==2008, 
		lc(blue*.5 purple*.5 red*.5) lp(- - -)) 
	(line pred_pr1 pred_pr2 pred_pr3 pred_lbinc_exp if pred_year==2010, 
		lc(blue*.5 purple*.5 red*.5) lp(dot dot dot)) 
	(line pred_pr1 pred_pr2 pred_pr3 pred_lbinc_exp if pred_year==2012, 
		lc(blue*.5 purple*.5 red*.5) lp(_ _ _))
	,
	legend(
		size(small) pos(3)
		label(1 2006) label(2 2006) label(3 2006)
		label(4 2008) label(5 2008) label(6 2008)
		label(7 2010) label(8 2010) label(9 2010)
		label(10 2012) label(11 2012) label(12 2012)
		order(- "{it: Not so Happy}" 1 4 7 10 
			  - "{it: Pretty Happy}" 2 5 8 11 
			  - "{it: Very Happy}" 3 6 9 12)
		subtitle("{bf: Happiness}")
	)
	xtitle("Constant Income (thousands)") ytitle("Predicted Probability")
	xlabel(1.25 2.5 5 10 20 40 80 160, angle(horizontal)) xscale(log) 
;

* drop variables created by mgen
drop pred_*


/** Generalized Ordered Logistic Reg **/

* As the gologit2 command is user-written, you have to download it
findit gologit2
which brant

* first we run the ologit again
//version 14: ologit rhappy lbinc Female year educ //run this and the following command if you are returned an error
//brant, detail 

ologit rhappy lbinc Female year educ

* We run the Brant test, which tests whether the proportional odds (parallel lines) assumption
* is violated
brant, detail
* you see that the test suggests that the assumption is indeed violated the "All" row shows a
* Chi-squared statistic of 47 with 6 degrees of freedom 
* Also, there is especially strong evidence that the parallel lines assumption does not hold for "educ"

findit gologit
* So let us use the gologit command
* you have to use the xi: prefix to use factor variables ..
* pl option specifies that all slopes should be parallel (this is just the same model as the ologit)
xi: gologit2 rhappy lbinc i.Female i.year educ, pl
estimates store pl
* npl options specifies that all constraints on the slopes are relaxed
xi: gologit2 rhappy lbinc i.Female i.year educ, npl
estimates store npl
* you can also relax only "some" of the slopes 
xi: gologit2 rhappy lbinc i.Female i.year educ, npl(educ)
estimates store np_educ
* so, this model assumes that only the "educ" variable has different slopes across categories


* note that the unconstraint model is the most general, but the least parsimonious
* we can compare the fit of these models with the lrtest
lrtest pl np_educ
lrtest np_educ npl
* it seems relaxing the PO assumption for educ is not enough ...
* so let us relax the assumption for income as well
xi: gologit2 rhappy lbinc i.Female i.year educ, npl(educ lbinc)
estimates store np_ei

* LR-tests
lrtest np_educ np_ei
lrtest np_ei npl
* Okay, this suggests that the partial model (where we have relaxed the proportional odds assumption
* for both income and education) is not statistically different from the unconstraint model (
* which relaxes the assumption for all variables)


* let us look at the results again
xi: gologit2 rhappy lbinc i.Female i.year educ, npl(educ lbinc)

*** IT IS IMPORTANT TO NOTE THAT THE INTERPRETATION THAT I HAVE OFFERED YOU ABOVE
*** IN TERMS OF A LATENT VARIABLE WITH LOGISTICALLY DISTRIBUTED ERROR CANNOT BE APPLIED
*** TO THE GENERALIZED ORDERED LOGISTIC REGRESSION MODEL !

*** The best way to interpreted these results is simply to think of running 
*** "two" binary logistic regressions:
***
*** 1) The first regression is on the outcome which 1 if "rhappy" is either {pretty happy} or {very happy}
***    and 0 if rhappy is {not so happy}
*** 2) The second regression is on th outcome which is 1 if "rhappy" is {very happy}
***    and 0 if rhappy is either {not so happy} or {pretty happy}
*** 
*** Note that if you run two separate models, you will get two sets of estimated regression coefficients,
*** i.e., for each variable you get a coefficient from regression 1) and one from regression 2).
*** The generalized ordered logistic regression model constraints some (in our case lbinc and educ) to be
*** the same across these models when estimating their parameters.

* So in the results we see, for example, that both lbinc and educ has a stronger association with the logit
* of moving from {not so happy} into {pretty happy, very happy} then moving from {not so happy, pretty happy}
* to {very happy}

* Just to fix ideas, let us compare the models directly
* Generate two new outcomes as explained above 
gen tempo1 = rhappy>1 if rhappy <.
gen tempo2 = rhappy>2 if rhappy <.
* and run two logistic regressions
logit tempo1 lbinc i.Female i.year educ, nolog nohead
estimates store logit1
logit tempo2 lbinc i.Female i.year educ, nolog nohead
estimates store logit2

* Now compare the results

est tab logit1 logit2

** Unfortunately, the mgen mtable does not work for the gologit2 command 
** So let us do it by hand ;(

* Step 1: First, we get the coefficient vector
xi: qui gologit2 rhappy lbinc i.Female i.year educ, npl
mat b = e(b)
mat list b
scalar b_inc1 = b[1,1]
scalar b_fem  = b[1,2]
scalar b_eu1  = b[1,3]
scalar b_y08  = b[1,4]
scalar b_y10  = b[1,5]
scalar b_y12  = b[1,6]
scalar b_con1 = b[1,7]
scalar b_inc2 = b[1,8]
scalar b_eu2  = b[1,10]
scalar b_con2 = b[1,14]

*fix coefficients to Female=1 and educ=12, year = 2012
gen cc1 = b_con1 + b_fem * (1) + b_eu1 * (12) + b_y12 * (1)
gen cc2 = b_con2 + b_fem * (1) + b_eu2 * (12) + b_y12 * (1)

* Now, recall that the generalized ordered logistic regression can be understood as 
* two logit regressions with outcomes
* 1) {not so} vs. {pretty, very} and 2) {not so, pretty} vs. {very} 
* So, if we use the formula we have used so far, namely, pr(x) = 1/(1+exp(-x)), we will get
* predicted probabilities for the outcomes {pretty, very} and {very}
* Therefore, we have to transform them to get probabilities of each separate outcome, namely,
* {not so}, {pretty}, and {very}

* predicted probability: higher than "not so happy", which is equal to "very happy" or "pretty happy"
gen PVprob = 1/(1+exp(-(cc1 + b_inc1 * lbinc)))
* predicted probability: higher than "pretty happy", which is equal to "very happy"
gen Vprob = 1/(1+exp(-(cc2 + b_inc2 * lbinc)))

* Next, we generate the predcited probabilities for {not so}, {pretty}, and {very}
* The easiest is Prob[{very happy}]
gen p3= Vprob
* Next comes Prob[{pretty happy}] = Prob[{pretty or very}] - Pr[{very}]
gen p2= PVprob - Vprob
* Lastly, as the probabilities have to sum to one, we get Pr[{not so}] = 1 - Pr[{pretty}] - Pr[{very}]
gen p1= 1 - p2 - p3
tw connected p1 p2 p3 lbinc, sort 

