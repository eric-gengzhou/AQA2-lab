****************************************
** Lab 6  							  **
**									  **
** 1) Smoothing (Lowess) 			  **
** 2) Regression Splines			  **
** 3) Information Criteria			  **
**									  **
****************************************


* SETUP 
set maxvar 20000
use GSS_lab_6.dta, clear

/** Smoothing **/

* Let us generate some variables:
* party identification: -3 =strong Democrat up to 3 =strong Republican
gen pid = partyid - 3 if partyid < 7 //centering come into play again, any increase above 0 is republican leaning, vice versa
* log income
gen lbinc = ln(income)/ln(2)
* female
gen female = sex ==2 if sex < .

* race (this is a little bit more complex)
gen lat=hispanic>1 if hispanic<.
replace lat = 1 if hispanic>=. & ethnic==17
replace lat = 1 if hispanic>=. & ethnic==22
replace lat = 1 if hispanic>=. & ethnic==25
replace lat = 1 if hispanic>=. & ethnic==28
replace lat = 1 if hispanic>=. & ethnic==38
replace lat = 1 if hispanic>=. & eth1==17
replace lat = 1 if hispanic>=. & eth1==22
replace lat = 1 if hispanic>=. & eth1==25
replace lat = 1 if hispanic>=. & eth1==28
replace lat = 1 if hispanic>=. & eth1==38
replace lat = 1 if hispanic>=. & eth2==17
replace lat = 1 if hispanic>=. & eth2==22
replace lat = 1 if hispanic>=. & eth2==25
replace lat = 1 if hispanic>=. & eth2==28
replace lat = 1 if hispanic>=. & eth2==38
replace lat = 1 if hispanic>=. & eth3==17
replace lat = 1 if hispanic>=. & eth3==22
replace lat = 1 if hispanic>=. & eth3==25
replace lat = 1 if hispanic>=. & eth3==28
replace lat = 1 if hispanic>=. & eth3==38
lab var lat "Hispanic Heritage"
lab def lat 0 "Other" 1 "Hispanic"
lab val lat lat

recode race 1=1 2=2 3=4
replace race = 3 if lat==1 & race!=2
lab var race "Racial ancestry"
lab def race 1 White 2 Black 3 Latino 4 Other, modify
lab val race race


* let us look into the post-2008 years 
keep if year > 2008

* we might explore the association between pid and education by using loess
tab educ, nol
replace educ =. if educ > 90

tw lowess pid educ, bw(1) 
* so it seems that up to 6 years, pid remains stable at -.6 (slightly Democratic leaning),
* then from 6 to 15 years, individuals become, on average, more Republican,
* and after a BA, they become more Democrat
tw lowess pid educ, bw(2) 
tw lowess pid educ, bw(5) // not much change by tweaking the bandwidth

* overall the association between education and pid is relative weak considering that
* pid ranges from -3 to 3
tw (scatter pid educ, m(oh) mc(blue) msize(small)) ///
	(lowess pid educ, bw(1)), ylabel(-3(1)3) xlabel(0(4)20)
* note that the scatterplot doesn't really show what's going on in the data
* this is because pid and educ are both actually "discrete" variables (although we treat them as continuous)
* so no matter how many points are concentrated in one spot, STATA will show only one point

* to remedy this, we might use the jitter option	
tw (scatter pid educ, m(oh) mc(blue) msize(vsmall) jitter(4)) ///
	(lowess pid educ, bw(1)), ylabel(-3(1)3) xlabel(0(4)20)
* now, we see that most of the points are concentrated at education year 12 and 16	

* you can also save the smoothed curve using the "gen" option
lowess pid educ, bw(1) gen(spid)
summ spid pid if spid!=.// compare the two, you will notice the similarity





*Question, why the two have different num. of observations?
br spid pid educ if pid !=. & spid ==. //always check for what is called the local missing values






* Let us compare the nonparametric smooth with parametric curves	
reg pid c.educ // linear fit
predict yhat_linear if e(sample)
reg pid c.educ##c.educ // quadratic fit 
predict yhat_quad if e(sample)
reg pid c.educ##c.educ##c.educ // cubic fit
predict yhat_cubic if e(sample)
reg pid c.educ##c.educ##c.educ##c.educ // quartic fit, not significant

* comparing these models to the nonparametric smooth, we get
# delimit
tw  (line yhat_linear educ, sort) 
	(line yhat_quad educ, sort lc(blue)) 
	(line yhat_cubic educ, sort lc(green)) 
	(line spid educ if e(sample), lc(red) sort), 
	legend( 
		   label(1 "linear") 
		   label(2 "quadratic") 
		   label(3 "cubic")
		   label(4 "smoothed")
	)
;

lab def pid -3 "Strong Democrat" -2 "Democrat" -1 "Democrat Leaning" 0 "Independent" ///
		1 "Republican Leaning" 2 "Republican" 3 "Strong Republican"
lab val pid pid

# delimit
tw  (scatter pid educ, m(oh) mc(blue*.25) msize(vsmall) jitter(4)) 
	(line yhat_linear educ, sort) 
	(line yhat_quad educ, sort lc(blue)) 
	(line yhat_cubic educ, sort lc(green)) 
	(line spid educ if e(sample), lc(red) sort), 
	legend(pos(6) row(2) symxsize(small) size(small) holes(4)
		   label(1 "Observed (Jittered)")
		   label(2 "Linear Fit") 
		   label(3 "Quadratic Fit") 
		   label(4 "Cubic Fit")
		   label(5 "Lowess")
		  )
	ylabel(-3(1)3, labels)
	xlabel(0(4)20, angle(horizontal))
;

	

/** Controling for other variables **/

* We might do the same exercise, but this time while controling for other variables
* first we drop the predicted values
drop yhat_*

* Next we run the regressions with age, gender, income, and race as controls
reg pid c.educ c.age female lbinc i.race // linear fit
* Let's store these estimates
estimates store linear

* To get predictions for education we have to hold constant other variables at certain values
* here we set race to "white," gender to "male" and the rest to their mean
* (the choice of these categories is because they are the baseline categories for the dummies)
*
* THE FOLLOWING CODE MIGHT BE A LITTLE BIT COMPLEX. BUT BECOMING USED TO THESE KIND OF THINGS
* WILL BE VERY HELPFUL, WHEN YOU ARE PROGRAMMING IN STATA (AND ANY OTHER CODING LANGUAGE)

* So, first we save the coefficients into a matrix
mat B = e(b)
* we might look into the saved matrix
mat list B

* next we define some "local variables"
* local variables are variables (which hold usualy only a number or a string) that disappear after the execution of a command
* Thus the next chunk of code has to be executed in one run.

* If you want to refer to a local variable in the code, you have to enclose the name of the variable in ` and '.
* for example if you want refer to the local variable x, you have to use `x' in your code.
* Also, some commands in stata work only for strings. So, if you want to convert a number that is stored in a
* local variable into a string, you will need to write "`x'".
* you can search "help local" in stata for more details

* Lastly, after you run the command "summarize" in STATA, STATA automatically saves some values such as the mean, min, and max
* into local variables, which you can access after running the command.
* For example, you might type
*
* 		sum age
*		return list
*
* to see which values are saved under what names. The list will also show that the mean of "age" is saved in a scalar
* named r(mean). After running the summaize command, you can therefore use these values for other computations.
* From the example above, you might type
*
*		sum age
*		display r(mean)
*
* which will show you the mean of age
*
*
* OKAY, now, here is the code we want to run:

* first we define a local variable "cons_sum" and set its value to zero
local cons_sum = 0
* The "foreach" command is a looping command:
* at each iteration of the loop, age and lbinc (in turn) are substituted into the local variable `v'
foreach v of varlist age lbinc {
	* first we quietely summarize `v' (the "mean" option tells STATA to summarize only the mean)
	qui sum `v', mean
	* we define a new local variable that contains the mean of `v' 
	* the name of this local variable will be mean_`v', e.g., if v is "age", the name of the local 
	* variable is thus "mean_age"
	local mean_`v' = r(mean)
	* we display the mean
	di "mean of variable `v' : `mean_`v''"
	* (note that we have used `mean_`v'' to refer to the new local variable)
	* we extract the column number of B that corresponds to the variable `v' and save into `z'
	* This is simply the coefficient for `v' in the previous regression
	local z = colnumb(B, "`v'")
	* we multiply this coefficient by the mean of `v'
	local cons_sum = `cons_sum' + B[1,`z'] * `mean_`v''
}
* lastly, we add the constant to `cons_sum'
local z = colnumb(B, "_cons")
local cons_sum = `cons_sum' + B[1,`z']

* Now, what is contained in `cons_sum' is the follwing:
*
* cons_sum = constant + coef_age * mean_age + coef_lbinc * mean_lbinc + coef_female * 0 + coef_race * 0
*
* In other words, cons_sum contains the predicted outcome for individuals with
* average age, average income, who are male, and who are white (and for which educ is zero)
*
* Now, the last step is to let education vary to get predictions of how the outcome changes with education
* we generate the prediction of the outcome (yhat_linear),
* by generating the variable
gen yhat_linear = `cons_sum' + B[1,1]*educ if e(sample)

* This variable represents, therefore, the predicted change in the outcome (pid) when 
* age and income is hold constant at their mean, and female and race is set, respectively, to "male" and "white"



** quadratic fit **
reg pid c.educ##c.educ age female lbinc i.race // quadratic fit 
* again we store the estimates
estimates store quadratic
* and use the same routine to get predictions
mat B = e(b)
mat list B

local cons_sum = 0
foreach v of varlist age lbinc {
	qui sum `v', mean
	local mean_`v' = r(mean)
	local z = colnumb(B, "`v'")
	local cons_sum = `cons_sum' + B[1,`z'] * `mean_`v''
}
local z = colnumb(B, "_cons")
local cons_sum = `cons_sum' + B[1,`z']
* NOTE: now we have to use both the linear term and the quadratic term!!
local l = colnumb(B, "educ")
local q = colnumb(B, "c.educ#c.educ")
gen yhat_quadratic = `cons_sum' + B[1,`l']*educ + B[1, `q']*educ^2 if e(sample)

** cubic fit **
reg pid c.educ##c.educ##c.educ age female lbinc i.race // cubic fit
estimates store cubic

mat B = e(b)
mat list B

local cons_sum = 0
foreach v of varlist age lbinc {
	qui sum `v', mean
	local mean_`v' = r(mean)
	local z = colnumb(B, "`v'")
	local cons_sum = `cons_sum' + B[1,`z'] * `mean_`v''
}
local z = colnumb(B, "_cons")
local cons_sum = `cons_sum' + B[1,`z']
* NOW WE NEED THE LINEAR, QUADRATIC, AND CUBIC TERM
local l = colnumb(B, "educ")
local q = colnumb(B, "c.educ#c.educ")
local c = colnumb(B, "c.educ#c.educ#c.educ")
gen yhat_cubic = `cons_sum' + B[1,`l']*educ + B[1, `q']*educ^2 + B[1,`c']*educ^3 if e(sample)

* comparing these models, we get
# delimit
tw  (line yhat_linear educ, sort) 
	(line yhat_quad educ, sort lc(blue)) 
	(line yhat_cubic educ, sort lc(green)), 
	legend(
		   label(1 "linear") 
		   label(2 "quadratic") 
		   label(3 "cubic")
	) 
;


* To get a "feeling" of how the pattern in the data look like after controlling for a set of variables,
* we might also use lowess on two sets of residuals: 
* 1) residuals after regressing pid on the controls, and
* 2) residuals after regressing educ on the controls

* The procedure is as follows:

reg pid age female lbinc i.race // regress outcome on controls
predict e_cont if e(sample), res // save residuals
reg educ age female lbinc i.race // regress education on controls
predict e_educ if e(sample), res // save residuals
lowess e_cont e_educ, bw(1) nograph gen(res_smooth) // Run Lowess

* Now, looking at the pattern ...
tw line res_smooth e_educ,sort
tw (scatter e_cont e_educ, mc(blue*.25) m(oh) msize(vsmall)) ///
	(line res_smooth e_educ, sort)
* it seems that the first part of the lowess curve is estimated with only a few observations.

* (I HAVE ADDED A SHORT DISCUSSION AT THE END OF THE DO-FILE TO EXPLAIN, INTUITIVELY, WHY THIS PROCEDURE WORKS)

* Notice that both the x and the y-axis are residuals and not the original scales.
* So, let us also look how the smoothed residuals relate to education
tw scatter res_smooth educ, mc(blue*.5) m(oh) msize(vsmall) jitter(2)
* This plot is only "suggestive" and "might" be misleading since it does not take into account
* the correlation between educ and the control variables
* Still, it let's us guess where the major changes happens

* It appears that the major change in the relationship between pid and education occurs
* either at 12 or 16 of the education scale
* Let's try out splines to mimic the curvature of the function
* first we start with an additional `boost' for those who have more a hs diploma
gen educ_from12 = 0
replace educ_from12 = educ - 11 if educ > 11 & educ <.
* a look how the additional variable is defined
scatter educ_from12 educ

* Now, let's run the regression and look at the results
reg pid educ educ_from12 age female lbinc i.race 
* the slope for education is .0197374 for those with education lower than 12
* for those with more than a hs diploma the slope is .0197374 + ( -.0814923)* (educ - 11)
* The significant coefficient on from_12 shows us that there is indeed an additional (negative) boost
* after 12 years of education.

* lastly, let us store the estimates of the models
estimates store from_12

* Next, we run the same model for a change in the slopes after obtaining a BA
gen educ_from16 = 0
replace educ_from16 = educ - 15 if educ > 15 & educ < .
* run regression
reg pid educ educ_from16 age female lbinc i.race
estimates store from_16

* We can also get the predictions of the model using the code from above:
mat B = e(b)
mat list B

local cons_sum = 0
foreach v of varlist age lbinc {
	qui sum `v', mean
	local mean_`v' = r(mean)
	local z = colnumb(B, "`v'")
	local cons_sum = `cons_sum' + B[1,`z'] * `mean_`v''
}
local z = colnumb(B, "_cons")
local cons_sum = `cons_sum' + B[1,`z']

local base = colnumb(B, "educ")
local boost = colnumb(B, "educ_from16")
gen yhat_spline16 = `cons_sum' + B[1,`base']*educ + B[1, `boost']*educ_from16 if e(sample)

* The prediction looks something like...

tw line yhat_spline16 educ, sort ylabel(-.5(.25).5) 


* A last complication when running all these models is to decide which model to choose as the final model
* For the models that we've run so far, these might be some guidelines ...
* 1) note that the linear model and the splines are "nested". 
*    Thus a significant coefficient on the "boosting" term indicates that
*    its inclusion is statistically justified.
*    Also, the linear, quadratic, and cubic functions are nested as well.
*    So, again, a significance test is a good way to go.
* 2) models with different spline functions, or with the cubic term
*    are, however, not nested. In these situations,
*    information criteria such as BIC or AIC are often used.

* So, as both spline functions are significant, 
* and the cubic term in the cubic regression is significant as well, 
* we can rule out the linear and the quadratic model

* To select a model among the remaining ones, we might use
estimates stats cubic from_12 from_16
* Lower AIC and BIC values are "better". So, it seems that the spline with BA-boost seems to be prefered 


** WHY RUNNING REGRESSIONS OF RESIDUALS ON RESIDUALS?

* To get a "feeling" of how the pattern in the data look like after controlling for a set of variables,
* we might use lowess two sets of residuals.
* The procedure is as follows:

drop e_cont e_educ res_smooth

reg pid age female lbinc i.race if !missing(educ) // regress outcome on controls
* we have to specify !missing(educ) so that we use the same observations in both regressions
predict e_cont if e(sample), res // save residuals
reg educ age female lbinc i.race if !missing(pid) // regress education on controls
predict e_educ if e(sample), res // save residuals
lowess e_cont e_educ, bw(1) gen(res_smooth) // run loess on both residuals and save predictions

tw line res_smooth e_educ,sort
tw (scatter e_cont e_educ, mc(blue*.25) m(oh) msize(vsmall)) ///
	(line res_smooth e_educ, sort)


/** Why does this work? **/

* Intuitively, the very concept of "controlling for" or "holding constant" control variables
* is nothing else than "partialling out" the effect of these variables 
* from "both" 1) the focal predictor and 2) the outcome
* 
* The residual of a regression, on the other hand, "is" the variation of the outcome that remains
* after the "effect" of the predictors are paritalled out
* 
* Hence, if we residualize both the outcome and the focal predictor
* and regressing these residuals on one another, we recover the origian regression coefficient
* from a multiple regression!
*
* Or, at least, this is the intuition...
* For a more detailed explanation, you might search for the Frisch-Waugh Theorem.
* (The theorem should not be too technical if you are familiar with basic linear algebra..)

* Just to demonstrate that it works, you might try the following:
* regress pid on education and controls, and save coefficient in a scalar
reg pid educ age female lbinc i.race
mat B = e(b)
scalar educ_b = B[1,1]

* regress pid on controls, save residuals 
qui reg pid age female lbinc i.race if !missing(educ)
predict e1 if e(sample), res
* regress education on controls, save residuals
qui reg educ age female lbinc i.race if !missing(pid)
predict e2 if e(sample), res
* regress residuals of pid on residuals on education, save coefficient in scalar
qui reg e1 e2
mat B=e(b)
scalar educ_res = B[1,1]
* compare the two coefficients
di educ_b
di educ_res
* Voila!
