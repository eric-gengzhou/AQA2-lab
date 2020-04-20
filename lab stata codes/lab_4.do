**lab 4
**basic logit model diagnostics and model selection
clear all 
use GSS_lab_2.dta, clear

//goodness-of-fit test
gen ab = 1 if abany ==1
replace ab = 0 if abany ==2
logit abany sex educ age
lfit, group(10) table

fitstat //comprehensive stats of goodness-of-fit

logit abany sex educ age //run this model 1
fitstat, saving(m1) //save model for later comparison

logit abany sex educ age i.polviews i.wrkstat i.year
fitstat, using(m1) force //the basis for model selection and comparison is always that you have the same observation(same data)

//multi-collinearity
gen multi_co = age * educ //artificially creating a multi-collinear variable
logit abany sex educ age i.polviews i.wrkstat i.year multi_co 

lfit, group(10) //this command shows that the model can have a good fit even if multi-collinear

collin sex educ age polviews wrkstat year multi_co // command for multi-collinearity checking, VIF >= 10 should warn you

//multinomial logit model
mlogit polviews sex educ age i.year //clearly this model wont converge, mlogit takes out large computation power and sometimes it does not work

tab polviews
tab polviews, nol
mlogit polviews sex educ age, base(1) //set base to being extremely liberal
