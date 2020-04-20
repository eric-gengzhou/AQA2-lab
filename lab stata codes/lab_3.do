***Lab 3 

**simulating data for demonstration
**RCT of a labor training program
**outcome is income, and chance of employment in the original study

clear all 

matrix a = (1, 0.7, 0.8\ .7, 1, .2\.8, .2, 1) //covariance matrix
matrix b = (50000, 40, 1000) //mean
matrix c = (12000, 8, 200) //standard errors
drawnorm income age test_score, means(b) sd(c) corr(a) n(10000) seed(1234)

replace test_score = round(test_score)
replace age = round(age, .1)

gen treat = runiform(0,1) 
replace treat = 1 if treat>=.5
replace treat = 0 if treat<.5

gen coffee = rbinomial(10000, .4) //whether someone likes coffee
replace coffee = coffee>4000

gen error = rnormal()
gen epsilon = rnormal(0,5)

replace test_score = test_score + error

replace income = income + 3456*treat + epsilon //data generating process

reg income treat, r //model without any covariates
estimate store m1
reg income treat age test_score, r //model with all the covariates
estimate store m2
reg income treat age test_score coffee, r //model with all covariate plus an unrelevant variable
estimate store m3

estimate table m1 m2 m3, stats(N r2_a) b se p

**PS score matching
clear all 

matrix a = (1, 0.7, 0.8\ .7, 1, .2\.8, .2, 1)
matrix b = (50000, 40, 1000)
matrix c = (12000, 8, 200)
drawnorm income age test_score, means(b) sd(c) corr(a) n(10000) seed(1234)

replace test_score = round(test_score)
replace age = round(age, .1)

gen treat = runiform(0,1) 
replace treat = 1 if treat>=.5
replace treat = 0 if treat<.5

gen coffee = rbinomial(10000, .4) //whether someone likes coffee
replace coffee = coffee>4000

gen error = rnormal()
gen epsilon = rnormal(0,5)

replace test_score = test_score + error

gen skill = rnormal(50,10)
gen educ = rnormal(15,5)
gen marital = runiform(0,1)
replace marital = 1 > .3 & age >= 20

replace skill  = skill*1.25 if treat == 1
replace educ = educ + 5 if treat == 1

replace income = income + 3456*treat + epsilon + 200*skill + 25000*marital + 300*educ

psmatch2 treat skill educ marital age test_score coffee
reg income treat [pw= _pscore]
estimate store ps1
reg income treat skill educ marital age test_score coffee [pw= _pscore] //this is the only unbiased estimate
estimate store ps2

psmatch2 treat educ marital age test_score coffee //the two models below are biased due to omitted variable bias -- skill was left out
reg income treat [pw= _pscore]
estimate store ps3
reg income treat educ marital age test_score coffee [pw= _pscore] //the treatment effect is biased if you omit any relevant variables despite high R^2
estimate store ps4

estimate table ps1 ps2 ps3 ps4, stats(N r2_a) b se p




