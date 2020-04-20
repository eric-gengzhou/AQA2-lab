*AQAII - Spring 2020
*lab 8 


*** An illustration of direct/indirect effects

clear all 

matrix a = (1, 0, 0\ 0, 1, 0\0, 0, 1) //covariance matrix
matrix b = (50, 100, 20) //mean
matrix c = (20, 40, 5) //standard errors
drawnorm x y z, means(b) sd(c) corr(a) n(1000000) seed(12)

gen epis_1 = rnormal()
gen epis_2 = rnormal()
gen xi_1 = rnormal()

*another way to look at the data structure
corr x y z, means covariance 
pwcorr x y z, sig


*set beta_1 = .5, gamma = .8, alpha = .2
gen Z = 5 + .2*x + epis_2
gen Y_1 = 10 + .5*x + .8*Z + epis_1
reg Z x
predict res_1, res //to create the residualized Z, and this res_1 is basically the same as epis_2
estimates store m1  
reg Y_1 x //coef is biased, omitted variable bias 
estimates store m2
reg Y_1 x Z //coefs are not biased, as we include all relevant regressors 
estimates store m3
reg Y_1 x res_1 //coef is biased again when we partion Z out
estimates store m4

estimate table m1 m2 m3 m4, stats(N r2_a) b(%7.2f) se(%7.3f) //%7.2f means report only 2 digits of the estimate 

*************************************
*total effect = beta_1 + alpha*gamma 
*direct effect = beta_1
*indirect effect = alpha*gamma 
*effect proportion = beta_1 / (beta_1 + alpha*gamma) 
*************************************


*** An illustration of the omitted variable bias 

* a. when regressors are not correlated with the omitted variable
gen Y_2 = 10 + .5*x + .8*Z + 1.5*y + epis_1

reg Y_2 x Z //coefs are not biased, why? 
estimate store o1
reg Y_2 x Z y //coefs are not biased, but a good change happened too, what is it?
estimate store o2

* when regressors are correlated with the ommited variable
gen Y = .1*x + .1*Z + epis_1 + 1 + y
gen Y_3 = 10 + .5*x + .8*Z + 1.5*Y + epis_1

reg Y_3 x Z // coefs are biased, why? 
estimate store o3
reg Y_3 x Z Y //coefs are not biased, why? 
estimate store o4

estimate table o1 o2 o3 o4, stats(N r2_a) b(%7.2f) se(%7.5f)



