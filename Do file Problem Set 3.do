

*need to install grc1leg via findit

*RUN AT LEAST 10,000 simulations
*a) Sampling distribution under strong instruments. 
* Plot the sampling distributions for OLS and IV for all four sample sizes.

*Run for strength 0.5 for a) and 0.15 for b)


foreach correxz in 0.5 {

global loops 100 

*Now let´s define all the correlations given in the problem:

global correpsx = 0.4
global correpsz = 0
global correxz = 0.5

*Correlation matrix:

matrix corr=(1, $correpsx, $correxz \ $correpsx, 1, $correpsz \ $correxz, $correpsz, 1)

*Setting the different sample sizes
foreach samp in 50 100 250 1000 {

postutil clear

postfile buffer beta_ols_'samp' beta_iv_'samp' using data'samp'

*Set number of observations

local obs = 'samp'
*let´s define now the loop for different values of x, epsilon and z
qui forvalues i=1/$loops {


drawnorm x epsilon z, n('obs') corr(corr)

 gen alpha=0
 gen gamma=0
 gen beta=1
 
 gen y = alpha + beta*x + epsilon

reg y x
gen beta_ols_'samp' = _b[x]


ivregress 2sls y (x=z)

gen beta_iv_'samp' = _b[x]


*Estimating values for each regression
post buffer (beta_ols_'samp')(beta_iv_'samp')


drop _all

}

postclose buffer


use data 'samp', clear

*plot histograms with kernel density

#delimit;
twoway (hist beta_ols_'samp', legend(label(1 "OLS"))) (hist beta_iv_'samp', legend(label(2 "IV")))(kdensity beta_ols_'samp', lcolor(blue) legend(label(3 "Kdensity OLS"))) (kdensity beta_iv_'samp', lcolor(red) legend(label(4 "Kdensity IV"))), subtitle ("Sample Size='samp'"); 
graph save beta_'samp', replace; 
#delimit cr
clear

}

grc1leg beta_50.gph beta_100.gph beta_250.gph beta_1000.gph
graph save beta_ols_iv_cor, replace
graph export beta_ols_iv_cor.pdf, as (pdf) replace
}




