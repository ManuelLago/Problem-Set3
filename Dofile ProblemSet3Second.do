
foreach inst_strength in 0.5 0.15 {

global loops 100 

*Now let´s define all the correlations given in the problem:

global correpsx = 0.4
global correpsz = 0
global correxz = 'inst_strength'

*Correlation matrix:

matrix corr=(1, $correpsx, $correxz \ $correpsx, 1, $correpsz \ $correxz, $correpsz, 1)
local strength = 'inst_strength'

if 'strength' = 0.5 {local id = 1
}

else {local id = 2
}


*Setting the different sample sizes
foreach samp in 50 100 250 1000 {

postutil clear

postfile buffer beta_ols_'samp'_'id' beta_iv_'samp'_'id' using data'samp'_'id'

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
gen beta_ols_'samp'_'id' = _b[x]


ivregress 2sls y (x=z)

gen beta_iv_'samp'_'id' = _b[x]


*Estimating values for each regression
post buffer (beta_ols_'samp'_'id')(beta_iv_'samp'_'id')


drop _all

}

postclose buffer


use data 'samp'_'id', clear

*plot histograms with kernel density

#delimit;
twoway (hist beta_ols_'samp'_'id', legend(label(1 "OLS"))) (hist beta_iv_'samp'_'id', legend(label(2 "IV")))(kdensity beta_ols_'samp'_'id', lcolor(blue) legend(label(3 "Kdensity OLS"))) (kdensity beta_iv_'samp'_'id', lcolor(red) legend(label(4 "Kdensity IV"))), subtitle ("Sample Size='samp'"); 
graph save beta_'samp'_'id', replace; 
#delimit cr
clear

}

grc1leg beta_50_'id'.gph beta_100_'id'.gph beta_250_'id'.gph beta_1000_'id'.gph
graph save beta_ols_iv_cor_'id', replace
graph export beta_ols_iv_cor._'id'pdf, as (pdf) replace
}

