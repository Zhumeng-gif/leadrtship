cd "/Users/zhumeng/Desktop/Spring semester_2023/leadership/replication"
use "/Users/zhumeng/Desktop/Spring semester_2023/leadership/replication/worldleaders.dta"

rifle growth leader time unit, permnum(1000) report(50)  nostitch

* Endourages Test*
**This code creates a program called "retroriflesimple" that is used later**
program retroriflesimple, rclass
	version 14
	set more off
	set matsize 5000
	syntax varlist(min=4 max=4) [if], [Iterations(integer 100) Permnum(integer 19) Report(integer 10) SAVING(string) REPLACE noSTITCH noGRAPH GROWTH] 
	tokenize `varlist'
	local permutations = `permnum'
	local iter = `iterations'
	local reportevery = `report'
	local outcome `1'
	local leader `2'
	local period `3'
	local unit `4'
	preserve
	keep `outcome' `unit' `period' `leader'
	if "`growth'" == "growth" {
	sort `unit' `period'
	tempvar growth
	quietly gen `growth' = (`outcome' - `outcome'[_n-1])/`outcome'[_n-1] if `unit' == `unit'[_n-1] & `period' == `period'[_n-1] + 1
	local outcome `growth'
	}
sort `unit' `period'
tempvar turnover
gen `turnover' = `leader'[_n+1] != `leader'  
replace `turnover' = . if `unit'[_n+1] != `unit'
replace `turnover' = . if  `period'[_n+1] != `period' + 1
tempvar intercept
quietly: sum `turnover'
scalar `intercept' = r(mean)
tempvar std_outcome
egen `std_outcome' = std(`outcome')
quietly: areg `turnover' i.`period' if `std_outcome' != ., absorb(`unit')
	local r2baseline = e(r2)
quietly: areg `turnover' `std_outcome' i.`period', absorb(`unit')
	local slope = _b[`std_outcome']
	local se_slope = _se[`std_outcome']
	local r2change = e(r2) - `r2baseline'
tempvar unit_num
egen `unit_num' = group(`unit')
tsset `unit_num' `period'
tempvar laggy
gen `laggy' = l.`std_outcome'
quietly: reg `std_outcome' `laggy' i.`period'
	local serial = _b[`laggy']
scalar significant = 0
scalar successful = 0
while successful < `iterations' {
quietly {
tempvar seed
gen `seed' = invnormal(uniform())
tempvar simoutcome
sort `unit' `period'
gen `simoutcome' = `seed' if `unit' != `unit'[_n-1]
replace `simoutcome' = `simoutcome'[_n-1]*`serial' + `seed'*(1 - `serial') if `unit' == `unit'[_n-1]
tempvar simturnover
gen `simturnover' = 0 if `unit' != `unit'[_n-1]
replace `simturnover' = uniform() < `intercept' + `slope'*`simoutcome'[_n-1]*-1 if `unit' == `unit'[_n-1]
tempvar simleader2
sort `unit' `period'
gen `simleader2' = 1 if `unit' != `unit'[_n-1]
replace `simleader2' = `simturnover' + `simleader2'[_n-1] if `unit' == `unit'[_n-1]
tempvar simleader
egen `simleader' = group(`unit' `simleader2')
tempvar sdleader
egen `sdleader' = sd(`simleader'), by(`unit')
sum `sdleader'
if r(mean) > 0 {
	scalar successful = successful + 1
rifle `simoutcome' `simleader' `period' `unit', nograph permnum(`permutations') nostitch
if pval < .05 {
	scalar significant = significant + 1 
	}
	}
drop `sdleader' `simleader' `simleader2' `simturnover' `seed' `simoutcome'		
	}
disp "Iterations Completed: " successful
	}
disp "Avg. Turnover Rate: " `intercept'
disp "Estimated Slope: " `slope'
disp "Estimated Std Error of Slope: " `se_slope'
disp "Change in r-squared: " `r2change'
disp "Estimated Serial Correlation: " `serial'
disp "Estimated False Discorvery Rate: " significant/`iterations'
	return scalar avgturn = `intercept'
	return scalar slope = `slope'
	return scalar scorr = `serial'
	return scalar r2change = `r2change'
	return scalar fdr =  significant/`iterations'
	end
	

***Conducting the Simulations***
clear
set more off
postutil clear
local iterations = 200
local permutations = 19
local report = 10
postfile Results str30(datetime setting outcome_sample) intercept slope r2change sercorr frr using "ER_Results.dta", replace

*World Leaders
foreach i in auth {
clear
set seed 6063
use "worldleaders.dta"
keep if `i' == 1
retroriflesimple growth leader time unit, iterations(`iterations') permnum(`permutations') report(`report') nograph
post Results ("$S_TIME  $S_DATE") ("World Leaders") ("`i'") (r(avgturn)) (r(slope)) (r(r2change)) (r(scorr)) (r(fdr)) 
noisily: disp "$S_TIME  $S_DATE WorldLeaders `i'"
}
postclose Results

***Displaying the Results***
clear
use "ER_Results.dta"
foreach i of varlist intercept-frr {
tostring `i', replace format(%5.3f) force
replace `i' = subinstr(`i', "0.", ".", .)
}
split datetime
drop datetime
g datetime = datetime4 + " " + datetime3 + " " + datetime2 + " " + datetime1
replace setting = "1. World Leaders" if setting == "World Leaders"
sort setting datetime




*Conducting the leader Power which will be used in leader effect size*

**This is code for a program called "riflepower" which will be used later**
program riflepower, rclass
	version 14
	set more off
	set matsize 5000
	syntax varlist(min=3 max=3) [if] , Effects(numlist >=0 <1 min=1) [Iterations(integer 100) Permnum(integer 19) Report(integer 10) SAVING(string) REPLACE noSTITCH noGRAPH GROWTH] 
	tokenize `varlist'
	local permutations = `permnum'
	local iter = `iterations'
	local reportevery = `report'
	local leader `1'
	local period `2'
	local unit `3'
	preserve
	keep `unit' `period' `leader'
tempname powerhold
tempfile Power
postfile `powerhold' magnitude iteration pval diff using "`Power'", replace
forvalues k = 1/`iterations' {
quietly {
tempvar leadereffect
gen `leadereffect' = invnormal(uniform())
bysort `leader': replace `leadereffect' = `leadereffect'[1]
tempvar noise
g `noise' = invnormal(uniform())
foreach magnitude of numlist `effects' {
local amplification = `magnitude'/(1 - `magnitude')
tempvar outcome
g `outcome' = `noise' + `leadereffect'*`amplification'
rifle `outcome' `leader' `period' `unit', permnum(`permutations') nograph `growth' `stitch' report(10000)
post `powerhold' (`magnitude') (`k') (pval) (diff)
drop `outcome' 
}
drop `leadereffect' `noise' 
}
if `k'/`reportevery' == round(`k'/`reportevery') {
disp "iterations completed:" `k'
}
}
postclose `powerhold'
clear
use "`Power'"
if ("`saving'"!="") {
	save `saving', `replace'
	}
sort magnitude iteration
g significant = pval < .05
egen power = mean(significant), by(magnitude)
egen meandiff = mean(diff), by(magnitude)
sort magnitude
drop if magnitude == magnitude[_n-1]
keep magnitude power meandiff		
end

***Conducting the Power Simulations***

clear
display "$S_DATE $S_TIME"
foreach i in worldleaders{ 
clear
set seed 0528
use "`i'.dta"
riflepower leader time unit, effects(0(.05).3) iterations(100) permnum(19) report(10) nostitch saving(powerresults_`i') replace
clear
use "powerresults_`i'.dta"
g significant = pval < .05
collapse significant diff, by(magnitude) 
rename significant p_`i'
rename diff d_`i'
sort magnitude
save "power_`i'.dta", replace
display "$S_DATE $S_TIME"
}



**Conducting the leader effect size**
clear
postutil clear
local permutations = 1000
local report = 50
postfile Results str20(setting outcome_sample) r2 avg diff pval prop using "ResultsTable.dta", replace
*World Leaders
foreach i in auth {
clear
set seed 60637
use "worldleaders.dta"
keep if `i' == 1
rifle growth leader time unit, permnum(`permutations') report(`report') nostitch nograph saving("results_worldleaders_`i'.dta") replace
if diff <= 0 {
	scalar effect = 0
	}
if diff	> 0 {
	clear
	use "power_worldleaders.dta"
	sum d_
	if diff < r(min) {
		scalar effect = 0
		}
	if diff > r(max) {
		scalar effect = .3
		}
	else {
		g bigger = d_ > diff
		g endpoints = bigger == 1 & bigger[_n-1] == 0 | bigger == 0 & bigger[_n+1] == 1
		reg magnitude d_ if endpoints == 1
		scalar effect = _b[_cons] + _b[d_]*diff
		}
	}
post Results ("World Leaders") ("`i'") (realr2) (avg) (diff) (pval) (effect)
}
postclose Results
clear
use "ResultsTable.dta"
foreach i of varlist r2-prop {
tostring `i', replace format(%5.3f) force
replace `i' = subinstr(`i', "0.", ".", .)
}
save "ResultsTable_PoliticalLeaders.dta", replace
