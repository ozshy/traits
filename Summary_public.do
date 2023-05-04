
*Written by Ruth Cohen, 12.13.22
*Summarize the big 5 variables overall and by credit card debt, test for differences between groups
*exports results to Excel file titled summary where tables are then formatted using VLOOKUP function

clear all
set more off
set maxvar 30000


*set directory:
cd "[SET DIRECTORY HERE]"

*directory globals
global d_path "/[DIRECTORY]/data/"
global sum_path "/[DIRECTORY]/summary/"
global reg_path "/[DIRECTORY]/regressions/"
global fig_path "/[DIRECTORY]/figures/"

*demographic and financial globals
global age_cat "age_u25 age_2534 age_3544 age_4554 age_5564 age_o65 "
global education "edu_lhs edu_hs edu_sc edu_c edu_pgs"
global income "inc_lt25 inc_2549 inc_5074 inc_7599 inc_gt100"
global sex "male female"
global latino "latino nonlatino"
global race "white black asian other"
global homeownership "homeowner non_homeowner"
global employment "employed unemployed retired disabled_other" 
global marital_status "married separated divorced widowed never_married"
global environment "env_rural env_mixed env_urban"
global fico "fico_lt600 fico_600649 fico_650699 fico_700749 fico_750800 fico_gt800"
global fin_hardships "cc_froz_12mos lostjob_12mos bankruptcy_12mos foreclosure_12mos payday_12mos pawn_12mos rent2own_12mos refundloan_12mos autotiloan_12mos badloans_12mos"
global emergency  "e_exp_tot_saved eexpsaved_500 eexpcover_2000 eexplassets_2000" 
global balances "end_cash_bal"
global fin_lit "finlitscore_3"
global big5 "extroversion agreeableness conscientiousness neuroticism openness"
global demosfinancial age $age_cat $education $income $sex $latino $race $homeownership $employment $marital_status $environment $fico $fin_hardships $balances $fin_lit $big5 $emergency 

*load data
use "${d_path}/Traits_and_financial_behavior_data_public.dta", clear


*EXCLUDE THE OBS NOT PART OF REPRESENTITIVE SAMPLE
drop if mi(ind_weight)

*revolver 
gen rev_12mos = pu009
gen no_rev_12mos = 1 if rev_12mos ==0
replace no_rev_12mos = 0 if rev_12mos ==1 
gen rev_lastmo = 1 if pu010 > 0 & !mi(pu010)
replace rev_lastmo = 0 if pu010 == 0 
replace rev_lastmo = 0 if rev_12mos ==0
gen no_rev_lastmo =1 if rev_lastmo ==0
replace no_rev_lastmo = 0 if  rev_lastmo ==1
gen rev_bal_lastmo = pu010

*unbanked 
gen unbanked= bnk_acnt_adopt==0 & !mi(bnk_acnt_adopt)
replace unbanked = . if mi(bnk_acnt_adopt)
gen no_unbanked = bnk_acnt_adopt
replace chk_bal = . if unbanked ==1

*cc adopters
gen no_cc_adopt = 1 if cc_adopt == 0 
replace no_cc_adopt = 0 if cc_adopt ==1

*generate demographic variables

*age
gen age_cat = 1 if age<25 & !missing(age)
replace age_cat = 2 if age>=25 & age <35
replace age_cat = 3 if age>=35 & age <45
replace age_cat = 4 if age>=45 & age <55
replace age_cat = 5 if age>=55 & age <65
replace age_cat = 6 if age>=65 & !missing(age)

gen age_u25 = age<25 & !missing(age)
replace age_u25 = . if mi(age)
gen age_2534 = age>=25 & age <35
replace age_2534 = . if mi(age)
gen age_3544 = age>=35 & age <45
replace age_3544 = . if mi(age)
gen age_4554 = age>=45 & age <55
replace age_4554 = . if mi(age)
gen age_5564 = age>=55 & age <65
replace age_5564 = . if mi(age)
gen age_o65 = age>=65 & !missing(age)
replace age_o65 = . if mi(age)

*income
* use income_hh (continuos) and then sub in hhincome (categorical, less recent) if income_hh is missing

gen inc_lt25 = income_hh<25000 & !mi(income_hh)
replace inc_lt25= 1 if hhincome <=7 & !missing(hhincome) & mi(income_hh)
gen inc_2549 = income_hh>=25000 & income_hh<50000
replace inc_2549 = 1 if inrange(hhincome, 8, 11) & mi(income_hh)
gen inc_5074 = income_hh>=50000 & income_hh<75000
replace inc_5074 = 1 if inrange(hhincome, 12, 13) & mi(income_hh)
gen inc_7599 = income_hh>=75000 & income_hh<100000
replace inc_7599 = 1 if hhincome == 14 & mi(income_hh)
gen inc_gt100 = income_hh>=100000 & !mi(income_hh)
replace inc_gt100 = 1 if inrange(hhincome, 15, 16) & mi(income_hh)

gen inc_cat = 1 if inc_lt25 ==1
replace inc_cat = 2 if inc_2549 ==1
replace inc_cat = 3 if inc_5074==1
replace inc_cat = 4 if inc_7599 == 1
replace inc_cat = 5 if inc_gt100 ==1

*education
gen edu_lhs = highest_education <=8 & !missing(highest_education)
gen edu_hs = highest_education ==9
gen edu_sc = highest_education ==10
gen edu_c = inlist(highest_education, 11, 12, 13)
gen edu_pgs = highest_education >= 14 & !missing(highest_education)

gen edu_cat = 1 if edu_lhs ==1
replace edu_cat = 2 if edu_hs ==1
replace edu_cat = 3 if edu_sc ==1
replace edu_cat = 4 if edu_c ==1
replace edu_cat = 5 if edu_pgs ==1

*gender 
gen male = gender == 1
replace male = . if mi(gender)

gen female = gender ==0
replace female =. if mi(gender)

*ethnicity
gen latino = hispaniclatino== 1
replace latino = . if mi(hispaniclatino)

gen nonlatino = hispaniclatino ==0
replace nonlatino =. if mi(hispaniclatino)

*race
gen white = race == 1
gen black = race == 2
gen asian = race == 4 
gen other = inlist(race, 3, 5, 6)

gen race_cat = 1 if black==1
replace race_cat = 2 if asian  ==1
replace race_cat = 3 if other ==1
replace race_cat = 4 if white ==1

*homeownership
gen non_homeowner = 1 if homeowner == 0
replace non_homeowner = 0 if homeowner == 1


*employment 
gen employed = inlist(laborstatus, 1, 2)
gen unemployed = inlist(laborstatus, 3, 4)
gen retired = inlist(laborstatus, 5)
gen disabled_other = inlist(laborstatus,6,7,8) & employed != 1 & unemployed != 1& retired != 1

gen emp_cat = 1 if unemployed == 1 // unemployed 
replace emp_cat = 2 if retired == 1 // retired 
replace emp_cat = 3 if disabled_other == 1 // disabled_other
replace emp_cat = 4 if employed == 1 // employed


*marital status
gen married = inlist(marital_status, 1, 2)
gen separated = inlist(marital_status, 3)
gen divorced = inlist(marital_status, 4)
gen widowed = inlist(marital_status, 5)
gen never_married= inlist(marital_status, 6)

gen marriage_cat = 1 if married ==1 
replace marriage_cat = 2 if separated ==1
replace marriage_cat = 3 if divorced ==1
replace marriage_cat = 4 if widowed ==1
replace marriage_cat = 5 if never_married ==1


*urbanicity 
gen env_rural = urban_cat==1
replace env_rural  = . if mi(urban_cat)
gen env_mixed = urban_cat==2
replace env_mixed  = . if mi(urban_cat)
gen env_urban = urban_cat==3
replace env_urban  = . if mi(urban_cat)

*FICO Score 
rename ph006 fico_cat 

gen fico_lt600 = fico_cat ==1
gen fico_600649 = fico_cat ==2
gen fico_650699= fico_cat ==3
gen fico_700749= fico_cat ==4
gen fico_750800= fico_cat ==5
gen fico_gt800= fico_cat ==6
gen fico_unknown= fico_cat ==7

replace fico_cat = . if fico_cat == 7


*financial hardships
gen cc_froz_12mos = ph009_d
gen lostjob_12mos = ph009_a
gen bankruptcy_12mos = ph009_b
gen foreclosure_12mos = ph009_c

gen payday_12mos = pa055_b1
gen pawn_12mos = pa055_b2
gen rent2own_12mos = pa055_b3
gen refundloan_12mos = pa055_b4
gen autotiloan_12mos = pa055_b5

gen badloans_12mos = payday_12mos == 1 | pawn_12mos == 1  | rent2own_12mos ==1 | refundloan_12mos==1 | autotiloan_12mos == 1

*emergency savings 
gen eexpsaved_500 = 1 if e_exp_tot_saved >= 500 & !mi( e_exp_tot_saved)
replace eexpsaved_500 = 0 if e_exp_tot_saved <500 & !mi( e_exp_tot_saved)	 

gen eexpsaved_1000 = 1 if e_exp_tot_saved >= 1000 & !mi( e_exp_tot_saved)
replace eexpsaved_1000 = 0 if e_exp_tot_saved <1000 & !mi( e_exp_tot_saved)

gen eexpcover_2000 = 1 if e_exp_cover==2000
replace eexpcover_2000 = 0 if mi(eexpcover_2000) & !mi(e_exp_cover)

egen e_exp_lassets = rowtotal(e_exp_sav e_exp_prepaid e_exp_csh e_exp_chk)

gen eexplassets_2000 = 1 if e_exp_lassets==2000
replace eexplassets_2000 = 0 if mi(eexplassets_2000) & !mi(e_exp_cover)

*rescale behavioral big 5 to be on a scale from 1 to 5
*openness max 50 (civide by 10)
*agreeableness max 45 (divide by 9)
* extroversion max 40 (divide by 8)
*neuroticism max 40 (divide by 8)
*conscientiousness max 45 (divide by 9)

replace openness = openness/10
replace agreeableness = agreeableness/9
replace extroversion = extroversion/8
replace neuroticism = neuroticism/8
replace conscientiousness = conscientiousness/9

*okay, we want to get the mean for everyone, the mean for cc non adopters, cc convenience users, cc revolvers, non cc revolvers, unbanked, banked ; test for the differences  

count 
mat means_tests_uw = r(N)
mat colnames means_tests_uw = All
mat rownames means_tests_uw = N

count if !mi(ind_weight)
mat means_tests_w = r(N)
mat colnames means_tests_w = All 
mat rownames means_tests_w = N

svyset [pweight=ind_weight]

foreach v in  no_cc_adopt cc_adopt no_rev_12mos rev_12mos no_rev_lastmo rev_lastmo no_unbanked unbanked  {
	
	count if `v' == 1
	mat `v'_N = r(N)
	mat means_tests_uw = (means_tests_uw, `v'_N)
	
	svy: total `v'
	mat `v'_wN = e(b)
	mat means_tests_w = (means_tests_w, `v'_wN)
	
	
}


mat filler = (.)
foreach n of numlist 1(1)13 {
	
	mat filler = (filler,.)
	
}
mat means_tests_w = (means_tests_w, filler)
mat means_tests_uw = (means_tests_uw, filler)

*adding the share of the sample 
mat pct_uw = 100
mat pct_w = 100
mat rownames pct_uw = pct 
mat rownames pct_w = pct 

foreach v in  no_cc_adopt cc_adopt no_rev_12mos rev_12mos no_rev_lastmo rev_lastmo   no_unbanked unbanked {
	
	sum `v'
	mat `v'_uw = r(mean)*100
	mat pct_uw = (pct_uw, `v'_uw)
	
	svy: mean `v'
	mat `v'_w = e(b)*100
	mat pct_w = (pct_w, `v'_w)
	
	
}
mat pct_uw = (pct_uw, filler)
mat pct_w = (pct_w , filler)
mat means_tests_w = means_tests_w \ pct_w
mat means_tests_uw = means_tests_uw \ pct_uw

foreach y in $demosfinancial {
	
	*rescaling the weights, I checked and it makes pretty much no difference but better to be safe :)
	preserve 
	drop if mi(`y') 
	count if !mi(`y') 
	local num = r(N) 
	egen total_ind_weight = total(ind_weight)
	gen `y'_wgt = `num'*ind_weight/total_ind_weight
	
	svyset [pweight=`y'_wgt]
	
	
	svy: mean `y'
	mat mean_w = e(b) 
	mat num_w = e(_N_subp)
	mat p_w = .
	mat rownames mean_w = `y'
	
	mean `y'
	mat mean_uw = e(b) 
	mat num_uw = e(_N)
	mat p_uw = .
	mat rownames mean_uw = `y'
	
	restore 
	
	foreach x in cc_adopt rev_12mos rev_lastmo unbanked   {
	
		preserve 
		drop if mi(`y') 
		drop if mi(`x')
		count if !mi(`y') 
		local num = r(N) 
		egen total_ind_weight = total(ind_weight)
		gen `y'_`x'_wgt = `num'*ind_weight/total_ind_weight
	
		svyset [pweight=`y'_`x'_wgt]
		
		svy: reg `y' `x'
		mat a = e(b)[1,1] + e(b)[1,2]
		mat b = e(b)[1,2]
		mat m = (b,a)
		mat mean_w = (mean_w,m)
		test `x'
		mat p = r(p)
		mat p_w = (p_w,r(p))
		svy: mean `y' , over(`x')
		mat n = e(_N_subp)
		mat num_w = (num_w,n)
		
		*unweighted
		ttest `y', by(`x')
		mat m = (r(mu_1),r(mu_2))
		mat n = (r(N_1),r(N_2))
		mat p = r(p)
		mat mean_uw = (mean_uw,m)
		mat num_uw = (num_uw,n)
		mat p_uw = (p_uw,p)
		
		
		
		restore 
	
	}
	
	mat row_w = (mean_w,num_w,p_w)
	mat row_uw = (mean_uw,num_uw,p_uw)
	
	mat means_tests_w == means_tests_w \ row_w
	mat means_tests_uw == means_tests_uw \ row_uw
}

*checking account done separately because it is missing for unbanked 

foreach y in chk_bal {
	
	preserve 
	drop if mi(`y') 
	count if !mi(`y') 
	local num = r(N) 
	egen total_ind_weight = total(ind_weight)
	gen `y'_wgt = `num'*ind_weight/total_ind_weight
	
	svyset [pweight=`y'_wgt]
	
	
	svy: mean `y'
	mat mean_w = e(b) 
	mat num_w = e(_N_subp)
	mat p_w = .
	mat rownames mean_w = `y'
	
	mean `y'
	mat mean_uw = e(b) 
	mat num_uw = e(_N)
	mat p_uw = .
	mat rownames mean_uw = `y'
	
	restore 
	*calculate for the banked
	preserve 
		drop if mi(`y') 
		drop if mi(no_unbanked)
		count if !mi(`y') 
		local num = r(N) 
		egen total_ind_weight = total(ind_weight)
		gen `y'_banked_wgt = `num'*ind_weight/total_ind_weight
		svyset [pweight=`y'_banked_wgt]
		svy: mean `y' if no_unbanked == 1
		mat n_banked_w = e(_N_subp)
		mat m_banked_w = e(b)
				
		*unweighted
		sum `y' if no_unbanked == 1
		mat m_banked_uw = (r(mean))
		mat n_banked_uw = (r(N))
		
	restore 
	
	foreach x in cc_adopt rev_12mos rev_lastmo  {
	
		preserve 
		drop if mi(`y') 
		drop if mi(`x')
		count if !mi(`y') 
		local num = r(N) 
		egen total_ind_weight = total(ind_weight)
		gen `y'_`x'_wgt = `num'*ind_weight/total_ind_weight
	
		svyset [pweight=`y'_`x'_wgt]
		
		svy: reg `y' `x'
		mat a = e(b)[1,1] + e(b)[1,2]
		mat b = e(b)[1,2]
		mat m = (b,a)
		mat mean_w = (mean_w,m)
		test `x'
		mat p = r(p)
		mat p_w = (p_w,r(p))
		svy: mean `y' , over(`x')
		mat n = e(_N_subp)
		mat num_w = (num_w,n)
		
		*unweighted
		ttest `y', by(`x')
		mat m = (r(mu_1),r(mu_2))
		mat n = (r(N_1),r(N_2))
		mat p = r(p)
		mat mean_uw = (mean_uw,m)
		mat num_uw = (num_uw,n)
		mat p_uw = (p_uw,p)
		
		restore 
	
	}
	
	mat row_w = (mean_w,m_banked_w,.,num_w,n_banked_w,.,p_w,.)
	mat row_uw = (mean_uw,m_banked_uw,.,num_uw,n_banked_uw,.,p_uw,.)
	mat means_tests_w == means_tests_w \ row_w
	mat means_tests_uw == means_tests_uw \ row_uw
}



mat colnames means_tests_w =  All no_cc_adopt cc_adopt no_rev_12mos rev_12mos no_rev_lastmo rev_lastmo no_unbanked unbanked All_N no_cc_adopt_N cc_adopt_N no_rev_12mos_N rev_12mos_N no_rev_lastmo_N rev_lastmo_N no_unbanked_N unbanked_N NA cc_adopt_p rev_12mos_p rev_lastmo_p unbanked_p
mat colnames means_tests_uw =  All no_cc_adopt cc_adopt no_rev_12mos rev_12mos no_rev_lastmo rev_lastmo no_unbanked unbanked All_N no_cc_adopt_N cc_adopt_N no_rev_12mos_N rev_12mos_N no_rev_lastmo_N rev_lastmo_N no_unbanked_N unbanked_N NA cc_adopt_p rev_12mos_p rev_lastmo_p unbanked_p


putexcel clear 
putexcel set "${sum_path}/summary.xlsx", sheet("means_tests_w") modify
putexcel A1= matrix(means_tests_w) , names

putexcel clear 
putexcel set "${sum_path}/summary.xlsx", sheet("means_tests_uw") modify
putexcel A1= matrix(means_tests_uw) , names





*add stars 
preserve 

	clear
	import excel "${sum_path}/summary.xlsx", sheet("means_tests_w")  firstrow
	
	foreach v in cc_adopt rev_12mos rev_lastmo unbanked  {
		capture drop `v'_stars
		gen `v'_stars = ""
		replace `v'_stars = "*" if `v'_p < 0.05
		replace `v'_stars = "**" if `v'_p < 0.01
		replace `v'_stars= "***" if `v'_p < 0.001
	}
	
	capture rename A variable
	export excel using "${sum_path}/summary.xlsx", sheet("means_tests_w")  firstrow(variables) sheetmodify keepcellfmt

restore
preserve 

	clear
	import excel "${sum_path}/summary.xlsx", sheet("means_tests_uw")  firstrow
	
	foreach v in cc_adopt rev_12mos rev_lastmo unbanked  {
		capture drop `v'_stars
		gen `v'_stars = ""
		replace `v'_stars = "*" if `v'_p < 0.05
		replace `v'_stars = "**" if `v'_p < 0.01
		replace `v'_stars= "***" if `v'_p < 0.001
	}
	
	capture rename A variable
	export excel using "${sum_path}/summary.xlsx", sheet("means_tests_uw")  firstrow(variables) sheetmodify keepcellfmt

restore

