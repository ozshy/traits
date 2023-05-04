*Written by RC on 12.13.2022. 
*Last Modified 5.3.2023
*Purpose: Run regressions for the big 5 personality traits + financial behavior project
*Results then formatted in Excel using VLOOKUP function


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
global age " c.age##c.age"
global education "i.edu_cat"
global income "ib3.inc_cat"
global sex "i.male"
global latino "i.latino"
global race "ib(last).race_cat "
global homeownership "i.homeowner"
global employment "ib(last).emp_cat" 
global marital_status "i.marriage_cat"
global environment "i.urban_cat"
global fico "fico_lt600 fico_600649 fico_650699 fico_700749 fico_750800 fico_gt800"
global fin_hardships "cc_froz_12mos lostjob_12mos bankruptcy_12mos foreclosure_12mos payday_12mos pawn_12mos rent2own_12mos refundloan_12mos autotiloan_12mos badloans_12mos"
global big5 "extroversion agreeableness conscientiousness neuroticism openness"
*demographics 
global demos $age $education $sex $employment hh_size $race $latino $marital_status $environment

*run script that contains output programs (programs output regression results to an Excel file. Results are then formatted using VLOOKUP)
do "scripts/output programs_public.do"

*load data
use "${d_path}/Traits_and_financial_behavior_data_public.dta", clear



*EXCLUDING THE OBS NOT PART OF REPRESENTITIVE SAMPLE
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
*use income_hh (continuos) and then sub in hhincome (categorical, less recent) if income_hh is missing

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

gen log_income = log(income_hh)
gen log2_income = log(income_hh)/log(2)
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
*openness max 50 (divide by 10)
*agreeableness max 45 (divide by 9)
* extroversion max 40 (divide by 8)
*neuroticism max 40 (divide by 8)
*conscientiousness max 45 (divide by 9)

replace openness = openness/10
replace agreeableness = agreeableness/9
replace extroversion = extroversion/8
replace neuroticism = neuroticism/8
replace conscientiousness = conscientiousness/9

*************************************************************************************
*REGRESSION ANALYSIS

/*1.	Our dependent variables will be 0-1 dummies for banked/unbanked, credit card adopter/non-adopter, revolver/non-revolver (among cc adopters).
2.	The initial specification will have demographics, alternative specifications will add the Big 5, add financial variables, and add both the Big 5 and financial variables:

	a.	demographics
	b.	add Big 5 traits 
	c.	add financial
	d.	add both Big 5 and financial
*/

replace income_hh = income_hh / 1000
replace chk_bal = chk_bal/1000

foreach v in unbanked cc_adopt rev_lastmo {
	
	*spec a (for unbanked, use specifications that control for state in the loop below for credit card adoption, revolving)
	probit `v' $demos 
	matrix pseudo_r = round(`e(r2_p)', .001)
	margins, dydx(*) atmeans post
	output_reg "`v'_d_pr" "discrete"
	
	*spec c (for unbanked)
	probit `v' $demos $income finlitscore_3 $homeownership i.eexpcover_2000
	matrix pseudo_r = round(`e(r2_p)', .001)
	margins, dydx(*) atmeans post
	output_reg "`v'_dif3_pr" "discrete"
				
	*spec d (for unbanked)
	probit `v' $demos $income finlitscore_3 $homeownership $big5 i.eexpcover_2000
	matrix pseudo_r = round(`e(r2_p)', .001)
	margins, dydx(*) atmeans post
	output_reg "`v'_dif3b5_pr" "discrete"
	
	*spec b (for unbanked)
	probit `v' $demos $big5 
	matrix pseudo_r = round(`e(r2_p)', .001)
	margins, dydx(*) atmeans post
	output_reg "`v'_db5_pr" "discrete"
	
	*traits only
	probit `v'  $big5
	matrix pseudo_r = round(`e(r2_p)', .001)
	margins, dydx(*) atmeans post
	output_reg "`v'_b5_pr" "discrete"
	
}

foreach v in  cc_adopt rev_lastmo  {
	
	*Controls for state
	
	*spec a
	probit `v' $demos i.statereside
	matrix pseudo_r = round(`e(r2_p)', .001)
	margins, dydx(*) atmeans post
	output_reg "`v'_dSt_pr" "discrete"
	
	*spec c	
	probit `v' $demos $income finlitscore_3 $homeownership i.eexpcover_2000 i.statereside
	matrix pseudo_r = round(`e(r2_p)', .001)
	margins, dydx(*) atmeans post
	output_reg "`v'_dif3St_pr" "discrete"
	
	*spec b
	probit `v' $demos $big5  i.statereside
	matrix pseudo_r = round(`e(r2_p)', .001)
	margins, dydx(*) atmeans post
	output_reg "`v'_db5St_pr" "discrete"
	
	*spec d
	probit `v' $demos $income finlitscore_3 $homeownership $big5 i.eexpcover_2000 i.statereside
	matrix pseudo_r = round(`e(r2_p)', .001)
	margins, dydx(*) atmeans post
	output_reg "`v'_dif3b5St_pr" "discrete"
	
			
}

************************************************************************************
