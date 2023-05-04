/**********************************************************************************************************************************

	Created by: Liang Zhang, modified by Ruth Cohen
	Date: 2.14.2019
	Purpose: Output regression results into excel file - I'm automating this process and the esttab to csv process should be obsolete 
			 from now on.
			 1. output_reg: takes care of OLS and probit's marginal effects (note, not the coefficient!)
			 2. output_tobit: takes care of tobit and its mean marginal effect of censored outcome, specify what you want

			 
***********************************************************************************************************************************/

** A program that output the results directly to excel 
** define your reg_path local first if you want to recycle this!!! 
** all output functions are here. 

** this can output 1. OLS 2. Probit's marginal effect plus pseudo R-square - you need to save the pseudo R-square into a matrix, name it pseudo_r2 after you run the regression but  
**                           before you run margins.
** MAKE SURE YOU SPECIFY THE "POST" WHEN RUN MARGINAL EFFECT BEFORE USING ANY OF THIS, IF YOU DON'T THIS WILL OUTPUT THE PREVIOUS ESTIMATION BC THE MARGINAL EFFECTS ARE NOT SAVED
	
	cap program drop output_reg
		
	program define output_reg
		
		args name type
		
		local obs = e(N)
		mat N = `obs'
		mat rownames N = "obs"
		mat filler = J(2,1,.)
		
		** well we might have linear regression or discrete choice models (usually probit) so it could be R2 or pseudo R2 
		** for probit you have to save the pseudo R2 before you do margins otherwise you lost it, I will save it in a local pseudo_r 
		
		if "`type'" == "discrete"{
		
			mat r2 = pseudo_r
			mat rownames r2 = pseudo_r2
			
		} 
		else{
		
			mat r2 = e(r2_a)
			mat rownames r2 = adj_r2
		
		}
		
		mat coeff = e(b)'\N\r2
		mat variance = vecdiag(e(V))'\filler
		mat temp = coeff,variance
		mat colnames temp = coeff variance
		
		** you can probably use mata here to get the sqrt
		preserve 
		
		clear 
		svmat2 temp, rnames(var) names(col)
		order var
		
		gen std_err = sqrt(variance)
		drop variance
		
		
		if "`type'" == "discrete"{
		
		** if you are outputing marginal effect it employs a z test so the measure is the same (coeff/se) but the pvalue is calculated differently
		** bc 
			gen zstat = coeff/std_err
			gen pval = 2*normal(-abs(zstat))
			
		} 
		else{
		
			gen tstat = coeff/std_err
			
			** obs number - 1 is the degree of freedom but I'm sure you know that. 
			gen pval = 2*ttail(`obs'-1, abs(tstat))
		
		}

		** you can also add some testing procedure here, say Bonferroni if u want to test joint hypothesis etc
		gen stars = ""
		replace stars = "*" if pval < 0.05
		replace stars = "**" if pval < 0.01
		replace stars = "***" if pval < 0.001

		** okay EXPORT!
		export excel "${reg_path}/Regressions.xlsx", sheet("`name'", replace) firstrow(var)

		restore

	end
	


** another program that can output the tobit model - well technically I can combine this with the above but too much functionality can make the program hard to recycle
** because it will be really hard to modify

	cap program drop output_tobit 

	program define output_tobit 

		args name type 
		
		if "`type'" == "coeff"{
		
			local obs = e(N)
			local obs_lc = e(N_lc)
			local obs_rc = e(N_rc)
			local pseudo_r2 = e(r2_p)
			
			mat N = `obs'
			mat rownames N = "obs"
			
			mat N_lc = `obs_lc'
			mat rownames N_lc = "Obs_left_censored"
			
			mat N_rc = `obs_rc'
			mat rownames N_rc = "Obs_right_censored"
			
			mat pseudo_r2= `pseudo_r2'
			mat rownames pseudo_r2 = "pseudo_r2"
			
			mat filler = J(4,1,.)
			mat coeff = e(b)'\ N \ N_lc \ N_rc \ pseudo_r2
			
			mat variance = vecdiag(e(V))'\filler
			}
			else if "`type'" == "margins"{
				
				local obs = e(N)
				mat N = `obs'
				mat rownames N = "obs"
				mat filler = J(1,1,.)
				mat coeff = e(b)'\ N
				mat variance = vecdiag(e(V))'\filler
			
			}
		
		mat temp = coeff,variance
		mat colnames temp = coeff variance
		
		
		** you can probably use mata here to get the sqrt 
		preserve 
		
		clear 
		svmat2 temp, rnames(var) names(col)
		order var
		
		gen std_err = sqrt(variance)
		drop variance
		
		if "`type'" == "margins"{
		
		** if you are outputing marginal effect it employs a z test so the measure is the same (coeff/se) but the pvalue is calculated differently
		** bc 
			gen zstat = coeff/std_err
			gen pval = 2*normal(-abs(zstat))
			
		} 
		else if "`type'" == "coeff"{
		
			gen tstat = coeff/std_err
			
			** obs number - 1 is the degree of freedom but I'm sure you know that. 
			gen pval = 2*ttail(`obs'-1, abs(tstat))
		
		}

		** you can also add some testing procedure here, say Bonferroni if u want to test joint hypothesis etc
		gen stars = ""
		replace stars = "*" if pval < 0.05
		replace stars = "**" if pval < 0.01
		replace stars = "***" if pval < 0.001

		** okay EXPORT!
		export excel "${reg_path}/Regressions.xlsx", sheet("`name'_`type'", replace) firstrow(var)

		restore
		
		

	end

	