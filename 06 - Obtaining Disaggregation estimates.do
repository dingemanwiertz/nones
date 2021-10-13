*************************************************
///   6. Obtaining disaggregation estimates   ///
*************************************************



/* This script forms the foundation for the Disaggregation estimates that 
   are referred to in Figures 1 & 2. It produces estimates for 1990, 2001, 
   2008, and 2014, based on five-year samples from the GSS. See the file 
   "01 - Preparation GSS data.do" for details about the underlying data. */


foreach c in 1990 2001 2008 2014 {
  use "GSS7318_MRP", clear
  keep if year>=`c'-2 & year<=`c'+2
  gen one = 1
  bysort stnum: egen Nstate = total(one)
  collapse None Evangelical Mainline BlackProt Catholic ///
			WeeklyAttend NeverAttend StrongID Nstate ///
			[weight = wtssall], by(stnum) 
  foreach x in None Evangelical Mainline BlackProt Catholic ///
				WeeklyAttend NeverAttend StrongID {
     replace `x' = `x' * 100
  }
  gen year = `c'
  tempfile Disaggregation_5y_`c'
  save "`Disaggregation_5y_`c''", replace
}

use "Disaggregation_5y_1990", clear
append using "Disaggregation_5y_2001"
append using "Disaggregation_5y_2008"
append using "Disaggregation_5y_2014"
merge m:1 stnum using "state identifiers crosswalk", nogen
order Nstate - stname, first
order year Nstate, after(stnum)
sort stfips year 
save "Disaggregation estimates", replace 

/* Notice that the "state identifiers crosswalk" file, containing 
   a set of state identifiers, is available through the replication 
   repository: https://github.com/dingemanwiertz/nones.		*/ 
