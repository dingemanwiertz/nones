**********************************************
///   2. Preparation of state-level data   ///
**********************************************



/* This script prepares the state-level variables that will be used 
   in our analysis in the following steps: 

	1) Preparing the congregational density variables; 
	2) Preparing the Republican vote share variable; 
	3) Merging the variables and adding in auxiliary variables.  */



***************************************************************
///   2.1. Preparing the congregational density variables   ///
***************************************************************


/* We start from the year-specific files of the 1980, 1990, 2000, and 
   2010 Religious Congregations and Membership Study, which we downloaded 
   from Social Explorer (https://www.socialexplorer.com/). Similar files 
   are available via the ARDA website, although with some different 
   variable codings/labels (https://www.thearda.com/Archive/ChState.asp). */
   
	
	***************
	/* RCMS 1980 */
	***************

	use "state_rcms_1980", clear
	des T001_001 - T003_006 FIPS
	
	gen totcng = (T002_001 / T001_001) * 1000
	lab var totcng "Total # of all religious congregations per 1,000"

	gen evancng = (T003_003 / T001_001) * 1000
	lab var evancng "Total # of Evangelical congregations per 1,000"

	gen maincng = (T003_005 / T001_001) * 1000
	lab var maincng "Total # of Mainline congregations per 1,000"

	gen cathcng = (T003_002 / T001_001) * 1000
	lab var cathcng "Total # of Catholic congregations per 1000"

	rename FIPS stfips
	label variable stfips "State FIPS code"

	keep stfips totcng evancng maincng cathcng 
	drop if _n==52
	
	renvars totcng - cathcng, postfix(_1980)

	save "state_cng_1980", replace


	***************
	/* RCMS 1990 */
	***************

	use "state_rcms_1990", clear
	des T001_001 - T003_006 FIPS

	gen totcng = (T002_001 / T001_001) * 1000
	lab var totcng "Total # of all religious congregations per 1,000"

	gen evancng = (T003_003 / T001_001) * 1000
	lab var evancng "Total # of Evangelical congregations per 1,000"

	gen maincng = (T003_005 / T001_001) * 1000
	lab var maincng "Total # of Mainline congregations per 1,000"

	gen cathcng = (T003_002 / T001_001) * 1000
	lab var cathcng "Total # of Catholic congregations per 1000"

	rename FIPS stfips
	label variable stfips "State FIPS code"

	keep stfips totcng evancng maincng cathcng 
	
	renvars totcng - cathcng, postfix(_1990)

	save "state_cng_1990", replace

	
	***************
	/* RCMS 2000 */
	***************

	use "state_rcms_2000", clear
	des T001_001 - T003_007 FIPS

	gen totcng = (T002_001 / T001_001) * 1000
	lab var totcng "Total # of all religious congregations per 1,000"

	gen evancng = (T003_003 / T001_001) * 1000
	lab var evancng "Total # of Evangelical congregations per 1,000"

	gen maincng = (T003_005 / T001_001) * 1000
	lab var maincng "Total # of Mainline congregations per 1,000"

	gen cathcng = (T003_002 / T001_001) * 1000
	lab var cathcng "Total # of Catholic congregations per 1000"

	rename FIPS stfips
	label variable stfips "State FIPS code"

	keep stfips totcng evancng maincng cathcng 
	drop if stfips=="72" 
	
	renvars totcng - cathcng, postfix(_2000)

	save "state_cng_2000", replace

	
	***************
	/* RCMS 2010 */
	***************

	use "state_rcms_2010", clear
	des T001_001 - T003_007 FIPS

	gen totcng = (T002_001 / T001_001) * 1000
	lab var totcng "Total # of all religious congregations per 1,000"

	gen evancng = (T003_002 / T001_001) * 1000
	lab var evancng "Total # of Evangelical congregations per 1,000"

	gen maincng = (T003_004 / T001_001) * 1000
	lab var maincng "Total # of Mainline congregations per 1,000"

	gen blackcng = (T003_003 / T001_001) * 1000
	lab var blackcng "Total # of Black Protestant congregations per 1,000"
	replace blackcng = 0 if blackcng==.  /* 2 changes, for HI and ND. */ 

	gen cathcng = (T003_005 / T001_001) * 1000
	lab var cathcng "Total # of Catholic congregations per 1000"

	rename FIPS stfips
	label variable stfips "State FIPS code"

	keep stfips totcng evancng maincng blackcng cathcng 

	renvars totcng - cathcng, postfix(_2010)

	save "state_cng_2010", replace
	
	
	****************************
	/* Merging the RCMS waves */
	****************************
	
	use "state_cng_1980", clear 
	merge 1:1 stfips using "state_cng_1990", nogen
	merge 1:1 stfips using "state_cng_2000", nogen
	merge 1:1 stfips using "state_cng_2010", nogen
	
	destring stfips, replace
	label define STATE_FIPS 1 "AL" 2 "AK" 4 "AZ" 5 "AR" 6 "CA" 8 "CO" ///
							9 "CT" 10 "DE" 11 "DC" 12 "FL" 13 "GA" 15 "HI" ///
							16 "ID" 17 "IL" 18 "IN" 19 "IA" 20 "KS" 21 "KY" ///
							22 "LA" 23 "ME" 24 "MD" 25 "MA" 26 "MI" 27 "MN" ///
							28 "MS" 29 "MO" 30 "MT" 31 "NE" 32 "NV" 33 "NH" ///
							34 "NJ" 35 "NM" 36 "NY" 37 "NC" 38 "ND" 39 "OH" ///
							40 "OK" 41 "OR" 42 "PA" 44 "RI" 45 "SC" 46 "SD" ///
							47 "TN" 48 "TX" 49 "UT" 50 "VT" 51 "VA" 53 "WA" ///
							54 "WV" 55 "WI" 56 "WY"
	lab val stfips STATE_FIPS


	***************************************
	/* Interpolations for non-RCMS years */
	***************************************
	
	foreach x in totcng evancng maincng cathcng {
		forvalues c=1973/1979 {
			gen `x'_`c' = `x'_1980
		}
		forvalues c=1981/1989 { 
			gen `x'_`c' = `x'_1980 + (`x'_1990 - `x'_1980) * (`c'-1980) / 10
		}
		forvalues c=1991/1999 { 
			gen `x'_`c' = `x'_1990 + (`x'_2000 - `x'_1990) * (`c'-1990) / 10
		}
		forvalues c=2001/2009 { 
			gen `x'_`c' = `x'_2000 + (`x'_2010 - `x'_2000) * (`c'-2000) / 10
		}
		forvalues c=2011/2018 { 
			gen `x'_`c' = `x'_2010
		}
	}

	forvalues c=1973/2009 {
		gen blackcng_`c' = blackcng_2010
	}
	forvalues c=2011/2018 {
		gen blackcng_`c' = blackcng_2010
	}

	
	*******************************
	/* Reshaping and saving data */
	*******************************
	
	reshape long totcng_ evancng_ maincng_ blackcng_ cathcng_, ///
				i(stfips) j(year)
	
	renvars totcng_ - blackcng_, postdrop(1) 
	sort stfips year 
	
	save "state_cng_1973-2018", replace 



**************************************************************
///   2.2. Preparing the Republican vote share variables   ///
**************************************************************


/* We start from a file covering the Republican vote share for each state 
   for all presidential elections since 1972, obtained via the American 
   Presidency Project: https://www.presidency.ucsb.edu/statistics/elections.
   The American Presidency Project provides this data for each election 
   separately; we have collected all information in a single file. */ 

   
   use "state_rep_1972-2020", clear 
   
	
	*******************************************
	/* Interpolations for non-election years */
	*******************************************

	forvalues c=1973/1975 {
		gen repvote_`c' = repvote_1972 + (repvote_1976 - repvote_1972) * (`c'-1972) / 4
	}
	forvalues c=1977/1979 {
		gen repvote_`c' = repvote_1976 + (repvote_1980 - repvote_1976) * (`c'-1976) / 4
	}
	forvalues c=1981/1983 {
		gen repvote_`c' = repvote_1980 + (repvote_1984 - repvote_1980) * (`c'-1980) / 4
	}
	forvalues c=1985/1987 {
		gen repvote_`c' = repvote_1984 + (repvote_1988 - repvote_1984) * (`c'-1984) / 4
	}
	forvalues c=1989/1991 {
		gen repvote_`c' = repvote_1988 + (repvote_1992 - repvote_1988) * (`c'-1988) / 4
	}
	forvalues c=1993/1995 {
		gen repvote_`c' = repvote_1992 + (repvote_1996 - repvote_1992) * (`c'-1992) / 4
	}
	forvalues c=1997/1999 {
		gen repvote_`c' = repvote_1996 + (repvote_2000 - repvote_1996) * (`c'-1996) / 4
	}
	forvalues c=2001/2003 {
		gen repvote_`c' = repvote_2000 + (repvote_2004 - repvote_2000) * (`c'-2000) / 4
	}
	forvalues c=2005/2007 {
		gen repvote_`c' = repvote_2004 + (repvote_2008 - repvote_2004) * (`c'-2004) / 4
	}
	forvalues c=2009/2011 {
		gen repvote_`c' = repvote_2008 + (repvote_2012 - repvote_2008) * (`c'-2008) / 4
	}
	forvalues c=2013/2015 {
		gen repvote_`c' = repvote_2012 + (repvote_2016 - repvote_2012) * (`c'-2012) / 4
	}
	forvalues c=2017/2018 {
		gen repvote_`c' = repvote_2016 + (repvote_2020 - repvote_2016) * (`c'-2016) / 4
	}
	
	
	*******************************
	/* Reshaping and saving data */
	*******************************
	
	reshape long repvote_, i(stfips) j(year)
	
	rename repvote_ repvote 
	keep if year>=1973 & year<=2018
	sort stfips year 
	
	save "state_repvote_1973-2018", replace 


	
***************************************************
///   2.3. Completing the state-level dataset   ///
***************************************************


	******************************************
	/* Creating indicator for Census region */
	******************************************
	
	use "state identifiers crosswalk", clear 
	/* This file, containing a set of state identifiers, is available through 
	   the replication repository: https://github.com/dingemanwiertz/nones. */ 
	
	des stfips 
	labelbook STATE_FIPS
	
	recode stfips (9 11 23 25 33 34 36 42 44 50 = 1 "Northeast") ///
				(17/20 26 27 29 31 38 39 46 55 = 2 "Midwest") ///
				(1 5 10 12 13 21 22 24 28 37 40 45 47 48 51 54 = 3 "South") ///
				(2 4 6 8 15 16 30 32 35 41 49 53 56 = 4 "West"), into(region)
	lab var region "Major Census region"
	tab stfips region 
	
	
	***********************************************
	/* Merging in congregation and election data */
	***********************************************
	
	merge 1:m stfips using "state_cng_1973-2018", nogen
	merge 1:m stfips year using "state_repvote_1973-2018", nogen
	
	
	************************************
	/* Saving the state-level dataset */
	************************************
	
	save "state_allvars_7318", replace 
	