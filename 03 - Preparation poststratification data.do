*****************************************************
///   3. Preparation of poststratification data   ///
*****************************************************



/* This script prepares the poststratification dataset in the following steps: 

	1) Loading the data; 
	2) Recoding the socio-demographic covariates (in line with GSS); 
	3) Calculating the total population size for each state-year combo
		(this will become the denominator later on); 
	4) Setting up a dataset containing all respondent types; 
	5) Calculating the absolute frequency of each respondent type 
		for each state-year combo (this will become the numerator later on); 
	6) Merging together the numerators (step 5) and denominators (step 3); 
	7) Calculating the relative frequency of each respondent type
		for each state-year combo; 
	8) Cleaning up, stacking, and saving the data. 	 */

   
   
*********************************
///   3.1. Loading the data   ///
*********************************

	
use "census_acs_1970_2018", clear 
/* This file contains large individual-level samples from the 1970-2000 
   Census and 2001-2018 American Community Surveys (ACS). It contains 
   information about the sex, age, race, education level, and state of 
   residence for each individual included. This information was obtained 
   via IPUMS USA; see https://usa.ipums.org/usa-action/samples. Please 
   contact the authors if you require a copy of this dataset. */ 
   
/* We drop all individuals aged under 18: */ 
drop if age<18

   
   
**********************************************************
///   3.2. Recoding the socio-demographic covariates   ///
**********************************************************


	*********
	/* Sex */
	*********

	des sex 
	labelbook SEX 
	tab sex, miss 
	
	recode sex (2 = 1 "Female") (1 = 0 "Male"), into(female)
	lab var female "Dummy variable for being female"
	tab sex female, miss
	
	
	*********
	/* Age */
	*********

	des age
	labelbook AGE
	tab age, miss
	
	recode age (18/29 = 1 "18-29") (30/44 = 2 "30-44") ///
				(45/64 = 3 "45-64") (65/100 = 4 "65+") ///
				(else = .), into(age_cat)
	lab var age_cat "Age, 4 categories"
	tab age age_cat, miss
	
	
	***************
	/* Education */
	***************

	des educd 
	labelbook EDUCD
	tab educd, miss  /* There is 1 n/a case out of 96m. */
	foreach x in 1970 1980 1990 2000 2001 2007 2008 2018 { 
		display "`x'"
		tab educd if year==`x', miss
	}  /* Exploring the different codings by year. */ 
	
	recode educd (0/50 = 1) (60 = 2) (65/90 = 3) (100/111 = 4) ///
					if year==1970, gen(edu_cat70)
	recode educd (0/50 = 1) (60 = 2) (65/90 = 3) (100/113 = 4) ///
					if year==1980, gen(edu_cat80)
	recode educd (0/61 = 1) (62 = 2) (71/83 = 3) (101/116 = 4) ///
					if year==1990, gen(edu_cat90)
	recode educd (0/61 = 1) (62 = 2) (65/81 = 3) (101/116 = 4) ///
					if year==2000, gen(edu_cat00)
	recode educd (0/61 = 1) (62 = 2) (65/81 = 3) (101/116 = 4) ///
					if year>=2001 & year<=2007, gen(edu_cat0107)
	recode educd (0/61 = 1) (63/64 = 2) (65/81 = 3) (101/116 = 4) ///
					if year>=2008 & year<=2018, gen(edu_cat0818)

	gen edu_cat = .
	replace edu_cat = edu_cat70 if year==1970
	replace edu_cat = edu_cat80 if year==1980
	replace edu_cat = edu_cat90 if year==1990
	replace edu_cat = edu_cat00 if year==2000
	replace edu_cat = edu_cat0107 if year>=2001 & year<=2007
	replace edu_cat = edu_cat0818 if year>=2008 & year<=2018
	lab define EDU_CAT 1 "Less than HS" 2 "High school" ///
						3 "Some college" 4 "College degree"
	lab val edu_cat EDU_CAT
	lab var edu_cat "Educational attainment, 4 categories"
	foreach x in 1970 1980 1990 2000 2001 2007 2008 2018 { 
		display "`x'"
		tab educd edu_cat if year==`x', miss
	}  

	drop edu_cat70 edu_cat80 edu_cat90 edu_cat00 edu_cat0107 edu_cat0818
	
	
	**********
	/* Race */
	**********	

	des race hispan
	labelbook RACE HISPAN
	tab race hispan, miss	

	gen race_wbho = 4
	replace race_wbho = 1 if race==1 & hispan==0
	replace race_wbho = 2 if race==2 & hispan==0
	replace race_wbho = 3 if hispan>0 & hispan!=.
	lab define RACE_WBHO 1 "Non-Hisp White" 2 "Non-Hisp Black" ///
							3 "Hispanic" 4 "Other"
	lab val race_wbho RACE_WBHO
	lab var race_wbho "Race, 4 categories"
	tab race race_wbho if hispan==0, miss
	tab race race_wbho if hispan!=0, miss

	
	**************************
	/* Trimming the dataset */
	**************************
	
	/* First creating a unique person identifier: */ 
	des serial pernum
	sum serial pernum 
	gen double pid = serial + (pernum * 100000000)
	lab var pid "Unique person identifier"
	duplicates report pid statefip year  /* No duplicates. */
	
	keep year pid statefip region perwt ///
			female race_wbho edu_cat age_cat 
	save "census_acs_1970_2018_trimmed", replace


	
****************************************************************
///   3.3. Total population size for each state-year combo   ///
****************************************************************


global years 1970 1980 1990 2000 ///
			2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 ///
			2011 2012 2013 2014 2015 2016 2017 2018
			
foreach c of global years { 
	use "census_acs_1970_2018_trimmed", clear
	keep if year==`c'
	collapse (count) pop`c'=pid [fweight = perwt], by(statefip)
	tempfile pop`c'
	save "`pop`c''", replace
}



*********************************************************************
///   3.4. Setting up a dataset containing all respondent types   ///
*********************************************************************


use "census_acs_1970_2018_trimmed.dta", clear
collapse (count) cfreq=pid [fweight=perwt], ///
					by(female race_wbho edu_cat age_cat statefip)
di 51 * 2 * 4 * 4 * 4  /* Notice we miss one respondent type in our data. */

tab statefip    
tab female race_wbho if statefip==16
tab edu_cat age_cat if statefip==16 & female==1 & race_wbho==2
count if statefip==16 & female==1 & race_wbho==2 & edu_cat==4 & age_cat==4
/* The missing case is for Black 65+ women with college degree in Idaho. */ 

/* Adding the missing case to our dataset: */ 
set obs 6528
replace statefip = 16 in 6528
replace female = 1 in 6528
replace race_wbho = 2 in 6528
replace age_cat = 4 in 6528
replace edu_cat = 4 in 6528

/* Saving the full cross-classification: */ 
drop cfreq
sort statefip female race_wbho age_cat edu_cat
save "poststrat_shell", replace



**************************************************************
///   3.5. Absolute frequencies for all respondent types   ///
**************************************************************


foreach c of global years { 
	use "census_acs_1970_2018_trimmed.dta", clear
	keep if year==`c'
	collapse (count) cfreq`c'=pid [fweight=perwt], ///
		by(female race_wbho edu_cat age_cat statefip)	
	tempfile cfreq`c'
	save "`cfreq`c''", replace
}



****************************************************************
///   3.6. Combining population sizes and cell frequencies   ///
****************************************************************


use "poststrat_shell", clear

foreach c of global years { 
	display "`c'"
	merge m:1 statefip using "`pop`c''"
	drop _merge
	merge 1:1 statefip female race_wbho edu_cat age_cat using "`cfreq`c''"
	replace cfreq`c' = 0 if _merge==1
	drop _merge
}

save "poststrat_1970_2018", replace



**************************************************************
///   3.7. Relative frequencies for all respondent types   ///
**************************************************************


use "poststrat_1970_2018", clear

foreach c of global years { 
	gen cprop`c' = cfreq`c' / pop`c'
}

/* Interpolations for non-Census years: */ 
forvalues c=1971/1979 {
	gen cprop`c' = cprop1970 + (cprop1980 - cprop1970) * (`c' - 1970) / 10
}
forvalues c=1981/1989 {
	gen cprop`c' = cprop1980 + (cprop1990 - cprop1980) * (`c' - 1980) / 10
}
forvalues c=1991/1999 {
	gen cprop`c' = cprop1990 + (cprop2000 - cprop1990) * (`c' - 1990) / 10
}

save "poststrat_1970_2018", replace



***********************************************************
///   3.8. Cleaning up, stacking, and saving the data   ///
***********************************************************


/* Cleaning up the data and splitting by year: */ 
forvalues c=1973/2018 { 
	use "poststrat_1970_2018", clear
	keep statefip female race_wbho edu_cat age_cat cprop`c'
	rename cprop`c' prop_state
	lab var prop_state "Proportion of respondent type, by state-year"
	rename statefip stfips
	merge m:1 stfips using "state identifiers crosswalk", nogen
	gen year = `c'
	lab var year "Year"
	tempfile Census_poststrat_`c'
	save "`Census_poststrat_`c''", replace 
}
/* The "state identifiers crosswalk" file, containing a set of state 
   identifiers, is available through the replication repository: 
   https://github.com/dingemanwiertz/nones. */ 

/* Stacking up the data and dropping non-GSS years: */ 
use "`Census_poststrat_1973'", clear
forvalues c=1974/2018 {
    append using "`Census_poststrat_`c''"
}
egen state_tag = tag(stnum)
list stname stnum if state_tag==1
sort stnum year
list stname stnum if state_tag==1
drop state_tag 
merge m:1 year using "year identifiers crosswalk", nogen
drop if year==1979 | year==1981 | year==1992 | year==1995 | ///
        year==1997 | year==1999 | year==2001 | year==2003 | ///
		year==2005 | year==2007 | year==2009 | year==2011 | ///
		year==2013 | year==2015 | year==2017 | year>2018
sort stnum year
/* The "year identifiers crosswalk" file, containing a set of year
   identifiers, is available through the replication repository: 
   https://github.com/dingemanwiertz/nones. */ 

/* Saving the final dataset: */ 
save "Census_poststrat_7318", replace 
