********************************************
///   7. Preparation benchmark surveys   ///
********************************************



/* This script prepares the benchmark surveys that we use for comparing 
   the GSS-based estimates against. We draw on the following sources: 
   
   1. American Religious Identification Survey (1990, 2001, 2008); 
   2. Pew Religious Landscape Study (2007, 2014); 
   3. American Values Atlas (2014). 							*/
     

   
****************************************************************
///   7.1. American Religious Identification Survey (ARIS)   ///
****************************************************************


/* Data from the American Religious Identification Survey (previously called 
   National Survey of Religious Identification) can be accessed via: 
	- 1990: https://www.thearda.com/Archive/Files/Descriptions/NSRI1990.asp;
	- 2001: https://www.thearda.com/Archive/Files/Descriptions/ARIS2001.asp;
	- 2008: https://www.thearda.com/Archive/Files/Descriptions/ARIS2008.asp. */

	 
	***************
	/* NSRI 1990 */
	***************

	use "National Survey of Religious Identification 1990", clear

	des religaf 
	labelbook RELIGAF
	tab religaf, miss
	/* About 10% declare to be "Protestant" and about 5% declare to be 
	   "Christian". We will treat these people as Evangelical or Black 
	   Protestant, depending on their race. */ 
	
	codebook race hisprac
	tab race hisprac, miss 
	recode race (1 = 0 "Non-Black") (2 = 1 "Black") (nonm = .), into(black)
	lab var black "Whether or not respondent is Black"
	replace black = 0 if hisprac==1
	replace black = 1 if hisprac==2
	tab race black, miss

	lab define RELTRAD 1 "Evangelical Protestant" 2 "Mainline Protestant" ///
						3 "Black Protestant" 4 "Catholic" 5 "Jew" ///
						6 "Other" 7 "None"
	recode religaf (2 4 6 9 19 20 23 29/33 36 37 39 51 53 55/57 60 = 1) ///
			(8 15 16 21 38 43/46 49 50 = 2) (61 = 3) (1 = 4) (3 = 5) ///
			(5 7 10/14 17 18 22 24/28 34 35 40 47 48 52 54 58 59 62/97 = 6) ///
			(41 42 = 7) (nonm = .), into(reltrad)
	replace reltrad = 3 if black==1 & ///
							(religaf==2 | religaf==4 | religaf==6 | ///
								religaf==15 | religaf==16 | religaf==20 | ///
								religaf==31 | religaf==32 | religaf==57)
	lab var reltrad "Religious belonging, 7 categories"
	lab val reltrad RELTRAD
	tab reltrad, miss

	recode reltrad (1 = 100 "Evangelical") (else = 0 "Something else"), ///
					into(Evangelical)
	lab var Evangelical "Whether someone is an Evangelical Protestant"
	recode reltrad (2 = 100 "Mainline") (else = 0 "Something else"), ///
					into(Mainline)
	lab var Mainline "Whether someone is a Mainline Protestant"
	recode reltrad (3 = 100 "Black Prot") (else = 0 "Something else"), ///
					into(BlackProt)
	lab var BlackProt "Whether someone is a Black Protestant"
	recode reltrad (4 = 100 "Catholic") (else = 0 "Something else"), ///
					into(Catholic)
	lab var Catholic "Whether someone is Catholic"
	recode reltrad (7 = 100 "None") (else = 0 "Something else"), ///
					into(None)
	lab var None "Whether someone is a None"

	collapse None Evangelical Mainline BlackProt Catholic ///
				[weight = popwght], by(state)
 
	rename state stfips 
	lab var stfips "State FIPS code"

	gen year = 1990 
	lab var year "Year"

	gen source = "NSRI/ARIS"
	lab var source "Data source"

	save "ARIS 1990", replace

	
	***************
	/* ARIS 2001 */
	***************

	use "American Religious Identification Survey 2001", clear

	des jq1tot
	labelbook jq1tot
	tab jq1tot, miss
	/* About 2% declare to be "Protestant" and about 7% declare to be 
	   "Christian". We will treat these people as Evangelical or Black 
	   Protestant, depending on their race. */ 
	
	codebook dem10 - dem11
	tab dem11 dem10a, miss 
	recode dem11 (1 3/7 = 0 "Non-Black") (2 = 1 "Black") (nonm = .), into(black)
	lab var black "Whether or not respondent is Black"
	replace black = 0 if dem10a==1
	replace black = 1 if dem10a==2
	tab dem11 black, miss
	
	lab define RELTRAD 1 "Evangelical Protestant" 2 "Mainline Protestant" ///
						3 "Black Protestant" 4 "Catholic" 5 "Jew" ///
						6 "Other" 7 "None"
	recode jq1tot (1 2 7 10 11 13 15 16 18 20 25 26 29 69 72 74/76 79 80 = 1) ///
				(12 14 17 19 21 23 24 27 28 30 70 = 2) ///
				(61 = 3) (6 = 4) (3/5 62/64 = 5) ///
				(8 9 22 31/39 51 52 59 60 65/68 71 73 77 78 81/89 997 = 6) ///
				(40 53 54 990 = 7) (nonm = .), into(reltrad)			
	replace reltrad = 3 if black==1 & ///
							(jq1tot==2 | jq1tot==7 | jq1tot==10 | ///
							jq1tot==11 | jq1tot==12 | jq1tot==14 | ///
							jq1tot==16 | jq1tot==25 | jq1tot==72 | ///
							jq1tot==79 | jq1tot==80)
	lab var reltrad "Religious belonging, 7 categories"
	lab val reltrad RELTRAD
	tab reltrad, miss

	recode reltrad (1 = 100 "Evangelical") (else = 0 "Something else"), ///
					into(Evangelical)
	lab var Evangelical "Whether someone is an Evangelical Protestant"
	recode reltrad (2 = 100 "Mainline") (else = 0 "Something else"), ///
					into(Mainline)
	lab var Mainline "Whether someone is a Mainline Protestant"
	recode reltrad (3 = 100 "Black Prot") (else = 0 "Something else"), ///
					into(BlackProt)
	lab var BlackProt "Whether someone is a Black Protestant"
	recode reltrad (4 = 100 "Catholic") (else = 0 "Something else"), ///
					into(Catholic)
	lab var Catholic "Whether someone is Catholic"
	recode reltrad (7 = 100 "None") (else = 0 "Something else"), ///
					into(None)
	lab var None "Whether someone is a None"

	collapse None Evangelical Mainline BlackProt Catholic ///
				[weight = wgt_pop], by(state)

	rename state stname 
	merge 1:1 stname using "state identifiers crosswalk"
	drop if _merge!=3 
	drop _merge stname stnum stlabel
	/* Notice that the "state identifiers crosswalk" file, containing 
	   a set of state identifiers, is available through the replication 
       repository: https://github.com/dingemanwiertz/nones.		*/ 

 	gen year = 2001
	lab var year "Year"

	gen source = "NSRI/ARIS"
	lab var source "Data source"

	save "ARIS 2001", replace


	***************
	/* ARIS 2008 */
	***************

	use "American Religious Identification Survey 2008", clear

	des relig RELIG2 RELIG3
	labelbook RELIG RELIG2 RELIG3
	
	gen relig_comb = relig
	replace relig_comb = RELIG2 if relig==17
	replace relig_comb = RELIG3 if relig==8
	replace relig_comb = 17 if RELIG2==96 | RELIG2==97
	replace relig_comb = 8 if RELIG3==96 | RELIG3==97
	lab var relig_comb "Religious identification, combined"
	lab val relig_comb RELIG
	tab relig_comb, miss
	/* About 3% declare to be "Protestant" and about 6% declare to be 
	   "Christian". We will treat these people as Evangelical or Black 
	   Protestant, depending on their race. */ 
	   
	codebook race
	labelbook RACE
	recode race (3 6 = 1 "Black") (1 5 8/11 = 0 "Non-Black") ///
					(nonm = .), into(black)
	lab var black "Whether or not respondent is Black"
	tab race black, miss
	
	lab define RELTRAD 1 "Evangelical Protestant" 2 "Mainline Protestant" ///
						3 "Black Protestant" 4 "Catholic" 5 "Jew" ///
						6 "Other" 7 "None"
						
	recode relig_comb (2 4/8 11 15 17 19 22 24/26 28 29 31 32 35 36 41/43 = 1) ///
						(3 9 12/14 16 18 20 27 34 37 38 40 = 2) ///
						(10 = 3) (1 = 4) (60 = 5) ///
						(21 23 30 33 39 44/59 61/79 92 94 = 6) ///
						(90 91 93 95 = 7) (nonm = .), into(reltrad)			
	replace reltrad = 3 if black==1 & ///
							(relig_comb==2 | relig_comb==5 | relig_comb==7 | ///
							relig_comb==8 | relig_comb==9 | relig_comb==13 | ///
							relig_comb==14 | relig_comb==16 | relig_comb==17 | ///
							relig_comb==19 | relig_comb==26 | relig_comb==29 | ///
							relig_comb==36 | relig_comb==41)
	lab var reltrad "Religious belonging, 7 categories"
	lab val reltrad RELTRAD
	tab reltrad, miss

	recode reltrad (1 = 100 "Evangelical") (else = 0 "Something else"), ///
					into(Evangelical)
	lab var Evangelical "Whether someone is an Evangelical Protestant"
	recode reltrad (2 = 100 "Mainline") (else = 0 "Something else"), ///
					into(Mainline)
	lab var Mainline "Whether someone is a Mainline Protestant"
	recode reltrad (3 = 100 "Black Prot") (else = 0 "Something else"), ///
					into(BlackProt)
	lab var BlackProt "Whether someone is a Black Protestant"
	recode reltrad (4 = 100 "Catholic") (else = 0 "Something else"), ///
					into(Catholic)
	lab var Catholic "Whether someone is Catholic"
	recode reltrad (7 = 100 "None") (else = 0 "Something else"), ///
					into(None)
	lab var None "Whether someone is a None"

	collapse None Evangelical Mainline BlackProt Catholic ///
				[weight = popwght], by(state)

	rename state stname 
	merge 1:1 stname using "state identifiers crosswalk"
	drop if _merge!=3 
	drop _merge stname stnum stlabel
	/* Notice that the "state identifiers crosswalk" file, containing 
       a set of state identifiers, is available through the replication 
       repository: https://github.com/dingemanwiertz/nones.		*/ 

	gen year = 2008
	lab var year "Year"

	gen source = "NSRI/ARIS"
	lab var source "Data source"

	save "ARIS 2008", replace
	
	
	*****************************
	/* Stacking the ARIS waves */
	*****************************

	use "ARIS 1990", clear
	append using "ARIS 2001"
	append using "ARIS 2008"
	
	lab val stfips STATE_FIPS
	sort stfips year 

	save "ARIS all waves", replace 



*****************************************************
///   7.2. Pew Religious Landscape Study (PRLS)   ///
*****************************************************


/* Data from the Pew Religious Landscape Study can be accessed via: 
	- 2007: https://www.pewforum.org/dataset/u-s-religious-landscape-survey/;
	- 2014: https://www.pewforum.org/dataset/pew-research-center-2014-u-s-religious-landscape-study/. */

	 
	***************
	/* PRLS 2007 */
	***************

	use "Religious Landscape Survey 2007 - Continental", clear
	append using "Religious Landscape Survey 2007 - AK & HI"

	des reltrad 
	labelbook reltrad
	tab reltrad, miss

	recode reltrad (1100 = 1 "Evangelical Protestant") ///
					(1200 = 2 "Mainline Protestant") ///
					(1300 = 3 "Black Protestant") ///
					(10000 = 4 "Catholic") (50000 = 5 "Jew") ///
					(20000/40002 60000/90002 = 6 "Other religion") ///
					(100000 = 7 "None") (900000 = .), into(relig_7cat)
	lab var relig_7cat "Religious belonging, 7 categories"
	tab reltrad relig_7cat, miss

	recode relig_7cat (1 = 100 "Evangelical") (else = 0 "Something else"), ///
					into(Evangelical)
	lab var Evangelical "Whether someone is an Evangelical Protestant"
	recode relig_7cat (2 = 100 "Mainline") (else = 0 "Something else"), ///
					into(Mainline)
	lab var Mainline "Whether someone is a Mainline Protestant"
	recode relig_7cat (3 = 100 "Black Prot") (else = 0 "Something else"), ///
					into(BlackProt)
	lab var BlackProt "Whether someone is a Black Protestant"
	recode relig_7cat (4 = 100 "Catholic") (else = 0 "Something else"), ///
					into(Catholic)
	lab var Catholic "Whether someone is Catholic"
	recode relig_7cat (7 = 100 "None") (else = 0 "Something else"), ///
					into(None)
	lab var None "Whether someone is a None"

	des q20 
	labelbook q20
	tab q20, miss

	recode q20 (1 2 = 100 "At least once a week") ///
				(else = 0 "Less often / not at all"), into(WeeklyAttend)
	lab var WeeklyAttend "Religious service attendance: at least once a week"
	recode q20 (5 6 = 100 "(Almost) never") ///
				(else = 0 "More often"), into(NeverAttend)
	lab var NeverAttend "Religious service attendance: (almost) never"
	tab q20 WeeklyAttend, miss
	tab q20 NeverAttend, miss 
 
	des q21
	labelbook q21
	tab q21, miss
	
	recode q21 (1 = 100 "Religion is very important") ///
				(else = 0 "Religion is less important"), into(StrongID)
	lab var StrongID "Whether religion is very important in life"
	tab q21 StrongID, miss 
	
	collapse None Evangelical Mainline BlackProt Catholic ///
				WeeklyAttend NeverAttend StrongID ///
				[weight = weight], by(state)

	rename state stfips
	
	gen year = 2007
	lab var year "Year"

	gen source = "Pew RLS"
	lab var source "Data source"

	save "PRLS 2007", replace
	
	
	***************
	/* PRLS 2014 */
	***************

	use "Religious Landscape Survey 2014", clear
	
	des RELTRAD
	labelbook RELTRAD 
	tab RELTRAD, miss
	
	recode RELTRAD (1100 = 1 "Evangelical Protestant") ///
					(1200 = 2 "Mainline Protestant") ///
					(1300 = 3 "Black Protestant") ///
					(10000 = 4 "Catholic") (50000 = 5 "Jew") ///
					(20000/40002 60000/90002 = 6 "Other religion") ///
					(100000 = 7 "None") (900000 = .), into(relig_7cat)
	lab var relig_7cat "Religious belonging, 7 categories"
	tab RELTRAD relig_7cat, miss

	recode relig_7cat (1 = 100 "Evangelical") (else = 0 "Something else"), ///
					into(Evangelical)
	lab var Evangelical "Whether someone is an Evangelical Protestant"
	recode relig_7cat (2 = 100 "Mainline") (else = 0 "Something else"), ///
					into(Mainline)
	lab var Mainline "Whether someone is a Mainline Protestant"
	recode relig_7cat (3 = 100 "Black Prot") (else = 0 "Something else"), ///
					into(BlackProt)
	lab var BlackProt "Whether someone is a Black Protestant"
	recode relig_7cat (4 = 100 "Catholic") (else = 0 "Something else"), ///
					into(Catholic)
	lab var Catholic "Whether someone is Catholic"
	recode relig_7cat (7 = 100 "None") (else = 0 "Something else"), ///
					into(None)
	lab var None "Whether someone is a None"

	des attend
	labelbook attend
	tab attend, miss
	
	recode attend (1 2 = 100 "At least once a week") ///
					(else = 0 "Less often / not at all"), into(WeeklyAttend)
	lab var WeeklyAttend "Religious service attendance: at least once a week"
	recode attend (5 6 = 100 "(Almost) never") ///
					(else = 0 "More often"), into(NeverAttend)
	lab var NeverAttend "Religious service attendance: (almost) never"
	tab attend WeeklyAttend, miss
	tab attend NeverAttend, miss

	des qf2
	labelbook qf2 
	tab qf2, miss 
	
	recode qf2 (1 = 100 "Religion is very important") ///
				(else = 0 "Religion is less important"), into(StrongID)
	tab qf2 StrongID, miss 

	collapse None Evangelical Mainline BlackProt Catholic ///
				WeeklyAttend NeverAttend StrongID ///
				[weight = WEIGHT], by(state)

	rename state stfips
	
	gen year = 2014
	lab var year "Year"

	gen source = "Pew RLS"
	lab var source "Data source"

	save "PRLS 2014", replace

	
	*****************************
	/* Stacking the PRLS waves */
	*****************************

	use "PRLS 2007", clear
	append using "PRLS 2014"
		
	sort stfips year 

	save "PRLS all waves", replace 
	
	
	********************************************************************
	/* Denominational splits among Hispanic / Non-Western Protestants */
	********************************************************************
	
	/* In the American Values Atlas, the following Protestant groups are 
	   identified: White Evangelical, White Mainline, Black, Hispanic, and 
	   Other Non-White. To help split the Hispanic and Other Non-White
	   Protestants into different religious traditions, let's have a look 
	   at religious affiliations among Hispanic and Other Non-White 
	   Protestants in the 2014 Pew Religious Landscape Study. 			*/ 
	   
	use "Religious Landscape Survey 2014", clear 
	
	des racethn 
	labelbook racethn 
	tab racethn, miss 
	
	tab RELTRAD if racethn==3 & RELTRAD<10000 & state!=11
	tab RELTRAD if racethn==4 & RELTRAD<10000 & state!=11
	/* Hispanic Protestants: 71.7% Evan, 22.6% Main, 5.7% BlackProt. 
	   Other Non-White Protestants: 67.8% Evan, 30.6% Main, 1.6% BlackProt. */ 


	   
********************************************
///   7.3. American Values Atlas (AVA)   ///
********************************************


/* Annual data from the American Values Atlas from 2013 onwards can be 
   accessed via: http://ava.prri.org/. The dataset that will be loaded 
   below is the 2014 excerpt from this database, with "N/A" entries 
   already having been set to missing and "<0.5" entries to 0. In 
   addition, state FIPS codes have also been added already. 		*/
      
use "American Values Atlas 2014 - states", clear
append using "American Values Atlas 2016 - states"
append using "American Values Atlas 2018 - states" 

drop if state_fips==0 

/* Religious traditions in the AVA are partially identified by individuals' 
   race. We split Hispanic and Other Non-White Protestants across RELTRAD   
   categories based on the affiliation shares among Hispanic and Other Non-
   White Protestants in the PRLS 2014 data (see above): 
		- Hispanics: 71.6% Evan, 22.7% Main, 5.7% BlackProt; 
		- Other Non-Whites: 67.8% Evan, 30.6% Main, 1.6% BlackProt. 	*/ 
		
gen Evangelical = prot_evan + 0.717 * prot_hisp + 0.678 * prot_othnw 
lab var Evangelical "Percentage of Evangelical Protestants"

gen Mainline = prot_main + 0.226 * prot_hisp + 0.306 * prot_othnw 
lab var Mainline "Percentage of Mainline Protestants"

gen BlackProt = prot_black + 0.057 * prot_hisp + 0.016 * prot_othnw 
lab var BlackProt "Percentage of Black Protestants" 

gen Catholic = cath_white + cath_hisp + cath_othnw 
lab var Catholic "Percentage of Catholics" 

gen None = none 
lab var None "Percentage of Nones"

sum Evangelical Mainline BlackProt if year==2014
/* This gives: 23.7% (Pew 26.0%), 16.8% (Pew 16.4%), 7.2% (Pew 5.6%). */

keep state_fips year Evangelical Mainline BlackProt Catholic None 

rename state_fips stfips 
sort stfips year 

gen source = "AVA"
lab var source "Data source"

save "AVA all waves", replace



**********************************************
///   7.4. Merging the benchmark surveys   ///
**********************************************


use "ARIS all waves", clear
append using "PRLS all waves"
append using "AVA all waves"

save "External benchmark estimates", replace
