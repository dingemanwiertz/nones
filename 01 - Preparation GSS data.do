**************************************
///   1. Preparation of GSS data   ///
**************************************



/* This script prepares the GSS cumulative dataset in the following steps: 

	1) Loading the data; 
	2) Recoding the socio-demographic covariates; 
	3) Recoding the religious outcome variables; 
	4) Merging in the state identifiers; 
	5) Saving the data. 												*/
  
  
  
*********************************
///   1.1. Loading the data   ///
*********************************


cd "..."
set maxvar 10000
use "GSS7218_R3", clear
/* Data downloadable from: https://gss.norc.org/get-the-data/stata
   Also available via replication repository: 
   https://github.com/dingemanwiertz/nones */ 



**********************************************************
///   1.2. Recoding the socio-demographic covariates   ///
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
	tab year female, miss row nofreq
	
	
	*********
	/* Age */
	*********

	des age
	labelbook AGE
	tab age, miss

	recode age (18/29 = 1 "18-29") (30/44 = 2 "30-44") ///
				(45/64 = 3 "45-64") (65/89 = 4 "65+") ///
				(else = .), into(age_cat)
	lab var age_cat "Age, 4 categories"
	tab age age_cat, miss
	tab year age_cat, miss row nofreq
	
	
	***************
	/* Education */
	***************
	
	des degree
	labelbook LABL
	tab year degree, miss
	
	recode degree (0 = 1 "Less than HS") (1 = 2 "High school") ///
					(2 = 3 "Some college") (3 4 = 4 "College degree") ///
					(else = .), into(edu_cat)
	lab var edu_cat "Educational attainment, 4 categories"
	tab degree edu_cat, miss
	tab year edu_cat, miss row nofreq


	**********
	/* Race */
	**********

	des race hispanic ethnic 
	labelbook RACE HISPANIC LABTY
	tab year race, miss
	tab year hispanic, miss  /* Only available from 2000 onwards. */
	tab year ethnic, miss  /* Available for all years. */ 

	recode hispanic (1 = 0 "Non-Hispanic") (2/50 = 1 "Hispanic") ///
						(else = .), into(hisp)
	lab var hisp "Hispanic dummy (2000-onwards)"
	tab hispanic hisp, miss
	tab ethnic hisp, row nofreq
	/* 4 ethnic groups that classify themselves predominantly as "Hispanic":
	   Mexico, Puerto Rico, Spain, Other Spanish. */

	gen race_wbho = . 
	lab var race_wbho "Race, 4 categories"
	lab define RACE_WBHO 1 "Non-Hisp White" 2 "Non-Hisp Black" ///
							3 "Hispanic" 4 "Other"
	lab val race_wbho RACE_WBHO

	replace race_wbho = 1 if race==1 & year>=2000 & ///
								(hispanic==1 | hispanic==.) 
	replace race_wbho = 1 if race==1 & year<2000 & ///
								(ethnic!=17 & ethnic!=22 & ///
									ethnic!=25 & ethnic!=38)
								
	replace race_wbho = 2 if race==2 & year>=2000 & ///
								(hispanic==1 | hispanic==.) 
	replace race_wbho = 2 if race==2 & year<2000 & ///
								(ethnic!=17 & ethnic!=22 & ///
									ethnic!=25 & ethnic!=38)
						
	replace race_wbho = 3 if hispanic>1 & hispanic!=. & year>=2000
	replace race_wbho = 3 if year<2000 & ///
								(ethnic==17 | ethnic==22 | ///
									ethnic==25 | ethnic==38)
						
	replace race_wbho = 4 if race==3 & race_wbho!=3 

	tab race race_wbho, miss
	tab year race_wbho, miss row nofreq



*********************************************************
///   1.3. Recoding the religious outcome variables   ///
*********************************************************


	****************************
	/* Religious affiliations */
	****************************
	
	des relig denom other 
	labelbook RELIG DENOM OTHER 
	tab year relig, miss row nofreq 
		/* "Other" category furter decomposed from 1998. */
	tab year denom, miss row nofreq
		/* More detailed delineations from 1984 on. */
	
	/* The recodings below are based on the Steensland et al. 2000 
	   RELTRAD scheme, incl. the modifications by Stetzer & Burge 2015. 
	   The code is available via https://github.com/ryanburge/reltrad. 
	   Please note that in the meantime Burge & Djupe 2021 have proposed 
	   further modifications: https://github.com/ryanburge/new_reltrad. */

	gen xaffil=relig 
	recode xaffil 1=1 2=4 3=5 4=9 5/10=6 11=1 12=6 13=1 *=. 
	label def xaffil 1 prot 4 cath 5 jew 6 other 9 nonaf 
	label values xaffil xaffil 
	tab relig xaffil, miss
 
	/* Black Protestants: */
	gen xbp=other 
	recode xbp 7 14 15 21 37 38 56 78 79 85 86 87 88 98 103 104 128 133=1 *=0 
	recode xbp 0=1 if denom==12 
	recode xbp 0=1 if denom==13 
	recode xbp 0=1 if denom==20 
	recode xbp 0=1 if denom==21 
	gen bl=race 
	recode bl 2=1 *=0 
	gen bldenom=denom*bl 
	recode xbp 0=1 if bldenom==23 
	recode xbp 0=1 if bldenom==28 
	recode xbp 0=1 if bldenom==18 
	recode xbp 0=1 if bldenom==15 
	recode xbp 0=1 if bldenom==10 
	recode xbp 0=1 if bldenom==11 
	recode xbp 0=1 if bldenom==14 
	gen blother=other*bl 
	recode xbp 0=1 if blother==93 

	/* Evangelical Protestants: */
	gen xev=other 
	recode xev 2 3 5 6 9 10 12 13 16 18 20 22 24 26 27 28 31 32 34 35 36 ///
				39 41 42 43 45 47 51 52 53 55 57 63 65 66 67 68 69 76 77 ///
				83 84 90 91 92 94 97 100 101 102 106 107 108 109 110 111 ///
				112 115 116 117 118 120 121 122 124 125 127 129 131 132 ///
				134 135 138 139 140 146=1 *=0 
	recode xev 0=1 if denom==32 
	recode xev 0=1 if denom==33 
	recode xev 0=1 if denom==34 
	recode xev 0=1 if denom==42 
	gen wh=race 
	recode wh 1=1 2=0 3=1 
	gen whdenom=denom*wh 
	recode xev 0=1 if whdenom==23 
	recode xev 0=1 if whdenom==18 
	recode xev 0=1 if whdenom==15 
	recode xev 0=1 if whdenom==10 
	recode xev 0=1 if whdenom==14 
	gen whother=other*wh 
	recode xev 0=1 if whother==93 
	recode xev 1=0 if xbp==1 

	gen xprotdk=denom 
	recode xprotdk 70=1 *=0 
	recode xprotdk 1=0 if attend==0 
	recode xprotdk 1=0 if attend==1 
	recode xprotdk 1=0 if attend==2 
	recode xprotdk 1=0 if attend==3 
	recode xprotdk 1=0 if attend==. 
	recode xev 0=1 if xprotdk==1 

	/* Mainline Protestants: */
	gen xml=other 
	recode xml 1 8 19 23 25 40 44 46 48 49 50 54 70 71 72 73 81 ///
					89 96 99 105 119 148=1 *=0 
	recode xml 0=1 if denom==22 
	recode xml 0=1 if denom==30 
	recode xml 0=1 if denom==31 
	recode xml 0=1 if denom==35 
	recode xml 0=1 if denom==38 
	recode xml 0=1 if denom==40 
	recode xml 0=1 if denom==41 
	recode xml 0=1 if denom==43 
	recode xml 0=1 if denom==48 
	recode xml 0=1 if denom==50 
	recode xml 0=1 if whdenom==11 
	recode xml 0=1 if whdenom==28 

	/* Catholics: */
	gen xcath=other 
	recode xcath 123=1 *=0
	recode xcath 0=1 if xaffil==4 

	/* Jews: */
	gen xjew=0 
	recode xjew 0=1 if xaffil==5 
 
	/* Others: */
	gen xother=other  
	recode xother 11 17 29 30 33 58 59 60 61 62 64 74 75 80 82 95 113 ///
					114 130 136 141 145=1 *=0 
	gen noxev=1-xev 
	gen noxevxaf=noxev*xaffil 
	recode xother 0=1 if noxevxaf==6 
 
	/* Nones: */
	gen xnonaff=xaffil 
	recode xnonaff 9=1 *=0 

	/* Below are a couple of corrections from Stetzer & Burge 2015: */

		/* For people who responded that they were Christian in the relig 
			variable but did not get asked the follow-up questions: */
		gen xtn = relig
		gen denom2=denom
		recode denom2 70=1 10/60=0
		recode xtn 11=1 else=0
		recode xtn 1=2 if denom2==1
		recode xtn 1=1 else=0
		recode xtn 1=0 if attend==0 
		recode xtn 1=0 if attend==1 
		recode xtn 1=0 if attend==2 
		recode xtn 1=0 if attend==3 
		recode xtn 1=0 if attend==. 
		recode xev 0=1 if xtn==1 

		/* For people who responded that they were Interdenominational in the 
			relig variable but did not get asked the follow-up questions: */
		gen inter = relig
		recode inter 13=1 else=0
		recode inter 1=0 if attend==0 
		recode inter 1=0 if attend==1 
		recode inter 1=0 if attend==2 
		recode inter 1=0 if attend==3 
		recode inter 1=0 if attend==9 
		recode inter 1=0 if attend==. 
		recode xev 0=1 if inter==1 

	/* Collecting these recodings into one RELTRAD variable: */
	gen reltrad=0 
	recode reltrad 0=7 if xnonaf==1 
	recode reltrad 0=6 if xother==1 
	recode reltrad 0=5 if xjew==1 
	recode reltrad 0=4 if xcath==1 
	recode reltrad 0=3 if xbp==1 
	recode reltrad 0=2 if xml==1 
	recode reltrad 0=1 if xev==1 
	recode reltrad 0=.  
	label def RELTRAD 1 "Evangelical" 2 "Mainline" 3 "Black Protestant" ///
						4 "Catholic" 5 "Jewish" 6 "Other faith" 7 "None" 
	label values reltrad RELTRAD
	lab var reltrad "Religious tradition, 7 categories"

	/* Creating dummies for each of the religious traditions: */
	recode reltrad (1 = 1 "Yes") (else = 0 "No"), into(Evangelical)
	recode reltrad (2 = 1 "Yes") (else = 0 "No"), into(Mainline)
	recode reltrad (3 = 1 "Yes") (else = 0 "No"), into(BlackProt)
	recode reltrad (4 = 1 "Yes") (else = 0 "No"), into(Catholic)
	recode reltrad (5 = 1 "Yes") (else = 0 "No"), into(Jewish)
	recode reltrad (6 = 1 "Yes") (else = 0 "No"), into(OthFaith)
	recode reltrad (7 = 1 "Yes") (else = 0 "No"), into(None)
	lab var Evangelical "Whether one is Evangelical Protestant"
	lab var Mainline "Whether one is Mainline Protestant"
	lab var BlackProt "Whether one is Black Protestant"
	lab var Catholic "Whether one is Catholic"
	lab var Jewish "Whether one is Jewish"
	lab var OthFaith "Whether one is Other faith"
	lab var None "Whether one is None"

	/* Checks: */ 
	tab year if Evangelical==1 
	/* We get the same numbers as Stetzer & Burge 2015. */
	tab year reltrad, miss row nofreq 
	/* What is going on in 1982 & 1987? This reflects that 27% and 30% of 
	   all respondents are Black in those years (i.e., >500 vs. 150-200 in 
	   surrounding years). This resulted from purposeful oversampling. 
	   We will later exclude these oversamples from the dataset. */
	tab year race_wbh, row nofreq
	tab year reltrad if sample!=4 & sample!=5 & sample!=7, row nofreq 


	***********************************************
	/* Frequency of religious service attendance */
	***********************************************

	des attend 
	labelbook ATTEND 
	tab year attend, miss row nofreq
	
	recode attend (6 7 8 = 1 "Almost weekly or more") ///
					(else = 0 "Less than almost weekly"), ///
						into(WeeklyAttend)
	lab var WeeklyAttend "Whether one attends church (almost) weekly"
	
	recode attend (0 1 = 1 "Practically never") ///
					(else = 0 "Sometimes"), ///
						into(NeverAttend)
	lab var NeverAttend "Whether one attends church less than once a year" 

	tab attend WeeklyAttend, miss
	tab attend NeverAttend, miss 
	tab year WeeklyAttend, miss row nofreq
	tab year NeverAttend, miss row nofreq


	******************************************
	/* Strength of religious identification */
	******************************************
	
	des reliten
	labelbook RELITEN
	tab year reliten, miss row nofreq

	recode reliten (1 = 1 "Strong religious identification") ///
					(else = 0 "No strong religious identification"), ///
						into(StrongID)
	lab var StrongID "Strong religious identification"

	tab reliten StrongID, miss 
	tab year StrongID, miss row nofreq
	replace StrongID = . if year<1974



*************************************************
///   1.4. Merging in the state identifiers   ///
*************************************************


/* GSS geographic identification code files are only made available to 
   researchers through special contracts with NORC; for more details see 
   https://gss.norc.org/Documents/other/ObtainingGSSSensitiveDataFiles.pdf */ 

merge 1:1 year id using "gss7218_fipsstat"
tab year _merge 
drop _merge

codebook fipsstat  /* 51 unique states identified. */
tab fipsstat year, miss  /* Not all states observed in all years. */

egen tag_stateyear = tag(fipsstat year)
lab var tag_stateyear "Selects one state observation for each year"
bysort year: egen nstates = total(tag_stateyear)
lab var nstates "Number of states identified for each year"
mean nstates, over(year) 
/* Number of states identified per year fluctuates between 33 and 41. */ 

/* Merging in additional state identifiers, available via replication
	repository: https://github.com/dingemanwiertz/nones */
rename fipsstat stfips
merge m:1 stfips using "state identifiers crosswalk"
tab year _merge 
drop _merge


 
********************************
///   1.5. Saving the data   ///
********************************


keep year id stfips stname stnum stlabel sample wtssall ///
		female race_wbho edu_cat age_cat ///
		reltrad attend reliten ///
		Evangelical Mainline BlackProt Catholic Jewish OthFaith None ///
		WeeklyAttend NeverAttend StrongID 

order year id stfips stname stlabel stnum sample wtssall ///
		female race_wbho edu_cat age_cat ///
		reltrad attend relitent /// 
		Evangelical Mainline BlackProt Catholic Jewish OthFaith None ///
		WeeklyAttend NeverAttend StrongID 
		
/* Removing Black oversamples: */ 
drop if sample==4 | sample==5 | sample==7
drop sample

/* Merging in additional year identifiers, available via replication
	repository: https://github.com/dingemanwiertz/nones */
merge m:1 year using "year_key", nogen

/* Dropping the year 1972, for which no state identifiers are available: */ 
drop if year==1972

save "GSS7318_MRP", replace