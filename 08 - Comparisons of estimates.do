************************************************************************
///   8. Comparisons of GSS-based estimates to external benchmarks   ///
************************************************************************



/* This script conducts the comparisons between the GSS-based estimates and 
   the external benchmark estimates that are summarized in Figures 1 & 2. 
   It draws on the estimates derived in scripts 4, 5, 6 and 7. We go 
   through the following steps: 
   
   1) Cleaning up and stacking the data files with the estimates; 
   2) Calculating the performance metrics for Figure 1 (affiliations); 
   3) Calculating the performance metrics for Figure 2 (intensities); 
   4) Summarizing the performance metrics (drawing Figures 1 & 2); 
   5) One-to-one comparisons to benchmark estimates (attenuation graphs). 	
   
   In the script we draw on the auxiliary "state identifiers crosswalk.dta" 
   and "all state-year combinations.dta" files; these are available through 
   the replication repository: https://github.com/dingemanwiertz/nones. */
   
   
   
********************************************************
///   8.1. Cleaning up and stacking the data files   ///
********************************************************
   

	******************************
	/* Disaggregation estimates */
	******************************
   
	use "Disaggregation estimates", clear 

	renvars None Evangelical Mainline BlackProt Catholic ///
			WeeklyAttend NeverAttend StrongID, prefix(dis_)
			
	sort stnum year 
			
	save "Disaggregation estimates - for merge", replace 
			

	****************************
	/* Repeated MRP estimates */
	****************************
   
	use "Repeated MRP estimates", clear 

	renvars None Evangelical Mainline BlackProt Catholic ///
			WeeklyAttend NeverAttend StrongID, prefix(rmrp_)
			
	rename State stnum 
	rename Year year 
	merge m:1 stnum using "state identifiers crosswalk", nogen
	sort stnum year 
			
	save "Repeated MRP estimates - for merge", replace 


	***************************
	/* Dynamic MRP estimates */
	***************************
   
	use "Dynamic MRP estimates 1973-2018", clear 

	renvars None Evangelical Mainline BlackProt Catholic ///
			WeeklyAttend NeverAttend StrongID, prefix(dmrp_)

	keep State Year dmrp_* 
	
	rename State stnum 
	rename Year year 
	
	merge 1:1 stnum year using "all state-year combinations", nogen
	sort stnum year

	merge m:1 stnum using "state identifiers crosswalk", nogen
	sort stnum year 
	
	foreach x in None Evangelical Mainline BlackProt Catholic ///
					WeeklyAttend NeverAttend StrongID {
					    replace dmrp_`x' = 								///
								(dmrp_`x'[_n-1] + dmrp_`x'[_n+1]) / 2 	///
									if year==2001
					}
					
	keep if year==1990 | year==2001 | year==2008 | year==2014 | ///
				year==2016 | year==2018
			
	save "Dynamic MRP estimates - for merge", replace 


	*************************
	/* Benchmark estimates */
	*************************
   
	use "External benchmark estimates", clear 
	
	renvars None Evangelical Mainline BlackProt Catholic ///
			WeeklyAttend NeverAttend StrongID, prefix(bm_)
	
	sort stfips source year
			
	replace year = 2008 if year==2007 
	
	save "External benchmark estimates - for merge", replace 
	
	
	*************************************
	/* Stacking up the estimates files */
	*************************************

	use "Disaggregation estimates - for merge", clear 
	merge 1:1 stfips year using "Repeated MRP estimates - for merge", nogen
	merge 1:1 stfips year using "Dynamic MRP estimates - for merge", nogen 
	merge 1:m stfips year using "External benchmark estimates - for merge"
	tab year if _merge==1
	tab year if _merge==2
	drop _merge 

	save "Benchmark comparisons", replace


	
*************************************************
///   8.2. Performance metrics for Figure 1   ///
*************************************************


use "Benchmark comparisons", clear 
keep if year==1990 | year==2001 | year==2008 | year==2014

	******************
	/* Correlations */
	******************
	
	local f = 1 
	local g = 1 
	local h = 1 

	gen corr_dis = . 
	gen corr_rmrp = . 
	gen corr_dmrp = . 

	foreach x in Evangelical Mainline BlackProt Catholic None { 
		foreach c in 1990 2001 2008 2014 { 
			display "`x' `c'"
				pwcorr bm_`x' dis_`x' rmrp_`x' dmrp_`x' if year==`c', obs 
				replace corr_dis = r(C)[2,1] in `f++'
				replace corr_rmrp = r(C)[3,1] in `g++'
				replace corr_dmrp = r(C)[4,1] in `h++'
		}
	} 
	
	
	********************************************
	/* Mean absolute percentage errors (MAPE) */
	********************************************
	
	local i = 1 
	local j = 1 
	local k = 1 

	gen mape_dis = . 
	gen mape_rmrp = . 
	gen mape_dmrp = .
	
	foreach x in Evangelical Mainline BlackProt Catholic None { 
		foreach y in dis rmrp dmrp { 
			gen ape_`y'_`x' = abs(`y'_`x' - bm_`x') / bm_`x' * 100
			lab var ape_`y'_`x' "Absolute percentage error for `x', method `y'"
		}
	}
	
	foreach x in Evangelical Mainline BlackProt Catholic None { 
		foreach c in 1990 2001 2008 2014 { 
			display "`x' `c'"
				mean ape_dis_`x' if year==`c' 
				replace mape_dis = e(b)[1,1] in `i++'
		}
	}
	foreach x in Evangelical Mainline BlackProt Catholic None { 
		foreach c in 1990 2001 2008 2014 { 
			display "`x' `c'"
				mean ape_rmrp_`x' ape_dmrp_`x' if year==`c' 
				replace mape_rmrp = e(b)[1,1] in `j++'
				replace mape_dmrp = e(b)[1,2] in `k++'
		}
	}
	/* We run these loops separately for the disaggregation estimates, 
	   because otherwise the "mean" command only calculates statistics 
	   for the repeated/dynamic MRP estimates for states with non-missing 
	   disaggregation estimates. */ 


	********************************
	/* Mean absolute errors (MAE) */
	********************************

	local l = 1 
	local m = 1 
	local n = 1 

	gen mae_dis = . 
	gen mae_rmrp = . 
	gen mae_dmrp = . 

	foreach x in Evangelical Mainline BlackProt Catholic None { 
		foreach y in dis rmrp dmrp { 
			gen ae_`y'_`x' = abs(`y'_`x' - bm_`x') 
			lab var ae_`y'_`x' "Absolute error for `x', method `y'"
		}
	}
	
	foreach x in Evangelical Mainline BlackProt Catholic None { 
		foreach c in 1990 2001 2008 2014 { 
			display "`x' `c'"
				mean ae_dis_`x' if year==`c' 
				replace mae_dis = e(b)[1,1] in `l++'
		}
	}
	foreach x in Evangelical Mainline BlackProt Catholic None { 
		foreach c in 1990 2001 2008 2014 { 
			display "`x' `c'"
				mean ae_rmrp_`x' ae_dmrp_`x' if year==`c' 
				replace mae_rmrp = e(b)[1,1] in `m++'
				replace mae_dmrp = e(b)[1,2] in `n++'
		}
	}
	
	
	************************************************
	/* MAEs 10 smallest states (excl. DC, AK, HI) */
	************************************************

	local o = 1 
	local p = 1 

	gen mae10_rmrp = . 
	gen mae10_dmrp = .
	
	/* The 13 smallest states (from small to big): 
	   WY, VT, DC, AK, ND, SD, DE, RI, MT, ME, NH, HI, WV. 
	   DC, AK, HI excluded because not available in each benchmark source. */
   
	foreach x in Evangelical Mainline BlackProt Catholic None { 
		foreach c in 1990 2001 2008 2014 { 
			display "`x' `c'"
				mean ae_rmrp_`x' ae_dmrp_`x' ///
							if year==`c' & ///
							(stnum==9 | stnum==22 | stnum==27 | stnum==29 | ///
								stnum==31 | stnum==40 | stnum==42 | ///
								stnum==47 | stnum==50 | stnum==51)
				replace mae10_rmrp = e(b)[1,1] in `o++'
				replace mae10_dmrp = e(b)[1,2] in `p++'
		}
	}


	***************************************************
	/* Finishing up the metrics dataset for Figure 1 */
	***************************************************
	
	gen label_year = . 
	replace label_year = _n in 1/4 
	replace label_year = _n - 4 in 5/8
	replace label_year = _n - 8 in 9/12
	replace label_year = _n - 12 in 13/16
	replace label_year = _n - 16 in 17/20 
	lab var label_year "Year"
	lab define YEAR_aff 1 "1990" 2 "2001" 3 "2008" 4 "2014"
	lab val label_year YEAR_aff 

	gen label_outcome = . 
	replace label_outcome = 1 in 1/4
	replace label_outcome = 2 in 5/8
	replace label_outcome = 3 in 9/12 
	replace label_outcome = 4 in 13/16
	replace label_outcome = 5 in 17/20 
	lab var label_outcome "Outcome"
	lab define OUTCOME_aff 1 "Evangelical" 2 "Mainline" ///
							3 "Black Protestant" 4 "Catholic" 5 "None"
	lab val label_outcome OUTCOME_aff 

	keep label_outcome label_year corr_* mape_* mae_* mae10_* 
	keep if _n<21
	
	save "Performance metrics - affiliations", replace


	
*************************************************
///   8.3. Performance metrics for Figure 2   ///
*************************************************


use "Benchmark comparisons", clear 
drop if source!="Pew RLS" 


	******************
	/* Correlations */
	******************

	local f = 1 
	local g = 1 
	local h = 1 

	gen corr_dis = . 
	gen corr_rmrp = . 
	gen corr_dmrp = . 

	foreach x in WeeklyAttend NeverAttend StrongID { 
		foreach c in 2008 2014 { 
			display "`x' `c'"
				pwcorr bm_`x' dis_`x' rmrp_`x' dmrp_`x' if year==`c', obs 
				replace corr_dis = r(C)[2,1] in `f++'
				replace corr_rmrp = r(C)[3,1] in `g++'
				replace corr_dmrp = r(C)[4,1] in `h++'
		}
	} 
	
	
	********************************************
	/* Mean absolute percentage errors (MAPE) */
	********************************************

	local i = 1 
	local j = 1 
	local k = 1 

	gen mape_dis = . 
	gen mape_rmrp = . 
	gen mape_dmrp = .

	foreach x in WeeklyAttend NeverAttend StrongID { 
		foreach y in dis rmrp dmrp { 
			gen ape_`y'_`x' = abs(`y'_`x' - bm_`x') / bm_`x' * 100
			lab var ape_`y'_`x' "Absolute percentage error for `x', method `y'"
		}
	}

	foreach x in WeeklyAttend NeverAttend StrongID { 
		foreach c in 2008 2014 { 
			display "`x' `c'"
				mean ape_dis_`x' if year==`c' 
				replace mape_dis = e(b)[1,1] in `i++'
		}
	}
	foreach x in WeeklyAttend NeverAttend StrongID { 
		foreach c in 2008 2014 { 
			display "`x' `c'"
				mean ape_rmrp_`x' ape_dmrp_`x' if year==`c' 
				replace mape_rmrp = e(b)[1,1] in `j++'
				replace mape_dmrp = e(b)[1,2] in `k++'
		}
	}
	/* We run these loops separately for the disaggregation estimates, 
	   because otherwise the "mean" command only calculates statistics 
	   for the repeated/dynamic MRP estimates for states with non-missing 
	   disaggregation estimates. */ 

	
	********************************
	/* Mean absolute errors (MAE) */
	********************************
	
	local l = 1 
	local m = 1 
	local n = 1 

	gen mae_dis = . 
	gen mae_rmrp = . 
	gen mae_dmrp = . 
	
	foreach x in WeeklyAttend NeverAttend StrongID { 
		foreach y in dis rmrp dmrp { 
			gen ae_`y'_`x' = abs(`y'_`x' - bm_`x') 
			lab var ae_`y'_`x' "Absolute error for `x', method `y'"
		}
	}
	
	foreach x in WeeklyAttend NeverAttend StrongID { 
		foreach c in 2008 2014 { 
			display "`x' `c'"
				mean ae_dis_`x' if year==`c' 
				replace mae_dis = e(b)[1,1] in `l++'
		}
	}
	foreach x in WeeklyAttend NeverAttend StrongID { 
		foreach c in 2008 2014 { 
			display "`x' `c'"
				mean ae_rmrp_`x' ae_dmrp_`x' if year==`c' 
				replace mae_rmrp = e(b)[1,1] in `m++'
				replace mae_dmrp = e(b)[1,2] in `n++'
		}
	}


	************************************************
	/* MAEs 10 smallest states (excl. DC, AK, HI) */
	************************************************

	local o = 1 
	local p = 1 

	gen mae10_rmrp = . 
	gen mae10_dmrp = .
	
	/* The 13 smallest states (from small to big): 
	   WY, VT, DC, AK, ND, SD, DE, RI, MT, ME, NH, HI, WV. 
	   DC, AK, HI excluded because not available in each benchmark source. */

	foreach x in WeeklyAttend NeverAttend StrongID { 
		foreach c in 2008 2014 { 
			display "`x' `c'"
				mean ae_rmrp_`x' ae_dmrp_`x' ///
							if year==`c' & ///
							(stnum==9 | stnum==22 | stnum==27 | stnum==29 | ///
								stnum==31 | stnum==40 | stnum==42 | ///
								stnum==47 | stnum==50 | stnum==51)
				replace mae10_rmrp = e(b)[1,1] in `o++'
				replace mae10_dmrp = e(b)[1,2] in `p++'
		}
	}

	
	***************************************************
	/* Finishing up the metrics dataset for Figure 2 */
	***************************************************

	gen label_year = . 
	replace label_year = _n in 1/2 
	replace label_year = _n - 2 in 3/4
	replace label_year = _n - 4 in 5/6
	lab var label_year "Year"
	lab define YEAR_int 1 "2008" 2 "2014"
	lab val label_year YEAR_int 

	gen label_outcome = . 
	replace label_outcome = 1 in 1/2
	replace label_outcome = 2 in 3/4
	replace label_outcome = 3 in 5/6 
	lab var label_outcome "Outcome" 
	lab define OUTCOME_int 1 "Attend almost weekly" ///
							2 "Attend almost never" ///
							3 "Strong identification" 
	lab val label_outcome OUTCOME_int 

	keep label_outcome label_year corr_* mape_* mae_* mae10_*  
	keep if _n<7
	
	save "Performance metrics - intensities", replace


	
************************************************************
///   8.4. Visually displaying the performance metrics   ///
************************************************************


	*******************************************
	/* For religious affiliations (Figure 1) */
	*******************************************
	
	use "Performance metrics - affiliations", clear 

	gen yaxis = . 
	replace yaxis = 25 - label_year in 1/4 
	replace yaxis = 20 - label_year in 5/8 
	replace yaxis = 15 - label_year in 9/12 
	replace yaxis = 10 - label_year in 13/16 
	replace yaxis = 5 - label_year in 17/20

	/* Correlations: */
	tabstat corr*, statistics(min max)
	twoway dot corr_dis yaxis, ///
				horizontal msize(medsmall) msymbol(T) mcolor(black) ndots(40) dcolor(gs5) || /// 
			dot corr_rmrp yaxis, ///
				horizontal msize(medsmall) msymbol(Oh) mcolor(red) ndots(0) || /// 
			dot corr_dmrp yaxis, ///
				horizontal msize(medsmall) msymbol(O) mcolor(red) ndots(0) /// 
					ylabel(1 "14" 2 "08" 3 "01" 4 "None 90" ///
							6 "14" 7 "08" 8 "01" 9 "Catholic 90" ///
							11 "14" 12 "08" 13 "01" 14 "Black Prot 90" ///
							16 "14" 17 "08" 18 "01" 19 "Mainline 90" ///
							21 "14" 22 "08" 23 "01" 24 "Evangelical 90", ///
							angle(horizontal) nogrid labsize(small)) ///
					ytitle("") yscale(range(0 25)) ///
					xtitle("") xlabel(0.4(0.1)1, labsize(small)) ///
				legend(order(1 "Disaggregation" 2 "Repeated MRP" 3 "Dynamic MRP") ///
						size(small) row(1) colgap(3) keygap(1) symxsize(5) ///
						region(lcolor(white)) bmargin(tiny)) /// 
				graphregion(color(white) lcolor(white) icolor(white) ///
							ilcolor(white) margin(medsmall)) ///
				plotregion(lcolor(black)) ///
			title("{bf:Correlation}", ///
					color(black) size(medsmall) margin(0 0 4 0)) ///
		saving("affil_corr", replace)

	/* MAPEs: */ 
	tabstat mape*, statistics(min max)
	twoway dot mape_dis yaxis, ///
				horizontal msize(medsmall) msymbol(T) mcolor(black) ndots(40) dcolor(gs5) || /// 
			dot mape_rmrp yaxis, ///
				horizontal msize(medsmall) msymbol(Oh) mcolor(red) ndots(0) || /// 
			dot mape_dmrp yaxis, ///
				horizontal msize(medsmall) msymbol(O) mcolor(red) ndots(0) /// 
					ylabel(1 "14" 2 "08" 3 "01" 4 "None 90" ///
							6 "14" 7 "08" 8 "01" 9 "Catholic 90" ///
							11 "14" 12 "08" 13 "01" 14 "Black Prot 90" ///
							16 "14" 17 "08" 18 "01" 19 "Mainline 90" ///
							21 "14" 22 "08" 23 "01" 24 "Evangelical 90", ///
							angle(horizontal) nogrid labsize(small)) ///
					ytitle("") yscale(range(0 25)) ///
					xtitle("") xlabel(10(10)70, labsize(small)) ///
				legend(order(1 "Disaggregation" 2 "Repeated MRP" 3 "Dynamic MRP") ///
						size(small) row(1) colgap(3) keygap(1) region(margin(small))) /// 
				graphregion(color(white) lcolor(white) icolor(white) ///
							ilcolor(white) margin(medsmall)) ///
				plotregion(lcolor(black)) ///
			title("{bf:MAPE}", ///
					color(black) size(medsmall) margin(0 0 4 0)) ///
		saving("affil_mape", replace)

	/* MAEs: */ 
	tabstat mae_*, statistics(min max)
	twoway dot mae_dis yaxis, ///
				horizontal msize(medsmall) msymbol(T) mcolor(black) ndots(40) dcolor(gs5) || /// 
			dot mae_rmrp yaxis, ///
				horizontal msize(medsmall) msymbol(Oh) mcolor(red) ndots(0) || /// 
			dot mae_dmrp yaxis, ///
				horizontal msize(medsmall) msymbol(O) mcolor(red) ndots(0) /// 
					ylabel(1 "14" 2 "08" 3 "01" 4 "None 90" ///
							6 "14" 7 "08" 8 "01" 9 "Catholic 90" ///
							11 "14" 12 "08" 13 "01" 14 "Black Prot 90" ///
							16 "14" 17 "08" 18 "01" 19 "Mainline 90" ///
							21 "14" 22 "08" 23 "01" 24 "Evangelical 90", ///
							angle(horizontal) nogrid labsize(small)) ///
					ytitle("") yscale(range(0 25)) ///
					xtitle("") xlabel(0(2)10, labsize(small)) ///
				legend(order(1 "Disaggregation" 2 "Repeated MRP" 3 "Dynamic MRP") ///
						size(small) row(1) colgap(3) keygap(1) region(margin(small))) /// 
				graphregion(color(white) lcolor(white) icolor(white) ///
							ilcolor(white) margin(medsmall)) ///
				plotregion(lcolor(black)) ///
			title("{bf:MAE all states}", ///
					color(black) size(medsmall) margin(0 0 4 0)) ///
		saving("affil_mae", replace)

	/* MAEs (10 smallest states): */ 
	tabstat mae10_*, statistics(min max)
	twoway dot mae10_rmrp yaxis, ///
				horizontal msize(medsmall) msymbol(Oh) mcolor(red) ndots(40) dcolor(gs5) || /// 
			dot mae10_dmrp yaxis, ///
				horizontal msize(medsmall) msymbol(O) mcolor(red) ndots(0) /// 
					ylabel(1 "14" 2 "08" 3 "01" 4 "None 90" ///
							6 "14" 7 "08" 8 "01" 9 "Catholic 90" ///
							11 "14" 12 "08" 13 "01" 14 "Black Prot 90" ///
							16 "14" 17 "08" 18 "01" 19 "Mainline 90" ///
							21 "14" 22 "08" 23 "01" 24 "Evangelical 90", ///
							angle(horizontal) nogrid labsize(small)) ///
					ytitle("") yscale(range(0 25)) ///
					xtitle("") xlabel(0(3)15, labsize(small)) ///
				legend(order(1 "Disaggregation" 2 "Repeated MRP" 3 "Dynamic MRP") ///
						size(small) row(1) colgap(3) keygap(1) region(margin(small))) /// 
				graphregion(color(white) lcolor(white) icolor(white) ///
							ilcolor(white) margin(medsmall)) ///
				plotregion(lcolor(black)) ///
			title("{bf:MAE 10 smallest states}", ///
					color(black) size(medsmall) margin(0 0 4 0)) ///
		saving("affil_mae10", replace)

	/* Combining the panels: */ 
	grc1leg "affil_corr" "affil_mape" "affil_mae" "affil_mae10", ///
				graphregion(color(white) lcolor(white) icolor(white) ///
							ilcolor(white) margin(small)) ///
				plotregion(lcolor(black) lwidth(none)) ///
				legendfrom("affil_corr")
	graph display, ysize(8) xsize(6) 
	gr_edit .legend.DragBy 0 6 
	graph export "affil_all.png", replace

	
	******************************************
	/* For religious intensities (Figure 2) */
	******************************************
	
	use "Performance metrics - intensities", clear 

	gen yaxis = . 
	replace yaxis = 9 - label_year in 1/2 
	replace yaxis = 6 - label_year in 3/4 
	replace yaxis = 3 - label_year in 5/6

	/* Correlations: */
	tabstat corr*, statistics(min max)
	twoway dot corr_dis yaxis, ///
				horizontal msize(medsmall) msymbol(T) mcolor(black) ndots(40) dcolor(gs5) || /// 
			dot corr_rmrp yaxis, ///
				horizontal msize(medsmall) msymbol(Oh) mcolor(red) ndots(0) || /// 
			dot corr_dmrp yaxis, ///
				horizontal msize(medsmall) msymbol(O) mcolor(red) ndots(0) /// 
					ylabel(1 "14" 2 "Strong ID 08" ///
							4 "14" 5 "Never attend 08" ///
							7 "14" 8 "Weekly attend 08", ///
							angle(horizontal) nogrid labsize(medlarge)) ///
					ytitle("") yscale(range(0 9)) ///
					xtitle("") xlabel(0.4(0.1)1, labsize(medlarge)) ///
				legend(order(1 "Disaggregation" 2 "Repeated MRP" 3 "Dynamic MRP") ///
						size(medlarge) row(1) colgap(3) keygap(1) symxsize(5) ///
						region(lcolor(white)) bmargin(tiny)) /// 
				graphregion(color(white) lcolor(white) icolor(white) ///
							ilcolor(white) margin(medsmall)) plotregion(lcolor(black)) ///
			title("{bf:Correlation}", ///
					color(black) size(large) margin(0 0 4 0)) ///
		saving("intense_corr", replace)

	/* MAPEs: */ 
	tabstat mape*, statistics(min max)
	twoway dot mape_dis yaxis, ///
				horizontal msize(medsmall) msymbol(T) mcolor(black) ndots(40) dcolor(gs5) || /// 
			dot mape_rmrp yaxis, ///
				horizontal msize(medsmall) msymbol(Oh) mcolor(red) ndots(0) || /// 
			dot mape_dmrp yaxis, ///
				horizontal msize(medsmall) msymbol(O) mcolor(red) ndots(0) /// 
					ylabel(1 "14" 2 "Strong ID 08" ///
							4 "14" 5 "Never attend 08" ///
							7 "14" 8 "Weekly attend 08", ///
							angle(horizontal) nogrid labsize(medlarge)) ///
					ytitle("") yscale(range(0 9)) ///
					xtitle("") xlabel(10(10)70, labsize(medlarge)) ///
				legend(order(1 "Disaggregation" 2 "Repeated MRP" 3 "Dynamic MRP") ///
						size(small) row(1) colgap(3) keygap(1) region(margin(small))) ///
				graphregion(color(white) lcolor(white) icolor(white) ///
							ilcolor(white) margin(medsmall)) ///
				plotregion(lcolor(black)) ///
			title("{bf:MAPE}", ///
					color(black) size(large) margin(0 0 4 0)) ///
		saving("intense_mape", replace)

	/* MAEs: */ 
	tabstat mae_*, statistics(min max)
	twoway dot mae_dis yaxis, ///
				horizontal msize(medsmall) msymbol(T) mcolor(black) ndots(40) dcolor(gs5) || /// 
			dot mae_rmrp yaxis, ///
				horizontal msize(medsmall) msymbol(Oh) mcolor(red) ndots(0) || /// 
			dot mae_dmrp yaxis, ///
				horizontal msize(medsmall) msymbol(O) mcolor(red) ndots(0) /// 
					ylabel(1 "14" 2 "Strong ID 08" ///
							4 "14" 5 "Never attend 08" ///
							7 "14" 8 "Weekly attend 08", ///
							angle(horizontal) nogrid labsize(medlarge)) ///
					ytitle("") yscale(range(0 9)) ///
					xtitle("") xlabel(0(5)25, labsize(medlarge)) ///
				legend(order(1 "Disaggregation" 2 "Repeated MRP" 3 "Dynamic MRP") ///
						size(small) row(1) colgap(3) keygap(1) region(margin(small))) /// 
				graphregion(color(white) lcolor(white) icolor(white) ///
							ilcolor(white) margin(medsmall)) ///
				plotregion(lcolor(black)) ///
			title("{bf:MAE all states}", ///
					color(black) size(large) margin(0 0 4 0)) ///
		saving("intense_mae", replace)

	/* MAEs (10 smallest states): */ 
	tabstat mae10_*, statistics(min max)
	twoway dot mae10_rmrp yaxis, ///
				horizontal msize(medsmall) msymbol(Oh) mcolor(red) ndots(40) dcolor(gs5) || /// 
			dot mae10_dmrp yaxis, ///
				horizontal msize(medsmall) msymbol(O) mcolor(red) ndots(0) /// 
					ylabel(1 "14" 2 "Strong ID 08" ///
							4 "14" 5 "Never attend 08" ///
							7 "14" 8 "Weekly attend 08", ///
							angle(horizontal) nogrid labsize(medlarge)) ///
					ytitle("") yscale(range(0 9)) ///
					xtitle("") xlabel(0(5)25, labsize(medlarge)) ///
				legend(order(1 "Repeated MRP" 2 "Dynamic MRP") ///
						size(small) row(1) colgap(3) keygap(1) region(margin(small))) /// 
				graphregion(color(white) lcolor(white) icolor(white) ///
							ilcolor(white) margin(medsmall)) ///
				plotregion(lcolor(black)) ///
			title("{bf:MAE 10 smallest states}", ///
					color(black) size(large) margin(0 0 4 0)) ///
		saving("intense_mae10", replace)

	/* Combining the panels: */ 
	grc1leg "intense_corr" "intense_mape" "intense_mae" "intense_mae10", ///
				graphregion(color(white) lcolor(white) icolor(white) ///
							ilcolor(white) margin(small)) ///
				plotregion(lcolor(black) lwidth(none)) ///
				legendfrom("intense_corr")
	graph display, ysize(4) xsize(6)
	gr_edit .legend.DragBy 0 12
	graph export "intense_all.png", replace

	
	
**************************************************************
///   8.5. One-to-one comparisons to benchmark estimates   ///
**************************************************************

	
use "Benchmark comparisons", clear 

replace year = 2007 if year==2008 & source=="Pew RLS"


	**********************
	/* ARIS (Figure A1) */
	**********************
	
	foreach x in Evangelical Mainline BlackProt Catholic None { 
		twoway scatter bm_`x' dmrp_`x' ///
					if year==1990 & source=="NSRI/ARIS", mcolor(green%30) || ///
				scatter bm_`x' dmrp_`x' ///
					if year==2001 & source=="NSRI/ARIS", mcolor(orange%30) || ///
				scatter bm_`x' dmrp_`x' ///
					if year==2008 & source=="NSRI/ARIS", mcolor(red%30) || ///
				lfit bm_`x' dmrp_`x' ///
					if source=="NSRI/ARIS", lpattern(dash) lcolor(black) || ///
				line bm_`x' bm_`x' ///
					if source=="NSRI/ARIS", lpattern(solid) lcolor(black) ///
			legend(off) ///
			title("{bf:`x'}", margin(0 0 2 0) color(black)) ///
			xtitle("Dynamic MRP", margin(0 0 0 2)) ///
			ytitle("As-if-truth", margin(0 2 0 0)) ///
			xlabel(0(20)60) xscale(range(0 65)) ///
			ylabel(0(20)60, angle(horizontal) gmin gmax) yscale(range(0 65)) ///
			graphregion(color(white) lcolor(white) icolor(white) ///
						ilcolor(white) margin(medsmall)) ///
			plotregion(lcolor(black)) ///
		saving("`x'_ARIS", replace)
	}
	twoway scatter bm_Evangelical dmrp_Evangelical ///
				if year==1990 & source=="NSRI/ARIS", mcolor(green%30) || ///
			scatter bm_Evangelical dmrp_Evangelical ///
				if year==2001 & source=="NSRI/ARIS", mcolor(orange%30) || ///
			scatter bm_Evangelical dmrp_Evangelical ///
				if year==2008 & source=="NSRI/ARIS", mcolor(red%30) || ///
			lfit bm_Evangelical dmrp_Evangelical ///
				if source=="NSRI/ARIS", lpattern(dash) lcolor(black) || ///
			line bm_Evangelical bm_Evangelical ///
				if source=="NSRI/ARIS", lpattern(solid) lcolor(black) ///
		legend(order(1 "1990" 2 "2001" 3 "2008" 4 "Overall fit" 5 "Identity") ///
				title("{bf:ARIS}", color(black) margin(0 0 2 0)) ///
				size(medlarge) rows(5) region(lcolor(white)) ring(0) pos(0)) ///
		yscale(off) xscale(off) ///
		graphregion(color(white) lcolor(white) icolor(white) ///
					ilcolor(white) margin(medsmall)) ///
		plotregion(lcolor(black) lwidth(none)) 
	gr_edit .plotregion1.draw_view.setstyle, style(no) 
	gr_edit .legend.DragBy 0 4
	graph save "legend_ARIS", replace

	graph combine "Evangelical_ARIS" "Mainline_ARIS" "BlackProt_ARIS" /// 
					"Catholic_ARIS" "None_ARIS" "legend_ARIS", ///
				graphregion(color(white) lcolor(white) icolor(white) ///
							ilcolor(white) margin(small)) ///
				plotregion(lcolor(black) lwidth(none)) 
	graph export "Attenuation_ARIS.png", replace 

	
	*************************************
	/* PRLS - affiliations (Figure A2) */
	*************************************

	foreach x in Evangelical Mainline BlackProt Catholic None { 
		twoway scatter bm_`x' dmrp_`x' ///
					if year==2007 & source=="Pew RLS", mcolor(green%30) || ///
				scatter bm_`x' dmrp_`x' ///
					if year==2014 & source=="Pew RLS", mcolor(orange%30) || ///
				lfit bm_`x' dmrp_`x' ///
					if source=="Pew RLS", lpattern(dash) lcolor(black) || ///
				line bm_`x' bm_`x' ///
					if source=="Pew RLS", lpattern(solid) lcolor(black) ///
			legend(off) ///
			title("{bf:`x'}", margin(0 0 2 0) color(black)) ///
			xtitle("Dynamic MRP", margin(0 0 0 2)) ///
			ytitle("As-if-truth", margin(0 2 0 0)) ///
			xlabel(0(20)60) ///
			ylabel(0(20)60, angle(horizontal) gmin gmax) ///
			graphregion(color(white) lcolor(white) icolor(white) ///
						ilcolor(white) margin(medsmall)) ///
			plotregion(lcolor(black)) ///
		saving("`x'_PRLS", replace)
	}
	twoway scatter bm_Evangelical dmrp_Evangelical ///
				if year==2007 & source=="Pew RLS", mcolor(green%30) || ///
			scatter bm_Evangelical dmrp_Evangelical ///
				if year==2014 & source=="Pew RLS", mcolor(orange%30) || ///
			lfit bm_Evangelical dmrp_Evangelical ///
				if source=="Pew RLS", lpattern(dash) lcolor(black) || ///
			line bm_Evangelical bm_Evangelical ///
				if source=="Pew RLS", lpattern(solid) lcolor(black) ///
		legend(order(1 "2007" 2 "2014" 3 "Overall fit" 4 "Identity") ///
				title("{bf:PRLS}", color(black) margin(0 0 2 0)) ///
				size(medlarge) rows(5) region(lcolor(white)) ring(0) pos(0)) ///
		yscale(off) xscale(off) ///
		graphregion(color(white) lcolor(white) icolor(white) ///
					ilcolor(white) margin(medsmall)) ///
		plotregion(lcolor(black) lwidth(none)) 
	gr_edit .plotregion1.draw_view.setstyle, style(no) 
	gr_edit .legend.DragBy 0 4
	graph save "legend_PRLS", replace

	graph combine "Evangelical_PRLS" "Mainline_PRLS" "BlackProt_PRLS" /// 
					"Catholic_PRLS" "None_PRLS" "legend_PRLS", ///
				graphregion(color(white) lcolor(white) icolor(white) ///
							ilcolor(white) margin(small)) ///
				plotregion(lcolor(black) lwidth(none)) 
	graph export "Attenuation_PRLS1.png", replace 

	
	*********************
	/* AVA (Figure A3) */
	*********************
	
	foreach x in Evangelical Mainline BlackProt Catholic None { 
		twoway scatter bm_`x' dmrp_`x' ///
					if year==2014 & source=="AVA", mcolor(green%30) || ///
				scatter bm_`x' dmrp_`x' ///
					if year==2016 & source=="AVA", mcolor(orange%30) || ///
				scatter bm_`x' dmrp_`x' ///
					if year==2018 & source=="AVA", mcolor(red%30) || ///
				lfit bm_`x' dmrp_`x' ///
					if source=="AVA", lpattern(dash) lcolor(black) || ///
				line bm_`x' bm_`x' ///
					if source=="AVA", lpattern(solid) lcolor(black) ///
			legend(off) ///
			title("{bf:`x'}", margin(0 0 2 0) color(black)) ///
			xtitle("Dynamic MRP", margin(0 0 0 2)) ///
			ytitle("As-if-truth", margin(0 2 0 0)) ///
			xlabel(0(20)60) ///
			ylabel(0(20)60, angle(horizontal) gmin gmax) ///
			graphregion(color(white) lcolor(white) icolor(white) ///
						ilcolor(white) margin(medsmall)) ///
			plotregion(lcolor(black)) ///
		saving("`x'_AVA", replace)
	}
	twoway scatter bm_Evangelical dmrp_Evangelical ///
				if year==2014 & source=="AVA", mcolor(green%30) || ///
			scatter bm_Evangelical dmrp_Evangelical ///
				if year==2016 & source=="AVA", mcolor(orange%30) || ///
			scatter bm_Evangelical dmrp_Evangelical ///
				if year==2018 & source=="AVA", mcolor(red%30) || ///
			lfit bm_Evangelical dmrp_Evangelical ///
				if source=="AVA", lpattern(dash) lcolor(black) || ///
			line bm_Evangelical bm_Evangelical ///
				if source=="AVA", lpattern(solid) lcolor(black) ///
		legend(order(1 "2014" 2 "2016" 3 "2018" 4 "Overall fit" 5 "Identity") ///
				title("{bf:AVA}", color(black) margin(0 0 2 0)) ///
				size(medlarge) rows(5) region(lcolor(white)) ring(0) pos(0)) ///
		yscale(off) xscale(off) ///
		graphregion(color(white) lcolor(white) icolor(white) ///
					ilcolor(white) margin(medsmall)) ///
		plotregion(lcolor(black) lwidth(none)) 
	gr_edit .plotregion1.draw_view.setstyle, style(no) 
	gr_edit .legend.DragBy 0 4
	graph save "legend_AVA", replace

	graph combine "Evangelical_AVA" "Mainline_AVA" "BlackProt_AVA" /// 
					"Catholic_AVA" "None_AVA" "legend_AVA", ///
				graphregion(color(white) lcolor(white) icolor(white) ///
				ilcolor(white) margin(small)) plotregion(lcolor(black) lwidth(none)) 
	graph export "Attenuation_AVA.png", replace 
	
	
	************************************
	/* PRLS - intensities (Figure A4) */
	************************************
	
	foreach x in WeeklyAttend NeverAttend StrongID { 
		twoway scatter bm_`x' dmrp_`x' if year==2007 & source=="Pew RLS", mcolor(green%30) || ///
				scatter bm_`x' dmrp_`x' if year==2014 & source=="Pew RLS", mcolor(orange%30) || ///
				lfit bm_`x' dmrp_`x' if source=="Pew RLS", lpattern(dash) lcolor(black) || ///
				line bm_`x' bm_`x' if source=="Pew RLS", lpattern(solid) lcolor(black) ///
			legend(off) title("{bf:`x'}", margin(0 0 2 0) color(black)) ///
			xtitle("Dynamic MRP", margin(0 0 0 2)) ///
			ytitle("As-if-truth", margin(0 2 0 0)) ///
			xlabel(0(20)80) ///
			ylabel(0(20)80, angle(horizontal) gmin gmax) ///
			graphregion(color(white) lcolor(white) icolor(white) ///
						ilcolor(white) margin(medsmall)) ///
			plotregion(lcolor(black)) ///
		saving("`x'_PRLS", replace)
	}

	graph combine "WeeklyAttend_PRLS" "NeverAttend_PRLS" "StrongID_PRLS" "legend_PRLS", ///
				graphregion(color(white) lcolor(white) icolor(white) ///
							ilcolor(white) margin(small)) ///
				plotregion(lcolor(black) lwidth(none)) 
	graph export "Attenuation_PRLS2.png", replace 
	