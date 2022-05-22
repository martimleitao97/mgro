
program define mgro

				* Author: Martim Leitão, Católica Lisbon and Center of Economics for Prosperity
				* Date: 22/05/2022
				version 17.0 
				syntax varlist(min=3 max=3) [if] [ ,LCOLOR1(string) ///
											   LCOLOR2(string) ///
											   LWidth(string) ///
											   LPattern(string) ///
											   title(string) ///
											   RING(real 1) ///
											   LEGend(string) ///
											   GRAph(string) ///
											   TPOSITION(string) ///
											   PREserve(string) ///
											   LEGPOsition(real 1) ///
											   DEtail(string)]
											   
				// Begin command for the case "preserve=true"
											   											   
				if "`preserve'" == "true" {
				    
				*set trace on
				tempfile original
				save `original'		
				
				// Take into account "if" syntax
				
				marksample touse
				keep if `touse'
											   
				tokenize `varlist'
	
				di as blue "Time Variable chosen: `1' " 
				di as blue "Unit of Analysis Chosen: `3' "
				di as blue "Continuous Variable: `2' "
							
				*set trace on
				
				keep `3' `2' `1'
				sort `3' `1'
				
				qui: bys `1': egen double tot=total(`2')
	
				set more off
								
				// Intensive Growth

				qui: sum `1', de
				local minp = `r(min)'+1
				
				forvalues x=`minp'/`r(max)' {
				
					preserve 
						qui: keep if `1'==`x' | `1'==`x'-1
						qui: bys `3': egen nval=nvals(`1')
						qui: keep if nval==2
						qui: drop nval
						qui: bys `3' (`1'): gen double dif = (`2'[_n]-`2'[_n-1])/tot[_n-1]	
						gcollapse (sum) growth_int_`x'=dif, by(`1')
						cap: scalar intensi_`x' = growth_int_`x'[2]
					restore
					
					qui: gen intg_`x' = intensi_`x' if `1'==`x'
					
				}
				
				qui: egen intgrow = rowmax(intg_*)	
				drop intg_*
				
				// Entrant contribution
				
				qui: sum `1', de
				local minp = `r(min)'+1
				
				forvalues x=`minp'/`r(max)' {
		
				preserve 
					qui: keep if `1'==`x' | `1'==`x'-1
					qui: bys `3': egen nval=nvals(`1')
					qui: keep if nval==1
					drop nval
					qui: keep if `1'==`x'
					gen double xj = `2'
					egen entrant_g_`x' = total(xj)	
					cap: scalar entrant_g_`x' = entrant_g_`x'[1]
				restore 
				
				qui: gen entra_g_`x' = entrant_g_`x' if `1'==`x'
				
				}
				
				qui: egen entr_g = rowmax(entra_g_*)
				qui: replace entr_g = entr_g/tot[_n-1]
				drop entra_g_*
				
				// Overall Growth
				
				qui: sum `1', de
				gcollapse (mean) tot intgrow entr_g, by(`1')
				sort `1'
				qui: gen double tot_growth = ((tot[_n]-tot[_n-1])/tot[_n-1])*100
				keep tot_growth `1' intgrow entr_g
				qui: replace intgrow = intgrow*100
				
				// Extensive Margins
				
				qui: gen exten = tot_growth - intgrow

				
				// Exiting Effect
				
				qui: gen double exiting_g = exten - entr_g
				
				order exten exiting_g entr_g intgrow tot_growth
				
				// Plot 
				
				qui: drop if `1' == `minp'-1			
				qui: sum `1', de
				
				if "`graph'" == "true" {
					
				tw line intgrow `1', lcolor(`lcolor2') lp(`lpattern') lwidth(`lwidth') || line exten `1', lcolor(`lcolor1') lp(`lpattern') lwidth(`lwidth') ///
				xlabel(`r(min)'(1)`r(max)', nogrid tposition(`tposition') labsize(small)) ///
				yline(0, lpattern(dash) lcolor(gray) lwidth(thin)) ylabel(#6, nogrid tposition(`tposition') angle(horizontal) ///
				labsize(small)) xtitle("",size(small)) ytitle("Growth Rate (in %) ", size(small)) ///
				legend(order(1 "Intensive Growth" 2 "Extensive Growth") ring(`ring') size(vsmall) col(1) position(`legposition')) ///
				graphregion(ilpattern(blank)) graphregion(color(white)) title(`title', size(small))	legend(`legend')
				
				}
				
				else{
				di "Graph was omitted by default. Include graph(true) to see graph."				
				}
				
				// Table
				ren exten Extensive
				label var Extensive "Entrant + Exiting Effect"
				ren exiting_g Exiting
				label var Exiting "Exiting Effect"
				ren entr_g Entrant
				label var Entrant "Entrant Effect"
				ren intgrow Intensive
				label var Intensive "Incumbent/Intensive Effect"
				ren tot_growth Total
				label var Total "Total Growth Rate"
				
				if "`detail'"== "true" {
				    
					foreach var in Extensive Exiting Entrant Intensive {
					    gen Share_`var' = (`var'/Total)*100
						label var Share_`var' "Share Explained by `var' (in %)"
				
					}
					
					order Extensive Share_Extensive Exiting Share_Exiting Entrant Share_Entrant Intensive Share_Intensive Total 
					di "Growth Decomposition (in %):"
					tabstat Intensive Share_Intensive Extensive Share_Extensive Exiting Share_Exiting Entrant Share_Entrant ///
					Total, statistics( mean ) by(`1') nototal noseparator format(%5.0g)
				}
				
				else {
				// Show resuts in table
				di "Growth Decomposition (in %):"
				tabstat Intensive Extensive Exiting Entrant Total, statistics( mean ) by(`1') nototal noseparator format(%5.0g)
								
				}
				
				use `original', clear
				
				}
				
				// This begins the no preserve case 
				
				else {
				    
				// Take into account "if" condition from syntax
				
				marksample touse
				keep if `touse'
				    
				tokenize `varlist'
	
				di as blue "Time Variable chosen: `1' " 
				di as blue "Unit of Analysis Chosen: `3' "
				di as blue "Continuous Variable: `2' "
								
				*set trace on
				
				keep `3' `2' `1'
				sort `3' `1'
				
				qui: bys `1': egen double tot=total(`2')
	
				set more off
								
				// Intensive Growth

				qui: sum `1', de
				local minp = `r(min)'+1
				
				forvalues x=`minp'/`r(max)' {
				
					preserve 
						qui: keep if `1'==`x' | `1'==`x'-1
						qui: bys `3': egen nval=nvals(`1')
						qui: keep if nval==2
						qui: drop nval
						qui: bys `3' (`1'): gen double dif = (`2'[_n]-`2'[_n-1])/tot[_n-1]	
						gcollapse (sum) growth_int_`x'=dif, by(`1')
						cap: scalar intensi_`x' = growth_int_`x'[2]
					restore
					
					qui: gen intg_`x' = intensi_`x' if `1'==`x'
					
				}
				
				qui: egen intgrow = rowmax(intg_*)	
				drop intg_*
				
				// Entrant contribution
				
				qui: sum `1', de
				local minp = `r(min)'+1
				
				forvalues x=`minp'/`r(max)' {
		
				preserve 
					qui: keep if `1'==`x' | `1'==`x'-1
					qui: bys `3': egen nval=nvals(`1')
					qui: keep if nval==1
					drop nval
					qui: keep if `1'==`x'
					gen double xj = `2'
					egen entrant_g_`x' = total(xj)	
					cap: scalar entrant_g_`x' = entrant_g_`x'[1]
				restore 
				
				qui: gen entra_g_`x' = entrant_g_`x' if `1'==`x'
				
				}
				
				qui: egen entr_g = rowmax(entra_g_*)
				qui: replace entr_g = entr_g/tot[_n-1]
				drop entra_g_*
				
				// Overall Growth
				
				qui: sum `1', de
				gcollapse (mean) tot intgrow entr_g, by(`1')
				sort `1'
				qui: gen double tot_growth = ((tot[_n]-tot[_n-1])/tot[_n-1])*100
				keep tot_growth `1' intgrow entr_g
				qui: replace intgrow = intgrow*100
				
				// Extensive Margins
				
				qui: gen exten = tot_growth - intgrow

				
				// Exiting Effect
				
				qui: gen double exiting_g = exten - entr_g
				
				order exten exiting_g entr_g intgrow tot_growth
				
				// Plot 
				
				qui: drop if `1' == `minp'-1			
				qui: sum `1', de
				
				if "`graph'" == "true" {
					
				tw line intgrow `1', lcolor(`lcolor2') lp(`lpattern') lwidth(`lwidth') || line exten `1', lcolor(`lcolor1') lp(`lpattern') lwidth(`lwidth') ///
				xlabel(`r(min)'(1)`r(max)', nogrid tposition(`tposition') labsize(small)) ///
				yline(0, lpattern(dash) lcolor(gray) lwidth(thin)) ylabel(#6, nogrid tposition(`tposition') angle(horizontal) ///
				labsize(small)) xtitle("",size(small)) ytitle("Growth Rate (in %) ", size(small)) ///
				legend(order(1 "Intensive Growth" 2 "Extensive Growth") ring(`ring') size(vsmall) col(1) position(`legposition')) ///
				graphregion(ilpattern(blank)) graphregion(color(white)) title(`title', size(small))	legend(`legend')
				
				}
				
				else{
				di "Graph was omitted by default. Include graph(true) to see graph."				
				}
				
				// Table
				ren exten Extensive
				label var Extensive "Entrant + Exiting Effect"
				ren exiting_g Exiting
				label var Exiting "Exiting Effect"
				ren entr_g Entrant
				label var Entrant "Entrant Effect"
				ren intgrow Intensive
				label var Intensive "Incumbent/Intensive Effect"
				ren tot_growth Total
				label var Total "Total Growth Rate"
				
				if "`detail'"== "true" {
				    
					foreach var in Extensive Exiting Entrant Intensive {
					    gen Share_`var' = (`var'/Total)*100
						label var Share_`var' "Share Explained by `var' (in %)"
				
					}
					
					order Extensive Share_Extensive Exiting Share_Exiting Entrant Share_Entrant Intensive Share_Intensive Total 
					di "Growth Decomposition (in %):"
					tabstat Intensive Share_Intensive Extensive Share_Extensive Exiting Share_Exiting Entrant Share_Entrant ///
					Total, statistics( mean ) by(`1') nototal noseparator format(%5.0g)
				}
				
				else {
				// Show resuts in table
				di "Growth Decomposition (in %):"
				tabstat Intensive Extensive Exiting Entrant Total, statistics( mean ) by(`1') nototal noseparator format(%5.0g)
								
				}
					
				}
end


