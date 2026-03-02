
 cd"/Users/libingchen/Desktop/NTUP_BL_20251024_Package"
 
 global c "econ1 gov lpr infra open urban edu3" 
 
 global s "builtarea green_rate ave_dis adk_ec spat_compact" ///
         " ln_bh_2010_t ln_bh_2020_t c_ln_bh_2010_t c_ln_bh_2020_t" ///
         " c_did ln_bh_2010_t_did ln_bh_2020_t_did" ///
         " productmarket factormarket labor1 labor2" ///
         " east south sea center metro capitalcity oldbase trans resource"

 global r "event_time pre5 pre4 pre3 pre2 pre1 current aft1 aft2 aft3 aft4 aft5"///
          "IV3 IV2 m0_sample m0_fid_1 did_dynamic placebo1 placebo2 placebo3 placebo4 placebo5"///
          "_est_did_main _first_treat_ did_rail did_talent did_smart"

 global d "min_distance dist_0_50 dist_50_100 dist_100_150 dist_150_200 dist_200_250"///
          "dist_250_300 dist_300_350 dist_350_400 dist_400_450 dist_450_500"

 // descriptive
 asdoc tabstat EMP did $c  if year >= 2007 & treat == 0, ///
    stat(N mean sd) c(s) save(treat0.rtf)
 
 asdoc tabstat EMP did $c if year >= 2007 & treat == 1, ///
    stat(N mean sd) c(s) save(treat1.rtf)		 
	
  asdoc ttable3 EMP did $c if year >= 2007, by(treat) save(ttest.rtf)	

 *================================================================
 *                        baseline
 *================================================================
 // ====== psm ======
   psmatch2 treat $c , outcome(EMP) n(10) caliper(0.1)
   pstest $c ,both
 // ====== baseline regression ======
   qui reghdfe EMP did if year >= 2007,ab(city year) vce(r)
   eststo m1 
   qui reghdfe EMP did $c if year >= 2007,ab(city year)  vce(r)
   eststo m2
   qui reghdfe EMP did if _weight!=.&year >= 2007 ,ab(city year) vce(r)
   eststo m3
   qui reghdfe EMP did $c if _weight!=.&year >= 2007 ,ab(city year) vce(r)
   eststo m4  
   estfe m1 m2 m3 m4 , labels(city "CITY" year "YEAR")
   esttab m1 m2 m3 m4 using baseline.rtf , append nogap compress ///
       star( * 0.10 ** 0.05 *** 0.01 ) ///
	   b(%9.4f) se(%9.4f) sca(N r2_within) title("baseline") ///
	   indicate("CITY =*.city" "YEAR=*.year") order(did)
	  
 *================================================================
 *                       Robustness check
 *================================================================
 // ====== parallel trends assumption ======
gen first_treat_year = .
bysort city (year): replace first_treat_year = year if did == 1 & missing(first_treat_year)
egen min_treat_year = min(first_treat_year), by(city)

bysort city (year): replace first_treat_year = min_treat_year
drop min_treat_year
gen event_time = year - first_treat_year if first_treat_year != .

forvalues i = -5(1)5 {
    local vname = cond(`i' < 0, "pre" + string(abs(`i')), ///
                       cond(`i' == 0, "current", "aft" + string(`i')))
    gen `vname' = (event_time == `i')
}

tab first_treat_year if treat == 1
tab event_time if treat == 1
sum pre5-pre1 aft1-aft5

   reg EMP pre4-pre1 current aft1-aft5 $c if year >= 2007 ,vce(cluster prov)
   eststo m1
   reg EMP pre4-pre1 current aft1-aft5 $c if _weight!=.&year >= 2007 ,vce(cluster prov)
   eststo m2
   esttab m1 m2 using "event_study_table.rtf", append nogap compress ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.3f) t(%9.3f) sca(N r2_a) title("Event Study: Grant Log Applications") ///
    indicate("Controls = $c")

    coefplot m1, keep(pre* current aft*) vertical recast(connected) ///
    coeflabels( ///
        pre5 = "Pre–5" pre4 = "Pre–4" pre3 = "Pre–3" pre2 = "Pre–2" ///
        current = "Current" ///
        aft1 = "Post+1" aft2 = "Post+2" aft3 = "Post+3" aft4 = "Post+4" aft5 = "Post+5") ///
    yline(0, lcolor(gs8)) ///
    xline(5, lcolor(red) lpattern(dash)) ///
    ciopts(recast(rarea) color(%30 140 200%30)) ///
    lcolor(navy) mcolor(navy) msymbol(circle) ///
    scheme(s1mono) name(event_area, replace) ///
    ytitle("Estimated coefficients", size(medsmall)) ///
    xtitle("Period relative to the event", size(medsmall)) ///
    xlabel(, angle(25) labsize(small)) ///
    ylabel(-1.5(0.5)2.0, nogrid labsize(small)) /// 
    legend(order(1  "95% confidence interval" 2 "Point estimate") ///
           region(lstyle(none)) ///
           pos(6) ring(0) row(1) cols(2) size(small))
    graph export " parallel trends assumption.svg", replace

    coefplot m2, keep(pre* current aft*) vertical recast(connected) ///
    coeflabels( ///
        pre5 = "Pre–5" pre4 = "Pre–4" pre3 = "Pre–3" pre2 = "Pre–2" ///
        current = "Current" ///
        aft1 = "Post+1" aft2 = "Post+2" aft3 = "Post+3" aft4 = "Post+4" aft5 = "Post+5") ///
    yline(0, lcolor(gs8)) ///
    xline(5, lcolor(red) lpattern(dash)) ///
    ciopts(recast(rarea) color(%30 140 200%30)) ///
    lcolor(navy) mcolor(navy) msymbol(circle) ///
    scheme(s1mono) name(event_area, replace) ///
    ytitle("Estimated coefficients_PSM", size(medsmall)) ///
    xtitle("Period relative to the event", size(medsmall)) ///
    xlabel(, angle(25) labsize(small)) ///
    ylabel(-1.5(0.5)2.0, nogrid labsize(small)) /// 
    legend(order(1  "95% confidence interval" 2 "Point estimate") ///
           region(lstyle(none)) ///
           pos(6) ring(0) row(1) cols(2) size(small))
    graph export " parallel trends assumption_PSM.svg", replace

  // ====== Placebo test ======
  cap rm placebo.dta
  permute post beta = _b[c.treat#c.post] se = _se[c.treat#c.post] ///
          df = e(df_r), seed(10000) ///
          reps(500) saving("placebo.dta"): ///
          reghdfe EMP c.treat#c.post if _weight!=.&year >= 2007, ab(city year) vce(r)  	  	
  preserve

  use "placebo.dta" , clear 
  gen t_value = beta / se
  gen p_value = 2 * ttail(df, abs(beta/se))
 
  #delimit ;
		 twoway (kdensity beta, yaxis(1) lc(black) ) 
		 (scatter p_value beta, yaxis(2) m(Oh) mc(black))  , 
		 xline(0, lc(black*0.5) lp(solid)) 
		 xlabel(,format(%4.2f))
		 yline(0.05, lc(black*0.5) lp(dash) axis(2))
		 xtitle("Estimator", size(*0.8))
		 ytitle("Density", size(*0.8)) 
		 ylabel(, nogrid format(%4.1f) labsize(small)) 
		 ytitle("P Value", size(*0.8) axis(2)) 
		 ylabel(, nogrid format(%4.1f) labsize(small) axis(2))
		 legend(ring(2) order(1 "Estimator" 2 "P Value") pos(6) rows(1) )
		 graphregion(color(white)) scheme(s1mono) ;
  #delimit cr
  restore 
  graph export "placebo.svg"
  gr close 	
 
 use placebo.dta, replace
 gen t_value = beta / se
 gen p_value = 2 * ttail(df, abs(t_value))
 count if p_value > 0.05
 scalar prop_p_gt_0_1 = r(N) / _N
 display "ratio_p>0.1: " prop_p_gt_0_1

foreach y of numlist 1/14 {
    gen rugged_`y' = rugged * yd`y'
    gen rail_`y'   = rail1933 * yd`y'
}
 
 use data
 
 // ====== IV2-2SLS ======
 reghdfe did IV2 $c if year >= 2007, ab(city year) vce(robust)
  eststo m1
 ivreghdfe EMP $c (did=IV2) if year >= 2007, ab(city year) vce(robust)
  eststo m2
 reghdfe did IV3 $c if year >= 2007, ab(city year) vce(robust)
 eststo m3
  ivreghdfe EMP $c (did=IV3) if year >= 2007, ab(city year) vce(robust)
 eststo m4

  estfe m1 m2 m3 m4 , labels(city "CITY" year "YEAR")
 esttab m1 m2 m3 m4 using 2sls.rtf , append nogap compress ///
       star( * 0.10 ** 0.05 *** 0.01 ) ///
	   b(%9.4f) se(%9.4f) sca(N r2_within) title("2sls") ///
	    indicate("Controls = $c" "CITY =*.city" "YEAR=*.year") order(IV2 IV3)
		
// ====== Ruling out other confounding policies ======			
 reghdfe EMP did did_rail $c if year >= 2007,ab(city year)  vce(r)	
 eststo m1	
 reghdfe EMP did did_talent $c if year >= 2007,ab(city year)  vce(r)
 eststo m2
 reghdfe EMP did did_smart $c if year >= 2007,ab(city year)  vce(r)
 eststo m3
 estfe m1 m2 m3 , labels(city "CITY" year "YEAR")
 esttab m1 m2 m3 using policies.rtf , append nogap compress ///
       star( * 0.10 ** 0.05 *** 0.01 ) ///
	   b(%9.4f) se(%9.4f) sca(N r2_within) title("2sls") ///
	    indicate("Controls = $c" "CITY =*.city" "YEAR=*.year") order(did did_rail did_talent did_smart)
		
// ====== DML Estimator ======
  set seed 12345
  global Y EMP
  global D did
  global X econ1 gov labor infra open urban edu3

  // === model 1：pystacked + rf ===
  ddml init partial, kfolds(2)
  ddml E[Y|X]: pystacked $Y $X, type(reg) method(rf)
  ddml E[D|X]: pystacked $D $X, type(reg) method(rf)
  ddml crossfit
  ddml estimate, robust
  eststo m1

 // === model 2：pystacked + gradboost ===
  ddml init partial, kfolds(2)
  ddml E[Y|X]: pystacked $Y $X, type(reg) method(gradboost)
  ddml E[D|X]: pystacked $D $X, type(reg) method(gradboost)
  ddml crossfit
  ddml estimate, robust
  eststo m2
  
  esttab m1 m2 using ddml.rtf, replace nogap compress ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    b(%9.4f) se(%9.4f) ///
    scalar("Observations" N) ///
    title("DML Estimation using Different Learners") ///
    label

 *================================================================
 *                       mechanisms1
 *================================================================	
 // ====== 2D ======
  reghdfe EMP c.did##c.builtarea $c if year >= 2009,ab(city year)  vce(r)
  eststo m1
  reghdfe EMP c.did##c.green_rate $c if year >= 2009,ab(city year)  vce(r)
  eststo m2
  estfe m1 m2 , labels(city "CITY" year "YEAR")
  esttab m1 m2 using 2D.rtf , append nogap compress ///
       star( * 0.10 ** 0.05 *** 0.01 ) ///
	   b(%9.4f) se(%9.4f) sca(N r2_within) title("2D") ///
	   indicate("Controls = $c""CITY =*.city" "YEAR=*.year") order(did)
  
  // ====== compactness ======
    reghdfe EMP c.did##c.ave_dis $c if year >= 2007,ab(city year)  vce(r)
	eststo m1
	reghdfe EMP c.did##c.adk_ec $c if year >= 2007,ab(city year)  vce(r)
	eststo m2
	reghdfe EMP c.did##c.spat_compact $c if year >= 2007,ab(city year)  vce(r)
    eststo m3
	estfe m1 m2 , labels(city "CITY" year "YEAR")
	esttab m1 m2 m3 using compact.rtf , append nogap compress ///
       star( * 0.10 ** 0.05 *** 0.01 ) ///
	   b(%9.4f) se(%9.4f) sca(N r2_within) title("2D") ///
	   indicate("Controls = $c""CITY =*.city" "YEAR=*.year") order(did)
	   
 // ====== 3D ======
 winsor2 ln_bh_2010_t ln_bh_2020_t , cut(1 99) replace 
 
 center did ln_bh_2010_t  ln_bh_2020_t
 gen ln_bh_2010_t_did = c_ln_bh_2010_t * c_did
 gen ln_bh_2020_t_did = c_ln_bh_2020_t * c_did
 
 
 reghdfe EMP did ln_bh_2010_t ln_bh_2010_t_did $c if year>= 2009 ,absorb(city year) vce(robust)
   eststo m1
 reghdfe EMP did ln_bh_2020_t ln_bh_2020_t_did $c if year>= 2009 ,absorb(city year) vce(robust)
	 eststo m2

   estfe m1 m2 , labels(city "CITY" year "YEAR")
   esttab m1 m2 using 3D.rtf , append nogap compress ///
       star( * 0.10 ** 0.05 *** 0.01 ) ///
	   b(%9.4f) se(%9.4f) sca(N r2_within) title("3D") ///
	   indicate("Controls = $c""CITY =*.city" "YEAR=*.year") order(did)

 *================================================================
 *                       mechanisms2
 *================================================================
 // ====== market ======	
	reghdfe productmarket did $c if year >= 2007,ab(city year)  vce(r)
	eststo m1
	reghdfe factormarket did $c if year >= 2007,ab(city year)  vce(r)
	eststo m2  
    estfe m1 m2 , labels(city "CITY" year "YEAR")
    esttab m1 m2 using market.rtf , append nogap compress ///
       star( * 0.10 ** 0.05 *** 0.01 ) ///
	   b(%9.4f) se(%9.4f) sca(N r2_within) title("market") ///
	   indicate("Controls = $c""CITY =*.city" "YEAR=*.year") order(did)

  // ====== labor ======
	 reghdfe labor1 did $c if year >= 2007,ab(city year)  vce(r)
	 eststo m1
	 reghdfe labor2 did $c if year >= 2007,ab(city year)  vce(r)
	 eststo m2
	 estfe m1 m2 , labels(city "CITY" year "YEAR")
     esttab m1 m2 using labor.rtf , append nogap compress ///
       star( * 0.10 ** 0.05 *** 0.01 ) ///
	   b(%9.4f) se(%9.4f) sca(N r2_within) title("labor") ///
	   indicate("Controls = $c""CITY =*.city" "YEAR=*.year") order(did)
	   
 *================================================================
 *                      Hetro
 *================================================================
 // ======Geographical  ======
  reghdfe EMP  c.did#c.east $c if year >= 2007,ab(city year) vce(r)
  eststo m1
  reghdfe EMP  c.did#c.south $c if year >= 2007,ab(city year) vce(r) 
  eststo m2	
  reghdfe EMP  c.did#c.sea $c if year >= 2008,ab(city year) vce(r) 
  eststo m3
  estfe m1 m2 m3 , labels(code "CODE" year "YEAR" ind "INDU")
     esttab m1 m2 m3  using Geographical.rtf , append nogap compress ///
       star( * 0.10 ** 0.05 *** 0.01 ) ///
	   b(%9.4f) se(%9.4f) sca(N r2_within) title("City Level") ///
	   indicate("Controls = $c" "CITY =*.city" "YEAR=*.year") order(c.did#c.east c.did#c.south c.did#c.sea)
	   
 coefplot                                                    ///
    (m1, keep("c.did#c.east")   label("East")   mcolor(black) ciopts(lcolor(black) recast(rcap))) ///
    (m2, keep("c.did#c.south")  label("South")  mcolor(black) ciopts(lcolor(black) recast(rcap))) ///
    (m3, keep("c.did#c.sea")    label("Coastal") mcolor(black) ciopts(lcolor(black) recast(rcap))), ///
    vertical                                                     ///
    recast(scatter)                                              ///
    ciopts(recast(rcap) lcolor(black) lwidth(medthin))           ///
    yline(0, lcolor(gs8))                                        ///
    scheme(s1mono) name(geo_once, replace)                       ///
    ytitle("Estimated effect (DID × region)")                    ///
    xtitle("")                                                   ///
    xlabel(1 "East" 2 "South" 3 "Coastal", labsize(small))       ///
    ylabel(, nogrid labsize(small))                              ///
    plotregion(margin(b+12))                                     ///
    legend(order(1 "Point estimate" 4 "95% CI")                  ///
           pos(6) ring(1) col(2) size(small)                     ///
           region(lstyle(solid) lcolor(black)))

graph export "Geographical_interactions.svg", replace

 // ======City Level  ======
	 reghdfe EMP c.did#c.center $c if year >= 2007,ab(city year) vce(r) 
	 eststo m4  
	 reghdfe EMP c.did#c.metro $c if year >= 2007,ab(city year) vce(r) 
	 eststo m5
     reghdfe EMP c.did#c.capitalcity $c if year >= 2008,ab(city year) vce(r) 
	 eststo m6
	 estfe m4 m5 m6 , labels(code "CODE" year "YEAR" ind "INDU")
     esttab m4 m5 m6  using City_Level.rtf , append nogap compress ///
       star( * 0.10 ** 0.05 *** 0.01 ) ///
	   b(%9.4f) se(%9.4f) sca(N r2_within) title("City Level") ///
	   indicate("Controls = $c" "CITY =*.city" "YEAR=*.year") order(c.did#c.center c.did#c.metro c.did#c.capitalcity)
   
  coefplot                                                     ///
    (m4, keep("c.did#c.center")        label("Regional Center")   ///
         mcolor(black)  msymbol(O)     ciopts(recast(rcap) lcolor(black))) ///
    (m5, keep("c.did#c.metro")         label("Large City")        ///
         mcolor(black)  msymbol(D)     ciopts(recast(rcap) lcolor(black))) ///
    (m6, keep("c.did#c.capitalcity")   label("Provincial Capital") ///
         mcolor(black)  msymbol(S)     ciopts(recast(rcap) lcolor(black))), ///
    vertical                                                     ///
    recast(scatter)                                              ///
    ciopts(recast(rcap) lcolor(black) lwidth(medthin))           ///
    yline(0, lcolor(gs8))                                        ///
    scheme(s1mono) name(city_level_once, replace)                ///
    ytitle("Estimated effect (DID × city level)")                ///
    xtitle("")                                                   ///
    xlabel(1 "Regional Center" 2 "Large City" 3 "Provincial Capital", labsize(small)) ///
    ylabel(, nogrid labsize(small))                              ///
    plotregion(margin(b+12))                                     ///
    legend(order(1 "Point estimate" 4 "95% CI")                  ///
           pos(6) ring(1) col(2) size(small)                     ///
           region(lstyle(solid) lcolor(black)))

  graph export "CityLevel_interactions.svg", replace


  // ======City ECON  ======  
	reghdfe EMP c.did#c.oldbase $c if year >= 2007,ab(city year) vce(r) 
	eststo m7
	reghdfe EMP c.did#c.trans $c if year >= 2007,ab(city year) vce(r) 
	eststo m8
	reghdfe EMP c.did#c.resource $c if year >= 2007,ab(city year) vce(r)
	eststo m9
    estfe m7 m8 m9 , labels(code "CODE" year "YEAR" ind "INDU")
    esttab m7 m8 m9  using City_ECON.rtf , append nogap compress ///
       star( * 0.10 ** 0.05 *** 0.01 ) ///
	   b(%9.4f) se(%9.4f) sca(N r2_within) title("City ECON") ///
	   indicate("Controls = $c" "CITY =*.city" "YEAR=*.year") order(c.did#c.oldbase c.did#c.trans c.did#c.resource)

   coefplot                                                     ///
    (m7, keep("c.did#c.oldbase")   label("Old Industrial Base") ///
         msymbol(O)  mcolor(black)  ciopts(recast(rcap) lcolor(black))) ///
    (m8, keep("c.did#c.trans")     label("Transport Hub")       ///
         msymbol(D)  mcolor(black)  ciopts(recast(rcap) lcolor(black))) ///
    (m9, keep("c.did#c.resource")  label("Resource-based City") ///
         msymbol(S)  mcolor(black)  ciopts(recast(rcap) lcolor(black))), ///
    vertical                                                     ///
    recast(scatter)                                              ///
    yline(0, lcolor(gs8))                                        ///
    scheme(s1mono) name(econ_once, replace)                      ///
    ytitle("Estimated effect (DID × economic type)")             ///
    xtitle("")                                                   ///
    xlabel(1 "Old Base" 2 "Transport" 3 "Resource", labsize(small)) ///
    ylabel(, nogrid labsize(small))                              ///
    plotregion(margin(b+12))                                     ///
    legend(order(1 "Point estimate" 4 "95% CI")                  ///
           pos(6) ring(1) col(2) size(small)                     ///
           region(lstyle(solid) lcolor(black)))

    graph export "City_ECON_interactions.svg", replace
  
	   

 *================================================================
 *                        spatial spillover
 *================================================================	   
	
  reghdfe EMP did dist_0_50-dist_350_400 $c if year >= 2007,ab(year)  vce(r)
   eststo m1
   estfe m1 , labels(year "YEAR")
   esttab m1 using dis.rtf , append nogap compress ///
       star( * 0.10 ** 0.05 *** 0.01 ) ///
	   b(%9.4f) se(%9.4f) sca(N r2_a) title("dis") ///
	   indicate("Controls = $c" "YEAR=*.year") order(did)
	   
  coefplot m1, ///
    keep(dist_0_50 dist_50_100 dist_100_150 dist_150_200 ///
         dist_200_250 dist_250_300 dist_300_350) ///
    vertical omitted ///
    coeflabels( ///
        dist_0_50     = "0–50" ///
        dist_50_100   = "50–100" ///
        dist_100_150  = "100–150" ///
        dist_150_200  = "150–200" ///
        dist_200_250  = "200–250" ///
        dist_250_300  = "250–300" ///
        dist_300_350  = "300–350") ///
    levels(95) recast(connected) ///
    lcolor(black) lwidth(medthick) lpattern(solid) ///
    ciopts(recast(rcap) lcolor(black) lwidth(medium)) ///
    mlcolor(black) mfcolor(white) msize(medium) msymbol(circle) ///
    yline(0, lcolor(gs10) lwidth(medium)) ///
    xlabel(, angle(45) labsize(small) labcolor(black)) ///
    ytitle("Estimated Effect", size(medsmall)) ///
    xtitle("Distance to Treated City (km)", size(medsmall)) ///
    ylabel(-1.5(0.5)1.5, nogrid labsize(small) angle(horizontal)) ///
    legend(order(1 "Point Estimate" 2 "95% CI") ///
           pos(6) row(1) size(small) ///
           region(lstyle(solid) lcolor(black) lwidth(medium))) ///
    graphregion(color(white) lcolor(black) lwidth(medium)) ///
    plotregion(lcolor(black) lwidth(medium)) ///
    scheme(plotplain) name(spill_paper_border, replace) ///
    title("")

graph export "spillover_effects_paper_bordered_italicx.svg", width(1000) replace
