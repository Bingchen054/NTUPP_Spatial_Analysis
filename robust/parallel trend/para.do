
 cd "/Users/libingchen/Desktop/NTUP_BL/robust/parallel trend"
 use para_tr.dta ,clear 
 
 global c "econ1 gov lpr infra open urban edu3"

 gen period = year - first_treat_year 
 tab period ,mi
 replace period = -6 if period<-6
 replace period = 6 if period>6 & period!=.
 
 forvalues i = 6(-1)1 {
  gen pre`i' = (period == -`i' & treat == 1) 
}

 gen current = (period == 0 & treat == 1)
 
 forvalues j = 1(1)6 {
  gen aft`j' = (period == `j' & treat == 1)
 }
 
 
 reghdfe EMP pre6-pre1 aft1-aft6 $c current if year >= 2007, absorb(city year) vce(r)
 eststo m1

   
coefplot m1, baselevels omitted ///
    keep(pre6 pre5 pre4 pre3 pre2 zz0 current aft1 aft2 aft3 aft4 aft5 aft6) ///
    order(pre6 pre5 pre4 pre3 pre2 zz0 current aft1 aft2 aft3 aft4 aft5 aft6) ///
    vertical ///
    recast(connected) ///
    coeflabels( ///
        pre6    = "Pre 6" ///
        pre5    = "Pre 5" ///
        pre4    = "Pre 4" ///
        pre3    = "Pre 3" ///
        pre2    = "Pre 2" ///
		zz0     = "Pre 1" ///
        current = "Current" ///
        aft1   = "Post 1" ///
        aft2   = "Post 2" ///
        aft3   = "Post 3" ///
        aft4   = "Post 4" ///
        aft5   = "Post 5" ///
        aft6   = "Post 6" ///
    ) ///
    yline(0, lcolor(gs8)) ///
    xline(6, lcolor(red) lpattern(dash)) ///
    ciopts(recast(rarea) color(navy%20)) ///
    lcolor(navy) mcolor(navy) msymbol(circle) ///
    scheme(s1mono) ///
    name(event_area, replace) ///
    ytitle("Estimated coefficients", size(medsmall)) ///
    xtitle("Period relative to the event", size(medsmall)) ///
    xlabel(, angle(25) labsize(small)) ///
    ylabel(-.2(0.5).3, nogrid labsize(small)) ///
    legend(order(1 "95% confidence interval" 2 "Point estimate") ///
           region(lstyle(none)) pos(6) ring(0) row(1) cols(2) size(small))
		   
    graph export " para_A.png", replace
 
reghdfe EMP pre6-pre1 aft1-aft6  current if year >= 2007, absorb(city year) vce(r)
 eststo m2
coefplot m2, baselevels omitted ///
    keep(pre6 pre5 pre4 pre3 pre2 zz0 current aft1 aft2 aft3 aft4 aft5 aft6) ///
    order(pre6 pre5 pre4 pre3 pre2 zz0 current aft1 aft2 aft3 aft4 aft5 aft6) ///
    vertical ///
    recast(connected) ///
    coeflabels( ///
        pre6    = "Pre 6" ///
        pre5    = "Pre 5" ///
        pre4    = "Pre 4" ///
        pre3    = "Pre 3" ///
        pre2    = "Pre 2" ///
		zz0     = "Pre 1" ///
        current = "Current" ///
        aft1   = "Post 1" ///
        aft2   = "Post 2" ///
        aft3   = "Post 3" ///
        aft4   = "Post 4" ///
        aft5   = "Post 5" ///
        aft6   = "Post 6" ///
    ) ///
    yline(0, lcolor(gs8)) ///
    xline(6, lcolor(red) lpattern(dash)) ///
    ciopts(recast(rarea) color(navy%20)) ///
    lcolor(navy) mcolor(navy) msymbol(circle) ///
    scheme(s1mono) ///
    name(event_area, replace) ///
    ytitle("Estimated coefficients", size(medsmall)) ///
    xtitle("Period relative to the event", size(medsmall)) ///
    xlabel(, angle(25) labsize(small)) ///
    ylabel(-.2(0.5).3, nogrid labsize(small)) ///
    legend(order(1 "95% confidence interval" 2 "Point estimate") ///
           region(lstyle(none)) pos(6) ring(0) row(1) cols(2) size(small))
	    
		graph export " para_B(wt_ct).png", replace
 	
reghdfe EMP pre6-pre1 aft1-aft6 $c  current if _weight!=.&year >= 2007, absorb(city year) vce(r)
 eststo m3
coefplot m3, baselevels omitted ///
    keep(pre6 pre5 pre4 pre3 pre2 zz0 current aft1 aft2 aft3 aft4 aft5 aft6) ///
    order(pre6 pre5 pre4 pre3 pre2 zz0 current aft1 aft2 aft3 aft4 aft5 aft6) ///
    vertical ///
    recast(connected) ///
    coeflabels( ///
        pre6    = "Pre 6" ///
        pre5    = "Pre 5" ///
        pre4    = "Pre 4" ///
        pre3    = "Pre 3" ///
        pre2    = "Pre 2" ///
		zz0     = "Pre 1" ///
        current = "Current" ///
        aft1   = "Post 1" ///
        aft2   = "Post 2" ///
        aft3   = "Post 3" ///
        aft4   = "Post 4" ///
        aft5   = "Post 5" ///
        aft6   = "Post 6" ///
    ) ///
    yline(0, lcolor(gs8)) ///
    xline(6, lcolor(red) lpattern(dash)) ///
    ciopts(recast(rarea) color(navy%20)) ///
    lcolor(navy) mcolor(navy) msymbol(circle) ///
    scheme(s1mono) ///
    name(event_area, replace) ///
    ytitle("Estimated coefficients", size(medsmall)) ///
    xtitle("Period relative to the event", size(medsmall)) ///
    xlabel(, angle(25) labsize(small)) ///
    ylabel(-.2(0.5).3, nogrid labsize(small)) ///
    legend(order(1 "95% confidence interval" 2 "Point estimate") ///
           region(lstyle(none)) pos(6) ring(0) row(1) cols(2) size(small))
 	graph export " para_C(psm).png", replace
	
	
	
	
did_imputation EMP id year first_treat, ///
    controls($c) ///
    horizons(0/6) ///
    pretrend(6) ///
    fe(id year) ///
    autosample ///
    tol(1000) ///
    maxit(2000)

event_plot, ///
    default_look ///
    stub_lag(tau#) ///
    stub_lead(pre#) ///
    together ///
    plottype(connected) ///
    ciplottype(rarea) ///
    alpha(0.1) ///
    
    lag_opt(msymbol(circle) mcolor(navy) lcolor(navy) lwidth(medium)) ///
    lag_ci_opt(color(navy%20 navy%20) lpattern(solid)) ///
    
    lead_opt(msymbol(circle) mcolor(navy) lcolor(navy) lwidth(medium)) ///
    lead_ci_opt(color(navy%20 navy%20)) ///
    
    graph_opt( ///
        xtitle("Period relative to the event", size(medsmall)) ///
        ytitle("Estimated coefficients", size(medsmall)) ///
        xlabel(-6(1)6, angle(25) labsize(small)) ///
        ylabel(-.2(0.5).3, nogrid labsize(small)) ///
        yline(0, lcolor(gs8)) ///
        xline(6, lcolor(red) lpattern(dash)) ///
        graphregion(color(white)) ///
        bgcolor(white) ///
        scheme(s1mono) ///
        legend(order(1 "95% confidence interval" 2 "Point estimate") ///
               region(lstyle(none)) pos(6) ring(0) row(1) cols(2) size(small)) ///
    ) ///
    name(event_area, replace)

graph export "para_D(imp).png", replace

	
eventstudyinteract EMP ///
    pre6-pre1 ///
    aft1-aft6 ///
    $c, ///
    absorb(id year) ///
    cohort(first_treat) ///
    control_cohort(never_treat) ///
    vce(r)





did2s EMP, ///
    first_stage(id year $c) ///
    second_stage(aft1-aft6 pre6-pre1) ///
    treatment(did) ///
    cluster(prov_id)


 coefplot m1 , keep(pre* current aft*) vertical recast(connect) color(black) ///
             coeflabels(pre6 = "-6" ///
			            pre5 = "-5" ///
                        pre4 = "-4" ///
                        pre3 = "-3" ///
                        pre2 = "-2" ///
                        pre1 = "-1" ///
						current = "0" ///
                        aft1 = "+1" ///
                        aft2 = "+2" ///
                        aft3 = "+3" ///
                        aft4 = "+4" ///
                        aft5 = "+5" ///
                        aft6 = "+6" ) ///
         yline(0 , lp(dash)) xline(7, lp(dash)) ylabel(,format(%4.2f) nogrid) ///
		 ciopts(recast(rcap) msize(medium) lc(black)) ///
         msymbol(circle_hollow) plotre(color(white)) ///
         name(a1,replace) order(pre6 pre5 pre4 pre3 pre2 pre1 current) omit

	
reghdfe EMP did $c if year >= 2007, absorb(city year) vce(r)

