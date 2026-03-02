




xtset id year

reghdfe EMP did $c ///
    if year >= 2007, ///
    absorb(id year) ///
    vce(cluster prov)

estimates store did_main



didplacebo did_main, ///
    treatvar(did) ///
    pbotime(1(1)7)

	
	
	
	
	
	
didplacebo did_main, ///
    treatvar(did) ///
    pbounit ///
    seed(12345)

	
	
didplacebo did_main, ///
    treatvar(did) ///
    pbomix(2) ///
    seed(12345)
