context("ANOPA:: Testing conversion function (2 sections)")
    # expect_output( str(res), "data.frame")
    # expect_equal( "ggplot" %in% class(plt), TRUE)
    # expect_message( p <- superbPlot(dta2a, etc ))
    # expect_error(), expect_warning(), expect_condition( , class = "")

test_that("Testing the input format conversions (1/2)", {

	old <- options() 
	on.exit(options(old)) 
	options(ANOPA.feedback = 'none')

	# shortcuts... 
	# 1B = 1 between-subject factor	
	# 2B = 2 between-subject factors	
	# 1W = 1 within-subject factor	
	# 2W = 2 within-subject factors	
	# WB = mixed, 1 between and 1 within, factors.
	c1B <- minimalBSExample		#compiled, 	BS	(3; 	suj var; N = 175)
	w1B <- ctow(c1B, "s", "n")
	l1B <- ctol(c1B, "s", "n")

	c2B <- twoWayExample			#compiled,  BS 	(2x3;   suj var; N = 72)
	w2B <- ctow(c2B, "success", "total")
	l2B <- ctol(c2B, "success", "total")

	w1W <- minimalWSExample		#wide, 		WS	((3); 	19 suj;  N = 19)
    c1W <- wtoc(w1W, c("bpre","b1week","b5week"), "n")
	l1W <- wtol(w1W, c("bpre","b1week","b5week") )

	w2W <- twoWayWithinExample	#wide, 		WS	((2x3);   30 suj; N = 30)
	c2W <- wtoc(w2W, c("r11","r12","r13","r21","r22","r23"), "n")
	l2W <- wtol(w2W, c("r11","r12","r13","r21","r22","r23") )

	wWB <- minimalMxExample		#wide, 		Mix	(3x(3);   8 suj; N = 24)
	cWB <- wtoc(wWB, c("bpre","bpost","b1week","b5week"), "n")
	lWB <- wtol(wWB, c("bpre","bpost", "b1week", "b5week") )


	### testing Compiled to Wide	
	w1Bb <- ctow(c1B, "s", "n")
        expect_equal( dim(w1Bb), c(175,2) )
	w2Bb <- ctow(c2B, "success", "total")
        expect_equal( dim(w2Bb), c(72,3) )
	# ctow won't work for 1W, 2W and WB as it cannot restore WS factors


	### testing Compiled to Long
	l1Bc <- ctol(c1B,  "s", "n" )
        expect_equal( dim(l1Bc), c(175, 4) )
	l2Bc <- ctol(c2B,  "success", "total" )
        expect_equal( dim(l2Bc), c(72, 5) )
	# ctol won't work for 1W, 2W and WB as it cannot restore WS factors


	### testing Wide to Long
	l1Bd <- wtol(w1B, "s" )
        expect_equal( dim(l1Bd), c(175, 4) )
        expect_equal( all(l1B == l1Bd), TRUE )
	l2Bd <- wtol(w2B, "success" )
        expect_equal( dim(l2Bd), c(72, 5) )
        expect_equal( all(l2B == l2Bd), TRUE )
	l1Wd <- wtol(w1W, c("bpre","b1week","b5week") )
        expect_equal( dim(l1Wd), c(57, 3) )
        expect_equal( all(l1W == l1Wd), TRUE )
	l2Wd <- wtol(w2W, c("r11","r12","r13","r21","r22","r23") )
        expect_equal( dim(l2Wd), c(180, 3) )
        expect_equal( all(l2W == l2Wd), TRUE )
	lWBd <- wtol(wWB, c("bpre","bpost","b1week","b5week") )
        expect_equal( dim(lWBd), c(108, 4) )
        expect_equal( all(lWB == lWBd), TRUE )


	### testing Wide to Compiled: looses correlation
	c1Be <- wtoc(w1B, "s", "n")
        expect_equal( dim(c1Be), c(3, 3) )
        expect_equal( all(c1B == c1Be), TRUE )
	c2Be <- wtoc(w2B, "success", "total")
        expect_equal( dim(c2Be), c(6,4) )
        # expect_equal( all(c2B == c2Be), TRUE ) # needs sorting...
	c1We <- wtoc(w1W, c("bpre","b1week","b5week"), "n")
        expect_equal( dim(c1We), c(1, 5) )
        expect_equal( all(c1W == c1We), TRUE )
	c2We <- wtoc(w2W, c("r11","r12","r13","r21","r22","r23"), "n")
        expect_equal( dim(c2We), c(1, 8) )
        expect_equal( all(c2W == c2We), TRUE )
	cWBe <- wtoc(wWB, c("bpre","bpost","b1week","b5week"), "n")
        expect_equal( dim(cWBe), c(3, 7) )
        expect_equal( all(cWB == cWBe), TRUE )


	### testing Long to Wide
	w1Bf <- ltow(l1B, "Id", "Variable","Value" )
        expect_equal( dim(w1Bf), c(175, 2) )
        expect_equal( all(w1Bf == w1B), TRUE )
	w2Bf <- ltow(l2B, "Id", "Variable", "Value" )
        expect_equal( dim(w2Bf), c(72, 3) )
        expect_equal( all(w2Bf == w2B), TRUE )
	w1Wf <- ltow(l1W, "Id", "Variable", "Value" )
        expect_equal( dim(w1Wf), c(19, 3) )
        expect_equal( all(w1Wf == w1W), TRUE )
	w2Wf <- ltow(l2W, "Id", "Variable", "Value" )
        expect_equal( dim(w2Wf), c(30, 6) )
        expect_equal( all(w2Wf == w2W), TRUE )
	wWBf <- ltow(lWB, "Id", "Variable", "Value" )
        expect_equal( dim(wWBf), c(27, 5) )
        expect_equal( all(wWBf == wWB), TRUE )


	### testing Long to Compiled: looses correlation
	c1Bg <- ltoc(l1B,  "Id", "Variable", "Value", "n")
        expect_equal( dim(c1Bg), c(3, 3) )
        expect_equal( all(c1Bg == c1Be), TRUE )
	c2Bg <- ltoc(l2B,  "Id", "Variable", "Value", "n")
        expect_equal( dim(c2Bg), c(6,4) )	
        expect_equal( all(c2Bg == c2Be), TRUE )
	c1Wg <- ltoc(l1W,  "Id", "Variable", "Value", "n")
        expect_equal( dim(c1Wg), c(1, 5) )
        expect_equal( all(c1Wg==c1We), TRUE )
	c2Wg <- ltoc(l2W,  "Id", "Variable", "Value", "n")
        expect_equal( dim(c2Wg), c(1,8) )	
        expect_equal( all(c2Wg == c2We), TRUE )
	cWBg <- ltoc(lWB,  "Id", "Variable", "Value", "n")
        expect_equal( dim(cWBg), c(3,7) )	
        expect_equal( all(cWBg == cWBe), TRUE )


})

test_that("Testing compiled formats (2/2)", {

	# shortcuts... 
	# 1B = 1 between-subject factor	
	# 2B = 2 between-subject factors	
	# 1W = 1 within-subject factor	
	# 2W = 2 within-subject factors	
	# WB = mixed, 1 between and 1 within, factors.
	c1B <- minimalBSExample		#compiled, 	BS	(3; 	suj var; N = 175)
	w1B <- ctow(c1B, "s", "n")
	l1B <- ctol(c1B, "s", "n")

	c2B <- twoWayExample			#compiled,  BS 	(2x3;   suj var; N = 72)
	w2B <- ctow(c2B, "success", "total")
	l2B <- ctol(c2B, "success", "total")

	w1W <- minimalWSExample		#wide, 		WS	((3); 	19 suj;  N = 19)
    c1W <- wtoc(w1W, c("bpre","b1week","b5week"), "n")
	l1W <- wtol(w1W, c("bpre","b1week","b5week") )

	w2W <- twoWayWithinExample	#wide, 		WS	((2x3);   30 suj; N = 30)
	c2W <- wtoc(w2W, c("r11","r12","r13","r21","r22","r23"), "n")
	l2W <- wtol(w2W, c("r11","r12","r13","r21","r22","r23") )

	wWB <- minimalMxExample		#wide, 		Mix	(3x(3);   8 suj; N = 24)
	cWB <- wtoc(wWB, c("bpre","bpost","b1week","b5week"), "n")
	lWB <- wtol(wWB, c("bpre","bpost", "b1week", "b5week") )

	# these are no longer errors with compiled data 
    c1W <- wtoc(w1W, c("bpre","b1week","b5week"), "n")
	c2W <- wtoc(w2W, c("r11","r12","r13","r21","r22","r23"), "n")
	cWB <- wtoc(wWB, c("bpre","bpost","b1week","b5week"), "n")

	frmc1W <- {cbind(bpre,b1week,b5week);n;uAlpha} ~ . 
    r1Wa <- anopa(frmc1W, c1W, WSFactors = "moment(3)")
	expect_equal( round(r1Wa$omnibus[1,1]-0.003512,7), 0, tol=0.00001)
    
	frmc2W <- {cbind(r11,r12,r13,r21,r22,r23);n;uAlpha} ~ .
    r2Wa <- anopa(frmc2W, c2W, WSFactors = c("G(3)", "F(2)") )
	expect_equal( round(r2Wa$omnibus[1,1]-0.050236,7), 0, tol=0.00001)

	frmcWB <- {cbind(bpre,b1week,b5week); n; uAlpha} ~ Status
    rWBa <- anopa(frmcWB, cWB, WSFactors = "moment(3)")
	expect_equal( round(rWBa$omnibus[1,1]-0.009051,7), 0, tol=0.00001)

})

