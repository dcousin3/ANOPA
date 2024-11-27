context("ANOPA:: Testing anopa() function (3 sections)")
    # expect_output( str(res), "data.frame")
    # expect_equal( "ggplot" %in% class(plt), TRUE)
    # expect_message( p <- superbPlot(dta2a, etc ))
    # expect_error(), expect_warning(), expect_condition( , class = "")


test_that("Testing anopa on 5 simple data set designs (1/3)", {

	old <- options() 
	on.exit(options(old)) 
	options(ANOPA.feedback = 'none')

	#short names
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
	cWB <- wtoc(wWB, c("bpre", "bpost", "b1week", "b5week"), "n")
	lWB <- wtol(wWB, c("bpre", "bpost", "b1week", "b5week") )


	# all the examples with one BS factor (w/l/c)		DONE
	frmw1B <- s ~ state
	r1Bb <- anopa(frmw1B, w1B)
		expect_equal( dim(r1Bb$wideData), c(175,2))
		expect_equal( round(r1Bb$omnibus[1,1]-0.03238,7), 0, tol=0.00001)

	frml1B <- Value ~ state * Variable | Id
	r1Bc <- anopa(frml1B, l1B)
		expect_equal( dim(r1Bc$wideData), c(175,2))
		expect_equal( round(r1Bc$omnibus[1,1]-0.03238,7), 0, tol=0.00001)

	frmc1B <- {s;n} ~ state
	r1Ba <- anopa(frmc1B, c1B)
		expect_equal( dim(r1Ba$wideData), c(175,2))
		expect_equal( round(r1Ba$omnibus[1,1]-0.03238,7), 0, tol=0.00001)


	# all the examples with one WS factor (w/l)			DONE
	frmc1W <- {cbind(bpre, b1week, b5week); n; uAlpha} ~ .
	r1Wa <- anopa(frmc1W, c1W, WSFactors = "moment(3)")
		expect_equal( dim(r1Wa$wideData), c(19,3))
		expect_equal( round(r1Wa$omnibus[1,1]-0.003512184,7), 0, tol=0.00001)

	frmw1W <- cbind(bpre, b1week, b5week) ~ .
	r1Wb <- anopa(frmw1W, w1W, WSFactors = "moment(3)")
		expect_equal( dim(r1Wb$wideData), c(19,3))
		expect_equal( round(r1Wb$omnibus[1,1]-0.003512184,7), 0, tol=0.00001)

	frml1W <- Value ~  Variable | Id
	r1Wc <- anopa(frml1W, l1W, WSFactors="Moment(3)")
		expect_equal( dim(r1Wc$wideData), c(19,3))
		expect_equal( round(r1Wc$omnibus[1,1]-0.003512184,7), 0, tol=0.00001)


	# all the examples with two BS factors (w/l/c)		DONE
	frmc2B <- {success;total} ~ Class * Difficulty
	r2Ba <- anopa(frmc2B, c2B)
		expect_equal( dim(r2Ba$wideData), c(72,3))
		expect_equal( round(r2Ba$omnibus[1,1]-0.0325691,7), 0, tol=0.00001)
		expect_equal( round(r2Ba$omnibus[2,1]-0.136787,7), 0, tol=0.00001)

	frmw2B <- success ~ Class * Difficulty
	r2Bb <- anopa(frmw2B, w2B)
		expect_equal( dim(r2Bb$wideData), c(72,3))
		expect_equal( round(r2Ba$omnibus[1,1]-0.0325691,7), 0, tol=0.00001)
		expect_equal( round(r2Ba$omnibus[2,1]-0.136787,7), 0, tol=0.00001)

	frml2B <- Value ~ Class * Difficulty * Variable | Id
	r2Bc <- anopa(frml2B, l2B)
		expect_equal( dim(r2Bc$wideData), c(72,3))
		expect_equal( round(r2Ba$omnibus[1,1]-0.0325691,7), 0, tol=0.00001)
		expect_equal( round(r2Ba$omnibus[2,1]-0.136787,7), 0, tol=0.00001)


	# all the examples with two WS factors (w/l)		DONE
	frmc2W <- {cbind(r11,r12,r13,r21,r22,r23);n;uAlpha} ~ .
	r2Wa <- anopa(frmc2W, c2W, WSFactors = c("G(3)", "F(2)"))
		expect_equal( dim(r2Wa$wideData), c(30,6))
		expect_equal( round(r2Wa$omnibus[1,1]-0.0502356,7), 0, tol=0.00001)
		expect_equal( round(r2Wa$omnibus[2,1]-0.00732517,7), 0, tol=0.00001)

	frmw2W <- cbind(r11,r12,r13,r21,r22,r23) ~ .
	r2Wb <- anopa(frmw2W, w2W, WSFactors = c("G(3)", "F(2)"))
		expect_equal( dim(r2Wb$wideData), c(30,6))
		expect_equal( round(r2Wb$omnibus[1,1]-0.0502356,7), 0, tol=0.00001)
		expect_equal( round(r2Wb$omnibus[2,1]-0.00732517,7), 0, tol=0.00001)

	frml2W <- Value ~ Variable | Id
	r2Wc <- anopa(frml2W, l2W, WSFactors=c("G(3)","F(2)") )
		expect_equal( dim(r2Wc$wideData), c(30,6))
		expect_equal( round(r2Wc$omnibus[1,1]-0.0502356,7), 0, tol=0.00001)
		expect_equal( round(r2Wc$omnibus[2,1]-0.00732517,7), 0, tol=0.00001)


	# all the examples with BW factors (w/l)		    DONE
	frmcWB <- {cbind(bpre, bpost, b1week, b5week); n; uAlpha} ~ Status
	rWBa <- anopa(frmcWB, cWB, WSFactors = "Moment(4)" )
		expect_equal( dim(rWBa$wideData), c(27,5))
		expect_equal( round(rWBa$omnibus[1,1]-0.006472,7), 0, tol=0.00001)
		expect_equal( round(rWBa$omnibus[2,1]-0.174546,7), 0, tol=0.00001)
        
	frmwWB <- cbind(bpre, bpost, b1week, b5week) ~ Status
	rWBb <- anopa(frmwWB, wWB, WSFactors = "Moment(4)" )
		expect_equal( dim(rWBb$wideData), c(27,5))
		expect_equal( round(rWBb$omnibus[1,1]-0.006472,7), 0, tol=0.00001)
		expect_equal( round(rWBb$omnibus[2,1]-0.174546,7), 0, tol=0.00001)

	frmlWB <- Value ~ Status * Variable | Id
	rWBc <- anopa(frmlWB, lWB, WSFactors="Moment(4)")
		expect_equal( dim(rWBc$wideData), c(27,5))
		expect_equal( round(rWBc$omnibus[1,1]-0.006472,7), 0, tol=0.00001)
		expect_equal( round(rWBc$omnibus[2,1]-0.174546,7), 0, tol=0.00001)


})


test_that("Testing paper's examples (2/3)", {

    # first example of the paper
    res <- anopa( {nSuccess; nParticipants} ~ DistractingTask, ArticleExample1)
	expect_equal( dim(res$wideData), c(97,2))
    expect_equal( res$omnibus[1,1], 0.036803, tolerance = 0.0001)
    expect_equal( res$omnibus[2,1], 0.010478, tolerance = 0.0001)

    # second example of the paper
    res <-anopa( {s;n} ~ MofDiagnostic * SES, ArticleExample2)
	expect_equal( dim(res$wideData), c(445,3))
    expect_equal( res$omnibus[1,1], 0.001742, tolerance = 0.0001)
    expect_equal( res$omnibus[2,1], 0.022242, tolerance = 0.0001)

    # third example of the paper 
    res <- anopa( cbind(cBau, eaPoe, RnV, Placebo) ~ ., ArticleExample3, WSFactors = "Drug(4)" )
	expect_equal( dim(res$wideData), c(30,4))
    expect_equal( res$omnibus[1,1], 0.017791, tolerance = 0.0001)
    expect_equal( res$omnibus[2,1], 0.005834, tolerance = 0.0001)

})


test_that("Testing simulated data sets (3/3)", {

	old <- options() 
	on.exit(options(old)) 
	options(ANOPA.feedback = 'none')

	# one example from a 2 x (3) design
 	set.seed(41)
	frm      <- cbind(s.early, s.middle, s.late) ~ grp
	BSDesign <- list(grp = c("ctrl","plcbo"))
	WSDesign <- list(moment = c("early","middle","late"))
	thePs    <- c(0.3, 0.3, 0.3, 0.3, 0.7, 0.7)
	theN     <- 20

	smp <- GRP( thePs, theN, BSDesign, WSDesign )
		expect_equal( dim(smp), c(2*theN, 5)) #2*3*10 lines, 5 columns (Id, bs-factors, two measures)
	w   <- anopa(frm, smp[,2:5] , WSFactors = "hour(3)")
	summarize(w)
		expect_equal( summarize(w)[1,1], 0.152799, tolerance = 0.0001)

	# test type-I error rate when no effect
 	set.seed(41)
	dec <- c()
	for (i in 1:100) {
		smp <- GRP( thePs, theN, BSDesign, WSDesign )
		w   <- anopa(frm, smp[,2:5] , WSFactors = "hour(3)")
		dec <- c(dec, if(summarize(w)[2,4]<.05) 1 else 0)
	}
	typeI <- mean(dec)
	expect_equal( typeI, .05, tolerance = 0.035)

	# effect size for second factor
	f2 <- anopaProp2fsq( c(0.3,0.3,0.7), c(25,25,25) ); 
	desiredN <- anopaPower2N(.80, 3, f2)

	# test power rate when moderate main effect
 	set.seed(41)
	dec<- c()
	for (i in 1:100) {
		smp <- GRP( thePs, round(desiredN/(2*3)), BSDesign, WSDesign )
		w <- anopa(frm, smp[,2:5] , WSFactors = "hour(3)")
		dec <- c(dec, if(summarize(w)[1,4]<.05) 1 else 0)
	}
	pwr <- mean(dec)
	expect_equal( pwr, .80, tolerance = 0.05)


})


test_that("Testing two formats of the same data (3/3)", {

	old <- options() 
	on.exit(options(old)) 
	options(ANOPA.feedback = 'none')

    df <- GRP( props = c(.5, .5, .5, .5, .5, .7, .7, .2), n = c(81,21,16,30), 
        BSDesign = list( group = c("control","treatment1","treatment2","treament3") ),
        WSDesign = list( foraging = c("before", "after") ),
        sname = "frg"
    )

    ## OLD: anopa on the wide format
    w <- anopa( cbind( frg.before, frg.after) ~ group, df, WSFactors= "frg(2)")
    dfC <- toCompiled(w)
    print(pltw <- anopaPlot(w))
    expect_equal( "ggplot" %in% class(pltw), TRUE)

    ## NEW: compiled format with repeated measures
    v <- anopa( {cbind(frg.before,frg.after); Count; uAlpha} ~ group, dfC, WSFactors = "frg(2)")
    print(pltv <- anopaPlot(v))
    expect_equal( "ggplot" %in% class(pltv), TRUE)

    ## both returns the same response
    expect_equal( max(abs(summary(w) - summary(v)), na.rm = TRUE), 0, tol = 0.000001)

})