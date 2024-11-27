context("ANOPA:: Testing anopaPlot() function")
    # expect_output( str(res), "data.frame")
    # expect_equal( "ggplot" %in% class(plt), TRUE)
    # expect_message( p <- superbPlot(dta2a, etc ))
    # expect_error(), expect_warning(), expect_condition( , class = "")


test_that("TESTS of anopaPlot (1/2)", {

	old <- options() 
	on.exit(options(old)) 
	options(ANOPA.feedback = 'none')

	# shortcuts... 
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


	b <- anopa( {s;n} ~ state, c1B)
	plt <- anopaPlot(b)
    expect_equal( "ggplot" %in% class(plt), TRUE)
	plt <- anopaPlot(b, ~ state)
    expect_equal( "ggplot" %in% class(plt), TRUE)
	expect_error( plt <- anopaPlot(b, 3 +3)	)				# error 211
	expect_error(plt <- anopaPlot(b, s ~ state)	)			# error 212
	expect_error(plt <- anopaPlot(b, ~ state * moment | Id))# error 213
	expect_error(plt <- anopaPlot(b, ~ moment)  )			# error 214
	expect_error( plt <- anopaPlot(b, ~ state * moment) ) # error 214


	w <- anopa( cbind(bpre,b1week,b5week) ~ ., w1W, WSFactors="Moment(3)")
	plt <- anopaPlot(w)
    expect_equal( "ggplot" %in% class(plt), TRUE)
	plt <- anopaPlot(w, ~ Moment)
    expect_equal( "ggplot" %in% class(plt), TRUE)
	expect_error(plt <- anopaPlot(w, 3 +3))					# error 211
	expect_error(plt <- anopaPlot(w, s ~ Moment))			# error 212
	expect_error(plt <- anopaPlot(w, ~ state * Moment | Id))# error 213
	expect_error(plt <- anopaPlot(w, ~ state) ) 			# error 214
	expect_error(plt <- anopaPlot(w, ~ state * Moment))		# error 214


	m <- anopa( cbind(bpre,b1week,b5week) ~ Status, wWB, WSFactors="Moment(3)")
	plt1 <- anopaPlot(m) 						# Ok
    expect_equal( "ggplot" %in% class(plt1), TRUE)
	plt2 <- anopaPlot(m, ~ Moment)				# Ok
    expect_equal( "ggplot" %in% class(plt2), TRUE)
	plt3 <- anopaPlot(m, ~ Status)				# Ok
    expect_equal( "ggplot" %in% class(plt3), TRUE)
	plt4 <- anopaPlot(m, ~ Moment * Status) 	# Ok
    expect_equal( "ggplot" %in% class(plt4), TRUE)
	plt5 <- anopaPlot(m, ~ Status * Moment) 	# Ok
    expect_equal( "ggplot" %in% class(plt5), TRUE)
	expect_error( anopaPlot(m, 3 +3) )					# error 211
	expect_error( anopaPlot(m, s ~ Status) )			# error 212
	expect_error( anopaPlot(m, ~ Moment * Status | Id)) # error 213
	expect_error( anopaPlot(m, ~ state) ) 		    	# error 214
	expect_error( anopaPlot(m, ~ mm * Status) ) 		# error 214
	expect_error( anopaPlot(m, ~ Moment * sst) ) 		# error 214


	# with options
    pltA <- anopaPlot(m, errorbarParams = list( width =0.1, linewidth=2.0 )) 
    expect_equal( "ggplot" %in% class(pltA), TRUE)

    pltB <- anopaPlot(w, plotStyle = "bar") 
    expect_equal( "ggplot" %in% class(pltB), TRUE)

    pltC <- anopaPlot(w, plotStyle = "bar", barParams = list(width = .2)) 
    expect_equal( "ggplot" %in% class(pltC), TRUE)

    pltD <- anopaPlot(w, plotStyle = "bar", 
                        barParams = list(width = .2), 
                        errorbarParams = list(width =0.1, linewidth = 2.75) ) 
    expect_equal( "ggplot" %in% class(pltD), TRUE)


})


test_that("TESTS of anopaPlot (2/2)", {

	old <- options() 
	on.exit(options(old)) 
	options(ANOPA.feedback = 'none')

	w <- anopa( {s;n} ~ Location * Trophism * Diel, ArringtonEtAl2002) 

	plt <- anopaPlot(w)
    expect_equal( "ggplot" %in% class(plt), TRUE)
	#plt <- anopaPlot(w, ~ Diel * Trophism * Location ) # slow
    #expect_equal( "ggplot" %in% class(plt), TRUE)

	plt <- anopaPlot(w, plotStyle = "bar")
    expect_equal( "ggplot" %in% class(plt), TRUE)

	plt <- anopaPlot(w, plotStyle = "bar", 
            errorbarParams = list( width =0.9, linewidth=0.1, color="black"  ) 
        )
    expect_equal( "ggplot" %in% class(plt), TRUE)

	#plt1 <- anopaPlot(w, ~ Trophism) # slow
    #expect_equal( "ggplot" %in% class(plt1), TRUE)
	#plt2 <- anopaPlot(w, ~ Diel * Location) # slow
    #expect_equal( "ggplot" %in% class(plt2), TRUE)

	# forces a linear scale
    library(ggplot2)
	expect_message(plt3 <- anopaPlot(w) + ylim(0.0,1.0) + theme_classic() )
    expect_equal( "ggplot" %in% class(plt3), TRUE)
	expect_message(plt4 <- anopaPlot(w, ~ Diel * Location) + ylim(0.0,1.0) + theme_classic() )
    expect_equal( "ggplot" %in% class(plt4), TRUE)

})

