context("ANOPA:: Testing emProportions function")
    # expect_output( str(res), "data.frame")
    # expect_equal( "ggplot" %in% class(plt), TRUE)
    # expect_message( p <- superbPlot(dta2a, etc ))
    # expect_error(), expect_warning(), expect_condition( , class = "")


test_that("TESTS of emProportions function (1/3)", {

	old <- options() 
	on.exit(options(old)) 
	options(ANOPA.feedback = 'none')

    ###########################################################
    ## Testing Example 2 of the article
    ###########################################################

    w2 <-anopa( {s;n} ~ MofDiagnostic * SES, ArticleExample2)
    plt <- anopaPlot(w2, ~ MofDiagnostic * SES)
    expect_output( str(plt), "list")
    expect_equal( "ggplot" %in% class(plt), TRUE)

    e2a <- emProportions(w2, ~ SES | MofDiagnostic)
    expect_equal( e2a$simpleeffects[1,1], 0.0022048, tolerance = 0.0001)
    expect_equal( e2a$simpleeffects[2,4], 0.0003704, tolerance = 0.0001)

    e2b <- emProportions(w2, ~ MofDiagnostic | SES )
    expect_equal( e2b$simpleeffects[1,1], 0.0056748, tolerance = 0.0001)
    expect_equal( e2b$simpleeffects[3,4], 0.1795718, tolerance = 0.0001)


})

 test_that("TESTS of emProportions function (2/3)", {

	old <- options() 
	on.exit(options(old)) 
	options(ANOPA.feedback = 'none')

    ###########################################################
    ## Testing the dataset's example twoWayExample (between)
    ###########################################################

    w1  <- anopa( {success;total} ~ Class * Difficulty, twoWayExample)
    plt <- anopaPlot(w1, ~ Difficulty * Class)
    expect_output( str(plt), "list")
    expect_equal( "ggplot" %in% class(plt), TRUE)
    
    e1a <- emProportions(w1, ~ Difficulty | Class)
    expect_equal( e1a$simpleeffects[1,1], 0.0509317, tolerance = 0.0001)
    expect_equal( e1a$simpleeffects[2,4], 0.0113817, tolerance = 0.0001)

    e1b <- emProportions(w1, ~ Class | Difficulty)
    e1b$simpleeffects
    expect_equal( e1b$simpleeffects[1,1], 0.0300072, tolerance = 0.0001)
    expect_equal( e1b$simpleeffects[3,4], 0.6693006, tolerance = 0.0001)

    ###########################################################
    ## Testing the dataset's example minimalMxExample (mixed)
    ###########################################################

    w2  <- anopa( crange(bpre, b5week) ~ Status, minimalMxExample, WSFactors = "Moment(4)")
    plt <- anopaPlot(w2, ~ Moment * Status)
    expect_output( str(plt), "list")
    expect_equal( "ggplot" %in% class(plt), TRUE)

    e2a <- emProportions(w2, ~ Moment | Status)
    # running the subanalyses separately...
    anopa( crange(bpre,b5week) ~ ., WSFactors = "Moment(4)", minimalMxExample[minimalMxExample$Status == "Broken",] )$omnibus
    expect_equal( e2a$simpleeffects[1,1], 0.0037282, tolerance = 0.0001)
    expect_equal( e2a$simpleeffects[3,4], 0.9326541, tolerance = 0.0001)

    e2b <- emProportions(w2, ~ Status | Moment)
    # running the subanalyses separately...
    anopa( b1week ~ Status, minimalMxExample[,c("Status","b1week")])$omnibus
    expect_equal( e2b$simpleeffects[1,1], 0.0437595, tolerance = 0.0001)
    expect_equal( e2b$simpleeffects[4,4], 0.1356146, tolerance = 0.0001)

})

test_that("TESTs of emProportions function (3/3)", {

	old <- options() 
	on.exit(options(old)) 
	options(ANOPA.feedback = 'none')

    w4 <- anopa( {s; n} ~  Trophism * Location * Diel, ArringtonEtAl2002)
      
    e4c <- emProportions(w4, ~ Location * Trophism | Diel )
    e4d <- emProportions(w4, ~ Trophism * Location | Diel )
    #summarize(e4d)

    expect_equal( e4c$simpleeffects[4,3], 0.536291, tolerance = 0.0001)
    expect_equal( e4d$simpleeffects[4,3], 0.536291, tolerance = 0.0001)
    expect_equal( e4c$simpleeffects[4,3], e4d$simpleeffects[4,3], tolerance = 0.0001)

    # running separate analyses...
    #anopa( {s;n} ~ Location * Trophism, ArringtonEtAl2002[ArringtonEtAl2002$Diel == "Diurnal",] )$omnibus
    #anopa( {s;n} ~ Location * Trophism, ArringtonEtAl2002[ArringtonEtAl2002$Diel == "Nocturnal",] )$omnibus


    ####################################
    # second variation
    ####################################

    e4a <- emProportions(w4, ~ Location * Diel | Trophism)
    e4b <- emProportions(w4, ~ Diel * Location | Trophism) # is identical?
    expect_equal( e4a$simpleeffects[1,1], 0.029129, tolerance = 0.0001)
    expect_equal( e4b$simpleeffects[4,4], 0.989895, tolerance = 0.0001)
    expect_equal( e4a$simpleeffects[4,3], e4b$simpleeffects[4,3], tolerance = 0.0001)

    # running separate analyses...
    #anopa( {s;n} ~ Location * Diel, ArringtonEtAl2002[ArringtonEtAl2002$Trophism == "Detritivore",] )$omnibus
    #anopa( {s;n} ~ Location * Diel, ArringtonEtAl2002[ArringtonEtAl2002$Trophism == "Invertivore",] )$omnibus
    #anopa( {s;n} ~ Location * Diel, ArringtonEtAl2002[ArringtonEtAl2002$Trophism == "Omnivore",] )$omnibus
    #anopa( {s;n} ~ Location * Diel, ArringtonEtAl2002[ArringtonEtAl2002$Trophism == "Piscivore",] )$omnibus


    ####################################
    # Last variation
    ####################################

    e4e <- emProportions(w4, ~ Trophism * Diel | Location )
    e4f <- emProportions(w4, ~ Diel * Trophism | Location )
    expect_equal( e4e$simpleeffects[1,1], 0.108945, tolerance = 0.0001)
    expect_equal( e4f$simpleeffects[4,3], 1.069015, tolerance = 0.0001)
    expect_equal( e4e$simpleeffects[4,3], e4f$simpleeffects[4,3], tolerance = 0.0001)

    # running separate analyses...
    #anopa( {s;n} ~ Trophism * Diel, ArringtonEtAl2002[ArringtonEtAl2002$Location == "Africa",] )$omnibus
    #anopa( {s;n} ~ Trophism * Diel, ArringtonEtAl2002[ArringtonEtAl2002$Location == "Central/South America",] )$omnibus
    #anopa( {s;n} ~ Trophism * Diel, ArringtonEtAl2002[ArringtonEtAl2002$Location == "North America",] )$omnibus


})




