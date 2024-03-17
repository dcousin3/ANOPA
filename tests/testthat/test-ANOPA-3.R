context("ANOPA:: Testing power and GRP functions")
    # expect_output( str(res), "data.frame")
    # expect_equal( "ggplot" %in% class(plt), TRUE)
    # expect_message( p <- superbPlot(dta2a, etc ))
    # expect_error(), expect_warning(), expect_condition( , class = "")


test_that("TESTS of anofaN2Power and anofaPower2N (1/3)", {

	# example from article
	f2 <- anopaProp2fsq( c( 0.32, 0.64, 0.40, 0.16), c(25,25,25,25) ); 
		expect_equal( f2,  0.1280757, tolerance = 0.0001)
	f2X <- anopaProp2fsq( c( 0.32, 0.64, 0.40, 0.16), c(25,25,25,25), method="exact"  );
		expect_equal( f2X,  0.1306372 , tolerance = 0.0001)
	f2Y <- anopaProp2fsq( c( 0.32, 0.64, 0.40, 0.16), c(25,25,25,25), method="approximation" )
		expect_equal( f2Y,  0.1280757 , tolerance = 0.0001)

	pwr <- anopaPower2N(.8, 4, f2)
		expect_equal( pwr, 85.12596 , tolerance = 0.0001)
	pwrX <- anopaPower2N(.8, 4, f2X)
		expect_equal( pwrX, 83.45682 , tolerance = 0.0001)
	theN <- anopaN2Power(97, 4, f2)
		expect_equal( theN, 0.8538928 , tolerance = 0.0001)

    
})

test_that("TESTS of GRP() (2/3)", {

	# test of rBinomial
	set.seed(43)
	t <- rBernoulli(10000, 0.1)
	expect_equal( mean(t), 0.10, tolerance = 0.01)

	# test of gSwitch
	res <- gSwitch( 12, 10~"a", 11~"b", 12~"c")
	expect_equal( res, "c")
	res <- gSwitch( c(1,2), c(1,1)~ 11, c(1,2) ~ 12, c(1,3) ~ 13)
	expect_equal( res, 12)

	# data frame of succes/failure in a 3 x 2 x ( 2 ) design
	# with 6 groups and 2 measures.
	set.seed(43)
	BSDesign <- list(age = c(1,2,3), grp = c("ctrl","plcbo") )
	WSDesign <- list(moment = c("early","late"))
	thePs <- c(0.3, 0.3, 0.3, 0.5, 0.7, 0.9, 0.3, 0.3, 0.3, 0.5, 0.7, 0.9)
	res <- GRP( thePs, 10, BSDesign, WSDesign)
	expect_equal( dim(res), c(60, 5)) #2*3*10 lines, 5 columns (Id, bs-factors, two measures)

	# if we remove the Id column, we can convert to compiled format
    com <- ANOPA:::wtoc(res[,2:5], c("s.early","s.late"), "n" )
	expect_equal( dim(com), c(6,6) ) # 6 cells
	
	
})


test_that("TESTS of statistical power and type-I error rates (3/3)", {

	old <- options() 
	on.exit(options(old)) 
	options("ANOPA.feedback" = 'none')

    # the example from the article
	set.seed(43)
	frm <- s ~ grp
	BSDesign <- list(grp = c("1","2","3","4"))
	thePs <- c(0.32, 0.64, 0.40, 0.16)
	theN  <- 20
	smp <- GRP( thePs, theN, BSDesign )
		expect_equal( dim(smp), c(4*theN, 3)) 

	pwr  <- anopaPower2N(.8, 4, 0.1281)
	theN <-round(pwr/4)

	# test type-I error rate when no effect
	dec<- c()
	for (i in 1:100) { # a real simulation should have a lot more than 100 simulated samples...
		smp <- GRP( rep(mean(thePs),4), theN, BSDesign )
		w <- anopa(frm, smp[,2:3] )
		dec <- c(dec, if(summarize(w)[1,4]<.05) 1 else 0)
	}
	expect_equal(mean(dec), .05, tolerance = .025)
	# 0.05 +- .04

	# test power rate when moderate main effect
	dec<- c()
	for (i in 1:100) { # a real simulation should have a lot more than 100 simulated samples...
		smp <- GRP( thePs, theN, BSDesign )
		w <- anopa(frm, smp[,2:3] )
		dec <- c(dec, if(summarize(w)[1,4]<.05) 1 else 0)
	}
	expect_equal( mean(dec), .80, tolerance = .05)
	# 0.80 +- .05	

})

