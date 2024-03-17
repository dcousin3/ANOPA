######################################################################################
#' @title anopaPlot: Easy plotting of proportions.
#'
#' @aliases anopaPlot
#'
#' @md
#'
#' @description The function `anopaPlot()` performs a plot of proportions for designs 
#'      with up to 4 factors according to the
#'      `ANOPA` framework. See \insertCite{lc23;textual}{ANOPA} for more. The plot is 
#'      realized using the `suberb` library; see \insertCite{cgh21;textual}{ANOPA}.
#'      It uses the arc-sine transformation `A()`.
#'
#'   
#' @usage anopaPlot(w, formula, confidenceLevel = .95, allowImputing = FALSE,
#' 		showPlotOnly = TRUE, plotStyle = "line", 
#'      errorbarParams  = list( width =0.5, linewidth=0.75 ), ...) 
#'
#'
#' @param w An ANOPA object obtained with `anopa()`;
#'   
#'
#' @param formula (optional) Use formula to plot just specific terms of the omnibus test.
#'       For example, if your analysis stored in `w` has factors A, B and C, then
#'       `anopaPlot(w, ~ A * B)` will only plot the factors A and B.
#' @param confidenceLevel Provide the confidence level for the confidence intervals
#'       (default is 0.95, i.e., 95%).
#' @param allowImputing (default FALSE) if there are cells with no observations, can they be
#'      imputed? If imputed, the option "ANOPA.zeros" will be used to determine
#'		how many additional observations to add, and with how many successes.
#'		If for example, the option is (by default) `c(0.05, 1)`, then
#'		20 cases will be added, only one being a success (respecting the .05 target).
#'      _Keep in mind that imputations has never been studies with regards to proportions
#'      so be mindfull that the default optin has never been tested nor validated._
#' @param showPlotOnly (optional, default True) shows only the plot or else 
#'       shows the numbers needed to make the plot yourself.
#' @param plotStyle (optional; default "line") How to plot the proportions;
#'       see superb for other layouts (e.g., "line").
#' @param errorbarParams (optional; default list( width =0.5, linewidth=0.75 ) ) is
#'      a list of attributes used to plot the error bars. See superb for more.
#' @param ... Other directives sent to superb(), typically 'plotStyle', 
#'			'errorbarParams', etc.
#'   
#'
#' @return a ggplot2 object of the given proportions. 
#'   
#'
#' @details The plot shows the proportions on the vertical axis as a 
#'   function of the factors (the first on the horizontal axis, the second 
#'   if any in a legend; and if a third or even a fourth factors are present,
#'   as distinct rows and columns). It also shows 95% confidence intervals of 
#'   the proportions, adjusted for between-cells comparisons. 
#'   The confidence intervals are based on a z distribution, which is adequate
#'   for large samples \insertCite{c90,ll90}{ANOPA}. This "stand-alone" confidence
#'   interval is then adjusted for between-cell comparisons using the _superb_
#'   framework \insertCite{cgh21}{ANOPA}.
#' 
#' See the vignette [`DataFormatsForProportions`](../articles/B-DataFormatsForProportions.html) 
#'    for more on data formats and how to write their formula. 
#'    See the vignette [`ConfidenceIntervals`](../articles/C-ConfidenceIntervals.html) for 
#'    details on the adjustment and its purpose.
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' # 
#' # The Arrington Et Al., 2002, data on fishes' stomach
#' ArringtonEtAl2002
#'
#' # This examine the omnibus analysis, that is, a 3 x 2 x 4 ANOPA:
#' w <- anopa( {s;n} ~ Location * Trophism * Diel, ArringtonEtAl2002) 
#' 
#' # Once processed into w, we can ask for a standard plot
#' anopaPlot(w)
#' 
#' # As you may notice, there are points missing because the data have
#' # three missing cells. The litterature is not clear what should be 
#' # done with missing cells. In this package, we propose to impute
#' # the missing cells based on the option `getOption("ANOPA.zeros")`.
#' # Consider this option with care.  
#' anopaPlot(w, allowImputing = TRUE)
#' 
#' # We can place the factor `Diel` on the x-axis (first):
#' #anopaPlot(w, ~ Diel * Trophism * Location )
#' 
#' # Change the style for a plot with bars instead of lines
#' #anopaPlot(w, plotStyle = "bar")   # to speed tests, we comment these two lines
#' 
#' # Changing the error bar style
#' #anopaPlot(w, plotStyle = "bar", errorbarParams = list( width =0.1, linewidth=0.1 ) )
#' 
#' # Illustrating the main effect of Location (not interacting with other factors)
#' # and the interaction Diel * Trophism separately
#' #anopaPlot(w, ~ Location )        # to speed tests, we comment these two lines
#' #anopaPlot(w, ~ Diel * Trophism ) # to speed tests, we comment these two lines
#' 
#' # All these plots are ggplot2 so they can be followed with additional directives, e.g.
#' library(ggplot2)
#' anopaPlot(w, ~ Location) + ylim(0.0, 1.0) + theme_classic()
#' anopaPlot(w, ~ Diel * Trophism) + ylim(0.0, 1.0) + theme_classic()
#' 
#' # etc. Any ggplot2 directive can be added to customize the plot to your liking.
#' # See the vignette `ArringtonExample`.
#'
######################################################################################
#'
#' @importFrom Rdpack reprompt
#' @importFrom stats aggregate
#' @export anopaPlot
#
######################################################################################



################################################################
# create a custom scale based on the arcsin transform          #
################################################################
# This scale is adapted to handle cases where numbers outside of 
# the legitimate scale would be submitted. This happens with 
# difference-adjusted CI whose length are increased by sqrt(2). 
# This is tricky because ggplots send extreme cases to the 
# function to know how to draw it, including NAs.
anopa_asn_trans1 <- function(x) {
	w <- suppressWarnings(asin(sqrt(x)))
	if (!is.na(x)&&(x<0)) w = anopa_asn_trans1(0) ## boundary truncation
	if (!is.na(x)&&(x>1)) w = anopa_asn_trans1(1) ## boundary truncation
	return(w)
}
anopa_asn_trans2 <- function(x) {
	w <- suppressWarnings(sin(x)^2)
	return(w)
}
anopa_asn_trans <- function () {
    scales::trans_new("asn2", 
		function(x) {
			#cat("1:",x," = ", sapply(x, anopa_asn_trans1),"\n"); 
			sapply(x, anopa_asn_trans1)
		}, 
		function(x) {
			#cat("2:",x," = ", sapply(x, anopa_asn_trans2),"\n"); 
			sapply(x, anopa_asn_trans2)
		},
		domain = c(0, 1)
	)
}


################################################################
# This is it! let's make the plot                              #
################################################################

# make the plot: just a proxy for suberbPlot
anopaPlot <- function(w, 
                formula         = NULL,
                confidenceLevel = 0.95,
				allowImputing   = FALSE,
                showPlotOnly    = TRUE,
                plotStyle       = "line",                             # lines by default
                errorbarParams  = list( width =0.5, linewidth=0.75 ), # thicker error bars
                ...  # will be transmitted to superb as is
){

    ##############################################################################
    ## STEP 1: validating the formula if one is given
    ##############################################################################
    if (!is.null(formula)) {
        # 1.1. is it a legitimate formula?
        if (!(is.formula(formula))) 
            stop("ANOPA::error(211): The formula argument is not a legitimate formula. Exiting...")

		# 1.2. only lhs part
		if ( !(is.one.sided(formula))) 
			stop("ANOPA::error(212): Only one-sided, rhs, formulas can be given. Exiting...")

		# 1.3. No cbind, no nested
		if ((has.nested.terms(formula))||has.cbind.terms(formula)) 
		    stop("ANOPA::error(213): No cbind nor nested variables allowed. Exiting...")

		# 1.4. Are they existing factors?
		if (!all(all.vars(formula) %in% c(w$BSfactColumns,w$WSfactColumns))) 
			stop("ANOPA::error(214): The named factor(s) given in the formula are not in the model. Exiting...")

        # 1.6. if everything ok, let's extract the variables...
        facts  <- all.vars(formula)[ !(all.vars(formula) %in% w$DVvariables)]
		bsfact <- all.vars(formula)[ all.vars(formula) %in% w$BSfactColumns]
		wsfact <- all.vars(formula)[ all.vars(formula) %in% w$WSfactColumns]

		# 1.7. ... and aggregate the data accordingly
		ldata <- wtol( w$wideData, w$DVvariables )
		Ainv  <- function(a, n) (n+3/4)*(sin(a))^2 - 3/8
		Amean <- function(v) Ainv( Atrans(v), 1)
		if(length(wsfact)>0){
			toto        <- "Variable"
			tempvars    <- as.character(w$DVvariables)
		} else {
			toto        <- "dummy"
			ldata$dummy <- "s"	
			tempvars    <- "s"
		}
        # merge the within-subject variables to the long data frame
        if ( length(wsfact) != 0 ) {
            ldata <- merge(ldata, w$WSfactDesign, by = "Variable", all.x = TRUE)
        }

        adata <- stats::aggregate(as.formula(
						paste("Value", paste(
								c("Id", toto, facts),
							collapse="+"), sep="~")
					), 
                    data = ldata, Amean)
        #remove the within-subject variables
        adata[,wsfact] <- NULL

		wdata <- ltow( adata, "Id", toto, "Value" )			

    } else {
		facts    <- c(w$BSfactColumns, w$WSfactColumns)
        bsfact   <- w$BSfactColumns
        wsfact   <- w$WSfactColumns
        wdata    <- w$wideData
        tempvars <- w$DVvariables
    }

	# 1.8. If incorrect confidence level...
    if ((confidenceLevel >= 1)||(confidenceLevel <=0.0))
        stop("ANOPA::error(214): The confidence level is not within 0 and 1 (i.e., within 0% and 100%). Exiting...")

	# 1.9. Empty is not NULL
	if (length(bsfact)==0) bsfact = NULL
	if (length(wsfact)==0) {
		wsfact = NULL
	} else {
		wsfact = paste(w$WSfactColumn[w$WSfactColumn %in% wsfact], "(",
					   w$WSfactNlevels[w$WSfactColumn %in% wsfact], ")", sep="")
	}


    ##############################################################################
    ## Step 2: Verify missing data?
    ##############################################################################
    # partial observations are not preserved above if the zeros are smaller than 1.
	if (allowImputing) {
		missing <- addmissingcellsBSFactors(wdata, bsfact, tempvars)
		wdata <- rbind(wdata, missing)
	}


    ##############################################################################
    ## All done! ask for the plot or the data
    ##############################################################################
    # quiet superb's information
    opt <- getOption("superb.feedback")
    on.exit(options(opt))
    options(superb.feedback = 'none')

    if (showPlotOnly) { # generate the plot
        res <- superb::superbPlot( wdata,
            BSFactors      = bsfact,
            WSFactors      = wsfact,
			factorOrder    = facts,
            variables      = tempvars,
            statistic      = "prop",        # the summary statistics defined above
            errorbar       = "CI",          # its precision define above
            gamma          = confidenceLevel,
			adjustment     = list( 
				purpose       = "difference",     # difference-adjusted CI
				decorrelation = if(length(tempvars)>1) "UA" else "none"
			),
            # the following is for the look of the plot
            plotStyle      = plotStyle,
            errorbarParams = errorbarParams,   
            ... ## passed as is to superb
        ) + ggplot2::ylab("Proportion") + 
		# use if you want  the stretching visible
		ggplot2::scale_y_continuous(trans=anopa_asn_trans(), breaks = scales::breaks_pretty(n = 10)) 

    } else { # only compute the summary statistics
        res <- superb::superbData( wdata,
            BSFactors      = bsfact,
            WSFactors      = wsfact,
            variables      = tempvars,
            statistic      = "prop",        # the summary statistics defined above
            errorbar       = "CI",          # its precision define above
            gamma          = confidenceLevel,
			adjustment     = list( 
				purpose       = "difference",     # difference-adjusted CI
				decorrelation = if(length(tempvars)>1) "UA" else "none"
			)
        )#$summaryStatistics
    }
    # restore superb's information: done with on.exit()
    # options(superb.feedback = opt)

    return(res)
}


addmissingcellsBSFactors <- function( wData, BSFactors, ss) {
	# List the possible cells to see if some are missing...
	# The missing cells are added explicitely to the data
	# There ought to be better code... Sorry about this.

	toAdd <- wData[1,][-1,] # an empty line
	repl <- unlist(options("ANOPA.zeros"))
	repl <- repl/repl[1]

	if (length(BSFactors) == 2) {
		for (i in unique(wData[[BSFactors[[1]]]]) )
			for (j in unique(wData[[BSFactors[[2]]]]) )
				if (!any((wData[[BSFactors[[1]]]] == i)&(wData[[BSFactors[[2]]]] == j))){
					ANOPAwarning(paste("ANOPA::warning(201): Cells", i, j, "missing in data. Imputing..."))
					vide <- wData[1,][-1,]
					line <- wData[1,] # an empty line
					for (l in 1:repl[2]) {
						line[[BSFactors[[1]]]] <- i
						line[[BSFactors[2]]]   <- j
						for ( m in ss ) {line[[m]]<- if(l <= repl[1]) 1 else 0}
						vide <- rbind( vide, line )
					}
					toAdd <- rbind(toAdd, vide)
				}
	} else if (length(BSFactors) == 3) {
		for (i in unique(wData[[BSFactors[[1]]]]) )
			for (j in unique(wData[[BSFactors[[2]]]]) )
				for (k in unique(wData[[BSFactors[[3]]]]))
					if (!any((wData[[BSFactors[[1]]]] == i)&(wData[[BSFactors[[2]]]] == j)&(wData[[BSFactors[[3]]]] == k))){
						ANOPAwarning(paste("ANOPA::warning(201): Cell",i, j, k, "missing in data. Imputing..."))
						vide <- wData[1,][-1,]
						line <- wData[1,] # an empty line
						for (l in 1:repl[2]) {
								line[[BSFactors[[1]]]] <- i
								line[[BSFactors[2]]]   <- j
								line[[BSFactors[3]]]   <- k
								for ( m in ss ) {line[[m]] <- if(l <= repl[1]) 1 else 0}
							vide <- rbind( vide, line )
						}
						toAdd <- rbind(toAdd, vide)
					}
	}
	return( toAdd )
}


