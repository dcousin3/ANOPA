####################################################################################
#' @title ANOPA: analysis of proportions using Anscombe transform.
#'
#' @md
#'
#' @description The function 'anopa()' performs an ANOPA for designs with up to 4 factors
#'      according to the 'ANOPA' framework. See \insertCite{lc23;textual}{ANOPA} for more.
#' 
#'
#' @param formula A formula with the factors on the left-hand side. See below for writing the 
#' formula to match the data format.
#'
#' @param data Dataframe in one of wide, long, or compiled format;
#'
#' @param WSFactors For within-subject designs, provide the factor names and their number of levels.
#'   This is expressed as a vector of strings such as "Moment(2)".
#'
#' @return An omnibus analyses of the given proportions. Each factor's significance is 
#'    assessed, as well as their interactions when there is more than one factor. 
#'    The results are obtained with `summary()` or `summarize()` as usual. If desired,
#'    the corrected-only statistics can be presented \insertCite{w76}{ANOPA} using
#'    `corrected()`; the uncorrected statistics only are obtained with `uncorrected()`.
#'    For decomposition of the main analyses, follow the main analysis with `emProportions()`, 
#'    `contrastProportions()`, or `posthocProportions()`)
#'
#' @details Note the following limitations:
#' 1. The main analysis performed by `anopa()` is currently restricted to three 
#'   factors in total (between and/or within). Contact the author if you plan to analyze
#'   more complex designs.
#' 2. If you have repeated-measure design, the data *must* be provided in wide or
#'   long format. The correlation between successes cannot be assessed once the data are 
#'   in a compiled format.
#' 3. The data can be given in three formats:
#'     * `wide`: In the wide format, there is one line for each participant, and
#'      one column for each between-subject factors in the design. In the column(s), the level
#'      of the factor is given (as a number, a string, or a factor). For within-subject
#'      factors, the columns contains 0 or 1 based on the status of the measurement.
#'     * `long`: In the long format, there is an identifier column for each participant, 
#'      a factor column and a level number for that factor. If there are n participants
#'      and m factors, there will be in total n x m lines.
#'     * `compiled`: In the compiled format, there are as many lines as there are cells in the
#'      design. If there are two factors, with two levels each, there will be 4 lines.
#'
#' See the vignette [`DataFormatsForProportions`](../articles/B-DataFormatsForProportions.html) 
#'   for more on data format and how to write their formula.
#'
#'
#' @references
#' \insertAllCited
#'
#' @examples
#' # -- FIRST EXAMPLE --
#' # Basic example using a single between-subject factor design with the data in compiled format. 
#' # Ficticious data present success (1) or failure (0) of the observation according
#' # to the state of residency (three levels: Florida, Kentucky or Montana) for 
#' # 3 possible cells. There are 175 observations (with unequal n, Montana having only)
#' # 45 observations). 
#' minimalBSExample
#' # The data are in compiled format, consequently the data frame has only three lines.
#' # The complete data frame in wide format would be composed of 175 lines, one per participant.
#'
#' # The following formula using curly braces is describing this data format
#' # (note the semicolon to separate the number of successes from the number of observations):
#' formula <- {s; n} ~ state
#'
#' # The analysis is performed using the function `anopa()` with a formula and data:
#' w <- anopa(formula, minimalBSExample) 
#' summary(w)
#' # As seen, the proportions of success do not differ across states.
#'
#' # To see the proportions when the data is in compiled format, simply divide the 
#' # number of success (s) by the total number of observations (n):
#' minimalBSExample$s / minimalBSExample$n
#'
#' # A plot of the proportions with error bars (default 95% confidence intervals) is
#' # easily obtained with
#' anopaPlot(w)
#'
#' # The data can be re-formated into different formats with, 
#' # e.g., `toRaw()`, `toLong()`, `toWide()`
#' head(toWide(w))
#' # In this format, only 1s and 0s are shown, one participant per line.
#' # See the vignette `DataFormatsForFrequencies` for more.
#'
#' # -- SECOND EXAMPLE --
#' # Real-data example using a three-factor design with the data in compiled format:
#' ArringtonEtAl2002
#'
#' #  This dataset, shown in compiled format, has three cells missing 
#' # (e.g., fishes whose location is African, are Detrivore, feeding Nocturnally)
#' w <- anopa( {s;n} ~ Location * Trophism * Diel, ArringtonEtAl2002 )
#' 
#' # The function `anopa()` generates the missing cells with 0 success over 0 observations.
#' # Afterwards, cells with missing values are imputed  based on the option:
#' getOption("ANOPA.zeros")
#' # where 0.05 is 1/20 of a success over one observations (arcsine transforms allows 
#' # fractions of success; it remains to be studied what imputation strategy is best...)
#'
#' # The analysis suggests a main effect of Trophism (type of food ingested)
#' # but the interaction Trophism by Diel (moment of feeding) is not to be neglected...
#' summary(w) # or summarize(w)
#'
#' # The above presents both the uncorrected statistics as well as the corrected
#' # ones for small samples (Williams, 1976). You can obtain only the uncorrected...
#' uncorrected(w)
#'
#' #... or the corrected ones
#' corrected(w)
#'
#' # Finally, the data may have repeated measures and still be accessible in a compiled 
#' # format, as is the case of this short example:
#' minimalMxExampleCompiled
#' 
#' # As seen, it has one "group" factor (between) and two repeated measures (under the
#' # "foraging" or "frg" within factor). The groups are unequal, ranging form 16 to 81. 
#' # Finally, as this is repeated measures, there are correlations in each group
#' # (generally weak except possibly for the "treatment3" group).
#' 
#' # Such a compiled structure can be provided to anopa() by specifying the 
#' # repeated measures first (within cbind()), next the number of observation column, 
#' # and finally, the column containing the measure of correlation (any names can be used):
#' v <- anopa( {cbind(frg.before,frg.after); Count; uAlpha} ~ group, 
#'              minimalMxExampleCompiled,
#'              WSFactors = "foraging(2)")
#' anopaPlot(v)
#' summary(v)
#'
#'
#' # You can also ask easier outputs with:
#' explain(w)   # human-readable ouptut NOT YET DONE
#' 
####################################################################################
#'
#' @importFrom stats pchisq as.formula
#' @importFrom utils combn
#' @importFrom utils capture.output
#' @export     anopa
#' @importFrom Rdpack reprompt
#'
####################################################################################


anopa <- function(
    formula       = NULL,  #mandatory: the design of the data
    data          = NULL,  #mandatory: the data itself
    WSFactors     = NULL   #optional: if the data are in raw format, name the factors
) {
    ##############################################################################
	## NB. Herein, the following abbreviations are used
	##   WS: Within-subject design
	##   BS: Between-subject design
	##   MX: Mixed, within+between, design
    ##############################################################################

	
    ##############################################################################
    # STEP 0: preliminary preparations...
    ##############################################################################
	data     <- as.data.frame(data) # coerce to data.frame if tibble or compatible


    ##############################################################################
    # STEP 1: Input validation
    ##############################################################################    
    # 1.1: is the formula actually a valid formula?
    if (!is.formula(formula)) 
        stop("ANOPA::error(11): Argument `formula` is not a legitimate formula. Exiting...")

    # 1.2: has the formula 1 or more DV?
    if (is.one.sided( formula )) {
        stop("ANOPA::error(12): Argument `formula` has no DV. Exiting...")
    }

    # 1.3: are the data actually data?
    if( (!is.data.frame(data)) || (dim(data)[2] <= 1))
        stop("ANOPA::error(13): Argument `data` is not a data.frame or similar data structure. Exiting...")

    # 1.4: are the columns named in the formula present in the data?
    vars <- all.vars(formula)  # extract variables, cbind and nested alike
	vars <- vars[!(vars == ".")] # remove .
    if (!(all(vars %in% names(data)))) 
        stop("ANOPA::error(14): Variables in `formula` are not all in `data`. Exiting...")

	# 1.5: If wide format with repeated-measures, are the WSFactors given?
	if ((has.cbind.terms(formula)) && is.null(WSFactors)) 
		stop("ANOPA::error(15): Argument `WSFactors` must be defined in wide format with repeated measures Existing...")


    ##############################################################################
    # STEP 2: Manage WS factors
    ##############################################################################
    # 2.0: Keep only the columns named
    data     <- data[, names(data) %in% vars]


	# 2.1: get cbind variables if Wide (WS or MX) formats
	if (!in.formula(formula, "{") && has.cbind.terms(formula)) {
		# extract vars from cbind
		bvars <- c()
		for (i in 2:length(formula[[2]])) 
			bvars <- c(bvars, paste(formula[[2]][[i]]))
		cleanedWSF <- cleanWSFactors(WSFactors, bvars)
	}

	# 2.2: get cbind variables if Compiled (WS or MX)
	if (in.formula(formula, "{") && has.cbind.terms(formula)) {
		# extract vars from cbind
		bvars <- c()
		for (i in 2:length(formula[[2]][[2]])) 
			bvars <- c(bvars, paste(formula[[2]][[2]][[i]]))
		cleanedWSF <- cleanWSFactors(WSFactors, bvars)
	}

	# 2.3: get WSfactors in Long format before they are erased
	if (has.nested.terms(formula)) {
		tmp        <- getAroundNested(formula)
		wsvars     <- unique(data[[paste(tmp[[2]])]])
		cleanedWSF <- cleanWSFactors(WSFactors, wsvars)
	}

    ##############################################################################
    # STEP 3: Harmonize the data format to wide
    ##############################################################################

	# 3.1: Set defaults
	uAlpha    <- -99.9 # no correlation
	BSFactors <- WSFactors <- c()
	WSLevels  <- BSLevels  <- 1
	WSDesign  <- data.frame()
    compData  <- NULL 
    countCol  <- "n"
    alphaCol  <- "uAlpha"


    # 3.2: Convert data to wide format based on the format as infered from the formula
	if (in.formula(formula, "{") && has.cbind.terms(formula)) {
		# Case 1: Compiled (WS or MX)	template: {cbind(b1,..,bm);n;r} ~ Factors
		bracedvars <- c(paste(bvars), paste(formula[[2]][[3]]), paste(formula[[2]][[4]]))
		BSFactors  <- complement(vars, bracedvars)
        BSLevels   <- unlist(lapply(BSFactors, \(x) length(unique(data[,x])) ))
        DVvars <- bvars # ??
		WSFactors <- cleanedWSF[[1]]
        WSLevels  <- cleanedWSF[[2]]
        countCol  <- paste(formula[[2]][[3]])
        alphaCol  <- paste(formula[[2]][[4]]) 
        
        compData <- data # data are in compiled format
        wideData <- lapply(1:(dim(data)[1]), doONEline, 
                        DF = data, 
                        BSfacts  = BSFactors, 
                        WSfacts  = bvars, 
                        CountCol = paste(formula[[2]][[3]]), 
                        AlphaCol = paste(formula[[2]][[4]]) 
                    )
        wideData <- do.call(rbind, wideData)

	} else if (in.formula(formula, "{")) { 
		#case 1: Compiled (BS only)     template: {s;n} ~ Factors
		bracedvars <- c(paste(formula[[2]][[2]]), paste(formula[[2]][[3]]))
		wideData   <- ctow(data, bracedvars[1], bracedvars[2] )
		BSFactors  <- complement(vars, bracedvars)
        BSLevels   <- unlist(lapply(BSFactors, \(x) length(unique(wideData[,x])) ))
		DVvars     <- paste(formula[[2]][[2]])

	} else if (has.cbind.terms(formula)) {
		#case 2: Wide (WS or MX) 
		#     2.1: Wide (WS) 	template: cbind(b1,...,bm) ~ .
		#	  2.2: Wide (MX) 	template: cbind(b1,...,bm) ~ Factors
		BSFactors  <- complement(vars, bvars)
        BSLevels   <- unlist(lapply(BSFactors, \(x) length(unique(data[,x])) ))
		WSFactors  <- cleanedWSF[[1]]
		WSLevels   <- cleanedWSF[[2]]
		WSDesign   <- cleanedWSF[[3]]
		DVvars     <- bvars
		wideData <- data # nothing to do, already correct format

		# computes correlation with unitaryAlpha
		factors  <- names(data)[!(names(data) %in% bvars )]
		if (length(factors) == 0) {
			# adds a dummy BS factor
			data[["dummyBSfactor"]] <- 1; factors <- "dummyBSfactor"
		}
		uAlpha <- plyr::ddply(data, factors, function(x) {unitaryAlpha(as.matrix(x[bvars]))} )$V1

	} else if (has.nested.terms(formula)) {
		#case 3: long (BS or WS or MX)
		#     3.1: long (BS) 	template: b ~ BSFactors | Id
		#     3.2: long (WS)	template: b ~ WSConditions | Id
		#     3.3: long (MX) 	template: b ~ BSFactors * WSConditions | Id

		idvar <- getAfterNested( formula )
		DVvars <- paste(formula[[2]])

		# get vars that are changing for a given Id
		Factors <- names(data)[ !(names(data) %in% c(idvar, DVvars))]
		BSFactors <- c()
		WSFactors <- c()
		for (i in Factors) {
			if (dim(unique(data[data[[idvar]] == 1,][c(idvar, i)]))[1] > 1) {
				WSFactors <- c(WSFactors, i)
			} else {
				BSFactors <- c(BSFactors, i)
			}
		}
		wideData <- ltow(data, idvar, tmp[[2]], DVvars ) 
		# take the success name under Variable
		DVvars <- names(wideData)[!(names(wideData) %in% c( all.vars(formula[[3]]), "n"))]

		BSFactors <- complement(names(wideData), DVvars)
        BSLevels   <- unlist(lapply(BSFactors, \(x) length(unique(wideData[,x])) ) )
		WSFactors <- cleanedWSF[[1]]
		WSLevels  <- cleanedWSF[[2]]
		WSDesign  <- cleanedWSF[[3]]
		# computes correlation with unitaryAlpha
		if (!is.null(WSFactors)) {
			factors  <- names(wideData)[!(names(wideData) %in% wsvars )]
			if (length(factors) == 0) {
				# adds a dummy BS factor
				wideData[["dummyBSfactor"]] <- 1; factors <- "dummyBSfactor"
			}
			uAlpha <- plyr::ddply(wideData, factors, 
							function(x) {unitaryAlpha(as.matrix(x[wsvars]))}
			)$V1
			wideData[["dummyBSfactor"]] <- NULL
		}

	} else if (length(formula[[1]]) == 1) {
		#case 2.3: Wide (BS) 	template: b ~ Factors
		wideData <- data # nothing to do, already correct format

		# extract vars from rhs formula
		BSFactors <- all.vars(formula[[3]])
        BSLevels   <- unlist(lapply(BSFactors, \(x) length(unique(wideData[,x])) ) )
		WSLevels  <-1
		DVvars    <- paste(formula[[2]])

	} else {
		# error...
		stop("ANOPA::error(17): Unrecognized data format. Exiting...")
	}

    # 3.3: Keep the factor names 
	allFactors <- c(WSFactors, BSFactors)

	# 3.4: Acknolwedge limitations of the present package
    if( (length(allFactors) < 1) )
        stop("ANOPA::error(18a): No factor provided. Exiting...")
    if( (length(allFactors)>4) || (length(allFactors) < 1) )
        stop("ANOPA::error(18b): Too many factors. Exiting...")
    if( length(allFactors)==4 )
        stop("ANOPA::error(18c): Four factors; contact the author. Exiting...")


    ##############################################################################
    # STEP 4: run the analysis, depending on the number of factors
    ##############################################################################
	# 4.0: additional checks on within...
	if (prod(WSLevels) != length(DVvars))
		stop("ANOPA::error(19): There are missing within-subject level columns. Exiting...")

	# 4.1: to avoid integer overflow, convert integers to num
	for (i in DVvars) 
		wideData[[i]] <- as.numeric( wideData[[i]] )

	# 4.2: compile the data if was not given compiled
    if (is.null(compData)) 
        compData <- wtoc(wideData, DVvars, "n")

	# 4.3: check for all sorts of missing (not there or there with zeros...)
	compData <- checkmissingcellsBSFactors(compData, BSFactors, DVvars)
	compData <- checkforZeros(compData, DVvars)

    # 4.4: sort the data
	if (length(BSFactors) > 0) 
		compData <- compData[ do.call(order, data.frame(compData[,BSFactors]) ), ]

    # 4.4: perform the analysis based on the number of factors
    analysis <- switch( length(allFactors),
        anopa1way(compData, DVvars, countCol, alphaCol, BSFactors, WSFactors, WSLevels),
        anopa2way(compData, DVvars, countCol, alphaCol, BSFactors, WSFactors, WSLevels),
        anopa3way(compData, DVvars, countCol, alphaCol, BSFactors, WSFactors, WSLevels),
        anopa4way(compData, DVvars, countCol, alphaCol, BSFactors, WSFactors, WSLevels)
    )


    ##############################################################################
    # STEP 5: return the object
    ##############################################################################
    # 5.1:  preserve everything in an object of class ANOPAobject
    res <- list(
        type          = "ANOPAomnibus",
        formula       = as.formula(formula),
        BSfactColumns = BSFactors,
		BSfactNlevels = BSLevels,
        WSfactColumns = WSFactors,
		WSfactDesign  = WSDesign,
		WSfactNlevels = WSLevels,
		DVvariables   = DVvars,
        wideData      = wideData, # raw data are absolutely needed for plot only
		compData	  = compData, # compiled data are used for analyzes
		omnibus	      = analysis  # results of the omnibus analysis
    )
    class(res) <- c("ANOPAobject", class(res) )
    return( res )

}


##############################################################################
# Subfunctions to generate long from compiled
##############################################################################

getCorrStructure <- function(mat, targetUA) {
    # This function is used when the design has repeated measures
    # and the data are provided as compiled. In that case,
    # the exact dispositions of 1s and 0s cannot be determined and
    # so random rearrangments are tested to get uAlpha as close as possible.
    bestDiff = 999
    ncol = dim(mat)[2]
    for (seed in 1:2000) {
        set.seed(seed)
        for (col in 2:ncol) {
            mat[,col] = sample(mat[,col])
        }
        currentUA = unitaryAlpha(mat)
        if (abs(currentUA - targetUA) < .0001) {
            # close enough!
            bestmat = mat
            break
        }
        if (abs(currentUA - targetUA) < bestDiff) {
            bestDiff = abs(currentUA - targetUA)
            bestmat = mat
        }
    }
    # check that correlation are not too far from target
    if (abs(targetUA-unitaryAlpha(bestmat)) > 0.25) {
        ANOPAwarning("ANOPA::warning(111): Cannot reconstitute the correlation structure. Consider using raw (wide or long) data...")
    }
    return(bestmat)
}


getBaseStructure <- function(line, cnt) {
    # expand a line of the compiled data into a matrix
    mat <- data.frame( toDelete123 = 1:cnt )
    for (i in names(line)) {
        mat[[i]] <- c(rep(1, round(line[i]) ), rep(0, round(cnt-line[i]) ))
    }

    # delete sentinel column
    mat$toDelete123 <- NULL
    mat
}
addBSfcStructure <- function(mat, bs) {
    # adds to the mat the between-subject group(s)
    if (!is.null(bs)) {
        for (i in names(bs) )
            mat[[i]] <- rep( bs[[i]], dim(mat)[1])
    }
    mat
}

doONEline <- function(lineno, DF, BSfacts, WSfacts, CountCol, AlphaCol) {
    oneBScd <- DF[lineno, BSfacts, drop=FALSE]
    oneline <- DF[lineno, WSfacts]
    onecnt  <- DF[lineno, CountCol]
    oneual  <- DF[lineno, AlphaCol]

    mat1 <- getBaseStructure(oneline, onecnt)
    mat2 <- getCorrStructure(mat1, oneual )
    mat3 <- addBSfcStructure(mat2, oneBScd)
    mat3
}


##############################################################################
# Subfunctions for recognizing formats
##############################################################################

getAfterNested <- function(frm) {
	# There are three possible cases?
	#   frm1 <- b ~ BSFactors * WSConditions | Id 
	#   frm2 <- b ~ WSConditions | Id * BSFactors
	#   frm3 <- b ~ (WSConditions | Id) * BSFactors
	f <- sub.formulas(frm, "|")[[1]]
	if (length(f[[3]]) == 1) {
		v1 <- f[[3]]
	} else {
		if (f[[3]][[1]] == "*") {
			v1 <- f[[3]][[2]]
		} else {
			stop("ANOPA::internal(-1): Case non-existant: That should never happen")
		}
	}
	return( paste(v1)  )
}

getAroundNested <- function(frm) {
	# There are three possible cases?
	#   frm1 <- b ~ BSFactors * WSConditions | Id 
	#   frm2 <- b ~ WSConditions | Id * BSFactors
	#   frm3 <- b ~ (WSConditions | Id) * BSFactors
	f <- sub.formulas(frm, "|")[[1]]
	if (length(f[[3]]) == 1) {
		v1 <- f[[3]]
		if (length(f[[2]]) == 1) {
			v2 <- f[[2]]
		} else {
			v2 <- f[[2]][[length(f[[2]])]]
		}
	} else {
		if (f[[3]][[1]] == "*") {
			v1 <- f[[3]][[2]]
			if (length(f[[2]]) == 1) {
				v2 <- f[[2]]
			} else {
				v2 <- f[[2]][[length(f[[2]])]]
			}
		} else {
			stop("ANOPA::internal (-2): Case non-existant: That should never happen")
		}
	}
	return( c( paste(v1), paste(v2)) )
}

checkforZeros <- function( cData, ss) {
	# Are there emtpy cells, i.e. where the proportion is 0 on 0?
	# These must be imputed to avoid Undetermined scores
	# Adjust ANOPA.zeros vector for a different imputation strategy
    # DISCLAIMER: I did not validate if this is a sensible strategy. Do your homework.
	res  <- cData
	repl <- as.list(unlist(options("ANOPA.zeros"))) # niaisage...	
	for (i in ss) {
		if (any(mapply(\(x,y){(x==0)&&(y==0)}, res[[i]], res[["n"]]) ) ) {
			ANOPAwarning("ANOPA::warning(1): Some cells have zero over zero data. Imputing...")
			res[ res[[i]] == 0 & res[["n"]] == 0, c(i,"n")] <- repl
		}
	}
	return( res )
}

checkmissingcellsBSFactors <- function( cData, BSFactors, ss) {
    # all the combination of the levels of the factors
    a <- prod(unlist(lapply(BSFactors, \(x) length(unique(cData[,x])))))
    b <- dim(cData)[1]

    if (a != b) {
        # there are missing cases
        lvls     <- lapply(BSFactors, \(x) unique(cData[,x]))
        cmbn        <- expand.grid(lvls)
        names(cmbn) <- BSFactors
        notthere <- linesA1notInA2( cmbn, cData )

        for ( m in ss ) {notthere[[m]] <- 0}
        notthere[["n"]] <- 0
        cData <- rbind(cData, notthere)

        ANOPAmessage(paste("ANOPA::fyi(1): Combination of cells missing. Adding: "))
        temp <- paste0(capture.output(print(notthere, row.names=FALSE)),collapse="\n")
        ANOPAmessage(temp) 
    }
    return( cData )
}

cleanWSFactors <- function(WSFactors, ss) {
	# Unpack the WS factors and run a fyi if not inhibited...
	WSLevels <- c(1)
	WSDesign <- data.frame()
	if (!is.null(WSFactors)) {
		# separate name from nLevel
		for (i in 1:length(WSFactors)) {
			WSLevels[i]  <- as.integer(unlist(strsplit(WSFactors[i], '[()]'))[2]) 
			WSFactors[i] <-            unlist(strsplit(WSFactors[i], '[()]'))[1]
		}
		combinaisons <- expand.grid(lapply(WSLevels, seq))
		WSDesign       <- cbind(combinaisons, ss)
		colnames(WSDesign)[1:length(WSFactors)] <- WSFactors
		colnames(WSDesign)[length(WSFactors)+1] <- "Variable"
		
		if ( (!is.null(WSFactors)) & ('design' %in% getOption("ANOPA.feedback") ) ) {
			ANOPAmessage("ANOPA::fyi: Here is how the within-subject variables are understood:")
			temp <- paste0(capture.output(print(WSDesign[,c(WSFactors, "Variable") ], row.names=FALSE)),collapse="\n")
			ANOPAmessage(temp) 
		}
	}
	return( list(WSFactors, WSLevels, WSDesign ) )
}

# disjonction of two data.frames
linesA1notInA2 <- function( a1, a2 ) {
    # adds dummy columns
    a1$included_a1 <- TRUE
    a2$included_a2 <- TRUE
    res <- merge(a1, a2, all=TRUE)
    res <- res[ is.na(res$included_a2), ] 
    # removes the dummy columns
    res$included_a1 <- NULL
    res$included_a2 <- NULL
    return( res )
}

# harmonic mean
hmean <- function(v) (length(v)/ sum(1/v)) 


##############################################################################
##############################################################################
# Analyses functions per se
##############################################################################
##############################################################################


anopa1way <- function( cData, ss, ncol, acol, bsfacts, wsfacts, unneeded ) {
	# One-way ANOPA (either within- or between-subject design)
	# The observations are compiled into success (s) and number (n) per group and uAlpha when relevant

	if (length(wsfacts) == 1) { # within-subject design
		s     <- cData[ss]
		n     <- rep(cData[[ncol]], length(ss))
		facts <- wsfacts
		corlt <- cData[[acol]]		
	} else { # between-subject design
		s     <- cData[[ss]]
		n     <- cData[[ncol]]
		facts <- bsfacts
		corlt <- 0
	}

	# apply the transform to all the pairs (s, n)
	As <- mapply(A,    s, n)
	Vs <- mapply(varA, s, n)
	p  <- length(As)

	# compute mean square of effect P, mean square of error
	msP <- var(As)
	msE <- mean(Vs) * (1-corlt)

	# compute F ratio or chi-square test statistic
	F <- msP / msE
	g <- (p-1) * F

	# compute p value from the latter (the former has infinite df on denominator)
	pval <- 1 - pchisq(g, df = (p-1) )

	# the corrections
	cf   <- 1+ (p^2-1)/(6 * hmean(n) * (p-1) )
	pvaladj <- 1 - pchisq(g/cf,  df = (p-1) )

	# keep the results    
	results  <- data.frame(
		MS         = c(msP, msE), 
		df         = c(p-1,Inf), 
		F          = c(F, NA), 
		p          = c(pval, NA),
		correction = c(cf,NA),
		Fcorr      = c(g/cf/(p-1),NA), 
		pvalcorr   = c(pvaladj, NA)
	)
    rownames(results) <- c(facts, "Error")
	return(results)
}


anopa2way <- function( cData, ss, ncol, acol, bsfacts, wsfacts, wslevls ) {
	# Two-way ANOPA (within, between, or mixed design)
	# the observations are compiled into success (s) and number (n) per group and uAlpha when relevant
#print("Begin anopa2way")

	if (length(wsfacts) == 2) { # both within-subject factors
		s      <- cData[ss]
		n      <- rep(cData[[ncol]], length(ss))
		corlt  <- cData[[acol]]
		facts  <- wsfacts
		f1levl <- wslevls[2]
	} else if (length(wsfacts) == 1) { # mixed, within-between, design
		s      <- unlist(cData[ss]) # i.e., flatten
		n      <- rep(cData[[ncol]], length(ss))
		corlt  <- rep(cData[[acol]], wslevls[1])
		facts <- c(wsfacts, bsfacts)
		f1levl <- wslevls
	} else { # both between-subject factors
		s      <- cData[[ss]]
		n      <- cData[[ncol]]
		corlt  <- 0
		facts  <- bsfacts
		f1levl <- length(unique(cData[[bsfacts[1]]]))
	}

	# apply the transform to all the pairs (s, n)
	As <- mapply(A,    s, n)
	Vs <- mapply(varA, s, n)

	# fold the scores into a matrix
	Af  <- matrix( As, ncol = f1levl) 
	Vf  <- matrix( Vs, ncol = f1levl)
	ns  <- matrix( n,  ncol = f1levl)

	# compute marginal means (P is "column" factor, Q is "row" factor) and grand mean
	AP  <- colMeans(Af)   # marginal mean along Q factor
	AQ  <- rowMeans(Af)   # marginal mean along P factor
	Ag  <- mean(As)       # grand mean

	nP  <- colSums(ns)    # sample size across all Q levels
	nQ  <- colSums(t(ns)) # sample size across all P levels

	p   <- length(AP)
	q   <- length(AQ)

	# compute mean square of effects P, Q and PxQ
	msP <- q * var( AP )
	msQ <- p * var( AQ )
	msPQ    <- 1/((p-1)*(q-1)) * sum((As - outer(AQ, AP, `+`)  + mean(As) )^2 )

	# get mean squared of error for within and between respectively
	msEintra <- 1/(p*q) * sum( (1-corlt)/(4*(ns+1/2) )  )
	msEinter <- 1/(p*q) * sum( 1 / (4*(ns+1/2)) )	

	# assign error terms to each factor based on design
	msEp  <- if (facts[1] %in% wsfacts) msEintra else msEinter
	msEq  <- if (facts[2] %in% wsfacts) msEintra else msEinter
	msEpq <- if ((facts[1] %in% wsfacts)|(facts[2] %in% wsfacts)) msEintra else msEinter

	# compute F ratio or chi-square test statistic
	FP  <- msP/msEp
	FQ  <- msQ/msEq
	FPQ <- msPQ/msEpq
	gP  <- (p-1) * FP
	gQ  <- (q-1) * FQ
	gPQ <- (p-1) * (q-1) * FPQ

	# compute p value from the latter (the former has infinite df on denominator)
	pvalP  <- 1 - pchisq(gP,  df = (p-1) )
	pvalQ  <- 1 - pchisq(gQ,  df = (q-1) )
	pvalPQ <- 1 - pchisq(gPQ, df = (p-1) * (q-1)  )

	# If you want to apply the corrections
	cfP       <- 1 + (p^2-1)/(6 * hmean(nP) * (p-1) )
	cfQ       <- 1 + (q^2-1)/(6 * hmean(nQ) * (q-1) )
	cfPQ      <- 1 + ((p * q)^2-1)/(6 * hmean(ns) * (p-1)*(q-1) )

	pvalPadj  <- 1 - pchisq(gP/cfP,   df = (p-1) )
	pvalQadj  <- 1 - pchisq(gQ/cfQ,   df = (q-1) )
	pvalPQadj <- 1 - pchisq(gPQ/cfPQ, df = (p-1)*(q-1) )

	# keep the results    
	results  <- data.frame(
		MS         = c(msP, msQ, msPQ, msEintra, msEinter ), 
		df         = c(p-1, q-1, (p-1)*(q-1), Inf, Inf ), 
		F          = c(FP, FQ, FPQ, NA, NA ), 
		pvalue     = c(pvalP, pvalQ, pvalPQ, NA, NA ),
		correction = c(cfP, cfQ, cfPQ, NA, NA ),
		Fcorr      = c(FP/cfP, gQ/cfQ/(q-1), gPQ/cfPQ/((p-1)*(q-1)), NA, NA ),
		pvalcorr   = c(pvalPadj, pvalQadj, pvalPQadj, NA, NA)
	)

    rownames(results) <-  c(facts, paste(facts, collapse=":"), 
			"Error(within)", "Error(between)" )
	if (length(bsfacts) == 0) results <- results[-5,] #remove 5th line
	if (length(wsfacts) == 0) results <- results[-4,] #remove 4th line
	return(results)
}


anopa3way <- function( cData, ss, ncol, acol, bsfacts, wsfacts, wslevls ) {
	# Three-way ANOPA (any design with within or between subject factors)
	# the observations are compiled into success (s) and number (n) per group and uAlpha when relevant

	if (length(wsfacts) == 3) { # entirely within-subject factors
		s      <- cData[ss]
		n      <- rep(cData[[ncol]], length(ss))
		corlt  <- cData[[acol]]
		facts  <- wsfacts
		lvlp <- 1:wslevls[1]  # first is always within
		lvlq <- 1:wslevls[2]
		lvlr <- 1:wslevls[3]
		p <- length(lvlp)
		q <- length(lvlq)
		r <- length(lvlr)

	} else 	if (length(wsfacts) == 2) { # mixed, 2 within-subject factors
		s      <- cData[ss]
		n      <- rep(cData[[ncol]], length(ss))
		corlt  <- cData[[acol]]
		facts  <- c(wsfacts, bsfacts)
		lvlp <- 1:wslevls[1]
		lvlq <- 1:wslevls[2]
		lvlr <- unique(cData[[bsfacts[1]]])
		p <- length(lvlp)
		q <- length(lvlq)
		r <- length(lvlr)
		corlt <- array(unlist(lapply( corlt, rep, p*q)), dim = c(r,q,p))

	} else if (length(wsfacts) == 1) { # mixed, 1 within-subject factor
		s      <- c(t(cData[ss])) # i.e., flatten
		n      <- rep(cData[[ncol]], length(ss))
		corlt  <- rep(cData[[acol]], wslevls[1])

		facts <- c(wsfacts, bsfacts)
		lvlp <- 1:wslevls[1]
		lvlq <- unique(cData[[bsfacts[1]]])
		lvlr <- unique(cData[[bsfacts[2]]])
		p <- length(lvlp)
		q <- length(lvlq)
		r <- length(lvlr)
		corlt <- array(unlist(lapply( corlt, rep, p)), dim = c(r,q,p))

	} else { # entirely between-subject factors
		s      <- cData[[ss]]
		n      <- cData[[ncol]]
		corlt  <- 0
		facts  <- bsfacts
		lvlp <- unique(cData[[bsfacts[1]]])
		lvlq <- unique(cData[[bsfacts[2]]])
		lvlr <- unique(cData[[bsfacts[3]]])
		p <- length(lvlp)
		q <- length(lvlq)
		r <- length(lvlr)
	}

	# apply the transform to all the pairs (s, n)
	As <- mapply(A,    s, n)
	Vs <- mapply(varA, s, n)

	# fold the scores into an array
	# for between-subject design: https://stackoverflow.com/a/52435862/5181513
	namesOfDim <- list( lvlr, lvlq, lvlp )

	Af <- array(As, dim = c(r, q, p), dimnames = namesOfDim)
	Vf <- array(Vs, dim = c(r, q, p), dimnames = namesOfDim) 
	ns <- array(n,  dim = c(r, q, p), dimnames = namesOfDim) 

	# compute marginal means (P is "column" factor, Q is "row" factor) and grand mean
	AP <- apply(Af, 3, mean)
	AQ <- apply(Af, 2, mean)
	AR <- apply(Af, 1, mean)

	APQ <- apply( Af, c(3,2), mean ) ## Les dimensions sont inversées
	APR <- apply( Af, c(3,1), mean )
	AQR <- apply( Af, c(2,1), mean )

	nP  <- apply( ns, 3, sum )    # sample size across all R levels
	nQ  <- apply( ns, 2, sum )    # sample size across all R levels
	nR  <- apply( ns, 1, sum )    # sample size across all R levels
	nPQ <- apply( ns, c(3,2), sum )    # sample size across all R levels
	nPR <- apply( ns, c(3,1), sum )    # sample size across all R levels
	nQR <- apply( ns, c(2,1), sum )    # sample size across all R levels

	# compute mean square of effects P, Q and PxQ; Keppel, 1978
	msP <- q*r * var( AP )
	msQ <- p*r * var( AQ )
	msR <- p*q * var( AR )
	msPQ <- ((p*q-1) * r * var( c(APQ) ) - (p-1)*msP - (q-1)*msQ)/((p-1)*(q-1))
	msPR <- ((p*r-1) * q * var( c(APR) ) - (p-1)*msP - (r-1)*msR)/((p-1)*(r-1))
	msQR <- ((q*r-1) * p * var( c(AQR) ) - (q-1)*msQ - (r-1)*msR)/((q-1)*(r-1))
	msPQR <- ((p*q*r-1)*var( c(Af) ) -(p-1)*(q-1)*msPQ -(p-1)*(r-1)*msPR -(q-1)*(r-1)*msQR -(p-1)*msP -(q-1)*msQ -(r-1)*msR )/ ((p-1)*(q-1)*(r-1))

	# get mean squared of error for within and between respectively
	msEintra <- 1/(p*q*r)*sum( (1-corlt)/ (4*ns+1/2))
	msEinter <- 1/(p*q*r)*sum( 1 / (4*ns+1/2))

	# assign error terms to each factor based on design
	msEp  <- if (facts[1] %in% wsfacts) msEintra else msEinter
	msEq  <- if (facts[2] %in% wsfacts) msEintra else msEinter
	msEr  <- if (facts[3] %in% wsfacts) msEintra else msEinter
	msEpq <- if ((facts[1] %in% wsfacts)|(facts[2] %in% wsfacts)) msEintra else msEinter
	msEpr <- if ((facts[1] %in% wsfacts)|(facts[3] %in% wsfacts)) msEintra else msEinter
	msEqr <- if ((facts[2] %in% wsfacts)|(facts[3] %in% wsfacts)) msEintra else msEinter
	msEpqr <- if ((facts[1] %in% wsfacts)|(facts[2] %in% wsfacts)|(facts[3] %in% wsfacts)) msEintra else msEinter

	# compute F ratio or chi-square test statistic
	FP  <- msP/msEp
	FQ  <- msQ/msEq
	FR  <- msR/msEr
	FPQ <- msPQ/msEpq
	FPR <- msPR/msEpr
	FQR <- msQR/msEqr
	FPQR <- msPQR/msEpqr

	gP  <- (p-1) * FP
	gQ  <- (q-1) * FQ
	gR  <- (r-1) * FR
	gPQ <- (p-1) * (q-1) * FPQ
	gPR <- (p-1) * (r-1) * FPR
	gQR <- (q-1) * (r-1) * FQR
	gPQR <- (p-1) * (q-1) * (r-1) * FPQR

	# compute p value from the latter (the former has infinite df on denominator)
	pvalP   <- 1 - pchisq(gP,   df = (p-1) )
	pvalQ   <- 1 - pchisq(gQ,   df = (q-1) )
	pvalR   <- 1 - pchisq(gR,   df = (r-1) )
	pvalPQ  <- 1 - pchisq(gPQ,  df = (p-1) * (q-1)  )
	pvalPR  <- 1 - pchisq(gPR,  df = (p-1) * (r-1)  )
	pvalQR  <- 1 - pchisq(gQR,  df = (q-1) * (r-1)  )
	pvalPQR <- 1 - pchisq(gPQR, df = (p-1) * (q-1) * (r-1)  )

	# If you want to apply the corrections; we remove the imputed cells...
	cfP       <- 1 + (p^2-1)/(6 * hmean(nP) * (p-1) )
	cfQ       <- 1 + (q^2-1)/(6 * hmean(nQ) * (q-1) )
	cfR       <- 1 + (r^2-1)/(6 * hmean(nR) * (r-1) )
	cfPQ      <- 1 + ((p * q)^2-1)/(6 * hmean(ns[ns>1]) * (p-1)*(q-1) )
	cfPR      <- 1 + ((p * r)^2-1)/(6 * hmean(ns[ns>1]) * (p-1)*(r-1) )
	cfQR      <- 1 + ((q * r)^2-1)/(6 * hmean(ns[ns>1]) * (q-1)*(r-1) )
	cfPQR      <- 1 + ((p * q * r)^2-1)/(6 * hmean(ns[ns>1]) * (p-1)*(q-1)*(r-1) )

	pvalPadj   <- 1 - pchisq(gP/cfP,     df = (p-1) )
	pvalQadj   <- 1 - pchisq(gQ/cfQ,     df = (q-1) )
	pvalRadj   <- 1 - pchisq(gR/cfR,     df = (r-1) )
	pvalPQadj  <- 1 - pchisq(gPQ/cfPQ,   df = (p-1)*(q-1) )
	pvalPRadj  <- 1 - pchisq(gPR/cfPR,   df = (p-1)*(r-1) )
	pvalQRadj  <- 1 - pchisq(gQR/cfQR,   df = (q-1)*(r-1) )
	pvalPQRadj <- 1 - pchisq(gPQR/cfPQR, df = (p-1)*(q-1)*(r-1) )

	# keep the results    
	results  <- data.frame(
		MS         = c(msP, msQ, msR, msPQ, msPR, msQR, msPQR, msEintra, msEinter ), 
		df         = c(p-1, q-1, r-1, (p-1)*(q-1), (p-1)*(r-1), (q-1)*(r-1), (p-1)*(q-1)*(r-1), Inf, Inf ), 
		F          = c(FP, FQ, FR, FPQ, FPR, FQR, FPQR, NA, NA ), 
		pvalue     = c(pvalP, pvalQ, pvalR, pvalPQ, pvalPR, pvalQR, pvalPQR, NA, NA ),
		correction = c(cfP, cfQ, cfR, cfPQ, cfPR, cfQR, cfPQR, NA, NA ),
		Fcorr      = c(FP/cfP, gQ/cfQ/(q-1), gR/cfR/(r-1), 
					  gPQ/cfPQ/((p-1)*(q-1)), gPR/cfPR/((p-1)*(r-1)), gQR/cfQR/((q-1)*(r-1)),
					  gPQR/cfPQR/((p-1)*(q-1)*(r-1)),
		  			  NA, NA ),
		pvalcorr   = c(pvalPadj, pvalQadj, pvalRadj, pvalPQadj, pvalPRadj, pvalQRadj, pvalPQRadj, NA, NA)
	)

    rownames(results) <-  c(facts, 
							apply(data.frame(utils::combn(facts,2)), 2, paste, collapse = ":"),
							paste(facts, collapse=":"),
							"Error(within)", "Error(between)" )
	if (length(bsfacts) == 0) results <- results[-9,] #remove between-error line
	if (length(wsfacts) == 0) results <- results[-8,] #remove within-error line
	return(results)
}


anopa4way <- function( cData, ss, ncol, acol, bsfacts, wsfacts, wslevls ) {

	return("Not programmed so far. Contact Denis Cousineau if needed.")

}



