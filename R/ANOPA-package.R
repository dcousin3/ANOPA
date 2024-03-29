##############################################################################
#' @details ANOPA library for analyses of proportions using Anscombe transform  
#' 
#' @md
#' 
#' @description 'ANOPA' is a library to perform proportion analyses.
#' It is based on the F statistics (first developed by Fisher).
#' This statistics is fully additive and can be decomposed in 
#' main effects and interaction effects, in simple effects in the
#' decomposition of a significant interaction, in contrasts, etc.
#' The present library performs these analyses and also can be used
#' to plan statistical power for the analysis of proportions, obtain
#' plots of the various effects, etc. It aims at replicating the most
#' commonly-used ANOVA commands so that using this package should be
#' easy.
#' 
#' The data supplied to an ANOPA can be in three formats: (i) long format,
#' (ii) wide format, (iii) compiled format, or (iv) raw format. Check 
#' the 'anopa' commands for more precision (in what follow, we assume 
#' the compiled format where the proportions are given in a column name 'Freq')
#' 
#' The main function is
#' 
#'    \code{w <- anopa(formula, data)}  
#' 
#' where \code{formula} is a formula giving the factors, e.g., "Freq ~ A * B".
#'
#' For more details on the underlying math, see \insertCite{lc23;textual}{ANOPA}.
#' 
#' An omnibus analysis may be followed by simple effects or contrasts analyses:
#'   \code{emProportions(w, formula)}
#'   \code{contrast(w, listOfContrasts)}
#' 
#' As usual, the output can be obtained with
#'   \code{print(w) #implicite}
#'   \code{summary(w) # or summarize(w) for the G statistics table}
#'   \code{explain(w) # for human-readable output}
#' 
#' Data format can be converted to other format with
#'   \code{toLong(w)}
#'   \code{toWide(w)}
#'   \code{toCompiled(w) # the only format that cannot be used as input to anopa}
#' 
#' The package includes additional, helper, functions: \itemize{
#'      \item{\code{anopaPower2N()}} to compute sample size given effect size;
#'      \item{\code{anopaN2Power()}} to compute statistical power given a sample size;
#'      \item{\code{anopaPropTofsq()}} to compute the effect size;
#'      \item{\code{anopaPlot()}} to obtain a plot of the proportions with error bars;
#'      \item{\code{GRP()}} to generate random proportions from a given design.
#' }
#' and example datasets, some described in the article:  \itemize{
#'      \item{\code{ArringtonEtAl2002}} illustrates a 3 x 2 x 4 design;
#'      \item{\code{ArticleExample1}} illustrates a 4-way design;
#'      \item{\code{ArticleExample2}} illustrates a 2 x 3  design;
#'      \item{\code{ArticleExample3}} illustrates a (4) within-subject design;
#' }
#' 
#' The functions uses the following options: \itemize{
#'     \item{\code{ANOPA.feedback}} 'design', 'warnings', 'summary', 'all' or 'none';
#'     \item{\code{ANOPA.zeros}}    how are handled the zero trials to avoid 0 divided by 0 error;
#'     \item{\code{ANOPA.digits}}   for the number of digits displayed in the summary table.
#' }
#' 
#' @references
#' \insertAllCited{}
#' 
##############################################################################
#' @keywords internal
"_PACKAGE"
#> [1] "_PACKAGE"
##############################################################################

# Create a local environment for one temporary variable...
ANOPA.env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {    
    # Set the default feedback messages displayed and the number of digits:
    # You can use 'all' to see all the messages, or 'none'.
	# Also set the replacement if a between-subject cell is missing.
    options( 
		ANOPA.feedback = c('design','warnings','summary'), 
		ANOPA.digits = 4,
		ANOPA.zeros = c(0.05, 1) # a very small number of success onto a single try
	)

}

.onDetach <- function(libpath) {
    # remove the options
    options( ANOPA.feedback = NULL, ANOPA.digits = NULL, ANOPA.zeros = NULL )
}

# display or not stuff
#  warnings shows warning messages
#  design   shows fyi messages regarding how the design is understood
#  summary  is not used so far...
ANOPAwarning <- function( txt ) {
    if ( ("all" %in% getOption("ANOPA.feedback"))|("warnings" %in% getOption("ANOPA.feedback"))) {
      warning(txt, call. = FALSE)
    }
}
ANOPAmessage <- function( txt ) {
    if ( ("all" %in% getOption("ANOPA.feedback"))|("design" %in% getOption("ANOPA.feedback"))) {
      message(txt)
    }
}
##############################################################################
# to inhibit "no visible binding for global variable" errors from :
# globalVariables(c("xxx","xxx","xxx"))
##############################################################################

