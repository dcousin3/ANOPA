####################################################################################
#' @title Computing power within the ANOPA.
#'
#' @aliases anopaPower2N anopaN2Power anopaProp2fsq
#'
#' @md
#'
#' @description The function `anopaN2Power()` performs an analysis of statistical power
#'      according to the `ANOPA` framework. See \insertCite{lc23b;textual}{ANOPA} for more.
#'      `anopaPower2N()` computes the sample size to reach a given power.
#'		Finally, `anopaProp2fsq()` computes the f^2 effect size from a set of proportions.
#'
#' @usage anopaPower2N(power, P, f2, alpha)
#' @usage anopaN2Power(N, P, f2, alpha)
#' @usage anopaProp2fsq(props, ns, unitaryAlpha, method="approximation")
#'
#' @param power target power to attain;
#' @param N sample size;
#' @param ns sample size per group;
#' @param P number of groups;
#' @param f2 effect size Cohen's $f^2$;
#' @param props a set of expected proportions (if all between 0 and 1) or number of success per group.
#' @param alpha (default if omitted .05) the decision threshold.
#' @param method for computing effect size $f^2$ is 'approximation' or 'exact' only.
#' @param unitaryAlpha for within-subject design, the measure of correlation 
#'     across measurements.
#'
#' @return `anopaPower2N()` returns a sample size to reach a given power level.
#'         `anopaN2Power()` returns statistical power from a given sample size.
#'         `anopaProp2fsq()` returns $f^2$ the effect size from a set of proportions 
#'			and sample sizes.
#'
#' @details Note that for `anopaProp2fsq()`, the expected effect size $f^2$ 
#'    depends weakly on the sample sizes. Indeed, the Anscombe transform
#'    can reach more extreme scores when the sample sizes are larger, influencing
#'    the expected effect size.
#'
#' @references
#' \insertAllCited{}
#'
#'
#' @examples
#' # 1- Example of the article:
#' # with expected frequences .34 to .16, assuming as a first guess groups of 25 observations:
#' f2 <- anopaProp2fsq( c( 0.32, 0.64, 0.40, 0.16), c(25,25,25,25) );
#' f2
#' # f-square is 0.128.
#'
#' # f-square can be converted to eta-square with
#' eta2 <- f2 / (1 + f2)
#'
#'
#' # With a total sample of 97 observations over four groups,
#' # statistical power is quite satisfactory (85%).
#' anopaN2Power(97, 4, f2)
#' 
#' # 2- Power planning.
#' # Suppose we plan a four-classification design with expected proportions of:
#' pred <- c(.35, .25, .25, .15)
#' # P is the number of classes (here 4)
#' P <- length(pred)
#' # We compute the predicted f2 as per Eq. 5
#' f2 <- 2 * sum(pred * log(P * pred) )
#' # the result, 0.0822, is a moderate effect size.
#' 
#' # Finally, aiming for a power of 80%, we run
#' anopaPower2N(0.80, P, f2)
#' # to find that a little more than 132 participants are enough.
#' 
####################################################################################
#'
#' @importFrom stats var
#' @importFrom stats qchisq
#' @importFrom stats optimize
#' @export anopaN2Power
#' @export anopaPower2N
#' @export anopaProp2fsq
#
####################################################################################


# 1- Returns statistical power given N, P, f2 and alpha      
anopaN2Power <- function(N, P, f2, alpha = .05) {
    pFc <- stats::qchisq( p = 1 - alpha, df = P-1 )
    1- stats::pchisq(q = pFc,  df = P-1, ncp = N * f2)
}

# 2- Performs a search for N to reach a certain statistical power
anopaPower2N <- function(power, P, f2, alpha = .05) {
    # define the objective: minimize the lag between the desired power and
    # the power achieved by a certain N
    objective <- function(N, power, P, f2, alpha = .05) {
        (power - anopaN2Power(N, P, f2, alpha))^2 
    }
    # launch optimization search for the desired N between 10 and 1000
    stats::optimize( objective, interval = c(10,1000), P = P, f2 = f2, power = power)$minimum
}


# 3- propTof2 computes f^2 from proportions/nbre of success using 
#    approximation of Eq. (17) or exact formula
anopaProp2fsq <- function(props, ns, unitaryAlpha = NULL, method = "approximation") {
	if (is.null(props))
		stop("ANOPA::effect size: Empty vector of propoprtions. Exiting...")
	if (is.null(ns))
		stop("ANOPA::effect size: Empty vector of sample sizes. Exiting...")
	if (length(props)!=length(ns))
		stop("ANOPA::effect size: Number of proportions not equalt to number of sample sizes. Exiting...")
	if (!(method %in% c("approximation","exact")) )
		stop("ANOPA::effect size: Methods are 'approximation' or 'exact' only. Exiting...")
	if (all(props<1)) props = props * ns
	if (!(is.null(unitaryAlpha))) {if ((unitaryAlpha< -1)|(unitaryAlpha>1))
		stop("ANOPA::effect size: unitary Alpha must be between -1 and 1. Exiting...")
	}
	if (is.null(unitaryAlpha)) {m = 1} else {m = 1/sqrt(1-unitaryAlpha)}
	p <- length(props)
	if (method =="approximation") {
		return( m^2 * 4 * (p-1)/p * stats::var( mapply(A, props, ns) ) )
	} else {
		return( m^2 * (p-1) * stats::var( mapply(A, props, ns) ) / sum( ns / (4*(ns+1/2))) )
	}
}
