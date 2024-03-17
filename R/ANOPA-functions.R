###################################################################################
#' @title transformation functions
#'
#' @aliases A varA Atrans SE.Atrans var.Atrans CI.Atrans prop CI.prop
#'
#' @md
#'
#' @description The transformation functions `A()` performs the
#'      Anscombe transformation on a pair \{number of success; number
#'      of trials\} = \{s; n\} (where the symbol ";" is to be read "over".
#'      The function `varA()` returns the theoretical variance from
#'      the pair \{s; n\}. Both functions are central to the ANOPA
#'	    \insertCite{lc23}{ANOPA}. It was originally proposed by 
#'      \insertCite{z35}{ANOPA} and formalized by \insertCite{a48}{ANOPA}.
#'
#' @usage A(s, n)
#' @usage varA(s, n)
#' @usage Atrans(v)    
#' @usage SE.Atrans(v) 
#' @usage var.Atrans(v) 
#' @usage CI.Atrans(v, gamma) 
#' @usage prop(v)      
#' @usage CI.prop(v, gamma) 
#'
#'
#' @param s a number of success;
#' @param n a number of trials.
#' @param v a vector of 0s and 1s.
#' @param gamma a confidence level, default to .95 when omitted.
#'
#'
#' @return `A()` returns a score between 0 and 1.57 where a `s` of zero results in
#'     `A(0,n)` tending to zero when the number of trials is large, 
#'     and where the maximum occurs when `s` equals `n` and
#'     are both very large, so that for example `A(1000,1000) = 1.55`. The
#'     midpoint is always 0.786 irrespective of the number of trials
#'     `A(0.5 * n, n) = 0.786`.
#'	   The function `varA()` returns the theoretical variance of an Anscombe
#'     transformed score. It is exact as `n` gets large, and overestimate variance
#'     when `n` is small. Therefore, a test based on this transform is either exact
#'     or conservative.
#' @details The functions `A()` and `varA()` take as input two integers, `s` 
#'    the number of success and `n` the number of observations.
#'    The functions `Atrans()`, `SE.Atrans()`, `var.Atrans()`, `CI.Atrans()`, `prop()` and `CI.prop()`
#'    take as input a single vector `v` of 0s and 1s from which the number of 
#'    success and the number of observations are derived.
#'
#' @references
#' \insertAllCited
#'
#' @examples
#' # The transformations from number of 1s and total number of observations:
#' A(5, 10)
#'  
#' varA(5, 10)
#'  
#' # Same with a vector of observations:
#' Atrans( c(1,1,1,1,1,0,0,0,0,0) )
#'  
#' var.Atrans( c(1,1,1,1,1,0,0,0,0,0) )
#'  
#'
###################################################################################
#'
#' @importFrom stats qchisq
#' @export A
#' @export varA
#' @export Atrans
#' @export SE.Atrans
#' @export var.Atrans
#' @export CI.Atrans
#' @export prop
#' @export CI.prop
#'
###################################################################################


###################################################################################
# the Anscombe transformation on s, n
A <-function(s, n) {
    asin(sqrt( (s+3/8) / (n+3/4) ))
}
 
# ... and its theoretical variance
varA <- function(s, n) {
    1/ (4*(n+1/2))
}


###################################################################################
# the Anscombe transformation for a vector of binary data 0|1
Atrans <-function(v) {
    x <- sum(v)
    n <- length(v)
    asin(sqrt( (x+3/8) / (n+3/4) ))
} 
  
# its standard error...
SE.Atrans <- function(v) {
    0.5 / sqrt(length(v)+1/2)
}

# its variance...
var.Atrans <- function(v) {
print(length(v))
    1 / (4*(length(v)+1/2))
}

# ... and its confidence interval
CI.Atrans <- function(v, gamma = 0.95){
    SE.Atrans(v) * sqrt( stats::qchisq(gamma, df=1) )
}


###################################################################################
# the proportion of success for a vector of binary data 0|1
prop <- function(v){
    x <- sum(v)
    n <- length(v)
    x/n
}

# the error bar is an inverse transformation of the Anscombe error bars
CI.prop <- function(v, gamma = 0.95) {
    y     <- Atrans(v)
    n     <- length(v)
    cilen <- CI.Atrans(v, gamma)
    # the difference adjustment is done herein.
    ylo   <- y - sqrt(2) * cilen
    yhi   <- y + sqrt(2) * cilen
    # reverse arc-sin transformation
    xlo <- (n+3/4)*(sin(ylo)^2) - 3/8
    xhi <- (n+3/4)*(sin(yhi)^2) - 3/8

    # superb is running automatic checks for values outside valid range
    if (is.na(xlo)) xlo <- 0.0
    if (is.na(xhi)) xhi <- 1.0

    cilenlo <- xlo / n
    cilenhi <- xhi / n
    c(cilenlo, cilenhi)
}


###################################################################################
