######################################################################################
#' @title unitary alpha
#'
#' @md
#'
#' @description The function 'unitaryAlpha()' computes
#'      the unitary alpha (\insertCite{lc23}{ANOPA}). This
#'      quantity is a novel way to compute correlation in a matrix
#'      where each column is a measure and each line, a subject.
#'      This measure is based on Cronbach's alpha (which could be
#'      labeled a 'global alpha').
#'
#' @usage unitaryAlpha( m )
#'
#' @param m A data matrix for a group of observations.
#'
#' @return A measure of correlation between -1 and +1.
#'
#' @details This measure is derived from Cronbach' measure of 
#' reliability as shown by \insertCite{lc23;textual}{ANOPA}.
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' 
#' # Generate a random matrix (here binary entries)
#' set.seed(42)
#' N <- M <- 10
#' m <- matrix( runif(N*M), N, M)
#' 
#' # compute the unitary alpha from that random matrix
#' unitaryAlpha(m)
#'
######################################################################################
#' 
#' @export unitaryAlpha
#
######################################################################################


unitaryAlpha <- function(m) {
	k <- dim(m)[2]
	V <- var(apply(m, 1, FUN=sum))
	S <- sum(apply(m, 2, FUN=var))
	(V-S)/((k-1)*S)
}
