###################################################################################
#' @title ArticleExample1
#'
#' @name ArticleExample1
#'
#' @description These are the data from the first example reported in 
#'    \insertCite{lc23}{ANOPA}. It shows ficticious data with regards to 
#'	  the proportion of incubation as a function of the distracting task.
#'    The design is a between-subject design with 4 groups.
#'
#' @md
#'
#' @docType data
#'
#' @format An object of class data.frame.
#'
#' @keywords datasets
#'
#' @references 
#' \insertAllCited{}
#'
#' @source \doi{10.20982/tqmp.19.2.p173}
#'
#' @examples
#' library(ANOPA)
#'
#' # the ArticleExample1 data shows an effect of the type of distracting task 
#' ArticleExample1
#' 
#' # We perform an anopa on this dataset
#' w <- anopa( {nSuccess; nParticipants} ~ DistractingTask, ArticleExample1)
#' 
#' # We finish with post-hoc Tukey test
#' e <- posthocProportions( w )
#' 
#' # a small plot is *always* a good idea
#' anopaPlot(w)
#' 
#' 
"ArticleExample1"