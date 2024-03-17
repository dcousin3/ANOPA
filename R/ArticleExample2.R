###################################################################################
#' @title ArticleExample2
#'
#' @name ArticleExample2
#'
#' @description These are the data from the second example reported in 
#'    \insertCite{lc23}{ANOPA}. It shows ficticious data with regards to 
#'	  the proportion of graduation for persons with dyslexai as a function 
#'    of the moment of diagnostic (early or late) and the socoi-economic status (SES).
#'    The design is a between-subject design with 2 x 3 = 6 groups.
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
#' # the ArticleExample2 data shows an effect on the success to graduate as a function of
#' # socioeconomic status and moment of diagnostic:
#' ArticleExample2
#' 
#' # perform an anopa on this dataset
#' w <- anopa( {s;n} ~ MofDiagnostic * SES, ArticleExample2)
#' 
#' # a small plot is *always* a good idea
#' anopaPlot(w)
#' # here the plot is only for the main effect of SES.
#' anopaPlot(w, ~ SES)
#' 
"ArticleExample2"