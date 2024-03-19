###################################################################################
#' @title A collection of minimal Examples from various designs with one or two factors.
#'
#' @name minimalExamples
#'
#' @md
#'
#' @description The datasets present minimal examples that are analyzed with an 
#'    Analysis of Frequency Data method (described in \insertCite{lc23;textual}{ANOPA}.
#'  The five datasets are 
#'    - 'minimalBSExample': an example with a single factor (state of residency)
#'    - 'twoWayExample': an example with two factors, Class and Difficulty
#'    - 'minimalWSExample': an example with a within-subject design (three measurements)
#'    - 'twoWayWithinExample': an example with two within-subject factors 
#'    - 'minimalMxExample': a mixed design having one within and one between-subject factors
#'
#' @docType data
#'
#' @format Objects of class data.frame:
#'
#' @keywords datasets
#'
#' @references 
#' \insertAllCited{}
#'
#' @examples
#' library(ANOPA)
#'
#' # the twoWayExample data with proportions per Classes and Difficulty levels 
#' twoWayExample
#' 
#' # perform an anopa on this dataset
#' w <- anopa( {success;total} ~ Difficulty * Class, twoWayExample) 
#' 
#' # We analyse the proportions by Difficulty for each Class
#' e <- emProportions(w, ~ Difficulty | Class)
#' 

#' @rdname minimalExamples
"minimalBSExample"

#' @rdname minimalExamples
"twoWayExample"

#' @rdname minimalExamples
"minimalWSExample"

#' @rdname minimalExamples
"twoWayWithinExample"

#' @rdname minimalExamples
"minimalMxExample"