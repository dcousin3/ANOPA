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
#'    - `minimalBSExample`: an example with a single factor (state of residency)
#'    - `twoWayExample`: an example with two factors, Class and Difficulty
#'    - `minimalWSExample`: an example with a within-subject design (three measurements)
#'    - `twoWayWithinExample`: an example with two within-subject factors 
#'    - `minimalMxExample`: a mixed design having one within and one between-subject factors
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
#' # the minimalBSExample data with proportions per state of residency for three states 
#' minimalBSExample
#' 
#' # perform an anopa on this dataset
#' w <- anopa( {s;n} ~ state, minimalBSExample)
#' 
#' # We analyse the intensity by levels of pitch
#' #  e <- emProportions(w, ~ Intensity | Pitch)
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