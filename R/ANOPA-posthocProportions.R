######################################################################################
#' @title posthocProportions: post-hoc analysis of proportions.
#'
#' @md
#'
#' @description The function 'posthocProportions()' performs post-hoc analyses 
#'    of proportions after an omnibus analysis has been obtained with 'anopa()'
#'    according to the ANOPA framework. It is based on the tukey HSD test.
#'    See \insertCite{lc23b;textual}{ANOPA} for more.
#'
#' @usage posthocProportions(w, formula)
#'
#' @param w An ANOPA object obtained from `anopa()`;
#'
#' @param formula A formula which indicates what post-hocs to analyze. 
#'   only one simple effect formula at a time can be analyzed. The formula
#'   is given using a vertical bar, e.g., " ~ factorA | factorB " to obtain 
#'   the effect of Factor A within every level of the Factor B.
#'
#' @return a model fit of the simple effect. 
#' 
#' @details `posthocProportions()` computes expected marginal proportions and 
#'   analyzes the hypothesis of equal proportion. 
#'   The sum of the $F$s of the simple effects are equal to the 
#'   interaction and main effect $F$s, as this is an additive decomposition
#'   of the effects.
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' 
#' # -- FIRST EXAMPLE --
#' # This is a basic example using a two-factors design with the factors between 
#' # subjects. Ficticious data present the number of success according
#' # to Class (three levels) and Difficulty (two levels) for 6 possible cells
#' # and 72 observations in total (equal cell sizes of 12 participants in each group).
#' twoWayExample
#'
#' # As seen the data are provided in a compiled format (one line per group).
#' # Performs the omnibus analysis first (mandatory):
#' w <- anopa( {success;total} ~ Class * Difficulty, twoWayExample) 
#' summary(w)
#'
#' # The results shows an important interaction. You can visualize the data
#' # using anopaPlot:
#' anopaPlot(w)
#' # The interaction is overadditive, with a small differences between Difficulty
#' # levels in the first class, but important differences between Difficulty for 
#' # the last class.
#'
#' # Let's execute the post-hoc tests
#' e <- posthocProportions(w, ~ Difficulty | Class )
#' summary(e)
#'
#' 
#' # -- SECOND EXAMPLE --
#' # Example using the Arrington et al. (2002) data, a 3 x 4 x 2 design involving 
#' # Location (3 levels), Trophism (4 levels) and Diel (2 levels), all between subject.
#' ArringtonEtAl2002
#' 
#' # first, we perform the omnibus analysis (mandatory):
#' w <- anopa( {s;n} ~ Location * Trophism * Diel, ArringtonEtAl2002) 
#' summary(w)
#'
#' # There is a near-significant interaction of Trophism * Diel (if we consider
#' # the unadjusted p value, but you really should consider the adjusted p value...).
#' # If you generate the plot of the four factors, we don't see much:
#' # anopaPlot(w)
#' #... but with a plot specifically of the interaction helps:
#' anopaPlot(w, ~ Trophism * Diel )
#' # it seems that the most important difference is for omnivorous fishes
#' # (keep in mind that there were missing cells that were imputed but there does not
#' # exist to our knowledge agreed-upon common practices on how to impute proportions...
#' # Are you looking for a thesis topic?).
#'
#' # Let's analyse the simple effect of Tropism for every levels of Diel and Location
#' e <- posthocProportions(w, ~ Tropism | Diel )
#' summary(e)
#'
#'
#' # You can ask easier outputs with
#' summarize(w) # or summary(w) for the ANOPA table only
#' corrected(w)   # or uncorrected(w) for an abbreviated ANOPA table
#' explain(w)   # for a human-readable ouptut ((pending))
#' 
######################################################################################
#'
#' @export posthocProportions
#
######################################################################################


posthocProportions <- function(
    w       = NULL,
    formula = NULL
){
    ANOPAmessage("Not yet programmed...")
}