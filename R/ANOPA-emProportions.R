######################################################################################
#' @title emProportions: simple effect analysis of proportions.
#'
#' @md
#'
#' @description The function 'emProportions()' performs a _simple effect_ analyses 
#'    of proportions after an omnibus analysis has been obtained with 'anopa()'
#'    according to the ANOPA framework. Alternatively, it is also called an
#'    _expected marginal_ analysis of proportions. See \insertCite{lc23b;textual}{ANOPA} for more.
#'
#' @usage emProportions(w, formula)
#'
#' @param w An ANOPA object obtained from `anopa()`;
#'
#' @param formula A formula which indicates what simple effect to analyze. 
#'   Only one simple effect formula at a time can be analyzed. The formula
#'   is given using a vertical bar, e.g., " ~ factorA | factorB " to obtain 
#'   the effect of Factor A within every level of the Factor B.
#'
#' @return An ANOPA table of the various simple main effets and if relevant,
#'    of the simple interaction effets. 
#' 
#' @details `emProportions()` computes expected marginal proportions and 
#'   analyzes the hypothesis of equal proportion. 
#'   The sum of the _F_s of the simple effects are equal to the 
#'   interaction and main effect _F_s, as this is an additive decomposition
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
#' w <- anopa( {success;total} ~ Difficulty * Class, twoWayExample) 
#' summary(w)
#'
#' # The results shows an important interaction. You can visualize the data
#' # using anopaPlot:
#' anopaPlot(w)
#' # The interaction is overadditive, with a small differences between Difficulty
#' # levels in the first class, but important differences between Difficulty for 
#' # the last class.
#'
#' # Let's execute the simple effect of Difficulty for every levels of Class
#' e <- emProportions(w, ~ Difficulty | Class )
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
#' anopaPlot(w)
#'
#' #... but a plot specifically of the interaction helps:
#' anopaPlot(w, ~ Trophism * Diel )
#' # it seems that the most important difference is for omnivorous fishes
#' # (keep in mind that there were missing cells that were imputed but there does not
#' # exist to our knowledge agreed-upon common practices on how to impute proportions...
#' # Are you looking for a thesis topic?).
#'
#' # Let's analyse the simple effect of Trophism for every levels of Diel and Location
#' e <- emProportions(w, ~ Trophism | Diel )
#' summary(e)
#'
#' 
#' # You can ask easier outputs with
#' corrected(w) # or summary(w) for the ANOPA table only
#' explain(w)   # human-readable ouptut ((pending))
#' 
######################################################################################
#'
#' @export emProportions
#
######################################################################################


emProportions <- function(
    w       = NULL,
    formula = NULL
){
    ##############################################################################
    ## STEP 1: VALIDATION OF INPUT
    ##############################################################################
    # 1.1 Is w of the right type and having more than 1 factor
    if (!("ANOPAobject" %in% class(w))) 
        stop("ANOPA::error(21): The argument w is not an ANOPA object. Exiting...")
    if (length( c(w$BSfactColumns, w$WSfactColumns) )==1)
        stop("ANOPA::error(22): There must be at least two factors in the design. Exiting...")

    # 1.2 If formula indeed a formula
    if (!(is.formula(formula))) 
        stop("ANOPA::error(23): The formula argument is not a legitimate formula. Exiting...")

    # 1.3 Is the rhs having a | sign, and only one
    if (!(has.nested.terms(formula)))
        stop("ANOPA::error(24): The rhs of formula must have a variable nested within another variable. Exiting... ")
    if (length(tmp <- sub.formulas(formula, "|"))!=1) 
        stop("ANOPA::error(25): The rhs of formula must contain a single nested equation. Exiting...")
    if (tmp[[1]][[2]]==tmp[[1]][[3]])
        stop("ANOPA::error(26): The variables on either side of | must differ. Exiting...")

    # 1.4 If the dependent variable is named (optional), is it the correct variable
    if (length(formula)==3) {
         if (formula[[2]] != w$formula[[2]])
             stop("ANOPA::error(27): The lhs of formula is not the correct proportion variable. Exiting...")
    }

    # 1.5 Are the factors named in formula present in w
    vars <- all.vars(formula) # extract variables in cbind and with | alike
    if (!(all(vars %in% names(w$compData)))) 
        stop("ANOPA::error(28): variables in `formula` are not all in the data. Exiting...")


    ##############################################################################
    ## STEP 2: Run the analysis based on the number of factors
    ##############################################################################
    # 2.1: identify the factors
    subfactors <- all.vars(tmp[[1]][[2]])
    nestfactor <- all.vars(tmp[[1]][[3]])

ANOPAmessage("Not yet programmed...")
return(-99)

    # 2.2: perform the analysis based on the number of factors
    analysis <- switch( length(subfactors),
        emf1way(w, subfactors, nestfactor),
        emf2way(w, subfactors, nestfactor),
        emf3way(w, subfactors, nestfactor),
    )


    ##############################################################################
    # STEP 3: Return the object
    ##############################################################################
    # 3.1:  preserve everything in an object of class ANOPAobject
    res <- list(
        type          = "ANOPAsimpleeffects",
        formula       = as.formula(w$formula),
        compiledData  = w$compiledData,
        freqColumn    = w$freqColumn,
        factColumns   = w$factColumns,
        nlevels       = w$nlevels,
        clevels       = w$clevels,
        # new information added
        formulasimple = as.formula(formula),
        nestedFactors = nestfactor,
        subFactors    = subfactors,
        nestedLevels  = dim(analysis)[1],
        results       = analysis
    )
    class(res) <- c("ANOPAobject", class(res) )
    return( res )

}


##########################################
#                                        #
#    ██╗  ██╗███████╗██████╗ ███████╗    #
#    ██║  ██║██╔════╝██╔══██╗██╔════╝    #
#    ███████║█████╗  ██████╔╝█████╗      #
#    ██╔══██║██╔══╝  ██╔══██╗██╔══╝      #
#    ██║  ██║███████╗██║  ██║███████╗    #
#    ╚═╝  ╚═╝╚══════╝╚═╝  ╚═╝╚══════╝    #
#                                        #
##########################################
# Coding marginal-proportion effects     #
# is postponed until interest is shown.  #
##########################################


emf1way <- function(w, subfact, nestfact){
    ## Herein, the sole factor analyzed is called factor A, which is nested within factor B,
    ## that is, the effect of A within b1, the effect of A within b2, ... A within bj.
    ## There can only be a single nesting variable (e.g., B)
    ## We can get here if (a) there are 2 factors, (b) there are 3 factors, (c) there are 4 factors

    # The factors position in the factor list and their number of levels

    #    posB <- (1:length(w$factColumns))[w$factColumns==nestfact]
    #    B   <- w$nlevels[posB]

    # the total n and the vector marginals

    # First, we compute the expected proportions separately for each levels of factor B


    # Second, we get the G statistics for each level

    # Third, the correction factor (Williams, 1976) for each 

    # Finally, getting the p-values for each corrected effect

    # This is it! let's put the results in a table

}


emf2way <- function(w, subfacts, nestfact){
    ## Herein, the factors decomposed are called factors A*B,
    ## that is, the effects of (A, B, A*B) within c1, the effects of (A, B, A*B) within c2, etc.
    ## We can get here if (a) there are 3 factors, (b) there are 4 factors.

    # The factor positions in the factor list and their number of levels

    # posC <- (1:length(w$factColumns))[w$factColumns==nestfact]
    # C   <- w$nlevels[posC]


    # the total n and the matrix marginals

    # First, we compute the expected proportions e separately for each levels of factor C

    # Second, we get the G statistics for each level

    # Third, the correction factor (Williams, 1976) for each 
    
    # Finally, getting the p-values for each corrected effect

    # This is it! let's put the results in a table

}


emf3way <- function(w, subfacts, nestfact){
    ## Herein, the factor decomposed is called factor D,
    ## that is, the effect of (A, B, C, AB, AC, BC, A*B*C) within d1, 
    ## the effect of (A, B, C, AB, AC, BC, A*B*C) within d2, etc.
    ## There is only one way to get here: a 4 factor design is decomposed.
    
    "If you need this functionality, please contact the author...\n"
}


