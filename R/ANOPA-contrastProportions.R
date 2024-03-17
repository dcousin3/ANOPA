######################################################################################
#' @title contrastProportion: analysis of contrasts between proportions using Anscombe transform.
#'
#' @md
#'
#' @description The function `contrastProportions()` performs contrasts analyses 
#'    on proportion data after an omnibus analysis has been obtained with `anopa()`
#'    according to the ANOPA framework. See \insertCite{lc23;textual}{ANOPA} for more.
#'      
#' @param w An ANOPA object obtained from `anopa()` or `emProportions()`;
#'
#' @param contrasts A list that gives the weights for the contrasts to analyze. 
#'   The contrasts within the list can be given names to distinguish them.
#'   The contrast weights must sum to zero and their cross-products must equal 0 
#'   as well.
#'
#' @return A table of significance of the different contrasts. 
#' 
#' @details `contrastProportions()` computes the _F_s for the contrasts,
#'   testing the hypothesis that it equals zero.
#'   The contrasts are each 1 degree of freedom, and the sum of the contrasts' 
#'   degrees of freedom totalize the effect-being-decomposed's degree of freedom.
#'
#' @references
#' \insertAllCited{}
#'
#' @examples
#' # Basic example using a one between-subject factor design with the data in compiled format. 
#' # Ficticious data present success or failure of observation classified according
#' # to the state of residency (three levels); 175 participants have been observed in total.
#'
#' # The cells are unequal:
#' minimalBSExample
#'
#' # First, perform the omnibus analysis :
#' w <- anopa( {s;n} ~ state, minimalBSExample) 
#' summary(w)
#'
#' # Compare the first two states jointly to the third, and
#' # compare the first to the second state:
#' cw <- contrastProportions( w, list(
#'          contrast1 = c(1,  1, -2)/2,
#'          contrast2 = c(1, -1,  0) )
#'       )
#' #summary(cw)
#'
#' # Example using the Arrington et al. (2002) data, a 3 x 4 x 2 design involving 
#' # Location (3 levels), Trophism (4 levels) and Diel (2 levels).
#' ArringtonEtAl2002
#'
#' # performs the omnibus analysis first (mandatory):
#' w <- anopa( {s;n} ~ Location * Trophism * Diel, ArringtonEtAl2002) 
#' corrected(w)
#'
#' # execute the simple effect of Trophism for every levels of Diel and Location
#' e <- emProportions(w, ~ Trophism | Diel * Location)
#' #summary(e)
#'
#' # For each of these sub-cells, contrast the four tropisms, first
#' # by comparing the first two levels to the third (contrast1), second
#' # by comparing the first to the second level (contrast2), and finally by
#' # by comparing the first three to the last (contrast3) :
#' #f <- contrastProportions( e, list(
#' #              contrast1 = c(1,  1, -2, 0)/2,
#' #              contrast2 = c(1, -1,  0, 0),
#' #              contrast3 = c(1,  1,  1, -3)/3
#' #           )
#' #      )
#' #summary(f)
#' 
#' 
######################################################################################
#'
#' @export contrastProportions
#
######################################################################################


contrastProportions <- function(
    w         = NULL,
    contrasts = NULL
){
    ##############################################################################
    ## STEP 1: VALIDATION OF INPUT
    ##############################################################################
    # 1.1: Is w of the right type 
    if (!("ANOPAobject" %in% class(w))) 
        stop("ANOPA::error(31): The argument w is not an ANOPA object. Exiting...")

    # 1.2: Are the contrasts of the right length
    if (min(unlist(lapply(contrasts, length))) != max(unlist(lapply(contrasts, length))))
        stop("ANOPA::error(32): The constrasts have differing lengths. Exiting...")
    relevantlevels = if (w$type == "ANOPAomnibus") {
        prod( c(w$BSfactNlevels, w$WSfactNlevels) )
    } else {
        stop("ANOPA::oups(1): This part not yet done. Exiting...")
    } 
    if (!(all(unlist(lapply(contrasts, length)) == relevantlevels )))
        stop("ANOPA::error(33): The contrats lengths does not match the number of levels. Exiting...")

    # 1.3a: Are the contrasts legitimate (a) sum to zero; 
    if (!(all(round(unlist(lapply(contrasts,sum)),8)==0))) 
        stop("ANOPA::error(34): Some of the constrats do not sum to 0. Exiting...")
    # 1.3b: Are the contrasts legitimate (b) cross-product sum to zero
    sums = c()
    for (i in names(contrasts)) {for (j in setdiff(names(contrasts), i)) {
        sums <-append(sums, round(sum(contrasts[[i]]*contrasts[[j]]) ),8) } }
    if (!(all(sums == 0)))
        stop("ANOPA::error(35): Some of the cross-products of contrasts do not totalize 0. Exiting...")
    # 1.3c: Are the contrasts legitimate (c) all oppositions sum to 1
    if (!(all(round(unlist(lapply(contrasts, \(x) sum(abs(x)))),8)==2)))
        stop("ANOPA::error(36): Some of the contrasts' weigth does not equal 1 (for the positive weights) or -1 (for the negative weights). Exiting...")

    # 1.4: is there an acceptable number of contrasts
    if (length(contrasts) > relevantlevels-1)
        stop("ANOPA::error(37): There are more contrasts defined than degrees of freedom. Exiting...")
    
    
    ##############################################################################
    ## STEP 2: Run the analysis 
    ##############################################################################
    # 2.1: identify the factors
ANOPAmessage("Not yet programmed...")
return(-99)

    # 2.2: perform the analysis based on ???
    analysis <- if(w$type == "ANOPAomnibus") {
        ## Contrasts on the full design
        cst1way(w$compiledData, w$freqColumn, contrasts)
    } else { # "ANOPAsimpleeffects"
        ## Contrasts on sub-data based on nesting factor(s)
        cstMway(w$compiledData, w$freqColumn, w$subFactors, w$nestedFactors, contrasts)
    }

    ##############################################################################
    # STEP 3: Return the object
    ##############################################################################
    # 3.1:  preserve everything in an object of class ANOPAobject
    res <- list(
        type          = "ANOPAcontrasts",
        formula       = as.formula(w$formula),
        compiledData  = w$compiledData,
        freqColumn    = w$freqColumn,
        factColumns   = w$factColumns,
        nlevels       = w$nlevels,
        clevels       = w$clevels,
        # new information added
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
# Coding contrast effects                #
# is postponed until interest is shown.  #
##########################################


# ################################################################
# # Lets run the orthogonal contrasts                            #
# ################################################################

# there is only two possible cases:
# a) the contrasts are on the full data             ==> cst1way
# b) the contrasts are nested with some factor(s)   ==> cstMway
cst1way <- function(cData, ss, contrasts) {
    # extract the proportions and the total 

    # get the F statistics for each contrast

    # compute the correction factor cf
    
    # compute the p values on the corrected G as usual

    # This is it! let's put the results in a table

}

cstMway <- function(cData, ss, subfact, nestfact, contrasts) {
    ## run cst1way on every levels of the nesting factor(s)

    # in case other factors are not named, collapse over these

    ANOPAmessage("Not yet programmed...")
}

# ################################################################
# # Sub-functions to get observed and expected  proportions      #
# ################################################################

# the null hypothesis for the conditions implicated in the contrast
# ns is a vector
contrastNull <- function(contrast, ns) { 

}


# the contrast hypothesis for the conditions implicated in the contrast
# ns is a vector
contrastObsd <- function(contrast, ns) {

}

