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
#'   the effect of Factor A within every level of the Factor B. The dependent
#'   variable(s) (lhs of equation) are not needed as they are memorized 
#'   in the w object.
#'
#' @return An ANOPA table of the various simple main effects and if relevant,
#'    of the simple interaction effects. 
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
#' e <- emProportions(w, ~ Trophism * Location | Diel )
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
    if (!(all(vars %in% c(names(w$compData), w$WSfactColumns) ))) 
        stop("ANOPA::error(28): variables in `formula` are not all in the data. Exiting...")


    ##############################################################################
    ## STEP 2: Run the analysis based on the number of factors
    ##############################################################################
    # 2.1: identify the factors
    subfactors <- all.vars(tmp[[1]][[2]])
    nestfactor <- all.vars(tmp[[1]][[3]])

#print("about to dispatch")
#print(c(subfactors,"within",nestfactor))
#print( paste(length(w$BSfactColumns) + length(w$WSfactColumns), length(subfactors), sep="")) 

    # 2.2: perform the analysis based on the number of factors
    simpleeffects <- switch( 
        paste(length(w$BSfactColumns) + length(w$WSfactColumns), length(subfactors), sep=""),
        "21" = emf2fact1way(w, subfactors, nestfactor),
        "31" = emf3fact1way(w, subfactors, nestfactor),
        "32" = emf3fact2way(w, subfactors, nestfactor),
        "41" = {ANOPAmessage("Not yet programmed..."); return(-99)},
        "42" = {ANOPAmessage("Not yet programmed..."); return(-99)},
        "43" = {ANOPAmessage("Not yet programmed..."); return(-99)},
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
        nestedLevels  = dim(simpleeffects)[1], ##?
        simpleeffects = simpleeffects
    )
    class(res) <- c("ANOPAobject", class(res) )
    return( res )

}


## Granted, the coding of the following is very bad, reusing mutatis mutandis the same code
## on and on... If you ever want to contribute, do not hesitate!


emf2fact1way <- function(w, subfact, nestfact){
    ## Herein, the sole factor analyzed is called factor A, which is nested within factor B,
    ## that is, the effect of A within b1, the effect of A within b2, ... A within bj.
    ## There can only be a single nesting variable (e.g., B)
    #print("Begin emf2fact1way")

    if (length(w$WSfactColumns) == 2) { # both within-subject factors
        s      <- w$compData[w$DVvariables]
        n      <- rep(w$compData[[w$NVariable]], length(w$DVvariables))
        corlt  <- w$compData[[w$AlphaVariable]]
        facts  <- w$WSfactColumns
        f1levl <- w$WSfactNlevels[2]
    } else if (length(w$WSfactColumns) == 1) { # mixed, within-between, design
        s      <- unlist(w$compData[w$DVvariables]) # i.e., flatten
        n      <- rep(w$compData[[w$NVariable]], length(w$DVvariables))
        corlt  <- rep(w$compData[[w$AlphaVariable]], w$WSfactNlevels[1])
        facts <- c(w$WSfactColumns, w$BSfactColumns)
        f1levl <- w$WSfactNlevels
    } else { # both between-subject factors
        s      <- w$compData[[w$DVvariables]]
        n      <- w$compData[[w$NVariable]]
        corlt  <- 0
        facts  <- w$BSfactColumns
        f1levl <- length(unique(w$compData[[w$BSfactColumns[1]]]))
    }
    pos <- which( facts %in% nestfact)
#print(pos)

    # apply the transform to all the pairs (s, n)
    As <- mapply(A,    s, n)
    Vs <- mapply(varA, s, n)

    # fold the scores into a matrix
    Af  <- matrix( As, ncol = f1levl) 
    Vf  <- matrix( Vs, ncol = f1levl)
    ns  <- matrix( n,  ncol = f1levl)
    if (pos == 2) {Af = t(Af); Vf = t(Vf); ns = t(ns) }

    ### GET THE NUMBER OF LEVELS to cycle through...
    res <- data.frame()
    p   <- dim(Af)[1]
    q   <- dim(Af)[2]
#print(c (p,q))

    for (i in 1:q) {
        Ap   <- Af[,i]     # ith column of the scores
# print(Ap)
        np   <- ns[,i]     # sample size of the ith column of scores
# print(np)
        msp  <- var( Ap ) # MSa
# print(msp)
        #msE is unchanged
        msE  <- if ((!is.null(w$WSfactColumns))&&(!is.null(w$BSfactColumns))&&(!(nestfact %in% w$WSfactColumns))) {
            # mixed design: before last line contains the within mse
#print("mixed, withinfact")
            w$omnibus[dim(w$omnibus)[1]-1,1] 
        } else {
            w$omnibus[dim(w$omnibus)[1],1] 
        }
#print(msE)

        # get the F ratio and the chi-square statistics
        Fp   <- msp/msE
        # get the p values.
        pvalp  <- 1 - pchisq( (p-1)*Fp,  df = (p-1) )
        cf     <- 1+ (p^2-1)/(6 * hmean(ns) * (p-1) )
        Fpcorr <- Fp / cf
        pvalpcorr <- 1 - pchisq( (p-1)*Fpcorr, df = (p-1) )
        line   <- t(data.frame(c(msp, (p-1), Fp, pvalp, cf, Fpcorr, pvalpcorr)))
#print(w$WSfactColumns)
#print(nestfact)
        levellbl <- if (nestfact %in% w$WSfactColumns) {
                w$WSfactDesign[w$WSfactDesign[[nestfact]]==i,]$Variable
            } else {
                unique(w$compData[[nestfact]])[[i]]
            }
        rownames(line) <- paste(subfact, "|", nestfact, "=", levellbl )
        res    <- rbind(res, line)
    }
    names(res) <- c("MS","df","F","pvalue","correction","Fcorr","pvalcorr")
# print(res)
    return(res)
}


emf3fact2way <- function(w, subfacts, nestfact){
    ## Herein, the factors decomposed are called factors A*B,
    ## that is, the effects of (A, B, A*B) within c1, the effects of (A, B, A*B) within c2, etc.
    ## NOTE that it always performs a full-factorial simple effect irrespective of what you named
    # print("Begin emf3fact2way")

    if (length(w$WSfactColumns) == 3) { # entirely within-subject factors
        s      <- w$compData[w$DVvariables]
        n      <- rep(w$compData[[w$NVariable]], length(w$DVvariables))
        corlt  <- w$compData[[acol]]
        facts  <- w$WSfactColumns
        lvlp <- 1:(w$WSfactNlevels[1])        # first is always within
        lvlq <- 1:(w$WSfactNlevels[2])
        lvlr <- 1:(w$WSfactNlevels[3])
        p <- length(lvlp)
        q <- length(lvlq)
        r <- length(lvlr)

    } else     if (length(w$WSfactColumns) == 2) { # mixed, 2 within-subject factors
        s      <- w$compData[w$DVvariables]
        n      <- rep(w$compData[[w$NVariable]], length(w$DVvariables))
        corlt  <- w$compData[[w$AlphaVariable]]
        facts  <- c(w$WSfactColumns, w$BSfactColumns)
        lvlp <- 1:(w$WSfactNlevels[1])
        lvlq <- 1:(w$WSfactNlevels[2])
        lvlr <- unique(w$compData[[w$BSfactColumns[1]]])
        p <- length(lvlp)
        q <- length(lvlq)
        r <- length(lvlr)
        corlt <- array(unlist(lapply( corlt, rep, p*q)), dim = c(r,q,p))

    } else if (length(w$WSfactColumns) == 1) { # mixed, 1 within-subject factor
        s      <- c(t(w$compData[w$DVvariables])) # i.e., flatten
        n      <- rep(w$compData[[w$NVariable]], length(w$DVvariables))
        corlt  <- rep(w$compData[[w$AlphaVariable]], w$WSfactNlevels[1])
        facts <- c(w$WSfactColumns, w$BSfactColumns)
        lvlp <- 1:(w$WSfactNlevels[1])
        lvlq <- unique(w$compData[[w$BSfactColumns[1]]])
        lvlr <- unique(w$compData[[w$BSfactColumns[2]]])
        p <- length(lvlp)
        q <- length(lvlq)
        r <- length(lvlr)
        corlt <- array(unlist(lapply( corlt, rep, p)), dim = c(r,q,p))

    } else { # entirely between-subject factors
        s      <- w$compData[[w$DVvariables]]
        n      <- w$compData[[w$NVariable]]
        corlt  <- 0
        facts  <- w$BSfactColumns
        lvlp <- unique(w$compData[[w$BSfactColumns[1]]])
        lvlq <- unique(w$compData[[w$BSfactColumns[2]]])
        lvlr <- unique(w$compData[[w$BSfactColumns[3]]])
        p <- length(lvlp)
        q <- length(lvlq)
        r <- length(lvlr)
    }
    pos <- which( facts %in% nestfact)
#print(pos)
#print( c(p,q,r) )
    # reorder the factor names
    facts2 <- c(facts[-pos],facts[pos])
#print(facts2)

    # apply the transform to all the pairs (s, n)
    As <- mapply(A,    s, n)
    Vs <- mapply(varA, s, n)

    # fold the scores into a three-dimensional array
    # for between-subject design: https://stackoverflow.com/a/52435862/5181513
    namesOfDim <- list( lvlr, lvlq, lvlp )

    Af <- array(As, dim = c(r, q, p), dimnames = namesOfDim)
    Vf <- array(Vs, dim = c(r, q, p), dimnames = namesOfDim) 
    ns <- array(n,  dim = c(r, q, p), dimnames = namesOfDim) 

    # based on which is the factor to decompose from, 
    # "expose" in the tensor the two dimensions to analyze
    if (pos == 1) displ <- c(2,1,3)
    if (pos == 2) displ <- c(3,1,2)
    if (pos == 3) displ <- c(3,2,1)

    Af <- aperm( Af, displ )
    Vf <- aperm( Vf, displ )
    ns <- aperm( ns, displ )

    ### GET THE NUMBER OF LEVELS to cycle through...
    res <- data.frame()
    p   <- dim(Af)[1]
    q   <- dim(Af)[2]
    r   <- dim(Af)[3]
#print( c(p,q,r) ) #still ok?

    for (k in 1:r) {
        Apq  <- Af[,,k]         # ith matrix of the scores
        Aq   <- colMeans(Apq)   # marginal mean along Q factor
        Ap   <- rowMeans(Apq)   # marginal mean along P factor

        npq  <- ns[,,k]         # sample sizes of the ith matrix of scores
        nq   <- colSums(npq)    # sample size across all Q levels
        np   <- colSums(t(npq)) # sample size across all P levels

        mspq <- 1/((p-1)*(q-1)) * sum((Apq - outer(Ap, Aq, `+`)  + mean(Apq) )^2 )
        msp  <- q * var( Ap )
        msq  <- p * var( Aq )

        #msE is unchanged
        msEinter <- w$omnibus[dim(w$omnibus)[1],1]
        msEintra <- if (!is.null(w$WSfactColumns)) {w$omnibus[dim(w$omnibus)[1]-1,1] } else {1}
#print(c(msEinter,msEintra))        

        msEp  <- if (facts2[1] %in% w$WSfactColumns) msEintra else msEinter
        msEq  <- if (facts2[2] %in% w$WSfactColumns) msEintra else msEinter
        msEpq <- if ((facts2[1] %in% w$WSfactColumns)|(facts2[2] %in% w$WSfactColumns)) msEintra else msEinter
#print(c(msEp,msEq,msEpq))

        # get the F ratio and the chi-square statistics
        Fp  <- msp/msEp
        Fq  <- msq/msEq
        Fpq <- mspq/msEpq

        # compute p value from the latter (the former has infinite df on denominator)
        pvalp  <- 1 - pchisq( (p-1) * Fp,  df = (p-1) )
        pvalq  <- 1 - pchisq( (q-1) * Fq,  df = (q-1) )
        pvalpq <- 1 - pchisq( (p-1) * (q-1) * Fpq, df = (p-1) * (q-1)  )

        # If you want to apply the corrections
        cfp       <- 1 + (p^2-1)/(6 * hmean(np) * (p-1) )
        cfq       <- 1 + (q^2-1)/(6 * hmean(nq) * (q-1) )
        cfpq      <- 1 + ((p * q)^2-1)/(6 * hmean(npq) * (p-1)*(q-1) )

        pvalpadj  <- 1 - pchisq( (p-1) * Fp/cfp,   df = (p-1) )
        pvalqadj  <- 1 - pchisq( (q-1) * Fq/cfq,   df = (q-1) )
        pvalpqadj <- 1 - pchisq( (p-1) * (q-1) * Fpq/cfpq, df = (p-1)*(q-1) )

        # keep the results    
        threelines  <- data.frame(
            MS         = c(msp,     msq,    mspq ), 
            df         = c(p-1,     q-1,    (p-1)*(q-1) ), 
            F          = c(Fp,      Fq,     Fpq ), 
            pvalue     = c(pvalp,   pvalq,  pvalpq ),
            correction = c(cfp,     cfq,    cfpq ),
            Fcorr      = c(Fp/cfp,  Fq/cfq, Fpq/cfpq ),
            pvalcorr   = c(pvalpadj, pvalqadj, pvalpqadj )
        )

        factlbl  <- c(facts2[1:2], paste(facts2[1:2], collapse=":") )
        levellbl <- if (nestfact %in% w$WSfactColumns) {
                w$WSfactDesign[w$WSfactDesign[[nestfact]]==k,]$Variable
            } else {
                unique(w$compData[[nestfact]])[[k]]
            }
        rownames(threelines) <- paste(factlbl, "|", nestfact, "=", levellbl )

        res    <- rbind(res, threelines)
    }
    names(res) <- c("MS","df","F","pvalue","correction","Fcorr","pvalcorr")
    return(res)

}


emf3fact1way <- function(w, subfacts, nestfact){
    ## Herein, the factor decomposed is called factor A,
    ## that is, the effect of (A) within b1*c1, the effect of (A) within b1*c2, etc.
    #print("Begin emf3fact1way")

    "If you need this functionality, please contact the author...\n"

}


