###################################################################################
#' @name conversion
#'
#' @title Converting between formats
#'
#' @aliases toWide toLong toCompiled
#'
#' @md
#'
#' @description The functions 'toWide()', 'toLong()', and 'toCompiled()' 
#'      converts the data into various formats.
#' 
#' @usage toWide(w)
#' @usage toLong(w)
#' @usage toCompiled(w)
#' 
#' @param w An instance of an ANOPA object.
#'
#' @return A data frame in the requested format.
#'
#' @details The proportions of success of a set of _n_ participants can be
#'   given using many formats. In what follows, _n_ is the number of
#'   participants, _p_ is the number of between-subject factor(s),
#'   $q$ is the number of repeated-measure factor(s).
#'    * One basic format, called `wide`, has one line per
#'      participants, with a 1 if a "success" is observed
#'      or a 0 if no success is observed. What a success is 
#'      is entirely arbitrary. The proportion of success is then
#'      the number of 1s divided by the number of participants in each group.
#'      The data frame has $n$ lines and $p+q$ columns.
#'    * A second format, called `long`, has, on a line, the
#'      factor name(s) and 1s or 0s to indicate success or not.
#'      The data fame has $n x q$ lines and 
#'      4 columns (a Id column to identify the participant; $p$ columns
#'      to identify the groups, one column to identify which within-subject
#'      measure is given and finally, a 1 or 0 for the score of that measurement. 
#'    * A third format, called `compiled`, is to have a list of all
#'      the between-subject factors and the number of 
#'      success and the total number of participants. 
#'      This format is more compact  as if there are 6 groups,
#'      the data are all contained in six lines (one line per group). 
#'      This format however is only valid for between-subject design as
#'      we cannot infer the correlation between successes/failure.
#'
#' @details See the vignette [DataFormatsForProportions](../articles/B-DataFormatsForProportions.html)
#'    for more.
#'
#' @examples
#' 
#' # The minimalBSExample contains $n$ of 175 participants categorized according
#' # to one factor $f = 1$, namely `State of residency` (with three levels) 
#' # for 3 possible cells.
#' minimalBSExample
#'
#' # Lets incorporate the data in an ANOPA data structure
#' w <- anopa( {s;n} ~ state, minimalBSExample )
#'
#' # The data presented using various formats looks like
#' toWide(w)
#' # ... has 175 lines, one per participants ($n$) and 2 columns (state, success or failure)
#'
#' toLong(w)
#' # ... has 175 lines ($n x f$) and 4 columns (participant's `Id`, state name, measure name, 
#' # and success or failure)
#'
#' toCompiled(w)
#' # ... has 3 lines and 3 columns ($f$ + 2: number of succes and number of participants).
#'
#'
#' # This second example is from a mixed-design. It indicates the 
#' # state of a machine, grouped in three categories (the sole between-subject
#' # factor) and at four different moments. 
#' # The four measurements times are before treatment, post-treatment, 
#' # 1 week later, and finally, 5 weeks later.
#' minimalMxExample
#'
#' # Lets incorporate the data in an ANOPA data structure
#' w <- anopa( cbind(bpre,bpost,b1week,b5week) ~ Status, 
#'             minimalMxExample,
#'             WSFactors = "Moment(4)" )
#'
#' # -- Wide format --
#' # Wide format is actually the format of minimalMxExample
#' # (27 lines with 8 subjects in the first group and 9 in the second)
#' toWide(w)
#'
#' # -- Long format --
#' # (27 times 4 lines = 108 lines, 4 columns, that is Id, group, measurement, success or failure)
#' toLong(w)
#'
#' # -- Compiled format --
#' # (three lines as there are three groups, 7 columns, that is, 
#' # the group, the 4 measurements, the number of particpants, and the
#' # correlation between measurements for each group measured by unitary alphas)
#' toCompiled(w)
#'
#'
###################################################################################
#'
#' @importFrom stats  reshape
#' @export toWide
#' @export toLong
#' @export toCompiled
#
###################################################################################


toWide <- function( w = NULL ) {
    # is w of class ANOPA.object?
    if (!("ANOPAobject" %in% class(w) ))
        stop("ANOPA::error(101): argument is not an ANOPA generated object. Exiting...")

    return( w$wideData ) # nothing to do, the data are internally stored in wide format
}

toLong <- function( w = NULL ) {
    # is w of class ANOPA.object?
    if (!("ANOPAobject" %in% class(w) ))
        stop("ANOPA::error(102): argument is not an ANOPA generated object. Exiting...")

    return( wtol(w$wideData, w$DVvariables) )
}

toCompiled <- function( w = NULL ) {
    # is w of class ANOPA.object?
    if (!("ANOPAobject" %in% class(w) ))
        stop("ANOPA::error(104): argument is not an ANOPA generated object. Exiting...")

    return( wtoc(w$wideData, w$DVvariables, "Count" ) )
}


###################################################################################
### UTILITES
###################################################################################

# relative complement of x within universal set U
complement <- function(U, x) {U[is.na(pmatch(U,x))]}

colSums = function (x) {
    # the equivalent of colMeans for sum
    if (is.vector(x))          sum(x)
    else if (is.matrix(x))     apply(x, 2, sum)
    else if (is.data.frame(x)) apply(x, 2, sum)
    else "what the fuck??"
}


###################################################################################
### CONVERSIONS from wide to ...
###################################################################################

# wide => compiled:        DONE
#    ss: all the column names of repeated measures
#    n:  a novel name to contain the total number of observation per cell
#    for WS design, it adds the unitary Alpha to the groups.
wtoc <- function(d, ss, n) {
    factors  <- names(d)[!(names(d) %in% ss )]
    if (length(factors)==0) {
        # add a dummy BS factor
        d[["dummyBSfactor"]] <- 1
        factors = "dummyBSfactor"
    }

    res <- plyr::ddply(d, factors, function(x) colSums(x[ss]))
    res[[n]] <- plyr::ddply(d, factors, function(x) dim(x[ss])[1])$V1
    names(res)[names(res)=="V1"] <- n

    if (length(ss)>1) {
        # adds correlation with unitaryAlpha
        res[["uAlpha"]] <- plyr::ddply(d, factors, 
            function(x) {unitaryAlpha(as.matrix(x[ss]))}
        )$V1
        names(res)[names(res)=="V1"]="uAlpha"
    }

    # remove the dummy factor if needed
    res[["dummyBSfactor"]] <- NULL

    return(res)
}

# wide => long:         DONE
#    ss: all the column names of repeated measures
#    the columns Id and Variables will be added
wtol <- function(d, ss ) {
    res    <- reshape(d, idvar = "Id", 
                varying = ss, v.names = "Value", 
                times = ss, timevar = "Variable",
                direction = "long"
              )
    # move Id as first column
    Ids    <- res$Id
    res$Id <- NULL
    res    <- cbind(Id = Ids, res)
    # sort by Ids
    res    <- res[order(res$Id),]
    return(res)
}



###################################################################################
### CONVERSIONS from compiled to ...
###################################################################################

# compiled => wide:     DONE     possible iif no repeated measures
#     ss: the repeated measures (should be of length 1)
#   n:  the total number of observations per cell
ctow <- function(d, ss, n) {
    if (length(ss)>1) stop("ANOPA::error(99): internal error. Exiting...")
    y  <- d[,!(names(d)%in%c(ss,n)), drop=FALSE]
    res <- as.data.frame(lapply(y, rep, d[[n]]))
    for (i in ss) {
        res[[i]] <- c(unlist(mapply( 
                        \(s,n) {c(rep(1,round(s)),rep(0,round(n)-round(s)))}, 
                        d[[i]], d[[n]], SIMPLIFY=FALSE
                    )))
    }
    return(res)
}

# compiled => long:        DONE
ctol <- function( d, ss, n ) {
  tmp <- ctow(d, ss, n)
  res <- wtol(tmp, ss )
  return(res)
}


###################################################################################
### CONVERSIONS from long to ...
###################################################################################

# long => wide            DONE
#    idcol: name of participant identifier
#    condcol: the column containing the measure's name
#    scol:    column containing the success
ltow <- function(d, idcol, condcol, scol ){
    res <- reshape(d, idvar = idcol, 
        timevar = condcol, v.names = scol, 
        direction = "wide")
    res$Id     <- NULL
    names(res) <- gsub("Value.","", names(res) )
    return(res)

}

# long => compiled        DONE
ltoc <- function(d, idcol, condcol, scol, n) {
    t1  <- ltow(d, idcol, condcol, scol )
    ss  <- unique(d[[condcol]])
    res <- wtoc(t1, ss, n)
    return(res)
}



###################################################################################
### DONE...
###################################################################################
