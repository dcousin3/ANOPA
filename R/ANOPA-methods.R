##############################################################################
# DEFINITIONS of the three display methods: 
#   print: The usual, show everything
#   summarize (or summary): provides a human-readable output of the analysis table
#   explain : provides all sorts of information regarding the computations
##############################################################################
#
# As a reminder, the ANOPAobject has all these keys
#   type          : "ANOPAomnibus" or "ANOPAsimpleeffects"
#   formula       : The formula given to anopa
#   BSfactColumns : Between factor column names
#   BSfactNlevels :     number of levels for each between factor,
#   WSfactColumns : Within factor column names,
#   WSfactDesign  :    how the columns are assigned to a level of the within factors,
#   WSfactNlevels :    number of levels for each within factor,
#   DVvariables   : The dependent variable(s),
#   wideData      : Wide data are needed for plots to compute correlations
#   compData      : Compiled data are used for analyzes
#   omnibus       : Results of the omnibus analysis
#
##############################################################################
#'
#' @title explain 
#' @name explain
#'
#' @md
#'
#' @description
#' 'explain()' provides a human-readable, exhaustive, description of
#' the results. It also provides references to the key results.
#' 
#' @usage explain(object, ...)
#' 
#' @param object      an object to explain
#' @param ...         ignored
#' @return a human-readable output with details of computations.
#' 
#' @export
explain <- function(object, ...) {  UseMethod("explain") }

#' @export 
explain.default <- function(object, ...) { print(object) } 


#' @title summarize 
#' @name summarize
#'
#' @md
#'
#' @description 'summarize()' provides the statistics table an ANOPAobject. 
#' It is synonym of 'summary()' (but as actions are verbs, I used a verb).
#' 
#' @param object   an object to summarize
#' @param ...      ignored
#' @return an ANOPA table as per articles.
#' 
#' @export 
summarize <- function(object, ...) {  UseMethod("summarize") }

#' @method summarize default 
#' @export 
summarize.default <- function(object, ...) { print(object$results, ...) }

#' @export
summary.ANOPAobject <- function(object, ...) {
    summarize(object, ...)
}

##############################################################################
##
##  Implementation of the three methods
##
##############################################################################

#' @method print ANOPAobject
#' @export 
print.ANOPAobject <- function(x, ...) {
    ANOPAmessage("ANOPA completed! My first advise is to use anopaPlot() now. \nUse summary() or summarize() to obtain the ANOPA table.")
    y <- unclass(x)
    class(y) <- "list"
    print(y, digits = 5)
    return(invisible(x))
}

#' @method print ANOPAtable
#' @export 
print.ANOPAtable <- function(x, ...) {
    r <- x
    class(r) <- "data.frame"
    print( as.matrix(round(r, 
            getOption("ANOPA.digits")+2), 
            digits = getOption("ANOPA.digits"), scientific = FALSE
          ), na.print="", quote = FALSE )
}

#' @method summarize ANOPAobject 
#' @export 
summarize.ANOPAobject <-  function(object, ...) {
    if (object$type == "ANOPAsimpleeffects") 
        u <- object$simpleeffects
    else if (object$type == "ANOPAomnibus")
        u <- object$omnibus

    class(u) <- c("ANOPAtable", class(u))
    return(u)
}

#' @method explain ANOPAobject 
#' @export
explain.ANOPAobject <- function(object, ...) {
    print("method explain not yet done...")

    return(invisible(object))
}




#' @title corrected 
#' @name corrected
#'
#' @md
#'
#' @description
#' 'corrected()' provides an ANOPA table with only the corrected 
#' statistics.
#' 
#' @usage corrected(object, ...)
#' @param object      an object to explain
#' @param ...         ignored
#' 
#' @return An ANOPA table with the corrected test statistics.
#' 
#' @export
corrected <- function(object, ...) {  UseMethod("corrected") }

#' @export 
corrected.default <- function(object, ...) { print(object) } 
#' @export 
corrected.ANOPAobject <- function(object, ...) {
    if (object$type == "ANOPAsimpleeffects") 
        u <- object$simpleeffects[,c(1,2,3,5,6,7)]
    else if (object$type == "ANOPAomnibus")
        u <- object$omnibus[,c(1,2,3,5,6,7)]
        
    class(u) <- c("ANOPAtable", class(u))
    return(u)
}

#' @title uncorrected 
#' @name uncorrected
#'
#' @md
#'
#' @description
#' 'uncorrected()' provides an ANOPA table with only the uncorrected  
#' statistics.
#' 
#' @usage uncorrected(object, ...)
#' @param object      an object to explain
#' @param ...         ignored
#' 
#' @return An ANOPA table with the un-corrected test statistics.
#'   That should be avoided, more so if your sample is rather small.
#' 
#' @export
uncorrected <- function(object, ...) {  UseMethod("uncorrected") }

#' @export 
uncorrected.default <- function(object, ...) { print(object) } 
#' @export 
uncorrected.ANOPAobject <- function(object, ...) {
    if (object$type == "ANOPAsimpleeffects") 
        u <- object$simpleeffects[,c(1,2,3,4)]
    else if (object$type == "ANOPAomnibus")
        u <- object$omnibus[,c(1,2,3,4)]
        
    class(u) <- c("ANOPAtable", class(u))
    return(u)
}
