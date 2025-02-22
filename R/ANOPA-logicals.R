    ####################################################################################
## @name logicalfunctions
##
## @title logical functions for formulas
##
## @aliases is.formula is.one.sided has.nested.terms has.cbind.terms in.formula sub.formulas
##
## @md
##
## @description The functions 'is.formula()', 'is.one.sided()',
##      'has.nested.terms()', 
##      'has.cbind.terms()', 'in.formula()' and 'sub.formulas()'
##      performs checks or extract sub-formulas from a given formula.
##s
## @usage is.formula(frm)
## @usage is.one.sided(frm)
## @usage has.nested.terms(frm)
## @usage has.cbind.terms(frm)
## @usage in.formula(frm, whatsym)
## @usage sub.formulas(frm, head)
## @usage sub.formula.locations(frm, head)
##
## @param frm a formula;
## @param whatsym a symbol to search in the formula;
## @param head the beginning of a sub-formula to extract
##
## @return `is.formula(frm)`, `has.nested.terms(frm)`, and `has.cbind.terms(frm)`
##      returns TRUE if frm is a formula, contains a '|' or a 'cbind' respectively;
##      `in.formula(frm, whatsym)` returns TRUE if the symbol `whatsym` is somewhere in 'frm';
##      `sub.formulas(frm, head)` returns a list of all the sub-formulas which contains `head`;
##      `sub.formula.locations(frm, head)` returns the locations of such subformulas.
##
## @details These functions are for internal use only.
##
## @examples
## is.formula( success ~ Difficulty )
##  
## has.nested.terms( Level ~ Factor | Level )
##  
## has.cbind.terms( cbind(succ1, succ2, succ3) ~ Difficulty )
##  
## in.formula( success ~ Difficulty, "Difficulty" )
##  
## sub.formulas( cbind(succ1, succ2, succ3) ~ Difficulty, "cbind" )
##  
## sub.formula.locations( cbind(succ1, succ2, succ3) ~ Difficulty, "cbind" )
##
##################################################################################
##
## @export is.formula
## @export is.one.sided
## @export has.nested.terms
## @export has.cbind.terms
## @export in.formula
## @export sub.formulas
##
###################################################################################

#################################################################################
# logical functions:    
#################################################################################

is.formula <- function( frm ) inherits( frm, "formula")

is.one.sided <- function( frm ) 
    is.formula(frm)  && 
    length(frm) == 2 

has.nested.terms <- function( frm ) {
    if(!is.formula(frm)) return(FALSE)
    return ( in.formula(frm, "|"))
}

has.cbind.terms <- function( frm ) {
    if(!is.formula(frm)) return(FALSE)
    return ( in.formula(frm, "cbind"))
}
has.crange.terms <- function( frm ) {
    if(!is.formula(frm)) return(FALSE)
    return ( in.formula(frm, "crange"))
}

# performs a depth-first search in the language structure.
in.formula <- function( frm, whatsym) {
    if ((is.symbol(frm))&&(frm == whatsym)) 
        return(TRUE)

    if (!is.symbol(frm)) {
        for (i in (1:length(frm)) ) {
            if (in.formula( frm[[i]], whatsym) )
                return(TRUE)
        }
    }
    return(FALSE)        
}

## Lists all the subformulas with a certain head in formula
sub.formulas <- function( frm, head ) {
    if (!in.formula( frm, head)) stop("error! head not in frm")
    res <- rrapply::rrapply( frm,
                condition = function(x) x == head,
                f = function(x, .xpos) .xpos,
                how = "flatten"
    )
    # grab the terms, removing last index to have the subformula
    lapply(res, function(i) frm[[i[1:(length(i)-1)]]])

}
sub.formula.locations <- function( frm, head ) {
    if (!in.formula( frm, head)) stop("error! head not in frm")
    res <- rrapply::rrapply( frm,
                condition = function(x) x == head,
                f = function(x, .xpos) .xpos,
                how = "flatten"
    )
    # removing last index to have the subformula
    lapply(res, function(i) i[1:(length(i)-1)])

}


