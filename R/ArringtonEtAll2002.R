#' Arrington et al. (2002) dataset
#'
#' The data, taken from \insertCite{a02;textual}{ANOPA}, is a dataset examining
#' the distribution of fishes with empty stomachs, classified over
#' three factors:
#' 'Collection location' (3 levels: Africa, Central/South America, North America),
#' 'Diel feeding behavior' (2 levels: diurnal, nocturnal),
#' 'Trophic category' (4 levels: Detritivore, Invertivore, Omnivore, Piscivore).
#' It is therefore a 3 × 2 × 4 design with 24 cells.
#' The original data set also contains Order, Family and Species of the observed	
#' fishes and can be obtained from
#' https://figshare.com/collections/HOW_OFTEN_DO_FISHES_RUN_ON_EMPTY_/3297635
#' It was commented in \insertCite{wh11;textual}{ANOPA}.
#' 
#' @md
#'
#' @docType data
#'
#' @usage ArringtonEtAl2002
#'
#' @format A data frame.
#'
#' @keywords datasets
#'
#' @references 
#' \insertAllCited{}
#'
#' @source \doi{10.1890/0012-9658(2002)083[2145:HODFRO]2.0.CO;2}
#'
#' @examples
#' 
#' # see the dataset
#' ArringtonEtAl2002
#' 
#' # The columns s and n indicate the number of fishes with
#' # empty stomachs (the "success") and the total number
#' # of fishes observed, respectively. Thus s/n is the proportion.
#' 
#' # run the ANOPA analysis
#' w <- anopa( {s; n} ~  Location * Diel * Trophism, ArringtonEtAl2002)
#' 
#' # make a plot with all the factors
#' anopaPlot(w)
#' 
#' # ... or with a subset of factors, with
#' anopaPlot(w, ~ Location * Trophism)
#' 
#' # Because of the three-way interaction, extract simple effects for each Diel
#' e <- emProportions( w, {s;n} ~ Location * Trophism | Diel  ) 
#' 
#' # As the two-way simple interaction for Nocturnal * Diel is close to significant, 
#' # we extract the second-order simple effects for each Diel and each Location
#' e <- emProportions(w, {s;n} ~ Trophism | Location * Diel  ) 
#' # As seen, the Trophism is significant for Noctural fishes of 
#' # Central/South America.
#' 
#' 
"ArringtonEtAl2002"