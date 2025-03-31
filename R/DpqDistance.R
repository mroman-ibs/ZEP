#' Function to calculate D(p,q) distance.
#'
#' @description
#' `DpqDistance` calculates the generalized D(p,q) distance between two fuzzy numbers.
#'
#' @details
#' The function calculates the generalized D(p,q) distance between two fuzzy numbers \code{value1} and \code{value2},
#' where \code{p} is the value of the applied power, and \code{q} is the weight between these two fuzzy numbers.
#' 
#'  
#' 
#' All of the input values should be given by fuzzy numbers described by classes from \code{FuzzyNumbers} package.
#'
#' @md
#' 
#'
#' @return
#' The output is a numerical value (the calculated distance).
#'
#'
#'
#'
#'
#' @param value1 First fuzzy number.
#'
#'
#' @param value2 Second fuzzy number.
#' 
#' @param p Value of the power (and the the root) applied in the distance calculation.
#' 
#' @param q Value of the weight for the second fuzzy number (for the first one this weight is calculated as \code{1-q}, respectively).
#' 
#'
#'
#' @examples
#'
#' 
#' library(FuzzyNumbers)
#' 
#' # prepare two fuzzy numbers
#'
#' A <- TrapezoidalFuzzyNumber(0,1,2,3)
#' 
#' B <- TrapezoidalFuzzyNumber(1,3,4,6)
#' 
#' # calculate the distance
#' 
#' DpqDistance (A,B)
#' 
#'
#' @export



# Function to calculate D(p,q) distance

DpqDistance <- function(value1,value2,p=2,q=1/2)
{
  
  lowers <- function(alpha) 
    abs( FuzzyNumbers::alphacut(value1,alpha)[1]-FuzzyNumbers::alphacut(value2,alpha)[1] )^p
  
  L <- integrate(Vectorize(lowers), lower=0, upper=1)$value
  
  uppers <- function(alpha) 
    abs( FuzzyNumbers::alphacut(value1,alpha)[2]-FuzzyNumbers::alphacut(value2,alpha)[2] )^p
  
  U <- integrate(Vectorize(uppers), lower=0, upper=1)$value
  
  return( ( (1-q)*L + q*U )^(1/p) )
}
