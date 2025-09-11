#' Function for approximation with the help of methods other than in FuzzyNumbers package
#'
#' @description
#' `FuzzyApproximation` approximates the given fuzzy number.
#'
#' @details
#' The function approximates the fuzzy number given by \code{value} with the method selected by
#' \code{method}. The following approximations are possible: \code{ExpectedValueCore}--preserving the
#' expected value and the core of \code{value}, \code{TriangularSupportDist}--constructs the
#' triangular fuzzy number based on minimization of \code{DpqDistance}, preserving the support of \code{value},
#' and the approximation methods from the FuzzyNumbers package (namely: \code{Naive, NearestEuclidean, ExpectedIntervalPreserving,
#' SupportCoreRestricted,SupportCorePreserving}).
#' 
#'  
#' 
#' The input value should be given by a fuzzy number described by classes from \code{FuzzyNumbers} package.
#'
#' @md
#' 
#'
#' @return
#' The output is a fuzzy number (triangular or trapezoidal one) described by
#'  classes from \code{FuzzyNumbers} package.
#'
#'
#'
#'
#'
#' @param value Fuzzy number to approximate.
#'
#'
#' @param method The selected approximation method.
#'
#' @param piecewise If \code{piecewise=TRUE} is set, then the methods "Naive","NearestEuclidean" (from the FuzzyNumbers package) produce piecewise linear fuzzy number as the
#'  output, otherwise they result in trapezoidal fuzzy number.
#' 
#' @param ... Additional parameters passed to other functions (like approximation method from the FuzzyNumbers package).
#' 
#' 
#'
#'
#' @examples
#' 
#' library(FuzzyNumbers)
#' 
#' # prepare complex fuzzy number
#'
#' A <- FuzzyNumber(-5, 3, 6, 20, left=function(x)
#' pbeta(x,0.4,3),
#' right=function(x) 1-x^(1/4),
#' lower=function(alpha) qbeta(alpha,0.4,3),
#' upper=function(alpha) (1-alpha)^4)
#' 
#' # find approximation
#' 
#' FuzzyApproximation (A)
#' 
#'
#' @export


FuzzyApproximation <- function(value,method="ExpectedValueCore",piecewise=FALSE,...)
{
  # checking parameters
  
  if((length(value) != 1) || (!isFuzzyNumber(value)))
  {
    stop("Parameter value should be a single fuzzy number!")
    
  }
  
  if(!(method %in% c(approximationMehodsInside,approximationMehodsOutside)))
  {
    stop("Parameter method should be a proper name of approximation method!")
  }
  
  
  
  if(method=="ExpectedValueCore") {
    
    # apply the Expected-Value and Core Preserving Method
    
    # calculate the expected values
    
    EI1 <- stats::integrate(Vectorize(function(x) FuzzyNumbers::alphacut(value,x)[1]), lower=0, upper=1)$value
    
    EI2 <- stats::integrate(Vectorize(function(x) FuzzyNumbers::alphacut(value,x)[2]), lower=0, upper=1)$value
    
    # construct the output
    
    output <- FuzzyNumbers::TrapezoidalFuzzyNumber(EI1, FuzzyNumbers::core(value)[1], FuzzyNumbers::core(value)[2], EI2)
    
  }
  
  if(method=="TriangularSupportDist") {
    
    # apply the Triangular Approximation by SupportPreserving Method with the Distance Measure
    
    # the optimized distance is the function of the core
    
    distance <- function(x) 	{	
      
      DpqDistance(value,
                  FuzzyNumbers::TriangularFuzzyNumber(FuzzyNumbers::supp(value)[1], x, FuzzyNumbers::supp(value)[2]), ...)
      
      }
      
    # find core
      
    optimCore <- stats::optimise(distance, interval=FuzzyNumbers::supp(value), maximum=FALSE)$objective
    
    output <- FuzzyNumbers::TriangularFuzzyNumber(FuzzyNumbers::supp(value)[1],
                                                  optimCore,FuzzyNumbers::supp(value)[2])
     
    
  }
  
  # use various approximation methods from FuzzyNumbers package regarding the method and piecewise parameter
  
  if(method %in% c("SupportCoreRestricted","ExpectedIntervalPreserving")) {
    
    output <- FuzzyNumbers::trapezoidalApproximation(value, method=method,...)
    
  }
  
  if((method %in% c("Naive","NearestEuclidean")) && piecewise==FALSE) {
    
    output <- FuzzyNumbers::trapezoidalApproximation(value, method=method,...)
    
  }
  
  
  
  if((method %in% c("Naive","NearestEuclidean")) && piecewise==TRUE) {
 
    output <- FuzzyNumbers::piecewiseLinearApproximation(value, method=method,...)
    
  }
  
  if(method=="SupportCorePreserving") {
    
    output <- FuzzyNumbers::piecewiseLinearApproximation(value, method=method,...)
    
  }
  
  return(output)
}
