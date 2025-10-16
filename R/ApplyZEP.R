#' Function to apply the Zadeh's principle
#'
#' @description
#' `ApplyZEP` applies the selected function to a fuzzy number using the Zadeh's principle.
#'
#' @details
#' The function takes the input fuzzy number \code{value} (which should be described by one of the 
#' classes from \code{FuzzyNumbers} package) and applies the function \code{FUN} using
#' the Zadeh's principle. The output is given by a fuzzy number or its approximation (when 
#' \code{approximation} is set to \code{TRUE} and the respective \code{method} is selected).
#' To properly find the output, value of \code{FUN} is calculated for many alpha-cuts of \code{value}.
#' The number of these alpha-cuts is equal to \code{knots} (plus 2 for the support and the core).
#' 
#'  
#' 
#' The input fuzzy number \code{value} should be given by fuzzy number described by classes from \code{FuzzyNumbers} package.
#'
#' @md
#' 
#'
#' @return
#' The output is a fuzzy number described by
#'  classes from \code{FuzzyNumbers} package (piecewise linear fuzzy number without approximation,
#'  various types with the approximation applied).
#'
#'
#'
#'
#'
#' @param value Input fuzzy number.
#' 
#' @param FUN Function used for the input fuzzy number with the help of the Zadeh's principle.
#' 
#' @param knots Number of the alpha-cuts used during calculation of the output.
#' 
#' @param approximation If \code{TRUE}, the approximated output is calculated.
#' 
#'
#' @param method The selected approximation method.
#' 
#' @param ... Additional parameters passed to other functions.
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
#' # find the output via the Zadeh's principle
#' 
#' ApplyZEP(A,FUN=function(x)x^3+2*x^2-1)
#'
#' # find the approximated output via the Zadeh's principle
#' 
#' ApplyZEP(A,FUN=function(x)x^3+2*x^2-1,approximation=TRUE)
#' 
#'
#' @export


# apply function to FN via the Zadeh's principle

ApplyZEP <- function(value,FUN,knots=10,approximation=FALSE,method="NearestEuclidean",...)
{
  # checking parameters
  
  if((length(value) != 1) || (!isFuzzyNumber(value)))
  {
    stop("Parameter value should be a single fuzzy number!")
    
  }
  
  if(length(method) != 1)
  {
    stop("Parameter method should be a single value!")
  }
  
  if(!(method %in% c(approximationMehodsInside,approximationMehodsOutside)))
  {
    stop("Parameter method should be a proper name of approximation method!")
  }
  
  if((length(knots) != 1) || !IfInteger(knots) || knots <= 0)
  {
    stop("Parameter knots should be a single, positive integer value!")
  }
  
  if((length(approximation) != 1) || !is.logical(approximation))
  {
    stop("Parameter approximation should be a single logical value!")
  }
  
  # values for alpha-cuts
  
  alphas <- seq(0,1,len=knots+2)
  
  n <- knots+2
  
  # find alpha-cuts
  
  leftBounds <- FuzzyNumbers::alphacut(value, alphas)[,"L"]
  
  rightBounds <- FuzzyNumbers::alphacut(value, alphas)[,"U"]
  
  # calculate new alpha-cuts
  
  newLeftBounds <- rep(NA,n)
  
  newRightBounds <- rep(NA,n)
  
  # start from the core
  
  # check if triangular fuzzy number
  
  if(leftBounds[n]==rightBounds[n]) {
    
    newLeftBounds[n] <- FUN(leftBounds[n],...)
    
    newRightBounds[n] <- FUN(leftBounds[n],...)
    
  } else {
    
  
    temp <- c(FUN(leftBounds[n],...),FUN(rightBounds[n],...))
    
    newLeftBounds[n] <- min(c(stats::optimize(f=FUN,interval = c(leftBounds[n],rightBounds[n]),...)$objective,temp))
    
    newRightBounds[n] <- max(c(stats::optimize(f=FUN,interval = c(leftBounds[n],rightBounds[n]),maximum = TRUE,
                                               ...)$objective,temp))
    
  }
  
  
  # and other alpha-cuts
  
  for(i in (n-1):1) {
    
    # cat("bounds ", i, " :", c(leftBounds[i],rightBounds[i]), "\n")
    # 
    # cat("bounds ", i+1, " :", c(leftBounds[i+1],rightBounds[i+1]), "\n")
    
    temp <- c(FUN(leftBounds[i],...),FUN(rightBounds[i],...),newLeftBounds[i+1],newRightBounds[i+1])
    
    # check if intervals are not reduced to the point and add new values
    
    if(leftBounds[i] != leftBounds[i+1]) {
      
      temp <- c(temp, stats::optimize(f=FUN,interval = c(leftBounds[i],leftBounds[i+1]),...)$objective)
      
    } 
    
    if(rightBounds[i] != rightBounds[i+1]) {
      
      temp <- c(temp, stats::optimize(f=FUN,interval = c(rightBounds[i+1],rightBounds[i]),...)$objective)
      
    }
    
    newLeftBounds[i] <- min(temp)
    
    temp <- c(FUN(leftBounds[i],...),FUN(rightBounds[i],...),newLeftBounds[i+1],newRightBounds[i+1])
    
    # check if intervals are not reduced to the point and add new values
    
    if(leftBounds[i] != leftBounds[i+1]) {
      
      temp <- c(temp, stats::optimize(f=FUN,interval = c(leftBounds[i],leftBounds[i+1]),maximum = TRUE,...)$objective)
      
    } 
    
    if(rightBounds[i] != rightBounds[i+1]) {
      
      temp <- c(temp, stats::optimize(f=FUN,interval = c(rightBounds[i+1],rightBounds[i]),maximum = TRUE,...)$objective)
      
    }

    newRightBounds[i] <- max(temp)
    
  }
  
  
  # cat("newLeftBounds", newLeftBounds, "\n")
  # 
  # cat("newRightBounds", newRightBounds, "\n")
  
  # create piecewise-linear FN
  
  output <- FuzzyNumbers::PiecewiseLinearFuzzyNumber(a1=newLeftBounds[1],a2=newLeftBounds[n],
                                       a3=newRightBounds[n],a4=newRightBounds[1],
                                       knot.left = newLeftBounds[c(2:(n-1))],
                                       knot.right = newRightBounds[c((n-1):2)])
  
  # if approximation=TRUE, approximate using the selected method
  
  if(approximation) {
    
    if(method %in% approximationMehodsOutside) {
      
      output <- FuzzyNumbers::trapezoidalApproximation(output,method=method,...)
      
    }
    
    if(method %in% approximationMehodsInside) {
      
      output <- FuzzyApproximation(output,method=method,...)
      
    }
    
    
    
  }
  
  return(output)
  
}
