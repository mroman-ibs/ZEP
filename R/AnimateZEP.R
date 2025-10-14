#' Animate output for the Zadeh's principle
#'
#' @description
#' `AnimateZEP` applies the selected function to a fuzzy number using the Zadeh's principle, and then animates
#' the output.
#' 
#' @details
#' The function takes the input fuzzy number \code{value} (which should be described by one of the 
#' classes from \code{FuzzyNumbers} package) and applies the function \code{FUN} using
#' the Zadeh's principle. The output is given by a fuzzy number or its approximation (when 
#' \code{approximation} is set to \code{TRUE} and the respective \code{method} is selected).
#' To properly find the output, value of \code{FUN} is calculated for many alpha-cuts of \code{value}.
#' The number of these alpha-cuts is equal to \code{knots} (plus 2 for the support and the core).
#' The output fuzzy number and its approximation are animated for the decreasing value of \code{alpha} (i.e., the consecutive alpha-cuts).
#' If the approximation is used, then the approximated fuzzy number is shown with green line.
#' 
#'  
#' 
#' The input fuzzy number \code{value} should be given by fuzzy number described by classes from \code{FuzzyNumbers} package.
#'
#' @md
#' 
#'
#' @return
#' One (or two) figures are animated: the output fuzzy number (for the Zadeh's principle
#' and the applied function), and its approximation (if selected).
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
#' @param sleep Interval between frames in the animation.
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
#' # animate the output fuzzy number
#' 
#' AnimateZEP(A,FUN=function(x)x^3+2*x^2-1)
#'
#' # find and animate the approximated output via the Zadeh's principle
#' 
#' AnimateZEP(A,FUN=function(x)x^3+2*x^2-1,approximation=TRUE)
#' 
#' 
#'
#' @export
#' 


AnimateZEP <- function(value,FUN,knots=10,approximation=FALSE,method="NearestEuclidean",sleep=0.05,...)
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
  
  if((length(sleep) != 1) || !is.numeric(sleep) || sleep <= 0)
  {
    stop("Parameter sleep should be a single positive real value!")
  }
  
  # calculate alpha-cuts
  
  alphas <- seq(0,1,len=knots+2)
  
  # prepare the figure
  
  valueZFun <- ApplyZEP(value,FUN=FUN,knots=knots,approximation=FALSE,...)
  
  suppValueZFun <- FuzzyNumbers::supp(valueZFun)
  
  leftBoundsZFun <- FuzzyNumbers::alphacut(valueZFun, alphas)[,"L"]
  
  rightBoundsZFun <- FuzzyNumbers::alphacut(valueZFun, alphas)[,"U"]
  
  # check supp for approximation case
  
  if(approximation) {
    
    valueZApproximation <- ApplyZEP(value,FUN=FUN,knots=knots,approximation=TRUE,
                                          method=method,...)
    
    suppValueZFunApprox <- FuzzyNumbers::supp(valueZApproximation)
    
    suppValueZFun <- c(min(suppValueZFun[1],suppValueZFunApprox[1]),max(suppValueZFun[2],suppValueZFunApprox[2]))
    
    leftBoundsZFunApproximation <- FuzzyNumbers::alphacut(valueZApproximation, alphas)[,"L"]
    
    rightBoundsZFunApproximation <- FuzzyNumbers::alphacut(valueZApproximation, alphas)[,"U"]
    
  }
  
  graphics::plot.new()
  
  # prepare animation 
  
  oopt <- animation::ani.options(interval = sleep, nmax = knots + 2)
  
  for(i in seq_len(animation::ani.options("nmax"))) {
    
    grDevices::dev.hold()
    
    graphics::plot(c(leftBoundsZFun[(knots+3-i):(knots+2)],rev(rightBoundsZFun)[1:i]),c(alphas[(knots+3-i):(knots+2)],rev(alphas)[1:i]),
         type='l', frame.plot=TRUE,xlim=suppValueZFun, ylim=c(0,1),
         xlab="x", main=paste0("alpha=",rev(alphas)[i]),ylab="alpha")
    
    if(approximation) {
      
      # additional figure with approximation
      
      graphics::lines(c(leftBoundsZFunApproximation[(knots+3-i):(knots+2)],rev(rightBoundsZFunApproximation)[1:i]),
                      c(alphas[(knots+3-i):(knots+2)],rev(alphas)[1:i]),type='l', col="green")
    
    }
    
    animation::ani.pause()
  }
  
  # finishing animation
  
  animation::ani.options(oopt)
  
  
}