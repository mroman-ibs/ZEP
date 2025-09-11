#' Plot input and output for the Zadeh's principle
#'
#' @description
#' `PlotZFunction` applies the selected function to a fuzzy number using the Zadeh's principle, and plots
#' the input and output.
#'
#' @details
#' The function takes the input fuzzy number \code{value} (which should be described by one of the 
#' classes from \code{FuzzyNumbers} package) and applies the function \code{FUN} using
#' the Zadeh's principle. The output is given by a fuzzy number or its approximation (when 
#' \code{approximation} is set to \code{TRUE} and the respective \code{method} is selected).
#' To properly find the output, value of \code{FUN} is calculated for many alpha-cuts of \code{value}.
#' The number of these alpha-cuts is equal to \code{knots} (plus 2 for the support and the core).
#' The input and output fuzzy numbers are plotted together with the applied function.
#' If the approximation is used, then also the approximated fuzzy number is shown (green line).
#' 
#'  
#' 
#' The input fuzzy number \code{value} should be given by fuzzy number described by classes from \code{FuzzyNumbers} package.
#'
#' @md
#' 
#'
#' @return
#' Three (or four) figures are plotted: the input fuzzy number, the respective output (for the Zadeh's principle
#' and the applied function), and the function. The output fuzzy number can be approximated with the
#' selected method and also plotted.
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
#' @param grid If \code{TRUE}, then additional grid is plotted.
#' 
#' @param alternate If \code{TRUE}, the second type of the layout of figures is used.
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
#' # plot the figures
#' 
#' PlotZFunction(A,FUN=function(x)x^3+2*x^2-1)
#'
#' # find and plot the approximated output via the Zadeh's principle
#' 
#' PlotZFunction(A,FUN=function(x)x^3+2*x^2-1,approximation=TRUE)
#' 
#'
#' @export



# main function to plot input and output for function with the Zadeh's principle

PlotZFunction <- function(value,FUN,knots=10,grid=TRUE,alternate=FALSE,approximation=FALSE,method="NearestEuclidean",...)
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
  
  if((length(grid) != 1) || !is.logical(grid))
  {
    stop("Parameter grid should be a single logical value!")
  }
  
  if((length(alternate) != 1) || !is.logical(alternate))
  {
    stop("Parameter alternate should be a single logical value!")
  }
  
  # saving par
  
  oldpar <- graphics::par(no.readonly = TRUE)
  
  on.exit(graphics::par(oldpar))
  
  # prepare layout of the plots
  
  graphics::layout(mat = matrix(c(3, 4, 2, 1), nrow = 2, ncol = 2))  
  
  if(alternate==FALSE)
  {
    FuzzyNumbers::plot(value,main="value",...)
    
    if(grid==TRUE) grid()
    
    graphics::curve(FUN,xlim = FuzzyNumbers::supp(value),col="red",ylab="y",main="FUN",...)
    
    if(grid==TRUE) grid()
    
    outputFunction <- ApplyZFunction(value,FUN = FUN, knots = knots)
    
    FuzzyNumbers::plot(outputFunction,main="FUN(value)",col="blue",...)
    
    if(grid==TRUE) grid()
    
    # add approximation if selected
    
    if(approximation) {
      
      outputFunctionApprox <- ApplyZFunction(value,FUN = FUN, knots = knots,approximation = TRUE,method=method,...)
      
      FuzzyNumbers::plot(outputFunctionApprox,main="Approx(FUN(value))",col="green",...)
      
      if(grid==TRUE) grid()
      
    }
    
    
    
  } else {
    
    # calculate alpha-cuts
    
    alphas <- seq(0,1,len=knots+2)
    
    leftBounds <- FuzzyNumbers::alphacut(value, alphas)[,"L"]
    
    rightBounds <- FuzzyNumbers::alphacut(value, alphas)[,"U"]
    
    xlimVal <- FuzzyNumbers::supp(value)
    
    # first figure (value)
    
    graphics::par(mar=c(.8,.8,1.8,1.8)) 
    layout.matrix <- matrix(c(3, 0, 2, 1), nrow = 2, ncol = 2) 
    graphics::layout(mat = layout.matrix, #The order of drowing plots
           heights=c(2,1),      #Heights of the two rows
           widths=c(1,2))       #Widths of the two columns
    #layout.show(3)

    graphics::plot(c(leftBounds,rev(rightBounds)), -c(alphas,rev(alphas)), type='l', axes=FALSE,frame.plot=TRUE,xlim=xlimVal, ylim=c(-1,0),
         xlab=substitute(paste(bold('value'))),ylab=NA,...)
    
    graphics::Axis(side=2, labels=FALSE)
    
    graphics::Axis(side=3, labels=FALSE)
    
    if(grid==TRUE) grid()
    
    graphics::abline(v=FuzzyNumbers::core(value), col=4, lty=3)
    
    # prepare the second figure
    
    valueZFun <- ApplyZFunction(value,FUN=FUN,knots=knots,approximation=FALSE,
                                method=method,...)
    
    suppValueZFun <- FuzzyNumbers::supp(valueZFun)
    
    # check supp for approximation case
    
    if(approximation) {
      
      valueZApproximation <- ApplyZFunction(value,FUN=FUN,knots=knots,approximation=TRUE,
                                            method=method,...)
      
      suppValueZFunApprox <- FuzzyNumbers::supp(valueZApproximation)
      
      suppValueZFun <- c(min(suppValueZFun[1],suppValueZFunApprox[1]),max(suppValueZFun[2],suppValueZFunApprox[2]))
      
    }
    
    graphics::curve(FUN, col=2, xlab=NA, ylab=NA, xlim=xlimVal, ylim=suppValueZFun, main=substitute(paste(bold('FUN'))))
    
    if(grid==TRUE) grid()
    
    graphics::abline(v=FuzzyNumbers::core(value), h=FuzzyNumbers::core(valueZFun), col=4, lty=3)
    
    # third figure without approximation
    
    leftBoundsZFun <- FuzzyNumbers::alphacut(valueZFun, alphas)[,"L"]
    
    rightBoundsZFun <- FuzzyNumbers::alphacut(valueZFun, alphas)[,"U"]
    
    graphics::plot(-c(alphas,rev(alphas)),c(leftBoundsZFun,rev(rightBoundsZFun)),  type='l', axes=FALSE,frame.plot=TRUE,ylim=suppValueZFun, xlim=c(-1,0),
         xlab=NA, main=NA,ylab=NA)
    
    if(approximation) {
      
      # third figure with approximation
      
      leftBoundsZFunApproximation <- FuzzyNumbers::alphacut(valueZApproximation, alphas)[,"L"]
      
      rightBoundsZFunApproximation <- FuzzyNumbers::alphacut(valueZApproximation, alphas)[,"U"]
      
      graphics::lines(-c(alphas,rev(alphas)),c(leftBoundsZFunApproximation,rev(rightBoundsZFunApproximation)),  type='l', col="green")
      
      graphics::title(main=substitute(paste(bold('Approx(FUN(value))'))))
      
      
    } else {
      
      # without approximation
      
      graphics::title(main=substitute(paste(bold('FUN(value)'))))
      
    }
    
    graphics::Axis(side=1, labels=FALSE)
    
    graphics::Axis(side=4, labels=FALSE)
    
    if(grid==TRUE) grid()
    
    graphics::abline(h=FuzzyNumbers::core(valueZFun), col=4, lty=3)
    
    
  }
  
  
}
