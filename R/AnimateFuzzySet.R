#' Function for animation of the whole list of fuzzy numbers
#'
#' @description
#' `AnimateFuzzySet` animates the whole list consisting of fuzzy numbers.
#' 
#' @details
#' The function takes the list of input fuzzy numbers \code{listOfValues} (which should be described by one of the 
#' classes from \code{FuzzyNumbers} package) and applies the function \code{FUN} using
#' the Zadeh's principle. The output is given as animation of consecutive fuzzy numbers or their approximations (when 
#' \code{approximation} is set to \code{TRUE} and the respective \code{method} is selected).
#' To properly find the output, value of \code{FUN} is calculated for many alpha-cuts of \code{listOfValues}.
#' The number of these alpha-cuts is equal to \code{knots} (plus 2 for the support and the core).
#' If the approximation is used, then the approximated fuzzy number is shown with green line.
#' 
#'  
#' 
#' The input fuzzy number from a list \code{listOfValues} should be given by fuzzy number described by classes from \code{FuzzyNumbers} package.
#'
#' @md
#' 
#'
#' @return
#' One (or two) figures are animated: the series of the output fuzzy numbers (for the Zadeh's principle
#' and the applied function), and their approximation (if selected).
#'
#'
#'
#'
#'
#' @param listOfValues List of the input fuzzy numbers.
#' 
#' @param FUN Function used for the input fuzzy number with the help of the Zadeh's principle.
#' 
#' @param knots Number of the alpha-cuts used during calculation of the output.
#' 
#' @param grid If \code{TRUE}, then additional grid is plotted.
#' 
#' @param approximation If \code{TRUE}, the approximated output is calculated.
#' 
#'
#' @param method The selected approximation method.
#' 
#' @param interval Interval between frames in the animation.
#' 
#' @param ... Additional parameters passed to other functions.
#'
#'
#' @examples
#' 
#' library(FuzzyNumbers)
#' 
#' # prepare list of fuzzy numbers
#'
#' a <- seq(0,5,by=1)
#' 
#' fuzzyList <- list()
#' 
#' for (i in 1:length(a)) {
#'  
#'  fuzzyList[[i]] <- TrapezoidalFuzzyNumber(i,i+1,2*i+1,3*i+1)
#'  
#' }
#'
#' # check the list
#' fuzzyList
#'
#' # now some animations for various functions and then with approximation
#' AnimateFuzzySet(fuzzyList,FUN=function(x) x^2)
#'
#' AnimateFuzzySet(fuzzyList,FUN=function(x) sin(x))
#' 
#' AnimateFuzzySet(fuzzyList,FUN=function(x) x^3+1,approximation = TRUE)
#' 
#' 
#' 
#' 
#' @export
#' 
#' 



AnimateFuzzySet <- function(listOfValues,FUN,knots=10,grid=TRUE,approximation=FALSE,method="NearestEuclidean",interval=0.5,...)
{
  
  # saving par
  
  oldpar <- graphics::par(no.readonly = TRUE)
  
  on.exit(graphics::par(oldpar))
  
  graphics::plot.new()
  
  graphics::layout(mat = matrix(c(3, 4, 2, 1), nrow = 2, ncol = 2))  
  
  # modified plotting function
  
  PlotModified <- function(value,xlimVal1,ylimVal1,FUN,knots,grid,approximation,method,...)
  {
    
    alphas <- seq(0,1,len=knots+2)
    
    leftBounds <- FuzzyNumbers::alphacut(value, alphas)[,"L"]
    
    rightBounds <- FuzzyNumbers::alphacut(value, alphas)[,"U"]
    
    xlimVal <- xlimVal1
    
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
    
    if(grid==TRUE) graphics::grid()
    
    graphics::abline(v=FuzzyNumbers::core(value), col=4, lty=3)
    
    # prepare the second figure
    
    valueZFun <- ApplyZFunction(value,FUN=FUN,knots=knots,approximation=FALSE,
                                method=method,...)
    
    
    # check supp for approximation case
    
    if(approximation) {
      
      valueZApproximation <- ApplyZFunction(value,FUN=FUN,knots=knots,approximation=TRUE,
                                            method=method,...)
      
    }
    
    graphics::curve(FUN, col=2, xlab=NA, ylab=NA, xlim=xlimVal, ylim=ylimVal1, main=substitute(paste(bold('FUN'))))
    
    if(grid==TRUE) graphics::grid()
    
    graphics::abline(v=FuzzyNumbers::core(value), h=FuzzyNumbers::core(valueZFun), col=4, lty=3)
    
    # third figure without approximation
    
    leftBoundsZFun <- FuzzyNumbers::alphacut(valueZFun, alphas)[,"L"]
    
    rightBoundsZFun <- FuzzyNumbers::alphacut(valueZFun, alphas)[,"U"]
    
    graphics::plot(-c(alphas,rev(alphas)),c(leftBoundsZFun,rev(rightBoundsZFun)),  type='l', axes=FALSE,frame.plot=TRUE,ylim=ylimVal1, xlim=c(-1,0),
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
    
    if(grid==TRUE) graphics::grid()
    
    graphics::abline(h=FuzzyNumbers::core(valueZFun), col=4, lty=3)
    
    
  }
  
  # calculate the necessary limits for plots
  
  # first x axis
  
  listXlim <- lapply(listOfValues, FuzzyNumbers::supp)
  
  xlimValAll <- c(min(unlist(listXlim)),max(unlist(listXlim)))
  
  # next y axis
  
  listZVal <- lapply(listOfValues,ApplyZFunction,FUN,knots,approximation=FALSE,method,...)
  
  listYlim <- lapply(listZVal, FuzzyNumbers::supp)
  
  ylimValAll <- c(min(unlist(listYlim)),max(unlist(listYlim)))
  
  
  # prepare animation 
  
  oopt <- animation::ani.options(interval=interval, nmax = length(listOfValues))
  
  for(i in seq_len(animation::ani.options("nmax"))) {
    
    grDevices::dev.hold()
    
    PlotModified(value=listOfValues[[i]],xlimVal1=xlimValAll,ylimVal1=ylimValAll,
                 FUN=FUN,knots=knots,grid=grid,approximation=approximation,method=method,...)
    
    animation::ani.pause()
  }
  
  # finishing animation
  
  animation::ani.options(oopt)
  
  graphics::par(mfrow = c(1, 1))
  
  return(0)
  
}