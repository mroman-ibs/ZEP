#' Animate output for the Zadeh's principle
#'
#' @description
#' `AnimateZFunction` applies the selected function to a fuzzy number using the Zadeh's principle, and then animates
#' the output.
#' 
#' 
#' 
#'
#' @export
#' 


AnimateZFunction <- function(value,FUN,knots=10,...)
{
  # calculate alpha-cuts
  
  alphas <- seq(0,1,len=knots+2)
  
  # prepare the figure
  
  valueZFun <- ApplyZFunction(value,FUN=FUN,knots=knots,approximation=FALSE,...)
  
  suppValueZFun <- FuzzyNumbers::supp(valueZFun)
  
  leftBoundsZFun <- FuzzyNumbers::alphacut(valueZFun, alphas)[,"L"]
  
  rightBoundsZFun <- FuzzyNumbers::alphacut(valueZFun, alphas)[,"U"]
  
  # prepare animation 
  
  invisible(oopt <- ani.options(interval = 0.05, nmax = knots + 2))
  
  for(i in seq_len(ani.options("nmax"))) {
    
    dev.hold()
    
    plot(c(leftBoundsZFun[(knots+3-i):(knots+2)],rev(rightBoundsZFun)[1:i]),c(alphas[(knots+3-i):(knots+2)],rev(alphas)[1:i]),
         type='l', frame.plot=TRUE,xlim=suppValueZFun, ylim=c(0,1),
         xlab="x", main=paste0("alpha=",rev(alphas)[i]),ylab="alpha")
    
    ani.pause()
  }
  
  # finishing animation
  
  ani.options(oopt)
  
  
}