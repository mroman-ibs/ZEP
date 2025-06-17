# approximation methods from FuzzyNumbers package

approximationMehodsOutside <- c("Naive","NearestEuclidean","SupportCorePreserving")

# function to check if the parameter is given by the integer

IfInteger <- function(x)
{
  if(is.numeric(x))
  {
    test <- all.equal(x, as.integer(x), check.attributes = FALSE)
    
    if(test == TRUE)
    { return(TRUE) }
    else { return(FALSE) }
  }
  
  else { return(FALSE) }
}


isFuzzyNumber <- function(value)
{
  return(methods::is(value,"FuzzyNumber"))
}
