# Approximation method preserving the ambiguity and value
#
 

ApproximationAmbValPres <- function(value,...)
{
  
  # calculate some additional values
  
  alphaIntervals <- FuzzyNumbers::alphaInterval(value,...)
  
  alphaIntervalL <- alphaIntervals[1]
  
  alphaIntervalU <- alphaIntervals[2]
  
  expectedIntervals <- FuzzyNumbers::expectedInterval(value,...)
  
  expectedIntervalL <- expectedIntervals[1]
  
  expectedIntervalU <- expectedIntervals[2]
  
  expectedVal <- FuzzyNumbers::expectedValue(value,...)
 
  amb <- FuzzyNumbers::ambiguity(value,...)
  
  ambL <- 1/2*amb - alphaIntervalL
  
  ambU <- -1/2*amb + alphaIntervalU
  
  wdt <- FuzzyNumbers::width(value,...)
  
  # let's find the approximation
  
  if(amb >= 1/3 * wdt) {
    
    t1 <- -6 * alphaIntervalL + 4 * expectedIntervalL
    
    t2 <- 6 * alphaIntervalL -2 * expectedIntervalL
    
    t3 <- 6 * alphaIntervalU -2 * expectedIntervalU
    
    t4 <- -6 * alphaIntervalU +4 * expectedIntervalU
    
  } else {
    
    if(1/3 * ambL <= ambU & ambU <= 3 * ambL) {
      
      t1 <- 2 * expectedIntervalL + 2 * expectedIntervalU - 6 * alphaIntervalU
      
      t2 <- 3 * alphaIntervalL + 3 * alphaIntervalU - expectedIntervalL - expectedIntervalU
      
      t3 <- t2
      
      t4 <- 2 * expectedIntervalL + 2 * expectedIntervalU - 6 * alphaIntervalL
      
    } else {
      
      if(ambU < 1/3 * ambL) {
        
        t1 <- 2 * alphaIntervalL
        
        t2 <- t1
        
        t3 <- t1
        
        t4 <- 6 * alphaIntervalU - 4 * alphaIntervalL
        
      } else {
        
        t1 <- 6 * alphaIntervalL - 4 * alphaIntervalU
        
        t2 <- 2 * alphaIntervalU
        
        t3 <- t2
        
        t4 <- t2
        
      }
      
    }
    
  }
  
  return(FuzzyNumbers::TrapezoidalFuzzyNumber(t1,t2,t3,t4))
  
}