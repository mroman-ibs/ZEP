test_that("Function returns correct values", {
  
  # starting values
  
  A <- FuzzyNumbers::FuzzyNumber(-5, 3, 6, 20, left=function(x)
          pbeta(x,0.4,3),
        right=function(x) 1-x^(1/4),
        lower=function(alpha) qbeta(alpha,0.4,3),
        upper=function(alpha) (1-alpha)^4)
  
  B <- FuzzyNumbers::TrapezoidalFuzzyNumber(1,1.5,6,8)
  
  C <- FuzzyNumbers::TrapezoidalFuzzyNumber(-1,2,2,7)
  
  D <- FuzzyNumbers::PiecewiseLinearFuzzyNumber(1, 2, 3, 4,
                                                knot.n=1, knot.alpha=0.25, knot.left=1.5, knot.right=3.25)
  
  # snapshots tests
  
  expect_snapshot(
    
    {ApplyZFunction(A,FUN=function(x)x^3+2*x^2-1)}
  )
  
  expect_snapshot(
    
    {ApplyZFunction(B,FUN=function(x)sqrt(x))}
  )
  
  expect_snapshot(
    
    {ApplyZFunction(C,FUN=function(x)x^2+sin(x))}
  )
  
  expect_snapshot(
    
    {ApplyZFunction(D,FUN=function(x)x^2+x-4)}
  )
  
  expect_snapshot(
    
    {ApplyZFunction(A,FUN=function(x)x^3+2*x^2-1,approximation=TRUE)}
  )
  
  expect_snapshot(
    
    {ApplyZFunction(B,FUN=function(x)sqrt(x),approximation=TRUE)}
  )
  
  expect_snapshot(
    
    {ApplyZFunction(C,FUN=function(x)x^2+sin(x),approximation=TRUE,method="ExpectedIntervalPreserving")}
  )
  
  expect_snapshot(
    
    {ApplyZFunction(D,FUN=function(x)x^2+x-4,approximation=TRUE,method = "Naive")}
  )
  
  
  
})
