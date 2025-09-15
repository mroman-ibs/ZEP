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
    
    {DpqDistance(A,B)}
  )
  
  expect_snapshot(
    
    {DpqDistance(C,D)}
  )
  
  expect_snapshot(
    
    {DpqDistance(A,B,p=2,q=1)}
  )
  
  expect_snapshot(
    
    {DpqDistance(C,D,p=1/3,q=3)}
  )
  
  
  
})



test_that("Function reports errors", {
  
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
  # tests
  
  expect_error(DpqDistance(0.3,A),
               
               "Parameter value1 should be a single fuzzy number!")
  
  
  expect_error(DpqDistance(B,"c"),
               
               "Parameter value2 should be a single fuzzy number!")
  
  
  expect_error(DpqDistance(c(A,B),C),
               
               "Parameter value1 should be a single fuzzy number!")
  
  
  expect_error(DpqDistance(A,B,p=-3),
               
               "Parameter p should be a single numeric value!")
  
  expect_error(DpqDistance(A,B,p=c(5,7)),
               
               "Parameter p should be a single numeric value!")
  
  expect_error(DpqDistance(A,B,p="s"),
               
               "Parameter p should be a single numeric value!")
  
  
  expect_error(DpqDistance(A,B,q=-3),
               
               "Parameter q should be a single numeric value!")
  
  expect_error(DpqDistance(A,B,q=c(5,7)),
               
               "Parameter q should be a single numeric value!")
  
  expect_error(DpqDistance(A,B,q="s"),
               
               "Parameter q should be a single numeric value!")
  
  
  
})


