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
  
  expect_error(AnimateListZEP(0.3,FUN=function(x)sqrt(x)),
               
               "Parameter listOfValues should be a list of fuzzy numbers!")
  
  
  
  
  expect_error(AnimateListZEP(list(A,B,C,D),approximation=TRUE,method = "b"),
               
               "Parameter method should be a proper name of approximation method!")
  
  expect_error(AnimateListZEP(list(A,B,C,D),approximation=0.5),
               
               "Parameter approximation should be a single logical value!")
  
  expect_error(AnimateListZEP(list(A,B,C,D),FUN=function(x)sqrt(x),approximation=c(TRUE,FALSE)),
               
               "Parameter approximation should be a single logical value!")
  
  
  expect_error(AnimateListZEP(list(A,B,C,D),FUN=function(x)sqrt(x),knots = -5),
               
               "Parameter knots should be a single, positive integer value!")
  
  expect_error(AnimateListZEP(list(A,B,C,D),FUN=function(x)sqrt(x),knots = c(2,6)),
               
               "Parameter knots should be a single, positive integer value!")
  
  
  expect_error(AnimateListZEP(list(A,B,C,D),FUN=function(x)sqrt(x),knots = 0.8),
               
               "Parameter knots should be a single, positive integer value!")
  
  
  
  
})




