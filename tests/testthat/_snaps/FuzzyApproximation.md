# Function returns correct values

    Code
      FuzzyApproximation(A)
    Output
      Trapezoidal fuzzy number with:
         support=[-4.05882,8.8],
            core=[3,6].

---

    Code
      FuzzyApproximation(B, method = "Naive")
    Output
      Trapezoidal fuzzy number with:
         support=[1,8],
            core=[1.5,6].

---

    Code
      FuzzyApproximation(C, method = "ExpectedIntervalPreserving")
    Output
      Trapezoidal fuzzy number with:
         support=[-1,7],
            core=[2,2].

---

    Code
      FuzzyApproximation(D, method = "Naive", piecewise = TRUE)
    Output
      Piecewise linear fuzzy number with 1 knot(s),
         support=[1,4],
            core=[2,3].

