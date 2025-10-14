# Function returns correct values

    Code
      ApplyZEP(A, FUN = function(x) x^3 + 2 * x^2 - 1)
    Output
      Piecewise linear fuzzy number with 10 knot(s),
         support=[-76,8799],
            core=[44,287].

---

    Code
      ApplyZEP(B, FUN = function(x) sqrt(x))
    Output
      Piecewise linear fuzzy number with 10 knot(s),
         support=[1,2.82843],
            core=[1.22474,2.44949].

---

    Code
      ApplyZEP(C, FUN = function(x) x^2 + sin(x))
    Output
      Piecewise linear fuzzy number with 10 knot(s),
         support=[-0.232466,49.657],
            core=[4.9093,4.9093].

---

    Code
      ApplyZEP(D, FUN = function(x) x^2 + x - 4)
    Output
      Piecewise linear fuzzy number with 10 knot(s),
         support=[-2,16],
            core=[2,8].

---

    Code
      ApplyZEP(A, FUN = function(x) x^3 + 2 * x^2 - 1, approximation = TRUE)
    Output
      Trapezoidal fuzzy number with:
         support=[-253.039,3373.62],
            core=[-253.039,-253.039].

---

    Code
      ApplyZEP(B, FUN = function(x) sqrt(x), approximation = TRUE)
    Output
      Trapezoidal fuzzy number with:
         support=[1.00397,2.83274],
            core=[1.22828,2.45417].

---

    Code
      ApplyZEP(C, FUN = function(x) x^2 + sin(x), approximation = TRUE, method = "ExpectedIntervalPreserving")
    Output
      Trapezoidal fuzzy number with:
         support=[0.701674,42.3719],
            core=[1.90369,1.90369].

---

    Code
      ApplyZEP(D, FUN = function(x) x^2 + x - 4, approximation = TRUE, method = "Naive")
    Output
      Trapezoidal fuzzy number with:
         support=[-2,16],
            core=[2,8].

