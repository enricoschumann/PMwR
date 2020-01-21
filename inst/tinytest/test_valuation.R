pos <- position(c(A = 10, B = 5))
expect_equivalent(
    c(valuation(pos, t(c(2, 1)))), c(20,5))
