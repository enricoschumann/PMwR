w <- c(a = 0.4, b = 0.6)
a <- c(x = 0.9, y = 0.05, z = 0.05)
b <- c(A = 0.1, B = 0.2,  C = 0.7)
expect_equal(1,
             sum(replace_weight(w, a = a, b = b, x = c(aaa = 1))))

## -------------------

n <- names(
    replace_weight(w, a = a, b = b, x = c(aaa = 1),
                   prefix = FALSE))
expect_equal(n, c("aaa", "y", "z", "A", "B", "C"))


n <- names(
    replace_weight(w, a = a, b = b, x = c(aaa = 0.5, bbb = 0.5),
                   prefix = FALSE))
expect_equal(n, c("aaa", "bbb", "y", "z", "A", "B", "C"))
