## test subsetting journals, and using argument 'invert'

J <- journal(amount = 1:3, instrument = letters[1:3])

expect_equal(    length(J["a"]), 1)
expect_equal(instrument(J["a"]), "a")

expect_equal(    length(J[1]), 1)
expect_equal(instrument(J[1]), "a")



expect_equal(    length(J[1, invert = TRUE]), 2)
expect_equal(instrument(J[1, invert = TRUE]), letters[2:3])

expect_equal(    length(J[1, invert = TRUE]), 2)
expect_equal(instrument(J[1, invert = TRUE]), letters[2:3])


i <- logical(3L)
i[1L] <- !i[1L]
expect_equal(    length(J[i, invert = TRUE]), 2)
expect_equal(instrument(J[i, invert = TRUE]), letters[2:3])

expect_equal(    length(J[i, invert = TRUE]), 2)
expect_equal(instrument(J[i, invert = TRUE]), letters[2:3])
