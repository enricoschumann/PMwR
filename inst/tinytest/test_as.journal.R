## unnamed numeric vector
expect_equal(journal(amount = 1:5),
             as.journal(1:5))

## named numeric vector
x <- 1:3
names(x) <- letters[1:3]
expect_equal(journal(amount = 1:3,
                     instrument = letters[1:3]),
             as.journal(x))

## ERROR: no method for matrix
x <- array(1, dim = c(2,2))
expect_error(as.journal(x))
