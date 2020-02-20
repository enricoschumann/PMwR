##  input: no dim
amount <- c(1, 1, 1)
p <- position(amount)
expect_equivalent(c(p), 3)
expect_true(is.na(instrument(p)))
expect_equal(dim(p), c(1, 1))

amount <- c(1, 1, 1)
instrument <- c("A", "A", "B")
p <- position(amount = amount, instrument = instrument)
expect_equal(colnames(p), c("A", "B"))
expect_equal(dim(p), c(1, 2))

amount <- c(A = 1, B = 1, C = 1)
p <- position(amount)
expect_equivalent(c(p), amount)
expect_equal(colnames(p), c("A", "B", "C"))
expect_equal(dim(p), c(1, 3))

amount <- c(A = 1, A = 1, C = 1)
p <- position(amount)
expect_equivalent(c(p), c(2, 1))
expect_equal(colnames(p), c("A", "C"))
expect_equal(dim(p), c(1, 2))

p <- position(amount, use.names = FALSE)
expect_equivalent(c(p), 3)
expect_true(is.na(instrument(p)))
expect_equal(dim(p), c(1, 1))



## input: single row
A <- array(1:3, dim = c(1, 3))
colnames(A) <- letters[1:3]
p <- position(A)
expect_equivalent(c(p), c(A))
expect_equal(colnames(p), colnames(A))
expect_equal(dim(p), c(1, 3))

p <- position(A, instrument = letters[1:3])
expect_equivalent(c(p), c(A))
expect_equal(colnames(p), letters[1:3])
expect_equal(dim(p), c(1, 3))

p <- position(A, instrument = letters[4:6])
expect_equivalent(c(p), c(A))
expect_equal(colnames(p), letters[4:6])
expect_equal(dim(p), c(1, 3))

p <- position(A, use.names = TRUE)
expect_equivalent(c(p), c(A))
expect_equal(colnames(p), colnames(A))
expect_equal(dim(p), c(1, 3))

p <- position(A, use.names = FALSE)
expect_equivalent(c(p), sum(A))
expect_true(is.na(instrument(p)))
expect_equal(dim(p), c(1, 1))



## input: single column
A <- array(1:3, dim = c(3, 1))
colnames(A) <- "a"
p <- position(A)
expect_equivalent(c(p), sum(A))
expect_equal(colnames(p), colnames(A))
expect_equal(instrument(p), "a")
expect_equal(dim(p), c(1, 1))

p <- position(A, use.names = TRUE)
expect_equivalent(c(p), sum(A))
expect_equal(colnames(p), colnames(A))
expect_equal(instrument(p), "a")
expect_equal(dim(p), c(1, 1))

p <- position(A, use.names = FALSE)
expect_equivalent(c(p), sum(A))
expect_true(is.na(instrument(p)))
expect_equal(dim(p), c(1, 1))



## input: matrix
A <- array(1:6, dim = c(2, 3))
colnames(A) <- letters[1:3]
expect_error(position(A, use.names = FALSE))

p <- position(A)
expect_equivalent(c(p), colSums(A))
expect_equal(instrument(p), letters[1:3])
expect_equal(dim(p), c(1, 3))

p <- position(A, use.names = TRUE)
expect_equivalent(c(p), colSums(A))
expect_equal(instrument(p), letters[1:3])
expect_equal(dim(p), c(1, 3))

expect_error(position(A, timestamp = 3:1))

expect_equal(position(A, timestamp = 1:2),
             position(A, timestamp = 2:1))

