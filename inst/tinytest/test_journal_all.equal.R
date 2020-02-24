j1 <- journal(amount = 1:10, fees = 5)
j2 <- journal(fees = 5, amount = 1:10)
expect_true(isTRUE(all.equal(j1, j2)))

j1 <- journal(amount = 1:10, timestamp = 1:10)
j2 <- journal(amount = 10:1, timestamp = 10:1)
expect_true(isTRUE(all.equal(j1, j2)))
expect_true(!isTRUE(all.equal(j1, j2, ignore.sort = FALSE)))
