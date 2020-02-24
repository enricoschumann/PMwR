
amount <- c(1); price <- 100
expect_equal(.pl_stats(amount, price),
             list(average = 100, realised = 0))

amount <- c(1,-1); price <- c(100, 100)
expect_equal(.pl_stats(amount, price),
             list(average = c(100, 100), realised = c(0, 0)))

amount <- c(1,-1); price <- c(100, 101)
expect_equal(.pl_stats(amount, price),
             list(average = c(100, 101), realised = c(0, 1)))

amount <- c(1,-1); price <- c(100, 99)
expect_equal(.pl_stats(amount, price),
             list(average = c(100, 99), realised = c(0, -1)))

amount <- c(1,-5); price <- c(100, 101)
expect_equal(.pl_stats(amount, price),
             list(average = c(100, 101), realised = c(0, 1)))

amount <- c(1,0); price <- c(100, 101)
expect_equal(.pl_stats(amount, price),
             list(average = c(100, 100), realised = c(0, 0)))

amount <- c(1,0,1); price <- c(100, 101,102)
expect_equal(.pl_stats(amount, price),
             list(average = c(100, 100, 101), realised = c(0, 0, 0)))

amount <- c(1,-2,1); price <- c(100, 101, 99)
expect_equal(.pl_stats(amount, price),
             list(average = c(100, 101, 99), realised = c(0, 1, 3)))

amount <- c(0,0); price <- c(100, 200)
expect_equal(.pl_stats(amount, price),
             list(average = c(100, 100), realised = c(0, 0)))

amount <- c(-1,-1,-1); price <- c(100, 98, 96)
expect_equal(.pl_stats(amount, price),
             list(average = c(100,99,98), realised = c(0, 0, 0)))

amount <- c(-1,1,0,-1,0,1); price <- c(100, 98,95, 96,97, 94)
expect_equal(.pl_stats(amount, price),
             list(average  = c(100, 98, 98, 96, 96, 94),
                  realised = c(0, 2, 2, 2, 2, 4)))
