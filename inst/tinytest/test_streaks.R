x <- c(112, 102, 101, 104, 111, 98, 82, 93, 99, 105, 103, 110)

res <- streaks(x, up = 5, down = -5, relative = FALSE)

res <- streaks(x, up = 1.9, down = -1.9, relative = FALSE)

x <- c(100, 110, 105)
res <- streaks(x, up = .10, down = -.10)
expect_equal(nrow(res), 1)
expect_equal(res$state, "up")

x <- c(100, 110, 99 - 1e-12)
res <- streaks(x, up = .10, down = -.10)
expect_equal(nrow(res), 2)
expect_equal(res$state, c("up", "down"))

