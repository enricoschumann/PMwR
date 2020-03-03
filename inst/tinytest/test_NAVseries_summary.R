nav <- NAVseries(1:10)
bm <- NAVseries(1:10)
expect_equal(summary(nav)[[1]]$tracking.error, NULL)
expect_equal(summary(nav, bm = bm)[[1]]$tracking.error, 0)

nav <- NAVseries(cumprod(1+rnorm(10, sd = 0.01)))
bm <- NAVseries(cumprod(1+rnorm(10, sd = 0.01)))
expect_equal(summary(nav, bm = bm)[[1]]$tracking.error,
             sd(returns(nav) - returns(bm)))
expect_equal(summary(nav, bm = bm)[[2]]$tracking.error, 0)
expect_equal(summary(nav,
                     bm = bm,
                     assume.daily = TRUE)[[1]]$tracking.error,
             16*sd(returns(nav) - returns(bm)))
