p <- c(104, 108, 104)
expect_equivalent(scale1(p), p/104)
expect_equivalent(scale1(p, when = 2), p/108)
expect_equal(sd(returns(scale1(p, scale = TRUE))), 1)
expect_equal(scale1(p, when = 2, scale = TRUE)[2L], 1)

## NA handling
p <- cbind(c(104, 108, 104, 105),
           c( NA, 108, 104, 105))
expect_equivalent(scale1(p), p/108)
expect_equivalent(scale1(p, level = 100), p/1.08)

p <- cbind(c(104, 108, 104, 105),
           c(103, 108, 104, 105))

## centring (aka de-meaning) -- arithmetic
expect_equal(apply(returns(scale1(p, centre = TRUE, geometric=FALSE)),
                   2, mean, na.rm = TRUE),
             rep(0, ncol(p)))

## TODO: de-mean: geometric
## expect_equal(apply(returns(scale1(p, centre = TRUE, geometric=TRUE)),
##                   2, mean, na.rm = TRUE),
##             rep(0, ncol(p)))

## vol scaling -- target vol is 0.01
expect_equal(apply(returns(scale1(p, scale = 0.01)),
                   2, sd, na.rm = TRUE),
             rep(0.01, ncol(p)))

## de-mean & scale -- arithmetic
expect_equal(apply(returns(scale1(p,
                                  centre = TRUE,
                                  scale = 0.01,
                                  geometric=FALSE)),
                   2, mean, na.rm = TRUE),
             rep(0, ncol(p))) ## arith. mean is zero

expect_equal(apply(returns(scale1(p,
                                  centre = TRUE,
                                  scale = 0.01,
                                  geometric=FALSE)),
                   2, sd, na.rm = TRUE),
             rep(0.01, ncol(p))) ## sd is 0.01

## de-mean & scale -- geometric
P <- scale1(p, centre = TRUE,
            scale = 0.01,
            geometric = TRUE)
expect_equal(P[nrow(P), ]/P[1, ],
             rep(1, ncol(p))) ## geom. mean is zero ==
## total return is zero


## de-meaning
expect_equal(apply(returns(P), 2, sd, na.rm = TRUE),
             rep(0.01, ncol(p))) ## sd is 0.01



P1 <- cumprod(1 + c(0, rnorm(20, sd = 0.02)))
P1_scaled <- scale1(P1, centre = TRUE)

expect_equal(sd(returns(P1)), sd(returns(P1_scaled)))
expect_equivalent(tail(P1_scaled,1), 1)

P2 <- cumprod(1 + c(0, rnorm(20, sd = 0.02)))
P2_scaled <- scale1(P2, centre = TRUE, scale = 0.03)

expect_equal(sd(returns(P2_scaled)), 0.03)
expect_equal(tail(P2_scaled,1),head(P2_scaled,1))

## ... should not affect correlation
expect_equal(cor(returns(P1), returns(P2)),
             cor(returns(P1_scaled), returns(P2_scaled)))


## when: last, first
P <- cumprod(1 + c(0, rnorm(20, sd = 0.02)))

P.scaled <- scale1(P, when = "last", level = 42)
expect_equal(returns(P), returns(P.scaled))
expect_equal(P.scaled[length(P.scaled)], 42)

P.scaled <- scale1(P, when = "first", level = 42)
expect_equal(returns(P), returns(P.scaled))
expect_equal(P.scaled[1], 42)
