## return contribution

prices <- 1:10
prices <- cbind(A = prices, B = prices + 0.5)
signal <- function()
    if (Time() < 5L)
        c(1, 1) else c(0, 0)
bt <- btest(list(prices), signal,
            initial.cash = 100,
            instrument = colnames(prices),
            include.data = TRUE)

weights <- cbind(prices, 1) *
    position(bt, include.cash = TRUE) / bt$wealth
C <- rbind(0, returns(cbind(prices, 1)) * weights[-nrow(prices), ])
expect_equal(rowSums(C), returns(bt$wealth, pad = 0))


## -----------------------------
w <- c(0.5,0.5)
R <- c(0.1, 0.2)
ans <- rc(t(R), t(w))
expect_equal(ans$period_contributions$segment_1, (w*R)[1])
expect_equal(ans$period_contributions$segment_2, (w*R)[2])

ans <- rc(t(R), t(w), segments = c("A", "B"))
expect_equal(ans$period_contributions$A, (w*R)[1])
expect_equal(ans$period_contributions$B, (w*R)[2])


## -----------------------------
rc(R =       matrix(c(-3.70, -3.50, -0.10,
                       4.10,  3.70,  2.90,
                       3.50,  1.20,  1.00)/100, byrow = TRUE, nrow = 3),
   weights = matrix(c(.6, .1, .3,
                      .6, .1, .3,
                      .6, .1, .3), byrow = TRUE, nrow = 3),
   segments = c("large cap", "small cap", "fixed income"))

## -----------------------------
rc(R =       matrix(c(-3.70, -3.50, -0.10,
                       4.10,  3.70,  2.90,
                       3.50,  1.20,  1.00)/100, byrow = TRUE, nrow = 3),
   weights = matrix(c(.6, .1, .3,
                      .6, .1, .3,
                      .6, .1, .3), byrow = TRUE, nrow = 3),
   segments = c("large cap", "small cap", "fixed income"),
   linking.method = "logarithmic")


## -----------------------------
rc(segments = c("large cap", "small cap", "fixed income"),
   R =
       matrix(c(-3.70, -3.50, -0.10,
                 4.10,  3.70,  2.90,
                 3.50,  1.20,  1.00)/100, byrow = TRUE, nrow = 3)[1, ],
   weights =
       matrix(c(.6, .1, .3,
                .6, .1, .3,
                .6, .1, .3), byrow = TRUE, nrow = 3)[1, ],
   method   = "topdown",
   R.bm =
       matrix(c(-1.90, -5.30, -0.90,
                 2.10,  4.30,  2.40,
                 2.10,  1.40,  0.40)/100, byrow = TRUE, nrow = 3)[1, ],
   weights.bm =
       matrix(c(.55, .05, .4,
                .55, .05, .4,
                .55, .05, .4), byrow = TRUE, nrow = 3)[1, ],
   )


## -----------------------------
## Christopherson / Cari\~no 2009, Table 19.1
ans <- rc(segments = c("stocks", "bonds"),
          R = matrix(c(40, 10,
                       10, 20)/100, byrow = TRUE, nrow = 2),
          weights =
              matrix(c(50, 50,
                       55, 45)/100, byrow = TRUE, nrow = 2),
          linking.method = "geometric1")
expect_equal(ans$total_contributions[["total"]],  0.43125)
expect_equal(ans$total_contributions[["stocks"]], 0.284)
expect_equal(ans$total_contributions[["bonds"]],  0.14725)

ans <- rc(segments = c("stocks", "bonds"),
          R = matrix(c(40, 10,
                       10, 20)/100, byrow = TRUE, nrow = 2),
          weights =
              matrix(c(50, 50,
                       55, 45)/100, byrow = TRUE, nrow = 2),
          linking.method = "geometric0")
expect_equal(ans$total_contributions[["total"]],  0.43125)
expect_equal(ans$total_contributions[["stocks"]], 0.26875)
expect_equal(ans$total_contributions[["bonds"]],  0.1625)


## same tests as before, but now with "0-cumulative" etc
ans <- rc(segments = c("stocks", "bonds"),
          R = matrix(c(40, 10,
                       10, 20)/100, byrow = TRUE, nrow = 2),
          weights =
              matrix(c(50, 50,
                       55, 45)/100, byrow = TRUE, nrow = 2),
          linking.method = "1-cumulative")
expect_equal(ans$total_contributions[["total"]],  0.43125)
expect_equal(ans$total_contributions[["stocks"]], 0.284)
expect_equal(ans$total_contributions[["bonds"]],  0.14725)

ans <- rc(segments = c("stocks", "bonds"),
          R = matrix(c(40, 10,
                       10, 20)/100, byrow = TRUE, nrow = 2),
          weights =
              matrix(c(50, 50,
                       55, 45)/100, byrow = TRUE, nrow = 2),
          linking.method = "0-cumulative")
expect_equal(ans$total_contributions[["total"]],  0.43125)
expect_equal(ans$total_contributions[["stocks"]], 0.26875)
expect_equal(ans$total_contributions[["bonds"]],  0.1625)




## --------------------------------------------------------
## NAs in returns should not affect results as long as
## corresponding weights are zero

weights <- rbind(c( 1, 0.0),
                 c( 0.40, 0.60),
                 c( 0.25, 0.75))

R1 <- rbind(c( 1  ,    NA),
           c( 2.5, -1.0),
           c(-2  ,  0.5))/100
r1 <- rc(R1, weights, segment = c("equities", "bonds"))

R2 <- rbind(c( 1  ,    NA),
           c( 2.5, -1.0),
           c(-2  ,  0.5))/100
r2 <- rc(R2, weights, segment = c("equities", "bonds"))

expect_true(all.equal(r1, r2))


## -----------------------------
## order by timestamp

weights <- rbind(c( 0.25, 0.75),
                 c( 0.40, 0.60))
R <- rbind(c( 1  ,    0),
           c( 2.5, -1.0))/100
timestamp <- 1:2

rc1 <- rc(R, weights, timestamp = timestamp,
          segment = c("equities", "bonds"))

weights <- weights[2:1, ]
R <- R[2:1, ]
timestamp <- 2:1
rc2 <- rc(R, weights, timestamp = timestamp,
          segment = c("equities", "bonds"))

expect_equivalent(rc1$period_contributions,
                  rc2$period_contributions)

expect_equivalent(rc1$total_contributions,
                  rc2$total_contributions)


## -----------------------------

## single period case
w <- c(0.2, 0.5, 0.3)
R <- c(0.1, 0.2, -0.01)

expect_equivalent(sum(w*R),
                  rc(R = R, weights = w)$total_contributions[["total"]])


## named weights
w <- c(domestic = 0.2, foreign = 0.5, bonds = 0.3)
R <- c(0.1, 0.2, -0.01)
rc(R = R, weights = w)
expect_equal(names(rc(R = R, weights = w)$total_contributions)[1:3],
             names(w))

## named returns
w <- c(0.2, 0.5, 0.3)
R <- c(domestic = 0.1, foreign = 0.2, bonds = -0.01)
rc(R = R, weights = w)
expect_equal(names(rc(R = R, weights = w)$total_contributions)[1:3],
             names(R))

## mixed names
w <- c(A=0.2, B=0.5, C=0.3)
R <- c(domestic = 0.1, foreign = 0.2, bonds = -0.01)
rc(R = R, weights = w)
expect_equal(names(rc(R = R, weights = w)$total_contributions)[1:3],
             names(R))





w <- rbind(w, w)
rownames(w) <- c("1", "2")
R <- rbind(R, R)
rc(R = R, weights = w)

rc(R = R, weights = w, segments = c("A", "B", "C"))

rc(R = R, weights = w, segments = c("A", "B", "C"))



## -- ATTRIBUTION
## Christopherson / Cari\~no 2009, Table 18.4--18.6
segments <- c("Large Cap", "Small Cap", "Fixed Income")

w    <- matrix(c(60, 10, 30,
                 60, 10, 30,
                 60, 10, 30), nrow = 3, byrow = TRUE)/100
w.bm <- matrix(c(55,  5, 40,
                 55,  5, 40,
                 55,  5, 40), nrow = 3, byrow = TRUE)/100

R    <- matrix(c(-3.7,-3.5,-0.1,
                 +4.1,+3.7,+2.9,
                 +3.5,+1.2,+1.0), nrow = 3, byrow = TRUE)/100

R.bm <- matrix(c(-1.9,-5.3,-0.9,
                 +2.1,+4.3,+2.4,
                 +2.1,+1.4,+0.4), nrow = 3, byrow = TRUE)/100


ans <- rc(R    = R,    weights = w,
          R.bm = R.bm, weights.bm = w.bm)
expect_equivalent(ans$period_contributions$total,
                  rowSums(R * w))

ans <- rc(R = R.bm, weights = w.bm)
expect_equivalent(ans$period_contributions$total,
                  rowSums(R.bm * w.bm))


### single period (inputs don't have a dim)
ans <- rc(R    = R[1, ],    weights = w[1, ],
          R.bm = R.bm[1, ], weights.bm = w.bm[1, ],
          method = "attribution",
          segments = segments)

expect_equivalent(round(ans$allocation[, "total"], 4),
                  -0.0027)
expect_equivalent(round(ans$selection[, "total"], 4),
                  -0.0058)
expect_equivalent(round(ans$interaction[, "total"], 4),
                  -0.0008)

### single period (inputs are row vectors)
ans <- rc(R    = R[1, ,drop=FALSE],    weights = w[1, ,drop=FALSE],
          R.bm = R.bm[1, ,drop=FALSE], weights.bm = w.bm[1, ,drop=FALSE],
          method = "attribution",
          segments = segments)

expect_equivalent(round(ans$allocation[, "total"], 4),
                  -0.0027)
expect_equivalent(round(ans$selection[, "total"], 4),
                  -0.0058)
expect_equivalent(round(ans$interaction[, "total"], 4),
                  -0.0008)



### three periods
rc(R    = R,    weights = w,
   R.bm = R.bm, weights.bm = w.bm,
   method = "attribution",
   segments = segments)







seg <- c("A", "A", "A", "B", "B")
n <- c(1:3, 1:2)
r <- c(0.05, 0.02, -0.1, 0.02, -0.05)

w <- c(0.2, 0.1, 0.0, 0.4, 0.3)
b <- c(0.0, 0.2, 0.3, 0.1, 0.4)


rc(R = t(r),
   weights = t(w),
   segments = seg)
