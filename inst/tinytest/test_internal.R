## .may_be_Date2 <- function(x, ...) {
##     if (is.numeric(x)) {
##         res <- FALSE
##     } else {
##         ans <- try(as.Date(x), silent = TRUE)
##         res <- if (inherits(ans, "try-error"))
##                    FALSE
##                else if (all(is.na(ans)))
##                    FALSE
##                else
##                    TRUE
##     }
##     if (res)
##         attr(res, "Date") <- ans
##     res
## }

## library("tinytest")
## expect_true(.may_be_Date(Sys.Date() + 0:5))
## expect_false(.may_be_Date(1:10))
## expect_true(.may_be_Date(as.character(Sys.Date() + 0:5)))
## expect_true(.may_be_Date(as.factor(Sys.Date() + 0:5)))


## library("rbenchmark")
## benchmark(expect_true(PMwR:::.may_be_Date(Sys.Date() + 0:500)),
##           expect_true(.may_be_Date2(Sys.Date() + 0:500)),
##           replications = 10000)





## x <- c(9, NA, 2)
## PMwR:::.copy_fw(x)

## x <- c(9, NA, NA)
## PMwR:::.copy_fw(x)

## x <- c(NA, NA, 1)
## PMwR:::.copy_fw(x)

## x <- c(1, 2, 3)
## PMwR:::.copy_fw(x)

## x <- c(NA, NA, NA)
## PMwR:::.copy_fw(x)



## x <- c(9, NA, 2)
## x <- cbind(x, x)
## PMwR:::.copy_fw_matrix(x)

## x <- c(9, NA, NA)
## x <- cbind(x, x)
## PMwR:::.copy_fw_matrix(x)

## x <- c(NA, NA, 1)
## x <- cbind(x, x)
## PMwR:::.copy_fw_matrix(x)

## x <- c(1, 2, 3)
## x <- cbind(x, x)
## PMwR:::.copy_fw_matrix(x)

## x <- c(NA, NA, NA)
## x <- cbind(x, x)
## PMwR:::.copy_fw_matrix(x)
