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



