computeYield <- function(cashflows, times, y0,
                         tol = 1e-5, h = 1e-8, maxit = 10L) {
    n <- length(cashflows)
    if (n != length(times))
        stop("length of 'cashflows' differs from length of 'times'")
    .C("computeyield",
       c = as.double(cashflows),
       t = as.double(times),
       y = as.double(y0),
       tol = as.double(tol),
       h = as.double(h),
       n = as.integer(n),
       maxit = as.integer(maxit))$y
}
