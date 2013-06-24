if (FALSE) {

## forward <- function(spot, t2m, r, q = NULL, cf = NULL, t2cf = NULL) {
##     if (!is.null(q) && !is.null(cf))
##         stop("specify either 'q' or 'cf'")
##     if (t2m < 0)
##         return(NA)
##     if (is.null(cf) && is.null(q))
##         q <- 0
##     if (!is.null(q)) {
##         f <- spot * exp((r - q) * t2m)
##     } else {
##         f <- spot -  sum(exp(-r * t2cf) * cf)
##     }
##     f
## }

## #f <- forward(spot=100, t2m = 1, r = 0.1, cf = c(1,2), t2cf = c(.3,.6))
## #f <- forward(spot=100, t2m = 1, r = 0.1)
## #f <- forward(spot=100, t2m = 1, r = 0.1, q= 0.02)



## spot <- function(forward, t2m, r, q = NULL, cf = NULL, t2cf = NULL) {
##     if (!is.null(q) && !is.null(cf))
##         stop("specify either 'q' or 'cf'")
##     if (t2m < 0)
##         return(NA)
##     if (is.null(cf) && is.null(q))
##         q <- 0
##     if (!is.null(q)) {
##         s <- forward * exp((q - r) * t2m)
##     } else {
##         s <- forward +  sum(exp(-r * t2cf) * cf)
##     }
##     s
## }

#s <- spot(f, t2m = 1, r = 0.1, cf = c(1,2), t2cf = c(.3,.6))
#s <- spot(f, t2m = 1, r = 0.1)
#s <- spot(f, t2m = 1, r = 0.1, q= 0.02)


## putCallParity <- function(call = NULL, put = NULL, forward = NULL,
##     strike, t2m, r, q = NULL, cf = NULL, t2cf = NULL) {
##     if(is.null(call) + is.null(put) + is.null(forward) != 1L)
##         stop("provide call/put, call/forward, or put/forward")
##     if(is.null(call))
##         res <- put  + exp(-r * t2m) * (forward - strike)
##     if(is.null(put))
##         res <- call - exp(-r * t2m) * (forward - strike)
##     if(is.null(forward))
##         res <- (call - put) * exp(r*t2m) + strike
##     res
## }

    

    J <- structure(list(portfolio = c("Modulor", "Modulor", "Modulor",
                        "Modulor", "Modulor", "Modulor", "Modulor", "Modulor", "Modulor",
                        "Modulor", "Modulor", "Modulor", "Modulor", "Modulor", "Modulor",
                        "Modulor", "Modulor", "Modulor", "Modulor", "Modulor", "Modulor",
                        "Modulor", "Modulor", "Modulor", "Modulor", "Modulor", "Modulor",
                        "Modulor", "Modulor", "Modulor", "Modulor", "Modulor", "Modulor",
                        "Modulor", "Modulor", "Modulor", "Modulor", "Modulor", "Modulor",
                        "Modulor", "Modulor", "Modulor", "Modulor", "Modulor", "Modulor",
                        "Modulor", "Modulor", "Modulor", "Modulor", "Modulor", "Modulor",
                        "Modulor", "Modulor", "Modulor", "Modulor", "Modulor", "Modulor",
                        "Modulor", "Modulor", "Modulor", "Modulor", "Modulor", "Modulor",
                        "Modulor", "Modulor", "Modulor", "RentenPlus", "RentenPlus",
                        "RentenPlus", "RentenPlus", "RentenPlus", "RentenPlus", "RentenPlus",
                        "RentenPlus", "RentenPlus", "RentenPlus", "RentenPlus", "RentenPlus",
                        "RentenPlus", "RentenPlus", "RentenPlus", "RentenPlus", "RentenPlus",
                        "RentenPlus", "RentenPlus", "RentenPlus", "RentenPlus", "RentenPlus",
                        "RentenPlus", "RentenPlus", "RentenPlus", "RentenPlus", "RentenPlus",
                        "RentenPlus", "RentenPlus", "RentenPlus", "RentenPlus", "RentenPlus",
                        "RentenPlus", "RentenPlus", "RentenPlus", "RentenPlus", "RentenPlus",
                        "RentenPlus", "RentenPlus", "RentenPlus", "RentenPlus", "RentenPlus",
                        "RentenPlus", "RentenPlus", "RentenPlus", "RentenPlus", "RentenPlus",
                        "RentenPlus", "RentenPlus", "RentenPlus", "RentenPlus", "Modulor",
                        "Modulor", "Modulor", "Modulor", "Modulor", "Modulor", "Modulor",
                        "Modulor", "Modulor", "Modulor", "RentenPlus", "RentenPlus",
                        "RentenPlus", "RentenPlus", "RentenPlus", "RentenPlus", "RentenPlus",
                        "RentenPlus", "RentenPlus", "RentenPlus", "Modulor", "Modulor",
                        "Modulor", "Modulor"),
                        timestamp = c("2013/01/07", "2013/01/14",
                        "2013/01/14", "2013/01/14", "2013/01/14", "2013/01/14", "2013/01/14",
                        "2013/01/14", "2013/01/15", "2013/01/17", "2013/01/23", "2013/01/23",
                        "2013/01/23", "2013/01/28", "2013/01/28", "2013/01/29", "2013/01/29",
                        "2013/02/04", "2013/02/04", "2013/02/04", "2013/02/05", "2013/02/05",
                        "2013/02/05", "2013/02/05", "2013/02/05", "2013/02/05", "2013/02/05",
                        "2013/02/15", "2013/02/15", "2013/02/15", "2013/02/25", "2013/03/11",
                        "2013/03/11", "2013/03/11", "2013/03/11", "2013/03/11", "2013/03/11",
                        "2013/03/11", "2013/03/11", "2013/03/11", "2013/03/14", "2013/03/14",
                        "2013/03/15", "2013/03/15", "2013/03/15", "2013/03/15", "2013/03/15",
                        "2013/03/15", "2013/03/15", "2013/03/15", "2013/03/15", "2013/03/15",
                        "2013/03/15", "2013/03/22", "2013/03/22", "2013/03/22", "2013/03/22",
                        "2013/03/22", "2013/03/22", "2013/03/22", "2013/03/22", "2013/03/22",
                        "2013/04/16", "2013/04/16", "2013/04/16", "2013/04/16", "2013/01/07",
                        "2013/01/07", "2013/01/07", "2013/01/07", "2013/01/07", "2013/01/07",
                        "2013/01/16", "2013/01/16", "2013/01/16", "2013/01/16", "2013/01/17",
                        "2013/02/05", "2013/02/05", "2013/02/05", "2013/02/05", "2013/02/05",
                        "2013/02/05", "2013/02/05", "2013/02/18", "2013/02/18", "2013/02/18",
                        "2013/02/25", "2013/03/11", "2013/03/11", "2013/03/11", "2013/03/11",
                        "2013/03/11", "2013/03/11", "2013/03/14", "2013/03/14", "2013/03/15",
                        "2013/03/15", "2013/03/15", "2013/03/15", "2013/03/15", "2013/03/15",
                        "2013/03/15", "2013/03/15", "2013/03/15", "2013/03/15", "2013/03/22",
                        "2013/03/25", "2013/03/25", "2013/03/25", "2013/03/25", "2013/03/25",
                        "2013/03/25", "2013/03/25", "2013/03/25", "2013/04/16", "2013/04/16",
                        "2013/05/02", "2013/05/02", "2013/05/02", "2013/05/02", "2013/05/02",
                        "2013/05/02", "2013/05/02", "2013/05/02", "2013/05/08", "2013/05/09",
                        "2013/05/08", "2013/05/13", "2013/05/13", "2013/05/15", "2013/05/15",
                        "2013/05/16", "2013/05/16", "2013/05/17", "2013/05/17", "2013/05/17",
                        "2013/05/17", "2013/05/21", "2013/05/21", "2013/05/21"),
                        description =
                        c("FESX Mar 2013",
                          "Beiersdorf ", "Deutsche Telekom DTE", "Fresenius ", "Henkel",
                          "Linde", "Merck", "SAP", "OESX Mar 2013 P 2400", "FESX Mar 2013",
                          "OESX Mar 2013 C 2700", "OESX Mar 2013 P 2700", "OESX Mar 2013 P 2500",
                          "OESX Mar 2013 P 2750", "OESX Mar 2013 P 2550", "OESX Mar 2013 P 2750",
                          "OESX Mar 2013 P 2550", "OESX Mar 2013 C 2700", "OESX Mar 2013 P 2700",
                          "OESX Mar 2013 P 2500", "Beiersdorf ", "Deutsche Telekom DTE",
                          "Linde", "Fresenius ", "Henkel", "Merck", "SAP", "Metro", "Linde",
                          "Kali+Salz", "OESX Mar 2013 P 2700", "OESX Mar 2013 P 2700",
                          "OESX Mar 2013 C 2700", "OESX Mar 2013 P 2750", "OESX Mar 2013 P 2750",
                          "OESX Mar 2013 P 2700", "OESX Mar 2013 C 2700", "OESX Mar 2013 P 2750",
                          "OESX Mar 2013 P 2700", "OESX Mar 2013 C 2700", "OESX Jun 2013 C 2700",
                          "OESX Jun 2013 P 2450", "OESX Mar 2013 P 2550", "OESX Mar 2013 P 2500",
                          "OESX Mar 2013 P 2400", "Merck", "Kali+Salz", "SAP", "Deutsche Telekom DTE",
                          "Beiersdorf ", "Henkel", "Metro", "Fresenius ", "FDAX Jun 2013",
                          "Kali+Salz", "Metro", "Beiersdorf ", "Deutsche Telekom DTE",
                          "Fresenius ", "Henkel", "Merck", "SAP", "ODAX Dec 2013 P 7000",
                          "ODAX Dec 2013 P 7700", "ODAX Dec 2013 P 7000", "ODAX Dec 2013 P 7700",
                          "OESX Mar 2013 P 2700", "OESX Mar 2013 P 2500", "OESX Mar 2013 P 2700",
                          "OESX Mar 2013 P 2500", "OESX Mar 2013 P 2700", "OESX Mar 2013 P 2500",
                          "DTE Mar 2013 P 9 ", "DTE Mar 2013 P 8", "SAP Mar 2013 P 60",
                          "SAP Mar 2013 P 54", "OESX Mar 2013 C 2700", "Beiersdorf ", "Deutsche Telekom DTE",
                          "Linde", "Fresenius ", "Henkel", "Merck", "SAP", "Kali+Salz",
                          "Metro", "Linde", "OESX Mar 2013 P 2700", "OESX Mar 2013 P 2700",
                          "OESX Mar 2013 C 2700", "OESX Mar 2013 C 2700", "OESX Mar 2013 P 2700",
                          "OESX Mar 2013 C 2700", "OESX Mar 2013 P 2700", "OESX Jun 2013 C 2700",
                          "OESX Jun 2013 P 2450", "OESX Mar 2013 P 2500", "OESX Jun 2013 C 2700",
                          "OESX Jun 2013 P 2450", "SAP Mar 2013 P 60", "SAP Mar 2013 P 54",
                          "DTE Mar 2013 P 9 ", "DTE Mar 2013 P 8", "Deutsche Telekom DTE",
                          "DTE Jun 2013 P 7.6", "DTE Jun 2013 C 8.5", "DTE Jun 2013 C 8.5",
                          "Henkel", "Fresenius ", "Beiersdorf ", "Merck", "SAP", "Kali+Salz",
                          "Metro", "FDAX Jun 2013", "ODAX Dec 2013 P 7700", "ODAX Dec 2013 P 7000",
                          "Beiersdorf ", "Deutsche Telekom DTE", "Fresenius ", "Henkel",
                          "Linde", "Merck", "Metro", "Kali+Salz", "FDAX Jun 2013", "FDAX Jun 2013",
                          "FDAX Jun 2013", "DTE Jun 2013 C 8.5", "Deutsche Telekom DTE",
                          "DTE Jun 2013 C 8.5", "Deutsche Telekom DTE", "DTE Jun 2013 C 8.5",
                          "Deutsche Telekom DTE", "DTE Jun 2013 C 8.5", "Deutsche Telekom DTE",
                          "Deutsche Telekom DTE", "Deutsche Telekom DTE", "Deutsche Telekom DTE",
                          "Linde", "Metro"),
                        price = c(2693, 61.2272, 9.0385, 82.523, 60.7555,
                        130.5622, 101.5012, 61.1645, 8.4, 2711, 65.7, 53.1325, 11.3325,
                        51.9313, 9.85304, 60, 11.2, 53.5, 48.7, 9.1, 64.86063, 8.77851,
                        133.00048, 91.65895, 65.53981, 102.01919, 60.47017, 24.0063,
                        131.3805, 33.54741, 52.85, 11.225, 33.5, 37, 44, 14.7, 27.5,
                        43.5, 14, 25, 80.9, 31.1, 0, 0, 0, 112.862, 37.351, 64.676, 8.559,
                        70.094, 72.2735, 21.537, 94.925, 7925.21429, 36.93407, 22.81042,
                        71.13794, 8.47709, 94.2888, 73.16225, 116.13013, 62.27478, 227.84,
                        453.5, 213.96, 428.08, 72.33833, 21.35, 76.7599, 22.2798, 79.767,
                        23.2457, 0.2906, 0.05, 2.36, 0.44, 70.0116, 65.0717, 8.7677,
                        131.8261, 91.6607, 65.3102, 101.44, 60.1888, 33.6776, 24.2185,
                        131.7958, 53.1917, 13.7545, 29.8, 25.5, 16.1336, 27.5, 12.8273,
                        79.8176, 30.39, 0, 74.2446, 32.64, 0, 0, 0, 0, 9, 0.196, 0.2529,
                        0.22845, 73.8952, 96.0583, 71.9682, 114.3519, 62.9896, 36.7278,
                        22.7946, 7961.76, 438.23, 219.4427, 70.0145, 8.9077, 94.9848,
                        71.4246, 142.7055, 115.583, 24.63, 33.26, 8183.5, 8270, 8182.86,
                        0, 8.5, 0, 8.5, 0, 8.5, 0, 8.5, 9.178, 9.1572, 9.3483, 152.8263,
                        24.715),
                        amount = c(200L, 9000L, 60000L, 6500L, 9000L, 4500L,
                        5500L, 9000L, 400L, -200L, -80L, -120L, 120L, -100L, 100L, -100L,
                        100L, -80L, -120L, 120L, 7000L, 50000L, 3500L, 5500L, 7500L,
                        4500L, 7000L, 22000L, -8000L, 16000L, 120L, 40L, 55L, 65L, 65L,
                        40L, 55L, 70L, 40L, 50L, -90L, 90L, -200L, -240L, -400L, -5000L,
                        -8000L, -8000L, -55000L, -8000L, -8250L, -11000L, -6000L, -7L,
                        11500L, 19000L, 6000L, 50000L, 4500L, 5800L, 3600L, -8000L, 200L,
                        -100L, 200L, -100L, -1500L, 1500L, -1500L, 1500L, -1500L, 1500L,
                        -10000L, 10000L, -1800L, 1800L, -1500L, 60000L, 450000L, 30000L,
                        44000L, 60000L, 40000L, 63000L, 62000L, 80000L, -30000L, 1200L,
                        1100L, 500L, 500L, 1100L, 500L, 1100L, -500L, 500L, -4500L, -500L,
                        500L, 1800L, -1800L, 10000L, -10000L, 1000000L, 4500L, -4500L,
                        -5000L, 52000L, 39000L, 53000L, 33000L, -63000L, 102000L, 164000L,
                        -50L, -1500L, 3000L, 3200L, 31000L, 2200L, 2850L, 4700L, 1900L,
                        -2500L, 700L, -7L, -3L, -50L, 351L, -35100L, 3679L, -367900L,
                        2258L, -225800L, 3212L, -321200L, -500000L, -45000L, -91000L,
                        3200L, 20000L),
                        price.underlier = c(2700, 61.2272, 9.0385, 82.523,
                        60.7555, 130.5622, 101.5012, 61.1645, 2710, 2716, 2713, 2713,
                        2713, 2751, 2751, 2735, 2735, 2706, 2706, 2706, 64.85, 8.77,
                        133, 91.65, 65.53, 102, 60.47, 24, 131.4, 33.53, 2687, 2720,
                        2720, 2720, 2711, 2711, 2711, 2712, 2712, 2712, 2743, 2743, 2736,
                        2736, 2736, 112.87, 37.36, 64.68, 8.56, 70.1, 72.28, 21.54, 94.93,
                        7912, 36.93, 22.8, 71.13, 8.47, 94.28, 73.16, 116.13, 62.27,
                        7675, 7675, 7726, 7726, 2710, 2710, 2700, 2700, 2693, 2693, 8.96,
                        8.96, 58.7, 58.7, 2712, 65.07, 8.76, 131.8, 91.66, 65.3, 101.44,
                        60.18, 33.66, 24.21, 131.8, 2685, 2717, 2717, 2710, 2710, 2718,
                        2718, 2743, 2743, 2736, 2735, 2735, 64.7, 64.7, 8.56, 8.56, 8.56,
                        8.53, 8.53, 8.5, 73.89, 96.05, 71.96, 114.35, 62.99, 36.72, 22.79,
                        7951, 7725, 7725, 70, 8.9, 94.98, 71.42, 142.7, 115.58, 24.63,
                        33.26, 8182, 8269, 8181, 9.7, 9.7, 9.7, 9.7, 9.8, 9.8, 10, 10,
                        9.18, 9.16, 9.35, 152.82, 24.71),
                        implied.vol = c(0, 0, 0, 0,
                        0, 0, 0, 0, 21.45, 0, 14.8, 14.8, 18.8, 15.8, 17.45, 15.8, 17.45,
                        14.3, 14.3, 19.25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 18, 14.8, 14.8,
                        13.5, 13.7, 15, 15, 13.7, 15, 15, 16.8, 20.5, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 20.8, 17.9, 20.8,
                        17.9, 15.8, 19.5, 15.8, 19.5, 15.8, 19.5, 19.5, 23, 17.75, 22.3,
                        15.85, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 18, 14.8, 14.8, 15, 15,
                        15, 15, 16.8, 20.5, 0, 16.7, 20.4, 0, 0, 0, 0, 0, 19.5, 18.5,
                        18.5, 0, 0, 0, 0, 0, 0, 0, 0, 17.9, 20.8, 0, 0, 0, 0, 0, 0, 0,
                        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                        FX = c(NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                        interest.rate = c(NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                        expiry = c(NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                        strike = c(NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
                        broker = c("Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Expiry", "Expiry", "Expiry",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "Baader Bank - Non RealTick Trade",
                        "Baader Bank - Non RealTick Trade", "LBB", "LBB", "DEUTSCHE BANK",
                        "DEUTSCHE BANK", "DEUTSCHE BANK", "DEUTSCHE BANK", "LBB", "LBB",
                        "CREDIT SUISSE", "CREDIT SUISSE", "LBB", "DEUTSCHE BANK", "DEUTSCHE BANK",
                        "DEUTSCHE BANK", "DEUTSCHE BANK", "DEUTSCHE BANK", "DEUTSCHE BANK",
                        "DEUTSCHE BANK", "DEUTSCHE BANK", "DEUTSCHE BANK", "DEUTSCHE BANK",
                        "DEUTSCHE BANK", "DEUTSCHE BANK", "DEUTSCHE BANK", "CREDIT SUISSE",
                        "CREDIT SUISSE", "DEUTSCHE BANK", "DEUTSCHE BANK", "LBB", "LBB",
                        "Expiry", "LBB", "LBB", "Expiry", "Expiry", "Exercise", "Expiry",
                        "Exercise", "LBB", "LBB", "LBB", "LBB", "LBB", "LBB", "LBB",
                        "LBB", "LBB", "LBB", "LBB", "LBB", "LBB", "LBB", "LBB", "LBB",
                        "LBB", "LBB", "LBB", "LBB", "LBB", "LBB SWAP TRADE", "LBB SWAP TRADE",
                        "LBB", "assignment", "assignment", "assignment", "assignment",
                        "assignment", "assignment", "assignment", "assignment", "LBB",
                        "LBB", "LBB", "LBB", "LBB"),
                        comment = c(NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA)),
                   .Names = c("portfolio", "timestamp",
                   "description", "price", "amount", "price.underlier", "implied.vol",
                   "FX", "interest.rate", "expiry", "strike", "broker", "comment"),
                   class = "data.frame", row.names = c(NA, -141L))

    
    ## * CASHFLOW
    Cashflow <- function(cashflows, times) {
        if (length(cashflows) != length(times))
            stop("'cashflows' and 'times' need to have the same length")
        x <- list(cashflows = cf, times = times)
        class(x) <- "Cashflow"
        x
    }
    summary.Cashflow <- function(x) {
        res <- list(n = length(x$cashflows),
                    tmin = min(x$times),
                    tmax = max(x$times)
                    )
        class(res) <- "summary.Cashflow"
        res
    }
    print.Cashflow <- function(x) {
        print(unclass(x))
        invisible(x)
    }
    print.summary.Cashflow <- function(x) {
        cat("\n",  x$n," cashflows; ",
            "first at time ", x$tmin, ", last at ", x$tmax, ".\n", sep = "")
        invisible(x)
    }

                                        # example
    tm <- 1:3
    cf <- c(3,3,103)
    c1 <- Cashflow(cf, tm)
    summary(c1)


    ## * TIMESERIES
    ## index (t) is sorted, but not unique
    require("fastmatch")
    require("zoo")
    sts <- function(t, x, check.sorted = TRUE) {
        if (length(t) != NROW(x))
            stop("lengths of t and x differ")
        z <- list(t = t, x = x)
        class(z) <- "sts"
        z
    }
    ##window.sts
    N1 <- 100000
    N2 <- 200000
    t1 <- sts(1:N1, rnorm(N1))
    t2 <- sts(sort(runif(N2))*N1, rnorm(N2))
    t1z <- zoo(t1$x, t1$t)
    t2z <- zoo(t2$x, t2$t)
    merge.sts <- function(...) {
        args <- list(...)
        ## TODO: check classes of indices
        T <- sort(unlist(lapply(args, `[[`, "t"), use.names = FALSE))
        X <- array(NA, dim = c(length(T), length(args)))
        for (j in seq_along(args)) {
            i <- fmatch(args[[j]][["t"]], T)
            X[cbind(i,j)] <- args[[j]][["x"]]
        }
        sts(T, X)
    }
    STS <- merge.sts(t1,t2)
    merge(t1z,t2z)


    

    require("Infront")
    tmp <- getInfrontTradesP("Jaro", from = "20130215000000", to = "20130215230000")
    tmp0 <- tmp[tmp$ticker == "GBL201303",]
    jb <- Journal(datetime = tmp$datetime, tmp$volume, tmp$price, id = tmp$rownames,
                  instrument = tmp$ticker, account = "JB")
    jb

    


    ##                 in %
    ## RHOEN-KLINIKUM  15.0
    ## Suedzucker AG I 15.0
    ## Talanx AG Namen 15.0
    ## Kabel Deutschla 12.3
    ## TAG Immobilien  12.0
    ## SGL CARBON SE I  9.4
    ## Fielmann AG Inh  8.3
    ## Gagfah S.A. Act  6.5
    ## GSW Immobilien   5.9
    ## Symrise AG Inha  0.5

    png("~/Trading/aktienMDAX.png", width = 600, height = 400)

    ids <- c("de0007042301", "de0007297004", "de000tlx1005",
             "de000kd88880", "de0008303504", "de0007235301",
             "de0005772206", "lu0269583422", "de000gsw1111",
             "de000sym9999")
    w <- c(15.0, 15.0, 15.0, 12.3, 12.0, 9.4, 8.3, 6.5, 5.9, 0.5)/100
    w <- w/sum(w)
    data <- getTablesSelect(ids, "daily",
                            from = "20130311",
                            to   = "20130508",
                            columns = "close")

    u <- w/data$close[1,]
    plot(char2time(data$times), data$close %*% u, type = "l",
         ylim = c(0.96,1.04), xlab = "", ylab = "", col = "goldenrod3")
    sd(returns(data$close %*% u))*16
    
    data <- getTablesSelect("de0008467416", "indices",
                            from = "20130311",
                            to   = "20130508",
                            columns = "close")

    lines(char2time(data$times), data$close/data$close[1L], col = "blue")
    sd(returns(data$close/data$close[1L]))*16
    legend(x="topleft", legend = c("Portfolio", "MDAX"),
           col= c("goldenrod3","blue"), lwd=2)
    dev.off()






    

                                        # BINARY SEARCH
    ## x
    ## key, min, max, what = undef, first, last


    bs <- function(x, t, duplicates = "undef") {
        imin <- 1L 
        imax <- length(x)
        count <- 0L
        if (duplicates == "undef") {
            while (imax >= imin) {
                imid <- as.integer(imin + (imax - imin)/2)
                message("step ", count <- count + 1L, " -- ", imid)
                if (x[imid] > t)
                    imax <- imid - 1L
                else if (x[imid] < t)
                    imin <- imid + 1L
                else
                    break
            }
            imid
        } else if (duplicates == "last") {
            while (imax >= imin) {
                imid <- as.integer(imin + (imax - imin)/2)
                message("step ", count <- count + 1L, " -- ", imid)
                if (x[imid] > t)
                    imax <- imid - 1L
                else
                    imin <- imid + 1L
            }
            imid
        }
    }
    x <- sort(c(rnorm(1000000), 0.5))
    t <- 0.5    
    system.time(bs(x,t, "undef"))
    system.time(match(t, x))
    
    x <- rep(1, 10000)
    t <- 1
    bs(x,t)
    bs(x,t, "last")

    
    x <- 1:1000
    x[599:820] <- 700
    t <- 700
    bs(x,t, "undef")
    bs(x,t, "last")



    bs <- function(x, t) {
        lo <- 1L 
        hi <- length(x)
        count <- 0
        while (lo <= hi) {
            mid <- as.integer(lo + (hi - lo)/2)
            message("step ", count <- count + 1, "  ", mid)
            if (x[mid] > t)
                hi <- mid - 1L
            else if (x[mid] < t)
                lo <- mid + 1L
            else break
        }
        mid
    }



    x <- sort(c(rnorm(1000000), 0.5))
    t <- 0.5    
    bs(x,t)
    
    x <- rep(1, 10000)
    t <- 1
    bs(x,t)

    
    x <- 1:1000
    x[599:820] <- 700
    t <- 700
    bs(x,t)
    t <- 821.1
    x[bs(x,t)]



    
                                        # intraday




    ## backtest alt

    if (FALSE) {
    backtest <- function(prices,         ### matrices
                         signal,         ### a function
                         signalYN = TRUE, ### a function
                         rebalanceYN = TRUE, ###
                         printInfo = NULL,   ###
                         b = 1L,             ### burnin
                         phi = 1,            ### how much to rebalance
                         x0 = 0,             ### initial portfolio
                         c0 = 100,           ### initial cash
                         tc = 0, ...,
                         adjustSignal = NULL,
                         positionSize = 1,
                         tradeOnOpen = TRUE,
                         tol = 1e-5) {

        ## TODO: checks

        ## TODO: write useful tests

        getOpen <- function(lag = 1L)
            mO[t - lag, ,drop = FALSE]
        getHigh <- function(lag = 1L)
            mH[t - lag, ,drop = FALSE]
        getLow <- function(lag = 1L)
            mL[t - lag, ,drop = FALSE]
        getClose <- function(lag = 1L)
            mC[t - lag, ,drop = FALSE]

        doSignalYN <- TRUE
        if (is.null(signalYN)) {
            signalYN <- function() TRUE
            doSignalYN <- FALSE
        } else if (identical(signalYN, TRUE)) {
            signalYN <- function() TRUE
            doSignalYN <- TRUE
        } else if (identical(signalYN, FALSE)) {
            signalYN <- function() FALSE
            warning("'signalYN' is FALSE: this strategy will never trade")
        }

        doRebalanceYN <- TRUE
        if (is.null(rebalanceYN)) {
            rebalanceYN <- function() TRUE
        } else if (identical(rebalanceYN, TRUE)) {
            rebalanceYN <- function() TRUE
        } else if (identical(rebalanceYN, FALSE)) {
            rebalanceYN <- function() FALSE
        }

        doPrintInfo <- TRUE
        if (is.null(printInfo)) {
            doprintInfo <- FALSE
            printInfo <- function() NULL
        }
        if (is.null(adjustSignal))
            adjSignal <- FALSE else adjSignal <- TRUE

        ## assign the ... arguments
        vars <- list(...)
        if (length(vars)) {
            for(i in seq_len(length(vars)))
                assign(names(vars)[i], vars[[i]])
        }

        ## prepare prices
        if (is.list(prices)) {
            if (length(prices) == 1L) {
                mC <- as.matrix(prices[[1L]])
                tradeOnOpen <- FALSE
            } else if (length(prices) == 4L) {
                mO <- as.matrix(prices[[1L]])
                mH <- as.matrix(prices[[2L]])
                mL <- as.matrix(prices[[3L]])
                mC <- as.matrix(prices[[4L]])
            } else {
                stop("see documentation on 'prices'")
            }
        } else {
            prices <- as.matrix(prices)
            if (ncol(prices) == 1L) {
                mC <- as.matrix(prices)
                tradeOnOpen <- FALSE
            } else if (ncol(prices) == 4L) {
                mO <- as.matrix(prices[ ,1L])
                mH <- as.matrix(prices[ ,2L])
                mL <- as.matrix(prices[ ,3L])
                mC <- as.matrix(prices[ ,4L])
            } else {
                stop("see documentation on 'prices'")
            }
        }
        rm(prices)

        environment(signalYN) <- environment()
        environment(rebalanceYN) <- environment()
        environment(signal) <- environment()
        environment(printInfo) <- environment()

        ## param .... settings
        T <- nrow(mC)
        nA <- ncol(mC)

        ## tc can be of length nA or length 1L
        tccum <- numeric(T)

        ## X  .... actual portfolios over time
        ## Xs .... signals (recommended)
        X  <- array(NA, dim = c(T, nA))
        Xs <- array( 0, dim = c(T, nA))
        colnames(X) <- colnames(mC)
        colnames(Xs) <- colnames(mC)

        X[b, ] <- x0
        cash <- numeric(T)
        cash[b] <- c0
        v <- numeric(T)
        v[] <- NA
        v[b] <- c0 + x0 %*% mC[b, ]

        for ( t in (b+1L):T ) {
            t1 <- t-1L

            ## COMPUTE SIGNAL?
            computeSignal <- signalYN()

            if (computeSignal) {
                temp <- signal()
                if (adjSignal) {
                    switch(adjustSignal,
                           fixedPosition = {
                               temp <- positionSize *  temp
                           },
                           fixedWeight = {
                               temp <- positionSize * temp * v[t1] / mC[t1, ]
                           },
                           variableWeight = {
                               temp <- temp * v[t1] / mC[t1, ]
                           },
                           stop("unknown value for 'adjustSignal'")
                           ) ### end switch
                }
                Xs[t, ] <- temp
                computeSignal <- FALSE
            } else {
                Xs[t, ] <- Xs[t1, ]
            }

            ## REBALANCE?
            rebalance <- rebalanceYN()

            dXs <- Xs[t, ] - Xs[t1, ]
            if ( max(abs(dXs)) < tol ) rebalance <- FALSE

            if (rebalance) {
                dx <- phi * dXs
                if (tradeOnOpen) {
                    open <- mO[t, ] ### will convert to vector (drop = TRUE)
                } else {
                    open <- mC[t, ] ### will convert to vector (drop = TRUE)
                }
                sx <- dx %*% open
                abs_sx <- (abs(dx) * tc) %*% open
                tccum[t] <- tccum[t1] + abs_sx
                cash[t] <- cash[t1] - sx - abs_sx
                X[t, ] <- X[t1, ] + dx
                rebalance <- FALSE
            } else {
                tccum[t] <- tccum[t1]
                cash[t] <- cash[t1]
                X[t, ] <- X[t1, ]
            }

            ## WEALTH
            v[t] <- X[t, ] %*% mC[t, ] + cash[t]
            if (doPrintInfo) printInfo()
        } ### end of loop

        ## return list of suggested and actual positions
        list(X = X, Xs = Xs, cash = cash, v = v, tccum = tccum)
    }
}

}
