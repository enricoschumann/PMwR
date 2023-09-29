initial.pos   <- c(A = 100, B = 20, C = -50)
initial.price <- c(A =   1, B =  2, C =   3)

## trades <- read.table(text = "
## instrument,  timestamp, amount, price
##          A, 2023-09-26,      5,  1.2
##          A, 2023-09-26,     -2,  1.5
## ", sep = ",", header = TRUE, strip.white = TRUE)

trades <- structure(list(
    instrument = c("A", "A"),
    timestamp = c("2023-09-26",
                  "2023-09-26"),
    amount = c(5L, -2L),
    price = c(1.2, 1.5)),
    class = "data.frame", row.names = c(NA, -2L))

###   instrument  timestamp amount price
### 1          A 2023-09-26      5   1.2
### 2          A 2023-09-26     -2   1.5



txt <- capture.output(pl(trades,
                         vprice = c(A = 2, B = 2.1, C = 2),
                         initial.position = initial.pos,
                         initial.price = initial.price))

i <- grepl("P/L total ", txt)
expect_equal(
    as.numeric(sub(".*P/L total + ([0-9.,-]+).*", "\\1", txt[i])),
    c(103, 2, 50))
