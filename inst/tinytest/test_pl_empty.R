## Tests for computing 'pl' from an *empty* 'journal'

j <- journal()
ans <- pl(j)  ## no timestamps
expect_equal(ans[[1]]$pl , 0) 



## along.timestamp ==> TRUE
ans <- pl(j, along.timestamp = TRUE)  ## no timestamps

pl(amount = numeric(0),
   price = numeric(0),
   initial.position = 5,
   initial.price = 1)

## along.timestamp ==> custom-timestamp
## ans <- pl(j, along.timestamp = 1:3, vprice = c(10,10,10))  ## no timestamps
