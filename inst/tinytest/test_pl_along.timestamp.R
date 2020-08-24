## timestamps passed via 'along.timestamp' get sorted

ans <- pl(amount = c(1, -1),
          timestamp = c(2, 3),
          price = c(1, 2),
          along.timestamp = 3:1,
          vprice = 1:3)

expect_equal(ans[[1]]$timestamp , 1:3)

expect_equal(unname(ans[[1]]$pl), c(0,1,1))
