x <- NAVseries(101:110, 1:10)

expect_equal(window(x, 2, 3),
             structure(102:103, timestamp = 2:3,
                       description = character(0),
                       class = "NAVseries"))
expect_equal(window(x), x)
