
expect_equal(q32("100-170"),
             structure(100.53125,
                       handle = 100,
                       ticks = 17,
                       fraction = 0,
                       class = "quote32"))

expect_equal(q32("100-172"),
             structure(100.5390625,
                       handle = 100,
                       ticks = 17,
                       fraction = 1,
                       class = "quote32"))

expect_equal(q32("100-175"),
             structure(100.546875,
                       handle = 100,
                       ticks = 17,
                       fraction = 2,
                       class = "quote32"))

expect_equal(q32("100-177"),
             structure(100.5546875,
                       handle = 100,
                       ticks = 17,
                       fraction = 3,
                       class = "quote32"))


expect_equivalent(as.numeric(q32("109")),     109)
expect_equivalent(as.numeric(q32("109'00+")), 109 + 1/32/2)
expect_equivalent(as.numeric(q32("109'10")),  109 + 10/32)
expect_equivalent(as.numeric(q32("109-047")), 109+4.75/32)
expect_equal(q32("127-00+"), q32("127-005"))

expect_equal(q32("127:00+"), q32("127'005"))
