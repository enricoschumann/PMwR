if (requireNamespace("tinytest", quietly = TRUE))
    tinytest.results <- tinytest::test_package("PMwR",
                                               color = interactive(),
                                               verbose = 1)
