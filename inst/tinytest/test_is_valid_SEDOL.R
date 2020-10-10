SEDOL <- c("0263494", "B1F3M59", "0263491", "A", NA)

ans <- is_valid_SEDOL(SEDOL)
expect_equivalent(ans, c(TRUE, TRUE, FALSE, FALSE, NA))

ans <- is_valid_SEDOL(SEDOL, NA.FALSE = TRUE)
expect_equivalent(ans, c(TRUE, TRUE, FALSE, FALSE, FALSE))
