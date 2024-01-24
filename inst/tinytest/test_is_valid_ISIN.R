isin <- c("US0378331005",
          "AU0000XVGZA3",
          "DE000A0C3743",
          "not_an_isin")
expect_equal(unname(is_valid_ISIN(isin)),
             c(TRUE, TRUE,  TRUE, FALSE))

## case is ignored
expect_equal(unname(is_valid_ISIN(c("US0378331005",
                                    "us0378331005"))),
             c(TRUE, TRUE))

isin <- c("US0378331005",
          "AU00x0XVGZxx")
expect_equal(unname(is_valid_ISIN(isin)),
             c(TRUE, FALSE))

isin <- character(0L)
expect_equal(unname(is_valid_ISIN(isin)),
             logical(0L))

isin <- NA
expect_equal(unname(is_valid_ISIN(isin)),
             FALSE)

isin <- NA
expect_equal(unname(is_valid_ISIN(isin, NA.FALSE = FALSE)),
             FALSE)


isin <- c(NA, "US0378331005")
expect_equal(unname(is_valid_ISIN(isin)),
             c(NA, TRUE))

isin <- c(NA, "US0378331005")
expect_equal(unname(is_valid_ISIN(isin, NA.FALSE = TRUE)),
             c(FALSE, TRUE))
