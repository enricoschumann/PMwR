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
