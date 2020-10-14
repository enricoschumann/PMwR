expect_equal(summary(journal()),
             structure(list(n_transactions = 0L, stats = NA),
                       .Names = c("n_transactions",
                                  "stats"),
                       class = "summary.journal"))
