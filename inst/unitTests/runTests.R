localTesting <- TRUE
if (require("RUnit", quietly = TRUE)) {
    require("PMwR")
    if (localTesting)
        path <- "~/Packages/PMwR/inst/unitTests" else
    path <- system.file("unitTests", package = "PMwR")
    myTestSuite <- defineTestSuite("PMwR",
                                   dirs = path,
                                   testFileRegexp = "ut_.*")
    stopifnot(isValidTestSuite(myTestSuite))
    testResult <- runTestSuite(myTestSuite, verbose = 0L)
    printTextProtocol(testResult, showDetails = TRUE,
                      fileName = paste(file.path(path, "report"),
                      ".txt", sep = ""))
}
