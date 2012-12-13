localTesting <- TRUE
if (require("RUnit", quietly = TRUE)) {
    require("PM")
    if (localTesting)
        path <- "~/Packages/PM/inst/unitTests" else
    path <- system.file("unitTests", package = "PM")

    myTestSuite <- defineTestSuite("PM",
                                   dirs = path,
                                   testFileRegexp = "unitTests.+")
    stopifnot(isValidTestSuite(myTestSuite))
    testResult <- runTestSuite(myTestSuite, verbose = 0L)
    printTextProtocol(testResult, showDetails = TRUE,
                      fileName = paste(file.path(path, "report"),
                      ".txt", sep = ""))
}
