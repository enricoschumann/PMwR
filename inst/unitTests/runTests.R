pkg <- "PMwR"
if (tolower(Sys.getenv("ES_PACKAGE_TESTING")) == "true" &&
    require("RUnit", quietly = TRUE) &&
    require(pkg, quietly = TRUE, character.only = TRUE)) {

    path <- "~/Packages/PMwR/inst/unitTests"

    ## if the package is installed and you want to run
    ## the test, use this path:    
    ##
    ##     path <- system.file("unitTests", package = "PMwR")
    ##
    
    myTestSuite <- defineTestSuite(pkg,
                                   dirs = path,
                                   testFileRegexp = "ut_.*")
    stopifnot(isValidTestSuite(myTestSuite))
    testResult <- runTestSuite(myTestSuite, verbose = 0L)
    printTextProtocol(testResult,
                      showDetails = TRUE,
                      fileName = paste0(file.path(path, "test_results"), ".txt"))
}
