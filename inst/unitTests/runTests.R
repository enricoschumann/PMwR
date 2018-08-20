pkg <- "PMwR"
if (tolower(Sys.getenv("ES_PACKAGE_TESTING")) == "true" &&
    require("RUnit", quietly = TRUE) &&
    require(pkg, quietly = TRUE, character.only = TRUE)) {

    path <- paste0("~/Packages/", pkg, "/inst/unitTests")

    ## if the package is installed and you want to run
    ## the tests, use this path:    
    ##
    ##     path <- system.file("unitTests", package = pkg)
    ##
    
    myTestSuite <- defineTestSuite(pkg,
                                   dirs = path,
                                   testFileRegexp = "ut_.*[.]R$")
    stopifnot(isValidTestSuite(myTestSuite))
    testResult <- runTestSuite(myTestSuite, verbose = 0L)
    printTextProtocol(testResult,
                      showDetails = TRUE,
                      fileName = paste0(file.path(path, "test_results"), ".txt"))
}
