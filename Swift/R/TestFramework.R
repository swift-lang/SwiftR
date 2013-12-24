# Generic functions for running and displaying results
# from a suite of tests

mkTest <- function (f, prep=NULL) {
    test = list()
    test$name = deparse(substitute(f))
    test$prep = prep
    test$fun = f
    test$passed = NULL
    test$time <- NULL
    return (test)
}

makeTestGroup <- function (name, tests, setup=NULL, teardown=NULL) {
    group = list()
    group$name <- name
    group$setup = setup
    group$tests = tests
    group$teardown = teardown
    return (group)
}

makeParamTestGroup <- function (name, f, allargs,
                setup=NULL, teardown=NULL, perfparams=NULL,
                prep=NULL) {


    buildClosure <- function (f, args) {
        force(f)
        force(args)
        return (function () { do.call(f, args) })
    }
    fname = deparse(substitute(f))

    tests = list()
    for (args in allargs) {
        test = list()
        args <- as.list(args)
        test$name = paste(fname, substring(deparse(args), 5) , sep="") 
        if (!is.null(prep)) {
            test$fun <- f
            test$prep <- buildClosure(prep, args)
        }
        else {
            test$fun <- buildClosure(f, args)
        }
        
        test$args <- args # Store for later analysis
        test$passed <- NULL
        test$time <- NULL
        test$perfparams <- perfparams
        tests[[length(tests) + 1]] <- test
    }

    return (makeTestGroup(name, tests, setup, teardown))
}

analyseSuitePerf <- function (testSuite, ...) {
    # See analyse perf below: does the same for a test suite and returns a list of the results
    # for each group
    lapply(testSuite$groups, 
        function (g) { 
            res <- analysePerf(g, ...) 
            n <- length(res[[1]])
            res$group <- rep(g$name, n)
            return (res)
        })
}

analysePerf <- function (testGroupResults, argnames=NULL, perfparams=NULL, includePassed=FALSE) {
    # Build a list of vectors of performance results for easier analysis
    # argnames: names to give to the arguments to the test function, ordered by position  
    # The number here is the number of arguments that are extracted from 
    # the test result data.
    # perfparams: names of the parameters passed to swfit/snowfall init functions
    #   that you want to include in the final data

    # [[1]] -> first arg
    # [[2]] -> second arg
    # ...   ... .......
    # [[n]] -> nth arg
    # $name 
    # $passed 
    # $time
    collated <- list()
    if (length(argnames) >= 1) {
        for (i in 1:length(argnames)) {
            collated[[i]] <- unlist(lapply(testGroupResults$tests, 
                            function(test) {test$args[[i]]}))
            names(collated)[[i]] <- argnames[[i]]
        }
    }
    n <- length(testGroupResults$tests)

    collated$name <- unlist(lapply(testGroupResults$tests, 
                        function(test) {test$name }))
    if (includePassed) {
        collated$passed <-unlist(lapply(testGroupResults$tests, 
                            function(test) {test$passed }))
    }
    collated$time <- unlist(lapply(testGroupResults$tests, 
                        function(test) {test$time }))
    
    # Store the pnames as vectors
    if (!is.null(perfparams)) {
        for (pname in perfparams) {
            collated[[pname]] <- unlist(lapply(testGroupResults$tests, 
                        function(test) {test$perfparams[[pname]]} ))
        }
    }
    return (collated)
}


# Takes a list of analysed group results from analysePerf and combines them into 
# a single data frame.  This has to assume that all data in these lists is valid
# for inclusion in a data frame.  Merge rules are those of the built-in merge function
mergeGroupResults <- function (groupResults) {
    frames <- lapply(groupResults, data.frame)
    res <- frames[[1]]
    for (frame in frames[2:length(frames)]) {
        res <- merge(res, frame, all=T)
    }
    return (res)
}

runTestGroup <- function (group) {
    cat("\n*** Starting test group ", group$name, "***\n")
    if (!is.null(group$setup))
        group$setup()
    for (i in 1:length(group$tests)) {
        test <- group$tests[[i]]
        cat("\n== Starting test ", test$name, " ==\n")
        if (!is.null(test$prep)) {
            prep <- try(test$prep())
            if (inherits(res, "try-error")) {
                print("prep failed")
                group$tests[[i]]$passed <- res
                group$tests[[i]]$time <- 0
            }
            else {
                print("prep ran")
                startTime = proc.time()[["elapsed"]]
                group$tests[[i]]$passed <- try(test$fun(prep))
                endTime <- proc.time()[["elapsed"]]
                runTime <- endTime - startTime
                group$tests[[i]]$time <- runTime
            }
        }
        else {
            startTime = proc.time()[["elapsed"]]
            group$tests[[i]]$passed <- try(test$fun())
            endTime <- proc.time()[["elapsed"]]
            runTime <- endTime - startTime
            group$tests[[i]]$time <- runTime
        }
        
    }
    if (!is.null(group$teardown))
        group$teardown()
    return (group)
}

printTestGroup <- function (group) {
    cat("\n*** Test group ", group$name, "***\n")
    resFmt <- function(res) { 
            if (is.null(res)) {
                "???"
            } else if (inherits(res, "try-error")) {
                    paste("ERROR: '", res, "'") 
            } else if (identical(res, TRUE)) {
                    "PASSED" 
            } else if (identical(res, FALSE)) {
                    "FAILED"
            } else {
                    "<data>"
            }
        }
    timeFmt <- function (time) {
        if (is.null(time)) 
            "???"
        else    
            sprintf("%.2fs", time)
    }
    for (test in group$tests) {
        cat(paste(test$name, ": ", resFmt(test$passed), 
                " (", timeFmt(test$time), ")\n", sep=""))
    }
}

makeTestSuite <- function(groups, setup=NULL, teardown=NULL) {
    return (list(groups=groups, setup=setup, teardown=teardown))
}

runTestSuite <- function (suite) {
    if (!is.null(suite$setup))
        suite$setup()
    resSuite <- suite
    resSuite$groups <- lapply(suite$groups, runTestGroup)
    
    if (!is.null(suite$teardown))
        suite$teardown()

    lapply(resSuite$groups, printTestGroup)
    return (resSuite)
}
