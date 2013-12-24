

swiftTest_6.1.1 <- function () {
    swiftTest_6.1(2, 20)
}

sleeper <- function(delay,ncalls) {
  options(swift.initialexpr="initVar3 <- 123; initVar4 <- 100");

  timed <- function(delay) { Sys.sleep(delay); delay }

  args=list(delay)
  arglist = rep(list(args),ncalls)

  cat("\nTest of swiftapply(delay,arglist)\n")

  startTime = proc.time()[["elapsed"]]
  swiftres = swiftapply(timed,arglist)
  endTime = proc.time()[["elapsed"]]
  runTime <- endTime - startTime

  cat("\n\n ===> Ran for ",runTime," seconds.\n\n") 

  cat("Swift result:\n")
  print(swiftres[[1]])

  if(identical(delay,swiftres[[1]])) {
    cat("\n==> sleeper passed\n")
    return (TRUE)
  } else {
    cat("\n==> sleeper FAILED !!!!!\n")
    return (FALSE)
  }

}

id <- function () { 
    swiftLapply(rep(list(1), 100), 
                    function (x) {x}) 
}

mkPerfTest <- function() {
    # Have an initial test that will block until
    # resources are ready
    warmUpGroup <- makeTestGroup(
        name="Wait for server to start",
        tests=rep(list(mkTest(id)), 5))

    perfTestGroup1 <- makeParamTestGroup( 
        name="1 - basic performance test",
        f=sleeper,
        allargs=list(
            list(1, 8),
            list(1, 16),
            list(1, 32),
            list(1, 48),
            list(1, 64),
            list(1, 96),
            list(1, 128),
            list(2, 8),
            list(2, 16),
            list(2, 32),
            list(2, 48),
            list(2, 64),
            list(2, 96),
            list(2, 128),
            list(3, 8),
            list(3, 16),
            list(3, 32),
            list(3, 48),
            list(3, 64),
            list(3, 96),
            list(3, 128),
            list(4, 8),
            list(4, 16),
            list(4, 32),
            list(4, 48),
            list(4, 64),
            list(4, 96),
            list(4, 128),
            list(5, 8),
            list(5, 16),
            list(5, 32),
            list(5, 48),
            list(5, 64),
            list(5, 96),
            list(5, 128)))
    swiftTestSuite <- list(
            setup=function () { initSwiftTestOptions();
                               swiftInit() }, # swiftInit controlled via options
            groups=list(warmUpGroup, perfTestGroup1), 
            teardown=function () { swiftShutdown() })
    return (swiftTestSuite)
}

doPerfTest <- function () {
    runTestSuite(mkPerfTest())
}
