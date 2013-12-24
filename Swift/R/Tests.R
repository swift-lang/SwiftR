

prereqTest <- function() {
    # Check that prerequisites exist
    java <- system("java -version")
    if (java != 0) {
        cat("Java could not be found on path, Swift will not work\n")
    }
    perl <- system("/usr/bin/perl -v")
    if (perl != 0) {
        cat("Perl could not be found in /usr/bin, Swift will not work\n")
    }

    bash <- system("/usr/bin/env bash -version")
    if (bash != 0) {
        cat("Bash could not be found on path, Swift will not work\n")
    }

    return (java == 0) && (perl == 0) && (bash == 0)
}

basicSwiftTest <- function(...) { 
    swiftInit(...)
    testRes <- swiftTest_1.1() 
    swiftShutdown()
    return (invisible(testRes))
}

swiftTest_1.1 <- function() {

  cat("\n*** Starting  test 1.1 ***\n\n")

  sumstuff <- function(treedata,cardata) { sum( treedata$Height, cardata$dist ) }
  data(cars)
  data(trees)

  args=list(trees,cars)
  arglist = rep(list(args),1)

  cat("Test of local do.call(sumstuff)\n")
  localres = do.call(sumstuff,args)
  cat("local result=\n")
  print(localres)

  cat("\nTest of swiftapply(sumstuff,arglist)\n")
  swiftres = swiftapply(sumstuff,arglist)
  cat("Swift result:\n")
  print(swiftres)

  if(identical(localres,swiftres[[1]])) {
    cat("\n==> test 1.1 passed\n")
    return (TRUE)
  } else {
    cat("\n==> test 1.1 FAILED !!!!!\n")
    return (FALSE)
  }
}


swiftTest_1.2.1 <- function () {
    # test 10 remote calls
    sumstuff <- function(treedata,cardata) 
        { sum( treedata$Height, cardata$dist ) }
    data(cars)
    data(trees)

    args=list(trees,cars)
    arglist <- rep(list(args),10)

    localres = do.call(sumstuff,args)

    swiftres <- swiftapply(sumstuff,arglist)
    cat("Swift result:\n")
    format(swiftres)

    diffs <- 0
    for(i in 1:length(swiftres) ) {
      if( !identical(swiftres[[i]],localres) ) { 
        diffs <- diffs + 1
        if( diffs < 10 ) cat(sprintf("res[%d]=%s\n",i,format( swiftres[[i]] )))
      }
    }

    if(diffs == 0) {
      cat("\n==> test 1.2.1 passed\n")
      return (TRUE)
    } else {
      cat("\n!!!==> test 1.2.1 failed.\n")
      cat(sprintf(" %d result elements failed to match.\n",diffs));
      return (FALSE)
    }
}

swiftTest_1.2.2 <- function () {
    sumstuff <- function(treedata,cardata) 
        { sum( treedata$Height, cardata$dist ) }
    data(cars)
    data(trees)

    args=list(trees,cars)
    arglist <- rep(list(args),10)
    
    localres = do.call(sumstuff,args)

    cat("*** 10 calls to sumstuff() - callsperbatch=10\n")
    swiftres = swiftapply(sumstuff,arglist,callsperbatch=10)
    cat("Swift result:\n")
    format(swiftres)

    diffs <- 0
    for(i in 1:length(swiftres) ) {
      if( !identical(swiftres[[i]],localres) ) { 
        diffs <- diffs + 1
        if( diffs < 10 ) cat(sprintf("res[%d]=%s\n",i,format( swiftres[[i]] )))
      }
    }

    if(diffs == 0) {
      cat("\n==> test 1.2.2 passed\n")
      return (TRUE)
    } else {
      cat("\n!!!==> test 1.2.2 failed.\n")
      cat(sprintf(" %d result elements failed to match.\n",diffs));
      return (FALSE)
    }
}

swiftTest_1.2.3 <- function () {
    sumstuff <- function(treedata,cardata) 
        { sum( treedata$Height, cardata$dist ) }
    data(cars)
    data(trees)

    args=list(trees,cars)
    arglist <- rep(list(args),10)
    
    localres = do.call(sumstuff,args)

    cat("*** 10 calls to substuff() - callsperbatch=2\n")
    swiftres = swiftapply(sumstuff,arglist,callsperbatch=2)
    cat("Swift result:\n")
    format(swiftres)

    diffs <- 0
    for(i in 1:length(swiftres) ) {
      if( !identical(swiftres[[i]],localres) ) { 
        diffs <- diffs + 1
        if( diffs < 10 ) cat(sprintf("res[%d]=%s\n",i, format( swiftres[[i]] )))
      }
    }

    if(diffs == 0) {
      cat("\n==> test 1.2.3 passed\n")
      return (TRUE)
    } else {
      cat("\n!!!==> test 1.2.3 failed.\n")
      cat(sprintf(" %d result elements failed to match.\n",diffs));
      return (FALSE)
    }

}

swiftTest_1.2.5 <- function () {
    # Test null corner case
    f <- function (x) {NULL}
    inl <- list(1,2)

    localres = lapply(inl, f)
    cat("Local result:\n")
    print(localres)

    swiftres = swiftLapply(inl,f)
    cat("Swift result:\n")
    print(swiftres)

    diffs <- 0
    for(i in 1:length(swiftres) ) {
      if( !identical(swiftres[[i]],localres[[i]]) ) { 
        diffs <- diffs + 1
        if( diffs < 10 ) cat(sprintf("res[%d]=%s\n",i, format( swiftres[[i]] )))
      }
    }
    if(diffs == 0) {
      cat("\n==> test 1.2.5 passed\n")
      return (TRUE)
    } else {
      cat("\n!!!==> test 1.2.5 failed.\n")
      cat(sprintf(" %d result elements failed to match.\n",diffs));
      return (FALSE)
    }

}


swiftTest_1.2.4 <- function () {
    sumstuff <- function(treedata,cardata) 
        { sum( treedata$Height, cardata$dist ) }
    data(cars)
    data(trees)

    args=list(trees,cars)
    arglist <- rep(list(args),10)
    
    localres = do.call(sumstuff,args)
    
    cat("*** 10 calls to substuff() - callsperbatch=3\n")
    swiftres = swiftapply(sumstuff,arglist,callsperbatch=3)
    swiftres <- swiftapply(sumstuff,arglist)
    cat("Swift result:\n")
    format(swiftres)

    diffs <- 0
    for(i in 1:length(swiftres) ) {
      if( !identical(swiftres[[i]],localres) ) { 
        diffs <- diffs + 1
        if( diffs < 10 ) cat(sprintf("res[%d]=%d\n",i, format( swiftres[[i]] )))
      }
    }

    if(diffs == 0) {
      cat("\n==> test 1.2.4 passed\n")
      return (TRUE)
    } else {
      cat("\n!!!==> test 1.2.4 failed.\n")
      cat(sprintf(" %d result elements failed to match.\n",diffs));
      return (FALSE)
    }
}

#1.3.* tests export functionality
swiftTest_1.3.1 <- function() {

  cat("\n*** Starting  test 1.3.1 ***\n\n")
  y <- 1
  inc <- function(x) { x + y }

  arglist = list(1,2,3,4,5,6,7,8,9)

  cat("Test of local lapply\n")
  localres = lapply(arglist, inc)
  cat("local result=\n")
  print(localres)

  # Test swiftExport
  swiftExport(y)
  rm(y) # Remove y here so that swift can't resolve locally
  cat("\nTest of swiftLapply\n")
  swiftres = swiftLapply(arglist, inc)
  cat("Swift result:\n")
  print(swiftres)

  if(identical(localres,swiftres)) {
    cat("\n==> test 1.3.1 passed\n")
    return(TRUE)
  } else {
    cat("\n==> test 1.3.1 FAILED !!!!!\n")
    return(FALSE)
  }
}

swiftTest_1.3.2 <- function() {
 # Test swiftExportAll()

  cat("\n*** Starting  test 1.3.2 ***\n\n")
  # put in global environment so we can test global env
  swift.tmp.y <<- 1
  inc <- function(x) { x + swift.tmp.y }

  arglist = list(1,2,3,4,5,6,7,8,9)

  cat("Test of local lapply\n")
  localres = lapply(arglist, inc)
  cat("local result=\n")
  print(localres)

  # Test swiftExport
  swiftExportAll()

  # Remove y here so that swift can't resolve locally
  rm(swift.tmp.y, envir=globalenv())   
  cat("\nTest of swiftLapply\n")
  swiftres = swiftLapply(arglist, inc)
  cat("Swift result:\n")
  print(swiftres)

  if(identical(localres,swiftres)) {
    cat("\n==> test 1.3.2 passed\n")
    return(TRUE)
  } else {
    cat("\n==> test 1.3.2 FAILED !!!!!\n")
    return (FALSE)
  }
}


swiftTest_1.3.3 <- function() {

  cat("\n*** Starting  test 1.3.3 ***\n\n")
  y <- 1
  inc <- function(x) { x + y }

  arglist = list(1,2,3,4)

  # Test swiftExportAll
  swiftExportAll()
  rm(y) # Remove y here so that swift can't resolve locally
  cat("\nTest of swiftLapply\n")
  swiftres1 <- swiftLapply(arglist, inc)
  cat("Swift result before removal:\n")
  print(swiftres1)

  swiftRemoveAll()
  inc2 <- function(x) { x + y }

  swiftres2 <- swiftLapply(arglist, inc)
  cat("Swift result after removal:\n")
  print(swiftres2)
  for (e in swiftres2) {
    if (!inherits(e, "try-error")) {
        cat("\n==> test 1.3.3 FAILED !!!!!\n")
        return (FALSE)
        return()
    }
  }
  cat("\n==> test 1.3.3 passed\n")
  return(TRUE)
}

swiftTest_1.3.4 <- function () {
  # This test demonstrates a case where a variable
  # does not need to be exported.
  cat("\n*** Starting  test 1.3.4 ***\n\n")
  swiftRemoveAll() # Cleanup
  y <- 1
  inc <- function(x) { x + y }
  # DONT export
  arglist = list(1,2,3,4,5,6,7,8,9)

  cat("Test of local lapply\n")
  localres = lapply(arglist, inc)
  cat("local result=\n")
  print(localres)

  cat("\nTest of swiftLapply\n")
  swiftres = swiftLapply(arglist, inc)
  cat("Swift result:\n")
  print(swiftres)

  if(identical(localres,swiftres)) {
    cat("\n==> test 1.3.4 passed\n")
    return(TRUE)
  } else {
    cat("\n==> test 1.3.4 FAILED !!!!!\n")
    return (FALSE)
  }
}


swiftTest_1.3.5 <- function () {
  # This test demonstrates a case where a variable
  # does not need to be exported 

  cat("\n*** Starting  test 1.3.5 ***\n\n")
  swiftRemoveAll() # Cleanup
  f_inc <- function (z) { z + 1 }
  f <- function () {
      # Use another function
      inc <- function(x) { f_inc(x) }
      # DONT export
      arglist = list(1,2,3,4,5,6,7,8,9)

      cat("Test of local lapply\n")
      localres = lapply(arglist, inc)
      cat("local result=\n")
      print(localres)

      cat("\nTest of swiftLapply\n")
      swiftres = swiftLapply(arglist, inc)
      cat("Swift result:\n")
      print(swiftres)

      if(identical(localres,swiftres)) {
        cat("\n==> test 1.3.5 passed\n")
        return (TRUE)
      } else {
        cat("\n==> test 1.3.5 FAILED !!!!!\n")
        return (FALSE)
      }
  }
  f()
}


swiftTest_2.1 <- function() {
    matfunc <- function( m1, m2 )
    {
      (1/m1) %*% m2
    }

    n <- 5
    m1 <- array(sin(1:n**2), dim=c(n,n))
    m2 <- t(m1)

    localres = matfunc(m1,m2)

    cat("\n*** Test 2.1: 100 calls to matfunc(dim=5x5) - callsperbatch=9\n")

    args=list(m1,m2)
    arglist <- rep(list(args),100)

    swiftres = swiftapply(matfunc,arglist,callsperbatch=9)

    diffs <- 0
    for(i in c(seq(1,100,10),100)) {
      if( !all.equal(swiftres[[i]],localres) ) {
        diffs <- diffs + 1
        if( diffs < 10 ) cat(sprintf("res[%d]=%s\n",i,format(swiftres[[i]])))
      }
    }

    if(diffs == 0) {
      cat("\n==> test 2.1 passed\n")
      return (TRUE)
    } else {
      cat("\n!!!==> test 2.1 failed.\n")
      cat(sprintf(" %d result elements failed to match.\n",diffs));
      return (FALSE)
    }
}

swiftTest_2.2 <- function() {
    matfunc <- function( m1, m2 )
    {
      (1/m1) %*% m2
    }
    n <- 237
    n <- 50
    m1 <- array(sin(1:n**2), dim=c(n,n))
    m2 <- t(m1)

    localres = matfunc(m1,m2)

    cat("\n*** 123 calls to matfunc(dim=bigger) - callsperbatch=7\n") # FIXME make n easy to adjust and print actual value

    args=list(m1,m2)
    arglist <- rep(list(args),123)

    swiftres = swiftapply(matfunc,arglist,callsperbatch=7)

    diffs <- 0
    for(i in c(seq(1,length(swiftres),10),length(swiftres))) {

      if( !all.equal(swiftres[[i]],localres) ) { 
        diffs <- diffs + 1
        if( diffs < 10 ) cat(sprintf("res[%d]=%s\n",i,format(swiftres[[i]])))
      }
    }

    if(diffs == 0) {
      cat("\n==> test 2.2 passed\n")
      return (TRUE)
    } else {
      cat("\n!!!==> test 2.2 failed.\n")
      cat(sprintf(" %d result elements failed to match.\n",diffs));
      return (FALSE)
    }
}

data_3.1 <- function () {
    n <- 5
    m1 <- array(sin(1:n**2), dim=c(n,n))
    m2 <- t(m1)

    inlist = list()
    inlist[[1]]=123
    inlist[[2]]=456
    inlist$name1=789
    inlist$name2=987
    inlist$mat1 =  m1
    inlist[[99]] = m2
    return (inlist)
}

listfunc_3.1 <- function(ilist) {
  olist = ilist
  olist$sum = ilist[[1]] + ilist[[2]] + ilist$name1 + ilist$name2
  olist$names = names(ilist)
  olist$mprod = ilist$mat1 %*% ilist[[99]]
  return(olist)
}

swiftTest_3.1 <- function() {
  #Test if list element names are being sent and returned correctly
    inlist <- data_3.1() 
    localres = listfunc_3.1(inlist)

    cat("\n*** Starting test 3.1 - 4 calls in one batch of 5 ***\n")

    args=list(inlist)
    arglist <- rep(list(args),4)

    swiftres = swiftapply(listfunc_3.1,arglist,callsperbatch=5)

    diffs <- 0
    for(i in 1:length(swiftres) ) {
      if( !all.equal(swiftres[[i]],localres) ) { 
        diffs <- diffs + 1
        if( diffs < 10 ) cat(sprintf("res[%d=%s\n",i,format(swiftres[[i]])))
      }
    }

    if(diffs == 0) {
      cat("\n==> test 3.1 passed\n")
      return (TRUE)
    } else {
      cat("\n!!!==> test 3.1 failed.\n")
      cat(sprintf(" %d result elements failed to match.\n",diffs));
      return (FALSE)
    }
}

swiftTest_3.2 <- function() {
    inlist <- data_3.1() 
    localres = listfunc_3.1(inlist)
    cat("\n*** Starting test 3.2 - 99 calls in batches of 11 ***\n")

    inlist <- data_3.1() 
    args=list(inlist)
    arglist <- rep(list(args),99)

    swiftres = swiftapply(listfunc_3.1,arglist,callsperbatch=11)

    diffs <- 0
    for(i in 1:length(swiftres) ) {
      if( !all.equal(swiftres[[i]],localres) ) { 
        diffs <- diffs + 1
        if( diffs < 10 ) cat(sprintf("res[%d]=%s\n",i,format(swiftres[[i]])))
      }
    }

    if(diffs == 0) {
      cat("\n==> test 3.2 passed\n")
      return (TRUE)
    } else {
      cat("\n!!!==> test 3.2 failed.\n")
      cat(sprintf(" %d result elements failed to match.\n",diffs));
      return (FALSE)
    }

}

swiftTest_4.1 <- function() {
  sumivars <- function() { initVar1+initVar2 }

  args=list()
  arglist = rep(list(args),1)

  localres = 42

  cat("\nTest of swiftapply(sumivars,arglist)\n")
  swiftres = swiftapply(sumivars,arglist)
  cat("Swift result:\n")
  print(swiftres)

  if(identical(localres,swiftres[[1]])) {
    cat("\n==> test 4.1 passed\n")
    return (TRUE)
  } else {
    cat("\n==> test 4.1 FAILED !!!!!\n")
    return (FALSE)
  }
}

swiftTest_4.2 <- function() {

  options(swift.initialexpr="initVar3 <- 123; initVar4 <- 100");

  mulivars <- function() { initVar3*initVar4 }

  args=list()
  arglist = rep(list(args),1)

  localres = 12300;

  cat("\nTest of swiftapply(mulivars,arglist)\n")
  swiftres = swiftapply(mulivars,arglist)
  cat("Swift result:\n")
  print(swiftres)

  if(identical(localres,swiftres[[1]])) {
    cat("\n==> test 4.2 passed\n")
    return (TRUE)
  } else {
    cat("\n==> test 4.2 FAILED !!!!!\n")
    return (FALSE)
  }
}

swiftTest_5.1 <- function () {
    # Test error handling
    cat("\n*** Starting test group 5 - test ***\n")

    arglist = list(list(1.0),list(2.0),list("3.0"),list(4.0),list(5.0))

    cat("\nTest of swiftapply(sumivars,arglist)\n")
    swiftres = swiftapply(log,arglist)
    cat("Swift result:\n")
    print(swiftres)

    goodres = c("numeric","numeric","try-error","numeric","numeric")

    diffs <- 0
    for(i in 1:length(swiftres) ) {
      if( class(swiftres[[i]]) != goodres[i] ) { 
        diffs <- diffs + 1
        if( diffs < 10 ) cat(sprintf("res[%d]=%s\n",i,format(swiftres[[i]])))
      }
    }

    if(diffs == 0) {
      cat("\n==> test 5.1 passed\n")
      return (TRUE)
    } else {
      cat("\n!!!==> test 5.1 failed.\n")
      cat(sprintf(" %d result elements failed to match.\n",diffs));
      return (FALSE)
    }
}
swiftTest_6.1 <- function () {
    # test sapply
    args <- list(1,2,3,4,5,6,7)
    parRes <- swiftSapply(args, log)
    serRes <- sapply(args, log)

    if (!all(parRes == serRes)) {
        cat("\n!!!==> test 6.1 failed.\n")
        cat(paste("actual: ", parRes, "\nexpected:", serRes, "\n"))
        return (FALSE)
    }
    if (is.list(parRes)) {
        cat("\n!!!==> test 6.1 failed.\n")
        cat("parRes is a list\n")
        return (FALSE)
    }
    
    cat("\n!!!==> test 6.1 passed.\n")
    return (TRUE)

}

swiftTest_6.2 <- function () {
    # test sapply
    args <- list(list(1,2),list(3),list(4,5),list(6,7))
    f <- function (x) {lapply(x, function(y) { y * 2 })}
    serRes <- sapply(args, f)
    parRes <- swiftSapply(args, f)


    if (!identical(parRes, serRes)) {
        cat("\n!!!==> test 6.2 failed.\n")
        cat(paste("actual: ", parRes, "\nexpected:", serRes, "\n"))
        return (FALSE)
    }
    if (!is.list(parRes)) {
        cat("\n!!!==> test 6.2 failed.\n")
        cat("parRes is not a list\n")
        return (FALSE)
    }
    
    cat("\n!!!==> test 6.2 passed.\n")
    return (TRUE)

}

swiftTest_6.3 <- function () {
    # test sapply forms matrix ok
    args <- list(list(1,2),list(3, 5),list(4,5),list(6,7))
    f <- function (x) {lapply(x, function(y) { y * 2 })}
    serRes <- sapply(args, f)
    parRes <- swiftSapply(args, f)


    if (!identical(parRes, serRes) || (!is.matrix(parRes))) {
        cat("\n!!!==> test 6.3 failed.\n")
        cat(paste("actual: ", parRes, "\nexpected:", serRes, "\n"))
        return (FALSE)
    }
    cat("\n!!!==> test 6.3 passed.\n")
    return (TRUE)
}

pkgexists <- function(pkgname) {
    paste("package:", pkgname, sep='') %in% search()
}

swiftTest_7.1 <- function () {
    # test swiftLIbrary passing symbolic arg

    try(detach(package:cluster),silent=T)
    # Test swiftLibrary default

    # cluster is pkg in R std lib
    swiftLibrary(cluster)

    # test
    loadedok <- swiftLapply(list("cluster"), pkgexists)[[1]]

    if (!loadedok) {
        cat("cluster should be loaded on remote host\n")
        cat("\n!!!==> test 7.1 failed")
        return (FALSE)
    }
    if (pkgexists("cluster")) {
        cat("cluster should not be loaded in this session\n")
        cat("\n!!!==> test 7.1 failed")
        return (FALSE)
    }

    swiftDetach(package:cluster)
    loadedok <- swiftLapply(list("cluster"), pkgexists)[[1]]
    if (loadedok) {
        cat("cluster should be detached on remote host\n")
        cat("\n!!!==> test 7.1 failed")
        return (FALSE)
    }

    cat("\n!!!==> test 7.1 passed.\n")
    return (TRUE)
}

swiftTest_7.2 <- function () {
    # Test swiftLibrary string 
    try(detach(package:cluster), silent=T)
    # Test swiftLibrary default

    # cluster is pkg in R std lib
    swiftLibrary("cluster")

    # test
    loadedok <- swiftLapply(list("cluster"), pkgexists)[[1]]

    if (!loadedok) {
        cat("cluster should be loaded on remote host\n")
        cat("\n!!!==> test 7.2 failed")
        return (FALSE)
    }
    if (pkgexists("cluster")) {
        cat("cluster should not be loaded in this session\n")
        cat("\n!!!==> test 7.2 failed")
        return (FALSE)
    }

    swiftDetach("package:cluster")
    loadedok <- swiftLapply(list("cluster"), pkgexists)[[1]]
    if (loadedok) {
        cat("cluster should be detached on remote host\n")
        cat("\n!!!==> test 7.2 failed")
        return (FALSE)
    }

    cat("\n!!!==> test 7.2 passed.\n")
    return (TRUE)
}

swiftTest_7.3 <- function () {
    # Test swiftLibrary constructed string to make
    try(detach(package:cluster), silent=T)
    # Test swiftLibrary default

    # cluster is pkg in R std lib
    swiftLibrary(paste("clus", "ter", sep=""))

    # test
    loadedok <- swiftLapply(list("cluster"), pkgexists)[[1]]

    if (!loadedok) {
        cat("cluster should be loaded on remote host\n")
        cat("\n!!!==> test 7.3 failed")
        return (FALSE)
    }
    if (pkgexists("cluster")) {
        cat("cluster should not be loaded in this session\n")
        cat("\n!!!==> test 7.3 failed")
        return (FALSE)
    }

    swiftDetach(paste("package:","clus", "ter", sep=""))
    loadedok <- swiftLapply(list("cluster"), pkgexists)[[1]]
    if (loadedok) {
        cat("cluster should be detached on remote host\n")
        cat("\n!!!==> test 7.3 failed")
        return (FALSE)
    }

    cat("\n!!!==> test 7.3 passed.\n")
    return (TRUE)
}

runAllSwiftTests <- function(...) {

    startTime = proc.time()[["elapsed"]]


    testRes <- runTestSuite(makeFullTestSuite(...))

    endTime <- proc.time()[["elapsed"]]
    runTime <- endTime - startTime

    cat("\n\n ===> Total elapsed test time = ",runTime," seconds.\n\n") 
    return (invisible(testRes))
} 


testloop <- function(npass)
{
  for(i in 1:npass) {
    cat("\n\n\n ***** Starting test pass ", i, " ***** \n\n\n");
    runAllSwiftTests()
    cat("\n\n\n ***** Completed test pass ", i, " ***** \n\n\n");
    system("sleep 3")
  }
}


testGroup1.1 <- makeTestGroup(
    name="1.1 Sanity Check",
    tests = list(mkTest(swiftTest_1.1)))

testGroup1.2 <- makeTestGroup( 
    name="1.2 Basic Test - Adding data sets",
    tests = list(mkTest(swiftTest_1.2.1),
        mkTest(swiftTest_1.2.2), mkTest(swiftTest_1.2.3),
        mkTest(swiftTest_1.2.4), mkTest(swiftTest_1.2.5)))

testGroup1.3 <- makeTestGroup(
    name="1.3 - Export functionality",
    tests = list(mkTest(swiftTest_1.3.1),
        mkTest(swiftTest_1.3.2), mkTest(swiftTest_1.3.3),
        mkTest(swiftTest_1.3.4),  mkTest(swiftTest_1.3.5)) )

testGroup2 <- makeTestGroup(
    name="2 - test matrix passing",
    tests = list(mkTest(swiftTest_2.1),
        mkTest(swiftTest_2.2)) )

testGroup3 <- makeTestGroup(
    name="3 - test list element and name passing",
    tests = list(mkTest(swiftTest_3.1),
        mkTest(swiftTest_3.2)))

testGroup4 <- makeTestGroup(
    name="4 - test remote R service initialization string",
    tests = list(mkTest(swiftTest_4.1),
        mkTest(swiftTest_4.2)))

testGroup5 <- makeTestGroup(
    name="5 - remote R service error ",
    tests = list(mkTest(swiftTest_5.1)))

testGroup6 <- makeTestGroup(
    name="6 - test apply variants",
    tests = list(mkTest(swiftTest_6.1),
            mkTest(swiftTest_6.2),
            mkTest(swiftTest_6.3)))

testGroup7 <- makeTestGroup(
    name="7 - library imports",
    tests = list(mkTest(swiftTest_7.1),
            mkTest(swiftTest_7.2),
            mkTest(swiftTest_7.3)))


makeFullTestSuite <- function (...) {
    initArgs <- list(...)
    initArgs[['keepwork']] <- TRUE
        makeTestSuite(
        setup=function () { options(swift.runmode='service')
                            options(swift.initialexpr="initVar1 <- 19; initVar2 <- sqrt(400)+3")
                            do.call(swiftInit, initArgs) }, # swiftInit controlled via options
        groups=list(testGroup1.1, testGroup2, testGroup1.2, testGroup1.3, testGroup3, testGroup4,
            testGroup5, testGroup6, testGroup7), 
        teardown=function () { swiftShutdown() })
}
