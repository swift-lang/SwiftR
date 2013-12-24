# Helper functions for building performance tests suites that don't quite fit in the
# main SwiftR code because they can detach the Swift package..


swiftSetup <- function (...) {
    libraries <- search()
    if ("package:Swift" %in% libraries) {
        library(Swift)
        options(.swift.detach=T)
    }
    library(Swift)
    swiftSess <- swiftInit(...)
    options(.swift.printtiming=getOption("swift.printtiming"))
    options(swift.printtiming=TRUE)
    # Wait to start
    Sys.sleep(10)
    # Run some small jobs to ensure workers are started and warmed up
    swiftapply(function (x) { x }, 
            rep(1, swiftSess$cores * swiftSess$nodes * 2),
            callsperbatch=1)
    return (swiftSess)
}

swiftTearDown <- function (...) {
    swiftShutdown()
    options(swift.printtiming=getOption(".swift.printtiming"))
    dt <- getOption(".swift.detach")
    options(.swift.detach=NULL)
    if (!is.null(dt) && dt) {
        detach(package:Swift)
    }
}

sfSetup <- function(..., cpus=ncpus) {
    libraries <- search()
    if (!("package:snowfall" %in% libraries)) {
        library(snowfall)
        options(.snowfall.detach=T)
    }
    # Swift needs to be detached
    if ("package:Swift" %in% libraries) {
        detach(package:Swift)
        options(.swift.reattach=T)
    }

    sfInit(..., cpus=ncpus)
    # warmup
    sfLapply(rep(1, ncpus * 2), 
                function (x) { x })

}

sfTearDown <- function (...) {
    sfStop()
    dt <- getOption(".snowfall.detach")
    options(.snowfall.detach=NULL)
    if (!is.null(dt) && dt) {
        detach(package:snowfall)
    }
    rt <- getOption(".swift.reattach")
    options(.swift.reattach=NULL)
    if (!is.null(rt) && rt) {
        library(Swift)
    }
}



makePerfTestGroup <- function (mode, name, f, allargs, prep=NULL, ...) {
    params <- list(...)
    paramstr <- paste(names(params),
                rep("=", length(params)), 
                lapply(params, deparse), 
                sep="", collapse=", ")
    if (mode == "swift") {
        tg <- Swift:::makeParamTestGroup(name=paste("swift_",name, " ", paramstr,sep=""),
            f=f, prep=prep,
            allargs=allargs,
            perfparams = params,
            setup=function() {swiftSetup(...)},
            teardown = swiftTearDown)
    }
    else if (mode == "snowfall") {
        tg <- Swift:::makeParamTestGroup(name=paste("sf_",name, paramstr, sep=""),
            f=f,prep=prep,
            allargs=allargs,
            perfparams = params,
            setup=function() {sfSetup(...)},
            teardown = sfTearDown)

    }
    else {
        print("Making serial test")
        tg <- Swift:::makeParamTestGroup(name=paste("serial_",name, paramstr, sep=""),
            f=f,prep=prep,
            allargs=allargs,
            perfparams = list(),
            setup=function() 
                {try(detach(package:Swift)); try(detach(package:Snowfall))} )
    }
    return (tg)
}



