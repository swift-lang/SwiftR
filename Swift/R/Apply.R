

# The Modes for directories and files created by SwiftR
# We only give permissions to the current user.
kDIR_MODE <- "700"
kFILE_MODE <- "600"
swiftapplyScript <- system.file(package="Swift","exec/swiftapply.swift")
RunSwiftScript <- system.file(package="Swift","exec/RunSwiftScript.sh")

swiftapply <- function( func, arglists,
                        server=NULL,
                        callsperbatch=NULL,
                        runmode=NULL,
                        initialexpr=NULL,
                        workerhosts=NULL,
                        keepwork=NULL,
                        tmpdir=NULL,
                        timeout=NULL, 
                        quiet=NULL,
                        printTiming=NULL)
{
  # Set Swift default options if not passed as keywords or pre-set by user
  if(is.null(server)) server <- getOption("swift.server")
  
  if(is.null(quiet)) quiet <- getOption("swift.quiet")
  if(is.null(quiet)) quiet <- FALSE

  if(is.null(callsperbatch)) callsperbatch <- getOption("swift.callsperbatch")
  if(is.null(callsperbatch)) 
    callsperbatch <- chooseBatchSize(length(arglists), server, quiet=quiet)

  # service: send requests to Swift service loop via fifo
  # script:  run swift for each request, via RunSwiftScript.sh (broken)
  # manual:  for testing, let user run remote R server manually
  if(is.null(runmode)) runmode <- getOption("swift.runmode")
  if(is.null(runmode)) runmode <- "service"

  if(is.null(initialexpr)) initialexpr <- getOption("swift.initialexpr")
  # Have library imports before other expressions
  initialexpr <- paste(buildLibStr(), initialexpr, collapse=" ");

  if(is.null(workerhosts)) workerhosts <- getOption("swift.workerhosts")
  if(is.null(workerhosts)) workerhosts <- "localhost";

  if(is.null(keepwork)) keepwork <- getOption("swift.keepwork")
  if(is.null(keepwork)) keepwork <- FALSE;

  if(is.null(tmpdir)) tmpdir <- chooseTmpDir()
  
  if(is.null(printTiming)) printTiming <- getOption("swift.printtiming")
  if(is.null(printTiming)) printTiming <- FALSE

  # Inform user of some call info
  if (! quiet) {
      cat("\nswiftapply to ", length(arglists), " arg lists.\n")
      cat("\nSwift properties:\n")
      cat("  server =", server,"\n")
      cat("  callsperbatch =", callsperbatch,"\n")
      cat("  runmode =", runmode,"\n")
      cat("  tmpdir =", tmpdir,"\n")
      cat("  workerhosts =", workerhosts,"\n")
      cat("  initialexpr =", initialexpr,"\n\n")
  }

  # Execute the calls in batches
  reqdir <- setupRequestDir(tmpdir=tmpdir)
  if (printTiming) 
      startTime = proc.time()[["elapsed"]]
  batchsizes <- writeRequestBatches(func, arglists, initialexpr, 
                reqdir, callsperbatch)
  nbatches <- length(batchsizes)
  if (printTiming) {
    endTime = proc.time()[["elapsed"]]
    cat(nbatches, "Swift request files written to: ", reqdir,
        "in", sprintf("%.2f", endTime - startTime), "s\n")
  }
  else if (!quiet)  {
    cat(nbatches, "Swift request files written to: ", reqdir, "\n")
  }

  if( runmode == "manual" ) { 
    # Prompt for return (empty line) to continue; assumes user ran a 
    # manual R to process the call.
    cat("Manual Swift Run:\n  run dir: ", getwd(), "/", reqdir,"\n\n")
    cat("  swift script: ", RunSwiftScript, "\n")
    cat("  server: ", server,"\n")
    cat("  swiftapplyScript: ", swiftapplyScript,"\n")
    cat("  Use RunAllR.sh to process and press return when complete:")
    system(paste("cp ", system.file(package="Swift","exec/RunAllR.sh"), reqdir))
    readLines(n=1)
  }
  else if (runmode == "script") {
    system(paste(RunSwiftScript,reqdir,server,swiftapplyScript,
                shQuote(workerhosts)))
  }
  else if (runmode == "service") { 
    # Send request to service
    swiftServerDir = getWorkerDir(server) 

    requestPipeName=file.path(swiftServerDir,"requestpipe")
    resultPipeName=file.path(reqdir,"resultpipe")
    # Try sending: this function will cause error if it fails
    sendServiceRequest(requestPipeName, reqdir, server, timeout=timeout)
    
    res <- getServiceResponse(resultPipeName, timeout)
    # Check that the message was correct
    if (substring(res[[1]], 1, 6) == "error:")  {
      stop(paste("Received error message from Swift '", res, 
                "'.  The swift server has shut down, rerun swiftInit to"
                ," restart", sep=""))
    }
    else if (res[[1]] != "done") {
      stop(paste("Got unexpected message '", 
            paste(res, collapse="\n"),"' on fifo ",
            "aborting job", sep=""))
    }
  }
  else {
    stop(paste("Invalid runmode", runmode))
  }

 
  if (printTiming) startTime = proc.time()[["elapsed"]]
  # Fetch the batch results
  res <- fetchBatchResults(reqdir, batchsizes, arglists, keepwork, quiet)
  if (printTiming) {
    endTime = proc.time()[["elapsed"]]
    cat(paste("Swift results retrieved from disk in",
        sprintf("%.2f", endTime - startTime), "s\n"))
  }

  return (res)
}

swiftLapply <- function( tlist, func, ... )
{
  if (length(tlist) == 0) return(tlist)
  arglists <- list()
  narglists <- length(tlist)
  for(i in 1 : narglists) {
    arglists[[i]] <- list(tlist[[i]], ...);
  }
  names(arglists) = names(tlist)
  swiftapply(func, arglists)
}

swiftSapply <- function( tlist, func, ..., simplify=TRUE, USE.NAMES=TRUE) {
    # replicate sapply behaviour:
    # simplify to 1d vector or 2d matrix if possible
    res <- swiftLapply(tlist, func, ...)
    if (USE.NAMES) {
        # same rules as sapply
        if (is.null(names(res)) && is.character(tlist)) {
            names(res) <- tlist
        }
    }
    if (simplify && length(res) > 0) {
        # Check to see if list is multidimensional
        lens <- unique(lapply(res, length))
        if (length(lens) != 1) {
            # ragged list, can't simplify
            return (res)
        }
        else {
            if (lens == 1) {
                # just a plain list
                return (unlist(res, recursive=F))
            }
            else {
                # multidimensional list: convert to 2d matrix
                res <- unlist(res, recursive=F)
                if (!(is.null(n1 <- names(res[[1]]))) && 
                    !(is.null(n2 <- names(res)))) {
                    # names of first and second dimnesion
                    dimnames <- list(n1, n2)
                }
                else {
                    dimnames <- NULL
                }
                res <- array(res, dim=c(lens, length(tlist)),
                        dimnames=dimnames)
                return(res)
            }
        }
    }
    return (res)
}


setupRequestDir <- function (tmpdir) {
  # Initialize globals if first call in this invocation of R
  # Use the options mechanism so that setting is tied
  # to lifetime of this R process.  If we stored this in a global
  # variable, it is possible that, say, directory requests.55555/R0000005
  # is created, the user exits the session without saving, and therefore
  # the .swift.requestid counter is out of step with the file system
  requestdirbase = getOption("swift.requestdirbase") 
  if(!is.null(requestdirbase)) {
    requestid = getOption(".swift.requestid") + 1;
  }
  else {
    topdir <- file.path(tmpdir, Sys.info()[["user"]],"SwiftR")
    if (!dir.create(topdir,recursive=TRUE,showWarnings=FALSE, 
            mode=kDIR_MODE)) {
        # Might already have existed
        if (!file.exists(topdir) && file.info(topdir)$isdir) {
            oldtopdir <- topdir
            topdir <- tempdir()
            warning(paste("Could not create working directory", oldtopdir, 
                        "instead using", topdir))
        }
    }

    requestdirbase = file.path(topdir, 
                sprintf("requests.P%.5d",Sys.getpid()))
    dir.create(requestdirbase,recursive=TRUE,showWarnings=FALSE, 
            mode=kDIR_MODE)
    options(swift.requestdirbase=requestdirbase)
    requestid = 0;
  }
  options(.swift.requestid=requestid)
  reqdir = file.path(requestdirbase, sprintf("R%.7d",requestid))
  dir.create(reqdir,recursive=TRUE,showWarnings=FALSE,mode=kDIR_MODE)

  # Create a fifo that we can block on to get a response
  system(paste("mkfifo", shQuote(file.path(reqdir, "resultpipe"))))
  
  return (reqdir)
}

writeExportList <- function (reqdir, exportFileList) {
  # Write out a list of exported Rdata files, in text format 
  # with one file per line.  This is in a file called exports.txt
  expFile<-file(file.path(reqdir, "exports.txt"))
  writeLines(as.vector(exportFileList, mode="character"), expFile)
  close(expFile)
}

writeRequestBatches <- function (func, arglists, initialexpr, 
                        reqdir, callsperbatch, exportlist=NULL) {

  if (is.null(exportlist)) exportlist <- getOption(".swift.session")
  if (is.null(exportlist)) {
    exportlist <- newSession()
    options(.swift.session = exportlist)
  }
  writeExportList(reqdir, exportlist$exports)
  # Write the function call info out to cbatch.?.RData files in reqdir 
  # in batches of size specified by callsperbatch 
  # returns the number of batches written
  narglists <- length(arglists) # number of arglists to process
  batch <- 1   # Next arglist batch number to fill
  arglist <- 1 # Next arglist number to insert

  batchsizes <- list()
  while(arglist <= narglists) {
    arglistsleft <- narglists - arglist + 1
    if(arglistsleft >= callsperbatch) {
      batchsize <- callsperbatch
    }
    else {
      batchsize <- arglistsleft
    }
    arglistbatch <- list()
    for(i in 1 : batchsize) {
      arglistbatch[[i]] <- arglists[[arglist]]
      arglist <- arglist +1 
    }
    rcall <- list(initializer=initialexpr,
                    imports=exportlist,
                    func=func,arglistbatch=arglistbatch)
    save(rcall,
        file=file.path(reqdir,
            paste("cbatch.",as.character(batch),".Rdata",sep="")))
    batchsizes[[batch]] <- batchsize
    batch <- batch + 1;
  }
  return (batchsizes)
}

fetchBatchResults <- function (reqdir, batchsizes, arglists, keepwork, quiet) {
  rno <- 1
  rlist <- list() # Big result list
  nbatches <- length(batchsizes)
  for(batch in 1:nbatches) {
    # The result in the file will be named "result"
    # Initialize result variable to reflect
    result <- NULL
    load(file.path(reqdir,
            paste("rbatch.",as.character(batch),".Rdata",sep="")))

#    cat("Batch #", batch, " of ", nbatches, ":\n")
#    print(result)

    # Check returned results for errors
    for (res in result) {
      if(inherits(res, "try-error")) {
        cat("ERROR in eval: ", res, "\n");
      }
    }
    # Assign to correct result list slice
    # error will be thrown here if size of result list is wrong
    batchsize <- batchsizes[[batch]]
    rlist[rno:(rno + batchsize - 1)] <- result
    rno <- rno + batchsize
  }
  names(rlist) = names(arglists)
  if( ! keepwork ) {
    if (!quiet)
        cat("Removing ", reqdir, "\n")
    unlink(reqdir,recursive=TRUE)
  }
  return(rlist)
}

chooseBatchSize <- function (numargs, server, quiet=FALSE) {
    # Automatic selection of worker count
    # start with 1 batch per core
    wc <- workerCount(server)
    if (is.null(wc)) {
        if (! quiet ) 
            cat(paste("Information about current server of type", 
                    server, "not found"))
        callsperbatch <- 1
    }
    else {
        batches_per_worker <- getOption("swift.batchesperworker")
        if (is.null(batches_per_worker)) 
            batches_per_worker <- 1
        target_batches <- wc * batches_per_worker
        # We want to have at least batches_per_worker batches of calls
        # for each worker.  If it doesn't divide evenly, some will get
        # batches_per_worker + 1 batches.  
        # callsperbatch < 1 doesn't make sense, hence the use of max
        callsperbatch = max(1, floor(numargs/target_batches))
    }
}

chooseTmpDir <- function () {
  # Choose temporary directory based on global settings
  # with several fallbacks
  tmpdir <- getOption("swift.tmpdir")
  if(is.null(tmpdir))
    tmpdir <- Sys.getenv("SWIFTR_TMP");

  if(tmpdir=="") {
    tmpdir <- "/tmp";
  }
}

sendServiceRequest <- function (requestPipeName, reqdir, server=NULL,  timeout=NULL) {
    # fifo will block irrecoverably if there is no reader on the
    # other end of the requestPipe.  This is bad.  The swift worker
    # script is responsible for deleting the request pipe when it
    # shuts down, so we know if the requestPipe still exists there
    # should still be a worker (or the worker crashed in a funny way).
    if (file.exists(requestPipeName)) {
        # there is a race condition here if the fifo disappears in
        # between checking for existence and opening the fifo, but
        # the timeout will catch that unlikely case
        success <- writeFifo(requestPipeName,paste(reqdir,"\n",sep=""), 
                timeout=timeout)
        if (! success) {
            stop(paste("timeout of", timeout, 
                "ms exceeded when attempting to",
                "rendezvous with  SwiftR server of type", server, ".\n",
                "Maybe it is not running or it has crashed"))
        }
    }
    else {
        stop(paste("Have you run swiftInit?\n",
                "It appears that the SwiftR server is not running",  
                ", as no request pipe exists at", 
                requestPipeName))
    }

}

getServiceResponse <- function (resultPipeName, timeout) {
    # Blocking wait for response from service on a pipe
    # returns the string response. Raises error if timeout occurs.

    # Wait for reply from service
    res <- readFifo(resultPipeName, timeout=timeout)
    if (length(res) == 0) {
      stop(paste("Zero length response on named pipe ", resultPipeName))
    }
    if (is.na(res)) {
      stop(paste("Timeout of ", timeout, "ms exceeded when waiting",
            "for response from swift server"))
    }
    return (res)
}

