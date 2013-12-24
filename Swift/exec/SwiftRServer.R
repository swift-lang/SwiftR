#! /usr/bin/env Rscript

library(methods)



# Enclose everything within .main environment so that we can use global environmen tfor process data
# Note; Everything in the global environment that belong to this script should be prefixed
# with a .: other thrings are assumed to belong to script.
.main <- function (argv) {

    print("SwiftRServer arguments:")
    print(argv)
    print(".libPaths:")
    print(.libPaths())

    fifoDir = argv[1];  # FIXME: test for valid arguments

    cat(Sys.getpid(),file=paste(fifoDir,"/R.pid",sep=""))

    .current.initializer <<- ""

    SwiftRFifoServer <- function( fifoBasename )
    {
      inFifoName = file.path(fifoDir,"toR.fifo")
      outFifoName = file.path(fifoDir,"fromR.fifo")
      dir = getwd()
      cat("SwiftRServer starting in dir:",dir,"\n");
      repeat {
        cat("SwiftRServer at top of loop is in dir:",getwd(),"\n");
        setwd(dir) # FIXME: not yet sure what is changing the CWD

        # Read a line from fifo
        infifo <- fifo(inFifoName,open="rb",blocking=TRUE)
        inline <- readLines(infifo, 1)
        close(infifo)

        cmd <- unlist(strsplit(inline, " ", fixed=T))
        cat("cmd:\n")
        print(cmd)
        if (length(cmd) < 3) {
            stop(paste("Error: cmd provided was too short with length ",
                    length(cmd), ":", cmd))
        }
        op = cmd[[1]]
        callBatchFileName = cmd[[2]]
        resultBatchFileName = cmd[[3]]
        # NOTE: there has to be a correspondence between
        # the importFileNames received on the pipe and the
        # list of import file names in the call object.
        # They correspond 1:1.  The name in the call object
        # is the unique filepath on the client's system, which
        # we use to determine if an import file has been
        # already imported.  The name received on the pipe
        # is the name in the local file system.
        if (length(cmd) > 3) {
            importFileNames = cmd[4:length(cmd)]
        } else {
            importFileNames = NULL
        }
        cat("DB: cmd: op=",op," call batch=",callBatchFileName," result batch=",resultBatchFileName,"\n");
        if(is.null(op)) {
          cat("op is NULL\n")
          outfifo <- fifo(outFifoName,open="wb",blocking=TRUE)
          cat(file=outfifo,"op is NULL.\n")
        }
        else {
          if( identical(op,"quit")) {
            outfifo <- fifo(outFifoName,open="wb",blocking=TRUE)
            cat(file=outfifo,"Server closing down.\n")
            break
          }
          if( identical(op,"run")) {
            cat("DB: About to run batch file: ", callBatchFileName,"\n");
            runBatch(callBatchFileName,resultBatchFileName, importFileNames)
            outfifo <- fifo(outFifoName,open="wb",blocking=TRUE)
            cat(file=outfifo, "Batch completed: result batch file: ", resultBatchFileName,"\n");
          }
          else {
            cat("DB: got op [", op, "]\n")
          }
        }
        try(close(outfifo))
      }
    }

    doFail <- function( error )
    {
      outFifoName = file.path(fifoDir,"fromR.fifo")
      outfifo <- fifo(outFifoName,open="wb",blocking=TRUE)
      cat(paste("ERROR: R server failed with error", gsub("\n", " ", error)), "\n")
      cat(file=outfifo, paste("ERROR: R server failed with error", gsub("\n", " ", error)), "\n")
      try(close(outfifo))
    }

    doInit <- function(initializer) {
      #print(sprintf("received initializer=%s latestInitializer=%s\n",
      #                         initializer, latestInitializer));
      if( initializer != .current.initializer) {
        cat("DB: Running initializer:", initializer, "\n", file=stderr())
        eval(parse(text=initializer), envir=globalenv())
        .current.initializer <<- initializer
      }
    }

    failBatch <- function(rcall, try.error, resultBatchFileName) {
        # This function is called when an error occurs and
        # no valid argument values can be produced
        # It fills a result array with the try.error object
        # and writes it to resultBatchFileName
        result <- list()
        cat("DB: Failing with error:", try.error, "\n", file=stderr())
        for (c in 1:length(rcall$arglistbatch)) {
            result[[c]] <- try.error 
        }
        save(result,file=resultBatchFileName)
    }

    runBatch <- function( callBatchFileName, resultBatchFileName, importFileNames )
    {
      # Load contents into local environment
      success <- try(load(callBatchFileName, envir=environment()));
      if (inherits(success, "try-error")) {
        stop(paste(callBatchFileName, "could not be opened"))
        return()
      }
      success <- try(loadImports(rcall$imports, importFileNames))
      if (inherits(success, "try-error")) {
        cat("import failed!!: ", success)
        failBatch(rcall, success, resultBatchFileName)
        return()
      }


      success <- try(doInit(rcall$initializer))
      if (inherits(success, "try-error")) {
        failBatch(rcall, success, resultBatchFileName)
        return()
      }

      cat("DB: Doing apply\n", file=stderr())
      #Using lapply here will ensure that NULLs are handled
      # correctly
      result <- lapply(rcall$arglistbatch, 
            function (arglist) {
                try(do.call(rcall$func, as.list(arglist)))
                })
      cat("DB: Saving Results\n", file=stderr())
      save(result,file=resultBatchFileName)
      cat("DB: Results saved\n", file=stderr())
    }

    newSession <- function (session) {
        cat("DB: Setting up new session\n", file=stderr())
        cat("DB: Deleting:", ls(envir=globalenv()), "\n", file=stderr())
        #Setup new session
        .current.session <<- session
        # Use an environment as a hash table to track imports
        .current.imported <<- new.env(hash=T, parent=emptyenv())
        
        # Clear out global environment of anything not starting 
        # with a . - ie. things put there by previous imports
        rm(list=ls(envir=globalenv()),  envir=globalenv())

        .current.initializer <<- ""
        cat("DB: reset env\n")
    }

    loadImports <- function (importlist, importFileNames) {
        # First check whether the session id has changed
        #cat(paste("Import list:", importlist$exports, "\n"), file=stderr())
        #cat(paste("New Session:", importlist$session, "\n"), file=stderr())
        #cat(paste("Old Session:", try(get(".current.session", envir=globalenv())), 
        #            "\n"), file=stderr())
        doSetup <- FALSE
        if (!is.null(importlist)) {
            if (exists(".current.session", envir=globalenv())) {
                if (importlist$session != .current.session) 
                    doSetup <- TRUE
            }
            else {
                doSetup <- TRUE
            }
        }
        if (doSetup) {
            newSession(importlist$session)
        }
        # Co-iterate over the unique names and 
        # if length doesn't match, something has gone wrong
        numImports <- length(importlist$exports)
        if (numImports != length(importFileNames)) {
            stop(paste("Number of import file names provided on command line (",
                length(importFileNames),") does not match number in call object (",
                numImports, ").  ", importFileNames, " vs ", importlist$exports))
        }
        if (numImports > 0) {
            for (i in 1:numImports) {
                # Load the contents of the specified file
                # into the global environment
                uniqueId <- importlist$exports[[i]]
                localFile <- importFileNames[[i]]

                #cat("File: ", file, "\n")
                # check to see if already imported
                if (!exists(uniqueId, envir=.current.imported)) {
                    #TODO: load can fail with warning
                    load(localFile, envir=globalenv()) 
                    # if an error occurs here, assume calling function
                    # will catch it
                    assign(uniqueId, TRUE, envir=.current.imported)
                    cat("Loaded file ", localFile, " with unique path ", uniqueId, "\n")
                }
                else {
                    #cat("Ignored file ", file, "\n")
                }
            }
        }
    }

    cat("calling server: dir=",fifoDir,"\n")
    setwd(fifoDir)
    tryCatch(SwiftRFifoServer(fifoDir), error=doFail)
    cat("returned from server: dir=",fifoDir,"\n")
}

.main(commandArgs(TRUE))
