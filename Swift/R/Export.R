# EXPORT MECHANISM:
#==================
# We want the protocol to support workers coming and going from the 
#  cluster.  We also want to be robust to worker failures.  This
# means that we can't rely on state being retained at workers.  
# I.e. If a new worker is added, we need it to receive all of the
# relevant exported data.  However, for the purposes of efficiency,
# we don't want workers to have to repeatedly reload data they have
# already loaded.
#
# In order to achieve this, we will have the workers perform imports
# at apply time.  The worker will receive information as a part of the
#
#
#
#
# When swiftExport() is called:
# * Save all of the objects to a file 
#    - Q:Can we assume its on a shared filesystem?
# * MAYBE: put the file somewhere accessible by all workers
# * append the file's path to a stored list of files
# * MAYBE LATER: asynchronously start swift tasks to distribute
#   the data, to get a headstart on the work
# When swiftApply() is called:
#   - Inform all worker R processes through some mechanism (TBD)
#       of the list of all exported data files. (extra file?)
#   - The worker goes through the list in order to ensure that name 
#       conflicts are resolved in the way you would expect.  
#       If it has loaded a file previously, it does nothing.
#       If it hasn't loaded the file, it accesses the file (mechanism TBD)
#       and loads it into the global namespace.
#       data 
# 
# swiftRemoveAll():
#   delete the list of exported files from above and have a single
#   entry which instructs workers to delete all their data.  This is
#   mainly important because it eliminates possibility of using huge
#   amounts of memory.
#  swiftRemove()
#   Implementation of this function is somewhat problematic, as it only
#   partially undoes previous work


swiftExportAll <- function (file=NULL) {
    # Exports all functions and data in global environment
    swiftExport(list=ls(globalenv()), file=file)
}

swiftExport <- function (..., list=NULL, file=NULL, 
        quiet=getOption("swift.quiet")) {
    if (is.null(quiet)) quiet <- TRUE
    # List of object names (as R symbols or as strings)
    # These will be passed directly to save() to be serialized
    if (is.null(file)) {
        dir <- getExportDir()
        if (is.null(dir)) {
            error(paste("Could not determine an export directory, try", 
                "setting the",
                "swift.exportdir option to a directory accessible by this R",
                "session and the Swift worker processes"))
        }
        # Separate directory for each new R session to keep files together
        # and avoid conflicts.
        if (!quiet)    cat("Export directory: ", dir, "\n")
        dir.create(dir, recursive=T, showWarnings=FALSE, mode=kDIR_MODE)

        # add the file, keeping a counter to use in the file name
        expid = getOption(".swift.exportid")
        if (is.null(expid)) expid = 0
        options(.swift.exportid = expid + 1)
        file <- file.path(dir, sprintf("E%.7d",expid))
        if (!quiet)    cat("Export file: ", file, "\n")
        
        # Keep track of files for later cleanup
        expfiles <- getOption(".swift.exportfiles")
        if (is.null(expfiles)) expfiles = list()
        expfiles[[length(expfiles) + 1]] <- file
        options(.swift.exportfiles=expfiles)
    }
    
    #TODO: file mode?
    if (is.null(list))
        save(..., file=file, envir=parent.frame())
    else
        save(..., list=list, file=file, envir=parent.frame())
    session <- getOption(".swift.session")
    if (is.null(session))
        session <- newSession()
    session$exports[[length(session$exports) + 1]] <- file
    options(.swift.session=session)

    return(invisible(NULL))

}

newSession <- function () {
    exportList= list()
    exportList$session <- as.integer(runif(1, min=0, max=.Machine$integer.max))
    exportList$exports <- list()
    return (exportList)
}

swiftRemoveAll <- function () {
    # Cleans up all data in global namespace on workers
    # This is achieved by starting a new "session"
    # Use session numbers to track whether a worker has the
    # correct data
    # TODO: think about how to clean up data files.
    # it is hard to know when they can be safely removed
    options(.swift.session=newSession())
    return(invisible(NULL))
}

getExportDir <- function () {
    d <- getOption("swift.exportdir")
    if (is.null(d)) {
        # temp directory
        return (file.path(tempdir(), "swexports"))
    }
    if (d == "") {
        return (NULL)
    }
    else {
        d <- file.path(d, ".swiftr")
        return (file.path(d, sprintf("exports.P%.5d",Sys.getpid())))
    }
}

# Function called when Swift is unloaded or session ends
# that is responsible for cleanup
removeExports <- function () {
    cat("removing exports\n")
    expfiles <- getOption(".swift.exportfiles")
    for (file in expfiles) {
        #cat("removing", file, "\n")
        file.remove(file)
    }
    options(.swift.exportfiles=NULL)
    dir <- getExportDir()
    if (!is.null(dir)) {
        unlink(dir, recursive=T)
    }
}
