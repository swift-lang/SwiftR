# An R module for non-blocking fifo operations.

.checkFifoArgs <- function(fifopath, timeout) {
    # Catch errors proactively
    if (! is.null(timeout)) {
        tryCatch(
            timeout <- round(timeout),
            error = function (e) {
                stop(paste("Provided timeout value of", timeout, "is not numeric"))
            }
        )
    }
   
    if (! file.exists(fifopath) ) {
        stop(paste("fifo specified does not exist or has wrong permissions:", 
                fifopath))
    }
}


# writes to a fifo with an optional timeout value.
# if the timeout is exceeded, FALSE is returned
# otherwise TRUE is returned
writeFifo <- function (fifopath, message, timeout=NULL) {
    .checkFifoArgs(fifopath, timeout)
    bin <- file.path(.find.package("Swift"), "exec/fifowrite")


    if (is.null(timeout)) {
        cmdString <- paste(bin, fifopath)
    }
    else {
        cmdString <- paste(bin, fifopath, "-t", timeout)
    }
    cmdString <- paste(cmdString, "-m", 
            shQuote(message), "&>/dev/null")

    retval <- system(cmdString, wait=TRUE)

    return (retval == 0)
}

# reads from a fifo with an optional timeout value
# if the timeout is exceeded, NA is returned
# otherwise a vector of the lines of text is 
# read from the fifo
# timeout is in milliseconds.
readFifo <- function (fifopath, timeout=NULL) {
    .checkFifoArgs(fifopath, timeout)
    # TODO: fork off another process to do reading, to avoid 
    # the R process blocking irrecoverably
    # fiforeadln will try to read whatever is available in the fifo, but
    # if the timeout is exceeded, terminates and writes
    # _FIFOTIMEOUT_ to stdout
    bin <- file.path(.find.package("Swift"), "exec/fiforead")
    if (is.null(timeout)) {
        cmdString <- paste(bin, fifopath)
    }
    else {
        cmdString <- paste(bin, fifopath, "-t", timeout)
    }
    fifodata <- suppressWarnings(system(cmdString, intern=TRUE, ignore.stderr=TRUE))
            # stderr is ignored as we will get all relevant information
            # through stdout
            # want to suppress warning message about exit codea
    toutMsg <- "_FIFOTIMEOUT_"
    if ( (! length(fifodata) == 0)
        && substr(fifodata[[1]], 1,nchar(toutMsg)) == toutMsg) {
        return (NA)
    }
    else {
        return (fifodata)
    }

}
