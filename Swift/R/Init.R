# This file contains functions involved in starting up and shutting down
# SwiftR

.onLoad <- function (lib, pkg) {
    detectSystemInfo()
}

.First.lib <- function(lib, pkg) {
    # When the library is loaded, set up the
    # list of workers
    .swift.workers <<- list()
}

swiftCleanup <- function () {
    # Shut down workers
    swiftShutdown(all=TRUE)
    # Clean up exported files
    removeExports()

}

.Last.lib <- function(p) 
{
    # If the library is unloaded we need to do cleanup
    swiftCleanup()
    removeHook()
}

# Hook to perform cleanup of workers upon shutting down an R
# session
addHook <- function() {
    # Replace the user's last function with ours
    # If .UserLast already exists don't worry about it
    # as we've already added our hook
    if (!exists(".UserLast")) {
        if (!exists(".Last")) {
            # Create a dummy function
            .UserLast <<- function () {}
    
        }
        else {
            .UserLast <<- .Last
        }
        
        .Last <<- function () {
            swiftCleanup()
            .UserLast()
            removeHook()
        }
    }
}

removeHook <- function() {
    if (exists(".UserLast", where=".GlobalEnv")) {
        .Last <<- .UserLast
        rm(".UserLast", pos=".GlobalEnv")
    }
}

detectSystemInfo <- function () {
    # Do initial detection and setting of options to
    # reflect system info.  DOesn't guarantee options will be set
    os <- tolower(R.version$os)
    corecount <- 0
    if (substr(os, 1, 5) == "linux") {
        corecount <- try(as.numeric(system(
            "grep -c '^processor' /proc/cpuinfo 2> /dev/null", intern=T)))
    }
    if (substr(os, 1, 5) == "darwin") {
        #Mac OS X / Darwin
        corecount <- try(as.numeric(unlist(
            system(paste("/usr/sbin/sysctl hw.ncpu 2> /dev/null | ",
                    "awk '{ print $NF }' 2> /dev/null"), intern=T), 
                    )))
    }
    #TODO: detect cores on other systems:
    #   * BSD?
    #   * windows? 

    if (!inherits(corecount, "try-error") && corecount >= 1){
        options(swift.system.cores=corecount)
    }
}

