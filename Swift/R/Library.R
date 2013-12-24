
swiftLibrary <- function (packname) {
    # Add a library to be included on all workers
    # The package can be provided directly or alternatively
    # the package name can be provided.
    # If reset is true, then previously added libraries won't be
    # reimported on new workers.

    subpackname <- substitute(packname)
    # Check to see if it is a string, if it is not a string
    # then we will deparse it to get the expression text
    if (!is.character(subpackname)) {
        if (is.name(subpackname)) {
            # Maybe library was provided unquoted
            packname <- deparse(subpackname)
        }
    }

    if (!(packname %in% installed.packages())) {
        # Warn users in case they made a typo
        warning(paste("Package", packname, 
                "was not a installed package in this instance of R,", 
                "but may be installed on SwiftR workers"))
    }
    plist <- getOption(".swift.packages")
    if (is.null(plist)) {
        plist <- list()
    }
    else {
        # scan libs for previous detaches and libraries of 
        # this package.  Delete all of them
        plist = Filter(function(packcmd) 
                        { return (packcmd[2] != packname);},
                        plist)
    }
    # Add the library command to the end of the list of commands to be 
    # executed in slave R instances
        plist[[length(plist) + 1]] <- c("library", packname)
    
    options(.swift.packages=plist)
    return (invisible(NULL))
}

swiftDetachAll <- function () {
    # Detaches all packages previously loaded through swiftLibrary
    plist = getOption(".swift.packages")
    if (is.null(plist)) {
        # Nothing added before
        return
    }
    # replace all c("library", libname) entries in list with:
    #   c("detach", libname)
    plist = lapply(plist, 
            function (ent) {
                if (ent[1] == "library") {
                    c("detach", ent[2])
                }
                else {
                    # already detached
                    ent
                }
            })
    options(.swift.packages=plist)
    return (invisible(NULL))
}

sstartsWith <- function (str, prefix) {
    substr(str, 1, nchar(prefix)) == prefix
}

swiftDetach <- function (packname) {
    # name is an string or identifier such as "package:OpenMx",
    #   following the same pattern as the built in R detach() function
    # Detaches a library from workers and ensures that it will no longer be 
    # imported.  
    
    # Caveat: the effect on dependent imports is a little
    # messy and behaves differently on existing and new workers.  
    # If package A requires package B, and we import A on a worker,
    # package B will also be imported on the worker.  By detaching package A
    # we don't also detach B.  This contrasts to a fresh worker, where
    # package B will not be imported.

    # Check to see if it is a string, if it is not a string
    subpackname <- substitute(packname)
    # then we will deparse it to get the expression text
    if (!is.character(subpackname)) {
        # Check to see if symbol name has package: prefix
        deparsed <- deparse(subpackname)
        if (sstartsWith(deparsed, "package:")) {
            # Maybe library was provided unquoted
            packname <- deparsed
        }
    }
    
    #TODO: remove from options(".swift.packages")
    # Scan through list and remove any attach or detach of this name
    # add a detach cmd to the end
    if (!sstartsWith(packname, "package:"))
        stop("Can only detach packages.  Package must be specified as package:PackageName")
    # Get bit after "package:"
    packname = substr(packname, 9, nchar(packname))
    plist = getOption(".swift.packages")
    if (is.null(plist)) {
        plist = list()
    }
    else {
        # Previous export/detach commands are now redundant
        plist = Filter(function(packcmd) 
                        { return (packcmd[2] != packname);},
                        plist)
    }
    plist[[length(plist) + 1]] = c("detach", packname)

    options(.swift.packages=plist)
    return (invisible(NULL))
}

buildLibStr <- function () {
  # Build a list of "Library" statements for all of the libraries
  # already specified through swiftLibrary commands.
  lib <- getOption(".swift.packages")
  if (!is.null(lib)) {
    # library statements
    stmts <- lapply(lib, 
                function (libcmd) { 
                    verb <- libcmd[1];
                    if (verb == "library") {
                        return (sprintf("library(%s);", libcmd[2]));
                    }
                    else if (verb == "detach") {
                        return (sprintf("try(detach(package:%s),silent=T);", 
                                    libcmd[2]));
                    }
                    else {
                        error(paste("invalid verb ", verb, " in ",
                                ".swift.packages option"))
                    }
                })
    libstr = paste(stmts, collapse=" ")
  }
  else {
    libstr = ""
  }
  return (libstr)
}
