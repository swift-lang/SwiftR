#! /usr/bin/env Rscript

# Use this script to run batches manually for testing. See "manualmode" notes in R/Swift.R.

require(OpenMx)

argv = commandArgs(TRUE)

# load(argv[1]);

for(infile in argv) {
  cat("Processing file: ", infile, "\n")
  rcall <- NULL # Needed?
  load(infile)
  ofile <- sub('^c','r',infile)
  result <- list()
  cat("Number of calls: ", length(rcall$arglistbatch),"\n")
  for(c in 1:length(rcall$arglistbatch)) {
      # FIXME: run this under try/catch and save error status in results object (need to make it a list: rval + error status)
      cat("Running call # ", c, "\n");
      result[[c]] <- do.call( rcall$func, 
        as.list(rcall$arglistbatch[[c]]) )
  }
  save(result,file=ofile)
}
