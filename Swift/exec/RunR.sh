#! /usr/bin/env Rscript

argv = commandArgs(TRUE)

load(argv[1]);

result=list()
for(c in 1:length(rcall$arglistbatch)) {
    # FIXME: run this under try/catch and save error status in results object (need to make it a list: rval + error status)
    result[[c]] = do.call( rcall$func, 
        as.list(rcall$arglistbatch[[c]]) )
        # Coerce to list in case vector provided
}

save(result,file=argv[2])
