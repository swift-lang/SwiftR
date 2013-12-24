require(Swift)
source("Swift/tests/perf/perfutils.R")

cores <- 4
intsize <- 4

param.bytes <- lapply(1:24, function (x) {list(cores, 4 * (2**x))})

id <- function (x) {x}

test <- function (arr) {
    res <- swiftLapply(arr, id)
    length(res)
    return (NULL)
}
makeArray <- function (procs, arr.bytes) {
    #arrsize is in bytes
    args <- list()
    for (i in 1:procs) {
        args[[i]] <- as.vector(sample(1:1000, arr.bytes/intsize, replace=T), "integer")
    }
    return(args)
}

ts <- Swift:::makeTestSuite(groups=list(
            makePerfTestGroup(mode="swift", name="ArgSize", 
                f=test, prep=makeArray, 
                allargs=param.bytes,
                server="local", cores=cores),
            makePerfTestGroup(mode="swift", name="ArgSize", 
                f=test, prep=makeArray, 
                allargs=param.bytes,
                server="ssh", cores=1, hosts="nettle wapato dandelion cattail")
                ))
res <- Swift:::runTestSuite(ts)

anl <- mergeGroupResults(Swift:::analyseSuitePerf(res,
                argnames=c("procs", "arr.bytes"), perfparams=c('server')))



loc <- subset(anl, server=="local")
ssh <- subset(anl, server=="ssh")
plot(x=ssh$arr.bytes/1024, y=ssh$time, col='red', ylab="Time (s)", xlab="Argument size (bytes)",
#    log="xy", ylim=c(0.1, 500),
    xaxt="n")
title(main="Effect of argument size on execution time", line=2)
title(main="Execution time of identity function with one function invocation per core", line=1, font.main=4, cex.main=0.8)
axis(1, at=ssh$arr.bytes/1024, 
        labels=lapply(ssh$arr.bytes,
        function (n) { if (n >= 1024*1024) {sprintf("%dM", n/(1024*1024))}
                    else if (n >= 1024 ) {sprintf("%dK", n/1024)}
                        else {sprintf("%d", n)}})
    )
points(x=loc$arr.bytes/1024, y=loc$time)
legend("topleft", legend=c("Local (4 cores)", "Ssh (4 hosts, 1 core per host)"), 
        fill=c("black", "red"))
