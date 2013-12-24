
require(Swift)
source("Swift/tests/perf/perfutils.R")
source("Swift/tests/OpenMx/BootstrapParallelBigger.R")

args = 128
res = list(n=c(), create=c(), prepro=c(), write=c(), writesep=c(), writesepsplit=c())
i = 1
for (n in c(1,2,4,8,16,32,48, 64, 96, 128, 192, 256, 384, 512)) {
    cat("\n", n, "\n\n")
    res$n[[i]] = n

    startTime = proc.time()[["elapsed"]]
    model <- buildModels(75, 100, n)
    endTime = proc.time()[["elapsed"]]
    res$create[[i]] = endTime - startTime
    cat(paste("Took", endTime - startTime, "s to build models\n"))



    startTime = proc.time()[["elapsed"]]
    dataList <- generateDataList(model)
    dshare <- shareData(model)
    independents <- getAllIndependents(dshare)
    endTime = proc.time()[["elapsed"]]
    res$prepro[[i]] = endTime - startTime
    cat(paste("Took", endTime - startTime, "s to process model\n"))


    x = list(mxRun, independents)
    wStartTime = proc.time()[["elapsed"]]
    save(x, file="blah.Rdata")
    wEndTime = proc.time()[["elapsed"]]
    cat(paste("Took", wEndTime - wStartTime, "s to write to disk models\n"))
    res$write[[i]] = wEndTime - wStartTime
    
    wStartTime = proc.time()[["elapsed"]]
    for (j in 1:n) {
        x = list(mxRun, independents[[j]])
        save(x, file=paste("blah", j, ".Rdata", sep=""))
    }
    wEndTime = proc.time()[["elapsed"]]
    cat(paste("Took", wEndTime - wStartTime, "s to write to disk models individually\n"))
    res$writesep[[i]] = wEndTime - wStartTime
    
    
    wStartTime = proc.time()[["elapsed"]]
    y = list(mxRun)
    save(y, file=paste("mxRun.Rdata", sep=""))
    for (j in 1:n) {
        x = independents[[j]]
        save(x, file=paste("blah", j, ".Rdata", sep=""))
    }
    wEndTime = proc.time()[["elapsed"]]
    cat(paste("Took", wEndTime - wStartTime, "s to write to disk models individually split\n"))
    res$writesepsplit[[i]] = wEndTime - wStartTime
    
    i <- i + 1
}
plot(res$n, res$writesep, col="green", type="l", xlab="No. replicas", ylab="Time (s)")
points(res$n, res$write, col="red", type="l")
points(res$n, res$writesepsplit, col="blue", type="l")
points(res$n, res$create, col="black", type="l")
points(res$n, res$prepro, col="brown", type="l")

legend("topleft", 
    legend=c("write separately", "write single file", 
            "write function\nand args separately", "time to create omx models",
            "time to identify independent models"), 
        fill=c("green", "red", 'blue', 'black', 'brown'))
