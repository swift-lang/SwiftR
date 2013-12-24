
getNodeList <- function (server=getOption("swift.server")) {
    # Run within a job script submitted to a batch scheduler, this
    # function works out the nodes allocated.
    # It returns a data frame where the first column is the unique host names
    # and the second column is the number of processes for that node
    # if server is not specified and the swift.server option is unset,
    # this will try the different possibilities in sequence.
    hostinfo <- NULL
    if (!is.null(server) && !server %in% c("pbs", "sge", "cobalt", "pbsf")) {
        stop(paste("Invalid server setting for getNodeList:", server))
    }
    if (is.null(server) || server == "pbs" || server == "pbsf") {
        hostfile <- Sys.getenv("PBS_NODEFILE")
        if (hostfile[[1]] != "") {
            hostnames <- read.table(hostfile, stringsAsFactors=FALSE)[[1]]
            summ <- rle(sort(hostnames))
            hostinfo <- data.frame(hosts=summ[[2]], count=summ[[1]],
                         stringsAsFactors=FALSE)
        }
    }

    if (is.null(server) || server == "cobalt") {
        hostfile <- Sys.getenv(c("COBALT_NODEFILE"))
        if (hostfile[[1]] != "") {
            hostnames <- read.table(hostfile, stringsAsFactors=FALSE)[[1]]
            summ <- rle(sort(hostnames))
            hostinfo <- data.frame(hosts=summ[[2]], count=summ[[1]],
                        stringsAsFactors=FALSE)
        }
    }
    if (is.null(server) || server == "sge") {
        hostfile <- Sys.getenv(c("PE_HOSTFILE"))
        if (hostfile[[1]] != "") {
            hostinfo <- read.table(hostfile, stringsAsFactors=FALSE,
                    col.names=c("hosts", "count"))
        }
    }
    if (is.null(hostinfo))
        stop("Could not find environment variable pointing to node list.")
    else
        return (hostinfo)
}


swiftInit <- function( cores=getOption("swift.cores"), 
                    server=getOption("swift.server"), 
                    hosts=getOption("swift.hosts"), 
                    nodes=getOption("swift.nodes"), 
                    project=getOption("swift.project"), 
                    parEnv=getOption("swift.parenv"), 
                    kernel=getOption("swift.kernel"), 
                    workmode=getOption("swift.workmode"),
                    throttle=getOption("swift.throttle"), 
                    retries=getOption("swift.retries"),
                    queue=getOption("swift.queue"),
                    rcmd=getOption("swift.rcmd"), time=getOption("swift.time"),
                    workerLogging=getOption("swift.workerLogging"),
                    swiftLogging=getOption("swift.swiftLogging"), 
                    keepworkdir=getOption("swift.keepworkdir"), 
                    tc.file=getOption("swift.tc.file"), 
                    cf.file=getOption("swift.cf.file"), 
                    sites.file=getOption("swift.sites.file"))
{
    # server: which server backend to use to acquire workers
    #           for example, local runs tasks on the local machine
    #           pbs, uses the PBS scheduler to obtain nodes on a cluster,
    #           etc
    # hosts: list of hosts to use (for ssh backend)
    # nodes: number of hosts to use (for cluster-based backends)
    # cores: number of cores per host to use #TODO: check
    # time: (pbs and sge servers only) walltime in hh:mm:ss
    #       Default is 30 minutes on these servers, unlimited
    #       elsewhere
    # wkloglvl: logging level.  Settings are NONE, ERROR, WARn, INFO,
    #               DEBUG, TRACE
    # Options which are server and site-specific:
    #   project, queue
    # Swift returns a descriptor for the worker process, which 
    # can be passed to swiftShutdown to stop the worker.


    # In case it was somehow deleted
    if (!exists(".swift.workers") 
        || is.null(.swift.workers)) {
        .swift.workers <<- list()
    }

    # Find out where start-swift script lives in this
    # R installation
    # Presume UNIX path names - start-swift script
    cmdString <- file.path(.find.package("Swift"), "exec/start-swift-daemon")

    
    if(is.null(server))
        server <- "local"
    cmdString <- paste(cmdString, "-s", shQuote(server)) 

    
    if (is.null(cores)) {
        if (server == "local") {
            cores <- getOption("swift.system.cores")
            if (is.null(cores)) 
                cores <- 2
        }
        else if (server == "ssh") 
            cores <- 4
        else 
            cores <- 8
    }
    if(! is.null(cores) )  {
        cmdString <- paste(cmdString, "-c", shQuote(cores))
    }

    if( is.null(hosts) )  {
        if (server == "ssh") {
            stop(paste("Need to provide hosts list for ssh server."))
        }
    }
    else {
        if (length(hosts) > 1) {
            # Concatenate list of hosts
            hosts <- paste(hosts, collapse=" ")
        }
        else if (length(hosts) == 0) {
            stop(paste("Zero length host argument:", hosts))
        }
        cmdString <- paste(cmdString, "-h", shQuote(hosts) )
    }
    
    if(! is.null(parEnv) )  {
        cmdString <- paste(cmdString, "-e", shQuote(parEnv)) 
    }

    if(! is.null(kernel) )  {
        cmdString <- paste(cmdString, "-kernel", shQuote(kernel)) 
    }
   
    if(! is.null(workmode) )  {
        cmdString <- paste(cmdString, "-m", shQuote(workmode)) 
    }
    if (is.null(retries) ) {
        retries <- 3
    }
    cmdString <- paste(cmdString, "-retries", shQuote(retries))

    if (server == "local")
        nodes <- 1
    else {
        if(is.null(nodes))
            if (server == "ssh") {
                nodes <- length(strsplit(hosts, " ", fixed=T)[[1]])
#                cat("Node count for hosts '", hosts, "' is ", nodes)
            }
            else 
                nodes <- 1 # Default value
        if(! is.null(nodes) )  
            cmdString <- paste(cmdString, "-n", shQuote(nodes)) 
    }
    
    if (is.null(throttle)) {
        # number of simultaneous jobs / 10:
        #   Round up to nearest integer
        throttle <- as.integer((cores * nodes / 10.0) + 1)
    }
    if(! is.null(throttle)) 
        cmdString <- paste(cmdString, "-p", shQuote(throttle)) 

    if(! is.null(queue) )  
        cmdString <- paste(cmdString, "-q", shQuote(queue)) 
    
    if(! is.null(project) )  
        cmdString <- paste(cmdString, "-A", shQuote(project)) 
    
    if(! is.null(rcmd) )  
        cmdString <- paste(cmdString, "-r", shQuote(rcmd)) 

    if(! is.null(time) )  
        cmdString <- paste(cmdString, "-t", shQuote(time)) 
    
    if(! is.null(workerLogging) )  
        cmdString <- paste(cmdString, "-w", shQuote(workerLogging)) 
    
    if( (! is.null(swiftLogging)) && swiftLogging )  
        cmdString <- paste(cmdString, "-L")
    
    if(!is.null(keepworkdir) && keepworkdir)
        cmdString <- paste(cmdString, "-k")

    if (!is.null(getOption("swift.trace")) && getOption("swift.trace"))
        cmdString <- paste(cmdString, "--trace")
    
    if (server == "custom") {
        if (is.null(sites.file)) {
            stop(paste("sites file must be specified by argument or",
                    "option for custom server"))
        }
        if (is.null(tc.file)) {
            stop(paste("tc file must be specified by argument or",
                    "option for custom server"))
        }
        if (is.null(cf.file)) {
            stop(paste("cf file must be specified by argument or",
                    "option for custom server"))
        }
        cmdString <- paste(cmdString, "--cf.file", shQuote(cf.file), 
                "--tc.file", shQuote(tc.file), "--sites.file", shQuote(sites.file))
    }

    # launch server. 
    out <- suppressWarnings(system(cmdString, intern=TRUE))
    if (length(out) != 3 && length(out) != 2) {
        stop(paste("Unexpected output from start-swift: '", 
            paste(out, collapse="\n"), "'", "Launching may have failed"))

    }
    pid <- out[[1]]
    workdir <- out[[2]]
    
    output <- list()
    output$pid <- pid
    output$server <- server
    output$workdir <- workdir
    output$cores <- cores
    output$nodes <- nodes
    
    if (length(out) == 2) {
        # didn't get status message
        killWorkerProcess(output, quiet=T)
        stop("swiftInit failed: no status message from start-swift\n")
    }
    stat <- out[[3]]
    if (stat == "ok") {
        cat("Started worker manager with pid", pid, "\n")
        # store worker info
        .swift.workers[[length(.swift.workers) + 1]] <<- output
        # add hook to ensure child process will be killed when 
        # this process exits
        addHook()

        # Sleep to give start-swift time to set up fifos,etc

        return (invisible(output))
    }
    else {
        killWorkerProcess(output, quiet=T)
        stop(paste("swiftInit failed with error:", stat))
    }
}

swiftShutdown <- function(handle=NULL, all=FALSE) {
    if (!exists(".swift.workers") 
            || is.null(.swift.workers)
            || length(.swift.workers) == 0) {
        cat("No swift workers were started in R\n")
        return
    }
    if (is.null(handle)) {
        if (all) {
            workers <- .swift.workers
            .swift.workers <<- list()
            cat("Shutting down all Swift worker processes\n")
        }
        else {
            # Remove the last started worker
            workers <- .swift.workers[length(.swift.workers)]
            .swift.workers <<- .swift.workers[1:length(.swift.workers)-1]
        }
    }
    else {
        # Split between matching workers and non-matching workers
        workers <- Filter(
            function (worker) { return (worker$pid == as.character(handle$pid)) ; },
            .swift.workers)
        .swift.workers <<- Filter( 
            function (worker) { return (worker$pid != as.character(handle$pid)) ; },
            .swift.workers)
    }
    # shut down all worker processes using kill
    for (worker in workers) {
        killWorkerProcess(worker)
    }
}

killWorkerProcess <- function (worker, quiet=F) {
    if (! quiet) {
        cat(paste("Terminating worker", worker$pid, "of type", 
                worker$server, "\n"))
    }
    cmdString <- file.path(.find.package("Swift"), "exec/killtree")
    killCmd <- paste(cmdString, worker$pid, " &> /dev/null")
    system(killCmd, wait=FALSE)
}

workerCount <- function (server) {
    # workerCount gets the number of workers launched by the previous

    # Find the last launched worker of the right type.
    # TODO: this is a bit flakey as there is no guarantee
    # the currently active worker of this type was launched within
    # R
    worker <- getWorker(server)    
    if (is.null(worker)) {
        return (NULL)
    }
    else {
        return (worker$cores * worker$nodes)
    }
}

# Find the most recently launched instance of a server type.  If
# no argument provided, the most recent server of any type
getWorker <- function (server=NULL) {
    if (!exists(".swift.workers") 
        || is.null(.swift.workers)
        || (length(.swift.workers) == 0)) {
        return (NULL)
    }
    if (is.null(server)) {
        return (.swift.workers[[length(.swift.workers)]])
    } 
    else {
        worker <- Find(
                function (worker) { return (worker$server == server) ; },
                .swift.workers, right=TRUE)
        return (worker)
    }
}

getWorkerDir <- function (server=NULL, tmpdir=NULL) {
    worker <- getWorker(server)
    if (!is.null(worker)) {
        return (worker$workdir)
    }
    else {
        if (!is.null(server)) {
            if (is.null(tmpdir)) tmpdir <- chooseTmpDir()
            user <- Sys.info()[["user"]]
            return (file.path(tmpdir,user,"SwiftR", 
                paste("swift.",server,sep="")))
        }
        else {
            stop(paste("No SwiftR servers launched within R and no server type",
                    "specified, can't identify a likely location for a swiftR",
                    "service"))
        }
    }
}
