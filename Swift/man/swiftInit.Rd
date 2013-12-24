\name{swiftInit}
\alias{swiftShutdown}
\alias{swiftInit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Starting and Stopping Swift Workers
}
\description{
\code{swiftInit} prepares for parallel Swift jobs to be run by initialising a pool of workers.  Depending on the arguments, these workers may be on the local host, or on a remote host.  \code{swiftShutdown} shuts down workers that were started by \code{swiftInit}.
}
\usage{
swiftInit(cores = getOption("swift.cores"), server = getOption("swift.server"), hosts = getOption("swift.hosts"), nodes = getOption("swift.nodes"), project = getOption("swift.project"), parEnv = getOption("swift.parenv"), kernel = getOption("swift.kernel"), workmode = getOption("swift.workmode"), throttle = getOption("swift.throttle"), retries = getOption("swift.retries"), queue = getOption("swift.queue"), rcmd = getOption("swift.rcmd"), time = getOption("swift.time"), workerLogging = getOption("swift.workerLogging"), swiftLogging = getOption("swift.swiftLogging"), keepworkdir = getOption("swift.keepworkdir"), tc.file = getOption("swift.tc.file"), cf.file = getOption("swift.cf.file"), sites.file = getOption("swift.sites.file"))
swiftShutdown(handle = NULL, all=FALSE)
}
\arguments{
  All arguments to \code{swiftInit} are optional, however some for some
  values of the \code{server} argument other arguments may need to be
  specified directly or via a Swift option.

  If any of the arguments are not provided to swiftInit, the function
  tries to obtain a value using \code{getOption}.  The option
  names correspond to the argument names prefixed with "swift.", for example
  "swift.cores" or "swift.server".

  \item{cores}{
    The number of cores per host.  The default values vary from 2 to 8 depending on the server type.
}
  \item{server}{
    One of: "local", "ssh", "pbs", "sge", "pbsf", "cobalt", "crayxt",
    "pbsauto", "crayxtauto".
    How Swift will run the jobs: for example, if "local" is chosen, they
    will be run on the local machine, or if "pbs" is chosen, they will be
    run through the pbs scheduler.  
    This must correspond to a server type started by \code{swiftInit()}
    If server argument is not provided, the swift.server option will
    be used, and if that is unavailable, "local" will be assumed.
}
  \item{hosts}{
    A string containing a list of remote hosts to run jobs on
    , separated by spaces.  This only is used if server is "ssh".
}
  \item{nodes}{
  The number of nodes to request from the batch scheduler.  This only
  is used if server is "pbs", "sge", "pbsf" and "cobalt".
}
  \item{project}{
  The project name passed to the PBS, Cobalt or SGE batch scheduler.  
	Site-specific.
}
  \item{parEnv}{
  SGE only.  This is the parallel environment setting passed to the
    Sun Grid Engine scheduler, and is required in order to run
    multi-node jobs with SwiftR on Sun Grid Engine sites.  The "mpi"
    environment is often a suitable choice.
}
  \item{kernel}{
  Cobalt only. the kernel to run on the compute nodes.
  }
  \item{workmode}{
  Can be "node" or "slot".
  If "node", one worker is started for all slots on a node.  
  If "slot", one worker is started per slot (multiple workers per node).
}
  \item{throttle}{
  The throttle setting to be used by Swift: controls the rate of sending jobs
  to workers.  0 means that only two function invocations are run at a time.
  1 means that 102 function invocations can be run concurrently.  The general 
  formula for maximum number of parallel function invocations is
  2 + 100 * throttle.

}
  \item{retries}{
  The number of times Swift should retry a task if it encounters an execution error.
  R code which throws errors will not be re-executed, only errors external to R
  such as computer failures will trigger a retry.
}
  \item{queue}{
    The scheduler queue to put jobs in.  This is only relevant for PBS
    and SGE.
}
  \item{rcmd}{
    Specific to SGE clusters: this is the remote shell command that is 
    used, for example \code{ssh} or \code{qrsh}.  
    For most clusters, the default setting "ssh" works
    , but if you have problems it may need to be changed.
}
  \item{time}{
    The duration to request nodes for from the PBS or SGE scheduler.
    Time should be specified in hh:mm:ss format.
    Note that for "local" and "ssh" servers, workers remain indefinitely
    until shut down.
    
}
  \item{workerLogging}{
    For testing purposes: the swift worker logging level.
}
  \item{swiftLogging}{
    For testing purposes: if set to true, log additional trace information to
    Swift log files.
}
  \item{keepworkdir}{
    For debugging purposes: whether to keep the contents of the Swift 
    working directory.
}
  \item{tc.file}{
    If server is set to custom, the path to the Swift transformation catalog 
    file.
}
  \item{sites.file}{
    If server is set to custom, the path to the Swift site file
    file.
}
  \item{cf.file}{
    If server is set to custom, the path to the Swift configuration
    file.
}
  \item{handle}{
    The handle value returned by "\code{swiftInit}" when a worker was
    started up.  If provided, only that worker will be shut down.
    If handle is not provided and \code{all} is FALSE, then the most
    recently started worker that is still running will be shut down.

}
  \item{all}{
    logical; if set to TRUE, shut down all running R workers, rather
    than just workers from a single \code{swiftInit} call.
}
}
\details{
    As a result of calling \code{swiftInit}, a swift server process is started
    in the background.  This process will be cleaned up automatically when
    you exit your R session.  Alternatively, \code{swiftShutdown} can shut down
    some or all server processes without closing R.


}
\value{
    \code{swiftInit} returns a handle to the swift server process started.
    This handle can be used later as an argument to \code{swiftShutdown} to
    shut down the server process.  The actual data in the handle may
    change in later versions of Swift.
}
\references{
\url{http://www.ci.uchicago.edu/swift}
}
\author{
Swift was developed by: Mihael Hategan, Ben Clifford, Justin Wozniak,
Yong Zhao, Ian Foster, and Michael Wilde with contributions from Sarah
Kenny, Ioan Raicu, Luiz Gadelha, Allan Espinosa, Zhao Zhang, David
Kelly, Jon Monette, Glen Hocky, Tom Uram, Wenjun Wu, and other users.

Swift R package developed by Michael Wilde, Tim Armstrong and the OpenMx project

Maintainer: Michael Wilde <wilde@mcs.anl.gov>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{Swift}}
\code{\link{swiftShutdown}}
}
\examples{
# Running a job on the local machine with 8 cores
options(swift.server="local")
serverHandle <- swiftInit(cores=8)
swiftapply(log,list(list(3), list(4)))
swiftLapply(list(3, 4),
            function (x) { return (2*x); })
# Now shut down the server
swiftShutdown(serverHandle)

# Now run remotely via ssh on 2 remote machines called foo and bar
\dontrun{options(swift.server="ssh")
swiftInit(hosts="foo bar")
swiftLapply(list(3, 4), log)
# ssh servers will keep running

# Run using the PBS scheduler
options(swift.server="pbs")
# start a worker through pbs.  Note that, depending on your PBS 
# installation's utilization level and policies, it may take
# a while before swiftapply calls can be processed.
swiftInit()
swiftLapply(c(3, 4), log)

# Close all the workers we started up
swiftShutdown(all=TRUE)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
