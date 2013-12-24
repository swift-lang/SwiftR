\name{swiftapply}
\alias{swiftLapply}
\alias{swiftSapply}
\alias{swiftapply}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Applying a Function to a List of Arguments
}
\description{
    Several variations of a parallel apply call are provided.
    All variations take a function, and list of argument lists,
    and applies the function to each of the argument lists.
    This is done in parallel using the Swift engine.

    swiftLapply behaves in the same way as the serial \code{lapply}
    function.
    
    swiftSapply behaves in the same way as the serial \code{sapply}
    function.

    swiftapply is a more general function that takes additional arguments.
}
\usage{
swiftapply(func, arglists, server = NULL, callsperbatch = NULL, runmode = NULL, initialexpr = NULL, workerhosts = NULL, keepwork = NULL, tmpdir = NULL, timeout = NULL, quiet = NULL, printTiming = NULL)

swiftLapply(tlist, func, ...) 

swiftSapply(tlist, func, ..., simplify = TRUE, USE.NAMES = TRUE) 
}
\arguments{
  The first two arguments are required for all variations of the
  apply.  
  
  All other arguments are optional.  
  swiftapply takes the full range of options specified below directly
  as arguments.    If they are not provided as arguments, settings specified
  through R's options mechanism will be used, for example 
  \verb{options(swift.server="local"}.
  swiftLapply and other variations also respect arguments
  set through the R options mechanism.

  \item{func}{
    The function to apply.
}
  \item{arglists}{
    A list of argument lists to be given to the function. 
}
  \item{tlist}{
    For swiftLapply and swiftSapply, a list which contains the first
    argument for each function invocation.
}
  \item{...}{
    For swiftLapply and swiftSapply, the second, third, etc arguments
    for each function invocation.  For example, 
    \verb{swiftLapply(list(1,2), f, "x", 20)} results in the function 
    invocations \verb{f(1, "x", 20)} and \verb{f(2, "x", 20)}.
}
  \item{server}{
    The swift server type to use to run.  The possible values are
    "local", "ssh", "pbs", "sge" and "pbsf", the same as \verb{swiftInit}.
    The most recently started server of the specified type will be used
    to execute the apply call. If not specified, then 
    the swift.server option will be used.  If that is not specified, the
    most recently launched server of any type will be used.
}
  \item{callsperbatch}{
    The number of function calls to group together into a single batch.
    A higher batching factor reduces the overhead of running many batchs,
    but reduces the available amount of parallelism.  Consider a low 
    value if each function call involves a substantial amount of work: 
    1 may be an appropriate value if each function call takes more than 2-3 
    seconds to complete.  Consider a high value if each function call
    involves a minimal amount of worker.
}
  \item{runmode}{
    For testing purposes only.
}
  \item{initialexpr}{
    Each function call is run in a fresh R session.  initialexpr is a
    fragment of R code that performs any required setup in the session.
}
  \item{workerhosts}{
    For testing purposes only.
}
  \item{keepwork}{
    Whether to store intermediate files.  For testing purposes.
}
  \item{tmpdir}{
    The system's temporary directory to store working files under.  
    The default value is generally sufficient.
}
  \item{timeout}{
    If provided, swiftapply will return if a response is not received by
    the swift server in the specified time window in milliseconds. The timeout
    applies to two time intervals: receiving an acknowledgement from the swift
    server that the request was received, and receiving the final result.
    
}
  \item{quiet}{
    Whether to suppress informational output about the job.
}
  \item{printTiming}{
    If set to TRUE, prints timing information on components of the
    apply operation.
}
  \item{simplify}{
    Whether to simplify the return value of \code{swiftSapply} following the
    same rules as \code{sapply}.
}
  \item{USE.NAMES}{
    Same behaviour as the equivalent argument to \code{sapply}.

}
}
\details{
Before running \verb{swiftapply} for the first time in a session you will likely 
want to run \verb{swiftInit} to start a Swift server.
}
\value{
    Returns a list with return values corresponding to the argument 
    lists provided to swiftapply.
    An error will be raised if for some reason no results could be
    retrieved.  If individual argument lists cause an error when
    the function is run, the corresponding list entry in the
    return list will contain a "\code{try-error}" object.
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
\code{\link{swiftInit}}
\code{\link{Swift}}
}
\examples{
\dontshow{library(Swift)}
\dontshow{swiftInit()}
swiftapply(log, list(list(5),list(6)))

# swiftLapply returns result as list
swiftLapply(list(1,2,3), function (x) { 2 * x })
swiftLapply(c(1,2,3), function (x) { 2 * x })

# swiftSapply will coerce result to vector
swiftSapply(c(1,2,3), function (x) { 2 * x })

pow <- function (x, y) {
    x ^ y
}

# swiftapply allows multiple argument functions by taking a list of lists
swiftapply(pow, list(list(2,2), list(2,3), list(7,3)))

# swiftLapply and swiftSapply allow only one value to be provided for the
# second and subsequent arguments for the multiple function invocations
swiftLapply(list(1,2,3,4,5,6,7,8,9), pow, 2)
swiftSapply(list(1,2,3,4,5,6,7,8,9), pow, 3)
\dontshow{swiftShutdown()}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
