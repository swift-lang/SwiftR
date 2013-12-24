\name{swiftExport}
\alias{swiftExport}
\alias{swiftExportAll}
\alias{swiftRemoveAll}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Exporting global data to remove R workspaces.
}
\description{
    \code{swiftExport} provides a mechanism to export data and functions to remote
    R workspaces.  It is mainly useful in the case where a function being
    used in a swift*apply call references functions or data in the global 
    workspace.  \code{swiftExportAll} exports all non-hidden variables in 
    the global environment of the R session (i.e. those shown by an
    invocation of \code{ls()} at the interactive prompt).
    \code{swiftRemoveAll} removes all previously exported data from
    remote R workspaces.
    
}

\usage{
swiftExport(..., list = NULL, file = NULL, quiet=getOption("swift.quiet"))
swiftExportAll(file = NULL)
swiftRemoveAll()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
    Names of data items to export, either as strings or R identifiers.
}
  \item{list}{
    A list of strings which name data items to export.
}
  \item{file}{
    Optional: a file which is on a file system shared between the local
    R session and remote R workers in which to store the exported data.
    If not provided, a file will automatically be chosen.
    A directory can be specified via the swift.exportdir option.
}
  \item{quiet}{
    Whether informational messages should be suppressed
  }
}
\details{
In many cases if a data or a function is referenced within a function
given to swift*apply it will automatically be transferred when the 
apply call is carried out.  This happens if it is a local
variable in some function, or if the reference refers to something
defined in a package that is loaded on the remote machines.

However, functions or data in the global environment (e.g. those defined 
directly in an interactive R session, or assignments using the \code{<<-} 
operator are not automatically transferred to remote R workspaces 
when a swift*apply call is carried out.  

If you are working with a large data set, it can be more efficient to store
the data in the global environment and use swiftExport to transfer the 
data.  R workers load exported data on demand,
so if it is referenced in multiple apply calls, the exported data only needs
t be transferred once.

swiftExport currently assumes that the swift worker processors share a file 
system with the current machine.  The directory through which exported data
is transferred can be specified with the swift.exportdir option.

The set of exported data persists across multiple swift sessions: if you shut
down a swift session and start a new one with \code{swiftInit}, the set of 
exported data will be the same until you run \code{swiftRemoveAll}.

}
\value{
    No values are returned.
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
\code{\link{Swift}}
}
\examples{
    
swiftInit(server="local", cores=2)

# global data
mydata <- c(1,2,3,4)

getter <- function (i) { mydata[[i]] }

# Won't be able to find mydata
swiftLapply(1:4, getter)

swiftExport(mydata)
# or: swiftExport(list=c("mydata"))
# This should work better!
swiftLapply(1:4, getter)

swiftRemoveAll()
# won't work again
swiftLapply(1:4, getter)

swiftExportAll()
# works again
swiftLapply(1:4, getter)

swiftShutdown()

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
