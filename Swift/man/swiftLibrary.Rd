\name{swiftLibrary}
\alias{swiftLibrary}
\alias{swiftDetach}
\alias{swiftDetachAll}
\title{
    Loading and unloading remote Swift libraries.
}
\description{
These functions allow packages to be loaded and unloaded on remote R instances
managed by SwiftR.  
}
\usage{
swiftLibrary(packname)
swiftDetach(packname)
swiftDetachAll()
}
\arguments{
  \item{packname}{
    The name of the package to attach or detach.
    The arguments accepted are the same as the standard R \code{library}
    and \code{detach} functions.  They can either be provided as R identifiers
    (e.g. \code{library(Swift)}) or as strings (e.g. \code{library("Swift")}).
    swiftDetach accepts package names with a "package:" prefix in the same
    was as \code{detach} (e.g. \code{detach(package:Swift)}).
}
}
\details{
If a library is added through the \code{swiftLibrary} call, 
it will be loaded for any future swift*apply calls.  The library can be unloaded using \code{swiftDetach} or \code{swiftDetachAll}.  
The implementation guarantees that any subsequent swift*apply calls are run in 
R workspaces with the appropriate set of packages loaded.

Note that the package will not be loaded into the current R workspace. 

If you pair up \code{swiftLibrary} and 
\code{swiftDetach} calls with the local versions, 
you will have the same libraries loaded on remote R workspaces and in the local
workspace.

\code{swiftDetachAll} unloads all libraries previously loaded using this
mechanism.
}
\value{
    No value is returned.
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
}
\examples{
library(Swift)
swiftInit()
swiftLibrary(stats)
swiftLapply(list(0.1, 0.2, 0.3), 
    function (x) { pbeta(x, 2, 3) })
swiftShutdown()

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
