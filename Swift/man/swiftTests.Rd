\name{swiftTests}
\alias{runAllSwiftTests}
\alias{basicSwiftTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Swift Test Suite
}
\description{
These functions run automated tests for the Swift package.
}
\usage{
runAllSwiftTests(...)
basicSwiftTest(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
    Any arguments to be passed to \code{swiftInit} when starting up a 
    test cluster.
}
}
\details{
    Run the SwiftR test suite and report on successes and failures.
    \code{basicSwiftTest} runs a single test as a sanity check for
    the system.  \code{runAllSwiftTests} tests a broader range of
    SwiftR functionality.
}
\value{
    \code{runAllSwiftTests} returns a data structure describing the results of the test suite.
    \code{basicSwiftTest} returns a boolean value, TRUE if the test was successful.
    In the event of some failures the test suite may hang.
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

\seealso{
\code{\link{swiftInit}}
\code{\link{Swift}}
}
\examples{
library(Swift)
runAllSwiftTests()
\dontrun{runAllSwiftTests(server="local", cores=4)}
\dontrun{runAllSwiftTests(server="ssh", cores=4, 
        hosts=c("larry.example.com", "moe.example.com", "curly.example.com"))}
\dontrun{basicSwiftTest(server="pbsauto", nodes=4, cores=8, time="00:30:00")}
}
