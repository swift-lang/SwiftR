\name{getNodeList}
\alias{getNodeList}
\title{
Retrieve the names of all nodes for cluster batch job
}
\description{
When R is running in the context of a pbs, sge or cobalt batch job,
finds the list of hosts assigned to the job.
}
\usage{
getNodeList(server = getOption("swift.server"))
}
\arguments{
  \item{server}{ Optional.  If set to "pbs", "pbsf", "sge" or "cobalt" - 
        corresponding to values of the server argument of \code{swiftInit} -
        then it will only try to get the node list via the appropriate 
        mechanism for that batch scheduler.  
        
        If unset, the value is obtained from the swift.server option.
        
        If the argument value cannot be obtained directly or via options,
        then it will try to find the node list using any of the above batch
        schedulers.
}
}
\value{
This function will return a data frame where the first column
is the unique host name for the cluster node, and the second
column is the number of processes assigned to run on that node.
}

\seealso{
\code{\link{swiftInit}}
}
