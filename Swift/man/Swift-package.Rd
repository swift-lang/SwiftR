\name{Swift-package}
\alias{Swift-package}
\alias{Swift}
\docType{package}
\title{
R interface to for parallel apply() calls using Swift
}
\description{

Apply R functions to lists of arguments in parallel, on distributed
resources.  Resources selection and access, scheduling, and throttling
are provided using the Swift parallel scripting language,
transparently to the R user.

Currently supports local multicore CPUs; clusters runnng PBS; and
ad-hoc clusters composed of one or more remote multicore systems
accessible via SSH.

Remote resources are managed by Swift worker agents started by SSH or
the cluster scheduler, and accessed via a Swift protocol. Each pool of
resources is called a "Swift server". Multiple Swift servers can be
accessed from one R client,

}

\details{
\tabular{ll}{
Package: \tab Swift\cr
Type: \tab Package\cr
Version: \tab 0.1.2\cr
Date: \tab 2011-01-28\cr
License: \tab Globus Toolkit Public License v3 (based on Apache License 2.0):
http://www.globus.org/toolkit/legal/4.0/license-v3.html \cr
LazyLoad: \tab yes\cr
}

FIXME: Use the traditional R format for the initial part of the man page.

The main function in this package is:

resultList = swiftapply(function,listOfArgumentLists)

swiftapply() invokes function on each of the arguments lists in
listOfArgumentLists, and returns a list of the result of the invocation in the corresponding
member of resultList. Each invocation is executed in parallel in an
external, independent copy of R.

This is the most general function in the Swift package, as each
arglist is specified in full and can contain unique values for all
arguments for each invocation of function.

Options that determine the behavior of swiftapply can be set via
options(swift.*=value) or via optional keyword arguments.

To use this package, create a list of argument lists, and then invoke:

\verb{resultList = swiftapply(function, listOfArgumentLists)}

For example:

\preformatted{
arglists=list()   
for(i in 1:10) {
  arglists[[i]] = list(i)
}
resultlist = swiftapply(sqrt,arglists)
}

The same conventions as used by the Snowfall package are used for swiftLapply, analogous to sfLapply:

\preformatted{
r = swiftLapply(seq(1,10),sqrt)
}

Currently swiftLapply is the only one implemented (i.e. swiftSapply etc are not yet provided but will be soon).

Arbitrary R objects can be passed. For example:

\preformatted{
    data(cars)
    data(trees)
    sumstuff <- function(treedata, cardata) {
        sum(treedata$Height, cardata$dist)
    }
    args = list(trees, cars)
    arglist = rep(list(args), 10)
    res = swiftapply(sumstuff, arglist)
}


\code{swiftapply} and \code{swiftInit} take a range of arguments to
control the setup of the cluster and how parallel apply calls are handled.
You can set global default values of these settings through R options().

Some key options are:

options(swift.callsperbatch=n) # n = number of R calls to perform in
each Swift job.

options(swift.server="servername") # servername = "local" to run on
the current host, "ssh" to run on remote machines via ssh, "pbs" to submit to a local PBS cluster and "sge" for a local Sun Grid Engine cluster. An additional
setting, "pbsf" is provided for clusters such as Merlot which have 
firewalls that restrict outbound cnnectivity from the worker nodes 
to the Swift server running on the login node.
 
options(swift.keepwork=TRUE) # Retain the temporary files that the
Swift functions use to pass R data from client t remote R
servers. This is useful for debugging.

}

\section{PREREQUISITES}{

To run Swift, you need a Java runtime environment (JRE) installed on
the client machine (where the client R workspace will be executed).
Worker nodes (remote resources) only need Perl (to run the Swift
worker agent).

Remote resources can be accessed via SSH or the PBS batch scheduler.

Details:

1) Sun Java 1.4 or higher (preferably 1.6) installed and in your PATH

Download the appropriate Java Runtime (JRE) for Linux at:\verb{
     http://www.java.com/en/download/manual.jsp}

Typically either 32 bit with the link named:\verb{
     "Linux (self-extracting file)  filesize: 19.9"}

...or 64 bit with the link named: \verb{
	"Linux x64 (self-extracting file)  filesize: 19.3 MB" }

Its better to install the full development kit (JDK, ~80MB) from:

	http://www.oracle.com/technetwork/java/javase/downloads/jdk6-jsp-136632.html (JDKs)

(This will enable you to compile Swift revisions from the Swift development trunk)

Mac OS X: Download ...  FIXME: Mac OS X is not yet tested for use with
this package. Few issues are expected, but some shell command
differences may affect the scripts in this package.

Verify that you have Sun Java installed and in your PATH correctly by doing:

\verb{
$ java -version
java version "1.6.0_21"
Java(TM) SE Runtime Environment (build 1.6.0_21-b06)
Java HotSpot(TM) 64-Bit Server VM (build 17.0-b16, mixed mode)
$ }

2) R v2.11 or higher in your PATH (on client and server machines)

(Testing has been done on R 2.11.  The package is likely to operate on
older R versions as well, but has not yet been validated on them).

3) Access to Parallel Resources - You can run Swift R package using:

(a) Multiple cores on your local login host or workstation/laptop

(b) One or more remote machines, possibly each a multicore, accessed via ssh

(c) Clusters running PBS, or SGE schedulers.  More cluster schedulers will
    be supported in the future.

You can select between options a), b) and c) using the "swift.server" option
or by providing a "server" argument to \code{swiftInit} and \code{swiftapply}.
Other arguments, documented on the \code{swiftInit} manual page, allow
you to specify the parameters, such as number of cores.

In cases b) and c), the Swift workers, running on remote machines or 
cluster nodes, must be able to connect back to the Swift server (running
on the same machine as R) on TCP
ports in the range of 30000 and higher.  (FIXME: determine specifics).


If this is not available on a cluster (e.g., Merlot), then the pbsf
server will tunnel the Swift port over the standard ssh port, assuming
that is reachable.  If pbsf is in use, and 
your Swift server machine has multiple network
interfaces, you may need to set the GLOBUS_HOSTNAME environment variable
to specify the network address workers should connect to.



}

\section{INSTALLATION}{
Installation is through the standard R CMD INSTALL command.


If you have access to install libraries directly in your R
installation.
\preformatted{
wget http://www.ci.uchicago.edu/~wilde/Swift_0.2.tar.gz
R CMD INSTALL Swift_0.2.tar.gz
}

If you keep your libraries in a separate location in your home directory,
or you do not have access to modify the R installation.
\preformatted{
mkdir ~/RLibrary # if you have not already created a library folder
wget http://www.ci.uchicago.edu/~wilde/Swift_0.2.tar.gz
R CMD INSTALL -l ~/RLibrary Swift_0.2.tar.gz

# tell R where to find the library: add this to your .bashrc file or equivalent
export R_LIBS=~/RLibrary:$R_LIBS 

}
}

\section{QUICK_START}{
In a terminal window:
\preformatted{

$ export R_LIBS=$HOME/RLibrary

$ R
}
Now in R:
\preformatted{
> require(Swift)
> options(swift.server="local")
> basicSwiftTest()   # should take about 1 second
> runAllSwiftTests() # should take < 60 seconds


}

}

\section{ENVIRONMENT_VARIABLES}{

If used, these variables must be exported in the UNIX environment in which
swift-start is executed:

SWIFTR_TMP sets the root directory below which Swift will maintain its
directory structure.  Defaults to /tmp. For, e.g., the PADS cluster,
its best to set this to /scratch/local, as /tmp is very limited in
space. Seldom needed.

For the pbsf server type only: GLOBUS_HOSTNAME should be set to the IP address of the login host if
it contains multiple network interfaces (see /sbin/ifconfig) and if
the worker nodes can only reach the login host on a subset of these
interfaces.
}

\section{START_SERVERS}{

To run \code{swiftapply()} and any of the \code{swiftXapply()} 
functions, you first
start one or more "Swift servers" on your local host (where you will
run the R client workspace.  You can do this with the \code{swiftInit()}
function in R.
Examples of starting the Swift server follow.

To run 4 parallel R servers on the local host:

\verb{swiftInit(server="local", cores=4)}

To run 4 R servers on each of two hosts that can be reach by ssh:

\verb{swiftInit(server="ssh", cores="4", hosts="hostname1 hostname2")}

To run 8 R servers for 30 minutes on each of 3 nodes of the Merlot cluster, run this on the login host "merlot", using its "serial" queue:

\verb{swiftInit(server="pbsf", cores=8, nodes=3, time="00:30:00", 
        queue="serial")}

These Swift servers can be started and left running for multiple 
\code{swiftapply()}
calls, and will be shut down when you close your R session, when the Swift
package is unloaded or when \code{swiftShutdown()} is called.

}

\section{TESTS}{

Running a hello world test:

\preformatted{
# Start swift local server as above

require(Swift)
basicSwiftTest()
}

Running a full test

\preformatted{
require(Swift)
# runAllSwiftTests automatically starts up and shuts down a swift server
runAllSwiftTests()
}

Running full tests n times:

\preformatted{
testLoop(n)
}

}

\section{OPENMX_EXAMPLES}{

This section is specific to users of the OpenMX R package for
structural equation modeling.

}

\section{SSH-specific issues}{

For the "ssh" server to work correctly within SwiftR, it is best that
you have ssh configured for password-free login to the remote worker nodes
you will be using.



%Ability to ssh to server machines (without password: agents, master
%control channel, etc) (FIXME: Are these limitations necessary?)
%Passwords or ssh key passphrases OK for some scenarios.

%ssh from Mac

%ssh -A when jumping to a new host (to forward the ssh agent)

%(or set up ssh agents manually)

%(document ssh tricks here for pw-less access)

}

\section{Swift Runtime Directory Structure}{
SwiftR stores various files in the file system in the course of its operation.
These are, by default, stored under the directory "$TMP/$USER/SwiftR".

The Swift server services will store logs and other information under 
directories
with names beginning with "swift.".
One of these directories is created per \code{swiftInit} call.  

Swift workers store data files and logs under "$TMP/$USER/SwiftR" on
whichever machine they are currently running on.

}

\section{DEBUGGING AND TROUBLESHOOTING}{

* manual mode

You can get Swift to stop after it has produced the call batch file
(an R save()'d object file. You can manually load this in another R
workspace, inspect the contents, manually onvoke the remote call, and
end the data back. This is useful if issues arise in the transparency
of function, argument, and retrun value marshalling, and if there are
concerns about the transparency of the remote execution.

* logs to look at

See the section on the directory structure of Swift services.

"info" files returned from each execution can contain messages if the remote R jobs fail to launch.

Each R server has its own log, on the host on which the server executes.

* is my swift server responding?

tail -f $TMP/$USER/SwiftR/swift.local/swift.stdouterr

You should see periodic status update lines such as the following:

\preformatted{Progress:  Selecting site:6  Active:2  Finished successfully:84
Progress:  uninitialized:1  Finished successfully:92
Progress:  Stage in:1  Finished successfully:101
}

* reporting bugs: what to send  (FIXME: need swiftsnapshot script)

* setting Swift worker logging with $HOME/.globus/coasters/loglevel
file. This file should contain a single text integer: 0=most detailed,
4=least detaild, 5=off.  This is an interim log control mechanism and
may be deprecated in the future.
}

\section{CHECKOUT AND BUILD SWIFT R PACKAGE FROM SVN}{

(with compiled swift binary release (trunk) from svn)

\preformatted{
cd ~
svn checkout https://svn.ci.uchicago.edu/svn/vdl2/SwiftApps/SwiftR
cd SwiftR/Swift
mkdir swift
cd swift
wget http://www.ci.uchicago.edu/~wilde/swift.rNNNN.cog.rNNNN.tar.gz

cd ~/SwiftR
make install
}

}

\section{CAVEATS}{

Swift requires Sun Java 1.4 or above; preferably 1.6. It will not run
under gcj (GNU Java) although it is getting closer to being able to
and may work - to some extent in in some settings.  You need to ensure
that a suitable Sun Java in in your PATH.

In addition, the environment variable CLASSPATH should not be set.

Variables set in the initialize script will appear in the global
environment of the remote workers.  
The initialexpr script is passed in the same saved/loaded R
object as R functions and arguments. Thus if the expr needs to be run
before these objects can be loaded, then an alternate mathod of
intialization needs to be used. (Eg the original method of passing th
einitvar as a separate file).

A set of libraries to be loaded remotely can be maintained with the
\code{swiftLibrary} and \code{swiftDetach} calls.  In some cases
it is desirable to send data back and forth using the \code{swiftExport}
call.

All Swift servers do a \verb{require(methods)} call when they
start. Its not clear if this is unversally desired. It was not done by
default in Rserver but seems to be in interactive R.

Running on systems like Merlot with very restrctive firewalls requires
that the user first export the env var GLOBUS_HOSTNAME, set to the IP
address of the network interface by which the worker nodes can connect
to the login host on which the R client and start-swift commands are
running.

pbs and pbsf servers try to clean up all worker nodes used with a
killlall -u $USER. This can be made more precise to avoid killing jobs
on shared worker nodes.

The following caveats are high priority on the FIXME list:

Only lapply is implemented (also SwiftApply) - need to see if we can
cut down arg passing overhead for many of the apply() cases.

Log records build up fast; these will be reduced as we get more
confidence with the code and shake out bugs,

There is no asynchronous swiftapply call yet. Each call must complete
before control is returned to the R command loop.

Lower priority issues are:

There is no easy way yet to alter Swift configuration file variables

Swift echoes its options on every call. It will be made silent and
"verbose" will be made an option.

}

\author{

Swift was developed by: Mihael Hategan, Ben Clifford, Justin Wozniak,
Yong Zhao, Ian Foster, and Michael Wilde with contributions from Sarah
Kenny, Ioan Raicu, Luiz Gadelha, Allan Espinosa, Zhao Zhang, David
Kelly, Jon Monette, Glen Hocky, Tom Uram, Wenjun Wu, and other users.

Swift R package developed by Michael Wilde, Tim Armstrong and the OpenMx project

Maintainer: Michael Wilde <wilde@mcs.anl.gov>

}
\references{
\url{http://www.ci.uchicago.edu/swift}
}
\keyword{ parallel and distributed execution }
\seealso{
\code{\link{swiftInit}}
\code{\link{swiftShutdown}}
\code{\link{swiftapply}}
\code{\link{swiftLibrary}}
\code{\link{swiftExport}}
\code{\link{runAllSwiftTests}}
}
\examples{
library(Swift)
job <- swiftInit()

myfunc <- function(treedata,cardata) { sum( treedata$Height, cardata$dist ) }
data(cars)
data(trees)

args=list(trees,cars)
arglist = rep(list(args),10) # Create 10 calls

res = swiftapply(myfunc,arglist)

res = swiftapply(myfunc,arglist,callsperbatch=5)

# res = swiftapply(myfunc,arglist,callsperbatch=2,site="pbs")
swiftShutdown(job)
}
