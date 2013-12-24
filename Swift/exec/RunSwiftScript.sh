rundir=$1
site=$2
script=$3
workerhosts=$4

SWIFTRBIN=$(cd $(dirname $0); pwd)
cp $script $SWIFTRBIN/passive-coaster-swift $rundir

# FIXME: rework this script to transfer all shells and rscripts
# needed, and to copy in the R prelude for the R server processes (to
# incude for example the OpenMx library)

cd $rundir

# FIXME: how to get the right PATH for R in here?
# use /bin/sh and count on users PATH?
# then add ENV::PATH as an option, e.g., from options(swift.remotepath and swift.remotepath.sitename)

app=/bin/bash

cat >tc <<END
$site	bash	$app    	null	null
#$site	bash	$app    	null	null	ENV::R_TESTS=""
#$site	bash	/bin/bash	null	null	ENV::R_TESTS="";ENV::PATH="$PATH"
#$site	Rscript	Rscript	        null	null	ENV::R_TESTS="";ENV::PATH="$PATH"
# $site	RunR	$runR	        null	null	ENV::R_TESTS="";ENV::PATH="$PATH"
END

cat >sites.xml <<END
<config>

  <pool handle="local">
    <execution provider="local" url="none" />
    <profile namespace="karajan" key="initialScore">10000</profile>
    <profile namespace="karajan" key="jobThrottle">.11</profile>
    <filesystem provider="local"/>
    <workdirectory>$(pwd)</workdirectory>
  </pool>

  <pool handle="passive">
    <execution provider="coaster" url="none" jobmanager="local:local"/>
    <profile namespace="globus" key="workerManager">passive</profile> 
    <profile namespace="globus" key="workersPerNode">8</profile>
    <profile key="jobThrottle" namespace="karajan">.07</profile>
    <profile namespace="karajan" key="initialScore">10000</profile>
    <filesystem provider="local" url="none" />
    <workdirectory>/home/wilde/swiftwork</workdirectory>
  </pool>

  <pool handle="service">
    <execution provider="coaster-persistent" url="http://localhost" jobmanager=""/>
    <profile namespace="globus" key="workerManager">passive</profile>
    <profile namespace="globus" key="workersPerNode">4</profile> <!-- FIXME: make these settable -->
    <profile key="jobThrottle" namespace="karajan">.03</profile>
    <profile namespace="karajan" key="initialScore">10000</profile>
    <filesystem provider="local" url="none" />
    <workdirectory>/home/wilde/swiftwork</workdirectory>
  </pool>

  <pool handle="pbs">
    <profile namespace="globus" key="maxwalltime">00:00:10</profile>
    <profile namespace="globus" key="maxtime">1800</profile>
    <execution provider="coaster" url="none" jobManager="local:pbs"/>
    <profile namespace="globus" key="workersPerNode">1</profile>
    <profile namespace="karajan" key="initialScore">10000</profile>
    <profile namespace="karajan" key="jobThrottle">5.99</profile>
    <filesystem provider="local"/>
    <workdirectory>$(pwd)</workdirectory>
  </pool>

</config>
END

# FIXME: fossil: was here: cat >RunR.sh <<ENDX  # Note: \$ escapes and nested "<<" in "here document" below 

cp $SWIFTRBIN/{EvalRBatchPersistent.sh,SwiftRServer.R} .

script=$(basename $script)
if [ _$site = _passive ]; then
    ./passive-coaster-swift "$workerhosts" -tc.file tc -sites.file sites.xml $script
else
    swift -tc.file tc -sites.file sites.xml $script
fi

exit # FIXME: for debugging: remove fossil code below when no longer needed for reference

# FIXME: for reference

if [ _$site = _passive ]; then
    swiftcmd=./passive-coaster-swift
    # workhosts passed in as argument
else
    swiftcmd=swift
    workerhosts=""
fi

script=$(basename $script)
$swiftcmd "$workerhosts" -tc.file tc -sites.file sites.xml $script

exit 

# FIXME: Remove fossil:

cat >RunR.sh <<ENDX  # Note: \$ escapes and nested "<<" in "here document" below 
#echo PRINTENV:
#printenv
#echo ENV:
#env

cat >serverscript.R <<END

# User initialization statements (if specified):
$(cat initialize.R 2>/dev/null)
# End of user initialization statements

argv = commandArgs(TRUE)

load(argv[1]);

result=list()
for(c in 1:length(rcall\\\$arglistbatch)) {
    # FIXME: run this under try/catch and save error status in results object (need to make it a list: rval + error status)
    result[[c]] = do.call( rcall\\\$func, rcall\\\$arglistbatch[[c]] )
}

save(result,file=argv[2])

END
Rscript serverscript.R \$*

ENDX
