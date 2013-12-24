#! /usr/bin/env bash

export TRAPEVENTS="EXIT 1 2 3 15"  # Signals and conditions to trap

# * expands to nothing if no match
shopt -s nullglob

# Set the umask to prevent any access by other users:
# there is no reason why any other user should need to look at
# the temporary files, etc that we create 
umask 077


# Define internal functions

get-contact()
{
  # Look for:
  # Passive queue processor initialized. Callback URI is http://140.221.8.62:55379

  for try in $($SEQ 1 120); do
    uriline=$(grep "Passive queue processor initialized. Callback URI is" $out 2> /dev/null)
    if [ "_$uriline" = _ ]; then
      sleep 1
    else
      break;
    fi
  done

  if [ "_$uriline" = _ ]; then
    echo "$0: No passive state message from Swift - exiting."
    stdcleanup_start
    stdcleanup_end
    exit 1
  fi

  CONTACT=$(echo $uriline | sed -e 's/^.*http:/http:/')
  #echo Coaster service contact URI: $CONTACT
}

function wait-and-start-ssh-workers
{
  get-contact
  LOGDIR=$(pwd)/swiftworkerlogs # full path. FIXME: Generate this with remote-side paths if not shared dir env?
  LOGDIR=$topdir/swiftworkerlogs  # FIXME: left this in /tmp so it works on any host. Better way?

  #  mkdir -p $LOGDIR # is done with the ssh command, below

  IDLETIMEOUT=$((60*60*240)) # 10 days: FIXME: make this a command line arg
  sshpids=
  for host in $(echo $hosts); do
    timestamp=$(date "+%Y.%m%d.%H%M%S")
    random=$(awk "BEGIN {printf \"%0.5d\", $RANDOM}")
    ID=$timestamp.$random
    echo "Starting to launch worker on host: $host"
       # FIXME: make logging an argument; set false by default
       # fixme:send worker.pl to remote host via stdin or scp.
       if ssh $host /bin/sh -c \'"mkdir -p $LOGDIR"\'
       then 
           if scp $SWIFTBIN/worker.pl $host:$LOGDIR > /dev/null
           then
               STARTCMD="WORKER_LOGGING_LEVEL=$workerLogging; $LOGDIR/worker.pl $CONTACT $ID $LOGDIR $IDLETIMEOUT 2>&1 &"'
                            echo PGID=`ps --no-headers -o '%r' \$\$`'
               #echo ssh $host '/bin/sh -c '\'"$STARTCMD"\'  >remotepid.$host </dev/null &
               ssh $host '/bin/sh -c '\'"$STARTCMD"\'  >remotepid.$host </dev/null &
               sshpids="$sshpids $!"
               echo "Workers started on $host"
           else
               echo "Error sending file to $host"
           fi
      else
          echo "Error contacting $host or creating directory $LOGDIR on host"
      fi


  done
  if [ "$sshpids" = "" ]; then
      echo No ssh workers successfully launched
      if [ "$doack" = TRUE ]; then
        echo 'Error: no ssh workers launched' > ackfifo
      fi
  else
      echo Started workers from ssh processes $sshpids
      echo $sshpids > $sshpidfile
      if [ "$doack" = TRUE ]; then
        echo ok > ackfifo
      fi
  fi
}

function setup-crayxt-lustre {
    # Set up working directories on lustre file system
    #FIXME: beagle-specific code
    LUSTRE_TMPROOT=/lustre/beagle/$USER/swiftRtmp
    if mkdir -p $LUSTRE_TMPROOT; then
        :
    else 
        echo "Could not create temporary directory $LUSTRE_TMPROOT"
        stdcleanup_start
        stdcleanup_end
        exit 1
    fi


    while true
    do
        LUSTRE_TMPSESSION=$LUSTRE_TMPROOT/$RANDOM
        if mkdir $LUSTRE_TMPSESSION; then
            echo "Temporary files will be stored in $LUSTRE_TMPSESSION" 1>&2
            break
        fi
    done
    # Cray XT cluster nodes don't have local writable tmp storage
    export LUSTRE_TMP=$LUSTRE_TMPSESSION
    export LUSTRE_RTMP=$LUSTRE_TMPSESSION/Rtmp
    mkdir -p $LUSTRE_RTMP
}

make-pbs-submit-file()
{
  SUBMIT_FILE=$1
  if [ $queue != NONE ]; then
    queueDirective="#PBS -q $queue"
  else
    queueDirective=""
  fi
cat > $SUBMIT_FILE <<END
#PBS -S /bin/sh
#PBS -N SwiftR-workers
#PBS -m n
#PBS -l nodes=$nodes:ppn=$cores
#PBS -l walltime=$time
#PBS -o $HOME
#PBS -e $HOME
$queueDirective
export WORKER_LOGGING_LEVEL=$workerLogging # FIXME: parameterize; fix w PBS -v
#cd / && /usr/bin/perl $SWIFTBIN/worker.pl $CONTACT SwiftR-workers $HOME/.globus/coasters $IDLETIMEOUT
HOST=\$(echo $CONTACT | sed -e 's,^http://,,' -e 's/:.*//')
PORT=\$(echo $CONTACT | sed -e 's,^.*:,,')
echo '***' PBS_NODEFILE file: \$PBS_NODEFILE CONTACT:$CONTACT
cat \$PBS_NODEFILE
echo '***' unique nodes are:
sort < \$PBS_NODEFILE|uniq
for h in \$(sort < \$PBS_NODEFILE|uniq); do
  ssh \$h "echo Swift R startup running on host; hostname; cd /; /usr/bin/perl $SWIFTBIN/worker.pl $CONTACT SwiftR-\$h $HOME/.globus/coasters $IDLETIMEOUT" &
done
wait
END
}

# Submit file for PBS systems with firewall restrictions: specifically, Merlot@VCU

make-pbsf-submit-file()
{
  SUBMIT_FILE=$1
  if [ _$GLOBUS_HOSTNAME = _ ]; then
    echo GLOBUS_HOSTNAME must be set to worker-reachable address of submit host for pbsf server mode.
    usage
    if [ "$doack" = TRUE ]; then
      echo ok > ackfifo
    fi
    stdcleanup_start
    stdcleanup_end
    exit 1
  fi
  if [ $queue != NONE ]; then
    queueDirective="#PBS -q $queue"
  else
    queueDirective=""
  fi
cat > $SUBMIT_FILE <<END
#PBS -S /bin/sh
#PBS -N SwiftR-workers
#PBS -m n
#PBS -l nodes=$nodes:ppn=$cores
#PBS -l walltime=$time
#PBS -o $HOME
#PBS -e $HOME
  $queueDirective
  export WORKER_LOGGING_LEVEL=$workerLogging # FIXME: parameterize; fix w PBS -v
  HOST=\$(echo $CONTACT | sed -e 's,^http://,,' -e 's/:.*//')
  PORT=\$(echo $CONTACT | sed -e 's,^.*:,,')
  CONTACT=http://localhost:\$PORT
  echo '***' PBS_NODEFILE file: \$PBS_NODEFILE CONTACT:$CONTACT
  cat \$PBS_NODEFILE
  echo '***' unique nodes are:
  sort < \$PBS_NODEFILE|uniq
  for h in \$(sort < \$PBS_NODEFILE|uniq); do
    ssh \$h "echo Swift R startup running on host; hostname; echo HOST=\$HOST PORT=\$PORT CONTACT=\$CONTACT; cd /; ( ssh -N -L \$PORT:\$HOST:\$PORT \$HOST & sleep 3; /usr/bin/perl $SWIFTBIN/worker.pl \$CONTACT SwiftR-\$h $HOME/.globus/coasters $IDLETIMEOUT ; wait)" &
  done
#
  ontrap()
  {
    echo in ontrap
    # Kill our processes on each node; do first node (on which this is running) last
    for h in \$(sort < \$PBS_NODEFILE|uniq | sort -r); do
      echo killing processes on host \$h
      ssh \$h killall -u \$USER
    done;
    killall -u \$USER
  }
#
  trap ontrap $TRAPEVENTS
  wait
#
END
}

make-cobalt-submit-file() 
{
  SUBMIT_FILE=$1
# Most cobalt options are passed through the command line, we just need to
# include relevant arguments for worker.pl in here.
# Create a batch file in home, as the file needs to be readable
# by cluster nodes
cat > $SUBMIT_FILE <<END
#!/bin/sh
export WORKER_LOGGING_LEVEL=$workerLogging
echo '***' COBALT_NODEFILE file: \$COBALT_NODEFILE CONTACT:$CONTACT
cat \$COBALT_NODEFILE
echo '***' unique nodes are:
sort < \$COBALT_NODEFILE|uniq
for h in \$(sort < \$COBALT_NODEFILE|uniq); do
  ssh \$h "echo Swift R startup running on host; hostname; cd /; /usr/bin/perl $SWIFTBIN/worker.pl $CONTACT SwiftR-\$h $HOME/.globus/coasters $IDLETIMEOUT" &
done
wait

END
chmod +x $SUBMIT_FILE 
}

make-crayxt-submit-file()
{
  SUBMIT_FILE=$1
  if [ $queue != NONE ]; then
    queueDirective="#PBS -q $queue"
  else
    queueDirective=""
  fi
cat > $SUBMIT_FILE <<END
#PBS -S /bin/sh
#PBS -N SwiftR-workers
#PBS -l mppwidth=$(($nodes))
#PBS -l mppnppn=1
#PBS -l mppdepth=$cores
#PBS -l walltime=$time
#PBS -m n
#PBS -o $HOME
#PBS -e $HOME
$queueDirective
#export WORKER_LOGGING_LEVEL=$workerLogging # FIXME: parameterize; fix w PBS -v
#cd / && /usr/bin/perl $SWIFTBIN/worker.pl $CONTACT SwiftR-workers $HOME/.globus/coasters $IDLETIMEOUT

#TODO: lustre working directory?

aprun -B \
    /usr/bin/perl $SWIFTBIN/worker.pl $CONTACT SwiftR $HOME/.globus/coasters $IDLETIMEOUT

END
}


# FIXME: for big systems like Ranger, need to use ssh_tree to avoid socket FD exhastion?

make-sge-submit-file()
{
  SUBMIT_FILE=$1
  if [ $queue != NONE ]; then
    queueDirective="#$ -q $queue"
  else
    queueDirective=""
  fi
  if [ $project != NONE ]; then
    projectDirective="#$ -A $project"
  else
    projectDirective=""
  fi
  if [ $parEnv != NONE ]; then
    parEnvDirective="#$ -pe $parEnv $(($nodes*$cores))"
  else
    if [ $nodes -gt 1 ]; then
        echo "Warning: requested $nodes nodes without parEnv directive"
        echo "SGE provider is defaulting to a single node and $cores cores"
    fi
    parEnvDirective=""
  fi

#  rcmd="qrsh" # FIXME - need to set on system basis; qrsh works for siraf

cat > $SUBMIT_FILE <<END
#!/bin/bash
#$ -S /bin/bash
#$ -o $HOME
#$ -e $HOME
#$ -N SwiftR
#$ -l h_rt=$time
#$ -V
$parEnvDirective
$queueDirective
$projectDirective

# $ -v WORKER_LOGGING_LEVEL=$workerLogging

# Ranger Site-specific:
# $ -pe 16way 256
# $ -q development
# $ -A TG-DBS080004N

  cd /
  HOST=$(echo $CONTACT | sed -e 's,^http://,,' -e 's/:.*//')
  PORT=$(echo $CONTACT | sed -e 's,^.*:,,')
  if [ "\$PE_HOSTFILE" = "" ] ; then
    # Not a parallel environment, just run job on this host
    # with the specified number of cores
    echo '***' Single host \$(hostname) CONTACT:$CONTACT
    # Mimic the first two pe_hostfile columns
    HOSTS="\$(hostname) $cores"
  else   
    echo '***' PE_HOSTFILE file: \$PE_HOSTFILE CONTACT:$CONTACT
    cat \$PE_HOSTFILE
    HOSTS=\$(cat \$PE_HOSTFILE)
  fi

  

  if [ $workmode = slot ]; then
    NODES=\`echo "\$HOSTS" | awk '{ for(i=0;i<\$2;i++){print \$1} }'\`
  else 
    NODES=\`echo "\$HOSTS"  | awk '{print \$1}'\` # Better for Ranger, Eddie, ...
  fi


  for h in \$NODES; do
    workerCmd="echo Swift R startup running on host; hostname; cd /; WORKER_LOGGING_LEVEL=$workerLogging /usr/bin/perl $SWIFTBIN/worker.pl $CONTACT SwiftR-\$h $HOME/.globus/coasters $IDLETIMEOUT"
    if [ $rcmd = ssh ]; then
      ssh \$h "\$workerCmd" &
    else
      qrsh -nostdin -l hostname=\$h "\$workerCmd" &
    fi
  done
  wait
END
}

function wait-and-start-batch-workers
{
  get-contact
  LOGDIR=$(pwd)/swiftworkerlogs # full path. FIXME: Generate this with remote-side paths if not shared dir env?
  LOGDIR=$topdir/swiftworkerlogs  # FIXME: left this in /tmp so it works on any host. Better way?

  mkdir -p $LOGDIR

  IDLETIMEOUT=$((60*60*240)) # 10 days: FIXME: make this a command line arg

  # FIXME: set up for capturing batch job id: rm -rf remotepid.* # FIXME: should not be needed if we start in a new dir each time

  if [ "${server}" = "cobalt" ] ; then
      # mktemp only works on some unices - but I don't believe
      # Cobalt is deployed on too wide a varieties of settings
      SUBMIT_FILE=$(mktemp -p $HOME SwiftR-batch.XXXXXX)
  else
      SUBMIT_FILE=batch.sub
  fi
  make-${server}-submit-file $SUBMIT_FILE
  
  if [ "${server}" = "sge" ]
  then
    # Sun grid engine inconviently returns a bunch of text surrounding
    # the job id.  There is no documented way to obtain the job number
    # directly from qsub.  We will parse out the first number in this text
    # and assume this is the job ID (this is true for versions of SGE
    # this was tested on).
    qsub $SUBMIT_FILE | sed 's/[^0-9 ]//g' | awk '{ print $1 }' > $jobidfile
    succ=$?
  elif [ "${server}" = "cobalt" ]
  then
    # cobalt arguments are specified through command ine rather than
    # through a submit file
    #cobalt qsub statement with relevant parameters
    # queue name
    if [ "$queue" != NONE ]; then
        queueDirective="-q $queue"
    else
        queueDirective=""
    fi
    if [ "$kernel" != NONE ]; then
        kernDirective="--kernel $kernel"
    else
        kernDirective=""
    fi
    if [ "$project" != NONE ]; then
        projDirective="-A $project"
    else
        projDirective=""
    fi
    # for now, rely on swift worker to fork off worker processes 
    # so we have multiple workers per node.  In future could
    # add support for virtual node mode, etc
    #CONTACT="http://10.40.9.151:$(echo $CONTACT | sed -e 's,^.*:,,')"
    wkdir=`pwd`
    qsub -t "$time" -n $nodes $queueDirective $kernDirective \
	$projDirective -e $wkdir/SwiftR-workers.err \
        -o $wkdir/SwiftR-workers.out --debuglog $wkdir/cobalt.log \
        --cwd / $SUBMIT_FILE > $jobidfile
    succ=$?
  else
    qsub $SUBMIT_FILE > $jobidfile
    succ=$?

  fi

  if [ $succ -eq 0 ]
  then
    
    # torque's error code is 0 sometimes when failed: make sure there 
    # is something in job id file
    jobid=$(cat $jobidfile 2> /dev/null)
    jobid=`echo $jobid`
    if [ -z "$jobid" ]; then
        echo Batch queue submission failed, exiting.
        if [ "$doack" = TRUE ]; then
          echo "Error: batch submission failed: qsub returned $succ" > ackfifo
        fi
        stdcleanup_start
        stdcleanup_end
        exit 1
    fi
    echo Started workers from batch job $jobid
    if [ "$doack" = TRUE ]; then
      echo ok > ackfifo
    fi
  else
    echo Batch queue submission failed, exiting.
    if [ "$doack" = TRUE ]; then
      echo "Error: batch submission failed: qsub returned $succ" > ackfifo
    fi
    stdcleanup_start
    stdcleanup_end
    exit 1
  fi
}

verify-is-one-of()
{
  argname=$1
  arg=$2
  shift 2
  for v in $*; do
    if [ $arg = $v ]; then
      return 0
    fi
  done
  echo $0: value for argument $argname was $arg - must be one of: $*
  usage
  exit 1
}

verify-is-numeric()
{
  argname=$1; shift
  if test $1 -ge 0 2>/dev/null; then
    return 0
  else
    echo $0: value for $argname must be a positive integer, was: $1
    usage
    exit 1
  fi
}

verify-is-time()
{
    # Three valid formats, following maxWallTime globus param:
    # minutes (single int)
    # h:m
    # h:m:s
    argname=$1; shift
    #check if positive integer
    if [ "$1" -gt 0 ] 2> /dev/null; then
        time_secs=$(( $time * 60 ))
 #       echo time_secs: $time_secs
        return 0
    fi

    h=${1%%:*} 
    ms=${1#*:}
    m=${ms%%:*} 
    s=${ms#*:}

    # if in h:m format, then #*: won't have deleted anything from m
    if [ "$ms" = "$s" ]; then
        s=0
    fi
    # check than they are integers >= 0 and at least one is > 0
    if [ \( "$h" -ge 0 \) -a \( "$m" -ge 0 \) -a \( "$s" -ge 0 \) \
        -a \( \( "$h" -gt 0 \) -o \( "$m" -gt 0 \) -o \( "$s" -gt 0 \) \) \
        ] 2> /dev/null ; then
        time_secs=$(($s + 60 * ($m + $h * 60)))
#        echo time_secs: $time_secs
        return 0
    else
        echo $0: "value for $argname was neither valid d:m:s time, or positive
            integer number of minutes"
        usage
        exit 1
    fi
}

verify-not-null()
{
  argname=$1; shift
  if [ _$1 != _ ]; then
    return 0
  else
    echo $0: value for $argname can not be null
    usage
    exit 1
  fi
}

usage()
{
  cat <<END
 
   usage: start-swift -s server -n nodes -c cores \\
                      -h 'host1 ... hostN' -q queue -p throttle -t walltime \\
                      -m mode -r rcmd -A project -e parallelEnvironment

   Argument    Default     Valid Values

   -A project              site-specific
   -c cores    2,4,5       >= 1 (default is: local 2; ssh 4; cluster 8)
   -e parEnv               site specific, SGE only
   -h hosts    1           list of hosts, quoted as one argument, space separated
   -m workmode node        node: start one worker for all slots on a node; 
                            slot: one worker on each slot (Currently ignored)
   -n nodes    1
   -p throttle 10          >= 1 integer
   -q queue                site speific (PBS, SGE, Cobalt)
   -r rcmd     ssh         site specific, SGE only, typically ssh. 
                                    qrsh for siraf cluster
   -s server   local       local, pbs, sge, ssh, pbsf (for firewalled workers)
                            ,cobalt,crayxt,custom, pbsauto, crayxtauto
   -t time     00:30:00    hh:mm:ss, for PBS, Cobalt and SGE only
   -w wkloglvl NONE        NONE, ERROR, WARN, INFO, DEBUG, TRACE
   -k keepdir              No argument, if flag is set, will keep working 
                                                                directory
   --trace                 If provided, echo bash commands

    Examples:

    Local:  start-swift -c 4
    ssh:    start-swift -s ssh -c 8 -h "crush stomp thwomp"
    PBS:    start-swift -s pbs -c 8 -n 4 -A CI-87654 -q short -t "02:00:00"
    SGE:    start-swift -s sge -c 8 -n 4 -A CI-87654 -q short -t "02:00:00" -m node -e smp
    siraf:  start-swift -s sge -c 8 -n 4 -A CI-87654 -q bigmem.q -t "02:00:00" -m node -e shm -r qrsh

END
}

# main script

tmp=${SWIFTR_TMP:-/tmp}

# Process command line args

server=local
time="00:30:00"
time_secs=1800
nodes=1
queue=short
cores=0
defaultLocalCores=2
defaultSshCores=4
defaultClusterCores=8
throttle=10
num_retries=0
hosts=no-hosts-specified
queue=NONE
project=NONE
parEnv=NONE
kernel=NONE
workdir=NONE
workerLogging=ERROR
swiftLoggingFlag="-minimal.logging"
keepdir=FALSE
sites_file=
tc_file=
cf_file=
warmupjob=true

rcmd=ssh      # rcmd: ssh (typical) or qrsh (eg for siraf with node login restrictions)
workmode=slot # slot: start one worker on each slot; node: start one worker for all slots on a node

while [ $# -gt 0 ]
do
  case "$1" in
    -A) project=$2; verify-not-null project $project; shift ;;
    -c) cores=$2; verify-is-numeric cores $cores; shift ;;
    -e) parEnv=$2; verify-not-null parEnv $parEnv; shift ;; 
    -kernel) kernel=$2; verify-not-null kernel $kernel; shift ;; 
    -h) hosts=$2; verify-not-null hosts $hosts; shift ;; 
    -m) workmode=$2; verify-is-one-of workmode $workmode slot node; shift ;; 
    -n) nodes=$2; verify-is-numeric nodes $nodes; shift ;;
    -p) throttle=$2; verify-is-numeric throttle $throttle; shift ;;
    -q) queue=$2; verify-not-null queue $queue; shift ;;
    -r) rcmd=$2; verify-is-one-of rcmd $rcmd ssh qrsh; shift ;;
    -s) server=$2; verify-is-one-of server $server local ssh pbs pbsf sge cobalt crayxt custom pbsauto crayxtauto; shift ;;
    -t) time=$2; verify-is-time time $time; shift ;;
    -w) workerLogging=$2; verify-is-one-of workerLoggingLevel $workerLogging NONE ERROR WARN INFO DEBUG TRACE; shift ;;
    -L) swiftLoggingFlag="" ;; # swift default is lots of logging
    -k) keepdir=TRUE ;;
    --trace) set -x ;;
    -d) workdir=$2; verify-not-null workdir $workdir; shift ;;
    --tc.file) tc_file=$2; shift ;;
    --sites.file) sites_file=$2; shift ;;
    --cf.file) cf_file=$2; shift ;;
    -retries) num_retries=$2; verify-is-numeric num_retries $num_retries; shift ;;
    *)  usage; exit 1 ;;
  esac
  shift
done

SWIFTRBIN=$(cd $(dirname $0); pwd)
source $SWIFTRBIN/compat-setup
SWIFTBIN=$SWIFTRBIN/../swift/bin  # This depends on ~/SwiftR/Swift/swift being a symlink to swift in RLibrary/Swift



# Setup a working directory
if [ "$workdir" = NONE ]
then
    topdir=$tmp/$USER/SwiftR

    if mkdir -p $topdir 2> /dev/null ; then
        :
    else
        topdir=$(mktemp -d $tmp/SwiftR.XXXXXX)
        created_ok=$?
        if [ "$created_ok" -ne 0 ]; then
            echo "Could not create temporary directory under $tmp"
            exit 1
        fi
    fi

    rundir=$topdir/swift.$server  # rundir prefix 
    doack=FALSE
    trundir=$(mktemp -d $rundir.XXXX) 
    created_ok=$?
    if [ "$created_ok" -ne 0 ]
    then
        echo "Could not create temporary directory under $topdir/SwiftR"
        exit 1
    fi
else 
    topdir=`dirname "$workdir"`
    rundir=$topdir/swift.$server  # rundir prefix 
    doack=TRUE # let -daemon script know when we are done
    echo Working in $workdir
    trundir=$workdir
    mkdir -p $workdir
fi

# link temporary dir with different server name
rm -f $rundir
ln -s $trundir $rundir

# Before we cd into dir, copy across
if [ $server = custom ]; then
    if [ ! -f "$tc_file" ]; then
        echo --tc.file not provided or does not exist
        cd /; rm -rf $trundir
        exit 1
    elif [ ! -f "$sites_file" ]; then
        echo --sites.file not provided or does not exist
        cd /; rm -rf $trundir
        exit 1
    elif [ ! -f "$cf_file" ]; then
        echo --cf.file not provided or does not exist
        cd /; rm -rf $trundir
        exit 1
    fi
    cp $tc_file $trundir/tc
    cp $cf_file $trundir/cf
    cp $sites_file $trundir/sites.xml
fi

cd $trundir

# Standard clenuup actions
function stdcleanup_start {
    # don't accept any more requests: unlink fifo from filesystem
    if [ -p requestpipe ]; then
        rm requestpipe 
    fi
}
function stdcleanup_end {
    # Check to see if we should leave
    # logs, etc around
    if [ "$keepdir" = "FALSE" ]; then
        cd /
        rm -rf "$trundir"
        if [ "$server" = "cobalt" ]; then
            # Submit file in different directory
            rm $SUBMIT_FILE
        fi 
    fi
}

echo Running in $trundir "(linked to $rundir)"

script=$SWIFTRBIN/rserver.swift
#cp $script $SWIFTRBIN/passive-coaster-swift $SWIFTRBIN/swift.properties $rundir
cp $script .
script=$(basename $script)
cp $SWIFTRBIN/{EvalRBatchPersistent.sh,SwiftRServer.R} .

# DONE: FIXME: rework this script to transfer all shells and rscripts
# needed, and to copy in the R prelude for the R server processes (to
# include for example the OpenMx library)  NOTE: Both were done in older version of this script.

mkfifo requestpipe 

out=swift.stdouterr
touch $out

# Function to run on termination of swift
exitcmd=""

# Set up working directories on crayxt
if [ $server = crayxt -o $server = crayxtauto ] ; then
    setup-crayxt-lustre
fi


if [ $server = custom ]; then
    warmupjob=false
    # have already set up tc.data and sites.xml files, just set
    #onexit
    function onexit {
        trap - $TRAPEVENTS
        stdcleanup_start
        stdcleanup_end
        exit 0
    }
    trap onexit $TRAPEVENTS
    exitcmd=onexit
    if [ "$doack" = TRUE ]; then
      echo ok > ackfifo
    fi
elif [ $server = local ]; then
  warmupjob=true
  if [ $cores -eq 0 ]; then
    cores=$defaultLocalCores
  fi
  echo project=$project cores=$cores nodes=$nodes queue=$queue server=$server 
 
  source $SWIFTRBIN/configure-server-local
  function onexit {
    stdcleanup_start
    # For time being, allow R workers to just time out
    stdcleanup_end
    exit 0
  }

  trap onexit $TRAPEVENTS
  exitcmd=onexit
  if [ "$doack" = TRUE ]; then
    echo ok > ackfifo
  fi
elif [ $server = ssh ]; then
  warmupjob=true
  if [ $cores -eq 0 ]; then
    cores=$defaultSshCores
  fi
  echo project=$project cores=$cores nodes=$nodes queue=$queue server=$server 

  source $SWIFTRBIN/configure-server-ssh

  sshpidfile=${out/stdouterr/workerpids}


  function onexit {
    stdcleanup_start
    coasterservicepid="" # null: saved in case we go back to using coaster servers
    trap - $TRAPEVENTS
    sshpids=$(cat $sshpidfile 2> /dev/null)

#    echo Terminating worker processes $sshpids, starter $starterpid
    for rpfile in $(echo remotepid.*); do
      rpgid=$(grep PGID= $rpfile | sed -e 's/PGID=//')
      rhost=$(echo $rpfile | sed -e 's/remotepid.//')
      #echo Based on $rpfile: terminating process process group $rpgid on $rhost
      echo Shutting down worker processes on $rhost
      ssh $rhost sh -c \'"kill -s TERM -- -$rpgid &>/dev/null"\' 
      echo Shut down worker process on $rhost
    done
    if [ "_$sshpids$starterpid$coasterservicepid" != _ ]; then
      echo kill $sshpids $starterpid $coasterservicepid >& /dev/null
    fi
    stdcleanup_end
    # exit cleanly
    exit 0
  }

  trap onexit $TRAPEVENTS
  exitcmd=onexit

  wait-and-start-ssh-workers &
  starterpid=$!

elif [ \( $server = pbsauto \) -o \( $server = crayxtauto \) ]; then
    warmupjob=false
    # Systems where Swift manages workers
  if [ $cores -le 0 ]; then
    cores=$defaultClusterCores
  fi
  echo server=$server project=$project cores=$cores nodes=$nodes queue=$queue 
  source $SWIFTRBIN/configure-server-$server

  function onexit {
    stdcleanup_start
    trap - $TRAPEVENTS
    # exit cleanly
    stdcleanup_end
    exit 0   
}

  trap onexit $TRAPEVENTS
  exitcmd=onexit


  if [ "$doack" = TRUE ]; then
    echo ok > ackfifo
  fi

elif [ \( $server = pbs \) -o \( $server = pbsf \) -o \( $server = sge \) \
        -o \( $server = cobalt \) -o \( $server = crayxt \) ]; then
  warmupjob=true
  # Batch systems where we need to launch workers
  if [ $cores -le 0 ]; then
    cores=$defaultClusterCores
  fi
  echo server=$server project=$project cores=$cores nodes=$nodes queue=$queue 

  DIRS_TO_DELETE=
  if [ $server = crayxt ]; then
    source $SWIFTRBIN/configure-server-crayxt 
  elif [ $server = pbsf ]; then
    source $SWIFTRBIN/configure-server-pbs
  else
    source $SWIFTRBIN/configure-server-${server}
  fi

  jobidfile=${out/stdouterr/jobid}


  function onexit {
    stdcleanup_start

    coasterservicepid="" # null: saved in case we go back to using coaster servers
    trap - $TRAPEVENTS
    jobid=$(cat $jobidfile 2> /dev/null)
    echo Terminating worker processes starter $starterpid and batch job $jobid
    if [ "_$starterpid" != _ ]; then
      kill $starterpid &> /dev/null
    fi
    if [ "_$jobid" != _ ]; then
      qdel "$jobid" &> /dev/null
    fi
    if [ $server = crayxt -a "$keepdir" = FALSE ]; then 
        # Clean up session working directory
        rm -rf $LUSTRE_TMPSESSION
    fi
    stdcleanup_end
    # eit cleanly
    exit 0   
}

  trap onexit $TRAPEVENTS
  exitcmd=onexit

  wait-and-start-batch-workers &
  starterpid=$!

fi

$SWIFTRBIN/../swift/bin/swift $swiftLoggingFlag \
        -config cf -tc.file tc -sites.file sites.xml $script -pipedir=$(pwd) \
        -warmup=$warmupjob \
        >& $out </dev/null
exitcode=$?
# Do any cleanup if swift exits in this manner
if [ "$exitcode" != 0 ]; then
    echo "Error: Swift exited unexpectedly with return code $exitcode"


    if [ ! -z "$swiftLoggingFlag" ]; then
        echo "Verbose swift logging was disabled, enable for more information"
    fi
    if [ ! "$keepdir" = TRUE ]; then
        echo "Turn on keep work option to retain more logs"
    fi
    echo "Last 100 lines of Swift output follow.  See logs in $trundir for more information"
    tail -n 100 $trundir/$out

    # TODO: resultpipe used togo somewhere need to handle some other way
    (   echo "error: swift exited unexpectedly with return code $exitcode" > swift.error &
        echopid=$!
        sleep 5
        kill $echopid 
    ) &> /dev/null &
fi

