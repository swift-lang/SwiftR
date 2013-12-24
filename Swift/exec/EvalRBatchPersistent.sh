#! /usr/bin/env bash

# Arguments: inputBatchSaveFile outputBatchSaveFile
#   bash @shellscript @RServerScript @rcall @result stdout=@stout stderr=@sterr;


# Set restrictive umask for duration of script
# This value prevents any other users from reading
# or writing
DEFAULT_UMASK=077
USER_UMASK=$(umask)
umask $DEFAULT_UMASK

#if [ $# != 4 ]; then
if [ $# -lt 4 ]; then
  echo $0: expecting 4+ arguments, got $#: $* 1>&2
  exit 1
fi

# tmp=/tmp # FIXME: allow this to change eg for sites with main tmp dir elsewhere
# tmp=/scratch/local # FIXME: allow this to change eg for sites with main tmp dir elsewhere
if [ -z $SWIFTR_TMP ]; then
    tmproot=${TMP:-/tmp}
    tmp=$tmproot/$(id -nu)/SwiftR
else
    tmp=${SWIFTR_TMP}
fi

RServerScript=$1
callFile=$2
resultFile=$3
shift; shift; shift;

if [ $1 != "--imports" ];
then
    echo $0: expected --imports to be provided
fi
shift;
# now $@ contains imports
imports=$@



# Find our bin dir (to use for running utility scripts)

SWIFTRBIN=$(cd $(dirname $0); pwd)

# This script expects to be called (typically from Swift worker.pl)
# with the following env vars set:
#  SWIFT_WORKER_PID - process ID of the worker.pl that launched this script
#  SWIFT_JOB_SLOT   - slot ID of the worker job that launched this script (0..maxWorkers-1)

if [ _$SWIFT_WORKER_PID = _ ]; then
  echo $0: SWIFT_WORKER_PID is not set. SWIFT_JOB_SLOT=$SWIFT_JOB_SLOT ARGS=$* 1>&2
  exit 1
fi
if [ _$SWIFT_JOB_SLOT = _ ]; then
  echo $0: SWIFT_JOB_SLOT is not set. SWIFT_WORKER_PID=$SWIFT_WORKER_PID ARGS=$* 1>&2
  exit 1
fi

TRAPS="EXIT 1 2 3 15"
HAS_MUTEX=false
RPID=

function onexit() { # FIXME: move this logic into worker.pl, or try to use R timeout option if it works on fifos - need to test.
  trap - $TRAPS
  echo "EvalRBatchPersistent.sh: onexit trap: SIGNAL=$1 RPIDS=$RPIDS"
  if [ "_$RPIDS" != _ ]; then
    kill $RPIDS >& /dev/null
  fi

  if [ "$HAS_MUTEX" = "true" ]; then
    rmdir $SLOTDIR/mutex
    HAS_MUTEX=false
  fi
}

function launchRServer() {
  #Start R server and set RPID to the value
  
  mkfifo $SLOTDIR/toR.fifo
  mkfifo $SLOTDIR/fromR.fifo

  echo "$0: INFO: Launching $RServerScript $SLOTDIR"
  #echo PATH: $PATH > $SLOTDIR/R.log
  #which Rscript >> $SLOTDIR/R.log
  
  echo LD_LIBRARYPATH= $LD_LIBRARY_PATH
  PATH=.:$PATH
  echo PATH= $PATH
  echo R_LIBS_USER= $R_LIBS_USER

  slotdir=$1
  
  # Use R instead of Rscript due to Rscript issues on some
  # platforms (e.g. cray xt)
  umask $USER_UMASK
  R --slave --no-restore --file=$RServerScript \
                --args $slotdir >> $slotdir/R.log 2>&1 &   # launch R server
  umask $DEFAULT_UMASK
  RPID=$!
  echo $RPID > $SLOTDIR/R.pid
  echo "$0: INFO: Launched $RServerScript $slotdir Rscript"
}

trap "onexit 1" 1
trap "onexit 2" 2
trap "onexit 3" 3
trap "onexit 15" 15
trap "onexit EXIT" EXIT


function start_idletimer {
  # After the specified number of seconds
  # kill the specified R process

  # Cancel old timeout
  if [ -f $SLOTDIR/idletimer_pid ] ; then
    term_timeout $(cat $SLOTDIR/idletimer_pid)
  fi

  local timeout=$1
  local rpid=$2
  local idletimer_id=$$$(date '+%s%N') # timestamp in ns plus pid should be unique
  echo "$idletimer_id" > $SLOTDIR/idletimer
  (
    trap "exit 0" 1 # timeout cancelled with SIGHUP
    sleep ${timeout}s
    
    # if mutex has been acquired, know another process active
    if mkdir $SLOTDIR/mutex ; then
        new_idletimer_id=$(cat $SLOTDIR/idletimer)
        if [ "$new_idletimer_id" = "$idletimer_id" ]; then
          echo killing idle R process $rpid
          kill $rpid
          rm -f $SLOTDIR/idletimer $SLOTDIR/idletimer_pid \
                $SLOTDIR/fromR.fifo $SLOTDIR/toR.fifo
          touch $SLOTDIR/timedout
        fi
        rmdir $SLOTDIR/mutex
    fi
  ) &> /dev/null &
  echo $! > $SLOTDIR/idletimer_pid
}

# Ensure that the dir for this slot exists. 


BASEDIR=$tmp/Rworkers.$(hostname)
if mkdir -p $BASEDIR; then
  :
else
    # Sometimes the old directory exists with wrong permissions
    OLD_BASEDIR=$BASEDIR
    BASEDIR=$tmp/SwiftR.$(hostname).${SWIFT_WORKER_PID}.Rworkers
    mkdir -p $BASEDIR
    basedir_ok=$?
    if [ $basedir_ok != 0 ]; then
        echo "Could not create worker directory in either $OLD_BASEDIR or $BASEDIR"
        exit 1
    fi
fi

WORKERDIR=$BASEDIR/worker.$SWIFT_WORKER_PID
SLOTDIR=$WORKERDIR/${SWIFT_JOB_SLOT}

# terminate timeout as early as possible
rm -f ${SLOTDIR}/idletimer

mkdir -p $WORKERDIR

RPIDS=

TIMEOUT=${SWIFTR_TIMEOUT:-10}
IDLE_TIMEOUT=120
timeout_pid=

function term_timeout {
    local pid=$1
    local others=`ps -o pid --no-headers --ppid $pid`
    kill -1 "$pid" $others &> /dev/null
}

function start_timeout {
    local ppid=$$
    local child_pid=$1
    trap "timeout_handler" SIGHUP
    (
        trap "exit 0" 1 # timeout cancelled with SIGHUP
        sleep ${TIMEOUT}s
        kill -1 $ppid  &> /dev/null #SIGHUP 
        if [ "$child_pid" != "" ]; then
            kill -1 $child_pid  &> /dev/null #SIGHUP 
        fi
    ) &
    timeout_pid=$!
}

function stop_timeout {
    #DEBUG
    trap "" SIGHUP
    if [ ! -z "$timeout_pid" ]; then
        # kill timeout and sleep process 
        term_timeout $timeout_pid &
        timeout_pid=
    fi
}

function timeout_handler {
    echo "Timed out after ${TIMEOUT}s waiting to contact R process"
    echo 'R log follows:'
    echo '>>>>>R.log>>>>>'
    cat $SLOTDIR/R.log
    echo '<<<<<R.log<<<<<'
    exit 1
}



# Try to make slotdir.
# If the mkdir succeeds, this is the first request to the slot,
# so we create a new R server and send the current request to it;
# else we send the current request to an existing R server.

mkdir $SLOTDIR >& /dev/null
if [ $? = 0 ]; then
  launchRServer $SLOTDIR
else
  # wait to make sure fifo exists
  echo DB: Slot directory $SLOTDIR already exists
  # R.pid is created last and left if, so wait for that
  while true; do 
    if [ -f $SLOTDIR/R.pid ]; then
      RPID=$(cat $SLOTDIR/R.pid)
      break
    fi
  done
fi
# At this point we've launched an R server, or maybe discovered
# an existing server.  IT is still possible that the old server
# was timed out, btu to be sure we will acquire a mutex first

# Ready to talk to the server: send request and read response
#FIXME: what if mutex isn't cleaned up?
start_timeout
while true; do
  HAS_MUTEX=true
  mkdir $SLOTDIR/mutex
  if [ $? != 0 ]; then
    sleep 1;
  else
    if [ -f $SLOTDIR/timedout ]; then
      launchRServer $SLOTDIR
      rm -f $SLOTDIR/timedout
    else 
        if ps -p $RPID &> /dev/null ; then
            :
        else
            # process no longer running
          launchRServer $SLOTDIR
        fi
    fi

    break;
  fi
done
stop_timeout
echo DB: Obtained $SLOTDIR/mutex


absimports=
for im in $imports; do
    absimports="$absimports $(pwd)/$im"
done
echo imports: $imports
echo absimports: $absimports
echo run $(pwd)/$callFile $(pwd)/$resultFile $absimports > $SLOTDIR/toR.fifo &
echopid=$!
start_timeout $echopid

if wait $echopid
then
    echo DB: Sent request
    stop_timeout # started up ok
    echo dummy stderr response 1>&2 # FIXME - testing if this is the provider staging problem (not xfering zero len stderr)

    res=$(cat < $SLOTDIR/fromR.fifo)
    echo DB: Got response: $res
    
    
    # While still holding mutex, start timer to shut down idle R server
    start_idletimer $IDLE_TIMEOUT $RPID   

    rmdir $SLOTDIR/mutex
    HAS_MUTEX=false

    echo DB: Freed $SLOTDIR/mutex


    # Test if R server reported an error
    if echo "$res" | grep -q '^ERROR:'
    then
        exit 1
    fi
else
    stop_timeout
    echo "ERROR: Could not write to fifo ok"
    rmdir $SLOTDIR/mutex
    HAS_MUTEX=false

    exit 1
fi

# Fixme: how to get exceptions and stdout/stderr text from R server ???
