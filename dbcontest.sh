#!/bin/sh

DbPattern="$1"

TempDir=.
#RunId=test
RunId=`uname -n`.`date '+%y%m%d_%H%M%S'`
BD=`tput smso`
UB=`tput rmso`

# First available english locale:
EnglishLC=`locale -a 2>/dev/null | awk '/^en_/ {print; exit}'`
EnglishLC=${EnglishLC-"en_US"}

# awk or nawk?
# ------------
case "$OSNAME" in
 Linux|HP-UX) awk=awk ;;
 SunOS|AIX)   awk=nawk;;
 *)
  for awk in awk nawk
  do
   Test=`echo test | $awk 'BEGIN {prinf"test"}; $1~/^t/ {print}' 2>/dev/null`
   test "$Test" && break
  done
  test "$Test" || Exit "Failed to find awk functionality."
  ;;
esac

OSNAME=`uname`
case "$OSNAME" in
 SunOS)  SIGUSR1=16;;
 HP-UX)  SIGUSR1=16;;
 AIX)    SIGUSR1=30;;
 SCO_SV) SIGUSR1=16;;
 Linux)  SIGUSR1=10;;
 *)      SIGUSR1=USR1;;
esac

# Change the language:
PrevLCMESSAGES=LC_MESSAGES
LC_MESSAGES=EnglishLC; export LC_MESSAGES

# The list of shared memory segments:
# ----------------------------------
OsIpcsFile=$TempDir/env.ipcs.os.$RunId.txt
ipcs -ma >>$OsIpcsFile

DbIpcsFile=$TempDir/env.ipcs.db.$RunId.txt
$DLC/bin/proutil -C dbipcs >$DbIpcsFile


#------------------------------------------------------------------------------
# Convert path to a name.

Path2Name()
{
# Remove the first slash and replace next slashes by tilde:

 echo $1 | sed -e 's/^\///' | tr -s "\/" "~"
} # Path2Name()


#------------------------------------------------------------------------------
# Create the list of the running databases:

CreateDbList()
{
# Check only databases when their full path names match the specified pattern:
  DbPattern="$*"

# The columns in $DbList file:
# 1) db - full path to a database;
# 2) OutPrefix - prefix for misc files that will be created by other commands;
# 3) NAttach - the number of processes attahed to the shared memory;
# 4) Status - empty, "skip" or the name of last test that failed to connect.

# Check the positions of the "shmid" and "nattch" columns:
  ShmIdCol="unknown"
  ShmIdCol=`$awk '{for(i=1;i<=4;i++) if (match(tolower($i),"id")>0) print i}
                  NR>2 {exit}' $OsIpcsFile`

# Find the "nattch" word in first two lines of ipcs output:
  AttchCol="unknown"
  AttchCol=`$awk '{
              for(i=4;i<=NF;i++)
              if (match(tolower($i),"nattch")>0)
              print i
            }
            NR>2 {exit}' $OsIpcsFile`
# Sun:
# IPC status from <running system> as of Mon Jun  4 16:12:22 MSK 2012
# T ID KEY MODE OWNER GROUP CREATOR CGROUP NATTCH SEGSZ CPID LPID ATIME DTIME CTIME
# Shared Memory:
#
# AIX:
# IPC status from /dev/mem as of Mon May 14 16:11:05 EDT 2012
# T ID KEY MODE OWNER GROUP CREATOR CGROUP NATTCH SEGSZ CPID LPID ATIME DTIME CTIME
# Shared Memory:
#
# Linux:
# ------ Shared Memory Segments --------
# key shmid owner perms bytes nattch status
#
# Sun,AIX: ShmIdCol=2  AttchCol=9
# Linux:   ShmIdCol=2  AttchCol=6


# Parsing proutil -C dbipcs:
# PROGRESS SHARED MEMORY STATUS
#       ID ShMemVer Seg# InUse Database
#        0       -    - -      (not PROGRESS)
#        1       -    - -      (not PROGRESS)
# 13631490  6412383    0 Yes   /usr/db/sports.db

  $awk '$3==0 && $4=="Yes" {printf"%d %s\n",$1,$5}
       ' $DbIpcsFile | \
  while read ShmId db
  do
    db=`echo $db | sed -e 's/.db$//'`

#   OutPrefix=$TempDir/`Path2Name  $db`/`basename $db`
    OutPrefix=$TempDir/`Path2Name  $db`

    if [ "$AttchCol" = "unknown" ]
    then
      NAttch="unknown"
    else
      if [ "$ShmIdCol" = "unknown" ]
      then NAttch=`grep " $ShmId " $OsIpcsFile | \
                   $awk '{print $'$AttchCol'; exit}'`
      else NAttch=`$awk '$'$ShmIdCol'=="'$ShmId'" \
                         {print $'$AttchCol'; exit}' $OsIpcsFile`
      fi
    fi

    Sort=`echo $NAttch | $awk '{while(length($1)<5) $1="0" $1; print $1}'`

    if test -z "$DbPattern" || \
       echo $db | grep "$DbPattern" >/dev/null
    then Status="use"
    else Status="skip"
    fi

    echo $Status$Sort $db $OutPrefix $NAttch $Status

  done | sort -r | cut -d " " -f2,3,4,5 >$DbList

# Restore the language:
  LC_MESSAGES=$PrevLCMESSAGES; export LC_MESSAGES

} # CreateDbList()


#------------------------------------------------------------------------------
# The tests to probe db connection.

TestDbConnections()
{

# The number of USR latch locks during login:logout
# 2:2 | _progres   | all versions
# 1:1 | promon     | 10.2B and above
# 1:2 | promon     | 10.2A and below
# 1:1 | promon -F  | 10.2B-10.2B05 only
# 0:1 | promon -F  | all except 10.2B-10.2B05
# 1:2 | proshut    | all versions
# 0:1 | proshut -F | all versions
#
# USR 0: prostrct statistics
# USR 5: proutil -C describe
# USR 5: proutil -C dbanalys
# USR 5: proutil -C viewB2
# Related articles:
# KB-P14220: How to debug PROGRESS process hang.
# KB-P105698: PROSHUT -C DISCONNECT with -F is hanging the database

  for CurrTest in connect promon promon-F proshut-F
  do

# Test will create the temp files:
    WaitFlag=$TempDir/TestDbConnections.WaitFlag.$CurrTest.$$
    TestInput=$TempDir/TestDbConnections.input.$CurrTest.$$
#   TestOutput=$OutPrefix.$TestSuffix where:
    TestSuffix=TestDbConnections.output.$CurrTest.$$

    test -f $WaitFlag && rm $WaitFlag

# Phase 1: ------------------------------------------------
# Launch the test sessions to probe db connection.
    while read db OutPrefix NAttch PrevTest Status
    do
      if [ "$PrevTest" = "skip" ] || [ "$Status" = "OK" ]
      then # Just copy the previous information:
        echo $db $OutPrefix $NAttch $PrevTest $Status
        continue # with next db
      fi

      TestOutput=$OutPrefix.$TestSuffix

      case $CurrTest in
        connect)
# _progres -b writes to the stdout stream:
# ** Batch-mode PROGRESS requires a startup procedure. (1144)
          touch $TestInput
          TestCmd="$DLC/bin/_progres $db -b"
        ;;
        promon)
# Promon writes to the stderr stream.
          echo Q >$TestInput
          TestCmd="DLC/bin/_mprshut $db -0"
        ;;
        promon-F)
          echo Q >$TestInput
          TestCmd="$DLC/bin/_mprshut $db -0 -F"
        ;;
        proshut-F)
# proshut -F writes to the stdout stream:
          echo n >$TestInput
          TestCmd="$DLC/bin/_mprshut $db -F"
        ;;
# To immitate the hang on connection uncomment:
#       connect)
## No output, process terminates immediately:
#         touch $TestInput
#         TestCmd="sleep 0"
#       ;;
#       promon)
## No output but promon continues to run uninterrupted:
#         TestOutput=/dev/null
#         TestCmd="$DLC/bin/_mprshut $db -0"
#echo "5
#U" >$TestInput
#        ;;
      esac

      $TestCmd <$TestInput >$TestOutput 2>&1 &

      touch $WaitFlag
      echo $db $OutPrefix $NAttch $CurrTest $!

    done <$DbList >$DbList.test # Phase 1 -----------------

# Wait 1 sec to allow the sessions to connect to the databases:
    if [ -f $WaitFlag ]
    then
      rm $WaitFlag
      sleep 1
# All test processes were launched, input file can be deleted:
      rm $TestInput
    else
# Current test has not been applied at least to one of the databases:
      rm $DbList.test
      break  # No more tests!
    fi

# Phase 2: ------------------------------------------------
# Check if the test sessions had wrote to the output file.
    while read db OutPrefix NAttch PrevTest pid
    do
      if [ "$PrevTest" != "$CurrTest" ]
      then # Just copy the previous information:
        echo $db $OutPrefix $NAttch $PrevTest $pid
        continue # with next db
      fi

      TestOutput=$OutPrefix.$TestSuffix

# The proof of connection is a non-empty output file:
      if test -s $TestOutput
      then   # Connection was successful:
        kill $pid >/dev/null 2>&1
        echo $db $OutPrefix $NAttch $CurrTest "OK"
      else   # Connection failed:
        echo $db $OutPrefix $NAttch $CurrTest $pid

# If the process still exists then try to generate a protrace file:
        kill -$SIGUSR1 $pid >/dev/null 2>&1 && \
        touch $WaitFlag
      fi

# The Moor has done his duty, the Moor can go:
      test -f $TestOutput && \
      rm      $TestOutput

    done <$DbList.test >$DbList # Phase 2 -----------------

    rm $DbList.test

# Wait while test sessions generate the protrace files:
    test -f $WaitFlag && \
    rm $WaitFlag && \
    sleep 1

# Phase 3: ------------------------------------------------
# Save the protrace files and report the connection failure.
    while read db OutPrefix NAttch PrevTest pid
    do
      if [ "$PrevTest" != "$CurrTest" ] || [ $pid = "OK" ]
      then
        continue
      fi

      echo "$CurrTest test (PID=$pid) failed on $BD$db$UB!"
      touch $WaitFlag

      ProtraceFile=$OutPrefix.connect.$CurrTest.protrace.$pid.txt
      mv ./protrace.$pid $ProtraceFile 2>/dev/null && \
      echo "Protrace file: `basename $ProtraceFile`"

# An useless [;-)] attempt to terminate the test process:
      expr $pid + 1 >/dev/null 2>&1 && \
      kill $pid >/dev/null 2>&1

    done <$DbList # Phase 3 -------------------------------

# Current test was successful on all databases:
    test -f $WaitFlag || \
    break

    rm $WaitFlag
  done # for CurrTest

# Cleaning when even the last test failed:
  test -f $WaitFlag && \
  rm $WaitFlag

} # TestDbConnections()


# The list of the running databases:
# ---------------------------------
DbList=$TempDir/db.list.$RunId.txt
CreateDbList $DbPattern

TestDbConnections
rm $OsIpcsFile $DbIpcsFile

echo cat $DbList
cat $DbList
ps -ef | grep [_]mprshut | grep -e -0
