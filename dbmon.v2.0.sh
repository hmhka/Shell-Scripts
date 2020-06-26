#!/bin/sh
#
  THIS=`basename $0`
# DbMon - shell script to collect a database statistics
# Written by George Potemkin, Jul 07, 2004

Release="2.0, Mar 23, 2012"
# Fixes:
# Jan 03, 2012: Added proutil -C describe and prostrct statistics

# Default sampling interval (secs):
DefMonIntrv=300

# Default number of sampling intervals:
DefMonCount=100

# Save the logs in directory specified by SAVEDIR or in current one:
SaveDir=`test -d "$SAVEDIR" && echo $SAVEDIR || echo "."`

# List of OS specific commands to monitor the system activity.
# Add the commands available on your Unix.
# 
# %i will be substituted by monitoring interval ($MonIntrv).
# %c will be substituted by interval count ($MonCount).

OSNAME=`uname`
case "$OSNAME" in

 "SunOS")    # Sun Solaris v2.5.1/v2.6

    CmdList="sar -u %i %c"
#   CmdList="vmstat %i %c; iostat -d -T d %i %c"

# iostat
#   -d    For  each  disk,  report  the  number   of   kilobytes
#         transferred  per  second,  the number of transfers per
#         second, and the average service time in milliseconds.
#   -T u | d
#         Display a time stamp.
#
#         Specify u for a printed representation of the internal
#         representation  of  time.  See  time(2). Specify d for
#         standard date format. See ctime(3C).
  ;;

 "HP-UX")    #  HP UNIX 11.00

    CmdList="vmstat %i %c; iostat %i %c"

# iostat [-t] [interval [ count ] ]
#   -t    Report terminal statistics as well as disk statistics.
#
# vmstat [-dnS] [interval [count] ]
# vmstat -f | -s | -z
#   -d    Report disk transfer information as a separate section,
#         in the form of transfers per second.
#   -n    Provide an output format that is more easily viewed on
#         an 80-column display device. This format separates the
#         default output into two groups: virtual memory information
#         and CPU data. Each group is displayed as a separate line
#         of output. On multiprocessor systems, this display format
#         also provides CPU utilization on a per CPU basis for the
#         active processors.
#   -S    Report the number of processes swapped in and out (si and
#         so) instead of page reclaims and address translation
#         faults (re and at).
  ;;

 "AIX")      #  IBM UNIX

    CmdList="vmstat %i %c; iostat %i %c"

# vmstat [ -f ] [ -i ] [ -s ] [ PhysicalVolume ... ] [ Interval [ Count ] ]
#   -f    Reports the number of folks since system startup.
#   -i    Reports the number of interrupts taken by each device since
#         system startup.
#   -s    Writes to standart output the contents of the sum structure
#         which contins an absolute count of paging events since system
#         initialization. The -s option is exclusive of the vmstat
#         command options.
# iostat [ -d | -t ] [ PhysicalVolume ... ] [ Interval [ Count ] ]
#   -d    The -d option is exclusive of the -t option and
#         displays only the disk utilization report.
#   -t    The -t option is exclusive of the -d option and
#         displays only the TTY and CPU usage reports.
  ;;

 "OSF1")     #  Digital UNIX

    CmdList="vmstat %i %c; iostat %i %c"

# iostat [drive ...] [interval] [count]
# vmstat interval [count]
# vmstat [-f | -M | -P | -s]
  ;;

 "OpenUNIX") # Unixware V8

    CmdList="sar -u %i %c; sar -d %i %c"

#   -u    Report processor utilization.
#   -d    Report activity for hard disks.
  ;;

 "UnixWare") # Unixware

    CmdList="sar -u %i %c; sar -d %i %c"

  ;;

 "SCO_SV")   # SCO Open Server 5

    CmdList="sar -u %i %c; sar -d %i %c"

  ;;

 "Linux")

    CmdList="vmstat %i %c; iostat -k -t %i %c"

# iostat
#   -k    Display statistics in kilobytes per second instead of
#         blocks per second. Data displayed are valid only with
#         kernels 2.4 and newer. 
#   -t    Print the time for each report displayed.
  ;;

  *)         # Unrecognized uname

    CmdList="sar -u %i %c; sar -d %i %c; vmstat %i %c; iostat %i %c"

  ;;
esac


#-----------------------------------------------------------
Usage()
{
# Optional parameter - an error text:

  test -z "$*" || \
  echo "
$THIS: ERROR: $*" >&2

  echo "
$BD$THIS$UB: System and Database Monitoring, Release $Release
$THIS collects system and databases statistics.

Usage: $THIS Database [...] [Interval [Count]]
Where
'Database' is a database to monitor.
 You can specify more than one database.
'Interval' is a monitoring interval. Default is $DefMonIntrv secs.
'Count' is the number of intervals.  Default is $DefMonCount.
'Interval' and 'Count' should be specified only as last parameters. 
" >&2
  exit 1
} # Usage()


#-----------------------------------------------------------
# Send a sequence of the promon controls.
#
PromonMenu()
{
# Parameter:
  Sequence=$1

# MANY - many enters for some menu points:
  if [ -z "$MANY" ]; then
    Count=100
    while [ $Count -gt 0 ]; do
      Count=`expr $Count - 1`
      MANY="$MANY\n"
    done
  fi

  case $Sequence in

   STARTUP)               # Use "STARTUP" only as a first sequence.

    $ECHO "M\n1\n9999"    # Modify Defaults: Page size
    $ECHO "Q\n6"          # Shared Resources
    $ECHO "Q\n7"          # Database Status
    $ECHO "Q\nR&D\ndebghb"
    $ECHO "5\n1\n9999"    # Adjust Monitor Options: Display page length
    $ECHO "3\n$MonIntrv"  # Adjust Monitor Options: Monitor sampling interval
    $ECHO "T\n1\n1"       # Status: Database
    $ECHO "P\n8"          # Status: Logging Summary
    $ECHO "P\n12"         # Status: Startup Parameters
    $ECHO "T\n4\n5"       # Administrative Functions: Adjust Page Writer Options
    $ECHO "P\n5"          # Status: Files
    $ECHO "T\n1\n2"       # Status: Backup
   ;;

   ACTIVITY)

# Database access:
   $ECHO "T\n4\n4"       # Administrative Functions: Adjust Latch Options
   $ECHO "T\n2\n1"       # Activity: Summary
   $ECHO "T\n1\n7"       # Status: Buffer Cache
   $ECHO "T\n2\n3"       # Activity: Buffer Cache
   $ECHO "T\n2\n4"       # Activity: Page Writers
   $ECHO "T\n3\n1"       # Activity: Performance Indicators
   $ECHO "T\n6\n9"       # Activity: TXE Lock Activity
   $ECHO "T\n2\n11"      # Activity: Index
   $ECHO "T\n2\n12"      # Activity: Record
   $ECHO "T\n2\n13"      # Activity: Other
   $ECHO "T\n6\n8"       # Activity: Resource Queues
   $ECHO "T\n6\n11"      # Activity: Latch Counts
   $ECHO "P\n14"         # Status: Shared Memory Segments

# Lock table:
   $ECHO "T\n1\n13"      # Status: Shared Resources
   $ECHO "T\n2\n7\n"     # Activity: Lock Table
   $ECHO "T\n1\n4\n3"    # Status: Active Transactions
   $ECHO "T\n1\n4\n2"    # Status: Blocked Clients
   $ECHO "T\n6\n15"      # Status: Buffer Lock Queue
#  $ECHO "T\n1\n6\n1"    # Status: Lock Table
# promon/Status: Lock Table MIGHT HUNG DB!!!

# Logging:
   $ECHO "T\n2\n5"       # Activity: BI Log
   $ECHO "T\n1\n9"       # Status: BI Log
   $ECHO "T\n3\n4"       # Other Displays: Checkpoints
   $ECHO "T\n1\n10"      # Status: AI Log
   $ECHO "T\n2\n6"       # Activity: AI Log
   $ECHO "T\n1\n16"      # Status: Database Service Manager

# Disk I/O:
   $ECHO "T\n2\n8"       # Activity: I/O Operations by Type
   $ECHO "T\n2\n9$MANY"  # Activity: I/O Operations by File
   $ECHO "T\n2\n10"      # Activity: Space Allocation

# Remote Client Servers:
   $ECHO "T\n1\n3"       # Status: Servers
   $ECHO "T\n2\n2$MANY"  # Activity: Servers
  ;;

  PerUSER)

   $ECHO "T\n3\n2"       # Other Displays: I/O Operations by Process
   $ECHO "P\n3"          # Other Displays: Lock Requests By User
#  $ECHO "T\n1\n6\n1"    # Status: Lock Table: Display all lock entries
  ;;

  SAMPLE)
   $ECHO "T\n2\n9"       # Useless statistics for .db file. Just to be on activity screen.
   $ECHO "S"             # Sample activity counters
  ;;

  EXIT)
   $ECHO "X"             # Exit from the Progress Monitor utility
  ;;

  *) $ECHO $THIS: PromonMenu: unexpected sequence: $Sequence 2>&1
   exit 1
  ;;
 esac
} # PromonMenu()


#-----------------------------------------------------------
# Start Database Monitoring...
StartDbMonitoring()
{
# Parameter:
  Db=$1

  OutFile=$SaveDir/`basename $Db`.promon.$Startup.log

  echo Promon \($MonIntrv sec x $MonCount\) started at `date '+%a %b %e %T %Y'` >$OutFile
  echo $THIS Release $Release >>$OutFile
  cat $DLC/version            >>$OutFile
  echo Database: $Db          >>$OutFile
  echo Host: `uname -a`       >>$OutFile

  echo ""                                  >>$OutFile
  $DLC/bin/_proutil $Db -C describe        >>$OutFile
  $DLC/bin/_dbutil prostrct statistics $Db >>$OutFile

# Start promon.
 (PromonMenu STARTUP
  PromonMenu ACTIVITY

  ProMonCount=0
  while expr $ProMonCount \< $MonCount >/dev/null
  do
    ProMonCount=`expr $ProMonCount + 1`

    PromonMenu SAMPLE
    PromonMenu ACTIVITY
  done

  PromonMenu Exit
 ) | \
  $DLC/bin/_mprshut $Db -0 -NL 2>/dev/null | \
  tr -d "\f" >>$OutFile &

# PID of last command in pipe:
  LastPID=$!

  if kill -0 $LastPID 2>/dev/null; then
    echo "Promon on $BD$Db$UB started (last PID=$LastPID)."
    PIDList="$PIDList $LastPID"
  else
    echo "Promon on $BD$Db$UB failed." >&2
  fi
} # StartDbMonitoring()


#-----------------------------------------------------------
# Start system command in background mode.
StartSysCommand()
{
# Parameter:
  Cmd=$*

# OutFile:
# Cut off %i and %c items:
  OutFile1=`echo $Cmd | nawk 'BEGIN {RS=" "}; $0 !~/^%/ {print}' 2>/dev/null`
  OutFile2=`echo $Cmd |  awk 'BEGIN {RS=" "}; $0 !~/^%/ {print}' 2>/dev/null`

  OutFile=${OutFile1:-$OutFile2}
# Cut off spaces:
  OutFile=`echo $OutFile | tr -d " "`
# Add timestamp:
  OutFile=$SaveDir/$OutFile.$Startup.log

# Substitute %i monitoring interval ($MonIntrv).
# Substitute %c by interval count ($MonCount).

  RealCmd=`echo $Cmd | sed -e 's/%i/'$MonIntrv'/; s/%c/'$MonCount'/'`

  echo Command $RealCmd started at `date '+%a %b %e %T %Y'` >$OutFile
  echo $THIS Release $Release >>$OutFile
  echo Host: `uname -a`       >>$OutFile
  $RealCmd                    >>$OutFile 2>&1 &

  LastPID=$!
  if kill -0 $LastPID 2>/dev/null; then
    echo "Command $BD$RealCmd$UB started (last PID=$LastPID)."
    PIDList="$PIDList $LastPID"
# else
#   echo "Command $BD$RealCmd$UB failed." >&2
  fi
} # StartSysCommand()


#-----------------------------------------------------------
# Main block

# Bold/unbold terminal codes:
BD=`tput smso`
UB=`tput rmso`

#Sometimes the color of the terminal is changed:
echo $BD$UB  

# echo command should correctly treat "\n" symbol:
OSNAME=`uname`
case "$OSNAME" in
 "Linux") ECHO="echo -e";;
       *) ECHO="echo";;
esac

# Is DLC correct?
test "$DLC" && \
test -d $DLC && \
test -x $DLC/bin/_mprshut && \
test -x $DLC/bin/_proutil

if [ $? -ne 0 ]; then
  echo "Incorrect value of DLC=$DLC" >&2
  exit 2
fi

# If $DLC/bin in PATH?
type _dbutil 2>/dev/null >/dev/null || PATH=$DLC/bin:$PATH && export PATH

# Initialize variables:
MonIntrv=""
MonCount=""
DbList=""

# Parse the script's startup parameters.

while [ $# -gt 0 ]
do
 case $1 in

# Integer values are MonIntrv and MonCount:
  [0-9]*)
    if     [ -z "$MonIntrv" ]; then
      MonIntrv=$1
    else
    if [ -z "$MonCount" ]; then
      MonCount=$1
    else
     Usage "$1 is a third integer argument."
    fi; fi
  ;;

# The unknown parameters:
  -*) Usage "$1 is an unknown option."
  ;;

# Database names are Non-dash parameters...
  *)
# Cut off file extension (.db or .lk).
   Db=`echo $1|sed -e 's/.db$//; s/.lk$//'`

   if [ ! -f $Db.db ]
   then
    echo "Database $BD$Db.db$UB not exist!" >&2
    exit 1
   fi

# Is a database used in single- or multi-user mode?
   $DLC/bin/_proutil $Db -C holder >/dev/null

   case $? in
    0)  echo "There is no server for database $BD$Db$UB." >&2
        exit 1
    ;;
    14) echo "Database $BD$Db$UB is in use in single-user mode." >&2
        exit 1
    ;;
    16) #The database is busy in multi-user mode.
    ;;
    *)  $ECHO "proutil -C holder failed on $BD$Db$UB.\07" >&2
        exit 1
    ;;
   esac  # case $? in

# Set full path name:
   Dir=`dirname $Db`
   Db=`(cd $Dir 2>/dev/null && pwd)`/`basename $Db`
   DbList="$DbList $Db"
  ;;

 esac # case $1 in

 shift
done  # while [ $# -gt 0 ]

# Exit if database is not specified:
test -z "$DbList" && Usage "Database is not specified."

# If the values are not specified in command line or are zeroes:
test -z "$MonIntrv" || test $MonIntrv -eq 0 && MonIntrv=$DefMonIntrv
test -z "$MonCount" || test $MonCount -eq 0 && MonCount=$DefMonCount

# Ready to launch the monitoring processes...

# Timestamp used as a part of output file name: 
Startup=`date '+%y%m%d_%H%M%S'`

#Now start running.
echo "$THIS: System and Database Monitoring, Release $Release
"

for Db in $DbList
do
 StartDbMonitoring $Db
done


# Parse CmdList:
echo $CmdList | \
awk '
 BEGIN {RS=";"}
 {Cmd=$0
# Trim the leading spaces:
  while(Cmd ~/^ /) Cmd=substr(Cmd,2)
  print Cmd
 }' | \
while read Cmd
do
  test -z "$Cmd" || StartSysCommand $Cmd
done

echo "
Results are writing to $SaveDir/*.$Startup.log files."

