#!/usr/bin/ksh
#
# Note: the script is sensitive to a shell!
#
  THIS=`basename $0`
#
# DbMon - shell script to collect a database statistics
# Written by George Potemkin, Jul 07, 2004
#
# The most recent version of the script can be downloaded from:
# ftp://ftp.progress-tech.ru/pub/Users/george/Scripts/dbmon/

Release="3.1.5, Feb 22, 2018"

#------------------------------------------------------------------------------
# The required permissions:
#
# 1. Read permissions on the .lg files is mandatory.
# 2. Read permissions on the .lk files is highly recommended. Permissions on
#    the .lk file is defined by the umask of a process that starts a database.
# 3. The -protrace option uses the SIGUSR1 signal:
#    For a process to have permission to send a signal it must either be
#    privileged (under Linux: have the CAP_KILL capability), or the real or
#    effective user ID of the sending process must equal the real or saved
#    set-user-ID of the target process.

#------------------------------------------------------------------------------
# Fixes:
# Jun 19, 2016: /bin/ksh is replaced by /usr/bin/ksh:
# the "test" command in sh on SunOS do not have the "-nt" option.
# 3.1.1 Jun 25, 2016: Check the permissions on the .lk file.
# 3.1.2 Jun 28, 2016: Use proutil -C busy to check if db is starting up
# 3.1.3 Jun 28, 2016: Temporary files that exist only for a few seconds will be
#                     created in /tmp directory. Other temp files are in WrkDir.
#                     Add CPU info and mount command written to RunLog.
# 3.1.4 Dec 16, 2017: Add DsrUtilMon to gather the replication statistics.
#                     Add "Status: Buffer Locks" to StatusPMon
# 3.1.5 Feb 17, 2018: Update the list of promon menus to gather by the script.

# Script was tested on:
#   Red Hat Enterprise Linux Server release 5.2 (Tikanga)
#   Red Hat Enterprise Linux Server release 6.1 (Santiago)
#   SUSE Linux Enterprise Server 11
#   SunOS 10
#   AIX 6.1
#   HP-UX 11.31

#------------------------------------------------------------------------------
# Default values:

SampleIntrv=4   # Sampling interval (sec)
SampleCount=5   # Number of sampling intervals

# Set Progress environment to run the script by cron:
#
# DLC=

# Change monitoring strategy if the monitoring duration is longer than LongRun:
#
LongRun=300 # sec

# 4GL monitoring procedure to run as a supplement to promon:
#
# DbMonProc="dbmon.p"
DbMonProc=""

# Email list to send the results to:
#
MailList="" # "g.potemkin@progress-tech.ru"

# Directory to save the logs:
#
WorkDir="."

# Create protrace files for all processes if promon cannot connect database:
# The list of processes running on a hung database will be created
# from "lsof" output or by parsing "ps -ef" output.
#
ProtraceOption="" # ="enable"

# Check the last lines in db log:
#
LastLines=100000 # to read for the recent login/logout statistics
ShutLines=10000  # to search for last shutdown message
LastLines=1000   # to search for my last message

#------------------------------------------------------------------------------
#
Usage()
{
  echo "Usage:
$BD$THIS$UB [${UL}dbname${UU}] [${UL}interval${UU} [${UL}count${UU}]] [${UL}Email${UU}]
or
${BD}$THIS$UB [-h|-help]
    [-db ${UL}dbname${UU}] [...]
    [-i ${UL}interval${UU}]
    [-c ${UL}count${UU}]
    [-p ${UL}MonProc${UU}]
    [-w ${UL}WorkDir${UU}]
    [${UL}Email${UU}] ...
    [-filemon]
    [-protrace ${UL}enable|all|dbname${UU}]
where:
${UL}-help${UU} is this help.
${UL}dbname${UU}    Mask(s) for database names. Default: all running dbs.
${UL}interval${UU}  Sample interval (seconds).  Default: $SampleIntrv sec
${UL}count${UU}     Number of the intervals.    Default: $SampleCount
${UL}MonProc${UU}   Test procedure to run by the batch sessions. Default: None
${UL}WorkDir${UU}   Directory to save the logs. Default: \"$WorkDir\"
${UL}Email${UU}     Email(s) to send the results to.
-filemon         Monitor the changes of database extents (run DbFileMon).
-protrace        Create protraces for processes connected to database:
-protrace ${UL}enable${UU} only for databases that promon failed to connect to.
-protrace ${UL}dbname${UU} only for a specified database.
-protrace ${UL}all${UU}    for all databases in the list.

The most recent version of the tool can be downloaded from:
ftp://ftp.progress-tech.ru/pub/Users/george/Scripts/dbmon.sh" | \
more

  exit
} # Usage()


#------------------------------------------------------------------------------

OSNAME=`uname`

#
#    UID   PID  PPID   C    STIME TTY         TIME CMD
#   root 12635 12616   0 12:36:37 pts/5       0:00 /usr/bin/ksh
#
ShellPs=`ps -fp $$ | grep " $$ "`
Shell=`echo $ShellPs | cut -d " " -f 8`
MyTTY=`echo $ShellPs | cut -d " " -f 6`
Shell=`basename ${Shell:-ksh}`

#
# Should the script send the notification messages to a controlling terminal
# while waithing for the monitoring processes to complete:
#
Notify="no"
test "$TERM" && test "$MyTTY" != "?" && tty >/dev/null 2>&1 && \
Notify="yes"


#------------------------------------------------------------------------------
#
# SIGUSR1 will be used to create a protrace file if promon hangs.
#
# A process without superuser privilege can signal another process only if the
# sender_s real or effective UID matches the real or saved UID of the receiver.
#
# Signame can be specified with or without the SIG prefix.
#
case $OSNAME in
  AIX)    SIGUSR1=SIGUSR1; SIGTERM=$SIGTERM;;
  *)      SIGUSR1=USR1;    SIGTERM=TERM;;
# Linux)  SIGUSR1=10;      SIGTERM=15;;
# SunOS)  SIGUSR1=16;      SIGTERM=15;;
# HP-UX)  SIGUSR1=16;      SIGTERM=15;;
# AIX)    SIGUSR1=30;      SIGTERM=15;;
# SCO_SV) SIGUSR1=16;      SIGTERM=15;;
esac


#------------------------------------------------------------------------------
#
# The list of OS commands to monitor system statistics (CPUs and disks).
# The commands should understand the input parameters: interval count

case $OSNAME in
 Linux)    SysMonList="vmstat; iostat -k -t";;
 SunOS)    SysMonList="sar -u; sar -d";;
 HP-UX)    SysMonList="vmstat; iostat";;
 AIX)      SysMonList="vmstat; iostat";;
 OSF1)     SysMonList="vmstat; iostat";;
 OpenUNIX) SysMonList="sar -u; sar -d";;
 UnixWare) SysMonList="sar -u; sar -d";;
 SCO_SV)   SysMonList="sar -u; sar -d";;
  *)       SysMonList="sar -u; sar -d; vmstat; iostat";;
esac


#==============================================================================
#
# Function       Description
#---------       --------------------------------------------------------------
# psEF           Return the name of a file with the output of ps -ef command
# GetDLC         Find a location of DLC directory based on the running Progress executables
# LkInfo         Read the contents of database .lk file
# DbFileList     List the database extents (called by DbDescribe)
# DbFileMon      Monitor the changes of timestamps/sizes for database extents
# DbProcList     List the processes that use a database
# LastLogMsg     Return the most recent message in database log
# LogMsgStat     Parse db log to create the statistics of the recent messages.
# LogParser      Creates statistics of the errors or login/logout messages in db log file (called by LogMsgStat)
# LicenseFile    Output the last lines of Progress license file (called by DbDescribe and in main block)
# MoveProtrace   Find, copy and rename an existent protrace file (called by DbProcList and in main block)
# DbConnectStat  Count the current number of sessions connected to a database (called by DbProcList and in main block for "Too many users")
# FreeDbConnect  Disconnect old sessions to allow new connections (called in main block)
# LogHeader      Create a standard log header (used in DbProcList, DbDescribe and LogMsgStat)
# EnvInventory   Report the basic information about OS and Progress environment
# ExtendDbipcs   Combine the outputs of "proutil -C dbipcs" and "ipcs -ma" (called by DbProcList and DbDescribe)
# DbDescribe     Run various commands to gather the basic information about database
# ProbeConnect   Launch a probe session that connect a database and immediately quit
# LruShot        Make ACO Object Blocks insensitive to the contention on LRU latch
# MainPromon     Run promon to gather the main database statistics
# LatchPMon      Creares a log of the latch owners (Activity: Latch Counts)
# LatchInfo      Report the detailed information about all latches (Restricted Options: 8j2hhs7gio)
# StatusPMon     Run promon to gather the status information
# DsrUtilMon     Run dsrutil to gather the replication statistics
# RunMonProc     Run DbMonProc to gather the additional information using 4GL
# SystemMon      Run system monitoring command
# CheckProcesses Check the status of all monitoring processes started by the script
# MainPID1       Return PID of a specified command started in a pipe
# MainPID2       Return PID of a specified command started in a pipe
# WaitForPIDs    Wait for all specified processes to terminate
# LogArchiver    Archive the log files
# SendEmail      Email an archive file to the recipients
# TimeInterval   Returns time difference in seconds between the dates
# FileSize       Return a file size
# echoT          Write a message with timestamp to stdout while launching the sessions
# echo1          Write a message with timestamp to RunLog while waiting for running processes
# echo2          Write a message with timestamp to RunLog and stderr while waiting for the processes

#------------------------------------------------------------------------------
#
psEF()
{
# Return the name of a file with the output of ps -ef command.
#
  PsOutput=${LogPrefix:-`basename $0`.$$}.ps-ef.log
#
# Cannot use "ps -ef" too often because it runs slowly when there are the large
# number of processes running on system and it might temporarily hangs the whole
# system. Use the previously saved output if it exists Otherwise create new one.
#
# Output to a file will ignore your terminal settings and the CMD column will
# show the full command.
#
# Extend the output of ps -ef in Solaris to display more than 80 characters.
# Try to use BSD-style ps command: /usr/ucb/ps -www PID
# UCB = University of California at Berkeley
#
# The issue is fixed in Solaris 11.3 SRU 5: all original argument vectors
# as well as the environment variables can be retrieved from /proc.
# ps will now print all of the command line.
#

# Return the previously saved result:
#
  test -s $PsOutput && \
  echo $PsOutput && \
  return

# Otherwise save the output for possible re-use:
#
  ps -ef >$PsOutput 2>&1

  if [ ${OSNAME:-`uname`} != "SunOS" ] || [ ! -x /usr/ucb/ps ]
  then
    echo $PsOutput
    return
  fi # if not SunOS

#
# SunOS:
# if ps -ef retuns the lines truncated at 130 characters then use /usr/ucb/ps
#
  WidthLimit=130
  TmpFile1=${TmpPrefix:-/tmp/`basename $0`.$$}.ps-ef.1.tmp
  TmpFile2=${TmpPrefix:-/tmp/`basename $0`.$$}.ps-ef.2.tmp
#
# Divide ps -ef output into two lists: the short and long commands.
#
# CmdOffset is the offset of the CMD column in ps -ef output:
#
  CmdOffset=`
    awk '
      BEGIN {
        TmpFile1="'$TmpFile1'"
        TmpFile2="'$TmpFile2'"
        getline # Header
# Print the title of "ps -ef" to the file with the short commands:
#     UID   PID  PPID   C    STIME TTY         TIME CMD
        Title=$0
        print Title >TmpFile1
#
# Offset of the "CMD" column in ps -ef output:
        print index(Title,$8)
      }
# TmpFile1:
      length($0)!='$WidthLimit' {
        print $0 >TmpFile1
        next
      }
# TmpFile2:
# Save $0 to a variable before using $i to keep all its spaces.
# Add PID as a first field.
      { ps=$0; print $2 "|" ps >TmpFile2}
    '  $PsOutput
  ` # CmdOffset=

# Use /usr/ucb/ps -www only for PIDs in TmpFile2:
  cat $TmpFile2 | \
  while IFS="|" read CmdPID CmdPs
  do
   (echo "$CmdPs"             # 1st line is the output of the standard ps -fp
    /usr/ucb/ps -www $CmdPID  # 2nd and 3rd line are the output of /usr/ucb/ps
   ) | \
    awk '
      BEGIN {
        FS=" "
        Offset1='$CmdOffset'
        getline
# root 2597    1 0 16:43:25 ?   0:02 /usr/dlc/bin/_mprosrv -classpath /usr/dlc
        ps=substr($0,1, Offset1-1)
        Short=substr($0,Offset1)
      } # BEGIN

# Offset of the "COMMAND" column in /usr/ucb/ps output:
#   PID TT S TIME COMMAND
      $1=="PID" {
        Title=$0
        Offset2=index(Title,$5)
        next
      } # Title of /usr/ucb/ps

# /usr/ucb/ps -www 2597
#   PID TT S TIME COMMAND
#  2597 ?  S 0:01 /usr/dlc/bin/_mprosrv -classpath ... sports
      { Long=substr($0,Offset2)
        if(index(Long, Short)==1) print ps Long
                             else print ps Short
      }
    ' # awk
  done >>$TmpFile1 # Append the long commands to the short ones.

  rm $PsOutput $TmpFile2 2>/dev/null && \
  mv $TmpFile1 $PsOutput

  echo $PsOutput

} # psEF()


#------------------------------------------------------------------------------
#
GetDLC()
{
# Find a location of DLC directory based on the running Progress executables:
#
  Exec=$1 # For example: _mprosrv (without $DLC/bin/)

# UID  PID PPID C STIME TTY       TIME CMD
# root 819    1 0 12:11 pts/1 00:00:00 /usr/dlc/bin/_mprosrv -classpath ...

  PsOutput=`psEF`
  Dir=`
    awk '
      BEGIN {
        Exec="'$Exec'"
        CutOff=length("/bin/" Exec)
        getline # Title
        CmdOffset=index($0,$8)
      } # BEGIN

#UID     PID PPID C STIME TTY       TIME CMD
# root 27390    1 0 Jun06 pts/2 00:00:00 /usr/dlc/bin/_mprshut sports -0
# OR
#  UID   PID PPID C STIME TTY       TIME CMD
# root 20905    1 0 Jun 10 ?        0:08 /usr/dlc/bin/_mprosrv -classpath

    { Cmd=substr($0,CmdOffset)
      n=split(Cmd,Arr)
      Cmd=Arr[1]
      n=split(Cmd,Arr,"/")
      if(n<3)             next
      if(Arr[n]!=Exec)    next
      if(Arr[n-1]!="bin") next
      print substr(Cmd,1,length(Cmd)-CutOff)
    }' $PsOutput | \
    sort | \
    uniq
  ` # Dir=

  Dir=`echo $Dir`
  Num=`echo $Dir | awk '{print NF}'`
  test "$Num" -eq 1 2>/dev/null && \
  echo $Dir
} # GetDLC()


#------------------------------------------------------------------------------
#
LkInfo()
{
# Read the contents of database .lk file. Options:
# -mode: Return a database mode (single-, multi-user or crash recover);
# -pid:  Return PID of a process that has opened database;
# -host: Return host name where the process is running.
#
# Article: The database .lk file.
# http://knowledgebase.progress.com/articles/Article/P19984
#
  Option=$1
  LK=$2

  DB=`echo $LK | sed -e 's/\.lk$//'`
  DB=`echo $DB | sed -e 's/\.db$//'`
  LK=$DB.lk
#
# If lk file does not exist then return nothing
# You used to have the permissions to read lk file:
# -r--r--r-- 1 root root 38 Apr 28 18:21 sports.lk

#
  test ! -r "$LK" && return

  case $Option in

# Database mode:
#
#  1 = Single-user mode
#  2 = Multi-user mode
#  5 = Crash recovery
# 64 = Crash recovery too ?
#
    -mode)
      dd  bs=4 count=1 if=$LK 2>/dev/null | \
      od -d | \
      awk '$2!=0 {printf"%d",$2
                  exit}
           $3!=0 {printf"%d",$3
                  exit}'
    ;; # -mode

# ID of the process that opens database:
    -pid)
      dd  bs=4 count=2 if=$LK 2>/dev/null | \
      od -d $LK | \
      awk '$2!=0 {print $5*65536+$4
                  exit}
           $3!=0 {print $4*65536+$5
                  exit}'
# Not all Unix flavours seem to have the od command with -i or -D options:
    ;; # -pid

# Host where the process is running:
    -host)
      Host="`dd bs=1 skip=8 if=$LK 2>/dev/null`"
      echo $Host
    ;; # -host
  esac # case Option

} # LkInfo()


#------------------------------------------------------------------------------
#
DbFileList()
{
# List the database extents:
#
  DB=$1

  DBUTIL=${DBUTIL:-$DLC/bin/_dbutil}

  Prefix=${DbLogPrefix:-`basename $0`.$$.`basename $DB`}.DbFileList
  Prefix=/tmp/`basename $Prefix`
  TmpFile1=$Prefix.1.tmp
  TmpFile2=$Prefix.2.tmp

# In case of the relative pathes:
# DBUTIL   : (12475) Unable to get file status for extent ./sports.db

( cd `dirname $DB`
  $DBUTIL prostrct list $DB $TmpFile1 >$TmpFile2 2>&1

  if [ ! -f $TmpFile1 ]
  then
# -rw-------. 1 root root 32768 Jun 23 10:54 sports.db
# PROSTRCT LIST: file /usr/wrk/sports.db does not exist, command ignored. (6921)

    echo "DbFileList: prostrct list failed on $BD$DB$UB"
    ls -l $DB.db
    cat $TmpFile2
    rm  $TmpFile2

    test ! -r $DB.st && \
    return 1

    cp $DB.st $TmpFile1
    echo "The existent file will be used: $DB.st"
    ls -l $DB.st
  fi # if ! -f TmpFile1

  test -f $TmpFile2 && rm $TmpFile2

  awk '
    BEGIN {print "Data area(s) of '$DB':"}
    $1=="d" {
      s=substr($0,3)
      i=index(s,":")
      r=substr(s,i+1)
      i+=index(r," ")
      s=substr(s,1,i-1)
      i=index(r,",")
      n=substr(r,1,i-1)
      Area[n]=s
    } # "d"
    END {for(n in Area) print Area[n]}
  ' $TmpFile1 | \
  tr -d "\""

  echo "
Files:"
  ls -l $DB.db
  ls -l $DB.lk
  ls -l $DB.lg

# Cut off the area info and print the rest (file name and size):
  awk '
    $1~/^[abt]$/ {
      print substr($0,3)
      next
    }
    $1=="d" {
      s=substr($0,3)
      i=index(s,":")
      s=substr(s,i+1)
      i=index(s," ")
      print substr(s,i+1)
    } # "d"
  ' $TmpFile1 | \
  while read File Attr
  do
    echo "`ls -l $File` $Attr"
  done
)
  rm $TmpFile1

} # DbFileList()


#------------------------------------------------------------------------------
#
DbFileMon()
{
# Monitor the changes of timestamps/sizes for database extents:
#
  DB=$1
  MonIntrv=${2-1}
  MonCount=${3-0}

  Log=${DbLogPrefix:-`basename $0`.$$.`basename $DB`}.filemon.log

  DB=`echo $DB | sed -e 's/\.db$//'`

  Dir=`dirname $DB`
  Dir=`(cd $Dir; pwd)`
  DB=$Dir/`basename $DB`

  ls="ls -l"

# Use the file timestamps with the seconds where possible:
  case ${OSNAME:-`uname`} in
    Linux) ls -l --time-style=full-iso $DB.db >/dev/null 2>&1 && \
       ls="ls -l --time-style=full-iso"
    ;;
    SunOS) ls -le $DB.db >/dev/null 2>&1 && \
       ls="ls -le"
    ;;
  esac # uname

  DBUTIL=${DBUTIL:-$DLC/bin/_dbutil}

  Prefix=`pwd`/${DbLogPrefix:-`basename $0`.$$.`basename $DB`}.DbFileMon
  TmpFile1=$Prefix.1.mon
  TmpFile2=$Prefix.2.mon

# In case of the relative pathes:
# DBUTIL   : (12475) Unable to get file status for extent ./sports.db

( cd $Dir

  $DBUTIL prostrct list $DB $TmpFile1 >$TmpFile2 2>&1

# Parse database structure file to get the list of database extents:
#
# b ./sports.b1
# d "Schema Area":6,32;1 ./sports.d1
# d "Info Area":7,32;1 ./sports_7.d1 f 320
# a ./sports.a1
#
  if [ ! -f $TmpFile1 ]
  then
# -rw-------. 1 root root 32768 Jun 23 10:54 sports.db
# PROSTRCT LIST: file /usr/wrk/sports.db does not exist, command ignored. (6921)

    echo "DbFileMon: prostrct list failed on $BD$DB$UB"
    ls -l $DB.db
    cat $TmpFile2
    rm  $TmpFile2

    test ! -r $DB.st && \
    return 1

    cp $DB.st $TmpFile1
    echo "The existent file will be used: $DB.st"
    ls -l $DB.st
  fi # if ! -f TmpFile1

  awk '
    BEGIN {Dir="'$Dir'"}
    {File=""}

    $1~/^[abt]$/ {File=$2}

    $1=="d" {
      i=index($0,":")
      File=substr($0,i+1)
      n=split(File,Arr)
      File=Arr[2]
    } # $1 = d

    { if(File ~/^\.\//) File=Dir substr(File,2)
      if(File!="") print File
    }
  ' $TmpFile1 >$TmpFile2
)

  test ! -f $TmpFile2 && \
  return 1

  cat $TmpFile2 | \
  while read File
  do

# -rwxr-xr-x. 1 root root 50280 Apr  9  2011 ab
# -rw-rw-rw-. 1 root root 32768 Jun  8 21:50 sports.db
# -rw-rw-rw-. 1 root root 32768 2016-06-08 21:50:19.955823160 +0400 sports.db
# -rw-r--r--   1 root     root      655360 Jun  7 16:43:25 2016 sports.db

    FileInfo=`$ls $File`
    CurrSize=`echo $FileInfo | awk '{print $5}'`
    CurrTime=`echo $FileInfo | awk 'BEGIN {RS=" "}
              /:/ {print substr($0,1,8)}'`
    echo $File $CurrSize $CurrTime
  done >$TmpFile1

  CurrLkPID=`LkInfo -pid  $DB`
  CurrState=`LkInfo -mode $DB`

  if [ "$CurrLkPID" ]
  then
    CurrLkPs=`ps -fp $CurrLkPID | grep " $CurrLkPID "`

    case  $CurrState in
       1) StateDesc="Single-user mode";;
       2) StateDesc="Multi-user mode";;
       5) StateDesc="Crash recovery";;
       *) StateDesc="Unknown";;
    esac # CurrState
  else
    CurrLkPs="Do not have permissions to read .lk file"
    StateDesc="`ls -l $DB.lk`"
  fi # if -r DB.lk

  RunTime=`expr $MonIntrv \* $MonCount`
(
#
# Create a log header:
#
  echo "$THIS Release $Release
DbFileMon
   DB: $DB
 Date: `date '+%a %b %e %T %Y'`
 Host: `uname -a`
Intrv: $MonIntrv sec
Count: $MonCount
Total: $RunTime sec
.
 Database: $DB
Opened by: $CurrLkPs
  Db Mode: $CurrState ($StateDesc)
 DB Files:" # echo

  FileInfo=`$ls $DB.db`
  CurrSize=`echo $FileInfo | awk '{print $5}'`
  CurrTime=`echo $FileInfo | awk 'BEGIN {RS=" "}
            /:/ {print substr($0,1,8)}'`

  echo $DB.db $CurrSize $CurrTime | \
  awk 'BEGIN {printf"%13s %-8s %s\n","Size","Time","DB File"}
             {printf"%13d %-8s %s\n",$2,$3,$1}
  ' - $TmpFile1

  echo "
`date '+%Y/%m/%d %H:%M:%S'` DbFileMon started."

  while [ $MonCount -gt 0 ]
  do
    sleep $MonIntrv
    Date=`date '+%Y/%m/%d %H:%M:%S'`

    if [ "$CurrLkPID" ]
    then
      kill -0 $CurrLkPID 2>/dev/null || \
      echo "$Date  PID $CurrLkPID disappeared"

      test ! -f $DB.lk && \
      echo "$Date LOCK $CurrLkPID $CurrState $DB.lk disappeared"
    fi

    PrevLkPID=$CurrLkPID
    PrevState=$CurrState

    CurrLkPID=`LkInfo -pid  $DB`
    CurrState=`LkInfo -mode $DB`

    test "$PrevLkPID" != "$CurrLkPID" && \
    echo "$Date  PID ${PrevLkPID:-None} ${CurrLkPID:-None} $DB.lk" \
         `test "$CurrLkPID" && ps -fp $CurrLkPID | grep " $CurrLkPID "`

    test "$PrevState" != "$CurrState" && \
    echo "$Date MODE ${PrevState:-None} ${CurrState:-None} $DB.lk"

    rm  $TmpFile2 2>/dev/null
    cat $TmpFile1 | \
    while read File PrevSize PrevTime
    do
      FileInfo=`$ls $File`
      CurrSize=`echo $FileInfo | awk '{print $5}'`
      CurrTime=`echo $FileInfo | awk 'BEGIN {RS=" "}
               /:/ {print substr($0,1,8)}'`

      echo $File $CurrSize $CurrTime >>$TmpFile2

      if test $File -nt $TmpFile1
      then
        echo "$Date TIME $PrevTime $CurrTime $File"
        Size=`expr $CurrSize - $PrevSize`
        test $Size -gt 0 && \
        echo "$Date Size $PrevSize $CurrSize $File changed by $Size bytes"
      fi

    done # while read $TmpFile1 and write to $TmpFile2

    TmpFile0=$TmpFile1
    TmpFile1=$TmpFile2
    TmpFile2=$TmpFile0

    MonCount=`expr $MonCount - 1`

  done # while MonCount -gt 0

  echo "$Date DbFileMon finished."

  rm $TmpFile1 $TmpFile2 2>/dev/null

) >>$Log 2>&1 &

  echo "$!||DbFileMon|$DB" >>$MyProcList

} # DbFileMon()


#------------------------------------------------------------------------------
#
DbProcList()
{
# List the processes that use a database:
#
  DB=$1
  Option=$2  # "protrace" or empty

  Prefix=${DbLogPrefix:-`basename $0`.$$.`basename $DB`}.DbProcList
  PsOutput=$Prefix.3.lst
  ProcList=$Prefix.4.lst
# Format of the ProcList file: PID CMD

( LogHeader $DB DbProcList $Option
#
# Database broker:
#
  if [ -r $DB.lk ]
  then
    LkPID=`LkInfo  -pid $DB`
    LkHost=`LkInfo -host $DB`
    LkState=`LkInfo -mode $DB`

    case  $LkState in
       1) StateDesc="Single-user mode";;
       2) StateDesc="Multi-user mode";;
       5) StateDesc="Crash recovery";;
       *) StateDesc="Unknown";;
    esac # LkState

    test "$LkHost" != "`uname -n`" && \
    echo "$DB is opened by PID $LkPID on host $LkHost" && \
    echo "Database mode: $LkState ($StateDesc)" && \
    return

    echo "lk File Owner:" && \
    ps -fp $LkPID && \
    echo "Database mode: $LkState ($StateDesc)"
  fi # if -r DB.lk
#
# Shared Memory Segments:
#
  ExtendDbipcs $DB

  echo "
List of processes that use $DB:"

  if type lsof >/dev/null 2>&1
  then
    echo "(based on lsof output)"

    lsof $DB.db | \
    while read CmdName CmdPID Other
    do
      test $CmdPID != "PID" && \
      echo $CmdPID $CmdName
    done | \
    sort -n | \
    uniq >>$ProcList

# Title of "ps -fp":
    ps -fp 1 2>/dev/null | awk '{print
                                 exit}'
    cat $ProcList | \
    while read CmdPID CmdName
    do
      ps -fp $CmdPID | grep " $CmdPID "
    done

  else
#
# If lsof is not available then parse the output of "ps -ef":
#
    echo "(based on ps -ef output)"

    PsOutput=`psEF`
    awk '
      BEGIN {
        ProcList="'$ProcList'"
        FullName="'$DB'"
        BaseName="'`basename $DB`'"
        Pattern[1]=" " FullName " "
        Pattern[2]=" " FullName ".db "
        Pattern[3]=" " BaseName ".db "
        Pattern[4]="/" BaseName ".db "
        Pattern[5]="/" BaseName " "
        Pattern[6]=" " BaseName " "
        getline # Title
        CmdOffset=index($0,$8)
        print $0
    } # BEGIN

#UID     PID PPID C STIME TTY       TIME CMD
# root 27390    1 0 Jun06 pts/2 00:00:00 _mprosrv -classpath ... sports
      { Cmd=substr(p$0,CmdOffset) " "
        for(i=1;i<=6;i++) if(index(Cmd, Pattern[i])>0) {
          print $0
          n=split(Cmd,Arr)
          Cmd=Arr[1]
          n=split(Cmd,Arr,"/")
          Exec=Arr[n]
          printf"%8d %s\n",$2,Exec >ProcList
          next
        } # for i
      } # ps output
    ' $PsOutput

  fi # if lsof

#
# Process statistics per executables:
#
  awk '
# Initiate "Count" as an array. Otherwise empty Count will rise
# the error on SunOS: awk: Count is not an array
    BEGIN {Count[""]=""}
   {Count[$2]++}
    END {
      print "Summary:"
      for(Exec in Count) {if(Count[Exec]>0)
        printf"%5d %s\n", Count[Exec], Exec
        Total+=Count[Exec]
      }
      printf"%5d %s\n",Total,"processes are running on '$DB'"
    } # END
  '  $ProcList # awk

#
# Create the protrace files: --------------------------------------------------
#
  if [ "$Option" = "protrace" ] && [ "$SIGUSR1" ] && [ -s $ProcList ]
  then
    case ${OSNAME:-`uname`} in
      AIX)   ProcStack="procstack";;
      SunOS) ProcStack="pstack -F";; # -F Force. Grabs the target process
      *)     ProcStack="";;          #  even if another process has control.
    esac

    Date="`date '+%y%m%d_%H%M%S'`"
    Dir="`dirname $DB`"
    DbName="`basename $DB`"

    echo "Sending $SIGUSR1 signal:"
    cat $ProcList | \
    while read ProPID ProExec
    do
# Promon or proshut are terminated after receiving the USR1 signal.
# The same is true for all java processes like
# AdminServer, NameServer, AppServer and Webspeed Brokers and AppServer Agents
#
      case $ProExec in
        _mprshut)
          test "$ProcStack" && \
          $ProcStack $ProPID >"protrace.$ProPID.$Date.$DbName.$ProExec.txt"
        ;;
        *)
          kill -$SIGUSR1 $ProPID 2>/dev/null && \
          echo "OK"
        ;;
#       _dbagent) ;;
#       _dbutil)  ;;
#       _mprosrv) ;;
#       _mprshut) ;;
#       _proapsv) ;;
#       _progres) ;;
#       _proutil) ;;
#       _rfutil ) ;;
#       _sqlsrv2) ;;
      esac # case Exec
    done | wc -l
#
# Allow time for the processes to complete the protrace files:
#
    sleep 1

    echo "Gathering protrace files:"
    cat $ProcList | \
    while read ProPID ProExec
    do
      test $ProExec != "_mprshut" && \
      MoveProtrace $ProPID $Dir ".$DbName.$ProExec.txt" && \
      echo "OK"
    done | wc -l

  fi # if not protrace

  rm $ProcList 2>/dev/null

) >$DbLogPrefix.processes.log 2>&1 &

  Msg="DbProcList started for $UL$DB$UU"
  test "$Option" && Msg="$Msg with $BD$Option$UB"
  echoT "$Msg (PID=$!)"

} # DbProcList()


#------------------------------------------------------------------------------
#
LastLogMsg()
{
# Return the most recent message in database log:
#
  DB=$1
  Pattern="$2"

  Msg=`tail -${LastLines:-1000} $DB.lg | grep "$Pattern" | tail -1`

  test -z "$Msg" && \
  echo "The $Pattern messages not found in db log!" && \
  return

  LastTime=`echo $Msg | awk '{print $1}'`
  CurrTime="`date '+%Y/%m/%d %H:%M:%S%z'`"
  LastTime=`TimeInterval "$LastTime" "$CurrTime"`
  echo "$LastTime sec ago: $Msg"
} # LastLogMsg()


#------------------------------------------------------------------------------
#
LogMsgStat()
{
# Parse db log to create the statistics of the recent messages.
#
# LogMsgStat will be run in background mode but its PID will not be added
# to the list of PIDs that the script should wait for (before archiving).
# LogMsgStat is expected to finish in one-two seconds.
#
  DB=$1

( LogHeader $DB "LogParser"

  BegTime="`date '+%Y/%m/%d %H:%M:%S%z'`"

  echo "Last 10 messages:"
  tail -10 $DB.lg
  echo "[`date '+%Y/%m/%d@%H:%M:%S.000%z'`] P-$$ <-- Time of $THIS run"

  echo "
Recent login/logout statistics:"
  tail -1000 $DB.lg | LogParser -l 0 | tail -30

  echo "
Summary for recent messages:"
  tail -$LastLines $DB.lg | LogParser -prs

  EndTime="`date '+%Y/%m/%d %H:%M:%S%z'`"
  ExecTime=`TimeInterval "$BegTime" "$EndTime"`
  echo "
LogParser finished in $ExecTime seconds at `date '+%a %b %e %T %Y'`"

) >$DbLogPrefix.messages.log 2>&1 &

} # LogMsgStat()


#------------------------------------------------------------------------------
#
LogParser()
{
# Creates statistics of the errors or login/logout messages in db log file.
# LogParser reads the messages from a standard input stream (stdin).
#
# It processes approximately 50,000 lines per sec on CPU @ 2.40 GHz.
#
# Number of messages added by Progress versions:
# Ver   Date       Msg # Range Added
# 10.2B 2009/12/14 12419-15762 3344
# 11.0  2011/12/02 15763-16640  878
# 11.1  2012/06/15 16641-16738   98
# 11.2  2013/02/13 16739-17035  296
# 11.3  2013/07/17 17036-17338  302
# 11.4  2014/07/25 17339-17759  421
# 11.5  2014/12/05 17760-17919  160
# 11.6  2015/10/16 17920-18388  468
# 11.7A 2016/03/25 18389-18505  117
#
# Only 200-300 messages used to be written to db log.

# Options:
#
# -f Show the fatal errors (marked as "F" in Progress log files)
#    They are the messages with %g or %G tags in promsgs file.
#    The tags are telling to the process to terminate processing;
# -e Show the system errors (the messages with words "SYSTEM ERROR:");
# -r Show the remained messages (except "Logins", "Params" and "Ignore" groups);
# -p Show the startup parameters of Progress multi-user sessions ("Params" group);
# -s Summary: the number of messages per message numbers (and without numbers);
# -l Report the statistics for the messages in the "Logins" group;
# CutTimeDigits - the number of the digits in timestamps to cut off
# while creating the login/logout statistics.
# Range of CutTimeDigits values: 0-8
# 0   = 2016/05/12@12:34:56 = per seconds
# 1   = 2016/05/12@12:34:5  = per 10 seconds
# 2,3 = 2016/05/12@12:34:   = per minutes
# 4   = 2016/05/12@12:3     = per 10 minutes
# 5,6 = 2016/05/12@12:      = per hours
# 7   = 2016/05/12@1        = per 10 or 4 hours
# 8   = 2016/05/12@         = per days

  CutTimeDigits=0 # Default is per second statistics.
  Logins="no"
  Params="no"
  Fatal="no"
  Errors="no"
  Remains="no"
  Summary="no"
  Options=""
  while [ $# -gt 0 ]
  do
# CutTimeDigits:
    case "$1" in
      [0-9]*)  CutTimeDigits=$1; shift; continue;;
    esac

# Params:
    case "$1" in
      -*[pP]*) Params="yes";  Options="$Options Params";;
    esac

# Logins:
    case "$1" in
      -*[lL]*) Logins="yes";  Options="$Options Logins";;
    esac

# Fatal:
    case "$1" in
      -*[fF]*) Fatal="yes";   Options="$Options Fatal";;
    esac

# Errors:
    case "$1" in
      -*[eE]*) Errors="yes"; Options="$Options Errors";;
    esac

# Remains:
    case "$1" in
      -*[rR]*) Remains="yes"; Options="$Options Remains";;
    esac

# Summary:
    case "$1" in
      -*[sS]*) Summary="yes"; Options="$Options Summary";;
    esac

    Rest=`echo $1 | tr -d "lLpPeEfFrRsS\-"`
    if [ "$Rest" ]
    then
      echo "LogParser: \"$Rest\" is an unknown option."
      exit 1
    fi # if Rest

    shift
  done  # while [ $# -gt 0 ]

  test $CutTimeDigits -gt 8 && \
  CutTimeDigits=8
# CutTimeDigits=0: 2016/05/12@14:03:52
# CutTimeDigits=1: 2016/05/12@14:03:5
# ...
# CutTimeDigits=7: 2016/05/12@1
# CutTimeDigits=8: 2016/05/12@

  Options=`echo $Options | \
           awk 'BEGIN {RS=" "}
                {print}' | \
           sort` # Options
  Options=`echo $Options`
  echo "LogParser Options: $Options"
  test "$Logins" = "yes" && \
  case "$CutTimeDigits" in
    0)   echo "Login statistics per second:"    ;; # 2016/05/12@12:34:56
    1)   echo "Login statistics per 10 seconds:";; # 2016/05/12@12:34:5
    2|3) echo "Login statistics per minute:"    ;; # 2016/05/12@12:34:
    4)   echo "Login statistics per 10 minutes:";; # 2016/05/12@12:3
    5|6) echo "Login statistics per hour:"      ;; # 2016/05/12@12:
    7)   echo "Login statistics per 10 hours:"  ;; # 2016/05/12@1
    8)   echo "Login statistics per day:"       ;; # 2016/05/12@
  esac # CutTimeDigits

  Prefix=${DbLogPrefix:-`basename $0`.$$}.LogParser
  Prefix=/tmp/`basename $Prefix`
  TmpFile=$Prefix.tmp

# Get a unique name for the temporary file:
#
  Count=0
  while [ -f $TmpFile ]
  do
    Count=`expr $Count + 1`
    TmpFile=$Prefix.$Count.tmp
  done

  awk '
    BEGIN {
      NoNum="(-----)"
      TmpFile="'$TmpFile'"
      TimeSize=19-'$CutTimeDigits'
      Logins="'$Logins'"
      Params="'$Params'"
      Fatal="'$Fatal'"
      Errors="'$Errors'"
      Remains="'$Remains'"
      Summary="'$Summary'"
#
# "Logins" = the group of the messages related to the activity on User Control Table:
#
      LoginsNum[  "(298)"]="KILL signal received."
      LoginsNum[  "(333)"]="Multi-user session begin."
      LoginsNum[  "(334)"]="<Multi/Single> session end. (334)"
      LoginsNum[  "(451)"]="<session-type> session begin for <user> on <tty>. (451)"
      LoginsNum[  "(452)"]="Login by <user> on <tty>."
      LoginsNum[  "(453)"]="Logout by <user> on <tty>."
      LoginsNum[  "(562)"]="HANGUP signal received."
      LoginsNum[  "(708)"]="Userid is now <name>."
      LoginsNum[  "(742)"]="Login usernum <n>, userid <name>, on <tty/node>."
      LoginsNum[  "(739)"]="Logout usernum <n>, userid <name>, on <tty/node>."
      LoginsNum[  "(792)"]="Message received with bad usernum."
      LoginsNum[  "(794)"]="Usernum  terminated abnormally."
      LoginsNum[  "(795)"]="Error reading socket=<n> ret=<n> errno=<n>."
      LoginsNum[  "(796)"]="Error writing msg, socket=<n> errno=<n> usernum=<n> disconnected."
      LoginsNum[  "(797)"]="Partial write, socket=<n> msglen=<n> ret=<n> user <n> disconnected."
      LoginsNum[ "(1153)"]="BROKER detects death of server <pid>."
      LoginsNum[ "(1334)"]="Rejecting login -- too many users for this server."
      LoginsNum[ "(1166)"]="Server disconnecting user <n>."
      LoginsNum[ "(2252)"]="Begin transaction backout."
      LoginsNum[ "(2253)"]="Transaction backout completed."
      LoginsNum[ "(2266)"]="Expected user <n> received user <n>."
      LoginsNum[ "(2518)"]="Started. (APW, BIW, AIW or WDOG)"
      LoginsNum[ "(2519)"]="Disconnected. (APW or WDOG)"
      LoginsNum[ "(2520)"]="Stopped. (BIW or AIW)"
      LoginsNum[ "(2268)"]="Invalid message was received for an attempted login."
      LoginsNum[ "(2525)"]="Disconnecting dead server <n>."
      LoginsNum[ "(2526)"]="Disconnecting client <n> of dead server <n>."
      LoginsNum[ "(2527)"]="Disconnecting dead user <n>."
      LoginsNum[ "(4375)"]="Received signal <sigNum>; handling as SIGHUP."
      LoginsNum[ "(4698)"]="User count exceeded. Failed attempt to login by <userid> on <devtype>."
#     LoginsNum[ "(4732)"]="Login by <name> on <tty>. Pid <pid>."             # DEC/OpenVMS
#     LoginsNum[ "(4734)"]="Logout by <name> on <tty>. Pid <pid>."            # DEC/OpenVMS
      LoginsNum[ "(5569)"]="Quiet point request login by <user> on <ttyxxx>." # V8.2A
      LoginsNum[ "(5570)"]="Quiet point request logout"                       # V8.2A
      LoginsNum[ "(7129)"]="Usr <n> set name to <name>."                      # V8.3B Dbdes, Aimage new, Aimage list, BACKUP
      LoginsNum[ "(8873)"]="Login usernum <n>, remote SQL client."            # V9.1B, logout with msg 453
#     LoginsNum["(12092)"]="User <n> disconnect initiated."                   # V10.1A, Not used
      LoginsNum["(12454)"]="Server <n> has <n> unresolved pending connections(s). Please check port <n>."
      LoginsNum["(12455)"]="Clearing pending connections from server <n>"     # V10.2B
      LoginsNum["(17961)"]="User <n> set tty to <name>. (17961)"              # V11.6
#
# Identify the messages without the numbers by their text pattern:
#
# PROMON  5: (-----) Login by root.
# BACKUP  7: (-----) Login by root.
# SQLSRV2 3: (-----) Login by user 2525. Using TCP/IP IPV4 address 192.168.10.162
      LoginsMsg["Login by"]="(SQL User, PROMON or BACKUP)"
      LoginsMsg["Logout by"]="SQL User (SQLSRV2)" # SQLSRV2 3: (-----) Logout by user 2525.
      LoginsMsg["Sending signal to user"]="(BROKER or PROSHUT)"
      LoginsMsg["User disconnect initiated"]="(PROSHUT)"

      LoginsMsg["Received RECONNECT from WTB"]="(Webspeed user/agent)"
# This is an informational message which is sent on behalf of a user.
# This message indicates that the user who connected to the database
# server is a Webspeed user/agent and that the initial connection
# between the WebSpeed broker and agent was completed.

      LoginsMsg["Notifying srvr"]="<n>, conn <n>, to terminate remote user <n> (12093)"
# SHUT 2146: (-----) User 7775 disconnect initiated
# SHUT 2146: (453)   Logout by <user> on batch.
# BROKER  0: (-----) Notifying srvr 24, conn 24, to terminate remote user 7775 <== should be (12093)
# BROKER  0: (-----) Sending signal 5 to user 7775
# SRV    24: (1166)  Server disconnecting user 7775.

#
# "Ignore" = the group of the informational messages that can be ignored:
#
# Miscellaneous:
#
      IgnoreNum[  "(439)"]="** Save file named core for analysis by Progress Software Corporation."
      IgnoreNum[ "(2261)"]="Sending signal <n> to <n> connected user(s)."
      IgnoreNum[ "(2263)"]="Resending shutdown request to <n> user(s)."
      IgnoreNum[ "(8863)"]="This broker supports 4GL server groups only."
      IgnoreNum[ "(8864)"]="This broker supports SQL server groups only."
      IgnoreNum[ "(8865)"]="This broker supports both 4GL and SQL server groups."
      IgnoreNum["(10471)"]="Database connections have been enabled."
      IgnoreNum["(16869)"]="Removed shared memory with segment_id: <n>"
#
# Warnings:
#
      IgnoreNum[ "(5407)"]="WARNING: -nb exceeded. Automatically increasing from <old> to <new>."
      IgnoreNum[ "(5408)"]="WARNING: -l exceeded. Automatically increasing from <old> to <new>."
      IgnoreNum[ "(5409)"]="WARNING: -mmax exceeded. Automatically increasing from <old> to <new>."
      IgnoreNum[ "(5410)"]="WARNING: -D limit has been exceeded; automatically increasing to <new>."
#
# Connection:
#
      IgnoreNum[ "(5644)"]="Started for <name> using tcp IPV4 address 0.0.0.0, pid <n>."
      IgnoreNum[ "(5645)"]="This is an additional broker for this protocol."
      IgnoreNum[ "(5646)"]="Started on port <port> using tcp IPV4 address 0.0.0.0, pid <n>."
      IgnoreNum[ "(5512)"]="Previous message sent on behalf of user <n>. (5512)"
      IgnoreNum["(14658)"]="Previous message sent on behalf of user <n>, server pid <n>, broker pid <n>. (5512)"
      IgnoreNum["(10836)"]="Database connections are not allowed at this time."
      IgnoreNum["(12698)"]="User <name> Roles: <name>"
      IgnoreNum["(12699)"]="Database <name> Options: <name>"
      IgnoreNum["(15646)"]="Started on port <port> using tcp, pid <pid>."
#
# SQLSRV2: Database statistics
#
      IgnoreNum["(10995)"]="Database statistics (<type>) updated for all tables."
      IgnoreNum["(10996)"]="Database statistics (<type>) updated for table <name>."
      IgnoreNum["(10997)"]="Database statistics (<schema table>) updated by direct user sql statement (<type>)."
#
# RFUTIL:
#
      IgnoreNum[ "(3774)"]="Backup after-image extent and mark it as empty."
      IgnoreNum[ "(3776)"]="Backup ai extent and mark it as empty."
      IgnoreNum[ "(3777)"]="Switched to ai extent <name>."
      IgnoreNum[ "(3778)"]="This is after-image file number <n> since the last AIMAGE BEGIN"
      IgnoreNum[ "(3789)"]="Marked after-image extent <name> EMPTY."
      IgnoreNum["(11805)"]="Unlocking after-image file <n> and locking ALL FULL after-image files beginning with file <n>."
      IgnoreNum["(12074)"]="The After-image extent <name> was successfully extracted."
      IgnoreNum["(13154)"]="Marked after-image extent <name> ARCHIVED."
      IgnoreNum["(13199)"]="After-image extent <name> has been copied to <name>."
      IgnoreNum["(13231)"]="From this point forward all after-image extents will be archived to <name>."
#
# BACKUP:
#
      IgnoreNum[ "(1074)"]="Wrote a total of <blocks> backup blocks using <bytes> bytes of media."
      IgnoreNum[ "(1361)"]="Incremental backup started."
      IgnoreNum[ "(1362)"]="Full backup started."
      IgnoreNum[ "(1363)"]="Incremental backup successfully completed."
      IgnoreNum[ "(1364)"]="Full backup successfully completed."
      IgnoreNum[ "(1365)"]="Database copied from <database-name>."
      IgnoreNum[ "(1366)"]="Incremental restore of sequence <n> started."
      IgnoreNum[ "(1367)"]="Incremental restore of sequence <n> completed."
      IgnoreNum[ "(3664)"]="Incremental backup started."
      IgnoreNum[ "(5459)"]="Begin backup of Before Image file(s)."
      IgnoreNum[ "(5460)"]="End backup of Before Image file(s)."
      IgnoreNum[ "(5461)"]="Begin backup of Data file(s)."
      IgnoreNum[ "(5462)"]="End backup of Data file(s)."
      IgnoreNum[ "(6678)"]="NOTE: Backup media estimates done without database scan." # V8.3A
      IgnoreNum[ "(6680)"]="Estimate for uncompressed full backup is provided."
      IgnoreNum[ "(6686)"]="<n> active blocks out of <n> blocks in <name> will be dumped."
      IgnoreNum[ "(6688)"]="<n> BI blocks will be dumped."                            # V8.3B
      IgnoreNum[ "(6689)"]="Backup requires  blocks ( Mb) of media."
      IgnoreNum[ "(6699)"]="Restore would require  blocks ( Mb) of media."
      IgnoreNum[ "(6760)"]="This backup was taken <time>."                            # V8.3B
      IgnoreNum[ "(9285)"]="Backup requires an estimated <size+id> of media."         # V9.1C
      IgnoreNum[ "(9286)"]="Restore would require an estimated  db blocks using <size+id> of media."
      IgnoreNum["(12850)"]="Backup blocks will be written to <device/file>."          # V10.2B
      IgnoreNum["(13625)"]="Wrote a total of <blocks> backup blocks using <size+id> of media."
      IgnoreNum["(13786)"]="This backup was also used to enable after-image on the source database."
      IgnoreNum["(14256)"]="The Replication Server has been notified that an online backup is about to be performed."
      IgnoreNum["(14258)"]="The online backup of this database has completed normally."
      IgnoreNum["(15108)"]="The online backup has finished backing up the BI. The Replication Agent will now begin recovery synchronization followed by normal processing."
      IgnoreNum["(15109)"]="At Database close the number of live transactions is <n>."
      IgnoreNum["(15321)"]="Before Image Log Initialisation at block <n> offset <n>."
      IgnoreNum["(15743)"]="Before Image Log Completion at Block <n> Offset <n>."      # V10.2B
      IgnoreNum["(16866)"]="Dumped <n> active BI blocks."                              # V11.2
#
# Identify the messages without the numbers by their text pattern:
#
# BROKER  2: (-----) Shared Library Path set to LIBPATH=<path>
#
      IgnoreMsg["Shared Library Path set to"]=""

# SQLSRV2 3: (-----) SQL Server 11.6.01 started, configuration: "dbname.virtualconfig"
# SQLSRV2 3: (-----) "/path/to/dbname" started on IPv4 port 1025 for address 0.0.0.0, pid 9568520 (0x00920108).
# SQLSRV2 3: (-----) Thread stack size: 1024000 (bytes).
# SQLSRV2 3: (-----) DLC from ENVIRONMENT VARIABLE is: /usr/dlc
# SQLSRV2 3: (-----) WRKDIR from ENVIRONMENT VARIABLE is: /usr/wrk/
# SQLSRV2 3: (-----) JDKHOME from ENVIRONMENT VARIABLE is: /usr/dlc/jdk
# SQLSRV2 3: (-----) JREHOME from ENVIRONMENT VARIABLE is: /usr/dlc/jdk/jre
# SQLSRV2 3: (-----) CLASSPATH from DEFAULT is:
# SQLSRV2 3: (-----) PROSQL_LOCKWAIT_TIMEOUT value is: 5 seconds

      IgnoreMsg["started, configuration:"]=""
      IgnoreMsg["started on port for address pid"]=""
      IgnoreMsg["Thread stack size:"]=""
      IgnoreMsg["DLC from ENVIRONMENT VARIABLE is:"]=""
      IgnoreMsg["WRKDIR from ENVIRONMENT VARIABLE is:"]=""
      IgnoreMsg["JDKHOME from ENVIRONMENT VARIABLE is:"]=""
      IgnoreMsg["JREHOME from ENVIRONMENT VARIABLE is:"]=""
      IgnoreMsg["CLASSPATH from DEFAULT is:"]=""
      IgnoreMsg["PROSQL_LOCKWAIT_TIMEOUT value is:"]=""

      PrevLogins=""
      PrevLoginTime=""

# SunOS can rise the error if the arrays are not "initiated":
# awk: Count is not an array
      TotalNumCount[""]=0
      TotalMsgCount[""]=0
      LoginNumCount[""]=0
      LoginMsgCount[""]=0
    } # BEGIN
#
# Skip the lines that do not begin with timestamps
# like [2016/05/12@14:03:52.486+0400]
#       1234567890123456789012345678
#
    ! /^\[/ {next}
#
# Lines with timestamps:
#
    { CurrTime=substr($1,2,length($1)-2) # Time without "[]"
      if(InitTime=="") InitTime=CurrTime
      if(PrevTime=="") PrevTime=CurrTime

      D=substr(CurrTime,1,11) # Year/Month/Day
      H=substr(CurrTime,12,2) # Hours
      M=substr(CurrTime,15,2) # Minutes
      S=substr(CurrTime,18,2) # Seconds
      Z=substr(CurrTime,length(CurrTime)-4) # Timezone -0400
    }
    Z ~/^\+/ {                # TZ +0400
      H-=substr(Z,2,2)        # Hours
      M-=substr(Z,4,2)        # Minutes
    } # TZ +0400
    Z ~/^\-/ {                # TZ -0400
      H+=substr(Z,2,2)        # Hours
      M+=substr(Z,4,2)        # Minutes
    } # TZ -0400
#
# Article Number: 000064856
# Defect PSC00256028, PSC00257044
# Upgrade to OpenEdge 11.4 or later
# A partial fix is included in OpenEdge 11.2.1 and 11.3.x
# As a side effect of the fix, messages from signal handlers will always show
# as UTC time instead of the database timezone.
#
# Example:
# [2016/05/23@04:11:27.185-0400] P-12 T-1 I ABL 8: (452) Login by user on /dev/pts/4.
# [2016/05/23@08:52:07.000+0000] P-12 T-1 I ABL 8: (562) HANGUP signal received.
# [2016/05/23@04:52:07.926-0400] P-12 T-1 I ABL 8: (453) Logout by hchen on /dev/pts/4.
#
# Convert the current time to UTC time to get the correct "Logins" statistics:
    { if(length(H)==1) H="0" H
      if(length(M)==1) M="0" M
      if(length(M)==1) S="0" S

      CurrLogins=D H ":" M ":" S
      CurrLogins=substr(CurrLogins,1,TimeSize) # TimeSize=19 = no cuts
    } # Timestamps
#
# Logins/logouts statistics report:
#
    Logins=="yes" && PrevLogins!=CurrLogins {
      for(Num in LoginNumCount) if(LoginNumCount[Num]>0)
      {
# Time: 2016/05/12@14:03:52.486+0400
#       1234567890123456789012345678
#
        printf"%10s %8s %8s %4d %7s %s\n",\
          substr(PrevLoginTime,1,10),\
          substr(PrevLoginTime,12),\
          substr(LastLoginTime,12),\
          LoginNumCount[Num],\
          Num,\
          LoginsNum[Num]

# Either set to zero or delete an element of the array - any option that will work:
        LoginNumCount[Num]=0
        delete LoginNumCount[Num]
      } # for Num in LoginNumCount

      for(Msg in LoginMsgCount) if(LoginMsgCount[Msg]>0) {
        printf"%10s %8s %8s %4d %7s %s\n",\
          substr(PrevLoginTime,1,10),\
          substr(PrevLoginTime,12),\
          substr(LastLoginTime,12),\
          LoginMsgCount[Msg],\
          NoNum,\
          Msg " " LoginsMsg[Msg]

# Either set to zero or delete an element of the array - any option that will work:
        LoginMsgCount[Msg]=0
        delete LoginMsgCount[Msg]
      } # for Msg in LoginMsgCount

      PrevLoginTime=""
      PrevLogins=CurrLogins
    } # Logins=="yes"
#
# Msg number is column 7 if there is a space between user type and user number:
# [2016/05/12@14:03:52.486+0400] P-1 T-1 I ABL 10: (452) Login by root on batch.
#
# Msg number is column 6 if there is no space between user type and user number:
# [2016/04/05@06:00:23.102+0600] P-2 T-1 I RFUTI1312: (452) Login by root on batch.
#
    { Num=""
      for(Col=6;Col<=7;Col++) if($Col ~ /^\([0-9]*-*\)$/) {
        Num=$Col
        break
      } # for Col
      if(Num=="") next
#
# Cleaning a message: remove any word containing a digit.
#
      Msg=$(Col+1)
      for(i=Col+2;i<=NF;i++) if($i !~/[0-9]/) Msg=Msg " " $i
    }
#
# Messages without numbers:
#
    Num==NoNum {
#
# Ignore the block dumps (dont count them in the Summary):
# (-----) 1fd0:  652d 5883 4e53 5056 0000 2565 2d74 7970
#
      if(NF==Col+9 && $(Col+1) ~ /^[0-1]/ && $(Col+1) ~ /:$/) next

# Summary:
      TotalMsgCount[Msg]++

# Fatal:
      if($4=="F") {
        FatalCount++
        if(Fatal=="yes") {
          print
          next
        } # if Fatal
      } # if "F"

# Errors:
      if(Msg ~ /SYSTEM ERROR: /) {
        ErrorsCount++
        if(Errors=="yes") {
          print
          next
        } # if Errors
      } # if SYSTEM ERROR:

      for(Pattern in IgnoreMsg) if(index(Msg,Pattern)>0) next

      for(Pattern in LoginsMsg) if(index(Msg,Pattern)>0) {
        LoginMsgCount[Pattern]++
        if(PrevLoginTime=="")
        PrevLoginTime=CurrTime
        LastLoginTime=CurrTime
        next
      } # LoginsMsg

      if(Startup=="yes") {
        if(Params=="yes") print
        next
      } # if Startup

      RemainsCount++
      if(Remains=="yes") print
      next
    } #  Messages without numbers
#
# The numbered messages:
#
# Summary:
    { TotalNumCount[Num]++
      Example[Num]=Msg

# Fatal:
      if($4=="F") {
        FatalCount++
        if(Fatal=="yes") {
          print
          next
        } # if Fatal
      } # if "F"

# Errors:
      if(Msg ~ /SYSTEM ERROR: /) {
        ErrorsCount++
        if(Errors=="yes") {
          print
          next
        } # if Errors
      } # if SYSTEM ERROR:
#
# "Params" group:
#
# BROKER  0: (333)   Multi-user session begin.
# BROKER  0: (4234)  Progress OpenEdge Release <VERSION_NUMBER> on <OS_PLATFORM>.
# BROKER  0: (10471) Database connections have been enabled.
# PROLOG  5: (12684) prolog log file truncation <start/end>. (12684)
#
      if(Num=="(333)")   Startup="yes"
      if(Num=="(4234)")  Startup="yes"
      if(Num=="(10471)") Startup="no"
      if($5!="BROKER" && $5!="PROLOG") Startup="no"

      for(Pattern in IgnoreNum) if(Num==Pattern) next
      for(Pattern in LoginsNum) if(Num==Pattern) {
        LoginNumCount[Pattern]++
        if(PrevLoginTime=="")
        PrevLoginTime=CurrTime
        LastLoginTime=CurrTime
        next
      } # LoginsNum
      if(Startup=="yes") {
        if(Params=="yes")  print
        next
      } # if Startup

      RemainsCount++
      if(Remains=="yes") print
      next
    } # Numbered messages

    END {
      if(Logins=="yes")
      for(Num in LoginNumCount) if(LoginNumCount[Num]>0)
        printf"%10s %8s %8s %4d %7s %s\n",\
          substr(PrevLoginTime,1,10),\
          substr(PrevLoginTime,12),\
          substr(LastLoginTime,12),\
          LoginNumCount[Num],\
          Num,\
          LoginsNum[Num]
      # if Logins

      Count=0
      NumTypeCount=0
      MsgTypeCount=0
      for(Num in TotalNumCount) if(TotalNumCount[Num]>0) {
        Count+=TotalNumCount[Num]
        NumTypeCount++
      } # for Num if
      for(Msg in TotalMsgCount) if(TotalMsgCount[Msg]>0) {
        Count+=TotalMsgCount[Msg]
        MsgTypeCount++
      } # for Msg if

      printf"\nProcessed %d lines, %d messages with timestamps\n",NR,Count
      printf"from %s\n",InitTime
      printf"  to %s\n",CurrTime
      printf"Numbered messages: %d, without numbers: %d\n",NumTypeCount,MsgTypeCount
      if(Fatal!="yes")  RemainsCount-=FatalCount
      if(Errors!="yes") RemainsCount-=ErrorsCount
      printf"System errors: %d, fatal errors: %d, \"remains\": %d\n",ErrorsCount,FatalCount,RemainsCount

      if("'$Summary'"!="yes") exit
#
# Replace the examples by the known message descriptions:
#
      for(Num in LoginsNum) Example[Num]=LoginsNum[Num]
      for(Num in IgnoreNum) Example[Num]=IgnoreNum[Num]

      printf"\nSummary per the message numbers:\n"
      for(Num in TotalNumCount) if(TotalNumCount[Num]>0)
              printf"%6d %7s %s\n",TotalNumCount[Num],Num,Example[Num] >TmpFile
      for(Msg in TotalMsgCount) if(TotalMsgCount[Msg]>0)
              printf"%6d %7s %s\n",TotalMsgCount[Msg],NoNum,Msg >TmpFile
      close(TmpFile)
    } # END
  ' -

  test -f $TmpFile && \
  cat     $TmpFile | sort -n -r && \
  rm   -f $TmpFile

} # LogParser()


#------------------------------------------------------------------------------
#
LicenseFile()
{
# Output the last lines of Progress license file.

  Lic=$1      # Db name or .lic file
  Num=${2:-2} # Number of lines to display

#  1. Current date
#  2. Current time
#  3. Number of licensed users specified by the Progress configuration file
#  4. Current number of total connections
#  5. Maximum number of total connections for the last hour
#  6. Minimum number of total connections for the last hour
#  7. Current number of interactive connections
#  8. Maximum number of interactive connections for the last hour
#  9. Minimum number of interactive connections for the last hour
# 10. Current number of batch connections
# 11. Maximum number of batch connections for the last hour
# 12. Minimum number of batch connections for the last hour
#
# Example:
# 07/04/14  15:00:00  1200 2623 2624 2589 2226 2228 2192 397 398 395
# 07/04/14  16:00:02  1200 2656 2674 2622 2235 2245 2225 421 429 397
# 07/04/14  17:00:01  1200 2333 2333 0 1912 1912 0 421 421 0
# 07/04/14  18:00:05  1200 2453 2477 2332 2020 2044 1911 433 438 421

  Lic=`echo $Lic | sed -e 's/\.lic$//'`
  Lic=`echo $Lic | sed -e 's/\.db$//'`
  Lic=$Lic.lic

  echo "License file: $Lic
Date     Time    Limit ----Total----- ---Interact--- ----Batch-----"
  tail -$Num $Lic 2>/dev/null | \
  awk '{printf"%8s %8s",$1,$2
        for(i=3;i<=12;i++) printf"%5d",$i
        printf"\n"
  }' # awk

} # LicenseFile()


#------------------------------------------------------------------------------
#
MoveProtrace()
{
# Find, copy and rename an existent protrace file.

# ID of running process that created the protrace file:
  ProPID=$1

# The estimated location of protrace file:
  ProDir=$2

# Suffix to add to the file name (to easy identify the protrace file):
  Suffix=$3

  Ver=1
  MyCopy=protrace.$ProPID.`date '+%y%m%d_%H%M%S'`$Suffix
  while [ -f $MyCopy ]
  do
    Ver=`expr $Ver + 1`
    MyCopy=protrace.$ProPID.`date '+%y%m%d_%H%M%S'`.$Ver$Suffix
  done

#
# Attempt to use the estimated location:
#
  Protrace=$ProDir/protrace.$ProPID
  test -f $Protrace && \
  mv $Protrace $MyCopy 2>/dev/null && \
  return
#
# Attempt to use /proc
#
  if [ -d /proc ] && [ -r /proc/$ProPID/cwd ]
  then
    Protrace=/proc/$ProPID/cwd/protrace.$ProPID
    test -f $Protrace && \
    mv $Protrace $MyCopy 2>/dev/null
    return
  fi # if /proc
#
# Attempt to use pwdx:
#
  case ${OSNAME:-`uname`} in
   Linux) PWDX=pwdx;;
   SunOS) PWDX=pwdx;;
     AIX) PWDX=procwdx;;

# HP-UX: current working directory of process
# http://community.hpe.com/t5/Languages-and-Scripting/current-working-directory-of-process/td-p/5093802

# Just in case if pwdx is available:
       *) PWDX=pwdx;;
  esac

# Output should look like: 1234: /home/wrk
# Ingnore output like:     1234: Permission denied
  ProDir=`
    $PWDX $ProPID 2>/dev/null | \
    awk 'NF==2 {print $2}'
  ` # ProDir
  test "$ProDir" || return 1
  test "$ProDir" = "/" && ProDir=""
  Protrace=$ProDir/protrace.$ProPID
  test -f $Protrace && \
  mv $Protrace $MyCopy 2>/dev/null && \
  return

  return 1

} # MoveProtrace()


#------------------------------------------------------------------------------
#
DbConnectStat()
{
# Count the current number of sessions connected to a database.

  DB=$1

  PROSHUT=${PROSHUT:-$DLC/bin/_mprshut}

  echo Connection statistics for database $DB:
  $PROSHUT $DB -C list 2>/dev/null | \
  awk '

# usr    pid    time of login           user id    Type  tty           Limbo?
#   5   6023 Fri May 20 10:14:25 2016   root       SELF  /dev/pts/3    no
#   6  26680 Thu May 19 21:19:55 2016   root       APW   /dev/pts/1    no
#   8  10452 Fri May 20 12:06:52 2016   george     SHUT  /dev/pts/0    no
#   9   2370 Fri May 20 08:41:45 2016   root       WDOG  /dev/pts/1    no
#  24   9349 Fri May 20 11:39:04 2016   root       REMC  localhost 1   no

    BEGIN {
      getline
# No header => proshut returns an error:
      if($1!="usr") {
        print
        exit
      } # if "usr"
      UsrCount=0
      MinUsr=99999
      MaxUsr=0
      TypeCount[""]=0
    } # BEGIN

    {UsrCount++
     TypeCount[$9]++
     if($1<MinUsr) MinUsr=$1
     if($1>MaxUsr) MaxUsr=$1
    }
    END {
      Range=MaxUsr-MinUsr+1
      Unused=Range-UsrCount
      if(UsrCount==0) print "none"
      else printf"Usr=%d-%d, range=%d, active=%d, unused=%d\n",
            MinUsr, MaxUsr, Range, UsrCount, Unused
      for(Type in TypeCount) if(TypeCount[Type]>0)
        printf"%-6s%6d\n", Type,TypeCount[Type]
    } # END
  ' # awk

} # DbConnectStat()


#------------------------------------------------------------------------------
#
FreeDbConnect()
{
# Disconnect old sessions to allow new connections.
#
# It will choose the old sessions only of the following types:
# PROSHUT, PROMON, WATCHDOG and APW (exactly in this order).
#
# FreeDbConnect returns the list of the types of the disconnected sessions:
# SHUT SHUT SHUT MON
#
# You have attempted to connect to a database with too many
# users connected to it. Retry the connection later,
# or increase -n on the server. (5291)
#
# But Progress reserves one slot (with highest Usr number) for proshut.

  DB=$1

# The number of sessions to disconnect (usually one):
  Num=${2:-1}

# The "candidates" should have the priority higher than MinPriority:
  MinPriority=${3:-0}

  PROSHUT=${PROSHUT:-$DLC/bin/_mprshut}

  $PROSHUT $DB -C list 2>/dev/null | \
  awk '

# If some proshut already uses the reserved slot then our proshut will crash:
#
# proshut sports -C list 2>/dev/null
# SYSTEM ERROR: Memory violation. (49)
# ** Save file named core for analysis by Progress Software Corporation. (439)
# Quit
# echo $?
# 131
    BEGIN {
      getline
# No header => proshut returns an error:
      if($1!="usr") exit
    } # BEGIN

# proshut sports -C list
# usr    pid    time of login           user id     Type  tty           Limbo?
#   5  23883 Thu May 19 20:09:38 2016   george      MON   /dev/pts/0    no
#   6  23899 Thu May 19 20:09:50 2016   root        SHUT  /dev/pts/1    no
#   7  26680 Thu May 19 21:19:55 2016   root        APW   /dev/pts/1    no

    $9=="SHUT" {Priority=4}
    $9=="MON"  {Priority=3}
    $9=="WDOG" {Priority=2}
    $9=="APW"  {Priority=1}
    Priority>'$MinPriority' {
      printf"%d %4s",Priority,$9
      for(i=1;i<=7;i++) printf" %s",$i
      printf"\n"
      Priority=0
    } # Priority
  ' | \
  while read Priority Type Usr Pid Time
  do
# "%s" = seconds since 1970-01-01 00:00:00 UTC (Unix epoch)
# Use time in seconds to disconnect the most recent sessions:
    Time=`date +"%s" -d "$Time"`
    echo "$Priority $Time $Type $Pid $Usr"
  done | \
  sort | \
  tail -$Num | \
  while read Priority Time Type Pid Usr
  do
    $PROSHUT $DB -C disconnect $Usr && \
    echo2 "$Type (Usr $Usr) was disconnected"
    echo  "$Type"
  done

} # FreeDbConnect()


#------------------------------------------------------------------------------
#
LogHeader()
{
# Create a standard log header:
#
  DB=$1; shift
  Title=$*

  echo "$THIS Release $Release
$Title
   DB: $DB
 Date: `date '+%a %b %e %T %Y'`
 Host: `uname -a`
."

} # LogHeader()


#------------------------------------------------------------------------------
#
EnvInventory()
{
# Report the basic information about OS and Progress environment.
#
  echo "$THIS $RunParams
     PID: $$
    File: `ls -l $ScriptDir/$THIS`
 Release: $Release
    Date: `date`
    Host: `uname -a`
    User: $LOGNAME (`test "$TERM" && who am i`)
    TERM: $TERM
     TTY: $MyTTY
   SHELL: $Shell
 DB List: $DbNameList
Interval: $SampleIntrv "sec"
   Count: $SampleCount
 MonProc: $DbMonProc
ProTrace: $ProtraceOption
 WorkDir: $WorkDir
 StCaDir: $StCaDir
   email: $MailList
   Other: $StopRemains

------------------------------------------------------------------------------

OS environment:"

  case ${OSNAME:-`uname`} in
    Linux) # -----------------------------------------------------------------

      cat /proc/version                                          # Kernel type
      echo "Number of CPUs:" `grep processor /proc/cpuinfo | wc -l`
      grep "cpu MHz"    /proc/cpuinfo | tail -1                  # Speed
      grep "model name" /proc/cpuinfo | tail -1                  # Type
      grep "MemTotal:"  /proc/meminfo                            # RAM
      free -m                               # Free and used memory (-m for MB)
    ;;

    AIX) # -------------------------------------------------------------------

     echo "oslevel:" `oslevel -s`
#     4 . 3 . 3 . 0 <--------- Preventive Maintenance Level
#     |   |   |
#     |   |   +----------------Modification
#     |   +--------------------Release
#     +------------------------Version
      echo "highest technology level:" `oslevel -r`

      echo "Machine type:" `uname -Mu`
#
# Machine type: Machine type: IBM,8284-22A IBM,0221904AV
#
# The option -M gives the machine type and model.
# -u gives the plant code and machine identifier.
#
# The Machine type is 8284
# The Model is 22A
# OF prefixm IBM, 0221904AV

      /usr/sbin/prtconf | \
      awk 'NF==0 {exit}
           {print}'
#
# System Model: IBM,8284-22A
# Machine Serial Number: 21904AV
# Processor Type: PowerPC_POWER8
# Processor Implementation Mode: POWER 7
# Processor Version: PV_7_Compat
# Number Of Processors: 8
# Processor Clock Speed: 3425 MHz
# CPU Type: 64-bit
# Kernel Type: 64-bit
# LPAR Info: 11 corpdevx
# Memory Size: 32768 MB
# Good Memory Size: 32768 MB
# Platform Firmware level: SV810_101
# Firmware Version: IBM,SV810_101
# Console Login: enable
# Auto Restart: true
# Full Core: false

      lsattr -El proc0
#
# attribute   value          description           user_settable
# frequency   332000000      Processor Speed       False
# smt_enabled false          Processor SMT enabled False
# smt_threads 0              Processor SMT threads False
# state       enable         Processor state       False
# type        PowerPC_POWER7 Processor type       False
#
      lsattr -El sys0 | grep realmem  #Real memory on the system
# realmem         130809856          Amount of usable physical memory in Kbytes        False
#
    ;;

    SunOS) # -----------------------------------------------------------------

      isainfo -kv                                             # Kernel type
# 64-bit sparcv9 kernel modules

      uname -X | grep "NumCPU"                                # Number of CPUs
# NumCPU = 96

      /usr/sbin/psrinfo -v | grep "operates at" | sort | uniq # Type & Speed
#   The sparcv9 processor operates at 1165 MHz,

      /usr/sbin/prtconf | grep "Memory size:"                 # Memory
# Memory size: 32544 Megabytes

      swap -l                 # Details of the system_s virtual swap space
# swapfile             dev  swaplo blocks   free
# /dev/dsk/c1t0d0s1   32,33     16 67120880 67120880
    ;;

    HP-UX) # -----------------------------------------------------------------

      machinfo
#
# CPU info:
#    Intel(R)  Itanium(R)  Processor 9350 (1.73 GHz, 24 MB)
#    4 cores, 8 logical processors per socket
#    4.79 GT/s QPI, CPU version E0
#           Active processor count:
#           2 sockets
#           8 cores (4 per socket)
#           8 logical processors (4 per socket)
#           LCPU attribute is disabled
# 
# Memory: 65434 MB (63.9 GB)
# 
# Firmware info:
#    Firmware revision:  01.72
#    FP SWA driver revision: 1.18
#    IPMI is supported on this system.
#    BMC firmware revision: 1.40
# 
# Platform info:
#    Model:                  "ia64 hp Integrity BL860c i2"
#    Machine ID number:      7bfa4cc0-c44e-11e0-8605-3ebccb98f116
#    Machine serial number:  CZ3127L609
# 
# OS info:
#    Nodename:  qpt3
#    Release:   HP-UX B.11.31
#    Version:   U (unlimited-user license)
#    Machine:   ia64
#    ID Number: 2080001216
#    vmunix _release_version:               
# @(#) $Revision: vmunix:    B.11.31_LR FLAVOR=perf 
    ;;

  esac # case OSNAME


  echo "Uptime: `uptime`"

  type java 2>/dev/null && \
  java -version

  echo "
PATH=$PATH

File systems (mount):"
mount

  echo "
------------------------------------------------------------------------------

Progress environment:"
  echo DLC=$DLC
  cat $DLC/version
  file $PROSHUT
  PROCFG=${PROCFG:-$DLC/progress.cfg}
  $DLC/bin/showcfg $PROCFG | grep -i "Product Name:"
  $DLC/bin/showcfg $PROCFG | grep -i "Port Number" | uniq

  grep -i platform $DLC/installd.ini 2>&1
#
# nPlatformId=43

  PLATID=`grep "PLATID=" $DLC/bin/java_env | cut -d "=" -f 2`
  echo PLATID=$PLATID
#
# PLATID=43

  grep " $PLATID[\) ]" $DLC/bin/java_env | tail -1
#
# 43)    # PortBit="Linux-64bit"
#      39)  # Solaris64
#      36 | 40) # HPUX-64bit or HPUX-64bit-Itanium

# _dbutil
# %8d       -    - -      (not PROGRESS)
# %8d %8d    - -     (unsupported shared memory version)
#
  echo "
Variables:"
  set | grep "PRO"

  echo ""
  $PROUTIL -C dbipcs | \
  grep -v "not PROGRESS" | \
  grep -v "unsupported shared memory version"

  echo "
------------------------------------------------------------------------------
"

} # EnvInventory()


#------------------------------------------------------------------------------
#
ExtendDbipcs()
{
# Combine the outputs of "proutil -C dbipcs" and "ipcs -ma"
#
  DB=$1
#
# Full path dbname:
#
  Dir=`dirname $DB`
  Dir=`(cd $Dir; pwd)`
  DB=$Dir/`basename $DB`

  PROUTIL=${PROUTIL:-$DLC/bin/_proutil}

  IPCS=`
    ipcs -ma | \
    awk '
      $2 ~/[0-9]/ {exit}
      length($0)>length(Title) {Title=$0}
      END {print Title}
    ' # awk
  ` # IPCS

  echo "
PROGRESS SHARED MEMORY STATUS: $DB
      ID ShMemVer Seg# InUse $IPCS"

#       ID ShMemVer Seg# InUse Database
#  1114130  6413712    0 Yes   /usr/wrk/sports.db

  $PROUTIL -C dbipcs | \
  awk '$5=="'$DB'.db" {printf"%8d %8d %4d %3s\n",$1,$2,$3,$4}' | \
  while IFS="" read Dbipcs
  do
    Id=`echo $Dbipcs | awk '{print $1}'`
    Ipcs=`ipcs -ma | awk '$2=='$Id''`
    echo "$Dbipcs   $Ipcs"
  done

} # ExtendDbipcs()


#------------------------------------------------------------------------------
#
DbDescribe()
{
# Run various commands to gather the basic information about database.

  DB=$1
  Option=$2  # with/without -F

  Log=${DbLogPrefix:-`basename $0`.$$.`basename $DB`}.description.log

  PROUTIL=${PROUTIL:-$DLC/bin/_proutil}
  DBUTIL=${DBUTIL:-$DLC/bin/_dbutil}

# Output to Log:
(
  LogHeader $DB DbDescribe $Option

  if [ -r $DB.lk ]
  then
    echo "lk File Owner:"
    LkPID=`LkInfo -pid $DB`
    ps -fp $LkPID 2>&1
  fi # if -r DB.lk

  echo "
------------------------------------------------------------------------------

Shared Memory:"

  ExtendDbipcs $DB

# Run "proutil -C describe" only when there are no problem with db connection.
  if [ -z "$Option" ]
  then
    echo "
------------------------------------------------------------------------------

proutil $DB -C describe"

    $PROUTIL $DB -C describe
#
# Display the descriptive information about the database and enabled features:
# Latches: 5*USR,24*SCH,3*LKP,4*GST,18*LKT,21*LKF,42*BHT,42*LRU,84*BUF
# Reads:
# _Db._Database
# _File._File-Name
# _User._Userid
# _Sysdbauth._Idxsysdbauth
# _seq-granted-role._Grantee
# _seq-authentication-domain._Domain-name

  fi # if -z Option

  echo "
------------------------------------------------------------------------------

License File"

# Process counts for the last 24 hours:
#
  LicenseFile $DB 24

  echo "
------------------------------------------------------------------------------

List of database extents:
"
  DbFileList $DB

  echo "
------------------------------------------------------------------------------

prostrct statistics
"
  $DBUTIL prostrct statistics $DB
#
# Displays statistics about storage areas:
# Reads a block that is not on LRU chain (master block?). It also reads
# ACO blocks but directly from disk rather than from database buffer pool.
# It's safe to run prostrct statistics even when database hangs.
# Though "prostrct statistics" itself can die with the error:
# SYSTEM ERROR: Shared memory access permission denied (1136)
# ** Save file named core for analysis by Progress Software Corporation. (439)

# Run "proutil -C viewB2" only when there are no problem with db connection.
  if [ -z "$Option" ]
  then
    echo "
------------------------------------------------------------------------------

proutil $DB -C viewB2"

    $PROUTIL $DB -C viewB2
#
# Display the descriptive information about the database and enabled features:
# Latches: 5*USR,24*SCH,3*LKP,2*GST,136*LKT,139*LKF,3815*BHT,3815*LRU,7630*BUF

  fi # if -z Option
) >$Log 2>&1 &

} # DbDescribe()


#------------------------------------------------------------------------------
ProbeConnect()
{
#
# Launch a probe session that connect a database and immediately quit.

  DB=$1
  Log=$2

  PROSHUT=${PROSHUT:-$DLC/bin/_mprshut}
  echo "Q" | $PROSHUT $DB -0 -NL 2>/dev/null >$Log &
#
# Since V10.2A01 the messages in db log:
# PROMON  5: (-----) Login by root.
# PROMON  5: (453)   Logout by root on batch.
#
# OE00179544 fixed in 10.2A01
# promon and proshut utilities do not log their login or logout
# into database log file
#
# For Progress versions below 10.2A01 you can use 4GL session:
#
# PROEXE=${PROEXE:-$DLC/bin/_progres}
# $PROEXE $DB -b >$Log 2>&1 &
# Session will immediately terminate with the error:
# ** Batch-mode PROGRESS requires a startup procedure. (1144)
# in db log:
# ABL     5: (452)   Login by root on /dev/pts/1.
# ABL     5: (12699) Database sports Options:
# ABL     5: (453)   Logout by root on /dev/pts/1.

# To test the logic of dbmon script:
#
# Immitation of "probe session hung connecting DB" (Connect = "Locked"):
# Process will exist 1 sec after its launch but no login message in db log:
# sleep 10 &

# Immitation of "probe session hung connecting DB" (Connect = "Failed"):
# Process will exist 1 sec after its launch and writes the login messages:
# echo "5
#U
#Q
#Q" | $PROSHUT $DB -0 -NL 2>/dev/null >$Log &

# Immitation of "probe session failed to connect DB" (Connect = "ErrorNum")
# echo "Q" | $PROSHUT $DB -0 -NL -zp 2>/dev/null >$Log &
# Note about the -zp parameter:
# The id of this process is <PID>. (1408)
# The message is written to stdout before db will be connected.

} # ProbeConnect()


#------------------------------------------------------------------------------

LruShot()
{
# "Flu Shot": make ACO ("Area Control Object") Object Blocks
# insensitive to the contention on LRU latch.
#
# ACO (Area Control Object) object block = bk_type 12 + objectId 0,
# ACO block is the block 3 in each area.
#
# Script sets "Skips" value in Cache Entries (promon/R&D/debghb/6/1)
# to 2147483647 (a maximum value of the -lruskips). Access to these blocks
# will not acquire the LRU latch the next 2 billions times and  promon will
# work even when LRU latch is locked.
#
  DB=$1

  MaxSkips=2147483647
  MinSkips=1000

# Example for sports db (see "Skips" column):
#
#   promon/R&D/debghb/6/4. Lru Chains
#
# 05/18/16        Status: Lru Chains
# 08:56:13
#
#   Num  DBKEY  Area  Hash T S Usect Flags  Updctr  Lsn Chkpnt  Lru      Skips
#
#     1  11744     6   302 D       0 L          85    0      0    0          0
#     2   4032     6    61 D       0 L         166    0      0    0          0
#     3    576     6   840 I       0 L           2    0      0    0          0
#     4     64    11   622 O       0 L           6    0      0    0 2147483646
#     5      2    10   485 O       0 L           6    0      0    0 2147483646
#     6      2     9   348 O       0 L           6    0      0    0 2147483646
#     7     64     8   211 O       0 L          10    0      0    0 2147483646
#     8     64     7    74 O       0 L          43    0      0    0 2147483646
#     9     64     6   824 O       0 L        1385    0      0    0 2147483646
#    10     64     1   139 O       0 L           4    0      0    0 2147483646
#    11    928     6   851 I       0 L           7    0      0    0          0
#    12   3712     6    51 I       0 L           2    0      0    0          0

# Do nothing if the current -lruskips is higher than MinSkips.
# Otherwise set it to MaxSkips for a short period of time.
# The higher the current lruskips the longer the script will work:
# Approximately 1 sec per 1000 skips.

  PROSHUT=${PROSHUT:-$DLC/bin/_mprshut}

# Get the current value of the -lruskips:
  LruSkips=`
   (echo "R&D"     # Advanced options
    echo "4"       # 4. Administrative Functions ...
    echo "4"       # 4. Adjust Latch Options
                   # 4. Adjust LRU force skips: 0
   ) | \
    $PROSHUT $DB -0 -NL 2>/dev/null | tr -d "\f" | \
    awk '/Adjust LRU force skips:/ {print $NF}'
  ` # LruSkips

  test "$LruSkips" || \
  return

  echoT "LruShot -lruskips $LruSkips on $UL$DB$UU"

  test $LruSkips -le $MinSkips && \
# echo Reading ACO blocks in loop... && \
# time \
 (
# Set the -lruskips to MaxSkips:
  echo "R&D"       # Advanced options
  echo "4"         # 4. Administrative Functions ...
  echo "4"         # 4. Adjust Latch Options
  echo "4"         # 4. Adjust LRU force skips:
  echo "$MaxSkips" # Enter new LRU force skips value

# Read ACO blocks = Update activity counters:
  echo "T"         # Return to the top level (main) menu.
  echo "2"         # 2. Activity Displays ...
  echo "9"         # 9. I/O Operations by File

  MinSkips=$LruSkips
  while [ $MinSkips -ge 0 ]
  do
    MinSkips=`expr $MinSkips - 1`
    echo "U"       # Update activity counters.
  done

# Reset the -lruskips to its initial value:
  echo "T"         # Return to the top level (main) menu.
  echo "4"         # 4. Administrative Functions ...
  echo "4"         # 4. Adjust Latch Options
  echo "4"         # 4. Adjust LRU force skips:
  echo $LruSkips   # Enter new LRU force skips value
  echo "X"         # Exit from the OpenEdge Monitor utility.
 ) | \
 $PROSHUT $DB -0 -NL >/dev/null 2>&1

} # LruShot()


#------------------------------------------------------------------------------
#
MainPromon()
{
# Run promon to gather the main database statistics.

  DB=$1
  MonIntrv=$2
  MonCount=$3
  Option=$4  # with/without -F

  Log=${DbLogPrefix:-`basename $0`.$$.`basename $DB`}.promon.log

  RunTime=`expr $MonIntrv \* $MonCount`

# Adjust MonIntrv and MonCount
# because promon uses a sample interval that is multiple of two:
#
  MonIntrv=`expr $MonIntrv + 1`
  MonIntrv=`expr $MonIntrv / 2`
  MonIntrv=`expr $MonIntrv \* 2`

  test $MonIntrv -lt 2 &&
  MonIntrv=2

  MonCount=`expr $RunTime + 1` # just for a rounding up
  MonCount=`expr $MonCount / $MonIntrv`

# Total time to run promon:
#
  RunTime=`expr $MonIntrv \* $MonCount`

  PROSHUT=${PROSHUT:-$DLC/bin/_mprshut}
# ECHO="echo -e" - Enable interpretation of backslash escapes.
  case ${OSNAME:-`uname`} in
   Linux) ECHO="echo -e";;
       *) ECHO="echo";;
  esac

  Count=100
  while [ $Count -gt 0 ]; do
    Count=`expr $Count - 1`
    MANY="$MANY\n"
  done

#
# Create a header of promon.log:
#
  echo "$THIS Release $Release
Main promon $Option
   DB: $DB
 Date: `date '+%a %b %e %T %Y'`
 Host: `uname -a`
Intrv: $MonIntrv sec
Count: $MonCount
Total: $RunTime sec
." >$Log

#
# The changes in promon's menus by Progress versions
# 
# Introduced in  Menu          Name
# -------------  ------------  ----------------------------------
# 6.3            R&D/debghb/6  This menu is not here Menu
# 8.3A           R&D/6/9       TXE Lock Activity (two grant limits only)
# 8.3A           R&D/6/10      Adjust TXE Options
# 8.3A           R&D/6/15      Semaphores (see the -semsets)
# 9.0A           R&D/6/15      Buffer Lock Queue
# 9.1B           R&D/6/17      Global Transactions (Removed in 10.1A)
# 9.1E, 10.0B           5      Activity (New feature: *** Quiet Point is Active ***)
# 9.1E, 10.1C    R&D/4/7       Terminate a Server (9.1E: _mprosrv, 10.1C: _sqlsrv2)
# 9.1E04,10.0B03 R&D/6/10/3    TXE commit lock skip limit (see the -TXESkipLimit introduced in 10.1B)
# 10.0A          R&D/4/6       Restricted Options (+ new options in 9.1D-10.1C01)
# 10.0A          R&D/5/7       Change working area
# 10.1A                 J      Resolve JTA Transactions
# 10.1A          R&D/1/15      AI Extents
# 10.1A02        R&D/1/16      Database Service Manager
# 10.1B          R&D/1/17      Servers By Broker
# 10.1B          R&D/3/5       I/O Operations by User by Table
# 10.1B          R&D/3/6       I/O Operations by User by Index
# 10.2B          R&D/4/6       Restricted Options -> Turning on/off inservice check
# 10.1B02        R&D/3/7       Total Locks per User
# 10.1B02        R&D/4/8       Enable/Disable block level consistency check (see the -*Check parameters)
# 10.2B06        R&D/4/7       Server Options
# 10.2B06,11.2.0 R&D/1/16      Database Service Manager -> Used HighWater Mark, Area Filled Count, Access Count, Access Collisions
# 10.1C          R&D/1/18      Client Database-Request Statement Cache ...
# 10.1C          R&D/4/4/??    Record Free Chain Search Depth Factor (see the -recspacesearchdepth)
# 10.2A01   promon logs its login/logout into database log file
# 10.2B04        R&D/1/19      Schema Locks & Wait Queue (see the -schlockwq)
# 10.2B05        R&D/4/4/??    Enable LRU2 alternate buffer pool replacement policy
# 10.2B05        R&D/1/16      Database Service Manager -> Service Latch Holder
# 10.2B06        R&D/4/4/??    Adjust LRU force skips (see the -lruskips)
# 10.2B06        R&D/4/4/??    Adjust LRU2 force skips (see the -lru2skips)
# 10.2B08,11.3.3 R&D/1/18/??   Database statement cache locks
# 11.0.0                9      Currently Connected Tenants
# 11.0.0         R&D/1/4/8     Connected Tenants
# 11.0.0         R&D/1/18/9    Toggle between User and Tenant Options
# 11.0.0         R&D/4/9       Shutdown Database
# 11.0.0         R&D/4/10      Disconnect a tenant's users
# 11.1.0         R&D/4/11      Adjust Governor options
# 11.0.0         R&D/5/8       Sort user lists
# 11.0.0         R&D/5/9       Tenant filter for user lists
# 11.3.3         R&D/6/10/4    TXE lock retry limit (see the -TXERetryLimit)
# 11.5.0         R&D/1/20      Broker Startup Parameters
# 11.6.3   bug   R&D/1/14      Shared Memory Segments (PSC00356177: Memory violation (49) when the debghb is enabled. Fixed in 11.7.1)
# 11.7.0         R&D/1/4/9     User Notification Processes
# 11.7.0         R&D/4/12      Toggle redundantlockdiag setting (for use when directed by Progress Technical Support only)
# 11.7.0         R&D/4/13      Caches
# 11.7.2         R&D/4/13      Caches (PSC00363832: Memory violation when running against a replication target)
# 11.7.0         R&D/4/14      Diagnostic Data Collection
# 11.7.0         R&D/6/18      CDC Cache Info
# 11.7.0         R&D/4/7/8     User notify time (see the -usernotifytime)
# 11.7.2         R&D/4/7/9     Limit .lg message payload (see the -limitLgPayload)
# 
# 
# "??" means renumbering:
# 
# 8.2A->8.3A    R&D/6/9 ->11   Latch Counts
# 8.2A->8.3A    R&D/6/10->12   Latch Times
# 8.2A->8.3A    R&D/6/11->13   I/O Wait Time by Type
# 8.2A->8.3A    R&D/6/12->14   I/O by File
# 8.2A->8.3A    R&D/6/13->16   Shutdown
# 8.3A->9.1B    R&D/6/16->18   Shutdown
# 10.0A->10.1C  R&D/6/16->17   Shutdown
# 10.2B->11.3.3 R&D/1/18/9->10 Database statement cache locks
# debghb        R&D/4/4/2->6   Record Free Chain Search Depth Factor
# debghb        R&D/4/4/3->7   Enable LRU2 alternate buffer pool replacement policy
# debghb        R&D/4/4/4->8   Adjust LRU force skips
# debghb        R&D/4/4/5->9   Adjust LRU2 force skips
# 
# Beginning of promon input sequences:
(
#
# Menus to enter only once (Q = "quit" = "return to a previous level menu")

#  M.  Modify Defaults     ---------------------------

  $ECHO "M\n1\n9999\nQ"     # M.  Modify Defaults: Page size: 99999

  $ECHO "1\n1\nQ"           # 1.  User Control 1.  Display all entries
# $ECHO "2\n1\nQ"           # 2.  Locking and Waiting Statistics
# $ECHO "3\n1\nQ"           # 3.  Block Access
# $ECHO "4\n1\nQ"           # 4.  Record Locking Table
# $ECHO "5\nQ"              # 5.  Activity
  $ECHO "6\nQ"              # 6.  Shared Resources
  $ECHO "7\nQ"              # 7.  Database Status
# $ECHO "9\nQ"              # 9.  Currently Connected Tenants
# $ECHO "T\nQ"              # T.  2PC Transactions Control
# $ECHO "L\nQ"              # L.  Resolve 2PC Limbo Transactions
# $ECHO "C\nQ"              # C.  2PC Coordinator Information
# $ECHO "J\nQ"              # J.  Resolve JTA Transactions

  $ECHO "R&D"               # R&D.  Advanced options

# 1. Status Displays ...
# 2. Activity Displays ...
# 3. Other Displays ...
# 4. Administrative Functions ...
# 5. Adjust Monitor Options

# 5. Adjust Monitor Options  ---------------------------

# 1. Display page length           : 24 lines
# 2. Clear screen for first page   : Yes
# 3. Monitor sampling interval     : 10 sec.
# 4. Pause between displays        : 10 sec.
# 5. Pause between screens         : 5 sec.
# 6. Number of auto repeats        : 9999
# 7. Change working area           : All areas
# 8. Sort user lists               : by user number
# 9. Tenant filter for user lists  : will display all tenants

  $ECHO "5\n1\n9999"        # 5/1. Display page length
  $ECHO "3\n$MonIntrv"      # 5/3. Monitor sampling interval
  $ECHO "7\n3"              # 5/7. Change working area => 3. Primary Recovery Area
#
# Every time when promon updates Activity counters it reads the object blocks of the working areas.
# If the related resources are locked by another process then promon will hang.
# Primary Recovery Area does not have an object block. 
# Choosing it as the working area decreases the resource contention for promon.
#

#  1. Status Displays ...  ---------------------------

#  T - Return to the top level menu.

# Uncomment menus that should be used only once (at dbmon startup):

# $ECHO "T\n1\n1\nP"        #  1. Status: Database
  $ECHO "T\n1\n2\nP"        #  2. Status: Backup
# $ECHO "T\n1\n3\nP"        #  3. Status: Servers
#                              4. Processes/Clients
# $ECHO "T\n1\n4\n1\nP"     #  4/1. All Processes
# $ECHO "T\n1\n4\n2\nP"     #  4/2. Blocked Clients
# $ECHO "T\n1\n4\n3\nP"     #  4/3. Active Transactions
# $ECHO "T\n1\n4\n4\nP"     #  4/4. Local Clients
# $ECHO "T\n1\n4\n5\nP"     #  4/5. Batch Clients
# $ECHO "T\n1\n4\n6\nP"     #  4/6. Remote Clients
# $ECHO "T\n1\n4\n7\nP"     #  4/7. Background Processes
# $ECHO "T\n1\n4\n8\nP"     #  4/8. Connected Tenants                          11.0.0
# $ECHO "T\n1\n4\n9\nP"     #  4/9. User Notification Processes                11.7.0
#
# $ECHO "T\n1\n5\nP"        #  5. Status: Files
# $ECHO "T\n1\n6\n1\nP"     #  6. Status: Lock Table
# $ECHO "T\n1\n7\nP"        #  7. Status: Buffer Cache
  $ECHO "T\n1\n8\nP"        #  8. Status: Logging Summary
# $ECHO "T\n1\n9\nP"        #  9. Status: BI Log
# $ECHO "T\n1\n10\nP"       # 10. Status: AI Log
# $ECHO "T\n1\n11\nP"       # 11. Status: Two-Phase Commit
  $ECHO "T\n1\n12\nP"       # 12. Status: Startup Parameters
# $ECHO "T\n1\n13\nP"       # 13. Status: Shared Resources
  $ECHO "T\n1\n14\nP"       # 14. Status: Shared Memory Segments
# $ECHO "T\n1\n15\nP"       # 15. Status: AI Extents                           10.1A
# $ECHO "T\n1\n16\nP"       # 16. Status: Database Service Manager             10.1A02
# $ECHO "T\n1\n17\nP"       # 17. Status: Servers By Broker                    10.1B
#                            18. Client Database-Request Statement Cache ...   10.1C
# $ECHO "T\n1\n18\n1\nQ"    # 18/1. Activate For Selected Users
# $ECHO "T\n1\n18\n2\nQ"    # 18/2. Activate For All Users
# $ECHO "T\n1\n18\n3\nQ"    # 18/3. Activate For All Future Users
# $ECHO "T\n1\n18\n4\nP"    # 18/4. Deactivate For Selected Users
# $ECHO "T\n1\n18\n5"       # 18/5. Deactivate For All Users
# $ECHO "T\n1\n18\n6"       # 18/6. Deactivate For All Future Users
# $ECHO "T\n1\n18\n7\nP"    # 18/7. View Database-Request Statement Cache
# test "$StCaDir" && test -d $StCaDir && \
# $ECHO "T\n1\n18\n8\n$StCaDir" # 18/8. Specify Directory for Statement Cache Files
  $ECHO "T\n1\n18\n8\n"         # 18/8. Specify Directory for Statement Cache Files
# If it was previously set by promon then promon displays:
# Current Client Statement Cache Temporary Directory:
# /path/to/dir
# $ECHO "T\n1\n18\n9"       # 18/9. Toggle between User and Tenant Options     11.0.0
# $ECHO "T\n1\n18\n9\nP"    # 18/9.  Database statement cache locks            10.2B08
# $ECHO "T\n1\n18\n10\nP"   # 18/10. Database statement cache locks            11.3.3
#
# $ECHO "T\n1\n19\nP"       # 19. Status: Schema Locks & Wait Queue            10.2B04
# $ECHO "T\n1\n20\n\n\n"    # 20. Status: Broker Startup Parameters            11.5.0
#                 ^^^^^  only for first 4 login brokers

#  2. Activity Displays ... ---------------------------

# $ECHO "T\n2\n1\nP"        #  1. Activity: Summary
# $ECHO "T\n2\n2${MANY}"    #  2. Activity: Servers
# $ECHO "T\n2\n3\nP"        #  3. Activity: Buffer Cache
# $ECHO "T\n2\n4\nP"        #  4. Activity: Page Writers
# $ECHO "T\n2\n5\nP"        #  5. Activity: BI Log
# $ECHO "T\n2\n6\nP"        #  6. Activity: AI Log
# $ECHO "T\n2\n7\n\nP"      #  7. Activity: Lock Table - needs extra "\n" to display "Waits:" (before 10.2B08?)
# $ECHO "T\n2\n8\nP"        #  8. Activity: I/O Operations by Type
# $ECHO "T\n2\n9${MANY}"    #  9. Activity: I/O Operations by File
# $ECHO "T\n2\n10\nP"       # 10. Activity: Space Allocation
# $ECHO "T\n2\n11\nP"       # 11. Activity: Index
# $ECHO "T\n2\n12\nP"       # 12. Activity: Record
# $ECHO "T\n2\n13\nP"       # 13. Activity: Other

#  3. Other Displays ...   ---------------------------

# $ECHO "T\n3\n1\nP"        # 1. Performance Indicators
  $ECHO "T\n3\n2\nP"        # 2. I/O Operations by Process
# $ECHO "T\n3\n3\nP"        # 3. Lock Requests By User
# $ECHO "T\n3\n4\nP"        # 4. Checkpoints
# $ECHO "T\n3\n5\nP"        # 5. I/O Operations by User by Table               10.1B
# $ECHO "T\n3\n6\nP"        # 6. I/O Operations by User by Index               10.1B
# $ECHO "T\n3\n7\nP"        # 7. Total Locks per User                          10.1B02

# PSC00356177
# =============================
# Exiting PROMON after running the R&D option #14 to display shared memory
# segments when the "debghb" debug option was enabled will result in a Memory
# Violation (49) error.  This in turn causes the database to shutdown due to
# promon exiting while holding the USR latch (latch #2). This problem was
# introduced in the OE 11.6.3 service pack.
# Fixed in 11.7.1

  $ECHO "T\ndebghb"         # Enabling This menu is not here Menu

#  6. This menu is not here ------------------------

# $ECHO "T\n6\n1\nP"        #  1. Status: Cache Entries
# $ECHO "T\n6\n2\nP"        #  2. Status: Hash Chain
# $ECHO "T\n6\n3\nP"        #  3. Status: Page Writer Queue
# $ECHO "T\n6\n4\nP"        #  4. Status: Lru Chains
# $ECHO "T\n6\n5\nP"        #  5. Status: Locked Buffers
# $ECHO "T\n6\n6\nP"        #  6. Status: Buffer Locks
# $ECHO "T\n6\n7\nP"        #  7. Status: Buffer Use Counts
# $ECHO "T\n6\n8\nP"        #  8. Activity: Resource Queues
# $ECHO "T\n6\n9\nP"        #  9. Activity: TXE Lock Activity                  8.3A
# $ECHO "T\n6\n10\nP"       # 10. Adjust TXE Options                           8.3A
# $ECHO "T\n6\n11\nP"       # 11. Activity: Latch Counts # was R&D/6/9  before 8.3A
# $ECHO "T\n6\n12\nP"       # 12. Activity: Latch Times  # was R&D/6/10 before 8.3A
# $ECHO "T\n6\n13\nP"       # 13. Activity: I/O Wait Time by Type #  11 before 8.3A
# $ECHO "T\n6\n14\nP"       # 14. Activity: I/O by File  # was R&D/6/12 before 8.3A
# $ECHO "T\n6\n15\nP"       # 15. Status: Buffer Lock Queue                    9.0A
  $ECHO "T\n6\n16\nP"       # 16. Semaphores                                   8.3A
# $ECHO "T\n6\n18\nP"       # 18. CDC Cache Info                               11.7.0

# Menus to enter once per sample interval:

  while [ "sampling" ]
  do

#  1. Status Displays ...  ---------------------------

# Uncomment menus that should be used per each snapshot:

    $ECHO "T\n1\n1\nP"      #  1. Status: Database
#   $ECHO "T\n1\n2\nP"      #  2. Status: Backup
#   $ECHO "T\n1\n3\nP"      #  3. Status: Servers
#                              4. Processes/Clients
#   $ECHO "T\n1\n4\n1\nP"   #  4/1. All Processes
    $ECHO "T\n1\n4\n2\nP"   #  4/2. Blocked Clients
#
# Display the list of active transactions only if
# the monitoring duration is longer than LongRun.
# Otherwise this information will be reported by StatusPMon promon:
    test "$ShortRun" = "yes" || \
    $ECHO "T\n1\n4\n3\nP"   #  4/3. Active Transactions
#   $ECHO "T\n1\n4\n4\nP"   #  4/4. Local Clients
#   $ECHO "T\n1\n4\n5\nP"   #  4/5. Batch Clients
#   $ECHO "T\n1\n4\n6\nP"   #  4/6. Remote Clients
#   $ECHO "T\n1\n4\n7\nP"   #  4/7. Background Processes
    $ECHO "T\n1\n4\n8\nP"   #  4/8. Connected Tenants                          11.0.0
    $ECHO "T\n1\n4\n9\nP"   #  4/9. User Notification Processes                11.7.0
#
#   $ECHO "T\n1\n5\nP"      #  5. Status: Files
#   $ECHO "T\n1\n6\n1\nP"   #  6. Status: Lock Table
    $ECHO "T\n1\n7\nP"      #  7. Status: Buffer Cache
#   $ECHO "T\n1\n8\nP"      #  8. Status: Logging Summary
    $ECHO "T\n1\n9\nP"      #  9. Status: BI Log
    $ECHO "T\n1\n10\nP"     # 10. Status: AI Log
#   $ECHO "T\n1\n11\nP"     # 11. Status: Two-Phase Commit
#   $ECHO "T\n1\n12\nP"     # 12. Status: Startup Parameters
    $ECHO "T\n1\n13\nP"     # 13. Status: Shared Resources
#   $ECHO "T\n1\n14\nP"     # 14. Status: Shared Memory Segments
#   $ECHO "T\n1\n15\nP"     # 15. Status: AI Extents                           10.1A
    $ECHO "T\n1\n16\nP"     # 16. Status: Database Service Manager             10.1A02
    $ECHO "T\n1\n17\nP"     # 17. Status: Servers By Broker                    10.1B
#                            18. Client Database-Request Statement Cache ...   10.1C
#   $ECHO "T\n1\n18\n1\nQ"  # 18/1. Activate For Selected Users
#   $ECHO "T\n1\n18\n2\nQ"  # 18/2. Activate For All Users
#   $ECHO "T\n1\n18\n3\nQ"  # 18/3. Activate For All Future Users
#   $ECHO "T\n1\n18\n4\n"   # 18/4. Deactivate For Selected Users
#   $ECHO "T\n1\n18\n5"     # 18/5. Deactivate For All Users
#   $ECHO "T\n1\n18\n6"     # 18/6. Deactivate For All Future Users
    $ECHO "T\n1\n18\n7\nP"  # 18/7. View Database-Request Statement Cache
#   $ECHO "T\n1\n18\n8\n"   # 18/8. Specify Directory for Statement Cache Files
#             Promon does not allow to reset Directory for Statement Cache Files.
#   $ECHO "T\n1\n18\n9"     # 18/9. Toggle between User and Tenant Options     11.0.0
#   $ECHO "T\n1\n18\n10\nP" # 18/9.  Database statement cache locks            10.2B08
#   $ECHO "T\n1\n18\n10\nP" # 18/10. Database statement cache locks            11.3.3
#
    $ECHO "T\n1\n19\nP"     # 19. Status: Schema Locks & Wait Queue            10.2B04
#   $ECHO "T\n1\n20\n\n\n"  # 20. Status: Broker Startup Parameters            11.5.0

#  2. Activity Displays ... ---------------------------

    $ECHO "T\n2\n1\nP"      #  1. Activity: Summary
    $ECHO "T\n2\n2${MANY}"  #  2. Activity: Servers
    $ECHO "T\n2\n3\nP"      #  3. Activity: Buffer Cache
    $ECHO "T\n2\n4\nP"      #  4. Activity: Page Writers
    $ECHO "T\n2\n5\nP"      #  5. Activity: BI Log
    $ECHO "T\n2\n6\nP"      #  6. Activity: AI Log
    $ECHO "T\n2\n7\n\nP"    #  7. Activity: Lock Table - needs extra "\n" to display "Waits:" (before 10.2B08?)
    $ECHO "T\n2\n8\nP"      #  8. Activity: I/O Operations by Type
    $ECHO "T\n2\n9${MANY}"  #  9. Activity: I/O Operations by File
    $ECHO "T\n2\n10\nP"     # 10. Activity: Space Allocation
    $ECHO "T\n2\n11\nP"     # 11. Activity: Index
    $ECHO "T\n2\n12\nP"     # 12. Activity: Record
    $ECHO "T\n2\n13\nP"     # 13. Activity: Other

#  3. Other Displays ...   ---------------------------

    $ECHO "T\n3\n1\nP"      # 1. Performance Indicators
#   $ECHO "T\n3\n2\nP"      # 2. I/O Operations by Process
#   $ECHO "T\n3\n3\nP"      # 3. Lock Requests By User
    $ECHO "T\n3\n4\nP"      # 4. Checkpoints
#   $ECHO "T\n3\n5\nP"      # 5. I/O Operations by User by Table               10.1B
#   $ECHO "T\n3\n6\nP"      # 6. I/O Operations by User by Index               10.1B
#   $ECHO "T\n3\n7\nP"      # 7. Total Locks per User                          10.1B02

#  4. Administrative Functions ...  ---------------------------

#   $ECHO "T\n4\n1\nP"      #  1. Check Active Transaction Status
#   $ECHO "T\n4\n2\nQ"      #  2. Check Two-Phase Transactions
#   $ECHO "T\n4\n3\nQ"      #  3. Resolve Limbo Transactions
    $ECHO "T\n4\n4\nP"      #  4. Adjust Latch Options
# Latch Options cannot be adjusted without an Enterprise RDBMS License.
#   $ECHO "T\n4\n5\nP"      #  5. Adjust Page Writer Options
#   $ECHO "T\n4\n6\n..."    #  6. Restricted Options
    $ECHO "T\n4\n7\nP"      #  7. Server Options                               10.2B06
    $ECHO "T\n4\n8\nP"      #  8. Enable/Disable block level consistency check 10.1B02
    $ECHO "T\n4\n11\nP"     # 11. Adjust Governor options                      11.1.0
#   $ECHO "T\n4\n12\n"      # 12. Toggle redundantlockdiag setting             11.1.0
#   $ECHO "T\n4\n13\nP"     # 13. Caches                                       11.1.0
# 11.7.2 PSC00363832: PROMON Caches, crashes when run against a target replication enabled database
#   $ECHO "T\n4\n14\nP"     # 14. Diagnostic Data Collection                   11.1.0

#  6. This menu is not here ------------------------

#   $ECHO "T\n6\n1\nP"      #  1. Status: Cache Entries
#   $ECHO "T\n6\n2\nP"      #  2. Status: Hash Chain
#   $ECHO "T\n6\n3\nP"      #  3. Status: Page Writer Queue
#   $ECHO "T\n6\n4\nP"      #  4. Status: Lru Chains
#   $ECHO "T\n6\n5\nP"      #  5. Status: Locked Buffers
#   $ECHO "T\n6\n6\nP"      #  6. Status: Buffer Locks
#   $ECHO "T\n6\n7\nP"      #  7. Status: Buffer Use Counts
    $ECHO "T\n6\n8\nP"      #  8. Activity: Resource Queues
    $ECHO "T\n6\n9\nP"      #  9. Activity: TXE Lock Activity                  8.3A
    $ECHO "T\n6\n10\nP"     # 10. Adjust TXE Options                           8.3A
    $ECHO "T\n6\n11\nP"     # 11. Activity: Latch Counts # was R&D/6/9  before 8.3A
#   $ECHO "T\n6\n12\nP"     # 12. Activity: Latch Times  # was R&D/6/10 before 8.3A
#   $ECHO "T\n6\n13\nP"     # 13. Activity: I/O Wait Time by Type #  11 before 8.3A
#   $ECHO "T\n6\n14\nP"     # 14. Activity: I/O by File  # was R&D/6/12 before 8.3A
#   $ECHO "T\n6\n15\nP"     # 15. Status: Buffer Lock Queue                    9.0A
#   $ECHO "T\n6\n16\nP"     # 16. Semaphores                                   8.3A
#   $ECHO "T\n6\n18\nP"     # 18. CDC Cache Info                               11.7.0

    if [ $MonCount -gt 0 ]
    then
# Useless statistics for .db file. Just to be on activity screen.
      $ECHO "T\n2\n9"      # Activity: I/O Operations by File
      $ECHO "S"            # Sample activity counters
    else

# See PSC00356177 above.     So just in case:
      $ECHO "T\ndebghb"    # Disabling This menu is not here Menu
      $ECHO "X"            # Exit from the OpenEdge Monitor utility.

      break # while sampling
    fi # if MonCount -gt 0

    MonCount=`expr $MonCount - 1`

  done # while "sampling"

# End of promon input sequences
) | \
  $PROSHUT $DB -0 -NL -zp $Option 2>/dev/null | \
  tr -d "\f" >>$Log 2>&1 &

  echo "$!|$PROSHUT|MainPromon|$DB|$Option" >>$MyProcList

} # MainPromon()


#------------------------------------------------------------------------------
#
LatchPMon()
{
# Creares a log of the latch owners:
# promon/R&D/debghb/6/11. Activity: Latch Counts

  DB=$1
  MonIntrv=$2
  MonCount=$3
  Option=$4

  Log=${DbLogPrefix:-`basename $0`.$$.`basename $DB`}.latches.log

# Check promon every second no matter what MonIntrv says:

  MonCount=`expr $MonIntrv \* $MonCount`
  MonIntrv=1

  PROSHUT=${PROSHUT:-$DLC/bin/_mprshut}
# ECHO="echo -e" - Enable interpretation of backslash escapes.
  case ${OSNAME:-`uname`} in
   Linux) ECHO="echo -e";;
       *) ECHO="echo";;
  esac

  Date=`date '+%m/%d/%y'`
  Time=`date '+%H:%M:%S'`

# Create a log header:
  echo "$THIS Release $Release
Latch promon (LatchPMon) $Option
   DB: $DB
 Date: `date '+%a %b %e %T %Y'`
 Host: `uname -a`
Intrv: $MonIntrv sec
Count: $MonCount
.
$Date        Activity: Latch Counts" >$Log

#
# Start promon using [Z]ero and [U]pdate the activity counters:
#
 ($ECHO "R&D\ndebghb"
  $ECHO "5\n1\n9999"   # Adjust Monitor Options: Display page length

  while [ $MonCount -gt 0 ]
  do
    MonCount=`expr $MonCount - 1`
    $ECHO "T\n2\n9\nZ" # Zero the activity counters.
    sleep $MonIntrv
    $ECHO "U"          # Update activity counters.
    $ECHO "T\n6\n11"   # Activity: Latch Counts
  done

  $ECHO "T\ndebghb"    # Disabling This menu is not here Menu
  $ECHO "X"            # Exit from the OpenEdge Monitor utility.
 ) | \
  $PROSHUT $DB -0 -NL $Option 2>/dev/null | \
  tr -d "\f" | \
  awk '
    BEGIN {
      Sample=0
      Date="'$Date'"
      Time="'$Time'"
      Intrv='$MonIntrv'
      CurrSecond=substr(Time,1,2)*3600
      CurrSecond+=substr(Time,4,2)*60
      CurrSecond+=substr(Time,7,2)
print "Sample Date     Time   Latch Owner   Locks/Sec Naps/Sec Interval"
printf"%6d %8s %8s %-3s%6s%12d%9d%9d\n",Sample,Date,Time,"bgn","--",0,0,Intrv
    } # BEGIN

    /Activity: Latch Counts/ {
      Sample++
      Date=$1
      getline
      Time=$1
      PrevSecond=CurrSecond
      CurrSecond=substr(Time,1,2)*3600
      CurrSecond+=substr(Time,4,2)*60
      CurrSecond+=substr(Time,7,2)
      Intrv=CurrSecond-PrevSecond
      if(Intrv<0) Intrv+=86400
      next
    } # Activity: Latch Counts

# 04/03/13        Activity: Latch Counts
# 11:55:21        04/03/13 11:55 to 04/03/13 11:55 (10 sec)
#                 ----- Locks -----        ------ Busy ------        Naps
#    Owner        Total        /Sec        /Sec           Pct        /Sec
#
# LRU  836      1424651      142465           0           0.0         128
# BUF  --        782529       78252           0           0.0           0

    NF==12 && $2!="--" {    # $1=Name, $2=Owner, $4=Locks/Sec, $7=Naps/Sec
printf"%6d %8s %8s %-3s%6s%12d%9d%9d\n",Sample,Date,Time,$1,$2,$4,$7,Intrv
    } # Owner != "--"

    END {
printf"%6d %8s %8s %-3s%6s%12d%9d%9d\n",Sample,Date,Time,"end","--",0,0,Intrv
    } # END
  ' >>$Log 2>&1 &

  echo "$!|$PROSHUT|LatchPMon|$DB" >>$MyProcList

} # LatchPMon()


#------------------------------------------------------------------------------
#
LatchInfo()
{
# Report the detailed information about all latches:
# promon/R&D/4/6. Restricted Options: 8j2hhs7gio

  DB=$1
  MonIntrv=$2
  MonCount=$3
  Option=$4

  Log=${DbLogPrefix:-`basename $0`.$$.`basename $DB`}.latches.detailed.log

# Check promon every second no matter what MonIntrv says:

  MonCount=`expr $MonIntrv \* $MonCount`
  MonIntrv=1

  PROSHUT=${PROSHUT:-$DLC/bin/_mprshut}
# ECHO="echo -e" - Enable interpretation of backslash escapes.
  case ${OSNAME:-`uname`} in
   Linux) ECHO="echo -e";;
       *) ECHO="echo";;
  esac

# Create a log header:
  echo "$THIS Release $Release
Latch promon (LatchInfo) $Option
   DB: $DB
 Date: `date '+%a %b %e %T %Y'`
 Host: `uname -a`
Intrv: $MonIntrv sec
Count: $MonCount
." >$Log

  Date=`date '+%m/%d/%y'`
  Time=`date '+%H:%M:%S'`

#
# Start promon:
#
 ($ECHO "R&D\n4"        # 4. Administrative Functions ...
  $ECHO "6\n8j2hhs7gio" # Detailed information about all latches

  while [ $MonCount -gt 0 ]
  do
    MonCount=`expr $MonCount - 1`
    sleep $MonIntrv
    $ECHO "6\n8j2hhs7gio" # Detailed information about all latches
  done

  $ECHO "X"            # Exit from the OpenEdge Monitor utility.
 ) | \
  $PROSHUT $DB -0 -NL $Option 2>/dev/null | \
  tr -d "\f" | \
  awk '
    BEGIN {
      Sample=0
      for(Id=0;Id<=31;Id++) Saved[Id]=""
      PrintTitle="yes"
    } # BEGIN

# 14:09:21        Restricted Options
    /Restricted Options/ {
      Sample++
      Time=$1
      next
    } # Time

# restricted option key: 8j2hhs7gio
# Latch 0 (L00):
#      lp:        (00000000023D1600)
#      fastlock   (00000000023D1600): 0
#      slowLock   (00000000023D1604): 0
#      latchId    (00000000023D1608): 0
#      holder     (00000000023D160A): -1
#      qholder    (00000000023D160C): -1
#      ltype      (00000000023D160E): 0
#      qhead      (00000000023D1610): 0
#      qtail      (00000000023D1618): 0
#      mask       (00000000023D1628): 4294967296.00  <--- Alignment shift: 8
#      lockCnt    (00000000023D1620): 0           <--- Alignment shift: -4
#      busyCnt    (00000000023D1630): 0           <--- Alignment shift: 4
#      spinCnt    (00000000023D162C): 0           <--- Alignment shift: -8
#      waitCnt    (00000000023D1638): 0
#      lockedTime (00000000023D1640): 0
#      lockTime   (00000000023D1644): 0
#      waitTime   (00000000023D1648): 0
#      napMaxCnt   (00000000023D1650): 0           <--- Alignment shift: 4
#      napHWM     (00000000023D1658): 0  <--- Alignment shift: 4
#      latchDepth (00000000023D165A): 0  <--- Alignment shift: 4

    $1 == "Latch" {
      Id=$2
      Value=substr($3,2,3)
      Title="Nm"
      next
    } # Latch header

    $1 == "latchId" {next}

    $1 == "mask" {
      Mask=$3
      next
    } # "mask" is a field with long value

    NF >= 3 {
      Value=Value " " $3
      Title=Title " " $1
    } # Value

    $1 == "latchDepth" {
      if(Value == Saved[Id]) next

      if(PrintTitle == "yes") {
        n=split(Title,Arr)
        printf"%6s %8s %2s %3s","Sample","Time","Id","Nm"
        for(i=2;i<=n;i++) printf" %8s",substr(Arr[i],1,8)
        printf" %9s\n","mask"
        PrintTitle="no"
      } # if PrintTitle

      n=split(Value,Arr)
      printf"%6d %8s %2d %3s",Sample,Time,Id,Arr[1]
      for(i=2;i<=n;i++) printf" %8d",Arr[i]
      printf" %9d\n",Mask
      Saved[Id]=Value
    } # "latchDepth" is the last field
  ' >>$Log 2>&1 &

  echo "$!|$PROSHUT|LatchInfo|$DB" >>$MyProcList

} # LatchInfo()


#------------------------------------------------------------------------------
#
StatusPMon()
{
# Run promon to gather the status information:
# Status: Blocked Clients
# Status: Active Transactions
# Status: Buffer Lock Queue

# Update status informaton once per MaxIntrv seconds or less:
  MaxIntrv=10 # sec

  DB=$1
  MonIntrv=$2
  MonCount=$3
  Option=$4

  Prefix=${DbLogPrefix:-`basename $0`.$$.`basename $DB`}.status

  RunTime=`expr $MonIntrv \* $MonCount`

  test $MonIntrv -gt $MaxIntrv && \
  MonIntrv=$MaxIntrv

# Adjust MonIntrv and MonCount
# because promon uses a sample interval that is multiple of two:
#
  MonIntrv=`expr $MonIntrv + 1`
  MonIntrv=`expr $MonIntrv / 2`
  MonIntrv=`expr $MonIntrv \* 2`

  test $MonIntrv -lt 2 &&
  MonIntrv=2

  MonCount=`expr $RunTime + 1` # just for a rounding up
  MonCount=`expr $MonCount / $MonIntrv`

# Total time to run promon:
#
  RunTime=`expr $MonIntrv \* $MonCount`

  PROSHUT=${PROSHUT:-$DLC/bin/_mprshut}
# ECHO="echo -e" - Enable interpretation of backslash escapes.
  case ${OSNAME:-`uname`} in
   Linux) ECHO="echo -e";;
       *) ECHO="echo";;
  esac

  Date=`date '+%m/%d/%y'`
  Time=`date '+%H:%M:%S'`

  StartupTime=`date '+%a %b %e %T %Y'`
  Host=`uname -a`

(
  $ECHO "R&D\ndebghb"      # R&D.  Advanced options
  $ECHO "5\n1\n9999"       # 5. Adjust Monitor Options 1. Display page length
  $ECHO "3\n$MonIntrv"     # 5. Adjust Monitor Options 3. Monitor sampling interval

  while [ "sampling" ]
  do
    $ECHO "T\n1\n4\n2"     #  4. Processes/Clients 2. Blocked Clients
    $ECHO "T\n1\n4\n3"     #  4. Processes/Clients 3. Active Transactions
    $ECHO "T\n6\n6"        #  6. Status: Buffer Locks
    $ECHO "T\n6\n15"       # 15. Status: Buffer Lock Queue

    if [ $MonCount -gt 0 ]
    then
# Useless statistics for .db file. Just to be on activity screen.
      $ECHO "T\n2\n9"      # Activity: I/O Operations by File
      $ECHO "S"            # Sample activity counters
    else
      $ECHO "T\ndebghb"    # Disabling This menu is not here Menu
      $ECHO "X"            # Exit from the OpenEdge Monitor utility.
      break # while true
    fi # if MonCount -gt 0

    MonCount=`expr $MonCount - 1`

  done # while sampling

) | \
  $PROSHUT $DB -0 -NL $Option 2>/dev/null | \
  tr -d "\f" | \
  awk "
# Examples of input streams:
#
# 03/28/16        Status: Active Transactions [by user number for all tenants]
# 12:20:09
#   Usr Name     Type       Login time     Tx start time  Trans id Trans State
#  2710 USER1    SELF/ABL  03/28/16 12:20 03/28/16 12:20 2062034534  Active
#  3051 USER2    SELF/ABL  03/28/16 09:55 03/28/16 12:19 2062028166  Active
#  3633          SELF/ABL  03/28/16 12:20 -              2062034633  Begin
#
# 10/02/16        Status: Blocked Clients [by user number for all tenants]
# 17:07:53
#   Usr Name     Type      Wait        Wait Info  Trans id   Login time
#   Usr:Ten   Name      Domain     Type      Wait            Wait Info  Trans id   Num Txns   BI RRead  BI RWries  Login time     Schema Timestamp
#     7      USER1          -4   SELF/ABL    BKEX             384:8          400                                   10/02/16 13:57
#
# 5. Status: Buffer Locks scans Usrctl Table but reports only one lock per buffer.
#
# 10/02/16        Status: Buffer Locks by user number for all tenants
# 17:07:53
#        User:Ten                    DBKEY Area                 Hash T S Usect
# 1         6                          384    8                  221 D S     1
#
# 15. Status: Buffer Lock Queue scans buffer headers by hash chains
# and reports all buffer locks but it does not show the user number of
# the first user in the queue (Usr -1 issue).
#
# 10/02/16        Status: Buffer Lock Queue [by user number for all tenants]
# 17:07:53
#   User               DBKEY Area T         Status           Type     Usect
#     -1                 384    8 D         LOCKED          SHARE         1
#      7                 384    8 D        WAITING       EXCLWAIT

    \$2==\"Activity:\" {
      OutFile=\"\"
      next
    } # Activity:

    \$2==\"Status:\" {
      OutFile=\"$Prefix\"
      for(i=3;i<=NF;i++) if(\$i==\"by\") break
                         else
      OutFile=OutFile \"_\" \$i
      OutFile=OutFile \".log\"
      Title=\$0
      Date=\$1
      getline
      Time=\$1
      getline
      if(\$0==\"\") getline # Now the current line is a title
      if(Header[OutFile]!=\"yes\") {
        print \"Status promon (StatusPMon)\" >OutFile
        print \"   DB: $DB\"                 >OutFile
        print \" Date: $StartupTime\"        >OutFile
        print \" Host: $Host\"               >OutFile
        print \"Intrv: $MonIntrv sec\"       >OutFile
        print \"Count: $MonCount\"           >OutFile
        print \"Total: $RunTime sec\"        >OutFile
        print \".\"                          >OutFile
        print Title                          >OutFile
        printf\"%6s %-8s %-8s %s\n\",\"Sample\",\"Date\",\"Time\",\$0 >OutFile
        printf\"%6d %-8s %-8s %s\n\", 0,       Date,  Time,\"Begin\" >OutFile
      } # if Header
      Header[OutFile]=\"yes\"
      Sample[OutFile]++
      next
    } # Status:

    OutFile!=\"\" && length(\$0)>60 {
      printf\"%6d %8s %8s %s\n\",Sample[OutFile],Date,Time,\$0 >OutFile
    }

    END {
      for(OutFile in Header)
        printf\"%6d %-8s %-8s %s\n\",Sample[OutFile],Date,Time,\"End\" >OutFile
    } # END
  # awk
  " >>$RunLog 2>&1 &

  echo "$!|$PROSHUT|StatusPMon|$DB" >>$MyProcList

} # StatusPMon()


#------------------------------------------------------------------------------
#
DsrUtilMon()
{
# DsrUtilMon     Run dsrutil to gather the replication statistics

  DB=$1
  MonIntrv=$2
  MonCount=$3

  Prefix=${DbLogPrefix:-`basename $0`.$$.`basename $DB`}.dsrutil

# Total time to run promon:
#
  RunTime=`expr $MonIntrv \* $MonCount`

  PROUTIL=${PROUTIL:-$DLC/bin/_proutil}
  DSRHOME=${DSRHOME:-$DLC}; export DSRHOME
  DSRUTIL=${DSRUTIL:-$DSRHOME/bin/rprepl}
# ECHO="echo -e" - Enable interpretation of backslash escapes.
  case ${OSNAME:-`uname`} in
   Linux) ECHO="echo -e";;
       *) ECHO="echo";;
  esac

#
# Database Features
#    ID  Feature                           Active Details
#  ----  --------------------------------- ------ -------
#     1 OpenEdge Replication               Yes Source database
#
  Replication=`$PROUTIL $DB -C describe | grep "OpenEdge Replication"`

  case "$Replication" in
    *Source*) Option="S"; Log="$Prefix.source.log";; # S. Server status
    *Target*) Option="A"; Log="$Prefix.target.log";; # A. Agent status
           *) Option="" ; return 0;;      # Replication is not enabled.
  esac # Replication

# OpenEdge Replication Monitor Page 1
# Database: /path/to/db
# S. Replication server status
# R. Replication server remote agents
# A. Replication agent status
# I. Replication inter-agent status
# M. Modify display defaults
# Q. Quit
# Enter your selection:

# RETURN - repeat, U - continue uninterrupted, Q - quit:

# M. Modify display defaults
#     1.  Page size:                          %d
#     2.  Clear screen for first page:        %s
#     3.  Automatic display update:           %s
#     4.  Pause after each page:              %d %s
#     5.  Pause after each display:           %d %s
#     6.  Number of times to repeat display:  
#     R.  Restore defaults
#     Q.  Return
#     Enter your selection: 

# Create a log header:
  echo "$THIS Release $Release
DSRUTIL -C monitor (DsrUtilMon)
   DB: $DB
 Date: `date '+%a %b %e %T %Y'`
 Host: `uname -a`
Intrv: $MonIntrv sec
Count: $MonCount
." >$Log

# The -verbose option is available only since V11.7:
  ReturnCode=`$DSRUTIL $DB -C status -detail 2>&1`
  echo "DSRUTIL -C status -detail: $ReturnCode" >>$Log

  case "$ReturnCode" in
    1001) echo "Server to Agent: Initial Connection";;
    1002) echo "Server to Agent: Initialising";;
    1003) echo "Server to Agent: Target in Quiet Point";;
    1032) echo "Server to Agent: Initial Connection Failed";;
    1033) echo "Server to Agent: Recovery Failed";;
    1034) echo "Server to Agent: Invalid Target Database Configuration";;
    1035) echo "Server to Agent: Agent Failed";;
    1036) echo "Server to Agent: Agent is Ignored";;
    1037) echo "Server to Agent: Agent is Stopped";;
    1038) echo "Server to Agent: Agent is Terminated";;
    1063) echo "Server to Agent: Agent is Ended";;
    1100) echo "Server: connecting status";;
    1101) echo "Server: processing status";;
    1102) echo "Server: stalled status";;
    1103) echo "Server: pre_transition status";;
    1104) echo "Server: listening status";;
    1105) echo "Server: transition status";;
    1199) echo "Server: inactive";;
    1255) echo "Server: Unknown";;
    6001) echo "Server: Server Initialisation";;
    6002) echo "Server: Connecting to Agent(s)";;
    6003) echo "Server: Configuring Agent(s)";;
    6004) echo "Server: Recovery Processing";;
    6005) echo "Server: Startup Synchronization";;
    6021) echo "Server: Normal Processing";;
    6022) echo "Server: Performing Transition";;
    6023) echo "Server: Replication is Suspended";;
    6060) echo "Server: Server is ended";;
    2080) echo "Control Agent: Pre Transition";;
    2081) echo "Control Agent: Applying After-image Extent";;
    2082) echo "Control Agent: Transitioning";;
    2083) echo "Control Agent: Listening";;
    2084) echo "Control Agent: Waiting while JTA transactions are resolved";;
    2100) echo "Control Agent: connecting status";;
    2101) echo "Control Agent: processing status";;
    2102) echo "Control Agent: stalled status";;
    2103) echo "Control Agent: pre_transition status";;
    2104) echo "Control Agent: listening status";;
    2105) echo "Control Agent: transition status";;
    2199) echo "Control Agent: inactive";;
    2255) echo "Control Agent: Unknown";;
    3048) echo "Agent: Startup Synchronisation";;
    3049) echo "Agent: Normal Processing";;
    3050) echo "Agent: Recovery Synchronisation";;
    3051) echo "Agent: Online backup of Target Database or Being Transitioned";;
    3052) echo "Agent: Target Database in Quiet Point";;
    3053) echo "Agent: Target Database is in a BI stall";;
    3054) echo "Agent: Target Database is in an AI stall";;
    3055) echo "Agent: Being Transitioned";;
    3100) echo "Agent: connecting status";;
    3101) echo "Agent: processing status";;
    3102) echo "Agent: stalled status";;
    3103) echo "Agent: pre_transition status";;
    3104) echo "Agent: listening status";;
    3105) echo "Agent: transition status";;
    3199) echo "Agent: inactive";;
    3255) echo "Agent: Unknown";;
       *) echo "DSRUTIL: Unknown status";;
  esac >>$Log

# %BThe Replication %s is not running, the Replication Monitor cannot start. (18392)
# %BDatabase %s is not enabled for OpenEdge Replication. (18393)

 ($ECHO "M" # Modify display defaults
  $ECHO "1\n9999"         # 1.  Page size: 9999
  $ECHO "5\n$SampleIntrv" # 5.  Pause after each display
  $ECHO "6\n$SampleCount" # 6.  Number of times to repeat display
  $ECHO "Q"               # Q.  Return
  $ECHO "$Option"         # R.  Replication server status
                          # A.  Replication agent status
  $ECHO "U"               # U - continue uninterrupted
  $ECHO "Q"               # Q - quit (report)
  $ECHO "Q"               # Q.  Quit (dsrutil)
 ) | \
  $DSRUTIL $DB -C monitor 2>&1 | \
  while IFS="" read Line
  do
    echo `date '+%Y/%m/%d %H:%M:%S'` "$Line"
  done >>$Log &

  echo "$!|$DSRUTIL|DsrUtilMon|$DB" >>$MyProcList

} # DsrUtilMon()


#------------------------------------------------------------------------------
#
RunMonProc()
{
# Run DbMonProc to gather the additional information using 4GL.
#
  DB=$1
  MonIntrv=$2
  MonCount=$3
  Option=$4

  Prefix=${DbLogPrefix:-`basename $0`.$$.`basename $DB`}
#
# Is the monitoring procedure available?:
#
  test "$DbMonProc" && \
  test -f $DbMonProc || \
  return
#
# Did the script find a directory for statement cache files?:
#
  test "$StCaDir" && \
  test -d $StCaDir || \
  return
#
# Do not run DbMonProc if monitoring duration is long:
#
# test "$ShortRun" != "yes" && \
# return

  PROEXE=${PROEXE:-$DLC/bin/_progres}

  $PROEXE $DB -b -p $DbMonProc -tmpbsize 8 -Bt 128 $Option \
    -param $MonIntrv,$MonCount,$Prefix \
  >>$RunLog 2>&1 &

  echo "$!||`basename $DbMonProc`|$DB" >>$MyProcList

} # RunMonProc()


#------------------------------------------------------------------------------
#
SystemMon()
{
# Run system monitoring command.
#
  LogFile=$1
  Command=$2
  Params="$3"
  MonIntrv=$4
  MonCount=$5

# Skip a command if not found:
  type "$Command" >/dev/null 2>&1 || \
  return

# vmstat and iostat on Linux, SunOS and HP-UX:
# The first report produced gives averages since the last reboot.
# Only additional reports give information on a sampling period:
#
# vmstat and iostat on AIX:
# If the Count parameter is specified, the value of count determines
# the number of reports generated at Interval seconds apart.
# If the Interval parameter is specified without the Count parameter,
# the iostat command generates reports continuously.
#
  OSNAME=${OSNAME:-`uname`}
  case $Command in
    iostat|vmstat)
      test "$OSNAME" != "AIX" && \
      MonCount=`expr $MonCount + 1`
    ;;
  esac # case Command

# Create a log header:
  echo "$THIS Release $Release
Command: $Command $Params $MonIntrv $MonCount
   Date: `date '+%a %b %e %T %Y'`
   Host: `uname -a`
 Uptime: `uptime`
." >$LogFile  # dot ends the log header

  $Command $Params $MonIntrv $MonCount 2>&1 | \
  while IFS="" read Line
  do
    echo `date '+%Y/%m/%d %H:%M:%S'` "$Line"
  done >>$LogFile 2>&1 &

  echo "$!|$Command|$Command $Params" >>$MyProcList

} # SystemMon()


#------------------------------------------------------------------------------
#
MainPID1()
{
# Return PID of a specified command started in a pipe.
# Exit with code 1 if the function failed to find the PID.
# This version (V1) is for the sh and ksh shells on AIX, HP-UX and SunOS.
#
# Alternate solution is Job Control: jobs -l %%
# "set -m" enable Job Control for background jobs
#
  PipePID=$1 # PID of a parent process;
  MyPsOut=$2 # File with the output of the "ps -f";
  CmdName=$3 # Command whose PID we are looking for;
#
# PID=$$ is a parent for the last process (PID=$!) in pipe (i.e. its PPID=$$)
# Last process is a a parent for the rest processes in pipe (their PPID=$!)
#
# Example:
# pid 1       pid 2  pid 3           pid 4
# iostat 4 2 | cat | awk '{print}' | while read Line
# do
#   echo $Line
# done | grep "" >/dev/null 2>&1 & # pid 5 = $!
#
# AIX: $$=49349030 > $!=63898048=pid5 -> pid 1-4 (PPID=$1)
# ps -fp $!
#      UID      PID     PPID   C    STIME    TTY  TIME CMD
# 5 job183 63898048 49349030   0 14:52:20  pts/3  0:00 grep
# ps -f
#   job183 49349030 43581932   0 14:52:20  pts/3  0:00 -ksh
# 1 job183 36110718 63898048   0 14:52:20  pts/3  0:00 iostat 4 2
# 2 job183 15598054 63898048   0 14:52:20  pts/3  0:00 cat
# 3 job183 60424714 63898048   0 14:52:20  pts/3  0:00 awk {print}
# 4 job183 53543576 63898048   0 14:52:20  pts/3  0:00 -ksh
# 5 job183 63898048 49349030   0 14:52:20  pts/3  0:00 grep
#   job183 25297204 49349030   2 14:52:20  pts/3  0:00 ps -f

  grep " $PipePID " $MyPsOut | \
  grep " $CmdName " | \
  awk '$3=='$PipePID' && $2 ~ /^[0-9]*$/ {
         MainPID=$2
         Count++
       }
       END {if(Count==1) print MainPID
            else         exit 1}
  ' # awk

} # MainPID1()


#------------------------------------------------------------------------------
#
MainPID2()
{
# Return PID of a specified command started in a pipe.
# Exit with code 1 if the function failed to find the PID.
# This version (V2) is for the bash and csh shells.
#
  PipePID=$1 # PID of last command in pipe;
  MyPsOut=$2 # File with the output of the "ps -f";
  CmdName=$3 # Command whose PID we are looking for;
#
# All processes in pipe has the same parent (PPID=$$).
# Their PIDs are sequentially increasing by their order in pipe (except on AIX)
#
# Example:
# ps -fp $!
#      UID   PID  PPID  C    STIME TTY       TIME COMMAND
# 5 u03000 22009  9923  0 16:59:04 pts/19    0:00 grep
# ps -f
#   u03000  9923  9921  1 15:57:28 pts/19    0:01 -bash
# 1 u03000 22005  9923  6 16:59:04 pts/19    0:00 iostat 4 2@
# 2 u03000 22006  9923  0 16:59:04 pts/19    0:00 cat
# 3 u03000 22007  9923  0 16:59:04 pts/19    0:00 awk {print}
# 4 u03000 22008  9923  1 16:59:04 pts/19    0:00 -bash
# 5 u03000 22009  9923  0 16:59:04 pts/19    0:00 grep
#   u03000 22011  9923  1 16:59:04 pts/19    0:00 ps -f
#
# LoPID is the largest PID of ChildCMD below PipeLPID.
#
# Rare but still possible situation:
# PID reaches its maximum in the middle of pipe and the counter wraps back
# to the beginning (300 or 100 on HP-UX).
#
# Example:
# ps -fp $!
#   UID        PID  PPID  C STIME TTY          TIME CMD
#   root       300  8781  0 11:06 pts/9    00:00:00 grep
# ps -f
#   root      8781  8753  0 00:29 pts/9    00:00:08 bash
# 1 root     32764  8781  0 11:06 pts/9    00:00:00 iostat 1 2
# 2 root     32765  8781  0 11:06 pts/9    00:00:00 cat
# 3 root     32766  8781  0 11:06 pts/9    00:00:00 awk {print}
# 4 root     32767  8781  0 11:06 pts/9    00:00:00 bash
# 5 root       300  8781  0 11:06 pts/9    00:00:00 grep
#   root       302  8781  0 11:06 pts/9    00:00:00 ps -f
#
# HiPID for the largest PID of ChildCMD above PipeLPID:

#
# PPID of PipePID
#
  ParentID=`awk '$2=='$PipePID' {print $3}' $MyPsOut`

  grep " $CmdName " $MyPsOut | \
  awk '
    BEGIN {
      LoPID=0
      HiPID=0
    } # BEGIN

    $3!='$ParentID' {next}
    $2<'$PipePID' && $2>LoPID {LoPID=$2}
    $2>'$PipePID' && $2>HiPID {HiPID=$2}

    END {
      if(LoPID>0) print LoPID # LoPID has a higher priority to HiPID.
      else
      if(HiPID>0) print HiPID
      else exit 1
    } # END
  ' # awk

} # MainPID2()


#------------------------------------------------------------------------------
#
CheckProcesses()
{
# Check the status of all monitoring processes started by the script.
#

# File to save the output of the ps -ft TTYList (if needed).
#
  PsOutput=${LogPrefix:-`basename $0`.$$}.ps-ft-mytty.log

#
# Temporary file to update the list of running processes.
#
  NewProcList=${TmpPrefix:-/tmp/`basename $0`.$$}.CheckProcesses.tmp

#
# DbList2 was already copied to RunLog for the information purposes:
# rm command may take a few /seconds/ after fork()'ing new processes!
# Gus Bjorklund: Unless you have noatime mount option, a read is really a read
# data + write metadata (inode, etc). This has unpleasant side effects on some
# operating systems. Don't recall when but there were versions of HP-UX that
# had a bottleneck on that. Also: fork()'ing new processes is a relatively
# expensive system call that hammers the o/s virtual memory system.
#
# Report the time of the rm command (just in case):
#
  test -f $DbList2 && \
  (echo "
rm $DbList2"
   time rm $DbList2) >>$RunLog 2>&1

  test -z "$MyProcList" && \
  echoT "CheckProcesses: Error: MyProcList variable is not set!" && \
  return 1
#
# If the list of the running processes is empty then exit:
#
  test ! -s $MyProcList && rm $MyProcList 2>/dev/null && \
  echoT "$THIS terminated." && \
  return 1

  test -z "$MyTTY" && \
  test "$TERM" && \
  MyTTY=`tty 2>/dev/null | sed -e 's/^\/dev\///'`

  MyTTY=${MyTTY:-"?"}
  Shell=${Shell:-"ksh"}
  OSNAME=${OSNAME:-`uname`}

#
# Wait a bit to let all processes to start "completely":
#
  sleep 1
#
# Get the list of all processes started by the my session.
#
# Cannot use "ps -f" when running by cron: "ps: no controlling terminal"
#
# Cannot use "ps -ef" = Select all processes.
# It runs slowly when there are the large number of proceses running on system.
# Also "ps -ef" might temporarily hangs the whole system.
#
# Cannot use "ps -u userlist" = Select by effective user ID (EUID) or name.
# The effective user ID may vary depending from the setuserid of $MainCMD.
#
# Use: "ps -t ttylist" = Select by tty.
# This selects the processes associated with the terminals given in ttylist.
# Terminals (ttys, or screens for text output) can be specified in several
# forms: /dev/ttyS1, ttyS1, S1. A plain "-" may be used to select processes
# not attached to any terminal.
#
# "ps -ft" seemed to be twice faster than "ps -ef". Not too much faster though.
#
# Limit of the ps lists:
# AIX: The command-line flags that accept a list of parameters
# (the -o, -G, -g, -p, -t, -U, and -u flags) are limited to 128 items.
# For example, the -u Ulist flag can specify no more than 128 users.
#
  ps -ft "$MyTTY" >$PsOutput

  cat $MyProcList | \
  while IFS="|" read LastPID MainCmd CmdName DB Option
  do
#
# Did any processes failed to start?
#
    kill -0 $LastPID 2>/dev/null
    if [ $? -ne 0 ]
    then
      Msg="$BD$CmdName$UB terminated"
      test "$DB" && \
      Msg="$Msg for $BD$DB$UB"
      echoT "$Msg!"
      continue
    fi # if $?
#
# Empty MainCmd means the command was not run in a pipe.
# In this case LastPID is a PID of the running command (MainPID).
# Otherwise try to find a PID of the main command:
#
    test -z "$MainCmd" && \
    MainPID=$LastPID || \
    case $OSNAME-$Shell in
      Linux-ksh|AIX*|HP-UX*|SunOS*)
        MainPID=`MainPID1 $LastPID $PsOutput $MainCmd`
      ;;
      Linux-sh)
        MainPID=`MainPID2 $LastPID $PsOutput $MainCmd`
      ;;
      *)
        MainPID=`MainPID1 $LastPID $PsOutput $MainCmd || \
                 MainPID2 $LastPID $PsOutput $MainCmd`
      ;;
    esac # case OSNAME

    MainPID=${MainPID:-$LastPID}
    Msg=`echo "$CmdName" | awk '{printf"%10s started",$0}'`
    test "$DB"      && Msg="$Msg for $UL$DB$UU"
    test "$Options" && Msg="$Msg with $BD$Options$UB"
    Msg="$Msg (PID=$MainPID)"
    echoT "$Msg"
#
# Print "ps -ef" for MainPID to RunLog:
#
    awk '
      { ps=$0 } # Save the line to keep its formatting
      $2=='$MainPID' {print ps}
      $2=='$LastPID' {ps2=ps}
      END {if('$MainPID' != '$LastPID') print substr(ps2,1,120)}
    ' $PsOutput >>$RunLog
#
# MainPID login messages:
#
    test "$DB" && \
    tail -${LastLines:-1000} $DB.lg | \
    grep " P-$MainPID " >>$RunLog
    echo "" >>$RunLog

    echo "$MainPID|$MainCmd|$CmdName|$DB|$Option" >>$NewProcList
  done # while read $MyProcList

  rm $MyProcList 2>/dev/null && \
  mv $NewProcList $MyProcList

  test ! -s $MyProcList && \
  echoT "$THIS terminated." && \
  return 1 || \
  return 0

} # CheckProcesses()


#------------------------------------------------------------------------------
#
WaitForPIDs()
{
# Wait for all processes specified in the MyProcList file to terminate.

#
# Get the PIDs from the MyProcList file.
#
  WaitList=`
    cat $MyProcList | \
    while IFS="|" read CmdPID CmdName DB
    do
      echo $CmdPID
    done
  ` # WaitList=

  WaitList="`echo $WaitList`" # <- Remove the line breaks
  echo1 "Waiting for PIDs: $WaitList"
  WaitTime=0
#
# Wait for the processes with PIDs stored in the WaitList variable to terminate.
#
  while [ "$WaitList" ]
  do
    sleep 1
    WaitTime=`expr $WaitTime + 1`
#
# Check if PIDs a still running:
#
    MissedPIDs=""
    RemainPIDs=""
    for WaitPID in $WaitList
    do
      kill -0 $WaitPID 2>/dev/null && \
      RemainPIDs="$RemainPIDs$WaitPID "|| \
      MissedPIDs="$MissedPIDs$WaitPID "
    done # for WaitPID
#
# All PIDs terminated:
#
    test -z "$RemainPIDs" && \
    break # while sleep 1
#
# Wait timeout is over:
#
    test $WaitTime -gt $RunTime && \
    echo2 "Wait timeout ($RunTime sec) is over." && \
    break # while sleep 1
#
# All PIDs still running:
#
    test -z "$MissedPIDs" && \
    continue # while sleep 1
#
# Some PIDs terminated:
#
    echo1 "Terminated PID(s): $MissedPIDs"
    for CmdPID in $MissedPIDs
    do
      awk '
        BEGIN {FS="|"}
# MyProcList  format: 1=MainPID 2=MainCmd 3=CmdName 4=DB 5=Option
        $1=='$CmdPID' {
          printf"%d|%10s (PID %d) terminated",$1,$3,$1
          if(length($4)>0) printf" for %s|%s",$4,$4
          printf"\n"
          exit
        } # CmdPID
      ' $MyProcList | \
      while IFS="|" read MainPID Msg DB
      do
        echo2 "$Msg"
#
# MainPID logout message:
#
        test "$DB" && \
        tail -${LastLines:-1000} $DB.lg | \
        grep " P-$MainPID " >>$RunLog && \
        echo "" >>$RunLog
      done # while read MainPID Msg DB
    done # for CmdPID

    test "$StopRemains" = "stop" && \
    break # while sleep 1
#
# Less PIDs to wait:
#
    WaitList="$RemainPIDs"
    echo1 "Waiting for PIDs: $WaitList"

  done # while sleep 1
#
# RemainPIDs can be non-empty
# if the StopRemains flag is set or if wait time is over:
#
  test "$RemainPIDs" && \
  echo2 "Stopping PID(s): $RemainPIDs"

  for CmdPID in $RemainPIDs
  do
    kill -$SIGTERM $CmdPID 2>/dev/null
  done

  for CmdPID in $RemainPIDs
  do
    awk '
      BEGIN {FS="|"}
# MyProcList  format: 1=MainPID 2=MainCmd 3=CmdName 4=DB 5=Option
      $1=='$CmdPID' {
        printf"%d|%10s (PID %d) terminated",$1,$3,$1
        if(length($4)>0) printf" for %s|%s",$4,$4
        printf"\n"
        exit
      } # CmdPID
    ' $MyProcList | \
    while IFS="|" read MainPID Msg DB
    do
      echo2 "$Msg"
#
# MainPID logout message:
#
      test "$DB" && \
      tail -${LastLines:-1000} $DB.lg | \
      grep " P-$MainPID " >>$RunLog && \
      echo "" >>$RunLog
    done # while read MainPID Msg DB
  done # for CmdPID

  rm $MyProcList 2>/dev/null

  return 0

} # WaitForPIDs()


#------------------------------------------------------------------------------
#
LogArchiver()
{
# Archive the log files:
#
  Prefix=$1

  Archive=$Prefix.tar
  FileList="`ls -1 ${Prefix}*.log 2>/dev/null`"

  test "$FileList" || \
  return 1
#
# Wait a bit for the buffer flushes:
#
  sleep 1
#
# Add the log files created by the script to an archive file:
#
  tar -cf $Archive $FileList && \
  rm $FileList
#
# Add to the archive the text files created by DbMonProc:
#
  ls -1 ${Prefix}*.txt 2>/dev/null |
  while read File
  do
    tar -rf $Archive $File && rm $File
  done # while read File
#
# Add to the archive the protrace files:
#
  ls -1 protrace.* 2>/dev/null |
  while read File
  do
    tar -rf $Archive $File && rm $File
  done # while read File
#
# Add to the archive any temporary files left by my processes:
#
  ls -1 ${Prefix}* 2>/dev/null |
  while read File
  do
    test "$File" != "$Archive" && \
    tar -rf $Archive $File && rm $File
  done # while read File

  gzip $Archive || \
  return 1

} # LogArchiver()


#------------------------------------------------------------------------------
#
SendEmail()
{
# Email an archive file to the recipients:
#
  Archive=$1; shift
  MailList="$*"

  test "$MailList" || \
  return 1

  test -s "$Archive" || \
  return 1

  Subject="$THIS $RunParams $Archive"
#
# Use uuencode if available:
#
  if type uuencode >/dev/null 2>&1
  then
    uuencode $Archive $Archive | \
    mailx -s "$Subject" $MailList

    test "$Notify" = "yes" && \
    echoT "$THIS: Log archive sent to $MailList" >&2
    return 0
  fi # if uuencode
#
# Use base64 if available:
#
  if type base64 >/dev/null 2>&1
  then
    v_mailpart="$(uuidgen)/$(hostname)"

    echo "To: $MailList
Subject: $Subject
Content-Type: multipart/mixed; boundary=\"$v_mailpart\"
MIME-Version: 1.0

This is a multi-part message in MIME format.
--$v_mailpart
Content-Type: text/html
Content-Disposition: inline

<html><body>$Archive sent by $THIS `date`</body></html>

--$v_mailpart
Content-Transfer-Encoding: base64
Content-Type: application/octet-stream; name=$Archive
Content-Disposition: attachment; filename=$Archive

`base64 $Archive`
 --$v_mailpart--" | \
    /usr/sbin/sendmail -t

    test "$Notify" = "yes" && \
    echoT "$THIS: Log archive sent to $MailList" >&2
    return 0
  fi # if base64

# echo Mail is not sent

} # SendEmail()


#------------------------------------------------------------------------------
#
TimeInterval()
{
# Returns time difference in seconds between the dates.
# Example:
# TimeInterval "[2016/05/26@09:56:32.499+0400]" "`date '+%Y/%m/%d %H:%M:%S%z'`"

  echo "$1
$2" | \
  tr -d "[]" | \
  tr "@" " " | \
  awk '
    {
      Day[NR]=$1                # Year/Month/Day
      H=substr($2,1,2)          # Hours
      M=substr($2,4,2)          # Minutes
      S=substr($2,7,2)          # Seconds
      Z=substr($2,length($2)-4) # Timezone -0400
    }
    Z ~/^\+/ {                  # TZ +0400
      H-=substr(Z,2,2)          # Hours
      M-=substr(Z,4,2)          # Minutes
    } # TZ +0400
    Z ~/^\-/ {                  # TZ -0400
      H+=substr(Z,2,2)          # Hours
      M+=substr(Z,4,2)          # Minutes
    } # TZ -0400
    {
      Sec[NR]=H*3600+M*60+S     # Seconds (UTC)
    }
    END {
      Intetrval=Sec[2]-Sec[1]
# Function will not return a correct value
# if the time difference is greater that one day:
      if(Day[2]>Day[1]) Intetrval+=86400
      if(Day[2]<Day[1]) Intetrval-=86400
      print Intetrval
    } # END
  ' # awk
} # TimeInterval()


#------------------------------------------------------------------------------
#
FileSize()
{
# Return a file size:
#
  File=$1

  ls -l $File | awk '{print $5}' 2>/dev/null
} # FileSize()


#------------------------------------------------------------------------------
#
echoT()
{
# Write message with timestamp to stdout while launching the sessions.
#
  Time2=`date '+%H:%M:%S'`
  echo "$Time2 $*"
} # echoT()


#------------------------------------------------------------------------------
#
echo1()
{
# Write a message with timestamp to RunLog while waiting for running processes.
#
  Time1="[`date '+%Y/%m/%d@%H:%M:%S.000%z'`]"
  echo "$Time1 $*" >>$RunLog
} # echo1()


#------------------------------------------------------------------------------
#
echo2()
{
# Write a message with timestamp to RunLog and stderr
# while waiting for the running processes.
#
  Time1="[`date '+%Y/%m/%d@%H:%M:%S.000%z'`]"
  Time2=`date '+%H:%M:%S'`

  test "$Notify" && \
  echo "$Time2 $*" >&2
  echo "$Time1 $*" >>$RunLog
} # echo2()


#------------------------------------------------------------------------------
#

# TERM is not set if the script is running by cron:
if [ "$TERM" ]
then
# Bold/unbold terminal codes:
  BD=`tput smso` # Enter standout mode (bold on rxvt)
  UB=`tput rmso` # Exit standout mode

# Underline mode:
  UL=`tput smul` # Begin underline mode
  UU=`tput rmul` # Exit underline mode
fi

echo "
${BD}$THIS$UB [${UL}dbname${UU}] [...] [${UL}interval${UU} [${UL}count${UU}]] [${UL}-help${UU}]
System and Database Monitoring, Release $Release
" # echo


#------------------------------------------------------------------------------
#
# Parse the script's startup parameters:

RunParams=$*

RunFileMon=""
RunLatches=""
StopRemains=""

NonameCount=0
while [ $# -gt 0 ]
do
 case $1 in

  -h*|-help|--help) Usage
  ;;

# Integer values are SampleIntrv and SampleCount:
  [0-9]*) NonameCount=`expr $NonameCount + 1`
    case $NonameCount in
      1) SampleIntrv=$1;;
      2) SampleCount=$1;;
      3) echo "Unknown third noname option: $1. Use $UL$THIS -help$UU."
         exit 1;;
      9) echo "Noname options are not allowed after the -i/-c options: $1"
         exit 1;;
    esac
  ;;

# Database to be monitored: ---------------------------------------------------
  -db) DbNameList="$DbNameList $2"; shift
  ;;

# Sample interval: ------------------------------------------------------------
  -i) SampleIntrv=$2; NonameCount=9; shift
  ;;

# Number of the sample intervals: ---------------------------------------------
  -c) SampleCount=$2; NonameCount=9; shift
  ;;

# Monitoring procedure to run: ------------------------------------------------
  -p) DbMonProc=$2; shift
  ;;

# Directory to save the logs: -------------------------------------------------
  -w) WorkDir=$2; shift
  ;;

# If one monitoring process is terminated then stop the remaining processes: --
  -s*|-stop)
    StopRemains="stop"   # Unus pro omnibus, omnes pro uno
  ;;

# Gather the detailed information about all latches (Restricted Options): -----
  -l*|-latches)
    RunLatches="yes"
  ;;

#  Create protrace files for all processes if promon cannot connect database. -
  -f*|-filemon)
    RunFileMon="yes"
  ;;

#  Create protrace files for all processes if promon cannot connect database.
  -protrace)
    ProtraceOption=${2-"enable"}
    case $# in
     1) ProtraceOption="enable";;
     2) ProtraceOption=$2; shift;;
     *) test $2 -gt 0 2>/dev/null && \
        echo "Syntax error: $BD$*$UB. Use $UL$THIS -h$UU for a help." && \
        exit 1
        ProtraceOption=$2; shift;;
    esac # case $#

    test ! -w /etc/passwd && \
    echo "Warning: You may not have enough ${UL}permissions${UU} to create protrace files."
  ;;

# The unknown parameter: ------------------------------------------------------
  -*) echo "$BD$1$UB is an unknown option. Use $UL$THIS -h$UU for a help."
      exit 1
  ;;

# The list of emails to send the results to:
  *@*) MailList="$MailList $1"
  ;;

# Database to monitor: -------------------------------------------------------
  *) DbNameList="$DbNameList $1"
  ;;

 esac # case $1 in

 shift

done  # while [ $# -gt 0 ]

#------------------------------------------------------------------------------
#
# Main block
#

#
# Check the input parameters: -------------------------------------------------
#

# Check if SampleIntrv has a valid value:
test $SampleIntrv -ge 0 2>/dev/null
case $? in
  0) ;;
  1) echo "Sample interval has a negative value: $SampleIntrv"; exit 1;;
  *) echo "Integer value expected as sample interval: -i \"$SampleIntrv\""; exit 1;;
esac

# Check if SampleCount has a valid value:
test $SampleCount -ge 0 2>/dev/null
case $? in
  0) ;;
  1) echo "Interval count has a negative value: $SampleCount"; exit 1;;
  *) echo "Integer value expected as interval count: -c \"$SampleCount\""; exit 1;;
esac

# If sample interval is zero, only one report is printed
# with the statistics since startup.
#
if [ $SampleIntrv -eq 0 ]
then
  SampleIntrv=2
  SampleCount=0
  RunTime=0
else
#
# Adjust SampleIntrv and SampleCount
# because promon uses a sample interval that is multiple of two:
#
  RunTime=`expr $SampleIntrv \* $SampleCount`

  SampleIntrv=`expr $SampleIntrv + 1`
  SampleIntrv=`expr $SampleIntrv / 2`
  SampleIntrv=`expr $SampleIntrv \* 2`

  test $SampleIntrv -lt 2 &&
  SampleIntrv=2

  SampleCount=`expr $RunTime + 1` # just for a rounding up
  SampleCount=`expr $SampleCount / $SampleIntrv`

fi # if SampleIntrv -eq 0

#
# Change monitoring strategy if the monitoring duration is longer than LongRun:
#
ShortRun="no"
RunTime=`expr $SampleIntrv \* $SampleCount`
test $RunTime -lt $LongRun && \
ShortRun="yes" || \
Notify="no"
#
# Check availability of DbMonProc procedure: ----------------------------------
#
if [ "$DbMonProc" ]
then
  MyMonProc=$DbMonProc
  ScriptDir=`dirname $0`
  ScriptDir=`(cd $ScriptDir; pwd)`

  ProcDir=`dirname $DbMonProc`
  ProcDir=`(cd $ProcDir; pwd)`

  DbMonProc=`basename $DbMonProc`
  DbMonProc=`echo $DbMonProc | sed -e 's/\.p$//'`
  DbMonProc=`echo $DbMonProc | sed -e 's/\.r$//'`
  LowerCase=`echo $DbMonProc | awk '{print tolower($0)}'`

  for Proc in $DbMonProc $DbMonProc.p $DbMonProc.r \
              $LowerCase $LowerCase.p $LowerCase.r
  do
    for Dir in $ScriptDir $ProcDir $WorkDir .
    do
      DbMonProc=$Dir/$Proc
      test -f $DbMonProc && \
      break 2
    done # for Dir
  done # for Proc

  test ! -f $DbMonProc && \
  echo Procedure $MyMonProc not found and will be ignored... && \
  DbMonProc=""
fi # if DbMonProc

#
# Add protrace enabled database to the list of databases to monitoring: -------
#
test "$ProtraceOption" && \
case "$ProtraceOption" in
  "enable"|"all") ;;
   *) DbNameList="$DbNameList $ProtraceOption"
      ProtraceOption=`echo $ProtraceOption | sed -e 's/\.db$//'`
   ;;
esac

#
# Remove the leading spaces from the list:
#
DbNameList=`echo $DbNameList`

#
# Set the locale environment to "en_US":
#
en_LC=`locale -a 2>/dev/null | \
awk '/^en_/ {print
             exit}'`
# test "$en_LC" && LC_TIME=$en_LC && export LC_TIME
if [ "$en_LC" ]
then
  LC_ALL=$en_LC
  LC_TIME=$en_LC
  export LC_ALL LC_TIME
fi # if en_LC

#
# Directory to save the logs that will be created by the script:
#
for Dir in $WorkDir "." $HOME /var/tmp /tmp
do
  test -d $Dir && \
  test -w $Dir && \
  break
done

test "$Dir" = "$WorkDir" && Set="is set" || Set="was reset"
WorkDir="`(cd $Dir 2>/dev/null; pwd)`"

cd $WorkDir && \
test "$TERM" && \
echo "Working directory $Set to $WorkDir"

# Naming convention for the log files: ----------------------------------------
# LogName=LogPrefix.LogType.log
# where LogPrefix=dbmon.hostname.date_time
#
LogPrefix=`echo $THIS | cut -d "." -f 1`
LogPrefix=$LogPrefix.`uname -n`.`date '+%y%m%d_%H%M%S'`
# LogPrefix=$LogPrefix.`hostname`.`date '+%y%m%d_%H%M%S'`

# Prefix for temp files:
#
TmpPrefix=/tmp/`basename $0`.$$

# Log to save the messages from stdout stream:
#
RunLog=$LogPrefix.run.log

# Check if someone is starting the script exactly at the same time. For example,
# the cron can be used to run the script for each database separately.
#
test -f $RunLog && \
LogPrefix=$LogPrefix.$$ && \
RunLog=$LogPrefix.run.log

#
# File to save information about the background processes started by script:
#
MyProcList=$LogPrefix.pidlist.log

DebugLog=$LogPrefix.debug.log
# echo "[`date '+%Y/%m/%d@%H:%M:%S%z'`] Comments or information" >>$DebugLog

#
# Sanity check: Is DLC set correctly? -----------------------------------------
#
# The executables used by the script:
#
PROEXE=${PROEXE:-$DLC/bin/_progres}
DBUTIL=${DBUTIL:-$DLC/bin/_dbutil}
PROSHUT=${PROSHUT:-$DLC/bin/_mprshut}
PROUTIL=${PROUTIL:-$DLC/bin/_proutil}
DSRHOME=${DSRHOME:-$DLC}; export DSRHOME
DSRUTIL=${DSRUTIL:-$DSRHOME/bin/rprepl}

test "$DLC" && \
test -d $DLC && \
test -x $PROEXE && \
test -x $DBUTIL && \
test -x $PROSHUT && \
test -x $PROUTIL

if [ $? -ne 0 ]
then
  test "$TERM" && \
  echo "Incorrect value of DLC=$DLC"
# exit 1

# If the script is by cron then find a location of DLC directory based on
# the running Progress executable. Though the preferable method is to set
# DLC explicitly at the header of the script.
#
  DLC="`GetDLC _mprosrv`"
  test "$DLC" || exit 1

  export DLC

  PROEXE=$DLC/bin/_progres
  DBUTIL=$DLC/bin/_dbutil
  PROSHUT=$DLC/bin/_mprshut
  PROUTIL=$DLC/bin/_proutil

  echo "DLC variable was reset to $DLC"
fi # if incorrect DLC

#
# Sanity check: Is $DLC/bin in PATH?
#
type _progres >/dev/null 2>&1 || \
PATH=$DLC/bin:$PATH
export PATH

# Progress working directory created during Progress installation:
# Proinst creates it with the "drwxrwxrw" permissions.
#
ProDir=`awk 'BEGIN {FS="="}
                    $1=="workDir" {print $2}
       ' $DLC/installd.ini`
#ProDir
#
# Directory for Statement Cache Files
# where .cst temporary files store the information when it exceeds 256 bytes of memory.
#
# promon/R&D
#  1. Status Displays
# 18. Client Database-Request Statement Cache
#  8. Specify Directory for Statement Cache Files
# http://knowledgebase.progress.com/articles/Article/000040544

for StCaDir in $ProDir /tmp /var/tmp $WorkDir $HOME
do
  test -d $StCaDir && \
  test -w $StCaDir && \
  ls -ld  $StCaDir | grep "^drwxrwxrw" >/dev/null 2>&1 && \
  break
done

StCaDir="`(cd $StCaDir 2>/dev/null; pwd)`"

#
# System can run the hundreds databases. Store their list in a file:
#
DbList1=$TmpPrefix.dblist.1.tmp  # Preliminary list of databases to monitor;
DbList2=$TmpPrefix.dblist.2.tmp  # The final list of databases to monitor.

#
# List of all databases running on the system Use (proutil -C dbipcs): --------
# Note: db can be linked to the different shared memory segments:
# PROGRESS SHARED MEMORY STATUS
#       ID ShMemVer Seg# InUse Database
# 1572872    10213    0 Yes   /home/wrk/sports.db
# 1835020    10213    0 Yes   /home/wrk/sports.db
# First segment is in use only by apw

$PROUTIL -C dbipcs | \
awk 'NR<3 {next}
     $4=="Yes" {print $5}
' | \
sort | \
uniq >$DbList1

if [ ! -s $DbList1 ] # File has a zero size
then
  rm $DbList1 2>/dev/null
  echo "${BD}There are no running databases on the system!${UB}"
  exit 1
fi

#
# List of running databases that match the specified name patterns: -----------
# Write results to DbList2:
#

if [ "$DbNameList" ]
then
  for DbName in $DbNameList
  do
    grep "$DbName" $DbList1 >>$DbList2 && \
    continue # for DbName

    DB=`echo $DbName | sed -e 's/\.db$//'`
    test -f $DB.db && \
    echo "There are no shared memory segments for database $BD$DbName$UB!" || \
    echo "Name pattern \"$BD$DbName$UB\" does not match any running database!"

    rm $DbList1 $DbList2 2>/dev/null
    exit 1
  done # for DbName

# The same database can match a few name patterns.
# Make the list of the unique names.
# Write results to DbList1
#
  cat $DbList2 | sort | uniq >$DbList1
fi

#
# Check the actual status of the selected databases: --------------------------
#
# Write results to DbList2:
rm $DbList2 2>/dev/null

cat $DbList1 | \
while read DB
do
  DB=`echo $DB | sed -e 's/\.db$//'`

  $PROUTIL $DB -C holder >/dev/null

  case $? in
     0) echo "Warning: There is no server for database $BD$DB$UB"
        echo "but its shared memory is not removed:"
        echo "      ID ShMemVer Seg# InUse Database"
        $PROUTIL -C dbipcs | grep " $DB.db"

        echo "Shared memory might be used by the following processes:"
        PsOutput=`psEF`
        grep `basename $DB` $PsOutput

        continue # while read DB
    ;;
# ** The database %s is in use in single-user mode. (263)
    14) echo "Warning: Database $BD$DB$UB is in use in single-user mode:"

        if [ -r $DB.lk ]
        then
          echo "lk File Owner:"
          LkPID=`LkInfo -pid $DB`
          ps -fp $LkPID 2>&1
        fi # if -r DB.lk

        echo "But database shared memory is not removed:"
        echo "      ID ShMemVer Seg# InUse Database"
        $PROUTIL -C dbipcs | grep " $DB.db"

        echo "Shared memory might be used by the following processes:"
        PsOutput=`psEF`
        grep `basename $DB` $PsOutput

        continue # while read DB
    ;;
# ** The database %s is in use in multi-user mode. (276)
    16) if [ ! -r $DB.lg ]
        then
          echo "Warning: You do not have permission to read database log."
          ls -l $DB.lg 2>&1
          echo "Database $BD$DB$UB will be skipped."
          continue # while read DB
        fi # if ! -r DB.lg

        echo $DB >>$DbList2
    ;;
# proutil failed
     *) echo "Warning: proutil $BD$DB$UB -C holder failed."
        continue # while read DB
    ;;
  esac # case $?
done # while read DB

# From here the names in DbList2 do not have .db extensions.

rm $DbList1 2>/dev/null

if [ ! -s $DbList2 ] # File has a zero size
then
  rm $DbList2 2>/dev/null
  echo "There are no databases to monitor!" && \
  exit 1
fi

#
# If base names are unique than use them as a part of log file name.
# Otherwise use the full db names:
#
FullCount=`cat $DbList2 | wc -l`
BaseCount=`cat $DbList2 | while read DB; do basename $DB; done | \
           sort | uniq  | wc -l`
test $FullCount -eq $BaseCount && \
NameFormat="Base" || \
NameFormat="Full"

#
# Check if we can connect a database: -----------------------------------------
# Write results to DbList1: DB ProbePID DbLogPrefix LgOffset
#

ProbeSuffix="probe.tmp"

cat $DbList2 | \
while read DB
do
  DB=`echo $DB | sed -e 's/\.db$//'`

  test "$NameFormat" = "Base" && \
  UniqueName=`basename $DB` || \
  UniqueName=`echo $DB | sed -e 's/^\///' | tr -s "\/" "~"`
  DbLogPrefix=$LogPrefix.$UniqueName
#
# Check the recent errors in db log files: ------------------------------------
# Skip the check if a monitoring duration is too short.
#
  test $RunTime -gt 2 && \
  LogMsgStat $DB
#
# Offset in db log to start a search for login messages:
#
  DbLogSize=`FileSize $DB.lg`
#
# Check if database is in crash recovery mode: --------------------------------
#
# Database connections are not allowed at this time.
#
  $PROUTIL $DB -C busy >/dev/null
#
# Return Code = 64 = Database is in process of starting up.
#
  if [ $? -eq 64 ]
  then
    echo $DB 0 $DbLogPrefix $Mode # <= 0 means no probe connection
    continue # while read DB
  fi
#
# Start a probe session (connect and quit): -----------------------------------
#
  ProbeLog=$DbLogPrefix.$ProbeSuffix
  ProbeConnect $DB $ProbeLog

  echo $DB $! $DbLogPrefix $DbLogSize

done >$DbList1 # while read DB from $DbList2

#
# Create the file with the "ps -ef" output in advance
# if script was started with the -filemon or -protrace all option.
# The result can also catch the probe sessions if they are slow.
#
if [ "$RunFileMon" = "yes" ] || [ "$ProtraceOption" = "all" ]
then
  psEF >/dev/null
fi

#
# Wait a bit while probe session connects and disconnects from a database:
#
sleep 1
#
# Write results to DbList2: DB Connect DbLogPrefix
#
rm $DbList2 2>/dev/null

#
# RunLog begins here! =========================================================
#
EnvInventory >$RunLog 2>&1

cat $DbList1 | \
while read DB ProbePID DbLogPrefix PrevSize
do
  if [ $ProbePID -eq 0 ]
  then
    echoT "Warning: $BD$DB$UB is in crash recovery mode (lk=$PrevSize)."
    LastLogMsg $DB " (333) "
    LastLogMsg $DB " BROKER "
    echo "Database connections are not allowed at this time."

    Connect="Recovery"

    echo $DB $Connect $DbLogPrefix >>$DbList2
    continue # while read DB

  fi # if crash recovery
#
# Check db connection of the previously launched probe session:
#
  ProbeLog=$DbLogPrefix.$ProbeSuffix
  ProbeMsg=$DbLogPrefix.probemsg.tmp
#
# Check if a probe session left any messages in db log file:
#
  CurrSize=`FileSize $DB.lg`
  LastSize=`expr $CurrSize - $PrevSize`
  LastSize=`expr $LastSize \* 2` # Double the size just in case
  tail -${LastSize}c $DB.lg | grep " P-$ProbePID " >$ProbeMsg

  if [ -s "$ProbeMsg" ]
  then
    echo "Probe session login/logout to $DB:"
    cat $ProbeMsg
  fi >>$RunLog

#
# If a probe session is still running: ----------------------------------------
#
  if kill -0 $ProbePID 2>/dev/null
  then
    echoT "Warning: probe session (PID=$ProbePID) hung connecting $BD$DB$UB"

    kill -$SIGUSR1 $ProbePID 2>/dev/null

    case ${OSNAME:-`uname`} in
      AIX)   procstack $ProbePID >>$RunLog 2>/dev/null;;
      SunOS) pstack -F $ProbePID >>$RunLog 2>/dev/null;;
# pstack -F Force. Grabs the target process even if another process has control.
    esac

#
# If a probe session left any messages in its own log file:
#
    test -s "$ProbeLog" && \
    echo "Probe session issued the ${BD}error${UB}:" && \
    cat $ProbeLog

    test -s "$ProbeMsg" && \
    echo "Probe session has written to the database log:" && \
    cat $ProbeMsg
#
# If probe session did not write a login message to a database log
# and/or it it did not issue the errors to stdout then
# connect status is "Locked" (login semaphore or USR latch are locked).
# Otherwise the connect status is "Failed".
#
    if [ -s "$ProbeMsg" ]
    then Connect="Failed"
    else Connect="Locked"
    fi

    rm $ProbeMsg $ProbeLog 2>/dev/null

    MoveProtrace $ProbePID "." ".`basename $DB`.connect.txt" && \
    echo "See connect protrace.$ProbePID" >>$RunLog

    echo $DB $Connect $DbLogPrefix >>$DbList2
    continue # while read DB

  fi # if kill -0 $ProbePID
#
# If a probe session has finished with the errors: ----------------------------
#
  if [ -s "$ProbeLog" ]
  then
    ErrorNum=`awk '$NF ~ /^\(/ {
                print $NF
                exit}' $ProbeLog`
    ErrorNum=${ErrorNum:-"unknown"}
#
# Depending from a type of process used in ProbeConnect
# some messages in db log should be ignored:
#
    case "$ErrorNum" in
# ** Batch-mode PROGRESS requires a startup procedure. (1144)
      "(1144)") ErrorNum="";;
    esac
  else
    ErrorNum=""
  fi

  if [ "$ErrorNum" ]
  then
    echoT "Warning: probe session failed to connect $BD$DB$UB with ${BD}error${UB}:"
    cat $ProbeLog
    Connect=$ErrorNum
  else
    Connect="Successful"
  fi

  rm $ProbeMsg $ProbeLog 2>/dev/null

  echo $DB $Connect $DbLogPrefix >>$DbList2

done | \
tee -a $RunLog
# while read DB from $DbList1

#
# Start the database monitoring: ----------------------------------------------
#
# DbList1 will not be used anymore:
#
rm $DbList1 2>/dev/null

( echo "
------------------------------------------------------------------------------

The list of databases to monitor:"
  cat $DbList2
  echo "
------------------------------------------------------------------------------
") >>$RunLog

cat $DbList2 | \
while read DB Connect DbLogPrefix
do
  PrevSize=`FileSize $DB.lg`

  Protrace=""
  test "$ProtraceOption" && \
  case "$ProtraceOption" in
    enable)
 # Protrace will depend from $Connect status
    ;;
    all)
       Protrace="protrace"
    ;;
    *) echo $DB | grep "$ProtraceOption" >/dev/null 2>&1 && \
       Protrace="protrace"
    ;;
  esac # case ProtraceOption

  case "$Connect" in

    "Successful") # -----------------------------------------------------------

      echoT "Starting processes for ${UL}$DB${UU}..."

      test "$Protrace" && \
      DbProcList $DB $Protrace
#
# Run LruShot() to make the subsequent promon sessions insensitive
# to the LRU latch contention:
#
#     LruShot $DB &
#
# Run the full list of Progress monitoring sessions:
#
      DbDescribe $DB
      MainPromon $DB $SampleIntrv $SampleCount
      LatchPMon  $DB $SampleIntrv $SampleCount
      StatusPMon $DB $SampleIntrv $SampleCount
      RunMonProc $DB $SampleIntrv $SampleCount
      test "$RunFileMon" = "yes" && \
      DbFileMon  $DB $SampleIntrv $SampleCount
      test "$RunLatches" = "yes" && \
      LatchInfo  $DB $SampleIntrv $SampleCount
      test -x $DSRUTIL && \
      DsrUtilMon $DB $SampleIntrv $SampleCount

    ;; # Connect = Successful

    "Failed") # ---------------------------------------------------------------

      echoT "Failed to connect ${UL}$DB${UU}..."
      echo "Starting the file level monitoring..."

      test "$ProtraceOption" = "enable" && \
      Protrace="protrace"

      DbProcList $DB $Protrace
      DbDescribe $DB "Failed to connect"
      DbFileMon  $DB $SampleIntrv $SampleCount

    ;; # Connect = Failed

    "Locked") # ---------------------------------------------------------------
#
# Normal connection hung. Try to connect a database with the -F option:
#
      echoT "Trying to use the ${BD}-F${UB} option to connect ${UL}$DB${UU}..."

      test "$ProtraceOption" = "enable" && \
      Protrace="protrace"

      DbProcList $DB $Protrace
      DbDescribe $DB "Connection hung"
      MainPromon $DB $SampleIntrv $SampleCount -F
      DbFileMon  $DB $SampleIntrv $SampleCount

    ;; # Connect = Locked

    "(5291)") # ---------------------------------------------------------------
#
# You have attempted to connect to a database with too many
# users connected to it. Retry the connection later,
# or increase -n on the server. (5291)

      echoT "Too many users connected to $BD$DB$UB."
      echo "Still trying to start the monitoring processes..."

      DbConnectStat $DB
      LicenseFile   $DB 2

      test "$ProtraceOption" = "enable" && \
      Protrace="protrace"

      DbProcList $DB $Protrace
      DbDescribe $DB "Too many users"
#
# If monitoring duration is longer than 60 sec then rise the priority.
# Do not disconnect WDOG or APW:
#
      Priority=0
      test $RunTime -gt 60 &&
      Priority=2
#
# Try to disconnect any "insignificant" sessions
# with the priority higher than Priority:
#
      DisconnectList=`FreeDbConnect $DB 1 $Priority`

      test "$DisconnectList" && \
      MainPromon $DB $SampleIntrv $SampleCount || \
      DbFileMon  $DB $SampleIntrv $SampleCount
#
# Deferred restart of the disconnected batch processes:
#
      Delay=`expr $RunTime + 1` # Add one second just in case...

      for Helper in $DisconnectList
      do
        case $Helper in
          WDOG) (sleep $Delay; $PROSHUT $DB -C watchdog) >/dev/null 2>&1 & ;;
           APW) (sleep $Delay; $PROSHUT $DB -C apw     ) >/dev/null 2>&1 & ;;
        esac # Helper
      done # for Helper

    ;; # Connect Error: (5291) too many users connected

    "(1384)") # ---------------------------------------------------------------
#
# The database is being shutdown. (1384)
#
      echoT "Warning: $BD$DB$UB is being shutdown."
      echo "Starting the file level monitoring..."

      LastLogMsg $DB " shutdown "
      LastLogMsg $DB " BROKER "
      echo "Database connections are not allowed at this time."

      test "$ProtraceOption" = "enable" && \
      Protrace="protrace"

      DbProcList $DB $Protrace
      DbFileMon  $DB $SampleIntrv $SampleCount

    ;; # Connect Error: (1384) The database is being shutdown.

    "Recovery") # -------------------------------------------------------------

      echoT "Warning: $BD$DB$UB is in crash recovery."
      echo "Starting the file level monitoring..."

      if [ "$Protrace" = "protrace" ] || \
         [ "$ProtraceOption" = "enable" ] && \
         [ -r $DB.lk ]
      then
        Dir=`dirname $DB`
        LkPID=`LkInfo -pid $DB`
        test "$LkPID" && \
        kill -$SIGUSR1 $LkPID 2>/dev/null && \
        sleep 1 && \
        MoveProtrace $LkPID $Dir ".recovery.txt" &
      fi # if protrace

      DbFileMon $DB $SampleIntrv $SampleCount

    ;; # Connect = Recovery

    *) # ----------------------------------------------------------------------
#
# Unexpected error returned by probe connection. Do not know what to do.
#
# Examples of the possible errors:
#
# SYSTEM ERROR: Shared memory access permission denied (1136)
# There is no server for database <dbname>. (1423)
# Attempted to connect to another database_s shared memory. (10834)
#
      echoT "Error $Connect: No actions will be taken for $BD$DB$UB"
    ;;

  esac # case Connect
#
# the run log:
#
   CurrSize=`FileSize $DB.lg`
   LastSize=`expr $CurrSize - $PrevSize`

   if [ $LastSize -gt 0 ]
   then
     echo "
Logins to  $DB"
     tail -${LastSize}c $DB.lg
     echo "
------------------------------------------------------------------------------
"
   fi >>$RunLog

done | \
tee -a $RunLog
# while read DB from DbList2

#
# Start the system monitoring commands specified in SysMonList: ---------------
#

# Run the system commands only if at least some promon sessions were started:

test -s $MyProcList && \
echo $SysMonList | \
awk 'BEGIN {RS=";"}
     {print}' | \
while read Command Parameters
do
  LogFile=`echo $LogPrefix.$Command$Parameters.log | tr -d " "`
  SystemMon $LogFile $Command "$Parameters" $SampleIntrv $SampleCount
done | \
tee -a $RunLog

InitTime=`date '+%Y/%m/%d@%H:%M:%S%z'`

CheckProcesses | \
tee -a $RunLog && \
echoT "Basic monitoring: $SampleIntrv sec sample * $SampleCount intervals" && \
echoT "Estimated monitoring duration: $BD$RunTime$UB sec" || \
exit

#
# Archive the log when the monitoring processes will terminate: ---------------
#
trap "" 1

WaitForPIDs && \
LogArchiver $LogPrefix && \
SendEmail   $LogPrefix.tar.gz $MailList &

#
#------------------------------------------------------------------------------
#
# This is the end of dbmon.sh
