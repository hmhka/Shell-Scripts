#!/bin/sh

THIS=`basename $0`
Release="V1.1 as of July 16, 2014"

# Default values:
#
# Monitoring interval to gather the system statistics during the run:

MonIntrv=3

#------------------------------------------------------------------------------
# SizeOfFiles expects the output of ls -l and returns the total size of files:

SizeOfFiles()
{
# ls -l
# total 1148
# -rw-rw-r--    1 root     system         2089 Feb 03 14:13 tmp.lg

 awk 'NF>=9 {Size+=$5}; END {print Size}' -

} # SizeOfFiles()


#------------------------------------------------------------------------------
# AddCurrTime reads the input stream and adds the current time to each line.

AddCurrTime()
{
  while IFS="" read Line
  do
    echo `date '+%H:%M:%S'` "$Line"
  done
}


#------------------------------------------------------------------------------
# CleanCurrentLine: clean the current line on the screen.

CleanCurrentLine()
{
  $echo \
"                                                                         \r\c"
} # CleanCurrentLine()


#------------------------------------------------------------------------------
# Choosing the versions of basic Unix commands: echo and awk

OSNAME=`uname`

case "$OSNAME" in
 "Linux") echo="echo -e";;
       *) echo="echo";;
esac

#------------------------------------------------------------------------------
# awk or nawk?

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


#------------------------------------------------------------------------------
# The auxiliary variables:

# Bold/unbold terminal codes:
BD=`tput smso`
UB=`tput rmso`

#------------------------------------------------------------------------------
# Parsing the script's input parameters:

# rfutil db-name -C qualifier

RfutilParamList="$*"

while [ $# -gt 0 ]
do
  case $1 in
   -a) test ! -f $2 && \
       echo "AI file $2 not found." && \
       exit 1
       AiFile=$2;;
   -*) ;;
    *) if [ -z "$DbName" ]
       then
         test -f $1 || test -f $1.db && DbName=$1
       fi;;
  esac  #case $1

  shift
done

if [ -z "$DbName" ]
then
  echo $THIS: You must supply a database name.
  exit 1
fi

#------------------------------------------------------------------------------

# Main block:

# Progress environment:

if [ -x $DLC/bin/_proutil ]
then
  PATH=$DLC/bin:$PATH
  export PATH
else
  echo Check DLC=$DLC
  exit
fi

# Set English locale to get the command's messages in English:

EnglishLC=`locale -a 2>/dev/null | $awk '/^en_/ {print; exit}'`
EnglishLC=${EnglishLC-"en_US"}
LC_TIME=$EnglishLC;     export LC_TIME
LC_MESSAGES=$EnglishLC; export LC_MESSAGES

LogPrefix=rfutil.`date '+%y%m%d_%H%M%S'`


LogFile=${LogPrefix}.describe.log
$DLC/bin/_proutil $DbName -C describe -cpinternal undefined -cpcoll basic >$LogFile 2>&1

LogFile=${LogPrefix}.prostrct.before.log
$DLC/bin/_dbutil prostrct statistics $DbName >$LogFile 2>&1


# Run the system commands to monitor the disk and CPU activity:

MonCount=3600
OSNAME=`uname`
case "$OSNAME" in

 "Linux")    SysMonCmd1="vmstat $MonIntrv $MonCount"
             SysMonCmd2="iostat -k -t $MonIntrv $MonCount"
             MemMonCmd="ps u -p %PID"
  ;;
 "SunOS")    SysMonCmd1="sar -u $MonIntrv $MonCount"
             SysMonCmd2="iostat -d -T d $MonIntrv $MonCount"
             MemMonCmd="ps -o vsz -p %PID"
  ;;
 "HP-UX")    SysMonCmd1="vmstat $MonIntrv $MonCount"
             SysMonCmd2="iostat $MonIntrv $MonCount"
             MemMonCmd="ps -lp %PID"
  ;;
 "AIX")      SysMonCmd1="vmstat -t $MonIntrv $MonCount"
             SysMonCmd2="iostat -T $MonIntrv $MonCount"
             MemMonCmd="svmon -P %PID"
  ;;
 "OpenUNIX") SysMonCmd1="sar -u $MonIntrv $MonCount"
             SysMonCmd2="sar -d $MonIntrv $MonCount"
             MemMonCmd="ps -lp %PID"
  ;;
 "UnixWare") SysMonCmd1="sar -u $MonIntrv $MonCount"
             SysMonCmd2="sar -d $MonIntrv $MonCount"
  ;;
 "SCO_SV")   SysMonCmd1="sar -u $MonIntrv $MonCount"
             SysMonCmd2="sar -d $MonIntrv $MonCount"
             MemMonCmd="ps -lp %PID"
  ;;
  *)         SysMonCmd1="vmstat $MonIntrv $MonCount"
             SysMonCmd2="iostat $MonIntrv $MonCount"
             MemMonCmd="ps -lp %PID"
  ;;
esac

LogFile=${LogPrefix}.cpustat.log
(uname -a; echo "$SysMonCmd1")  >$LogFile
$SysMonCmd1 2>&1 | AddCurrTime >>$LogFile &
PID1=$!

LogFile=${LogPrefix}.iostat.log
(uname -a; echo "$SysMonCmd2")  >$LogFile
$SysMonCmd2 2>&1 | AddCurrTime >>$LogFile &
PID2=$!

$echo \
"Gathering system statistics... Rfutil will run in $MonIntrv seconds.\r\c"
sleep $MonIntrv
CleanCurrentLine

LogFile=${LogPrefix}.screen.log
(echo $THIS $Release
 uname -a
 echo "$DLC/bin/_rfutil $RfutilParamList"
 test "$AiFile" && echo AI file: `ls -l $AiFile`
) >$LogFile
$DLC/bin/_rfutil $RfutilParamList 2>&1 | AddCurrTime | tee -a $LogFile &
PID=$!

LogFile=${LogPrefix}.memory.log

MemMonCmd=`echo $MemMonCmd | sed -e 's/%PID/'$PID'/'`

while kill -0 $PID 2>/dev/null
do
  $MemMonCmd
  sleep $MonIntrv
done | AddCurrTime >>$LogFile


# Let to complete the last interval of system monitoring:
$echo "Test will be completed in $MonIntrv seconds...\r\c"
sleep $MonIntrv
CleanCurrentLine
 
# Stop gathering the system statistics:
for PID in $PID1 $PID2
do
  kill $PID 2>/dev/null
done

LogFile=${LogPrefix}.prostrct.after.log
$DLC/bin/_dbutil prostrct statistics $DbName >$LogFile 2>&1

TarFile=${LogPrefix}.tar
tar -cf $TarFile ${LogPrefix}.*.log

for Type in screen memory cpustat iostat describe prostrct.before prostrct.after
do
  LogFile=${LogPrefix}.${Type}.log
  test -f $LogFile && rm $LogFile 2>/dev/null
done

echo The results are saved in $BD$TarFile$UB
