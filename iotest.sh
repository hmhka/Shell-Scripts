#!/bin/sh

THIS=`basename $0`
Release="V2.1 as of July 07, 2014 12:10"

# iotest.sh - shell script to test the throughput of disk subsystem.
# Written by George Potemkin, June 28, 2014
# Last modified: July 07, 2014

# Default values:
#
# Directory to create the temp files in. It can be set through command line.
TestDir="."

# Base disk size to use in the tests (the -size parameter):
TestSize=50M

# Total size of writes during ddWriteTest (the -write parameter):
# Default is $TestSize
ddWriteSize=""

# Total size of writes during biWriteTes (the -unbuf parameter):
# Default is ($TestSize / $biWriteFact)
biWriteSize=""
biWriteFact=4

# Total size of reads during ddReadTest (the -read parameter):
# Default is ($TestSize * $ddReadFact)
ddReadSize=""
ddReadFact=4

# Minimal size to run the read test (ddReadTest):
MinReadSize=10M

# Blocksize to use in read/write tests:
BlockSize=8K

# Number of threads to spawm in multi-threaded tests.It can be set by -n.
# Zero means the number of logical processors.
NumThreads=0

# Minimal number of threads to spawm in multi-threaded tests:
MinThreads=2

# Minimal file access time subtracted from the current time:
MinFileTime=30 #  in minutes

# Minimal size to use a file in the read test (ddReadTest):
MinFileSize=512K

# Log to save the statistics in. I/O rates will be reported on screen as well.
MyLog=`pwd`/`echo $THIS | sed -e 's/.sh$//'`.log


#------------------------------------------------------------------------------

Usage()
{
  echo "
$BD$THIS$UB: disk I/O tests, Release $Release

Set of disk I/O tests:
${BD}ddWriteTest$UB creates the temp files using dd command (buffered writes);
${BD}ddReadTest$UB reads any existing files that were not recently accessed;
${BD}biWriteTest$UB writes on disk in unbuffered mode using proutil -C bigrow.

The tests run in the single- and multi-threaded mode.

${BD}Usage:$UB $THIS [dir] [options]
where options:
 \"dir\" is a directory where script will create temp files to test disk I/O.
 By default the current directory will be used.

 -h|-help is help.

 -n|-num Threads is the number of threads than will be spawn in the tests.
    Default value is the number of logical CPUs. Minimum is $MinThreads.

 -s|-size Size[K|M|G] is a basic size of the test disk activity.
    Default value is $TestSize. Max is a half of space available in filesystem.
    If the size for a particular test is not set explicitly then:
    -write (ddWriteTest) = Size
    -unbuf (biWriteTest) = Size / $biWriteFact
    -read  (ddReadTest)  = Size * $ddReadFact

 -w|-ws|-write Size[K|M|G] is the total size of temp files in ddWriteTest.

 -u|-us|-unbuf Size[K|M|G] is the total size of buffered writes in biWriteTest.

 -r|-rs|-read  Size[K|M|G] is the total size to read in ddReadTest.
  ddReadTest will be skipped if it fails to find at least $MinReadSize

 To skip any test just set its size to zero.

 -a|-amin AccessTime (in minutes). Default value is $MinFileTime.
  ddReadTest will try to find the files that were not recently accessed.
  
 The most recent version of the script can be downloaded from:
 ${BD}ftp://ftp.progress-tech.ru/pub/Users/george/Scripts/iotest.sh$UB" | \
  more

  exit
} # Usage()


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
# SizeInBytes converts a size value in KB, GB or TB to integer value in bytes.
# SizeInBytes returns an empty string if input value was incorrect.
# Kilobyte   = 1024 Bytes      
# Megabyte   = 1024 Kilobytes  
# Gigabyte   = 1024 Megabytes  
# Terabyte   = 1024 Gigabytes  
# Petabyte   = 1024 Terabytes  
# Exabyte    = 1024 Petabytes  
# Zettabyte  = 1024 Exabytes   
# Yottabyte  = 1024 Zettabytes 
# Brontobyte = 1024 Yottabytes 
# Geopbyte   = 1024 Brontobytes

SizeInBytes()
{
  Size=$1
  case $Size in
    *[bB]) Size=`echo $Size | sed -e 's/b$//; s/B$//'`
    ;;
    *k) Size=`echo $Size | sed -e 's/k$//'`
        Size=`expr $Size \* 1000 2>/dev/null`
    ;;
    *K) Size=`echo $Size | sed -e 's/K$//'`
        Size=`expr $Size \* 1024 2>/dev/null`
    ;;
    *m) Size=`echo $Size | sed -e 's/m$//'`
        Size=`expr $Size \* 1000000 2>/dev/null`
    ;;
    *M) Size=`echo $Size | sed -e 's/M$//'`
        Size=`expr $Size \* 1048576 2>/dev/null`
    ;;
    *g) Size=`echo $Size | sed -e 's/g$//'`
        Size=`expr $Size \* 1000000000 2>/dev/null`
    ;;
    *G) Size=`echo $Size | sed -e 's/G$//'`
        Size=`expr $Size \* 1073741824 2>/dev/null`
    ;;
    *)  Size=`expr $Size + 0 2>/dev/null`
    ;;
  esac  #case $Size

  echo $Size
}


#------------------------------------------------------------------------------
# SizeInUnits converts the size value to a short form (KB, GB or TB):

SizeInUnits()
{
# $1 is a size in bytes.
# Output is the size in KB, MB or GB.

  echo $1 | \
  $awk '
    $1>=1073741824 {Size=sprintf("%i",$1/107374182.4);print Size/10,"GB";exit}
    $1>=1048576    {Size=sprintf("%i",$1/104857.6);   print Size/10,"MB";exit}
    $1>=1024       {Size=sprintf("%i",$1/102.4);      print Size/10,"KB";exit}
                   {Size=$1;                          print Size,"Bytes";exit}
  ' #awk
} # SizeInUnits()


#------------------------------------------------------------------------------

IORate()
{
# IORate() takes the input from time command.
# $1 is a size of IO activity.
# Output is the ratio of size to the elapsed time (in KB/sec or MB/sec).


# Sample output of time:
# real  0m4.949s
# user  0m0.066s
# sys   0m0.476s
#
# time -p sleep 72
# real 72.00
# user 0.00
# sys  0.00

# The statistics reported by time are gathered from various system calls.
# User and Sys come from wait(2) or times(2), depending on the particular system.
# Real is calculated from a start and end time gathered from the gettimeofday(2).
# These calls have the different precision. For small times we can get:
# real  0m0.028s
# user  0m0.000s
# sys   0m0.035s
# The script will use real or sum of user and sys whatever is bigger.

  $awk '
    /^real/ {print}

    NF==2 {T=$2; M=0; S=0
# Minutes:
           i=index(T,"m")
           if(i>1) {M=substr(T,1,i-1); T=substr(T,i+1)}
# Seconds:
           i=index(T,".")
           if(i>1) {S=substr(T,1,i-1); T=substr(T,i+1)}
# Milliseconds:
           i=length(T)
           if(substr(T,i,1)=="s") T=substr(T,1,i-1)
           while(length(T)<3) T=T "0"
# Time in milliseconds:
           S+=M*60
           T+=S*1000
           if($1=="real") Time=T
                     else Rest+=T
    }
    END {if(Rest>Time) Time=Rest;
         if(Time==0) exit
         Time=Time/1000.0
         Rate='$1'/Time
         if(Rate>=1048576) {Rate=sprintf("%i",Rate/10485.76)
                            print Rate/100,"MB/sec";exit}
         if(Rate>=1024)    {Rate=sprintf("%i",Rate/10.24)
                            print Rate/100,"KB/sec";exit}
         if(Rate>0)        {print Rate,"Bytes/sec"; exit}
    } #END
  ' #awk
} # IORate()


#------------------------------------------------------------------------------

ddWriteTest()
{
  test $1 -gt 0 || return

  WriteSize=$1
  RunThreads=$2
  BlockCount=`expr $WriteSize / $BlockSize`
  BlockCount=`expr $BlockCount / $RunThreads`
  test $BlockCount -gt 0 || BlockCount=1
  WriteSize=`expr $BlockCount \* $BlockSize`
  WriteSize=`expr $WriteSize \* $RunThreads`

# Check if there the files with names that script is going to create:
  Thread=$RunThreads
  while [ $Thread -gt 0 ]
  do
    TmpFile=$TmpPrefix$Thread
    test -f $TmpFile && \
    echo2 ddWriteTest did not expect to find $TmpFile. The test is skipped.
    Thread=`expr $Thread - 1`
  done

  Size=`SizeInUnits $WriteSize`
  echo2 "Writing $Size by $RunThreads threads (dd if=/dev/zero of=Tmp)..."

  Thread=$RunThreads
 (while [ $Thread -gt 0 ]
  do
    TmpFile=${TmpPrefix}.$Thread
    dd if=/dev/zero of=$TmpFile bs=$BlockSize count=$BlockCount && \
    rm $TmpFile &
    Thread=`expr $Thread - 1`
  done >/dev/null 2>&1

  time wait
 ) 2>&1 | \
  IORate $WriteSize | \
  while read Line
  do
    echo2 "$Line"
  done

#FileSize=`ls -l ${TmpPrefix}* | SizeOfFiles`
#echo2 "Total size of temp files ($TmpPrefix) is $FileSize bytes."

  EndOfSection

} # ddWriteTest()


#------------------------------------------------------------------------------
# ddReadTest finds the set of the files that were not recently accessed
# and uses these files to test the speed of reads from the disks.

ddReadTest()
{
  test $1 -lt $MinReadSize && return

  TestSize=$1
  RunThreads=$2

# Find the files with size at least MinFileSize
# and accessed at least $MinFileTime minutes ago.
# Stop the find if the total size of the found files is enough to the tests.
# The find command will be stopped either by SIGPIPE or by StopFlag.

# The list of the files choosen for the read test (in format of ls -l):
  FileList=$TmpPrefix.FileList

# StopFlag file will contain the total size of the found files.
  StopFlag=$TmpPrefix.StopFlag

  $echo "Searching for the files to use in read test...\r\c"

  find $TestDir -type f \
    -size +$MinFileSize \
    -amin +$MinFileTime \
    -exec test -r {} \; \
    -exec test ! -f $StopFlag \; \
    -exec ls -lu {} \; 2>/dev/null | \
  awk '
    BEGIN        {StopFlag="'$StopFlag'"
                  Limit="'$TestSize'"; Limit+=0} # to interpret as integer
    NF>=9        {Total+=$5; print}  # $5 in ls output is a file size
    Total>=Limit {exit}
    END          {print Total >StopFlag}
  ' >$FileList

  CleanCurrentLine

  TotalSize=`test -f $StopFlag && cat $StopFlag && rm $StopFlag`

  if [ -z "$TotalSize" ]
  then
    echo2 ddReadTest failed to find the files for read test. The test is skipped.
    rm $FileList
    return
  fi

  if [ $TotalSize -lt $MinReadSize ]
  then
    Size=`SizeInUnits $TotalSize`
    echo2 ddReadTest found only $Size to read. The test is skipped.
    echo "FileList:" >>$MyLog
    cat  $FileList   >>$MyLog
    rm   $FileList
    return
  fi

# TotalSize=`cat $FileList | SizeOfFiles`

  test $TotalSize -gt $TestSize && \
  TotalSize=$TestSize
  TotalBlocks=`expr $TotalSize / $BlockSize`

# The number of file blocks to read by each thread:
  ThreadBlocks=`expr $TotalBlocks / $RunThreads`

  PlanPrefix=$TmpPrefix.ReadPlan.

# Check if there the files with names that script is going to create:
  Thread=$RunThreads
  while [ $Thread -gt 0 ]
  do
    ReadPlan=$PlanPrefix$Thread
    if [ -f $ReadPlan ]
    then
      echo2 ddReadTest did not expect to find $ReadPlan. The test is skipped.
      rm $FileList
      return
    fi
    Thread=`expr $Thread - 1`
  done

# Create the read plans (ReadPlan) for each thread.
# Read plan contains the parameters for dd command to run by thread:
# if=    Input file to read from;
# skip=  The number of blocks to skip in the input file;
# count= The number of blocks to read from the input file.

  PlanCount=`
    $awk '
      BEGIN {
        BlockSize='$BlockSize'
        RunThreads='$RunThreads'
        ThreadBlocks='$ThreadBlocks'
        PlanPrefix="'$PlanPrefix'"
        PlanCount=1
        PlanFile=PlanPrefix PlanCount
        PlanBlocks=ThreadBlocks
      }
# The output of ls -l should have 9 fields:
      NF<9 {next}
      
# Assign the file to the threads"
      { FileSize=$5
        FileName=$NF
        SkipBlocks=0
        FileBlocks=FileSize/BlockSize
        
# File has more blocks than the current thread needs:
        while(FileBlocks>=PlanBlocks)
        { #print FileName " " SkipBlocks " " PlanBlocks >PlanFile
          printf"%s %s %s\n",FileName,SkipBlocks,PlanBlocks >PlanFile
          FileBlocks-=PlanBlocks
          SkipBlocks+=PlanBlocks
        
# Found blocks were enough for all therads:
          if(PlanCount==RunThreads) exit
        
          close(PlanFile)
          PlanCount+=1
          PlanFile=PlanPrefix PlanCount
          PlanBlocks=ThreadBlocks
        } # while(FileBlocks>0)
        
# If file is small then thread will read all its blocks (FileBlocks<PlanBlocks):
        PlanBlocks-=FileBlocks
#       print FileName " " SkipBlocks " " FileBlocks >PlanFile
        printf"%s %s %s\n",FileName,SkipBlocks,FileBlocks >PlanFile
      }
      
      END {print PlanCount}
    ' $FileList` # $awk
# PlanCount

# In case if the script failed to create the read plans for all threads:

  RunThreads=$PlanCount
  TotalBlocks=`expr $ThreadBlocks \* $RunThreads`
  TotalSize=`expr $TotalBlocks \* $BlockSize`

  Size=`SizeInUnits $TotalSize`
  echo2 "ddReadTest reads $Size by $RunThreads threads (dd if=File of=/dev/null)..."

# Debug:
for f in `ls -1 ${PlanPrefix}*`
do
 echo ReadPlan=$f
 cat $f
done >>$MyLog

  Thread=$RunThreads
 (while [ $Thread -gt 0 ]
  do
    ReadPlan=$TmpPrefix.ReadPlan.$Thread
    cat $ReadPlan | \
    while read File Skip Count
    do
      dd if=$File of=/dev/null bs=$BlockSize skip=$Skip count=$Count
    done 2>>$MyLog &
    rm $ReadPlan
    Thread=`expr $Thread - 1`
  done >/dev/null 2>&1

  time wait
 ) 2>&1 | \
  IORate $TotalSize | \
  while read Line
  do
    echo2 "$Line"
  done

  echo "List of the files used by ddReadTest:" >>$MyLog
  cat $FileList >>$MyLog
  rm $FileList

  EndOfSection

} # ddReadTest()


#------------------------------------------------------------------------------
# Create temp databases for bi write tests:

CreateTmpDb()
{
  test $1 -gt 0 || return

  DbCount=$1

  echo2 "Creating $DbCount database(s) for bi write tests..."

 (time \
  while [ $DbCount -gt 0 ]
  do
    TmpDb=$DbPrefix$DbCount
    $DLC/bin/prodb $TmpDb $DLC/empty1     >/dev/null 2>&1 || \
    echo "Failed to run: $DLC/bin/prodb $TmpDb $DLC/empty1" >>$MyLog
    DbCount=`expr $DbCount - 1`    
  done
 ) 2>&1 | \
  grep "real" | \
  while read Line
  do
    echo2 "$Line"
  done

  Size=`ls -l ${DbPrefix}* | SizeOfFiles`
  Size=`SizeInUnits $Size`
  echo2 "Total size of temp db files is $Size."

  EndOfSection

} # CreateTmpDb()


#------------------------------------------------------------------------------
# Delete temp databases:

DeleteTmpDb()
{
  DbCount=$1

  while [ $DbCount -gt 0 ]
  do
    TmpDb=$DbPrefix$DbCount
    echo y | $DLC/bin/prodel $TmpDb
    rm -f $TmpDb.st
    DbCount=`expr $DbCount - 1` 
  done >/dev/null 2>&1

} # DeleteTmpDb()


#------------------------------------------------------------------------------
# biWriteTest: the buffered (-r) or unbuffered (O_DSYNC) synchronous bi writes

# KB-P108815: Does Progress use Synchronous or Asynchronous I/O?
#
# bi writes    = unbuffered synchronous I/O (O_RDWR|O_DSYNC)
# -r (non-raw) = buffered synchronous I/O (O_RDWR)
# db writes    = buffered synchronous I/O (O_RDWR)
# -directio    = unbuffered synchronous I/O (O_RDWR|O_DSYNC)
#
# fd = open("/path/to/dir", O_RDWR [, mode_t mode])
# 
# O_DSYNC
# Write operations on the file will complete according to the
# requirements of synchronized I/O data integrity completion.
#
# By the time write(2) (and similar) return, the output data has
# been transferred to the underlying hardware, along with any
# file metadata that would be required to retrieve that data
# (i.e., as though each write(2) was followed by a call to
# fdatasync(2)).
#
# KB-P97412: As of OpenEdge 10.0B, Progress uses the  fdatasync() call.


biWriteTest()
{
  test $1 -gt 0 || return

  TestSize=$1
  NumDbs=$2
  Option="$3"

  BiBlockSize=`expr $BlockSize / 1024`

# WriteSize will be splitted in 8 bi clusters for $NumDbs databases:
  BiClSize=`expr $TestSize / 8192`
  BiClSize=`expr $BiClSize / $NumDbs`

# BiClSize must be a multiple of 16 ranging from 16 to 262128 (16K to 256MB)
  test $BiClSize -lt 16     2>/dev/null && BiClSize=16
  test $BiClSize -gt 262128 2>/dev/null && BiClSize=262128

  TestSize=`expr $BiClSize \* $NumDbs`
  TestSize=`expr $TestSize \* $BlockSize`

  test "Z$Option" = "Z" && \
  Mode="unbuffered (O_DSYNC)" || \
  Mode="buffered ("${Option}")"

  Bigrow=`expr $TestSize / $BiClSize`
  Bigrow=`expr $Bigrow / 1024`
  Bigrow=`expr $Bigrow / $NumDbs`
  Bigrow=`expr $Bigrow - 4`  #Minus default 4 clusters
  test $Bigrow -lt 0 && Bigrow=0

# Truncate bi (just in case if databases were used in previous tests):
  DbCount=$NumDbs
  while [ $DbCount -gt 0 ]
  do
    TmpDb=$DbPrefix$DbCount
    $DLC/bin/_proutil $TmpDb -C truncate bi \
      -biblocksize $BiBlockSize \
      -bi $BiClSize \
     -cpinternal undefined -cpcoll basic >/dev/null 2>&1
    DbCount=`expr $DbCount - 1`    
  done

  Size=`SizeInUnits $TestSize`

  echo2 "Writing $Size in $Mode mode by $NumDbs threads (biWriteTest)."

  DbCount=$NumDbs
 (while [ $DbCount -gt 0 ]
  do
    TmpDb=$DbPrefix$DbCount
    $DLC/bin/_proutil $TmpDb -C bigrow $Bigrow $Option \
      -cpinternal undefined -cpcoll basic &
    DbCount=`expr $DbCount - 1`    
  done >/dev/null 2>&1

  time wait
 ) 2>&1 | \
  IORate $TestSize | \
  while read Line
  do
    echo2 "$Line"
  done

  EndOfSection

} # biWriteTest()


#------------------------------------------------------------------------------

NumOfCPUs()
{
 OSNAME=`uname`
 case "$OSNAME" in
  "Linux") # -----------------------------------------------

     nproc 2>/dev/null || \
     dmesg 2>/dev/null | grep CPUs | $awk '{print $(NF-1)}'

#    lscpu 2>/dev/null | $awk '/CPU\(s\)/ {print $NF}')

# dmesg
# Initializing CPU#0
# Total of 1 processors activated (3605.17 BogoMIPS).
# Brought up 1 CPUs

# nproc - print the number of processing units available
# Sample outputs:
# 
# 8

# lscpu gathers CPU architecture information from sysfs and /proc/cpuinfo.
# Sample outputs:
# 
# Architecture:          x86_64
# CPU op-mode(s):        32-bit, 64-bit
# Byte Order:            Little Endian
# CPU(s):                8
# On-line CPU(s) list:   0-7
# Thread(s) per core:    1
# Core(s) per socket:    4
# CPU socket(s):         2
# NUMA node(s):          1
# Vendor ID:             GenuineIntel
# CPU family:            6
# Model:                 15
# Stepping:              7
# CPU MHz:               1866.669
# BogoMIPS:              3732.83
# Virtualization:        VT-x
# L1d cache:             32K
# L1i cache:             32K
# L2 cache:              4096K
# NUMA node0 CPU(s):     0-7
   ;;

  "AIX") # -------------------------------------------------

# AIX 5.1 and higher:

     pmcycles -m 2>/dev/null | wc -l

# sar 1 1
# Sample output:
# 
# AIX ren-mskosi01 1 6 00F6483D4C00    07/04/12
# System configuration: lcpu=32 ent=0.80 mode=Uncapped 
   ;;

  "SunOS") # -----------------------------------------------

     psrinfo 2>/dev/null | wc -l

# psrinfo displays information about processors.
#
# number of physical cpu: "psrinfo -p"
# number of cores: "kstat cpu_info|grep core_id|sort -u|wc -l"
# number of threads: "psrinfo -pv"
#
# Sample outputs:
# 
# psrinfo -p
# 2
#
# root@pbiudb01 # psrinfo -pv
# The physical processor has 64 virtual processors (0-63)
#   UltraSPARC-T2+ (cpuid 0 clock 1165 MHz)
# The physical processor has 64 virtual processors (64-127)
#   UltraSPARC-T2+ (cpuid 64 clock 1165 MHz)
# root@pbiudb01 # kstat cpu_info|grep core_id|sort -u|wc -l
#       16
#
# psrinfo -p <= socket(s)
# 2
#
# kstat -m cpu_info|grep -w core_id|uniq|wc -l <= core(s)
# 8
#
# psrinfo|wc -l <= logical (virtual) processor(s)
# 64
   ;;

  "HP-UX")

     machinfo 2>/dev/null | grep -i CPUs | $awk '{print $5}'

# the number of processor cores, not physical chips:
# ioscan -kfnC processor | grep processor | wc -l
# 4
# sar -Mu 1 1 | awk 'END {print NR-5}'
# 4
#
# ioscan -fk |grep -c processor
# 4
#
# cat /var/adm/syslog/syslog.log|grep processor|wc -l
# 4
# ioscan -k -C processor
# H/W Path Class Description
# ===================================
# 0/120 processor Processor
# 0/121 processor Processor
#
# machinfo
# CPU info:
#   8 s (1.6 GHz, 20 MB)
#           4794 MT/s bus, CPU version 4
#           32 logical processors (4 per socket)
# 
  ;;
esac

} # NumOfCPUs()


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
# echo2 displays the messages on screen as well as copies them to my log file:

echo2()
{
  echo "$*"
  echo "$*" | AddCurrTime  >>$MyLog
} # echo2()


#------------------------------------------------------------------------------
# Run the system commands to monitor the disk and CPU activity:

RunSysMon()
{

  LogPrefix=$1

  MonIntrv=1
  MonCount=3600
  OSNAME=`uname`
  case "$OSNAME" in

   "Linux")    SysMonCmd1="vmstat $MonIntrv $MonCount"
               SysMonCmd2="iostat -k -t $MonIntrv $MonCount"
    ;;
   "SunOS")    SysMonCmd1="sar -u $MonIntrv $MonCount"
               SysMonCmd2="iostat -d -T d $MonIntrv $MonCount"
    ;;
   "HP-UX")    SysMonCmd1="vmstat $MonIntrv $MonCount"
               SysMonCmd2="iostat $MonIntrv $MonCount"
    ;;
   "AIX")      SysMonCmd1="vmstat -t $MonIntrv $MonCount"
               SysMonCmd2="iostat -T $MonIntrv $MonCount"
    ;;
   "OpenUNIX") SysMonCmd1="sar -u $MonIntrv $MonCount"
               SysMonCmd2="sar -d $MonIntrv $MonCount"
    ;;
   "UnixWare") SysMonCmd1="sar -u $MonIntrv $MonCount"
               SysMonCmd2="sar -d $MonIntrv $MonCount"
    ;;
   "SCO_SV")   SysMonCmd1="sar -u $MonIntrv $MonCount"
               SysMonCmd2="sar -d $MonIntrv $MonCount"
    ;;
   "OSF1")     SysMonCmd1="vmstat $MonIntrv $MonCount"
               SysMonCmd2="iostat $MonIntrv $MonCount"
    ;;
    *)         SysMonCmd1="vmstat $MonIntrv $MonCount"
               SysMonCmd2="iostat $MonIntrv $MonCount"
    ;;
  esac

  SysMonLog=${LogPrefix}.1
  echo "$SysMonCmd1" | AddCurrTime  >$SysMonLog
  $SysMonCmd1 2>&1   | AddCurrTime >>$SysMonLog &
  echo $!

  SysMonLog=${LogPrefix}.2
  echo "$SysMonCmd2" | AddCurrTime  >$SysMonLog
  $SysMonCmd2 2>&1   | AddCurrTime >>$SysMonLog &
  echo $!

} # RunSysMon()


#------------------------------------------------------------------------------
# Exit if the input parameters were incorrectly specified:

Exit()
{
  echo $THIS: Error: $*
  exit 1
} # Exit()


#------------------------------------------------------------------------------
# CleanCurrentLine: clean the current line on the screen.

CleanCurrentLine()
{
  $echo \
"                                                                         \r\c"
} # CleanCurrentLine()


#------------------------------------------------------------------------------
# EndOfSection: put a dividing line on the screen and an empty line in MyLog.

DividingLine=\
"-----------------------------------------------------------------------------"

EndOfSection()
{
  echo $DividingLine
  echo "" >>$MyLog
} # EndOfSection()


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

while [ $# -gt 0 ]
do
  case $1 in
   -h|-help) Usage
   ;;

   -n|-num) NumThreads=$2
       test "$NumThreads" -gt 0 >/dev/null 2>&1 || \
       Exit "The -n option should be followed by numeric argument > 0."
       shift
   ;;

   -s|-size)      TestSize=$2;    shift;;
   -w|-ws|-write) ddWriteSize=$2; shift;;
   -u|-us|-unbuf) biWriteSize=$2; shift;;
   -r|-rs|-read)  ddReadSize=$2;  shift;;
   -a|-amin)   MinFileTime=$2; shift;;

   -*) Exit "$1 is an unknown option. Use $THIS -h for a help.";;

   *) TestDir=$1;;

  esac  #case $1

  shift
done

#------------------------------------------------------------------------------
# Sanity checks of the script's input parameters:

# Is TestDir exist?: ------------------------------------------

test -d $TestDir || \
Exit "Directory $TestDir does not exist."

# Full pathname: ----------------------------------------------

TestDir=`(cd $TestDir; pwd) 2>/dev/null`

# Is TestSize valid?: -----------------------------------------

TestSize=`SizeInBytes $TestSize`
test "$TestSize" || \
Exit "The -size option has an incorrect value."

# Don't allow for tests to use more than a half of available space on filesystem:
OSNAME=`uname`
case "$OSNAME" in

 "Linux")
# df -k
# Filesystem           1K-blocks      Used Available Use% Mounted on
# /dev/mapper/VolGroup00-LogVol00
#                       19172036  11528128   6654316  64% /
# /dev/sda1               101086     11870     83997  13% /boot

    AvailKB=`df -k $TestDir | $awk 'NF>=5 {Avail=$(NF-2)}; END {print Avail}'`
  ;;
 "AIX")
# df -k
# Filesystem    1024-blocks      Free %Used    Iused %Iused Mounted on
# /dev/lvusr1      51773440   3120000   94%   343681    25% /usr1

    AvailKB=`df -k $TestDir | $awk 'NF>=5 {Avail=$(NF-4)}; END {print Avail}'`
  ;;
  *) # I don't have the output of df -k. Just allow to run the tests:
    AvailKB=$TestSize
  ;;
esac

test `expr $TestSize / 512` -gt $AvailKB && \
TestSize=`$AvailKB \* 512`

# Total size of writes during ddWriteTest:
test "$ddWriteSize" && \
ddWriteSize=`SizeInBytes $ddWriteSize` || \
ddWriteSize=$TestSize
# If SizeInBytes returns an empty string:
test "$ddWriteSize" || \
Exit "The -write option has an incorrect value."

# Total size of writes during biWriteTest:
test "$biWriteSize" && \
biWriteSize=`SizeInBytes $biWriteSize` || \
biWriteSize=`expr $TestSize / $biWriteFact`
# If SizeInBytes returns an empty string:
test "$biWriteSize" || \
Exit "The -buffered option has an incorrect value."

# Total size of reads during ddReadTest:
test "$ddReadSize" && \
ddReadSize=`SizeInBytes $ddReadSize` || \
ddReadSize=`expr $TestSize \* $ddReadFact`
# If SizeInBytes returns an empty string:
test "$ddReadSize" || \
Exit "The -read option has an incorrect value."

# Minimal size to run the read test (ddReadTest):
MinReadSize=`SizeInBytes $MinReadSize`
test "$MinReadSize" || \
Exit "The MinReadSize variable has an incorrect value."

# Blocksize (in KB) to use for reads and writes:
BlockSize=`SizeInBytes $BlockSize`
test "$BlockSize" || \
expr $BlockSize -lt 1024 || \
Exit "The BlockSize variable has an incorrect value."

# Minimal size to use a file in the read test (ddReadTest):
MinFileSize=`SizeInBytes $MinFileSize`
test "$MinFileSize" || \
Exit "The MinFileSize variable has an incorrect value."
# Size in blocks (used by the -size option of find command):
MinFileSize=`expr $MinFileSize / 512`
# By default the number of threads match the number of logical processors:
test $NumThreads -eq 0 && \
NumThreads=`NumOfCPUs`

test "$NumThreads" -gt $MinThreads 2>/dev/null || \
NumThreads=$MinThreads


#------------------------------------------------------------------------------

# Main block:

# Set English locale to get the command's messages in English:

EnglishLC=`locale -a 2>/dev/null | $awk '/^en_/ {print; exit}'`
EnglishLC=${EnglishLC-"en_US"}
LC_TIME=$EnglishLC;     export LC_TIME
LC_MESSAGES=$EnglishLC; export LC_MESSAGES
echo Locale is changed to $EnglishLC

# Results of the tests will be saved in MyLog:

# Log Header/Introduction:

echo2 $THIS $Release
echo2 "   Date:" `date '+%a %b %e %T %Y'`
echo2 "   Host:" `uname -a`
echo2 " Uptime:" `uptime | $awk '{$1=""; print}'`
echo2 "    DLC:" $DLC `test -x $DLC/bin/_proutil || echo " (not found)"`
echo2 "Threads:" $NumThreads
echo2 "TestSiz:" `expr $TestSize / 1024` KB
echo2 "TestDir:" $TestDir
df -k $TestDir
df -k $TestDir >>$MyLog

EndOfSection

# Prefix for the names of temp files:

TmpPrefix=$TestDir/$THIS.$$

# Run system monitoring:

MonPrefix=${TmpPrefix}.monitor
MonPIDs=`RunSysMon $MonPrefix`

$echo \
"Gathering the current system statistics... Test will begin in 2 seconds.\r\c"
sleep 2
CleanCurrentLine

ddWriteTest $ddWriteSize 1
ddWriteTest $ddWriteSize $NumThreads

if [ -x $DLC/bin/_proutil ]
then

  PATH=$DLC/bin:$PATH
  export PATH

# The dot is not a legal character for a database name.
# Database names (without .db) must not be longer than 11 characters:

  DbPrefix=$TestDir/db`expr $$ % 65536`_

# For safety - interrupt the tests if there are the files that match DbPrefix:
  ls $DbPrefix* 2>/dev/null && \
  Exit There are the files matched ${DbPrefix}. Can not create the temp databases.

  test $ddWriteSize -gt 0 || \
  test $biWriteSize -gt 0 && \
  CreateTmpDb $NumThreads

# With the -r option use ddWriteSize rather than biWriteSize:
  biWriteTest $ddWriteSize 1           -r
  biWriteTest $ddWriteSize $NumThreads -r

  biWriteTest $biWriteSize 1
  biWriteTest $biWriteSize $NumThreads

  DeleteTmpDb $NumThreads
fi

ddReadTest $ddReadSize $NumThreads


# Let to complete the last interval of system monitoring:
$echo "Test will be completed in 2 seconds...\r\c"
sleep 2
CleanCurrentLine
 
# Stop gathering the system statistics:
for MonPID in $MonPIDs
do
  kill $MonPID 2>/dev/null
done

# Copy the system statistics to my log:
for MonLog in `ls -1 ${MonPrefix}.*`
do
  echo $DividingLine
  cat $MonLog && rm -f $MonLog
done  >>$MyLog

echo $DividingLine >>$MyLog

$echo The results are saved in $BD$MyLog$UB
