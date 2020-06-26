#!/bin/sh

THIS=`basename $0`

# emon.sh - shell script to collect Progress and system statistics
# in case of performance degradation or during any incidents.
# Script checks all Progress databases and all Progress processes
# running on the box.
# Written by George Potemkin, Mar 24, 2012
# Last modified:

Release="V1.2 as of Jun 17, 2012"

# Fixes:
# V1.1: Added the copying of the AppServer's logs (based on ubroker.properties);
# V1.1: Interval count for vmstat/iostat on Linux is increased by 1;
# V1.1: Report java version;
# V1.1: Zero is now a valid value for monitoring duration;
# V1.2: Changes in SysEnvInfo();
# V1.2: Added date statistics for the files in PROPATHs;
# V1.2: Db list is sorted by the number of processes connected to shared memory.

#------------------------------------------------------------------------------
Usage()
{
  echo "$BD$THIS$UB: System and Database Monitoring, Release $Release

Usage: $THIS [-h|help] [-t time] [-db pattern] [-F] [SaveDir]
where:
 -h is help.

 -t \"time\" is a monitoring duration.
 The entire interval will be divided into a few ($MonCount) subintervals.
 Default duration is $MonDuration sec. The minimal value is $MinMonDuration.

 -db \"pattern\" is a pattern to select the databases to monitor.
 The pattern will be applied to the full db path.

 -F is Promon Force Access.
 Start the promon utility with the -F option to connect the databases
 without using the login semaphore. Use this option only when processes
 can't connect the databases in the normal way.

 \"SaveDir\" is the directory to save the results.
 By default the logs will be saved in the current directory.
" >&2
  exit
} # Usage()


# Default values of input parameters:
# ----------------------------------
MonDuration=60
SaveDir="."

# ************ Environment **************************************************
# ---------------------------------------------------------------------------

# Set PROPATH used by the application:
# It will be used to collect the stats of r-code dates.
# $DLC/tty is just an example.
PROPATH=$DLC/tty

# The list of directories to search the protrace files:
WrkDirList=""

# Set Progress environment:
#
#DLC=/usr/wsrt_64; export DLC
#PATH=$DLC/bin:$PATH; export PATH


# ************ System Commands **********************************************
# ---------------------------------------------------------------------------
# List of OS specific commands to monitor the system activity.
# Add the commands available on your Unix.
# 
# %i will be substituted by monitoring interval ($MonIntrv).
# %c will be substituted by interval count ($MonCount).

OSNAME=`uname`
case "$OSNAME" in

 "Linux")    CmdList="vmstat %i %c; iostat -k -t %i %c"
  ;;
 "SunOS")    CmdList="sar -u %i %c; iostat -d -T d %i %c"
#            CmdList="vmstat %i %c; iostat -d -T d %i %c"
  ;;
 "HP-UX")    CmdList="vmstat %i %c; iostat %i %c"
  ;;
 "AIX")      CmdList="vmstat -t %i %c; iostat -T %i %c"
  ;;
 "OSF1")     CmdList="vmstat %i %c; iostat %i %c"
  ;;
 "OpenUNIX") CmdList="sar -u %i %c; sar -d %i %c"
  ;;
 "UnixWare") CmdList="sar -u %i %c; sar -d %i %c"
  ;;
 "SCO_SV")   CmdList="sar -u %i %c; sar -d %i %c"
  ;;
  *)         CmdList="sar -u %i %c; sar -d %i %c; vmstat %i %c; iostat %i %c"
  ;;
esac


# Internal parameters:
# -------------------

# Minimal time to monitor the system:
MinMonDuration=6

# The number of sampling intervals:
MonCount=3

# The options to launch the promon sessions:
PromonOptions="-NL"

# Avoid of simultanious connections if promon is started with -F:
DelayNextPromon="n"

# Max number of lines from db logs to copy to an archive:
DbLogLimit=100000

# Max number of lines from appserver's (etc) logs to copy to an archive:
UBrokerLogLimit=10000


# Bold/unbold terminal codes:
BD=`tput smso`
UB=`tput rmso`

# Clean the current line:
Clean="                                                                    \r\c"


# Is it run with root privileges?
if [ ! -w /etc/passwd ]
then
  echo "It's recommended to run the script as ${BD}root${UB}. Execution will continue..." >&2
  sleep 2
fi

# Sanity check: Is DLC set correctly?
# ----------------------------------
test "$DLC" && \
test -d $DLC && \
test -x $DLC/bin/_mprosrv && \
test -x $DLC/bin/_proutil

if [ $? -ne 0 ]; then
  echo "Incorrect value of DLC=$DLC" >&2
  exit 2
fi

# Sanity check: Is $DLC/bin in PATH?
#
type _dbutil >/dev/null 2>&1 || PATH=$DLC/bin:$PATH && export PATH


# Choosing the versions of basic Unix commands: echo and awk
# ----------------------------------------------------------
OSNAME=`uname`

case "$OSNAME" in
 "Linux") echo="echo -e";;
       *) echo="echo";;
esac

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


Exit()
{
  echo $THIS: Error: $*
  exit 1
} # Exit()


# Parsing the script's input parameters:
# -------------------------------------
EmonParams="$*"
while [ $# -gt 0 ]
do
  case $1 in
   -h|-help) Usage
   ;;

# Monitoring duration: -t Time
   -t)
# The next argument should be integer:
    expr $2 + 0 2>&1 >/dev/null || \
    Exit "The -t option should be followed by numeric argument."
    MonDuration=$2
    shift
   ;;

# Monitoring duration: -tTime (without space in-between)
   -t[0-9]*)
# Cut off the "-t" prefix:
    MonDuration=`echo $1 | sed -e 's/^-t//'`
   ;;

# Pattern to select databases to monitor:
   -db) DbPattern=$2; shift
   ;;

# Promon Force Access:
   -F) PromonOptions="$PromonOptions -F"
       DelayNextPromon="y"
   ;;

# The unknown parameter:
   -*) Exit "$1 is an unknown option. Use $THIS -h for a help."
   ;;

# SaveDir:
   *) test -d $1 || Exit "Directory $1 does not exist."
      SaveDir=$1
   ;;

  esac  #case $1

  shift
done

# Full path to the save directory:
SaveDir=`(cd $SaveDir; pwd)`


# Create temp dir for the logs:
# ----------------------------

# Where and when the script were run:
RunId=`uname -n`.`date '+%y%m%d_%H%M%S'`

TempDir=$SaveDir/emon.$RunId
mkdir $TempDir
test -d $TempDir || Exit "Failed to create a temp dir in $SaveDir"

case $MonDuration in
 0) MonIntrv=0
    MonCount=0
 ;;
 *) test $MonDuration -eq 0 || \
    test $MonDuration -lt $MinMonDuration && \
    MonDuration=$MinMonDuration

# Sampling interval:
    MonIntrv=`expr $MonDuration / $MonCount`
    MonDuration=`expr $MonIntrv \* $MonCount`
 ;;
esac

# ************ Procedures *****************************************************

#------------------------------------------------------------------------------
# Convert path to a name.

Path2Name()
{
# Remove the first slash and replace next slashes by tilde:

 echo $1 | sed -e 's/^\///' | tr -s "\/" "~"
} # Path2Name()


#------------------------------------------------------------------------------
# Echo with timestamp.
#
echoT()
{
 echo "`date '+%H:%M:%S:'` $*" | tee -a $EmonLog 2>/dev/null
} # echoT()


#------------------------------------------------------------------------------
# Echo system Command and run it.

Run()
{
echo "
----------------------------------------------------------------------------
$*"
$* 2>&1 | tail -1000
}


#------------------------------------------------------------------------------
# Progress Environment Information.

ProEnvInfo()
{
 echo DLC=$DLC
 cat $DLC/version
 PROCFG=${PROCFG:-$DLC/progress.cfg}
 echo "Configuration File: " $PROCFG
 $DLC/bin/showcfg $PROCFG | grep -i "Product Name:"
 $DLC/bin/showcfg $PROCFG | grep -i "Port Number" | uniq

# From $DLC/bin/java_env:
# Full list of the platform ids:
# 11) PortBit="Solaris-32bit";;
# 39) PortBit="Solaris-64bit";;
# 17) PortBit="AIX-32bit";;
# 37) PortBit="AIX-64bit";;
# 27) PortBit="Tru64-64bit";;
# 34) PortBit="HPUX-32bit";;
# 36) PortBit="HPUX-64bit";;
# 40) PortBit="HPUX-64bit-Itanium";;
# 38) PortBit="Linux-32bit";;
# 43) PortBit="Linux-64bit";;
# 31) PortBit="Windows-32bit";;
# 41) PortBit="Unixware-32bit";;
# 45) PortBit="LinuxPowerPC-64bit";;

# Type of Progress executables:
#  SunOS: ELF 64-bit | ELF 32-bit
#  HP-UX: ELF-64 executable
#  Linux: ELF 64-bit | ELF 32-bit
 Run file $DLC/bin/_mprosrv

 Run cat $DLC/startup.pf
 Run cat $DLC/installd.ini
} # ProEnvInfo()


#------------------------------------------------------------------------------
# System Environment Information.

SysEnvInfo()
{
 uname -a
 echo "uptime: `uptime`"
 echo "User: `who am i`"
 id

 Run type java
 java -version
 
 OSNAME=`uname`
 case "$OSNAME" in
  "Linux") # -----------------------------------------------

# OS version/patches:

# /etc/SuSE-release
# /etc/debian_version
     for File in `ls -1d /etc/*release* /etc/*version* 2>/dev/null`
     do
# test if it's a file (e.g. /etc/lsb-release.d is dir):
       test -f $File && test -r $File && Run cat $File
     done

# Kernel:

# /proc/sys/kernel/shmmax - The maximum size of a shared memory segment.
# /proc/sys/kernel/shmmni - The maximum number of shared memory segments.
# /proc/sys/kernel/shmall - The maximum amount of shared memory.
# /proc/sys/kernel/sem - The maximum number and size of semaphore sets.
# SEMMSL - The maximum number of semaphores in a sempahore set.
# SEMMNS - The maximum number of sempahores in the system.
# SEMOPM - The maximum number of operations in a single semop call.
# SEMMNI - The maximum number of sempahore sets.
     for File in shmmax shmmni shmall sem
     do
       test -r $File && echo $File: `cat /proc/sys/kernel/$File`
     done

# Memory: 
     Run cat /proc/meminfo
     Run free -m  # -m for MB

# CPU:
     Run cat /proc/cpuinfo

# Devices:
     Run cat /proc/devices
     Run lsdev
# all PCI devices
     Run lspci -v
     Run cat /proc/scsi/scsi

# Filesystems:
     Run mount
     Run df -h
     Run cat /etc/fstab

     df | \
     $awk 'BEGIN {getline}; /^\// {print $1}' | \
     while read fs
     do
       Run dumpe2fs -h $fs
     done

# Boot messages:
     Run dmesg 
   ;;

 
  "AIX") # -------------------------------------------------
#Aix useful commands
#http://www.tek-tips.com/faqs.cfm?fid=3180

# OS version/patches:

echo "
----------------------------------------------------------------------------
Machine type:" `uname -MuL`

# The option -M gives the machine type and model.
# -u gives the plant code and machine identifier.
# -L show the LPAR number and name.
# 
# Example: uname -MuL
# IBM,9113-550 IBM,0210A463E 1 10-A463E 
# 
# The Machine type is 9113
# The Model is 550
# OF prefixm IBM, 0210A463E
# Plant Code, 1
# Sequence number, 10-A463E

     echo "oslevel:" `oslevel`
#     4 . 3 . 3 . 0 <--------- Preventive Maintenance Level
#     |   |   |
#     |   |   +----------------Modification
#     |   +--------------------Release
#     +------------------------Version
     echo "highest technology level:" `oslevel -r`

# Kernel: None

# Memory:
echo "
----------------------------------------------------------------------------
lsattr -El"
lsattr -El sys0 | grep realmem  #Real memory on the system
# CPU:
lsattr -El proc0 | $awk '/type/ {print $2}' #Processor type

     Run pmcycles -m
     Run sar -u -P ALL 1 2

# Devices:
# This command shows basic hardware and configuration details.
# Cut off Network Information:
     Run lsconf | \
     grep -vi "IP Address" | \
     grep -vi "Sub Netmask" | \
     grep -vi "Gateway" | \
     grep -vi "Name Server"

# Filesystems: 
     Run mount

# lsfs -q Displays additional Journaled File System (JFS) or Enhanced Journaled
# File System (JFS2) characteristics specific to the file system type. 
     Run lsfs -q
     Run df -kg

# Boot messages:
     for File in /etc/tunables/lastboot /etc/tunables/lastboot.log
     do
       test -r $File && Run cat $File
     done
   ;;


  "SunOS") # -----------------------------------------------

# Kernel:
      sysdef -i | grep -i shm

# Memory: 
     /usr/sbin/prtconf | grep Memory

# CPU:

# psrinfo displays information about processors.
# -v - additional information about the specified processors, including:
# processor type, floating point unit type and clock speed.
     Run psrinfo -v

# Devices:

# Filesystems:
     Run mount -v
     Run df -h
     Run df -g

# Details of the system’s virtual swap space: 
     Run swap -l

# OS version/patches:

# What patches are currently installed:
#    showrev -p

# Show what patches are currently installed:
     Run showrev -a

# Boot messages:
     for File in /var/log/syslog /var/adm/messages
     do
       test -r $File && Run grep -v "sendmail" $File
     done

# Display messages:
     Run dmesg
   ;;

  "HP-UX") # -----------------------------------------------
# OS version/patches:
# Kernel:
# Memory: 
# CPU:
# Devices:
# Filesystems:
# Boot messages:
   ;;

  "OpenUNIX") # --------------------------------------------
# OS version/patches:
# Kernel:
# Memory: 
# CPU:
# Devices:
# Filesystems:
# Boot messages:
   ;;

  "UnixWare") # --------------------------------------------
# OS version/patches:
# Kernel:
# Memory: 
# CPU:
# Devices:
# Filesystems:
# Boot messages:
   ;;

  "SCO_SV") # ----------------------------------------------
# OS version/patches:
# Kernel:
# Memory: 
# CPU:
# Devices:
# Filesystems:
# Boot messages:
   ;;
 esac
 
} # SysEnvInfo()


#------------------------------------------------------------------------------
# List the running _progres and _proapsv processes:

ListPro()
{
#UID        Pid  PPid  C STIME TTY          TIME CMD
#root      5978 27355  0 13:19 pts/1    00:00:00 grep _progres
#root     26291  5767  0 Jun15 pts/1    00:00:00 /usr/dlc/bin/_progres sports

  for ProName in _progres _proapsv
  do
    grep $ProName $PsProLog
  done
} # ListPro()


#------------------------------------------------------------------------------
# Get PROPATH environment variables used by running Progress processes.

GetPropaths()
{
# test -w /etc/passwd || exit

  OSNAME=`uname`
  case "$OSNAME" in
  
   "Linux") # -----------------------------------------------------------------
# /proc/$Pid/environ
      ListPro | \
      while read Userid Pid Rest
      do
        Propath=`strings /proc/$Pid/environ 2>/dev/null | \
                 $awk '$1~/^PROPATH=/ {print}'`
        test "$Propath" && \
        echo  $Propath $Userid $Pid $Rest
      done
    ;;
  
   "SunOS") # -----------------------------------------------------------------
# pargs - print process arguments, environment variables, or auxiliary vector 
# -e Prints process environment variables and values as pointed at
#    by the _environ symbol or by pr_envp in /proc/pid/psinfo. 
# Example: pargs -e 10154
# 10154:  /usr/wsrt_64/bin/_proapsv -logginglevel 1 -logfile /var/webspeed
# envp[26]: PROPATH=/home/app/dir41a/r-new,/home/app/dir41a/r

# or /usr/ucb/ps -p <PID> -wwwe

      ListPro | \
      while read Userid Pid Rest
      do
        Propath=`pargs -e $Pid 2>/dev/null | \
                 $awk '$1~/^env/ && $2~/^PROPATH=/ {$1=""; print}'`
        test "$Propath" && \
        echo  $Propath $Userid $Pid $Rest
      done
    ;;
  
   "AIX")   # -----------------------------------------------------------------
# ps wwe - report a snapshot of the current processes.
# w  Wide output. Use this option twice for unlimited width.
# e  Show the environment after the command.
      ListPro | \
      while read Userid Pid Rest
      do
        Propath=`ps wwe $Pid 2>/dev/null | \
                 tr " " \\n | \
                 $awk '$1~/^PROPATH=/'`
        test "$Propath" && \
        echo  $Propath $Userid $Pid $Rest
      done
    ;;
  
   "HP-UX") # -----------------------------------------------------------------
    ;;
   "OSF1")     
    ;;
   "OpenUNIX") 
    ;;
   "UnixWare") 
    ;;
   "SCO_SV")   
    ;;
    *)         
    ;;
  esac | \
  $awk '
    BEGIN {
      Prefix="'$TempDir'" "/propath."
      PropathID=0
      Line="-----------------------------------------------------------------"
    }
    $1~/^PROPATH=/ {
      Propath=substr($1,9); $1="" 
      if(ID[Propath]=="")
      {PropathID++
       ID[Propath]=PropathID
       printf"%s\n%s\n",Propath,Line >Prefix ID[Propath] ".txt"
      }
# Cut off the leading space left after $1 and print process info:
     print substr($0,2) >Prefix ID[Propath] ".txt"
    }
  ' # awk
} # GetPropaths()


#------------------------------------------------------------------------------
# The list of unique root directories in the specified dir list.
# Exclude subdirs if their parent dirs are also in Path:
# For example: "/a/b:/a" will be replace by "/a".
# The root dir ("/") will be removed from the list.

UniqDirList()
{
# $* - colon/comma/space-separated list of directories or files.

  echo $* | \
  tr ":," " " | \
  $awk '
   {for(i=1;i<=NF;i++)
    {if($i~/\/$/) $i=substr($i,1,length($i)-1)
     n=length($i); if(n>0)
     for(j=1;j<=NF;j++)
     {if(i!=j && $i==$j) $j=""
      if(length($j)>n && $i "/"==substr($j,1,n+1)) $j=""
   }}}
   {for(i=1;i<=NF;i++) if($i!="") printf"%s ",$i}
  ' # awk
} # UniqDirList()


#------------------------------------------------------------------------------
# The list of the files in the specified directory:
# The output format: date fullfilename
# where date in format YYYY:MM:DD.

FileList()
{
  Dir=$1
  Mask=$2

# Ignore the current dir and the root dir:
  test "$1" = "." && exit
  test "$1" = "/" && exit

  CurrYear=`date '+%Y'`

  case $Dir in
    */)      ls -lRF $Dir;;
     *) if [ -d $Dir ]
        then ls -lRF $Dir/
        else ls -lF  $Dir
        fi;;
  esac 2>/dev/null | \
  $awk '
#drwxrwxrwx 1 root root   4096 Jan 12 22:25 directory/
#-rwxrwxrwx 1 root root  81920 Jan 12 22:24 recent.file
#-rwxrwxrwx 1 root root   1184 Dec 10  2009 old.file
#lrwxrwxrwx 1 root root 19 Jun 14 16:39 link -> /path/to/my/file
    BEGIN {
     CurrYear='$CurrYear'
     NMask=split("'$Mask'",Mask,",")
     M["Jan"]="01"; M["Feb"]="02"; M["Mar"]="03"; M["Apr"]="04";
     M["May"]="05"; M["Jun"]="06"; M["Jul"]="07"; M["Aug"]="08";
     M["Sep"]="09"; M["Oct"]="10"; M["Nov"]="11"; M["Dec"]="12";
    }

# ls -R: Recursively lists subdirectories encountered.
# Dir for ls command is used with a trailing slash.
# So the subdirectories also have the trailing slashes:
    NF==1 && $1~/:$/ {
      SubDir=substr($1,1,length($1)-1)
      if(substr(SubDir,length(SubDir),1)!="/") SubDir=SubDir "/"
    }

    NF<9 {next}

# d The entry is a directory,
# D The entry is a door,
# l The entry is a symbolic link.
# - The entry is a file.

    {Type=substr($1,1,1)}

    Type=="d" || Type=="D" {next}

    Type=="l" {
      FileName=$9
# The only reason to use the -F option is to check if a link is a dir:
      if(substr($11,length($11),1)=="/") Type="d"
    }

    Type=="-" {
# -F option adds the trailing symbols:
# / Directories,
# > Doors,
# * Executables,
# | FIFOs, 
# @ Symbolic links,  
# = AF_UNIX  address family sockets.

      Trail=substr($9,length($9),1)
      if(index("/>*|@=", Trail)==0) FileName=$9
      else FileName=substr($9,1,length($9)-1)
    }

# Check if filename matches the specified masks:
    Type!="d" && NMask!=0 {
      Match="n"
      for(i=1;i<=NMask;i++) 
      if(match(FileName, Mask[i] "$")>0) {Match="y"; break}
      if(Match=="n") next
    }

# Reformat the file date as "Month Day, Year":
    {if($8~/:/) $8=CurrYear
     if(length($7)==1) $7="0" $7
# Output format: Date FullFileName
     if(Type=="d") Date=Type
     else          Date=$8 M[$6] $7 #Year/Month/Day
     printf"%8s %s\n",Date,SubDir FileName
    }
# awk
  ' | \
  while read Date FileName
  do
    case $Date in
# Link to a direcrtory found:
     "d") FileList $FileName $Mask
      ;;
# Ordinary file:
      *) echo $Date $FileName
      ;;
    esac
  done  
} # FileList()


#------------------------------------------------------------------------------
# Create date statiscts for the files outputed by FileList().
# Input stream should begin with Date in format "YYYY/MM/DD".

DateStat()
{
  $awk '{Count[$1]++} 
    END {for(Date in Count) print Date " " Count[Date]}
  ' | \
  sort -r | \
  $awk '
    BEGIN {
     M["01"]="Jan"; M["02"]="Feb"; M["03"]="Mar"; M["04"]="Apr"
     M["05"]="May"; M["06"]="Jun"; M["07"]="Jul"; M["08"]="Aug"
     M["09"]="Sep"; M["10"]="Oct"; M["11"]="Nov"; M["12"]="Dec"
    }

# Parsing input date: 
    {Year=substr($1,1,4)
     Month=substr($1,5,2)
     Month=M[Month]
     Day=substr($1,7,2)

# CurrDate like Jun 09, 2012:
     CurrDate=Month " " Day ", " Year
     CurrMnth=Month ", " Year
     CurrYear=Year
    }

# New month:
    CurrMnth!=PrevMnth {
     MnthCount++
# Print the monthly statistics during first year:
     if(MnthCount>1 && YearCount==1) \
     printf"%12s: %6d\n",PrevMnth,MnthSum
     PrevMnth=CurrMnth
     MnthSum=0;
    }

# New year:
    CurrYear!=PrevYear {
     YearCount++
# Print the annual statistics after first year:
     if(YearCount>1) printf"%12s: %6d\n",PrevYear,YearSum
     YearSum=0
     PrevYear=CurrYear
    }
  
# Print the daily statistics during first month:
    MnthCount==1 {printf"%12s: %6d\n",CurrDate,$2}
  
    {MnthSum=MnthSum+$2
     YearSum=YearSum+$2
     TotlSum=TotlSum+$2
    }
    END {
     if(YearCount>1) printf"%12s: %6d\n",PrevYear,YearSum
     else 
     if(MnthCount>1) printf"%12s: %6d\n",PrevMnth,MnthSum
     printf"%12s: %6d\n","Total",TotlSum
    }
  ' #awk
} # DateStat()


#------------------------------------------------------------------------------
# Date statistics of the files resided in dir list:

PathDateStat()
{
  echo "
----------------------------------------------------------------------------
Date statistics for $Mask files in $Path"

# Save current locale:
  PrevLCTIME=LC_TIME
  LC_TIME=en_ENG; export LC_TIME

  for Dir in `UniqDirList $Path`
  do
    FileList $Dir $Mask
  done | \
  DateStat

# Restore previous locale:
  LC_TIME=PrevLCTIME; export LC_TIME
} # PathDateStat()


#------------------------------------------------------------------------------
# The locations of the AppServer's logs, PROPATHs etc:

UBrokerLogs()
{
  PropFile=$DLC/properties/ubroker.properties

# WRKDIR specified during Progress installation:
  WRKDIR=`$awk 'BEGIN {FS="="}
               $1=="workDir" {print $2; exit}
               ' $DLC/installd.ini
         ` # WRKDIR
  
  TempFile=/tmp/$THIS.`date '+%y%m%d_%H%M%S'`.tmp
  touch $TempFile
  chmod 777 $TempFile
  
  for Type in as ns ws wsm wsa aia adapt
  do
    case "$Type" in
      as)    Section="UBroker.AS";;
      ns)    Section="NameServer";;
      ws)    Section="UBroker.WS";;
      wsm)   Section="WebSpeed.Messengers";;
      wsa)   Section="WSA";;
      aia)   Section="AIA";;
      adapt) Section="Adapter";;
    esac
  
  
# The list of names defined for the component:
    NameList=`
      cat $PropFile | \
      tr -d " \r"   | \
      $awk '/^\['$Section'\./ {print}' | \
      $awk 'BEGIN {FS="."}; {print substr($NF,1,length($NF)-1)}'
    ` # NameList
  
    for Name in $NameList
    do
      echo Name: $Section.$Name
  
# Default WRKDIR:
      echo "WRKDIR=$WRKDIR" >$TempFile

# Variables defined in [Environment] and [Environment.Broker]
      cat $PropFile | \
      tr -d " \r"   | \
      $awk '
        BEGIN {Section="Environment"; Level=0; MaxLevel=2}
        $1~/^\[/            {Action="ignore"}
        $1=="[" Section "]" {Level++; if(Level>2) exit
                             Action="print"
                             Section=Section ".'$Name'"
                             next}
        Action=="print" && $0~/\=/ {print}
      ' >>$TempFile


# Log Files and workDir:
      cat $PropFile | \
      tr -d " \r"   | \
      $awk 'BEGIN {FS="="
                   MaxLevel=split("'$Section.$Name'",arr,".")+1
                   Section=arr[1]; Level=1}
        $1~/^\[/            {Action="ignore"}
        $1=="[" Section "]" {Level++; if(Level>MaxLevel) exit
                             Action="print"
                             Section=Section "." arr[Level]
                             next}
        Action!="print"     {next}
        $1=="workDir" || $1=="PROPATH" || \
        $1~/LogFile$/ || $1~/logFile$/ {
                             print
                             VarName[$1]}
        END {for(Name in VarName) printf"echo %s=$%s\n",Name,Name}
      ' >>$TempFile

# Show the final values of the defined variables:  
      $TempFile
      echo ""
    done # for Name in $NameList
  done # for Type in $TypeList
  
  rm $TempFile
} # UBrokerLogs()


#------------------------------------------------------------------------------
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

    $echo "M\n1\n9999"    # Modify Defaults: Page size
    $echo "Q\n6"          # Shared Resources
    $echo "Q\n7"          # Database Status
    $echo "Q\nR&D\ndebghb"
    $echo "5\n1\n9999"    # Adjust Monitor Options: Display page length
    $echo "3\n$MainMonIntrv" # Adjust Monitor Options: Monitor sampling interval
    $echo "T\n1\n1"       # Status: Database
    $echo "P\n8"          # Status: Logging Summary
    $echo "P\n12"         # Status: Startup Parameters
    $echo "T\n4\n5"       # Administrative Functions: Adjust Page Writer Options
    $echo "P\n5"          # Status: Files
    $echo "T\n1\n2"       # Status: Backup
   ;;


   ADJUST)                # Use "ADJUST" only as a first sequence.

    $echo "R&D\ndebghb"
    $echo "5\n1\n9999"    # Adjust Monitor Options: Display page length
    $echo "3\n$ResrcMonIntrv" # Adjust Monitor Options: Monitor sampling interval
   ;;


   ACTIVITY)

# Database access:
   $echo "T\n4\n4"       # Administrative Functions: Adjust Latch Options
   $echo "T\n2\n1"       # Activity: Summary
   $echo "T\n1\n7"       # Status: Buffer Cache
   $echo "T\n2\n3"       # Activity: Buffer Cache
   $echo "T\n2\n4"       # Activity: Page Writers
   $echo "T\n3\n1"       # Activity: Performance Indicators
   $echo "T\n6\n9"       # Activity: TXE Lock Activity
   $echo "T\n2\n11"      # Activity: Index
   $echo "T\n2\n12"      # Activity: Record
   $echo "T\n2\n13"      # Activity: Other
   $echo "T\n6\n8"       # Activity: Resource Queues
   $echo "T\n6\n11"      # Activity: Latch Counts
   $echo "P\n14"         # Status: Shared Memory Segments

# Lock table:
   $echo "T\n1\n13"      # Status: Shared Resources
   $echo "T\n2\n7\n"     # Activity: Lock Table
   $echo "T\n1\n4\n3"    # Status: Active Transactions
   $echo "T\n1\n4\n2"    # Status: Blocked Clients
   $echo "T\n6\n15"      # Status: Buffer Lock Queue
#  $echo "T\n1\n6\n1"    # Status: Lock Table
# promon/Status: Lock Table MIGHT HUNG DB!!!

# Logging:
   $echo "T\n2\n5"       # Activity: BI Log
   $echo "T\n1\n9"       # Status: BI Log
   $echo "T\n3\n4"       # Other Displays: Checkpoints
   $echo "T\n1\n10"      # Status: AI Log
   $echo "T\n2\n6"       # Activity: AI Log
   $echo "T\n1\n16"      # Status: Database Service Manager

# Disk I/O:
   $echo "T\n2\n8"       # Activity: I/O Operations by Type
   $echo "T\n2\n9$MANY"  # Activity: I/O Operations by File
   $echo "T\n2\n10"      # Activity: Space Allocation

# User's I/O:
   $echo "T\n3\n2"       # Other Displays: I/O Operations by Process

# Remote Client Servers:
   $echo "T\n1\n3"       # Status: Servers
   $echo "T\n2\n2$MANY"  # Activity: Servers
  ;;


  RESOURCES)

# Latches and Resources
   $echo "T\n6\n11"      # Activity: Latch Counts
   $echo "T\n1\n4\n3"    # Status: Active Transactions
   $echo "T\n6\n9"       # Activity: TXE Lock Activity
   $echo "T\n1\n4\n2"    # Status: Blocked Clients
   $echo "T\n6\n15"      # Status: Buffer Lock Queue
  ;;


  PerUSER)

   $echo "T\n3\n2"       # Other Displays: I/O Operations by Process
   $echo "T\n3\n3"       # Other Displays: Lock Requests By User
#  $echo "T\n1\n6\n1"    # Status: Lock Table: Display all lock entries
  ;;


  SAMPLE)
   $echo "T\n2\n9"       # Useless statistics for .db file. Just to be on activity screen.
   $echo "S"             # Sample activity counters
  ;;


  SAMPLE2)
   $echo "T\n2\n13"      # Activity: Other
   $echo "S"             # Sample activity counters
  ;;


  EXIT)
   $echo "X"             # Exit from the Progress Monitor utility
  ;;


  *) $echo $THIS: PromonMenu: unexpected sequence: $Sequence 2>&1
   exit 1
  ;;
 esac
} # PromonMenu()


#------------------------------------------------------------------------------
# Start Main Database Monitoring...

StartMainDbMon()
{
# Parameter:
  db=$1
  OutFile=$2.promon.main.$RunId.log

  MainMonIntrv=$MonIntrv
  MainMonCount=$MonCount

 (echo $THIS Release $Release
  echo Promon \($MainMonIntrv sec x $MainMonCount\) started at `date '+%a %b %e %T %Y'`
  echo Database: $db
  cat  $DLC/version
  echo Host: `uname -a`
  echo ""
 ) >$OutFile

# Start promon.
 (PromonMenu STARTUP
  PromonMenu PerUSER
  PromonMenu ACTIVITY

  while [ $MainMonCount -gt 0 ]
  do
    MainMonCount=`expr $MainMonCount - 1`

    PromonMenu SAMPLE
    PromonMenu ACTIVITY
  done

  PromonMenu EXIT
 ) | \
  $DLC/bin/_mprshut $db -0 $PromonOptions 2>/dev/null | \
  tr -d "\f" >>$OutFile && \
  echoT "Main promon on $BD$db$UB has finished." &

# PID of last command in pipe:
  LastPID=$!

  if kill -0 $LastPID 2>/dev/null; then
    echoT "Main promon on $BD$db$UB started (last PID=$LastPID)."
  else
    echoT "Main promon on $BD$db$UB failed." >&2
  fi
} # StartMainDbMon()


#------------------------------------------------------------------------------
# Start Resource Database Monitoring...
StartResrcDbMon()
{
# Parameter:
  db=$1
  OutFile=$2.promon.resrc.$RunId.log

  ResrcMonIntrv=2
  ResrcMonCount=`expr $MonDuration / $ResrcMonIntrv`

 (echo $THIS Release $Release
  echo Promon \($ResrcMonIntrv sec x $ResrcMonCount\) started at `date '+%a %b %e %T %Y'`
  echo Database: $db
  cat  $DLC/version
  echo Host: `uname -a`
 ) >$OutFile

# Start promon.
 (PromonMenu ADJUST
  PromonMenu RESOURCES

  while [ $ResrcMonCount -gt 0 ]
  do
    ResrcMonCount=`expr $ResrcMonCount - 1`

    PromonMenu SAMPLE2
    PromonMenu RESOURCES
  done

  PromonMenu EXIT
 ) | \
  $DLC/bin/_mprshut $db -0 $PromonOptions 2>/dev/null | \
  tr -d "\f" >>$OutFile && \
  echoT "Resrc promon on $BD$db$UB has finished." &

# PID of last command in pipe:
  LastPID=$!

  if kill -0 $LastPID 2>/dev/null; then
    echoT "Resrc promon on $BD$db$UB started (last PID=$LastPID)."
  else
    echoT "Resrc promon on $BD$db$UB failed." >&2
  fi
} # StartResrcDbMon()


#------------------------------------------------------------------------------
# Start 4GL session to run the DbStatMon.p program:

StartDbStatMon()
{
  test "$DbStatMonProc" || exit

  db=$1
  OutPrefix=$2

# Log for the session errors:
  OutFile=$OutPrefix.DbStatMon.protocol.$RunId.log

  $DLC/bin/_progres $db -b -p $DbStatMonProc \
  -param $MonIntrv,$MonCount,$OutPrefix,$RunId >$OutFile 2>&1 && \
  echoT "DbStatMon on $BD$db$UB has finished." && \
  (test -s $OutFile || rm $OutFile) &

# PID of last command in pipe:
  LastPID=$!

  if kill -0 $LastPID 2>/dev/null; then
    echoT "DbStatMon on $BD$db$UB started (last PID=$LastPID)."
  else
    echoT "DbStatMon on $BD$db$UB failed." >&2
    test -s $OutFile && cat $OutFile >&2
  fi
} #StartDbStatMon()


#------------------------------------------------------------------------------
# Start system command in background mode.

StartSysCommand()
{
# Can find the command?
  type $1 >/dev/null 2>&1 || exit

# Parameter:
  Cmd=$*

# OutFile:
# Cut off %i and %c items:
  OutFile=`echo $Cmd | $awk 'BEGIN {RS=" "}; $0 !~/^%/ {print}'`

# Cut off spaces:
  OutFile=`echo $OutFile | tr -d " "`

# Add timestamp:
  OutFile=$TempDir/$OutFile.$RunId.log

  OSNAME=`uname`
  case "$OSNAME" in
   "Linux") SysCount=`expr $MonCount + 1`;;
         *) SysCount=$MonCount;;
  esac

  if [ $MonIntrv \> 0 ]
  then SysIntrv=$MonIntrv
  else SysIntrv=1
  fi

# Substitute %i monitoring interval ($MonIntrv).
# Substitute %c by interval count ($MonCount).

  RealCmd=`echo $Cmd | sed -e 's/%i/'$SysIntrv'/; s/%c/'$SysCount'/'`

 (echo Command $RealCmd started at `date '+%a %b %e %T %Y'` >$OutFile
  echo $THIS Release $Release                              >>$OutFile
  echo Host: `uname -a`                                    >>$OutFile
  $RealCmd                                                 >>$OutFile 2>&1
  man $1 | tr -d "\f\b"                                    >>$OutFile 2>/dev/null
  echoT "Command $BD$RealCmd$UB has finished."
 ) &

  LastPID=$!
  if kill -0 $LastPID 2>/dev/null; then
    echoT "Command $BD$RealCmd$UB started (last PID=$LastPID)."
  else
    echoT "Command $BD$RealCmd$UB failed." >&2
  fi
} # StartSysCommand()


#------------------------------------------------------------------------------
# Generate the protrace files...

GetProtraces()
{
  while read user pid rest
  do
    kill -$SIGUSR1 $pid
  done <$PsProLog

  test -d $TempDir/protraces || \
  mkdir   $TempDir/protraces
  ProtraceArchive=$TempDir/protraces/protraces.`uname -n`.`date '+%y%m%d_%H%M%S'`

  ProtraceList=$ProtraceArchive.txt
  ProtraceArchive=$ProtraceArchive.tar

# Wait a bit while the protrace files are generated:
  sleep 1

  while read WrkDir
  do
    find $WrkDir -type f -name "protrace.*" -print 2>/dev/null | \
    while read Protrace
    do
      mv $Protrace $TempDir 2>/dev/null && echo $Protrace
    done
  done <$ProtraceDirList >$ProtraceList

  if [ -s $ProtraceList ]
    then (cd $TempDir; tar -cf $ProtraceArchive protrace.* && rm -f protrace.*)
    else rm $ProtraceList
  fi

  if [ $? -ne 0 ]; then
   echo "Failed to get the protrace files." >&2
  fi
} # GetProtraces()


#------------------------------------------------------------------------------
# Start GetProtraces:

StartGetProtraces()
{
# Define SIGUSR1 signal:
  OSNAME=`uname`
  case "$OSNAME" in
   SunOS)  SIGUSR1=16;;
   HP-UX)  SIGUSR1=16;;
   AIX)    SIGUSR1=30;;
   SCO_SV) SIGUSR1=16;;
   Linux)  SIGUSR1=10;;
   *)      SIGUSR1=USR1;;
  esac
  
  GetProtracesLog=$TempDir/getprotraces.$RunId.log
  (GetProtraces >$GetProtracesLog 2>&1
   GetCount=$MonCount
   GetIntrv=`expr $MonIntrv - 1` #Each GetProtraces waits 1 sec
  
   while [ $GetCount -gt 0 ]
   do
     GetCount=`expr $GetCount - 1`
     sleep $GetIntrv
  
     GetProtraces >>$GetProtracesLog 2>&1
   done
   $echo "$Clean"
   echoT "${BD}GetProtraces${UB} has finished."
  ) &
  echoT "${BD}GetProtraces${UB} is started (last PID=$!)."
} # StartGetProtraces()


#------------------------------------------------------------------------------
# Wait the specified number of seconds.

CountDown()
{
  CountDown=$1
  while [ $CountDown -gt 0 ]
  do
   $echo "$CountDown sec left.\r\c"
   sleep 1
   CountDown=`expr $CountDown - 1`
  done
  echoT "$1 secs of waiting are over."
} # CountDown()


#------------------------------------------------------------------------------
# If wait is interrupted:

WaitInterrupted()
{
  $echo "$Clean"
  echoT "Wait was interrupted. Some logs may be incomplete."
} # WaitInterrupted()


# ************ Main Block *****************************************************
# -----------------------------------------------------------------------------

# Start monitoring:

# Log to capture the screen messages:
# ----------------------------------
EmonLog=$TempDir/$THIS.protocol.$RunId.log
echo $0 $EmonParams >$EmonLog
echo "$BD$THIS$UB: System and Database Monitoring, Release $Release" | tee -a $EmonLog


echoT "Checking System and Progress environment..."
# ------------------------------------------------
ProEnvInfo >$TempDir/env.info.pro.$RunId.txt 2>&1
SysEnvInfo >$TempDir/env.info.sys.$RunId.txt 2>&1


# The locations of Unified Broker's or NameServer's log files:
# -----------------------------------------------------------

UBrokerDir=$TempDir/ubroker
test -d $UBrokerDir || mkdir $UBrokerDir
UBrokerLogList=$UBrokerDir/ubroker.properties.logfiles.$RunId.txt
UBrokerLogs > $UBrokerLogList


# Change the language: 
PrevLCMESSAGES=LC_MESSAGES
LC_MESSAGES=us,US; export LC_MESSAGES

# The list of shared memory segments:
# ----------------------------------
OsIpcsLog=$TempDir/env.ipcs.os.$RunId.txt
ipcs -ma >>$OsIpcsLog

DbIpcsLog=$TempDir/env.ipcs.db.$RunId.txt
$DLC/bin/proutil -C dbipcs >$DbIpcsLog

# Check the positions of the "shmid" and "nattch" columns:
ShmIdCol="unknown"
ShmIdCol=`$awk '{for(i=1;i<=4;i++) if (match(tolower($i),"id")>0) print i}
                NR>2 {exit}' $OsIpcsLog`

AttchCol="unknown"
AttchCol=`$awk '{for(i=4;i<=NF;i++) if (match(tolower($i),"nattch")>0) print i}
                NR>2 {exit}' $OsIpcsLog`

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

# Sun,AIX: ShmIdCol=2  AttchCol=9
# Linux:   ShmIdCol=2  AttchCol=6


# The list of running databases:
# -----------------------------
DbList=$TempDir/db.list.$RunId.txt

# Parsing proutil -C dbipcs:
# PROGRESS SHARED MEMORY STATUS
#       ID ShMemVer Seg# InUse Database
#        0       -    - -      (not PROGRESS)
#        1       -    - -      (not PROGRESS)
# 13631490  6412383    0 Yes   /usr/db/sports.db

$awk '$3==0 && $4=="Yes" {print $1 " " $5}' $DbIpcsLog | \
sed -e 's/.db$//' | \
while read ShmId db
do
  if [ "$AttchCol" = "unknown" ]
  then
    NAttch="unknown"
  else
    if [ "$ShmIdCol" = "unknown" ]
    then NAttch=`grep " $ShmId " $OsIpcsLog | $awk '{print $'$AttchCol'; exit}'`
    else NAttch=`$awk '$'$ShmIdCol'=="'$ShmId'" {print $'$AttchCol'; exit}' $OsIpcsLog`
    fi    
  fi

# Db list will be sorted by the number of processes attached to the shared memory:
  Sort=`echo $NAttch | $awk '{while(length($1)<6) $1="0" $1; print $1}'`
  
  OutPrefix=$TempDir/`Path2Name $db`/`basename $db`
  echo $Sort $db $OutPrefix $NAttch
done | \
sort -r | \
while read Sort db OutPrefix NAttch
do
# Cut off Sort and add "skip" flag for dbs that don't match the pattern:

  if [ "$DbPattern" ]
  then Skip=`echo $db | grep $DbPattern >/dev/null || echo "skip"`
  else Skip=""
  fi
  echo $db $OutPrefix $NAttch $Skip
done >$DbList

# Restore the language: 
LC_MESSAGES=$PrevLCMESSAGES; export LC_MESSAGES


# The list of running processes:
# -----------------------------
PsAllLog=$TempDir/env.ps.all.$RunId.txt
ps -ef >$PsAllLog


# The list of Progress processes:
# ------------------------------
PsProLog=$TempDir/env.ps.pro.$RunId.txt

for ProName in _progres _proapsv _mprosrv _sqlsrv2 _proutil _rfutil rpserver rpagent
do
  grep $ProName $PsAllLog
done > $PsProLog

# Exclude promon (_mprshut -0) processes because they terminate after kill -SIGUSR1:
grep _mprshut $PsAllLog | grep -v " -0" >> $PsProLog


# The list of directories to check for protrace files:
# ---------------------------------------------------
ProtraceDirList=$TempDir/protraces.wrkdir.$RunId.txt
# Full list of Working directories :
( 
# List defined by WrkDirList variable
  echo $WrkDirList | $awk 'BEGIN {RS=":"}; $1!="" {print $1}'

# Add db directories:
  while read db Rest
  do
    dirname $db
  done <$DbList

# Add workDir of AppServers and other services:
  grep "workDir=" $UBrokerLogList | \
  tr "=" " " | \
  while read LogName WRKDIR
  do
    test -d $WRKDIR 2>/dev/null && echo $WRKDIR
  done | uniq

# Add the process working directories:
# pwdx is available on Linux and Solaris and procwdx is on AIX.

  type pwdx >/dev/null 2>&1
  if [ $? -eq 0 ]
  then
    PWDX=pwdx
  else
    type procwdx >/dev/null 2>&1 && \
    PWDX=procwdx
  fi

  if [ "$PWDX" ]
  then
    while read user pid rest
    do
# Output should look like: 6831: /home/pro
# Ingnore output like:     6831: Permission denied
      $PWDX $pid | $awk 'NF==2 {print $2}'
    done <$PsProLog
  else
    test -d /proc && \
    while read user pid rest
    do
      test -r /proc/$pid/cwd && \
      ls -l /proc/$pid/cwd 2>/dev/null | \
      $awk '$1~/^l/ {print $NF}'
    done <$PsProLog
  fi    
) | uniq | sort | uniq >$ProtraceDirList
# First uniq should reduce the list from the beginning:
# a lot processes can share the same working dir.
# Then sorting will take less resources.

# The list of old protrace files found by the script:
# --------------------------------------------------
ProtraceArchive=$TempDir/protraces.found.$RunId
ProtraceList=$ProtraceArchive.txt
ProtraceArchive=$ProtraceArchive.tar

while read WrkDir
do
  find $WrkDir -type f -name "protrace.*" -print 2>/dev/null | \
  while read Protrace
  do
    mv $Protrace $TempDir && echo $Protrace
  done
done <$ProtraceDirList >$ProtraceList

if [ -s $ProtraceList ]
  then (cd $TempDir; tar -cf $ProtraceArchive protrace.* && rm -f protrace.*)
  else rm $ProtraceList
fi


# Start GetProtraces:
# ------------------
StartGetProtraces


# Start System Monitoring:
# -----------------------
echo $CmdList | \
$awk '
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


# Start Database Monitoring:
# -------------------------

# DbStatMon.p should be located at the same dir as this script:

ProgDir=`dirname $0`
ProgDir=`(cd $ProgDir; pwd)`
DbStatMonProc=""
for File in DbStatMon.r dbstatmon.r DbStatMon.p dbstatmon.p 
do
  test -f $ProgDir/$File && DbStatMonProc=$ProgDir/$File && break
done

while read db OutPrefix NAttch Skip
do
  test "$Skip" && continue

  DbTempDir=`dirname $OutPrefix`
  test -d $DbTempDir || mkdir $DbTempDir

# In case if the uitilities will hang on db connection start them in background mode:
  OutFile=$OutPrefix.proshut.list.$RunId.txt
  $DLC/bin/_mprshut  $db  -C list >$OutFile &

  OutFile=$OutPrefix.prostrct.statistics.$RunId.txt
  $DLC/bin/_dbutil   prostrct statistics $db >$OutFile 2>/dev/null || \
  rm $OutFile &

  OutFile=$OutPrefix.proutil.describe.$RunId.txt
  $DLC/bin/_proutil  $db  -C describe >$OutFile 2>/dev/null || \
  rm $OutFile &

  OutFile=$OutPrefix.proutil.viewB2.$RunId.txt
  $DLC/bin/_proutil  $db  -C viewB2 >$OutFile 2>/dev/null || \
  rm $OutFile &

  StartMainDbMon $db $OutPrefix

  StartDbStatMon $db $OutPrefix

done < $DbList

test "$DelayNextPromon" = "y" && sleep 1
while read db OutPrefix NAttch Skip
do
  test "$Skip" && continue
  StartResrcDbMon $db $OutPrefix
done < $DbList


# Wait for background processes:
# -----------------------------
WaitTime=`expr $MonDuration + 2`

echoT "Waiting for background processes..."
CountDown $WaitTime

$echo "Still waiting for active processes (Ctrl-C to interrupt)...\a\r\c"

OSNAME=`uname`
case "$OSNAME" in
 "SunOS") trap WaitInterrupted 2
          wait
          trap -
  ;;
  *)      trap WaitInterrupted SIGINT
          wait
          trap - SIGINT
  ;;
esac

$echo "$Clean"
test -s $GetProtracesLog || rm $GetProtracesLog

echoT "Moving the logs to the archive..."
while read db OutPrefix NAttch Skip
do
  test "$Skip" && continue

  DbTempDir=`dirname $OutPrefix`
  test -d $DbTempDir || mkdir $DbTempDir

  tail -$DbLogLimit $db.lg > $OutPrefix.$RunId.lg
  cp $db.lic $OutPrefix.$RunId.lic
done < $DbList 2>&1 | tee -a $EmonLog

cp $DLC/properties/ubroker.properties $UBrokerDir/ubroker.properties.$RunId.txt

# Copy the logs of AppServers and other services:
grep -v "workDir=" $UBrokerLogList | grep "=" | tr "=" " " | \
while read LogName LogPath
do
  test -f $LogPath 2>/dev/null && \
  tail -$UBrokerLogLimit $LogPath > $UBrokerDir/`Path2Name $LogPath`.$RunId.log

# Multi-extent logs:
  ls -1 `echo $LogPath | sed -e 's/.log$//'`.*.log 2>/dev/null | \
  while read LogPath
  do
    test   "$LogPath" && \
    test -r $LogPath  && \
    tail -$UBrokerLogLimit $LogPath > $UBrokerDir/`Path2Name $LogPath`.$RunId.log
  done
done 2>&1 | tee -a $EmonLog


echoT "Parsing ubroker.properties file (it may take a few seconds)..."
for Config in asconfig nsconfig wsconfig wsaconfig aiaconfig adaptconfig
do
  $DLC/bin/$Config | \
  awk '
    /^Configuration Information for / {
    close(Outfile); Outfile="'$UBrokerDir'/config."$4"'.$RunId.txt'"}
    Outfile!="" {print >Outfile}
  '
done

echoT "Getting the list of PROPATHs that are used by the running processes..."
GetPropaths

echoT "Collecting date statistics for r-codes in PROPATHs..."
#
#PathDateStat "$PROPATH" ".r,.pl" >>$TempDir/env.info.pro.$RunId.txt

$awk '$1=="Name:" {Config=$2}
      $1~/^PROPATH=/ {print Config " " substr($0,9)}
' $UBrokerLogList | \
while read Config ConfigPropath
do
  PathDateStat $ConfigPropath ".r,.pl" >>$UBrokerDir/config.$Config.$RunId.txt
done 2>&1 | tee -a $EmonLog


echoT "Creating the archive..."
tar -cf  $TempDir.tar $TempDir 2>/dev/null && \
gzip  $TempDir.tar && \
echoT "...done: `ls -1 $TempDir.tar.*`"
# && rm -rf $TempDir
