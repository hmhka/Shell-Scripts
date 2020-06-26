#!/bin/sh
#
# Script to get PROPATHs of running Progress processes.

TempDir=.
RunId=`uname -n`.`date '+%y%m%d_%H%M%S'`

# awk or nawk?
# ------------
OSNAME=`uname`
case "$OSNAME" in
 Linux|HP-UX) awk=awk ;;
 SunOS|AIX)   awk=nawk;;
 *)
  for awk in awk nawk gawk
  do
   Test=`echo test | $awk 'BEGIN {prinf"test"}; $1~/^t/ {print}' 2>/dev/null`
   test "$Test" && break
  done
  test "$Test" || (echo "Failed to find awk functionality."; exit 1)
  ;;
esac


#------------------------------------------------------------------------------
# List the running _progres and _proapsv processes:

ListPro()
{
#UID        Pid  PPid  C STIME TTY          TIME CMD
#root      5978 27355  0 13:19 pts/1    00:00:00 grep _progres
#root     26291  5767  0 Jun15 pts/1    00:00:00 /usr/dlc/bin/_progres sports

  PsAllLog=$TempDir/ps.all.$RunId.log
  ps -ef >$PsAllLog

  for ProName in _progres _proapsv
  do
    grep $ProName $PsAllLog
  done

  rm $PsAllLog
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
                 $awk 'BEGIN {RS=" "}; $1~/^PROPATH=/'`
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
      if(Id[Propath]=="") {
        PropathID++
        Id[Propath]=PropathID
        OutFile=Prefix Id[Propath] ".txt"
        printf"%s\n%s\n",Propath,Line >OutFile
      }
# Cut off the leading space left after $1 and print process info:
      OutFile=Prefix Id[Propath] ".txt"
      print substr($0,2) >OutFile
    }
  ' # awk
} # GetPropaths()


GetPropaths
ls $TempDir/propath*.txt
