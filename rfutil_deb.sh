#!/bin/sh

# Run roll forward utility in the "debug" mode:
# Script enables the "verbose" mode for roll forward
# and reports the time to process every "n" notes
# where "n" is 200,000 (set in rfutil_deb.p).

# Written by George Potemkin, July 19, 2014
# Last modified: July 21, 2014

# The most recent version of the script can be downloaded from:
# ftp://ftp.progress-tech.ru/pub/Users/george/Scripts/rfutil_deb.sh
# ftp://ftp.progress-tech.ru/pub/Users/george/Scripts/rfutil_deb.p

THIS=`basename $0`
Release="V1.0 as of July 21, 2014"

Program=rfutil_deb.p
# Script is looking for the program in the current working directory
# or in location of the script.


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
  for awk in awk nawk gawk
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

if [ -z "$AiFile" ]
then
  echo "$THIS: You must supply ai file name (-a)."
  exit 1
fi

if [ ! -f $Program ]
then
  Program2=`dirname $THIS`/`basename $Program`
  test -f $Program2 && \
  Program=$Program2
fi

if [ ! -f $Program ]
then
  echo "$THIS: can not find ${Program}."
  exit 1
fi

echo $RfutilParamList | grep "roll forward" >/dev/null || \
(echo "$THIS: Debug mode is available only for roll forward."; exit 1)


#------------------------------------------------------------------------------

# Main block:

# Progress environment:

if [ -x $DLC/bin/_proutil ]
then
  PATH=$DLC/bin:$PATH
  export PATH
else
  echo Check DLC=$DLC
  exit 1
fi

LogFile=rfutil.`date '+%y%m%d_%H%M%S'`.$$.log
Rfutil="$DLC/bin/_rfutil $RfutilParamList"

echo $Rfutil | grep " verbose " >/dev/null || \
Rfutil=`echo $Rfutil | sed -e 's/roll forward/roll forward verbose/'`

(echo $THIS $Release
 uname -a
 echo "$Rfutil"
 test "$AiFile" && echo AI file: `ls -l $AiFile`
) >$LogFile

$DLC/bin/_progres -p $Program -b -param "$Rfutil 2>&1" | tee -a $LogFile

