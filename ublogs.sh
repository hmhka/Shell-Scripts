#/bin/sh

THIS=`basename $0`

# ubp.sh - shell script to parce the configs in ubroker.properties.

Release="V1.0 as of Apr 07, 2012"

# Fixes:

#-----------------------------------------------------------
Usage()
{
  echo "$BD$THIS$UB: script to parce ubroker.properties, Release $Release

Usage: $THIS [-h|help] [Type] [-name|-i Name] [-f|-propfile propFilePath]
where:
Type can have one of the following values:
as    - AppServer
ns    - NameServer
ws    - WebSpeed Broker
wsm   - WebSpeed Messenger
wsa   - Web Services Adapter
aia   - AppServer Internet Adapter Broker
adapt - SonicMQ Adapter Broker

-help or -h                     Display command line help;
-name or -i (Name)              Name of component;
-f or -propfile (propFilePath)  Full properties file path;
no options                      All components defined.
 
" >&2
  exit
}

# Sanity check: Is DLC set correctly?
# ----------------------------------
test "$DLC" && \
test -d $DLC && \
test -f $DLC/properties/ubroker.properties && \
test -f $DLC/installd.ini

if [ $? -ne 0 ]; then
  echo "Incorrect value of DLC=$DLC" >&2
  exit 2
fi


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
  test "$Test" || (echo "Failed to find awk functionality." >&2; exit 3)
  ;;
esac


# Defaults:
# --------
PropFile=$DLC/properties/ubroker.properties
TypeList="as ns ws wsm wsa aia adapt"
NameList=""


# Parsing the script's input parameters:
# -------------------------------------
while [ $# -gt 0 ]
do
  case $1 in
   -h|-help) Usage
   ;;
# Type of component:
   as|ns|ws|wsm|wsa|aia|adapt) TypeList=$1
   ;;
# Name of component:
   -name|-i) NameList=`echo $2 | tr "," " "`; shift
   ;;

# ubroker.properties file:
   -f|-propfile) PropFile=$2; shift
   ;;
   *) echo "$1 is an unknown option. Use $THIS -h for a help."; exit 1
   ;;
  esac  #case $1

  shift
done


# WRKDIR specified during Progress installation:
WRKDIR=`$awk 'BEGIN {FS="="}
             $1=="workDir" {print $2; exit}
            ' $DLC/installd.ini
       ` # WRKDIR

TempFile=/tmp/$THIS.`date '+%y%m%d_%H%M%S'`.tmp
touch $TempFile
chmod 777 $TempFile

for Type in $TypeList
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
  test "$NameList" || \
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
      $1=="workDir" || $1~/LogFile$/ || $1~/logFile$/ {
                           print
                           VarName[$1]}
      END {for(Name in VarName) printf"echo %s=$%s\n",Name,Name}
    ' >>$TempFile

# Show the final values of the defined variables:  
    $TempFile | \
    tr "=" " " | \
    while read LogName LogPath
    do
      if [ $LogName == "workDir" ]
      then test -d $LogPath 2>/dev/null
      else test -f $LogPath 2>/dev/null
      fi
      if [ $? -eq 0 ]
      then
        Status="exist"
      else
        Status="missed"
      fi
      echo "$LogName=$LogPath ($Status)"

# Multi-extent logs:
      LogMask=`echo $LogPath | sed -e 's/.log$//'`
      ls -1 $LogMask.*.log 2>/dev/null
    done # while read LogName LogPath
    echo ""
  done # for Name in $NameList
done # for Type in $TypeList

rm $TempFile
