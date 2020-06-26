#!/bin/bash
#############################################################################################
# Script                : disconnect.sh
# Description           : Disconnect users from databases using "proshut"
#                         
# Input parameters      : see "usage" section below.
# Required files        : $STARTDIR/usr.registry
#
# Modification history
# Date          Modified by     Version         Details
# -----------   -----------     -------         --------------------------
# 04/03/2014    drouinre 	1.0		initial
#
#############################################################################################
STARTDIR=$HOME/bin
DBREG=$STARTDIR/usr.registry
DLC=/usr/dlc;export DLC
DLCPATH=$DLC/bin
PROMSGS=$DLC/promsgs;export PROMSGS
PROTERMCAP=$DLC/protermcap;export PROTERMCAP
TERM=vt100
PATH=$PATH:$DLCPATH;export PATH
display_banner=no

usage() {
echo "Syntax is: $0"
echo "           [ -h ] (Shows this help page)"
echo "           [ -d devicename ]"
echo "           [ -p pid ]"
echo "           [ -n username ]"
echo "           [ -u usernumber ]"
exit
}

cleanfiles() {
rm -f $tmpfile 2>/dev/null
rm -f $tmpfile2 2>/dev/null
}

if [ $# -eq 0 ]
then
  usage
fi

devicename=
pid=
username=
userid=

while getopts ":d:p:n:u:h:" option
do 
case $option in
   d)
   devicename="$OPTARG"
   ;;
   p) 
   pid="$OPTARG"
   expr $pid + 0 > /dev/null 2>&1
   if [ $? -ne 0 ]
   then
     echo "PID should be numeric only..."
     exit
   fi 
   ;;
   n)
   username="$OPTARG"
   ;;
   u)
   userid="$OPTARG"
   expr $userid + 0 > /dev/null 2>&1
   if [ $? -ne 0 ]
   then
     echo "User ID should be numeric only..."
     exit
   fi
   ;;
   h) 
   usage
   ;;
   *) 
   usage
   ;;
esac
done 

PARMS="$devicename,$pid,$username,$userid" 

if [ "$PARMS" = ",,," ]
then
  usage
fi

cd $STARTDIR

if [ ! -f $DBREG ]
then
  echo "$DBREG is missing. It should have the entry like below format. Under the same header"
  echo "DB  DBPATH  PF-File"
  echo "Script exiting"
  exit
fi

tmpfile=/tmp/disconnect.$$.txt
export tmpfile2=/tmp/disconnect2.$$.txt

cat $DBREG |grep -v DB|
while read line
do
  DBNAME=`echo $line|awk -F" " '{print $1}'`
  DBPATH=`echo $line|awk -F" " '{print $2}'`
	
  if [ ! -f $DBPATH/$DBNAME.db ]
  then
    echo "DB name is not valid $DBPATH/$DBNAME"
    cleanfiles
    exit
  fi

  echo "Connections from $DBPATH/$DBNAME:" >> $tmpfile
  $DLCPATH/_progres -b $DBPATH/$DBNAME -U applid -P applid -p disconnect.p -param "$PARMS" >> $tmpfile 
  echo >> $tmpfile

done

cat $tmpfile

fromtime=$SECONDS
ans=n
echo "Do you want to disconnect the users above? y/n (Default is 'n')"
echo "You have 4 seconds to respond..."
read ans
if [ "$ans" = "y" -o "$ans" = "Y" ]
then
  totime=$SECONDS
  if [ `expr $totime - $fromtime` -gt 4 ]
  then
    echo "Timeout expired, exiting..."
    cleanfiles
    exit
  fi
  cat $tmpfile2 | 
  while read db usrno
  do
    echo $db $usrno
    $DLCPATH/proshut $db -C disconnect $usrno
  done
fi

cleanfiles
