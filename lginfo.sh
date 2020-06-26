#!/bin/sh
#
  Prog=`basename $0`
# lginfo - print the abnormal events messages from the database log files.
# Written by George Potemkin, 12/01/96
  Release="3.0, Feb 04, 1999"

# Period to check the database logs.
DaemonSleep=${DBLOGSLEEP:-20}

# Command used with -send option.
# Message text will be send through a pipe and the subject - as parameter.
SendCmd="mail -s"
                                    # #-----------------------------------------------------------
Usage()
{
 echo "\n\
$Prog  Database Log Viewer, Release $Release\n\
Prints the abnormal events messages from the database log files.\n\n\
USAGE: $Prog [-+=<ErrList>] [-last] [-cmd <Command>] [-send <Mail>] <DbLog> ...
       $Prog [-last] -brief <DbLog>[.lg|.db|.lk] ...\n\
       $Prog [-<ErrList>] [-find] -msg\n\
       $Prog [-<ErrList>] [-find] -id\n\
where \"<ErrList>\" is a list of the error numbers. Example: 123,456-654;\n\
      -<ErrList> don't show this errors (and a default list as well);\n\
      +<ErrList> show these errors even if they are of information type;\n\
      =<ErrList> show these errors only;\n\
      -last      show the errors only for the last session;\n\
      -brief     show day-to-day message statistics;\n\
      -msg       print the text of the messages to cut off;\n\
      -id        print only the message numbers to cut off;\n\
      -find      use \$DLC/prohelp/msgsdata to find the information messages;\n\
      -cmd <Command> send a message through a pipe to <Command>;\n\
      -send <Email>  use a mail command to send a message to DBA's email.\n\
Note: With -cmd or -send options $Prog will run in background mode;\n\
      -time $DaemonSleep   set a time interval for background process to check \
db log." >&2
 exit 1
}

#-----------------------------------------------------------
UnwrapIDList() #e.g. "123,456-458" is converted to "123\n456\n457\n458" 
{
 awk 'BEGIN {RS=","}
      {i=index($0,"-")
       if(i==0) {if($0>0) printf"%5d\n",$0; next}
       Min=substr($0,1,i-1)
       Max=substr($0,i+1)
       for(i=Min;i<=Max;i++) printf"%5d\n",i
      }' -
}

#-----------------------------------------------------------
WrapIDList() #e.g. "123\n456\n457\n458" is converted to "(123)(456)(457)(458)"
{
# Parameter:
 Sign=$1
 awk 'BEGIN {MsgList=""}
      {if(length(MsgList)>120) {print Sign " " MsgList; MsgList=""}
       MsgList=MsgList "(" $1 ")" #Example: MsgList=(123)(456)
      }
      END {if(MsgList) print Sign " " MsgList}
     ' Sign=$Sign -
}

#-----------------------------------------------------------
PrintMsgID()
{
 test "$FindFlag" && \
 echo "Looking for the list of the \"normal\" messages.\nPlease, wait...">&2 &&\
 ls -1 $DLC/prohelp/msgdata | \
 while read File
 do
# Try to find "normal" messages:
# 1. Some SYSTEM ERROR messages are described as informational ones.
#    I restore their status quo.
# 2. Compile-time errors seem to be marked as "c". I exclude them
#    from the list because they will not be in a database log.
  egrep "\
WARNING| informational message| information message| informational purpose" \
         $DLC/prohelp/msgdata/$File | \
  grep -v -i "\"c\"" | \
  awk 'BEGIN {FS="\""}
       NF==1 || $2 ~/SYSTEM ERROR/ {next}
       $1 ~/^[0-9]/ {printf"%5d\n",$1}
      '
 done
 
# The normal messages which should not be displayed.
# EDIT THIS LIST TO BE UPTODAY WITH YOUR VERSION OF PROGRESS.
# Message number high water mark by version:
#   2710  7.3C01
#   6494  8.2C
#   7268  8.3A
#   8346  9.0A2B
#-----------------------------------------------------------
 cat <<EOF | UnwrapIDList
123,298,333,334,451-453,512,541,542,562,644,663,708,739,742,794,895,896,1520,
1522,1526,1552,1613,1614,1630,1644,1665,1750,1824,1886,1887,1980,2015,2016,2019,
2027,2033,2245,2248,2252-2254,2261,2311,2353,2518-2520,2527,2689,2690,3665-3668,
3691,3694,3706,3747,3762,3763,3781,3785-3787,3789,3848,3849,4174,4197,4198,4203,
4216,4224,4225,4234-4247,4249-4265,4281,4282,4295,4348,4367,4375,4412,4421,4509,
4525,4534,4549,4588,4654,4684,4701,4702,4776,4783-4786,4788,5121,5283,5284,5288,
5326-5331,5407-5411,5432,5465,5468,5473,5512,5556,5560,5561,5619,5644-5649,5796,
6101,6126,6127,6207,6244,6526,6550-6574,6592,7168,7255,7367,8000,
$Out
EOF
}

#-----------------------------------------------------------
PrintMsgText()
{
 # If FindFlag is on then PrintMsgID will says "Please, wait...".
 test -z "$FindFlag" && \
 echo "Please, wait..." >&2 

 PrintMsgID | sort | uniq | \
 while read MsgID
 do
  File=`expr $MsgID - 1`
  File=`expr $File / 50 + 1`
  File=$DLC/prohelp/msgdata/msg$File
   
  if [ -f $File ]
  then
   grep "^$MsgID " $File | \
   awk 'BEGIN {RS="\""}; {ID=$1; getline; printf"%5d: %s\n",ID,$0; exit}'
  else
   echo $MsgID | \
   awk '{printf"%5d: Unused message, reserved for long message\n",$1}'
  fi
 done | pg
}

#-----------------------------------------------------------
# Print the abnormal messages.
PrintAbnormal()
{
# Parameter:
# Output file when run as background process and
# empty when run as foreground process.
# 

 awk '
# Read MessageID from Temp file.
  FILENAME!="-" {
   if($1=="Out") {NOut++; OutList[NOut]=$2}
        else     {NIn++;  InList[NIn]=$2}
   next
  } 
# Save date...
  /^ /    {Date=$0; next}

# Skip the lines which do not end by a error number.
  $NF !~ /^\([0-9]*\)$/ {next}

 Daemon!="" && $NF=="(334)" {exit}

# Check InList.
  {IsInList="No"
   for(i=1;i<=NIn;i++) {
    List=InList[i]
    if(index(List,$NF)>0) {IsInList="Yes"; break}
   }
   if(NOut==0 && IsInList=="No") next   #InOnlyFlag
  }
# Check OutList.
  IsInList=="No" {
   for(i=1;i<=NOut;i++) {
    List=OutList[i]
    if(index(List,$NF)>0) next
  }}
# Now printing...
  Daemon {print >>Daemon; close(Daemon); next}
 
  Date!="" {print "\n" Date; Date=""}
  {print}
 ' $Temp Daemon="$1" -
}

#-----------------------------------------------------------
# Cut a sign from a startup parameter.
Param()
{
 echo $1|tail +2c
}

#-----------------------------------------------------------
# Check DLC variable.
CheckDLC()
{
 test -z "$DLC" && \
 echo "$Prog: DLC variable is not set.\nExiting..." >&2 && \
 exit 2
 
 test ! -d $DLC/prohelp/msgdata && \
 echo "$Prog: The variable DLC has wrong value ($DLC).\n\
Directory $DLC/prohelp/msgdata not exist.\n\
Exiting..." >&2 && \
 exit 2
}

#-----------------------------------------------------------
# Check Command.
CheckCmd()
{
 type $1 >/dev/null 2>&1
 if [ $? -ne 0 ]; then
   echo "$Prog: $1 not found.\07" >&2 &&
   exit 3
 fi
}

#-----------------------------------------------------------
SetMode()
{
 if [ -z "$Mode" ] || [ "X$Mode" -eq "X$1" ]
 then Mode="$1"
 else echo "$Prog: You could not use both $Mode and $1 options." >&2
      Usage
 fi
}

#-----------------------------------------------------------
# Check command line parameters.
while [ ! -z "$1" ] #equal to [ "$1" ] but $1 can begin with a dash.
do
 case $1 in
  -f|-find)   FindFlag="On";  CheckDLC;;
  -m|-msg)    SetMode $1;     CheckDLC;;
  -i|-id)     SetMode $1;;
  -c|-cmd)    SetMode daemon; Command="$2"; CheckCmd $Command; shift;;
  -s|-send)   SetMode daemon; SendTo="$2";  CheckCmd $SendCmd; shift;;
  -t|-time)   test "$2" -gt 0 >/dev/null 2>&1;
              test $? -ne 0 && echo "$Prog: \
The value of -time option must be integer and greater than zero.\07" >&2 &&\
              Usage
              DaemonSleep=$2; shift;;
  -b|-brief)  SetMode $1;;
  -l|-last)   LastSessionFlag="Yes";;
  -debug)     DebugFlag="On";;
  -[0-9]*)    Out=`Param $1`;;
  +[0-9]*)    In=`Param $1`;;
  =[0-9]*)    In=`Param $1`; InOnlyFlag="On";;
  -*) echo "$Prog: $1 is an unknown parameter." >&2 && Usage;;

  *)  Db=`echo $1|sed -e 's/.lg$//; s/.db$//; s/.lk$//'`

      test ! -r $Db.lg && \
      echo "$Prog: $Db.lg not exists or an access denied.\07" >&2 && \
      shift && continue

      Dir=`dirname $Db`
      Dir=`(cd $Dir; pwd)`   #Path from root directory.
      Db=$Dir/`basename $Db` #Full path to the database.
      DbList="$DbList $Db";;
 esac
 shift
done

#-----------------------------------------------------------
# Options consistency check.
case $Mode in
  -b|-brief)
    test "$FindFlag" && \
    echo "$Prog: WARNING: the -find option is ignored." >&2

    test "$InOnlyFlag$In" && \
    echo "$Prog: WARNING: <ErrList> options are ignored." >&2
  ;;
  -m|-msg|-i|-id) # These modes use only Out list and ignore In list.

    test "$InOnlyFlag" && \
    echo "$Prog: You could not use =<ErrList> with $Mode option.\07" >&2 && \
    Usage

    if [ "$In" ]; then
      echo "$Prog: WARNING: +$In list is ignored.\07" >&2
      sleep 3
    fi
  ;;
esac

#-----------------------------------------------------------
# Options that don't use a database log.
case $Mode in
  -m|-msg) PrintMsgText; exit;;
  -i|-id)  PrintMsgID;   exit;;
esac

test "$DbList" || Usage

Temp=/tmp/$Prog$$.tmp
trap 'rm $Temp 2>/dev/null; exit 1' 2

#-----------------------------------------------------------
# Brief Log.
case $Mode in
-b|-brief)
 echo "Please, wait..." >&2
 test -d $DLC/prohelp/msgdata && UseMsgData="Yes"

 for Db in $DbList
 do
# If a message count great than 1 then get the message text from msgdata.
   if [ "$LastSessionFlag" ] #-- only last multi/single-user session --
   then Offset=`egrep -n "(333)|(451)$" $Db.lg|tail -1|awk -F: '{print $1}'`
   else Offset=1
   fi

   echo "Database log: $Db.lg"
   (tail +${Offset}l $Db.lg; echo "  Date 2 3 4 5") | \
   awk '
# Line contains a date: print a statistics of the previous date.
     NF==0 {next}
     $0~/^ / && NF==5 {
       print "Date " Date
       for(Err in Cnt) {
         printf"%4d  %s  %s\n",Cnt[Err],Err,Msg[Err]
         delete Cnt[Err]
         delete Msg[Err]
       }
       Date=$0
       next
     } 

     {if($NF ~/^\([0-9]*\)$/) {Err=substr($NF,2)      # Removing brackets.
                               Err=substr(Err,1,length(Err)-1)}
      else           # Some messages do not have a number.
      if($3 ~/^[0-9]*:$/)     {Err=$4; for(i=5;i<=NF;i++) Err=Err "_" $i}
      else                     Err=$2 "_" $NF

      Cnt[Err]++
      if($1 !~/^[0-9][0-9]:[0-9][0-9]:[0-9][0-9]$/) Msg[Err]=$0; else
      if($3 !~/^[0-9]*:/) Msg[Err]=substr($0,index(" " $0," " $2 " ")); else
                          Msg[Err]=substr($0,index(" " $0," " $4 " "))
#print "Err=" Err " Cnt=" Cnt[Err] " Msg=" Msg[Err]
     }

     END {print "Date"} #Pseudo date to initiate the printing for last date.

   ' | \
   while read Cnt Err Msg
   do 
#echo "Cnt=$Cnt Err=$Err Msg=$Msg"
# Is a line contain a date?
     if [ "$Cnt" = "Date" ]
     then
       test -f $Temp && \
       echo "\n       $Date\n" && \
       cat $Temp | sort -r | \
       awk '{printf"%4d  %s\n",$1,substr($0,index(" " $0," " $3 " "))}' && \
       rm $Temp
       Date="$Err $Msg"
       continue
     fi

     if test $Err -gt 0 >/dev/null 2>&1
     then ErrIsNum="Yes"; ErrWeight=`expr 10000 - $Err`
     else ErrIsNum="";    ErrWeight="`echo $Err|cut -c1-16`"
     fi
     if [ "$UseMsgData" ] && [ "$ErrIsNum" ] && [ $Cnt -gt 1 ]
     then
       File=`expr $Err - 1`
       File=`expr $File / 50 + 1`
       File=$DLC/prohelp/msgdata/msg$File

       grep "^$Err " $File | \
       awk 'BEGIN {RS="\""}
            {Err=$1; getline; printf"%4d  %5d  %s\n",Cnt,'$ErrWeight',$0
             exit
            }
       ' Cnt=$Cnt - >>$Temp
     else
       echo "$Cnt $ErrWeight $Msg" | \
       awk '{printf"%4d  %5s %s\n",\
             $1,$2,substr($0,index(" " $0," " $3 " "))}' >>$Temp
     fi
   done 2>/dev/null  # while read Cnt Err Msg
 done | pg         # for Db in $DbList
 exit
;;             # -brief mode
esac


# InList  - print these messages even if they are in OutList.
# OutList - ignore these messages. If OutList is empty then
# print only the messages from InList.

(test "$InOnlyFlag" || PrintMsgID | WrapIDList "Out"
 test "$In" && echo $In | UnwrapIDList | WrapIDList "In"
) >$Temp

test "$DebugFlag" && pg $Temp

case "$Mode" in
#-----------------------------------------------------------
# Background mode.
 daemon)
  
  cd /   # If the current directory is on mounted system
         # then a running process will not let to unmount it.
 
  DbID=0

  for Db in $DbList
  do

# Database sequential number.
    DbID=`expr $DbID + 1`

# Output of PrintAbnormal.
# It will be created only when an abnormal message is found.
# It will be deleted $DaemonSleep time later.
    Exchange=/tmp/$Prog$$.exchange.$DbID  
   

    tail -n -0 -f $Db.lg | PrintAbnormal $Exchange >/dev/null 2>&1 &
   
# TailPID is the tail's PID.
# It will be written to the database log.
# Do kill -15 $TailPID to stop the daemon.
    AwkPID=$!
    TailPID=`ps -f|awk '$3=='$AwkPID' {print $2}'`

# Copy of $Exchange to send.
    Dispatch=/tmp/$Prog$$.dispatch.$TailPID

# Mail subject:
    Subject="$Prog on $Db."

    Msg=`date +"%H:%M:%S $Prog: Started. PID=$TailPID (2518)"`
    echo $Msg >>$Db.lg
    echo $Msg >&2

# Run background process that should send the message.
    while sleep $DaemonSleep
    do
# if $Exchange exist then an abnormal message(s) is found.
      if [ -f $Exchange ]; then
      
# Try to rename the file. If the attemp fails then try next time.
        mv $Exchange $Dispatch 2>/dev/null || continue
        
# Who is a user(s)?: 13:17:30 Usr     3: Login by <user> on <ttyxxx>. (452)
        echo "------------------------------" >> $Dispatch
        for Usr in `awk '$2=="Usr" {print $3}' $Dispatch`
        do
          test "$User" -eq "0:" continue
          grep "(452)$" $Db.lg|grep " $Usr"|tail -1
        done >> $Dispatch
        
        if [ "$Command" ]
        then $Command $Dispatch 2>/dev/null
        else cat $Dispatch | $SendCmd "$Subject" $SendTo >/dev/null 2>&1
        fi
        rm $Dispatch 2>/dev/null
      fi

# Is the database server running?
      test -f $Db.lk || kill $TailPID 2>/dev/null

# Is awk of PrintAbnormal still running?
      kill -0 $AwkPID 2>/dev/null && continue

# Time to stop PrintAbnormal...
      kill $TailPID 2>/dev/null

# PrintAbnormal is stopped. Exiting...
      date +"%H:%M:%S $Prog: Stopped. PID=$TailPID (2520)" >>$Db.lg
      exit
    done &
  done  # for Db in $DbList
 ;;
#-----------------------------------------------------------
# Foreground mode.
 *)
 # If FindFlag is on then PrintMsgID will says "Please, wait...".
  test "$LastSessionFlag" -a -z "$FindFlag" && \
  echo "Please, wait..." >&2 

  for Db in $DbList
  do
   if [ "$LastSessionFlag" ] #-- only last multi/single-user session --
   then Offset=`egrep -n "(333)|(451)$" $Db.lg | tail -1 | awk -F: '{print $1}'`
   else Offset=1
   fi

   echo "Database: $Db"
   tail +${Offset}l $Db.lg | PrintAbnormal
  done | pg
 ;;
esac

test -f $Temp && rm $Temp


