#!/bin/sh
#
# probkupbinfo,  version 2.0 as of Dec 22, 2002
#
# Shows a Progress backup information
#
# Written by George Potemkin (potemkin@csbi.ru)  
## Localization:
#
# TimeZone variable is used to convert date in the form of elapsed seconds
# from the start of the Epoch (00:00:00, January 1, 1970) to a local time.
# The variable sets a default value that can be overrided by -tz parameter.
#
# TimeZone format: [GMT][+|-]HH[:MM[:SS]]

TimeZone=GMT+03:00 # Moscow, St.Petersburg

# File name of this script:
Script=`basename $0`

# Temporal file (an absolute path is required):
TmpFile=/tmp/$Script.$$

# Bold/unbold terminal capacities:
BD=`tput smso`
UB=`tput rmso`

# ----------------------------------------------------------------------

Usage()
{
  echo "\n\
$Script - shows backup header information.\n\
\n\
Usage: $Script [-tz <TimeZone>] <backup_file_or_device> ...\n\
\n\
Options:\n\
<backup_file_or_device> specifies the backup whose information you want;\n\
\n\
-tz <TimeZone> where <TimeZone> is [GMT][+|-]HH[:MM[:SS]]\n\
This value is used to convert date in the form of elapsed seconds\n\
from the start of the Epoch (00:00:00, January 1, 1970) to a local time.\n\
Default value is $TimeZone." >&2
}

# ----------------------------------------------------------------------

GetByte()
{
# Returns 1-byte unsigned integer.

  Offset=$1

# od -b  Interpret bytes in octal:
  od -b $AnalyzedFile +${Offset}. | \
  awk '{
    Byte=0
    for(i=1;i<=length($2);i++) Byte=substr($2,i,1)+Byte*8
    print Byte; exit
  }' # awk
} # GetByte()

# ----------------------------------------------------------------------

GetShort()
{
# Returns 2-byte unsigned integer.

  Offset=$1

# od -d  Interpret words in unsigned decimal:
  od -d $AnalyzedFile +${Offset}. | \
  awk '{ printf"%d",$2; exit }'

} # GetShort()

# ----------------------------------------------------------------------

GetLong()
{
# Returns 4-byte unsigned integer.

  Offset=$1

# od -D  Interpret long words in unsigned decimal:
  od -D $AnalyzedFile +${Offset}. | \
  awk '{ printf"%d",$2; exit }'

} # GetShort()

# ----------------------------------------------------------------------

GetText()
{
# Returns a null-terminated string.

# $1 sets an offset
  Position=`expr $1 + 1`

  tail +${Position}c $AnalyzedFile | awk 'NR==1 {print; exit}'

} # GetText()

# ----------------------------------------------------------------------

GetDate()
{
# Interprets long words as the time and date in the form of elapsed
# seconds from the start of the Epoch (00:00:00, January 1, 1970).

  Offset=$1

# od -D  Interpret long words in unsigned decimal:
  od -D $AnalyzedFile +${Offset}. | \
  awk '{
    if($2==0) {print "None"; exit}

# TimeZone variable is assigned on input line of awk.
# TimeZone format: [GMT][+|-]HH[:MM[:SS]]
#
# Trim GMT prefix:
    i=index(TimeZone,"GMT")
    if(i>0) TimeZone=substr(TimeZone,i+3)

# Trim the leading blanks:
    while(substr(TimeZone,1,1)==" ") TimeZone=substr(TimeZone,2)

# Get a sign of time zone offset:
    tzSign=substr(TimeZone,1,1)
    if(tzSign=="+") TimeZone=substr(TimeZone,2); else
    if(tzSign=="-") TimeZone=substr(TimeZone,2); else tzSign="+"

# Convert hours, minute and seconds to two digits format:
    n=split(TimeZone,tzTime,":")
    for(i=1;i<=3;i++) while(length(tzTime[i])<2) tzTime[i]="0" tzTime[i]

# Convert timezone to an offset in seconds:
    tzOffset=0; for(i=1;i<=3;i++) tzOffset=60*tzOffset+tzTime[i]

# Set TimeZone in standardized form:
    if(tzTime[2]==0 && tzTime[3]==0) TimeZone=tzTime[1]; else
    if(tzTime[3]==0)   TimeZone=tzTime[1] ":" tzTime[2]; else
    TimeZone=tzTime[1] ":" tzTime[2] ":" tzTime[3]
    TimeZone="GMT" tzSign TimeZone

    if(tzSign=="+") Number=$2+tzOffset
    else            Number=$2-tzOffset

# MonthName array:
    n=split("Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec",MonthName)

# WeekdayName array:
    n=split("Sun Mon Tue Wed Thu Fri Sat",WeekdayName)


# Day number one is Thursday (5) January 1, 1970.
    Day=int(Number/86400)+1

    WeekDay=(Day+3)%7+1
    WeekDay=WeekdayName[WeekDay]

# Year loop to find last Jan 1 before the Day:
    Year=1970; DayBefore=0

    while("true") {
      if(Year%4==0 && (Year%100!=0 || Year%400==0)) 
           {Leap="yes"; DayAfter=DayBefore+366}
      else {Leap="no";  DayAfter=DayBefore+365}
    
      if(DayAfter>Day) break
      Year++;           DayBefore=DayAfter
    } # Year loop

   Day-=DayBefore  # Now it is a number of day from the first day of the year.

# Month loop:
    Month=1; DayBefore=0
    
    while(Month<=12) {
      if(Month==2) if(Leap=="yes") n=29; else n=28
      else if(Month%2==(Month>=8)) n=30; else n=31
    
      DayAfter=DayBefore+n
      if(DayAfter>Day) break
    
      Month++
      DayBefore=DayAfter
    } # Month loop

    Month=MonthName[Month]

    Day-=DayBefore;     #  if(length(Day)==1) Day=" " Day
   
    Number=Number%86400
    Hour=int(Number/3600); if(length(Hour)==1) Hour="0" Hour

    Number=Number%3600
    Min=int(Number/60);    if(length(Min)==1) Min="0" Min
    Sec=Number%60;         if(length(Sec)==1) Sec="0" Sec
   
# Returned string is 33-39 chars long:
    print \
     WeekDay " " Month " " Day " " Hour ":" Min ":" Sec " (" TimeZone ") " Year
    exit
  }' TimeZone="$TimeZone" -   # end of awk
} # GetDate()

# ----------------------------------------------------------------------

BackupHeaderInfo()
{

# Backup header information.

# File:
  Backup=$1

# Progress backup header:
#
# V8     V9     
# Offset Offset Len  Description
# ------ ------ ---  -----------
#   0      0     80  Backup volume description
#  80     80     80  Full path to the database
# 160    160     80  Incremental backup description
# 264    264      4  Date/Time of current backup
# 268    268      4  Date/Time of previous full backup
# 272    272      4  Date/Time of previous incr backup
# 276    276      4  Incremental backup number
# 280    280      4  Database high water (V8 only)
# 284    284      4  Value 0
# 288    288      4  BI blocksize
# 293    293      1  Is online (1 - online, 0 - offline)
# 296    296    4/8  Backup Volume Size (-vs) divided by blocking factor (-bf)
# 300    304      4  Blocking factor (-bf)
# 304    308      4  Backup volume number
# 308    312      4  First backup block in volume: 1+VolSize*(BkupVolNumr-1)
# 312    316      1  Is incremental (2 = Incremental)
# 313    317 60/255  Database path
# 376    620      4  \07 \07 \07 \07 
# 380    624      4  Last transaction number
# 384    628      4  Overlap factor (-io)
# 391    635      1  Is compressed (-com)  (2 = Enable)
# 392    636      2  Redundancy (-red)
# 396    640      4  Backup header CRC
# 400    644      2  Database blocksize

# Copying extent header to a temporal file...
  dd if=$Backup bs=1024 count=1 2>/dev/null >$TmpFile
  
  AnalyzedFile=$TmpFile

  while [ "not break" ]
  do
    
# Backup volume description:  
# volume 1 of backup taken Fri Aug  2 12:34:56 2000 for Progress database:
    BackupVolumeDesc="`GetText 0`"
    
    case "$BackupVolumeDesc" in
      volume*) ;; # expected
            *) echo "$Script: $Backup is not a Progress backup." >&2
               break;;
    esac
    
    echo $BackupVolumeDesc
    
# Full path to the database (80 chars only):
    DbPath="`GetText 80`" 
    echo $DbPath
    
# Incremental Backup Decription
# incremental backup 1, taken after full backup of Fri Aug 1 12:34:56 2000
    IncrBackupDesc="`GetText 160`"
    test "$IncrBackupDesc" && \
    echo "$IncrBackupDesc"
    
# BI Block Size:
    BiBlockSize=`GetLong 288`
    
# Date/Time of current backup:
    CurrBackupDate="`GetDate 264`"
    
# Date/Time of previous full backup:
    PrevFullBackupDate="`GetDate 268`"
    
# Date/Time of previous incr backup:
    PrevIncrBackupDate="`GetDate 272`"
    
# Incremental backup number:
    IncrBackupCount="`GetLong 276`"
    
# Database high water (non-zero value in V8 only):
    DbHWM="`GetLong 280`"
    
# Is online backup (1 - online, 0 - offline)
    Online="`GetByte 293`"
    
    if expr "$DbHWM" != 0 >/dev/null 2>&1; then
#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# if it's V8 backup... 
      
# Backup volume size (-vs):
      VolumeSize=`GetLong 296`
      
# Blocking factor (-bf):
      BlockingFactor=`GetLong 300`
      
# Backup volume number:
      VolumeNumber=`GetLong 304`
      
# First backup block in volume: 1+VolSize*(BkupVolNumr-1)
      FirstBlock=`GetLong 308`
      
# Is incremental (2 = Incremental):
      Incremental=`GetByte 312`
      
# Full path to the database:
      DbPath="`GetText 313`" 
      
# Last transaction number:
      DbTaskId=`GetLong 380`
      
# Overlap factor (-io):
      OverlapFactor=`GetLong 384`
      
# Is compressed (-com)  (2 = Enable):
      Compressed=`GetByte 391`
      
# Redundancy (-red):
      Redundancy=`GetShort 392`
      
# Backup header CRC:
      HeaderCRC=`GetLong 396`
      
# Database blocksize
      DbBlockSize=`GetShort 400`
      
      case $DbBlockSize in
       8192) DbBlocks=`expr $DbHWM / 64`;;
          *) DbBlocks=`expr $DbHWM / 32`;;
      esac
      
    else
#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# if it's V9 backup...
      
# Backup volume size (-vs):
      VolumeSize1=`GetLong 296`
      VolumeSize2=`GetLong 300`
      VolumeSize=$VolumeSize2
      
# Blocking factor (-bf):
      BlockingFactor=`GetLong 304`
      
# Backup volume number:
      VolumeNumber=`GetLong 308`
      
# First backup block in volume: 1+VolSize*(BkupVolNumr-1)
      FirstBlock=`GetLong 312`
      
# Is incremental (2 = Incremental):
      Incremental=`GetByte 316`
      
# Full path to the database:
      DbPath="`GetText 317`" 
      
# Last transaction number:
      DbTaskId=`GetLong 624`
      
# Overlap factor (-io):
      OverlapFactor=`GetLong 628`
      
# Is compressed (-com)  (2 = Enable):
      Compressed=`GetByte 635`
      
# Redundancy (-red):
      Redundancy=`GetShort 636`
      
# Backup header CRC:
      HeaderCRC=`GetLong 640`
      
# Database blocksize
      DbBlockSize=`GetShort 644`
      
# In V9 HWMs are per area:
      DbBlocks=""
      
    fi  # if expr "$DbHWM" != 0
#  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    
    case "$Incremental" in
      2) Incremental="Incremental" ;;
      *) Incremental="Full"        ;;
    esac
    
    case "$Online" in
      1) Online="Online"  ;;
      *) Online="Offline" ;;
    esac
    
    case "$Compressed" in
      2) Compressed="enabled"  ;;
      *) Compressed="disabled" ;;
    esac
    
    VolumeSize=`expr $VolumeSize \* $BlockingFactor`
    
    echo "\nThis is a $Incremental $Online backup."
    echo "              Volume Number: $VolumeNumber"
    echo "        Current Backup Date: $CurrBackupDate"
    echo "       Previous Full Backup: $PrevFullBackupDate"
    
    if [ "$Incremental" = "Incremental" ]; then
    echo "         Incremental Number: $IncrBackupCount"
    echo "Previous Incremental Backup: $PrevIncrBackupDate"
    echo "       Overlap Factor (-io): $OverlapFactor"
    fi
    
    echo "          Redundancy (-red): $Redundancy"
    echo "      Blocking Factor (-bf): $BlockingFactor"
    echo "          Volume Size (-vs): $VolumeSize"
    echo "         Compression (-com): $Compressed"
    echo "    Last Transaction Number: $DbTaskId"
    echo "        Database Block Size: $DbBlockSize"
    echo "    Before-image Block Size: $BiBlockSize"
    echo "  Number of Database Blocks: $DbBlocks"
    
# If V8 backup:    
    test $DbHWM -ne 0 && break
    
# In V9 use prorest -list
    test -x $DLC/bin/_dbutil && \
    $DLC/bin/_dbutil prorest foo $Backup -list 2>/dev/null | \
    awk '
      BEGIN {
        Q=sprintf("%c",34)   # Quotation Mark
        printf"\n"
      }
      /^Area Name: / { AreaName=substr($0,12); next }
      
# Size = (_AreaStatus-Totblocks + 1) * RecordsPerBlock
      /Size:/ {gsub(","," ")
        printf"   %-12s  %8d blocks\n",(Q AreaName Q ":" $7 "," $4),($2/$4-1)
      }
    ' # awk
    
    break
    
  done  #  while [ "not break" ]
  
} # BackupHeaderInfo()

# ----------------------------------------------------------------------

# Parsing input parameters...

while [ "$1" != "" ]
do
  case $1 in
    -tz) shift
         if [ $1 = "" ]; then
           echo "\
$Script: You have not supplied an argument for -tz parameter." >&2
           Usage
           exit 1
         fi
         TimeZone=$1; shift
         continue # while $1 != ""
      ;;
       
     -*) echo "\
$Script: Could not recognize argument: $1." >&2
         Usage
         exit 1
      ;;
       
      *) BackupList="$BackupList $1"; shift
         continue # while $1 != ""
      ;;
  esac
done  # while $1 != ""


# Input parameters do not reffer to a valid database name:
if [ "$BackupList" = "" ]
then
  echo "$Script: You must supply a backup file." >&2
  Usage
  exit 1
fi


for Backup in $BackupList
do
  BackupHeaderInfo $Backup
done

test $TmpFile && rm $TmpFile

