#!/bin/sh
#
# dbinfo,   Version 1.0 as of Jan 07, 2003
#
# Shows a database information (The Truth is Out There ;-).
#
# Written by George Potemkin (potemkin@csbi.ru)  
# 
# 
# The script shows:
#
# 1. Database attributes stored in master block.
#    Some of them are not available through Progress commands or VSTs.
#    Some (e.g. timestamps) are distorted ever in read-only mode;
# 2. Timestamps of each database extents;
# 3. Highwatermarks, number of empty and free blocks per area;
# 3. Shared-memory segment and semaphore ID (only first ones) used by database;
# 4. Status of the process that opened database (based on .lk file);
# 5. Login statistics for last database session (based on .lg file).
#
# The script requires a read access to the database files.
#
# Localization:
#
# TimeZone variable is used to convert date in the form of elapsed seconds
# from the start of the Epoch (00:00:00, January 1, 1970) to a local time.
# TimeZone format: [GMT][+|-]HH[[:]MM[[:]SS]]

TimeZone=`date "+%z" 2>/dev/null`
# e.g. 0300 - Moscow, St.Petersburg

# Time Zome Acronyms:
#http://greenwichmeantime.com/info/timezone.htm
case "$TimeZone" in
[0-9]*) ;;
GMT)  TimeZone=GMT+00;; # Greenwich Mean
UTC)  TimeZone=GMT+00;; # Universal Co-ordinated
WET)  TimeZone=GMT+00;; # Western European
CET)  TimeZone=GMT+01;; # Central European
EET)  TimeZone=GMT+02;; # Eastern European
ART)  TimeZone=GMT+02;;
BT)   TimeZone=GMT+03;; # Baghdad
CCT)  TimeZone=GMT+08;; # China Coast
JST)  TimeZone=GMT+09;; # Japan Standard
GST)  TimeZone=GMT+10;; # Guam Standard
IDLE) TimeZone=GMT+12;; # International Date Line East
NZST) TimeZone=GMT+12;; # New Zealand Standard
WAT)  TimeZone=GMT-01;; # West Africa
AT)   TimeZone=GMT-02;; # Azores
AST)  TimeZone=GMT-04;; # Atlantic Standard
EST)  TimeZone=GMT-05;; # Eastern Standard
IET)  TimeZone=GMT-05;;  
CST)  TimeZone=GMT-06;; # Central Standard
MST)  TimeZone=GMT-07;; # Mountain Standard
PNT)  TimeZone=GMT-07;; # US/Arizona, America/Denver, Canada/Mountain...
PST)  TimeZone=GMT-08;; # Pacific Standard
YST)  TimeZone=GMT-09;; # Yukon Standard
AHST) TimeZone=GMT-10;; # Alaska-Hawaii Standard
CAT)  TimeZone=GMT-10;; # Central Alaska
HST)  TimeZone=GMT-10;; # Hawaii Standard
NT)   TimeZone=GMT-11;; # Nome
MIT)  TimeZone=GMT-11;;
IDLW) TimeZone=GMT-12;; # International Date Line West

# Daylight Time
BST)  TimeZone=GMT+01;; # British Summer Time
ADT)  TimeZone=GMT-03;; # Atlantic Daylight
EDT)  TimeZone=GMT-04;; # Eastern Daylight
CDT)  TimeZone=GMT-05;; # Central Daylight
MDT)  TimeZone=GMT-06;; # Mountain Daylight
PDT)  TimeZone=GMT-07;; # Pacific Daylight
YDT)  TimeZone=GMT-08;; # Yukon Daylight
HDT)  TimeZone=GMT-09;; # Hawaii Daylight
MEST) TimeZone=GMT+02;; # Middle European Summer
MESZ) TimeZone=GMT+02;; # Middle European Summer
SST)  TimeZone=GMT+02;; # Swedish Summer
FST)  TimeZone=GMT+02;; # French Summer

# Unknown timezone:
*)    TimeZone=GMT+00;;
esac

# File name of this script:
Script=`basename $0`

# Temporal file (an absolute path is required):
TmpFile=/tmp/$Script.$$

# Bold/unbold terminal capacities:
BD=`tput smso`
UB=`tput rmso`

# Default flags (script options). See Usage() below.
DefaultFlags="ml"
ValidFlags="maelA"

# ----------------------------------------------------------------------

Usage()
{
  echo "\n\
$Script - shows a database information.\n\
Script requires a read access to the database files.\n\
\n\
Usage: $Script [-maeA] <dbname> ...\n\
\n\
Options:\n\
<dbname> specifies the database whose information you want;\n\
\n\
-m  Report field values stored in Master block.\n\
-a  Report area statistics stored in Object blocks.\n\
-e  Report extent information.\n\
-l  Report login statistics of last database session based on the .lg file.\n\
-A  Report all data. Equivalent to -mael.\n\
Default options: -$DefaultFlags" >&2
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

  tail +${Position}c $AnalyzedFile | \
  dd bs=512 count=1 2>/dev/null | \
  awk '{print $0; exit}'

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
# TimeZone format: [GMT][+|-]HH[[:]MM]
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
    i=index(TimeZone,":")
    if(i>0) {HH=substr(TimeZone,1,i-1); MM=substr(TimeZone,i+1)}
      else  {HH=substr(TimeZone,1,2);   MM=substr(TimeZone,3)}
    while(length(HH)<2) HH="0" HH
    while(length(MM)<2) MM="0" MM

# Set TimeZone in standardized form:
    TimeZone="GMT" tzSign HH
    if(MM!="00") TimeZone="GMT" tzSign HH ":" MM; else
    if(HH!="00") TimeZone="GMT" tzSign HH       ; else
                 TimeZone="GMT"

# Convert timezone to an offset in seconds:
    tzOffset=3600*HH+60*MM

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

    Day-=DayBefore;        if(length(Day)==1) Day=" " Day
   
    Number=Number%86400
    Hour=int(Number/3600); if(length(Hour)==1) Hour="0" Hour

    Number=Number%3600
    Min=int(Number/60);    if(length(Min)==1) Min="0" Min
    Sec=Number%60;         if(length(Sec)==1) Sec="0" Sec
   
# Returned string is 33-39 chars long:
    printf"%3s %3s %2s %s:%s:%s (%s) %d",
          WeekDay, Month, Day, Hour, Min, Sec, TimeZone, Year
    exit
  }' TimeZone="$TimeZone" -   # end of awk
} # GetDate()

# ----------------------------------------------------------------------

# Standard 16-byte block header:
#
# Offset Len  Field         Description
# ------ ---  ---------     -----------
# 0-3      4                Dbkey
# 4        1                Block type
# 5        1  bk_frchn      Chain type        
# 6-7      2  bk_incr       Backup counter    
# 8-11     4  bk_nextf      Chain next block  
# 12-15    4  bk_updctr     Block version     

# ----------------------------------------------------------------------

ExtentHeader83() 
{
# Information in V8 extent header.
# Version of the structure: 80 or 83

# V8 Extent Header Structure:
# 
# version 80      PROGRESS Release 8.[0-2]
# version 83      PROGRESS Release 8.3
# 
# Structure of V8 Extent Header exactly matches to a structure of
# V8 Master Block (msg 6596 calls it as an extent master block).
# There are the minor differences:
#
# 1. Depending from how a database was created BlockType and ChainType of
#    Extent Headers can be zeroed or equates 1 and 127 correspondingly
#    (i.e. the same as of Master Block).
#    
# 2. The mb_dbcnt and mb_bicnt fields are zeroed in Master Block.
#    These fields in Extent Header of a .db file store
#    the numbers, correspondingly, of the .d# and .b# extents.
#    
# 3. Master Block reserves a space for a path to a .db file but it's empty.
#    The field in Extent Headers stores the correct path.
#
# The fields that are meaningfull in Extent Header
#
# Offset Len  Field         Description
# ------ ---  ---------     -----------
# 00-15   16  bk_*          Standard 16-byte block header
# 16-17    2  mb_dbvers     Database version number
# 116-119  4  mb_crdate     Date/Time of database creation         Msg 604
# 120-123  4  mb_oprdate    Date/Time of most recent database open Msg 606
# 140-141  2  mb_dbcnt      Number of the .d# extents
# 142-143  2  mb_bicnt      Number of the .b# extents
# 328-582 255               Path to .db file (in extent headers only)

# Extent header should be already copied to a temporal file (TmpFile).
  AnalyzedFile=$TmpFile
  
# Extent create date (mb_crdate):
  ExtentCreateDate=`GetDate 116`
  
# Extent last open date (mb_oprdate):
  ExtentOpenDate=`GetDate 120`
  test "$DbExtentOpenDate"
  
# Number of the .d# extents (mb_dbcnt):
  ExtentDbCnt=`GetShort 140` 
  
# Number of the .b# extents (mb_bicnt):
  ExtentBiCnt=`GetShort 142`
  
# Path to the .db file:
  ExtentDbPath=`GetText 328`

} # ExtentHeader83()


# ----------------------------------------------------------------------

ExtentHeader91() 
{
# Information in V9 extent header.
# Version of the structure: 91

# version 91      PROGRESS Release 9.[0-1]

# V9 Extent Header Structure
#
# Offset Len  Description
# ------ ---  -----------
# 00-15   16  Standard 16-byte block header
# 16-17    2  Db long version = db blocksize + db version
# 18-19    2  Value 64 (00 40)
# 24-27    4  Extent create date     (msg 9214,9216)
# 28-31    4  Zero. Treated as a create date.
# 32-35    4  Extent last open date  (msg 9215,9217)
# 36-39    4  Extent last close date (msg 9215,9217)
#    
# In V9 only first 40th bytes in Extent Header Block are meaningfull.
  
# Extent header should be already copied to a temporal file (TmpFile).
  AnalyzedFile=$TmpFile
  
# Extent create date:
  ExtentCreateDate=`GetDate 24`
  
# Extent last open date:
  ExtentOpenDate=`GetDate 32`
  
# Extent last close date:
  ExtentCloseDate=`GetDate 36`

} # ExtentHeader91()


# ----------------------------------------------------------------------

ExtentHeaderInfo() 
{
# Database extent information.

# File:
  ExtentFile=$1

# Copying extent header to a temporal file...
  dd if=$ExtentFile bs=1024 count=1 2>/dev/null >$TmpFile
  
  AnalyzedFile=$TmpFile
  
  Dbkey=`GetLong 0`
  BlockType=`GetByte 4`
  ChainType=`GetByte 5`

# Db block size and db version as specified in extent header:
  ExtentVersion=`GetShort 16` # = DbBlockSize + DbVersion

# Structure version:
  StructureVersion=`expr $ExtentVersion % 256`
  
  
  case $StructureVersion in
    78) echo "\
WARNING: File $BD$ExtentFile$UB is a V7 database.\n\
$Script does not support this version of Progress."
        ExtentVersion=""  # Means - stop processing.
        ;;
    80) ExtentHeader83;;  # Progress V8.[0-2]
    83) ExtentHeader83;;  # Progress V8.3
    91) ExtentHeader91;;  # Progress V9.X
     *) echo "\
ERROR: File $BD$ExtentFile$UB has a unknown structure.\n\
Found: dbkey $Dbkey, type $BlockType, chain $ChainType, version $ExtentVersion"
        ExtentVersion=""  # Means - stop processing.
        ;;
  esac
  
  
  case $ExtentFile in
    *.db)
# Save the extent attributes of Control Area for a comparison.
          DbExtentVersion=$ExtentVersion
          DbExtentCreateDate=$ExtentCreateDate
          DbExtentOpenDate=$ExtentOpenDate
# V9 only:
          DbExtentCloseDate=$ExtentCloseDate
# V8 only:
          MVDbPath=$ExtentDbPath
          mb_dbcnt=$ExtentDbCnt
          mb_bicnt=$ExtentBiCnt
          ;;
       *)
# Sanity checks:
          test "$ExtentVersion" != "$DbExtentVersion" && \
          echo "\
WARNING: Version number mismatch.\n\
Extent $BD$ExtentFile$UB\n\
   has a different version number $ExtentVersion,\n\
Control Area has a version number $DbExtentVersion.\n"
# Progress ignores this mismatch.
           
          test "$ExtentCreateDate" != "$DbExtentCreateDate" && \
          echo "\
ERROR: Creation date mismatch. (9214)\n\
Extent $BD$ExtentFile$UB\n\
     has the wrong creation date of $ExtentCreateDate, (9212)\n\
Control Area has a creation date of $DbExtentCreateDate. (9216)\n"
          
# Extent last open date:
          test "$ExtentOpenDate" != "$DbExtentOpenDate" && \
          echo "\
ERROR: Last open date mismatch. (9215)\n\
Extent $BD$ExtentFile$UB\n\
   has a different last open date of $ExtentOpenDate,(9213)\n\
Control Area has a last open date of $DbExtentOpenDate.(9217)\n"
  
# Extent last close date:
          test "$ExtentCloseDate" != "$DbExtentCloseDate" && \
          echo "\
WARNING: Last close date mismatch.\n\
Extent $BD$ExtentFile$UB\n\
   has a different last close date of $ExtentCloseDate,\n\
Control Area has a last close date of $DbExtentCloseDate.\n"
# Progress reports it as open date mismatch.
          
          test "$ExtentDbPath" != "$MVDbPath" && \
          echo "\
WARNING: Extent $BD$ExtentFile$UB is part\n\
 of MV database $ExtentDbPath. (608)\n"
# Progress ignores this mismatch.

  esac # $ExtentFile in

} # ExtentHeaderInfo()

# ----------------------------------------------------------------------

MasterBlockExtent83()
{
# A list of extent path names of V8 multi-volume databaseis
# stored in after Extent Header block.
# Note that these blocks does not have a standard 16-byte header.
# Dbname.d1 is a first name in the list.

# Offset Len  Description
# ------ ---  -----------
# 0-4      4  Size of extent in KB
# 8        1  Length of extent path
# 9-267  255  Extent path
# Shift by 268 bytes for next extents.

# File (must be *.db):
  DbExtent=$1

# Copying object block to a temporal file...
  dd if=$DbExtent bs=$DbBlockSize skip=1 count=1 2>/dev/null >$TmpFile
  
  AnalyzedFile=$TmpFile
  
  GetText 9

} # MasterBlockExtent83()

# ----------------------------------------------------------------------

MasterBlockExtent91()
{

# Path to dbname.d1 extent is stored in Control Block (block type 13).

# Control Block is a second block in *.db file.

# Offset Len  Description
# ------ ---  -----------
# 00-15   16  Standard 16-byte block header
# 16-19    4  Dbkey of root block of _Area._Area-Number index
# 24-27    4  Dbkey of root block of _AreaExtent._AreaExtent-Area index
# 32-288 255  Path to dbname.d1 extent

# File (must be *.db):
  DbExtent=$1

# Copying object block to a temporal file...
  dd if=$DbExtent bs=$DbBlockSize skip=1 count=1 2>/dev/null >$TmpFile
  
  AnalyzedFile=$TmpFile
  
  GetText 32

} # MasterBlockExtent91()

# ----------------------------------------------------------------------

ObjectBlockInfo()
{
# Exist only in V9
# Block type is 12.

# Object Block is a third block in .d1 extents and stores information like
# _AreaStatus-Totblocks, _AreaStatus-Hiwater,
# _AreaStatus-Freenum,   _AreaStatus-Rmnum

# File (must be *.d1):
  ExtentFile=$1

  echo "\n\
Object block information (`date '+%a %b %e %T %Y'`):\n\
--------------------------------------------------- \n\
Extent: $ExtentFile\n"

# Offset Len  Description
# ------ ---  -----------
# 00-15   16  Standard 16-byte block header
# 16-19    4  Total number of blocks                 _AreaStatus-Totblocks
# 20-23    4  High Water Mark                        _AreaStatus-Hiwater
# 24-27    4  Head block of Free Chain
# 28-31    4  Head block of RM Chain
# 32-35    4  Head block of Index Delete Chain
# 36-39    4  Num blocks on Free Chain               _AreaStatus-Freenum
# 40-43    4  Num blocks on RM Chain                 _AreaStatus-Rmnum
# 44-47    4  Num blocks on Index Delete Chain
# 48-51    4  Tail block of Free Chain
# 52-55    4  Tail block of RM Chain
# 56-59    4  Tail block of Index Delete Chain
                            
# Only first 60 bytes in Object Block are meaningfull.

# Copying object block to a temporal file...
  dd if=$ExtentFile bs=$DbBlockSize skip=2 count=1 2>/dev/null >$TmpFile
  
  AnalyzedFile=$TmpFile
  
  Dbkey=`GetLong 0`
  BlockType=`GetByte 4`
  ChainType=`GetByte 5`
  
# Object Block - block Type: 12, chain type: 127
  if [ "$BlockType" -ne 12 -a "$ChainType" -ne 127 ]; then
    Offset=`expr $DbBlockSize \* 2`
    echo "\
ERROR: Block at offset $Offset in $BD$ExtentFile$UB has an unexpected type,\n\
    Found: type $BlockType, chain $ChainType (dbkey $Dbkey)\n\
Should be: type 12, chain 127 (Object Block)"
  
  else
# If a valid block type:
    
    RecordPerBlock=`expr $Dbkey / 2` 
    echo "      Record per block: $RecordPerBlock"
    
# Total number of blocks in area:
    TotalBlockNum=`GetLong 16`
    echo "Total number of blocks: $TotalBlockNum"
    
# Area high water mark:
    HighWaterMark=`GetLong 20`
    echo "  Area high water mark: $HighWaterMark"
    
# Dbkey of the first block of Free chain:
    FreeChainHead=`GetLong 24`
    
# Dbkey of the first block of RM chain:
    RMChainHead=`GetLong 28`
    
# Dbkey of the first block of Index Delete chain:
    IdxChainHead=`GetLong 32`
    
# Number of blocks in Free chain:
    FreeChainLen=`GetLong 36`
    
# Number of blocks in RM chain:
    RMChainLen=`GetLong 40`
    
# Number of blocks in Index Delete chain:
    IdxChainLen=`GetLong 44`
    
# Dbkey of the last block of Free chain:
    FreeChainTail=`GetLong 48`
    
# Dbkey of the last block of RM chain:
    RMChainTail=`GetLong 52`
    
# Dbkey of the last block of Index Delete chain:
    IdxChainTail=`GetLong 56`
    
    echo "            Free chain: $FreeChainLen blocks \
(first dbkey: $FreeChainHead, last dbkey: $FreeChainTail)"
    
    echo "              RM chain: $RMChainLen blocks \
(first dbkey: $RMChainHead, last dbkey: $RMChainTail)"
    
    echo "    Index Delete chain: $IdxChainLen blocks \
(first dbkey: $IdxChainHead, last dbkey: $IdxChainTail)"
    
  fi # if valid block type
  
} # ObjectBlockInfo()

# ----------------------------------------------------------------------

ShmIdInfo()
{
# For specified ID print ipcs -m and proutil -C dbipcs

  ShmID=$1

# Clean the temp file. 
# awk will put here CPID - process ID of the creator of shared memory.
  echo "" > $TmpFile
      
  ipcs -ma | \
  awk '
    $2=="ID"     {
      LabelLst=substr($0, 2) " "
      next
    }
    $2=='$ShmID' {
      ValueLst=substr($0, 2) " "
    
      print "ipcs -ma:"
      while("not break") {
      
# LabelBeg is a first position of Label in LabelLst:
        LabelBeg=1
        while(substr(LabelLst, LabelBeg, 1)==" " ) LabelBeg++
        
# ValueEnd is a position of first blank after Label:
        LabelEnd=LabelBeg + index(substr(LabelLst, LabelBeg+1)," ")
        
# LabelNxt is a begin of next Label in LabelLst:
        LabelNxt=LabelEnd+1
        while( substr(LabelLst,LabelNxt,1)==" " ) LabelNxt++
        
# ValueBeg is a starting position of Value in ValueLst:
        ValueBeg = 1
        while(substr(ValueLst, ValueBeg, 1)==" ") ValueBeg++
        
# ValueEnd is a position first blank after Value:
        ValueEnd=ValueBeg + index(substr(ValueLst, ValueBeg+1)," ")
        
# NATTCH and SEGSZ columns can have no space in between, e.g.
# NATTCH  SEGSZ  CPID
#     155173248  1938
# In this case the end position of Value is defined
# by the end position of label:
        
        if(ValueEnd>LabelNxt) ValueEnd=LabelEnd
        
        Label=substr(LabelLst, LabelBeg, LabelEnd-LabelBeg)
        Value=substr(ValueLst, ValueBeg, ValueEnd-ValueBeg)
        
        if(Label=="" || Value=="") break
        
        printf"%8s: %s\n", Label, Value
        
        if(Label ~/PID/)  \
        system("ps -fp " Value " || echo Process PID=" Value " not found.")
        
        if(Label ~/CPID/) print Value >"'$TmpFile'"
        
# ValueEnd=max(ValueEnd,LabelEnd):
        if(ValueEnd<LabelEnd) ValueEnd=LabelEnd
        
# Do not truncate the list from ValueEnd+1
# ValueEnd can be a position of non-blank character:
        LabelLst=substr(LabelLst, ValueEnd)
        ValueLst=substr(ValueLst, ValueEnd)
      } # while( ValueLst > "" && LabelLst > "" )

      exit

    } # $2==ID
  '  # end of awk
  
  ShmPID="`cat $TmpFile`"
    
#PROGRESS SHARED MEMORY STATUS
# ID   ShMemVer   Seg#  InUse     Database
  test -x $DLC/bin/_proutil && \
  $DLC/bin/_proutil -C dbipcs 2>/dev/null | \
  awk '
    $1=='$ShmID' {
      printf"%8s: %s\n", "ShMemVer", $2
      printf"%8s: %s\n", "Database", substr($0,index($0,$5))
      exit }
  ' # end of awk
} #  ShmIdInfo()

# ----------------------------------------------------------------------

SemIdInfo()
{
# For specified ID print ipcs -s

  SemID=$1

  ipcs -sa | \
  awk '
# ipcs -sa does not have the columns that can have no space in between.
    $2=="ID"     { for(i=2;i<=NF;i++) Label[i]=$i; next }
    $2=='$SemID' { print "ipcs -sa:"
                   for(i=2;i<=NF;i++) printf"%8s: %s\n", Label[i], $i
                   exit }
  ' # end of awk
} #  ShmIdInfo()

# ----------------------------------------------------------------------

MasterBlock83()
{
# Database master block information.
# Block type is 1.

# Database must have a structure version 80 or 83.
# version 80      PROGRESS Release 8.[0-2]
# version 83      PROGRESS Release 8.3
# 
# Offset Len  Field         Description
# ------ ---  ---------     -----------
# 00-15   16  bk_*          Standard 16-byte block header
# 16-17    2  mb_dbvers     Database version number
# 18-19    2  mb_dbstate    Current state of the database
# 20-21    2  mb_cfilnum    Highest table number defined in database
# 22       1  mb_tainted    Tainted flag
# 23       1  mb_flags      Integrity flag
# 24-27    4  mb_totblks    Total number of database blocks
# 28-31    4  mb_lasttask   Last transaction number
# 32-35    4  mb_frstf[1]   Dbkey of first block on Free chain
# 36-39    4  mb_frstf[2]   Dbkey of first block on RM chain
# 40-43    4  mb_numf[1]    Free blocks below highwater mark
# 44-47    4  mb_numf[2]    Record blocks with free space
# 48-51    4  mb_hiwater    Database blocks high water mark
# 52-55    4  mb_lastf      Dbkey of last block on RM chain
# 56-59    4  mb_aibegin    Date/time rfutil aimage begin was executed
# 60-63    4  mb_ainew      Date/Time of last rfutil aimage new
# 64-67    4  mb_aigennbr   After Image extent sequence number
# 68-71    4  mb_aiopen     After-image open date
# 72-75    4  mb_lstmod     Date/Time of last database update
# 76-79    4  mb_aiwrtloc   Bytes written to AI file
# 80-83    4  mb_aictr      Number of AI notes
# 84       1  mb_aiflgs     
# 85       1                Word-break rule number
# 86-87    2                Word-break file CRC
# 88-91    4  mb_biopen     Last open date of BI file
# 92-95    4  mb_biprev     Date/Time of previous BI open (zero)
# 96-100   4  mb_rltime     How long database was in use since BI truncated
# 100-101  2  mb_rlclsize   BI cluster size in 16K
# 104      1  mb_aisync     After-image I/O: 1 - BUFFERED, 0 - Reliable
# 105      1  mb_chgd       Database changed since last probkup
# 106      1  mb_bistate    BI truncated: 19 - yes, 20 - not
# 107      1  mb_langok     
# 108      1  br_language   proutil -C language <collation> (V6.2D-6.3)
# 110-111  2  mb_aiseq      
# 114-115  2  mb_buseq      Full backup sequence number
# 116-119  4  mb_crdate     Date/Time of database creation 
# 120-123  4  mb_oprdate    Date/Time of most recent database open
# 124-127  4  mb-oppdate    Date/Time of previous database open
# 128-131  4  mb_fbdate     Date/Time of most recent full backup
# 132-135  4  mb_ibdate     Date/Time of most recent increment backup
# 136-139  4  mb_ibseq      Incremental backup sequence number
# 140-141  2  mb_dbcnt      Number of .d# extents (in extent header only)
# 142-143  2  mb_bicnt      Number of .b# extents (in extent header only)
# 144-151  8                Code of demo version
# 152-155  4                Demo version (0x0000000A)  Msg 144
# 168      1  mb_lightai    On if 2phase commit is enabled
# 169      1  mb_crd        Priority for coordinator database (_Logging-2PCPriority)
# 172-175  4  mb_tl.aibegin Date/time of proutil -C 2phase begin
# 176-179  4  mb_tl.ainew   Date/time of last re-use of Transaction Log file
# 180-183  4  mb_tl.aigennbr Number of times Transaction Log was re-used
# 184-187  4  mb_tl.aiopen  Transaction Log open date
# 188-191  4  mb_tlwrtloc   Bytes written to Transaction Log file
# 192-195  4  mb_tlctr      Number of TL notes
# 196-197  2                BI blockSize
# 198-199  2                AI blocksize
# 200-203  4  mb_ixdbk[0]   Dbkey of first Index Anchor block
# ...                       Kbase #20477
# 324-327  4  mb_ixdbk[31]  Dbkey of 32nd Index Anchor block
# 328-582 255               Path to .db file (in extent headers only)
# 588-591  4                Dbkey of Sequence Block
# 592-595  4  mb_dbrecid    Recid of _Db record
# 600-603  4  mb_timestamp  Schema timestamp
# 604-607  4                Shared-memory ID (V8.0B and higher)
# 608-611  4                Semaphore ID
  
# Current state of database (_MstrBlk-dbstate or _DbStatus-state)
# Database Administration Guide and Reference ->
# Progress Monitor R&D Options -> Status Displays -> Database ->
# Table A-2: Database States
# or kbase 13922
# cmpdb messages when compare two databases, mb_dbstate 
    
  mb_dbstate=`GetShort 18`
  case $mb_dbstate in
    0) Desc="Empty database"     ;;
    1) Desc="Changed since open" ;;
    2) Desc="Closed or not changed since open";;
    4) Desc="Crash recovery"     ;;
    5) Desc="Idxbuild is running";;
    8) Desc="Restore is underway";;
    *) Desc="Unknown state"      ;;
  esac
  echo "         Current state of database: $mb_dbstate ($Desc)"
    
# Tainted flag (_MstrBlk-tainted or _DbStatus-tainted):
# Database Administration Guide and Reference ->
# Table A-3: Database Damaged Flags
    
  mb_tainted=`GetByte 22`
  case $mb_tainted in
    0) Desc="Untainted database";;
    1) Desc="Opened with -F" ;; # Your database was damaged. (37)
    2) Desc="Crashed with -i";;                    # Msg 510,509
    4) Desc="Crashed with -r";;                    # Msg 514,517
    8) Desc="Not been fully restored";;            # Msg 1621
   16) Desc="Backup not completed"   ;;            # Msg 1553
   32) Desc="Db requires rebuild of all indexes";; # Msg 2356
    *) Desc="Unknown flag";;
  esac
  echo "                      Tainted flag: $mb_tainted ($Desc)"
    
# Integrity flag (_MstrBlk-integrity or _DbStatus-IntFlags)
# Database Administration Guide and Reference ->
# Table A–4: Integrity Flags
  mb_flags=`GetByte 23`
  case $mb_flags in
    0) Desc="Full integrity"    ;;
    2) Desc="Executing with -i" ;;
    4) Desc="Executing with -r" ;;
   16) Desc="Backup in progress";;
    *) Desc="Unknown flag"      ;;
  esac
  echo "                    Integrity flag: $mb_flags ($Desc)"
  
# Highest table number defined in database:
  mb_cfilnum=`GetShort 20`
  echo "              Highest table number: $mb_cfilnum"
    
# Last transaction ID (_MstrBlk-lasttask):
  mb_lasttask=`GetLong 28`
  echo "               Last transaction ID: $mb_lasttask"
  
# Word-break rule number:
  WrdRuleNum=`GetByte 85`
  echo "            Word-break rule number: $WrdRuleNum"
    
# Word-break file CRC:
  WrdFileCRC=`GetShort 86`
  echo "               Word-break file CRC: $WrdFileCRC"
  
# The mb_dbcnt field stored in extent header of .db file:
  echo "         Number of the .d# extents: $mb_dbcnt"
  
# The mb_bicnt field stored in extent header of .db file:
  echo "         Number of the .b# extents: $mb_bicnt"

# Total number of database blocks:
  mb_totblks=`GetLong 24`
  echo "   Total number of database blocks: $mb_totblks"
  
# Database blocks high water mark:
  mb_hiwater=`GetLong 48`
  echo "   Database blocks high water mark: $mb_hiwater"

# Free blocks below highwater mark:
  mb_numf1=`GetLong 40`
  echo "  Free blocks below highwater mark: $mb_numf1"
  
# Dbkey of first block on Free chain:
  mb_frstf1=`GetLong 32`
  echo "Dbkey of first block on Free chain: $mb_frstf1"
  
# Record blocks with free space:
  mb_numf2=`GetLong 44`
  echo "     Record blocks with free space: $mb_numf2"
  
# Dbkey of first block on RM chain:
  mb_frstf2=`GetLong 36`
  echo "  Dbkey of first block on RM chain: $mb_frstf2"
  
# Dbkey of last block on RM chain
  mb_lastf=`GetLong 52`
  echo "   Dbkey of last block on RM chain: $mb_lastf"

# Dbkey of Sequence block (block type 6):
  SeqDbkey=`GetLong 588`
  echo "           Dbkey of Sequence Block: $SeqDbkey"
  
# Recid of _Db record (mb_dbrecid):
  mb_dbrecid=`GetLong 592`
  echo "               Recid of _Db record: $mb_dbrecid"
  
# Dbkeys of Index Anchor blocks:
  echo "     Dbkeys of Index Anchor blocks:"
  Offset=200
  Count=0
  while test $Count -lt 32
  do
    mb_ixdbk=`GetLong $Offset`
    test $mb_ixdbk -ne 0 &&\
    echo "     mb_ixdbk[$Count]: $mb_ixdbk"

    Offset=`expr $Offset + 4`
    Count=`expr $Count + 1`
  done  

# After-Image information:
  echo ""
  
# After-Image block size:
  AiBlockSize=`GetShort 198`
  echo "            After-Image block size: $AiBlockSize"
   
# Date/time rfutil -C aimage begin was executed (_Logging-AiBegin);
  mb_aibegin=`GetDate 56`
  echo "  Date of last rfutil aimage begin: $mb_aibegin"
  
  if [ "$mb_aibegin" != "None" ]; then
# Date/Time of last rfutil aimage new (_Logging-AiNew):
    mb_ainew=`GetDate 60`
    echo "    Date of last rfutil aimage new: $mb_ainew"
      
# After Image extent sequence number (_Logging-AiGenNum)
    mb_aigennbr=`GetLong 64`
    echo "After-Image extent sequence number: $mb_aigennbr"
      
# mb_aiflgs:
    mb_aiflgs=`GetByte 84`
    echo "                 After-Image flags: $mb_aiflgs"
    
# After-Image I/O (_Logging-AiIO):
# rfutil db -C aimage begin|change [buffered|unbuffered]
# Buffered writes are not allowed with after-image extents (3689)
    mb_aisync=`GetByte 104`
    case $mb_aisync in
      0) Desc="Unbuffered"   ;;
      1) Desc="Buffered"     ;;
      *) Desc="Unknown value";;
    esac    
    echo "                   After-Image I/O: $mb_aisync ($Desc)"
      
# Bytes written to AI file:
    mb_aiwrtloc=`GetLong 76`
    echo "          Bytes written to AI file: $mb_aiwrtloc"
    
# Number of AI notes:
    mb_aictr=`GetLong 80`
    echo "                Number of AI notes: $mb_aictr"
  fi # if $mb_aibegin != "None"
  
# Two-phase commit attributes:
  echo ""
  
# Date/time proutil -C 2phase begin was executed (_Logging-AiBegin);
  mb_tlbegin=`GetDate 172`
  echo " Date of last proutil 2phase begin: $mb_tlbegin"
  
# Before-Image information:
  echo ""
  
# Before-Image block size:
  BiBlockSize=`GetShort 196`
  echo "           Before-Image block size: $BiBlockSize"
  
  mb_rlclsize=`GetShort 100`
  mb_rlclsize=`expr $mb_rlclsize \* 16`
  echo "         Before-Image cluster size: ${mb_rlclsize}K"
    
  mb_bistate=`GetByte 106`
  case $mb_bistate in
    19) Desc="Truncated"    ;; # msg 3615 calls it as BITRUNC
    20) Desc="Must exist"   ;;
     *) Desc="Unknown value";;
  esac    
  echo "            Before-Image truncated: $mb_bistate ($Desc)"
    
# How long database was in use since the BI file was truncated:
# _DbStatus-bitrunc / _MstrBlk-rltime
  mb_rltime=`GetLong 96`
  Desc=`echo $mb_rltime | \
        awk '{D=int($1/86400) # Days
              S=$1%86400
              H=int(S/3600)   # Hours
              S=S%3600
              M=int(S/60)     # Minutes
              S=S%60          # Seconds
              if(D>1)  printf"%d days ",D; else
              if(D==1) printf"%d day ", D
              printf"%dh %dm %ds",H,M,S  #10h 20m 30s
             }'`
  echo "Database in use since BI truncated: $mb_rltime secs ($Desc)"
  
# Database timestamps:
  echo ""

  echo "            Prostrct creation date: $DbExtentCreateDate"
  
# Database creation date (_DbStatus-CreateDate)
  mb_crdate=`GetDate 116`
  echo " Database was created with procopy: $mb_crdate"
  
# Schema timestamp (_DbStatus-CacheStamp / _MstrBlk-timestamp):
  mb_timestamp=`GetDate 600`
  echo "                  Schema timestamp: $mb_timestamp"
  
# Date/Time the database was last modified:
  mb_lstmod=`GetDate 72`
  echo "            Database last modified: $mb_lstmod"
  
# Most recent database open (_DbStatus-LastOpen or _MstrBlk-oprdate):
  mb_oprdate=`GetDate 120`
  echo "         Most recent database open: $mb_oprdate"
  
# Previous database open to check for 'backwards' time:
# _DbStatus-PrevOpen or _MstrBlk-oppdate
  mb_oppdate=`GetDate 124`
  echo "            Previous database open: $mb_oppdate"
  
# Most recent BI file open (_MstrBlk-biopen):
# _DbStatus-BiOpen or _MstrBlk-biopen
  mb_biopen=`GetDate 88`
  echo "          Most recent BI file open: $mb_biopen"
  
# Most recent AI file open (_Logging-AiOpen):
  mb_aiopen=`GetDate 68`
  test "$mb_aiopen" != "None" && \
  echo "          Most recent AI file open: $mb_aiopen"
  
  test "$DbExtentOpenDate" && \
  echo "      Extent header last open date: $DbExtentOpenDate"
  test "$DbExtentCloseDate" && \
  echo "     Extent header last close date: $DbExtentCloseDate"
  
# Backup information:
  echo ""
  
# Most recent Full backup (_DbStatus-fbDate or _MstrBlk-fbdate):
  mb_fbdate=`GetDate 128`
  echo "           Most recent Full backup: $mb_fbdate"
  
# Most recent incremental backup (_DbStatus-ibDate or _MstrBlk-ibdate):
  mb_ibdate=`GetDate 132`
  echo "    Most recent incremental backup: $mb_ibdate"
  
# Backup counter (full+incremental):
  mb_buseq=`GetShort 114`
  echo "                    Backup counter: $mb_buseq"
  
# Incremental backup counter:
  mb_ibseq=`GetLong 136`
  echo "        Incremental backup counter: $mb_ibseq"
  
# Database changed since last probkup (_DbStatus-Changed):
  mb_chgd=`GetByte 105`
  case $mb_chgd in
    0) Desc="No changes"   ;;
    1) Desc="Changed"      ;;
    *) Desc="Unknown value";;
  esac
  echo "Database changed since last backup: $mb_chgd ($Desc)"
  
  
  ShmID=`GetLong 604` # 0xffffffff if not
  SemID=`GetLong 608` # 0xffffffff if not
  
# Print ipcs -ma information with 1 column:
  if [ $ShmID -ne 0 -a $ShmID -ne -2147483648 ]; then
    echo "\n                  Shared Memory ID: " $ShmID
    ShmIdInfo $ShmID    
  fi  # if ShmID -ne 0 -a $ShmID -ne -2147483648 
  
  
# Print ipcs -sa information with 1 column:
  if [ $SemID -ne 0 -a $SemID -ne -2147483648 ]; then
    echo "\n                      Semaphore ID: " $SemID
    SemIdInfo $SemID
  fi  # if SemID -ne 0 -a SemID -ne -2147483648
  
} # MasterBlock83()


# ----------------------------------------------------------------------

MasterBlock91()
{
# Database master block information.
# Block type is 1.

# Database must have a structure version 91.
# version 91      PROGRESS Release 9.[0-1]
# 
# Offset Len  Field         Description
# ------ ---  ---------     -----------
# 00-15   16  bk_*          Standard 16-byte block header
# 16-17    2  mb_dbvers     Database version number
# 20-23    4                Database minor version (_DbStatus-DbVersMinor)
# 24-27    4                Client minor version   (_DbStatus-ClVersMinor) Msg 8405
# 28-29    2  mb_dbstate    Current state of the database
# 30-31    2  mb_cfilnum    Highest table number defined in database
# 32       1  mb_tainted    Tainted flag
# 33       1  mb_flags      Integrity flag
# 34       1                Are Large Files enabled: 1 - yes, 0 - no
# 40-43    4  mb_lasttask   Last transaction number
# 84-87    4  mb_aibegin    Date/time rfutil aimage begin was executed
# 88-91    4  mb_ainew      Date/Time of last rfutil aimage new
# 92-95    4  mb_aigennbr   After Image extent sequence number
# 96-99    4  mb_aiopen     After-Image open date
# 100-103  4  mb_lstmod     Date/Time of last database update
# 104-107  4  mb_aiwrtloc   Bytes written to AI file
# 108-111  4  mb_aictr      Number of AI notes
# 112      1  mb_aiflgs     
# 113      1                Word-break rule number
# 114-115  2                Word-break file CRC
# 116-119  4  mb_biopen     Last open date of BI file
# 120-123  4  mb_biprev     Date/Time of previous BI open (zero)
# 124-127  4  mb_rltime     How long database was in use since BI truncated
# 128-129  2  mb_rlclsize   BI cluster size in 16K
# 132      1  mb_aisync     After-Image I/O
# 133      1  mb_chgd       Database changed since last probkup
# 134      1  mb_bistate    BI truncated: 19 - yes, 20 - not
# 135      1  mb_langok     
# 136      1  br_language   
# 142-143  2  mb_buseq      Full backup sequence number
# 144-147  4  mb_crdate     Date/Time of database creation
# 148-151  4  mb_oprdate    Date/Time of most recent database open
# 152-155  4  mb-oppdate    Date/Time of previous database open
# 156-159  4  mb_fbdate     Date/Time of most recent full backup
# 160-163  4  mb_ibdate     Date/Time of most recent increment backup
# 164-167  4  mb_ibseq      Incremental backup sequence number
# 172-179  8                Code of demo version
# 180-183  4                Demo version (0)
# 196      1  mb_lightai    On if 2phase commit is enabled
# 197      1  mb_crd        Priority for the coordinator database (_Logging-2PCPriority)
# 198      1                Database is enabled for site replication
# 199      1                Database is enabled for Peer Direct replication
# 208-211  4  mb_tlwrtloc   Bytes written to TL file
# 212-215  4  mb_tlwrtoffset
# 216-223  8  mb_aiwrtloc64 Bytes written to AI file
# 224-225  2                BI blockSize
# 226-227  2                AI blocksize
# 616-619  4                Dbkey of Sequence Block
# 620-623  4  mb_dbrecid    Recid of _Db record
# 628-631  4  mb_timestamp  Schema timestamp
# 632-635  4                Shared-memory ID
# 636-639  4                Semaphore ID
# 640-643  4  mb_objectRoot Root of _StorageObject._Object-Id index
# ???-???  ?  mb_shutdown   (New in V9)      


# mb_objectRoot is:
# FIND FIRST _File NO-LOCK 
#      WHERE _File._File-Name EQ "_StorageObject".
# 
# FIND FIRST _Index NO-LOCK 
#      WHERE _Index._Index-Name EQ "_Object-Id".
# 
# FIND FIRST _StorageObject NO-LOCK
#      WHERE _StorageObject._Db-recid      EQ _File._Db-recid
#        AND _StorageObject._Object-type   EQ 2
#        AND _StorageObject._Object-number EQ _Index._Idx-num.
# 
# DISPLAY _StorageObject._Object-root LABEL "mb_objectRoot".
#
  
# Database minor version (_DbStatus-DbVersMinor)
  DbVersMinor=`GetLong 20`
  case $DbVersMinor in
    0) Desc="V9.0X"    ;;
    1) Desc="V9.1A"    ;;
    2) Desc="V9.1A05"  ;;
    3) Desc="V9.1[B-D]";;
    *) Desc="Unknown"  ;;
  esac
  echo "            Minor database version: $DbVersMinor ($Desc)"
  
# Current state of database (_MstrBlk-dbstate or _DbStatus-state)
# Database Administration Guide and Reference ->
# Progress Monitor R&D Options -> Status Displays -> Database ->
# Table A-2: Database States
# or kbase 13922
# cmpdb messages when compare two databases, mb_dbstate 
    
  mb_dbstate=`GetShort 28`
  case $mb_dbstate in
    0) Desc="Empty database"     ;;
    1) Desc="Changed since open" ;;
    2) Desc="Closed or not changed since open";;
    4) Desc="Crash recovery"     ;;
    5) Desc="Idxbuild is running";;
    8) Desc="Restore is underway";;
    *) Desc="Unknown state"      ;;
  esac
  echo "         Current state of database: $mb_dbstate ($Desc)"
    
# Tainted flag (_MstrBlk-tainted or _DbStatus-tainted):
# Database Administration Guide and Reference ->
# Table A-3: Database Damaged Flags
    
  mb_tainted=`GetByte 32`
  case $mb_tainted in
    0) Desc="Untainted database";;
    1) Desc="Opened with -F" ;; # Your database was damaged. (37)
    2) Desc="Crashed with -i";;                    # Msg 510,509
    4) Desc="Crashed with -r";;                    # Msg 514,517
    8) Desc="Not been fully restored";;            # Msg 1621
   16) Desc="Backup not completed"   ;;            # Msg 1553
   32) Desc="Db requires rebuild of all indexes";; # Msg 2356
    *) Desc="Unknown flag";;
  esac
  echo "                      Tainted flag: $mb_tainted ($Desc)"
    
# Integrity flag (_MstrBlk-integrity or _DbStatus-IntFlags)
# Database Administration Guide and Reference ->
# Table A–4: Integrity Flags
  mb_flags=`GetByte 33`
  case $mb_flags in
    0) Desc="Full integrity"    ;;
    2) Desc="Executing with -i" ;;
    4) Desc="Executing with -r" ;;
   16) Desc="Backup in progress";;
    *) Desc="Unknown flag"      ;;
  esac
  echo "                    Integrity flag: $mb_flags ($Desc)"
    
# Are Large Files enabled (1 - yes, 0 - no):
  EnableLargeFiles=`GetByte 34`
  case $EnableLargeFiles in
    0) Desc="Not enabled"  ;;
    1) Desc="Enabled"      ;;
    *) Desc="Unknown value";;
  esac
  echo "               Large Files enabled: $EnableLargeFiles ($Desc)"
    
# Last transaction ID (_MstrBlk-lasttask):
  mb_lasttask=`GetLong 40`
  echo "               Last transaction ID: $mb_lasttask"
  
# Word-break rule number:
  WrdRuleNum=`GetByte 113`
  echo "            Word-break rule number: $WrdRuleNum"
    
# Word-break file CRC:
  WrdFileCRC=`GetShort 114`
  echo "               Word-break file CRC: $WrdFileCRC"
    
# Database is enabled for site replication:
# proutil <dbname> -C enableSiteReplication [source | target]
  
  SiteReplication=`GetByte 198`
  case $SiteReplication in
    0) Desc="Not enabled"  ;;
    1) Desc="Enabled"      ;;
    *) Desc="Unknown value";;
  esac    
  echo "                  Site replication: $SiteReplication ($Desc)"
    
# Database is enabled for Peer Direct replication
# proutil <dbname> -C enablePDR
  
  PeerDirectReplication=`GetByte 199`
  case $PeerDirectReplication in
    0) Desc="Not enabled"  ;;
    1) Desc="Enabled"      ;;
    *) Desc="Unknown value";;
  esac    
  echo "            PeerDirect replication: $PeerDirectReplication ($Desc)"
  
# Dbkey of Sequence block (block type 6):
  SeqDbkey=`GetLong 616`
  echo "           Dbkey of Sequence Block: $SeqDbkey"
  
# Recid of _Db record (mb_dbrecid):
  mb_dbrecid=`GetLong 620`
  echo "               Recid of _Db record: $mb_dbrecid"
    
# Dbkey of root block of _StorageObject._Object-Id index
  mb_objectRoot=`GetLong 640`
  echo "Dbkey of _StorageObject index root: $mb_objectRoot"
  
# After-Image information:
  echo ""
  
# After-Image block size:
  AiBlockSize=`GetShort 226`
  echo "            After-Image block size: $AiBlockSize"
   
# Date/time rfutil aimage begin was executed (_Logging-AiBegin);
  mb_aibegin=`GetDate 84`
  echo "  Date of last rfutil aimage begin: $mb_aibegin"
  
  if [ "$mb_aibegin" != "None" ]; then
# Date/Time of last rfutil aimage new (_Logging-AiNew):
    mb_ainew=`GetDate 88`
    echo "    Date of last rfutil aimage new: $mb_ainew"
      
# After Image extent sequence number (_Logging-AiGenNum)
    mb_aigennbr=`GetLong 92`
    echo "After-Image extent sequence number: $mb_aigennbr"
      
# mb_aiflgs:
    mb_aiflgs=`GetByte 112`
    echo "                 After-Image flags: $mb_aiflgs"
    
# After-Image I/O (_Logging-AiIO):
# rfutil db -C aimage begin|change [buffered|unbuffered]
# Buffered writes are not allowed with after-image extents (3689)
    mb_aisync=`GetByte 132`
    case $mb_aisync in
      0) Desc="Unbuffered"   ;;
      1) Desc="Buffered"     ;;
      *) Desc="Unknown value";;
    esac    
    echo "                   After-Image I/O: $mb_aisync ($Desc)"
      
# Bytes written to AI file:
    mb_aiwrtloc=`GetLong 104`
    echo "          Bytes written to AI file: $mb_aiwrtloc"
    
# Number of AI notes:
    mb_aictr=`GetLong 108`
    echo "                Number of AI notes: $mb_aictr"
  fi # if $mb_aibegin != "None"
  
# 2phase commit:
  echo ""
  
# 2phase commit is enabled (_Logging-AiBegin):
  mb_lightai=`GetByte 196`
    case $mb_lightai in
      0) Desc="Not enabled"  ;;
      1) Desc="Enabled"      ;;
      *) Desc="Unknown value";;
    esac
  echo "          2phase commit is enabled: $mb_lightai ($Desc)"
  
# Before-Image information:
  echo ""
  
# Before-Image block size:
  BiBlockSize=`GetShort 224`
  echo "           Before-Image block size: $BiBlockSize"
  
  mb_rlclsize=`GetShort 128`
  mb_rlclsize=`expr $mb_rlclsize \* 16`
  echo "         Before-Image cluster size: ${mb_rlclsize}K"
    
  mb_bistate=`GetByte 134`
  case $mb_bistate in
    19) Desc="Truncated"    ;;
    20) Desc="Must exist"   ;;
     *) Desc="Unknown value";;
  esac    
  echo "            Before-Image truncated: $mb_bistate ($Desc)"
    
# How long database was in use since the BI file was truncated:
# _DbStatus-bitrunc / _MstrBlk-rltime
  mb_rltime=`GetLong 124`
  Desc=`echo $mb_rltime | \
        awk '{D=int($1/86400) # Days
              S=$1%86400
              H=int(S/3600)   # Hours
              S=S%3600
              M=int(S/60)     # Minutes
              S=S%60          # Seconds
              if(D>1)  printf"%d days ",D; else
              if(D==1) printf"%d day ", D
              printf"%dh %dm %ds",H,M,S  #10h 20m 30s
             }'`
  echo "Database in use since BI truncated: $mb_rltime secs ($Desc)"
  
# Database timestamps:
  echo ""
  
  echo "              Prostrct create date: $DbExtentCreateDate"
  
# Database creation date (_DbStatus-CreateDate or _MstrBlk-crdate)
# BTW, _DbStatus-CreateDate returns undefined value in each 
# odd database session (single- or multi-user).
  mb_crdate=`GetDate 144`
  echo "            Database creation date: $mb_crdate"
  
# Schema timestamp (_DbStatus-CacheStamp / _MstrBlk-timestamp):
  mb_timestamp=`GetDate 628`
  echo "                  Schema timestamp: $mb_timestamp"
  
# Date/Time the database was last modified:
  mb_lstmod=`GetDate 100`
  echo "            Database last modified: $mb_lstmod"
  
# Most recent database open (_DbStatus-LastOpen or _MstrBlk-oprdate):
  mb_oprdate=`GetDate 148`
  echo "         Most recent database open: $mb_oprdate"
  
# Previous database open to check for 'backwards' time:
# _DbStatus-PrevOpen or _MstrBlk-oppdate
  mb_oppdate=`GetDate 152`
  echo "            Previous database open: $mb_oppdate"
  
# Most recent BI file open (_MstrBlk-biopen):
# _DbStatus-BiOpen or _MstrBlk-biopen
  mb_biopen=`GetDate 116`
  echo "          Most recent BI file open: $mb_biopen"
  
# Most recent AI file open (_Logging-AiOpen):
  mb_aiopen=`GetDate 96`
  test "$mb_aiopen" != "None" && \
  echo "          Most recent AI file open: $mb_aiopen"
  
  echo "      Extent header last open date: $DbExtentOpenDate"
  echo "     Extent header last close date: $DbExtentCloseDate"
  
# Backup information:
  echo ""
  
# Most recent Full backup (_DbStatus-fbDate or _MstrBlk-fbdate):
  mb_fbdate=`GetDate 156`
  echo "           Most recent Full backup: $mb_fbdate"
  
# Most recent incremental backup (_DbStatus-ibDate or _MstrBlk-ibdate):
  mb_ibdate=`GetDate 160`
  echo "    Most recent incremental backup: $mb_ibdate"
  
# Backup counter (full+incremental):
  mb_buseq=`GetShort 142`
  echo "                    Backup counter: $mb_buseq"
  
# Incremental backup counter:
  mb_ibseq=`GetLong 164`
  echo "        Incremental backup counter: $mb_ibseq"
  
# Database changed since last probkup (_DbStatus-Changed):
  mb_chgd=`GetByte 133`
  case $mb_chgd in
    0) Desc="No changes"   ;;
    1) Desc="Changed"      ;;
    *) Desc="Unknown value";;
  esac
  echo "Database changed since last backup: $mb_chgd ($Desc)"
  
  
  ShmID=`GetLong 632` # 0xffffffff if not
  SemID=`GetLong 636` # 0xffffffff if not
  
# Print ipcs -ma information with 1 column:
  if [ $ShmID -ne 0 -a $ShmID -ne -2147483648 ]; then
    echo "\n                  Shared Memory ID: " $ShmID
    ShmIdInfo $ShmID    
  fi  # if ShmID -ne 0 -a $ShmID -ne -2147483648 
  
  
# Print ipcs -sa information with 1 column:
  if [ $SemID -ne 0 -a $SemID -ne -2147483648 ]; then
    echo "\n                      Semaphore ID: " $SemID
    SemIdInfo $SemID
  fi  # if SemID -ne 0 -a SemID -ne -2147483648
  
} # MasterBlock91()

# ----------------------------------------------------------------------

MasterBlockInfo()
{
# Database master block information.

# File (must be dbname.d1):
  ExtentFile=$1

  echo "\n\
Database master block information (`date '+%a %b %e %T %Y'`):\n\
------------------------------------------------------------ \n\
Database: $Db.db\n\
  Extent: $ExtentFile\n"

# Get DbBlockSize from previously gotten DbExtentVersion:
  DbVersion=`expr $DbExtentVersion % 256`
  DbBlockSize=`expr $DbExtentVersion - $DbVersion`
  
# Copying database master block to a temporal file...
  case $ExtentFile in
    *.db) Skip=0;;  # V8 single-volumme database
    *.d1) Skip=1;;  # V8/9 multi-voulme database
  esac
  
  dd if=$ExtentFile bs=$DbBlockSize skip=$Skip count=1 2>/dev/null >$TmpFile

  AnalyzedFile=$TmpFile

  Dbkey=`GetLong 0`
  BlockType=`GetByte 4`
  ChainType=`GetByte 5`

# Master Block - block Type: 1, chain type: 127
  if [ "$BlockType" -ne 1 -a "$ChainType" -ne 127 ]; then
    echo "\
ERROR: Block at offset $DbBlockSize in $BD$ExtentFile$UB has an unexpected type,\n\
    Found: type $BlockType, chain $ChainType (dbkey $Dbkey).\n\
Should be: type 1, chain 127 (Master Block)."
  
  else
# If valid block type:
    
    echo "                             Dbkey: $Dbkey"

# Db block size and db version as specified in master block:
    mb_dbvers=`GetShort 16`
    
# Compare database version in master block with version of .db extent:
    test "$mb_dbvers" -ne "$DbExtentVersion" && \
    echo "\
WARNING: Version number mismatch.\n\
Master Block in $BD$ExtentFile$UB\n\
   has a different version number $mb_dbvers,\n\
Control Area has a version number $DbExtentVersion.\n"
# Progress ignores this mismatch.

    
# Database version quates 91 for V9.0A through 9.1D
    DbVersion=`expr $mb_dbvers % 256`
    case $DbVersion in
      78) Desc="V7.[2-3]";;
      80) Desc="V8.[0-2]";;
      83) Desc="V8.3"    ;;
      91) Desc="V9.X"    ;;
       *) Desc="unknown" ;;
    esac
    echo "                        Db version: $DbVersion ($Desc)"
    
# Database blocksize:
    DbBlockSize=`expr $mb_dbvers - $DbVersion`
    echo "                     Db block size: $DbBlockSize"

# Don't care of void database. 
# Most of the fields (e.g. Db Open Date) will be just empty.

    case $DbVersion in
      80) MasterBlock83;;
      83) MasterBlock83;;
      91) MasterBlock91;;
       *) ;;
    esac

  fi # if valid block type
  
} # MasterBlockInfo()

# ----------------------------------------------------------------------

LockFileInfo()
{
# Parsing .lk file...

  AnalyzedFile=$1

# Database mode:
  lkMode=`GetLong 0`
  case $lkMode in
    1) Desc="Single-user mode";;
    2) Desc="Multi-user mode" ;;
   64) Desc="Crash recovery"  ;;
    *) Desc="Unknown mode"    ;;
  esac
  
# ID of the process that opens database:
  lkPID=`GetLong 4`
  
# Host where the process is running:
  lkHost=`GetText 8`

  echo "\n\n\
Lock file (.lk) information (`date '+%a %b %e %T %Y'`):\n\
------------------------------------------------------ \n"
  echo "Database mode: $lkMode ($Desc)"
  echo "Opened by PID: $lkPID"
  echo "      On Host: $lkHost"
  echo "    This host: `uname -n`"
  ps -fp $lkPID || echo "Process PID=$lkPID not found."

} # LockFileInfo()

# ----------------------------------------------------------------------

PrintExtentList()
{
# Print the extent names of a specified multi-volume database.

# Path to a database without .db extent (prostrct does not like .db)
  DbName=$1

# Postrct will create a temporal structure file:
  TmpStFile=${TmpFile:-/tmp/$$}.st
  test -f $TmpStFile && rm $TmpStFile

  test -x $DLC/bin/_dbutil && \
  DbUtil=$DLC/bin/_dbutil  || \
  DbUtil=_dbutil

  $DbUtil prostrct list $DbName $TmpStFile >/dev/null 2>&1

# if .st file exists and has a size greater than zero:
  if [ -s $TmpStFile ]; then

# Parsing .st file...
    awk '
      BEGIN {Quot=sprintf("%c",34)}  # Quotation Mark
      
      NF<2 {next}
      
      $1=="d" {
        Area=""
        Line=substr($0,3)
        i=index(Line,Quot)
        
        if(i>0) {
          n=1+index(substr(Line,i+1),Quot)  # Second quotation
          n=n+index(substr(Line,i+n)," ")   # Space after quotation
          Area=substr(Line,i,n-1)
          Line=substr(Line,n+1)
        }
        
        n=split(Line,arr)
        Path=arr[1]
        Type=arr[2]; if(Type=="") Type="v"
        Size=arr[3]; if(Size=="") Size=0
        
        printf"%s %s %d %s\n", Path, Type, Size, Area
        next
      } # $1=="d" 
      
# Other (b, a, t) extent types:      
      { Path=$2
        Type=$3; if(Type=="") Type="v"
        Size=$4; if(Size=="") Size=0
        
        printf"%s %s %d\n", Path, Type, Size
      }
    ' $TmpStFile # awk

  else # if prostrct list fails (e.g. due to a wrong version):

# Parsing .db file...

    case $DbVersion in
      80|83) Skip=1 # Skip Extent Header
             ;;
         91) Skip=2 # Skip Extent Header and Control Block
             ;;
    esac
    
    dd if=$DbName.db bs=$DbBlockSize skip=$Skip 2>/dev/null | \
    strings -       | \
    grep "/$DbName" | \
    awk '
      BEGIN {FS="."}
      
      $NF=="db" {next} # Skip *.db file.
      
      { 
# If extent path is long the strings can return garbage.

# Relative path is ./dbname_##.d## where length of dbname < 11
# i.e. total length could not be great than 32. No garbage here:
        if(length($0)<=32) Path=$0; else

# Full path should begins with "/" but the second char could not be a slash:
        if($1  ~/^\/\//) Path=substr($0,2); else
        if($1 !~/^\// )  Path=substr($0,index($0,"/"))
      
# Long string without garbage:
        else              Path= $0

        n=split(Path,arr,"/")
        Name=arr[n]
        Name=substr(Name,1,length(Name)-length($NF)-1)
        Sort=index("dbat",substr($NF,1,1)) "x"
        Area=""
        if($NF ~/^d/) {
          Area=substr(Name,length("'$DbName'")+1)
          if(Area ~/^_/) Area=substr(Area,2)
          if(Area == "") Area=6
          Sort=Area
          while(length(Sort)<4) Sort="0" Sort
          Sort=Sort Area
          Area="Data Area #" Area
        } else
          Sort=Sort $NF
        printf"%s %s ? 0 %s\n", Sort, Path, Area
      }
    '    | \
    sort | \
    while read Sort Other
    do
     echo $Other
    done
  fi  # if [ -s $TmpStFile ]
  
  test -f $TmpStFile && rm $TmpStFile

} # PrintExtentList()

# ----------------------------------------------------------------------

LogFileInfo()
{
# Parsing .lg file for startup parameter
# and login statistics of last database session.

  DbLogFile=$1

  LogSize=`ls -l $DbLogFile | awk '{ printf"%d", $5 / 1024}'`

  echo "\n\n\
Log file (.lg) information (`date '+%a %b %e %T %Y'`):\n\
----------------------------------------------------- \n\
File: $DbLogFile\n\
Size: ${LogSize}K"

# Multi-user session begin. (333)
  LastMultiUser=`
    grep -n "\(333\)" $DbLogFile | \
    tail -1 | \
    awk 'BEGIN {FS=":"}; {print $1-1}'`
# grep -n precedes each line by its line number
  LastMultiUser=${LastMultiUser:-"1"}

# <single-session-type> session begin for <user> on <ttyxxx>. (451)
  LastSingleUser=`
    tail +$LastMultiUser $DbLogFile | \
    grep -n "\(451\)" | \
    tail -1 | \
    awk 'BEGIN {FS=":"}; {print $1-1}'`

  if [ "$LastSingleUser" != "" ]; then
# Last time the database is/was opened in single-user mode:
# Printing all messages from this session...

    LastSingleUser=`expr $LastMultiUser + $LastSingleUser - 1`
    
    echo "Last single-user session (from line $LastSingleUser):\n"
    
    tail +$LastSingleUser $DbLogFile

  else  
# Last time the database is/was opened in multi-user mode
# Printing parameters and login statistics for this session...
      
    echo "Last multi-user session (from line $LastMultiUser):\n"

    tail +$LastMultiUser $DbLogFile | \
    awk '
      BEGIN {
        CurSelf=0; MaxSelf=0; TotSelf=0;
        CurRemC=0; MaxRemC=0; TotRemC=0
        MaxSrv=0
# First line is expected to be date/time.
        getline; print
      }
      
# Line contains Date. E.g.:        Sat Jan  4 12:25:08 2003
      $0 ~/^   / && NF==5 && $4 ~/[0-9]*:[0-9]*:[0-9]*/ {
        LastDate=$0
      }
      
# Print the parameters of last multi-user session:
      $2=="BROKER" {print; next}
      
# <Background process> Started. (2518) 
      /\(2518\)$/ {
        print
        next
      }
      
#12:34:56 BROKER  0: Multi-user session end. (334)
# On exit print the accumulated statistics:
#      /\(334\)/ && /BROKER/ {exit}
      
# Self-service user logins:
#12:34:56 Usr <Usr>: Login by <user> on <ttyxxx>. (452)
      /\(452\)$/ {
        CurSelf++
        TotSelf++
        if(CurSelf > MaxSelf) MaxSelf=CurSelf
        LastUser=$0
        next
      }
      
# Self-service user logouts or abnormal terminations:
#12:34:56 Usr <Usr>: Logout by USER on DEVICE. (453)
#12:34:56 WDOG  <n>: Disconnecting dead user <Usr>. (2527)
#12:34:56 Usr <Usr>: ** Save file named core for analysis by PSC. (439)
      /\(453\)$/ || /\(2527\)$/ || /\(439\)$/ { 
        if(CurSelf) CurSelf--
        LastUser=$0
        next
      }
      
# For each Remote Client Server stores a ports, pid and
# the numbers of current and max logins:
#Sv          Cur.   Max.   Port
#No    Pid  Users  Users    Num

# The rest deals with the messages from Remote Client Servers:      
      $2!="SRV" { next }
         
#Trimming a tailed colon from <Srv> number...
      { Srv=substr($3,1,length($3)-1) }
            
#12:34:56 SRV <Srv>: Started on port <port> using tcp, pid <pid>. (5646)
      /\(5646\)$/ {
        SrvPort[Srv]=$7
        SrvPID[Srv]=substr($(NF-1),1,length($(NF-1))-1) #Trimming dot
        CurUsers[Srv]=0
        MaxUsers[Srv]=0
        if(Srv > MaxSrv) MaxSrv=Srv
        next
      }
      
# Remote user logins:
#12:34:56 SRV <Srv>: Login usernum , userid , on . (742)
      /\(742\)$/ {
        CurUsers[Srv]=CurUsers[Srv]+1
        CurRemC++
        TotRemC++
        if(CurUsers[Srv] > MaxUsers[Srv]) MaxUsers[Srv]=CurUsers[Srv]
        if(CurRemC > MaxRemC) MaxRemC=CurRemC
        LastUser=$0
        next
      }
      
# Remote user logouts:
#12:34:56 SRV <Srv>: Logout usernum , userid , on . (739)
      /\(453\)$/ || /\(739\)$/ {
        CurUsers[Srv]=CurUsers[Srv]-1
        if(CurRemC) CurRemC--
        LastUser=$0
        next
      }
      
      END {
        printf"\n"
        printf"Self-service logins: current %d, max %d, total %d\n",\
                                    CurSelf,   MaxSelf, TotSelf
        printf"      Remote logins: current %d, max %d, total %d\n",\
                                    CurRemC,   MaxRemC, TotRemC
        
# Print the title only if at least one SERV was started:
        if(MaxSrv>0) {
          printf"\nStatistics by Servers:\n"
          printf"  Srv   Port          Cur.   Max.\n"
          printf"  Num    Num    Pid  Users  Users\n"
#            --> 12345  12345  12345  12345  12345
          
          for(Srv=1; Srv<=MaxSrv; Srv++) if(SrvPort[Srv]!=0)
            printf"%5d  %5d  %5d  %5d  %5d\n", \
              Srv, SrvPort[Srv], SrvPID[Srv], CurUsers[Srv], MaxUsers[Srv]
        }
        if(LastUser) {
          printf"\nLast login/logout:\n%s\n%s\n", LastDate, LastUser
        }
        printf"\n"
      }
    ' # awk
  fi
}  # LogFileInfo()

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# Main block:

# Parsing input parameters...

ActiveFlags=""
ArgList=""

while [ "$1" ]
do
  case $1 in
    -db) shift  # let -db <dbname> to be a valid format.
         if "$1" == "" ]; then
           echo "$Script: You have not supplied a parameter for argument -db." >&2
           break # while $1 != ""
         fi
      ;;
        
     -*) Flags=`echo $1 | tr -d "\-"`
         Other=`echo $Flags | tr -d "$ValidFlags"`
        
         if [ "$Other" ]; then
           echo "$Script: Could not recognize argument: -$Other." >&2
           Usage
           exit 1
         fi
         ActiveFlags=$ActiveFlags$Flags
         shift
         continue # while $1 != ""
      ;;
  esac

  ArgList="$ArgList $1"
  shift

done  # while $1 != ""

# Input parameters do not reffer to a valid database name:
if [ -z "$ArgList" ]
then
  echo "$Script: You must supply a database name." >&2
  Usage
  exit 1
fi

# If no options specified the default options will be used:
ActiveFlags=${ActiveFlags:-$DefaultFlags}

# The -A option means all options:
echo "$ActiveFlags" | grep "A" >/dev/null && \
ActiveFlags=$ValidFlags

echo "$ActiveFlags" | grep "m" >/dev/null && Flag_m="m"
echo "$ActiveFlags" | grep "a" >/dev/null && Flag_a="a"
echo "$ActiveFlags" | grep "e" >/dev/null && Flag_e="e"
echo "$ActiveFlags" | grep "l" >/dev/null && Flag_l="l"

for Arg in $ArgList
do

# Using fully specified pathenames without .db suffix... 
  DbPath=`dirname $Arg`
  DbPath=`(cd $DbPath; pwd)`  # Full path
  DbName=`basename $Arg .db`
  Db=$DbPath/$DbName
  
  if [ ! -f $Db.db ]; then
    echo "$Script: File $Db.db not found!" >&2
    continue # for Arg in $ArgList
  fi

  if [ ! -r $Db.db ]; then
# -rw-r--r--    1 user     group       262144 Dec  7 15:31 dbname.db
    Permissions="`ls -l $Db.db|awk '{printf"%s %s:%s",$1,$3,$4}'`"
    echo "\
$Script: You are not allow to read $Db.db file ($Permissions)." >&2
    continue # for Arg in $ArgList
  fi
         

# Extent header of .db file:
  
  ExtentHeaderInfo $Db.db

# Stop processing if the extent header has wrong format:
  test "$ExtentVersion" || \
  continue  # Arg in $ArgList

# Structure version:
  DbVersion=`expr $ExtentVersion % 256`
  
# Database blocksize:
  DbBlockSize=`expr $ExtentVersion - $DbVersion`
  
  case $DbBlockSize in
    1024|2048|4096|8192) # Valid blocksizes
    ;;
    *) echo "$Script: \
Invalid blocksize ($DbBlockSize) in extent header of $BD$Db.db$UB." >&2
       continue  # for Arg in $ArgList
    ;;
  esac

  test "$MVDbPath" && \
  test "$MVDbPath" != "$Db.db" && \
  echo "\
WARNING: Database $BD$Db.db$UB\n\
     is a copy of $MVDbPath. (598)"
  
# ---------------------------------------------------
# Flag -m  Report field values stored in Master block.
  if [ "$Flag_m" ]; then
  
# Get a MasterBlockExtent:
    case $DbVersion in
      80|83)
        test "$MVDbPath" && \
        MasterBlockExtent=`MasterBlockExtent83 $Db.db` || \
        MasterBlockExtent=$Db.db
      ;;
      91)
        MasterBlockExtent=`MasterBlockExtent91 $Db.db`
        
# Convert realtive path to a full one:
        test `dirname $MasterBlockExtent` = "." && \
        MasterBlockExtent=$DbPath/`basename $MasterBlockExtent`
      ;;
    esac
    
    if [ ! -f "$MasterBlockExtent" ]; then
      echo "\
ERROR: Extent $BD$MasterBlockExtent$UB not found."
      continue  # for Arg in $ArgList
    fi      
      
    if [ ! -r "$MasterBlockExtent" ]; then
      echo "\
WARNING: You are not allow to read $BD$MasterBlockExtent$UB."
      continue  # for Arg in $ArgList
    fi

    MasterBlockInfo $MasterBlockExtent
        
    test -r $Db.lk && \
    LockFileInfo $Db.lk
        
    test "$lkPID" != "$ShmPID" && \
    echo "$ERROR: \
SHM CPID ($BD$ShmPID$UB) does not match PID ($BD$lkPID$UB) in lk file."
  fi  # if [ "$Flag_m" ]; then

# ------------------------------------------------------
# Flag -a and/or flag -e
  
  test "$Flag_a$Flag_e" && \
  ( 
# Extracting pathes to other database extents...
#
# In relative path database the extent pathes are relative to the .db file.
# So temporally (in new shell) change the working directory
# to the database directory:
    
    cd $DbPath
    
    PrintExtentList $DbName | \
    while read ExtentName ExtentType ExtentSize AreaInfo
    do
    
      if [ ! -f "$ExtentName" ]; then
        echo "\
ERROR: Extent $BD$ExtentName$UB not found."
        continue  # while read Extent
      fi      
      
#-rw-r--r--    1 user     group     21234 Jan  1 12:34 dbname.d1
      Permissions=`ls -l $ExtentName | awk '{printf"%s %s:%s",$1,$3,$4}'`
      
      if [ ! -r $ExtentName ]; then
        echo "\
WARNING: You are not allow to read $BD$ExtentName$UB ($Permissions)."
        continue  # while read Extent
      fi
      
# Flag -a  Report area statistics stored in Object block:
# ------------------------------------------------------
      test "$Flag_a" && \
      case $ExtentName in *.d1)  # Data areas.
        echo "\nArea: $AreaInfo\n========================"
        ObjectBlockInfo $ExtentName
        ;;
      esac
      
# Flag -e  Report extent information:
# ----------------------------------
      if [ "$Flag_e" ]; then

        FileSize=`ls -l $ExtentName | awk '{printf"%d",$5}'`
        FileSizeK=`expr $FileSize / 1024`
        
# Variable length extent with defined upper size limit:
        test "$ExtentType" = "v" -a $ExtentSize -ne 0 \
        && Limit=", Limit: ${ExtentSize}K" \
        || Limit=""
        
        echo "\n\
Extent: $ExtentName\n\
  Type: $ExtentType, Size: ${FileSizeK}K$Limit, Permissions: $Permissions"
        
# Size of fixed length extent should  match the real size of the file:
        ExpectSize=`expr $ExtentSize \* 1024`
        test "$ExtentType" = "f" -a $ExpectSize -ne 0 && \
        test $FileSize -ne $ExpectSize && \
        echo "\
ERROR: Extent $BD$ExtentName$UB has wrong size.\
Found: $FileSize,  expected: $ExpectSize."
        
# ExtentHeaderInfo performs only a timestamp sanity check:
        ExtentHeaderInfo $ExtentName
      fi  # if $Flag_e
    done  # while read Extent*
  )

# ---------------------------------------------------------
# Flag -l  Report login statistics of last database session.
  test "$Flag_l" && \
  test -r $Db.lg && \
  LogFileInfo $Db.lg

done

test -f $TmpFile && \
rm $TmpFile
