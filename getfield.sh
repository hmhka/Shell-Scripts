# getfield.sh,   Version 1.0 as of Sep 25, 2003
#
# Written by George Potemkin (potemkin@csbi.ru)   
#
# The library contains the functions that read the fields of
# the based data types:
#
# GetByte()  returns 1-byte unsigned integer;
# GetShort() returns 2-byte unsigned integer;
# GetLong()  returns 4-byte unsigned integer;
# GetText()  returns a null-terminated string;
# GetDate()  interprets long words as the time and date in the form of elapsed
#            seconds from the start of the Epoch (00:00:00, January 1, 1970).
#
# GetDate function is based on timezone.

# ----------------------------------------------------------------------

GetByte()
{
# Returns 1-byte unsigned integer.
# 
# Parameters:
#  $1 - offset in the file;
#  $2 - file to read.

# od -b  Interpret bytes in octal:
  od -b $2 +$1. | \
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
#
# Parameters:
#  $1 - offset in the file;
#  $2 - file to read.

# od -d  Interpret words in unsigned decimal:
  od -d $2 +$1. | \
  awk '{ printf"%d",$2; exit }'

} # GetShort()

# ----------------------------------------------------------------------
 
GetLong()
{
# Returns 4-byte unsigned integer.
#
# Parameters:
#  $1 - offset in the file;
#  $2 - file to read.

# od -D  Interpret long words in unsigned decimal:
  od -D $2 +$1. | \
  awk '{ printf"%d",$2; exit }'

} # GetShort()

# ----------------------------------------------------------------------

GetText()
{
# Returns a null-terminated string.
#
# Parameters:
#  $1 - offset in the file;
#  $2 - file to read.

  Position=`expr $1 + 1`

  tail +${Position}c $2 | \
  dd bs=512 count=1 2>/dev/null | \
  awk '{print $0; exit}'

} # GetText()

# ----------------------------------------------------------------------
#
# Set TimeZone and tzOffset variables (used by GetDate() function):

TimeZone=`date "+%z" 2>/dev/null`
# e.g. 0300 - Moscow, St.Petersburg

# Time Zome Acronyms:
# http://greenwichmeantime.com/info/timezone.htm
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

#
# Set TimeZone in standardized format: [GMT][+|-]HH[[:]MM]

TimeZone=`
  echo $TimeZone | \
  awk '{
    TimeZone=$0    

# Trim GMT prefix:
    i=index(TimeZone,"GMT")
    if(i>0) TimeZone=substr(TimeZone,i+3)

# Trim the leading blanks:
    while(substr(TimeZone,1,1)==" ") TimeZone=substr(TimeZone,2)

# Get a sign of time zone offset:
    Sign=substr(TimeZone,1,1)
    if(Sign=="+") TimeZone=substr(TimeZone,2); else
    if(Sign=="-") TimeZone=substr(TimeZone,2); else Sign="+"

# Convert hours and minutes to two digits format:
    i=index(TimeZone,":")
    if(i>0) {HH=substr(TimeZone,1,i-1); MM=substr(TimeZone,i+1)}
      else  {HH=substr(TimeZone,1,2);   MM=substr(TimeZone,3)}
      
    while(length(HH)<2) HH="0" HH
    while(length(MM)<2) MM="0" MM

# Set TimeZone in standardized form:
    if(MM!="00") TimeZone="GMT" Sign HH ":" MM; else
    if(HH!="00") TimeZone="GMT" Sign HH       ; else
                 TimeZone="GMT"

    print TimeZone
  }' -`

# Convert timezone to the offset in seconds:
tzOffset=`
  echo $TimeZone | \
  awk '{
# Input TimeZone is in form GMT[+-]HH:MM
#                           123 4  56789
    Sign=substr($0,4,1)
    HH=substr($0,5,2)
    MM=substr($0,8,2)
    
    Offset=3600*HH+60*MM
    print Sign Offset
  }' -`

# ----------------------------------------------------------------------

GetDate()
{
# Interprets long words as the time and date in the form of elapsed
# seconds from the start of the Epoch (00:00:00, January 1, 1970).
#
# Parameters:
#  $1 - offset in the file;
#  $2 - file to read.

# od -D  Interpret long words in unsigned decimal:
  od -D $2 +$1. | \
  awk '{
    if($2==0) {print "None"; exit}

    Number=$2+tzOffset

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
  }' tzOffset="$tzOffset" TimeZone="$TimeZone" -   # end of awk
} # GetDate()

# ----------------------------------------------------------------------
