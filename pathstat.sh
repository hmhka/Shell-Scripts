#!/bin/sh
#
# Script to collect date statistics for .r and .pl files in the specified path.

Path="$*"
ExtList="r,pl"

test "$Path" || (
echo "
$0: Script to collect date statistics for $ExtList files in the specified path.
Usage: $0 PROPATH
where PROPATH is a colon- (or comma-) separated list of directories.
The current directory (.) in PROPATH will be ignored."
exit 1)

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
# The list of unique root directories in the specified dir list.
# Exclude subdirs if their parent dirs are also in Path:
# For example: "/a/b:/a" will be replace by "/a".
# The root dir ("/") (if it's a part of PROPATH) will be removed from the list.

UniqDirList()
{
# $* - colon/comma/space-separated list of directories or files.
# Note: tr needs two spaces on Solaris.

  echo $* | \
  tr ":," "  " | \
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
  ExtList=$2

# Ignore the current dir and the root dir:
  test "$1" = "." && exit
  test "$1" = "/" && exit

  CurrYear=`date '+%Y'`
  CurrDay=`date '+%m%d'`

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
     CurrDay='$CurrDay'
     ExtList="'$ExtList'"; if(ExtList!="") ExtList="," ExtList ","
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

# Check if filename has one of the specified extents:
    Type!="d" && ExtList!=0 {
      n=split(FileName,arr,".")
      Ext="," arr[n] ","
#     delete arr # does not work on AIX
      if(index(ExtList,Ext)==0) next
    }

# Reformat the file date as "Month Day, Year":
    {if($8~/:/) if(M[$6] $7>CurrDay) $8=CurrYear-1; else $8=CurrYear
     if(length($7)==1) $7="0" $7
# Output format: Date FullFileName
     if(Type=="d") Date=Type
     else          Date=$8 M[$6] $7 #Year Month Day
     printf"%8s %s\n",Date,SubDir FileName
    }                 # YYYYMMDD
# awk
  ' | \
  while read Date FileName
  do
    case $Date in
# Link to a direcrtory found:
     "d") FileList $FileName $ExtList
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
     MaxDays=10
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

# Print the daily statistics for recent MaxDays days:
    MnthCount==1 || DayCount<MaxDays {printf"%12s: %6d\n",CurrDate,$2; DayCount++}
  
# New month:
    CurrMnth!=PrevMnth {
     MnthCount++
# Print the monthly statistics during first year:
     if(MnthCount>1 && YearCount==1 && DayCount>=MaxDays) \
     printf"%12s: %6d\n",PrevMnth,MnthSum
     PrevMnth=CurrMnth
     MnthSum=0;
    }

# New year:
    CurrYear!=PrevYear {
     YearCount++
# Print the annual statistics after first year:
     if(YearCount>1 && DayCount>=MaxDays) printf"%12s: %6d\n",PrevYear,YearSum
     YearSum=0
     PrevYear=CurrYear
    }
  
    {MnthSum=MnthSum+$2
     YearSum=YearSum+$2
     TotlSum=TotlSum+$2
    }
    END {
     if(YearCount>1 && DayCount>=MaxDays) printf"%12s: %6d\n",PrevYear,YearSum
     else 
     if(MnthCount>1 && DayCount>=MaxDays) printf"%12s: %6d\n",PrevMnth,MnthSum
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
Date statistics for $ExtList files in $Path"

# Save current locale:
  PrevLCTIME=LC_TIME
  LC_TIME=en_ENG; export LC_TIME

  for Dir in `UniqDirList $Path`
  do
    FileList $Dir $ExtList
  done | \
  DateStat
# wc -l

# Restore previous locale:
  LC_TIME=PrevLCTIME; export LC_TIME
} # PathDateStat()

PathDateStat