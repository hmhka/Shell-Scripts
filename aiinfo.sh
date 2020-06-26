#!/bin/sh
#
# aiinfo,   Version 1.0 as of Sep 24, 2003
#
# Shows information stored in after-image file header.
#
# Written by George Potemkin (potemkin@csbi.ru)  
# 
# 
# The script shows information stored in the header of AI file.
#
# The script requires a read access to the database files.
# You should have the getfield.sh file in PATH
# or in the same directory as current script.

# File name of this script:
Script=`basename $0`

# Temporal file (an absolute path is required):
TmpFile=/tmp/$Script.$$

# Bold/unbold terminal capacities:
BD=`tput smso`
UB=`tput rmso`

# Linking getfield.sh library...

# The library getfield.sh should be in PATH
# or in the same directory as current script.
 type getfield.sh 2>/dev/null 1>&2 && \
GetFieldLib="getfield.sh" || \
GetFieldLib="`dirname $0`/getfield.sh"

. $GetFieldLib
 
if [ -z "$TimeZone" ]; then
  echo "\n
$Script: ERROR: Initialization of ${BD}getfield.sh$UB library failed."
  exit 1
fi
    

# ----------------------------------------------------------------------

Usage()
{
  echo "\n\
$Script - shows information stored in header of after-image file.\n\
Usage: $Script <ai_file> ...\n" >&2
  exit
}


# ----------------------------------------------------------------------

KBytes()
{
 echo $1 | awk '{ printf"%dK", $1 / 1024}'
}

# ----------------------------------------------------------------------

ExtentHeader83() 
{
# Information in V8 extent header.
# Version of the structure: 80 or 83

# Parameters:
  ExtentHeader=$1

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

  ExtentCreateDate=`GetDate 116 $ExtentHeader`
    ExtentOpenDate=`GetDate 120 $ExtentHeader`
      ExtentDbPath=`GetText 328 $ExtentHeader`

} # ExtentHeader83()


# ----------------------------------------------------------------------

ExtentHeader91() 
{
# Information in V9 extent header.
# Version of the structure: 91

# Parameters:
  ExtentHeader=$1

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
  
  ExtentCreateDate=`GetDate 24 $ExtentHeader`
    ExtentOpenDate=`GetDate 32 $ExtentHeader`
   ExtentCloseDate=`GetDate 36 $ExtentHeader`

} # ExtentHeader91()


# ----------------------------------------------------------------------

ExtentHeaderInfo() 
{
# Database extent information.

# Parameters:
  ExtentHeader=$1

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

  Dbkey=`GetLong 0 $ExtentHeader`
  BlockType=`GetByte 4 $ExtentHeader`
  ChainType=`GetByte 5 $ExtentHeader`

  Error=""  

# Sanity checks:
  if [ "$BlockType" != 254 -a "$BlockType" != 1 ]; then
    echo "\
ERROR: Wrong block type in extent header.\n\
Found $BlockType, should be 1 or 254."
    Error="Wrong block type in extent header"

  else
  if [ "$ChainType" != 127 ]; then
    echo "\
ERROR: Wrong chain type in extent header. Found $ChainType, should be 127."
    Error="Wrong chain type in extent header"
  fi; fi

# Db block size and db version as specified in extent header:
# Long Db Version = DbBlockSize + DbVersion
  dbvers=`GetShort 16 $ExtentHeader`

# Structure version:
  ExtentVersion=`expr $dbvers % 256`

  ExtentBlockSize=`expr $dbvers - $ExtentVersion`

  case $ExtentBlockSize in
    1024|2048|4096|8192) # Valid blocksizes
    ;;
    *) Error="Invalid blocksize"
       echo "\
ERROR: Invalid blocksize ($ExtentBlockSize) in extent header block."
    ;;
  esac

  ExtentDbPath=""     # does not exists in V9 extent header
  ExtentCloseDate=""  # does not exists in V8 extent header

  case $ExtentVersion in
    78) echo "\
ERROR: File $BD$ExtentFile$UB is a V7 database.\n
$Script does not support this version of Progress."
        Error="Unsupported version"
        ;;
    80) ExtentHeader83 $ExtentHeader;;  # Progress V8.[0-2]
    83) ExtentHeader83 $ExtentHeader;;  # Progress V8.3
    91) ExtentHeader91 $ExtentHeader;;  # Progress V9.X
     *) echo "\
ERROR: File $BD$ExtentFile$UB has a unknown structure.\n\
Found: dbkey $Dbkey, type $BlockType, chain $ChainType, version $ExtentVersion"
        Error="Unknown structure"
        ;;
  esac

  ExtentHeader=""

} # ExtentHeaderInfo()

# ----------------------------------------------------------------------

ControlBlockInfo91()
{

# Control Block is a second block in *.db file.

# Offset Len  Description
# ------ ---  -----------
# 00-15   16  Standard 16-byte block header
# 16-19    4  Dbkey of root block of _Area._Area-Number index
# 24-27    4  Dbkey of root block of _AreaExtent._AreaExtent-Area index
# 32-288 255  Path to dbname.d1 extent

# Parameters:
  ControlBlock=$1

  SchemaAreaPath=`GetText 32 $ControlBlock`

  ControlBlock=""

} # ControlBlockInfo91()

# ----------------------------------------------------------------------

AiHeaderInfo()
{
# Second block after extent header block.
# Apply to V8/9 database.

# Parameters:
  AiHeader=$1

# First 16 bytes are a standart 16-byte header that exists in each AI block:

# Offset Len  Description
# ------ ---  -----------
# 00-01    2  AiHeaderSize  The length of AI block header (56 in first block, 16 in others)
# 02-03    2  AiByteUsed    The number of bytes in current block (upto AiBlockSize)
# 04-07    4  AiNoteNumber  The number of AI notes in current block
# 08-11    4  AiBlockNumber Sequential number of AI blocks in current AI file
# 12-15    4  AiOpenDate    After-image open date 

# The fields below exist only in first AI block (AiNoteNumber eq 0).
 
# 16-19    4  Db_aibegin    Last AIMAGE BEGIN <date/time> (1640)
# 20-23    4  AiNewDate     Date/Time of last rfutil aimage new (mb_ainew)
# 24-27    4  AiGenNum      AI extent sequence number (mb_aigennbr, Seqno)
# 28-31    4  AiCloseDate   This file was last opened for output on <date/time>. (1643)
# 32-35    4  Db_lstmod     Date/Time of last database update (mb_lstmod)
# 36-37    2  AiBlockSize   AI blocksize
# 38-39    2  0
# 40-43    4  AiStatus      1=Busy, 2=Empty, 4=Full
# 44-47    4  0
# 48-51    4  AiByteUsed    The number of bytes written to the Full AI extent.
# 52-55    4  1 or 2        Stores the value 2 in V8 or 1 in V9.

 AiHeaderSize=`GetShort  0 $AiHeader`
AiBlockNumber=`GetLong   8 $AiHeader`

if [ $AiHeaderSize  -eq 56 -a \
     $AiBlockNumber -eq  0 ]
then
   Db_aibegin=`GetDate  16 $AiHeader`
    AiNewDate=`GetDate  12 $AiHeader`
     AiGenNum=`GetLong  24 $AiHeader`
  AiCloseDate=`GetDate  28 $AiHeader`
    Db_lstmod=`GetDate  32 $AiHeader`
  AiBlockSize=`GetShort 36 $AiHeader`
     AiStatus=`GetLong  40 $AiHeader`
   AiByteUsed=`GetLong  48 $AiHeader`
   
  Error=""
else
  Error="not AI Header block"
fi

} # AiHeaderInfo()


# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# Main block:

# Parsing input parameters...

test $# -eq 0 && \
Usage

for Arg in $*
do

# Using fully specified pathenames without .db suffix... 
  AiFile=`dirname $Arg`
  AiFile=`(cd $AiFile; pwd)`  # Full path
  AiFile=$AiFile/`basename $Arg`

  test ! -r $AiFile && \
  echo "\
File $BD$AiFile$UB does not exist or you have not a read permission." && \
  continue

# Copying extent header to a temporal file...
  dd if=$AiFile bs=1024 count=1 2>/dev/null >$TmpFile

  
# AI Header Block is a first block in single volume (-a) AI file (Progress V8):
  AiHeaderOffset=0
  AiHeaderInfo $TmpFile

# If first block is not AI header then it should be extent header:

  if [ $AiHeaderSize -ne 56 -0 $AiBlockNumber -ne 0 ]; then

    ExtentHeaderInfo  $TmpFile 
    test "$Error" && \
    continue

    echo "\n\
Extent header information (`date '+%a %b %e %T %Y'`):\n\
Ai File: $AiFile\n"

    echo "         Source db:" $ExtentDbPath

    case $ExtentVersion in
      78) Desc="V7.[2-3]";;
      80) Desc="V8.[0-2]";;
      83) Desc="V8.3"    ;;
      91) Desc="V9.X"    ;;
       *) Desc="unknown" ;;
    esac
    echo "        Db version: $ExtentVersion ($Desc)"
    echo "      Db blocksize: $ExtentBlockSize"
    echo "    Db create date: $ExtentCreateDate"
    echo " Db last open date: $ExtentOpenDate"
    test "$ExtentCloseDate" && \
    echo "Db last close date: $ExtentCloseDate"


# Looking for begining of AI Header Block...
# ------------------------------------------
# First AiBlockSize bytes is just a copy of the bytes in .db file.
# First DbBlockSize bytes are Extent Header Block parsed above.
# If AiBlockSize is great than DbBlockSize then AI file has extra space
# before AI Header Block.

    AiHeaderOffset=$ExtentBlockSize
    SchemaAreaPath=""

    while [ $AiHeaderOffset -le 16384 ]
    do
      dd if=$AiFile bs=$AiHeaderOffset skip=1 count=1 2>/dev/null >$TmpFile

      AiHeaderInfo $TmpFile

# Hurrah! We found AI header block!
      test -z "$Error" && \
      break  # while $AiHeaderOffset -le 16384

# If aiblocksize is higher than dbblocksize then there is a 'garbage' after extent header.
# The 'garbage' area could have a copy of Control Block.
# In this case we can get the path to the SourceDatabase.d1 file:

      BlockType=`GetByte 4 $TmpFile`
      ChainType=`GetByte 5 $TmpFile`

      if [ $BlockType -eq  13 -a \
           $ChainType -eq 127 -a \
           $AiHeaderOffset -eq $ExtentBlockSize ]; then
         ControlBlockInfo91 $TmpFile
      fi

# Try next Offset:

      AiHeaderOffset=`expr $AiHeaderOffset \* 2`

    done  # while $AiHeaderOffset -le 16384

  fi  # If first block is not AI header

  if [ "$Error" ]; then
    echo "\n\
ERROR: Failed to find AI Header Block in file $BD$AiFile$UB."
    continue  # with next Arg
  fi

  echo "\n\
AI Header information (`date '+%a %b %e %T %Y'`):\n\
AI File: $AiFile\n\
AI Header Block found at offset $AiHeaderOffset\n"

  test "$SchemaAreaPath" &&\
  echo "Schema Area of source database: $SchemaAreaPath"
  echo "   Date of rfutil aimage begin: $Db_aibegin"
  echo "        Database last modified: $Db_lstmod\n"

  case $AiStatus in
    0) Desc="None";;     #Msg  694
    1) Desc="Busy";;     #Msg 3792
    2) Desc="Empty";;    #Msg 3793
    4) Desc="Full";;     #Msg 3794
    *) Desc="Unknown";;  #Msg  737
  esac

  AiFileSize=`ls -l $AiFile | awk '{ printf"%d", $5}'`

  echo "AI sequence number: $AiGenNum"
  echo "         AI status: $AiStatus ($Desc)"
  echo "      AI blocksize: $AiBlockSize (`KBytes $AiBlockSize`)"
  echo "      AI file size: $AiFileSize (`KBytes $AiFileSize`)"
  echo "      AI byte used: $AiByteUsed (`KBytes $AiByteUsed`)"
  echo "      AI open date: $AiNewDate"
  echo "     AI close date: $AiCloseDate"
  echo "\n------------------------------------------------"

done

test -f $TmpFile && \
rm $TmpFile
