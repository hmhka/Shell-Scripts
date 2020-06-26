#!/bin/sh
#

THIS=`basename $0`

# DumpBlock.sh - the script to dump a specified block from any file.
# Written by George Potemkin, Feb 27, 2017

SaveDir=${SAVEDIR:-"/data/log/777"}
if [ ! -d $SaveDir ]
then
  echo "Directory $SaveDir does not exist." >&2
  SaveDir=`pwd`
  echo "Results will be saved in the current directory: $SaveDir" >&2
fi

#------------------------------------------------------------------------------
#
DumpBlock()
{
  BlockOffset="$1"
  SourceFile="$2"
  BlockSize=${3:-8192}

# Copy the output to the RunLog:
 (
# Sanity checks:

  test $BlockOffset -ge 0 2>/dev/null
  case $? in
    0) ;;
    1) echo "Block offset has a negative value: $BlockOffset.";  return 1;;
    *) echo "Block offset ($BlockOffset) should be an integer."; return 1;;
  esac

  test $BlockSize -gt 0 2>/dev/null
  case $? in
    0) ;;
    1) echo "Block size must be greater than zero: $BlockSize."; return 1;;
    *) echo "Block size ($BlockSize) should be an integer.";     return 1;;
  esac

  test ! -f $SourceFile && \
  echo "File $SourceFile does not exit." && return 1

  SourceSize=`ls -l $SourceFile | awk '{print $5}' 2>/dev/null`

  test $BlockOffset -ge $SourceSize 2>/dev/null && \
  echo "Block offset ($BlockOffset) must be less than file size ($SourceSize)." && \
  return 1


  case `uname` in
    Linux) ls -li --time-style=full-iso $SourceFile;;
    SunOS) ls -lei $SourceFile;;
        *) ls -li  $SourceFile;;
  esac # uname

  if type lsof >/dev/null 2>&1
  then
    echo "lsof $SourceFile"
    lsof $SourceFile 2>&1
  fi

  test ! -r $SourceFile && \
  echo "You do not permissions to read the file." && return 1

  BlockDump=$SaveDir/`basename $SourceFile`.$TimeStamp.block.$BlockSize.$BlockOffset.gz
 
  dd if=$SourceFile bs=1 count=$BlockSize skip=$BlockOffset 2>/dev/null | \
  gzip >$BlockDump && \
  echo "Block is dumped as $BlockDump"

  SourceFlag=$SourceFile.$THIS.flag
  if [ -f $SourceFlag ]
  then
#   test f1 -nt f2: file f1 is newer than f2
    test $SourceFile -nt $SourceFlag && \
    echo "$SourceFile is changed since the previous check." || \
    echo "$SourceFile is NOT changed since the previous check."
    rm -f $SourceFlag 
  else
    touch $SourceFlag 2>/dev/null
  fi # if [ -f $SourceFlag ]

  SourceArchive=$SaveDir/`basename $SourceFile`.$TimeStamp.gz
  if [ ! -f $SourceArchive ]
  then
    sleep 1
    echo "The size of $SourceFile is $SourceSize bytes." >&2
    echo "Would you like to archive the file? [Y/n}:" >&2
    read Answer
    Answer=${Answer:-"yes"}

    case $Answer in
      y*|Y*)
        echo "Archiving the file $SourceFile"
        gzip <$SourceFile >$SourceArchive
        echo "File is archived as $SourceArchive"
      ;;
    esac
  fi # if [ ! -f $SourceArchive ]

 ) | \
  while IFS="" read Msg
  do
    echo `date '+%H:%M:%S'` "$Msg"
    echo `date '+%Y/%m/%d %H:%M:%S'` "$Msg" >> $RunLog
  done

}  # DumpBlock()

#------------------------------------------------------------------------------
#
# Main Block:

# Bold/unbold terminal codes:
BD=`tput smso` # Enter standout mode (bold on rxvt)
UB=`tput rmso` # Exit standout mode

# Underline mode:
UL=`tput smul` # Begin underline mode
UU=`tput rmul` # Exit underline mode

# Run Identifier:
TimeStamp=`date '+%y.%m.%d_%H.%M.%S'`

RunLog=$SaveDir/$THIS.run.log

case $# in
  0) echo "$THIS dumps a specified block.
Usage:
$BD$THIS$UB ${UL}BlockOffset${UU} ${UL}FileName${UU} [${UL}BlockSize${UU}]]
or
$BD$THIS$UB ${UL}DbName.lg${UU}" >&2
  ;;

  1) DbLogFile=$1
     test ! -r $DbLogFile && \
     echo "$THIS: Db log file $DbLogFile does not exit
or you do not have permissions to read the file." >&2 && \
     exit 1
   
# Find all errors 9445 in DbLogFile
#[2017/01/16@10:18:11.094+0600] P-1263 T-1 I ABL  3104: (9445)  SYSTEM ERROR: read wrong dbkey at offset <offset> in file <file> 
#  found <dbkey>, expected <dbkey>, retrying. area <number>

     grep "\(9445\)" $DbLogFile | \
     awk '{print $(NF-3) " " $NF}' | \
     while read BlockOffset FileName
     do
       DumpBlock $BlockOffset $FileName 8192
     done
  ;;

  *) DumpBlock $*
  ;;
esac

echo See results in $SaveDir
