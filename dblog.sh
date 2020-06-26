#!/bin/sh
#
# dblog - shell script to parse the messages in db log files.
# Written by George Potemkin, Jun 02, 2016

Release="1.2, Jun 20, 2016"

# The most recent version of the script can be downloaded from:
# ftp://ftp.progress-tech.ru/pub/Users/george/Scripts/dblog.sh
#
# Script classifies the messages into 4 categories (groups):
# 1. Database startup parameters ("Parameters" group);
# 2. Logins/logouts messages ("Logins" group);
# 3. Informational messages that can be ignored ("Ignore" group);
# 4. Any messages that do not belong to the above groups ("Remains" group).
#    "Remains" group should mainly contains the error messages.
#
# Fixes:
# Release 1.1:
# Use timezone for the -l statistics (see "signal handlers and UTC time");
# Added the -f and -e options;
# Fixed a few minor bugs.
# Release 1.2: Fixed login/logout statistics

#------------------------------------------------------------------------------

THIS=`basename $0`

Usage()
{
# Bold/unbold terminal codes:
#
BD=`tput smso` # Enter standout mode (bold on rxvt)
UB=`tput rmso` # Exit standout mode

# Underline mode:
UL=`tput smul` # Begin underline mode
UU=`tput rmul` # Exit underline mode

  echo "$THIS Release $Release

Usage: 
$BD$THIS$UB [${UL}File${UU} ...] [${UL}Options${UU}]
or
tail -100000 ${UL}File${UU} | ${BD}$THIS$UB [${UL}Options${UU}]
where the options are:
${UL}File${UU}(s) are the log files of Progress databases;
-f Show the fatal errors (marked as \"F\" in Progress log files);
-e Show the system errors (the messages with words \"SYSTEM ERROR:\");
-r Show the remained messages (except \"Logins\", \"Params\" and \"Ignore\" groups);
-p Show the startup parameters of Progress multi-user sessions (\"Params\" group);
-s Summary: the number of messages per message numbers (and without numbers);
-l Report the statistics for the messages in the \"Logins\" group;
The above options can be concatenated. Example: dblog -prs
${UL}Number${UU} - number of last digits to cut from timestamps for \"Logins\" statistics;
-m ${UL}Num${UU} Display the description of Progress message number;
-h|-help Display this help.

The most recent version of the script can be downloaded from:
ftp://ftp.progress-tech.ru/pub/Users/george/Scripts/dblog.sh"
# echo

  exit
} # Usage()


#------------------------------------------------------------------------------
#
LogParser()
{
# Creates statistics of the errors or login/logout messages in db log file.
# LogParser reads the messages from a standard input stream (stdin).
#
# It processes approximately 50,000 lines per sec on CPU @ 2.40 GHz.
#
# Number of messages added by Progress versions:
# Ver   Date       Msg # Range Added
# 10.2B 2009/12/14 12419-15762 3344
# 11.0  2011/12/02 15763-16640  878
# 11.1  2012/06/15 16641-16738   98
# 11.2  2013/02/13 16739-17035  296
# 11.3  2013/07/17 17036-17338  302
# 11.4  2014/07/25 17339-17759  421
# 11.5  2014/12/05 17760-17919  160
# 11.6  2015/10/16 17920-18388  468
# 11.7A 2016/03/25 18389-18505  117
#
# Only 200-300 messages used to be written to db log.

# Options:
#
# -f Show the fatal errors (marked as "F" in Progress log files)
#    They are the messages with %g or %G tags in promsgs file.
#    The tags are telling to the process to terminate processing;
# -e Show the system errors (the messages with words "SYSTEM ERROR:");
# -r Show the remained messages (except "Logins", "Params" and "Ignore" groups);
# -p Show the startup parameters of Progress multi-user sessions ("Params" group);
# -s Summary: the number of messages per message numbers (and without numbers);
# -l Report the statistics for the messages in the "Logins" group;
# CutTimeDigits - the number of the digits in timestamps to cut off
# while creating the login/logout statistics.
# Range of CutTimeDigits values: 0-8
# 0   = 2016/05/12@12:34:56 = per seconds
# 1   = 2016/05/12@12:34:5  = per 10 seconds
# 2,3 = 2016/05/12@12:34:   = per minutes
# 4   = 2016/05/12@12:3     = per 10 minutes
# 5,6 = 2016/05/12@12:      = per hours
# 7   = 2016/05/12@1        = per 10 or 4 hours
# 8   = 2016/05/12@         = per days

  CutTimeDigits=0 # Default is per second statistics.
  Logins="no"
  Params="no"
  Fatal="no"
  Errors="no"
  Remains="no"
  Summary="no"
  Options=""
  while [ $# -gt 0 ]
  do
# CutTimeDigits:
    case "$1" in
      [0-9]*)  CutTimeDigits=$1; shift; continue;;
    esac

# Params:
    case "$1" in
      -*[pP]*) Params="yes";  Options="$Options Params";;
    esac

# Logins:
    case "$1" in
      -*[lL]*) Logins="yes";  Options="$Options Logins";;
    esac

# Fatal:
    case "$1" in
      -*[fF]*) Fatal="yes";   Options="$Options Fatal";;
    esac

# Errors:
    case "$1" in
      -*[eE]*) Errors="yes"; Options="$Options Errors";;
    esac

# Remains:
    case "$1" in
      -*[rR]*) Remains="yes"; Options="$Options Remains";;
    esac

# Summary:
    case "$1" in
      -*[sS]*) Summary="yes"; Options="$Options Summary";;
    esac

    Rest=`echo $1 | tr -d "lLpPeEfFrRsS\-"`
    if [ "$Rest" ]
    then
      echo "LogParser: \"$Rest\" is an unknown option."
      exit 1
    fi # if Rest

    shift
  done  # while [ $# -gt 0 ]

  test $CutTimeDigits -gt 8 && \
  CutTimeDigits=8
# CutTimeDigits=0: 2016/05/12@14:03:52
# CutTimeDigits=1: 2016/05/12@14:03:5
# ...
# CutTimeDigits=7: 2016/05/12@1
# CutTimeDigits=8: 2016/05/12@

  Options=`echo $Options | \
           awk 'BEGIN {RS=" "}
                {print}' | \
           sort` # Options
  Options=`echo $Options`
  echo "LogParser Options: $Options"
  test "$Logins" = "yes" && \
  case "$CutTimeDigits" in
    0)   echo "Login statistics per second:"    ;; # 2016/05/12@12:34:56
    1)   echo "Login statistics per 10 seconds:";; # 2016/05/12@12:34:5
    2|3) echo "Login statistics per minute:"    ;; # 2016/05/12@12:34:
    4)   echo "Login statistics per 10 minutes:";; # 2016/05/12@12:3
    5|6) echo "Login statistics per hour:"      ;; # 2016/05/12@12:
    7)   echo "Login statistics per 10 hours:"  ;; # 2016/05/12@1
    8)   echo "Login statistics per day:"       ;; # 2016/05/12@
  esac # CutTimeDigits

  TmpFile=${DbLogPrefix:-`basename $0`.$$}.LogParser.tmp

  awk '
    BEGIN {
      NoNum="(-----)"
      TmpFile="'$TmpFile'"
      TimeSize=19-'$CutTimeDigits'
      Logins="'$Logins'"
      Params="'$Params'"
      Fatal="'$Fatal'"
      Errors="'$Errors'"
      Remains="'$Remains'"
      Summary="'$Summary'"
#
# "Logins" = the group of the messages related to the activity on User Control Table:
#
      LoginsNum[  "(298)"]="KILL signal received."
      LoginsNum[  "(333)"]="Multi-user session begin."
      LoginsNum[  "(334)"]="<Multi/Single> session end. (334)"
      LoginsNum[  "(451)"]="<session-type> session begin for <user> on <tty>. (451)"
      LoginsNum[  "(452)"]="Login by <user> on <tty>."
      LoginsNum[  "(453)"]="Logout by <user> on <tty>."
      LoginsNum[  "(562)"]="HANGUP signal received."
      LoginsNum[  "(708)"]="Userid is now <name>."
      LoginsNum[  "(742)"]="Login usernum <n>, userid <name>, on <tty/node>."
      LoginsNum[  "(739)"]="Logout usernum <n>, userid <name>, on <tty/node>."
      LoginsNum[  "(792)"]="Message received with bad usernum."
      LoginsNum[  "(794)"]="Usernum  terminated abnormally."
      LoginsNum[  "(795)"]="Error reading socket=<n> ret=<n> errno=<n>."
      LoginsNum[  "(796)"]="Error writing msg, socket=<n> errno=<n> usernum=<n> disconnected."
      LoginsNum[  "(797)"]="Partial write, socket=<n> msglen=<n> ret=<n> user <n> disconnected."
      LoginsNum[ "(1153)"]="BROKER detects death of server <pid>."
      LoginsNum[ "(1334)"]="Rejecting login -- too many users for this server."
      LoginsNum[ "(1166)"]="Server disconnecting user <n>."
      LoginsNum[ "(2252)"]="Begin transaction backout."
      LoginsNum[ "(2253)"]="Transaction backout completed."
      LoginsNum[ "(2266)"]="Expected user <n> received user <n>."
      LoginsNum[ "(2518)"]="Started. (APW, BIW, AIW or WDOG)"
      LoginsNum[ "(2519)"]="Disconnected. (APW or WDOG)"
      LoginsNum[ "(2520)"]="Stopped. (BIW or AIW)"
      LoginsNum[ "(2268)"]="Invalid message was received for an attempted login."
      LoginsNum[ "(2525)"]="Disconnecting dead server <n>."
      LoginsNum[ "(2526)"]="Disconnecting client <n> of dead server <n>."
      LoginsNum[ "(2527)"]="Disconnecting dead user <n>."
      LoginsNum[ "(4375)"]="Received signal <sigNum>; handling as SIGHUP."
      LoginsNum[ "(4698)"]="User count exceeded. Failed attempt to login by <userid> on <devtype>."
#     LoginsNum[ "(4732)"]="Login by <name> on <tty>. Pid <pid>."             # DEC/OpenVMS
#     LoginsNum[ "(4734)"]="Logout by <name> on <tty>. Pid <pid>."            # DEC/OpenVMS
      LoginsNum[ "(5569)"]="Quiet point request login by <user> on <ttyxxx>." # V8.2A
      LoginsNum[ "(5570)"]="Quiet point request logout"                       # V8.2A
      LoginsNum[ "(7129)"]="Usr <n> set name to <name>."                      # V8.3B Dbdes, Aimage new, Aimage list, BACKUP
      LoginsNum[ "(8873)"]="Login usernum <n>, remote SQL client."            # V9.1B, logout with msg 453
#     LoginsNum["(12092)"]="User <n> disconnect initiated."                   # V10.1A, Not used
      LoginsNum["(12454)"]="Server <n> has <n> unresolved pending connections(s). Please check port <n>."
      LoginsNum["(12455)"]="Clearing pending connections from server <n>"     # V10.2B
      LoginsNum["(17961)"]="User <n> set tty to <name>. (17961)"              # V11.6
#
# Identify the messages without the numbers by their text pattern:
#
# PROMON  5: (-----) Login by root.
# BACKUP  7: (-----) Login by root.
# SQLSRV2 3: (-----) Login by user 2525. Using TCP/IP IPV4 address 192.168.10.162
      LoginsMsg["Login by"]="(SQL User, PROMON or BACKUP)"
      LoginsMsg["Logout by"]="SQL User (SQLSRV2)" # SQLSRV2 3: (-----) Logout by user 2525.
      LoginsMsg["Sending signal to user"]="(BROKER or PROSHUT)"
      LoginsMsg["User disconnect initiated"]="(PROSHUT)"

      LoginsMsg["Received RECONNECT from WTB"]="(Webspeed user/agent)"
# This is an informational message which is sent on behalf of a user.
# This message indicates that the user who connected to the database
# server is a Webspeed user/agent and that the initial connection
# between the WebSpeed broker and agent was completed.

      LoginsMsg["Notifying srvr"]="<n>, conn <n>, to terminate remote user <n> (12093)"
# SHUT 2146: (-----) User 7775 disconnect initiated
# SHUT 2146: (453)   Logout by <user> on batch.
# BROKER  0: (-----) Notifying srvr 24, conn 24, to terminate remote user 7775 <== should be (12093)
# BROKER  0: (-----) Sending signal 5 to user 7775
# SRV    24: (1166)  Server disconnecting user 7775.

#
# "Ignore" = the group of the informational messages that can be ignored:
#
# Miscellaneous:
#
      IgnoreNum[  "(439)"]="** Save file named core for analysis by Progress Software Corporation."
      IgnoreNum[ "(2261)"]="Sending signal <n> to <n> connected user(s)."
      IgnoreNum[ "(2263)"]="Resending shutdown request to <n> user(s)."
      IgnoreNum[ "(8863)"]="This broker supports 4GL server groups only."
      IgnoreNum[ "(8864)"]="This broker supports SQL server groups only."
      IgnoreNum[ "(8865)"]="This broker supports both 4GL and SQL server groups."
      IgnoreNum["(10471)"]="Database connections have been enabled."
      IgnoreNum["(16869)"]="Removed shared memory with segment_id: <n>"
#
# Warnings:
#
      IgnoreNum[ "(5407)"]="WARNING: -nb exceeded. Automatically increasing from <old> to <new>."
      IgnoreNum[ "(5408)"]="WARNING: -l exceeded. Automatically increasing from <old> to <new>."
      IgnoreNum[ "(5409)"]="WARNING: -mmax exceeded. Automatically increasing from <old> to <new>."
      IgnoreNum[ "(5410)"]="WARNING: -D limit has been exceeded; automatically increasing to <new>."
#
# Connection:
#
      IgnoreNum[ "(5644)"]="Started for <name> using tcp IPV4 address 0.0.0.0, pid <n>."
      IgnoreNum[ "(5645)"]="This is an additional broker for this protocol."
      IgnoreNum[ "(5646)"]="Started on port <port> using tcp IPV4 address 0.0.0.0, pid <n>."
      IgnoreNum[ "(5512)"]="Previous message sent on behalf of user <n>. (5512)"
      IgnoreNum["(14658)"]="Previous message sent on behalf of user <n>, server pid <n>, broker pid <n>. (5512)"
      IgnoreNum["(10836)"]="Database connections are not allowed at this time."
      IgnoreNum["(12698)"]="User <name> Roles: <name>"
      IgnoreNum["(12699)"]="Database <name> Options: <name>"
      IgnoreNum["(15646)"]="Started on port <port> using tcp, pid <pid>."
#
# SQLSRV2: Database statistics
#
      IgnoreNum["(10995)"]="Database statistics (<type>) updated for all tables."
      IgnoreNum["(10996)"]="Database statistics (<type>) updated for table <name>."
      IgnoreNum["(10997)"]="Database statistics (<schema table>) updated by direct user sql statement (<type>)."
#
# RFUTIL:
#
      IgnoreNum[ "(3774)"]="Backup after-image extent and mark it as empty."
      IgnoreNum[ "(3776)"]="Backup ai extent and mark it as empty."
      IgnoreNum[ "(3777)"]="Switched to ai extent <name>."
      IgnoreNum[ "(3778)"]="This is after-image file number <n> since the last AIMAGE BEGIN"
      IgnoreNum[ "(3789)"]="Marked after-image extent <name> EMPTY."
      IgnoreNum["(11805)"]="Unlocking after-image file <n> and locking ALL FULL after-image files beginning with file <n>."
      IgnoreNum["(12074)"]="The After-image extent <name> was successfully extracted."
      IgnoreNum["(13154)"]="Marked after-image extent <name> ARCHIVED."
      IgnoreNum["(13199)"]="After-image extent <name> has been copied to <name>."
      IgnoreNum["(13231)"]="From this point forward all after-image extents will be archived to <name>."
#
# BACKUP:
#
      IgnoreNum[ "(1074)"]="Wrote a total of <blocks> backup blocks using <bytes> bytes of media."
      IgnoreNum[ "(1361)"]="Incremental backup started."
      IgnoreNum[ "(1362)"]="Full backup started."
      IgnoreNum[ "(1363)"]="Incremental backup successfully completed."
      IgnoreNum[ "(1364)"]="Full backup successfully completed."
      IgnoreNum[ "(1365)"]="Database copied from <database-name>."
      IgnoreNum[ "(1366)"]="Incremental restore of sequence <n> started."
      IgnoreNum[ "(1367)"]="Incremental restore of sequence <n> completed."
      IgnoreNum[ "(3664)"]="Incremental backup started."
      IgnoreNum[ "(5459)"]="Begin backup of Before Image file(s)."
      IgnoreNum[ "(5460)"]="End backup of Before Image file(s)."
      IgnoreNum[ "(5461)"]="Begin backup of Data file(s)."
      IgnoreNum[ "(5462)"]="End backup of Data file(s)."
      IgnoreNum[ "(6678)"]="NOTE: Backup media estimates done without database scan." # V8.3A
      IgnoreNum[ "(6680)"]="Estimate for uncompressed full backup is provided."
      IgnoreNum[ "(6686)"]="<n> active blocks out of <n> blocks in <name> will be dumped."
      IgnoreNum[ "(6688)"]="<n> BI blocks will be dumped."                            # V8.3B
      IgnoreNum[ "(6689)"]="Backup requires  blocks ( Mb) of media."
      IgnoreNum[ "(6699)"]="Restore would require  blocks ( Mb) of media."
      IgnoreNum[ "(6760)"]="This backup was taken <time>."                            # V8.3B
      IgnoreNum[ "(9285)"]="Backup requires an estimated <size+id> of media."         # V9.1C
      IgnoreNum[ "(9286)"]="Restore would require an estimated  db blocks using <size+id> of media."
      IgnoreNum["(12850)"]="Backup blocks will be written to <device/file>."          # V10.2B
      IgnoreNum["(13625)"]="Wrote a total of <blocks> backup blocks using <size+id> of media."
      IgnoreNum["(13786)"]="This backup was also used to enable after-image on the source database."
      IgnoreNum["(14256)"]="The Replication Server has been notified that an online backup is about to be performed."
      IgnoreNum["(14258)"]="The online backup of this database has completed normally."
      IgnoreNum["(15108)"]="The online backup has finished backing up the BI. The Replication Agent will now begin recovery synchronization followed by normal processing."
      IgnoreNum["(15109)"]="At Database close the number of live transactions is <n>."
      IgnoreNum["(15321)"]="Before Image Log Initialisation at block <n> offset <n>."
      IgnoreNum["(15743)"]="Before Image Log Completion at Block <n> Offset <n>."      # V10.2B
      IgnoreNum["(16866)"]="Dumped <n> active BI blocks."                              # V11.2
#
# Identify the messages without the numbers by their text pattern:
#
# BROKER  2: (-----) Shared Library Path set to LIBPATH=<path>
#
      IgnoreMsg["Shared Library Path set to"]=""

# SQLSRV2 3: (-----) SQL Server 11.6.01 started, configuration: "dbname.virtualconfig"
# SQLSRV2 3: (-----) "/path/to/dbname" started on IPv4 port 1025 for address 0.0.0.0, pid 9568520 (0x00920108).
# SQLSRV2 3: (-----) Thread stack size: 1024000 (bytes).
# SQLSRV2 3: (-----) DLC from ENVIRONMENT VARIABLE is: /usr/dlc
# SQLSRV2 3: (-----) WRKDIR from ENVIRONMENT VARIABLE is: /usr/wrk/
# SQLSRV2 3: (-----) JDKHOME from ENVIRONMENT VARIABLE is: /usr/dlc/jdk
# SQLSRV2 3: (-----) JREHOME from ENVIRONMENT VARIABLE is: /usr/dlc/jdk/jre
# SQLSRV2 3: (-----) CLASSPATH from DEFAULT is:
# SQLSRV2 3: (-----) PROSQL_LOCKWAIT_TIMEOUT value is: 5 seconds

      IgnoreMsg["started, configuration:"]=""
      IgnoreMsg["started on port for address pid"]=""
      IgnoreMsg["Thread stack size:"]=""
      IgnoreMsg["DLC from ENVIRONMENT VARIABLE is:"]=""
      IgnoreMsg["WRKDIR from ENVIRONMENT VARIABLE is:"]=""
      IgnoreMsg["JDKHOME from ENVIRONMENT VARIABLE is:"]=""
      IgnoreMsg["JREHOME from ENVIRONMENT VARIABLE is:"]=""
      IgnoreMsg["CLASSPATH from DEFAULT is:"]=""
      IgnoreMsg["PROSQL_LOCKWAIT_TIMEOUT value is:"]=""

      PrevLogins=""
      PrevLoginTime=""

# SunOS can rise the error if the arrays are not "initiated":
# awk: Count is not an array
      TotalNumCount[""]=0
      TotalMsgCount[""]=0
      LoginNumCount[""]=0
      LoginMsgCount[""]=0
    } # BEGIN
#
# Skip the lines that do not begin with timestamps
# like [2016/05/12@14:03:52.486+0400]
#       1234567890123456789012345678
#
    ! /^\[/ {next}
#
# Lines with timestamps:
#
    { CurrTime=substr($1,2,length($1)-2) # Time without "[]"
      if(InitTime=="") InitTime=CurrTime
      if(PrevTime=="") PrevTime=CurrTime

      D=substr(CurrTime,1,11) # Year/Month/Day
      H=substr(CurrTime,12,2) # Hours
      M=substr(CurrTime,15,2) # Minutes
      S=substr(CurrTime,18,2) # Seconds
      Z=substr(CurrTime,length(CurrTime)-4) # Timezone -0400
    }
    Z ~/^\+/ {                # TZ +0400
      H-=substr(Z,2,2)        # Hours
      M-=substr(Z,4,2)        # Minutes
    } # TZ +0400
    Z ~/^\-/ {                # TZ -0400
      H+=substr(Z,2,2)        # Hours
      M+=substr(Z,4,2)        # Minutes
    } # TZ -0400
#
# Article Number: 000064856
# Defect PSC00256028, PSC00257044
# Upgrade to OpenEdge 11.4 or later
# A partial fix is included in OpenEdge 11.2.1 and 11.3.x
# As a side effect of the fix, messages from signal handlers will always show
# as UTC time instead of the database timezone.
#
# Example:
# [2016/05/23@04:11:27.185-0400] P-12 T-1 I ABL 8: (452) Login by user on /dev/pts/4.
# [2016/05/23@08:52:07.000+0000] P-12 T-1 I ABL 8: (562) HANGUP signal received.
# [2016/05/23@04:52:07.926-0400] P-12 T-1 I ABL 8: (453) Logout by hchen on /dev/pts/4.
#
# Convert the current time to UTC time to get the correct "Logins" statistics:
    { if(length(H)==1) H="0" H
      if(length(M)==1) M="0" M
      if(length(M)==1) S="0" S

      CurrLogins=D H ":" M ":" S
      CurrLogins=substr(CurrLogins,1,TimeSize) # TimeSize=19 = no cuts
    } # Timestamps
#
# Logins/logouts statistics report:
#
    Logins=="yes" && PrevLogins!=CurrLogins {
      for(Num in LoginNumCount) if(LoginNumCount[Num]>0)
      {
# Time: 2016/05/12@14:03:52.486+0400
#       1234567890123456789012345678
#
        printf"%10s %8s %8s %4d %7s %s\n",\
          substr(PrevLoginTime,1,10),\
          substr(PrevLoginTime,12),\
          substr(LastLoginTime,12),\
          LoginNumCount[Num],\
          Num,\
          LoginsNum[Num]

# Either set to zero or delete an element of the array - any option that will work:
        LoginNumCount[Num]=0
        delete LoginNumCount[Num]
      } # for Num in LoginNumCount

      for(Msg in LoginMsgCount) if(LoginMsgCount[Msg]>0) {
        printf"%10s %8s %8s %4d %7s %s\n",\
          substr(PrevLoginTime,1,10),\
          substr(PrevLoginTime,12),\
          substr(LastLoginTime,12),\
          LoginMsgCount[Msg],\
          NoNum,\
          Msg " " LoginsMsg[Msg]

# Either set to zero or delete an element of the array - any option that will work:
        LoginMsgCount[Msg]=0
        delete LoginMsgCount[Msg]
      } # for Msg in LoginMsgCount

      PrevLoginTime=""
      PrevLogins=CurrLogins
    } # Logins=="yes"
#
# Msg number is column 7 if there is a space between user type and user number:
# [2016/05/12@14:03:52.486+0400] P-1 T-1 I ABL 10: (452) Login by root on batch.
#
# Msg number is column 6 if there is no space between user type and user number:
# [2016/04/05@06:00:23.102+0600] P-2 T-1 I RFUTI1312: (452) Login by root on batch.
#
    { Num=""
      for(Col=6;Col<=7;Col++) if($Col ~ /^\([0-9]*-*\)$/) {
        Num=$Col
        break
      } # for Col
      if(Num=="") next
#
# Cleaning a message: remove any word containing a digit.
#
      Msg=$(Col+1)
      for(i=Col+2;i<=NF;i++) if($i !~/[0-9]/) Msg=Msg " " $i
    }
#
# Messages without numbers:
#
    Num==NoNum {
#
# Ignore the block dumps (dont count them in the Summary):
# (-----) 1fd0:  652d 5883 4e53 5056 0000 2565 2d74 7970
#
      if(NF==Col+9 && $(Col+1) ~ /^[0-1]/ && $(Col+1) ~ /:$/) next

# Summary:
      TotalMsgCount[Msg]++

# Fatal:
      if($4=="F") {
        FatalCount++
        if(Fatal=="yes") {
          print
          next
        } # if Fatal
      } # if "F"

# Errors:
      if(Msg ~ /SYSTEM ERROR: /) {
        ErrorsCount++
        if(Errors=="yes") {
          print
          next
        } # if Errors
      } # if SYSTEM ERROR:

      for(Pattern in IgnoreMsg) if(index(Msg,Pattern)>0) next

      for(Pattern in LoginsMsg) if(index(Msg,Pattern)>0) {
        LoginMsgCount[Pattern]++
        if(PrevLoginTime=="")
        PrevLoginTime=CurrTime
        LastLoginTime=CurrTime
        next
      } # LoginsMsg

      if(Startup=="yes") {
        if(Params=="yes") print
        next
      } # if Startup

      RemainsCount++
      if(Remains=="yes") print
      next
    } #  Messages without numbers
#
# The numbered messages:
#
# Summary:
    { TotalNumCount[Num]++
      Example[Num]=Msg

# Fatal:
      if($4=="F") {
        FatalCount++
        if(Fatal=="yes") {
          print
          next
        } # if Fatal
      } # if "F"

# Errors:
      if(Msg ~ /SYSTEM ERROR: /) {
        ErrorsCount++
        if(Errors=="yes") {
          print
          next
        } # if Errors
      } # if SYSTEM ERROR:
#
# "Params" group:
#
# BROKER  0: (333)   Multi-user session begin.
# BROKER  0: (4234)  Progress OpenEdge Release <VERSION_NUMBER> on <OS_PLATFORM>.
# BROKER  0: (10471) Database connections have been enabled.
# PROLOG  5: (12684) prolog log file truncation <start/end>. (12684)
#
      if(Num=="(333)")   Startup="yes"
      if(Num=="(4234)")  Startup="yes"
      if(Num=="(10471)") Startup="no"
      if($5!="BROKER" && $5!="PROLOG") Startup="no"

      for(Pattern in IgnoreNum) if(Num==Pattern) next
      for(Pattern in LoginsNum) if(Num==Pattern) {
        LoginNumCount[Pattern]++
        if(PrevLoginTime=="")
        PrevLoginTime=CurrTime
        LastLoginTime=CurrTime
        next
      } # LoginsNum
      if(Startup=="yes") {
        if(Params=="yes")  print
        next
      } # if Startup

      RemainsCount++
      if(Remains=="yes") print
      next
    } # Numbered messages

    END {
      if(Logins=="yes")
      for(Num in LoginNumCount) if(LoginNumCount[Num]>0)
        printf"%10s %8s %8s %4d %7s %s\n",\
          substr(PrevLoginTime,1,10),\
          substr(PrevLoginTime,12),\
          substr(LastLoginTime,12),\
          LoginNumCount[Num],\
          Num,\
          LoginsNum[Num]
      # if Logins

      Count=0
      NumTypeCount=0
      MsgTypeCount=0
      for(Num in TotalNumCount) if(TotalNumCount[Num]>0) {
        Count+=TotalNumCount[Num]
        NumTypeCount++
      } # for Num if
      for(Msg in TotalMsgCount) if(TotalMsgCount[Msg]>0) {
        Count+=TotalMsgCount[Msg]
        MsgTypeCount++
      } # for Msg if

      printf"\nProcessed %d lines, %d messages with timestamps\n",NR,Count
      printf"from %s\n",InitTime
      printf"  to %s\n",CurrTime
      printf"Numbered messages: %d, without numbers: %d\n",NumTypeCount,MsgTypeCount
      if(Fatal!="yes")  RemainsCount-=FatalCount
      if(Errors!="yes") RemainsCount-=ErrorsCount
      printf"System errors: %d, fatal errors: %d, \"remains\": %d\n",ErrorsCount,FatalCount,RemainsCount

      if("'$Summary'"!="yes") exit
#
# Replace the examples by the known message descriptions:
#
      for(Num in LoginsNum) Example[Num]=LoginsNum[Num]
      for(Num in IgnoreNum) Example[Num]=IgnoreNum[Num]

      printf"\nSummary per the message numbers:\n"
      for(Num in TotalNumCount) if(TotalNumCount[Num]>0)
              printf"%6d %7s %s\n",TotalNumCount[Num],Num,Example[Num] >TmpFile
      for(Msg in TotalMsgCount) if(TotalMsgCount[Msg]>0)
              printf"%6d %7s %s\n",TotalMsgCount[Msg],NoNum,Msg >TmpFile
      close(TmpFile)
    } # END
  ' -

  test -f $TmpFile && \
  cat     $TmpFile | sort -n -r && \
  rm   -f $TmpFile

} # LogParser()


#------------------------------------------------------------------------------
#
Msg()
{
  Num=$1

  test -z "$DLC" && \
  echo "Progress DLC environment variable variable is not set." >&2 && \
  return

  if [ ! -f $DLC/prohelp/msgdata/msg1 ]
  then
    echo "Progress DLC environment variable may not be set correctly.
Set DLC variable to Progress installation directory.
Progress DLC setting: $DLC" >&2
    return
  fi

  test -z "$Num" && \
  echo "You do't specify the message number." >&2 && \
  return

  test $Num -gt 0 2>/dev/null
  if [ $? -ne 0 ]
  then
    echo "Message number ($Num) should be an integer greater than 0." >&2
    return
  fi

  i=`expr $Num - 1`
  i=`expr $i / 50`
  i=`expr $i + 1`

  File=$DLC/prohelp/msgdata/msg$i

  if [ ! -f $File ]
  then
    echo "Failed to find $File"
    echo "$Num seemed too large for current Progress version"
    cat $DLC/version
    return
  fi >&2

  test ! -r $File && \
  echo "You don't have the permissions to read $File" >&2 && \
  return

  awk '$1=='$Num' {
    i=length($1)+3
    Text=substr($0,i)
    i=index(Text,"\" \"")
    Desc=substr(Text,i+3)
    Text=substr(Text,1,i-1)
    i=index(Desc,"\" \"")
    Desc=substr(Desc,1,i-1)

    print Text
    print Desc
    exit
  }' $File | \
  sed -e 's/\"\"/\"/'
} # Msg()

#------------------------------------------------------------------------------
#

# Set the default interval for login/logout statistics:
#
Options=6  # per hour

FileList=""
while [ $# -gt 0 ]
do
  case $1 in
    -m) Msg $2; exit;;
    -h*|-help|--help) Usage;;
    [0-9]*|-*) Options="$Options $1";;
    *) test ! -f "$1" && echo "Input file $1 does not exist!" && exit 1
       FileList="$FileList $1"
    ;;
  esac # case $1
  shift
done  # while [ $# -gt 0 ]

# Trim the spaces:
FileList=`echo $FileList`
test "$Options" || \
Options="-prs" # = Params Remains Summary

echo "$THIS Release $Release"

test -z "$FileList" && \
LogParser $Options || \
for File in $FileList
do
  Size=`ls -l $File | awk '{print $5}' 2>/dev/null`
  echo "
Log File: $File
Log Size: $Size"
  test $Size -gt 20000000 && \
  echo "Wait..."
  cat $File | LogParser $Options
done 2>&1

#
#------------------------------------------------------------------------------
#
# This is the end of dblog.sh
