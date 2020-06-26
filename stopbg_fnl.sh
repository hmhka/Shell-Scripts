#! /bin/sh
#############################################################################
# PROGRAM NAME: stopbg_fnl.sh
# PROGRAMMER:   Rejean Drouin
# DATE:         01/30/2014
# VERSION:      1.0
# REVISION:
# USAGE:        stopbg_fnl.sh primary | secondary | tertiary | rqbgs | all
#
# CHANGE 1:     Added stopping secondary Bg's in the output log file 01/30/2014  -- SSS
# CHANGE 2:     Added stopping tertiary Bg's 04/28/2014 -- SSS
# CHANGE 3:    Added stopping RS_ODS  Bg's 09/10/2014 -- Ved Prakash
# CHANGE 4:    Added stopping 4new set of Bg's 22/02/2015 [this is for rabbitmq] -- SSS
# CHANGE 5:    Added stopping ICWMQ bg's 26/jul/15 [for icasework project] -- SSS
#############################################################################

# Script added to operation menu to disconnect all users from a database
#
# Takes the database name in a loop and disconnects the users  (ie: amos mss)

if ! [ "$1" = "all" -o "$1" = "primary" -o "$1" = "secondary" -o "$1" = "tertiary" -o "$1" = "RS_ODS" -o "$1" = "ACBInbMQ" -o "$1" = "ACBDmnMQ" -o "$1" = "CMSDmnMQ" -o "$1" = "CMSInbMQ" -o "$1" = "ICWMQ" ]
then
  echo "You must pass a valid parameter."
  echo "Usage: $0 primary | secondary | tertiary | RS_ODS | ACBInbMQ | ACBDmnMQ | CMSDmnMQ | CMSInbMQ | ICWMQ | all"
  exit 1

fi

###set -x
tokendir=/tmp                          
dbase=/emc/reale/prod		# DB Base Location
DLC=/usr/dlc; export DLC
LOG=$HOME/log/stopbg.log
TERM=vt100
STARTDIR=$HOME/bin
cd $STARTDIR

echo "**** Start Time: `date` stopping $1 BG's ID=`who am i|awk '{print $1}'` ****" >> $LOG
dblist="amos mss"
cnt=0
       for db in $dblist
       do
	    echo "Running for $db">>$LOG
            cnt=`expr $cnt + 1`
             # sleep 2 
                 
## added grep "-i _auto-bu" to make sure we are evicting only batch job
                $DLC/bin/mpro -b $dbase/$db/data/$db -U applid -P applid -p stopbg_fnl.p | grep _auto-bu|
                while read i
                  do
                    usrno=`echo $i|cut -d, -f1`
                    pid=`echo $i|cut -d, -f2`
                    if [ "$1" = "secondary" -o "$1" = "tertiary" -o "$1" = "RS_ODS" -o "$1" = "ACBInbMQ" -o "$1" = "ACBDmnMQ" -o "$1" = "CMSDmnMQ" -o "$1" = "CMSInbMQ" -o "$1" = "ICWMQ" ]
                    then
                      if ! ps -o args -p $pid | grep "$1" >/dev/null 2>&1
                      then
                        continue
                      fi
                    elif [ "$1" = "primary" ]      
                    then
                      if ps -o args -p $pid | grep "secondary" >/dev/null 2>&1
                      then
                        continue
                      fi
                      if ps -o args -p $pid | grep "tertiary" >/dev/null 2>&1
                      then
                        continue
                      fi
                      if ps -o args -p $pid | grep "RS_ODS" >/dev/null 2>&1
                      then
                        continue
                      fi
                      if ps -o args -p $pid | grep "ACBInbMQ"  >/dev/null 2>&1
                      then
                        continue
                      fi
                      if ps -o args -p $pid | grep "ACBDmnMQ"  >/dev/null 2>&1
                      then
                        continue
                      fi
                      if ps -o args -p $pid | grep "CMSDmnMQ"  >/dev/null 2>&1
                      then
                        continue
                      fi
                      if ps -o args -p $pid | grep "CMSInbMQ"  >/dev/null 2>&1
                      then
                        continue
	              fi
                      if ps -o args -p $pid | grep "ICWMQ" >/dev/null 2>&1
                      then
                        continue	
                      fi
                    fi
                    echo $i >> $LOG
                    $DLC/bin/proshut $dbase/$db/data/$db -C disconnect $usrno
                  done
       done
echo "**** End Time: `date` Stopped above $1 BG's ****" >> $LOG
