#!/bin/bash
#########################################################################################################
# Script                : apache_start_stop.sh
# Description           : Script for start/stopp the apache in RS Servers
# Input parameters      : Input parametr file with server details (eg: appsrv_webspeed_stop_start.par)
#
# Modification history
# Date          Modified by     Version         Details
# -----------   -----------     -------         --------------------------
# 07-Mar-2014   Ved Prakash      1.0            Start up
# 19-OCT-2015   Suneesh          1.1            Changed the logic to take server details from input file
#                                               and removed all hard coded details.
#########################################################################################################

DLC=/usr/dlc; export DLC
PATH=.:/usr/local/bin:/usr/dlc/bin:/usr/bin:/bin:/sbin:/usr/home/progress/bin:/usr/sbin:$DLC/bin:$DLC:/usr/ucb;export PATH
SCRPT_PTH=`dirname $0`
#SCRPT_PTH=/usr/home/progress/bin
HOSTN=`hostname`
WS_APP_REG="${SCRPT_PTH}/wa.registry"
SCR_DIR=$HOME/bin
export SCR_DIR
export LOG_DIR=${SCR_DIR}/log
export FAILURE_NOTIFY_LIST=Progress-DBA@altisource.com
##export FAILURE_NOTIFY_LIST=Suneesh.Babu@altisource.com
export ACTION=$1

cd $SCR_DIR
if [ $ACTION = "start" ] || [ "$ACTION" = "stop" ]
   then
   export LOG_FILE=${LOG_DIR}/apache_${ACTION}.log
   tail -1000 ${LOG_DIR}/apache_${ACTION}.log >${LOG_DIR}/apache_${ACTION}.log.tmp
   mv ${LOG_DIR}/apache_${ACTION}.log.tmp ${LOG_DIR}/apache_${ACTION}.log
else
   echo "`date`:Incorrect action $ACTION specified for Apache restart script." |mailx -s "JOB FAILURE: Apache restart failed" $FAILURE_NOTIFY_LIST
   exit 1
fi
if [ ! -s $WS_APP_REG ]
   then
   echo "`date`:Apache restart failed. Could not find the input parameter file $PARAM_FILE" |mailx -s "JOB FAILURE: Apache restart failed" $FAILURE_NOTIFY_LIST
   exit 1
fi

if [ "$ACTION" = "stop" ]
   then
   export CMD=disable
else
   export CMD=enable
fi

for i in `cat ${WS_APP_REG}|grep -v "#" |grep -v "novxpwrsnp09" |tr " " ","`; do
   f=`echo $i|tr "," " "`
   PSEV=`echo $f|awk -F" " '{print $1}'| tr '[A-Z]' '[a-z]'`
   if [ "${HOSTN}" = "${PSEV}" ];
      then
      /usr/bin/sudo /sbin/svcadm $CMD /network/http
      echo "Apache ${ACTION} performed in ${PSEV} at `date` by" `logname` >> $LOG_FILE
      sleep 1
   else
      ssh -t -t $PSEV "/usr/bin/sudo /usr/local/REALServicing/apache-2.2.25/bin/apachectl $ACTION " >> $LOG_FILE
      echo "Apache ${ACTION} performed in ${PSEV} at `date` by" `logname` >> $LOG_FILE
      sleep 2
   fi
done

sleep 8

if [ "$ACTION" = "start" ]
   then
   for i in `cat ${WS_APP_REG}|grep -v "#" | tr " " ","`; do
      f=`echo $i|tr "," " "`
      PSEV=`echo $f|awk -F" " '{print $1}'| tr '[A-Z]' '[a-z]'`
      if [ "$HOSTN" = "${PSEV}" ];
         then
         ps -ef |grep -v grep |grep "httpd -k start" >/tmp/$PSEV.start.out
         APACHVL=`head /tmp/$PSEV.start.out -1 |tr -d '\r' |awk -F" " '{print $NF  }'`
         if [ "$APACHVL" = "start" ];
            then
            echo "`date`:Apache is up in ${PSEV} after the restart" >> $LOG_FILE
         else
            echo "`date`:Apache is still down in ${PSEV} After the restart, check it immediately."|mailx -s "JOB FAILURE: Apache restart failed" $FAILURE_NOTIFY_LIST
         fi
      else
         ssh -t -t $PSEV "ps -ef |grep -v grep |grep 'httpd -k start'" >/tmp/$PSEV.start.out
         APACHVL=`head /tmp/$PSEV.start.out -1 |tr -d '\r' |awk -F" " '{print $NF  }'`
         if [ "$APACHVL" = "start" ];
            then
            echo "`date`:Apache is up in ${PSEV} after the restart" >> $LOG_FILE
         else
            echo "`date`:Apache is still down in ${PSEV} After the restart, check it immediately."|mailx -s "JOB FAILURE: Apache restart failed" $FAILURE_NOTIFY_LIST
         fi
      fi
   done
elif [ "$ACTION" = "stop" ]
   then
   for i in `cat ${WS_APP_REG}|grep -v "#" |grep -v "novxpwrsnp09"| tr " " ","`; do
      f=`echo $i|tr "," " "`
      PSEV=`echo $f|awk -F" " '{print $1}'| tr '[A-Z]' '[a-z]'`
      if [ "$HOSTN" = "${PSEV}" ];
         then
         ps -ef |grep -v grep |grep "httpd -k start" >/tmp/$PSEV.start.out
         APACHVL=`head /tmp/$PSEV.start.out -1 |tr -d '\r' |awk -F" " '{print $NF  }'`
         if [ "$APACHVL" = "start" ];
            then
            echo "`date`:Apache is still up in ${PSEV} after the shutdown, Please cross check it."|mailx -s "JOB FAILURE: Apache restart failed" $FAILURE_NOTIFY_LIST
         else
            echo "`date`:Apache is down in ${PSEV} after the restart" >> $LOG_FILE
        fi
      else
         ssh -t -t $PSEV "ps -ef |grep -v grep |grep 'httpd -k start'" >/tmp/$PSEV.start.out
         APACHVL=`head /tmp/$PSEV.start.out -1 |tr -d '\r' |awk -F" " '{print $NF  }'`
         if [ "$APACHVL" = "start" ];
            then
            echo "`date`:Apache is still up in ${PSEV} after the shutdown, Please cross check it."|mailx -s "JOB FAILURE: Apache restart failed" $FAILURE_NOTIFY_LIST
         else
            echo "`date`:Apache is down in ${PSEV} after the restart" >> $LOG_FILE
         fi
      fi
   done
fi

