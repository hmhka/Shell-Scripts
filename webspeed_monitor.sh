#!/bin/bash
#############################################################################################
# Script:  webspeed_monitor.sh                                                              #
# Description   : Script to monitor webspeed and appserver, it will provide input for zabbix#
#                 provide input for Noc and will send a alert once it hit threshold         #
#			                                                                    #
# Modification history									    #
# Date  	Modified by     Version 	Details					    #
# -----------   -----------     ------- 	--------------------------                  #
# 23-Sep-2015   Suneesh 1.0     1.0    		Script to check the status from all Servers #
#############################################################################################
DLC=/usr/dlc; export DLC
PATH=.:/usr/local/bin:/usr/dlc/bin:/usr/bin:/bin:/sbin:/usr/home/progress/bin:/usr/sbin:$DLC/bin:$DLC:/usr/ucb;export PATH
#SCRPT_PTH=`dirname $0`
SCRPT_PTH=/usr/home/progress/bin
LOGPT=/emc/reale/apps/webbroker_status
HOSTN=`hostname`
WS_APP_REG="${SCRPT_PTH}/wa.registry"
OPDIREC=/emc/reale/apps/pseudo/output/perf_test/
cd $LOGPT
XTIME=`date|awk '{print $3 $2 $6 " " $4}'`
MAIL_LIST=progress-dba@altisource.com
###MAIL_LIST=Suneesh.Babu@altisource.com

callmenu()
{
   f=`echo $i|tr "," " "`
   PSEV=`echo $f|awk -F" " '{print $1}'| tr '[A-Z]' '[a-z]'`
   PSWA=`echo $f|awk -F" " '{print $2}'| tr '[A-Z]' '[a-z]'`
   PSCM=`echo $f|awk -F" " '{print $3}'| tr '[A-Z]' '[a-z]'`
   if [ "$PSCM" = "wtbman" ]; then
      PAA=webspeed
   else
      PAA=appserver
   fi
}

statcheck()
{
   cd $LOGPT
   TS=`date +'%Y%m%d%H%M%S'`
   TOTAL_AGENTS=`cat ${1}.${2}.list | grep maxSrvr | awk -F'=' '{print $2}' |sed -e "s///"`
   BUSY_AGENTS=`cat ${1}.${2}.stat | egrep -i "Busy Agents|Busy Servers" | awk '{print $4}' |sed -e "s///"`
   tail -2050 broker_logs.${PSEV}.${PSWA} > broker_logs.${PSEV}.${PSWA}.tmp
   mv broker_logs.${PSEV}.${PSWA}.tmp broker_logs.${PSEV}.${PSWA}
   if [ -s $BUSY_AGENTS ]
      then
      BUSY_AGENTS=0
   fi
   cd $OPDIREC
   tail -2000 ${PSEV}_${PSWA} > ${PSEV}_${PSWA}.tmp
   mv ${PSEV}_${PSWA}.tmp ${PSEV}_${PSWA}
   echo "${TS}|${PSWA}|${TOTAL_AGENTS}|${BUSY_AGENTS}" >> ${PSEV}_${PSWA}
}

## Below to calculate automatically the 70% of max(agents)
for i in `cat ${WS_APP_REG}|grep -v "#" |tr " " ","`; do
   f=`echo $i|tr "," " "`
   PSEV=`echo $f|awk -F" " '{print $1}'| tr '[A-Z]' '[a-z]'`
   PSWA=`echo $f|awk -F" " '{print $2}'| tr '[A-Z]' '[a-z]'`

   cd $LOGPT
   MAX_AGENT=`cat ${PSEV}.${PSWA}.list| grep maxSrvr | awk -F'=' '{print $2}' |sed -e "s///"`
   if [ -s $MAX_AGENT ]
      then
      MAX_AGENT=0
   fi
   TEMP_VAL1=`expr ${MAX_AGENT} \* 70`
   TEMP_VAL2="$TEMP_VAL1/100"
   PER_VAL=`echo $TEMP_VAL2 |bc -l`
   AGENT_LIMIT=`echo $PER_VAL|awk '{print int($1+0.5)}'`
   LOGFILE_NAME='broker_logs.'${PSEV}."${PSWA}"
   TMP_FILE='busy_agents.'${PSEV}."${PSWA}"
   BUSY_AGENT_NO=`cat ${PSEV}.${PSWA}.stat |egrep -i "Busy Agents|Busy Servers" |awk -F":" '{print $2}'|sed -e "s///"`
   echo "No. of busy agents : $BUSY_AGENT_NO"

   if [ $BUSY_AGENT_NO -ge $AGENT_LIMIT -a "$AGENT_LIMIT" != "0" ]
      then
      echo "sent mail"
      echo "Hi,">mail_body.txt
      echo "  Webspeed Broker ${PSWA} hit the maximum threshold ($AGENT_LIMIT) at $XTIME">>mail_body.txt
      echo "">>mail_body.txt
      echo "No. of busy agents @ $XTIME is : $BUSY_AGENT_NO">>mail_body.txt
      echo "">>mail_body.txt
      echo "Progress DBA">>mail_body.txt
      mailx -s "Webspeed Broker status--${PSWA}" $MAIL_LIST< mail_body.txt
      echo "No. of busy agents @ $XTIME is : $BUSY_AGENT_NO"
   else
      echo "dont sent"
      echo "No. of busy agents @ $XTIME is : $BUSY_AGENT_NO">>$LOGFILE_NAME
   fi
done
for i in `cat ${WS_APP_REG}|grep -v "#" |tr " " ","`; do
   callmenu i;
   statcheck ${PSEV} ${PSWA} ;
done
