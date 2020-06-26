#!/bin/bash
#############################################################################################
# Script                : appsrv_webspeed_restart.sh
# Description           : Script to restart the webspeed,appserver,apache and clear /u02/protemp/ directory daily
#
# Input parameters      : N/A
# Required files        : N/A
#
# Modification history
# Date          Modified by     Version       Details
# -----------   -----------     -------       --------------------------
# 25-March-2014 Ved Prakash     1.0           Initial
# 11-April-2014 Ved Prakash     1.1           Apache restart,disconnect _auto-ba/bs,protemp clear.
# 17-Nov-2014   Rejean D.       1.2           Truncate/Purge WS/AS logs.
# 06-Jul-2015   Suneesh Babu    1.3           Password less operation
# 14-Dec-2015   Suneesh Babu    1.4           Bug fixes to avoid unwanted alerts
#############################################################################################
DLC=/usr/dlc; export DLC
PATH=.:/usr/local/bin:/usr/dlc/bin:/usr/bin:/bin:/sbin:/usr/home/progress/bin:/usr/sbin:$DLC/bin:$DLC:/usr/ucb;export PATH
SCRPT_PTH=`dirname $0`
#SCRPT_PTH=/export/home/progress/bin
TERM=vt100
export TERM
EDITOR=vi
export EDITOR
ulimit -n 8192
HOSTN=`hostname`
HOMEN=$HOME/bin

cd $HOMEN

#touching below file to avoind nagios alert during maintenance
touch /emc/reale/apps/pseudo/stopweb_alerts.txt

## Script (apache_start_stop.sh) Stops Apache in all Server except novxpwrsnp09 Server
apache_start_stop.sh  stop

sleep 2
## Below Script Stops Webspeed in all Server except novxpwrsnp09 Server
appsrv_webspeed_stop_start.sh appsrv_webspeed_stop_start.par stop
sleep 5
appsrv_webspeed_stop_start.sh appsrv_webspeed_stop_start2.par stop

###sleep 10
## Below Script Disconnect Webspeed and app Server Connection except novxpwrsnp09 Server
# Commented by fazlur... Below script need lot of changes
#disconnect_auto-ba-bs.sh  disconnect

sleep 10
## Below Script Delete the files from /u02/protemp/ directory in all Production Server except novxpwrsnp09 Server
protemp_del.sh  delete

sleep 3

## Below Script truncates/purges WS/AS logs except novxpwrsnp09 Server
wsaslog.sh

## Below Script START Webspeed in all Server except novxpwrsnp09 Server
appsrv_webspeed_stop_start.sh appsrv_webspeed_stop_start.par start
sleep 10
appsrv_webspeed_stop_start.sh appsrv_webspeed_stop_start2.par start
sleep 10

## Script (apache_start_stop.sh) START Apache in all Server except novxpwrsnp09 Server
apache_start_stop.sh  start
sleep 5

rm /emc/reale/apps/pseudo/stopweb_alerts.txt
exit 0
