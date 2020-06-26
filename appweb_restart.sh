#!/bin/bash
###############################################################################################
#       Script Name     : appweb_restart.sh
#       Purpose         : to stop/start/query the appserver/webspeed
#       Author          : Sreedev M/ Suneesh
#       Primary version : 10-Mar-2015
#       Usage           : appweb_restart.sh <REGION> <Broker Type>  <start/stop/query> [<Brkr_name>]
###############################################################################################

WS_APP_REG="${HOME}/bin/ws.registry"
#WS_APP_REG="/tmp/suneesh/ws.registry"
WS_APP_REG_TMP="${WS_APP_REG}.tmp"
if [ ! -f ${WS_APP_REG} ]; then
   echo "$WS_APP_REG is missing on this server. Exiting...."
   exit
fi

cat /dev/null>${WS_APP_REG_TMP}
webmenu()
{
   echo "In WebMenu"
   if [ `cat ${DLC}/properties/ubroker.properties|grep -w "WS.${1}"|wc -l` -eq 0 ]; then
      echo "${1} Webspeed is not configured in ubroker.properties."
      return
   fi
   if [ "$BRKR_NME" != "" -a "$BRKR_NME" != "$1" ]; then
      return
   fi

   if [ "$2" = "start" ]; then
      wtbman -i ${1} -start 
      #echo "Started the Webspeed ${1}..."
   elif [ "$2" = "stop" ]; then
      wtbman -i ${1} -stop 
      #echo "Initiated stop of Webspeed ${1}..."
   elif [ "$2" = "query" ]; then
      wtbman -i ${1} -q
      echo "---------------------------------------------"
   else
      echo "3rd parameter should be either start/stop/query"
   fi
}
appmenu()
{
   echo "In AppMenu"
   if [ `cat ${DLC}/properties/ubroker.properties|grep -w "AS.${1}"|wc -l` -eq 0 ]; then
      echo "${1} appserver is not configured in ubroker.properties."
      return
   fi
   if [ "$BRKR_NME" != "" -a "$BRKR_NME" != "$1" ]; then
      return
   fi
   if [ "$2" = "start" ]; then
      asbman -i ${1} -start
      #echo "Started the appserver ${1}..."
   elif [ "$2" = "stop" ]; then
      asbman -i ${1} -stop 
      #echo "Initiated stop of appserver ${1}..."
   elif [ "$2" = "query" ]; then
      asbman -i ${1} -q
      echo "---------------------------------------------"
   else
      echo "3rd parameter should be either start/stop/query"
   fi
}

### Main Loop
if [ $# -lt 3 -o $# -gt 4 ]; then
  echo "Wrong Usage. Run as below."
  echo "$0 <REGION> <Broker Type> <start/stop/query> [<Brkr_name>]"
  exit
fi

REGION=`echo $1| tr '[A-Z]' '[a-z]'`
WS_APP_TYP=`echo $2| tr '[A-Z]' '[a-z]'`
REQ_TYP=`echo $3| tr '[A-Z]' '[a-z]'`
BRKR_NME="$4"
echo "REGION ---> $REGION"
##Region selection
if [ "$REGION" = "all" ]; then
   cat ${WS_APP_REG}|grep -v  "^#">${WS_APP_REG_TMP}
else
   #cat ${WS_APP_REG}|awk -F" " '{print $1}'|grep -iw $REGION

   if [ `cat ${WS_APP_REG}|awk -F" " '{print $1}'|grep -v "^#"|grep -iw $REGION|wc -l` -gt 0 ]; then
exec<${WS_APP_REG}
      while read f; do
         F_ENV=`echo $f|grep -v "^#"|awk -F" " '{print $1}'| tr '[A-Z]' '[a-z]'`
######echo "F_ENV= $F_ENV REGION=$REGION"
         if [ "$F_ENV" = "$REGION" ]; then
            echo "$f" >> ${WS_APP_REG_TMP}
         fi
      done
   else
      echo "No Webspeed configured for $REGION !!!"
      exit
   fi
fi
echo "WS_APP_TYP=$WS_APP_TYP"
##Broker type selection
if [ "$WS_APP_TYP" = "all" ]; then
exec<${WS_APP_REG_TMP}
    while read f; do
       IN_APP_TYP=`echo $f|awk -F" " '{print $2}'| tr '[A-Z]' '[a-z]'`
       IN_APP_NAM=`echo $f|awk -F" " '{print $3}'`
       if [ "$IN_APP_TYP" = "wbs" ]; then
          webmenu ${IN_APP_NAM} ${REQ_TYP}
       elif [ "$IN_APP_TYP" = "aps" ]; then
	  appmenu ${IN_APP_NAM} ${REQ_TYP}
       else
          echo "$IN_APP_NAM -- Wrong broker type. Should be either APS or WBS."
       fi
    done
elif [ "$WS_APP_TYP" = "wbs" ]; then
    if [ `cat ${WS_APP_REG_TMP}|awk -F" " '{print $2}'|grep -iw $WS_APP_TYP|wc -l` -gt 0 ]; then
exec<${WS_APP_REG_TMP}
      while read f; do
         F_ENV=`echo $f|awk -F" " '{print $2}'| tr '[A-Z]' '[a-z]'`
         if [ "$F_ENV" = "$WS_APP_TYP" ]; then
	    IN_APP_NAM=`echo $f|awk -F" " '{print $3}'`
            webmenu ${IN_APP_NAM} ${REQ_TYP}
         fi
      done
   else
      echo "No Webspeed configured as per ${WS_APP_REG}!!!"
      exit
   fi
elif [ "$WS_APP_TYP" = "aps" ]; then
    if [ `cat ${WS_APP_REG_TMP}|awk -F" " '{print $2}'|grep -iw $WS_APP_TYP|wc -l` -gt 0 ]; then
exec<${WS_APP_REG_TMP}
      while read f; do
         F_ENV=`echo $f|awk -F" " '{print $2}'| tr '[A-Z]' '[a-z]'`
         if [ "$F_ENV" = "$WS_APP_TYP" ]; then
            IN_APP_NAM=`echo $f|awk -F" " '{print $3}'`
            appmenu ${IN_APP_NAM} ${REQ_TYP}
         fi
      done
   else
      echo "No appserver configured as per ${WS_APP_REG}!!!"
      exit
   fi
else
   echo "Wrong broker type. Should be either APS or WBS."
fi
