#!/bin/sh
#
#
# Below is used for the pulling the area utilization report againt single Db or all DBs under same server
#
#
# Usage: ./check_area_util.sh <ALL/DB Name>
# Functionality:
#       Get the Actual DB Size
#	Get Max area size & Current utilization per area.
#	Get the current utilization of all the file system where the database files resides. 
#	Sent mail to specified group as per the configuration. 
#	It.s a generic script and can be used for any database on any server. Just need to pass the database name as the parameter. 
#
DB_NAME="${1%%\.db}"
MAIL_ID="xxxxxxx@xxx.com"
CNT=0

if [ $# != 1 ]
then
  echo "Shuld run in format $0 <ALL/Database Name>"
  exit
fi

if [ ! -f ${DB_NAME}.db ]; then
   if [ "${DB_NAME}" = "ALL" ]; then
      echo "Running for all the DBs"
   else
      echo "Database ${DB_NAME} doesn't exist. Pass parameter as ALL or please enter the correct DB. Exiting..."
      exit
   fi
fi

DATE=`date +%Y-%m-%d:%H:%M:%S`

echo "Report Time:$DATE">/tmp/area_rpt.txt
echo "============================">>/tmp/area_rpt.txt

#Below Function for statistics calculation
#below for the area utilization check
area_util()
{
   DB_NM="${1%%\.db}"
   echo "Database Name: ${DB_NM}">>/tmp/area_rpt.txt
   prostrct statistics ${DB_NM}>/tmp/stat_rpt.txt
   DB_BLK_SIZE=`cat /tmp/stat_rpt.txt|grep "Primary data block size:"|awk -F":" '{print $2}'| sed 's,^ *,,; s, *$,,'` 
   TOT_BLK_USD=`cat /tmp/stat_rpt.txt|egrep "Database Block Usage for Area|Data blocks"|tail -1|awk -F":" '{print $2}'| sed 's,^ *,,; s, *$,,'`
   TOT_DB_SIZE=`echo "scale=4;(${TOT_BLK_USD}*${DB_BLK_SIZE})/(1024*1024*1024)"|bc`
   if [ ${TOT_DB_SIZE} -lt 1 ]; then
      TOT_DB_SIZE="0${TOT_DB_SIZE}"
   fi
   cat /tmp/stat_rpt.txt|egrep "Database Block Usage for Area|Data blocks"|awk 'NR>2'|sed '$ d'>/tmp/area_rpt
   echo "Total DB Size:${TOT_DB_SIZE} GB">>/tmp/area_rpt.txt
   echo "">>/tmp/area_rpt.txt
exec</tmp/area_rpt
   while read LINE; do
      if [ "$CNT" = "0" ]; then
         AREA_NAM=`echo $LINE|awk -F":" '{print $2}'| sed 's,^ *,,; s, *$,,'`
         CNT=1
      else
         CNT=0
         USED_BLK=`echo $LINE|awk -F":" '{print $2}'| sed 's,^ *,,; s, *$,,'`
         USED_SIZE=`echo "scale=4;(${USED_BLK}*${DB_BLK_SIZE})/(1024*1024*1024)"|bc`
         if [ ${USED_SIZE} -lt 1 ]; then
	    USED_SIZE="0${USED_SIZE}"
         fi
         REC_PER_BLK=`cat ${DB_NM}.st|grep -w "${AREA_NAM}"|tail -1|awk -F":" '{print $2}'|awk -F" " '{print $1}'|awk -F";" '{print $1}'|awk -F"," '{print $2}'`
         CLUST_SIZE=`cat ${DB_NM}.st|grep -w "${AREA_NAM}"|tail -1|awk -F":" '{print $2}'|awk -F" " '{print $1}'|awk -F";" '{print $2}'`
         if [ "${CLUST_SIZE}" = "1" -o "${CLUST_SIZE}" = "" ]; then
            MAX_AREA_SIZE=`echo "((2147483648/${REC_PER_BLK})*${DB_BLK_SIZE})/(1024*1024*1024)"|bc`
            MAX_AREA_SIZE="${MAX_AREA_SIZE} GB"
  	 else
	    MAX_AREA_SIZE="1024 TB"
     	 fi
	 echo "${AREA_NAM}">>/tmp/area_rpt.txt
         echo "-----------------">>/tmp/area_rpt.txt
	 echo "Max Area Size:${MAX_AREA_SIZE}">>/tmp/area_rpt.txt
       	 echo "Area Utilized:${USED_SIZE} GB">>/tmp/area_rpt.txt
     fi
  done
  echo "">>/tmp/area_rpt.txt
  echo "Database Filesystem Utilization">>/tmp/area_rpt.txt
  echo "-----------------------------------">>/tmp/area_rpt.txt
  cat ${DB_NM}.st|grep "^d"|awk -F":" '{print $2}'|awk -F" " '{print $2}'|awk -F"_" '{print $1}'|awk -F"." '{print $1}'|sort -u>/tmp/db_files.txt
  for LINE in `cat /tmp/db_files.txt`; do
     df -k ${LINE}*|sort -u|grep -v "Filesystem"|sed '$d'|awk -F" " '{print $5 "--" $4 " Used"}'>>/tmp/file_util.txt
  done
  cat /tmp/file_util.txt|sort -u>>/tmp/area_rpt.txt
  rm /tmp/file_util.txt
}
if [ "${DB_NAME}" = "ALL" ]; then
   for i in `cat /pro/admin/ctl/protab|grep -v "#"|awk 'NF>0'|awk -F" " '{print $3"/"$1}'|awk -F"," '{print $1}'`; do
      area_util $i
      echo "************************************************************************">>/tmp/area_rpt.txt
      echo "">>/tmp/area_rpt.txt
   done
else
   area_util ${DB_NAME}
fi
echo "">>/tmp/area_rpt.txt
echo "Regards,">>/tmp/area_rpt.txt
echo "Progress DBA">>/tmp/area_rpt.txt
cat /tmp/area_rpt.txt|mailx -r ehmaholddbamailbox@hpe.com -s "NOTIFY:: AREA UTILIZATION REPORT -- ${DB_NAME} -- `hostname`" ${MAIL_ID}
