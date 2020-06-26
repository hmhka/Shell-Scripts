#!/bin/bash
#############################################################################################
# Script                : gen_strg_grth_rpt.sh 
# Description           : This script will generate csv file with area growth report within the given i/p date range 
#
# Input parameters      : start_date,end_date,mail_flg 
# Required files        : /tmp/area<start_date>.txt, /tmp/area<end_date>.txt 
#
# Modification history
# Date          Modified by     Version         Details
# -----------   -----------     -------         --------------------------
# 02/08/2015    Sreedev M       1.0             initial
#############################################################################################

#mkdir /tmp/tt${1}
RPT_PTH=/emc/reale/archive/dban
RPT_PTH_TMP=/tmp
STRT_FL="area${1}.txt"
END_FL="area${2}.txt"
MAIL_FLG=$3
STORAGE_OP_FL="/tmp/area_rpt.csv"
MAIL_GRP=Progress-DBA@altisource.com
MAIL_BDY=/tmp/mail_bdy.txt
echo "Please find the attached area growth report for ${2}.">${MAIL_BDY}
#echo ",,,,,${1},,${2},,,,,">$STORAGE_OP_FL
if [ ! -f /tmp/rpt_flg_${1}_${2} ]; then
   #echo "DB Nm,AreaName,MaxBlocks,Size(in MB),Old- HiWater Used,Old - HiWater Used (in MB),Current HiWater Used,Current HiWater Used(MB),Free Blks,Free(MB),Monthly Blocks,M.Grw(MB),Mths Avail,">$STORAGE_OP_FL
   rm ${STORAGE_OP_FL}
   #cp /emc/reale/archive/dban/${STRT_FL} /emc/reale/archive/dban/${END_FL} ${RPT_PTH}
   if [ ! -d "$RPT_PTH" ]; then
      mkdir $RPT_PTH
   fi
   if [ $# != 3 ]; then
      echo "Wrong usage. Please run as below"
      echo "$0 <start Date in MMDDYY> <end Date in MMDDYY>"
      exit
   fi

   if [ ! -f ${RPT_PTH}/${STRT_FL} ]; then
      echo "Old date data file is missing"
      echo "Input File not Available">${STORAGE_OP_FL}
      touch /tmp/strg_fl_not_found
      exit
   fi
   if [ ! -f ${RPT_PTH}/${END_FL} ]; then
      echo "new date data file is missing"
      echo "Input File not Available">${STORAGE_OP_FL}
      touch /tmp/strg_fl_not_found
      exit
   fi
   cp /emc/reale/archive/dban/${STRT_FL} /emc/reale/archive/dban/${END_FL} ${RPT_PTH_TMP}
   function ceil()
   {
      VAL=$(echo $1 +.5 | bc)
      RET_VAL=`echo "${VAL%.*}"`
      if [ "${RET_VAL}" = "" ]; then
         RET_VAL=0
      fi
      echo "${RET_VAL}"
      #echo "${VAL%.*}"
   }
   cd ${RPT_PTH}
   echo -e "START FILE:${STRT_FL}\nEND FILE:${END_FL}"

   cat ${STRT_FL}|egrep -vi "Control Area|Primary Recovery Area|Schema Area|After Image Area|^UNIQUEID|^FIELDHSTSEQ"|awk 'NF>0'>${STRT_FL}.tmp
   cat ${END_FL}|egrep -vi "Control Area|Primary Recovery Area|Schema Area|After Image Area|^UNIQUEID|^FIELDHSTSEQ"|awk 'NF>0'>${END_FL}.tmp
   #for i in `cat ${STRT_FL}.tmp`; do
   exec<${STRT_FL}.tmp
   while read i; do
echo "---------->$i<------------------"
      OLD_DB_NM=`echo $i|awk -F" " '{print $1}'`
      OLD_AREA_NM=`echo $i|awk -F" " '{print $2}'`
      OLD_WTR_MRK=`echo $i|awk -F" " '{print $4}'`
      if [ `cat ${RPT_PTH}/${END_FL}.tmp|grep -w ${OLD_DB_NM}|grep -w ${OLD_AREA_NM}|wc -l` -gt 0 ]; then
         NEW_DB_NM=`cat ${END_FL}.tmp|grep -w ${OLD_DB_NM}|grep -w ${OLD_AREA_NM}|awk -F" " '{print $1}'`
         NEW_AREA_NM=`cat ${END_FL}.tmp|grep -w ${OLD_DB_NM}|grep -w ${OLD_AREA_NM}|awk -F" " '{print $2}'`
         NEW_MAX_BLOK=`cat ${END_FL}.tmp|grep -w ${OLD_DB_NM}|grep -w ${OLD_AREA_NM}|awk -F" " '{print $3}'`
         NEW_WTR_MRK=`cat ${END_FL}.tmp|grep -w ${OLD_DB_NM}|grep -w ${OLD_AREA_NM}|awk -F" " '{print $4}'`
         SIZE_IN_MB_TMP=`echo "${NEW_MAX_BLOK}*8192/(1024*1024)"|bc -l`
         SIZE_IN_MB=`ceil $SIZE_IN_MB_TMP`
         OLD_HI_WTR_IN_MB_TMP=`echo "${OLD_WTR_MRK}*8192/(1024*1024)"|bc -l`
         OLD_HI_WTR_IN_MB=`ceil $OLD_HI_WTR_IN_MB_TMP`
         CURR_HI_WTR_IN_MB_TMP=`echo "${NEW_WTR_MRK}*8192/(1024*1024)"|bc -l`
         CURR_HI_WTR_IN_MB=`ceil $CURR_HI_WTR_IN_MB_TMP`
         FREE_BLK_TMP=`echo "${NEW_MAX_BLOK}-${OLD_WTR_MRK}"|bc -l`
         FREE_BLK=`ceil $FREE_BLK_TMP`
         FREE_BLK_IN_MB_TMP=`echo "${FREE_BLK}*8192/(1024*1024)"|bc -l`
         FREE_BLK_IN_MB=`ceil $FREE_BLK_IN_MB_TMP`
         MNTHLY_BLK_TMP=`echo "${NEW_WTR_MRK}-${OLD_WTR_MRK}"|bc -l`
         MNTHLY_BLK=`ceil $MNTHLY_BLK_TMP`
         MGROW_IN_MB_TMP=`echo "${MNTHLY_BLK}*8192/(1024*1024)"|bc -l`
         MGROW_IN_MB=`ceil $MGROW_IN_MB_TMP`
         if [ ${MNTHLY_BLK} -ne 0 ]; then
            MNTHS_AVAIL=`echo "scale=2; ${FREE_BLK}/${MNTHLY_BLK}"|bc`
            #MNTHS_AVAIL=`ceil $MNTHS_AVAIL_TMP`
         else
            MNTHS_AVAIL="###"
         fi
         echo "$NEW_DB_NM,$NEW_AREA_NM,$NEW_MAX_BLOK,${SIZE_IN_MB}MB,$OLD_WTR_MRK,$OLD_HI_WTR_IN_MB,$NEW_WTR_MRK,$CURR_HI_WTR_IN_MB,$FREE_BLK,${FREE_BLK_IN_MB}MB,$MNTHLY_BLK,${MGROW_IN_MB}MB,$MNTHS_AVAIL,">>$STORAGE_OP_FL
     fi
echo "+++++++++++++ $NEW_DB_NM,$NEW_AREA_NM,$OLD_WTR_MRK +++++++++++++"
     #echo "$NEW_DB_NM,$NEW_AREA_NM,$NEW_MAX_BLOK,${SIZE_IN_MB}MB,$OLD_WTR_MRK,$OLD_HI_WTR_IN_MB,$NEW_WTR_MRK,$CURR_HI_WTR_IN_MB,$FREE_BLK,${FREE_BLK_IN_MB}MB,$MNTHLY_BLK,${MGROW_IN_MB}MB,$MNTHS_AVAIL,">>$STORAGE_OP_FL
     #  echo "$NEW_DB_NM,$NEW_AREA_NM,$NEW_MAX_BLOK,${SIZE_IN_MB}MB,$OLD_WTR_MRK,$OLD_HI_WTR_IN_MB,$NEW_WTR_MRK,$CURR_HI_WTR_IN_MB,$FREE_BLK,${FREE_BLK_IN_MB}MB,$MNTHLY_BLK,${MGROW_IN_MB}MB,$MNTHS_AVAIL"
   done
   exec<${END_FL}.tmp
   while read i; do
      NEW_DB_NM=`echo $i|awk -F" " '{print $1}'`
      NEW_AREA_NM=`echo $i|awk -F" " '{print $2}'`
      NEW_MAX_BLOK=`echo $i|awk -F" " '{print $3}'`
      NEW_WTR_MRK=`echo $i|awk -F" " '{print $4}'`
      if [ `cat ${RPT_PTH}/${STRT_FL}.tmp|grep -w ${NEW_DB_NM}|grep -w ${NEW_AREA_NM}|wc -l` -eq 0 ]; then
         SIZE_IN_MB_TMP=`echo "${NEW_MAX_BLOK}*8192/(1024*1024)"|bc -l`
         SIZE_IN_MB=`ceil $SIZE_IN_MB_TMP`
         CURR_HI_WTR_IN_MB_TMP=`echo "${NEW_WTR_MRK}*8192/(1024*1024)"|bc -l`
         CURR_HI_WTR_IN_MB=`ceil $CURR_HI_WTR_IN_MB_TMP`
         FREE_BLK_IN_MB_TMP=`echo "${NEW_MAX_BLOK}*8192/(1024*1024)"|bc -l`
         FREE_BLK_IN_MB=`ceil $FREE_BLK_IN_MB_TMP`
         MGROW_IN_MB_TMP=`echo "${NEW_WTR_MRK}*8192/(1024*1024)"|bc -l`
         MGROW_IN_MB=`ceil $MGROW_IN_MB_TMP`
         if [ ${MNTHLY_BLK} -ne 0 ]; then
            MNTHS_AVAIL=`echo "scale=2; ${NEW_MAX_BLOK}/${NEW_WTR_MRK}"|bc`
            #MNTHS_AVAIL=`ceil $MNTHS_AVAIL_TMP`
         else
            MNTHS_AVAIL="###"
         fi
         echo "$NEW_DB_NM,$NEW_AREA_NM,$NEW_MAX_BLOK,${SIZE_IN_MB}MB,0,0,$NEW_WTR_MRK,$CURR_HI_WTR_IN_MB,$NEW_MAX_BLOK,${FREE_BLK_IN_MB}MB,$NEW_WTR_MRK,${MGROW_IN_MB}MB,$MNTHS_AVAIL,">>$STORAGE_OP_FL
     fi
   done
   touch /tmp/rpt_flg_${1}_${2}
fi
   if [ "${MAIL_FLG}" = "on" ]; then
      echo "DB Nm,AreaName,MaxBlocks,Size(in MB),Old- HiWater Used,Old - HiWater Used (in MB),Current HiWater Used,Current HiWater Used(MB),Free Blks,Free(MB),Monthly Blocks,M.Grw(MB),Mths Avail,">${STORAGE_OP_FL}.tmp
      cat ${STORAGE_OP_FL} >>${STORAGE_OP_FL}.tmp
      #mv ${STORAGE_OP_FL}.tmp ${STORAGE_OP_FL}
      echo -e "\nRegards,\nProgress DBA">>$MAIL_BDY
      (cat ${MAIL_BDY};uuencode ${STORAGE_OP_FL}.tmp area_rprt.csv)|mailx -s "STORAGE REPORT-AUTO" $MAIL_GRP
      #rm ${STORAGE_OP_FL}.tmp
   fi
   #rm ${STRT_FL}.tmp ${END_FL}.tmp ${RPT_PTH_TMP}/${STRT_FL} ${RPT_PTH_TMP}/${END_FL}
   rm ${STRT_FL}.tmp ${END_FL}.tmp 

