#!/bin/bash
###############################################################################################
#       Script Name     : picamon.sh                                                          #
#       Purpose         : Monitoring Pica value                                               #
#       Author          : Ravi G                                                              #
#       Primary version : 18-Dec-2014                                                         #
#       Update          : 09-Sep-2015  Changed the logic added log purging also               #
###############################################################################################

TERM=vt100
export TERM
EDITOR=vi
export EDITOR
ulimit -n 8192
DLC=/usr/dlc;export DLC
PATH=$DLC:$DLC/bin:$PATH;export PATH
PROGHOME=$HOME
OPDIREC=/emc/reale/apps/pseudo/output/perf_test
DBREG=$PROGHOME/bin/db.registry
#DAT=`date +"%m%d%y-%H:%M"`
DAT=`date '+%Y%m%d %H:%M:%S'`

if [ $# -ne 1 ]
then
  echo 'Usage: stop_db_IR.sh DBNAME '
  exit 1
fi
IN_DBNAME=`echo $1 | tr '[A-Z]' '[a-z]'`


DBREG_TMP="/tmp/db.registry.${IN_ENV}.tmp"
cat /dev/null > ${DBREG_TMP}
if [ "$IN_DBNAME" = "all" ]; then
   cat ${DBREG}|grep -v "^#">${DBREG_TMP}
else
   if [ `cat ${DBREG}|awk -F" " '{print $1}'|grep -iw $IN_DBNAME|wc -l` -gt 0 ]; then
      for i in `cat ${DBREG}|tr " " ","`; do
         f=`echo $i|tr "," " "`
         F_IDB=`echo $f|awk -F" " '{print $1}'| tr '[A-Z]' '[a-z]'`
         if [ "$F_IDB" = "$IN_DBNAME" ]; then
            echo "$f" >> ${DBREG_TMP}
         fi
      done
   else
      echo "$IN_DBNAME -- Wrong DB NAME !!!"
      exit
   fi
fi

for i in `cat ${DBREG_TMP}|tr " " ","`; do
    f=`echo $i|tr "," " "`
    PPATH=`echo $f|awk -F" " '{print $2}'| tr '[A-Z]' '[a-z]'`
    PSDB=`echo $f|awk -F" " '{print $1}'| tr '[A-Z]' '[a-z]'`
    $DLC/bin/promon $PPATH/$PSDB < $PROGHOME/bin/picain.txt > /tmp/picaout.$PSDB
    echo "=================== $DAT ====================" >> /tmp/pica.$PSDB
    cat /tmp/picaout.$PSDB | egrep "Total Message Entries|Free Message Entries|Used Message Entries|Used HighWater Mark" >> /tmp/pica.$PSDB
    cat /tmp/picaout.$PSDB | egrep "Total Message Entries|Free Message Entries|Used Message Entries|Used HighWater Mark" > /tmp/pica.tmp.$PSDB
    a=`sed -n '1p' /tmp/pica.tmp.$PSDB | awk -F" " '{print $5}'`
    b=`sed -n '2p' /tmp/pica.tmp.$PSDB | awk -F" " '{print $5}'`
    c=`sed -n '3p' /tmp/pica.tmp.$PSDB | awk -F" " '{print $5}'`
    d=`sed -n '4p' /tmp/pica.tmp.$PSDB | awk -F" " '{print $5}'`

    tail -1500 ${OPDIREC}/pica.$PSDB.zabbix > ${OPDIREC}/pica.$PSDB.zabbix.tmp
    mv ${OPDIREC}/pica.$PSDB.zabbix.tmp ${OPDIREC}/pica.$PSDB.zabbix
    echo "$DAT|$a|$b|$c|$d" >> /emc/reale/apps/pseudo/output/perf_test/pica.$PSDB.zabbix
   # echo "$DAT|$a|$b|$c|$d"
done

