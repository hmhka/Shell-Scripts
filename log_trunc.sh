##############################################################
# Script Name: /avg/bin/controlm/log_trunc.sh
# Purpose: This script takes the backup and then Truncates the Orion DB logs to /probackup/avg/logs on MINERVA server for arcsight project
# version: 1.0
#
#Modification Log:
#    20-jul-2016:Sreedev: Changed the bkup_logs and db_logs path from /pro/admin/gall/prd to /probackup/gall/prd/cosmos
##############################################################
#DBPATH=/pro/admin/gall/prd/lis
now=$(date +"%Y_%m_%d")
prev=$(TZ=GMT+24 date +%Y:%m:%d)
scriptlog=/pro/admin/bin/Orion_Truncation.log
bkplog=/probackup/gall/prd/cosmos/bkup_logs
#ls -ltr $DBPATH/*.db > dblist.txt
rm $scriptlog
#awk '{print $9;}' dblist.txt | cut -d'.' -f1 > list.txt
cat /pro/admin/ctl/protab|grep -v "#"|awk 'NF>0'|awk -F" " '{print $3"/"$1}'|awk -F"," '{print $1}'|egrep -w "/pro/admin/gall/prd/pakket/ppakket|/pro/admin/gall/prd/cosmos/pbatchbh|/pro/admin/gall/prd/cosmos/pcosmos|/pro/admin/gall/prd/cosmos/pklantkaart|/pro/admin/gall/prd/cosmos/pmutatie|/pro/admin/gall/prd/cosmos/ppakketbh|/pro/admin/gall/prd/cosmos/ppromotie|/pro/admin/gall/prd/cosmos/pstatistiek|/pro/admin/gall/prd/lisa/plisa">list.txt
echo "=====================================================" > $scriptlog
echo "Starting Copy and Truncation of Orion DB logs for arcsight at `date`" >> $scriptlog
file=list.txt
while IFS= read -r line
do
cp $line.lg /probackup/gall/prd/cosmos/bkup_logs
cp $line.lg /probackup/gall/prd/cosmos/db_logs
prolog $line -online >> $scriptlog
echo "Log File for $line.db has been truncated successfully at `date`" >> $scriptlog
done < "$file"
echo "Completed Copy and Truncation of Orion DB logs for arcsight at `date`" >> $scriptlog
echo "=====================================================" >> $scriptlog
ls -ltr $bkplog/*.lg > loglist.txt
awk '{print $9;}' loglist.txt > list1.txt
file1=list1.txt
while IFS= read -r line
do
mv $line $line.$now
done < "$file1"
rm loglist.txt
rm list1.txt
#rm dblist.txt
rm list.txt

