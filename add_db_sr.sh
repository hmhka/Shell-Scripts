#!/usr/bin/ksh
LOGPATH=/pro/admin/gall/prd/db_logs
LOGPATH1=/arcsight
SRVRNAME=`hostname`
now=$(date +"%Y_%m_%d")
ls -ltr $LOGPATH/*.lg > list1.txt
awk '{print $9;}' list1.txt > list2.txt
for i in `cat list2.txt`
do
cat $i|grep -v "^ "|awk 'NF>0' > $i.$now
done
ls -ltr $LOGPATH/*.$now > list3.txt
awk '{print $9;}' list3.txt > list4.txt
file=list4.txt
while IFS= read -r line
do
DBNAME=`basename "$line" | cut -d'.' -f1` 
sed "s/\]/ ] $DBNAME-$SRVRNAME/" $line > $LOGPATH1/$DBNAME.lg.$now
done < "$file"
rm list1.txt
rm list2.txt
rm list3.txt
rm list4.txt
cd $LOGPATH
rm -rf *.$now
