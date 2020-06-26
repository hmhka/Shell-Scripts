#!/usr/bin/ksh

#Make Progress backups of all databases save Multistore,
#which is handled separately (gall220u).

probackup=$(ls -1 /pro/admin/bin/backup.sh | head -1)
startpro=$(ls -1 /pro/admin/bin/startpro.sh | head -1)
rc=0

#Fetch list of all databases on this host, but exclude
#multistore if it is present.
cat /pro/admin/ctl/protab | grep -v multistore |
awk ' ! ( /^ *$/ || /^#/ ) { print $3 "/" $1 }' |
while read db
do
	bkpname=$(echo $db | sed 's!^/pro/admin!/probackup!').cbu
	echo $(date) : Start backup of $db

	#Make cold backup of this database using Zielsoft tool.
	#Three variants, activate only one.

	#Debug variant:
	#echo $probackup $db cold $bkpname || rc=1
	#echo $startpro $db 
	#Running as probeh variant:
	#$probackup $db cold $bkpname || rc=1
	#$startpro $db 
	#Running as root variant:
	su - probeh -c "$probackup $db cold $bkpname" || rc=1
	su - probeh -c "$startpro $db"

	echo $(date) : Backup of $db done.
done

if [ $rc -ne 0 ]
then
	echo At least one error occurred making backups.
fi

exit $rc
