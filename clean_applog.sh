######### STILL UNDER CONSTRUCTION                                         ############
######### meant for maintenance of the multistore-logfiles in /pro/workdir ############
######### this script should only run if the dbase is not in use, it will  ############
######### be scheduled as part of the cold-backup procedure                ############
######### Bart de Vries  11-09-2008                                        ############

mv /pro/workdir/app-mstore.broker.log /pro/workdir/app-mstore.broker.log.done$(date '+%y%m%d%H%M%S')
mv /pro/workdir/app-mstore.server.log /pro/workdir/app-mstore.server.log.done$(date '+%y%m%d%H%M%S')

find /pro/workdir -type f -mtime +14 |\
grep .log.done | while read file
do
  script_echodo rm -f $file
done
