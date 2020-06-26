#!/bin/sh
JOB_ID=checkpro
set +x
#=============================================================================#
#                                                                             #
# file    : checkpro.sh                                                       #
# Version : 1.0.0                                                             #
# function: Perform some tests on the progress databases identified in protab #
# use     : once a day at 07:00                                               #
#                                                                             #
# call    : checkdup : Test servers are running                               #
#         : checkext : Test extent size of the variable extents               #
#                                                                             #
#-----------------------------------------------------------------------------#
#                                                                             #
# Ver.  Date       Who           What                                         #
# ----  -------    ---           ----                                         #
# 1.0.0 2003-04-22 ZielSoft      First release                                #
# 1.0.1 2006-08-17 R Visser      Changed mailaddress                          #
# 1.0.2 2008-07-23 Bart de Vries Changed mail command and adress              #
#=============================================================================#
. /pro/admin/ctl/setenv.pro

echo ${longline}                                                      >${logda}
echo ${longline}                                                     >>${logda}
echo "chkdbup.sh started at $(date '+%Y-%m-%d %H:%M:%S')"            >>${logda}
if [[ -r "${protab}" ]]                                                         
then                                                                            
   echo "processing ${protab}"                                       >>${logda} 
else                                                                            
   echo "ERROR: protab ${protab} not found or not readable."         >>${logda} 
   echo "Abort"                                                      >>${logda} 
   exit 1                                                                       
fi                                                                              
echo ${longline}                                                     >>${logda}
echo ${longline}                                                     >>${logda}
echo                                                                 >>${logda}

typeset -u strt

grep -v "^ *#" ${protab} | grep -v "^ *$" |\
while read dbnm vers hdir lgrp strt filler
   do
      if [[ -z "$strt" ]]
      then
         echo ${longline}                                            >>${logda}
         echo "ERROR: Invalid protab row, not enough args., ignored.">>${logda}
         echo '   >>:' $dbnm $vers $hdir $lgrp $strt $filler         >>${logda}
         continue
      fi

      if [[ -n "$filler" ]]
      then
         echo ${longline}                                            >>${logda}
         echo "ERROR: Invalid protab row, to many args., ignored."   >>${logda}
         echo '   >>:' $dbnm $vers $hdir $lgrp $strt $filler         >>${logda}
         continue
      fi

      case "${strt}" in
         NO|N|NEE|NON)
           run=n
         ;;

         YES|Y|JA|J|OUI)
           run=y
         ;;

         *)
           echo ${longline}                                          >>${logda}
           echo "ERROR: Invalid value for 'start' parameter, ignored."\
                                                                     >>${logda}
           echo '   >>:' $dbnm $vers $hdir $lgrp $strt $filler       >>${logda}
           continue
         ;;
      esac

      dbname=${dbnm%%,*}   
      dbnam=${dbname%%.db} 

      interpretDbName ${hdir}/${dbnam}                            >>${logda} ||
              { echo '   >>:' $dbnm $vers $hdir $lgrp $strt $filler  >>${logda}
                continue ;}
      setProVersion ${vers}                                       >>${logda} ||
              { echo '   >>:' $dbnm $vers $hdir $lgrp $strt $filler  >>${logda}
                continue ;}

      echo ${longline}                                               >>${logda}

      if [[ "${run}" = "y" ]]
      then
         ${proAdmin}bin/checkdup.sh ${hdir}/${dbnam}            >>${logda} 2>&1
      else
         echo "INFO: Status not checked, start at boot=${strt}."     >>${logda}
         echo '  >>:' $dbnm $vers $hdir $lgrp $strt $filler          >>${logda}
      fi
      ${proAdmin}bin/checkext.sh ${hdir}/${dbnam}               >>${logda} 2>&1

done

if (( ${maxrc} == 0 ))
then
   echo ${longline}                                                  >>${logda}
   echo                                                              >>${logda}
fi
echo ${longline}                                                     >>${logda}
echo ${longline}                                                     >>${logda}
echo "chkdbup.sh ended at $(date '+%Y-%m-%d %H:%M:%S')"              >>${logda}
echo ${longline}                                                     >>${logda}
echo ${longline}                                                     >>${logda}

if (( $(grep "^ERROR" ${logda} | wc -l) > 0 ))
then
   grep "^ERROR" ${logda}  | mailx -r ehmaholddbamailbox@hpe.com -s "Progress  
 on $(uname -n) has errors." ehmaholddbamailbox@hpe.com
#verwijderd dd 20060411 op verzoek van Koen.Beuselinck@realsoftwaregroup.com
#   grep "^ERROR" ${logda}  | mailx -s "Progress on $(uname -n) has errors." \
#   unix.administrators@sycron-it.com
fi

#=============================================================================#
# EOF                                                                         #
#=============================================================================#
