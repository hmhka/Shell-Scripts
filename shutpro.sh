#!/bin/sh
JOB_ID=shutpro
set +x
#=============================================================================#
#                                                                             #
# file    : shutpro.sh                                                        #
# Version : 1.0.0                                                             #
# function: Stop  all progress databases identified in the protab file.       #
# use     : At system shutdown (or lazy DBAs).                                #
#                                                                             #
#-----------------------------------------------------------------------------#
#                                                                             #
# Ver.  Date       Who      What                                              #
# ----  -------    ---      ----                                              #
# 1.0.0 2003-04-22 ZielSoft First release                                     #
#                                                                             #
#=============================================================================#
. /pro/admin/ctl/setenv.pro

echo ${longline}                                                    >>${logdtm}
echo ${longline}                                                    >>${logdtm}
echo "shutpro.sh started at $(date '+%Y-%m-%d %H:%M:%S')"           >>${logdtm}
if [[ -r "${protab}" ]]
then
   echo "processing ${protab}"                                      >>${logdtm}
else
   echo "ERROR: protab ${protab} not found or not readable."        >>${logdtm}
   echo "Abort"                                                     >>${logdtm}
   exit 1
fi
echo ${longline}                                                    >>${logdtm}
echo ${longline}                                                    >>${logdtm}
echo                                                                >>${logdtm}

typeset -u strt

grep -v "^ *#" ${protab} | grep -v "^ *$" |\
while read dbnm vers hdir lgrp strt filler
   do
      if [[ -z "$strt" ]]
      then
         echo ${longline}                                           >>${logdtm}
         echo "ERROR: Invalid protab row, not enough args., ignored."\
                                                                    >>${logdtm}
         echo '   >>:' $dbnm $vers $hdir $lgrp $strt $filler        >>${logdtm}
         continue
      fi

      if [[ -n "$filler" ]]
      then
         echo ${longline}                                           >>${logdtm}
         echo "ERROR: Invalid protab row, to many args., ignored."  >>${logdtm}
         echo '   >>:' $dbnm $vers $hdir $lgrp $strt $filler        >>${logdtm}
         continue
      fi

      dbname=${dbnm%%,*}  
      dbnam=${dbname%%.db}

      echo ${longline}                                              >>${logdtm}

      ${proAdmin}bin/stoppro.sh ${hdir}/${dbnam}                    >>${logdtm}\
                                                                    2>&1
done

echo ${longline}                                                    >>${logdtm}
echo ${longline}                                                    >>${logdtm}
echo "shutpro.sh ended at $(date '+%Y-%m-%d %H:%M:%S')"             >>${logdtm}
echo ${longline}                                                    >>${logdtm}
echo ${longline}                                                    >>${logdtm}
#=============================================================================#
# EOF                                                                         #
#=============================================================================#
