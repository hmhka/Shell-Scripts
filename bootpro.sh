#!/bin/sh
JOB_ID=bootpro
set +x
#=============================================================================#
#                                                                             #
# file    : bootpro.sh                                                        #
# Version : 1.0.0                                                             #
# function: Start all progress databases identified in the protab file.       #
# use     : At system boot.    (or lazy DBAs)                                 #
#                                                                             #
# call    : startpro.sh                                                       #
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
echo "bootpro.sh started at $(date '+%Y-%m-%d %H:%M:%S')"           >>${logdtm}
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
         echo "ERROR: Invalid protab row, not enough args., ignored." \
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

      case "${strt}" in
         NO|N|NEE|NON)
           echo ${longline}                                         >>${logdtm}
           echo "INFO: Not started, start at boot=${strt}."         >>${logdtm}
           echo '  >>:' $dbnm $vers $hdir $lgrp $strt $filler       >>${logdtm}
           continue
         ;;

         YES|Y|JA|J|OUI)
           :
         ;;

         *)
           echo ${longline}                                         >>${logdtm}
           echo "ERROR: Invalid value for 'start' parameter, ignored."\
                                                                    >>${logdtm}
           echo '   >>:' $dbnm $vers $hdir $lgrp $strt $filler      >>${logdtm}
           continue
         ;;
      esac

      doOpt=$(echo ${dbnm} | cut -f2- -d',' -s)

      dbname=${dbnm%%,*}
      dbnam=${dbname%%.db}
      pfile=${dbnam}.pf

      echo ${longline}                                              >>${logdtm}

      [[ -r "${hdir}/${pfile}" ]] || pfile='>>none<<'

      echo "Bringing up database: ${dbnam}.db"                      >>${logdtm}
      echo "                  in: $hdir"                            >>${logdtm}
      echo "         using pfile: $pfile"                           >>${logdtm}
      if [[ -n "${doOpt}" ]]
      then
          echo "   including writers: ${doOpt}"                     >>${logdtm}
      fi

      ${proAdmin}bin/startpro.sh ${hdir}/${dbnam}                   >>${logdtm}\
                                                                       2>&1
done

echo ${longline}                                                    >>${logdtm}
echo ${longline}                                                    >>${logdtm}
echo "bootpro.sh ended at $(date '+%Y-%m-%d %H:%M:%S')"             >>${logdtm}
echo ${longline}                                                    >>${logdtm}
echo ${longline}                                                    >>${logdtm}
#=============================================================================#
# EOF                                                                         #
#=============================================================================#
