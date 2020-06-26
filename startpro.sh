#!/bin/sh
JOB_ID=startpro
set +x
#=============================================================================#
#                                                                             #
# file    : startpro.sh                                                       #
# Version : 1.4.0                                                             #
# function: Start a single progress database. (server for)                    #
# usage   : startpro.sh dbname [pfile] ["-i"] ["-s"] ["force"]                # 
#                                                                             #
#-----------------------------------------------------------------------------#
#                                                                             #
# Ver.  Date       Who      What                                              #
# ----  -------    ---      ----                                              #
# 1.0.0 2003-04-22 ZielSoft First release.                                    #
# 1.1.0 2003-05-01 ZielSoft optional parameters -i and force added.           #
# 1.2.0 2005-01-07 ZielSoft apw added.                                        #
# 1.3.0 2005-04-22 ZielSoft -s added.   (switch logfile)                      #
# 1.4.0 2006-04-14 ZielSoft autoextent added.                                 #
#                                                                             #
#=============================================================================#
. /pro/admin/ctl/setenv.pro
MinI=""
MinS=""
MinT=""
bForce="0"

#-----------------------------------------------------------------------------#
# First parameter must be database name.                                      #
#-----------------------------------------------------------------------------#
interpretDbName $1            || exit 1
shift

#-----------------------------------------------------------------------------#
# Optional "-i" and "force" parameters.                                       #
#-----------------------------------------------------------------------------#
eliminateParm "-i"    "$@" && MinI="-i";  eval set Filler "${_setList}"; shift
eliminateParm "-s"    "$@" && MinS="-s";  eval set Filler "${_setList}"; shift
eliminateParm "-t"    "$@" && MinT="-t";  eval set Filler "${_setList}"; shift
eliminateParm "force" "$@" && bForce="1"; eval set Filler "${_setList}"; shift

#-----------------------------------------------------------------------------#
# Optional pfile "-pf" "name"  OR  "-pf name"                                 #
#-----------------------------------------------------------------------------#
interpretPfFile $1 $2         || exit 1

setProVersion ${_protab_vers} || exit 1

#-----------------------------------------------------------------------------#
# If not given, get optional writers from protab                              #
#-----------------------------------------------------------------------------#
: ${_dbwriters:=${_protab_dbwriters}}

#-----------------------------------------------------------------------------#
# database down ?                                                             #
#-----------------------------------------------------------------------------#
echo
type proutil
echo '>>' "proutil ${_ldbnam} -C busy"
proutil ${_ldbnam} -C busy ||
      { 
       if [[ "${bForce}" = "1" ]]
       then
          ${proAdmin}bin/stoppro.sh  ${_ldbnam} || exit 1
       else
          echo "WARNING: ${_ldbnam} not started. (already running?)"
          exit 0
       fi ;}

#-----------------------------------------------------------------------------#
# OK, just a notice                                                           #
#-----------------------------------------------------------------------------#
(( "$_isDefaultPf" == 1 )) && echo "INFO: default .pf file used (${_lpfnam})"

#-----------------------------------------------------------------------------#
# if -t on command line first truncate BI file.                               #
#-----------------------------------------------------------------------------#
if [[ "${MinT}" = "-t" ]]
then
   echo "truncate BI start"
   proutil ${_ldbnam} -C truncate BI     || exit 1
   echo "truncate BI ready"
fi

#-----------------------------------------------------------------------------#
# if -s on command line first switch logfiles.                                #
#-----------------------------------------------------------------------------#
if [[ "${MinS}" = "-s" ]]
then
   echo "Switching logfiles start"
   [[ -r "${_ldbnam}.lg[-6]" ]] && mv ${_ldbnam}.lg[-6] ${_ldbnam}.lg[-7]
   [[ -r "${_ldbnam}.lg[-5]" ]] && mv ${_ldbnam}.lg[-5] ${_ldbnam}.lg[-6]
   [[ -r "${_ldbnam}.lg[-4]" ]] && mv ${_ldbnam}.lg[-4] ${_ldbnam}.lg[-5]
   [[ -r "${_ldbnam}.lg[-3]" ]] && mv ${_ldbnam}.lg[-3] ${_ldbnam}.lg[-4]
   [[ -r "${_ldbnam}.lg[-2]" ]] && mv ${_ldbnam}.lg[-2] ${_ldbnam}.lg[-3]
   [[ -r "${_ldbnam}.lg[-1]" ]] && mv ${_ldbnam}.lg[-1] ${_ldbnam}.lg[-2]
   [[ -r "${_ldbnam}.lg" ]]     && mv ${_ldbnam}.lg     ${_ldbnam}.lg[-1]
   >  ${_ldbnam}.lg
   echo "Switching logfiles ready"
fi

#-----------------------------------------------------------------------------#
# check for autoextent file to process.                                       #
#-----------------------------------------------------------------------------#
if [[ -r "${_ldbnam}.autoextent.st" ]]
then
   echo "prostrct add/list start"
   type prostrct
   echo '>>' "prostrct add  ${_ldbnam} ${_ldbnam}.autoextent.st"
   prostrct add  ${_ldbnam} ${_ldbnam}.autoextent.st || exit 1

   echo '>>' "prostrct list ${_ldbnam} ${_ldbnam}.st"
   prostrct list ${_ldbnam} ${_ldbnam}.st            || exit 1
   echo "prostrct add/list ready"

   mv ${_ldbnam}.autoextent.st ${_ldbnam}.autoextent.done$(date '+%y%m%d%H%M%S')
fi

#-----------------------------------------------------------------------------#
# start the server for this database                                          #
#-----------------------------------------------------------------------------#
type proserve
echo '>>' "proserve ${_ldbnam} ${_pfile} ${MinI}"
proserve ${_ldbnam} ${_pfile} ${MinI} ||
       { echo "ERROR: during proserve, see ${_ldbnam}.lg for details."
         exit 1 ;}

echo "${_ldbnam} started."

#-----------------------------------------------------------------------------#
# optional writers to do ?                                                    #
#-----------------------------------------------------------------------------#
[[ -z "${_dbwriters}" ]] && exit 0

typeset -u writer
typeset -i aaStarted
typeset -i abStarted
#-----------------------------------------------------------------------------#
#  V1.2.0  APW added..                                                        #
# For both the AIW and BIW, progress will exit 0, even if not started         #
# we have to examine the .lg file.                                            #
#     aaStarted == #Starts before                                             #
#     abStarted == #Starts after we started the writer.                       #
#  so: abStarted minus aaStarted MUST be 1.                                   #
#                                                                             #
# The sleeps are only there to avoid the writer and server logging getting    #
# mixed up.                                                                   #
#-----------------------------------------------------------------------------#
for writer in $(echo ${_dbwriters} | sed 's/,/ /g')
 do
    case "${writer}" in
       AI|AIW)
         sleep 3

         aaStarted=$(grep AIW ${_ldbnam}.lg | grep -i Started | wc -l)
          
         echo
         type proaiw
         echo '>>' "proaiw ${_ldbnam}"
         proaiw ${_ldbnam} ||
              { echo "ERROR: during proaiw ${_ldbnam}, AI writer not stared."
                continue ;}

         abStarted=$(grep AIW ${_ldbnam}.lg | grep -i Started | wc -l)

         if (( $(($abStarted - $aaStarted)) != 1 ))
         then
            echo "ERROR: during proaiw ${_ldbnam}, AI writer not stared."
         else
            echo "AI writer started."
         fi

         continue
       ;;

       BI|BIW)
         sleep 3

         aaStarted=$(grep BIW ${_ldbnam}.lg | grep -i Started | wc -l)

         echo
         type probiw
         echo '>>' "probiw ${_ldbnam}"
         probiw ${_ldbnam} ||
              { echo "ERROR: during probiw ${_ldbnam}, BI writer not stared."
                continue ;}

         abStarted=$(grep BIW ${_ldbnam}.lg | grep -i Started | wc -l)

         if (( $(($abStarted - $aaStarted)) != 1 ))
         then
            echo "ERROR: during probiw ${_ldbnam}, BI writer not stared."
         else
            echo "BI writer started."
         fi

         continue
       ;;

       AP|APW)
         sleep 3
         aaStarted=$(grep APW ${_ldbnam}.lg | grep -i Started | wc -l)

         echo
         type proapw
         echo '>>' "proapw ${_ldbnam}"

         proapw ${_ldbnam} ||
              { echo "ERROR: during proapw ${_ldbnam}, AP writer not stared."
                continue ;}

         abStarted=$(grep APW ${_ldbnam}.lg | grep -i Started | wc -l)

         if (( $(($abStarted - $aaStarted)) != 1 ))
         then
            echo "ERROR: during proapw ${_ldbnam}, AP writer not stared."
         else
            echo "AP writer started."
         fi

         continue
       ;;

       *)
         echo
         echo "WARNING: Invalid writer specified (${writer}), ignored."
         continue
       ;;
    esac
done
#-----------------------------------------------------------------------------#
# EOF ------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
