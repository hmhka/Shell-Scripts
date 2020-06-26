#!/bin/sh
JOB_ID=backup
#set +x
. /pro/admin/ctl/setenv.pro


#-----------------------------------------------------------------------------#
# Get backup mode (hot or cold)                                               #
#-----------------------------------------------------------------------------#
if [[ -z "$1" ]]
then
   echo "ERROR, backup mode, hot (online) or cold, missing."
   exit 1
else
   typeset -l bumode=$1
   case $bumode in
        -h|-on|h|hot|onl|online)
             bumode=online
             bum=online
             suf1=hbu
        ;;
        -c|-off|c|cold|offl|offline)
             bumode=offline
             unset bum
             suf1=cbu
        ;;
        *)
             echo "ERROR: Invalid backup mode $2"
             exit 1
        ;;
    esac
fi
shift

#-----------------------------------------------------------------------------#
# -i must cold backup                                                         #
#-----------------------------------------------------------------------------#
if [[ "${bumode}" = "online" && "${MinI}" = "-i" ]]
then
   echo "ERROR: Conflicting parameters -i and backup mode online"
   exit 1
fi

#-----------------------------------------------------------------------------#
# -s must cold backup                                                         #
#-----------------------------------------------------------------------------#
if [[ "${bumode}" = "online" && "${MinS}" = "-s" ]]
then
   echo "ERROR: Conflicting parameters -s and backup mode online"
   exit 1
fi

#-----------------------------------------------------------------------------#
# Get backup mode (full or incremental)                                       #
#-----------------------------------------------------------------------------#
if [[ -n "$1" ]]
then
   typeset -l incrmode=$1
   case $incrmode in
        incr|incremental)
             incrmode=incremental
             suf2=incr
             shift
        ;;
        *)
             unset incrmode
             unset suf2
        ;;
   esac
fi

#-----------------------------------------------------------------------------#
# extra suffix for budest                                                     #
#-----------------------------------------------------------------------------#
if [[ -n "$1" && "$(echo $1 | cut -c1)" = "+" ]]
then
   suf3=$(echo $1 | sed 's/+/./')
   shift
else
   suf3=""
fi

#-----------------------------------------------------------------------------#
# Get backup destination                                                      #
#-----------------------------------------------------------------------------#
if [[ -n "$1" && "$(echo $1 | cut -c1)" != "-" ]]
then
   budest=$1
   isDefaultBuDest=0
   shift
else
   budest=$(echo $_dbpath | sed "s;${proAdmin};${buBase};")
   isDefaultBuDest=1
fi

#-----------------------------------------------------------------------------#
# Get extra options                                                           #
#-----------------------------------------------------------------------------#
buOptions=$@

echo "Perform ${bumode} ${incrmode:-full} backup on ${_ldbnam}"
echo "   with extra options: ${buOptions:->>none<<}"

#-----------------------------------------------------------------------------#
# Verify default destination                                                  #
#-----------------------------------------------------------------------------#
if [[ "${isDefaultBuDest}" = "1" ]]
then
   if [[ -d "${budest}" ]]
   then
      echo "   dest ${budest}"
   else
      echo "   dest ${budest} (will be created)"
      mkdir -p ${budest} || exit 1
   fi
else
   echo "   dest ${budest}"
fi

#-----------------------------------------------------------------------------#
# database up in muti user mode ?                                             #
#-----------------------------------------------------------------------------#
proutil ${_ldbnam} -C busy

if (( $? == 6 ))
then
   dbIsUp=1
else
   dbIsUp=0
fi

#-----------------------------------------------------------------------------#
# if online backup, DB must be up, otherwise force offline.                   #
#-----------------------------------------------------------------------------#
if [[ "${bumode}" = "online" && "${dbIsUp}" = "0" ]]
then
   echo "WARNING: Online backup requested, but ${_ldbnam} not running,"
   echo "       : changed to offline backup."
   bumode=offline
   unset bum
   suf1=cbu
fi

#-----------------------------------------------------------------------------#
# if offline backup, first stop DB an truncate BI file.                       #
#-----------------------------------------------------------------------------#
if [[ "${bumode}" = "offline" && "${dbIsUp}" = "1" ]]
then
   ${proAdmin}bin/stoppro.sh  ${_ldbnam} || exit 1
   echo "truncate BI start"
   proutil ${_ldbnam} -C truncate BI     || exit 1
   echo "truncate BI ready"
fi

#-----------------------------------------------------------------------------#
# Perform the backup                                                          #
#-----------------------------------------------------------------------------#
buTarget=${budest}/${_dbnam}.${suf1}${suf2}
buTargetSt=${budest}/${_dbnam}.${suf1}${suf2}.st${suf3}

if [[ "${isDefaultBuDest}" = "1" ]]
then
   buCmd="${bum} ${_ldbnam} ${incrmode}"
   probkup ${buCmd} ${buTarget}0${suf3} -com ${buOptions} <<EOF
                    ${buTarget}1${suf3}
                    ${buTarget}2${suf3}
                    ${buTarget}3${suf3}
                    ${buTarget}4${suf3}
                    ${buTarget}5${suf3}
                    ${buTarget}6${suf3}
                    ${buTarget}7${suf3}
                    ${buTarget}8${suf3}
                    ${buTarget}9${suf3}
EOF
   mutrc=$?
   prostrct list ${_ldbnam} ${buTargetSt} >/dev/null
else
   probkup ${bum} ${_ldbnam} ${incrmode} ${budest}    -com ${buOptions}
   mutrc=$?
   prostrct list ${_ldbnam} ${budest}.st  >/dev/null
fi


if (( ${mutrc} != 0 )) 
then
   echo "probackup ended with errors, rc=${mutrc}"
   if [[ "${bFok}" = "1" ]]
   then
      ${proAdmin}bin/startpro.sh  ${_ldbnam} ${MinI} ${MinS} || exit 1
      exit 0
   fi
   exit 1
fi

#-----------------------------------------------------------------------------#
# if offline backup and db was running then restart it.                       #
#-----------------------------------------------------------------------------#
if [[ "${bumode}" = "offline" && "${dbIsUp}" = "1" ]]
then
   ${proAdmin}bin/startpro.sh  ${_ldbnam} ${MinI} ${MinS} || exit 1
fi

exit 0
#=============================================================================#
# EOF                                                                         #
#=============================================================================#
