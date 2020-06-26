#!/bin/ksh
JOB_ID=restore
set +x
#=============================================================================#
#                                                                             #
# file    : restore.sh                                                        #
# Version : 1.0.1                                                             #
# function: Perform a restore for a given database.                           #
# usage   : restore.sh dbname [[+]destination]                                #
#                                                                             #
#         : arg1, mandatory: dbname                                           #
#         : arg2, optional : +foo will add ".foo" to the default filename     #
#         : arg3, optional : device or file-name(s)                           #
#         : arg4, optional : -i, Passed to startpro                           #
#                                                                             #
# calls   : stoppro.sh                                                        #
#         : startpro.sh                                                       #
#                                                                             #
#-----------------------------------------------------------------------------#
#                                                                             #
# Ver.  Date       Who      What                                              #
# ----  -------    ---      ----                                              #
# 1.0.0 2004-03-25 ZielSoft First release                                     #
# 1.0.1 2004-04-02 ZielSoft Fix, use fullpath to restore database.            #
#                                                                             #
#=============================================================================#
. /pro/admin/ctl/setenv.pro

interpretDbName $1            || exit 1
setProVersion ${_protab_vers} || exit 1
shift

MinI=""
#-----------------------------------------------------------------------------#
# Optional "-i" parameters.                                                   #
#-----------------------------------------------------------------------------#
eliminateParm "-i"    "$@" && MinI="-i";  eval set Filler "${_setList}"; shift

#-----------------------------------------------------------------------------#
# extra suffix for budest                                                     #
#-----------------------------------------------------------------------------#
if [[ -n "$1" && "$(echo $1 | cut -c1)" = "+" ]]
then
   suf=$(echo $1 | sed 's/+/./')
   shift
else
   suf=""
fi

#-----------------------------------------------------------------------------#
# Get backup destination                                                      #
#-----------------------------------------------------------------------------#
if [[ -n "$1" ]]
then
   typeset -i i=0
   isDefaultBuDest=0
   budest0=""; budest1=""; budest2=""; budest3=""; budest4=""
   budest5=""; budest6=""; budest7=""; budest8=""; budest9=""
   while [[ -n "$1" ]]
      do
         eval budest${i}=$1
         i=$((i +1))
         shift
    done
   if (( $i > 9 ))
   then
      echo "To many files, this version supports max. 10 , found $i"
      exit 1
   fi
else
   budest=$(echo $_dbpath | sed "s;${proAdmin};${buBase};")
   isDefaultBuDest=1
fi

#-----------------------------------------------------------------------------#
# Verify default destination                                                  #
#-----------------------------------------------------------------------------#
if [[ "${isDefaultBuDest}" = "1" ]]
then
   if [[ -d "${budest}" ]]
   then
      echo "   dest ${budest}"
   else
      echo "   dest ${budest} not found"
      exit 1
   fi
fi

#-----------------------------------------------------------------------------#
# database up in multi user mode ?                                            #
#-----------------------------------------------------------------------------#
proutil ${_ldbnam} -C busy

if (( $? == 6 ))
then
   dbIsUp=1
else
   dbIsUp=0
fi

#-----------------------------------------------------------------------------#
# if db running, first stop DB                                                #
#-----------------------------------------------------------------------------#
if [[ "${dbIsUp}" = "1" ]]
then
   ${proAdmin}bin/stoppro.sh ${_ldbnam} || exit 1
fi

#-----------------------------------------------------------------------------#
# Determine last backup mode                                                  #
#-----------------------------------------------------------------------------#
if [[ "${isDefaultBuDest}" = "1" ]]
then
   effe=$(ls -ltr ${budest}/${_dbnam}.hbu.st${suf} ${budest}/${_dbnam}.cbu.st${suf} 2>/dev/null |\
   tail -1 | awk ' {print $NF} ')

   if [[ -z "${effe}" ]]
   then
      echo "No valid backup set found in ${budest}"
      exit 1
   else
      buMode="UNSET"
      [[ "${effe}" = "${budest}/${_dbnam}.hbu.st${suf}" ]] && buMode=hbu
      [[ "${effe}" = "${budest}/${_dbnam}.cbu.st${suf}" ]] && buMode=cbu
      if [[ "${buMode}" = "UNSET" ]]
      then
         echo "No valid backup set found in ${budest}"
         exit 1
      fi
      echo "Found backup : ${effe}"
   fi
fi

#-----------------------------------------------------------------------------#
# Perform the restore                                                         #
#-----------------------------------------------------------------------------#
if [[ "${isDefaultBuDest}" = "1" ]]
then
   prorest ${_ldbnam} ${budest}/${_dbnam}.${buMode}0${suf} << EOF
y
                      ${budest}/${_dbnam}.${buMode}1${suf}
                      ${budest}/${_dbnam}.${buMode}2${suf}
                      ${budest}/${_dbnam}.${buMode}3${suf}
                      ${budest}/${_dbnam}.${buMode}4${suf}
                      ${budest}/${_dbnam}.${buMode}5${suf}
                      ${budest}/${_dbnam}.${buMode}6${suf}
                      ${budest}/${_dbnam}.${buMode}7${suf}
                      ${budest}/${_dbnam}.${buMode}8${suf}
                      ${budest}/${_dbnam}.${buMode}9${suf}
EOF
   maxrc=$?
else
   prorest ${_ldbnam} ${budest0} << EOF
y
                      ${budest1}
                      ${budest2}
                      ${budest3}
                      ${budest4}
                      ${budest5}
                      ${budest6}
                      ${budest7}
                      ${budest8}
                      ${budest9}
EOF
   maxrc=$?
fi

(( ${maxrc} > 0 )) && exit 1

#-----------------------------------------------------------------------------#
# if db was running then restart it.                                          #
#-----------------------------------------------------------------------------#
if [[ "${dbIsUp}" = "1" ]]
then
   ${proAdmin}bin/startpro.sh  ${_ldbnam} ${MinI} || exit 1
fi

exit 0
#=============================================================================#
# EOF                                                                         #
#=============================================================================#
