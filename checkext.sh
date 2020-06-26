#!/bin/ksh
JOB_ID=checkext
set +x
#=============================================================================#
#                                                                             #
# file    : checkext.sh                                                       #
# Version : 1.0.0                                                             #
# function: check the size of the variable extents (< $extentMaxSize)         #
# usage   : checkext.sh  dbname                                               #
#                                                                             #
#-----------------------------------------------------------------------------#
#                                                                             #
# Ver.  Date       Who      What                                              #
# ----  -------    ---      ----                                              #
# 1.0.0 2003-04-22 ZielSoft First release                                     #
#                                                                             #
#=============================================================================#
. /pro/admin/ctl/setenv.pro

interpretDbName $1            || exit 1
setProVersion ${_protab_vers} || exit 1

#typeset -i extentMaxSize=$((1024 * 1024 * 1843))     # 1,8 GB
typeset -i extentMaxSize=$((1024 * 1024 * 1536))     # 1,5 GB
#typeset -i extentMaxSize=$((1024 * 1024 * 1024))     # 1GB

#-----------------------------------------------------------------------------#
# create a .st file                                                           #
#-----------------------------------------------------------------------------#
prostrct list ${_ldbnam} ${_ldbnam}.st >/dev/null
maxrc=$?

#-----------------------------------------------------------------------------#
# Multi volume DB                                                             #
#-----------------------------------------------------------------------------#
if (( ${maxrc} == 0 ))
then
for file in $(grep "^d " ${_ldbnam}.st | grep -v " f " | awk ' {print $NF} ')
   do
      cd ${_dbpath}
      cd $(dirname $file)
      fut=$(pwd)/$(basename $file)
      fs=$(ls -ltr ${fut} | awk ' {print $5} ')

      if (( ${fs} > $extentMaxSize ))
      then
         echo "ERROR: MaxSize ($extentMaxSize) reached for ${fut} (${fs})"
     fi
 done
#-----------------------------------------------------------------------------#
# Single volume DB                                                            #
#-----------------------------------------------------------------------------#
elif (( ${maxrc} == 2 ))
then
     fs=$(ls -ltr ${_ldbname} | awk ' {print $5} ')

     if (( ${fs} > $extentMaxSize ))
     then
        echo "ERROR: MaxSize ($extentMaxSize) reached for ${_ldbname} (${fs})"
     fi
else
     exit 1
fi

#-----------------------------------------------------------------------------#
# EOF ------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
