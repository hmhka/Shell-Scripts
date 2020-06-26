#!/bin/ksh
JOB_ID=checkdup
set +x
#=============================================================================#
#                                                                             #
# file    : checkdup.sh                                                       #
# Version : 1.0.0                                                             #
# function: check a single progress database (server for) is running.         #
# usage   : checkdup.sh dbname                                                #
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
interpretPfFile $2 $3         || exit 1
setProVersion ${_protab_vers} || exit 1

#-----------------------------------------------------------------------------#
# database down ?                                                             #
#-----------------------------------------------------------------------------#
type proutil
echo '>>' "proutil ${_ldbnam} -C busy"
proutil ${_ldbnam} -C busy

if (( $? != 6 ))
then
   echo "ERROR: ${_ldbnam} not running in multi user mode"
   exit 1
fi

#=============================================================================#
# EOF                                                                         #
#=============================================================================#
