#!/bin/sh
JOB_ID=stoppro
set +x
#=============================================================================#
#                                                                             #
# file    : stoppro.sh                                                        #
# Version : 1.0.0                                                             #
# function: Stop  a single progress database.                                 #
# usage   : stoppro.sh  dbname                                                #
#                                                                             #
#-----------------------------------------------------------------------------#
#                                                                             #
# Ver.  Date       Who      What                                              #
# ----  -------    ---      ----                                              #
# 1.0.0 2003-04-22 ZielSoft First release                                     #
#                                                                             #
#=============================================================================#
. /pro/admin/ctl/setenv.pro
MinT=""

interpretDbName $1            || exit 1

#-----------------------------------------------------------------------------#
# Optional "-i" and "force" parameters.                                       #
#-----------------------------------------------------------------------------#
eliminateParm "-t"    "$@" && MinT="-t";  eval set Filler "${_setList}"; shift 

interpretPfFile $2 $3         || exit 1
setProVersion ${_protab_vers} || exit 1

#-----------------------------------------------------------------------------#
# Is database running?                                                        #
#-----------------------------------------------------------------------------#
type proutil
echo '>>' "proutil ${_ldbnam} -C busy"
proutil ${_ldbnam} -C busy &&
      { echo "INFO: ${_ldbnam} not stopped. (already down)"
        exit 0 ;}

#-----------------------------------------------------------------------------#
# Stop database                                                               #
#-----------------------------------------------------------------------------#
echo
type proshut
echo '>>' "proshut ${_ldbnam} -by"
proshut ${_ldbnam} -by ||
      { echo "ERROR: during proshut, see ${_ldbnam}.lg for details"
        exit 1 ;}

#-----------------------------------------------------------------------------#
# if -t on command line first truncate BI file.                               #
#-----------------------------------------------------------------------------#
if [[ "${MinT}" = "-t" ]]
then
   echo "truncate BI start"
   proutil ${_ldbnam} -C truncate BI     || exit 1
   echo "truncate BI ready"
fi

exit 0
#=============================================================================#
# EOF                                                                         #
#=============================================================================#
