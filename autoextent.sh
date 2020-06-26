#!/bin/ksh
JOB_ID=autoextent
set +x
#=============================================================================#
#                                                                             #
# file    : autoextent.sh                                                     #
# function: add an extent to a database                                       #
# calls   : prostrct list                                                     #
#         : stoppro.sh                                                        #
#         : truncate dbname -C busy                                           #
#         : prostrct add                                                      #
#         : startpro.sh                                                       #
#         : prostrct list                                                     #
#         : backup.sh (optional)                                              #
#                                                                             #
#-----------------------------------------------------------------------------#
. /pro/admin/ctl/setenv.pro

#-----------------------------------------------------------------------------#
#  update structure description file                                          #
#-----------------------------------------------------------------------------#
/pro/product/91D/bin/prostrct list /pro/admin/gall/prd/cosmos/pmutatie /pro/admin/gall/prd/cosmos/pmutatie.st

#-----------------------------------------------------------------------------#
#  stop db                                                                    #
#-----------------------------------------------------------------------------#
/pro/admin/bin/stoppro.sh /pro/admin/gall/prd/cosmos/pmutatie

#-----------------------------------------------------------------------------#
#  truncate bi file                                                           #
#-----------------------------------------------------------------------------#
/pro/product/91D/bin/proutil /pro/admin/gall/prd/cosmos/pmutatie -C truncate bi

#-----------------------------------------------------------------------------#
#  add extent                                                                 #
#-----------------------------------------------------------------------------#
/pro/product/91D/bin/prostrct add /pro/admin/gall/prd/cosmos/pmutatie /pro/admin/gall/prd/cosmos/add.st

#-----------------------------------------------------------------------------#
#  start db                                                                   #
#-----------------------------------------------------------------------------#
/pro/admin/bin/startpro.sh /pro/admin/gall/prd/cosmos/pmutatie

#-----------------------------------------------------------------------------#
#  update structure description file                                          #
#-----------------------------------------------------------------------------#
/pro/product/91D/bin/prostrct list /pro/admin/gall/prd/cosmos/pmutatie /pro/admin/gall/prd/cosmos/pmutatie.st

#-----------------------------------------------------------------------------#
# Perform backup                                                              #
#-----------------------------------------------------------------------------#
#/pro/admin/bin/backup.sh /pro/admin/gall/prd/cosmos/pmutatie cold -s -fok

#=============================================================================#
# EOF                                                                         #
#=============================================================================#
