#!/usr/bin/ksh
###############################################################################
# Bellow will add the date to each line of the DB.lg 		              #
# Author: Sreedev M                                  		              #
# Modification Log:                                  		              #
#     First release:- 20/Jan/2015
#                                                                             #
###############################################################################

if [ $# != 2 ]
then
	echo "Shuld run in format $0 <LogFile> <OutputFile>"
	exit
fi

if [ "$1" = "$2" ]
then
        echo "Input and output file should hav different name"
        exit
fi
if [ -f $1 ]
then
	SRVRNAME=`hostname`
	LOGNME=`echo "$1"|awk -F"/" '{print $NF}'`
	DBNAME="${LOGNME%.lg}"
        #echo "SRVRNAME=$SRVRNAME DBNAME=$DBNAME"
	log_fl=$1
	op_fl=$2
	cat /dev/null>$op_fl
        cat $log_fl|sed 's///g'>/tmp/logfl.txt
	awk 'NF>0' /tmp/logfl.txt> /tmp/logfl.txt1
	cat /tmp/logfl.txt1|grep -v "^ ">/tmp/logfl.txt
	cat /tmp/logfl.txt|sed "s,] P-,] ${SRVRNAME}-${DBNAME} P-,g">$op_fl
	rm /tmp/logfl.txt /tmp/logfl.txt1
else
	echo "Log File $1 is missing."
fi

