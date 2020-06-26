#!/usr/bin/ksh
###############################################################################
# Bellow will add the date to each line of the DB.lg 		              #
# Author: Sreedev M                                  		              #
# Modification Log:                                  		              #
# Changed the date format to mm/dd/yy on 31-Mar-2014 -- Sreedev M             #
# Remove the date fied in between logs on 01-Apr-2014 -- Sreedev M            #
# Changed the format of the o/p on request of Ian on 12-Aug-2014 -- Sreedev M #
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
	date_val=0
	a=0
	b=0
	nodate_tm=0
        cat $log_fl|sed 's///g'>/avg/bin/logfl.txt
	awk 'NF>0' /avg/bin/logfl.txt> /avg/bin/logfl.txt1
	mv /avg/bin/logfl.txt1 /avg/bin/logfl.txt
	
	cat /avg/bin/logfl.txt|grep "^ "|awk -F" " '{print $1$2$3$4$5}'>/avg/bin/lgdt.txt
	exec</avg/bin/logfl.txt

	while read line; do
		date_ln=`echo $line|awk -F" " '{print $1$2$3$4$5}'`
		flg=`grep -iw $date_ln /avg/bin/lgdt.txt`
		if [ -z "$flg" ]
		then
				if [ "$a" = "1" -a "$b" = "0" ]
				then
					
					dd1=`echo $dd`
					mm1=`echo $mm`
					yy1=`echo $yy`
					#echo "dd1=$dd1 mm1=$mm1 yy1=$yy1"
					fstdate_tm=`echo $line|awk -F" " '{print $1}'|awk -F":" '{print$1}'`
					#echo "nodate_tm=$nodate_tm fstdate_tm=$fstdate_tm"
					if [ $nodate_tm -gt $fstdate_tm ]
					then

						dd1=`expr "$dd1" - 1` 
						case "$dd1" in 
        					0) 
           						mm1=`expr "$mm1" - 1` 
			                		case "$mm1" in 
                        				0) 
                           					mm1=12 
                           					yy1=`expr "$yy1" - 1` 
                        					;; 
                					esac 
        						dd1=`cal $mm1 $yy1 | grep . | fmt -1 | tail -1` 
							esac 
						#echo "Yesterday was: $dd1 $mm1 $yy1"
					fi
					b=`expr $b + 1`
					#add_dt1=`echo "$mm1/$dd1/$yy1"
 					add_dt1=`echo "$yy1/$mm1/$dd1"`
				        #echo "add_dt1=$add_dt1"
				fi
			if [ -z "$add_dt" ]
                        then
				nodate_tm=`echo $line|awk -F" " '{print $1}'|awk -F":" '{print$1}'`	
				TM_FLD=`echo $line|awk -F" " '{print $1}'`
                		LINE=`echo $line|sed "s,^$TM_FLD,${TM_FLD}+0200]${DBNAME}-${SRVRNAME},g"`
				echo "$LINE">>/avg/bin/nodate.txt
				#nodate_tm=`echo $line|awk -F" " '{print $1}'|awk -F":" '{print$1}'`	
			else
				TM_FLD=`echo $line|awk -F" " '{print $1}'`
                                LINE=`echo $line|sed "s,^$TM_FLD,${TM_FLD}+0200]${DBNAME}-${SRVRNAME},g"`
				echo "[$add_dt@$LINE">>$op_fl
			fi
		else
			a=`expr $a + 1`
			date_val=`echo $line`
			yy=`echo $date_val|awk -F" " '{print $5}'`
			dd=`echo $date_val|awk -F" " '{print $3}'`
			mm1=`echo $date_val|awk -F" " '{print $2}'|tr -s  '[:upper:]'  '[:lower:]'`
			if [ "$mm1" = "jan" ]
			then
				mm=01
			elif [ "$mm1" = "feb" ]
			then
				mm=02
			elif [ "$mm1" = "mar" ]
                	then
                	      	mm=03
			elif [ "$mm1" = "apr" ]
                	then
                	        mm=04
			elif [ "$mm1" = "may" ]
                	then
                	        mm=05
			elif [ "$mm1" = "jun" ]
                	then
                	        mm=06
			elif [ "$mm1" = "jul" ]
                	then
                	        mm=07
			elif [ "$mm1" = "aug" ]
                	then
                	        mm=08
			elif [ "$mm1" = "sep" ]
                	then
                	        mm=09
			elif [ "$mm1" = "oct" ]
                	then
                	        mm=10
			elif [ "$mm1" = "nov" ]
                	then
                	        mm=11
			elif [ "$mm1" = "dec" ]
                	then
                	        mm=12
			fi
			add_dt=`echo "$yy/$mm/$dd"`
			#echo "">>$op_fl
			#echo "                $line">>$op_fl

		fi
	done
	if [ -f /avg/bin/nodate.txt ]
	then	
		#echo "now ${add_dt1}"
		#cat /avg/bin/nodate.txt |sed "s/^/${add_dt1} /g">/avg/bin/del.txt
		sed "s,^,[${add_dt1}@,g" /avg/bin/nodate.txt>/avg/bin/del.txt
		cat $op_fl>>/avg/bin/del.txt
		mv /avg/bin/del.txt $op_fl
		rm /avg/bin/nodate.txt
	fi
	rm /avg/bin/logfl.txt /avg/bin/lgdt.txt
else
	echo "Log File $1 is missing."
fi

