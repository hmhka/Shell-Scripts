#!/bin/ksh
# Last modified 12/10/2013
# This script can be used to gather database information in  
# the case of a hang / problem situation or performance 
# analysis data gathering.
# Two parameters can be given to the script on command line 
# but the script defaults to situation 1.
# The first parameter is mandatory, the full path to the 
# database with the database name.
# The full path to the database and the database name must 
# be included for the script to run successfully because 
# the script performs a change directory. 
# If the script is to be used for performance data gathering
# then the word perf must be used as the second parameter 
# on the command line.
# Example of the script command line when gathering data 
# in a hang or problem situation: 
# scriptname.sh <dbname>

# Example of the script command line when gather data for 
# performance analysis: 
# /<fullpathtoscript>/<scriptname>.sh /<fullpath-to-db>/dbname perf

# To run as a background task using nohup (so loss of 
# connectivity if run remotely will not terminate job) use 
# the following command line:

# (Example when running without the perf option)
# nohup /<fullpathtoscript>/mygather.sh /<fullpathtodb>/dbname 2>/dev/null &

#
#
#   Removinging java from hang monitoring due to problems on several Unix
#   platforms where the java process dies as a result of the kill -s USR1
#
#

me=`whoami`
if [ $me != "root" ]
 then
  { 
   echo "You are not logged in as root."
   echo "Root permissions are needed to run "
   echo "some of the utilities in this script."
   exit 
  }
fi

if [ -z "$DLC" ] 
 then   
  {
   echo "DLC is not set script can not procede."
   exit  
  }
fi 


if [ -z "$1" ] 
 then   
  {
   echo "You must supply the absolute path to the database and database name."
   exit   
  }
fi

proutil $1 -C busy
if [ $? -eq 0 ] 
 then
  {
   echo "The database must be running for promon to connect."
   echo "Please serve the database and try again."
   exit
  }
fi


DATETIME=`date +%m%d%Y-%H%M%S` 
sleep 5
`mkdir $DATETIME`
HOST=`hostname`


if [ "$sleep_interval" ]
 then
  sleep_interval=${sleep_interval-1}  
      # sleep_interval = number of seconds to sleep between each gather run
 else
  sleep_interval=3
fi

if [ "$repeats" ]
 then
  repeats=${repeats-3}  # number of times to gather specific information
 else
  repeats=3         
fi

if [ "$os_repeats" ]
 then
  os_repeats=${os_repeats-60}  # number of times to gather specific information
 else
  os_repeats=120         
fi

if [ "$servers" ]
 then
  servers=${servers-200}   # number of servers 
 else
  servers=200
fi

if [ "$monitorsample" ]
 then
  monitorsample=${monitorsample-10}   # length of sample time
 else
  monitorsample=10
fi

if [ -d "${DATETIME}" ]
 then  
  {
   cd "$DATETIME"  
  }
 else  
  {
   echo "Unable to create temporary directory $DATETIME to store files."  
  }
fi

#If any probuild executables are made then they should also be searched 
#for and added to the pslist variable.
PROSRV=${PROSRV-_mprosrv}
PROEXE=${PROEXE-_progres}
PROSHUT=${PROSHUT-_mprshut}
PROAPSV=${PROAPSV-_proapsv}
PROUTIL=${PROUTIL-_proutil}
MPROAPSV=_mproapsv
pslist=`ps -ef|grep -v grep|grep -i $PROEXE|awk '{print $2}'`
pslist="$pslist `ps -ef|grep -v grep|grep -i $PROSRV|awk '{print $2}'`"
pslist="$pslist `ps -ef|grep -v grep|grep -i $PROSHUT|awk '{print $2}'`"
pslist="$pslist `ps -ef|grep -v grep|grep -i $PROUTIL|awk '{print $2}'`"
pslist="$pslist `ps -ef|grep -v grep|grep -i $PROAPSV|awk '{print $2}'`"
pslist="$pslist `ps -ef|grep -v grep|grep -i _rfutil|awk '{print $2}'`"
pslist="$pslist `ps -ef|grep -v grep|grep -i _dbutil|awk '{print $2}'`"
pslist="$pslist `ps -ef|grep -v grep|grep -i _sqlsrv2|awk '{print $2}'`"
pslist="$pslist `ps -ef|grep -v grep|grep -i _mproapsv|awk '{print $2}'`"
#pslist="$pslist `ps -ef|grep -v grep|grep -i java|awk '{print $2}'`"

ostype=`uname`

if [ "$MYDEBUG" = "true" -o -n "$MYDEBUG" ]
 then 
  echo $ostype
fi

echo "--------------------------------------------------------" >> gather.out
echo $HOST >> gather.out
echo "--------------------------------------------------------" >> gather.out
echo "--------------------------------------------------------" >> gather.out
echo "database: ${1} " >> gather.out
echo "sleep interval ${sleep_interval} " >> gather.out
echo "number of repeats ${repeats} " >> gather.out
echo "monitor sample length ${monitorsample}" >> gather.out
echo "m" >>gatherin.txt #Modify Defaults
echo "1" >>gatherin.txt #Change Page Size 
echo "9999" >>gatherin.txt #Set Page Size to 9999
echo "3" >>gatherin.txt #Short Pause after each page
echo "1" >>gatherin.txt #Pause duration after each page
echo "4" >>gatherin.txt #Pause after last page
echo "1" >>gatherin.txt #Pause after last page
echo "q" >>gatherin.txt #quit to main menu
echo "4" >>gatherin.txt #Record Locking Table
echo "1" >>gatherin.txt #Display all entries
echo "q" >>gatherin.txt #quit to main menu
echo "4" >>gatherin.txt #Record Locking Table
echo "1" >>gatherin.txt #Display all entries
echo "q" >>gatherin.txt #quit to main menu
echo "4" >>gatherin.txt #Record Locking Table
echo "1" >>gatherin.txt #Display all entries
echo "q" >>gatherin.txt #quit to main menu
echo "6" >>gatherin.txt #Shared Resources
echo "q" >>gatherin.txt #quit to main menu
echo "7" >>gatherin.txt #Database Status
echo "q" >>gatherin.txt #quit to main menu
echo "R&D" >>gatherin.txt             #Enter R&D Menu
echo 5 >>gatherin.txt                     #Adjust Monitor Options
echo 1 >>gatherin.txt                     #Display page length
echo 9999 >>gatherin.txt               #set value of page length
echo 3 >>gatherin.txt                     #Monitor sampling interval
echo ${monitorsample} >>gatherin.txt                     #set value of sampling interval
echo 4 >>gatherin.txt                     #Pause between displays
echo ${monitorsample} >>gatherin.txt                     #set value of display pause
echo 5 >>gatherin.txt                     #Pause between screens
echo 1 >>gatherin.txt                     #set value of screen pause
echo 6 >>gatherin.txt                      #Number of auto repeats
echo 3 >>gatherin.txt                      #set value of auto repeats
echo t >>gatherin.txt                      #go to top menu
echo 1 >>gatherin.txt                      #Status Displays
echo 4 >>gatherin.txt                      #Processes/Clients
echo 3 >>gatherin.txt                      #Active Transactions
echo p >>gatherin.txt                      #previous screen
echo 2 >>gatherin.txt                      #Blocked Clients
echo p >>gatherin.txt                      #previous screen
echo 5 >>gatherin.txt                      #Local Batch Clients
echo p >>gatherin.txt                      #previous screen
echo 6 >>gatherin.txt                      #Remote Clients
echo p >>gatherin.txt                      #previous screen
echo 4 >>gatherin.txt                      #Local Interactive Clients
echo p >>gatherin.txt                      #previous screen
echo 7 >>gatherin.txt                      #Background Processes
echo t >>gatherin.txt                      #go to top menu
echo 1 >>gatherin.txt                     #Status Displays
echo 3 >>gatherin.txt                     #Servers
echo p >>gatherin.txt                     #previous screen
echo 6 >>gatherin.txt                     #Lock Table
echo 1 >>gatherin.txt                     #Display all lock entries
echo p >>gatherin.txt                     #previous screen
echo 14 >>gatherin.txt                   #Shared Memory Segments
echo p >>gatherin.txt                     #previous screen
echo 12 >>gatherin.txt                   #Startup Parameters
echo p >>gatherin.txt                     #previous screen
echo 1 >>gatherin.txt                     #Database
echo t >>gatherin.txt                     #go to top menu
echo 2 >>gatherin.txt                    #Activity Displays
echo 1 >>gatherin.txt                    #Summary
echo a >>gatherin.txt                    #Activate auto repeat mode
echo t >>gatherin.txt                    #go to top menu
echo 2 >>gatherin.txt                    #Activity Displays
echo 2 >>gatherin.txt                    #Servers
echo z >>gatherin.txt                    #Zero the activity counters
echo u >>gatherin.txt                   #Update activity counters
count2=1 #define counter to iterate for blank lines for servers
while [ ${count2} -le ${servers} ]
do
  { 
      echo >> gatherin.txt       # return     
      count2=`expr $count2 + 1`  
  }
done
echo t >>gatherin.txt                    #go to top menu
echo 2 >>gatherin.txt                   #Activity Displays
echo 5 >>gatherin.txt                    #BI Log
echo a >>gatherin.txt                   #Activate auto repeat mode
echo p >>gatherin.txt                   #previous screen
echo 13 >>gatherin.txt                 #Activity Displays Other
echo a >>gatherin.txt                   #Activate auto repeat mode
echo p >>gatherin.txt                   #previous screen
echo 7 >>gatherin.txt                   #Lock Table
echo a >>gatherin.txt                   #Activate auto repeat mode
echo t >>gatherin.txt                   #go to top menu
echo 3 >>gatherin.txt                  #Other Displays
echo 2 >>gatherin.txt                  #I/O Operations by Process
echo p >>gatherin.txt                  #previous screen
echo 4 >>gatherin.txt                  #Checkpoints
echo t >>gatherin.txt                  #go to top menu
echo 3 >>gatherin.txt                 #Other Displays
echo 2 >>gatherin.txt                 #I/O Operations by Process
echo t >>gatherin.txt                 #go to top menu
echo 1 >>gatherin.txt                #Summary
echo 16 >>gatherin.txt              #Database Service Manager -- Replication Queue
echo r >>gatherin.txt # refresh Database Service Manager data
echo r >>gatherin.txt # refresh Database Service Manager data
echo r >>gatherin.txt # refresh Database Service Manager data
echo r >>gatherin.txt # refresh Database Service Manager data
echo r >>gatherin.txt # refresh Database Service Manager data
echo r >>gatherin.txt # refresh Database Service Manager data
echo r >>gatherin.txt # refresh Database Service Manager data
echo r >>gatherin.txt # refresh Database Service Manager data
echo r >>gatherin.txt # refresh Database Service Manager data
echo r >>gatherin.txt # refresh Database Service Manager data
echo r >>gatherin.txt # refresh Database Service Manager data
echo r >>gatherin.txt # refresh Database Service Manager data
echo r >>gatherin.txt # refresh Database Service Manager data
echo r >>gatherin.txt # refresh Database Service Manager data
echo r >>gatherin.txt # refresh Database Service Manager data
echo r >>gatherin.txt # refresh Database Service Manager data
echo r >>gatherin.txt # refresh Database Service Manager data
echo r >>gatherin.txt # refresh Database Service Manager data
echo r >>gatherin.txt # refresh Database Service Manager data
echo r >>gatherin.txt # refresh Database Service Manager data
# in some versions the prior menu does not exist so it will generate a repeat in
# the output but the t in the next line will bring it to the top menu
# this was added to collect Replication information
echo t >>gatherin.txt                 #go to top menu
echo debghb >>gatherin.txt      #activate debghb
echo 6 >>gatherin.txt                #Enter debghb menu
echo t >>gatherin.txt                #go to top menu
echo 3 >>gatherin.txt               #Other Displays
echo 1 >>gatherin.txt               #Performance Indicators
echo t >>gatherin.txt                #go to top menu
echo 6 >>gatherin.txt               #Enter debghb menu
echo 5 >>gatherin.txt               #Locked Buffers
echo p >>gatherin.txt               #previous screen
echo 6 >>gatherin.txt               #Buffer Locks
echo t >>gatherin.txt                #go to top menu
echo 6 >>gatherin.txt               #Enter debghb menu
echo 8 >>gatherin.txt               #Resource Queues
echo a >>gatherin.txt               #Activate auto repeat mode
echo p >>gatherin.txt               #previous screen
echo 9 >>gatherin.txt               #TXE Lock Activity
echo a >>gatherin.txt               #Activate auto repeat mode
echo t >>gatherin.txt                #go to top menu
echo 4 >>gatherin.txt               #Administrative Functions
echo 4 >>gatherin.txt               #Adjust Latch Options
echo 2 >>gatherin.txt               #Enable latch activity collection
echo 3 >>gatherin.txt               #Enable latch timing collection
echo t >>gatherin.txt                #go to top menu
echo 6 >>gatherin.txt               #Enter debghb menu
echo 10 >>gatherin.txt             #Adjust TXE options
echo p >>gatherin.txt               #previous screen
echo 11 >>gatherin.txt             #Latch Counts
echo a >>gatherin.txt               #Activate auto repeat mode
echo p >>gatherin.txt               #previous screen
echo 12 >>gatherin.txt             #Latch Times
echo a >>gatherin.txt               #Activate auto repeat mode
echo t >>gatherin.txt               #go to top menu
echo 5 >>gatherin.txt              #Adjust Monitor Options
echo 3 >>gatherin.txt              #Monitor Sampling Interval
echo 6 >>gatherin.txt              #set value of interval
echo t >>gatherin.txt               #go to top menu
echo 6 >>gatherin.txt               #Enter debghb menu
echo 11 >>gatherin.txt             #Latch Counts
echo a >>gatherin.txt               #Activate auto repeat mode
echo t >>gatherin.txt               #go to top menu
echo 4 >>gatherin.txt              #Administrative Functions
echo 4 >>gatherin.txt             #Adjust Latch Options
echo 2 >>gatherin.txt             #Disable latch activity collection
echo 3 >>gatherin.txt             #Disable latch timing collection
echo x >>gatherin.txt             #Exit Promon

if [ $ostype = "Linux" ]
 then  
  {
  if [ -x `which iostat` ] 
   then
    {
     echo "found iostat"
     iostat -dktx ${sleep_interval} ${os_repeats} >>iostat-dktx.out&  
    }
  fi

  if [ -x `which vmstat` ] 
   then  
    {
     echo "found vmstat"
     vmstat ${sleep_interval} ${os_repeats} >>vmstat.out&  
    }
  fi

  if [ -x `which sar` ] 
   then    
    {
     echo "found sar"
     sar -q  ${sleep_interval} ${os_repeats} >>sar-q.out& 
    }
  fi

  if [ -z "$2" ] || [ "$2" != "perf" ]
   then 
    {
      i=0
      for i in $pslist 
       do     
        {
          kill -s USR1 $i
          sleep 2
          kill -s USR1 $i 
        }
       done
    echo "Please locate and move all protrace files created to day to the "$DATETIME" directory." 
    }
   fi 
  }

fi

if [ $ostype = "AIX" ]
 then  
  {
    if [ -z "$2" ] || [ $2 != "perf" ]
     then 
       {
         if [ -x `whence errpt` ] 
          then     
            {
              echo "found errpt"
              errpt>>errors.txt& 
            }
         else    
          {
            echo "Couldn't find errpt.">>errpt.txt 
          }
         fi 
       }

     fi
     if [ -x `whence iostat` ] 
      then    
        {
          iostat -D ${sleep_interval} ${os_repeats} >>iostat-d.out& 
        }
     else    
       {
         echo "Couldn't find iostat.">>iostat.out 
       }
     fi

     if [ -x `whence vmstat` ] 
      then    
        {
          vmstat ${sleep_interval} ${os_repeats} >>vmstat.out&  
        }
     else 
        {
          echo "Couldn't find vmstat.">>vmstat.out  
        }
     fi
     if [ -z "$2" ] || [ "$2" != "perf" ]
      then     
        {
           i=0
                for i in $pslist
                do         
                 {
                      procstack $i >> procstack.out
                      sleep 2
                      procstack $i  >> procstack.out
                 }
                done
                echo "Please locate and move all protrace files created to day to the "$DATETIME" directory."     
       }
     fi 
    }
fi

if [ $ostype = "SunOS" ]
 then  
   {
     me=`/usr/ucb/whoami`

     if [ -z "$2" ] || [ $2 != "perf" ]
      then 
        {
          if [ -x `whence dmesg` ] 
           then  
             {
               dmesg>>errors.txt&  
             }
          else  
            {
               echo "Couldn't find dmesg.">>errors.txt  
            }
          fi 
        }
      fi

      if [ -x `whence sar` ] 
       then  
         {
           sar -d ${sleep_interval} ${os_repeats} >>sar-d.out&
           sar -q ${sleep_interval} ${os_repeats} >>sar-q.out&  
         }
      else 
        {
          echo "Couldn't find sar.">>sar-d.out  
        }
      fi

      if [ -x `whence iostat` ] 
       then  
         {
           iostat -cCxnmdM ${sleep_interval} ${os_repeats} >>iostat-cCxnmdM.out&
         }
      else  
        {
          echo "Couldn't find iostat.">>iostat-cCxnmdM.out& 
        }
      fi

      if [ -x `whence vmstat` ] 
       then 
         {
           vmstat ${sleep_interval} ${os_repeats} >>vmstat.out&
         }
       else 
         {
           echo "Couldn't find vmstat.">>vmstat.out  
         }
       fi

       if [ -z "$2" ] || [ "$2" != "perf" ]
        then  
          {
            if [ -x `whence pstack` ] 
             then  
               {
                 pstack $pslist>>pstack.out&
                 sleep 2
                 pstack $pslist>>pstack.out& 
               }
            else  
              {
                echo "Couldn't find pstack.">>pstack.out
                i=0
                for i in $pslist 
                  do 
                    {
                      kill -s USR1 $i
                      sleep 2
                      kill -s USR1 $i 
                    }
                  done
                  echo "Please locate and move all protrace files created to day to the "$DATETIME" directory."  
              }
            fi 
           }
         fi 
        } 
fi

if [ $ostype = "OSF1" ] 
    then 
    { 
      echo "ps -ef" >> ./howmany.txt 
      ps -ef | wc -l |awk '{ print "There are " $1 " processes running on the machine." }'>> ./howmany.txt 
      ps -ef|grep _progres|grep -v grep|wc -l|awk '{ print "There are " $1  " _progres sessions." }'>>howmany.txt
      ps -ef|grep _mprosrv|grep -v grep|wc -l|awk '{ print "There are " $1  " _mprosrv sessions." }'>>howmany.txt
      echo "SWAP" >> ./swapfileinfo.txt
      /usr/sbin/swapon -s >> ./swapfileinfo.txt 
      vmstat ${sleep_interval} ${os_repeats} >>vmstat.out& 
      iostat ${sleep_interval} ${os_repeats} >>iostat.out&

      if [ -z "$2" ] || [ $2 != "perf" ]
       then
         {
           i=0
           for i in $pslist
             do
                {
                  kill -s USR1 $i
                  sleep 2
                  kill -s USR1 $i
                }
             done
             echo "Please locate and move all protrace files created to day to the $DATETIME directory."
            }
          fi

      if [ -z "$2" ] || [ $2 != "perf" ]
            then 
              {
                if [ -x `whence dmesg` ] 
                 then  
                   {
                     dmesg >>errors.txt&
                   }
                else  
                  {
                     cat /var/adm/messages >>errors.txt&  
                  }
                fi 
              }
      fi
    } 
  fi

if [ $ostype = "HP-UX" ]
 then  
   { 
      if [ -x `whence vmstat` ]
       then  
         {
           vmstat ${sleep_interval} ${os_repeats} >>vmstat.out&
         }
      fi

      if [ -x `whence sar` ]
       then 
         {
           sar -q ${sleep_interval} ${os_repeats} >>sar-q.out&
           sar -o /tmp/temp -d ${sleep_interval} ${os_repeats} >>sar-d.out&
         }
      fi

      if [ -z "$2" ] || [ $2 != "perf" ]
       then 
         {
           i=0
           for i in $pslist 
             do 
                {
                  kill -s USR1 $i
                  sleep 2
                  kill -s USR1 $i  
                }
             done
             echo "Please locate and move all protrace files created to day to the $DATETIME directory." 
            }
          fi 
         }
fi

count=1
while [ ${count} -le ${repeats} ]
do  
  {
       promon ${1} -NL < gatherin.txt >> gather.out
       sleep ${sleep_interval}
       count=`expr $count + 1` 
  }
done

#The following section is useful in examining Record Locking issues.
#Uncomment all the lines beyond this point if exploring record locking issues.
#echo "{" >>chains.awk
#echo "   goneinthelooponce=0" >>chains.awk
#echo "   getline" >>chains.awk
#echo "      while ( \$0 != \"Record Locking Table:\")" >>chains.awk
#echo "         {" >>chains.awk
#echo "             getline" >>chains.awk
#echo "          }" >>chains.awk
#echo "      getline" >>chains.awk
#echo "      getline" >>chains.awk
#echo "      goneinthelooponce=1" >>chains.awk
#echo "         while ( \$0 != \"Record Locking Table:\" )" >>chains.awk
#echo "            {" >>chains.awk
#echo "               print \$0" >>chains.awk
#echo "               getline" >>chains.awk
#echo "            }" >>chains.awk
#echo "   if ( goneinthelooponce == 1)" >>chains.awk
#echo "      exit">>chains.awk
#echo "}" >>chains.awk

#awk -f ./chains.awk ./gather.out >>chains.txt

#echo "#!/bin/sh">>depth.sh
#echo "cat ./chains.txt | awk '{print \" \" \$4 \" \"}'|sort -n >sortedchain.txt" >>depth.sh
#echo "chainlist=\`cat ./sortedchain.txt\`" >>depth.sh
#echo "uniqchain=\`cat ./sortedchain.txt|uniq -u \`" >>depth.sh
#echo "cat ./sortedchain.txt | uniq -u > uchain.txt" >>depth.sh
#echo "cat ./sortedchain.txt | uniq -c > littlelist.txt" >>depth.sh
#chmod +x ./depth.sh
#exec ./depth.sh &
#rm -f ./uchain.txt ./sortedchain.txt


