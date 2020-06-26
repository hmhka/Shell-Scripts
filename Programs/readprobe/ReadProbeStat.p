&SCOPED-DEFINE Sep "~t"

ASSIGN SESSION:DATE-FORMAT = "mdy":U.

DEFINE VARIABLE vTestId AS INTEGER NO-UNDO INITIAL 0.

DEFINE TEMP-TABLE ttTestResults NO-UNDO
  FIELD TestId         AS INTEGER
  FIELD ResultType     AS CHARACTER /* AVERAGE, MINIMUM, MAXIMUM */
  FIELD BgnDate        AS DATE
  FIELD BgnTime        AS INTEGER
  FIELD EndDate        AS DATE   
  FIELD EndTime        AS INTEGER
/* Test Environment: */
  FIELD UserCount      AS INTEGER
  FIELD SpinValue      AS INTEGER
  FIELD NapValue       AS INTEGER
  FIELD NapmaxValue    AS INTEGER
  FIELD LruEnabled     AS LOGICAL
  FIELD MuxValue       AS INTEGER
  FIELD NumOfCPUs      AS INTEGER
/* CPU Load: */
  FIELD CPUIntervals   AS INTEGER /* Number of stat intervals */
  FIELD CPUUsr         AS INTEGER
  FIELD CPUSys         AS INTEGER
  FIELD CPUIdl         AS INTEGER
/* Activity: Latch Counts */
  FIELD LatchIntervals AS INTEGER /* Number of stat intervals */
  FIELD LRULocks       AS DECIMAL
  FIELD LRUNaps        AS DECIMAL
  FIELD BUFLocks       AS DECIMAL
  FIELD BUFNaps        AS DECIMAL
  FIELD BHTLocks       AS DECIMAL
  FIELD BHTNaps        AS DECIMAL
/* Activity: Performance Indicators */
  FIELD PerfIntervals  AS INTEGER /* Number of stat intervals */
  FIELD RecordReads    AS DECIMAL
  FIELD LatchTimeouts  AS DECIMAL
/* Protrace Stats: */
  FIELD UtnapCount     AS INTEGER
/* Three protrace types with highest counts */
  FIELD ProtraceCount  AS INTEGER   EXTENT 3
  FIELD ModuleList     AS CHARACTER EXTENT 3
  FIELD RestProtraceCount AS INTEGER /* The count of rest protraces  */
  FIELD RestProtraceTypes AS INTEGER /* and the count of their types */
  INDEX TestId IS UNIQUE
        TestId    
        ResultType
. /* DEFINE TEMP-TABLE ttTestResults */

DEFINE BUFFER AveResults FOR ttTestResults.
DEFINE BUFFER MinResults FOR ttTestResults.
DEFINE BUFFER MaxResults FOR ttTestResults.

DEFINE TEMP-TABLE ttModule NO-UNDO
  FIELD ModuleId      AS INTEGER
  FIELD ModuleName    AS CHARACTER
  FIELD ModuleLibrary AS CHARACTER
  FIELD ModuleCount   AS INTEGER
  INDEX ModuleName IS UNIQUE
        ModuleLibrary
        ModuleName
  INDEX ModuleId IS UNIQUE
        ModuleId
. /* DEFINE TEMP-TABLE ttModule */

/* Unique protrace call sets: 
ModuleListHead - the list of first six module names.
ModuleListTail - the rest of the module names.
First six modules are responsible for protrace functionality
and they should exist in all protraces.
ModuleIdList - the list of module IDs.
The link between module ID and module name is ttModule.
ModuleIdList is shorter than ModuleListHead + ModuleListTail and
it can be used as an index component.
*/
DEFINE TEMP-TABLE ttProtraceType NO-UNDO
  FIELD TypeCount      AS INTEGER
  FIELD ModuleIdList   AS CHARACTER
  FIELD ModuleListHead AS CHARACTER
  FIELD ModuleListTail AS CHARACTER
  INDEX ModuleIdList   AS UNIQUE
        ModuleIdList
. /* DEFINE TEMP-TABLE ttProtraceType */

DEFINE STREAM ProtraceDir.

/* ------------------------------------------------------------------------- */
FUNCTION String2Date RETURNS DATE (INPUT ipString AS CHARACTER):
  DEFINE VARIABLE vDate AS DATE NO-UNDO INITIAL ?.
/*2    7  10 13 16 19
[2010/05/06@11:00:02.319+0400] */
  ASSIGN vDate = DATE(INTEGER(SUBSTRING(ipString, 7,2)), /*month*/
                      INTEGER(SUBSTRING(ipString,10,2)), /* day */
                      INTEGER(SUBSTRING(ipString, 2,4))) /*year */
  NO-ERROR.
   
  RETURN vDate.
END FUNCTION. /* String2Date */

/* ------------------------------------------------------------------------- */
FUNCTION String2Time RETURNS INTEGER (ipString AS CHARACTER):
  DEFINE VARIABLE vTime AS INTEGER NO-UNDO INITIAL ?.
/*2   7  10 13 16 19
[2010/05/06@11:00:02.319+0400] */
  ASSIGN vTime = INTEGER(SUBSTRING(ipString,13,2)) /*HH*/ * 3600
               + INTEGER(SUBSTRING(ipString,16,2)) /*MM*/ * 60
               + INTEGER(SUBSTRING(ipString,19,2)) /*SS*/
  NO-ERROR.
  RETURN vTime.
END FUNCTION. /* String2Time */

/* ------------------------------------------------------------------------- */
PROCEDURE DbLogInfo.
  DEFINE INPUT PARAMETER  ipInputFile AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opLastLoginDate   AS DATE    NO-UNDO.
  DEFINE OUTPUT PARAMETER opLastLoginTime   AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER opFirstLogoutDate AS DATE    NO-UNDO.
  DEFINE OUTPUT PARAMETER opFirstLogoutTime AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE vVersion    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vSpinValue  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLruEnabled AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE vMuxValue   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLoginCount AS INTEGER NO-UNDO.
  DEFINE VARIABLE vLine       AS CHARACTER NO-UNDO.

  ASSIGN vLoginCount = 0.
  INPUT FROM VALUE(ipInputFile).
  REPEAT:
    ASSIGN vLine = "".
    IMPORT UNFORMATTED vLine.
    ASSIGN vLine = TRIM(vLine).

/*BROKER  0: (4234)  Progress OpenEdge Release 10.1C build 1526 SP04 on Linux*/
    IF vLine MATCHES "*BROKER  0: (4234) *":U THEN
    ASSIGN vVersion = SUBSTRING(vLine, INDEX(vLine, "Progress":U) + 9).
    ELSE
/*BROKER  0: (-----) LRU mechanism enabled.*/
/*BROKER  0: (-----) LRU mechanism disabled.*/
    IF vLine MATCHES "*BROKER  0: (-----) LRU mechanism *":U THEN
    ASSIGN vLruEnabled = vLine MATCHES "*enabled.":U.
    ELSE
/*BROKER  0: (4243)  Current Spin Lock Tries (-spin): 10. */
    IF vLine MATCHES "*BROKER  0: (4243) *":U THEN
    ASSIGN vLine      = SUBSTRING(vLine, R-INDEX(vLine, " ":U) + 1)
           vLine      = TRIM(vLine, ".":U)
           vSpinValue = INTEGER(vLine)
    . /* ASSIGN */
    ELSE
/*BROKER  0: (12821) Use muxlatches (-mux): 1 */
    IF vLine MATCHES "*BROKER  0: (12821) *":U THEN
    ASSIGN vLine     = SUBSTRING(vLine, R-INDEX(vLine, " ":U) + 1)
           vMuxValue = INTEGER(vLine)
    . /* ASSIGN */
    ELSE
/*[2011/10/07@12:30:38.286+0400] P-39402      T-465098496 I 
  ABL   105: (452)   Login by bis on batch. */
    IF vLine MATCHES "*: (452) *":U THEN
    ASSIGN vLoginCount     = vLoginCount + 1
           opLastLoginDate = String2Date(vLine)
           opLastLoginTime = String2Time(vLine)
    . /* ASSIGN */
    ELSE
/*SHUT  106: (542)   Server shutdown started by bis on /dev/pts/2. */
/*ABL    80: (453)   Logout by bis on batch. */
    IF vLine MATCHES "*: (542) *":U
    OR vLine MATCHES "*: (453) *":U THEN
    DO:
      ASSIGN opFirstLogoutDate = String2Date(vLine)
             opFirstLogoutTime = String2Time(vLine)
      . /* ASSIGN */
      LEAVE.
    END.
  END. /* REPEAT */
  INPUT CLOSE.

  DO TRANSACTION:
    ASSIGN AveResults.BgnDate    = opLastLoginDate
           AveResults.BgnTime    = opLastLoginTime
           AveResults.EndDate    = opFirstLogoutDate
           AveResults.EndTime    = opFirstLogoutTime
           AveResults.UserCount  = vLoginCount
           AveResults.SpinValue  = vSpinValue  
           AveResults.LruEnabled = vLruEnabled 
           AveResults.MuxValue   = vMuxValue   

           MinResults.BgnDate    = AveResults.BgnDate    
           MinResults.BgnTime    = AveResults.BgnTime    
           MinResults.EndDate    = AveResults.EndDate    
           MinResults.EndTime    = AveResults.EndTime    
           MinResults.UserCount  = AveResults.UserCount  
           MinResults.SpinValue  = AveResults.SpinValue  
           MinResults.LruEnabled = AveResults.LruEnabled 
           MinResults.MuxValue   = AveResults.MuxValue   

           MaxResults.BgnDate    = AveResults.BgnDate    
           MaxResults.BgnTime    = AveResults.BgnTime    
           MaxResults.EndDate    = AveResults.EndDate    
           MaxResults.EndTime    = AveResults.EndTime    
           MaxResults.UserCount  = AveResults.UserCount  
           MaxResults.SpinValue  = AveResults.SpinValue  
           MaxResults.LruEnabled = AveResults.LruEnabled 
           MaxResults.MuxValue   = AveResults.MuxValue   
    . /* ASSIGN */
  END.
END PROCEDURE. /* DbLogInfo */

/* -------------------------------------------------------------------------- */
PROCEDURE DbLogInfoFromPromon.

  DEFINE  INPUT PARAMETER ipInputFile AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opBgnDate   AS DATE      NO-UNDO.
  DEFINE OUTPUT PARAMETER opBgnTime   AS INTEGER   NO-UNDO.
  DEFINE OUTPUT PARAMETER opEndDate   AS DATE      NO-UNDO.
  DEFINE OUTPUT PARAMETER opEndTime   AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vHeader AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vLine   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vItem   AS CHARACTER NO-UNDO EXTENT 12.
  DEFINE VARIABLE vDate   AS DATE      NO-UNDO.
  DEFINE VARIABLE vTime   AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vSpinValue    AS INTEGER NO-UNDO.
  DEFINE VARIABLE vUserCount    AS INTEGER NO-UNDO.
  DEFINE VARIABLE vMaxUserCount AS INTEGER NO-UNDO.
  DEFINE VARIABLE vNapValue     AS INTEGER NO-UNDO.
  DEFINE VARIABLE vNapmaxValue  AS INTEGER NO-UNDO.
  
  ASSIGN vMaxUserCount = 0.
  INPUT FROM VALUE(ipInputFile).
  
PromonHeader:
  REPEAT:

    PROCESS EVENTS.
    ASSIGN vLine = "".
    IMPORT UNFORMATTED vLine.

/* 10:45:15        Adjust Latch Options */    
    IF vLine MATCHES "..:..:.. *Adjust Latch Options*"  THEN
    RUN AdjustLatchOptions(OUTPUT vSpinValue,
                           OUTPUT vNapValue,
                           OUTPUT vNapmaxValue).

    IF NOT vLine MATCHES "../../.. *Activity: Summary" THEN
    NEXT PromonHeader.

/* 09/30/11        Activity: Summary */
    ASSIGN 
      vHeader = TRIM(SUBSTRING(vLine, 9))
      vDate   = DATE(INTEGER(SUBSTRING(vLine, 1,2)), /*month*/
                     INTEGER(SUBSTRING(vLine, 4,2)), /* day */
              2000 + INTEGER(SUBSTRING(vLine, 7,2))) /*year */
    NO-ERROR.
    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    NEXT PromonHeader.

/*
04:02:15        01/09/10 14:39 to 01/21/10 04:02 (277 hrs 22 min)
*/  IMPORT UNFORMATTED vLine.
    IF NOT vLine MATCHES "..:..:.. *(*)" THEN
    NEXT PromonHeader.
  
    ASSIGN
      vTime = INTEGER(SUBSTRING(vLine,1,2)) /*HH*/ * 3600
            + INTEGER(SUBSTRING(vLine,4,2)) /*MM*/ * 60
            + INTEGER(SUBSTRING(vLine,7,2)) /*SS*/
    NO-ERROR.
    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    NEXT PromonHeader.

    REPEAT WHILE NOT vLine BEGINS "Buffer Hits":U 
           ON ENDKEY UNDO, LEAVE PromonHeader:
      ASSIGN vLine = "":U.
      IMPORT UNFORMATTED vLine.
    END.
    ASSIGN vItem = "":U.
    IMPORT vItem.
/* 0 Servers, 100 Users (100 Local, 0 Remote, 100 Batch), 0 Apws */
    IF vItem[4] NE "Users":U THEN
    IMPORT vItem.

    IF vItem[4] NE "Users":U THEN
    NEXT PromonHeader.

    ASSIGN vUserCount = INTEGER(vItem[3]) NO-ERROR.
    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    NEXT PromonHeader.

    IF vUserCount GT vMaxUserCount THEN
    ASSIGN
      vMaxUserCount = vUserCount
      opBgnDate     = vDate
      opBgnTime     = vTime
      opEndDate     = vDate
      opEndTime     = vTime
    . /* ASSIGN */
    ELSE
    IF vUserCount EQ vMaxUserCount THEN
    ASSIGN
      opEndDate     = vDate
      opEndTime     = vTime
    . /* ASSIGN */
    ELSE
    IF vUserCount LT vMaxUserCount THEN
    LEAVE PromonHeader.

  END. /* PromonHeader: REPEAT */
  INPUT CLOSE.

  DO TRANSACTION:
    ASSIGN AveResults.BgnDate    = opBgnDate
           AveResults.BgnTime    = opBgnTime
           AveResults.EndDate    = opEndDate
           AveResults.EndTime    = opEndTime
           AveResults.UserCount  = vMaxUserCount
           AveResults.SpinValue  = vSpinValue  
           AveResults.LruEnabled = ?
           AveResults.MuxValue   = ?

           MinResults.BgnDate    = opBgnDate    
           MinResults.BgnTime    = opBgnTime    
           MinResults.EndDate    = opEndDate    
           MinResults.EndTime    = opEndTime    
           MinResults.UserCount  = vMaxUserCount
           MinResults.SpinValue  = vSpinValue   
           MinResults.LruEnabled = ?
           MinResults.MuxValue   = ?            

           MaxResults.BgnDate    = opBgnDate    
           MaxResults.BgnTime    = opBgnTime    
           MaxResults.EndDate    = opEndDate    
           MaxResults.EndTime    = opEndTime    
           MaxResults.UserCount  = vMaxUserCount
           MaxResults.SpinValue  = vSpinValue   
           MaxResults.LruEnabled = ?
           MaxResults.MuxValue   = ?            
    . /* ASSIGN */
  END.
END PROCEDURE. /* DbLogInfoFromPromon */


/* ------------------------------------------------------------------------- */
PROCEDURE ReadProbeInfo.
  DEFINE INPUT PARAMETER ipInputFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vNumOfCPUs   AS INTEGER NO-UNDO.
  DEFINE VARIABLE vSpinValue   AS INTEGER NO-UNDO.
/*  
  Number of CPUs:           80
           -Spin:       100000 10,000 is the standard value.
   # of Sessions:          100     50 is the standard value.
         Comment: -spin 100000 -nap 10 -napmax 250                                           
*/
  DEFINE VARIABLE vLine   AS CHARACTER NO-UNDO.

  INPUT FROM VALUE(ipInputFile).
  REPEAT:
    ASSIGN vLine = "".
    IMPORT UNFORMATTED vLine.
    ASSIGN vLine = TRIM(vLine).

    IF vLine BEGINS "Number of CPUs: ":U THEN
    ASSIGN vNumOfCPUs = INTEGER(SUBSTRING(vLine, R-INDEX(vLine, " ":U) + 1)).
    ELSE
    IF vLine BEGINS "-Spin: ":U THEN
    ASSIGN vLine      = TRIM(SUBSTRING(vLine, 7))
           vSpinValue = INTEGER(SUBSTRING(vLine, 1, INDEX(vLine, " ":U)))
    . /* ASSIGN */
  END. /* REPEAT */
  INPUT CLOSE.

  IF vSpinValue NE AveResults.SpinValue THEN
  MESSAGE 
    "ReadProbeFile:" ipInputFile SKIP
    "Spin value in db log and in .rp file do not match:"  
    AveResults.SpinValue "vs" vSpinValue
    VIEW-AS ALERT-BOX WARNING BUTTONS OK.

  DO TRANSACTION:
    ASSIGN AveResults.NumOfCPUs = vNumOfCPUs
           MinResults.NumOfCPUs = AveResults.NumOfCPUs
           MaxResults.NumOfCPUs = AveResults.NumOfCPUs  
    . /* ASSIGN */
  END.
END PROCEDURE. /* ReadProbeInfo */

/* ------------------------------------------------------------------------- */
PROCEDURE CPUStat.
  DEFINE INPUT  PARAMETER ipInputFile AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipBgnTime   AS INTEGER   NO-UNDO.
  DEFINE INPUT  PARAMETER ipEndTime   AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vCPUUsr   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vCPUSys   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vCPUIdl   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vCurrTime AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vPrevTime AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLine     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vItem     AS CHARACTER NO-UNDO EXTENT 6.

  ASSIGN vCurrTime = 0.
  INPUT FROM VALUE(ipInputFile).
  REPEAT:
    ASSIGN vLine = "".
    IMPORT UNFORMATTED vLine.
    ASSIGN vLine = TRIM(vLine).

/*Time: 10:45:25
avg-cpu:  %user   %nice %system %iowait  %steal   %idle
           9,36    0,00    0,49    0,00    0,00   90,15
*/  IF vLine BEGINS "Time: ":U THEN
    ASSIGN vPrevTime = vCurrTime
           vCurrTime = String2Time("123456" + vLine)
    . /* ASSIGN */
    ELSE
    IF  vLine BEGINS "avg-cpu: ":U THEN
    IF  vPrevTime GT ipBgnTime
    AND vCurrTime GT ipBgnTime
    AND vCurrTime LE ipEndTime THEN
    DO:
      IMPORT vItem.
      ASSIGN vCPUUsr = DECIMAL(REPLACE(vItem[1], ",":U, ".":U))
             vCPUSys = DECIMAL(REPLACE(vItem[3], ",":U, ".":U))
             vCPUIdl = DECIMAL(REPLACE(vItem[6], ",":U, ".":U))
      . /* ASSIGN */
      ACCUMULATE vCPUUsr (AVERAGE MINIMUM MAXIMUM COUNT)
                 vCPUSys (AVERAGE MINIMUM MAXIMUM)
                 vCPUIdl (AVERAGE MINIMUM MAXIMUM)
      . /* ACCUMULATE */
    END.
  END. /* REPEAT */
  INPUT CLOSE.

  DO TRANSACTION:
    ASSIGN AveResults.CPUIntervals = ACCUM COUNT   vCPUUsr
           AveResults.CPUUsr       = ACCUM AVERAGE vCPUUsr
           AveResults.CPUSys       = ACCUM AVERAGE vCPUSys
           AveResults.CPUIdl       = ACCUM AVERAGE vCPUIdl

           MinResults.CPUIntervals = ACCUM COUNT   vCPUUsr
           MinResults.CPUUsr       = ACCUM MINIMUM vCPUUsr
           MinResults.CPUSys       = ACCUM MINIMUM vCPUSys
           MinResults.CPUIdl       = ACCUM MINIMUM vCPUIdl

           MaxResults.CPUIntervals = ACCUM COUNT   vCPUUsr
           MaxResults.CPUUsr       = ACCUM MAXIMUM vCPUUsr
           MaxResults.CPUSys       = ACCUM MAXIMUM vCPUSys
           MaxResults.CPUIdl       = ACCUM MAXIMUM vCPUIdl
    . /* ASSIGN */
  END.
END PROCEDURE. /* CPUStat */

/* -------------------------------------------------------------------------- */
PROCEDURE AdjustLatchOptions.
  DEFINE OUTPUT PARAMETER opSpinValue   AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER opNapValue    AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER opNapmaxValue AS INTEGER NO-UNDO.
/*
10:45:15        Adjust Latch Options

		1. Spins before timeout:       10
		2. Enable latch activity data collection
		3. Enable latch timing data collection
		4. Initial latch sleep time:   10 milliseconds
		5. Maximum latch sleep time:   250 milliseconds
		6. Record Free Chain Search Depth Factor: 5
*/
  DEFINE VARIABLE i     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLine AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vItem AS CHARACTER NO-UNDO EXTENT 7.

  REPEAT i = 1 TO 7:
    ASSIGN vItem = "".
    IMPORT vItem.
    IF vItem[2] EQ "Spins":U THEN
    ASSIGN opSpinValue = INTEGER(vItem[5]).
    ELSE
    IF vItem[2] EQ "Initial":U THEN
    ASSIGN opNapValue = INTEGER(vItem[6]).
    ELSE
    IF vItem[2] EQ "Maximum":U THEN
    ASSIGN opNapmaxValue = INTEGER(vItem[6]).
  END.

END PROCEDURE. /* ActivityLatchCounts */

/* -------------------------------------------------------------------------- */
PROCEDURE ActivityLatchCounts.
  DEFINE OUTPUT PARAMETER opLRULocks AS DECIMAL NO-UNDO.
  DEFINE OUTPUT PARAMETER opLRUNaps  AS DECIMAL NO-UNDO.
  DEFINE OUTPUT PARAMETER opBUFLocks AS DECIMAL NO-UNDO.
  DEFINE OUTPUT PARAMETER opBUFNaps  AS DECIMAL NO-UNDO.
  DEFINE OUTPUT PARAMETER opBHTLocks AS DECIMAL NO-UNDO.
  DEFINE OUTPUT PARAMETER opBHTNaps  AS DECIMAL NO-UNDO.

  DEFINE VARIABLE vLine   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE vItem   AS CHARACTER NO-UNDO EXTENT 7.

  ASSIGN opLRULocks = 0.0
         opLRUNaps  = 0.0
         opBUFLocks = 0.0
         opBUFNaps  = 0.0
         opBHTLocks = 0.0
         opBHTNaps  = 0.0
  . /* ASSIGN */

  IMPORT ^.
  PromonScreen:
  REPEAT ON ENDKEY UNDO, RETURN:

    ASSIGN vItem = "":U.
    IMPORT vItem.

/* Read until the empty line: */
    IF vItem[1] EQ "":U THEN
    LEAVE PromonScreen.
/*              ----- Locks -----        ------ Busy ------        Naps
   Owner        Total        /Sec        /Sec           Pct        /Sec
MTX 2293     86169346         416           0           0.0           6
USR  --      16860253          81           0           0.0           0
*/  ELSE
    IF vItem[1] EQ "LRU":U  THEN
    ASSIGN opLRULocks = opLRULocks + DECIMAL(vItem[4])
           opLRUNaps  = opLRUNaps  + DECIMAL(vItem[7])
    NO-ERROR.
    ELSE
    IF vItem[1] EQ "BUF":U  THEN
    ASSIGN opBUFLocks = opBUFLocks + DECIMAL(vItem[4])
           opBUFNaps  = opBUFNaps  + DECIMAL(vItem[7])
    NO-ERROR.
    ELSE
    IF vItem[1] EQ "BHT":U  THEN
    ASSIGN opBHTLocks = opBHTLocks + DECIMAL(vItem[4])
           opBHTNaps  = opBHTNaps  + DECIMAL(vItem[7])
    NO-ERROR.
  END. /* PromonScreen: REPEAT */
END PROCEDURE. /* ActivityLatchCounts */

/* -------------------------------------------------------------------------- */
PROCEDURE ActivityPerformanceIndicators.
  DEFINE OUTPUT PARAMETER opRecordReads   AS DECIMAL NO-UNDO.
  DEFINE OUTPUT PARAMETER opLatchTimeouts AS DECIMAL NO-UNDO.

  DEFINE VARIABLE vItem AS CHARACTER NO-UNDO EXTENT 7.

  ASSIGN opRecordReads   = ?
         opLatchTimeouts = ?
  . /* ASSIGN */

  PromonScreen:
  REPEAT ON ENDKEY UNDO, RETURN:

    ASSIGN vItem = "":U.
    IMPORT vItem.

/* Read until the empty line: */
    IF vItem[1] EQ "":U THEN
    LEAVE PromonScreen.
/*
01/21/10        Activity: Performance Indicators
04:02:15        01/09/10 14:39 to 01/21/10 04:02 (277 hrs 22 min)

                            Total   Per Min    Per Sec    Per Tx

Commits                  13778146       828      13.80      1.00
Undos                     1236024        74       1.24      0.09
Index operations         13191603K   811670   13527.83    980.41
Record operations        13730837K   844848   14080.81   1020.48
Total o/s i/o              613654K    37758     629.29     45.61
Total o/s reads            594692K    36591     609.85     44.20
Total o/s writes         19416853      1167      19.45      1.41
Background o/s writes    18808811      1130      18.84      1.37
Partial log writes        6567397       395       6.58      0.48
Database extends           628800        38       0.63      0.05
Total waits              19164665      1152      19.19      1.39
Lock waits                    125         0       0.00      0.00
Resource waits           19164540      1152      19.19      1.39
Latch timeouts            6493718       390       6.50      0.47
*/  ELSE
    IF vItem[1] EQ "Record":U  THEN
    ASSIGN opRecordReads = DECIMAL(vItem[5]) NO-ERROR.
    ELSE
    IF vItem[1] EQ "Latch":U  THEN
    ASSIGN opLatchTimeouts = DECIMAL(vItem[5]) NO-ERROR.
  END. /* PromonScreen: REPEAT */
END PROCEDURE. /* ActivityPerformanceIndicators */

/* -------------------------------------------------------------------------- */
PROCEDURE PromonStat.

  DEFINE INPUT PARAMETER ipInputFile AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipBgnDate   AS DATE      NO-UNDO.
  DEFINE INPUT PARAMETER ipBgnTime   AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipEndDate   AS DATE      NO-UNDO.
  DEFINE INPUT PARAMETER ipEndTime   AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vHeader AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vLine   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vItem   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDate   AS DATE      NO-UNDO.
  DEFINE VARIABLE vTime   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vInterval AS INTEGER NO-UNDO.

  DEFINE VARIABLE vSpinValue     AS INTEGER NO-UNDO.
  DEFINE VARIABLE vNapValue      AS INTEGER NO-UNDO.
  DEFINE VARIABLE vNapmaxValue   AS INTEGER NO-UNDO.
  DEFINE VARIABLE vLRULocks      AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vLRUNaps       AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vBUFLocks      AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vBUFNaps       AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vBHTLocks      AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vBHTNaps       AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vRecordReads   AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vLatchTimeouts AS DECIMAL NO-UNDO.

  INPUT FROM VALUE(ipInputFile).
  
PromonHeader:
  REPEAT:

    PROCESS EVENTS.
    ASSIGN vLine = "".
    IMPORT UNFORMATTED vLine.

/* 10:45:15        Adjust Latch Options */    
    IF vLine MATCHES "..:..:.. *Adjust Latch Options*"  THEN
    RUN AdjustLatchOptions(OUTPUT vSpinValue,
                           OUTPUT vNapValue,
                           OUTPUT vNapmaxValue).

    IF NOT vLine MATCHES "../../.. *Activity: *" THEN
    NEXT PromonHeader.

/* 09/30/11        Activity: Performance Indicators */
    ASSIGN 
      vHeader = TRIM(SUBSTRING(vLine, 9))
      vDate   = DATE(INTEGER(SUBSTRING(vLine, 1,2)), /*month*/
                     INTEGER(SUBSTRING(vLine, 4,2)), /* day */
              2000 + INTEGER(SUBSTRING(vLine, 7,2))) /*year */
    NO-ERROR.
    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    NEXT PromonHeader.

/*
04:02:15        01/09/10 14:39 to 01/21/10 04:02 (277 hrs 22 min)
*/  IMPORT UNFORMATTED vLine.
    IF NOT vLine MATCHES "..:..:.. *(*)" THEN
    NEXT PromonHeader.
  
    ASSIGN
      vTime = INTEGER(SUBSTRING(vLine,1,2)) /*HH*/ * 3600
            + INTEGER(SUBSTRING(vLine,4,2)) /*MM*/ * 60
            + INTEGER(SUBSTRING(vLine,7,2)) /*SS*/
/* vLine = Interval: */
      vLine = SUBSTRING(vLine, INDEX(vLine, "(":U) + 1)
      vLine = SUBSTRING(vLine, 1, LENGTH(vLine) - 1)
    NO-ERROR.
    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    NEXT PromonHeader.

/*
(121 hrs 22 min)
(30 min 0 sec)
(2 sec)
*/  IF vLine MATCHES "* hrs * min" THEN
    ASSIGN vInterval = INTEGER(ENTRY(1, vLine, " ":U)) * 3600
                     + INTEGER(ENTRY(3, vLine, " ":U)) * 60
    NO-ERROR.
    ELSE
    IF vLine MATCHES "* min * sec" THEN
    ASSIGN vInterval = INTEGER(ENTRY(1, vLine, " ":U)) * 60
                     + INTEGER(ENTRY(3, vLine, " ":U))
    NO-ERROR.
    ELSE
    IF vLine MATCHES "* sec" THEN
    ASSIGN vInterval = INTEGER(ENTRY(1, vLine, " ":U))
    NO-ERROR.
    ELSE
    NEXT PromonHeader.

    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    NEXT PromonHeader.

/* if the end of promon interval is after the requested interval: */
    IF (vDate - ipEndDate) * 3600 + (vTime - ipEndTime) GT 0 THEN
    LEAVE PromonHeader.

/* if the beginning of promon interval is before the requested interval: */
    IF (vDate - ipBgnDate) * 3600 + (vTime - ipBgnTime) LT vInterval THEN
    NEXT PromonHeader.

/* Skip header: */
    ASSIGN vLine = "":U.
    IMPORT vLine.
/* Header = 1 rows + 2 empty lines but not after SplitDbMon: */
    IF vLine EQ "":U THEN
    DO:
      IMPORT ^.
      IMPORT ^.
    END.
/*
06/15/10        Activity: Latch Counts
14:47:08        06/13/10 05:22 to 06/15/10 14:47 (57 hrs 24 min)
                ----- Locks -----        ------ Busy ------        Naps
   Owner        Total        /Sec        /Sec           Pct        /Sec
                           
01/21/10        Activity: Performance Indicators
04:02:15        01/09/10 14:39 to 01/21/10 04:02 (277 hrs 22 min)
                            Total   Per Min    Per Sec    Per Tx
*/  
    IF vHeader EQ "Activity: Latch Counts":U THEN
    DO:
      RUN ActivityLatchCounts(OUTPUT vLRULocks,
                              OUTPUT vLRUNaps,
                              OUTPUT vBUFLocks,
                              OUTPUT vBUFNaps, 
                              OUTPUT vBHTLocks,
                              OUTPUT vBHTNaps).

      ACCUMULATE vLRULocks (AVERAGE MINIMUM MAXIMUM COUNT)
                 vLRUNaps  (AVERAGE MINIMUM MAXIMUM)
                 vBUFLocks (AVERAGE MINIMUM MAXIMUM)
                 vBUFNaps  (AVERAGE MINIMUM MAXIMUM)
                 vBHTLocks (AVERAGE MINIMUM MAXIMUM)
                 vBHTNaps  (AVERAGE MINIMUM MAXIMUM).
    END.
    ELSE

    IF vHeader EQ "Activity: Performance Indicators":U THEN
    DO:
      RUN ActivityPerformanceIndicators(OUTPUT vRecordReads,
                                        OUTPUT vLatchTimeouts).
      ACCUMULATE vRecordReads   (AVERAGE MINIMUM MAXIMUM COUNT)
                 vLatchTimeouts (AVERAGE MINIMUM MAXIMUM).
    END.
  END. /* PromonHeader: REPEAT */
  INPUT CLOSE.

  IF AveResults.SpinValue NE vSpinValue THEN
  MESSAGE 
    "ReadProbeFile:" ipInputFile SKIP
    "Spin value in db log and in dbmon file do not match:"  
    AveResults.SpinValue "vs" vSpinValue
  VIEW-AS ALERT-BOX WARNING BUTTONS OK.

  DO TRANSACTION:
    ASSIGN AveResults.SpinValue      = vSpinValue  
           AveResults.NapValue       = vNapValue   
           AveResults.NapmaxValue    = vNapmaxValue
           AveResults.LatchIntervals = ACCUM COUNT   vLRULocks
           AveResults.LRULocks       = ACCUM AVERAGE vLRULocks
           AveResults.LRUNaps        = ACCUM AVERAGE vLRUNaps 
           AveResults.BUFLocks       = ACCUM AVERAGE vBUFLocks
           AveResults.BUFNaps        = ACCUM AVERAGE vBUFNaps 
           AveResults.BHTLocks       = ACCUM AVERAGE vBHTLocks
           AveResults.BHTNaps        = ACCUM AVERAGE vBHTNaps 
           AveResults.PerfIntervals  = ACCUM COUNT   vRecordReads
           AveResults.RecordReads    = ACCUM AVERAGE vRecordReads
           AveResults.LatchTimeouts  = ACCUM AVERAGE vLatchTimeouts

           MinResults.SpinValue      = vSpinValue  
           MinResults.NapValue       = vNapValue   
           MinResults.NapmaxValue    = vNapmaxValue
           MinResults.LatchIntervals = ACCUM COUNT   vLRULocks
           MinResults.LRULocks       = ACCUM MINIMUM vLRULocks
           MinResults.LRUNaps        = ACCUM MINIMUM vLRUNaps 
           MinResults.BUFLocks       = ACCUM MINIMUM vBUFLocks
           MinResults.BUFNaps        = ACCUM MINIMUM vBUFNaps 
           MinResults.BHTLocks       = ACCUM MINIMUM vBHTLocks
           MinResults.BHTNaps        = ACCUM MINIMUM vBHTNaps 
           MinResults.PerfIntervals  = ACCUM COUNT   vRecordReads
           MinResults.RecordReads    = ACCUM MINIMUM vRecordReads
           MinResults.LatchTimeouts  = ACCUM MINIMUM vLatchTimeouts

           MaxResults.SpinValue      = vSpinValue  
           MaxResults.NapValue       = vNapValue   
           MaxResults.NapmaxValue    = vNapmaxValue
           MaxResults.LatchIntervals = ACCUM COUNT   vLRULocks
           MaxResults.LRULocks       = ACCUM MAXIMUM vLRULocks
           MaxResults.LRUNaps        = ACCUM MAXIMUM vLRUNaps 
           MaxResults.BUFLocks       = ACCUM MAXIMUM vBUFLocks
           MaxResults.BUFNaps        = ACCUM MAXIMUM vBUFNaps 
           MaxResults.BHTLocks       = ACCUM MAXIMUM vBHTLocks
           MaxResults.BHTNaps        = ACCUM MAXIMUM vBHTNaps 
           MaxResults.PerfIntervals  = ACCUM COUNT   vRecordReads
           MaxResults.RecordReads    = ACCUM MAXIMUM vRecordReads
           MaxResults.LatchTimeouts  = ACCUM MAXIMUM vLatchTimeouts
    . /* ASSIGN */
  END.
END PROCEDURE. /* PromonStat */

/* -------------------------------------------------------------------------- */
PROCEDURE ProtraceStat.
  DEFINE INPUT PARAMETER  ipProtraceDir AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vProtraceCount   AS INTEGER  NO-UNDO.
  DEFINE VARIABLE vProtraceMinDate AS DATE     NO-UNDO.
  DEFINE VARIABLE vProtraceMaxDate AS DATE     NO-UNDO.
  DEFINE VARIABLE vProtraceMinTime AS INTEGER  NO-UNDO.
  DEFINE VARIABLE vProtraceMaxTime AS INTEGER  NO-UNDO.

  DEFINE VARIABLE vModuleNumber   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vModuleId       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vModuleIdList   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vModuleListHead AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vModuleListTail AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vProtraceDate   AS DATE      NO-UNDO.
  DEFINE VARIABLE vProtraceTime   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vFileName       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFilePath       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFileAttr       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vItem           AS CHARACTER NO-UNDO EXTENT 10.

  ASSIGN
    vModuleId        = 0
    vProtraceMinDate = 12/31/9999
    vProtraceMaxDate = 01/01/001
    vProtraceMinTime = 24 * 3600
    vProtraceMaxTime = 0
  . /* ASSIGN */

  INPUT STREAM ProtraceDir FROM OS-DIR(ipProtraceDir).
  REPEAT:
    IMPORT STREAM ProtraceDir vFileName vFilePath vFileAttr.

    IF NOT vFileAttr MATCHES "*F*":U
    OR NOT vFileName MATCHES "protrace.*":U THEN
    NEXT.

    INPUT FROM VALUE(vFilePath).

    ASSIGN
      vModuleIdList   = "":U
      vModuleListHead = "":U
      vModuleListTail = "":U
      vProtraceDate   = ?
      vProtraceTime   = ?
    . /* ASSIGN */

ProtraceLine:
    REPEAT:
      ASSIGN vItem = "".
      IMPORT vItem.
/* #1   [0x96a343] uttraceback+0x144  from /usr/dlc/bin/_progres */
 
      IF vItem[1] EQ "":U THEN
      NEXT ProtraceLine.
      ELSE
      IF vItem[1] BEGINS "#":U THEN
      DO TRANSACTION:

        ASSIGN vModuleNumber = INTEGER(SUBSTRING(vItem[1], 2)) NO-ERROR.
        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
        LEAVE ProtraceLine. /* Stop parcing the protrace file. */

        FIND FIRST ttModule EXCLUSIVE
             WHERE ttModule.ModuleName    EQ vItem[3]
               AND ttModule.ModuleLibrary EQ vItem[5]
        NO-ERROR.
        IF AVAILABLE ttModule THEN
        ASSIGN ttModule.ModuleCount = ttModule.ModuleCount + 1.
        ELSE
        DO:
          CREATE ttModule.
          ASSIGN vModuleId              = vModuleId + 1
                 ttModule.ModuleId      = vModuleId
                 ttModule.ModuleName    = vItem[3]
                 ttModule.ModuleLibrary = vItem[5]
                 ttModule.ModuleCount   = 1
          . /* ASSIGN */
        END.

        ASSIGN
          vModuleListHead = vModuleListHead + ",":U + ttModule.ModuleName
                            WHEN vModuleNumber LE 6
          vModuleListTail = vModuleListTail + ",":U + ttModule.ModuleName
                            WHEN vModuleNumber GT 6
          vModuleIdList   = vModuleIdList + ",":U + STRING(ttModule.ModuleId)
        . /* ASSIGN */
      END.
      ELSE

/* PROGRESS stack trace as of Thu Oct  6 14:18:01 2011
   1        2     3     4  5  6   7    8 9        10 */
      IF  vItem[1] EQ "PROGRESS"
      AND vItem[2] EQ "stack"
      AND vItem[3] EQ "trace"
      AND vItem[4] EQ "as"
      AND vItem[5] EQ "of" THEN
      ASSIGN
        vProtraceDate = DATE(LOOKUP(vItem[7],
                        "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec":U),
                        INTEGER(vItem[8]),
                        INTEGER(vItem[10]))
        vProtraceTime = INTEGER(ENTRY(1, vItem[9], ":")) * 3600 +
                        INTEGER(ENTRY(2, vItem[9], ":")) * 60 +
                        INTEGER(ENTRY(3, vItem[9], ":"))
      . /* ASSIGN */
    END. /* ProtraceLine: REPEAT */
    INPUT CLOSE.

    IF vModuleIdList   EQ "":U
    OR vProtraceDate EQ ?
    OR vProtraceTime EQ ? THEN
    NEXT. /* protrace file */

    ASSIGN
      vProtraceCount   = vProtraceCount + 1
      vModuleIdList    = SUBSTRING(vModuleIdList, 2)
      vProtraceMinDate = MIN(vProtraceMinDate, vProtraceDate)
      vProtraceMaxDate = MAX(vProtraceMaxDate, vProtraceDate)
      vProtraceMinTime = MIN(vProtraceMinTime, vProtraceTime)
      vProtraceMaxTime = MAX(vProtraceMaxTime, vProtraceTime)
    . /* ASSIGN */

    DO TRANSACTION:
      FIND FIRST ttProtraceType EXCLUSIVE
           WHERE ttProtraceType.ModuleIdList EQ vModuleIdList
      NO-ERROR.
      IF AVAILABLE ttProtraceType THEN
      ASSIGN ttProtraceType.TypeCount = ttProtraceType.TypeCount + 1.
      ELSE
      DO:
        CREATE ttProtraceType.
        ASSIGN ttProtraceType.TypeCount = 1
               ttProtraceType.ModuleIdList = vModuleIdList
               ttProtraceType.ModuleListHead = SUBSTRING(vModuleListHead, 2)
               ttProtraceType.ModuleListTail = SUBSTRING(vModuleListTail, 2)
        . /* ASSIGN */
      END. /* not AVAILABLE ttProtraceType */
    END. /* DO TRANSACTION */
  END. /* ProtraceDir: REPEAT */
  INPUT STREAM ProtraceDir CLOSE.

END PROCEDURE. /* ProtraceStat */

/* -------------------------------------------------------------------------- */
PROCEDURE ImportTestResults.

  DEFINE INPUT PARAMETER ipResultDir AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE vFileName AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFilePath AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFileAttr AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vDbLogFile       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vReadProbeFile   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vPromonLogFile   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vCPUStatFile     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vProtraceDirList AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE vLastLoginDate   AS DATE      NO-UNDO.
  DEFINE VARIABLE vLastLoginTime   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vFirstLogoutDate AS DATE      NO-UNDO.
  DEFINE VARIABLE vFirstLogoutTime AS INTEGER   NO-UNDO.

  ASSIGN vDbLogFile       = "":U
         vReadProbeFile   = "":U
         vPromonLogFile   = "":U
         vCPUStatFile     = "":U
         vProtraceDirList = "":U
  . /* ASSIGN */

  INPUT FROM OS-DIR(ipResultDir).
  REPEAT:
    IMPORT  vFileName vFilePath vFileAttr.

    IF  vFileAttr MATCHES "*D*":U
    AND vFileName NE  ".":U
    AND vFileName NE "..":U 
    AND vFileName BEGINS "protrace":U THEN
    ASSIGN vProtraceDirList = vProtraceDirList + MIN(vProtraceDirList, ",") 
                            + vFilePath
    . /* ASSIGN */
    ELSE

    IF vFileAttr MATCHES "*F*":U THEN
    IF vFileName MATCHES "sports.lg":U THEN
    ASSIGN vDbLogFile = vFilePath.
    ELSE

    IF vFileName MATCHES "*.rp":U THEN
    ASSIGN vReadProbeFile = vFilePath.
    ELSE

    IF vFileName MATCHES "sports.promon.*_*.log":U THEN
    ASSIGN vPromonLogFile = vFilePath.
    ELSE

    IF vFileName MATCHES "iostat-k-t.*_*.log":U THEN
    ASSIGN vCPUStatFile = vFilePath.
  END. /* REPEAT FROM OS-DIR(ipResultDir) */
  INPUT CLOSE.

  IF /*vDbLogFile       EQ "":U
OR*/ vReadProbeFile   EQ "":U 
  OR vPromonLogFile   EQ "":U 
  OR vCPUStatFile     EQ "":U 
  /*OR vProtraceDirList EQ "":U*/ THEN
  DO:
    MESSAGE 
      "ResultDir:" ipResultDir            SKIP
      "missed one or more files."         SKIP
      "DbLogFile      :" vDbLogFile       SKIP
      "ReadProbeFile  :" vReadProbeFile   SKIP
      "PromonFile     :" vPromonLogFile   SKIP
      "CPUStatFile    :" vCPUStatFile     SKIP
      "ProtraceDirList:" vProtraceDirList
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
  END.

  DO TRANSACTION:
    ASSIGN vTestId = vTestId + 1.
    CREATE AveResults.
    ASSIGN AveResults.TestId = vTestId
           AveResults.ResultType = "AVERAGE":U.
    CREATE MinResults.
    ASSIGN MinResults.TestId = vTestId
           MinResults.ResultType = "MINIMUM":U.
    CREATE MaxResults.
    ASSIGN MaxResults.TestId = vTestId
           MaxResults.ResultType = "MAXIMUM":U.
  END.

  IF vDbLogFile NE "":U THEN
  RUN DbLogInfo(INPUT vDbLogFile,
               OUTPUT vLastLoginDate,
               OUTPUT vLastLoginTime,
               OUTPUT vFirstLogoutDate,
               OUTPUT vFirstLogoutTime).
  ELSE
  RUN DbLogInfoFromPromon(INPUT vPromonLogFile,
                         OUTPUT vLastLoginDate,
                         OUTPUT vLastLoginTime,
                         OUTPUT vFirstLogoutDate,
                         OUTPUT vFirstLogoutTime).

  RUN ReadProbeInfo(INPUT vReadProbeFile).

  RUN PromonStat(INPUT vPromonLogFile,
                 INPUT vLastLoginDate,
                 INPUT vLastLoginTime,
                 INPUT vFirstLogoutDate,
                 INPUT vFirstLogoutTime).

  RUN CPUStat(INPUT vCPUStatFile,
              INPUT vLastLoginTime,
              INPUT vFirstLogoutTime).
  
  DEFINE VARIABLE vProtraceDirId  AS INTEGER NO-UNDO.
  DEFINE VARIABLE vTypeId AS INTEGER NO-UNDO.

  DEFINE VARIABLE vUtnapCount        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vProtraceCount     AS INTEGER   NO-UNDO EXTENT 3.
  DEFINE VARIABLE vModuleList        AS CHARACTER NO-UNDO EXTENT 3.
  DEFINE VARIABLE vAveModuleList     AS CHARACTER NO-UNDO EXTENT 3.
  DEFINE VARIABLE vMinProtraceCount  AS INTEGER   NO-UNDO EXTENT 3.
  DEFINE VARIABLE vMinModuleList     AS CHARACTER NO-UNDO EXTENT 3.
  DEFINE VARIABLE vMaxProtraceCount  AS INTEGER   NO-UNDO EXTENT 3.
  DEFINE VARIABLE vMaxModuleList     AS CHARACTER NO-UNDO EXTENT 3.
  DEFINE VARIABLE vRestProtraceCount AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vRestProtraceTypes AS INTEGER   NO-UNDO.

  REPEAT vProtraceDirId = 1 TO NUM-ENTRIES(vProtraceDirList):

    EMPTY TEMP-TABLE ttModule.
    EMPTY TEMP-TABLE ttProtraceType.

    ASSIGN vFilePath = ENTRY(vProtraceDirId, vProtraceDirList).

    RUN ProtraceStat(INPUT vFilePath).
  
    FOR FIRST ttModule NO-LOCK
        WHERE ttModule.ModuleName BEGINS "utnap+":U:

      ASSIGN vUtnapCount = ttModule.ModuleCount.
    END.
  
    ASSIGN vTypeId   = 0
           vMinProtraceCount = 1000000
           vMaxProtraceCount = 0
           vAveModuleList = ?
    . /* ASSIGN */

    FOR EACH ttProtraceType NO-LOCK
          BY ttProtraceType.TypeCount DESCENDING:
  
      ASSIGN vTypeId = vTypeId + 1.
  
      IF vTypeId LE EXTENT(vProtraceCount) THEN
      DO:
        ASSIGN
          vProtraceCount[vTypeId] = ttProtraceType.TypeCount
          vModuleList   [vTypeId] =
            IF ttProtraceType.ModuleIdList BEGINS "1,2,3,4,5,6,":U
            THEN ttProtraceType.ModuleListTail
            ELSE ("Full:" +
                 ttProtraceType.ModuleListHead + ",":U +
                 ttProtraceType.ModuleListTail)
        . /* ASSIGN */

        IF vAveModuleList[vTypeId] EQ ? THEN
        ASSIGN vAveModuleList[vTypeId] = vModuleList[vTypeId].
        ELSE
        IF vAveModuleList[vTypeId] NE vModuleList[vTypeId] THEN
        ASSIGN vAveModuleList[vTypeId] = "".

        IF vProtraceCount[vTypeId] LT vMinProtraceCount[vTypeId] THEN
        ASSIGN vMinProtraceCount[vTypeId] = vProtraceCount[vTypeId]
               vMinModuleList   [vTypeId] = vModuleList   [vTypeId]
        . /* ASSIGN */

        IF vProtraceCount[vTypeId] GT vMaxProtraceCount[vTypeId] THEN
        ASSIGN vMaxProtraceCount[vTypeId] = vProtraceCount[vTypeId]
               vMaxModuleList   [vTypeId] = vModuleList   [vTypeId]
        . /* ASSIGN */
      END.
      ELSE
      ACCUMULATE ttProtraceType.TypeCount (TOTAL COUNT).

    END. /* FOR EACH ttProtraceType */

    ASSIGN vRestProtraceCount = ACCUM TOTAL ttProtraceType.TypeCount
           vRestProtraceTypes = ACCUM COUNT ttProtraceType.TypeCount
    . /* ASSIGN */

    ACCUMULATE vUtnapCount        (AVERAGE MINIMUM MAXIMUM)
               vProtraceCount     (AVERAGE)
               vRestProtraceCount (AVERAGE MINIMUM MAXIMUM)
               vRestProtraceTypes (AVERAGE MINIMUM MAXIMUM)
    . /* ACCUMULATE */
  END. /* REPEAT vProtraceDirId = */

  DO TRANSACTION:
    ASSIGN AveResults.UtnapCount        = ACCUM AVERAGE vUtnapCount
           AveResults.ProtraceCount[1]  = ACCUM AVERAGE vProtraceCount[1]
           AveResults.ProtraceCount[2]  = ACCUM AVERAGE vProtraceCount[2]
           AveResults.ProtraceCount[3]  = ACCUM AVERAGE vProtraceCount[3]
           AveResults.ModuleList        = vAveModuleList
           AveResults.RestProtraceCount = ACCUM AVERAGE vRestProtraceCount
           AveResults.RestProtraceTypes = ACCUM AVERAGE vRestProtraceTypes

           MinResults.UtnapCount        = ACCUM MINIMUM vUtnapCount
           MinResults.ProtraceCount     = vMinProtraceCount
           MinResults.ModuleList        = vMinModuleList
           MinResults.RestProtraceCount = ACCUM MINIMUM vRestProtraceCount
           MinResults.RestProtraceTypes = ACCUM MINIMUM vRestProtraceTypes

           MaxResults.UtnapCount        = ACCUM MAXIMUM vUtnapCount
           MaxResults.ProtraceCount     = vMaxProtraceCount
           MaxResults.ModuleList        = vMaxModuleList
           MaxResults.RestProtraceCount = ACCUM MAXIMUM vRestProtraceCount
           MaxResults.RestProtraceTypes = ACCUM MAXIMUM vRestProtraceTypes
    . /* ASSIGN */
  END.

END PROCEDURE. /* ImportTestResults */

/* -------------------------------------------------------------------------- */
PROCEDURE ImportMultiTestResults.

  DEFINE INPUT PARAMETER ipBaseDir AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE vFileName AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFilePath AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFileAttr AS CHARACTER NO-UNDO.

  INPUT FROM OS-DIR(ipBaseDir).
  REPEAT:
    IMPORT vFileName vFilePath vFileAttr.

    IF  vFileAttr MATCHES "*D*":U
    AND vFileName NE  ".":U
    AND vFileName NE "..":U THEN
    RUN ImportTestResults(INPUT vFilePath).
  END.
  INPUT CLOSE.
END PROCEDURE. /* ImportTestResults */


PROCEDURE SaveReport.

  DEFINE INPUT PARAMETER ipOutputFile AS CHARACTER NO-UNDO.
  OUTPUT TO VALUE(ipOutputFile).

  PUT UNFORMATTED
           "TestId" 
    {&Sep} "ResultType"
    {&Sep} "Date"
    {&Sep} "BgnTime"
    {&Sep} "EndTime"
    {&Sep} "Intervals"
    {&Sep} "CPUs"
    {&Sep} "UserCount"
    {&Sep} "Spin"
    {&Sep} "Nap"
    {&Sep} "Napmax"
    {&Sep} "LRU Enabled"
    {&Sep} "Mux"
    {&Sep} "Record Reads"
    {&Sep} "CPU Usr"
    {&Sep} "CPU Sys"
    {&Sep} "CPU Idl"
    {&Sep} "Latch Timeouts"
    {&Sep} "LRU Locks"
    {&Sep} "LRU Naps "
    {&Sep} "BUF Locks"
    {&Sep} "BUF Naps"
    {&Sep} "BHT Locks"
    {&Sep} "BHT Naps"
    {&Sep} "Utnap Count"
    {&Sep} "ProtraceCount1"
    {&Sep} "ModuleList1"
    {&Sep} "ProtraceCount2"
    {&Sep} "ModuleList2"
    {&Sep} "ProtraceCount3"
    {&Sep} "ModuleList3"
    {&Sep} "RestProtraceCount"
    {&Sep} "RestProtraceTypes"
    SKIP
  . /* PUT */

  FOR EACH ttTestResults NO-LOCK:
    PUT UNFORMATTED
             /*TestId           */ ttTestResults.TestId    
      {&Sep} /*ResultType       */ ttTestResults.ResultType
      {&Sep} /*Date             */
        IF ttTestResults.BgnDate EQ ttTestResults.EndDate
        THEN STRING(ttTestResults.BgnDate, "99/99/9999":U)
        ELSE STRING(ttTestResults.BgnDate, "99/99/9999":U) + "!":U +
             STRING(ttTestResults.EndDate, "99/99/9999":U)
      {&Sep} /*BgnTime          */ STRING(ttTestResults.BgnTime,"HH:MM:SS":U)   
      {&Sep} /*EndTime          */ STRING(ttTestResults.EndTime,"HH:MM:SS":U)
      {&Sep} /*Intervals        */ 
        IF  ttTestResults.CPUIntervals EQ ttTestResults.LatchIntervals
        AND ttTestResults.CPUIntervals EQ ttTestResults.PerfIntervals
        THEN STRING(ttTestResults.CPUIntervals)
        ELSE STRING(ttTestResults.CPUIntervals)   + "!":U +
             STRING(ttTestResults.LatchIntervals) + "!":U +
             STRING(ttTestResults.PerfIntervals)
      {&Sep} /*CPUs             */ ttTestResults.NumOfCPUs
      {&Sep} /*UserCount        */ ttTestResults.UserCount  
      {&Sep} /*Spin             */ ttTestResults.SpinValue  
      {&Sep} /*Nap              */ ttTestResults.NapValue   
      {&Sep} /*Napmax           */ ttTestResults.NapmaxValue
      {&Sep} /*LRU Enabled      */ ttTestResults.LruEnabled 
      {&Sep} /*Mux              */ ttTestResults.MuxValue   
      {&Sep} /*Record Reads     */ ttTestResults.RecordReads  
      {&Sep} /*CPU Usr          */ ttTestResults.CPUUsr  
      {&Sep} /*CPU Sys          */ ttTestResults.CPUSys  
      {&Sep} /*CPU Idl          */ ttTestResults.CPUIdl  
      {&Sep} /*Latch Timeouts   */ ttTestResults.LatchTimeouts
      {&Sep} /*LRU Locks        */ ttTestResults.LRULocks
      {&Sep} /*LRU Naps         */ ttTestResults.LRUNaps 
      {&Sep} /*BUF Locks        */ ttTestResults.BUFLocks
      {&Sep} /*BUF Naps         */ ttTestResults.BUFNaps 
      {&Sep} /*BHT Locks        */ ttTestResults.BHTLocks
      {&Sep} /*BHT Naps         */ ttTestResults.BHTNaps 
      {&Sep} /*Utnap Count      */ ttTestResults.UtnapCount
      {&Sep} /*ProtraceCount1   */ ttTestResults.ProtraceCount[1]
      {&Sep} /*ModuleList1      */ ttTestResults.ModuleList   [1]
      {&Sep} /*ProtraceCount2   */ ttTestResults.ProtraceCount[2]
      {&Sep} /*ModuleList2      */ ttTestResults.ModuleList   [2]
      {&Sep} /*ProtraceCount3   */ ttTestResults.ProtraceCount[3]
      {&Sep} /*ModuleList3      */ ttTestResults.ModuleList   [3]
      {&Sep} /*RestProtraceCount*/ ttTestResults.RestProtraceCount
      {&Sep} /*RestProtraceTypes*/ ttTestResults.RestProtraceTypes
      SKIP
    . /* PUT */
  END.
  OUTPUT CLOSE.
  MESSAGE "Open in Excel:" ipOutputFile
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE. /* SaveReport */

/* -------------------------------------------------------------------------- */
/*
RUN ImportTestResults(
"D:\Support\BIS\2011.09.29 - Readprobe Tests\rp\10coresx99sessions").

RUN ImportTestResults(
  "D:\Support\BIS\2011.09.29 - Readprobe Tests\CPU#100\usrs10.spin10.nap10.napmax250.111010_104514").
*/

RUN ImportMultiTestResults(
"D:\Support\BIS\2011.09.29 - Readprobe Tests\rp").

RUN ImportMultiTestResults(
"D:\Support\BIS\2011.09.29 - Readprobe Tests\CPU#10").

RUN ImportMultiTestResults(
"D:\Support\BIS\2011.09.29 - Readprobe Tests\CPU#100").

RUN ImportMultiTestResults(
"D:\Support\BIS\2011.09.29 - Readprobe Tests\CPU#100-znolru").

RUN SaveReport(
"D:\Support\BIS\2011.09.29 - Readprobe Tests\ReadProbeStat.txt").
