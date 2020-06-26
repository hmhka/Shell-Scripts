DEFINE VARIABLE vDbLog  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vLkHash AS CHARACTER NO-UNDO.
DEFINE VARIABLE vItem   AS CHARACTER NO-UNDO EXTENT 14.
DEFINE VARIABLE vETime1 AS INTEGER   NO-UNDO.
DEFINE VARIABLE vETime2 AS INTEGER   NO-UNDO.
DEFINE VARIABLE vETime3 AS INTEGER   NO-UNDO.
DEFINE VARIABLE vETime4 AS INTEGER   NO-UNDO.
DEFINE VARIABLE vRecCount  AS INTEGER   NO-UNDO.

DEFINE VARIABLE vCurrLKP_Locks LIKE _Latch._Latch-Lock NO-UNDO.
DEFINE VARIABLE vCurrLKP_Waits LIKE _Latch._Latch-Wait NO-UNDO.
DEFINE VARIABLE vCurrLKT_Locks LIKE _Latch._Latch-Lock NO-UNDO.
DEFINE VARIABLE vCurrLKT_Waits LIKE _Latch._Latch-Wait NO-UNDO.

DEFINE VARIABLE vPrevLKP_Locks LIKE _Latch._Latch-Lock NO-UNDO.
DEFINE VARIABLE vPrevLKP_Waits LIKE _Latch._Latch-Wait NO-UNDO.
DEFINE VARIABLE vPrevLKT_Locks LIKE _Latch._Latch-Lock NO-UNDO.
DEFINE VARIABLE vPrevLKT_Waits LIKE _Latch._Latch-Wait NO-UNDO.

&SCOPED-DEFINE Sep "~t"

PROCEDURE LockLatches.
  DEFINE OUTPUT PARAMETER opLKP_Locks LIKE _Latch._Latch-Lock NO-UNDO.
  DEFINE OUTPUT PARAMETER opLKP_Waits LIKE _Latch._Latch-Wait NO-UNDO.
  DEFINE OUTPUT PARAMETER opLKT_Locks LIKE _Latch._Latch-Lock NO-UNDO.
  DEFINE OUTPUT PARAMETER opLKT_Waits LIKE _Latch._Latch-Wait NO-UNDO.

  ASSIGN opLKP_Locks = 0
         opLKP_Waits = 0
         opLKT_Locks = 0
         opLKT_Waits = 0.
  FOR EACH _Latch NO-LOCK:
  
    IF _Latch-Name MATCHES "*_LKP" THEN
    ASSIGN opLKP_Locks = opLKP_Locks + _Latch._Latch-Lock
           opLKP_Waits = opLKP_Waits + _Latch._Latch-Wait.
    ELSE
    IF _Latch-Name MATCHES "*_LHT*" THEN
    ASSIGN opLKT_Locks = opLKT_Locks + _Latch._Latch-Lock
           opLKT_Waits = opLKT_Waits + _Latch._Latch-Wait.
  END.
END PROCEDURE. /* LockLatches */

FOR FIRST _FileList NO-LOCK:
  ASSIGN 
    vDbLog = SUBSTRING(_FileList-Name, 1,LENGTH(_FileList-Name) - 3) + ".lg".
END.
FILE-INFO:FILE-NAME = vDbLog.
IF  FILE-INFO:FULL-PATHNAME NE ? 
AND FILE-INFO:FILE-SIZE LT 10000000 THEN
DO:
  INPUT FROM VALUE(FILE-INFO:FULL-PATHNAME).
  REPEAT:
    ASSIGN vItem = "".
    IMPORT vItem.
    IF vItem[7] EQ "(12815)" THEN
    ASSIGN vLkHash = vItem[14].
  END.
  INPUT CLOSE.
END.

FIND FIRST _Startup NO-LOCK.

ASSIGN vETime1 = ETIME.
FOR EACH _Connect NO-LOCK:
  ACCUMULATE "_Connect" (COUNT).
END.
ASSIGN vETime2 = ETIME
       vETime1 = vETime2 - vETime1
       vETime3 = vETime2 + 10000
       vRecCount = 0.

RUN LockLatches(OUTPUT vPrevLKP_Locks,
                OUTPUT vPrevLKP_Waits,
                OUTPUT vPrevLKT_Locks,
                OUTPUT vPrevLKT_Waits).
IF TRUE THEN
FOR EACH _UserLock NO-LOCK:
  ASSIGN vRecCount = vRecCount + 1.
END.
ELSE
REPEAT WHILE ETIME LT vETime3 TRANSACTION:
  FOR EACH _UserLock NO-LOCK:
    ASSIGN vRecCount = vRecCount + 1.
  END.
  /*
  FOR EACH customer NO-LOCK:
    ASSIGN vRecCount = vRecCount + 1.
  END.
  */
END.

ASSIGN vETime3 = ETIME
       vETime2 = vETime3 - vETime2.

RUN LockLatches(OUTPUT vCurrLKP_Locks,
                OUTPUT vCurrLKP_Waits,
                OUTPUT vCurrLKT_Locks,
                OUTPUT vCurrLKT_Waits).

/*
FOR EACH _Lock NO-LOCK WHILE _Lock-Id LE vRecCount:
  ACCUMULATE "_Lock" (COUNT).
END.
ASSIGN vETime3 = ETIME - vETime3.
*/
OUTPUT TO VALUE("UserLock.test.log") APPEND.
IF SEEK(OUTPUT) EQ 0 THEN
PUT UNFORMATTED
         "L" 
  {&Sep} "lkhash"
  {&Sep} "_Conn ETime"
  {&Sep} "Reads"
  {&Sep} "_UserLock ETime"
  {&Sep} "Reads" 
  {&Sep} "LKP Waits" 
  {&Sep} "LKP Locks" 
  {&Sep} "LKP/lkhash"
  {&Sep} "LKT Waits" 
  {&Sep} "LKT Locks" 
  {&Sep} "LKT/lkhash"
SKIP.
PUT UNFORMATTED
         /*L     */          _Startup-LockTable      
  {&Sep} /*lkhash*/          vLkHash
  {&Sep} /*_Conn ETime*/     vETime1
  {&Sep} /*Reads*/          (ACCUM COUNT "_Connect")
  {&Sep} /*_UserLock ETime*/ vETime2
  {&Sep} /*Reads*/           vRecCount
  {&Sep} /*LKP Waits*/       vCurrLKP_Waits - vPrevLKP_Waits
  {&Sep} /*LKP Locks*/       vCurrLKP_Locks - vPrevLKP_Locks
  {&Sep} /*LKP/lkhash*/      IF vLkHash EQ "" THEN ? ELSE
         (vCurrLKP_Locks - vPrevLKP_Locks) / INTEGER(vLkHash)
  {&Sep} /*LKT Waits*/       vCurrLKT_Waits - vPrevLKT_Waits
  {&Sep} /*LKT Locks*/       vCurrLKT_Locks - vPrevLKT_Locks
  {&Sep} /*LKT/lkhash*/      IF vLkHash EQ "" THEN ? ELSE
         (vCurrLKT_Locks - vPrevLKT_Locks) / INTEGER(vLkHash)
SKIP.
OUTPUT CLOSE.

MESSAGE 
  "-L:" _Startup-LockTable "-lkhash:" vLkHash  SKIP
  "_Connect :" vETime1 ACCUM COUNT "_Connect"  SKIP
  "_UserLock:" vETime2 vRecCount               SKIP
  "LKP"        vCurrLKP_Waits - vPrevLKP_Waits
               vCurrLKP_Locks - vPrevLKP_Locks 
  "LKP/lkhash" IF vLkHash EQ "" 
             THEN ? 
             ELSE (vCurrLKP_Locks - vPrevLKP_Locks) / INTEGER(vLkHash)
  SKIP
  "LKT"        vCurrLKT_Waits - vPrevLKT_Waits
               vCurrLKT_Locks - vPrevLKT_Locks 
  "LKT/lkhash" IF vLkHash EQ "" 
               THEN ? 
               ELSE (vCurrLKT_Locks - vPrevLKT_Locks) / INTEGER(vLkHash)
  SKIP
/*"_Lock    :" vETime3 ACCUM COUNT "_Lock" */
  VIEW-AS ALERT-BOX INFO BUTTONS OK.
