DEFINE NEW GLOBAL SHARED VARIABLE gvLkHash AS INTEGER NO-UNDO INITIAL ?.

DEFINE VARIABLE vRepeats AS INTEGER NO-UNDO INITIAL 1000000 /*2147483647*/.
DEFINE VARIABLE vLoop    AS INTEGER NO-UNDO.
DEFINE VARIABLE vNotes   AS CHARACTER NO-UNDO.

DEFINE VARIABLE vStatTime AS DATETIME NO-UNDO EXTENT 2.

DEFINE TEMP-TABLE ttResource NO-UNDO
  FIELD ResrcType AS CHARACTER /* "Latch", "Resrc", "TxeLock" */
  FIELD ResrcId   LIKE _Resrc._Resrc-Id
  FIELD ResrcName LIKE _Resrc._Resrc-Name
  FIELD ResrcLock LIKE _Resrc._Resrc-Lock EXTENT 2
  FIELD ResrcWait LIKE _Resrc._Resrc-Wait EXTENT 2
  INDEX ResrcId IS UNIQUE
        ResrcType
        ResrcId
. /* DEFINE TEMP-TABLE ttResource */


/* ------------------------------------------------------------------------- */

PROCEDURE GetStat:

  DEFINE INPUT PARAMETER ipStatId AS INTEGER NO-UNDO.

  DEFINE VARIABLE vMyUserNumber   AS INTEGER NO-UNDO.
  DEFINE VARIABLE vFirstStatID    AS INTEGER NO-UNDO.
  DEFINE VARIABLE vLastStatID     AS INTEGER NO-UNDO.
  DEFINE VARIABLE vTableRangeSize AS INTEGER NO-UNDO.
  DEFINE VARIABLE vHighestTableId AS INTEGER NO-UNDO.
  DEFINE VARIABLE vIndexRangeSize AS INTEGER NO-UNDO.
  DEFINE VARIABLE vHighestIndexId AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  ASSIGN vStatTime[ipStatId] = NOW.

/* _Latch */
  FOR EACH DICTDB._Latch NO-LOCK 
     WHERE DICTDB._Latch._Latch-Id GT 0
  TRANSACTION:
    FIND FIRST ttResource
         WHERE ttResource.ResrcType EQ "Latch"
           AND ttResource.ResrcId   EQ DICTDB._Latch._Latch-Id - 1
    NO-ERROR.
    IF NOT AVAILABLE ttResource THEN
    CREATE ttResource.
    ASSIGN ttResource.ResrcType = "Latch"
           ttResource.ResrcId   = DICTDB._Latch._Latch-Id - 1
           ttResource.ResrcName = SUBSTRING(DICTDB._Latch._Latch-Name, 5)
           ttResource.ResrcLock[ipStatId] = DICTDB._Latch._Latch-Lock
           ttResource.ResrcWait[ipStatId] = DICTDB._Latch._Latch-Wait
    . /* ASSIGN */
  END. /* FOR EACH _Latch */

/* _Resrc */
  FOR EACH DICTDB._Resrc NO-LOCK TRANSACTION:
    FIND FIRST ttResource
         WHERE ttResource.ResrcType EQ "Resrc"
           AND ttResource.ResrcId   EQ DICTDB._Resrc._Resrc-Id
    NO-ERROR.
    IF NOT AVAILABLE ttResource THEN
    CREATE ttResource.
    ASSIGN ttResource.ResrcType = "Resrc"
           ttResource.ResrcId   = DICTDB._Resrc._Resrc-Id
           ttResource.ResrcName = TRIM(DICTDB._Resrc._Resrc-Name)
           ttResource.ResrcLock[ipStatId] = DICTDB._Resrc._Resrc-Lock
           ttResource.ResrcWait[ipStatId] = DICTDB._Resrc._Resrc-Wait
    . /* ASSIGN */
  END. /* FOR EACH _Resrc */

/* _TxeLock - one per sample */
  FOR FIRST DICTDB._TxeLock NO-LOCK:
    DO i = 1 TO EXTENT(DICTDB._TxeLock._Txe-Type)
    TRANSACTION:
      FIND FIRST ttResource
           WHERE ttResource.ResrcType EQ "TxeLock"
             AND ttResource.ResrcId   EQ i
      NO-ERROR.
      IF NOT AVAILABLE ttResource THEN
      CREATE ttResource.
      ASSIGN ttResource.ResrcType = "TxeLock"
             ttResource.ResrcId   = i
             ttResource.ResrcName = TRIM(DICTDB._TxeLock._Txe-Type[i])
             ttResource.ResrcLock[ipStatId] = DICTDB._TxeLock._Txe-Locks[i]
             ttResource.ResrcWait[ipStatId] = DICTDB._TxeLock._Txe-Waits[i]
      . /* ASSIGN */
    END.
  END. /* FOR FIRST _TxeLock */

  FOR FIRST DICTDB._MyConnection NO-LOCK:
    ASSIGN vMyUserNumber = DICTDB._MyConnection._MyConn-UserId.
  END.
  
  FOR FIRST DICTDB._UserIO NO-LOCK
      WHERE DICTDB._UserIO._UserIO-Id EQ vMyUserNumber + 1
  TRANSACTION:
    FIND FIRST ttResource
         WHERE ttResource.ResrcType EQ "UserIO"
           AND ttResource.ResrcId   EQ 0
    NO-ERROR.
    IF NOT AVAILABLE ttResource THEN
    CREATE ttResource.
    ASSIGN ttResource.ResrcType = "UserIO"
           ttResource.ResrcId   = 0
           ttResource.ResrcName = "DbAccess"
           ttResource.ResrcLock[ipStatId] = DICTDB._UserIO._UserIO-DbAccess
           ttResource.ResrcWait[ipStatId] = 0
    . /* ASSIGN */
  END.

  FOR LAST DICTDB._TableStat NO-LOCK:
    ASSIGN vTableRangeSize = RECID(DICTDB._TableStat).
  END.
  
  FOR EACH DICTDB._File NO-LOCK
        BY DICTDB._File._File-Number DESCENDING:
    ASSIGN vHighestTableId = DICTDB._File._File-Number.
    LEAVE.
  END.
  
  ASSIGN vFirstStatID = vTableRangeSize * vMyUserNumber
         vLastStatID  = vTableRangeSize + vFirstStatID
         vFirstStatID = vFirstStatID + 1
  . /* ASSIGN */
  
  FOR EACH  DICTDB._UserTableStat NO-LOCK
     WHERE  DICTDB._UserTableStat._UserTableStat-id   GE vFirstStatID
       AND  DICTDB._UserTableStat._UserTableStat-id   LE vLastStatID
       AND  DICTDB._UserTableStat._UserTableStat-Read NE 0,

      FIRST DICTDB._File NO-LOCK
      WHERE DICTDB._File._File-Number EQ DICTDB._UserTableStat._UserTableStat-Num

      WHILE DICTDB._UserTableStat._UserTableStat-Num LE vHighestTableId
  TRANSACTION:

    FIND FIRST ttResource
         WHERE ttResource.ResrcType EQ "UserTableStat"
           AND ttResource.ResrcId   EQ DICTDB._UserTableStat._UserTableStat-Num
    NO-ERROR.
    IF NOT AVAILABLE ttResource THEN
    CREATE ttResource.
    ASSIGN ttResource.ResrcType = "UserTableStat"
           ttResource.ResrcId   = DICTDB._UserTableStat._UserTableStat-Num
           ttResource.ResrcName = DICTDB._File._File-Name
           ttResource.ResrcLock[ipStatId] = DICTDB._UserTableStat._UserTableStat-Read
           ttResource.ResrcWait[ipStatId] = 0
    . /* ASSIGN */
  END. /* FOR EACH _UserTableStat */
  
  FOR LAST DICTDB._IndexStat NO-LOCK:
    ASSIGN vIndexRangeSize = RECID(DICTDB._IndexStat).
  END.
  
  FOR EACH DICTDB._Index NO-LOCK,
     FIRST DICTDB._File OF DICTDB._Index NO-LOCK 
     WHERE DICTDB._File._File-Number GT 0
       AND DICTDB._File._File-Number LT 32768
        BY DICTDB._Index._Idx-num DESCENDING:
    ASSIGN vHighestIndexId = DICTDB._Index._Idx-num.
    LEAVE.
  END.
  
  ASSIGN vFirstStatID = vIndexRangeSize * vMyUserNumber
         vLastStatID  = vIndexRangeSize + vFirstStatID
         vFirstStatID = vFirstStatID + 1
  . /* ASSIGN */
  
  FOR EACH  DICTDB._UserIndexStat NO-LOCK
     WHERE  DICTDB._UserIndexStat._UserIndexStat-id   GE vFirstStatID
       AND  DICTDB._UserIndexStat._UserIndexStat-id   LE vLastStatID
       AND  DICTDB._UserIndexStat._UserIndexStat-Read NE 0,

      FIRST DICTDB._Index NO-LOCK
      WHERE DICTDB._Index._Idx-num EQ DICTDB._UserIndexStat._UserIndexStat-Num,

      FIRST DICTDB._File OF DICTDB._Index NO-LOCK

      WHILE DICTDB._UserIndexStat._UserIndexStat-Num LE vHighestIndexId
  TRANSACTION:

    FIND FIRST ttResource
         WHERE ttResource.ResrcType EQ "UserIndexStat"
           AND ttResource.ResrcId   EQ DICTDB._UserIndexStat._UserIndexStat-Num
    NO-ERROR.
    IF NOT AVAILABLE ttResource THEN
    CREATE ttResource.
    ASSIGN ttResource.ResrcType = "UserIndexStat"
           ttResource.ResrcId   = DICTDB._UserIndexStat._UserIndexStat-Num
           ttResource.ResrcName = DICTDB._File._File-Name + ".":U
                                + DICTDB._Index._Index-Name
           ttResource.ResrcLock[ipStatId] = DICTDB._UserIndexStat._UserIndexStat-Read
           ttResource.ResrcWait[ipStatId] = 0
    . /* ASSIGN */
  END. /* FOR EACH _UserIndexStat */

  FOR EACH  DICTDB._ActBILog NO-LOCK
  TRANSACTION:

    FIND FIRST ttResource
         WHERE ttResource.ResrcType EQ "ActBILog"
           AND ttResource.ResrcId   EQ 0
    NO-ERROR.
    IF NOT AVAILABLE ttResource THEN
    CREATE ttResource.
    ASSIGN ttResource.ResrcType = "ActBILog"
           ttResource.ResrcId   = 0
           ttResource.ResrcName = "BI notes":U
           ttResource.ResrcLock[ipStatId] = DICTDB._ActBILog._BiLog-RecWriten
           ttResource.ResrcWait[ipStatId] = DICTDB._ActBILog._BiLog-BBuffWaits
    . /* ASSIGN */

    FIND FIRST ttResource
         WHERE ttResource.ResrcType EQ "ActBILog"
           AND ttResource.ResrcId   EQ 1
    NO-ERROR.
    IF NOT AVAILABLE ttResource THEN
    CREATE ttResource.
    ASSIGN ttResource.ResrcType = "ActBILog"
           ttResource.ResrcId   = 1
           ttResource.ResrcName = "Trans":U
           ttResource.ResrcLock[ipStatId] = DICTDB._ActBILog._BiLog-Trans
           ttResource.ResrcWait[ipStatId] = 0
    . /* ASSIGN */

    FIND FIRST ttResource
         WHERE ttResource.ResrcType EQ "ActBILog"
           AND ttResource.ResrcId   EQ 2
    NO-ERROR.
    IF NOT AVAILABLE ttResource THEN
    CREATE ttResource.
    ASSIGN ttResource.ResrcType = "ActBILog"
           ttResource.ResrcId   = 2
           ttResource.ResrcName = "BI Bytes":U
           ttResource.ResrcLock[ipStatId] = DICTDB._ActBILog._BiLog-BytesWrtn
           ttResource.ResrcWait[ipStatId] = 0
    . /* ASSIGN */

  END. /* FOR EACH _ActBILog */
  



END PROCEDURE. /* GetStat */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportStat:

  DEFINE INPUT PARAMETER ipLoops AS INTEGER NO-UNDO.

  DEFINE VARIABLE vLogFile AS CHARACTER NO-UNDO
    INITIAL "protest.resources.txt".

  DEFINE VARIABLE vRepeats AS INTEGER NO-UNDO.

/* Report the latch activity if the number of locks generated per iteration
   exceeds the threshold: */
  DEFINE VARIABLE vLockThreshold AS DECIMAL NO-UNDO INITIAL 0.5.

  &SCOPED-DEFINE Sep "~t"
  
  DEFINE VARIABLE vResrcLock LIKE _Resrc._Resrc-Lock          NO-UNDO.
  DEFINE VARIABLE vResrcWait LIKE _Resrc._Resrc-Wait          NO-UNDO.
  DEFINE VARIABLE vMaxUsers  LIKE _Startup._Startup-MaxUsers  NO-UNDO.
  DEFINE VARIABLE vBuffers   LIKE _Startup._Startup-Buffs     NO-UNDO.
  DEFINE VARIABLE vLruSkips  LIKE _Startup._Startup-LRU-Skips NO-UNDO.
  DEFINE VARIABLE vLockTable LIKE _Startup._Startup-LockTable NO-UNDO.
  DEFINE VARIABLE vExecTime  AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vProgram   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vActivity  AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE vHostName  AS CHARACTER NO-UNDO.

  IF OPSYS EQ "UNIX":U THEN
  DO: /* get UNIX host name */
    INPUT THROUGH uname -n.
    IMPORT vHostName.
    INPUT CLOSE.
  END.
  ELSE
  ASSIGN vHostName = OS-GETENV("COMPUTERNAME").

  ASSIGN vLogFile = SUBSTITUTE("protest.resources.&1.&2.txt",
                               /* &1 */ vHostName,
                               /* &1 */ PROVERSION)
  . /* ASSIGN */

  OUTPUT TO VALUE(vLogFile) APPEND.

  IF SEEK(OUTPUT) EQ 0 THEN
  PUT UNFORMATTED
           "Resrc Id"
    {&Sep} "Type"
    {&Sep} "Name"
    {&Sep} "Locks"
    {&Sep} "Waits"
    {&Sep} "Repeats"
    {&Sep} "Duration"
    {&Sep} "Program"
    {&Sep} "n"
    {&Sep} "B"
    {&Sep} "lruskips"
    {&Sep} "L"
    {&Sep} "lkhash"
    {&Sep} "Notes"
    {&Sep} "Progress"
    {&Sep} "Host"
    {&Sep} "Date"
  SKIP. /* PUT */

  ASSIGN vActivity = FALSE
         vProgram  = ENTRY(2, PROGRAM-NAME(1), " ":U)
         vExecTime = INTERVAL(vStatTime[2], vStatTime[1], "milliseconds":U)
                   / 1000.0
  . /* ASSIGN */

  FOR FIRST DICTDB._Startup NO-LOCK:
    ASSIGN vMaxUsers  = DICTDB._Startup._Startup-MaxUsers 
           vBuffers   = DICTDB._Startup._Startup-Buffs    
           vLruSkips  = DICTDB._Startup._Startup-LRU-Skips
           vLockTable = DICTDB._Startup._Startup-LockTable
    . /* ASSIGN */
  END.

  FOR EACH ttResource NO-LOCK:

    ASSIGN
      vResrcLock = (ttResource.ResrcLock[2] - ttResource.ResrcLock[1]) / ipLoops
      vResrcWait = (ttResource.ResrcWait[2] - ttResource.ResrcWait[1]) / ipLoops
    . /* ASSIGN */
    
    IF vResrcLock LT vLockThreshold THEN
    NEXT.

    ASSIGN vActivity = TRUE.
    PUT UNFORMATTED
             ttResource.ResrcId   /* Resrc Id  */
      {&Sep} ttResource.ResrcType /* Type      */
      {&Sep} ttResource.ResrcName /* Name      */
      {&Sep} vResrcLock           /* Locks     */
      {&Sep} vResrcWait           /* Waits     */
      {&Sep} ipLoops              /* Repeats   */
      {&Sep} vExecTime            /* Duration  */
      {&Sep} vProgram             /* Program   */
      {&Sep} vMaxUsers            /* -n        */
      {&Sep} vBuffers             /* -B        */
      {&Sep} vLruSkips            /* -lruskips */
      {&Sep} vLockTable           /* -L        */
      {&Sep} gvLkHash             /* -lkhash   */
      {&Sep} vNotes               /* Notes    */
      {&Sep} PROVERSION           /* Progress */
      {&Sep} vHostName            /* Host     */
      {&Sep} ISO-DATE(NOW)        /* Date     */
    SKIP. /* PUT */

  END. /* FOR EACH _Latch, FIRST ttResource */

  IF NOT vActivity THEN
  PUT UNFORMATTED
           0             /* Resrc Id */
    {&Sep} "---":U       /* Type     */
    {&Sep} "---":U       /* Name     */
    {&Sep} 0             /* Locks    */
    {&Sep} 0             /* Waits    */
    {&Sep} ipLoops       /* Repeats  */
    {&Sep} vExecTime     /* Duration */
    {&Sep} vProgram      /* Program  */
    {&Sep} vMaxUsers     /* -n        */
    {&Sep} vBuffers      /* -B        */
    {&Sep} vLruSkips     /* -lruskips */
    {&Sep} vLockTable    /* -L        */
    {&Sep} gvLkHash      /* -lkhash   */
    {&Sep} vNotes        /* Notes    */
    {&Sep} PROVERSION    /* Progress */
    {&Sep} vHostName     /* Host     */
    {&Sep} ISO-DATE(NOW) /* Date     */
  SKIP. /* PUT */

  OUTPUT CLOSE.

END PROCEDURE. /* SaveStat */

/* ------------------------------------------------------------------------- */

FUNCTION DbLogValue RETURNS CHARACTER (ipKeyword AS CHARACTER):

/* Return the text after keyword in the recent messages in db log.
For example: DbLogValue("-lkhash") will return 1237:
BROKER  0: (12815) Lock table hash table size (-lkhash): 1237
*/
  DEFINE VARIABLE vDbLog AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vValue AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vLine  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i      AS INTEGER   NO-UNDO.

  ASSIGN vDbLog = PDBNAME("DICTDB")
         vDbLog = IF vDbLog MATCHES "*~~~.db":U
                  THEN SUBSTRING(vDbLog, 1, LENGTH(vDbLog) - 3) + ".lg"
                  ELSE vDbLog + ".lg"
         vValue = ?
  . /* ASSIGN */
  
  INPUT FROM VALUE(vDbLog).
  REPEAT:
    IMPORT UNFORMATTED vLine.
    ASSIGN i = INDEX(vLine, ipKeyword).
    ASSIGN vValue = SUBSTRING(vLine, i) WHEN i GT 0.
  END.
  INPUT CLOSE.

  ASSIGN i = INDEX(vValue, " ":U)
         vValue = SUBSTRING(vValue, i + 1)
         vValue = TRIM(vValue)
  . /* ASSIGN */
  RETURN vValue.
END FUNCTION. /* DbLogValue */

/* ------------------------------------------------------------------------- */

ASSIGN gvLkHash = INTEGER(DbLogValue("-lkhash")) WHEN gvLkHash EQ ?.
