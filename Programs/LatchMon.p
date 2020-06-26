DEFINE VARIABLE vSampleIntrv  AS INTEGER NO-UNDO INITIAL 4.   /* sec */
DEFINE VARIABLE vSampleCount  AS INTEGER NO-UNDO INITIAL 4.
DEFINE VARIABLE vPollingIntrv AS DECIMAL NO-UNDO INITIAL 0.1. /* sec */

IF SESSION:BATCH-MODE THEN
ASSIGN vSampleIntrv  = INTEGER(ENTRY(1, SESSION:PARAMETER))
                       WHEN NUM-ENTRIES(SESSION:PARAMETER) GE 1
       vSampleCount  = INTEGER(ENTRY(2, SESSION:PARAMETER))
                       WHEN NUM-ENTRIES(SESSION:PARAMETER) GE 2
       vPollingIntrv = DECIMAL(ENTRY(3, SESSION:PARAMETER))
                       WHEN NUM-ENTRIES(SESSION:PARAMETER) GE 3
NO-ERROR. /* ASSIGN */

RUN LatchMon(vSampleIntrv, vSampleCount, vPollingIntrv).

/* ----------------------------------------------------------------------------
    File        : LatchMon.p
    Purpose     : Gather latch statistics for DICTDB database.

    Author(s)   : George Potemkin
    Created     : Oct 24, 2016
    Modified    : Oct 30, 2016
    Version     : 1.1

    The most recent version can be downloaded from here:
    ftp://ftp.progress-tech.ru/pub/Users/george/Programs/LatchMon.p

    Syntax      : See the examples above.

    Input parameters:
    1 = Sampling interval in the seconds;
    2 = Number of the sampling interval;
    3 = Polling interval in the seconds - how often to re-read the _Latch table

    Program creates the reports:
    1. LatchStat.<dbname>.txt - latch atcivity per sampling intervals;
    2. LatchHold.<dbname>.txt - latch atcivity per users
                                (only for the most active users);
    3. LatchLock.<dbname>.txt - long latch locks (longer than polling interval)
                                (created only when needed).
*/

/* ------------------------------------------------------------------------- */

DEFINE VARIABLE lvStatTime   AS DATETIME INITIAL ? EXTENT 3.
DEFINE VARIABLE lvStatCount  AS INTEGER  INITIAL 0.
DEFINE VARIABLE lvLastHoldId AS INTEGER  INITIAL 0.

DEFINE TEMP-TABLE ttLatch NO-UNDO
  FIELD LatchId    AS DECIMAL
  FIELD AccumId    AS DECIMAL INITIAL 0 /* Pseudo LatchId to aggregate stats*/
  FIELD LatchName  LIKE _Latch._Latch-Name
  FIELD LatchType  LIKE _Latch._Latch-Type
  FIELD LatchHold  LIKE _Latch._Latch-Hold  INITIAL -1
  FIELD LatchOwner LIKE _Latch._Latch-Owner INITIAL -1
  FIELD LatchBusy  AS INTEGER INITIAL 0
  FIELD HoldCount  AS INTEGER INITIAL 0
  FIELD LatchLock  LIKE _Latch._Latch-Lock EXTENT 3 INITIAL  0
  FIELD LatchWait  LIKE _Latch._Latch-Wait EXTENT 3 INITIAL  0
  INDEX LatchId    IS UNIQUE
        LatchId
  INDEX AccumId
        AccumId
. /* DEFINE TEMP-TABLE ttLatch */

DEFINE TEMP-TABLE ttLatchHold NO-UNDO
  FIELD HoldId        AS INTEGER
  FIELD ConnectUsr    LIKE _Connect._Connect-Usr
  FIELD ConnectDevice LIKE _Connect._Connect-Device
  FIELD ConnectTime   LIKE _Connect._Connect-Time
  FIELD ConnectPid    LIKE _Connect._Connect-Pid
  FIELD ConnectName   LIKE _Connect._Connect-Name
  FIELD ConnectType   LIKE _Connect._Connect-Type
  FIELD ConnectBatch  LIKE _Connect._Connect-Batch
  FIELD ConnectFlag   LIKE _Connect._Connect-Disconnect
  FIELD DbAccess      LIKE _UserIO._UserIO-DbAccess EXTENT 2
  FIELD DbRead        LIKE _UserIO._UserIO-DbRead   EXTENT 2
  FIELD DbWrite       LIKE _UserIO._UserIO-DbWrite  EXTENT 2
  FIELD BiRead        LIKE _UserIO._UserIO-BiRead   EXTENT 2
  FIELD BiWrite       LIKE _UserIO._UserIO-BiWrite  EXTENT 2
  FIELD AiRead        LIKE _UserIO._UserIO-AiRead   EXTENT 2
  FIELD AiWrite       LIKE _UserIO._UserIO-AiWrite  EXTENT 2
  FIELD StatTime      AS DATETIME                   EXTENT 2
  INDEX HoldId     IS UNIQUE
        HoldId
  INDEX ConnectUsr IS UNIQUE
        ConnectUsr
        ConnectDevice
        ConnectTime
        ConnectPid
. /* DEFINE TEMP-TABLE ttLatchHold */
/* ttLatchHold.ConnectFlag:
   1 = _Connect-Disconnect
   2 = _Connect-Resync 
   4 = _Connect-Interrupt
   8 = User is just disconnected
*/

DEFINE TEMP-TABLE ttLatchHoldStat NO-UNDO
  FIELD LatchId   AS DECIMAL
  FIELD HoldId    AS INTEGER
  FIELD HoldCount AS INTEGER
  INDEX UniqueKey IS UNIQUE
        HoldId
        LatchId
. /* DEFINE TEMP-TABLE ttLatchHoldStat */

DEFINE TEMP-TABLE ttLatchLock NO-UNDO
  FIELD LatchId     AS DECIMAL
  FIELD HoldId      AS INTEGER
  FIELD StatId      AS INTEGER              EXTENT 2
  FIELD StatTime    AS DATETIME             EXTENT 2
  FIELD LatchWait   LIKE _Latch._Latch-Wait EXTENT 2
  FIELD BiFullBuffs LIKE _Logging._Logging-BiFullBuffs
  FIELD BiBytesFree LIKE _Logging._Logging-BiBytesFree
  FIELD LastCkp     LIKE _Logging._Logging-LastCkp
  INDEX HoldLatch
        HoldId
        LatchId
. /* DEFINE TEMP-TABLE ttLatchLock */

DEFINE STREAM LatchStat.
DEFINE STREAM UserStat.
DEFINE STREAM LatchAlerts.

&SCOPED-DEFINE Sep "~t"

/* ------------------------------------------------------------------------- */

PROCEDURE LatchMon:

  DEFINE INPUT PARAMETER ipSampleIntrv AS INTEGER NO-UNDO. /* sec */
  DEFINE INPUT PARAMETER ipSampleCount AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipStatIntrv   AS DECIMAL NO-UNDO. /* sec */

  DEFINE VARIABLE vSampleCount AS INTEGER NO-UNDO.
  DEFINE VARIABLE vJumpIntrv   AS INTEGER NO-UNDO.

  DEFINE VARIABLE vInitStat    AS INTEGER NO-UNDO.
  DEFINE VARIABLE vPrevStat    AS INTEGER NO-UNDO.
  DEFINE VARIABLE vLastStat    AS INTEGER NO-UNDO.

  DEFINE VARIABLE vLastTime    AS DATETIME NO-UNDO.
  DEFINE VARIABLE vNextTime    AS DATETIME NO-UNDO.
  DEFINE VARIABLE vJumpTime    AS DATETIME NO-UNDO.

  RUN CreateLatchStat.

  ASSIGN vJumpIntrv = - ipStatIntrv * 1500 /* milliseconds */
         vLastStat = 1
         vPrevStat = 2
  . /* ASSIGN */

  RUN GetLatchStat(vLastStat).

  FOR FIRST DICTDB._ActSummary NO-LOCK:
    ASSIGN  lvStatTime[vPrevStat] = ADD-INTERVAL(lvStatTime[vLastStat],
                                  - DICTDB._ActSummary._Summary-UpTime,
                                    "seconds":U)
    . /* ASSIGN */
    RUN ReportLatchStat(vPrevStat, vLastStat, 0).
  END.

  DO vSampleCount = 1 TO ipSampleCount:

    EMPTY TEMP-TABLE ttLatchLock.
    EMPTY TEMP-TABLE ttLatchHold.
    EMPTY TEMP-TABLE ttLatchHoldStat.

    ASSIGN lvStatCount  = 0
           lvLastHoldId = 0
           vInitStat = vLastStat
           vLastTime = lvStatTime[vLastStat]
           vNextTime = ADD-INTERVAL(vLastTime, ipSampleIntrv, "seconds":U)
           vJumpTime = ADD-INTERVAL(vNextTime, vJumpIntrv, "milliseconds":U)
    . /* ASSIGN */

    DO WHILE vLastTime LT vJumpTime:

      PAUSE ipStatIntrv NO-MESSAGE.
      PROCESS EVENTS.

      IF vLastStat EQ vInitStat THEN
      ASSIGN vLastStat = vPrevStat
             vPrevStat = vInitStat
      . /* ASSIGN */
      ELSE
      ASSIGN vPrevStat = vLastStat
             vLastStat = 6 - vPrevStat - vInitStat
      . /* ASSIGN */

      RUN GetLatchStat               (vLastStat).
      RUN GetLatchHoldStat(vPrevStat, vLastStat).
      RUN GetLatchLock    (vPrevStat, vLastStat).

      ASSIGN vLastTime = lvStatTime[vLastStat].
    END. /* DO WHILE vLastTime LT vNextTime */

    PAUSE INTERVAL(vNextTime, vLastTime, "milliseconds":U) / 1000.0 NO-MESSAGE.

    IF vLastStat EQ vInitStat THEN
    ASSIGN vLastStat = vPrevStat
           vPrevStat = vInitStat
    . /* ASSIGN */
    ELSE
    ASSIGN vPrevStat = vLastStat
           vLastStat = 6 - vPrevStat - vInitStat
    . /* ASSIGN */

    RUN GetLatchStat               (vLastStat).
    RUN GetLatchHoldStat(vPrevStat, vLastStat).
    RUN GetLatchLock    (vPrevStat, vLastStat).

    RUN ReportLatchStat (vInitStat, vLastStat, vSampleCount).
    RUN ReportLatchHold (vInitStat, vLastStat, vSampleCount).
    RUN ReportLatchLock.

  END. /* DO vSampleCount */

END PROCEDURE. /* LatchMon */

/* ------------------------------------------------------------------------- */

FUNCTION DateTime2String RETURN CHARACTER (ipDateTime AS DATETIME):
  DEFINE VARIABLE vString AS CHARACTER NO-UNDO.
  ASSIGN vString = ISO-DATE(ipDateTime)
         vString = REPLACE(SUBSTRING(vString, 1, 10), "-":U, "/":U)
                 + "@":U + SUBSTRING(vString, 12)
  . /* ASSIGN */
  RETURN vString.
END FUNCTION. /* DateTime2String */

/* ------------------------------------------------------------------------- */

PROCEDURE CreateLatchStat:

  DEFINE VARIABLE vStatId    AS INTEGER NO-UNDO.
  DEFINE VARIABLE vAccumId   AS DECIMAL NO-UNDO.

  FOR EACH DICTDB._Latch NO-LOCK
     WHERE DICTDB._Latch._Latch-Id GE 2
  TRANSACTION:
    CREATE ttLatch.
    ASSIGN ttLatch.LatchId   = DICTDB._Latch._Latch-Id - 1
           ttLatch.LatchName = TRIM(SUBSTRING(DICTDB._Latch._Latch-Name,5))
           ttLatch.LatchType = TRIM(SUBSTRING(DICTDB._Latch._Latch-Type,7))
           ttLatch.AccumId   = 0
    . /* ASSIGN */
  END. /* FOR EACH _Latch */

/* "LHT*" is the aggregator of LHT, LHT2, LHT3 and LHT4 statistics --------- */
  FOR FIRST ttLatch NO-LOCK
      WHERE ttLatch.LatchName EQ "LHT4":U:
    ASSIGN vAccumId = ttLatch.LatchId + 0.5.
  END.

  DO TRANSACTION:
    FOR EACH ttLatch EXCLUSIVE-LOCK WHERE
      LOOKUP(ttLatch.LatchName, "LHT,LHT2,LHT3,LHT4":U) GT 0:
      ASSIGN ttLatch.AccumId = vAccumId.
    END.

    CREATE ttLatch.
    ASSIGN ttLatch.LatchName = "LHT*":U
           ttLatch.LatchId   = vAccumId
    . /* ASSIGN */
  END.

/* "BUF*" is the aggregator of BF1, BF2, BF3 and BF4 statistics: ----------- */
  FOR FIRST ttLatch NO-LOCK
      WHERE ttLatch.LatchName EQ "BF4":U:
    ASSIGN vAccumId = ttLatch.LatchId + 0.5.
  END.

  DO TRANSACTION:
    FOR EACH ttLatch EXCLUSIVE-LOCK WHERE
      LOOKUP(ttLatch.LatchName, "BF1,BF2,BF3,BF4":U) GT 0:
      ASSIGN ttLatch.AccumId = vAccumId.
    END.

    CREATE ttLatch.
    ASSIGN ttLatch.LatchName = "BUF*":U
           ttLatch.LatchId   = vAccumId
    . /* ASSIGN */
  END.

END PROCEDURE. /* CreateLatchStat */

/* ------------------------------------------------------------------------- */

PROCEDURE GetLatchStat:

  DEFINE INPUT PARAMETER ipStatId AS INTEGER NO-UNDO.

  DEFINE BUFFER bfLatch FOR ttLatch.

  DEFINE VARIABLE vStatTime   AS DATETIME              NO-UNDO.
  DEFINE VARIABLE vLatchId    LIKE _Latch._Latch-Id    NO-UNDO.
  DEFINE VARIABLE vLatchLock  LIKE _Latch._Latch-Lock  NO-UNDO EXTENT 32.
  DEFINE VARIABLE vLatchWait  LIKE _Latch._Latch-Wait  NO-UNDO EXTENT 32.
  DEFINE VARIABLE vLatchHold  LIKE _Latch._Latch-Hold  NO-UNDO EXTENT 32.
  DEFINE VARIABLE vLatchOwner LIKE _Latch._Latch-Owner NO-UNDO EXTENT 32.

/* Two phase method to copy data from _Latch VST to ttLatche temp-tables:
   First phase is to copy data to the arrays. An access to the arrays is
   a few hundreds times faster than a finding a temp-table's record.
   Hence we will get a snapshot as fast as possible.
   Second phase is to copy data to the ttLatch temp-table
   and the preliminary processing of the collected data.
 */
  FOR EACH DICTDB._Latch NO-LOCK
     WHERE DICTDB._Latch._Latch-Id GE 2:
    ASSIGN vLatchLock [DICTDB._Latch._Latch-Id] = DICTDB._Latch._Latch-Lock
           vLatchWait [DICTDB._Latch._Latch-Id] = DICTDB._Latch._Latch-Wait
           vLatchHold [DICTDB._Latch._Latch-Id] = DICTDB._Latch._Latch-Hold
           vLatchOwner[DICTDB._Latch._Latch-Id] = DICTDB._Latch._Latch-Owner
    . /* ASSIGN */
  END.

  ASSIGN lvStatCount = lvStatCount + 1
         lvStatTime[ipStatId] = NOW
  . /* ASSIGN */

  DO vLatchId = 2 TO 32:
    FOR FIRST ttLatch EXCLUSIVE-LOCK
        WHERE ttLatch.LatchId EQ vLatchId - 1
    TRANSACTION:
      ASSIGN  ttLatch.LatchLock[ipStatId] = vLatchLock[vLatchId]
              ttLatch.LatchWait[ipStatId] = vLatchWait[vLatchId]
              ttLatch.LatchHold  = vLatchHold [vLatchId]
              ttLatch.LatchOwner = vLatchOwner[vLatchId]
              ttLatch.LatchBusy  = ttLatch.LatchBusy + 1
                               WHEN vLatchOwner[vLatchId] GE 0
      . /* ASSIGN */
    END. /* FOR FIRST ttLatch */
  END. /* DO vLatchId */

/* Aggregation of statistics per ttLatch.AccumId: */
  FOR EACH ttLatch NO-LOCK
     WHERE ttLatch.AccumId GT 0
  BREAK BY ttLatch.AccumId:

    ACCUMULATE ttLatch.LatchLock[ipStatId] (SUB-TOTAL BY ttLatch.AccumId)
               ttLatch.LatchWait[ipStatId] (SUB-TOTAL BY ttLatch.AccumId)
    . /* ACCUMULATE */

    IF LAST-OF(ttLatch.AccumId) THEN
    FOR FIRST bfLatch EXCLUSIVE-LOCK
        WHERE bfLatch.LatchId EQ ttLatch.AccumId
    TRANSACTION:
      ASSIGN  bfLatch.LatchLock[ipStatId] = ACCUM SUB-TOTAL BY ttLatch.AccumId ttLatch.LatchLock[ipStatId]
              bfLatch.LatchWait[ipStatId] = ACCUM SUB-TOTAL BY ttLatch.AccumId ttLatch.LatchWait[ipStatId]
      . /* ASSIGN */
    END. /* FOR FIRST bfLatch */
  END. /* FOR EACH ttLatch */

END PROCEDURE. /* GetLatchStat */

/* ------------------------------------------------------------------------- */

FUNCTION LatchHoldId RETURNS INTEGER (ipLatchHold LIKE _Latch._Latch-Hold):

/* Use _Connect-Id to read the _Connect record almost instantly.
   Use _Connect-Usr to verify that a session is still connected:
*/
  FOR FIRST DICTDB._Connect NO-LOCK
      WHERE DICTDB._Connect._Connect-Id  EQ ipLatchHold + 1,

      FIRST DICTDB._UserIO NO-LOCK
      WHERE DICTDB._UserIO._UserIO-Id EQ DICTDB._Connect._Connect-Id:

/* User has disconnected during the last polling interval
   and the slot in the _Connect is not yet re-used:
*/  IF DICTDB._Connect._Connect-Usr EQ ? THEN
    FOR EACH ttLatchHold EXCLUSIVE-LOCK
       WHERE ttLatchHold.ConnectUsr  EQ ipLatchHold
         AND ttLatchHold.ConnectFlag LT 8
          BY ttLatchHold.ConnectTime DESCENDING
    TRANSACTION:
      ASSIGN ttLatchHold.ConnectFlag = ttLatchHold.ConnectFlag + 8
             ttLatchHold.StatTime[2] = NOW
      . /* ASSIGN */
      RETURN ttLatchHold.HoldId.
    END. /* FOR EACH ttLatchHold */
    ELSE

/* ttLatchHold already exists: */
    FOR FIRST ttLatchHold EXCLUSIVE-LOCK
        WHERE ttLatchHold.ConnectUsr    EQ DICTDB._Connect._Connect-Usr
          AND ttLatchHold.ConnectDevice EQ DICTDB._Connect._Connect-Device
          AND ttLatchHold.ConnectTime   EQ DICTDB._Connect._Connect-Time
          AND ttLatchHold.ConnectPid    EQ DICTDB._Connect._Connect-Pid
    TRANSACTION:
      ASSIGN ttLatchHold.DbAccess[2] = DICTDB._UserIO._UserIO-DbAccess
             ttLatchHold.DbRead  [2] = DICTDB._UserIO._UserIO-DbRead
             ttLatchHold.DbWrite [2] = DICTDB._UserIO._UserIO-DbWrite
             ttLatchHold.BiRead  [2] = DICTDB._UserIO._UserIO-BiRead
             ttLatchHold.BiWrite [2] = DICTDB._UserIO._UserIO-BiWrite
             ttLatchHold.AiRead  [2] = DICTDB._UserIO._UserIO-AiRead
             ttLatchHold.AiWrite [2] = DICTDB._UserIO._UserIO-AiWrite
             ttLatchHold.StatTime[2] = NOW
      . /* ASSIGN */
      RETURN ttLatchHold.HoldId.
    END. /* FOR FIRST ttLatchHold */

/* ttLatchHold does not exist: */
    IF DICTDB._Connect._Connect-Usr NE ? THEN
    DO TRANSACTION:
/* Begin of debug **
OUTPUT TO VALUE("LatchHold.debug.txt") APPEND.
PUT UNFORMATTED
  NOW
  {&Sep} lvLastHoldId + 1
  {&Sep} DICTDB._Connect._Connect-Usr
  {&Sep} DICTDB._Connect._Connect-Device
  {&Sep} DICTDB._Connect._Connect-Time
  {&Sep} DICTDB._Connect._Connect-Pid
  {&Sep} DICTDB._Connect._Connect-Name
  {&Sep} DICTDB._Connect._Connect-Type
  {&Sep} DICTDB._Connect._Connect-Batch
SKIP.
OUTPUT CLOSE.
** End of debug */
      CREATE ttLatchHold.
      ASSIGN lvLastHoldId              = lvLastHoldId + 1
             ttLatchHold.HoldId        = lvLastHoldId
             ttLatchHold.ConnectUsr    = DICTDB._Connect._Connect-Usr
             ttLatchHold.ConnectDevice = DICTDB._Connect._Connect-Device
             ttLatchHold.ConnectTime   = DICTDB._Connect._Connect-Time
             ttLatchHold.ConnectPid    = DICTDB._Connect._Connect-Pid
             ttLatchHold.ConnectName   = DICTDB._Connect._Connect-Name
             ttLatchHold.ConnectType   = DICTDB._Connect._Connect-Type
             ttLatchHold.ConnectBatch  = DICTDB._Connect._Connect-Batch
             ttLatchHold.ConnectFlag   = DICTDB._Connect._Connect-Disconnect
                                   + 2 * DICTDB._Connect._Connect-Resync 
                                   + 4 * DICTDB._Connect._Connect-Interrupt
             ttLatchHold.DbAccess      = DICTDB._UserIO._UserIO-DbAccess
             ttLatchHold.DbRead        = DICTDB._UserIO._UserIO-DbRead
             ttLatchHold.DbWrite       = DICTDB._UserIO._UserIO-DbWrite
             ttLatchHold.BiRead        = DICTDB._UserIO._UserIO-BiRead
             ttLatchHold.BiWrite       = DICTDB._UserIO._UserIO-BiWrite
             ttLatchHold.AiRead        = DICTDB._UserIO._UserIO-AiRead
             ttLatchHold.AiWrite       = DICTDB._UserIO._UserIO-AiWrite
             ttLatchHold.StatTime      = NOW
      . /* ASSIGN */
      RETURN ttLatchHold.HoldId.
    END. /* DO TRANSACTION */
  END. /* FOR FIRST _Connect, FIRST _UserIO */

/* User has connected and disconnected between the polling interval: */
  RETURN ?.
END FUNCTION. /* LatchHoldId */

/* ------------------------------------------------------------------------- */

PROCEDURE GetLatchHoldStat:

  DEFINE INPUT PARAMETER ipPrevStat AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipLastStat AS INTEGER NO-UNDO.

  DEFINE VARIABLE vLatchHoldId AS INTEGER NO-UNDO.

  IF ipLastStat EQ ipPrevStat THEN
  RETURN.

LatchStat:
  FOR EACH ttLatch NO-LOCK
     WHERE ttLatch.LatchHold GE 0:

    ASSIGN vLatchHoldId = ?.

    IF ttLatch.LatchLock[ipLastStat] NE ttLatch.LatchLock[ipPrevStat]
    THEN ASSIGN vLatchHoldId = LatchHoldId(ttLatch.LatchHold).
    ELSE ASSIGN vLatchHoldId = LatchHoldId(ttLatch.LatchOwner)
                                      WHEN ttLatch.LatchOwner GE 0.
      
    IF vLatchHoldId EQ ? THEN
    NEXT LatchStat.

    FOR FIRST ttLatchHoldStat EXCLUSIVE-LOCK
        WHERE ttLatchHoldStat.LatchId EQ ttLatch.LatchId
          AND ttLatchHoldStat.HoldId  EQ vLatchHoldId
    TRANSACTION:
      ASSIGN ttLatchHoldStat.HoldCount = ttLatchHoldStat.HoldCount + 1.
      NEXT LatchStat.
    END.

    DO TRANSACTION:
      CREATE ttLatchHoldStat.
      ASSIGN ttLatchHoldStat.LatchId   = ttLatch.LatchId
             ttLatchHoldStat.HoldId    = vLatchHoldId
             ttLatchHoldStat.HoldCount = 1
      . /* ASSIGN */
    END.
  END. /* FOR EACH ttLatch */

END PROCEDURE. /* GetLatchHoldStat */

/* ------------------------------------------------------------------------- */

PROCEDURE GetLatchLock:

  DEFINE INPUT PARAMETER ipPrevStat AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipLastStat AS INTEGER NO-UNDO.

  IF ipLastStat EQ ipPrevStat THEN
  RETURN.

LatchStat:
  FOR EACH ttLatch NO-LOCK
     WHERE ttLatch.LatchOwner GE 0
       AND ttLatch.LatchLock[ipLastStat]
        EQ ttLatch.LatchLock[ipPrevStat],

     FIRST DICTDB._Connect NO-LOCK
     WHERE DICTDB._Connect._Connect-Id EQ ttLatch.LatchOwner + 1,

     FIRST ttLatchHold NO-LOCK
     WHERE ttLatchHold.ConnectUsr    EQ DICTDB._Connect._Connect-Usr
       AND ttLatchHold.ConnectDevice EQ DICTDB._Connect._Connect-Device
       AND ttLatchHold.ConnectTime   EQ DICTDB._Connect._Connect-Time
       AND ttLatchHold.ConnectPid    EQ DICTDB._Connect._Connect-Pid:

    FOR FIRST ttLatchLock EXCLUSIVE-LOCK
        WHERE ttLatchLock.LatchId   EQ ttLatch.LatchId
          AND ttLatchLock.HoldId    EQ ttLatchHold.HoldId
          AND ttLatchLock.StatId[2] EQ lvStatCount - 1
    TRANSACTION:
      ASSIGN ttLatchLock.StatId[   2] = lvStatCount
             ttLatchLock.StatTime [2] = lvStatTime  [ipLastStat]
             ttLatchLock.LatchWait[2] = ttLatch.LatchWait[ipLastStat]
      . /* ASSIGN */
      NEXT LatchStat.
    END. /* FOR FIRST ttLatchLock */

    FOR FIRST DICTDB._Logging NO-LOCK
    TRANSACTION:
      CREATE ttLatchLock.
      ASSIGN ttLatchLock.LatchId      = ttLatch.LatchId
             ttLatchLock.HoldId       = ttLatchHold.HoldId
             ttLatchLock.StatId   [1] = lvStatCount - 1
             ttLatchLock.StatId   [2] = lvStatCount
             ttLatchLock.StatTime [1] = lvStatTime  [ipPrevStat]
             ttLatchLock.StatTime [2] = lvStatTime  [ipLastStat]
             ttLatchLock.LatchWait[1] = ttLatch.LatchWait[ipPrevStat]
             ttLatchLock.LatchWait[2] = ttLatch.LatchWait[ipLastStat]
             ttLatchLock.BiFullBuffs  = DICTDB._Logging._Logging-BiFullBuffs
             ttLatchLock.BiBytesFree  = DICTDB._Logging._Logging-BiBytesFree
             ttLatchLock.LastCkp      = DICTDB._Logging._Logging-LastCkp
      . /* ASSIGN */
    END. /* FOR FIRST _Logging */
  END. /* FOR EACH ttLatch, _Connect, ttLatchHold */

END PROCEDURE. /* GetLatchLock */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportLatchStat:

  DEFINE INPUT PARAMETER ipPrevStat AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipLastStat AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipSample   AS INTEGER NO-UNDO.

  DEFINE VARIABLE vIntrv AS DECIMAL NO-UNDO.

  OUTPUT TO VALUE(SUBSTITUTE("LatchStat.&1.txt", LDBNAME("DICTDB"))) APPEND.
  IF SEEK(OUTPUT) EQ 0 THEN
  PUT UNFORMATTED
           "Sample"
    {&Sep} "Time"
    {&Sep} "Intrv"
    {&Sep} "Polls"
    {&Sep} "Id"
    {&Sep} "Latch"
    {&Sep} "Busy%"
    {&Sep} "Users"
    {&Sep} "Locks"
    {&Sep} "Naps"
  SKIP.

  ASSIGN vIntrv = INTERVAL(lvStatTime[ipLastStat],
                           lvStatTime[ipPrevStat],
                           "milliseconds":U) / 1000.0
  . /* ASSIGN */

  FOR EACH ttLatch EXCLUSIVE-LOCK
     WHERE ttLatch.LatchLock[ipLastStat] NE ttLatch.LatchLock[ipPrevStat]
        OR ttLatch.LatchWait[ipLastStat] NE ttLatch.LatchWait[ipPrevStat]
        BY ttLatch.LatchId:

    FOR EACH ttLatchHoldStat NO-LOCK
       WHERE ttLatchHoldStat.LatchId EQ ttLatch.LatchId:
      ACCUMULATE "Holder":U (COUNT).
    END.

    PUT UNFORMATTED
             ipSample                                /* Sample */
      {&Sep} DateTime2String(lvStatTime[ipLastStat]) /* Time   */
      {&Sep} vIntrv                                  /* Intrv  */
      {&Sep} lvStatCount                             /* Polls  */
      {&Sep} ttLatch.LatchId                         /* Id     */
      {&Sep} ttLatch.LatchName                       /* Latch  */
      {&Sep} IF lvStatCount  LE 1                    /* Busy   */
             OR ttLatch.LatchHold LT 0 THEN "":U ELSE
             STRING(ROUND(ttLatch.LatchBusy / lvStatCount * 100.0, 0))
      {&Sep} IF ttLatch.LatchHold LT 0 THEN "":U ELSE
             STRING(ACCUM COUNT "Holder":U)          /* Users  */
      {&Sep} ( ttLatch.LatchLock[ipLastStat]         /* Locks  */
             - ttLatch.LatchLock[ipPrevStat]
             ) / vIntrv
      {&Sep} ( ttLatch.LatchWait[ipLastStat]         /* Naps   */
             - ttLatch.LatchWait[ipPrevStat]
             ) / vIntrv
    SKIP.

/* Zero stat counter: */
    ASSIGN ttLatch.LatchBusy = 0.

  END. /* FOR EACH ttLatch */

  PUT CONTROL NULL(0).
  OUTPUT CLOSE.

END PROCEDURE. /* ReportLatchStat */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportLatchHold:

  DEFINE INPUT PARAMETER ipPrevStat AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipLastStat AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipSample   AS INTEGER NO-UNDO.

  DEFINE VARIABLE vStatIntrv AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vHoldIntrv AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vUserIntrv AS DECIMAL NO-UNDO.

  OUTPUT TO VALUE(SUBSTITUTE("LatchHold.&1.txt", LDBNAME("DICTDB"))) APPEND.
  IF SEEK(OUTPUT) EQ 0 THEN
  PUT UNFORMATTED
           "Sample"
    {&Sep} "Time"
    {&Sep} "Intrv"
    {&Sep} "Polls"
    {&Sep} "Id"
    {&Sep} "Latch"
    {&Sep} "Locks"
    {&Sep} "User%"
    {&Sep} "User"
    {&Sep} "Name"
    {&Sep} "BgnTime"
    {&Sep} "EndTime"
    {&Sep} "Intrv"
    {&Sep} "Login"
    {&Sep} "Type"
    {&Sep} "Batch"
    {&Sep} "Device"
    {&Sep} "PID"
    {&Sep} "Flag"
    {&Sep} "DbAccess"
    {&Sep} "DbRead"
    {&Sep} "DbWrite"
    {&Sep} "BiRead"
    {&Sep} "BiWrite"
    {&Sep} "AiRead"
    {&Sep} "AiWrite"
  SKIP.

  ASSIGN vStatIntrv = INTERVAL(lvStatTime[ipLastStat],
                               lvStatTime[ipPrevStat],
                               "milliseconds":U) / 1000.0
  . /* ASSIGN */

  FOR EACH ttLatch EXCLUSIVE-LOCK
     WHERE ttLatch.LatchHold GE 0:

    FOR EACH ttLatchHoldStat NO-LOCK
       WHERE ttLatchHoldStat.LatchId EQ ttLatch.LatchId:
      ACCUMULATE ttLatchHoldStat.HoldCount (TOTAL).
    END.

    ASSIGN ttLatch.HoldCount = ACCUM TOTAL ttLatchHoldStat.HoldCount.
  END. /* FOR EACH ttLatch */

  FOR EACH ttLatchHold NO-LOCK
        BY ttLatchHold.ConnectUsr:

    ASSIGN vHoldIntrv = INTERVAL(ttLatchHold.StatTime[2],
                                 ttLatchHold.StatTime[1],
                                "milliseconds":U) / 1000.0
           vUserIntrv = IF vHoldIntrv EQ 0.0 THEN 1.0 ELSE vHoldIntrv
    . /* ASSIGN */

    FOR EACH ttLatchHoldStat NO-LOCK
       WHERE ttLatchHoldStat.HoldId EQ ttLatchHold.HoldId,

       FIRST ttLatch NO-LOCK
       WHERE ttLatch.LatchId EQ ttLatchHoldStat.LatchId

          BY ttLatchHoldStat.LatchId:

      PUT UNFORMATTED
                ipSample                                         /* Sample */
        {&Sep}  DateTime2String(lvStatTime[ipLastStat])          /* Time   */
        {&Sep}  vStatIntrv                                       /* Intrv  */
        {&Sep}  lvStatCount                                      /* Polls  */
        {&Sep}  ttLatch.LatchId                                  /* Id     */
        {&Sep}  ttLatch.LatchName                                /* Latch  */
        {&Sep} (ttLatch.LatchLock[ipLastStat]                    /* Locks  */
              - ttLatch.LatchLock[ipPrevStat] ) / vStatIntrv
        {&Sep} ROUND(ttLatchHoldStat.HoldCount / ttLatch.HoldCount * 100.0, 0)
        {&Sep}  ttLatchHold.ConnectUsr                           /* User   */
        {&Sep}  ttLatchHold.ConnectName                          /* Name   */
        {&Sep}  SUBSTRING(ISO-DATE(ttLatchHold.StatTime[1]), 12) /* BgnTime*/
        {&Sep}  SUBSTRING(ISO-DATE(ttLatchHold.StatTime[2]), 12) /* EndTime*/
        {&Sep}  vHoldIntrv                                       /* Intrv  */
        {&Sep}  ttLatchHold.ConnectTime                          /* Login  */
        {&Sep}  ttLatchHold.ConnectType                          /* Type   */
        {&Sep}  ttLatchHold.ConnectBatch                         /* Batch  */
        {&Sep}  ttLatchHold.ConnectDevice                        /* Device */
        {&Sep}  ttLatchHold.ConnectPid                           /* PID    */
        {&Sep}  ttLatchHold.ConnectFlag                          /* Flag   */
        {&Sep} (ttLatchHold.DbAccess[2] - ttLatchHold.DbAccess[1]) / vUserIntrv
        {&Sep} (ttLatchHold.DbRead  [2] - ttLatchHold.DbRead  [1]) / vUserIntrv
        {&Sep} (ttLatchHold.DbWrite [2] - ttLatchHold.DbWrite [1]) / vUserIntrv
        {&Sep} (ttLatchHold.BiRead  [2] - ttLatchHold.BiRead  [1]) / vUserIntrv
        {&Sep} (ttLatchHold.BiWrite [2] - ttLatchHold.BiWrite [1]) / vUserIntrv
        {&Sep} (ttLatchHold.AiRead  [2] - ttLatchHold.AiRead  [1]) / vUserIntrv
        {&Sep} (ttLatchHold.AiWrite [2] - ttLatchHold.AiWrite [1]) / vUserIntrv
      SKIP.
    END. /* FOR EACH ttLatchHoldStat, FIRST ttLatch */
  END. /* FOR EACH ttLatchHold */

  PUT CONTROL NULL(0).
  OUTPUT CLOSE.

END PROCEDURE. /* ReportLatchHold */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportLatchLock:

  DEFINE VARIABLE vStatIntrv AS DECIMAL NO-UNDO.

  IF NOT CAN-FIND(FIRST ttLatchLock) THEN
  RETURN.

  OUTPUT TO VALUE(SUBSTITUTE("LatchLock.&1.txt", LDBNAME("DICTDB"))) APPEND.
  IF SEEK(OUTPUT) EQ 0 THEN
  PUT UNFORMATTED
           "Id"
    {&Sep} "Latch"
    {&Sep} "Naps"
    {&Sep} "Time"
    {&Sep} "Intrv"
    {&Sep} "User"
    {&Sep} "Name"
    {&Sep} "Login"
    {&Sep} "Type"
    {&Sep} "Flag"
    {&Sep} "BiFullBuffs"
    {&Sep} "BiBytesFree"
    {&Sep} "LastCkp"
  SKIP.

  FOR EACH ttLatchLock NO-LOCK,

     FIRST ttLatchHold NO-LOCK
     WHERE ttLatchHold.HoldId EQ ttLatchLock.HoldId,

     FIRST ttLatch NO-LOCK
     WHERE ttLatch.LatchId EQ ttLatchLock.LatchId

        BY ttLatchLock.LatchId:

    ASSIGN vStatIntrv = INTERVAL(ttLatchLock.StatTime[2],
                                 ttLatchLock.StatTime[1],
                                 "milliseconds":U) / 1000.0
    . /* ASSIGN */

    PUT UNFORMATTED
              ttLatchLock.LatchId                      /* Id          */
      {&Sep}  ttLatch.LatchName                        /* Latch       */
      {&Sep} (ttLatchLock.LatchWait[2] - ttLatchLock.LatchWait[1]) / vStatIntrv
      {&Sep}  DateTime2String(ttLatchLock.StatTime[1]) /* Time        */
      {&Sep}  vStatIntrv                               /* Intrv       */
      {&Sep}  ttLatchHold.ConnectUsr                   /* User        */
      {&Sep}  ttLatchHold.ConnectName                  /* Name        */
      {&Sep}  ttLatchHold.ConnectTime                  /* Login       */
      {&Sep}  ttLatchHold.ConnectType                  /* Type        */
      {&Sep}  ttLatchHold.ConnectFlag                  /* Flag        */
      {&Sep}  ttLatchLock.BiFullBuffs                  /* BiFullBuffs */
      {&Sep}  ttLatchLock.BiBytesFree                  /* BiBytesFree */
      {&Sep}  ttLatchLock.LastCkp                      /* LastCkp     */
    SKIP.
  END. /* FOR FIRST ttLatchLock, FIRST ttLatchHold */

  PUT CONTROL NULL(0).
  OUTPUT CLOSE.

END PROCEDURE. /* ReportLatchLock */

/* ------------------------------------------------------------------------- */
