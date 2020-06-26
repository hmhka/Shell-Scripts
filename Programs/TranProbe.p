DEFINE VARIABLE vTranTable AS CHARACTER NO-UNDO INITIAL "_User".
DEFINE VARIABLE vTranSize  AS INTEGER   NO-UNDO INITIAL 3000. /* bytes to BI */
DEFINE VARIABLE vProbeDuration AS INTEGER NO-UNDO INITIAL 30. /* sec */

&SCOPED-DEFINE Sep "~t"

DEFINE TEMP-TABLE ttActivity NO-UNDO
  FIELD SnapshotID    AS INTEGER
  FIELD SnapshotTime1 AS DATETIME /* begin of snapshot */
  FIELD SnapshotTime2 AS DATETIME /*   end of snapshot */
  FIELD RecUpdate   LIKE _ActRecord._Record-RecUpd
  FIELD RecCreate   LIKE _ActRecord._Record-RecCreat
  FIELD RecDelete   LIKE _ActRecord._Record-RecDel
  FIELD IdxCreate   LIKE _ActIndex._Index-Create
  FIELD IdxDelete   LIKE _ActIndex._Index-Delete
  FIELD IdxRemove   LIKE _ActIndex._Index-Remove
  FIELD IdxSplits   LIKE _ActIndex._Index-Splits
  FIELD BiNotesWrtn LIKE _ActBILog._BiLog-RecWriten
  FIELD BiBytesWrtn LIKE _ActBILog._BiLog-BytesWrtn
  FIELD BiNotesRead LIKE _ActBILog._BiLog-RecRead
  FIELD BiBytesRead LIKE _ActBILog._BiLog-BytesRead
  FIELD BiBufFull   AS DECIMAL /* = _Logging-BiFullBuffs / _Logging-BiBuffs  */
  FIELD BiClsFree   AS DECIMAL /* = _Logging-BiBytesFree / _Logging-BiClSize */
  FIELD LastTask    LIKE _MstrBlk._MstrBlk-lasttask
  FIELD TransActiv  AS INTEGER
  FIELD TransAlloc  AS INTEGER
  FIELD TransDead   AS INTEGER
  FIELD Commits     LIKE _ActOther._Other-Commit   
  FIELD Undos       LIKE _ActOther._Other-Undo     
  FIELD LatchLocks  AS INT64 /*LIKE _Latch._Latch-Lock*/
  FIELD LatchWaits  AS INT64 /*LIKE _Latch._Latch-Wait*/
  FIELD ResrcLocks  LIKE _Resrc._Resrc-Lock
  FIELD ResrcWaits  LIKE _Resrc._Resrc-Wait
  FIELD SemWaits    LIKE _ActOther._Other-Wait       
  FIELD FlushMblk   LIKE _ActOther._Other-FlushMblk
  FIELD TxeType     LIKE _TxeLock._Txe-Type
  FIELD TxeLocks    LIKE _TxeLock._Txe-Locks 
  FIELD TxeWaits    LIKE _TxeLock._Txe-Waits
  FIELD TxeLockss   LIKE _TxeLock._Txe-Lockss /*for update locks (8)*/
  FIELD TxeWaitss   LIKE _TxeLock._Txe-Waitss /*and commit locks (9) only*/
  FIELD CurrUsers   LIKE _Server._Server-CurrUsers
  FIELD PendConn    LIKE _Server._Server-PendConn
  FIELD MsgRec      LIKE _ActServer._Server-MsgRec   
  FIELD MsgSent     LIKE _ActServer._Server-MsgSent  
  FIELD ByteRec     LIKE _ActServer._Server-ByteRec  
  FIELD ByteSent    LIKE _ActServer._Server-ByteSent 
  FIELD RecRec      LIKE _ActServer._Server-RecRec   
  FIELD RecSent     LIKE _ActServer._Server-RecSent  
  FIELD QryRec      LIKE _ActServer._Server-QryRec   
  FIELD TimeSlice   LIKE _ActServer._Server-TimeSlice
  INDEX SnapshotID  IS UNIQUE
        SnapshotID
. /* DEFINE TEMP-TABLE ttActivity  */

DEFINE TEMP-TABLE ttLatch NO-UNDO
  FIELD SnapshotID AS INTEGER
  FIELD LatchId    LIKE _Latch._Latch-Id   
  FIELD LatchName  LIKE _Latch._Latch-Name 
  FIELD LatchLock  LIKE _Latch._Latch-Lock 
  FIELD LatchWait  LIKE _Latch._Latch-Wait 
  FIELD LatchHold  LIKE _Latch._Latch-Hold 
  FIELD LatchQhold LIKE _Latch._Latch-Qhold
  INDEX LatchId IS UNIQUE
        SnapshotID
        LatchId
. /* DEFINE TEMP-TABLE ttLatch */

DEFINE TEMP-TABLE ttLatchHolder NO-UNDO
  FIELD LatchList  AS CHARACTER
  FIELD ConnectUsr LIKE _Latch._Latch-Hold
  INDEX ConnectUsr IS UNIQUE
        ConnectUsr
. /* DEFINE TEMP-TABLE ttLatchHolder */

DEFINE TEMP-TABLE ttTrans NO-UNDO
  FIELD TransUsr      LIKE _Trans._Trans-Usrnum
  FIELD TransNum      LIKE _Trans._Trans-Num
  FIELD TransState    LIKE _Trans._Trans-State
  FIELD TransCounter  LIKE _Trans._Trans-counter
  FIELD TransTime     LIKE _Trans._Trans-txtime
  FIELD TransDuration LIKE _Trans._Trans-Duration
  INDEX TransUsr    IS UNIQUE
        TransUsr
. /* DEFINE TEMP-TABLE ttTrans */

DEFINE TEMP-TABLE ttResrc NO-UNDO
  FIELD SnapshotID AS INTEGER
  FIELD ResrcName  LIKE _Resrc-Name
  FIELD ResrcLock  LIKE _Resrc-Lock
  FIELD ResrcWait  LIKE _Resrc-Wait
  INDEX ResrcName  IS UNIQUE
        SnapshotID
        ResrcName
. /* DEFINE TEMP-TABLE ttResrc */

DEFINE TEMP-TABLE ttUser NO-UNDO
  FIELD SnapshotSet         AS INTEGER /*for check if user is still connected*/
  FIELD SnapshotID          AS INTEGER
  FIELD SnapshotTime        AS DATETIME
  FIELD UserNumber          LIKE _Connect._Connect-Usr
  FIELD uConnect-Time       LIKE _Connect._Connect-Time
  FIELD uConnect-Device     LIKE _Connect._Connect-Device
  FIELD uConnect-Pid        LIKE _Connect._Connect-Pid
  FIELD uConnect-Name       LIKE _Connect._Connect-Name
  FIELD uConnect-Type       LIKE _Connect._Connect-Type
  FIELD uConnect-Batch      LIKE _Connect._Connect-Batch
  FIELD uConnect-Server     LIKE _Connect._Connect-Server
  FIELD uConnect-Wait1      LIKE _Connect._Connect-Wait1
  FIELD uConnect-Wait       LIKE _Connect._Connect-Wait
  FIELD uConnect-TransId    LIKE _Connect._Connect-TransId /*or _Trans-Num*/

  FIELD uTrans-State        LIKE _Trans._Trans-State /*ACTIVE,ALLOCATED,DEAD*/
  FIELD uTrans-counter      LIKE _Trans._Trans-counter
  FIELD uTrans-txtime       LIKE _Trans._Trans-txtime
  FIELD uTrans-Duration     LIKE _Trans._Trans-Duration

  FIELD uConnect-Disconnect LIKE _Connect._Connect-Disconnect
  FIELD uConnect-Resync     LIKE _Connect._Connect-Resync

  FIELD uUserIO-DbAccess    LIKE _UserIO._UserIO-DbAccess
  FIELD uUserIO-DbRead      LIKE _UserIO._UserIO-DbRead
  FIELD uUserIO-BiRead      LIKE _UserIO._UserIO-BiRead

  INDEX UserStatIdx IS UNIQUE
        SnapshotID
        UserNumber
        uConnect-Time
        uConnect-Device
        uConnect-Pid
  INDEX SnapshotSet
        SnapshotSet
. /* TEMP-TABLE ttUser */

DEFINE VARIABLE vVersion AS CHARACTER NO-UNDO INITIAL
  "TranProbe.p Release 1.4 as of Tuesday, July 02 13:00, 2013".
/*
1.2 (June 28, 2013):
ttActivity.LatchLocks and ttActivity.LatchWaits are changed to INT64.
vTranTable, vTranSize , vProbeDuration can be set through the -param option.
1.4 (July 02, 2013):
1) LatchHolders report is replaced by Users reports - all sessions that
either marked as latch holder or has non-zero BI Reads.
2) Added BiNotesRead and BiBytesReads columns to Activity report.
*/

/* ******************************  Functions  ****************************** */

/* ------------------------------------------------------------------------- */
FUNCTION String2Date RETURNS DATE (INPUT ipString AS CHARACTER).
/* Input string must have a format like: Tue Jul 27 12:11:45 2004 */
  DEFINE VARIABLE vDate AS DATE NO-UNDO INITIAL ?.
  DEFINE VARIABLE vMonthList AS CHARACTER NO-UNDO
    INITIAL "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec".

  ASSIGN ipString = TRIM(REPLACE(ipString, "  ":U, " ":U))
         vDate = DATE( /*Month:*/ LOOKUP (ENTRY(2,ipString," ":U),vMonthList),
                       /*Day:  */ INTEGER(ENTRY(3,ipString," ":U)),
                       /*Year: */ INTEGER(ENTRY(5,ipString," ":U)))
  NO-ERROR.
  RETURN vDate.
END FUNCTION. /* String2Date */


/* ------------------------------------------------------------------------- */
FUNCTION String2Time RETURNS INTEGER (INPUT ipString AS CHARACTER).
/* Input string must have a format like: Tue Jul 27 12:11:45 2004 */
  DEFINE VARIABLE vTime AS INTEGER NO-UNDO INITIAL ?.
  ASSIGN ipString = TRIM(REPLACE(ipString, "  ":U, " ":U))
         ipString = ENTRY(4,ipString," ":U)
         vTime    = INTEGER(ENTRY(1, ipString, ":":U)) * 3600 /* HH */
                  + INTEGER(ENTRY(2, ipString, ":":U)) * 60   /* MM */
                  + INTEGER(ENTRY(3, ipString, ":":U))        /* SS */
  NO-ERROR.
  RETURN vTime.
END FUNCTION. /* String2Time */

/* ------------------------------------------------------------------------- */

FUNCTION LkHost RETURNS CHARACTER (ipLkPath AS CHARACTER):

  DEFINE VARIABLE vHost AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vRaw  AS RAW       NO-UNDO.

  IF ipLkPath MATCHES "*~~~.db":U THEN
  ASSIGN ipLkPath = SUBSTRING(ipLkPath, 1, LENGTH(ipLkPath) - 3).

  ASSIGN FILE-INFO:FILE-NAME = ipLkPath + ".lk":U.
  IF FILE-INFO:FULL-PATHNAME EQ ? THEN
  RETURN ?.

  INPUT FROM VALUE(FILE-INFO:FULL-PATHNAME) BINARY.
  ASSIGN      LENGTH(vRaw) = FILE-INFO:FILE-SIZE.
  IMPORT UNFORMATTED vRaw.
  INPUT CLOSE.

  ASSIGN vHost = GET-STRING (vRaw, 9)
         LENGTH(vRaw) = 0.
  RETURN vHost.
  
END FUNCTION. /* LkHost */

/* ******************************  Procedures  ***************************** */

/* ------------------------------------------------------------------------- */

PROCEDURE CreateLatchHolder:

  DEFINE INPUT PARAMETER ipLatchHolder LIKE _Latch._Latch-Hold NO-UNDO.
  DEFINE INPUT PARAMETER ipLatchName AS CHARACTER NO-UNDO.

  IF ipLatchHolder LT 0 THEN
  RETURN.

/* If Latch holder already exists then update its latch list: */
  FOR FIRST ttLatchHolder
      WHERE ttLatchHolder.ConnectUsr EQ ipLatchHolder
  TRANSACTION:

    IF LOOKUP(ipLatchName, ttLatchHolder.LatchList) EQ 0 THEN
    ASSIGN ttLatchHolder.LatchList = ttLatchHolder.LatchList + ","
                                   + ipLatchName
    . /* ASSIGN */
    RETURN.                               
  END.
  
/* Create new latch holder: */
  DO TRANSACTION:
    CREATE ttLatchHolder.
    ASSIGN ttLatchHolder.ConnectUsr = ipLatchHolder
           ttLatchHolder.LatchList  = ipLatchName
    . /* ASSIGN */
  END.
END PROCEDURE. /* CreateLatchHolder */

/* ------------------------------------------------------------------------- */

PROCEDURE InitiateStat:

  DEFINE INPUT PARAMETER ipSnapshotId AS INTEGER NO-UNDO.

  DEFINE VARIABLE vLatchName LIKE _Latch._Latch-Name NO-UNDO.

  FOR FIRST DICTDB._TxeLock NO-LOCK
  TRANSACTION:
    CREATE ttActivity.
    ASSIGN ttActivity.SnapshotID = ipSnapshotId
           ttActivity.TxeType    = DICTDB._TxeLock._Txe-Type
    . /* ASSIGN */
  END. /* FOR FIRST _TxeLock */

  FOR EACH DICTDB._Resrc NO-LOCK
  TRANSACTION:
    CREATE ttResrc.
    ASSIGN ttResrc.SnapshotID = ipSnapshotId
           ttResrc.ResrcName  = DICTDB._Resrc._Resrc-Name
    . /* ASSIGN */
  END. /* FOR EACH _Resrc */

  FOR EACH DICTDB._Latch NO-LOCK
     WHERE DICTDB._Latch._Latch-Id GE 2
  TRANSACTION:

    ASSIGN vLatchName = SUBSTRING(DICTDB._Latch._Latch-Name, 5, 3).
    CREATE ttLatch.
    ASSIGN ttLatch.SnapshotID = ipSnapshotId
           ttLatch.LatchId    = DICTDB._Latch._Latch-Id
           ttLatch.LatchName  = vLatchName.
    . /* ASSIGN */
  END. /* FOR EACH _Latch */
END PROCEDURE. /* InitiateTempTables */

/* ------------------------------------------------------------------------- */

PROCEDURE GetActivity:

  DEFINE INPUT PARAMETER ipSnapshotId AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipServerNum LIKE _Server._Server-Num NO-UNDO.

  FOR FIRST ttActivity
      WHERE ttActivity.SnapshotID EQ ipSnapshotId:

/* Snapshot start time: */
    ASSIGN ttActivity.SnapshotTime1 = DATETIME(TODAY, MTIME).

/* _Resrc: */
    FOR EACH DICTDB._Resrc NO-LOCK,
  
       FIRST ttResrc
       WHERE ttResrc.SnapshotID EQ ipSnapshotId
         AND ttResrc.ResrcName  EQ DICTDB._Resrc._Resrc-Name:
      ASSIGN ttResrc.ResrcLock = DICTDB._Resrc._Resrc-Lock
             ttResrc.ResrcWait = DICTDB._Resrc._Resrc-Wait
      . /* ASSIGN */
      ACCUMULATE DICTDB._Resrc._Resrc-Lock (TOTAL)
                 DICTDB._Resrc._Resrc-Wait (TOTAL).
    END. /* FOR EACH _Resrc */

/* _Latch: */
    FOR EACH DICTDB._Latch NO-LOCK
       WHERE DICTDB._Latch._Latch-id GT 1,
  
       FIRST ttLatch 
       WHERE ttLatch.SnapshotID EQ ipSnapshotId
         AND ttLatch.LatchId    EQ DICTDB._Latch._Latch-Id:
      ASSIGN ttLatch.LatchLock  = DICTDB._Latch._Latch-Lock
             ttLatch.LatchWait  = DICTDB._Latch._Latch-Wait
             ttLatch.LatchHold  = DICTDB._Latch._Latch-Hold
             ttLatch.LatchQhold = DICTDB._Latch._Latch-Qhold
      . /* ASSIGN */
      ACCUMULATE INT64(DICTDB._Latch._Latch-Lock) (TOTAL)
                 INT64(DICTDB._Latch._Latch-Wait) (TOTAL).
      
      IF ttLatch.LatchHold GE 0 THEN
      RUN CreateLatchHolder(ttLatch.LatchHold, ttLatch.LatchName).
    END. /* FOR EACH _Latch*/

/* Misc VSTs: */
    FOR FIRST DICTDB._MstrBlk   NO-LOCK,
        FIRST DICTDB._ActRecord NO-LOCK,
        FIRST DICTDB._ActIndex  NO-LOCK,
        FIRST DICTDB._ActBILog  NO-LOCK,
        FIRST DICTDB._Logging   NO-LOCK,
        FIRST DICTDB._ActOther  NO-LOCK,
        FIRST DICTDB._TxeLock   NO-LOCK:
  
      ASSIGN
        ttActivity.RecUpdate  = DICTDB._ActRecord._Record-RecUpd
        ttActivity.RecCreate  = DICTDB._ActRecord._Record-RecCreat
        ttActivity.RecDelete  = DICTDB._ActRecord._Record-RecDel
        ttActivity.IdxCreate  = DICTDB._ActIndex._Index-Create
        ttActivity.IdxDelete  = DICTDB._ActIndex._Index-Delete
        ttActivity.IdxRemove  = DICTDB._ActIndex._Index-Remove
        ttActivity.IdxSplits  = DICTDB._ActIndex._Index-Splits
        ttActivity.BiNotesWrtn = DICTDB._ActBILog._BiLog-RecWriten
        ttActivity.BiBytesWrtn = DICTDB._ActBILog._BiLog-BytesWrtn
        ttActivity.BiNotesRead = DICTDB._ActBILog._BiLog-RecRead
        ttActivity.BiBytesRead = DICTDB._ActBILog._BiLog-BytesRead
        ttActivity.BiBufFull  = DECIMAL(DICTDB._Logging._Logging-BiFullBuffs)
                              / DECIMAL(DICTDB._Logging._Logging-BiBuffs)
        ttActivity.BiClsFree  = DECIMAL(DICTDB._Logging._Logging-BiBytesFree)
                              / DECIMAL(DICTDB._Logging._Logging-BiClSize)
        ttActivity.LastTask   = DICTDB._MstrBlk._MstrBlk-lasttask
        ttActivity.Commits    = DICTDB._ActOther._Other-Commit   
        ttActivity.Undos      = DICTDB._ActOther._Other-Undo     
        ttActivity.LatchLocks = ACCUM TOTAL INT64(DICTDB._Latch._Latch-Lock)
        ttActivity.LatchWaits = ACCUM TOTAL INT64(DICTDB._Latch._Latch-Wait)
        ttActivity.ResrcLocks = ACCUM TOTAL DICTDB._Resrc._Resrc-Lock
        ttActivity.ResrcWaits = ACCUM TOTAL DICTDB._Resrc._Resrc-Wait
        ttActivity.SemWaits   = DICTDB._ActOther._Other-Wait       
        ttActivity.FlushMblk  = DICTDB._ActOther._Other-FlushMblk
        ttActivity.TxeLocks   = DICTDB._TxeLock._Txe-Locks 
        ttActivity.TxeWaits   = DICTDB._TxeLock._Txe-Waits
        ttActivity.TxeLockss  = DICTDB._TxeLock._Txe-Lockss
        ttActivity.TxeWaitss  = DICTDB._TxeLock._Txe-Waitss
      . /* ASSIGN */
/*
_Txe-Lockss and _Txe-Waitss applies to the "update" and "commit" locks only.

_Txe-Lockss - Number of TXE locks held simultaneously.
This is an indicator of how concurrency there is.

_Txe-Waitss - Number of queued locks held simultaneously.
How many other users were in the queue when I entered the queue.
*/
    END. /* FOR FIRST _MstrBlk, _ActRecord, _ActIndex, _ActBILog,
                      _Logging, _ActOther, _TxeLock*/

    IF ipServerNum NE ? AND ipServerNum NE 0 THEN
    FOR FIRST DICTDB._Server NO-LOCK
        WHERE DICTDB._Server._Server-Id EQ ipServerNum + 1,

        FIRST DICTDB._ActServer NO-LOCK
        WHERE DICTDB._ActServer._Server-Id EQ DICTDB._Server._Server-Id:

      ASSIGN ttActivity.CurrUsers = DICTDB._Server._Server-CurrUsers
             ttActivity.PendConn  = DICTDB._Server._Server-PendConn
             ttActivity.MsgRec    = DICTDB._ActServer._Server-MsgRec   
             ttActivity.MsgSent   = DICTDB._ActServer._Server-MsgSent  
             ttActivity.ByteRec   = DICTDB._ActServer._Server-ByteRec  
             ttActivity.ByteSent  = DICTDB._ActServer._Server-ByteSent 
             ttActivity.RecRec    = DICTDB._ActServer._Server-RecRec   
             ttActivity.RecSent   = DICTDB._ActServer._Server-RecSent  
             ttActivity.QryRec    = DICTDB._ActServer._Server-QryRec   
             ttActivity.TimeSlice = DICTDB._ActServer._Server-TimeSlice
      . /* ASSIGN  */
    END.
/* Snapshot end time: */
    ASSIGN ttActivity.SnapshotTime2 = DATETIME(TODAY, MTIME).
  END. /* FOR FIRST ttActivity*/

END PROCEDURE. /* GetActivity */

/* ------------------------------------------------------------------------- */

PROCEDURE CreateRecord:
  DEFINE INPUT PARAMETER ipTable AS HANDLE  NO-UNDO.
  DEFINE INPUT PARAMETER ipRPB   AS INTEGER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ioDbkeyList AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vDbkey AS INT64   NO-UNDO.

  ipTable:BUFFER-CREATE().
  ASSIGN vDbkey = ipTable:RECID
         vDbkey = vDbkey - (vDbkey MOD ipRPB)
  . /* ASSIGN */
  IF NOT CAN-DO(ioDbkeyList, STRING(vDbkey)) THEN
  ASSIGN ioDbkeyList = ioDbkeyList + ",":U + STRING(vDbkey).
END PROCEDURE. /* CreateRecord */

/* ------------------------------------------------------------------------- */

PROCEDURE TranProbe:

  DEFINE  INPUT PARAMETER ipTableName AS CHARACTER NO-UNDO.
  DEFINE  INPUT PARAMETER ipTranRecs  AS INTEGER   NO-UNDO.
  DEFINE  INPUT PARAMETER ipTranMode  AS INTEGER   NO-UNDO.
  DEFINE  INPUT PARAMETER ipRPB       AS INTEGER   NO-UNDO.
  DEFINE OUTPUT PARAMETER opTrid      LIKE _Trans._Trans-Num NO-UNDO.
  DEFINE OUTPUT PARAMETER opDbkeyList AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vDbkey AS INT64   NO-UNDO.
  DEFINE VARIABLE hTable AS HANDLE  NO-UNDO.
  DEFINE VARIABLE i      AS INTEGER NO-UNDO.

  CREATE BUFFER hTable FOR TABLE ipTableName.
  hTable:DISABLE-LOAD-TRIGGERS(NO).
  
  ASSIGN
    opTrid      = ?
    opDbkeyList = "":U
  . /* ASSIGN */

  CASE ipTranMode:

/* no db updates, savepoints only: */
    WHEN 0 THEN
Probe0:
    DO TRANSACTION:
      REPEAT i = 1 TO ipTranRecs:
        hTable:BUFFER-CREATE().
        hTable:BUFFER-DELETE().
      END.
      ASSIGN opTrid = DBTASKID("DICTDB").
    END. /* 0 */

    WHEN 1 THEN
Probe1:
    DO TRANSACTION:
      REPEAT i = 1 TO ipTranRecs:
        hTable:BUFFER-CREATE().
        hTable:BUFFER-DELETE().
      END.
      ASSIGN opTrid = DBTASKID("DICTDB").
      UNDO Probe1.
    END. /* 1 */


/* subtransactions / savepoints: */
    WHEN 2 THEN
Probe2:
    DO TRANSACTION:
      REPEAT i = 1 TO ipTranRecs:
        RUN CreateRecord(hTable, ipRPB, INPUT-OUTPUT opDbkeyList).
        hTable:BUFFER-DELETE().
      END.
      ASSIGN opTrid = DBTASKID("DICTDB").
    END. /* 2 */

/* subtransactions / savepoints / undo: */
    WHEN 3 THEN
Probe3:
    DO TRANSACTION:
      REPEAT i = 1 TO ipTranRecs:
        RUN CreateRecord(hTable, ipRPB, INPUT-OUTPUT opDbkeyList).
        hTable:BUFFER-DELETE().
      END.
      ASSIGN opTrid = DBTASKID("DICTDB").
      UNDO Probe3.
    END. /* 3 */

/* no subtransactions / no savepoints: */
    WHEN 4 THEN
Probe4:
    DO TRANSACTION:
      DO i = 1 TO ipTranRecs:
        RUN CreateRecord(hTable, ipRPB, INPUT-OUTPUT opDbkeyList).
        hTable:BUFFER-DELETE().
      END.
      ASSIGN opTrid = DBTASKID("DICTDB").
    END. /* 4 */

/* no subtransactions / no savepoints / undo: */
    WHEN 5 THEN
Probe5:
    DO TRANSACTION:
      DO i = 1 TO ipTranRecs:
        RUN CreateRecord(hTable, ipRPB, INPUT-OUTPUT opDbkeyList).
        hTable:BUFFER-DELETE().
      END.
      ASSIGN opTrid = DBTASKID("DICTDB").
      UNDO Probe5.
    END. /* 5 */

  END CASE.

  IF opDbkeyList BEGINS ",":U THEN
  ASSIGN opDbkeyList = SUBSTRING(opDbkeyList, 2).

  DELETE WIDGET hTable.

END PROCEDURE. /* TranProbe */


/* ------------------------------------------------------------------------- */
PROCEDURE GetUsers.
  
  DEFINE INPUT PARAMETER ipSnapshotId  AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipSnapshotSet AS INTEGER NO-UNDO.

  FOR EACH  DICTDB._UserIO NO-LOCK /* INDEX _UserIO-Id */
     WHERE  DICTDB._UserIO._UserIO-Usr    NE ?
       AND (DICTDB._UserIO._UserIO-BiRead GT 0
        OR  CAN-FIND(FIRST ttLatchHolder
                     WHERE ttLatchHolder.ConnectUsr
                        EQ DICTDB._UserIO._UserIO-Usr))
  TRANSACTION:

    FIND FIRST ttUser /* INDEX UserStatIdx */
         WHERE ttUser.SnapshotID EQ ipSnapshotId
           AND ttUser.UserNumber EQ DICTDB._UserIO._UserIO-Usr
    NO-ERROR.

    IF NOT AVAILABLE ttUser THEN
    CREATE ttUser.
    ASSIGN ttUser.SnapshotID       = ipSnapshotID
           ttUser.SnapshotSet      = ipSnapshotSet
           ttUser.SnapshotTime     = DATETIME(TODAY, MTIME)
           ttUser.UserNumber       = DICTDB._UserIO._UserIO-Usr
           ttUser.uUserIO-DbAccess = DICTDB._UserIO._UserIO-DbAccess
           ttUser.uUserIO-DbRead   = DICTDB._UserIO._UserIO-DbRead
           ttUser.uUserIO-BiRead   = DICTDB._UserIO._UserIO-BiRead
    . /* ASSIGN */
  END. /* FOR EACH _UserIO */

  FOR EACH DICTDB._Connect NO-LOCK /* INDEX _Connect-Id */
     WHERE DICTDB._Connect._Connect-Usr NE ?,

     FIRST ttUser
     WHERE ttUser.SnapshotID EQ ipSnapshotId
       AND ttUser.UserNumber EQ DICTDB._Connect._Connect-Usr
  TRANSACTION:

    ASSIGN ttUser.uConnect-Time       = DICTDB._Connect._Connect-Time
           ttUser.uConnect-Device     = DICTDB._Connect._Connect-Device
           ttUser.uConnect-Pid        = DICTDB._Connect._Connect-Pid
           ttUser.uConnect-Name       = DICTDB._Connect._Connect-Name
           ttUser.uConnect-Type       = DICTDB._Connect._Connect-Type
           ttUser.uConnect-Batch      = DICTDB._Connect._Connect-Batch
           ttUser.uConnect-Server     = DICTDB._Connect._Connect-Server
           ttUser.uConnect-Wait1      = DICTDB._Connect._Connect-Wait1
           ttUser.uConnect-Wait       = DICTDB._Connect._Connect-Wait
           ttUser.uConnect-TransId    = DICTDB._Connect._Connect-TransId
           ttUser.uConnect-Disconnect = DICTDB._Connect._Connect-Disconnect
           ttUser.uConnect-Resync     = DICTDB._Connect._Connect-Resync
    . /* ASSIGN */
  END. /* FOR EACH _Connect */

  FOR EACH DICTDB._Trans NO-LOCK /* INDEX _Trans-Id */
     WHERE DICTDB._Trans._Trans-Num NE ?
  TRANSACTION:

    CASE DICTDB._Trans._Trans-State:
      WHEN "ACTIVE":U    THEN ACCUMULATE "ACTIVE":U    (COUNT).
      WHEN "ALLOCATED":U THEN ACCUMULATE "ALLOCATED":U (COUNT).
      WHEN "DEAD":U      THEN ACCUMULATE "DEAD":U      (COUNT).
    END CASE.

    FOR FIRST ttUser
        WHERE ttUser.SnapshotID      EQ ipSnapshotId
          AND ttUser.UserNumber      EQ DICTDB._Trans._Trans-Usrnum:
      ASSIGN  ttUser.uConnect-TransId = DICTDB._Trans._Trans-Num
              ttUser.uTrans-State     = DICTDB._Trans._Trans-State
              ttUser.uTrans-counter   = DICTDB._Trans._Trans-counter
              ttUser.uTrans-txtime    = DICTDB._Trans._Trans-txtime
              ttUser.uTrans-Duration  = DICTDB._Trans._Trans-Duration
      . /* ASSIGN */
    END. /* FOR FIRST ttUser */
  END. /* FOR EACH _Trans */

  FOR FIRST ttActivity
      WHERE ttActivity.SnapshotId EQ ipSnapshotId:
    ASSIGN  ttActivity.TransActiv = ACCUM COUNT "ACTIVE":U
            ttActivity.TransAlloc = ACCUM COUNT "ALLOCATED":U
            ttActivity.TransDead  = ACCUM COUNT "DEAD":U
    . /* ASSIGN */
  END. /* FOR FIRST ttActivity */

END PROCEDURE. /* GetUsers */

/* ------------------------------------------------------------------------- */

PROCEDURE PutStartup:

  DEFINE  INPUT PARAMETER ipFilePrefix     AS CHARACTER NO-UNDO.
  DEFINE  INPUT PARAMETER ipTranTable      AS CHARACTER NO-UNDO.
  DEFINE  INPUT PARAMETER ipTranSize       AS INTEGER   NO-UNDO.
  DEFINE OUTPUT PARAMETER opTranRecs       AS INTEGER   NO-UNDO.
  DEFINE OUTPUT PARAMETER opRPB            AS INTEGER   NO-UNDO.
  DEFINE OUTPUT PARAMETER opServerNum LIKE _Server._Server-Num NO-UNDO.
  DEFINE OUTPUT PARAMETER opHavePermission AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE vDbHost  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE hQuery   AS HANDLE    NO-UNDO.
  DEFINE VARIABLE hBuffer  AS HANDLE    NO-UNDO.
  DEFINE VARIABLE vRecSize AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vDbPath    LIKE _FileList._FileList-Name NO-UNDO.
  DEFINE VARIABLE vServerPid LIKE _Server._Server-Pid      NO-UNDO.

  ASSIGN vDbHost = "LocalHost":U.
  DO i = NUM-ENTRIES(DBPARAM("DICTDB"), ",":U) TO 1 BY -1:
    IF ENTRY(i, DBPARAM("DICTDB"), ",":U) BEGINS "-H ":U THEN
    ASSIGN vDbHost = SUBSTRING(ENTRY(i, DBPARAM("DICTDB"), ",":U) , 4).
  END.

  OUTPUT TO VALUE(ipFilePrefix + ".Startup.txt").
  
  FOR FIRST DICTDB._MyConnection NO-LOCK:
  
    FOR FIRST DICTDB._Connect NO-LOCK
        WHERE DICTDB._Connect._Connect-Id EQ
              DICTDB._MyConnection._MyConn-UserId + 1,
        
        FIRST DICTDB._FileList NO-LOCK
        WHERE DICTDB._FileList._FileList-Name MATCHES "*~~~.db":U,
    
        FIRST DICTDB._MstrBlk NO-LOCK,
      
        FIRST DICTDB._Logging NO-LOCK:
    
      IF vDbHost EQ "LocalHost":U THEN
      ASSIGN vDbHost = LkHost(DICTDB._FileList._FileList-Name).
    
      ASSIGN vDbPath = DICTDB._FileList._FileList-Name
             vDbPath = SUBSTRING(vDbPath, 1, R-INDEX(vDbPath,
                         IF OPSYS EQ "UNIX":U THEN "/":U ELSE "~\":U) - 1)
     . /* ASSIGN */
    
      PUT UNFORMATTED
        vVersion                                               SKIP(1)
        "Db Host        : " vDbHost                            SKIP
        "Db Path        : " DICTDB._FileList._FileList-Name    SKIP
        "Db Version     : " DICTDB._MstrBlk._MstrBlk-DbVers         " (":U
                            DICTDB._MstrBlk._MstrBlk-DbVers MOD 256 ") ":U
                  "a.k.a. " DBVERSION("DICTDB")                SKIP
      . /* PUT */
    
      FOR FIRST DICTDB._DbStatus NO-LOCK:
        PUT UNFORMATTED
        "Shared Mem Vers: " DICTDB._DbStatus._DbStatus-ShmVers SKIP.
      END.
    
      PUT UNFORMATTED
        "Db Codepage    : " DBCODEPAGE("DICTDB")               SKIP
        "Db Collation   : " DBCOLLATION("DICTDB")              SKIP(1)
    
        "Db Created     : " DICTDB._MstrBlk._MstrBlk-crdate    SKIP
        "Db Opened      : " DICTDB._MstrBlk._MstrBlk-oprdate   SKIP
        "Last Trid      : " DICTDB._MstrBlk._MstrBlk-lasttask  SKIP(1)

        "Bi Size        : " TRUNCATE(DICTDB._Logging._Logging-BiLogSize / 1024,
                                     2) SPACE "M"              SKIP
        "Bi Cluster Size: " TRUNCATE(DICTDB._Logging._Logging-BiClSize / 1048576,
                                     1) SPACE "M"              SKIP
        "Bi Block Size  : " DICTDB._Logging._Logging-BiBlkSize / 1024 "K" SKIP
        "Last Checkpoint: " DICTDB._Logging._Logging-LastCkp   SKIP(1)
      . /* PUT */

      IF DICTDB._Logging._Logging-AiJournal EQ "yes":U THEN
      PUT UNFORMATTED
        "Ai Size        : " TRUNCATE(DICTDB._Logging._Logging-AiLogSize / 1024,
                                     2) SPACE "M"              SKIP
        "Ai Block Size  : " DICTDB._Logging._Logging-AiBlkSize / 1024 "K" SKIP
        "Ai New Date    : " DICTDB._Logging._Logging-AiNew     SKIP
        "Ai Seq Number  : " DICTDB._Logging._Logging-AiGenNum  SKIP(1)
      . /* PUT */

      PUT UNFORMATTED
"Database Features"                                            SKIP
"   ID   Feature                            Active"            SKIP
"  ----  ---------------------------------  ------"            SKIP
      . /* PUT */

      FOR EACH DICTDB._Database-Feature NO-LOCK
         WHERE DICTDB._Database-Feature._DBFeature_Enabled EQ "1":
        PUT
     DICTDB._Database-Feature._DBFeature-ID            FORMAT ">>>>>9" SPACE(2)
     DICTDB._Database-Feature._DBFeature_Name          FORMAT "x(33)"  SPACE(2)
     DICTDB._Database-Feature._DBFeature_Active EQ "1" FORMAT "Yes/No" SKIP
        . /* PUT */
      END.

      PUT UNFORMATTED                                          SKIP(1)
        "Client Version : " PROVERSION                         SKIP
        "My Session     : "
                     "Usr " DICTDB._Connect._Connect-Usr
               ", Userid: " DICTDB._Connect._Connect-Name
                  ", PID: " DICTDB._Connect._Connect-Pid
               ", Device: " DICTDB._Connect._Connect-Device
           ", Login time: " DICTDB._Connect._Connect-Time      SKIP
        "Session Startup: " SESSION:STARTUP-PARAMETERS         SKIP
        "Db Connection  : " DBPARAM("DICTDB":U)                SKIP
        "Db Restrictions: " DBRESTRICTION("DICTDB")            SKIP(1)
      . /* PUT */
    
      ASSIGN opServerNum = DICTDB._Connect._Connect-Server.
    
    END. /* FOR FIRST _Connect, _FileList, _MstrBlk */

    IF opServerNum NE ? AND opServerNum NE 0 THEN
    FOR FIRST DICTDB._Server NO-LOCK
        WHERE DICTDB._Server._Server-Id EQ opServerNum + 1:
    
      ASSIGN vServerPid = DICTDB._Server._Server-Pid.
    
      PUT UNFORMATTED
        "My Server      : "
                     "SRV " DICTDB._Server._Server-Num
                   ", PID " DICTDB._Server._Server-Pid
                  ", Port " DICTDB._Server._Server-PortNum
                ", Logins " DICTDB._Server._Server-Logins
                   ", Max " DICTDB._Server._Server-MaxUsers
      SKIP. /* PUT*/
    
      IF DICTDB._Server._Server-Logins GT 1 THEN
      PUT UNFORMATTED "Other sessions : " SKIP.

      FOR EACH DICTDB._Connect NO-LOCK
         WHERE DICTDB._Connect._Connect-Server EQ opServerNum
           AND DICTDB._Connect._Connect-Usr
            NE DICTDB._MyConnection._MyConn-UserId:

        PUT UNFORMATTED
          "               : "
                       "Usr " DICTDB._Connect._Connect-Usr
                  ", Userid " DICTDB._Connect._Connect-Name
                     ", PID " DICTDB._Connect._Connect-Pid
                  ", Device " DICTDB._Connect._Connect-Device
             ", Login time: " DICTDB._Connect._Connect-Time     
                    ", Trid " DICTDB._Connect._Connect-TransId
        SKIP. /* PUT */
      END. /* FOR EACH _Connect */
    END. /* FOR FIRST _Server */
  END. /* FOR FIRST _MyConnection */

  PUT UNFORMATTED SKIP(1) "Tran Table     : " ipTranTable SKIP.

  ASSIGN opHavePermission = FALSE.

  FOR FIRST DICTDB._Db NO-LOCK WHERE DICTDB._Db._Db-local EQ TRUE,

      FIRST DICTDB._File OF DICTDB._Db NO-LOCK
      WHERE DICTDB._File._File-Name EQ ipTranTable:
    
    ASSIGN opRPB = ?.

    FOR FIRST DICTDB._StorageObject OF DICTDB._Db NO-LOCK
        WHERE DICTDB._StorageObject._Object-Type   EQ 1
          AND DICTDB._StorageObject._Object-Number EQ DICTDB._File._File-Number,

        FIRST DICTDB._Area NO-LOCK
        WHERE DICTDB._Area._Area-Number EQ DICTDB._StorageObject._Area-Number,
      
        FIRST DICTDB._AreaStatus NO-LOCK
        WHERE DICTDB._AreaStatus._AreaStatus-Areanum EQ DICTDB._Area._Area-Number:

      ASSIGN opRPB = EXP(2,DICTDB._Area._Area-recbits).

      PUT UNFORMATTED
        'Table Area     : "' DICTDB._Area._Area-Name '"'
                       ":":U DICTDB._Area._Area-Number
                       ",":U opRPB
                       ";":U DICTDB._Area._Area-clustersize SPACE
          "Size: " TRUNCATE((DICTDB._AreaStatus._AreaStatus-Hiwater / 1024.0)
                          * (DICTDB._Area._Area-blocksize / 1024),
                             2) "MB":U
                      "=":U  DICTDB._AreaStatus._AreaStatus-Hiwater
                      "*":U (DICTDB._Area._Area-blocksize / 1024) "K":U SKIP
        "Table Object Bl: "  DICTDB._StorageObject._Object-block
      SKIP. /* PUT */
    END. /* FOR FIRST _StorageObject, _Area */

/* If ipTranTable is a system table then stop: */
    IF opRPB EQ ? THEN
    RETURN.

    PUT UNFORMATTED
      "Can-Read       : " CAN-DO(DICTDB._File._Can-Read)    SKIP
      "Can-Write      : " CAN-DO(DICTDB._File._Can-Write)   SKIP
      "Can-Load       : " CAN-DO(DICTDB._File._Can-Load)    SKIP
    . /* PUT */

    ASSIGN opHavePermission = CAN-DO(DICTDB._File._Can-Read)
                          AND CAN-DO(DICTDB._File._Can-Write)
                          AND CAN-DO(DICTDB._File._Can-Load)
    . /* ASSIGN */

    FOR EACH DICTDB._File-Trig OF DICTDB._File NO-LOCK:
      IF CAN-DO("CREATE,DELETE":U, DICTDB._File-Trig._Event) THEN
      PUT UNFORMATTED
        DICTDB._File-Trig._Event " Trigger : " 
        DICTDB._File-Trig._Proc-Name
      SKIP. /* PUT */
    END. /* FOR EACH _File-Trig OF _File */

    IF NOT opHavePermission THEN
    RETURN.

/* Check the size of template record of ipTranTable: */
    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(BUFFER _File:HANDLE).
    CREATE BUFFER hBuffer FOR TABLE ipTranTable.
    hQuery:SET-BUFFERS(hBuffer).
    hQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 WHERE RECID(&1) EQ &2 NO-LOCK",
                        /* &1 */ DICTDB._File._File-Name,
                        /* &2 */ STRING(DICTDB._File._Template))).
    hQuery:QUERY-OPEN.
    hQuery:GET-NEXT() NO-ERROR.
    ASSIGN vRecSize = hBuffer:RECORD-LENGTH.
    hQuery:QUERY-CLOSE.

    DELETE WIDGET hQuery.
    DELETE WIDGET hBuffer.

/* Rough estimate of the number of records to create BI notes of ipTranSize: */
    ASSIGN opTranRecs = (ipTranSize - 128 - LENGTH(USERID("DICTDB")))
                      / (128 + 2 * vRecSize)
           opTranRecs = MAX(opTranRecs, 1)
   . /* ASSIGN */

/*If TableName does not exist then output file will miss the following lines:*/
    PUT UNFORMATTED
      "Template Record: " vRecSize " bytes"                 SKIP
      "Tran Size      : " ipTranSize " bytes"               SKIP
      "Tran Records   : " opTranRecs " records"             SKIP
    . /* PUT */
  END. /* FOR FIRST _File */

  OUTPUT CLOSE.

END PROCEDURE. /* PutStartup */

/* ------------------------------------------------------------------------- */

PROCEDURE PutActivity:

  DEFINE INPUT PARAMETER ipFilePrefix AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipSnapshot1  AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipSnapshot2  AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipTranMode   AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipMyTrid     LIKE _Trans._Trans-Num NO-UNDO.
  DEFINE INPUT PARAMETER ipDbkeyList  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipServerNum  LIKE _Server._Server-Num NO-UNDO.

  DEFINE BUFFER bActivity1 FOR ttActivity.
  DEFINE BUFFER bActivity2 FOR ttActivity.

  DEFINE BUFFER bLatch1 FOR ttLatch.
  DEFINE BUFFER bLatch2 FOR ttLatch.

  DEFINE VARIABLE vTxeLocks LIKE _TxeLock._Txe-Locks NO-UNDO EXTENT 0.
  DEFINE VARIABLE vTxeWaits LIKE _TxeLock._Txe-Waits NO-UNDO EXTENT 0.
  DEFINE VARIABLE d AS DECIMAL NO-UNDO. /* = Interval */
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  OUTPUT TO VALUE(ipFilePrefix + ".Activity.txt") APPEND.

  IF SEEK(OUTPUT) EQ 0 THEN
  DO:
    PUT UNFORMATTED 
             "Date"
      {&Sep} "Time"
      {&Sep} "Interval"
      {&Sep} "Delta"
      {&Sep} "Probe"
      {&Sep} "MyTrid"
      {&Sep} "MyDbkeys"
      {&Sep} "LastTask"
      {&Sep} "Commits"
      {&Sep} "Undos"
      {&Sep} "Activ"
      {&Sep} "Alloc"
      {&Sep} "Dead"
      {&Sep} "LatchLocks"
      {&Sep} "LatchWaits"
    . /* PUT */

    FOR EACH bLatch1 NO-LOCK
       WHERE bLatch1.SnapshotID EQ ipSnapshot1
         AND CAN-DO("MTX,BIB,AIB,TXT,TXQ,USR":U, bLatch1.LatchName)
          BY bLatch1.LatchId:
      PUT UNFORMATTED
        {&Sep} bLatch1.LatchName ":":U (bLatch1.LatchId - 1) " Locks"
        {&Sep} bLatch1.LatchName ":":U (bLatch1.LatchId - 1) " Waits"
      . /* PUT */
    END. /* FOR EACH bLatch1, FIRST bLatch2 */

    PUT UNFORMATTED
      {&Sep} "TxeLocks"
      {&Sep} "TxeWaits"
      {&Sep} "ResrcLocks"
      {&Sep} "ResrcWaits"
      {&Sep} "SemWaits "
      {&Sep} "FlushMblk"
      {&Sep} "BiNotesWrtn"
      {&Sep} "BiBytesWrtn"
      {&Sep} "BiNotesRead"
      {&Sep} "BiBytesRead"
      {&Sep} "%BiBufFull"
      {&Sep} "%BiClsFree"
      {&Sep} "RecUpdate"
      {&Sep} "RecCreate"
      {&Sep} "RecDelete"
      {&Sep} "IdxCreate"
      {&Sep} "IdxDelete"
      {&Sep} "IdxRemove"
      {&Sep} "IdxSplits"
    . /* PUT */

    IF ipServerNum NE ? AND ipServerNum NE 0 THEN
    PUT UNFORMATTED
      {&Sep} "CurrUsers"
      {&Sep} "PendConn"
      {&Sep} "MsgRecv"
      {&Sep} "MsgSent"
      {&Sep} "ByteRecv"
      {&Sep} "ByteSent"
      {&Sep} "RecRecv"
      {&Sep} "RecSent"
      {&Sep} "QryRecv"
      {&Sep} "TimeSlice"
    . /* PUT */

    PUT UNFORMATTED SKIP.

  END. /* IF SEEK(OUTPUT) EQ 0 */

  FOR FIRST bActivity1 NO-LOCK
      WHERE bActivity1.SnapshotId EQ ipSnapshot1,

      FIRST bActivity2 NO-LOCK
      WHERE bActivity2.SnapshotId EQ ipSnapshot2:

    ASSIGN 
      d = IF    ipTranMode EQ ?
          AND   bActivity2.SnapshotTime1 NE bActivity1.SnapshotTime2
          THEN (bActivity2.SnapshotTime1 -  bActivity1.SnapshotTime2)
               / 1000.0
          ELSE 1.0 /* to protect against division by zero */
    . /* ASSIGN */

    PUT UNFORMATTED 
          /* Date */ STRING(DATE(bActivity2.SnapshotTime1), "99/99/9999")
   {&Sep} /* Time */ ENTRY(2, STRING(bActivity2.SnapshotTime1), " ":U)
   {&Sep} /* Interval */ (bActivity2.SnapshotTime1
                        - bActivity1.SnapshotTime2) / 1000.0
   {&Sep} MAX(bActivity1.SnapshotTime2 - bActivity1.SnapshotTime1,
              bActivity2.SnapshotTime2 - bActivity2.SnapshotTime1)
           / 1000.0
   {&Sep} ipTranMode
   {&Sep} ipMyTrid
   {&Sep} ipDbkeyList
   {&Sep} /* LastTask */ (bActivity2.LastTask   - bActivity1.LastTask) / d
   {&Sep} /* Commits  */ (bActivity2.Commits    - bActivity1.Commits ) / d
   {&Sep} /* Undos    */ (bActivity2.Undos      - bActivity1.Undos   ) / d
   {&Sep} /* Activ    */  bActivity1.TransActiv
   {&Sep} /* Alloc    */  bActivity1.TransAlloc
   {&Sep} /* Dead     */  bActivity1.TransDead 
   {&Sep} /*LatchLocks*/ (bActivity2.LatchLocks - bActivity1.LatchLocks) / d
   {&Sep} /*LatchWaits*/ (bActivity2.LatchWaits - bActivity1.LatchWaits) / d
    . /* PUT */

    FOR EACH bLatch1 NO-LOCK
       WHERE bLatch1.SnapshotID EQ ipSnapshot1
         AND CAN-DO("MTX,BIB,AIB,TXT,TXQ,USR":U, bLatch1.LatchName),

      FIRST bLatch2 NO-LOCK
      WHERE bLatch2.SnapshotID EQ ipSnapshot2
        AND bLatch2.LatchId    EQ bLatch1.LatchId
         BY bLatch1.LatchId:

      PUT UNFORMATTED
   {&Sep} /* LatchLock*/ (bLatch2.LatchLock - bLatch1.LatchLock) / d
   {&Sep} /* LatchWait*/ (bLatch2.LatchWait - bLatch1.LatchWait) / d
      . /* PUT */
    END. /* FOR EACH bLatch1, FIRST bLatch2 */

    ASSIGN vTxeLocks = 0
           vTxeWaits = 0.
    DO i = EXTENT(bActivity1.TxeType) TO 1 BY -1:
      ASSIGN vTxeLocks = vTxeLocks
                       + (bActivity2.TxeLocks[i] - bActivity1.TxeLocks[i])
             vTxeWaits = vTxeWaits
                       + (bActivity2.TxeWaits[i] - bActivity1.TxeWaits[i])
      . /* ASSIGN */
    END.

    PUT UNFORMATTED
{&Sep} /*TxeLocks   */  vTxeLocks
{&Sep} /*TxeWaits   */  vTxeWaits
{&Sep} /*ResrcLocks */ (bActivity2.ResrcLocks  - bActivity1.ResrcLocks) / d
{&Sep} /*ResrcWaits */ (bActivity2.ResrcWaits  - bActivity1.ResrcWaits) / d
{&Sep} /*SemWaits   */ (bActivity2.SemWaits    - bActivity1.SemWaits  ) / d
{&Sep} /*FlushMblk  */ (bActivity2.FlushMblk   - bActivity1.FlushMblk ) / d
{&Sep} /*BiNotesWrtn*/ (bActivity2.BiNotesWrtn - bActivity1.BiNotesWrtn) / d
{&Sep} /*BiBytesWrtn*/ (bActivity2.BiBytesWrtn - bActivity1.BiBytesWrtn) / d
{&Sep} /*BiNotesRead*/ (bActivity2.BiNotesRead - bActivity1.BiNotesRead) / d
{&Sep} /*BiBytesRead*/ (bActivity2.BiBytesRead - bActivity1.BiBytesRead) / d
{&Sep} /*BiBufFull  */  bActivity1.BiBufFull * 100.0
{&Sep} /*BiClsFree  */  bActivity1.BiClsFree * 100.0
{&Sep} /*RecUpdate  */ (bActivity2.RecUpdate   - bActivity1.RecUpdate) / d
{&Sep} /*RecCreate  */ (bActivity2.RecCreate   - bActivity1.RecCreate) / d
{&Sep} /*RecDelete  */ (bActivity2.RecDelete   - bActivity1.RecDelete) / d
{&Sep} /*IdxCreate  */ (bActivity2.IdxCreate   - bActivity1.IdxCreate) / d
{&Sep} /*IdxDelete  */ (bActivity2.IdxDelete   - bActivity1.IdxDelete) / d
{&Sep} /*IdxRemove  */ (bActivity2.IdxRemove   - bActivity1.IdxRemove) / d
{&Sep} /*IdxSplits  */ (bActivity2.IdxSplits   - bActivity1.IdxSplits) / d
    . /* PUT */

    IF ipServerNum NE ? AND ipServerNum NE 0 THEN
    PUT UNFORMATTED
   {&Sep} /* CurrUsers*/ IF bActivity1.CurrUsers EQ bActivity2.CurrUsers
                         THEN STRING(bActivity1.CurrUsers)
                         ELSE STRING(bActivity1.CurrUsers) + ",":U +
                              STRING(bActivity2.CurrUsers)
   {&Sep} /* PendConn */ IF bActivity1.PendConn EQ bActivity2.PendConn
                         THEN STRING(bActivity1.PendConn)
                         ELSE STRING(bActivity1.PendConn) + ",":U +
                              STRING(bActivity2.PendConn)
   {&Sep} /* MsgRec   */ (bActivity2.MsgRec    - bActivity1.MsgRec   ) / d
   {&Sep} /* MsgSent  */ (bActivity2.MsgSent   - bActivity1.MsgSent  ) / d
   {&Sep} /* ByteRec  */ (bActivity2.ByteRec   - bActivity1.ByteRec  ) / d
   {&Sep} /* ByteSent */ (bActivity2.ByteSent  - bActivity1.ByteSent ) / d
   {&Sep} /* RecRec   */ (bActivity2.RecRec    - bActivity1.RecRec   ) / d
   {&Sep} /* RecSent  */ (bActivity2.RecSent   - bActivity1.RecSent  ) / d
   {&Sep} /* QryRec   */ (bActivity2.QryRec    - bActivity1.QryRec   ) / d
   {&Sep} /* TimeSlice*/ (bActivity2.TimeSlice - bActivity1.TimeSlice) / d
    . /* PUT */

    PUT UNFORMATTED SKIP.

  END. /* FOR FIRST bActivity1, FIRST bActivity2 */
END PROCEDURE. /* PutActivity */

/* ------------------------------------------------------------------------- */

PROCEDURE PutLatches:

  DEFINE INPUT PARAMETER ipFilePrefix AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipSnapshot1  AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipSnapshot2  AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipTranMode   AS INTEGER   NO-UNDO.

  DEFINE BUFFER bActivity1 FOR ttActivity.
  DEFINE BUFFER bActivity2 FOR ttActivity.

  DEFINE BUFFER bLatch1 FOR ttLatch.
  DEFINE BUFFER bLatch2 FOR ttLatch.

  DEFINE VARIABLE d AS DECIMAL NO-UNDO. /* Interval */

  OUTPUT TO VALUE(ipFilePrefix + ".Latches.txt") APPEND.

  IF SEEK(OUTPUT) EQ 0 THEN
  PUT UNFORMATTED 
           "Date"
    {&Sep} "Time"
    {&Sep} "Interval"
    {&Sep} "Delta"
    {&Sep} "Probe"
    {&Sep} "Latch"
    {&Sep} "Id"
    {&Sep} "Locks"
    {&Sep} "Waits"
    {&Sep} "Holder1"
    {&Sep} "Holder2"
    {&Sep} "QHolder1"
    {&Sep} "QHolder2"
  SKIP. /* PUT */

  FOR EACH bLatch1 NO-LOCK
     WHERE bLatch1.SnapshotID EQ ipSnapshot1,
    
     FIRST bLatch2 NO-LOCK
     WHERE bLatch2.SnapshotID EQ ipSnapshot2
       AND bLatch2.LatchId    EQ bLatch1.LatchId,
    
     FIRST bActivity1 NO-LOCK
     WHERE bActivity1.SnapshotID EQ ipSnapshot1,
    
     FIRST bActivity2 NO-LOCK
     WHERE bActivity2.SnapshotID EQ ipSnapshot2

        BY bLatch1.LatchId:

    ASSIGN 
      d = IF    ipTranMode EQ ?
          AND   bActivity2.SnapshotTime1 NE bActivity1.SnapshotTime2
          THEN (bActivity2.SnapshotTime1 -  bActivity1.SnapshotTime2)
               / 1000.0
          ELSE 1.0 /* to protect against division by zero */
    . /* ASSIGN */

    IF bLatch1.LatchLock NE bLatch2.LatchLock
    OR bLatch1.LatchWait NE bLatch2.LatchWait THEN
    PUT UNFORMATTED 
             /* Date */ STRING(DATE(bActivity2.SnapshotTime1), "99/99/9999")
      {&Sep} /* Time */ ENTRY(2, STRING(bActivity2.SnapshotTime1), " ":U)
      {&Sep} /* Interval */ (bActivity2.SnapshotTime1
                           - bActivity1.SnapshotTime2) / 1000.0
      {&Sep} MAX(bActivity1.SnapshotTime2 - bActivity1.SnapshotTime1,
                 bActivity2.SnapshotTime2 - bActivity2.SnapshotTime1)
              / 1000.0
      {&Sep} /* TranMode */  ipTranMode
      {&Sep} /* Latch    */  bLatch1.LatchName
      {&Sep} /* Id       */  bLatch1.LatchId - 1
      {&Sep} /* Locks    */ (bLatch2.LatchLock - bLatch1.LatchLock) / d
      {&Sep} /* Waits    */ (bLatch2.LatchWait - bLatch1.LatchWait) / d
      {&Sep} /* Holder1  */  bLatch1.LatchHold
      {&Sep} /* Holder2  */  bLatch2.LatchHold
      {&Sep} /* QHolder1 */  bLatch1.LatchQhold
      {&Sep} /* QHolder2 */  bLatch2.LatchQhold
    SKIP. /* PUT */
  END. /* FOR EACH bLatch1, FIRST bLatch2, bActivity1, bActivity2 */
END PROCEDURE. /* PutLatches */

/* ------------------------------------------------------------------------- */

PROCEDURE PutTXELocks:

  DEFINE INPUT PARAMETER ipFilePrefix AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipSnapshot1  AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipSnapshot2  AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipTranMode   AS INTEGER   NO-UNDO.

  DEFINE BUFFER bActivity1 FOR ttActivity.
  DEFINE BUFFER bActivity2 FOR ttActivity.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE d AS DECIMAL NO-UNDO. /* Interval */

  OUTPUT TO VALUE(ipFilePrefix + ".TXELocks.txt") APPEND.

  IF SEEK(OUTPUT) EQ 0 THEN
  PUT UNFORMATTED 
           "Date"
    {&Sep} "Time"
    {&Sep} "Interval"
    {&Sep} "Delta"
    {&Sep} "Probe"
    {&Sep} "Id"
    {&Sep} "Type"
    {&Sep} "Locks"
    {&Sep} "Waits"
  SKIP. /* PUT */

  FOR FIRST bActivity1 NO-LOCK
      WHERE bActivity1.SnapshotID EQ ipSnapshot1,
    
      FIRST bActivity2 NO-LOCK
      WHERE bActivity2.SnapshotID EQ ipSnapshot2:

    ASSIGN
      d = IF    ipTranMode EQ ?
          AND   bActivity2.SnapshotTime1 NE bActivity1.SnapshotTime2
          THEN (bActivity2.SnapshotTime1 -  bActivity1.SnapshotTime2)
               / 1000.0
          ELSE 1.0 /* to protect against division by zero */
    . /* ASSIGN */

    REPEAT i = 1 TO EXTENT(bActivity1.TxeType):
      IF bActivity1.TxeLocks[i] NE bActivity2.TxeLocks[i]
      OR bActivity1.TxeWaits[i] NE bActivity2.TxeWaits[i] THEN
      PUT UNFORMATTED
         /* Date */ STRING(DATE(bActivity2.SnapshotTime1), "99/99/9999")
  {&Sep} /* Time */ ENTRY(2, STRING(bActivity2.SnapshotTime1), " ":U)
  {&Sep} /* Interval */ (bActivity2.SnapshotTime1
                       - bActivity1.SnapshotTime2) / 1000.0
  {&Sep} MAX(bActivity1.SnapshotTime2 - bActivity1.SnapshotTime1,
             bActivity2.SnapshotTime2 - bActivity2.SnapshotTime1)
          / 1000.0
  {&Sep} /*TranMode*/ ipTranMode
  {&Sep} /* Id     */ i
  {&Sep} /* Type   */ TRIM(bActivity1.TxeType[i])
  {&Sep} /* Locks  */ (bActivity2.TxeLocks[i] - bActivity1.TxeLocks[i]) / d
  {&Sep} /* Waits  */ (bActivity2.TxeWaits[i] - bActivity1.TxeWaits[i]) / d
      SKIP. /* PUT */
    END. /* REPEAT */

    REPEAT i = 8 TO 9:
      IF bActivity1.TxeLockss[i] NE bActivity2.TxeLockss[i]
      OR bActivity1.TxeWaitss[i] NE bActivity2.TxeWaitss[i] THEN
      PUT UNFORMATTED

         /* Date */ STRING(DATE(bActivity2.SnapshotTime1), "99/99/9999")
  {&Sep} /* Time */ ENTRY(2, STRING(bActivity2.SnapshotTime1), " ":U)
  {&Sep} /* Interval */ (bActivity2.SnapshotTime1
                       - bActivity1.SnapshotTime2) / 1000.0
  {&Sep} MAX(bActivity1.SnapshotTime2 - bActivity1.SnapshotTime1,
             bActivity2.SnapshotTime2 - bActivity2.SnapshotTime1)
          / 1000.0
  {&Sep} /*TranMode*/ ipTranMode
  {&Sep} /* Id   */ i + 2
  {&Sep} /* Type */ TRIM(bActivity1.TxeType[i]) " Simultaneous"
  {&Sep} /* Locks*/ (bActivity2.TxeLockss[i] - bActivity1.TxeLockss[i]) / d
  {&Sep} /* Waits*/ (bActivity2.TxeWaitss[i] - bActivity1.TxeWaitss[i]) / d
      SKIP. /* PUT */
    END. /* REPEAT */
  END. /* FOR FIRST bActivity1, bActivity2 */
END PROCEDURE. /* PutTXELocks */

/* ------------------------------------------------------------------------- */

PROCEDURE PutResources:

  DEFINE INPUT PARAMETER ipFilePrefix AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipSnapshot1  AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipSnapshot2  AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipTranMode   AS INTEGER   NO-UNDO.

  DEFINE BUFFER bActivity1 FOR ttActivity.
  DEFINE BUFFER bActivity2 FOR ttActivity.

  DEFINE BUFFER bResrc1 FOR ttResrc.
  DEFINE BUFFER bResrc2 FOR ttResrc.

  DEFINE VARIABLE d AS DECIMAL NO-UNDO. /* Interval */

  OUTPUT TO VALUE(ipFilePrefix + ".Resources.txt") APPEND.

  IF SEEK(OUTPUT) EQ 0 THEN
  PUT UNFORMATTED 
           "Date"
    {&Sep} "Time"
    {&Sep} "Interval"
    {&Sep} "Delta"
    {&Sep} "Probe"
    {&Sep} "Name"
    {&Sep} "Locks"
    {&Sep} "Waits"
  SKIP. /* PUT */

  FOR FIRST bActivity1 NO-LOCK
      WHERE bActivity1.SnapshotID EQ ipSnapshot1,
    
      FIRST bActivity2 NO-LOCK
      WHERE bActivity2.SnapshotID EQ ipSnapshot2,
    
       EACH bResrc1 NO-LOCK
      WHERE bResrc1.SnapshotID EQ ipSnapshot1,

      FIRST bResrc2 NO-LOCK
      WHERE bResrc2.SnapshotID EQ ipSnapshot2
        AND bResrc2.ResrcName  EQ bResrc1.ResrcName:

    ASSIGN
      d = IF    ipTranMode EQ ?
          AND   bActivity2.SnapshotTime1 NE bActivity1.SnapshotTime2
          THEN (bActivity2.SnapshotTime1 -  bActivity1.SnapshotTime2)
               / 1000.0
          ELSE 1.0 /* to protect against division by zero */
    . /* ASSIGN */

    IF bResrc1.ResrcLock NE bResrc2.ResrcLock
    OR bResrc1.ResrcWait NE bResrc2.ResrcWait THEN
    PUT UNFORMATTED
             /* Date */ STRING(DATE(bActivity2.SnapshotTime1), "99/99/9999")
      {&Sep} /* Time */ ENTRY(2, STRING(bActivity2.SnapshotTime1), " ":U)
      {&Sep} /* Interval */ (bActivity2.SnapshotTime1
                           - bActivity1.SnapshotTime2) / 1000.0
      {&Sep} MAX(bActivity1.SnapshotTime2 - bActivity1.SnapshotTime1,
                 bActivity2.SnapshotTime2 - bActivity2.SnapshotTime1)
                / 1000.0
      {&Sep} /*TranMode*/  ipTranMode
      {&Sep} /* Type   */  TRIM(bResrc1.ResrcName)
      {&Sep} /* Locks  */ (bResrc2.ResrcLock - bResrc1.ResrcLock) / d
      {&Sep} /* Waits  */ (bResrc2.ResrcWait - bResrc1.ResrcWait) / d
    SKIP. /* PUT */
  END. /* FOR FIRST bActivity1, bActivity2, EACH bResrc1, bResrc1 */
END PROCEDURE. /* PutResources */

/* ------------------------------------------------------------------------- */

PROCEDURE PutUsers:

  DEFINE INPUT PARAMETER ipFilePrefix  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipSnapshot1   AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipSnapshot2   AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipSnapshotSet AS INTEGER   NO-UNDO.

  DEFINE BUFFER bUser1 FOR ttUser.
  DEFINE BUFFER bUser2 FOR ttUser.

  DEFINE VARIABLE vDbAccess LIKE _UserIO._UserIO-DbAccess NO-UNDO.
  DEFINE VARIABLE vDbRead   LIKE _UserIO._UserIO-DbRead   NO-UNDO.
  DEFINE VARIABLE vBiRead   LIKE _UserIO._UserIO-BiRead   NO-UNDO.
  DEFINE VARIABLE d AS DECIMAL NO-UNDO. /* = Interval */

  FOR EACH ttUser
     WHERE ttUser.SnapshotSet NE ipSnapshotSet
  TRANSACTION:
    DELETE ttUser.
  END.

  OUTPUT TO VALUE(ipFilePrefix + ".Users.txt") APPEND.

  IF SEEK(OUTPUT) EQ 0 THEN
  PUT UNFORMATTED 
           "Date"
    {&Sep} "Time"
    {&Sep} "Interval"
    {&Sep} "LatchList"
    {&Sep} "Usr"
    {&Sep} "Userid"
    {&Sep} "PID"
    {&Sep} "Type"
    {&Sep} "Login"
    {&Sep} "SRV"
    {&Sep} "Wait"
    {&Sep} "Wait1"
    {&Sep} "TranNum"
    {&Sep} "TranState"
    {&Sep} "TranTime"
    {&Sep} "Duration"
    {&Sep} "RlCounter"
    {&Sep} "UsrToDie"
    {&Sep} "Resync"
    {&Sep} "BiRead"
    {&Sep} "DbRead"
    {&Sep} "DbAccess"
  SKIP. /* PUT */

  FOR EACH bUser2 NO-LOCK
     WHERE bUser2.SnapshotID EQ ipSnapshot2:
    
    FIND FIRST bUser1 NO-LOCK
         WHERE bUser1.SnapshotID EQ ipSnapshot1
           AND bUser1.UserNumber EQ bUser2.UserNumber
    NO-ERROR.

/* If user has been disconnected from database
  and its UsrNum was re-used by new session:
*/  IF AVAILABLE bUser1
    AND (bUser1.uConnect-Time   NE bUser2.uConnect-Time
     OR  bUser1.uConnect-Device NE bUser2.uConnect-Device
     OR  bUser1.uConnect-Pid    NE bUser2.uConnect-Pid) THEN
    DO TRANSACTION:
      DELETE bUser1.
    END.

    IF AVAILABLE bUser1 THEN
    ASSIGN d = IF    bUser2.SnapshotTime NE bUser1.SnapshotTime
               THEN (bUser2.SnapshotTime -  bUser1.SnapshotTime) / 1000.0
               ELSE 1.0 /* to protect against division by zero */
           vDbAccess = (bUser2.uUserIO-DbAccess - bUser1.uUserIO-DbAccess) / d
           vDbRead   = (bUser2.uUserIO-DbRead   - bUser1.uUserIO-DbRead)   / d
           vBiRead   = (bUser2.uUserIO-BiRead   - bUser1.uUserIO-BiRead)   / d
    . /* ASSIGN */
    ELSE
    ASSIGN d = bUser2.SnapshotTime - 
               DATETIME(String2Date(bUser2.uConnect-Time),
                        String2Time(bUser2.uConnect-Time) * 1000)
           vDbAccess = bUser2.uUserIO-DbAccess / d
           vDbRead   = bUser2.uUserIO-DbRead   / d
           vBiRead   = bUser2.uUserIO-BiRead   / d
    . /* ASSIGN */

    FIND FIRST ttLatchHolder NO-LOCK
         WHERE ttLatchHolder.ConnectUsr EQ bUser2.UserNumber
    NO-ERROR.

    IF AVAILABLE ttLatchHolder
    OR vBiRead NE 0 THEN
    PUT UNFORMATTED 
             /* Date */ STRING(DATE(bUser2.SnapshotTime), "99/99/9999")
      {&Sep} /* Time */ ENTRY(2, STRING(bUser2.SnapshotTime), " ":U)
      {&Sep} /* Interval */ d
      {&Sep} /* LatchList*/ IF AVAILABLE ttLatchHolder
                            THEN ttLatchHolder.LatchList
                            ELSE "":U
      {&Sep} /* Usr      */ bUser2.UserNumber
      {&Sep} /* Userid   */ bUser2.uConnect-Name
      {&Sep} /* PID      */ bUser2.uConnect-Pid
      {&Sep} /* Type     */ bUser2.uConnect-Type
      {&Sep} /* Login    */ bUser2.uConnect-Time
/*
 uConnect-Device
 uConnect-Batch
*/    {&Sep} /* SRV      */ bUser2.uConnect-Server
      {&Sep} /* Wait     */ bUser2.uConnect-Wait  
      {&Sep} /* Wait1    */ bUser2.uConnect-Wait1 
      {&Sep} /* TranNum  */ bUser2.uConnect-TransId
      {&Sep} /* TranState*/ bUser2.uTrans-State
      {&Sep} /* TranTime */ bUser2.uTrans-txtime
      {&Sep} /* Duration */ bUser2.uTrans-Duration
      {&Sep} /* RlCounter*/ bUser2.uTrans-counter
      {&Sep} /* UsrToDie */ bUser2.uConnect-Disconnect
      {&Sep} /* Resync   */ bUser2.uConnect-Resync
      {&Sep} /* BiRead   */ vBiRead
      {&Sep} /* DbRead   */ vDbRead
      {&Sep} /* DbAccess */ vDbAccess
    SKIP. /* PUT */
  END. /* FIRST bActivity1, bActivity2 */

END PROCEDURE. /* PutUsers */

/* ------------------------------------------------------------------------- */

PROCEDURE PutReports:

  DEFINE INPUT PARAMETER ipFilePrefix  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipSnapshot1   AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipSnapshot2   AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipSnapshotSet AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipTranMode    AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipMyTrid      LIKE _Trans._Trans-Num NO-UNDO.
  DEFINE INPUT PARAMETER ipDbkeyList   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipServerNum   LIKE _Server._Server-Num NO-UNDO.

  RUN PutActivity (INPUT ipFilePrefix,
                   INPUT ipSnapshot1,  /* Snapshot From */
                   INPUT ipSnapshot2,  /* Snapshot To   */
                   INPUT ipTranMode,   /* TranMode      */
                   INPUT ipMyTrid,     /* MyTrid        */
                   INPUT ipDbkeyList,  /* DbkeyList     */
                   INPUT ipServerNum).
                  
  RUN PutLatches  (INPUT ipFilePrefix,
                   INPUT ipSnapshot1,  /* Snapshot From */
                   INPUT ipSnapshot2,  /* Snapshot To   */
                   INPUT ipTranMode).  /* TranMode      */
                  
  RUN PutTXELocks (INPUT ipFilePrefix,
                   INPUT ipSnapshot1,  /* Snapshot From */
                   INPUT ipSnapshot2,  /* Snapshot To   */
                   INPUT ipTranMode).  /* TranMode      */

  RUN PutResources(INPUT ipFilePrefix,
                   INPUT ipSnapshot1,  /* Snapshot From */
                   INPUT ipSnapshot2,  /* Snapshot To   */
                   INPUT ipTranMode).  /* TranMode      */

  IF ipTranMode EQ ? THEN
  RUN PutUsers(    INPUT ipFilePrefix,
                   INPUT ipSnapshot1,  /* Snapshot From */
                   INPUT ipSnapshot2,  /* Snapshot To   */
                   INPUT ipSnapshotSet).

END PROCEDURE. /* PutReports */

/* ----------------------------- Main procedure ---------------------------- */

DEFINE VARIABLE vFilePrefix AS CHARACTER NO-UNDO.
DEFINE VARIABLE vTranRecs   AS INTEGER   NO-UNDO.
DEFINE VARIABLE vTranMode   AS INTEGER   NO-UNDO.
DEFINE VARIABLE vTranLoop   AS INTEGER   NO-UNDO.
DEFINE VARIABLE vRPB        AS INTEGER   NO-UNDO.
DEFINE VARIABLE vMyServer   LIKE _Server._Server-Num NO-UNDO.
DEFINE VARIABLE vMyTrid     LIKE _Trans._Trans-Num   NO-UNDO.
DEFINE VARIABLE vDbkeyList  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vIsOK       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE vEndTime    AS INTEGER   NO-UNDO.
DEFINE VARIABLE i           AS INTEGER   NO-UNDO.

FOR FIRST DICTDB._MyConnection NO-LOCK:
  ASSIGN vFilePrefix = "TranProbe.":U
                     + STRING(YEAR (TODAY), "9999":U)
                     + STRING(MONTH(TODAY), "99":U)
                     + STRING(DAY  (TODAY), "99":U) + "_":U
                     + REPLACE(STRING(TIME, "HH:MM:SS":U), ":":U, "":U)
           + ".P-":U + STRING(DICTDB._MyConnection._MyConn-Pid)
  . /* ASSIGN */
END.

DO i = NUM-ENTRIES(SESSION:PARAMETER) TO 1 BY -1:
  IF ENTRY(i, SESSION:PARAMETER) NE "":U THEN
  CASE i:
    WHEN 1 THEN ASSIGN vTranTable     =         ENTRY(i, SESSION:PARAMETER).
    WHEN 2 THEN ASSIGN vTranSize      = INTEGER(ENTRY(i, SESSION:PARAMETER)) NO-ERROR.
    WHEN 3 THEN ASSIGN vProbeDuration = INTEGER(ENTRY(i, SESSION:PARAMETER)) NO-ERROR.
  END CASE.
END.

RUN PutStartup(INPUT vFilePrefix,
               INPUT vTranTable,
               INPUT vTranSize,
              OUTPUT vTranRecs,
              OUTPUT vRPB,
              OUTPUT vMyServer,
              OUTPUT vIsOK).
IF NOT vIsOK THEN QUIT.

RUN InitiateStat(1).
RUN InitiateStat(2).

ASSIGN vEndTime  = ETIME + vProbeDuration * 1000
       vTranMode = 0
       vTranLoop = 0.
/*
/* Remove recid holders from previous run: */
RUN TranProbe(INPUT vTranTable,
              INPUT 1,
              INPUT 1,
              INPUT vRPB,
             OUTPUT vMyTrid,
             OUTPUT vDbkeyList).
*/
DO WHILE ETIME LT vEndTime:

  IF NOT SESSION:BATCH-MODE THEN
  DISPLAY
    vTranLoop FORMAT ">>>,>>9"           LABEL "Transactions" SKIP
    INTEGER((vEndTime - ETIME) / 1000.0) LABEL "Time Left"
  WITH SIDE-LABEL 1 DOWN.

/* Watch the current activity: */
  RUN GetUsers(1, vTranLoop). /* Before GetActivity */
  RUN GetActivity(1, vMyServer).

  PAUSE 1 NO-MESSAGE.

  RUN GetActivity(2, vMyServer).
  RUN GetUsers(2, vTranLoop). /* After GetActivity */
/* End of Watch. */

  RUN PutReports(INPUT vFilePrefix,
                 INPUT 1,         /* Snapshot From */
                 INPUT 2,         /* Snapshot To   */
                 INPUT vTranLoop, /* Snapshot Set  */
                 INPUT ?,         /* TranMode      */
                 INPUT 0,         /* MyTrid        */
                 INPUT "":U,      /* DbkeyList     */
                 INPUT vMyServer).

  ASSIGN vTranMode = (vTranMode + 1) MOD 6
         vTranLoop =  vTranLoop + 1.

/* Watch the my own activity: */
  RUN GetUsers(1, vTranLoop). /* Before GetActivity */
  RUN GetActivity(1, vMyServer).

  RUN TranProbe(INPUT vTranTable,
                INPUT vTranRecs,
                INPUT vTranMode,
                INPUT vRPB,
               OUTPUT vMyTrid,
               OUTPUT vDbkeyList).

  RUN GetActivity(2, vMyServer).
  RUN GetUsers(2, vTranLoop). /* After GetActivity */
/* End of Watch. */

  RUN PutReports(INPUT vFilePrefix,
                 INPUT 1,          /* Snapshot From */
                 INPUT 2,          /* Snapshot To   */
                 INPUT vTranLoop,  /* Snapshot Set  */
                 INPUT vTranMode,  /* TranMode      */ 
                 INPUT vMyTrid,    /* MyTrid        */ 
                 INPUT vDbkeyList, /* DbkeyList     */ 
                 INPUT vMyServer).

END. /* DO WHILE ETIME LT vEndTime */

IF NOT SESSION:BATCH-MODE THEN
DO:
  HIDE NO-PAUSE.
  ASSIGN FILE-INFO:FILE-NAME = ".".
  MESSAGE FILE-INFO:FULL-PATHNAME vFilePrefix + ".*.txt"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

QUIT.
