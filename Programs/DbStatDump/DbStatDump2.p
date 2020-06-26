/*------------------------------------------------------------------------
    File        : DbStatDump2.p
    Purpose     : Dump _TableStat/_IndexStat for DICTDB db.

    Syntax      :

    Description :

    Author(s)   : George Potemkin
    Created     : May 23, 2010
    Modified    : Jul 01, 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT PARAMETER ipLocalHost AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipTimeStamp AS CHARACTER NO-UNDO.

/* Define the shared temp-tables to store the most recent stats: */
{DbStatDump.i}

/* Caching Db Schema: */

DEFINE TEMP-TABLE tt_Area NO-UNDO
  FIELD uArea-Number      LIKE _Area._Area-Number
  FIELD uArea-Name        LIKE _Area._Area-Name
  FIELD uArea-blocksize   LIKE _Area._Area-blocksize
  FIELD uArea-recbits     LIKE _Area._Area-recbits
  FIELD uArea-clustersize LIKE _Area._Area-clustersize
  FIELD uArea-extents     LIKE _Area._Area-extents
  FIELD AreaHWM           LIKE _AreaStatus._AreaStatus-Hiwater
  FIELD AreaReads         LIKE _ActIOFile._IOFile-Reads
  FIELD AreaWrites        LIKE _ActIOFile._IOFile-Writes

  FIELD TableCount        AS INTEGER
  FIELD IndexCount        AS INTEGER

  FIELD TableRead         LIKE _TableStat._TableStat-Read
  FIELD TableUpdate       LIKE _TableStat._TableStat-Update
  FIELD TableCreate       LIKE _TableStat._TableStat-Create
  FIELD TableDelete       LIKE _TableStat._TableStat-Delete

  FIELD IndexRead         LIKE _IndexStat._IndexStat-Read
  FIELD IndexCreate       LIKE _IndexStat._IndexStat-Create
  FIELD IndexDelete       LIKE _IndexStat._IndexStat-Delete

  INDEX uArea-Number
        uArea-Number
. /* DEFINE TEMP-TABLE tt_Area */

DEFINE TEMP-TABLE tt_File NO-UNDO
  FIELD uFile-Number LIKE _File._File-Number
  FIELD uFile-Name   LIKE _File._File-Name
  FIELD uPrime-Index LIKE _File._Prime-Index
  FIELD uFileRecid   AS RECID
  FIELD IndexCount   AS INTEGER
  INDEX uFile-Number
        uFile-Number
  INDEX uFileRecid
        uFileRecid
. /* DEFINE TEMP-TABLE tt_File */

DEFINE TEMP-TABLE tt_Index NO-UNDO
  FIELD uIdx-Num    LIKE _Index._Idx-Num
  FIELD uIndex-Name LIKE _Index._Index-Name
  FIELD uFile-recid LIKE _Index._File-recid
  FIELD IndexAttr   AS CHARACTER
  INDEX uIdx-Num
        uIdx-Num
. /* DEFINE TEMP-TABLE tt_Index */

DEFINE TEMP-TABLE tt_StorageObject NO-UNDO
  FIELD uObject-Type   LIKE _StorageObject._Object-Type
  FIELD uObject-Number LIKE _StorageObject._Object-Number
  FIELD uArea-Number   LIKE _StorageObject._Area-Number
  FIELD uObject-root   LIKE _StorageObject._Object-root
  INDEX uObject-Number
        uObject-Type
        uObject-Number
. /* DEFINE TEMP-TABLE tt_StorageObject */

DEFINE VARIABLE vDbHost  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vDbPath  LIKE _FileList._FileList-Name NO-UNDO.
DEFINE VARIABLE vConnect AS CHARACTER NO-UNDO.
DEFINE VARIABLE vParam   AS CHARACTER NO-UNDO.
DEFINE VARIABLE vDbSize  AS DECIMAL   NO-UNDO.

DEFINE VARIABLE vTableRangeSize AS INTEGER NO-UNDO.
DEFINE VARIABLE vMinTableStatId LIKE _TableStat._TableStat-Id NO-UNDO.
DEFINE VARIABLE vMaxTableStatId LIKE _TableStat._TableStat-Id NO-UNDO.
DEFINE VARIABLE vHighestTableId LIKE _File._File-Number NO-UNDO.
DEFINE VARIABLE vDbTableCount   AS INTEGER NO-UNDO.

DEFINE VARIABLE vIndexRangeSize AS INTEGER NO-UNDO.
DEFINE VARIABLE vMinIndexStatId LIKE _IndexStat._IndexStat-Id NO-UNDO.
DEFINE VARIABLE vMaxIndexStatId LIKE _IndexStat._IndexStat-Id NO-UNDO.
DEFINE VARIABLE vHighestIndexId LIKE _Index._Idx-Num NO-UNDO.
DEFINE VARIABLE vDbIndexCount   AS INTEGER NO-UNDO.

DEFINE VARIABLE vPrevStatDate   AS DATE    NO-UNDO.
DEFINE VARIABLE vPrevStatTime   AS INTEGER NO-UNDO.
DEFINE VARIABLE vCurrInterval   AS INTEGER NO-UNDO.
DEFINE VARIABLE vIgnorePrevStat AS LOGICAL NO-UNDO.

DEFINE STREAM DumpFile.
DEFINE VARIABLE vDumpPrefix AS CHARACTER NO-UNDO.

DEFINE VARIABLE i AS INTEGER NO-UNDO.

/* ******************************  Functions  ****************************** */

/* ------------------------------------------------------------------------- */
FUNCTION Stamp2Date RETURNS DATE (INPUT ipString AS CHARACTER):
/* Input string must have a format like: 20100607_145148 */
  DEFINE VARIABLE vDate AS DATE NO-UNDO INITIAL ?.
  ASSIGN vDate = DATE(INTEGER(SUBSTRING(ipString, 5,2)), /*Month*/
                      INTEGER(SUBSTRING(ipString, 7,2)), /*Day  */
                      INTEGER(SUBSTRING(ipString, 1,4))) /*Year */
  NO-ERROR.
  RETURN vDate.
END FUNCTION. /* String2Date */

/* ------------------------------------------------------------------------- */
FUNCTION Stamp2Time RETURNS INTEGER (INPUT ipString AS CHARACTER):
/* Input string must have a format like: 20100607_145148 */
  DEFINE VARIABLE vTime AS INTEGER NO-UNDO INITIAL ?.
  ASSIGN vTime = INTEGER(SUBSTRING(ipString,10,2)) /*HH*/ * 3600
               + INTEGER(SUBSTRING(ipString,12,2)) /*MM*/ * 60
               + INTEGER(SUBSTRING(ipString,14,2)) /*SS*/
  NO-ERROR.
  RETURN vTime.
END FUNCTION. /* String2Time */

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
FUNCTION PerSec RETURNS DECIMAL (INPUT ipValue AS DECIMAL).
  RETURN TRUNCATE(ipValue / vCurrInterval, 2).
END FUNCTION. /* String2Time */

/* ******************************  Procedures  ***************************** */

/* ------------------------------------------------------------------------- */
PROCEDURE ReadSchema.

  DEFINE OUTPUT PARAMETER opHighestTableId LIKE _File._File-Number NO-UNDO.
  DEFINE OUTPUT PARAMETER opHighestIndexId LIKE _Index._Idx-Num NO-UNDO.
  DEFINE OUTPUT PARAMETER opDbTableCount   AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER opDbIndexCount   AS INTEGER NO-UNDO.

  FOR FIRST  DICTDB._Db NO-LOCK
      WHERE  DICTDB._Db._Db-local:

    FOR EACH DICTDB._File OF DICTDB._Db NO-LOCK
       WHERE DICTDB._File._File-Number GT 0
         AND DICTDB._File._File-Number LT 32768
    TRANSACTION:

      CREATE tt_File.
      ASSIGN tt_File.uFile-Number = DICTDB._File._File-Number
             tt_File.uFile-Name   = DICTDB._File._File-Name
             tt_File.uFileRecid   = RECID(DICTDB._File)
             tt_File.uPrime-Index = DICTDB._File._Prime-Index
             opDbTableCount       = opDbTableCount + 1
      . /* ASSIGN */
    END. /* FOR EACH _File */

    FOR EACH DICTDB._StorageObject OF DICTDB._Db NO-LOCK
       WHERE DICTDB._StorageObject._Object-type   GE 1
         AND DICTDB._StorageObject._Object-type   LE 2
    TRANSACTION:

      CREATE tt_StorageObject.
      ASSIGN
        tt_StorageObject.uObject-Type   = DICTDB._StorageObject._Object-Type
        tt_StorageObject.uObject-Number = DICTDB._StorageObject._Object-Number
        tt_StorageObject.uArea-Number   = DICTDB._StorageObject._Area-Number
        tt_StorageObject.uObject-root   = DICTDB._StorageObject._Object-root
      . /* ASSIGN */
    END. /* FOR EACH _StorageObject */
  END. /* FOR FIRST _Db */

  FOR EACH DICTDB._Area NO-LOCK
     WHERE DICTDB._Area._Area-Type GE 3
       AND DICTDB._Area._Area-Type LE 6
  TRANSACTION:

    CREATE tt_Area.
    ASSIGN tt_Area.uArea-Number      = DICTDB._Area._Area-Number
           tt_Area.uArea-Name        = DICTDB._Area._Area-Name
           tt_Area.uArea-blocksize   = DICTDB._Area._Area-blocksize
           tt_Area.uArea-recbits     = DICTDB._Area._Area-recbits
           tt_Area.uArea-clustersize = DICTDB._Area._Area-clustersize
           tt_Area.uArea-extents     = DICTDB._Area._Area-extents

    . /* ASSIGN */
  END. /* EACH _Area */

  FOR EACH DICTDB._Index NO-LOCK
     WHERE DICTDB._Index._Idx-Num GT 0
       AND DICTDB._Index._Idx-Num LT 32768,

    FIRST tt_File NO-LOCK
    WHERE tt_File.uFileRecid EQ DICTDB._Index._File-Recid
  TRANSACTION:

    CREATE tt_Index.
    ASSIGN tt_Index.uIdx-Num    = DICTDB._Index._Idx-Num
           tt_Index.uIndex-Name = DICTDB._Index._Index-Name
           tt_Index.uFile-Recid = DICTDB._Index._File-Recid
           tt_Index.IndexAttr   =
             (IF DICTDB._Index._Active THEN "":U ELSE "i":U) +
             (IF tt_File.uPrime-Index EQ RECID(DICTDB._Index)
              THEN "p":U ELSE "":U) +
             (IF DICTDB._Index._Unique THEN "u":U ELSE "":U) +
             (IF DICTDB._Index._Wordidx GT 0 THEN "w":U ELSE "":U) +
             "c" + STRING(DICTDB._Index._num-comp)
           opDbIndexCount     = opDbIndexCount     + 1
           tt_File.IndexCount = tt_File.IndexCount + 1
    . /* ASSIGN */
  END. /* FOR EACH _Index */

/* HighestTableId: */
  FOR EACH tt_File NO-LOCK
        BY tt_File.uFile-Number DESCENDING:
    ASSIGN opHighestTableId = tt_File.uFile-Number.
    LEAVE.
  END.

/* HighestIndexId: */
  FOR EACH tt_Index NO-LOCK
        BY tt_Index.uIdx-Num DESCENDING:
    ASSIGN opHighestIndexId = tt_Index.uIdx-Num.
    LEAVE.
  END.

END PROCEDURE. /* ReadSchema */

/* -------------------------------------------------------------------- */
PROCEDURE DbStat.

  DEFINE VARIABLE vBiSize AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vLatchLock LIKE _Resrc._Resrc-lock /*_Latch._Latch-lock*/ NO-UNDO.
  DEFINE VARIABLE vLatchWait LIKE _Resrc._Resrc-wait /*_Latch._Latch-wait*/ NO-UNDO.
  DEFINE VARIABLE vResrcLock LIKE _Resrc._Resrc-lock           NO-UNDO.
  DEFINE VARIABLE vResrcWait LIKE _Resrc._Resrc-wait           NO-UNDO.
  DEFINE VARIABLE vTxeLock   LIKE _TxeLock._Txe-Locks EXTENT 0 NO-UNDO.
  DEFINE VARIABLE vTxeWait   LIKE _TxeLock._Txe-Waits EXTENT 0 NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  OUTPUT STREAM DumpFile TO VALUE( vDumpPrefix + "DbStat.txt":U ) APPEND.

/* Create a header: */
  IF SEEK(DumpFile) EQ 0 THEN
  PUT STREAM DumpFile UNFORMATTED
           "DbHost"
    {&Sep} "DbName"
    {&Sep} "Full DbPath"
    {&Sep} "UpTime"
    {&Sep} "DbBlockSize"
    {&Sep} "DbVersion"
    {&Sep} "ConnectNotes"
    {&Sep} "LastTranID"
    {&Sep} "DbSize"
    {&Sep} "BiSize"
    {&Sep} "Spin"
    {&Sep} "TableStatBase"
    {&Sep} "TableRangeSize"
    {&Sep} "HighestTableId"
    {&Sep} "TableCount"
    {&Sep} "IndexStatBase"
    {&Sep} "IndexRangeSize"
    {&Sep} "HighestIndexId"
    {&Sep} "IndexCount"
    {&Sep} "DbStartTime"
    {&Sep} "DbTimeStamp"
    {&Sep} "PrevSnapshot"
    {&Sep} "CurrSnapshot"
    {&Sep} "Interval"
    {&Sep} "DbAccesses"
    {&Sep} "RecRead"
    {&Sep} "RecUpdate"
    {&Sep} "RecCreate"
    {&Sep} "RecDelete"
    {&Sep} "BytesRead"
    {&Sep} "BytesUpdate"
    {&Sep} "BytesCreate"
    {&Sep} "BytesDelete"
    {&Sep} "IdxRead"
    {&Sep} "IdxCreate"
    {&Sep} "IdxDelete"
    {&Sep} "DbReads"
    {&Sep} "DatDbReads"
    {&Sep} "IdxDbReads"
    {&Sep} "DbWrites"
    {&Sep} "DatDbWrites"
    {&Sep} "IdxDbWrites"
    {&Sep} "BiWrites"
    {&Sep} "BiBytesWrtn"
    {&Sep} "BiNotesWrtn"
    {&Sep} "TransComm"
    {&Sep} "LatchLock"
    {&Sep} "LatchWait"
    {&Sep} "ResrcLock"
    {&Sep} "ResrcWait"
    {&Sep} "SemWaits"
    {&Sep} "TxeLock"
    {&Sep} "TxeWait"
    {&Sep} "ETime"
  SKIP. /* PUT */

/* Primary Recovery Area: */
  FOR FIRST DICTDB._AreaStatus NO-LOCK
      WHERE DICTDB._AreaStatus._AreaStatus-Areanum EQ 3,
      FIRST tt_Area NO-LOCK
      WHERE tt_Area.uArea-number EQ DICTDB._AreaStatus._AreaStatus-Areanum:

    ASSIGN vBiSize = DICTDB._AreaStatus._AreaStatus-Hiwater
                   * tt_Area.uArea-blocksize.
  END.

/* Resources: */
  ASSIGN vResrcLock = 0  /* Number of times the resource was locked */
         vResrcWait = 0  /* Number of times a process waited on resource */
  . /* ASSIGN */
  FOR EACH DICTDB._Resrc NO-LOCK:
    ASSIGN vResrcLock = vResrcLock + DICTDB._Resrc._Resrc-lock
           vResrcWait = vResrcWait + DICTDB._Resrc._Resrc-wait.
  END.

/* Latches: */
  ASSIGN vLatchLock = 0  /* Number of times the latch was locked */
         vLatchWait = 0  /* Number of times a process had to nap */
  . /* ASSIGN */
  FOR EACH DICTDB._Latch NO-LOCK
     WHERE DICTDB._Latch._Latch-id GT 1:
    ASSIGN vLatchLock = vLatchLock + DICTDB._Latch._Latch-lock
           vLatchWait = vLatchWait + DICTDB._Latch._Latch-wait.
  END.

/* TXE Locks: */
  FOR FIRST DICTDB._TxeLock NO-LOCK:
    DO i = 1 TO 9:
      ASSIGN vTxeLock = vTxeLock + DICTDB._TxeLock._Txe-Locks[i]
             vTxeWait = vTxeWait + DICTDB._TxeLock._Txe-Waits[i].
    END.
  END.

  FOR FIRST ttDbStat EXCLUSIVE
      WHERE ttDbStat.DbHost EQ vDbHost
        AND ttDbStat.DbPath EQ vDbPath,

      FIRST tt_Area NO-LOCK WHERE tt_Area.uArea-number EQ 6, /* Schema Area */

      FIRST DICTDB._ActSummary NO-LOCK,
      FIRST DICTDB._ActIOType  NO-LOCK,
      FIRST DICTDB._ActRecord  NO-LOCK,
      FIRST DICTDB._ActIndex   NO-LOCK,
      FIRST DICTDB._ActBILog   NO-LOCK,
      FIRST DICTDB._ActOther   NO-LOCK,
      FIRST DICTDB._MstrBlk    NO-LOCK,
      FIRST DICTDB._Startup    NO-LOCK
  TRANSACTION:

    IF ttDbStat.DbTimeStamp NE ? THEN
    IF ttDbStat.DbTimeStamp NE DICTDB._MstrBlk._MstrBlk-TimeStamp THEN
    ASSIGN vConnect = "SchemaChanged ":U + vConnect.

    PUT STREAM DumpFile UNFORMATTED
/* DbHost:        */        vDbHost
/* DbName:        */ {&Sep} LDBNAME("DICTDB":U)
/* Full DbPath:   */ {&Sep} vDbPath
/* UpTime:        */ {&Sep} DICTDB._ActSummary._Summary-UpTime
/* DbBlockSize:   */ {&Sep} tt_Area.uArea-blocksize
/* DbVersion:     */ {&Sep} DICTDB._MstrBlk._MstrBlk-DbVers - tt_Area.uArea-blocksize
/* DbRestriction: */ {&Sep} vConnect " ":U DBRESTRICTION("DICTDB":U)
/* LastTranID:    */ {&Sep} DICTDB._MstrBlk._MstrBlk-LastTask
/* DbSize:        */ {&Sep} STRING(vDbSize * tt_Area.uArea-blocksize)
/* BiSize:        */ {&Sep} STRING(vBiSize)
/* Spin:          */ {&Sep} DICTDB._Startup._Startup-Spin
/* TableStatBase: */ {&Sep} vMinTableStatId
/* TableRangeSize:*/ {&Sep} vTableRangeSize
/* HighestTableId:*/ {&Sep} vHighestTableId
/* TableCount:    */ {&Sep} vDbTableCount
/* IndexStatBase: */ {&Sep} vMinIndexStatId
/* IndexRangeSize:*/ {&Sep} vIndexRangeSize
/* HighestIndexId:*/ {&Sep} vHighestIndexId
/* IndexCount:    */ {&Sep} vDbIndexCount
/* DbStartTime:   */ {&Sep} DICTDB._MstrBlk._MstrBlk-oprdate
/* DbTimeStamp:   */ {&Sep} DICTDB._MstrBlk._MstrBlk-TimeStamp

/* PrevSnapshot:  */ {&Sep} ttDbStat.SnapshotTimeStamp
/* CurrSnapshot:  */ {&Sep} ipTimeStamp
/* Interval:      */ {&Sep} vCurrInterval

/* DbAccesses: */ {&Sep} DICTDB._ActSummary._Summary-DbAccesses - ttDbStat.DbAccesses

/* RecRead:    */ {&Sep} DICTDB._ActRecord._Record-RecRead  - ttDbStat.RecRead
/* RecUpdate:  */ {&Sep} DICTDB._ActRecord._Record-RecUpd   - ttDbStat.RecUpdate
/* RecCreate:  */ {&Sep} DICTDB._ActRecord._Record-RecCreat - ttDbStat.RecCreate
/* RecDelete:  */ {&Sep} DICTDB._ActRecord._Record-RecDel   - ttDbStat.RecDelete

/* BytesRead:  */ {&Sep} DICTDB._ActRecord._Record-BytesRead  - ttDbStat.BytesRead
/* BytesUpdate:*/ {&Sep} DICTDB._ActRecord._Record-BytesUpd   - ttDbStat.BytesUpdate
/* BytesCreate:*/ {&Sep} DICTDB._ActRecord._Record-BytesCreat - ttDbStat.BytesCreate
/* BytesDelete:*/ {&Sep} DICTDB._ActRecord._Record-BytesDel   - ttDbStat.BytesDelete

/* IdxRead:    */ {&Sep} DICTDB._ActIndex._Index-Find   - ttDbStat.IdxRead
/* IdxCreate:  */ {&Sep} DICTDB._ActIndex._Index-Create - ttDbStat.IdxCreate
/* IdxDelete:  */ {&Sep} DICTDB._ActIndex._Index-Delete - ttDbStat.IdxDelete

/* DbReads:    */ {&Sep} DICTDB._ActSummary._Summary-DbReads  - ttDbStat.DbReads
/* DatDbReads: */ {&Sep} DICTDB._ActIOType._IOType-DataReads  - ttDbStat.DatDbReads
/* IdxDbReads: */ {&Sep} DICTDB._ActIOType._IOType-IdxRds     - ttDbStat.IdxDbReads
/* DbWrites:   */ {&Sep} DICTDB._ActSummary._Summary-DbWrites - ttDbStat.DbWrites
/* DatDbWrites:*/ {&Sep} DICTDB._ActIOType._IOType-DataWrts   - ttDbStat.DatDbWrites
/* IdxDbWrites:*/ {&Sep} DICTDB._ActIOType._IOType-IdxWrts    - ttDbStat.IdxDbWrites

/* BiWrites:   */ {&Sep} DICTDB._ActBILog._BiLog-TotalWrts - ttDbStat.BiWrites
/* BiBytesWrtn:*/ {&Sep} DICTDB._ActBILog._BiLog-BytesWrtn - ttDbStat.BiBytesWrtn
/* BiNotesWrtn:*/ {&Sep} DICTDB._ActBILog._BiLog-RecWriten - ttDbStat.BiNotesWrtn
/* TransComm:  */ {&Sep} DICTDB._ActSummary._Summary-TransComm  - ttDbStat.TransComm

/* LatchLock:  */ {&Sep} vLatchLock - ttDbStat.LatchLock
/* LatchWait:  */ {&Sep} vLatchWait - ttDbStat.LatchWait
/* ResrcLock:  */ {&Sep} vResrcLock - ttDbStat.ResrcLock
/* ResrcWait:  */ {&Sep} vResrcWait - ttDbStat.ResrcWait
/* SemWaits:   */ {&Sep} DICTDB._ActOther._Other-Wait - ttDbStat.SemWaits
/* TxeLock:    */ {&Sep} vTxeLock   - ttDbStat.TxeLock
/* TxeWait:    */ {&Sep} vTxeWait   - ttDbStat.TxeWait

/* ETime:      */ {&Sep} ETIME
    SKIP. /* PUT */

/* Save the current snapshot: */
    ASSIGN
      ttDbStat.DbStartTime = DICTDB._MstrBlk._MstrBlk-oprdate
      ttDbStat.DbTimeStamp = DICTDB._MstrBlk._MstrBlk-TimeStamp

      ttDbStat.SnapshotTimeStamp = ipTimeStamp

      ttDbStat.DbAccesses  = DICTDB._ActSummary._Summary-DbAccesses
      ttDbStat.RecRead     = DICTDB._ActRecord._Record-RecRead
      ttDbStat.RecUpdate   = DICTDB._ActRecord._Record-RecUpd
      ttDbStat.RecCreate   = DICTDB._ActRecord._Record-RecCreat
      ttDbStat.RecDelete   = DICTDB._ActRecord._Record-RecDel

      ttDbStat.BytesRead   = DICTDB._ActRecord._Record-BytesRead
      ttDbStat.BytesUpdate = DICTDB._ActRecord._Record-BytesUpd
      ttDbStat.BytesCreate = DICTDB._ActRecord._Record-BytesCreat
      ttDbStat.BytesDelete = DICTDB._ActRecord._Record-BytesDel

      ttDbStat.IdxRead     = DICTDB._ActIndex._Index-Find
      ttDbStat.IdxCreate   = DICTDB._ActIndex._Index-Create
      ttDbStat.IdxDelete   = DICTDB._ActIndex._Index-Delete

      ttDbStat.DbReads     = DICTDB._ActSummary._Summary-DbReads
      ttDbStat.DbWrites    = DICTDB._ActSummary._Summary-DbWrites
      ttDbStat.DatDbReads  = DICTDB._ActIOType._IOType-DataReads
      ttDbStat.DatDbWrites = DICTDB._ActIOType._IOType-DataWrts
      ttDbStat.IdxDbReads  = DICTDB._ActIOType._IOType-IdxRds
      ttDbStat.IdxDbWrites = DICTDB._ActIOType._IOType-IdxWrts

      ttDbStat.BiWrites    = DICTDB._ActBILog._BiLog-TotalWrts
      ttDbStat.BiBytesWrtn = DICTDB._ActBILog._BiLog-BytesWrtn
      ttDbStat.BiNotesWrtn = DICTDB._ActBILog._BiLog-RecWriten
      ttDbStat.TransComm   = DICTDB._ActSummary._Summary-TransComm

      ttDbStat.LatchLock   = vLatchLock
      ttDbStat.LatchWait   = vLatchWait
      ttDbStat.ResrcLock   = vResrcLock
      ttDbStat.ResrcWait   = vResrcWait
      ttDbStat.SemWaits    = DICTDB._ActOther._Other-Wait
      ttDbStat.TxeLock     = vTxeLock
      ttDbStat.TxeWait     = vTxeWait
    . /* ASSIGN */
  END. /* FOR FIRST ttDbStat */

/* Flush data on disk: */
  PUT STREAM DumpFile CONTROL NULL(0).

  OUTPUT STREAM DumpFile CLOSE.
END PROCEDURE. /* DbStat */

/* -------------------------------------------------------------------- */
PROCEDURE AreaStat.

  DEFINE VARIABLE vExtentId AS INTEGER NO-UNDO.

  OUTPUT STREAM DumpFile TO VALUE( vDumpPrefix + "AreaStat.txt":U ) APPEND.

/* Create a header: */
  IF SEEK(DumpFile) EQ 0 THEN
  PUT STREAM DumpFile UNFORMATTED
           "DbHost"
    {&Sep} "DbName"
    {&Sep} "AreaName"
    {&Sep} "AreaNumber"
    {&Sep} "AreaRPB"
    {&Sep} "AreaClusterSize"
    {&Sep} "AreaExtents"
    {&Sep} "TableCount"
    {&Sep} "IndexCount"
    {&Sep} "AreaHWM"
    {&Sep} "AreaReads"
    {&Sep} "AreaWrites"
    {&Sep} "TableRead"
    {&Sep} "TableUpdate"
    {&Sep} "TableCreate"
    {&Sep} "TableDelete"
    {&Sep} "IndexRead"
    {&Sep} "IndexCreate"
    {&Sep} "IndexDelete"
  SKIP. /* PUT */

/* Performance rules for remote connections:
   1. Use FOR EACH for db tables when possible.
   2. Don't read db tables inside transaction block.
*/
/* Initiate ttAreaStat records and copy _AreaStatus to tt_Area: */
  FOR EACH DICTDB._AreaStatus NO-LOCK
     WHERE DICTDB._AreaStatus._AreaStatus-Areanum GE 6,

     FIRST tt_Area NO-LOCK
     WHERE tt_Area.uArea-number EQ DICTDB._AreaStatus._AreaStatus-Areanum
  TRANSACTION:

    FIND FIRST ttAreaStat NO-LOCK
         WHERE ttAreaStat.DbHost     EQ vDbHost
           AND ttAreaStat.DbPath     EQ vDbPath
           AND ttAreaStat.AreaNumber EQ tt_Area.uArea-number
    NO-ERROR.

    IF NOT AVAILABLE ttAreaStat THEN
    CREATE ttAreaStat.

    IF vIgnorePrevStat THEN
    ASSIGN
      ttAreaStat.AreaHWM     = 0
      ttAreaStat.AreaReads   = 0
      ttAreaStat.AreaWrites  = 0
      ttAreaStat.TableRead   = 0
      ttAreaStat.TableUpdate = 0
      ttAreaStat.TableCreate = 0
      ttAreaStat.TableDelete = 0
      ttAreaStat.IndexRead   = 0
      ttAreaStat.IndexCreate = 0
      ttAreaStat.IndexDelete = 0
    . /* ASSIGN */
    
    ASSIGN
      ttAreaStat.DbHost      = vDbHost
      ttAreaStat.DbPath      = vDbPath
      ttAreaStat.AreaNumber  = tt_Area.uArea-number
      ttAreaStat.AreaName    = tt_Area.uArea-Name

      tt_Area.AreaHWM    = DICTDB._AreaStatus._AreaStatus-Hiwater
      tt_Area.AreaReads  = 0
      tt_Area.AreaWrites = 0
    . /* ASSIGN */
  END.

  ASSIGN vExtentId = 0.
  FOR EACH DICTDB._AreaExtent NO-LOCK:

    ASSIGN vExtentId = vExtentId + 1.
/* The records in _ActIOFile and _AreaExtent used to be in the same order: */
    FIND FIRST DICTDB._ActIOFile NO-LOCK
         WHERE DICTDB._ActIOFile._IOFile-Id EQ vExtentId
    NO-ERROR.

/* ...otherwise do a full scan for the matches: */
    IF NOT AVAILABLE DICTDB._ActIOFile
    OR NOT DICTDB._ActIOFile._IOFile-FileName MATCHES
    ("*" + DICTDB._AreaExtent._Extent-path)
    THEN
    FIND FIRST DICTDB._ActIOFile NO-LOCK
         WHERE DICTDB._ActIOFile._IOFile-FileName MATCHES
        ("*" + DICTDB._AreaExtent._Extent-path)
    NO-ERROR.

    IF AVAILABLE DICTDB._ActIOFile THEN
    FOR FIRST tt_Area
        WHERE tt_Area.uArea-number EQ DICTDB._AreaExtent._Area-number:
      ASSIGN
        vExtentId          = DICTDB._ActIOFile._IOFile-Id
        tt_Area.AreaReads  = tt_Area.AreaReads
                           + DICTDB._ActIOFile._IOFile-Reads
        tt_Area.AreaWrites = tt_Area.AreaWrites
                           + DICTDB._ActIOFile._IOFile-Writes
      . /* ASSIGN */
    END.
  END. /* FOR EACH DICTDB._AreaExtent OF DICTDB._Area */

  FOR EACH tt_Area NO-LOCK
     WHERE tt_Area.uArea-number GE 6,
    
     FIRST ttAreaStat EXCLUSIVE
     WHERE ttAreaStat.AreaNumber EQ tt_Area.uArea-number:

    PUT STREAM DumpFile UNFORMATTED
    /* DbHost:         */        vDbHost
    /* DbName:         */ {&Sep} LDBNAME("DICTDB":U)
    /* AreaName:       */ {&Sep} tt_Area.uArea-name
    /* AreaNumber:     */ {&Sep} tt_Area.uArea-number
    /* AreaRPB:        */ {&Sep} INTEGER(EXP(2, tt_Area.uArea-recbits))
    /* AreaClusterSize:*/ {&Sep} tt_Area.uArea-clustersize
    /* AreaExtents:    */ {&Sep} tt_Area.uArea-extents
    /* TableCount:     */ {&Sep} tt_Area.TableCount
    /* IndexCount:     */ {&Sep} tt_Area.IndexCount
    /* AreaHWM:        */ {&Sep} tt_Area.AreaHWM
    /* AreaReads:      */ {&Sep} tt_Area.AreaReads   - ttAreaStat.AreaReads
    /* AreaWrites:     */ {&Sep} tt_Area.AreaWrites  - ttAreaStat.AreaWrites
    /* TableRead:      */ {&Sep} tt_Area.TableRead   - ttAreaStat.TableRead
    /* TableUpdate:    */ {&Sep} tt_Area.TableUpdate - ttAreaStat.TableUpdate
    /* TableCreate:    */ {&Sep} tt_Area.TableCreate - ttAreaStat.TableCreate
    /* TableDelete:    */ {&Sep} tt_Area.TableDelete - ttAreaStat.TableDelete
    /* IndexRead:      */ {&Sep} tt_Area.IndexRead   - ttAreaStat.IndexRead
    /* IndexCreate:    */ {&Sep} tt_Area.IndexCreate - ttAreaStat.IndexCreate
    /* IndexDelete:    */ {&Sep} tt_Area.IndexDelete - ttAreaStat.IndexDelete
    SKIP. /* PUT */

    ASSIGN
      vDbSize = vDbSize + tt_Area.AreaHWM
/* Save the current snapshot: */
      ttAreaStat.AreaReads   = tt_Area.AreaReads
      ttAreaStat.AreaWrites  = tt_Area.AreaWrites
      ttAreaStat.TableRead   = tt_Area.TableRead
      ttAreaStat.TableUpdate = tt_Area.TableUpdate
      ttAreaStat.TableCreate = tt_Area.TableCreate
      ttAreaStat.TableDelete = tt_Area.TableDelete
      ttAreaStat.IndexRead   = tt_Area.IndexRead
      ttAreaStat.IndexCreate = tt_Area.IndexCreate
      ttAreaStat.IndexDelete = tt_Area.IndexDelete
    . /* ASSIGN */

    ACCUMULATE tt_Area.uArea-extents (TOTAL).
    ACCUMULATE tt_Area.TableCount    (TOTAL).
    ACCUMULATE tt_Area.IndexCount    (TOTAL).
/* DbStat reports vDbTableCount and vDbIndexCount.
   The accumulated values reported by AreaStat must match the values in DbStat.
   It's just a kind of sanity check.
*/
  END. /* FOR EACH _Area */

  FOR FIRST ttDbStat NO-LOCK
      WHERE ttDbStat.DbHost EQ vDbHost
        AND ttDbStat.DbPath EQ vDbPath,

      FIRST DICTDB._ActRecord NO-LOCK,
      FIRST DICTDB._ActIndex  NO-LOCK,
      FIRST DICTDB._ActIOType NO-LOCK:

    PUT STREAM DumpFile UNFORMATTED
/* DbHost:         */        vDbHost
/* DbName:         */ {&Sep} LDBNAME("DICTDB":U)
/* AreaName:       */ {&Sep} "WholeDb.DataBlocks"
/* AreaNumber:     */ {&Sep} 0
/* AreaRPB:        */ {&Sep} 0
/* AreaClusterSize:*/ {&Sep} 0
/* AreaExtents:    */ {&Sep} ACCUM TOTAL tt_Area.uArea-extents
/* TableCount:     */ {&Sep} vDbTableCount
/* IndexCount:     */ {&Sep} vDbIndexCount
/* AreaHWM:        */ {&Sep} vDbSize
/* AreaReads:      */ {&Sep} DICTDB._ActIOType._IOType-DataReads - ttDbStat.DatDbReads
/* AreaWrites:     */ {&Sep} DICTDB._ActIOType._IOType-DataWrts  - ttDbStat.DatDbWrites
/* TableRead:      */ {&Sep} DICTDB._ActRecord._Record-RecRead   - ttDbStat.RecRead
/* TableUpdate:    */ {&Sep} DICTDB._ActRecord._Record-RecUpd    - ttDbStat.RecUpdate
/* TableCreate:    */ {&Sep} DICTDB._ActRecord._Record-RecCreat  - ttDbStat.RecCreate
/* TableDelete:    */ {&Sep} DICTDB._ActRecord._Record-RecDel    - ttDbStat.RecDelete
/* IndexRead:      */ {&Sep} 0
/* IndexCreate:    */ {&Sep} 0
/* IndexDelete:    */ {&Sep} 0
                       SKIP

/* DbHost:         */        vDbHost
/* DbName:         */ {&Sep} LDBNAME("DICTDB":U)
/* AreaName:       */ {&Sep} "WholeDb.IndexBlocks"
/* AreaNumber:     */ {&Sep} 0
/* AreaRPB:        */ {&Sep} 0
/* AreaClusterSize:*/ {&Sep} 0
/* AreaExtents:    */ {&Sep} ACCUM TOTAL tt_Area.uArea-extents
/* TableCount:     */ {&Sep} vDbTableCount
/* IndexCount:     */ {&Sep} vDbIndexCount
/* AreaHWM:        */ {&Sep} vDbSize
/* AreaReads:      */ {&Sep} DICTDB._ActIOType._IOType-IdxRds  - ttDbStat.IdxDbReads
/* AreaWrites:     */ {&Sep} DICTDB._ActIOType._IOType-IdxWrts - ttDbStat.IdxDbWrites
/* RecRead:        */ {&Sep} 0
/* RecUpdate:      */ {&Sep} 0
/* RecCreate:      */ {&Sep} 0
/* RecDelete:      */ {&Sep} 0
/* IndexRead:      */ {&Sep} DICTDB._ActIndex._Index-Find   - ttDbStat.IdxRead
/* IndexCreate:    */ {&Sep} DICTDB._ActIndex._Index-Create - ttDbStat.IdxCreate
/* IndexDelete:    */ {&Sep} DICTDB._ActIndex._Index-Delete - ttDbStat.IdxDelete
    SKIP. /* PUT */
  END. /* FOR FIRST ttDbStat */

/* Flush data on disk: */
  PUT STREAM DumpFile CONTROL NULL(0).

  OUTPUT STREAM DumpFile CLOSE.
END PROCEDURE. /* AreaStat */

/* -------------------------------------------------------------------- */
PROCEDURE TableStat.

  OUTPUT STREAM DumpFile TO VALUE( vDumpPrefix + "TableStat.txt":U ) APPEND.

/* Create a header: */
  IF SEEK(DumpFile) EQ 0 THEN
  PUT STREAM DumpFile UNFORMATTED
           "DbHost"
    {&Sep} "DbName"
    {&Sep} "AreaName"
    {&Sep} "TableName"
    {&Sep} "IndexCount"
    {&Sep} "TableRead"
    {&Sep} "TableUpdate"
    {&Sep} "TableCreate"
    {&Sep} "TableDelete"
&IF DEFINED(Version_GE_102B)
&THEN
    {&Sep} "TableOsRead"
&ENDIF
    {&Sep} "TableRead"
    {&Sep} "TableUpdate"
    {&Sep} "TableCreate"
    {&Sep} "TableDelete"
&IF DEFINED(Version_GE_102B)
&THEN
    {&Sep} "TableOsRead"
&ENDIF
  SKIP. /* PUT */

  FOR FIRST ttDbStat NO-LOCK
      WHERE ttDbStat.DbHost EQ vDbHost
        AND ttDbStat.DbPath EQ vDbPath,

      FIRST DICTDB._ActRecord NO-LOCK,
      FIRST DICTDB._ActIOType NO-LOCK:

    PUT STREAM DumpFile UNFORMATTED
/* DbHost:      */        vDbHost
/* DbName:      */ {&Sep} LDBNAME("DICTDB":U)
/* AreaName:    */ {&Sep} "WholeDb"
/* TableName:   */ {&Sep} "Bytes"
/* IndexCount   */ {&Sep} 0
/* BytesRead:   */ {&Sep} DICTDB._ActRecord._Record-BytesRead  - ttDbStat.BytesRead
/* BytesUpdate: */ {&Sep} DICTDB._ActRecord._Record-BytesUpd   - ttDbStat.BytesUpdate
/* BytesCreate: */ {&Sep} DICTDB._ActRecord._Record-BytesCreat - ttDbStat.BytesCreate
/* BytesDelete: */ {&Sep} DICTDB._ActRecord._Record-BytesDel   - ttDbStat.BytesDelete
&IF DEFINED(Version_GE_102B)
&THEN
/* TableOsRead: */ {&Sep} 0
&ENDIF
/* BytesRead:   */ {&Sep} PerSec(DICTDB._ActRecord._Record-BytesRead  - ttDbStat.BytesRead)
/* BytesUpdate: */ {&Sep} PerSec(DICTDB._ActRecord._Record-BytesUpd   - ttDbStat.BytesUpdate)
/* BytesCreate: */ {&Sep} PerSec(DICTDB._ActRecord._Record-BytesCreat - ttDbStat.BytesCreate)
/* BytesDelete: */ {&Sep} PerSec(DICTDB._ActRecord._Record-BytesDel   - ttDbStat.BytesDelete)
&IF DEFINED(Version_GE_102B)
&THEN
/* TableOsRead: */ {&Sep} 0.0
&ENDIF
                    SKIP
/* DbHost:      */        vDbHost
/* DbName:      */ {&Sep} LDBNAME("DICTDB":U)
/* AreaName:    */ {&Sep} "WholeDb"
/* TableName:   */ {&Sep} "Records"
/* IndexCount   */ {&Sep} 0
/* RecRead:     */ {&Sep} DICTDB._ActRecord._Record-RecRead  - ttDbStat.RecRead
/* RecUpdate:   */ {&Sep} DICTDB._ActRecord._Record-RecUpd   - ttDbStat.RecUpdate
/* RecCreate:   */ {&Sep} DICTDB._ActRecord._Record-RecCreat - ttDbStat.RecCreate
/* RecDelete:   */ {&Sep} DICTDB._ActRecord._Record-RecDel   - ttDbStat.RecDelete
&IF DEFINED(Version_GE_102B)
&THEN
/* TableOsRead: */ {&Sep} DICTDB._ActIOType._IOType-DataReads - ttDbStat.DatDbReads
&ENDIF
/* RecRead:     */ {&Sep} PerSec(DICTDB._ActRecord._Record-RecRead  - ttDbStat.RecRead)
/* RecUpdate:   */ {&Sep} PerSec(DICTDB._ActRecord._Record-RecUpd   - ttDbStat.RecUpdate)
/* RecCreate:   */ {&Sep} PerSec(DICTDB._ActRecord._Record-RecCreat - ttDbStat.RecCreate)
/* RecDelete:   */ {&Sep} PerSec(DICTDB._ActRecord._Record-RecDel   - ttDbStat.RecDelete)
&IF DEFINED(Version_GE_102B)
&THEN
/* TableOsRead: */ {&Sep} PerSec(DICTDB._ActIOType._IOType-DataReads - ttDbStat.DatDbReads)
&ENDIF
    SKIP. /* PUT */
  END. /* FOR FIRST ttDbStat, FIRST _ActRecord */

  FOR EACH DICTDB._TableStat NO-LOCK,

     FIRST tt_File NO-LOCK
     WHERE tt_File.uFile-Number EQ DICTDB._TableStat._TableStat-id,

     FIRST tt_StorageObject NO-LOCK
     WHERE tt_StorageObject.uObject-Type   EQ 1
       AND tt_StorageObject.uObject-Number EQ tt_File.uFile-Number,

     FIRST tt_Area
     WHERE tt_Area.uArea-Number EQ tt_StorageObject.uArea-Number

     WHILE DICTDB._TableStat._TableStat-id LE vHighestTableId
  TRANSACTION:

    FIND FIRST ttTableStat EXCLUSIVE
         WHERE ttTableStat.DbHost      EQ vDbHost
           AND ttTableStat.DbPath      EQ vDbPath
           AND ttTableStat.TableNumber EQ tt_File.uFile-Number
    NO-ERROR.

    IF NOT AVAILABLE ttTableStat THEN
    CREATE ttTableStat.

    IF vIgnorePrevStat THEN
    ASSIGN
      ttTableStat.TableRead   = 0
      ttTableStat.TableUpdate = 0
      ttTableStat.TableCreate = 0
      ttTableStat.TableDelete = 0
      &IF DEFINED(Version_GE_102B)
      &THEN
      ttTableStat.TableOsRead = 0
      &ENDIF
    . /* ASSIGN */

    PUT STREAM DumpFile UNFORMATTED
/* DbHost:      */        vDbHost
/* DbName:      */ {&Sep} LDBNAME("DICTDB":U)
/* AreaName:    */ {&Sep} tt_Area.uArea-Name
/* TableName:   */ {&Sep} tt_File.uFile-Name
/* IndexCount   */ {&Sep} tt_File.IndexCount
/* TableRead:   */ {&Sep} DICTDB._TableStat._TableStat-Read   - ttTableStat.TableRead
/* TableUpdate: */ {&Sep} DICTDB._TableStat._TableStat-Update - ttTableStat.TableUpdate
/* TableCreate: */ {&Sep} DICTDB._TableStat._TableStat-Create - ttTableStat.TableCreate
/* TableDelete: */ {&Sep} DICTDB._TableStat._TableStat-Delete - ttTableStat.TableDelete
&IF DEFINED(Version_GE_102B)
&THEN
                   {&Sep} DICTDB._TableStat._TableStat-OsRead - ttTableStat.TableOsRead
&ENDIF
/* TableRead:   */ {&Sep} PerSec(DICTDB._TableStat._TableStat-Read   - ttTableStat.TableRead)
/* TableUpdate: */ {&Sep} PerSec(DICTDB._TableStat._TableStat-Update - ttTableStat.TableUpdate)
/* TableCreate: */ {&Sep} PerSec(DICTDB._TableStat._TableStat-Create - ttTableStat.TableCreate)
/* TableDelete: */ {&Sep} PerSec(DICTDB._TableStat._TableStat-Delete - ttTableStat.TableDelete)
&IF DEFINED(Version_GE_102B)
&THEN
                   {&Sep} PerSec(DICTDB._TableStat._TableStat-OsRead - ttTableStat.TableOsRead)
&ENDIF
    SKIP. /* PUT */

    ASSIGN
/* Accumulate table stats per area: */
      tt_Area.TableCount  = tt_Area.TableCount  + 1
      tt_Area.TableRead   = tt_Area.TableRead   + DICTDB._TableStat._TableStat-Read
      tt_Area.TableUpdate = tt_Area.TableUpdate + DICTDB._TableStat._TableStat-Update
      tt_Area.TableCreate = tt_Area.TableCreate + DICTDB._TableStat._TableStat-Create
      tt_Area.TableDelete = tt_Area.TableDelete + DICTDB._TableStat._TableStat-Delete
/* Save the current snapshot: */
      ttTableStat.DbHost      = vDbHost
      ttTableStat.DbPath      = vDbPath
      ttTableStat.TableNumber = tt_File.uFile-Number
      ttTableStat.TableName   = tt_File.uFile-Name
      ttTableStat.TableRead   = DICTDB._TableStat._TableStat-Read
      ttTableStat.TableUpdate = DICTDB._TableStat._TableStat-Update
      ttTableStat.TableCreate = DICTDB._TableStat._TableStat-Create
      ttTableStat.TableDelete = DICTDB._TableStat._TableStat-Delete
      &IF DEFINED(Version_GE_102B)
      &THEN
      ttTableStat.TableOsRead = DICTDB._TableStat._TableStat-OsRead
      &ENDIF
    . /* ASSIGN  */
  END. /* FOR EACH _TableStat etc */

/* The list of tables outside TableRangeSize: */
  FOR EACH tt_File NO-LOCK
     WHERE tt_File.uFile-Number LT vMinTableStatId
        OR tt_File.uFile-Number GT vMaxTableStatId,

     FIRST tt_StorageObject NO-LOCK
     WHERE tt_StorageObject.uObject-Type   EQ 1
       AND tt_StorageObject.uObject-Number EQ tt_File.uFile-Number,

     FIRST tt_Area
     WHERE tt_Area.uArea-Number EQ tt_StorageObject.uArea-Number:

    PUT STREAM DumpFile UNFORMATTED
/* DbHost:      */        vDbHost
/* DbName:      */ {&Sep} LDBNAME("DICTDB":U)
/* AreaName:    */ {&Sep} tt_Area.uArea-Name
/* TableName:   */ {&Sep} tt_File.uFile-Name
/* IndexCount   */ {&Sep} tt_File.IndexCount
/* TableRead:   */ {&Sep} "?":U
/* TableUpdate: */ {&Sep} "?":U
/* TableCreate: */ {&Sep} "?":U
/* TableDelete: */ {&Sep} "?":U
&IF DEFINED(Version_GE_102B)
&THEN
/* TableOsRead: */ {&Sep} "?":U
&ENDIF
/* TableRead:   */ {&Sep} "?":U
/* TableUpdate: */ {&Sep} "?":U
/* TableCreate: */ {&Sep} "?":U
/* TableDelete: */ {&Sep} "?":U
&IF DEFINED(Version_GE_102B)
&THEN
/* TableOsRead: */ {&Sep} "?":U
&ENDIF
    SKIP. /* PUT */

    ASSIGN tt_Area.TableCount = tt_Area.TableCount + 1.
  END. /* EACH DICTDB._TableStat */

/* Flush data on disk: */
  PUT STREAM DumpFile CONTROL NULL(0).

  OUTPUT STREAM DumpFile CLOSE.
END PROCEDURE. /* TableStat */

/* -------------------------------------------------------------------- */
PROCEDURE IndexStat.

  OUTPUT STREAM DumpFile TO VALUE( vDumpPrefix + "IndexStat.txt":U ) APPEND.

/* Create a header: */
  IF SEEK(DumpFile) EQ 0 THEN
  PUT STREAM DumpFile UNFORMATTED
           "DbHost"
    {&Sep} "DbName"
    {&Sep} "AreaName"
    {&Sep} "TableName"
    {&Sep} "IndexName"
    {&Sep} "IndexAttr"
    {&Sep} "RootBlock"
    {&Sep} "IndexRead"
    {&Sep} "IndexCreate"
    {&Sep} "IndexDelete"
&IF DEFINED(Version_GE_102B)
&THEN
    {&Sep} "IndexOsRead"
&ENDIF
    {&Sep} "IndexRead"
    {&Sep} "IndexCreate"
    {&Sep} "IndexDelete"
&IF DEFINED(Version_GE_102B)
&THEN
    {&Sep} "IndexOsRead"
&ENDIF
  SKIP. /* PUT */

  FOR FIRST ttDbStat NO-LOCK
      WHERE ttDbStat.DbHost EQ vDbHost
        AND ttDbStat.DbPath EQ vDbPath,

      FIRST DICTDB._ActIndex  NO-LOCK,
      FIRST DICTDB._ActIOType NO-LOCK:

    PUT STREAM DumpFile UNFORMATTED
/* DbHost:    */        vDbHost
/* DbName:    */ {&Sep} LDBNAME("DICTDB":U)
/* AreaName:  */ {&Sep} "WholeDb"
/* TableName: */ {&Sep} "AllTables"
/* IndexName: */ {&Sep} "AllIndexes"
/* IndexAttr: */ {&Sep} "":U
/* RootBlock: */ {&Sep} 0
/* IdxRead:   */ {&Sep} DICTDB._ActIndex._Index-Find   - ttDbStat.IdxRead
/* IdxCreate: */ {&Sep} DICTDB._ActIndex._Index-Create - ttDbStat.IdxCreate
/* IdxDelete: */ {&Sep} DICTDB._ActIndex._Index-Delete - ttDbStat.IdxDelete
&IF DEFINED(Version_GE_102B)
&THEN
/*IndexOsRead:*/ {&Sep} DICTDB._ActIOType._IOType-IdxRds - ttDbStat.IdxDbReads
&ENDIF
/* IdxRead:   */ {&Sep} PerSec(DICTDB._ActIndex._Index-Find   - ttDbStat.IdxRead  )
/* IdxCreate: */ {&Sep} PerSec(DICTDB._ActIndex._Index-Create - ttDbStat.IdxCreate)
/* IdxDelete: */ {&Sep} PerSec(DICTDB._ActIndex._Index-Delete - ttDbStat.IdxDelete)
&IF DEFINED(Version_GE_102B)
&THEN
/*IndexOsRead:*/ {&Sep} PerSec(DICTDB._ActIOType._IOType-IdxRds - ttDbStat.IdxDbReads)
&ENDIF
    SKIP. /* PUT */
  END. /* FOR FIRST ttDbStat, FIRST _ActIndex */

  FOR EACH DICTDB._IndexStat NO-LOCK,

     FIRST tt_Index NO-LOCK
     WHERE tt_Index.uIdx-Num EQ DICTDB._IndexStat._IndexStat-id,

     FIRST tt_StorageObject NO-LOCK
     WHERE tt_StorageObject.uObject-Type   EQ 2
       AND tt_StorageObject.uObject-Number EQ tt_Index.uIdx-Num,

     FIRST tt_File NO-LOCK
     WHERE tt_File.uFileRecid EQ tt_Index.uFile-Recid,

     FIRST tt_Area NO-LOCK
     WHERE tt_Area.uArea-Number EQ tt_StorageObject.uArea-Number

     WHILE DICTDB._IndexStat._IndexStat-id LE vHighestIndexId
  TRANSACTION:

    FIND FIRST ttIndexStat
         WHERE ttIndexStat.DbHost      EQ vDbHost
           AND ttIndexStat.DbPath      EQ vDbPath
           AND ttIndexStat.IndexNumber EQ tt_Index.uIdx-Num
    NO-ERROR.

    IF NOT AVAILABLE ttIndexStat THEN
    CREATE ttIndexStat.

    IF vIgnorePrevStat THEN
    ASSIGN
      ttIndexStat.IndexRead   = 0
      ttIndexStat.IndexCreate = 0
      ttIndexStat.IndexDelete = 0
      &IF DEFINED(Version_GE_102B)
      &THEN
      ttIndexStat.IndexOsRead = 0
      &ENDIF

    . /* ASSIGN */


    PUT STREAM DumpFile UNFORMATTED
/* DbHost:     */        vDbHost
/* DbName:     */ {&Sep} LDBNAME("DICTDB":U)
/* AreaName:   */ {&Sep} tt_Area.uArea-Name
/* TableName:  */ {&Sep} tt_File.uFile-Name
/* IndexName:  */ {&Sep} tt_Index.uIndex-Name
/* IndexAttr:  */ {&Sep} tt_Index.IndexAttr
/* RootBlock:  */ {&Sep} tt_StorageObject.uObject-root
/* IndexRead:  */ {&Sep} DICTDB._IndexStat._IndexStat-Read   - ttIndexStat.IndexRead
/* IndexCreate:*/ {&Sep} DICTDB._IndexStat._IndexStat-Create - ttIndexStat.IndexCreate
/* IndexDelete:*/ {&Sep} DICTDB._IndexStat._IndexStat-Delete - ttIndexStat.IndexDelete
&IF DEFINED(Version_GE_102B)
&THEN
/* IndexOsRead:*/ {&Sep} DICTDB._IndexStat._IndexStat-OsRead - ttIndexStat.IndexOsRead
&ENDIF
/* IndexRead:  */ {&Sep} PerSec(DICTDB._IndexStat._IndexStat-Read   - ttIndexStat.IndexRead  )
/* IndexCreate:*/ {&Sep} PerSec(DICTDB._IndexStat._IndexStat-Create - ttIndexStat.IndexCreate)
/* IndexDelete:*/ {&Sep} PerSec(DICTDB._IndexStat._IndexStat-Delete - ttIndexStat.IndexDelete)
&IF DEFINED(Version_GE_102B)
&THEN
/* IndexOsRead:*/ {&Sep} PerSec(DICTDB._IndexStat._IndexStat-OsRead - ttIndexStat.IndexOsRead)
&ENDIF
    SKIP. /* PUT */

    ASSIGN
/* Accumulate index stats per area: */
      tt_Area.IndexCount  = tt_Area.IndexCount + 1
      tt_Area.IndexRead   = tt_Area.IndexRead   + DICTDB._IndexStat._IndexStat-Read
      tt_Area.IndexCreate = tt_Area.IndexCreate + DICTDB._IndexStat._IndexStat-Create
      tt_Area.IndexDelete = tt_Area.IndexDelete + DICTDB._IndexStat._IndexStat-Delete
/* Save the current snapshot: */
      ttIndexStat.DbHost      = vDbHost
      ttIndexStat.DbPath      = vDbPath
      ttIndexStat.IndexNumber = tt_Index.uIdx-Num
      ttIndexStat.TableName   = tt_File.uFile-Name
      ttIndexStat.IndexName   = tt_Index.uIndex-Name
      ttIndexStat.IndexRead   = DICTDB._IndexStat._IndexStat-Read
      ttIndexStat.IndexCreate = DICTDB._IndexStat._IndexStat-Create
      ttIndexStat.IndexDelete = DICTDB._IndexStat._IndexStat-Delete
      &IF DEFINED(Version_GE_102B)
      &THEN
      ttIndexStat.IndexOsRead = DICTDB._IndexStat._IndexStat-OsRead
      &ENDIF
    . /* ASSIGN */

  END. /* EACH _IndexStat etc */

/* The list of indexes outside IndexRangeSize: */
  FOR EACH tt_Index NO-LOCK
     WHERE tt_Index.uIdx-Num LT vMinIndexStatId
        OR tt_Index.uIdx-Num GT vMaxIndexStatId,

     FIRST tt_StorageObject NO-LOCK
     WHERE tt_StorageObject.uObject-Type   EQ 2
       AND tt_StorageObject.uObject-Number EQ tt_Index.uIdx-Num,

     FIRST tt_File NO-LOCK
     WHERE tt_File.uFileRecid EQ tt_Index.uFile-Recid,

     FIRST tt_Area NO-LOCK
     WHERE tt_Area.uArea-Number EQ tt_StorageObject.uArea-Number:

    PUT STREAM DumpFile UNFORMATTED
/* DbHost:     */        vDbHost
/* DbName:     */ {&Sep} LDBNAME("DICTDB":U)
/* AreaName:   */ {&Sep} tt_Area.uArea-Name
/* TableName:  */ {&Sep} tt_File.uFile-Name
/* IndexName:  */ {&Sep} tt_Index.uIndex-Name
/* IndexAttr:  */ {&Sep} tt_Index.IndexAttr
/* RootBlock:  */ {&Sep} tt_StorageObject.uObject-root
/* IndexRead:  */ {&Sep} "?":U
/* IndexCreate:*/ {&Sep} "?":U
/* IndexDelete:*/ {&Sep} "?":U
&IF DEFINED(Version_GE_102B)
&THEN
/* TableOsRead:*/ {&Sep} "?":U
&ENDIF
/* IndexRead:  */ {&Sep} "?":U
/* IndexCreate:*/ {&Sep} "?":U
/* IndexDelete:*/ {&Sep} "?":U
&IF DEFINED(Version_GE_102B)
&THEN
/* TableOsRead:*/ {&Sep} "?":U
&ENDIF
    SKIP. /* PUT */

    ASSIGN tt_Area.IndexCount = tt_Area.IndexCount + 1.

  END. /* EACH DICTDB._IndexStat */


/* Flush data on disk: */
  PUT STREAM DumpFile CONTROL NULL(0).

  OUTPUT STREAM DumpFile CLOSE.
END PROCEDURE. /* IndexStat */

/* -------------------------------------------------------------------- */
PROCEDURE ResrcStat.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  OUTPUT STREAM DumpFile TO VALUE( vDumpPrefix + "ResrcStat.txt":U ) APPEND.

/* Create a header: */
  IF SEEK(DumpFile) EQ 0 THEN
  PUT STREAM DumpFile UNFORMATTED
           "DbHost"
    {&Sep} "DbName"
    {&Sep} "ResrcType"
    {&Sep} "ResrcName"
    {&Sep} "ResrcLock"
    {&Sep} "ResrcWait"
    {&Sep} "ResrcLock"
    {&Sep} "ResrcWait"
  SKIP. /* PUT */

/* Resources: */
  FOR EACH DICTDB._Resrc NO-LOCK
  TRANSACTION:

    FIND FIRST ttResrcStat EXCLUSIVE
         WHERE ttResrcStat.DbHost    EQ vDbHost
           AND ttResrcStat.DbPath    EQ vDbPath
           AND ttResrcStat.ResrcType EQ "Resource":U
           AND ttResrcStat.ResrcName EQ DICTDB._Resrc._Resrc-Name
    NO-ERROR.

    IF NOT AVAILABLE ttResrcStat THEN
    CREATE ttResrcStat.

    IF vIgnorePrevStat THEN
    ASSIGN
      ttResrcStat.ResrcLock = 0
      ttResrcStat.ResrcWait = 0
    . /* ASSIGN */

    PUT STREAM DumpFile UNFORMATTED
/* DbHost:   */        vDbHost
/* DbName:   */ {&Sep} LDBNAME("DICTDB":U)
/* ResrcType:*/ {&Sep} "Resource":U
/* ResrcName:*/ {&Sep} DICTDB._Resrc._Resrc-Name
/* ResrcLock:*/ {&Sep} DICTDB._Resrc._Resrc-lock - ttResrcStat.ResrcLock
/* ResrcWait:*/ {&Sep} DICTDB._Resrc._Resrc-wait - ttResrcStat.ResrcWait
/* ResrcLock:*/ {&Sep} PerSec(DICTDB._Resrc._Resrc-lock - ttResrcStat.ResrcLock)
/* ResrcWait:*/ {&Sep} PerSec(DICTDB._Resrc._Resrc-wait - ttResrcStat.ResrcWait)
    SKIP. /* PUT */

/* Save the current snapshot: */
    ASSIGN
      ttResrcStat.DbHost    = vDbHost
      ttResrcStat.DbPath    = vDbPath
      ttResrcStat.ResrcType = "Resource":U
      ttResrcStat.ResrcName = DICTDB._Resrc._Resrc-Name
      ttResrcStat.ResrcLock = DICTDB._Resrc._Resrc-lock
      ttResrcStat.ResrcWait = DICTDB._Resrc._Resrc-wait
    . /* ASSIGN */

  END. /* FOR EACH _Resrc */

/* Latches: */
  FOR EACH DICTDB._Latch NO-LOCK
     WHERE DICTDB._Latch._Latch-id NE 1
  TRANSACTION:

    FIND FIRST ttResrcStat EXCLUSIVE
         WHERE ttResrcStat.DbHost    EQ vDbHost
           AND ttResrcStat.DbPath    EQ vDbPath
           AND ttResrcStat.ResrcType EQ "Latch":U
           AND ttResrcStat.ResrcName EQ DICTDB._Latch._Latch-Name
     NO-ERROR.

    IF NOT AVAILABLE ttResrcStat THEN
    CREATE ttResrcStat.

    IF vIgnorePrevStat THEN
    ASSIGN
      ttResrcStat.ResrcLock = 0
      ttResrcStat.ResrcWait = 0
    . /* ASSIGN */

    PUT STREAM DumpFile UNFORMATTED
/* DbHost:   */        vDbHost
/* DbName:   */ {&Sep} LDBNAME("DICTDB":U)
/* ResrcType:*/ {&Sep} "Latch":U
/* ResrcName:*/ {&Sep} DICTDB._Latch._Latch-Name
/* ResrcLock:*/ {&Sep} DICTDB._Latch._Latch-lock - ttResrcStat.ResrcLock
/* ResrcWait:*/ {&Sep} DICTDB._Latch._Latch-wait - ttResrcStat.ResrcWait
/* ResrcLock:*/ {&Sep} PerSec(DICTDB._Latch._Latch-lock - ttResrcStat.ResrcLock)
/* ResrcWait:*/ {&Sep} PerSec(DICTDB._Latch._Latch-wait - ttResrcStat.ResrcWait)
    SKIP. /* PUT */

/* Save the current snapshot: */
    ASSIGN
      ttResrcStat.DbHost    = vDbHost
      ttResrcStat.DbPath    = vDbPath
      ttResrcStat.ResrcType = "Latch":U
      ttResrcStat.ResrcName = DICTDB._Latch._Latch-Name
      ttResrcStat.ResrcLock = DICTDB._Latch._Latch-lock
      ttResrcStat.ResrcWait = DICTDB._Latch._Latch-wait
    . /* ASSIGN */

  END. /* FOR EACH _Latch */

/* TXE Locks: */
  FOR FIRST DICTDB._TxeLock NO-LOCK:
    DO i = 1 TO 9
    TRANSACTION:

      FIND FIRST ttResrcStat EXCLUSIVE
           WHERE ttResrcStat.DbHost    EQ vDbHost
             AND ttResrcStat.DbPath    EQ vDbPath
             AND ttResrcStat.ResrcType EQ "TxeLock":U
             AND ttResrcStat.ResrcName EQ DICTDB._TxeLock._Txe-Type[i]
       NO-ERROR.

      IF NOT AVAILABLE ttResrcStat THEN
      CREATE ttResrcStat.

      IF vIgnorePrevStat THEN
      ASSIGN
        ttResrcStat.ResrcLock = 0
        ttResrcStat.ResrcWait = 0
      . /* ASSIGN */

      PUT STREAM DumpFile UNFORMATTED
/* DbHost:   */        vDbHost
/* DbName:   */ {&Sep} LDBNAME("DICTDB":U)
/* ResrcType:*/ {&Sep} "TxeLock":U
/* ResrcName:*/ {&Sep} DICTDB._TxeLock._Txe-Type[i]
/* ResrcLock:*/ {&Sep} DICTDB._TxeLock._Txe-Locks[i] - ttResrcStat.ResrcLock
/* ResrcWait:*/ {&Sep} DICTDB._TxeLock._Txe-Waits[i] - ttResrcStat.ResrcWait
/* ResrcLock:*/ {&Sep} PerSec(DICTDB._TxeLock._Txe-Locks[i] - ttResrcStat.ResrcLock)
/* ResrcWait:*/ {&Sep} PerSec(DICTDB._TxeLock._Txe-Waits[i] - ttResrcStat.ResrcWait)
      SKIP. /* PUT */

/* Save the current snapshot: */
      ASSIGN
        ttResrcStat.DbHost    = vDbHost
        ttResrcStat.DbPath    = vDbPath
        ttResrcStat.ResrcType = "TxeLock":U
        ttResrcStat.ResrcName = DICTDB._TxeLock._Txe-Type[i]
        ttResrcStat.ResrcLock = DICTDB._TxeLock._Txe-Locks[i]
        ttResrcStat.ResrcWait = DICTDB._TxeLock._Txe-Waits[i]
      . /* ASSIGN */

    END. /* DO i = 1 TO 9 */
  END. /* FOR FIRST _TxeLock */

/* Flush data on disk: */
  PUT STREAM DumpFile CONTROL NULL(0).

  OUTPUT STREAM DumpFile CLOSE.
END PROCEDURE. /* ResrcStat */

/* -------------------------------------------------------------------- */
PROCEDURE CheckPrevStat.

  DEFINE OUTPUT PARAMETER opIgnorePrevStat AS LOGICAL NO-UNDO.

  ASSIGN opIgnorePrevStat = TRUE.

  FIND FIRST ttDbStat
       WHERE ttDbStat.DbHost EQ vDbHost
         AND ttDbStat.DbPath EQ vDbPath
  NO-ERROR.

  FOR FIRST DICTDB._ActSummary NO-LOCK:

    IF AVAILABLE ttDbStat THEN
    ASSIGN
      vCurrInterval = (TODAY - Stamp2Date(ttDbStat.SnapshotTimeStamp)) * 86400
                    + (TIME  - Stamp2Time(ttDbStat.SnapshotTimeStamp))
      opIgnorePrevStat = vCurrInterval GT DICTDB._ActSummary._Summary-UpTime
    . /* ASSIGN */
    ELSE
    CREATE ttDbStat.

    IF opIgnorePrevStat THEN
    ASSIGN
      vCurrInterval   = DICTDB._ActSummary._Summary-UpTime

      ttDbStat.DbHost = vDbHost
      ttDbStat.DbPath = vDbPath
      ttDbStat.SnapshotTimeStamp = ""

      ttDbStat.DbAccesses  = 0
      ttDbStat.RecRead     = 0
      ttDbStat.RecUpdate   = 0
      ttDbStat.RecCreate   = 0
      ttDbStat.RecDelete   = 0

      ttDbStat.BytesRead   = 0
      ttDbStat.BytesUpdate = 0
      ttDbStat.BytesCreate = 0
      ttDbStat.BytesDelete = 0

      ttDbStat.IdxRead     = 0
      ttDbStat.IdxCreate   = 0
      ttDbStat.IdxDelete   = 0

      ttDbStat.DbReads     = 0
      ttDbStat.DbWrites    = 0
      ttDbStat.DatDbReads  = 0
      ttDbStat.DatDbWrites = 0
      ttDbStat.IdxDbReads  = 0
      ttDbStat.IdxDbWrites = 0

      ttDbStat.BiWrites    = 0
      ttDbStat.BiBytesWrtn = 0
      ttDbStat.BiNotesWrtn = 0
      ttDbStat.TransComm   = 0

      ttDbStat.LatchLock   = 0
      ttDbStat.LatchWait   = 0
      ttDbStat.ResrcLock   = 0
      ttDbStat.ResrcWait   = 0
      ttDbStat.SemWaits    = 0
      ttDbStat.TxeLock     = 0
      ttDbStat.TxeWait     = 0
    . /* ASSIGN */
  END. /* FOR FIRST _ActSummary */
END PROCEDURE. /* CheckPrevStat */

/* ***************************  Main Block  *************************** */

ASSIGN
  vDumpPrefix = "DbStatDump." + ipTimeStamp + ".":U
  vDbHost = ipLocalHost
  vConnect = "Local"
. /* ASSIGN */

DO i = 1 TO NUM-ENTRIES( DBPARAM("DICTDB":U) ):
  ASSIGN vParam = ENTRY(i, DBPARAM("DICTDB":U) ).

  IF NOT vParam BEGINS "-H ":U THEN
  NEXT.

  ASSIGN vDbHost = SUBSTRING(vParam, 4)
         vConnect = "Remote".
  LEAVE.
END.

FOR FIRST DICTDB._FileList NO-LOCK
    WHERE DICTDB._FileList._FileList-Name MATCHES "*.db":U:

  ASSIGN vDbPath = SUBSTRING(DICTDB._FileList._FileList-Name, 1,
                      LENGTH(DICTDB._FileList._FileList-Name) - 3) /*Cut .db*/
  . /* ASSIGN */
END. /* FOR FIRST _FileList */

FOR LAST DICTDB._TableStat NO-LOCK:
  ASSIGN
    vTableRangeSize = RECID(DICTDB._TableStat)
    vMaxTableStatId = DICTDB._TableStat._TableStat-Id
    vMinTableStatId = vMaxTableStatId - vTableRangeSize + 1
  . /* ASSIGN */
END.

FOR LAST DICTDB._IndexStat NO-LOCK:
  ASSIGN
    vIndexRangeSize = RECID(DICTDB._IndexStat)
    vMaxIndexStatId = DICTDB._IndexStat._IndexStat-Id
    vMinIndexStatId = vMaxIndexStatId - vIndexRangeSize + 1
  . /* ASSIGN */
END.

DEFINE VARIABLE vETime AS INTEGER NO-UNDO.
ETIME(TRUE).

DEFINE VARIABLE vErrorsLogFile AS CHARACTER NO-UNDO.
ASSIGN vErrorsLogFile = "DbStatDump.":U + ipTimeStamp + ".Errors.log":U.

OUTPUT TO VALUE(vErrorsLogFile) APPEND KEEP-MESSAGES.

RUN ReadSchema(OUTPUT vHighestTableId,
               OUTPUT vHighestIndexId,
               OUTPUT vDbTableCount,
               OUTPUT vDbIndexCount).

RUN CheckPrevStat(OUTPUT vIgnorePrevStat).

RUN IndexStat.  /* set tt_Area.IndexCount
                       tt_Area.IndexRead
                       tt_Area.IndexCreate
                       tt_Area.IndexDelete */

RUN TableStat.  /* set tt_Area.TableCount
                       tt_Area.TableRead
                       tt_Area.TableUpdate
                       tt_Area.TableCreate
                       tt_Area.TableDelete */

RUN AreaStat.   /* set vDbSize */
RUN ResrcStat.
RUN DbStat.

OUTPUT CLOSE.

FILE-INFO:FILE-NAME = vErrorsLogFile.
IF  FILE-INFO:FULL-PATHNAME NE ?
AND FILE-INFO:FILE-SIZE     EQ 0 THEN
OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).
