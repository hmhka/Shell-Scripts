/* ----------------------------------------------------------------------------
    File        : DbStatDump.p
    Purpose     : Dump the _TableStat/_IndexStat statistics and 
                  the sequence current values for all connected databases.

    Syntax      : Connect all databases you need to process and run the program

    Description : The output file can be opened in Excel or
                  processed by DbStatDiff.p to get the statistics per intervals.

    Author(s)   : George Potemkin
    Created     : May 12, 2015
    Modified    : May 30, 2015
    Version     : 2.0

    The most recent version can be downloaded from here:
    ftp://ftp.progress-tech.ru/pub/Users/george/Programs/DbStatDump.p

    Fixes       :
May 27, 2015: Correction of _IndexStat-split/_IndexStat-blockdelete values
              to work around the Progress bug.
                 
---------------------------------------------------------------------------- */

/* Pseuso area for per databases statistics: */
&SCOPED-DEFINE TotalDbStat "Total per database"

/* TableStat statistics per database (TableId = 0): */
&SCOPED-DEFINE TotalTableStat  "ALL tables" /*based on _ActRecord/_ActIOType */

/* IndexStat statistics per database and per table (IndexId = 0): */
&SCOPED-DEFINE TotalIndexStat  "Total per indexes" /*based on _ActIndex/_ActIOType */

/* Byte statistics per database (TableId = ?, IndexId = ?): */
&SCOPED-DEFINE RecByteStat  "Bytes per database"
&SCOPED-DEFINE IdxByteStat  "Not applicable to indexes"

/* Extra rows in the sequence statistics (SeqId = ?): */
&SCOPED-DEFINE LastTask   "Last Transaction"
&SCOPED-DEFINE SeqLocks   "SEQ Latch Locks"

/*
/* The difference between _ActRecord and sum of _TableStat 
           and/or between _ActIndex and sum of _IndexStat:
Some utilities (idxfix/dbanalys) don't update _TableStat/_IndexStat counters.
Non-zero value will be reported as index stats: */
&SCOPED-DEFINE HiddenRecReads "Hidden Rec Reads" /*TableId = 0, IndexId = 1 */
&SCOPED-DEFINE HiddenIdxReads "Hidden Idx Reads" /*TableId = 0, IndexId = 2 */

/* The difference between _TableStat and sum of _IndexStat per table.
   Reported as index statistics (IndexId = ?): */
&SCOPED-DEFINE RowidReads "Reads by Rowid"
*/

/* ******************************  Temp-tables  **************************** */

DEFINE TEMP-TABLE ttDb NO-UNDO
  FIELD DbId      AS INTEGER
  FIELD HostName  AS CHARACTER
  FIELD PhysName  AS CHARACTER
  FIELD FullPath  AS CHARACTER
  FIELD BuildTime AS CHARACTER
  FIELD StartTime AS CHARACTER
  FIELD MyUserNumber   LIKE _MyConnection._MyConn-UserId
  FIELD IndexStatShift LIKE _IndexStat._IndexStat-id    
  FIELD IndexRangeSize LIKE _IndexStat._IndexStat-id    
  
  INDEX DbId IS UNIQUE
        DbId
. /* TEMP-TABLE ttDb */

DEFINE TEMP-TABLE ttArea NO-UNDO
  FIELD DbId            AS INTEGER
  FIELD AreaNumber      LIKE _Area._Area-Number
  FIELD AreaName        LIKE _Area._Area-Name
  FIELD AreaType        LIKE _Area._Area-type
  FIELD AreaBlockSize   LIKE _Area._Area-blocksize
  FIELD AreaRecPerBlock LIKE _Area._Area-recbits
  FIELD AreaClusterSize LIKE _Area._Area-clustersize
  FIELD AreaAttr        LIKE _Area._Area-attrib
  FIELD AreaHWM         LIKE _AreaStatus._AreaStatus-Hiwater
  FIELD TblCount        AS INTEGER
  FIELD IdxCount        AS INTEGER
  FIELD LobCount        AS INTEGER
  FIELD AreaInfo        AS CHARACTER
  INDEX AreaNumber IS UNIQUE
        DbId
        AreaNumber
. /* TEMP-TABLE ttArea */

DEFINE TEMP-TABLE ttTable NO-UNDO
  FIELD DbId        AS INTEGER
  FIELD TableId     LIKE _File._File-Number
  FIELD TableName   LIKE _File._File-Name
  FIELD AreaNumber  LIKE _Area._Area-Number
  FIELD NumFld      LIKE _File._numfld
  FIELD NumKey      LIKE _File._numkey
  FIELD NumKeyFld   LIKE _File._numkfld
  FIELD LastChange  LIKE _File._Last-Change
  FIELD PrimeIndex  LIKE _File._Prime-Index
  FIELD FileRecid   LIKE _Index._File-recid
  FIELD Template    LIKE _File._Template
  FIELD ObjectBlock LIKE _StorageObject._Object-block
  FIELD TableAttr   LIKE _StorageObject._Object-attrib
  FIELD CreateLimit LIKE _StorageObject._Create-Limit
  FIELD TossLimit   LIKE _StorageObject._Toss-Limit
  FIELD RecReads    LIKE _TableStat._TableStat-read  
  FIELD RecUpdates  LIKE _TableStat._TableStat-update
  FIELD RecCreates  LIKE _TableStat._TableStat-create
  FIELD RecDeletes  LIKE _TableStat._TableStat-delete
  FIELD BlockReads  LIKE _TableStat._TableStat-OsRead
  FIELD StatTime    AS DATETIME
  INDEX TableId     IS UNIQUE
        DbId
        TableId
  INDEX FileRecid   IS UNIQUE
        DbId
        FileRecid
. /* DEFINE TEMP-TABLE ttTable */

DEFINE TEMP-TABLE ttIndex NO-UNDO
  FIELD DbId        AS INTEGER
  FIELD TableId     LIKE _File._File-Number
  FIELD IndexId     LIKE _Index._idx-num
  FIELD IndexName   LIKE _Index._Index-Name
  FIELD AreaNumber  LIKE _Area._Area-Number
  FIELD NumComp     LIKE _Index._num-comp
  FIELD IndexActive LIKE _Index._Active
  FIELD IndexUnique LIKE _Index._Unique
  FIELD WordIdx     LIKE _Index._Wordidx
  FIELD IndexAttr   LIKE _StorageObject._Object-attrib
  FIELD ObjectBlock LIKE _StorageObject._Object-block
  FIELD RootBlock   LIKE _StorageObject._Object-root
  FIELD IndexInfo   AS CHARACTER
  FIELD LevelCount  AS INTEGER
  FIELD IdxReads    LIKE _IndexStat._IndexStat-read
  FIELD IdxCreates  LIKE _IndexStat._IndexStat-create
  FIELD IdxDeletes  LIKE _IndexStat._IndexStat-delete
  FIELD BlockReads  LIKE _IndexStat._IndexStat-OsRead
  FIELD BlockSplits LIKE _IndexStat._IndexStat-split 
  FIELD BlockFree   LIKE _IndexStat._IndexStat-blockdelete
  FIELD StatTime    AS DATETIME
  INDEX TableId
        DbId
        TableId
  INDEX IndexId IS UNIQUE
        DbId
        IndexId
. /* DEFINE TEMP-TABLE ttIndex */

DEFINE TEMP-TABLE ttSequence NO-UNDO
  FIELD DbId     AS INTEGER
  FIELD SeqId    LIKE _Sequence._Seq-Num  
  FIELD SeqName  LIKE _Sequence._Seq-Name
  FIELD SeqInit  LIKE _Sequence._Seq-Init
  FIELD SeqIncr  LIKE _Sequence._Seq-Incr
  FIELD SeqValue AS INT64
  FIELD StatTime AS DATETIME
  INDEX SeqId    IS UNIQUE
        DbId
        SeqId
. /* TEMP-TABLE ttSequence */

/* *******************************  Functions  ***************************** */

FUNCTION XOR RETURNS CHARACTER (ipInt1 AS INTEGER, ipInt2 AS INTEGER):

  DEFINE VARIABLE vBitList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i        AS INTEGER   NO-UNDO.

  ASSIGN vBitList = "":U.
  DO i = 32 TO 1 BY -1:
    ASSIGN vBitList = vBitList + IF GET-BITS(ipInt1, i, 1) EQ 1
                                 OR GET-BITS(ipInt2, i, 1) EQ 1
                                 THEN "1":U
                                 ELSE "0":U
    . /* ASSIGN */
  END.
  RETURN LEFT-TRIM(vBitList, "0":U).

END FUNCTION. /* XOR */

/* ------------------------------------------------------------------------- */

FUNCTION Integer2DateTime RETURNS DATETIME (INPUT ipLastChange AS INTEGER).

/* Input value must be the number of seconds since beginning of Unix epoch */

  RETURN ADD-INTERVAL(DATETIME(1, 1, 1970, 0, 0, 0, 0),
                      ipLastChange + TIMEZONE * 60,
                      "seconds":U).

END FUNCTION. /* Integer2DateTime */

/* ------------------------------------------------------------------------- */

FUNCTION String2DateTime RETURNS DATETIME (INPUT ipString AS CHARACTER).

/* Input string must have a format like: Tue Jul 27 12:11:45 2004 */

  DEFINE VARIABLE vDateTime  AS DATETIME  NO-UNDO INITIAL ?.
  DEFINE VARIABLE vMonthList AS CHARACTER NO-UNDO
    INITIAL "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec".
  DEFINE VARIABLE vTime AS CHARACTER NO-UNDO.

  ASSIGN ipString = TRIM(REPLACE(ipString, "  ":U, " ":U))
         vTime = ENTRY(4, ipString, " ":U)
         vDateTime = DATETIME(
          /* Month */ LOOKUP (ENTRY(2, ipString, " ":U), vMonthList),
          /* Day   */ INTEGER(ENTRY(3, ipString, " ":U)),
          /* Year  */ INTEGER(ENTRY(5, ipString, " ":U)),
          /* Hours */ INTEGER(ENTRY(1, vTime, ":":U)),
          /* Min   */ INTEGER(ENTRY(2, vTime, ":":U)),
          /* Sec   */ INTEGER(ENTRY(3, vTime, ":":U)))
  NO-ERROR.

  RETURN vDateTime.

END FUNCTION. /* String2DateTime */

/* ------------------------------------------------------------------------- */

FUNCTION DateTime2String RETURNS CHARACTER (INPUT ipDateTime AS DATETIME).

/* Retuns the datetime in format: "YYYY.MM.DD_HH.MM.SS" */

  DEFINE VARIABLE vDate AS DATE    NO-UNDO.
  DEFINE VARIABLE vTime AS INTEGER NO-UNDO.

  ASSIGN vDate = DATE(ipDateTime)
         vTime = INTERVAL(ipDateTime, DATETIME(vDate), "seconds":U)
  NO-ERROR.

  RETURN      STRING( YEAR(vDate), "9999":U)
    + ".":U + STRING(MONTH(vDate),   "99":U)
    + ".":U + STRING(  DAY(vDate),   "99":U)
    + "_":U + REPLACE(STRING(vTime, "HH:MM:SS":U), ":":U, ".":U)
  . /* RETURN */

END FUNCTION. /* DateTime2String */

/* ------------------------------------------------------------------------- */

FUNCTION DateTime2Excel RETURNS CHARACTER (INPUT ipDateTime AS DATETIME).

/* Retuns the datetime in format: DD:MM:YYYYY HH:MM:SS */

  DEFINE VARIABLE vDate AS DATE    NO-UNDO.
  DEFINE VARIABLE vTime AS INTEGER NO-UNDO.

  ASSIGN vDate = DATE(ipDateTime)
         vTime = INTERVAL(ipDateTime, DATETIME(vDate), "seconds":U)
  NO-ERROR.

  RETURN      /* Day   */ STRING( YEAR(vDate), "9999":U)
    + "/":U + /* Month */ STRING(MONTH(vDate),   "99":U)
    + "/":U + /* Year  */ STRING(  DAY(vDate),   "99":U)
    + " ":U + /* Time  */ STRING(vTime, "HH:MM:SS":U)
  . /* RETURN */

END FUNCTION. /* DateTime2Excel */

/* ------------------------------------------------------------------------- */

FUNCTION LkHostName RETURNS CHARACTER (ipLkFile AS CHARACTER):

/* Returns the host name based on lk file of the locally connected database.*/

  DEFINE VARIABLE vHostName AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vLkSize   AS INTEGER   NO-UNDO INITIAL 38.
  DEFINE VARIABLE vRawData  AS RAW       NO-UNDO.

/* Try to open .lk file for reads: */
  DO ON ERROR UNDO, RETRY:
    IF RETRY THEN
    RETURN ERROR SUBSTITUTE("Unable to read &1 (Progress error &2)",
                  /* &1 */  ipLkFile,
                  /* &1 */  STRING(_Msg(1))).
    INPUT FROM VALUE(ipLkFile) BINARY.
  END.
/* Read .lk file: */
  LENGTH(vRawData) = vLkSize.
  IMPORT UNFORMATTED vRawData.
  INPUT CLOSE.
  ASSIGN vHostName = GET-STRING(vRawData, 9)
         LENGTH(vRawData) = 0.

  RETURN vHostName.

END FUNCTION. /* LkHostName */

/* ------------------------------------------------------------------------- */

FUNCTION LocalHostName RETURNS CHARACTER:

/* Returns the host name of the current session is running on. */

  DEFINE VARIABLE vHostName AS CHARACTER NO-UNDO INITIAL 'unknown'.
  DEFINE VARIABLE vTcpName  AS CHARACTER NO-UNDO INITIAL ''.
  DEFINE VARIABLE vLength   AS INTEGER   NO-UNDO INITIAL 100.
  DEFINE VARIABLE vReturn   AS INTEGER   NO-UNDO INITIAL 0.

  IF OPSYS EQ "WIN32" THEN
  DO: /* Call Win32 routine to get host name */
    RUN gethostname(OUTPUT vTcpName,
                    INPUT  vLength,
                    OUTPUT vReturn).
    IF vReturn EQ 0 THEN
    ASSIGN vHostName = ENTRY(1, vTcpName, CHR(0)).
  END.
  ELSE DO:
    /* get UNIX host name */
    INPUT THROUGH uname -n.
    IMPORT vHostName.
    INPUT CLOSE.
  END.

  RETURN vHostName.

END FUNCTION. /* LocalHostName */

/* ------------------------------------------------------------------------- */

FUNCTION DbParamHostName RETURNS CHARACTER (ipLDBName AS CHARACTER).

/* Returns the host name of the database connected remotely. */

  DEFINE VARIABLE vParam AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i      AS INTEGER   NO-UNDO.

  DO i = 1 TO NUM-ENTRIES(DBPARAM(ipLDBName)):
    ASSIGN vParam = ENTRY(i, DBPARAM(ipLDBName)).

    IF vParam BEGINS "-H ":U THEN
    RETURN SUBSTRING(vParam, 4).
  END.

  RETURN ?.

END FUNCTION. /* DbParamHostName */

/* ------------------------------------------------------------------------- */

PROCEDURE gethostname EXTERNAL "wsock32.dll" :
   DEFINE OUTPUT PARAMETER p-Hostname AS CHARACTER.
   DEFINE INPUT  PARAMETER p-Length   AS LONG.
   DEFINE RETURN PARAMETER p-Return   AS LONG.
END PROCEDURE. /* gethostname */

/* ------------------------------------------------------------------------- */

FUNCTION MyUserNumber RETURNS INTEGER:
  
/* Returns the user number of current session connected to DICTDB database. */

  DEFINE VARIABLE vUserId LIKE _MyConnection._MyConn-UserId NO-UNDO.
  DEFINE VARIABLE hQuery  AS HANDLE NO-UNDO. /*Query handle*/
  DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO. /*Buffer handle*/

  ASSIGN vUserId = ?.

  CREATE BUFFER hBuffer FOR TABLE "DICTDB._MyConnection":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH DICTDB._MyConnection NO-LOCK":U) NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN vUserId.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  IF NOT hQuery:QUERY-OFF-END THEN
  ASSIGN vUserId = hBuffer:BUFFER-FIELD("_MyConn-UserId":U):BUFFER-VALUE().
  hQuery:QUERY-CLOSE().

  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.

  RETURN vUserId.
END FUNCTION. /* MyUserNumber */

/* ------------------------------------------------------------------------- */

PROCEDURE GetIndexRange:
  
  DEFINE OUTPUT PARAMETER opIndexStatShift LIKE _IndexStat._IndexStat-id NO-UNDO.
  DEFINE OUTPUT PARAMETER opIndexRangeSize LIKE _IndexStat._IndexStat-id NO-UNDO.

  DEFINE VARIABLE hQuery  AS HANDLE NO-UNDO. /*Query handle*/
  DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO. /*Buffer handle*/

  ASSIGN opIndexStatShift = ?
         opIndexRangeSize = ?
  . /* ASSIGN */

  CREATE BUFFER hBuffer FOR TABLE "DICTDB._IndexStat":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE(
    "FOR EACH DICTDB._IndexStat NO-LOCK BY _IndexStat-id DESCENDING":U)
  NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

  IF NOT hQuery:QUERY-OFF-END THEN
  ASSIGN
    opIndexStatShift = hBuffer:BUFFER-FIELD("_IndexStat-id":U):BUFFER-VALUE()
    opIndexRangeSize = hBuffer:RECID
    opIndexStatShift = opIndexStatShift  - opIndexRangeSize
  . /* ASSIGN */

  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.

END PROCEDURE. /* GetIndexRange */

/* ------------------------------------------------------------------------- */

FUNCTION IndexLevels RETURN INTEGER (
         ipTableName      AS CHARACTER,
         ipIndexName      AS CHARACTER,
         ipMyUserNumber   AS INTEGER,
         ipIndexStatShift AS INTEGER,
         ipIndexRangeSize AS INTEGER).

/* Retuns the number of the levels in the index tree. */

  DEFINE VARIABLE vIndexLevels AS INTEGER                              NO-UNDO.
  DEFINE VARIABLE vProbeQuery  AS CHARACTER                            NO-UNDO.
  DEFINE VARIABLE vRecid       AS RECID                                NO-UNDO.
  DEFINE VARIABLE vRowid       AS ROWID                                NO-UNDO.
  DEFINE VARIABLE vDbAccess    LIKE _UserIO._UserIO-DbAccess           NO-UNDO.
  DEFINE VARIABLE vDbAccess1   LIKE _UserIO._UserIO-DbAccess           NO-UNDO.
  DEFINE VARIABLE vDbAccess2   LIKE _UserIO._UserIO-DbAccess           NO-UNDO.
  DEFINE VARIABLE vDbAccess3   LIKE _UserIO._UserIO-DbAccess           NO-UNDO.
  DEFINE VARIABLE vRecReads    LIKE _UserIO._UserIO-DbAccess           NO-UNDO.
  DEFINE VARIABLE vFragReads   LIKE _UserIO._UserIO-DbAccess           NO-UNDO.
  DEFINE VARIABLE vKeyReads    LIKE _UserIndexStat._UserIndexStat-Read NO-UNDO.
  DEFINE VARIABLE vIndexStatId LIKE _UserIndexStat._UserIndexStat-id   NO-UNDO.
  DEFINE VARIABLE vIndexNumber LIKE _Index._idx-num                    NO-UNDO.
  DEFINE VARIABLE vWordIndex   LIKE _Index._Wordidx                    NO-UNDO.
  DEFINE VARIABLE vUniqIndex   LIKE _Index._Unique                     NO-UNDO.
  DEFINE VARIABLE vFieldName   LIKE _Field._Field-Name                 NO-UNDO.

  DEFINE VARIABLE hQuery1  AS HANDLE NO-UNDO.
  DEFINE VARIABLE hQuery2  AS HANDLE NO-UNDO.
  DEFINE VARIABLE hQuery3  AS HANDLE NO-UNDO.
  DEFINE VARIABLE hBuffer1 AS HANDLE NO-UNDO.
  DEFINE VARIABLE hBuffer2 AS HANDLE NO-UNDO.
  DEFINE VARIABLE hBuffer3 AS HANDLE NO-UNDO.

  ASSIGN vIndexLevels = ?.

MainBlock:
  DO:

/* Check if _idx-num is inside of the _IndexStat range: */

    CREATE BUFFER hBuffer1 FOR TABLE "DICTDB._File":U.
    CREATE BUFFER hBuffer2 FOR TABLE "DICTDB._Index":U.
    CREATE QUERY hQuery1.
    hQuery1:SET-BUFFERS(hBuffer1, hBuffer2).
    hQuery1:QUERY-PREPARE(SUBSTITUTE(
"FOR EACH DICTDB._File  NO-LOCK WHERE _File-Name EQ '&1',":U +
"    EACH DICTDB._Index NO-LOCK OF DICTDB._File WHERE _Index-Name EQ '&2'":U,
    /* &1 */ ipTableName,
    /* &1 */ ipIndexName))
    NO-ERROR.

    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    LEAVE MainBlock.
  
    hQuery1:QUERY-OPEN().
    hQuery1:GET-FIRST().
    IF NOT hQuery1:QUERY-OFF-END THEN
    ASSIGN vIndexNumber = hBuffer2:BUFFER-FIELD("_idx-num":U):BUFFER-VALUE()
           vWordIndex   = hBuffer2:BUFFER-FIELD("_Wordidx":U):BUFFER-VALUE()
           vUniqIndex   = hBuffer2:BUFFER-FIELD("_Unique":U ):BUFFER-VALUE()
           vRecid       = hBuffer2:RECID
    . /* ASSIGN */  
    hQuery1:QUERY-CLOSE().
    
    DELETE OBJECT hQuery1.
    DELETE OBJECT hBuffer1.
    DELETE OBJECT hBuffer2.
  
/* ipIndexStatShift equates _StatBase._IndexBase - 1 */
    ASSIGN vIndexStatId = vIndexNumber - ipIndexStatShift.

/* vIndexStatId should belong to the range: [1, ipIndexRangeSize] */
    IF vIndexStatId LE 0
    OR vIndexStatId GT ipIndexRangeSize THEN
    LEAVE MainBlock.

/* Special case for the word indexes: */
    IF vWordIndex GT 0 THEN
    DO:
      CREATE BUFFER hBuffer1 FOR TABLE "DICTDB._Index-Field":U.
      CREATE BUFFER hBuffer2 FOR TABLE "DICTDB._Field":U.
      CREATE QUERY hQuery1.
      hQuery1:SET-BUFFERS(hBuffer1, hBuffer2).
      hQuery1:QUERY-PREPARE(SUBSTITUTE(
    "FOR EACH DICTDB._Index-Field NO-LOCK NO-PREFETCH       ":U +
    "          WHERE _Index-recid EQ &1 AND _Index-Seq EQ 1,":U +
    "    EACH DICTDB._Field NO-LOCK OF DICTDB._Index-Field  ":U,
        /* &1 */ STRING(vRecid)))
      NO-ERROR.
  
      IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
      LEAVE MainBlock.
  
      hQuery1:QUERY-OPEN().
      hQuery1:GET-FIRST().
      IF NOT hQuery1:QUERY-OFF-END THEN
      ASSIGN vFieldName = hBuffer2:BUFFER-FIELD("_Field-Name":U):BUFFER-VALUE().
      hQuery1:QUERY-CLOSE().
  
      DELETE OBJECT hQuery1.
      DELETE OBJECT hBuffer1.
      DELETE OBJECT hBuffer2.
  
      ASSIGN vProbeQuery = SUBSTITUTE(
             "FOR EACH &1 NO-LOCK NO-PREFETCH WHERE &2 CONTAINS 'AnyWordHere'":U,
                  /* &1 */ ipTableName,
                  /* &2 */ vFieldName)
      . /* ASSIGN */
    END.
    ELSE
    ASSIGN vProbeQuery = SUBSTITUTE(
             IF ipIndexName EQ "default":U
             THEN "FOR EACH &1 NO-LOCK NO-PREFETCH":U
             ELSE "FOR EACH &1 NO-LOCK NO-PREFETCH USE-INDEX &2":U,
                 /* &1 */ ipTableName,
                 /* &2 */ ipIndexName)
    . /* ASSIGN */

/* _UserIO: */
    CREATE BUFFER hBuffer1 FOR TABLE "DICTDB._UserIO":U.
    CREATE QUERY hQuery1.
    hQuery1:SET-BUFFERS(hBuffer1).
    hQuery1:QUERY-PREPARE(SUBSTITUTE(
      "FOR EACH DICTDB._UserIO NO-LOCK WHERE _UserIO-Id EQ &1":U,
                           /* &1 */ STRING(ipMyUserNumber + 1)))
    NO-ERROR.
  
    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    LEAVE MainBlock.

/* Shifting vIndexStatId for _UserIndexStat: */
    ASSIGN vIndexStatId = vIndexStatId + ipIndexRangeSize * ipMyUserNumber.

/* _UserIndexStat: */
    CREATE BUFFER hBuffer2 FOR TABLE "DICTDB._UserIndexStat":U.
    CREATE QUERY hQuery2.
    hQuery2:SET-BUFFERS(hBuffer2).
    hQuery2:QUERY-PREPARE(SUBSTITUTE(
      "FOR EACH DICTDB._UserIndexStat NO-LOCK WHERE _UserIndexStat-id EQ &1":U,
                           /* &1 */ STRING(vIndexStatId)))
    NO-ERROR.
  
    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    LEAVE MainBlock.
  
  /* ipTableName USE-INDEX ipIndexName or ipTableName CONTAINS "word": */
    CREATE BUFFER hBuffer3 FOR TABLE ipTableName.
    CREATE QUERY hQuery3.
    hQuery3:SET-BUFFERS(hBuffer3).
  
    hQuery3:QUERY-PREPARE(vProbeQuery) NO-ERROR.
  
    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    LEAVE MainBlock.
  
    hQuery1:QUERY-OPEN().
    hQuery2:QUERY-OPEN().
    hQuery3:QUERY-OPEN().
  
    hQuery1:GET-FIRST().
    hQuery2:GET-FIRST().
  
    ASSIGN 
      vDbAccess1 = hBuffer1:BUFFER-FIELD("_UserIO-DbAccess":U   ):BUFFER-VALUE()
      vKeyReads  = hBuffer2:BUFFER-FIELD("_UserIndexStat-Read":U):BUFFER-VALUE()
      vRecReads  = 0
      vFragReads = 0
    . /* ASSIGN */
/*
MESSAGE
"debug" SKIP
"vKeyReads:" vKeyReads SKIP
hQuery2:QUERY-OFF-END
VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/
    hQuery3:GET-FIRST().
  
    hBuffer1:BUFFER-RELEASE().
    hBuffer2:BUFFER-RELEASE().
  
    hQuery1:GET-FIRST().
    hQuery2:GET-FIRST().
  
    IF NOT hQuery3:QUERY-OFF-END THEN
    ASSIGN
      vRecReads  = 1
      vFragReads = ?
      vRowid     = hBuffer3:ROWID
    . /* ASSIGN */
    
    ASSIGN
      vDbAccess2 = hBuffer1:BUFFER-FIELD("_UserIO-DbAccess":U   ):BUFFER-VALUE()
      vKeyReads  = hBuffer2:BUFFER-FIELD("_UserIndexStat-Read":U):BUFFER-VALUE()
                 - vKeyReads
      vDbAccess  = vDbAccess2 - vDbAccess1
    . /* ASSIGN */


/* ipTableName by ROWID: */
    IF vRecReads GT 0 THEN
    DO:
      hQuery3:QUERY-CLOSE().
      hQuery3:QUERY-PREPARE(SUBSTITUTE(
        "FOR EACH &1 NO-LOCK WHERE ROWID(&1) EQ TO-ROWID('&2')":U,
                           /* &1 */ ipTableName,
                           /* &2 */ STRING(vRowid)))
      NO-ERROR.

      IF ERROR-STATUS:NUM-MESSAGES NE 0 THEN
      LEAVE MainBlock.

      hBuffer1:BUFFER-RELEASE().
      hQuery3:QUERY-OPEN().
      hQuery3:GET-FIRST().
      hQuery1:GET-FIRST().

      ASSIGN 
        vDbAccess3 = hBuffer1:BUFFER-FIELD("_UserIO-DbAccess":U):BUFFER-VALUE()
        vFragReads = vDbAccess3 - vDbAccess2
      . /* ASSIGN */
    END. /* IF vRecReads GT 0 */
  
    hQuery1:QUERY-CLOSE().
    hQuery2:QUERY-CLOSE().
    hQuery3:QUERY-CLOSE().

    ASSIGN vIndexLevels = vDbAccess - vKeyReads - vFragReads + 1.

  END.

/* Report the errors generated by QUERY-PREPARE(): */
  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  MESSAGE
    "IndexLevels() function:"           SKIP
    "Errors:" ERROR-STATUS:NUM-MESSAGES SKIP
    ERROR-STATUS:GET-MESSAGE(1)         SKIP
    ERROR-STATUS:GET-MESSAGE(2)         SKIP
    ERROR-STATUS:GET-MESSAGE(3)
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

  DELETE OBJECT hQuery1  NO-ERROR.
  DELETE OBJECT hQuery2  NO-ERROR.
  DELETE OBJECT hQuery3  NO-ERROR.
  DELETE OBJECT hBuffer1 NO-ERROR.
  DELETE OBJECT hBuffer2 NO-ERROR.
  DELETE OBJECT hBuffer3 NO-ERROR.

  RETURN IF vRecReads EQ 0 THEN 0 ELSE
         IF vUniqIndex AND vKeyReads GT 100 THEN ?
         ELSE vIndexLevels.

END FUNCTION. /* IndexLevels */

/* ------------------------------------------------------------------------- */

/* *****************************  Procedures  ****************************** */

PROCEDURE GetMetaSchema.

  DEFINE INPUT PARAMETER ipDbId AS INTEGER NO-UNDO.

  DEFINE VARIABLE vObjectType LIKE _StorageObject._Object-type      NO-UNDO.
  DEFINE VARIABLE vObjectId   LIKE _StorageObject._Object-number    NO-UNDO.
  DEFINE VARIABLE vIndexStatShift LIKE _IndexStat._IndexStat-id     NO-UNDO.
  DEFINE VARIABLE vIndexRangeSize LIKE _IndexStat._IndexStat-id     NO-UNDO.
  DEFINE VARIABLE vMyUserNumber   LIKE _MyConnection._MyConn-UserId NO-UNDO.
  DEFINE VARIABLE hQuery      AS HANDLE    NO-UNDO.
  DEFINE VARIABLE hBuffer     AS HANDLE    NO-UNDO.
  DEFINE VARIABLE vHostName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vPDbName    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i           AS INTEGER   NO-UNDO.

  ASSIGN vHostName = DbParamHostName("DICTDB":U)
         vPDbName  = PDBNAME("DICTDB":U).

  ASSIGN vHostName = LocalHostName() WHEN vHostName EQ ?
                                       OR vHostName EQ "localhost":U
         vPDbName = SUBSTRING(vPDbName, 1, LENGTH(vPDbName) - 3)
                         WHEN vPDbName MATCHES "*~~~.db":U.

  ASSIGN vHostName = LkHostName(vPDbName + ".lk":U) WHEN vHostName EQ ?
         vPDbName = SUBSTRING(vPDbName, 
                  MAX(R-INDEX(vPDbName, "/":U),
                      R-INDEX(vPDbName, "~\":U)) + 1)
  . /* ASSIGN */

/* Get _MstrBlk: ----------------------------------------------------------- */
  CREATE BUFFER hBuffer FOR TABLE "DICTDB._MstrBlk":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH DICTDB._MstrBlk NO-LOCK":U) NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

  IF hQuery:QUERY-OFF-END THEN
  RETURN ERROR.

  ASSIGN vMyUserNumber = MyUserNumber().

  RUN GetIndexRange(OUTPUT vIndexStatShift,
                    OUTPUT vIndexRangeSize).

  DO TRANSACTION:
    CREATE ttDb.
    ASSIGN ttDb.DbId      = ipDbId
           ttDb.HostName  = vHostName
           ttDb.PhysName  = vPDbName
           ttDb.FullPath  = ? /* _FileList._FileList-Name */
           ttDb.BuildTime = hBuffer:BUFFER-FIELD("_MstrBlk-Crdate":U ):BUFFER-VALUE()
           ttDb.BuildTime = STRING(String2DateTime(ttDb.BuildTime))
           ttDb.BuildTime = SUBSTRING(ttDb.BuildTime, 1, 19)
           ttDb.StartTime = hBuffer:BUFFER-FIELD("_MstrBlk-oprdate":U):BUFFER-VALUE()
           ttDb.StartTime = STRING(String2DateTime(ttDb.StartTime))
           ttDb.StartTime = SUBSTRING(ttDb.StartTime, 1, 19)
           ttDb.MyUserNumber   = vMyUserNumber
           ttDb.IndexStatShift = vIndexStatShift
           ttDb.IndexRangeSize = vIndexRangeSize
    . /* ASSIGN */

  END. /* DO TRANSACTION */

/* Pseudo-area for per database statistics: */
  DO TRANSACTION:
    CREATE ttArea.
    ASSIGN
      ttArea.DbId            = ipDbId
      ttArea.AreaNumber      = 0
      ttArea.AreaName        = {&TotalDbStat}
      ttArea.AreaInfo        = ttArea.AreaName
      ttArea.AreaType        = ?
      ttArea.AreaBlockSize   = ?
      ttArea.AreaRecPerBlock = ?
      ttArea.AreaClusterSize = ?
      ttArea.AreaAttr        = ?
      ttArea.AreaHWM         = hBuffer:BUFFER-FIELD("_MstrBlk-totblks":U):BUFFER-VALUE()
    . /* ASSIGN */
  END. /* REPEAT */

  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.

/* Get _Area: -------------------------------------------------------------- */
  CREATE BUFFER hBuffer FOR TABLE "DICTDB._Area":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE(
  "FOR EACH DICTDB._Area NO-LOCK WHERE _Area-Number GE 6 AND _Area-type EQ 6":U)
  NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

  REPEAT WHILE NOT hQuery:QUERY-OFF-END:

    DO TRANSACTION:
      CREATE ttArea.
      ASSIGN ttArea.DbId            = ipDbId
             ttArea.AreaNumber      = hBuffer:BUFFER-FIELD("_Area-Number":U   ):BUFFER-VALUE()
             ttArea.AreaName        = hBuffer:BUFFER-FIELD("_Area-Name":U     ):BUFFER-VALUE()
             ttArea.AreaType        = hBuffer:BUFFER-FIELD("_Area-type"       ):BUFFER-VALUE()
             ttArea.AreaBlockSize   = hBuffer:BUFFER-FIELD("_Area-blocksize"  ):BUFFER-VALUE()
             ttArea.AreaRecPerBlock = EXP(2,
                                      hBuffer:BUFFER-FIELD("_Area-recbits"    ):BUFFER-VALUE())
             ttArea.AreaClusterSize = hBuffer:BUFFER-FIELD("_Area-clustersize"):BUFFER-VALUE()
             ttArea.AreaAttr        = hBuffer:BUFFER-FIELD("_Area-attrib"     ):BUFFER-VALUE()
             ttArea.TblCount        = 0
             ttArea.IdxCount        = 0
             ttArea.LobCount        = 0
             ttArea.AreaInfo        = ttArea.AreaName
                     + ":":U + STRING(ttArea.AreaNumber)
                     + ",":U + STRING(ttArea.AreaRecPerBlock)
                     + ",":U + STRING(ttArea.AreaClusterSize)
      . /* ASSIGN */
    END. /* DO TRANSACTION */

    hQuery:GET-NEXT().
  END. /* REPEAT */
  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.

/* Get _AreaStatus: -------------------------------------------------------- */
  CREATE BUFFER hBuffer FOR TABLE "DICTDB._AreaStatus":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE(
    "FOR EACH DICTDB._AreaStatus NO-LOCK WHERE _AreaStatus-Areanum GE 6":U)
  NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  
    FOR FIRST ttArea EXCLUSIVE-LOCK
        WHERE ttArea.DbId       EQ ipDbId
          AND ttArea.AreaNumber EQ
              hBuffer:BUFFER-FIELD("_AreaStatus-Areanum":U):BUFFER-VALUE()
    TRANSACTION:
      ASSIGN ttArea.AreaHWM =
              hBuffer:BUFFER-FIELD("_AreaStatus-Hiwater":U):BUFFER-VALUE()
      . /* ASSIGN */
    END. /* FOR FIRST ttArea TRANSACTION */

    hQuery:GET-NEXT().
  END. /* REPEAT */
  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.

/* Get _Sequence:  --------------------------------------------------------- */
  CREATE BUFFER hBuffer FOR TABLE "DICTDB._Sequence":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH DICTDB._Sequence NO-LOCK":U)
  NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:

    DO TRANSACTION:
      CREATE ttSequence.
      ASSIGN ttSequence.DbId    = ipDbId
             ttSequence.SeqId   = hBuffer:BUFFER-FIELD("_Seq-Num":U  ):BUFFER-VALUE()
             ttSequence.SeqName = hBuffer:BUFFER-FIELD("_Seq-Name":U ):BUFFER-VALUE()
             ttSequence.SeqInit = hBuffer:BUFFER-FIELD("_Seq-Init":U ):BUFFER-VALUE()
             ttSequence.SeqIncr = hBuffer:BUFFER-FIELD("_Seq-Incr":U ):BUFFER-VALUE()
      . /* ASSIGN */
    END. /* DO TRANSACTION */

    hQuery:GET-NEXT().
  END. /* REPEAT TRANSACTION */
  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.

  DO TRANSACTION i = 1 TO 2:
    CREATE ttSequence.
    ASSIGN ttSequence.DbId    = ipDbId
           ttSequence.SeqId   = ?
           ttSequence.SeqInit = 0
           ttSequence.SeqIncr = 1
    . /* ASSIGN */
    CASE i:
      WHEN 1 THEN ASSIGN ttSequence.SeqName = {&LastTask}.
      WHEN 2 THEN ASSIGN ttSequence.SeqName = {&SeqLocks}.
    END CASE.
  END. /* DO TRANSACTION */

/* Get _File:  ------------------------------------------------------------- */
  CREATE BUFFER hBuffer FOR TABLE "DICTDB._File":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE(
"FOR EACH DICTDB._File NO-LOCK WHERE _File-Number GT 0 AND _File-Number LT 32768":U)
  NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:

    DO TRANSACTION:
      CREATE ttTable.
      ASSIGN ttTable.DbId        = ipDbId
             ttTable.TableId     = hBuffer:BUFFER-FIELD("_File-Number":U):BUFFER-VALUE()
             ttTable.TableName   = hBuffer:BUFFER-FIELD("_File-Name":U  ):BUFFER-VALUE()
             ttTable.Template    = hBuffer:BUFFER-FIELD("_Template":U   ):BUFFER-VALUE()
             ttTable.PrimeIndex  = hBuffer:BUFFER-FIELD("_Prime-Index":U):BUFFER-VALUE()
             ttTable.FileRecid   = hBuffer:RECID
             ttTable.LastChange  = hBuffer:BUFFER-FIELD("_Last-Change":U):BUFFER-VALUE()
             ttTable.NumKey      = hBuffer:BUFFER-FIELD("_numkey":U     ):BUFFER-VALUE()
             ttTable.NumFld      = hBuffer:BUFFER-FIELD("_numfld":U     ):BUFFER-VALUE()
                                 - 1 /* = the real count of table's fields */
             ttTable.NumKeyFld   = hBuffer:BUFFER-FIELD("_numkfld":U    ):BUFFER-VALUE()
             ttTable.RecReads    = ?
             ttTable.RecUpdates  = ?
             ttTable.RecCreates  = ?
             ttTable.RecDeletes  = ?
             ttTable.BlockReads  = ?
             ttTable.StatTime    = ?
      . /* ASSIGN */
    END. /* DO TRANSACTION */

    ACCUMULATE ttTable.LastChange  (MAX)
               ttTable.NumKey    (TOTAL)
               ttTable.NumFld    (TOTAL)
               ttTable.NumKeyFld (TOTAL)
    . /* ACCUMULATE */

    hQuery:GET-NEXT().
  END. /* REPEAT TRANSACTION */
  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.

/* Pseudo table for the per database statistics: */
  DO TRANSACTION i = 0 TO 1:
    CREATE ttTable.
    ASSIGN
      ttTable.DbId        = ipDbId
      ttTable.AreaNumber  = 0
      ttTable.Template    = ?
      ttTable.PrimeIndex  = ?
      ttTable.FileRecid   = ?
      ttTable.LastChange  = ACCUMULATE MAX   ttTable.LastChange
      ttTable.NumKey      = ACCUMULATE TOTAL ttTable.NumKey
      ttTable.NumFld      = ACCUMULATE TOTAL ttTable.NumFld
      ttTable.RecReads    = 0
      ttTable.RecUpdates  = 0
      ttTable.RecCreates  = 0
      ttTable.RecDeletes  = 0
      ttTable.BlockReads  = 0
      ttTable.StatTime    = ?
    . /* ASSIGN */
    CASE i:
      WHEN 0 THEN ASSIGN ttTable.TableId   = 0
                         ttTable.TableName = {&TotalTableStat}.
      WHEN 1 THEN ASSIGN ttTable.TableId   = ?
                         ttTable.TableName = {&RecByteStat}.
    END CASE.
  END. /* DO TRANSACTION */

/* Get _Index:  ------------------------------------------------------------ */
  CREATE BUFFER hBuffer FOR TABLE "DICTDB._Index":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH DICTDB._Index NO-LOCK WHERE _Idx-Num GT 0":U)
  NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

  REPEAT WHILE NOT hQuery:QUERY-OFF-END:

    FOR FIRST ttTable NO-LOCK
        WHERE ttTable.DbId EQ ipDbId
          AND ttTable.FileRecid EQ hBuffer:BUFFER-FIELD("_File-recid":U):BUFFER-VALUE():
      
      DO TRANSACTION:
        CREATE ttIndex.
        ASSIGN ttIndex.DbId        = ipDbId
               ttIndex.TableId     = ttTable.TableId
               ttIndex.IndexId     = hBuffer:BUFFER-FIELD("_Idx-Num":U   ):BUFFER-VALUE()
               ttIndex.IndexName   = hBuffer:BUFFER-FIELD("_Index-Name":U):BUFFER-VALUE()
               ttIndex.IndexActive = hBuffer:BUFFER-FIELD("_Active":U    ):BUFFER-VALUE()
               ttIndex.IndexUnique = hBuffer:BUFFER-FIELD("_Unique":U    ):BUFFER-VALUE()
               ttIndex.WordIdx     = hBuffer:BUFFER-FIELD("_Wordidx":U   ):BUFFER-VALUE()
               ttIndex.NumComp     = hBuffer:BUFFER-FIELD("_num-comp":U  ):BUFFER-VALUE()
               ttIndex.IndexInfo   =
                  IF ttIndex.IndexActive THEN
               ( (IF ttTable.PrimeIndex EQ hBuffer:RECID THEN "P":U ELSE "":U)
               + (IF ttIndex.IndexUnique  THEN "U":U ELSE "":U)
               + (IF ttIndex.WordIdx GT 0 THEN "W":U ELSE "":U)
               )  ELSE "I":U
               ttIndex.IdxReads    = ?
               ttIndex.IdxCreates  = ?
               ttIndex.IdxDeletes  = ?
               ttIndex.BlockReads  = ?
               ttIndex.BlockSplits = ?
               ttIndex.BlockFree   = ?
               ttIndex.StatTime    = ?
        . /* ASSIGN */
      END. /* DO TRANSACTION */

      ASSIGN
        ttIndex.LevelCount = IndexLevels(ttTable.TableName, ttIndex.IndexName,
                              vMyUserNumber, vIndexStatShift, vIndexRangeSize)
      . /* ASSIGN */
    END. /* FOR FIRST ttTable */

    hQuery:GET-NEXT().
  END. /* REPEAT */
  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.

/* Pseudo index for the per database statistics: */
  DO TRANSACTION i = 1 TO 2:
    CREATE ttIndex.
    CASE i:
      WHEN 1 THEN
      ASSIGN ttIndex.IndexName = {&TotalIndexStat}
             ttIndex.DbId      = ipDbId
             ttIndex.TableId   = 0
             ttIndex.IndexId   = 0
      . /* ASSIGN */
      WHEN 2 THEN
      ASSIGN ttIndex.IndexName = {&IdxByteStat}
             ttIndex.DbId      = ipDbId
             ttIndex.TableId   = ?
             ttIndex.IndexId   = ?
      . /* ASSIGN */
    END CASE.
    ASSIGN ttIndex.AreaNumber  = 0
           ttIndex.IndexInfo   = "":U
           ttIndex.IndexActive = TRUE
           ttIndex.IndexUnique = ?
           ttIndex.WordIdx     = ?
           ttIndex.NumComp     = ACCUMULATE TOTAL ttTable.NumKeyFld
           ttIndex.LevelCount  = ?
           ttIndex.IdxReads    = 0
           ttIndex.IdxCreates  = 0
           ttIndex.IdxDeletes  = 0
           ttIndex.BlockReads  = 0
           ttIndex.BlockSplits = 0
           ttIndex.BlockFree   = 0
           ttIndex.StatTime    = ?
    . /* ASSIGN */
  END. /* DO TRANSACTION */

/* Get _StorageObject:  ---------------------------------------------------- */
  CREATE BUFFER hBuffer FOR TABLE "DICTDB._StorageObject":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE(
    "FOR EACH DICTDB._StorageObject NO-LOCK WHERE _Object-number GT 0":U)
  NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    
    ASSIGN vObjectType = hBuffer:BUFFER-FIELD("_Object-type":U  ):BUFFER-VALUE()
           vObjectId   = hBuffer:BUFFER-FIELD("_Object-number":U):BUFFER-VALUE()
    . /* ASSIGN */

    CASE vObjectType:

      WHEN 1 THEN
      FOR FIRST ttTable EXCLUSIVE-LOCK
          WHERE ttTable.DbId    EQ ipDbId
            AND ttTable.TableId EQ vObjectId
      TRANSACTION:
        ASSIGN
          ttTable.AreaNumber  = hBuffer:BUFFER-FIELD("_Area-number":U  ):BUFFER-VALUE()
          ttTable.TableAttr   = hBuffer:BUFFER-FIELD("_Object-attrib":U):BUFFER-VALUE()
          ttTable.ObjectBlock = hBuffer:BUFFER-FIELD("_Object-block":U ):BUFFER-VALUE()
          ttTable.CreateLimit = hBuffer:BUFFER-FIELD("_Create-Limit":U ):BUFFER-VALUE()
          ttTable.TossLimit   = hBuffer:BUFFER-FIELD("_Toss-Limit":U   ):BUFFER-VALUE()
        . /* ASSIGN */
        FOR FIRST ttArea EXCLUSIVE
            WHERE ttArea.DbId       EQ ttTable.DbId
              AND ttArea.AreaNumber EQ ttTable.AreaNumber:
          ASSIGN ttArea.TblCount = ttArea.TblCount + 1.
        END.
      END. /* FOR FIRST ttTable TRANSACTION */

      WHEN 2 THEN
      FOR FIRST ttIndex EXCLUSIVE-LOCK
          WHERE ttIndex.DbId    EQ ipDbId
            AND ttIndex.IndexId EQ vObjectId
      TRANSACTION:
        ASSIGN
          ttIndex.AreaNumber  = hBuffer:BUFFER-FIELD("_Area-number":U  ):BUFFER-VALUE()
          ttIndex.IndexAttr   = hBuffer:BUFFER-FIELD("_Object-attrib":U):BUFFER-VALUE()
          ttIndex.ObjectBlock = hBuffer:BUFFER-FIELD("_Object-block":U ):BUFFER-VALUE()
          ttIndex.RootBlock   = hBuffer:BUFFER-FIELD("_Object-root":U  ):BUFFER-VALUE()
        . /* ASSIGN */
        FOR FIRST ttArea EXCLUSIVE
            WHERE ttArea.DbId       EQ ttIndex.DbId
              AND ttArea.AreaNumber EQ ttIndex.AreaNumber:
          ASSIGN ttArea.IdxCount = ttArea.IdxCount + 1.
        END.
      END. /* FOR FIRST ttIndex TRANSACTION */
    
      WHEN 3 THEN
      FOR FIRST ttArea EXCLUSIVE
          WHERE ttArea.DbId       EQ ipDbId
            AND ttArea.AreaNumber EQ hBuffer:BUFFER-FIELD("_Area-number":U  ):BUFFER-VALUE():
        ASSIGN ttArea.LobCount = ttArea.LobCount + 1.
      END.

    END CASE.

    hQuery:GET-NEXT().
  END. /* REPEAT */

  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.

  FOR EACH ttArea EXCLUSIVE
     WHERE ttArea.DbId       EQ ipDbId
       AND ttArea.AreaNumber GT 0:
    ACCUMULATE ttArea.TblCount (TOTAL)
               ttArea.IdxCount (TOTAL)
               ttArea.LobCount (TOTAL)
    . /* ACCUMULATE */
    ASSIGN
      ttArea.AreaInfo = ttArea.AreaInfo + "|":U
      ttArea.AreaInfo = ttArea.AreaInfo + "t":U + STRING(ttArea.TblCount)
                                                    WHEN ttArea.TblCount GT 0
      ttArea.AreaInfo = ttArea.AreaInfo + "i":U + STRING(ttArea.IdxCount)
                                                    WHEN ttArea.IdxCount GT 0
      ttArea.AreaInfo = ttArea.AreaInfo + "L":U + STRING(ttArea.LobCount)
                                                    WHEN ttArea.LobCount GT 0
    . /* ASSIGN */
  END.

  FOR EACH ttArea EXCLUSIVE
     WHERE ttArea.DbId       EQ ipDbId
       AND ttArea.AreaNumber EQ 0:
    ASSIGN ttArea.TblCount = ACCUM TOTAL ttArea.TblCount
           ttArea.IdxCount = ACCUM TOTAL ttArea.IdxCount
           ttArea.LobCount = ACCUM TOTAL ttArea.LobCount
           ttArea.AreaInfo = SUBSTITUTE("t&1i&2L&3":U,
                        /* &1 */ STRING(ttArea.TblCount),
                        /* &2 */ STRING(ttArea.IdxCount),
                        /* &3 */ STRING(ttArea.LobCount))
    . /* ASSIGN */
  END.

END PROCEDURE. /* GetMetaSchema */


/* ------------------------------------------------------------------------- */

PROCEDURE GetDbStat.

  DEFINE INPUT PARAMETER ipDbId AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE hQuery  AS HANDLE NO-UNDO. /*Query handle*/
  DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO. /*Buffer handle*/
  DEFINE VARIABLE vCurrTableId LIKE _File._File-Number NO-UNDO.
  DEFINE VARIABLE vLastTableId LIKE _File._File-Number NO-UNDO.
  DEFINE VARIABLE vCurrIndexId LIKE _Index._idx-num    NO-UNDO.
  DEFINE VARIABLE vLastIndexId LIKE _Index._idx-num    NO-UNDO.
  DEFINE VARIABLE vIdxUpdates  LIKE _IndexStat._IndexStat-create NO-UNDO.

/* Get _MstrBlk: ----------------------------------------------------------- */
  CREATE BUFFER hBuffer FOR TABLE "DICTDB._MstrBlk":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH DICTDB._MstrBlk NO-LOCK":U) NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

  IF NOT hQuery:QUERY-OFF-END THEN
  FOR EACH ttSequence EXCLUSIVE-LOCK
     WHERE ttSequence.DbId    EQ ipDbId
       AND ttSequence.SeqName EQ {&LastTask}
  TRANSACTION:
    ASSIGN ttSequence.SeqValue = hBuffer:BUFFER-FIELD("_MstrBlk-lasttask":U ):BUFFER-VALUE()
           ttSequence.StatTime = NOW
    . /* ASSIGN */
  END. /* DO TRANSACTION */
  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.

/* Get _Latch: ----------------------------------------------------------- */
  CREATE BUFFER hBuffer FOR TABLE "DICTDB._Latch":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE(
    "FOR EACH DICTDB._Latch WHERE _Latch-Name MATCHES '*SEQ' NO-LOCK":U)
  NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

  IF NOT hQuery:QUERY-OFF-END THEN
  FOR EACH ttSequence EXCLUSIVE-LOCK
     WHERE ttSequence.DbId    EQ ipDbId
       AND ttSequence.SeqName EQ {&SeqLocks}
  TRANSACTION:
    ASSIGN ttSequence.SeqValue = hBuffer:BUFFER-FIELD("_Latch-Lock":U ):BUFFER-VALUE()
           ttSequence.StatTime = NOW
    . /* ASSIGN */
  END. /* DO TRANSACTION */
  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.

/* Get sequence current values: -------------------------------------------- */
  
  FOR EACH ttSequence EXCLUSIVE-LOCK
     WHERE ttSequence.DbId  EQ ipDbId
       AND ttSequence.SeqId NE ?:
  /* No TRANSACTION keyword to avoid a transaction allccated in database */
    ASSIGN ttSequence.SeqValue = DYNAMIC-CURRENT-VALUE(ttSequence.SeqName, "DICTDB":U)
           ttSequence.StatTime = NOW
    . /* ASSIGN */
  END. /* DO TRANSACTION */

/* Get _ActIOType: --------------------------------------------------------- */

  CREATE BUFFER hBuffer FOR TABLE "DICTDB._ActIOType":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH DICTDB._ActIOType NO-LOCK":U) NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

/* Record block reads per database: */
  FOR EACH ttTable EXCLUSIVE-LOCK
     WHERE ttTable.DbId    EQ ipDbId
       AND ttTable.TableId EQ 0
  TRANSACTION:
    ASSIGN ttTable.BlockReads = hBuffer:BUFFER-FIELD("_IOType-DataReads":U):BUFFER-VALUE()
           ttTable.StatTime   = NOW
    . /* ASSIGN */
  END. /* FOR EACH ttTable TRANSACTION */

/* Index block reads per database: */
  FOR EACH ttIndex EXCLUSIVE-LOCK
     WHERE ttIndex.DbId    EQ ipDbId
       AND ttIndex.IndexId EQ 0
  TRANSACTION:
    ASSIGN ttIndex.BlockReads = hBuffer:BUFFER-FIELD("_IOType-IdxRds":U):BUFFER-VALUE().
           ttIndex.StatTime   = NOW
    . /* ASSIGN */
  END. /* FOR EACH ttIndex TRANSACTION */

  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.

/* Get _ActRecord: --------------------------------------------------------- */

  CREATE BUFFER hBuffer FOR TABLE "DICTDB._ActRecord":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH DICTDB._ActRecord NO-LOCK":U) NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

/* Record reads per database: */
  FOR FIRST ttTable EXCLUSIVE-LOCK
      WHERE ttTable.DbId    EQ ipDbId
        AND ttTable.TableId EQ 0
  TRANSACTION:
    ASSIGN ttTable.RecReads   = hBuffer:BUFFER-FIELD("_Record-RecRead":U ):BUFFER-VALUE()
           ttTable.RecUpdates = hBuffer:BUFFER-FIELD("_Record-RecUpd":U  ):BUFFER-VALUE()
           ttTable.RecCreates = hBuffer:BUFFER-FIELD("_Record-RecCreat":U):BUFFER-VALUE()
           ttTable.RecDeletes = hBuffer:BUFFER-FIELD("_Record-RecDel":U  ):BUFFER-VALUE()
           ttTable.StatTime   = NOW
    . /* ASSIGN */
  END. /* FOR FIRST ttTable TRANSACTION */

/* Byte reads per database: */
  FOR FIRST ttTable EXCLUSIVE-LOCK
      WHERE ttTable.DbId    EQ ipDbId
        AND ttTable.TableId EQ ?
  TRANSACTION:
    ASSIGN ttTable.RecReads   = hBuffer:BUFFER-FIELD("_Record-BytesRead":U ):BUFFER-VALUE()
           ttTable.RecUpdates = hBuffer:BUFFER-FIELD("_Record-BytesUpd":U  ):BUFFER-VALUE()
           ttTable.RecCreates = hBuffer:BUFFER-FIELD("_Record-BytesCreat":U):BUFFER-VALUE()
           ttTable.RecDeletes = hBuffer:BUFFER-FIELD("_Record-BytesDel":U  ):BUFFER-VALUE()
           ttTable.StatTime   = NOW
    . /* ASSIGN */
  END. /* FOR FIRST ttTable TRANSACTION */

  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.

/* Get _TableStat: --------------------------------------------------------- */

  CREATE BUFFER hBuffer FOR TABLE "DICTDB._TableStat":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH DICTDB._TableStat NO-LOCK":U) NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  ASSIGN vCurrTableId = 0
         vLastTableId = 0
  . /* ASSIGN */

  FOR EACH ttTable NO-LOCK
     WHERE ttTable.DbId    EQ ipDbId
       AND ttTable.TableId NE ?
        BY ttTable.DbId
        BY ttTable.TableId DESCENDING:
    ASSIGN vLastTableId = ttTable.TableId.
    LEAVE.
  END.
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

  REPEAT WHILE vCurrTableId LT vLastTableId AND NOT hQuery:QUERY-OFF-END:

    ASSIGN vCurrTableId = hBuffer:BUFFER-FIELD("_TableStat-id":U):BUFFER-VALUE().

    FOR FIRST ttTable EXCLUSIVE-LOCK
        WHERE ttTable.DbId    EQ ipDbId
          AND ttTable.TableId EQ vCurrTableId
    TRANSACTION:
      ASSIGN ttTable.RecReads   = hBuffer:BUFFER-FIELD("_TableStat-read":U  ):BUFFER-VALUE()
             ttTable.RecUpdates = hBuffer:BUFFER-FIELD("_TableStat-update":U):BUFFER-VALUE()
             ttTable.RecCreates = hBuffer:BUFFER-FIELD("_TableStat-create":U):BUFFER-VALUE()
             ttTable.RecDeletes = hBuffer:BUFFER-FIELD("_TableStat-delete":U):BUFFER-VALUE()
             ttTable.BlockReads = hBuffer:BUFFER-FIELD("_TableStat-OsRead":U):BUFFER-VALUE()
             ttTable.StatTime   = NOW
      . /* ASSIGN */
    END. /* FOR FIRST ttTable TRANSACTION */

    hQuery:GET-NEXT().
  END. /* REPEAT WHILE vCurrTableId LT vLastTableId */

  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.

/* Get _ActIndex: --------------------------------------------------------- */

  CREATE BUFFER hBuffer FOR TABLE "DICTDB._ActIndex":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH DICTDB._ActIndex NO-LOCK":U) NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

/* Record reads per database: */
  FOR FIRST ttIndex EXCLUSIVE-LOCK
      WHERE ttIndex.DbId    EQ ipDbId
        AND ttIndex.IndexId EQ 0
  TRANSACTION:
    ASSIGN ttIndex.IdxReads    = hBuffer:BUFFER-FIELD("_Index-Find":U  ):BUFFER-VALUE()
           ttIndex.IdxCreates  = hBuffer:BUFFER-FIELD("_Index-Create":U):BUFFER-VALUE()
           ttIndex.IdxDeletes  = hBuffer:BUFFER-FIELD("_Index-Delete":U):BUFFER-VALUE()
           ttIndex.BlockSplits = hBuffer:BUFFER-FIELD("_Index-Splits":U):BUFFER-VALUE()
           ttIndex.BlockFree   = hBuffer:BUFFER-FIELD("_Index-Free":U  ):BUFFER-VALUE()
           ttIndex.StatTime    = NOW
    . /* ASSIGN */
  END. /* FOR FIRST ttIndex TRANSACTION */

  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.

/* Get _IndexStat: --------------------------------------------------------- */

  CREATE BUFFER hBuffer FOR TABLE "DICTDB._IndexStat":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH DICTDB._IndexStat NO-LOCK":U) NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  ASSIGN vCurrIndexId = 0
         vLastIndexId = 0
  . /* ASSIGN */

  FOR EACH ttIndex NO-LOCK
     WHERE ttIndex.DbId    EQ ipDbId
       AND ttIndex.IndexId NE ?
        BY ttIndex.DbId
        BY ttIndex.IndexId DESCENDING:
    ASSIGN vLastIndexId = ttIndex.IndexId.
    LEAVE.
  END.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

  REPEAT WHILE vCurrIndexId LT vLastIndexId AND NOT hQuery:QUERY-OFF-END:

    ASSIGN vCurrIndexId = hBuffer:BUFFER-FIELD("_IndexStat-id":U):BUFFER-VALUE().

    FOR FIRST ttIndex EXCLUSIVE-LOCK
        WHERE ttIndex.DbId    EQ ipDbId
          AND ttIndex.IndexId EQ vCurrIndexId
    TRANSACTION:
      ASSIGN ttIndex.IdxReads    = hBuffer:BUFFER-FIELD("_IndexStat-read":U  ):BUFFER-VALUE()
             ttIndex.IdxCreates  = hBuffer:BUFFER-FIELD("_IndexStat-create":U):BUFFER-VALUE()
             ttIndex.IdxDeletes  = hBuffer:BUFFER-FIELD("_IndexStat-delete":U):BUFFER-VALUE()
             ttIndex.BlockReads  = hBuffer:BUFFER-FIELD("_IndexStat-OsRead":U):BUFFER-VALUE()
             ttIndex.BlockSplits = hBuffer:BUFFER-FIELD("_IndexStat-split":U ):BUFFER-VALUE()
             ttIndex.BlockFree   = hBuffer:BUFFER-FIELD("_IndexStat-blockdelete":U):BUFFER-VALUE()
             ttIndex.StatTime    = NOW

/* Correction of Progress bug:
  _IndexStat-split returns the sum of the real splits and deletes.
  _IndexStat-blockdelete always returns zero.
  Restore the real real splits and deletes based on their sum
  in proportion of _IndexStat-create and _IndexStat-delete.
*/           vIdxUpdates = ttIndex.IdxCreates + ttIndex.IdxDeletes
      . /* ASSIGN */

      IF ttIndex.BlockFree EQ 0 AND vIdxUpdates GT 0 THEN
      ASSIGN ttIndex.BlockFree   =  ttIndex.BlockSplits
                                 * (ttIndex.IdxDeletes / vIdxUpdates)
             ttIndex.BlockSplits =  ttIndex.BlockSplits
                                 -  ttIndex.BlockFree
      . /* ASSIGN */
    END. /* FOR FIRST ttIndex TRANSACTION */
    
    hQuery:GET-NEXT().
  END. /* REPEAT WHILE vCurrIndexId LT vLastIndexId */

  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.

END PROCEDURE. /* GetDbStat */


/* ------------------------------------------------------------------------- */

PROCEDURE PutDbStat.

  DEFINE INPUT PARAMETER ipOutputPrefix AS CHARACTER NO-UNDO.

/* Excel limit: rows per worksheet: 1,048,576.
   Approximate row size in vDbStatFile ~ 193 bytes.
   Approximate row size in vSeqDmpFile ~ 82 bytes.
   Rename the existing files if their size exceeds vFileSizeLimit (80 MB):
*/
  DEFINE VARIABLE vFileSizeLimit AS INTEGER NO-UNDO INITIAL 80000000.

  DEFINE VARIABLE vDbStatFile   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vSeqDmpFile   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vTableInB2    AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE vIndexInB2    AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE vSnapshotTime AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vStatDelay    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vSeqUpdates   AS INT64     NO-UNDO.
  DEFINE VARIABLE i             AS INTEGER   NO-UNDO.

  DEFINE BUFFER bfTableArea FOR ttArea.
  DEFINE BUFFER bfIndexArea FOR ttArea.

  DEFINE VARIABLE vErrorDesc    AS CHARACTER NO-UNDO EXTENT 18 INITIAL [
    /* 1*/ "Not owner",
    /* 2*/ "No such file or directory",
    /* 3*/ "Interrupted system call",
    /* 4*/ "I/O error",
    /* 5*/ "Bad file number",
    /* 6*/ "No more processes",
    /* 7*/ "Not enough core memory",
    /* 8*/ "Permission denied",
    /* 9*/ "Bad address",
    /*10*/ "File exists",
    /*11*/ "No such device",
    /*12*/ "Not a directory",
    /*13*/ "Is a directory",
    /*14*/ "File table overflow",
    /*15*/ "Too many open files",
    /*16*/ "File too large",
    /*17*/ "No space left on device",
    /*18*/ "Directory not empty"
  ]. /* vErrorDesc */

  &SCOPED-DEFINE Sep "~t"

  ASSIGN ipOutputPrefix = "DbStatDump" WHEN ipOutputPrefix EQ "":U
                                         OR ipOutputPrefix EQ ?
         ipOutputPrefix = ipOutputPrefix + ".":U + LocalHostName() 
         vDbStatFile = ipOutputPrefix + ".DbStat.txt"
         vSeqDmpFile = ipOutputPrefix + ".SeqDmp.txt"
  . /* ASSIGN */

  FILE-INFO:FILE-NAME = vDbStatFile.
  IF  FILE-INFO:FULL-PATHNAME NE ?
  AND FILE-INFO:FILE-SIZE GE vFileSizeLimit THEN
  OS-RENAME VALUE(FILE-INFO:FULL-PATHNAME)
            VALUE(SUBSTRING(FILE-INFO:FULL-PATHNAME, 1,
                     LENGTH(FILE-INFO:FULL-PATHNAME) - 3)
                + DateTime2String(DATETIME(TODAY, MTIME))
                + ".txt").

/*OS-DELETE VALUE(vDbStatFile).*/
  OUTPUT TO VALUE(vDbStatFile) APPEND.
  IF SEEK(OUTPUT) EQ 0 THEN
  PUT UNFORMATTED
           "Db Host"
    {&Sep} "Db Name"
    {&Sep} "From"
    {&Sep} "To"
    {&Sep} "T(ms)"
    {&Sep} "I(ms)"
    {&Sep} "Table"
    {&Sep} "Table #"
    {&Sep} "Fields"
    {&Sep} "Indexes"
    {&Sep} "B2"
    {&Sep} "Table Area"
    {&Sep} "Area #"
    {&Sep} "HWM"
    {&Sep} "Index"
    {&Sep} "Index #"
    {&Sep} "Fields"
    {&Sep} "State"
    {&Sep} "B2"
    {&Sep} "Index Area"
    {&Sep} "Area #"
    {&Sep} "HWM"
    {&Sep} "Levels"
    {&Sep} "RecReads"
    {&Sep} "IdxReads"
    {&Sep} "RecUpdates"
    {&Sep} "RecCreates"
    {&Sep} "IdxCreates"
    {&Sep} "RecDeletes"
    {&Sep} "IdxDeletes"
    {&Sep} "RecBlkReads"
    {&Sep} "IdxBlkReads"
    {&Sep} "BlockSplits"
    {&Sep} "BlockFree"
  SKIP . /* PUT */

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  DO:
/* Report the errors to the main out file: */
    DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
      PUT UNFORMATTED
              DateTime2Excel(DATETIME(TODAY, MTIME))
        SPACE "Error:"
        SPACE ERROR-STATUS:GET-NUMBER(i)
        SPACE ERROR-STATUS:GET-MESSAGE(i)
      SKIP. /* PUT */
    END.
    OUTPUT CLOSE.

/* Report the errors on screen of interactive session: */
    IF SESSION:BATCH-MODE THEN
    QUIT.

    MESSAGE
      "Errors querying database(s):" 
      ERROR-STATUS:NUM-MESSAGES   SKIP
      ERROR-STATUS:GET-MESSAGE(1) SKIP
      ERROR-STATUS:GET-MESSAGE(2) SKIP
      ERROR-STATUS:GET-MESSAGE(3) SKIP
      "Saved to" vDbStatFile
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.

    RETURN.
  END.

/* Snapshot time is a time when first _TableStat record was queried: */
  ASSIGN vSnapshotTime = NOW.
  FOR EACH ttDb NO-LOCK,

      EACH ttTable NO-LOCK
     WHERE ttTable.DbId     EQ ttDb.DbId
       AND ttTable.TableId  GT 0
       AND ttTable.RecReads NE ?

     BY ttDb.DbId
     BY ttTable.TableId:

    ASSIGN vSnapshotTime = ttTable.StatTime.
    LEAVE.
  END.

/* Report for _TableStat/_IndexStat: */

  FOR EACH ttDb NO-LOCK,

      EACH bfTableArea NO-LOCK
     WHERE bfTableArea.DbId EQ ttDb.DbId,

      EACH ttTable NO-LOCK
     WHERE ttTable.DbId       EQ bfTableArea.DbId
       AND ttTable.AreaNumber EQ bfTableArea.AreaNumber

        BY ttDb.PhysName
        BY bfTableArea.AreaNumber
        BY ttTable.TableName:

    ASSIGN vTableInB2 = GET-BITS(bfTableArea.AreaAttr, 7, 1) EQ 1
                     OR GET-BITS(ttTable.TableAttr,    7, 1) EQ 1
           vStatDelay = INTERVAL(ttTable.StatTime, vSnapshotTime,
                                "milliseconds":U)
           vStatDelay = ABSOLUTE(vStatDelay)
    . /* ASSIGN */
 
    FOR EACH ttIndex NO-LOCK
       WHERE ttIndex.DbId    EQ ttTable.DbId   
         AND ttIndex.TableId EQ ttTable.TableId,
   
       FIRST bfIndexArea NO-LOCK
       WHERE bfIndexArea.DbId       EQ ttIndex.DbId      
         AND bfIndexArea.AreaNumber EQ ttIndex.AreaNumber
   
          BY ttIndex.IndexName:

      ASSIGN vIndexInB2 = GET-BITS(bfIndexArea.AreaAttr, 7, 1) EQ 1
                       OR GET-BITS(ttIndex.IndexAttr,    7, 1) EQ 1
      . /* ASSIGN */

      PUT UNFORMATTED
               ttDb.HostName                /* Db Host     */
        {&Sep} ttDb.PhysName                /* Db Name     */
        {&Sep} ttDb.StartTime               /* From        */
        {&Sep} vSnapshotTime                /* To          */
        {&Sep} vStatDelay                   /* T(ms)       */
        {&Sep} INTERVAL(ttIndex.StatTime, ttTable.StatTime, "milliseconds":U) /*I(ms)*/
        {&Sep} ttTable.TableName            /* Table       */
        {&Sep} ttTable.TableId              /* Table #     */
        {&Sep} ttTable.NumFld               /* Fields      */
        {&Sep} ttTable.NumKey               /* Indexes     */
     /* {&Sep} ttTable.Template              */
        {&Sep} IF vTableInB2 THEN "*" ELSE "":U /* B2      */
        {&Sep} bfTableArea.AreaInfo         /* Tbl Area    */
        {&Sep} bfTableArea.AreaNumber       /* Area #      */
        {&Sep} bfTableArea.AreaHWM          /* HWM         */
        {&Sep} ttIndex.IndexName            /* Index       */
        {&Sep} ttIndex.IndexId              /* Index #     */
        {&Sep} ttIndex.NumComp              /* Fields      */
        {&Sep} ttIndex.IndexInfo            /* State       */
        {&Sep} IF vIndexInB2 THEN "*" ELSE "":U /* B2      */
        {&Sep} bfIndexArea.AreaInfo         /* Idx Area    */
        {&Sep} bfIndexArea.AreaNumber       /* Area #      */
        {&Sep} bfIndexArea.AreaHWM          /* HWM         */
        {&Sep} ttIndex.LevelCount           /* Levels      */
        {&Sep} ttTable.RecReads             /* RecReads    */
        {&Sep} ttIndex.IdxReads             /* IdxReads    */
        {&Sep} ttTable.RecUpdates           /* RecUpdates  */
        {&Sep} ttTable.RecCreates           /* RecCreates  */
        {&Sep} ttIndex.IdxCreates           /* IdxCreates  */
        {&Sep} ttTable.RecDeletes           /* RecDeletes  */
        {&Sep} ttIndex.IdxDeletes           /* IdxDeletes  */
        {&Sep} ttTable.BlockReads           /* RecBlkReads */
        {&Sep} ttIndex.BlockReads           /* IdxBlkReads */
        {&Sep} ttIndex.BlockSplits          /* BlockSplits */
        {&Sep} ttIndex.BlockFree            /* BlockFree   */
      SKIP . /* PUT */

      ACCUMULATE
        ttIndex.AreaNumber  (MINIMUM MAXIMUM)
        ttIndex.LevelCount  (AVERAGE)
      . /* ACCUMULATE */

      IF ttIndex.IdxReads GT 0 THEN
      ACCUMULATE
        ttIndex.StatTime    (MINIMUM MAXIMUM)
        ttIndex.IdxReads    (TOTAL)
        ttIndex.IdxCreates  (TOTAL)
        ttIndex.IdxDeletes  (TOTAL)
        ttIndex.BlockReads  (TOTAL)
        ttIndex.BlockSplits (TOTAL)
        ttIndex.BlockFree   (TOTAL)
      . /* ACCUMULATE */

      IF vIndexInB2 THEN
      ACCUMULATE "Index in B2":U (COUNT).
    END. /* FOR EACH ttIndex, FIRST bfIndexArea */

    IF (ACCUM MINIMUM ttIndex.AreaNumber)
    EQ (ACCUM MAXIMUM ttIndex.AreaNumber) THEN
    FIND FIRST bfIndexArea NO-LOCK
         WHERE bfIndexArea.DbId       EQ ttDb.DbId      
           AND bfIndexArea.AreaNumber EQ ACCUM MINIMUM ttIndex.AreaNumber.
    ELSE
    RELEASE bfIndexArea.
    
    IF ttTable.TableId EQ 0
    OR ttTable.TableId EQ ? THEN
    NEXT.

    PUT UNFORMATTED
             ttDb.HostName                    /* Db Host     */
      {&Sep} ttDb.PhysName                    /* Db Name     */
      {&Sep} ttDb.StartTime                   /* From        */
      {&Sep} vSnapshotTime                    /* To          */
      {&Sep} vStatDelay                       /* T(ms)       */
      {&Sep} INTERVAL(ACCUM MAX ttIndex.StatTime,
                      ACCUM MIN ttIndex.StatTime,
                      "milliseconds":U)       /* I(ms)       */
      {&Sep} ttTable.TableName                /* Table       */
      {&Sep} ttTable.TableId                  /* Table #     */
      {&Sep} ttTable.NumFld                   /* Fields      */
      {&Sep} ttTable.NumKey                   /* Indexes     */
   /* {&Sep} ttTable.Template              */
      {&Sep} IF vTableInB2 THEN "*" ELSE "":U /* B2          */
      {&Sep} bfTableArea.AreaInfo             /* Tbl Area    */
      {&Sep} bfTableArea.AreaNumber           /* Area #      */
      {&Sep} bfTableArea.AreaHWM              /* HWM         */
      {&Sep} {&TotalIndexStat}                /* Index       */
      {&Sep} 0                                /* Index #     */
      {&Sep} ttTable.NumKeyFld                /* Fields      */
      {&Sep} "":U                             /* State       */
      {&Sep} IF (ACCUM COUNT "Index in B2":U) EQ 0 THEN "":U ELSE
             STRING(ACCUM COUNT "Index in B2":U) /* B2       */
    . /* PUT */
    IF AVAILABLE bfIndexArea THEN
    PUT UNFORMATTED
      {&Sep} bfIndexArea.AreaInfo             /* Idx Area    */
      {&Sep} bfIndexArea.AreaNumber           /* Area #      */
      {&Sep} bfIndexArea.AreaHWM              /* HWM         */
    . /* PUT */
    ELSE
    PUT UNFORMATTED
      {&Sep} "":U                             /* Idx Area    */
      {&Sep} "":U                             /* Area #      */
      {&Sep} "":U                             /* HWM         */
    . /* PUT */
    PUT UNFORMATTED
      {&Sep} ACCUM AVERAGE ttIndex.LevelCount /* Levels      */
      {&Sep} ttTable.RecReads                 /* RecReads    */
      {&Sep} ACCUM TOTAL ttIndex.IdxReads     /* IdxReads    */
      {&Sep} ttTable.RecUpdates               /* RecUpdates  */
      {&Sep} ttTable.RecCreates               /* RecCreates  */
      {&Sep} ACCUM TOTAL ttIndex.IdxCreates   /* IdxCreates  */
      {&Sep} ttTable.RecDeletes               /* RecDeletes  */
      {&Sep} ACCUM TOTAL ttIndex.IdxDeletes   /* IdxDeletes  */
      {&Sep} ttTable.BlockReads               /* RecBlkReads */
      {&Sep} ACCUM TOTAL ttIndex.BlockReads   /* IdxBlkReads */
      {&Sep} ACCUM TOTAL ttIndex.BlockSplits  /* BlockSplits */
      {&Sep} ACCUM TOTAL ttIndex.BlockFree    /* BlockFree   */
    SKIP . /* PUT */
  
  END. /* FOR EACH ttDb, EACH bfTableArea, EACH ttTable  */

  PUT CONTROL NULL(0).
  OUTPUT CLOSE.

/*OS-DELETE VALUE(vSeqDmpFile).*/
  IF NOT CAN-FIND(FIRST ttSequence WHERE ttSequence.SeqId NE ?) THEN
  ASSIGN vSeqDmpFile = "":U.
  ELSE
  DO:
    FILE-INFO:FILE-NAME = vSeqDmpFile.
    IF  FILE-INFO:FULL-PATHNAME NE ?
    AND FILE-INFO:FILE-SIZE GE vFileSizeLimit THEN
    OS-RENAME VALUE(FILE-INFO:FULL-PATHNAME)
              VALUE(SUBSTRING(FILE-INFO:FULL-PATHNAME, 1,
                       LENGTH(FILE-INFO:FULL-PATHNAME) - 3)
                  + DateTime2String(DATETIME(TODAY, MTIME))
                  + ".txt").

    OUTPUT TO VALUE(vSeqDmpFile) APPEND.
    IF SEEK(OUTPUT) EQ 0 THEN
    PUT UNFORMATTED
             "Db Host"
      {&Sep} "Db Name"
      {&Sep} "From"
      {&Sep} "To"
    /*{&Sep} "S(ms)"*/
      {&Sep} "Sequence"
      {&Sep} "Seq #"
      {&Sep} "Updates"
    SKIP . /* PUT */

/* Snapshot time is a time when first _TableStat record was queried: */
    ASSIGN vSnapshotTime = NOW.
    FOR EACH ttDb NO-LOCK,
  
        EACH ttSequence NO-LOCK
       WHERE ttSequence.DbId  EQ ttDb.DbId
         AND ttSequence.SeqId NE ?
  
       BY ttDb.DbId
       BY ttSequence.SeqId:
  
      ASSIGN vSnapshotTime = ttSequence.StatTime.
      LEAVE.
    END.

/* Dump of sequence current values: */
    FOR EACH ttDb NO-LOCK
          BY ttDb.PhysName:

      FOR EACH ttSequence NO-LOCK
         WHERE ttSequence.DbId EQ ttDb.DbId
            BY ttSequence.DbId
            BY ttSequence.SeqName:

        ASSIGN vSeqUpdates = (   ttSequence.SeqValue
                             -   ttSequence.SeqInit
                             ) / ttSequence.SeqIncr
        . /* ASSIGN */

        PUT UNFORMATTED
                  ttDb.HostName                /* Db Host  */
          {&Sep}  ttDb.PhysName                /* Db Name  */
          {&Sep}  IF ttSequence.SeqName EQ {&SeqLocks}
                  THEN ttDb.StartTime          /* From     */
                  ELSE ttDb.BuildTime
          {&Sep}  vSnapshotTime                /* To       */
          {&Sep}  ttSequence.SeqName           /* Sequence */
          {&Sep}  ttSequence.SeqId             /* Seq #    */
          {&Sep}  vSeqUpdates                  /* Updates  */
        SKIP. /* PUT */
  
        IF ttSequence.SeqId NE ? THEN
        ACCUMULATE vSeqUpdates (TOTAL).

      END. /* FOR EACH ttSequence */

      PUT UNFORMATTED
                ttDb.HostName           /* Db Host  */
        {&Sep}  ttDb.PhysName           /* Db Name  */
        {&Sep}  ttDb.BuildTime          /* From     */
        {&Sep}  vSnapshotTime           /* To       */
        {&Sep}  "Total per sequences"   /* Sequence */
        {&Sep}  ?                       /* Seq #    */
        {&Sep}  ACCUM TOTAL vSeqUpdates /* Updates  */
      SKIP. /* PUT */

    END. /* FOR EACH ttDb */

    PUT CONTROL NULL(0).
    OUTPUT CLOSE.
  END.

  ASSIGN FILE-INFO:FILE-NAME = vDbStatFile
         vDbStatFile = FILE-INFO:FULL-PATHNAME
  . /* ASSIGN */

  IF vSeqDmpFile NE "":U THEN
  ASSIGN FILE-INFO:FILE-NAME = vSeqDmpFile
         vSeqDmpFile = FILE-INFO:FULL-PATHNAME
  . /* ASSIGN */

  IF SESSION:BATCH-MODE
  THEN QUIT.
  ELSE
  IF vDbStatFile NE ? THEN
  MESSAGE 
    "See results in:" SKIP
    vDbStatFile SKIP
    vSeqDmpFile
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE. /* PutDbStat */

/* ------------------------------------------------------------------------- */

/* ******************************  Main block  **************************** */

DEFINE VARIABLE i AS INTEGER NO-UNDO.

DO ON ERROR UNDO, LEAVE:
/* Static data. Execution time is not too important. */

  DO i = 1 TO NUM-DBS:
    CREATE ALIAS DICTDB FOR DATABASE VALUE(LDBNAME(i)).
    RUN GetMetaSchema(i).
  END.

/* Dynamic data. Snapshot duration should be as short as possible. */
  DO i = 1 TO NUM-DBS:
    CREATE ALIAS DICTDB FOR DATABASE VALUE(LDBNAME(i)).
    RUN GetDbStat(i).
  END.
END. /* DO ON ERROR */

RUN PutDbStat(IF SESSION:BATCH-MODE AND SESSION:PARAMETER NE "":U
              THEN ENTRY(1, SESSION:PARAMETER) ELSE ?).

EMPTY TEMP-TABLE ttDb       NO-ERROR.
EMPTY TEMP-TABLE ttArea     NO-ERROR.
EMPTY TEMP-TABLE ttTable    NO-ERROR.
EMPTY TEMP-TABLE ttIndex    NO-ERROR.
EMPTY TEMP-TABLE ttSequence NO-ERROR.
