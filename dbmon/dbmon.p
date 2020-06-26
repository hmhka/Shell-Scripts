/*------------------------------------------------------------------------
    File        : dbmon.p (an adaptation of DbStatMon.p for dbmon.sh)
    Purpose     : Gather Table/Index/Sequence/User statistics for DICTDB db.


    Syntax      : vMonCount & vMonIntrv:
                  the number of sampling intervals and their length.

    Description : Program creates a few reports in current directory.
                  You can open these files in Excel.

    Author(s)   : George Potemkin
    Created     : May 27, 2012
    Modified    : Jun 20, 2016
*/

DEFINE VARIABLE vVersion AS CHARACTER NO-UNDO INITIAL
  "DbMon.p Release 2.0 as of Jun 20, 2016".

/* The most recent version of the program can be downloaded from:
   ftp://ftp.progress-tech.ru/pub/Users/george/Scripts/dbmon/

    Fixes       :

Fixes in V1.1:
 1) Assignment Array1 = Array2 is not supported in V10.1C
    Now program saves CacheInfo by individual elements.
 2) Now program does not output zero Table/Index stats.
 3) All statements that access db are moved outside transaction blocks.
    Now program does not allocate the transactions on db side except
    the updates of the _Connect-CachingType field that does increase lasttask.
 4) Corrected the length of sampling interval for first user's activity.
 5) Fixed the compile time error in V10.1B.
 6) Added vCheckUserLock that makes the reads of _UserLock to be optional.
 7) Program can be run in batch mode.
 8) Added PROVERSION and DBPARAM() to the output of TimingLog.
 9) Optimized the populating tt_Index.FieldList in case of remote connection.
10) Set vCheckUserLock = TRUE for local connections and 32-bit Progress.
11) Dump collation tables if session is running in batch mode.
12) Fixed: Updating _Connect Virtual System Table is not supported (14378)

Fixes in V1.2:
 1) Wrong user number passed to AddToTopList() procedure.
 2) Top-lists are re-created for each snapshot.
 3) *OsRead (subtotal per table/index) should have unknown value before V10.2B.
 4) Added procedure DeleteUserStat.
 5) Added the list of the pre-selected users (vSelectedUsers).
Fixed in V1.3:
 1) Added _LockReq-RecLock/_LockReq-RecWait to ttUserStat.
 2) Added timing for more procedures.
 3) UserCache is disable for read-only connection.
 4) *.SeqStat.* and *.SeqStat.* files will not be created
    if the corresponding statistics is empty.
Fixed in V1.4:
 1) Optimized the reads of _UserIO in IndexLevels() procedure.
 2) Added tt_File.CanRead field.
 3) Added PID column to the PutUserCache procedure.
 4) Fixed: the interval of Table/Index Stat for users selected first time.
 5) Added B2 (alternate buffer pool) attribute to the Table/Index reports.
Fixed in V1.5:
 Jul 21: Create tt_File rec for "All Tables" / tt_Index rec for "All Indexes";
 Jul 22: Gathering statistics is splitted in multiple procedure
         with separate timing for each of them.
 Jul 22: Added ServerStat report.
 Jul 26: Added AreaIOStat report.
Fixed in V1.6:
 Jul 29: Restructured the temp-tables.
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
&SCOPED-DEFINE Sep "~t"

/* The list of users (SELF or REMC type) for whom the program should collect
   the detailed information (_UserTableStat/_UserIndexStat/_Connect-Cache*.
   Specify the comma-separated list of users or use CAN-DO masks.
   The list will be applied to the user numbers as well as to the user names.
*/
DEFINE VARIABLE vSelectedUsers AS CHARACTER NO-UNDO INITIAL "".

/* Default number of sampling intervals and their length: */
DEFINE VARIABLE vMonIntrv AS INTEGER NO-UNDO INITIAL 4.
DEFINE VARIABLE vMonCount AS INTEGER NO-UNDO INITIAL 5.

/* _Connect-CachingType is updatable only for some _Connect-Type.
  For connections other than SELF,REMC,SERV,SQSV we will get the error:
  Updating _Connect Virtual System Table is not supported (14378)
  Program will select only users of these types: */
DEFINE VARIABLE vSelectableTypes AS CHARACTER NO-UNDO
        INITIAL "SELF,REMC,SERV,SQSV":U.

/* Select the users to gather the detailed information when the users
   get into the one of top lists (DbAccess, DbRead, LongTran, UserLock):
 vTop*Size - the number of places in the top list;
 vTop*Toss - the candidates to the top list should exceed this limit.
 */
DEFINE VARIABLE vTopDbAccessSize AS INTEGER NO-UNDO INITIAL 5.
DEFINE VARIABLE vTopDbAccessToss AS DECIMAL NO-UNDO INITIAL 0.0.  /*per sec*/

DEFINE VARIABLE vTopDbReadSize   AS INTEGER NO-UNDO INITIAL 5.
DEFINE VARIABLE vTopDbReadToss   AS DECIMAL NO-UNDO INITIAL 20.0. /*per sec*/

/* Transaction duration: */
DEFINE VARIABLE vTopLongTranSize AS INTEGER NO-UNDO INITIAL 5.
DEFINE VARIABLE vTopLongTranToss AS INTEGER NO-UNDO INITIAL 600.  /*in sec*/

/* Record lock requests: */
DEFINE VARIABLE vTopLockReqSize AS INTEGER NO-UNDO INITIAL 5.
DEFINE VARIABLE vTopLockReqToss AS INTEGER NO-UNDO INITIAL 0.     /*locks*/

/* Only one user with the highest lock count: */
DEFINE VARIABLE vTopUserLockToss AS INTEGER NO-UNDO INITIAL 50.   /*locks*/

/* To enable the features available since V10.1B:
 _UserIndexStat, _UserTableStat, _UserLock._UserLock-Table
*/
&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) GT 10.1
OR (DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) EQ 10.1
    AND SUBSTRING(PROVERSION, INDEX(PROVERSION,".") + 2, 1) GE "B":U)
&THEN
&GLOBAL-DEFINE Version_GE_101B TRUE
&ENDIF

/* To enable the features available since V10.1C: _Connect-Cache* */
&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) GT 10.1
OR (DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) EQ 10.1
    AND SUBSTRING(PROVERSION, INDEX(PROVERSION,".") + 2, 1) GE "C":U)
&THEN
&GLOBAL-DEFINE Version_GE_101C TRUE
&ENDIF

/* To enable the features available since V10.2B:
 _TableStat._TableStat-OsRead, _IndexStat._IndexStat-OsRead
*/
&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) GT 10.2
OR (DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) EQ 10.2
    AND SUBSTRING(PROVERSION, INDEX(PROVERSION,".") + 2, 1) GE "B":U)
&THEN
&GLOBAL-DEFINE Version_GE_102B TRUE
&ENDIF

DEFINE VARIABLE vLogPrefix     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vTableStatFile  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vIndexStatFile  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vSeqStatFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE vUserStatFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE vUserCacheFile  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vServerStatFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE vAreaIOStatFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE vTimingLogFile  AS CHARACTER NO-UNDO.

DEFINE STREAM TableStat.
DEFINE STREAM IndexStat.
DEFINE STREAM SeqStat.
DEFINE STREAM UserStat.
DEFINE STREAM UserCache.
DEFINE STREAM ServerStat.
DEFINE STREAM AreaIOStat.
DEFINE STREAM TimingLog.

DEFINE VARIABLE vCurrSnapshotID  AS INTEGER NO-UNDO.
DEFINE VARIABLE vCurrDate        AS DATE    NO-UNDO.
DEFINE VARIABLE vCurrTime        AS INTEGER NO-UNDO.
DEFINE VARIABLE vCurrETime       AS INTEGER NO-UNDO.
DEFINE VARIABLE vPrevSnapshotID  AS INTEGER NO-UNDO.
DEFINE VARIABLE vPrevDate        AS DATE    NO-UNDO.
DEFINE VARIABLE vPrevTime        AS INTEGER NO-UNDO.
DEFINE VARIABLE vPrevETime       AS INTEGER NO-UNDO.
DEFINE VARIABLE vSampleInterval  AS INTEGER NO-UNDO.
DEFINE VARIABLE vSnapshotCount   AS INTEGER NO-UNDO.
DEFINE VARIABLE vStatCount       AS INTEGER NO-UNDO.
DEFINE VARIABLE vTmpRecCount     AS INTEGER NO-UNDO.
DEFINE VARIABLE vTmpRecSize      AS INTEGER NO-UNDO.
DEFINE VARIABLE vCheckUserLock   AS LOGICAL NO-UNDO.
DEFINE VARIABLE vReadOnlyConnect AS LOGICAL NO-UNDO.
DEFINE VARIABLE vLocalConnection AS LOGICAL NO-UNDO.
DEFINE VARIABLE vMyUserNumber    LIKE _MyConnection._MyConn-UserId NO-UNDO.
DEFINE VARIABLE vDbHost          AS CHARACTER NO-UNDO.
DEFINE VARIABLE vB1ObjectCount   AS INTEGER NO-UNDO EXTENT 3.
DEFINE VARIABLE vB2ObjectCount   AS INTEGER NO-UNDO EXTENT 3.

/* The value of TableNumber, IndexNumber, UserNumber that would mean "all": */
&SCOPED-DEFINE ALL            0
/* Pseudo areas: the db objects grouped together by some attributes: */
&SCOPED-DEFINE AllObjects     0
&SCOPED-DEFINE AllTables     -1
&SCOPED-DEFINE AllIndexes    -2
&SCOPED-DEFINE AllLOBs       -3
&SCOPED-DEFINE AllB1Objects  -4
&SCOPED-DEFINE AllB2Objects  -5

/* _Area-type value: */
&SCOPED-DEFINE DataArea       6

/* Caching Db Schema: */
DEFINE TEMP-TABLE ttDbAttr NO-UNDO
  FIELD AttrName AS CHARACTER
  FIELD IntValue AS INTEGER INITIAL ?
  INDEX AttrName IS UNIQUE
        AttrName
. /* TEMP-TABLE ttDbAttr */

DEFINE TEMP-TABLE tt_Area NO-UNDO
  FIELD uArea-Number      LIKE _Area._Area-Number
  FIELD uArea-Name        LIKE _Area._Area-Name
  FIELD uArea-type        LIKE _Area._Area-type
  FIELD uArea-blocksize   LIKE _Area._Area-blocksize
  FIELD uArea-recbits     LIKE _Area._Area-recbits
  FIELD uArea-clustersize LIKE _Area._Area-clustersize
  FIELD uArea-attrib      LIKE _Area._Area-attrib
  FIELD AreaInfo          AS CHARACTER /* like in .st file */
  FIELD AreaSize /*MB*/   AS DECIMAL          INITIAL ?
  FIELD ObjectCount       AS INTEGER EXTENT 3 INITIAL 0

  INDEX uArea-Number IS UNIQUE
        uArea-Number
. /* TEMP-TABLE tt_Area */

DEFINE TEMP-TABLE tt_File NO-UNDO
  FIELD uFile-Number LIKE _File._File-Number
  FIELD uFile-Name   LIKE _File._File-Name
  FIELD uPrime-Index LIKE _File._Prime-Index
  FIELD uFileRecid   AS RECID
  FIELD LastChange   AS CHARACTER
  FIELD uNumKey      LIKE _File._numkey
  FIELD uNumFld      LIKE _File._numfld
  FIELD uNumKFld     LIKE _File._numkfld
  FIELD uNumKComp    LIKE _File._numkcomp
  FIELD AreaNumber   LIKE _StorageObject._Area-Number
  FIELD ObjectBlock  LIKE _StorageObject._Object-block
  FIELD ObjectAttrib LIKE _StorageObject._Object-attrib
  FIELD InB2         AS LOGICAL
  FIELD CanRead      AS LOGICAL
  INDEX uFile-Number IS UNIQUE
        uFile-Number
  INDEX uFileRecid   IS UNIQUE
        uFileRecid
. /* TEMP-TABLE tt_File */

DEFINE TEMP-TABLE tt_Index NO-UNDO
  FIELD uIdx-Num     LIKE _Index._Idx-Num
  FIELD uIndex-Name  LIKE _Index._Index-Name
  FIELD uFile-recid  LIKE _Index._File-recid
  FIELD uIndex-recid LIKE _Index-Field._Index-recid
  FIELD AreaNumber   LIKE _StorageObject._Area-Number
  FIELD ObjectBlock  LIKE _StorageObject._Object-block
  FIELD RootBlock    LIKE _StorageObject._Object-root INITIAL {&ALL}
  FIELD ObjectAttrib LIKE _StorageObject._Object-attrib
  FIELD InB2         AS LOGICAL
  FIELD IndexAttr    AS CHARACTER
  FIELD FieldList    AS CHARACTER
  FIELD IndexLevels  AS INTEGER INITIAL ?
  INDEX uIdx-Num     IS UNIQUE
        uIdx-Num
  INDEX uIndex-Recid IS UNIQUE
        uIndex-Recid
. /* TEMP-TABLE tt_Index */

DEFINE TEMP-TABLE tt_Sequence NO-UNDO
  FIELD uSeq-Name LIKE _Sequence._Seq-Name
  FIELD uSeq-Num  LIKE _Sequence._Seq-Num
  FIELD uSeq-Init LIKE _Sequence._Seq-Init
  FIELD uSeq-Incr LIKE _Sequence._Seq-Incr
  FIELD uSeq-Min  LIKE _Sequence._Seq-Min
  FIELD uSeq-Max  LIKE _Sequence._Seq-Max
  INDEX uSeq-Num  IS UNIQUE
        uSeq-Num
. /* TEMP-TABLE tt_Sequence */

DEFINE TEMP-TABLE ttSequenceStat NO-UNDO
  FIELD SnapshotID AS INTEGER
  FIELD uSeq-Num  LIKE _Sequence._Seq-Num
  FIELD CurrValue LIKE _Sequence._Seq-Init
  INDEX SequenceStatIdx
        SnapshotID
        uSeq-Num
. /* TEMP-TABLE tt_Sequence */

DEFINE TEMP-TABLE ttUserStat NO-UNDO
  FIELD SnapshotCount       AS INTEGER /*for check if user is still connected*/
  FIELD SnapshotID          AS INTEGER
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

  FIELD uLockReq-RecLock    LIKE _LockReq._LockReq-RecLock
  FIELD uLockReq-RecWait    LIKE _LockReq._LockReq-RecWait

  FIELD LockCount  AS INTEGER   INITIAL ?  /* _UserLock-Recid */
  FIELD LockList   AS CHARACTER INITIAL ?

  INDEX UserStatIdx IS UNIQUE
        SnapshotID
        UserNumber
        uConnect-Time
        uConnect-Device
        uConnect-Pid
. /* TEMP-TABLE ttUserStat */

DEFINE TEMP-TABLE ttTableStat NO-UNDO
  FIELD SnapshotID  AS INTEGER
  FIELD UserNumber  LIKE _Connect._Connect-Usr
  FIELD TableNumber LIKE _TableStat._TableStat-Id
  FIELD TableRead   LIKE _TableStat._TableStat-Read
  FIELD TableUpdate LIKE _TableStat._TableStat-Update
  FIELD TableCreate LIKE _TableStat._TableStat-Create
  FIELD TableDelete LIKE _TableStat._TableStat-Delete
  FIELD TableOsRead LIKE _ActIOType._IOType-DataReads INITIAL ?
  FIELD StatDesc    AS CHARACTER /* Reason why the stats was captured */
  INDEX TableStatIdx IS UNIQUE
        SnapshotID
        UserNumber
        TableNumber
. /* TEMP-TABLE ttTableStat */

DEFINE TEMP-TABLE ttIndexStat NO-UNDO
  FIELD SnapshotID  AS INTEGER
  FIELD UserNumber  LIKE _Connect._Connect-Usr
  FIELD IndexNumber LIKE _IndexStat._IndexStat-Id
  FIELD IndexRead   LIKE _IndexStat._IndexStat-Read
  FIELD IndexCreate LIKE _IndexStat._IndexStat-Create
  FIELD IndexDelete LIKE _IndexStat._IndexStat-Delete
  FIELD IndexOsRead LIKE _ActIOType._IOType-IdxRds INITIAL ?
  FIELD StatDesc    AS CHARACTER /* Reason why the stats was captured */
  INDEX IndexStatIdx IS UNIQUE
        SnapshotID
        UserNumber
        IndexNumber
. /* TEMP-TABLE ttIndexStat */

DEFINE TEMP-TABLE ttAreaIOStat NO-UNDO
  FIELD SnapshotID  AS INTEGER
  FIELD AreaNumber  LIKE _Area._Area-Number
  FIELD AreaReads   LIKE _ActIOFile._IOFile-Reads
  FIELD AreaWrites  LIKE _ActIOFile._IOFile-Writes
  FIELD AreaExtends LIKE _ActIOFile._IOFile-Extends
  FIELD TableRead   LIKE _TableStat._TableStat-Read
  FIELD TableUpdate LIKE _TableStat._TableStat-Update
  FIELD TableCreate LIKE _TableStat._TableStat-Create
  FIELD TableDelete LIKE _TableStat._TableStat-Delete
  FIELD IndexRead   LIKE _IndexStat._IndexStat-Read
  FIELD IndexCreate LIKE _IndexStat._IndexStat-Create
  FIELD IndexDelete LIKE _IndexStat._IndexStat-Delete
  FIELD SortOrder   AS INTEGER
  INDEX AreaIOStatIdx IS UNIQUE
        SnapshotID
        AreaNumber
  INDEX SortOrder
        SortOrder
. /* TEMP-TABLE ttAreaIOStat */

DEFINE TEMP-TABLE tt_Servers NO-UNDO
  FIELD uServer-Num       LIKE _Servers._Server-Num
  FIELD uServer-Id        LIKE _Servers._Server-Id
  FIELD uServer-Pid       LIKE _Servers._Server-Pid
  FIELD uServer-Type      LIKE _Servers._Server-Type
  FIELD uServer-Protocol  LIKE _Servers._Server-Protocol
  FIELD uServer-Logins    LIKE _Servers._Server-Logins
  FIELD uServer-CurrUsers LIKE _Servers._Server-CurrUsers
  FIELD uServer-MaxUsers  LIKE _Servers._Server-MaxUsers
  FIELD uServer-PortNum   LIKE _Servers._Server-PortNum
  FIELD uServer-PendConn  LIKE _Servers._Server-PendConn
  INDEX uServer-Num IS UNIQUE
        uServer-Num
  INDEX uServer-Id  IS UNIQUE
        uServer-Id
. /* TEMP-TABLE tt_Servers */

/* _Servers._Server-Num <=> _Connect._Connect-Usr
   _Servers._Server-Id  <=> _ActServer._Server-Id
*/

DEFINE TEMP-TABLE tt_ActServer NO-UNDO
  FIELD SnapshotID        AS INTEGER
  FIELD uServer-Id        LIKE _ActServer._Server-Id
  FIELD uServer-MsgRec    LIKE _ActServer._Server-MsgRec
  FIELD uServer-MsgSent   LIKE _ActServer._Server-MsgSent
  FIELD uServer-ByteRec   LIKE _ActServer._Server-ByteRec
  FIELD uServer-ByteSent  LIKE _ActServer._Server-ByteSent
  FIELD uServer-RecRec    LIKE _ActServer._Server-RecRec
  FIELD uServer-RecSent   LIKE _ActServer._Server-RecSent
  FIELD uServer-QryRec    LIKE _ActServer._Server-QryRec
  FIELD uServer-TimeSlice LIKE _ActServer._Server-TimeSlice
  INDEX tt_ActServerIdx IS UNIQUE
        SnapshotID
        uServer-Id
. /* TEMP-TABLE tt_ActServer */


DEFINE TEMP-TABLE ttUserLock
  FIELD LockCount AS INTEGER
  FIELD LockList  AS CHARACTER
  INDEX LockList  IS UNIQUE
        LockList
. /* TEMP-TABLE ttUserLock */

DEFINE TEMP-TABLE ttSelectedUser
  FIELD UserNumber LIKE _Connect._Connect-Usr
  FIELD UserDesc   AS CHARACTER /* Reason why user was selected */

&IF DEFINED(Version_GE_101C)
&THEN
  FIELD CachingType     LIKE _Connect._Connect-CachingType
  FIELD CacheLastUpdate LIKE _Connect._Connect-CacheLastUpdate
  FIELD CacheInfoType   LIKE _Connect._Connect-CacheInfoType
  FIELD CacheLineNumber LIKE _Connect._Connect-CacheLineNumber
  FIELD CacheInfo       LIKE _Connect._Connect-CacheInfo
  FIELD CacheLevels     AS INTEGER
  FIELD JustSelected    AS LOGICAL
  INDEX JustSelected
        JustSelected
&ENDIF
  INDEX UserNumber IS UNIQUE
        UserNumber
. /* TEMP-TABLE ttSelectedTable */

DEFINE TEMP-TABLE ttTopList NO-UNDO
  FIELD UserNumber AS INTEGER
  FIELD TopListId  AS CHARACTER
  FIELD TopValue   AS DECIMAL
  INDEX TopValue
        TopListId
        TopValue   DESCENDING
. /* TEMP-TABLE ttTopList */

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
FUNCTION CurrentRLCounter RETURNS INTEGER (
  INPUT ipTransRLCounter AS INTEGER,
  INPUT ipTransStartTime AS CHARACTER).

  DEFINE VARIABLE vTransDate AS DATE    NO-UNDO.
  DEFINE VARIABLE vTransTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE vChkptDate AS DATE    NO-UNDO.
  DEFINE VARIABLE vChkptTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE vBiClCount AS INTEGER NO-UNDO.

  IF ipTransRLCounter EQ ? THEN
  RETURN ?.

  ASSIGN vTransDate = String2Date(ipTransStartTime)
         vTransTime = String2Time(ipTransStartTime)
         vBiClCount = 0
  . /* ASSIGN */

/* Find a checkpoint that was started before transaction: */
  FOR EACH DICTDB._Checkpoint NO-LOCK
     WHILE DICTDB._Checkpoint._Checkpoint-Time NE ?:

    ASSIGN vChkptDate = String2Date(DICTDB._Checkpoint._Checkpoint-Time)
           vChkptTime = String2Time(DICTDB._Checkpoint._Checkpoint-Time)
    . /* ASSIGN */

    IF  vTransDate GT vChkptDate
    OR (vTransDate EQ vChkptDate AND vTransTime GE vChkptTime) THEN
    RETURN ipTransRLCounter + vBiClCount.

    ASSIGN vBiClCount = vBiClCount + 1.
  END. /* FOR EACH _Checkpoint */

  RETURN ?.

/* If transaction is older than last 8 checkpoint then let's estimate
  the number of the checkpoints since transaction start
  based on the checkpoints per time interval rate: */
/*
  DEFINE VARIABLE vClustersPerSec AS DECIMAL NO-UNDO.
  FOR FIRST DICTDB._Logging NO-LOCK:

    ASSIGN
      vClustersPerSec = vBiClCount
                      - (DICTDB._Logging._Logging-BiBytesFree
                      /  DICTDB._Logging._Logging-BiClSize)
      vClustersPerSec = vClustersPerSec
                      / ((TODAY - vChkptDate) * 86400 + (TIME - vChkptTime))
      vCurrentRLCounter = ipTransRLCounter
                      + vClustersPerSec
                      * ((TODAY - vTransDate) * 86400 + (TIME - vTransTime))
    . /* ASSIGN */

    RETURN vCurrentRLCounter.
  END. /* FOR FIRST _Logging */
*/
END FUNCTION. /* CurrentRLCounter */


/* ------------------------------------------------------------------------- */
FUNCTION GetDbAttr RETURNS INTEGER (INPUT ipAttrName AS CHARACTER).
  FIND FIRST ttDbAttr
       WHERE ttDbAttr.AttrName EQ ipAttrName
  NO-ERROR.
  RETURN IF AVAILABLE ttDbAttr THEN ttDbAttr.IntValue ELSE ?.
END FUNCTION. /* GetDbAttr */


/* ******************************  Procedures  ***************************** */

/* ------------------------------------------------------------------------- */
PROCEDURE SetDbAttr.
  DEFINE INPUT PARAMETER ipAttrName  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipAttrValue AS INTEGER   NO-UNDO.

  DO TRANSACTION:
    FIND FIRST ttDbAttr
         WHERE ttDbAttr.AttrName EQ ipAttrName
    NO-ERROR.
    IF NOT AVAILABLE ttDbAttr THEN
    CREATE ttDbAttr.
    ASSIGN ttDbAttr.AttrName = ipAttrName
           ttDbAttr.IntValue = ipAttrValue.
  END.
END PROCEDURE. /* SetDbAttr */


/* ------------------------------------------------------------------------- */
PROCEDURE CreateTopList.
  DEFINE INPUT PARAMETER ipTopListId AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipTopLimit  AS INTEGER   NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  REPEAT TRANSACTION i = 1 TO ipTopLimit:
    CREATE ttTopList.
    ASSIGN ttTopList.UserNumber = ?
           ttTopList.TopListId  = ipTopListId
           ttTopList.TopValue   = 0.
  END.
END PROCEDURE. /* CreateTopList */


/* ------------------------------------------------------------------------- */
PROCEDURE AddToTopList.
  DEFINE INPUT PARAMETER ipTopListId  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipUserNumber AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipTopValue   AS DECIMAL   NO-UNDO.

  IF ipUserNumber NE {&ALL} THEN
  FOR EACH ttTopList /* INDEX TopValue: TopListId + TopValue */
     WHERE ttTopList.TopListId EQ ipTopListId
       AND ttTopList.TopValue  LT ipTopValue
        BY ttTopList.TopListId
        BY ttTopList.TopValue:
    ASSIGN ttTopList.TopValue   = ipTopValue
           ttTopList.UserNumber = ipUserNumber.
    LEAVE.
  END.
END PROCEDURE. /* AddToTopList */

/* ------------------------------------------------------------------------- */
PROCEDURE ReadSchema.

/* Number of temp-table records created to cache the database schema: */
  DEFINE OUTPUT PARAMETER opTmpRecCount AS INTEGER NO-UNDO.

/* Total size of temp-table records: */
  DEFINE OUTPUT PARAMETER opTmpRecSize  AS INTEGER NO-UNDO.

  DEFINE VARIABLE vDbBlockSize LIKE _Area._Area-blocksize NO-UNDO.
  DEFINE VARIABLE vRecCount    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vFieldList   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vObjectInB2  AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  ASSIGN vStatCount    = 0
         opTmpRecCount = 0
         opTmpRecSize  = 0.
  FOR FIRST  DICTDB._Db NO-LOCK
      WHERE  DICTDB._Db._Db-local EQ TRUE:

    FOR EACH DICTDB._File OF DICTDB._Db NO-LOCK
       WHERE DICTDB._File._File-Number GT 0
         AND DICTDB._File._File-Number LT 32768
    TRANSACTION:

      CREATE tt_File.
      ASSIGN tt_File.uFile-Number = DICTDB._File._File-Number
             tt_File.uFile-Name   = DICTDB._File._File-Name
             tt_File.uFileRecid   = RECID(DICTDB._File)
             tt_File.uPrime-Index = DICTDB._File._Prime-Index
             tt_File.CanRead = CAN-DO(DICTDB._File._Can-Read,
                                      USERID("DICTDB":U))
             tt_File.LastChange   =
         STRING(DICTDB._File._Last-Change / 86400 + 01/01/1970, "99/99/9999":U)
       + "_":U /* let Excel to treat it as text field */
       + STRING(DICTDB._File._Last-Change MOD 86400, "HH:MM:SS":U)
             tt_File.uNumKey      = DICTDB._File._numkey
             tt_File.uNumFld      = DICTDB._File._numfld
             tt_File.uNumKFld     = DICTDB._File._numkfld
             tt_File.uNumKComp    = DICTDB._File._numkcomp
             vStatCount    = vStatCount    + 1
             opTmpRecCount = opTmpRecCount + 1
             opTmpRecSize  = opTmpRecSize  + RECORD-LENGTH(tt_File)
      . /* ASSIGN */

      ACCUMULATE
        tt_File.uNumFld  (TOTAL)
        tt_File.uNumKFld (TOTAL)
      . /* ACCUMULATE */
    END. /* FOR EACH _File */

/* TableCount: */
    RUN SetDbAttr("TableCount", vStatCount).

    ASSIGN vRecCount = 0.
    FOR EACH DICTDB._Index NO-LOCK
       WHERE DICTDB._Index._Idx-Num GT 0
         AND DICTDB._Index._Idx-Num LT 32768,

      FIRST tt_File NO-LOCK
      WHERE tt_File.uFileRecid EQ DICTDB._Index._File-Recid:

      DO TRANSACTION:
        CREATE tt_Index.
        ASSIGN tt_Index.uIdx-Num     = DICTDB._Index._Idx-Num
               tt_Index.uIndex-Name  = DICTDB._Index._Index-Name
               tt_Index.uFile-Recid  = DICTDB._Index._File-Recid
               tt_Index.uIndex-Recid = RECID(DICTDB._Index)
               tt_Index.IndexAttr    =
                 (IF DICTDB._Index._Active THEN "":U ELSE "i":U) +
                 (IF tt_File.uPrime-Index EQ RECID(DICTDB._Index)
                  THEN "p":U ELSE "":U) +
                 (IF DICTDB._Index._Unique THEN "u":U ELSE "":U) +
                 (IF DICTDB._Index._Wordidx GT 0 THEN "w":U ELSE "":U)
              /* + STRING(DICTDB._Index._num-comp) */
               vRecCount     = vRecCount     + 1
               opTmpRecCount = opTmpRecCount + 1
               opTmpRecSize  = opTmpRecSize  + RECORD-LENGTH(tt_Index)
        . /* ASSIGN */
      END.
    END. /* FOR EACH _Index */

/* IndexCount: */
    RUN SetDbAttr("IndexCount":U, vRecCount).

    ASSIGN vStatCount = vStatCount + vRecCount
           vRecCount  = 0. /* is used for tt_File {&ALL} */

/* Get the index field lists: */
    FOR EACH DICTDB._Index-Field NO-LOCK, /* INDEX _Index/Number */

       FIRST tt_Index /* INDEX uIndex-Recid */
       WHERE tt_Index.uIndex-Recid EQ DICTDB._Index-Field._Index-recid,

       FIRST DICTDB._Field OF DICTDB._Index-Field NO-LOCK

          BY DICTDB._Index-Field._Index-recid
          BY DICTDB._Index-Field._Index-Seq:

        ASSIGN tt_Index.FieldList =
               tt_Index.FieldList + DICTDB._Field._Field-Name + " ":U
               vRecCount = vRecCount + 1
               opTmpRecSize  = opTmpRecSize + LENGTH(DICTDB._Field._Field-Name)
      . /* ASSIGN */
    END. /* FOR EACH _Index-Field, FIRST tt_Index, FIRST _Field */
    ASSIGN vStatCount = vStatCount + 2 * vRecCount.

    FOR EACH DICTDB._Sequence NO-LOCK
    TRANSACTION:
      CREATE tt_Sequence.
      ASSIGN tt_Sequence.uSeq-Name = DICTDB._Sequence._Seq-Name
             tt_Sequence.uSeq-Num  = DICTDB._Sequence._Seq-Num
             tt_Sequence.uSeq-Init = DICTDB._Sequence._Seq-Init
             tt_Sequence.uSeq-Incr = DICTDB._Sequence._Seq-Incr
             tt_Sequence.uSeq-Min  = DICTDB._Sequence._Seq-Min
             tt_Sequence.uSeq-Max  = DICTDB._Sequence._Seq-Max
             vStatCount    = vStatCount    + 1
             opTmpRecCount = opTmpRecCount + 1
             opTmpRecSize  = opTmpRecSize  + RECORD-LENGTH(tt_Sequence)
      . /* ASSIGN */
    END.

    FOR EACH DICTDB._Area NO-LOCK
       WHERE DICTDB._Area._Area-Type EQ {&DataArea}
    TRANSACTION:

      CREATE tt_Area.
      ASSIGN tt_Area.uArea-Number      = DICTDB._Area._Area-Number
             tt_Area.uArea-Name        = DICTDB._Area._Area-Name
             tt_Area.uArea-type        = DICTDB._Area._Area-Type
             tt_Area.uArea-blocksize   = DICTDB._Area._Area-blocksize
             tt_Area.uArea-recbits     = DICTDB._Area._Area-recbits
             tt_Area.uArea-clustersize = DICTDB._Area._Area-clustersize
             tt_Area.uArea-attrib      = DICTDB._Area._Area-attrib
             tt_Area.AreaInfo = STRING(DICTDB._Area._Area-Number)
                      + ",":U + STRING(EXP(2,DICTDB._Area._Area-recbits))
                      + ";":U + STRING(DICTDB._Area._Area-clustersize)
             tt_Area.AreaInfo = tt_Area.AreaInfo + " B2"
                  WHEN GET-BITS(tt_Area.uArea-attrib, 7, 1) EQ 1
             vStatCount    = vStatCount    + 1
             opTmpRecCount = opTmpRecCount + 1
             opTmpRecSize  = opTmpRecSize  + RECORD-LENGTH(tt_Area)
      . /* ASSIGN */
    END. /* EACH _Area */

/* Read info from _StorageObject: */
    FOR EACH DICTDB._StorageObject OF DICTDB._Db NO-LOCK
       WHERE DICTDB._StorageObject._Object-Type      GE 1 /*Just in case of */
         AND DICTDB._StorageObject._Object-Type      LE 3 /*the future types*/
         AND DICTDB._StorageObject._Object-Number    GT 0 /*     and        */
         AND DICTDB._StorageObject._Object-associate GT 0, /*not the schema */

      FIRST tt_Area EXCLUSIVE
      WHERE tt_Area.uArea-Number EQ DICTDB._StorageObject._Area-Number
    TRANSACTION:

      ASSIGN
        i = DICTDB._StorageObject._Object-Type
        tt_Area.ObjectCount[i] = tt_Area.ObjectCount[i] + 1
        vObjectInB2 = GET-BITS(tt_Area.uArea-attrib, 7, 1) EQ 1 OR
                      GET-BITS(DICTDB._StorageObject._Object-attrib, 7, 1) EQ 1
        vStatCount = vStatCount + 1
      . /* ASSIGN */

      IF vObjectInB2
      THEN ASSIGN vB2ObjectCount[i] = vB2ObjectCount[i] + 1.
      ELSE ASSIGN vB1ObjectCount[i] = vB1ObjectCount[i] + 1.

      CASE DICTDB._StorageObject._Object-Type:
/* Table: */
        WHEN 1 THEN
        FOR FIRST tt_File
            WHERE tt_File.uFile-Number EQ DICTDB._StorageObject._Object-Number:
           ASSIGN tt_File.AreaNumber   =  DICTDB._StorageObject._Area-Number
                  tt_File.ObjectBlock  =  DICTDB._StorageObject._Object-block
                  tt_File.ObjectAttrib =  DICTDB._StorageObject._Object-attrib
                  tt_File.InB2         =  vObjectInB2
           . /* ASSIGN */
        END.

/* Index: */
        WHEN 2 THEN
        FOR FIRST tt_Index
            WHERE tt_Index.uIdx-Num    EQ DICTDB._StorageObject._Object-Number:
           ASSIGN tt_Index.AreaNumber   = DICTDB._StorageObject._Area-Number
                  tt_Index.ObjectBlock  = DICTDB._StorageObject._Object-block
                  tt_Index.RootBlock    = DICTDB._StorageObject._Object-root
                  tt_Index.ObjectAttrib = DICTDB._StorageObject._Object-attrib
                  tt_Index.InB2         = vObjectInB2
           . /* ASSIGN */
        END.
/*
/* LOB: */
        WHEN 3 THEN
        FOR FIRST DICTDB._Field NO-LOCK
            WHERE DICTDB._Field._fld-stlen EQ
                  DICTDB._StorageObject._Object-Number
             AND (
              OR  DICTDB._Field._Data-Type EQ "CLOB":U):

          CREATE ttLOB.
          ASSIGN ttLOB.LOBAttr =
              IF DICTDB._Field._Data-type = "CLOB":U
              THEN DICTDB._Field._Charset   + ",":U   + /* CLOB-CODEPAGE  */
                   DICTDB._Field._Collation + ",":U   + /* CLOB-COLLATION */
                   STRING(DICTDB._Field._Attributes1) + /* CLOB-TYPE      */
      /* _Attributes1 = IF _Field._Charset EQ _Db._Db-xl-name THEN 1 ELSE 2 */
                   (IF _Fld-case  THEN ", case-sensitive" ELSE "") +
                  ", LOB-Size: " + DICTDB._Field._fld-Misc2[1]
               ELSE
            /* IF DICTDB._Field._Data-Type EQ "BLOB":U THEN */
                    "LOB-Size: " + DICTDB._Field._fld-Misc2[1].
          . /* ASSIGN */
        END. /* FOR FIRST _Field */
*/    END CASE. /* _Object-Type */
    END. /* FOR EACH _StorageObject */
  END. /* FOR FIRST _Db */

/* Set tt_Area.AreaSize: */
  FOR EACH DICTDB._AreaStatus NO-LOCK,

     FIRST tt_Area
     WHERE tt_Area.uArea-Number EQ DICTDB._AreaStatus._AreaStatus-Areanum:

    ASSIGN tt_Area.AreaSize = (DICTDB._AreaStatus._AreaStatus-Hiwater / 1024.0)
                            * (tt_Area.uArea-blocksize / 1024) /* MB */
           vStatCount = vStatCount + 1
    . /* ASSIGN */
    ACCUMULATE tt_Area.AreaSize (TOTAL).
  END. /* FOR EACH _AreaStatus */

/* Create pseudo areas: the totals for the different groups of objects.*/
/* These areas are used in "AreaIO" report. */

  FOR FIRST tt_Area NO-LOCK
      WHERE tt_Area.uArea-Type EQ {&DataArea}:
    ASSIGN vDbBlockSize = tt_Area.uArea-blocksize.
  END.

/* All Objects: */
  DO TRANSACTION:
    CREATE tt_Area.
    ASSIGN tt_Area.uArea-Number = {&AllObjects}
           tt_Area.uArea-Name   = "Entire Db"
           tt_Area.uArea-type   = {&DataArea}
           tt_Area.uArea-blocksize = vDbBlockSize
           tt_Area.AreaSize = ACCUM TOTAL tt_Area.AreaSize
           tt_Area.AreaInfo = STRING(vDbBlockSize / 1024) + "K":U
           vStatCount    = vStatCount    + 1
           opTmpRecCount = opTmpRecCount + 1
           opTmpRecSize  = opTmpRecSize  + RECORD-LENGTH(tt_Area)
    . /* ASSIGN */

    DO i = 1 TO EXTENT(vB1ObjectCount):
      ASSIGN tt_Area.ObjectCount[i] = vB1ObjectCount[i] + vB2ObjectCount[i].
    END.
  END. /* DO TRANSACTION */

/* If there are the objects that use Alternate Buffer Pool: */
  IF vB2ObjectCount[1] + vB2ObjectCount[2] + vB2ObjectCount[3] GT 0 THEN
  FOR EACH DICTDB._ActBuffer NO-LOCK
     WHERE DICTDB._ActBuffer._Buffer-Id GE 2
       AND DICTDB._ActBuffer._Buffer-Id LE 3:

    CREATE tt_Area.
    ASSIGN tt_Area.uArea-Number = IF DICTDB._ActBuffer._Buffer-Id EQ 2
                                  THEN {&AllB1Objects}
                                  ELSE {&AllB2Objects}
           tt_Area.uArea-Name   = IF DICTDB._ActBuffer._Buffer-Id EQ 2
                                  THEN "Primary Buffer Pool"
                                  ELSE "Alternate Buffer Pool"
           tt_Area.uArea-type   = {&DataArea}
           tt_Area.uArea-blocksize = vDbBlockSize
           vStatCount    = vStatCount    + 1
           opTmpRecCount = opTmpRecCount + 1
           opTmpRecSize  = opTmpRecSize  + RECORD-LENGTH(tt_Area)
    . /* ASSIGN */

    DO i = 1 TO EXTENT(vB1ObjectCount):
      ASSIGN tt_Area.ObjectCount[i] = IF DICTDB._ActBuffer._Buffer-Id EQ 2
                                      THEN vB1ObjectCount[i]
                                      ELSE vB2ObjectCount[i].
    END.
  END. /* FOR EACH _ActBuffer */

/* Add the object counts to tt_Area.AreaInfo: */
  FOR EACH tt_Area EXCLUSIVE:
    ASSIGN
      vFieldList = ""
      vFieldList = "tbl:" + STRING(tt_Area.ObjectCount[1])
                              WHEN tt_Area.ObjectCount[1] GT 0
      vFieldList = vFieldList + MIN(vFieldList, ",":U) +
                   "idx:" + STRING(tt_Area.ObjectCount[2])
                              WHEN tt_Area.ObjectCount[2] GT 0
      vFieldList = vFieldList + MIN(vFieldList, ",":U) +
                   "lob:" + STRING(tt_Area.ObjectCount[3])
                              WHEN tt_Area.ObjectCount[3] GT 0
      tt_Area.AreaInfo = tt_Area.AreaInfo + " " + vFieldList
    . /* ASSIGN */
  END. /* FOR EACH tt_Area */

  DO TRANSACTION:

/* All Tables: */
    CREATE tt_Area.
    ASSIGN tt_Area.uArea-Number = {&AllTables}
           tt_Area.uArea-Name   = "All Tables"
           tt_Area.uArea-type   = {&DataArea}
           tt_Area.uArea-blocksize = vDbBlockSize
           tt_Area.AreaInfo = "tbl:" + STRING(vB1ObjectCount[1])
                                     + (IF    vB2ObjectCount[1]  GT 0 THEN
                               "+":U + STRING(vB2ObjectCount[1]) ELSE "":U)
           tt_Area.ObjectCount[1] = vB1ObjectCount[1] + vB2ObjectCount[1]
           vStatCount    = vStatCount    + 1
           opTmpRecCount = opTmpRecCount + 1
           opTmpRecSize  = opTmpRecSize  + RECORD-LENGTH(tt_Area)
    . /* ASSIGN */

/* All Indexes: */
    CREATE tt_Area.
    ASSIGN tt_Area.uArea-Number = {&AllIndexes}
           tt_Area.uArea-Name   = "All Indexes"
           tt_Area.uArea-type   = {&DataArea}
           tt_Area.uArea-blocksize = vDbBlockSize
           tt_Area.AreaInfo = "idx:" + STRING(vB1ObjectCount[2])
                                     + (IF    vB2ObjectCount[2]  GT 0 THEN
                               "+":U + STRING(vB2ObjectCount[2]) ELSE "":U)
           tt_Area.ObjectCount[2] = vB1ObjectCount[2] + vB2ObjectCount[2]
           vStatCount    = vStatCount    + 1
           opTmpRecCount = opTmpRecCount + 1
           opTmpRecSize  = opTmpRecSize  + RECORD-LENGTH(tt_Area)
    . /* ASSIGN */
  END. /* DO TRANSACTION */

/* All LOBs: */
  IF vB1ObjectCount[3] + vB2ObjectCount[3] GT 0 THEN
  DO TRANSACTION:
    CREATE tt_Area.
    ASSIGN tt_Area.uArea-Number = {&AllLOBs}
           tt_Area.uArea-Name   = "All LOBs (Dark Matter)"
           tt_Area.uArea-type   = {&AllLOBs}
           tt_Area.uArea-blocksize = vDbBlockSize
           tt_Area.AreaInfo = "lob:" + STRING(vB1ObjectCount[3])
                                     +    (IF vB2ObjectCount[3]  GT 0 THEN
                               "+":U + STRING(vB2ObjectCount[3]) ELSE "":U)
           tt_Area.ObjectCount[3] = vB1ObjectCount[3] + vB2ObjectCount[3]
           vStatCount    = vStatCount    + 1
           opTmpRecCount = opTmpRecCount + 1
           opTmpRecSize  = opTmpRecSize  + RECORD-LENGTH(tt_Area)
    . /* ASSIGN */
  END. /* DO TRANSACTION */

  FOR FIRST DICTDB._Logging NO-LOCK
  TRANSACTION:

    CREATE tt_Area.
    ASSIGN tt_Area.uArea-Number      = 3
           tt_Area.uArea-Name        = "Primary Recovery Area"
           tt_Area.uArea-type        = tt_Area.uArea-Number
           tt_Area.uArea-blocksize   = DICTDB._Logging._Logging-BiBlkSize
           tt_Area.uArea-clustersize = DICTDB._Logging._Logging-BiClSize
           tt_Area.AreaInfo = STRING(tt_Area.uArea-blocksize / 1024) + "K":U
                + " -bi ":U + TRIM(STRING(DICTDB._Logging._Logging-BiClSize
                                          / 1048576.0, ">>9.9":U)) + "M":U
           tt_Area.AreaSize = DICTDB._Logging._Logging-BiLogSize / 1024.0
           vStatCount    = vStatCount    + 1
           opTmpRecCount = opTmpRecCount + 1
           opTmpRecSize  = opTmpRecSize  + RECORD-LENGTH(tt_Area)
    . /* ASSIGN */
  END. /* FOR FIRST _Logging */

/* Create pseudo objects: the totals for the different groups of objects.*/
/* These objects are used in "Tables" and "Indexes" reports. */

  FOR FIRST DICTDB._MstrBlk NO-LOCK
  TRANSACTION:
    RUN SetDbAttr("DbVers":U, DICTDB._MstrBlk._MstrBlk-DbVers).

    CREATE tt_File.
    ASSIGN tt_File.uFile-Number = {&ALL}
           tt_File.uFile-Name   = "All Tables"
           tt_File.uFileRecid   = {&ALL}
           tt_File.AreaNumber   = {&AllObjects}
           tt_File.ObjectBlock  = ?
           tt_File.ObjectAttrib = ?
           tt_File.InB2         = vB2ObjectCount[1] GT 0
           tt_File.LastChange   =
        STRING(String2Date(DICTDB._MstrBlk._MstrBlk-timestamp), "99/99/9999":U)
      + "_":U /* let Excel to treat it as text field */
      + STRING(String2Time(DICTDB._MstrBlk._MstrBlk-timestamp), "HH:MM:SS":U)
           tt_File.uNumKey      = GetDbAttr("IndexCount":U)
           tt_File.uNumFld      = ACCUM TOTAL tt_File.uNumFld
           tt_File.uNumKFld     = ACCUM TOTAL tt_File.uNumKFld
           tt_File.uNumKComp    = vRecCount
           opTmpRecCount = opTmpRecCount + 1
           opTmpRecSize  = opTmpRecSize  + RECORD-LENGTH(tt_File)
    . /* ASSIGN */

    CREATE tt_Index.
    ASSIGN tt_Index.uIdx-Num    = {&ALL}
           tt_Index.uIndex-Name = "All Indexes"
           tt_Index.AreaNumber  = {&AllObjects}
           tt_Index.InB2        = vB2ObjectCount[2] GT 0
           tt_Index.uFile-recid = tt_File.uFileRecid
           tt_Index.IndexAttr   = "idx:" + STRING(GetDbAttr("IndexCount":U))
           tt_Index.IndexLevels = 0
           opTmpRecCount = opTmpRecCount + 1
           opTmpRecSize  = opTmpRecSize  + RECORD-LENGTH(tt_Index)
    . /* ASSIGN */
  END.

  FOR FIRST DICTDB._DbStatus NO-LOCK:
    RUN SetDbAttr("ShmVers", DICTDB._DbStatus._DbStatus-ShmVers).
  END.

/* HighestTableId: */
  FOR EACH tt_File NO-LOCK
        BY tt_File.uFile-Number DESCENDING:
    RUN SetDbAttr("HighestTableId":U, tt_File.uFile-Number).
    LEAVE.
  END.

/* HighestIndexId: */
  FOR EACH tt_Index NO-LOCK
        BY tt_Index.uIdx-Num DESCENDING:
    RUN SetDbAttr("HighestIndexId":U, tt_Index.uIdx-Num).
    LEAVE.
  END.

/* TableRangeSize, MinTableStatId, MaxTableStatId: */
  FOR LAST DICTDB._TableStat NO-LOCK:
    RUN SetDbAttr("TableRangeSize":U, RECID(DICTDB._TableStat)).
    RUN SetDbAttr("MaxTableStatId":U, DICTDB._TableStat._TableStat-Id).
    RUN SetDbAttr("MinTableStatId":U, DICTDB._TableStat._TableStat-Id
                      - INTEGER(RECID(DICTDB._TableStat)) + 1).
  END.

/* IndexRangeSize, MinIndexStatId, MaxIndexStatId: */
  FOR LAST DICTDB._IndexStat NO-LOCK:
    RUN SetDbAttr("IndexRangeSize":U, RECID(DICTDB._IndexStat)).
    RUN SetDbAttr("MaxIndexStatId":U, DICTDB._IndexStat._IndexStat-Id).
    RUN SetDbAttr("MinIndexStatId":U, DICTDB._IndexStat._IndexStat-Id
                      - INTEGER(RECID(DICTDB._IndexStat)) + 1).
  END.

  ASSIGN vStatCount = vStatCount + 4.

END PROCEDURE. /* ReadSchema */


/* -------------------------------------------------------------------- */
PROCEDURE IndexLevels.
  DEFINE INPUT  PARAMETER ipTableName AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipIndexName AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opIndexLevels AS INTEGER NO-UNDO.

  DEFINE VARIABLE vDbAccess LIKE _UserIO._UserIO-DbAccess NO-UNDO.
  DEFINE VARIABLE hQuery  AS HANDLE NO-UNDO. /*Query handle*/
  DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO. /*buffer handle*/

  CREATE BUFFER hBuffer FOR TABLE ipTableName.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH " + ipTableName +
                       " NO-LOCK NO-PREFETCH USE-INDEX " + ipIndexName)
  NO-ERROR.
  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  DO:
    ASSIGN opIndexLevels = ?.
    DELETE OBJECT hQuery  NO-ERROR.
    DELETE OBJECT hBuffer NO-ERROR.
    RETURN.
  END.
  hQuery:QUERY-OPEN().
  hQuery:GET-NEXT().
/*Ignore statistics for first read due to a possible read of template record.*/

/* Db access before record read: */
  FOR FIRST DICTDB._UserIO NO-LOCK
      WHERE DICTDB._UserIO._UserIO-Id EQ vMyUserNumber + 1:
    ASSIGN vDbAccess = _UserIO._UserIO-DbAccess.
  END.

  hQuery:GET-LAST().

/* Db access after record read: */
  FOR FIRST DICTDB._UserIO NO-LOCK
      WHERE DICTDB._UserIO._UserIO-Id EQ vMyUserNumber + 1:
    ASSIGN opIndexLevels = DICTDB._UserIO._UserIO-DbAccess - vDbAccess
                         - (IF hQuery:QUERY-OFF-END THEN 0 ELSE 1).
  END.

  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.

END PROCEDURE. /* IndexLevels */


/* -------------------------------------------------------------------- */
PROCEDURE CreateSelectedUser.
  DEFINE INPUT PARAMETER ipUserNumber LIKE _Connect._Connect-Usr NO-UNDO.
  DEFINE INPUT PARAMETER ipUserDesc AS CHARACTER NO-UNDO.

  IF ipUserNumber EQ ?
  OR ipUserNumber EQ {&ALL}
  OR NOT CAN-FIND(ttUserStat
            WHERE ttUserStat.SnapshotID EQ vCurrSnapshotID
              AND ttUserStat.UserNumber EQ ipUserNumber
              AND CAN-DO(vSelectableTypes, TRIM(ttUserStat.uConnect-Type))) THEN
  RETURN.

  FIND FIRST ttSelectedUser
      WHERE ttSelectedUser.UserNumber EQ ipUserNumber
  NO-ERROR.

  IF AVAILABLE ttSelectedUser THEN
  ASSIGN ttSelectedUser.UserDesc =
         ttSelectedUser.UserDesc + ",":U + ipUserDesc WHEN NOT
  CAN-DO(ttSelectedUser.UserDesc, ipUserDesc).
  ELSE
  DO TRANSACTION:

    CREATE ttSelectedUser.
    ASSIGN ttSelectedUser.UserNumber = ipUserNumber
           ttSelectedUser.UserDesc   = ipUserDesc
    . /* ASSIGN */

&IF DEFINED(Version_GE_101C)
&THEN
    IF NOT vReadOnlyConnect THEN
    FOR FIRST DICTDB._Connect /*for update*/
        WHERE DICTDB._Connect._Connect-Id EQ ipUserNumber + 1:

      ASSIGN ttSelectedUser.JustSelected = TRUE
             ttSelectedUser.CachingType  = /* Save the current value */
             DICTDB._Connect._Connect-CachingType
             DICTDB._Connect._Connect-CachingType = 3
      . /* ASSIGN */
/* _Connect-CachingType = 3:
   One Time SQL Statement or Partial ABL Program.
   Next user's query will populate _Connect-Cache* fields.
   It will be nearest query/stack from now.
   Later ( in GetSelectedUserCache) _Connect-CachingType will be reset to 2.

   _Connect-CachingType = 2:
   SQL Statement or Partial ABL Program. A latest query/stack before snapshot.
*/
    END. /* FOR FIRST _Connect */
&ENDIF
  END. /* DO TRANSACTION IF NOT AVAILABLE ttSelectedUser */
END PROCEDURE. /* CreateSelectedUser */


/* -------------------------------------------------------------------- */
PROCEDURE GetSelectedUserCache.

/* Get the _Connect-Cache* info for recently selected users.
   CreateSelectedUser sets _Connect-CachingType = 3.
   GetSelectedUserCache re-sets _Connect-CachingType = 2.

1 - SQL Statement or Single ABL Program Name
2 - SQL Statement or Partial ABL Program Stack
3 - One Time SQL Statement or Partial ABL Program
*/
&IF DEFINED(Version_GE_101C)
&THEN
  DEFINE VARIABLE vDelay      AS INTEGER NO-UNDO INITIAL 1.
  DEFINE VARIABLE vCacheLevel AS INTEGER NO-UNDO.
  DEFINE VARIABLE vMaxLevels  AS INTEGER NO-UNDO.

  IF vReadOnlyConnect
  OR NOT CAN-FIND(FIRST ttSelectedUser
                  WHERE ttSelectedUser.JustSelected EQ TRUE) THEN
  RETURN.

/* Wait a second to allow newly selected users to leave a cache info: */
  PAUSE vDelay NO-MESSAGE.

  ASSIGN vMaxLevels = EXTENT(ttSelectedUser.CacheInfo).
  FOR EACH ttSelectedUser
     WHERE ttSelectedUser.JustSelected EQ TRUE,

     FIRST DICTDB._Connect /*for update*/
     WHERE DICTDB._Connect._Connect-Id EQ ttSelectedUser.UserNumber + 1
     TRANSACTION:

/* Copy current CacheInfo: */
    DO vCacheLevel = 1 TO vMaxLevels
      WHILE DICTDB._Connect._Connect-CacheInfo[vCacheLevel] NE ?:

      ASSIGN      ttSelectedUser.CacheLineNumber[vCacheLevel] =
        DICTDB._Connect._Connect-CacheLineNumber[vCacheLevel]
                  ttSelectedUser.CacheInfo[vCacheLevel] =
        DICTDB._Connect._Connect-CacheInfo[vCacheLevel]
                  ttSelectedUser.CacheLevels = vCacheLevel
      . /* ASSIGN */
    END.

    ASSIGN
      vStatCount                     = vStatCount + 1
      ttSelectedUser.CacheLastUpdate = DICTDB._Connect._Connect-CacheLastUpdate
      ttSelectedUser.CacheInfoType   = DICTDB._Connect._Connect-CacheInfoType
      ttSelectedUser.JustSelected    = FALSE

/* Activate Client Database-Request Statement for a Selected User: */
      DICTDB._Connect._Connect-CachingType = 2
    . /* ASSIGN */
  END. /* FOR EACH ttSelectedUser*/
&ENDIF
END PROCEDURE. /* GetSelectedUserCache */


/* -------------------------------------------------------------------- */
PROCEDURE DisableUserCache.
/* Restore the original _Connect._Connect-CachingType. */

&IF DEFINED(Version_GE_101C)
&THEN
  IF NOT vReadOnlyConnect THEN
  FOR EACH ttSelectedUser NO-LOCK,

     FIRST DICTDB._Connect /*for update*/
     WHERE DICTDB._Connect._Connect-Id EQ ttSelectedUser.UserNumber + 1:

    ASSIGN DICTDB._Connect._Connect-CachingType =
             IF ttSelectedUser.CachingType EQ ? THEN 0 ELSE
                ttSelectedUser.CachingType /* initial value */
      . /* ASSIGN */
  END.
&ENDIF
END PROCEDURE. /* DisableUserCache */


/* -------------------------------------------------------------------- */
PROCEDURE PutUserCache.

&IF DEFINED(Version_GE_101C)
&THEN
  DEFINE VARIABLE vCacheLevel AS INTEGER NO-UNDO.

  IF NOT CAN-FIND(FIRST ttSelectedUser
                  WHERE ttSelectedUser.CacheLevels GT 0) THEN
  RETURN.

  OUTPUT STREAM UserCache TO VALUE(vUserCacheFile) APPEND.

  IF SEEK(UserCache) EQ 0 THEN
  PUT STREAM UserCache UNFORMATTED
           "Date"
    {&Sep} "Time"
    {&Sep} "LastUpdate"
    {&Sep} "Usr"
    {&Sep} "UserName"
    {&Sep} "PID"
    {&Sep} "Type"
    {&Sep} "Level"
    {&Sep} "Line"
    {&Sep} "Stack"
    {&Sep} "Notes"
  SKIP.

  FOR EACH ttSelectedUser NO-LOCK,

     FIRST ttUserStat NO-LOCK
     WHERE ttUserStat.SnapshotID EQ vCurrSnapshotID
       AND ttUserStat.UserNumber EQ ttSelectedUser.UserNumber:

    ASSIGN vStatCount = vStatCount + 1.

    REPEAT vCacheLevel = 1 TO ttSelectedUser.CacheLevels:
      PUT STREAM UserCache UNFORMATTED
               /*Date      */ vCurrDate
        {&Sep} /*Time      */ STRING(vCurrTime, "HH:MM:SS":U)
        {&Sep} /*LastUpdate*/ ttSelectedUser.CacheLastUpdate
        {&Sep} /*UserNum   */ ttUserStat.UserNumber
        {&Sep} /*UserName  */ ttUserStat.uConnect-Name
        {&Sep} /*PID       */ ttUserStat.uConnect-Pid
        {&Sep} /*Type      */ ttUserStat.uConnect-Type
        {&Sep} /*Level     */ vCacheLevel
        {&Sep} /*Line      */ ttSelectedUser.CacheLineNumber[vCacheLevel]
        {&Sep} /*Stack     */ ttSelectedUser.CacheInfo[vCacheLevel]
        {&Sep} /*Notes     */ IF ttUserStat.UserNumber EQ vMyUserNumber THEN
                              "My Connection" ELSE ttSelectedUser.UserDesc
      SKIP.
    END. /* REPEAT vCacheLevel = */
  END. /* FOR EACH ttSelectedUser */

  OUTPUT STREAM UserCache CLOSE.
&ENDIF
END PROCEDURE. /* PutUserCache */


/* ------------------------------------------------------------------------- */
PROCEDURE GetTableStat.

  DEFINE VARIABLE vHighestTableId AS INTEGER NO-UNDO.

  ASSIGN vHighestTableId = GetDbAttr("HighestTableId":U).

/* All Users: */
  FOR FIRST DICTDB._ActRecord  NO-LOCK,
      FIRST DICTDB._ActIndex   NO-LOCK,
      FIRST DICTDB._ActIOType  NO-LOCK
  TRANSACTION:

    FIND FIRST ttTableStat /* INDEX TableStatIdx */
         WHERE ttTableStat.SnapshotID  EQ vCurrSnapshotID
           AND ttTableStat.UserNumber  EQ {&ALL}
           AND ttTableStat.TableNumber EQ {&ALL}
    NO-ERROR.

    IF NOT AVAILABLE ttTableStat THEN
    CREATE ttTableStat.
    ASSIGN ttTableStat.SnapshotID  = vCurrSnapshotID
           ttTableStat.UserNumber  = {&ALL}
           ttTableStat.TableNumber = {&ALL}
           ttTableStat.TableRead   = DICTDB._ActRecord._Record-RecRead
           ttTableStat.TableUpdate = DICTDB._ActRecord._Record-RecUpd
           ttTableStat.TableCreate = DICTDB._ActRecord._Record-RecCreat
           ttTableStat.TableDelete = DICTDB._ActRecord._Record-RecDel
           ttTableStat.TableOsRead = DICTDB._ActIOType._IOType-DataReads
           ttTableStat.StatDesc    = "Total per db"
           vStatCount = vStatCount + 1
    . /* ASSIGN */

    FIND FIRST ttIndexStat  /* INDEX IndexStatIdx */
         WHERE ttIndexStat.SnapshotID  EQ vCurrSnapshotID
           AND ttIndexStat.UserNumber  EQ {&ALL}
           AND ttIndexStat.IndexNumber EQ {&ALL}
    NO-ERROR.

    IF NOT AVAILABLE ttIndexStat THEN
    CREATE ttIndexStat.
    ASSIGN ttIndexStat.SnapshotID  = vCurrSnapshotID
           ttIndexStat.UserNumber  = {&ALL}
           ttIndexStat.IndexNumber = {&ALL}
           ttIndexStat.IndexRead   = DICTDB._ActIndex._Index-Find
           ttIndexStat.IndexCreate = DICTDB._ActIndex._Index-Create
           ttIndexStat.IndexDelete = DICTDB._ActIndex._Index-Delete
           ttIndexStat.IndexOsRead = DICTDB._ActIOType._IOType-IdxRds
           ttIndexStat.StatDesc    = "Total per db"
           vStatCount = vStatCount + 1
    . /* ASSIGN */

    FOR FIRST ttAreaIOStat EXCLUSIVE
        WHERE ttAreaIOStat.SnapshotID EQ vCurrSnapshotID
          AND ttAreaIOStat.AreaNumber EQ {&ALL}:

       ASSIGN ttAreaIOStat.TableRead   = ttTableStat.TableRead
              ttAreaIOStat.TableUpdate = ttTableStat.TableUpdate
              ttAreaIOStat.TableCreate = ttTableStat.TableCreate
              ttAreaIOStat.TableDelete = ttTableStat.TableDelete
              ttAreaIOStat.IndexRead   = ttIndexStat.IndexRead
              ttAreaIOStat.IndexCreate = ttIndexStat.IndexCreate
              ttAreaIOStat.IndexDelete = ttIndexStat.IndexDelete
       . /* ASSIGN */
    END. /* FOR FIRST ttAreaIOStat */

/* All Tables: */
    FOR FIRST ttAreaIOStat EXCLUSIVE
        WHERE ttAreaIOStat.SnapshotID EQ vCurrSnapshotID
          AND ttAreaIOStat.AreaNumber EQ {&AllTables}:

       ASSIGN ttAreaIOStat.TableRead   = ttTableStat.TableRead
              ttAreaIOStat.TableUpdate = ttTableStat.TableUpdate
              ttAreaIOStat.TableCreate = ttTableStat.TableCreate
              ttAreaIOStat.TableDelete = ttTableStat.TableDelete
       . /* ASSIGN */
    END. /* FOR FIRST ttAreaIOStat */

/* All Indexes: */
    FOR FIRST ttAreaIOStat EXCLUSIVE
        WHERE ttAreaIOStat.SnapshotID EQ vCurrSnapshotID
          AND ttAreaIOStat.AreaNumber EQ {&AllIndexes}:

       ASSIGN ttAreaIOStat.IndexRead   = ttIndexStat.IndexRead
              ttAreaIOStat.IndexCreate = ttIndexStat.IndexCreate
              ttAreaIOStat.IndexDelete = ttIndexStat.IndexDelete
       . /* ASSIGN */
    END. /* FOR FIRST ttAreaIOStat */
  END. /* FOR FIRST _ActRecord, _ActIndex, _ActIOType */

/* _TableStat: ------------------------------------------------------------- */

  FOR EACH DICTDB._TableStat NO-LOCK /* INDEX _TableStat: _TableStat-d */
     WHERE DICTDB._TableStat._TableStat-read   NE 0
        OR DICTDB._TableStat._TableStat-update NE 0
        OR DICTDB._TableStat._TableStat-create NE 0
        OR DICTDB._TableStat._TableStat-delete NE 0
     WHILE DICTDB._TableStat._TableStat-id     LE vHighestTableId:

    FIND FIRST ttTableStat /* INDEX TableStatIdx */
      WHERE ttTableStat.SnapshotID  EQ vCurrSnapshotID
        AND ttTableStat.UserNumber  EQ {&ALL}
        AND ttTableStat.TableNumber EQ DICTDB._TableStat._TableStat-id
    NO-ERROR.

    IF NOT AVAILABLE ttTableStat THEN
    CREATE ttTableStat.
    ASSIGN ttTableStat.SnapshotID  = vCurrSnapshotID
           ttTableStat.UserNumber  = {&ALL}
           ttTableStat.TableNumber = DICTDB._TableStat._TableStat-id
           ttTableStat.TableRead   = DICTDB._TableStat._TableStat-read
           ttTableStat.TableUpdate = DICTDB._TableStat._TableStat-update
           ttTableStat.TableCreate = DICTDB._TableStat._TableStat-create
           ttTableStat.TableDelete = DICTDB._TableStat._TableStat-delete
           ttTableStat.StatDesc    = "Total per table"
           vStatCount = vStatCount + 1
&IF DEFINED(Version_GE_102B)
&THEN
           ttTableStat.TableOsRead = DICTDB._TableStat._TableStat-OsRead
&ENDIF
    . /* ASSIGN */

    FOR FIRST tt_File NO-LOCK
        WHERE tt_File.uFile-Number EQ ttTableStat.TableNumber:

      FOR FIRST ttAreaIOStat EXCLUSIVE
          WHERE ttAreaIOStat.SnapshotID EQ vCurrSnapshotID
            AND ttAreaIOStat.AreaNumber EQ tt_File.AreaNumber:

         ASSIGN ttAreaIOStat.TableRead   = ttAreaIOStat.TableRead
                                         +  ttTableStat.TableRead
                ttAreaIOStat.TableUpdate = ttAreaIOStat.TableUpdate
                                         +  ttTableStat.TableUpdate
                ttAreaIOStat.TableCreate = ttAreaIOStat.TableCreate
                                         +  ttTableStat.TableCreate
                ttAreaIOStat.TableDelete = ttAreaIOStat.TableDelete
                                         +  ttTableStat.TableDelete
         . /* ASSIGN */
      END. /* FOR FIRST ttAreaIOStat */

      IF tt_File.InB2 THEN
      FOR FIRST ttAreaIOStat EXCLUSIVE
          WHERE ttAreaIOStat.SnapshotID EQ vCurrSnapshotID
            AND ttAreaIOStat.AreaNumber EQ {&AllB2Objects}:

         ASSIGN ttAreaIOStat.TableRead   = ttAreaIOStat.TableRead
                                         +  ttTableStat.TableRead
                ttAreaIOStat.TableUpdate = ttAreaIOStat.TableUpdate
                                         +  ttTableStat.TableUpdate
                ttAreaIOStat.TableCreate = ttAreaIOStat.TableCreate
                                         +  ttTableStat.TableCreate
                ttAreaIOStat.TableDelete = ttAreaIOStat.TableDelete
                                         +  ttTableStat.TableDelete
         . /* ASSIGN */
      END. /* FOR FIRST ttAreaIOStat {&AllB2Objects} */
    END. /* FOR FIRST tt_File */
  END. /* FOR EACH _TableStat */
END PROCEDURE. /* GetTableStat */


/* ------------------------------------------------------------------------- */
PROCEDURE GetIndexStat.

  DEFINE VARIABLE vHighestIndexId AS INTEGER NO-UNDO.
  ASSIGN vHighestIndexId = GetDbAttr("HighestIndexId":U).

  FOR EACH DICTDB._IndexStat NO-LOCK /* INDEX _IndexStat: _IndexStat-id */
     WHERE DICTDB._IndexStat._IndexStat-read   NE 0
        OR DICTDB._IndexStat._IndexStat-create NE 0
        OR DICTDB._IndexStat._IndexStat-delete NE 0
     WHILE DICTDB._IndexStat._IndexStat-id     LE vHighestIndexId:

    FIND FIRST ttIndexStat /* INDEX IndexStatIdx */
      WHERE ttIndexStat.SnapshotID  EQ vCurrSnapshotID
        AND ttIndexStat.UserNumber  EQ {&ALL}
        AND ttIndexStat.IndexNumber EQ DICTDB._IndexStat._IndexStat-id
    NO-ERROR.

    IF NOT AVAILABLE ttIndexStat THEN
    CREATE ttIndexStat.
    ASSIGN ttIndexStat.SnapshotID  = vCurrSnapshotID
           ttIndexStat.UserNumber  = {&ALL}
           ttIndexStat.IndexNumber = DICTDB._IndexStat._IndexStat-id
           ttIndexStat.IndexRead   = DICTDB._IndexStat._IndexStat-read
           ttIndexStat.IndexCreate = DICTDB._IndexStat._IndexStat-create
           ttIndexStat.IndexDelete = DICTDB._IndexStat._IndexStat-delete
           ttIndexStat.StatDesc    = "Total per index"
           vStatCount              = vStatCount + 1
&IF DEFINED(Version_GE_102B)
&THEN
           ttIndexStat.IndexOsRead = DICTDB._IndexStat._IndexStat-OsRead
&ENDIF
    . /* ASSIGN */

    FOR FIRST tt_Index NO-LOCK
        WHERE tt_Index.uIdx-Num EQ ttIndexStat.IndexNumber:

      FOR FIRST ttAreaIOStat EXCLUSIVE
          WHERE ttAreaIOStat.SnapshotID EQ vCurrSnapshotID
            AND ttAreaIOStat.AreaNumber EQ tt_Index.AreaNumber:

         ASSIGN ttAreaIOStat.IndexRead   = ttAreaIOStat.IndexRead
                                         +  ttIndexStat.IndexRead
                ttAreaIOStat.IndexCreate = ttAreaIOStat.IndexCreate
                                         +  ttIndexStat.IndexCreate
                ttAreaIOStat.IndexDelete = ttAreaIOStat.IndexDelete
                                         +  ttIndexStat.IndexDelete
         . /* ASSIGN */
      END. /* FOR FIRST ttAreaIOStat */

      IF tt_Index.InB2 THEN
      FOR FIRST ttAreaIOStat EXCLUSIVE
          WHERE ttAreaIOStat.SnapshotID EQ vCurrSnapshotID
            AND ttAreaIOStat.AreaNumber EQ {&AllB2Objects}:

         ASSIGN ttAreaIOStat.IndexRead   = ttAreaIOStat.IndexRead
                                         +  ttIndexStat.IndexRead
                ttAreaIOStat.IndexCreate = ttAreaIOStat.IndexCreate
                                         +  ttIndexStat.IndexCreate
                ttAreaIOStat.IndexDelete = ttAreaIOStat.IndexDelete
                                         +  ttIndexStat.IndexDelete
         . /* ASSIGN */
      END. /* FOR FIRST ttAreaIOStat */
    END. /* FOR FIRST tt_Index */
  END. /* FOR EACH _IndexStat */
END PROCEDURE. /* GetIndexStat */


/* ------------------------------------------------------------------------- */
PROCEDURE GetUserStat.

  DEFINE VARIABLE vCacheLevel AS INTEGER NO-UNDO.
  DEFINE VARIABLE vMaxLevels  AS INTEGER NO-UNDO.

/* All Users: */
  FOR FIRST DICTDB._Connect    NO-LOCK /* INDEX _Connect-Id */
      WHERE DICTDB._Connect._Connect-Usr EQ 0, /* = Database Broker */
      FIRST DICTDB._ActSummary NO-LOCK,
      FIRST DICTDB._MstrBlk    NO-LOCK,
      FIRST DICTDB._Checkpoint NO-LOCK:

    FIND FIRST ttUserStat /* INDEX UserStatIdx */
         WHERE ttUserStat.SnapshotID EQ vCurrSnapshotID
           AND ttUserStat.UserNumber EQ {&ALL}
    NO-ERROR.

    IF NOT AVAILABLE ttUserStat THEN
    CREATE ttUserStat.
    ASSIGN ttUserStat.SnapshotID       = vCurrSnapshotID
           ttUserStat.UserNumber       = {&ALL}
           ttUserStat.uConnect-Time    = DICTDB._Connect._Connect-Time
           ttUserStat.uConnect-Device  = DICTDB._Connect._Connect-Device
           ttUserStat.uConnect-Pid     = DICTDB._Connect._Connect-Pid
           ttUserStat.uConnect-Name    = "All Users"
           ttUserStat.uConnect-Type    = "Total per db"
           ttUserStat.uConnect-Batch   = DICTDB._Connect._Connect-Batch
           ttUserStat.uConnect-Server  = DICTDB._Connect._Connect-Server
           ttUserStat.uConnect-Wait1   = DICTDB._Connect._Connect-Wait1
           ttUserStat.uConnect-Wait    = DICTDB._Connect._Connect-Wait
           ttUserStat.uConnect-TransId = DICTDB._MstrBlk._MstrBlk-lasttask
           ttUserStat.uTrans-State     = "lasttask"
           ttUserStat.uTrans-counter   = 0
           ttUserStat.uTrans-txtime    = DICTDB._Checkpoint._Checkpoint-Time
           ttUserStat.uTrans-Duration  = DICTDB._ActSummary._Summary-Chkpts
           ttUserStat.uConnect-Disconnect = DICTDB._Connect._Connect-Disconnect
           ttUserStat.uConnect-Resync  = DICTDB._Connect._Connect-Resync
           ttUserStat.uUserIO-DbAccess = DICTDB._ActSummary._Summary-dbaccesses
           ttUserStat.uUserIO-DbRead   = DICTDB._ActSummary._Summary-DbReads
           ttUserStat.uUserIO-BiRead   = DICTDB._ActSummary._Summary-BiReads
           ttUserStat.uLockReq-RecLock = DICTDB._ActSummary._Summary-RecLock
           ttUserStat.uLockReq-RecWait = DICTDB._ActSummary._Summary-RecWait
           vStatCount = vStatCount + 1
    . /* ASSIGN */

/* Note that sometimes the _Dbstatus record might be not available (bug).
   So just in case a separate ASSIGN statement is used:
*/  FOR FIRST DICTDB._DbStatus NO-LOCK:
      ASSIGN ttUserStat.LockCount = DICTDB._DbStatus._DbStatus-NumLocks
             ttUserStat.LockList  = "Total per db".
    END. /* FOR FIRST _DbStatus */
  END. /* FOR FIRST _Connect, _ActSummary NO-LOCK, _MstrBlk */

/* _Connect: */
  FOR EACH DICTDB._Connect NO-LOCK /* INDEX _Connect-Id */
     WHERE DICTDB._Connect._Connect-Usr GT {&ALL}
  TRANSACTION:

    IF vSelectedUsers NE ""
    AND (CAN-DO(vSelectedUsers, STRING(DICTDB._Connect._Connect-Usr))
     OR  CAN-DO(vSelectedUsers, STRING(DICTDB._Connect._Connect-Name))) THEN
    RUN CreateSelectedUser(DICTDB._Connect._Connect-Usr, "Pre-selected":U).

    FIND FIRST ttUserStat /* INDEX UserStatIdx */
         WHERE ttUserStat.SnapshotID EQ vCurrSnapshotID
           AND ttUserStat.UserNumber EQ DICTDB._Connect._Connect-Usr
    NO-ERROR.

    IF NOT AVAILABLE ttUserStat THEN
    CREATE ttUserStat.
    ASSIGN ttUserStat.SnapshotCount       = vSnapshotCount
           ttUserStat.SnapshotID          = vCurrSnapshotID
           ttUserStat.UserNumber          = DICTDB._Connect._Connect-Usr
           ttUserStat.uConnect-Time       = DICTDB._Connect._Connect-Time
           ttUserStat.uConnect-Device     = DICTDB._Connect._Connect-Device
           ttUserStat.uConnect-Pid        = DICTDB._Connect._Connect-Pid
           ttUserStat.uConnect-Name       = DICTDB._Connect._Connect-Name
           ttUserStat.uConnect-Type       = DICTDB._Connect._Connect-Type
           ttUserStat.uConnect-Batch      = DICTDB._Connect._Connect-Batch
           ttUserStat.uConnect-Server     = DICTDB._Connect._Connect-Server
           ttUserStat.uConnect-Wait1      = DICTDB._Connect._Connect-Wait1
           ttUserStat.uConnect-Wait       = DICTDB._Connect._Connect-Wait
           ttUserStat.uConnect-TransId    = DICTDB._Connect._Connect-TransId
           ttUserStat.uConnect-Disconnect = DICTDB._Connect._Connect-Disconnect
           ttUserStat.uConnect-Resync     = DICTDB._Connect._Connect-Resync
           vStatCount = vStatCount + 1
    . /* ASSIGN */

&IF DEFINED(Version_GE_101C)
&THEN
    FOR FIRST ttSelectedUser /* INDEX INDEX UserNumber */
        WHERE ttSelectedUser.UserNumber EQ DICTDB._Connect._Connect-Usr:

/* Copy current CacheInfo: */
      ASSIGN
        ttSelectedUser.CacheLastUpdate = DICTDB._Connect._Connect-CacheLastUpdate
        ttSelectedUser.CacheInfoType   = DICTDB._Connect._Connect-CacheInfoType
        vMaxLevels = EXTENT(ttSelectedUser.CacheInfo)
      . /* ASSIGN */

      DO vCacheLevel = 1 TO vMaxLevels
        WHILE DICTDB._Connect._Connect-CacheInfo[vCacheLevel] NE ?:

        ASSIGN      ttSelectedUser.CacheLineNumber[vCacheLevel] =
          DICTDB._Connect._Connect-CacheLineNumber[vCacheLevel]
                    ttSelectedUser.CacheInfo[vCacheLevel] =
          DICTDB._Connect._Connect-CacheInfo[vCacheLevel]
                    ttSelectedUser.CacheLevels = vCacheLevel
        . /* ASSIGN */
      END. /* DO vCacheLevel = */
    END. /* FOR FIRST ttSelectedUser */
&ENDIF

    IF ttUserStat.uConnect-Disconnect NE 0
    OR ttUserStat.uConnect-Resync     NE 0 THEN
    RUN CreateSelectedUser(ttUserStat.UserNumber, "Rollback":U).
  END. /* FOR EACH _Connect */
END PROCEDURE. /* GetUserStat */


/* ------------------------------------------------------------------------- */
PROCEDURE GetUserIO.

  FOR EACH DICTDB._UserIO NO-LOCK /* INDEX _UserIO-Id */
     WHERE DICTDB._UserIO._UserIO-Usr GT {&ALL},

     FIRST ttUserStat /* INDEX UserStatIdx */
     WHERE ttUserStat.SnapshotID EQ vCurrSnapshotID
       AND ttUserStat.UserNumber EQ DICTDB._UserIO._UserIO-Usr
  TRANSACTION:
    ASSIGN ttUserStat.uUserIO-DbAccess = DICTDB._UserIO._UserIO-DbAccess
           ttUserStat.uUserIO-DbRead   = DICTDB._UserIO._UserIO-DbRead
           ttUserStat.uUserIO-BiRead   = DICTDB._UserIO._UserIO-BiRead
           vStatCount = vStatCount + 1
    . /* ASSIGN */
  END. /* FOR EACH _UserIO */
END PROCEDURE. /* GetUserIO */


/* ------------------------------------------------------------------------- */
PROCEDURE GetSeqStat.

  DEFINE VARIABLE vSeqCurrValue LIKE _Sequence._Seq-Init NO-UNDO.

  FOR FIRST DICTDB._Latch NO-LOCK /* INDEX _Latch-Id */
      WHERE DICTDB._Latch._Latch-Name MATCHES "*SEQ":U
  TRANSACTION:

    FIND FIRST ttSequenceStat /* INDEX SequenceStatIdx: SnapshotID + uSeq-Num*/
         WHERE ttSequenceStat.SnapshotID EQ vCurrSnapshotID
           AND ttSequenceStat.uSeq-Num   EQ {&ALL}
    NO-ERROR.

    IF NOT AVAILABLE ttSequenceStat THEN
    CREATE ttSequenceStat.
    ASSIGN ttSequenceStat.SnapshotID = vCurrSnapshotID
           ttSequenceStat.uSeq-Num   = {&ALL}
           ttSequenceStat.CurrValue  = DICTDB._Latch._Latch-Lock.
  END. /* FOR FIRST _Latch */

  FOR EACH tt_Sequence NO-LOCK: /* INDEX uSeq-Num */

    ASSIGN
      vSeqCurrValue = DYNAMIC-CURRENT-VALUE(tt_Sequence.uSeq-Name, "DICTDB":U).

    DO TRANSACTION:
      FIND FIRST ttSequenceStat /* INDEX SequenceStatIdx */
           WHERE ttSequenceStat.SnapshotID EQ vCurrSnapshotID
             AND ttSequenceStat.uSeq-Num   EQ tt_Sequence.uSeq-Num
      NO-ERROR.

      IF NOT AVAILABLE ttSequenceStat THEN
      CREATE ttSequenceStat.
      ASSIGN ttSequenceStat.SnapshotID = vCurrSnapshotID
             ttSequenceStat.uSeq-Num   = tt_Sequence.uSeq-Num
             ttSequenceStat.CurrValue  = vSeqCurrValue
             vStatCount = vStatCount + 1
      . /* ASSIGN */
    END. /* DO TRANSACTION */
  END. /* FOR EACH tt_Sequence */
END PROCEDURE. /* GetSeqStat */


/* ------------------------------------------------------------------------- */
PROCEDURE GetTranStat.

  DEFINE VARIABLE vRecentTransTime  LIKE _Trans._Trans-txtime  NO-UNDO.
  DEFINE VARIABLE vRecentRLCounter  LIKE _Trans._Trans-counter NO-UNDO.
  DEFINE VARIABLE vCurrentRLCounter LIKE _Trans._Trans-counter NO-UNDO.

/* Set "NumOfBiClusters": */
  FOR FIRST DICTDB._MstrBlk    NO-LOCK,

      FIRST DICTDB._AreaStatus NO-LOCK /* INDEX _AreaStatus-Id */
      WHERE DICTDB._AreaStatus._AreaStatus-Areanum EQ 3:

     RUN SetDbAttr("NumOfBiClusters":U,
                    DICTDB._AreaStatus._AreaStatus-Hiwater
                 * (DICTDB._MstrBlk._MstrBlk-biblksize / 1024)
                 / (DICTDB._MstrBlk._MstrBlk-rlclsize * 16)).
  END. /* FOR FIRST _MstrBlk, _AreaStatus */

/* The rest of statistics has a sense only for a transaction database: */

/* _Trans: */

  ASSIGN vRecentRLCounter = ?
         vRecentTransTime = ?
  . /* ASSIGN */

  FOR EACH DICTDB._Trans NO-LOCK /* INDEX _Trans-Id */
     WHERE DICTDB._Trans._Trans-Num NE ?,

     FIRST ttUserStat /* INDEX UserStatIdx */
     WHERE ttUserStat.SnapshotID EQ vCurrSnapshotID
       AND ttUserStat.UserNumber EQ DICTDB._Trans._Trans-Usrnum
  TRANSACTION:

    ASSIGN ttUserStat.uConnect-TransId = DICTDB._Trans._Trans-Num
           ttUserStat.uTrans-State     = DICTDB._Trans._Trans-State
           ttUserStat.uTrans-counter   = DICTDB._Trans._Trans-counter
           ttUserStat.uTrans-txtime    = DICTDB._Trans._Trans-txtime
           ttUserStat.uTrans-Duration  = DICTDB._Trans._Trans-Duration
           vStatCount = vStatCount + 1
    . /* ASSIGN */

/* vRecentRLCounter = max of _Trans-counter */
    IF vRecentRLCounter EQ ?
    OR vRecentRLCounter LT ttUserStat.uTrans-counter THEN
    ASSIGN vRecentRLCounter = ttUserStat.uTrans-counter
           vRecentTransTime = ttUserStat.uTrans-txtime
    . /* ASSIGN */

    IF ttUserStat.uTrans-Duration GT vTopLongTranToss THEN
    RUN AddToTopList("LongTran":U,
                     ttUserStat.UserNumber,
                     ttUserStat.uTrans-Duration).
  END. /* FOR EACH _Trans */

/* Set misc DbAttr: */
  FOR FIRST DICTDB._ActSummary NO-LOCK:

    RUN SetDbAttr("Commits":U, DICTDB._ActSummary._Summary-Commits).

/* vCurrentRLCounter = current RL counter (_Trans-counter) */
    ASSIGN vCurrentRLCounter = DICTDB._ActSummary._Summary-Chkpts
                             + GetDbAttr("Chkpts2CounterShift":U).

    IF vCurrentRLCounter EQ ? THEN
    DO:
/* Set "Chkpts2CounterShift": */
      ASSIGN vCurrentRLCounter = CurrentRLCounter
                                (vRecentRLCounter, vRecentTransTime).
      IF vCurrentRLCounter NE ? THEN
      RUN SetDbAttr("Chkpts2CounterShift":U,
                    vCurrentRLCounter - DICTDB._ActSummary._Summary-Chkpts).
    END.

/* Set "CurrentRLCounter": */
    RUN SetDbAttr("CurrentRLCounter":U, vCurrentRLCounter).
  END. /* FOR FIRST _ActSummary */
END PROCEDURE. /* GetTranStat */


/* ------------------------------------------------------------------------- */
PROCEDURE GetLockReq.

  FOR EACH DICTDB._LockReq NO-LOCK /* INDEX _LockReq-Id */
     WHERE DICTDB._LockReq._LockReq-Num GT {&ALL},

     FIRST ttUserStat /* INDEX UserStatIdx */
     WHERE ttUserStat.SnapshotID EQ vCurrSnapshotID
       AND ttUserStat.UserNumber EQ DICTDB._LockReq._LockReq-Num
  TRANSACTION:

    ASSIGN ttUserStat.uLockReq-RecLock = DICTDB._LockReq._LockReq-RecLock
           ttUserStat.uLockReq-RecWait = DICTDB._LockReq._LockReq-RecWait
           vStatCount = vStatCount + 1
    . /* ASSIGN */
  END. /* FOR EACH _LockReq */
END PROCEDURE. /* GetLockReq */


/* ------------------------------------------------------------------------- */
PROCEDURE GetUserLock.

  DEFINE VARIABLE vMaxLockCount  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLockList      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vMaxUserLockId LIKE _UserLock._UserLock-Id NO-UNDO.

  ASSIGN vMaxLockCount  = 1
         vMaxUserLockId = ?
  . /* ASSIGN */

  IF vCheckUserLock THEN
  FOR EACH DICTDB._UserLock NO-LOCK /* INDEX _UserLock-Id */
     WHERE DICTDB._UserLock._UserLock-Usr GT {&ALL}:

MaxLockCount:
    REPEAT WHILE DICTDB._UserLock._UserLock-Recid[vMaxLockCount] NE ?:

      FOR FIRST ttUserStat /* INDEX UserStatIdx */
          WHERE ttUserStat.SnapshotID EQ vCurrSnapshotID
            AND ttUserStat.UserNumber EQ DICTDB._UserLock._UserLock-Usr:
         ASSIGN ttUserStat.LockCount = vMaxLockCount
                vMaxUserLockId       = DICTDB._UserLock._UserLock-Id.
      END.

      IF vMaxLockCount EQ 512 /* = EXTENT(_UserLock-Recid) */
      THEN LEAVE   MaxLockCount.
      ELSE ASSIGN vMaxLockCount  = vMaxLockCount + 1.
    END. /* REPEAT */
  END. /* FOR EACH _UserLock */

/* Assign ttUserStat.LockList for user with max lock count: */
  IF vMaxUserLockId NE ? THEN
  FOR FIRST DICTDB._UserLock NO-LOCK /* INDEX _UserLock-Id */
      WHERE DICTDB._UserLock._UserLock-Id EQ vMaxUserLockId:

    DO vMaxLockCount = 1 TO 512
       WHILE DICTDB._UserLock._UserLock-Recid[vMaxLockCount] NE ?
    TRANSACTION:

      ASSIGN vLockList = TRIM(DICTDB._UserLock._UserLock-Type [vMaxLockCount])
              + ":" + REPLACE(DICTDB._UserLock._UserLock-Flags[vMaxLockCount],
                              " ":U, "":U).
/* _UserLock-Table exists since V10.1B: */
&IF DEFINED(Version_GE_101B)
&THEN
      FOR FIRST tt_File NO-LOCK /* INDEX uFile-Number */
          WHERE tt_File.uFile-Number EQ
                DICTDB._UserLock._UserLock-Table[vMaxLockCount]:
        ASSIGN vLockList = vLockList + ":" + tt_File.uFile-Name.
      END. /* FOR FIRST tt_File */
&ENDIF

      FIND FIRST ttUserLock /* INDEX LockList */
           WHERE ttUserLock.LockList EQ vLockList
      NO-ERROR.

      IF NOT AVAILABLE ttUserLock THEN
      CREATE ttUserLock.
      ASSIGN ttUserLock.LockList  = vLockList
             ttUserLock.LockCount = ttUserLock.LockCount + 1.
    END.

    ASSIGN vLockList = "":U.
    FOR EACH ttUserLock NO-LOCK /* INDEX LockList */
          BY ttUserLock.LockCount DESCENDING: /* no index: one time sorting */
      ASSIGN vLockList = vLockList + ",":U
                       + STRING(ttUserLock.LockCount) + ":":U
                       + ttUserLock.LockList.
    END. /* FOR EACH ttUserLock */
    EMPTY TEMP-TABLE ttUserLock.

    FOR FIRST ttUserStat /* INDEX UserStatIdx */
        WHERE ttUserStat.SnapshotID EQ vCurrSnapshotID
          AND ttUserStat.UserNumber EQ DICTDB._UserLock._UserLock-Usr
    TRANSACTION:

      ASSIGN ttUserStat.LockList = SUBSTRING(vLockList, 2). /*cut first "," */

      IF ttUserStat.LockCount GT vTopUserLockToss THEN
      RUN CreateSelectedUser(ttUserStat.UserNumber, "UserLock":U).

    END. /* FOR FIRST ttUserStat */
  END. /* FOR FIRST DICTDB._UserLock with vMaxUserLockId */
END PROCEDURE. /* GetUserLock */


/* ------------------------------------------------------------------------- */
PROCEDURE GetSelectedUserStat.

/* Empty procedure for Progress versions before 10.1B: */
&IF DEFINED(Version_GE_101B)
&THEN
  DEFINE VARIABLE vFirstStatID    AS INTEGER NO-UNDO.
  DEFINE VARIABLE vLastStatID     AS INTEGER NO-UNDO.
  DEFINE VARIABLE vTableRangeSize AS INTEGER NO-UNDO.
  DEFINE VARIABLE vIndexRangeSize AS INTEGER NO-UNDO.
  DEFINE VARIABLE vHighestTableId AS INTEGER NO-UNDO.
  DEFINE VARIABLE vHighestIndexId AS INTEGER NO-UNDO.


  ASSIGN vTableRangeSize = GetDbAttr("TableRangeSize":U)
         vIndexRangeSize = GetDbAttr("IndexRangeSize":U)
         vHighestTableId = GetDbAttr("HighestTableId":U)
         vHighestIndexId = GetDbAttr("HighestIndexId":U)
         vStatCount     = 0
  . /* ASSIGN */

  FOR EACH ttSelectedUser:

    ASSIGN vFirstStatID = vTableRangeSize * ttSelectedUser.UserNumber
           vLastStatID  = vTableRangeSize + vFirstStatID
           vFirstStatID = vFirstStatID + 1
    . /* ASSIGN */

    FOR EACH  DICTDB._UserTableStat NO-LOCK
       WHERE  DICTDB._UserTableStat._UserTableStat-id      GE vFirstStatID
         AND  DICTDB._UserTableStat._UserTableStat-id      LE vLastStatID
         AND (DICTDB._UserTableStat._UserTableStat-Read    NE 0
           OR DICTDB._UserTableStat._UserTableStat-Update  NE 0
           OR DICTDB._UserTableStat._UserTableStat-Create  NE 0
           OR DICTDB._UserTableStat._UserTableStat-Delete  NE 0)
        WHILE DICTDB._UserTableStat._UserTableStat-Num     LE vHighestTableId:

      FIND FIRST ttTableStat
           WHERE ttTableStat.SnapshotID  EQ vCurrSnapshotID
             AND ttTableStat.UserNumber  EQ DICTDB._UserTableStat._UserTableStat-Conn
             AND ttTableStat.TableNumber EQ DICTDB._UserTableStat._UserTableStat-Num
      NO-ERROR.

      IF NOT AVAILABLE ttTableStat THEN
      CREATE ttTableStat.
      ASSIGN ttTableStat.SnapshotID  = vCurrSnapshotID
             ttTableStat.UserNumber  = DICTDB._UserTableStat._UserTableStat-Conn
             ttTableStat.TableNumber = DICTDB._UserTableStat._UserTableStat-Num
             ttTableStat.TableRead   = DICTDB._UserTableStat._UserTableStat-Read
             ttTableStat.TableUpdate = DICTDB._UserTableStat._UserTableStat-Update
             ttTableStat.TableCreate = DICTDB._UserTableStat._UserTableStat-Create
             ttTableStat.TableDelete = DICTDB._UserTableStat._UserTableStat-Delete
             ttTableStat.StatDesc    = ttSelectedUser.UserDesc
             vStatCount = vStatCount + 1
      . /* ASSIGN */
    END. /* FOR EACH _UserTableStat */


    ASSIGN vFirstStatID = vIndexRangeSize * ttSelectedUser.UserNumber
           vLastStatID  = vIndexRangeSize + vFirstStatID
           vFirstStatID = vFirstStatID + 1
    . /* ASSIGN */

    FOR EACH  DICTDB._UserIndexStat NO-LOCK
       WHERE  DICTDB._UserIndexStat._UserIndexStat-id      GE vFirstStatID
         AND  DICTDB._UserIndexStat._UserIndexStat-id      LE vLastStatID
         AND (DICTDB._UserIndexStat._UserIndexStat-Read    NE 0
           OR DICTDB._UserIndexStat._UserIndexStat-Create  NE 0
           OR DICTDB._UserIndexStat._UserIndexStat-Delete  NE 0)
        WHILE DICTDB._UserIndexStat._UserIndexStat-Num     LE vHighestIndexId:

      FIND FIRST ttIndexStat /* INDEX IndexStatIdx */
           WHERE ttIndexStat.SnapshotID  EQ vCurrSnapshotID
             AND ttIndexStat.UserNumber  EQ DICTDB._UserIndexStat._UserIndexStat-Conn
             AND ttIndexStat.IndexNumber EQ DICTDB._UserIndexStat._UserIndexStat-Num
      NO-ERROR.

      IF NOT AVAILABLE ttIndexStat THEN
      CREATE ttIndexStat.
      ASSIGN ttIndexStat.SnapshotID  = vCurrSnapshotID
             ttIndexStat.UserNumber  = DICTDB._UserIndexStat._UserIndexStat-Conn
             ttIndexStat.IndexNumber = DICTDB._UserIndexStat._UserIndexStat-Num
             ttIndexStat.IndexRead   = DICTDB._UserIndexStat._UserIndexStat-Read
             ttIndexStat.IndexCreate = DICTDB._UserIndexStat._UserIndexStat-Create
             ttIndexStat.IndexDelete = DICTDB._UserIndexStat._UserIndexStat-Delete
             ttIndexStat.StatDesc    = ttSelectedUser.UserDesc
             vStatCount = vStatCount + 1
      . /* ASSIGN */
    END. /* FOR EACH DICTDB._UserIndexStat */
  END. /* FOR EACH ttSelectedUser */
&ENDIF
END PROCEDURE. /* GetSelectedUserStat. */


/* ------------------------------------------------------------------------- */
PROCEDURE GetServerStat.

  ASSIGN vStatCount = 0.

  FOR EACH DICTDB._Servers NO-LOCK
     WHERE DICTDB._Servers._Server-Type NE "Login":U
       AND DICTDB._Servers._Server-Type NE "Inactive":U
    /* AND DICTDB._Servers._Server-Protocol NE "Default":U */
  TRANSACTION:

    FIND FIRST tt_Servers
         WHERE tt_Servers.uServer-Num EQ DICTDB._Servers._Server-Num
    NO-ERROR.
    IF NOT AVAILABLE tt_Servers THEN
    CREATE tt_Servers.
    ASSIGN tt_Servers.uServer-Num       = DICTDB._Servers._Server-Num
           tt_Servers.uServer-Id        = DICTDB._Servers._Server-Id
           tt_Servers.uServer-Pid       = DICTDB._Servers._Server-Pid
           tt_Servers.uServer-Type      = DICTDB._Servers._Server-Type
           tt_Servers.uServer-Protocol  = DICTDB._Servers._Server-Protocol
           tt_Servers.uServer-Logins    = DICTDB._Servers._Server-Logins
           tt_Servers.uServer-CurrUsers = DICTDB._Servers._Server-CurrUsers
           tt_Servers.uServer-MaxUsers  = DICTDB._Servers._Server-MaxUsers
           tt_Servers.uServer-PortNum   = DICTDB._Servers._Server-PortNum
           tt_Servers.uServer-PendConn  = DICTDB._Servers._Server-PendConn
           vStatCount = vStatCount + 1
    . /* ASSIGN */
  END. /* EACH _Servers */

  FOR EACH DICTDB._ActServer NO-LOCK WHERE CAN-FIND(
     FIRST tt_Servers
     WHERE tt_Servers.uServer-Id EQ DICTDB._ActServer._Server-Id)
  TRANSACTION:

    FIND FIRST tt_ActServer
         WHERE tt_ActServer.SnapshotID EQ vCurrSnapshotID
           AND tt_ActServer.uServer-Id EQ DICTDB._ActServer._Server-Id
    NO-ERROR.

    IF NOT AVAILABLE tt_ActServer THEN
    CREATE tt_ActServer.
    ASSIGN tt_ActServer.SnapshotID        = vCurrSnapshotID
           tt_ActServer.uServer-Id        = DICTDB._ActServer._Server-Id
           tt_ActServer.uServer-MsgRec    = DICTDB._ActServer._Server-MsgRec
           tt_ActServer.uServer-MsgSent   = DICTDB._ActServer._Server-MsgSent
           tt_ActServer.uServer-ByteRec   = DICTDB._ActServer._Server-ByteRec
           tt_ActServer.uServer-ByteSent  = DICTDB._ActServer._Server-ByteSent
           tt_ActServer.uServer-RecRec    = DICTDB._ActServer._Server-RecRec
           tt_ActServer.uServer-RecSent   = DICTDB._ActServer._Server-RecSent
           tt_ActServer.uServer-QryRec    = DICTDB._ActServer._Server-QryRec
           tt_ActServer.uServer-TimeSlice = DICTDB._ActServer._Server-TimeSlice
           vStatCount = vStatCount + 1
    . /* ASSIGN */
  END. /* EACH _ActServer */
END PROCEDURE. /* GetServerStat */


/* ------------------------------------------------------------------------- */
PROCEDURE PutServerStat.

  DEFINE BUFFER bCurrServerStat FOR tt_ActServer.
  DEFINE BUFFER bPrevServerStat FOR tt_ActServer.

  DEFINE VARIABLE vInterval  AS INTEGER NO-UNDO.
  DEFINE VARIABLE vMsgRec    AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vMsgSent   AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vByteRec   AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vByteSent  AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vRecRec    AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vRecSent   AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vQryRec    AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vTimeSlice AS DECIMAL NO-UNDO.

  OUTPUT STREAM ServerStat TO VALUE(vServerStatFile) APPEND.
  IF SEEK(ServerStat) EQ 0 THEN
  PUT STREAM ServerStat UNFORMATTED
           "Date"
    {&Sep} "Time"
    {&Sep} "Interval"
    {&Sep} "Srv"
    {&Sep} "PID"
    {&Sep} "Port"
    {&Sep} "Logins"
    {&Sep} "Curr"
    {&Sep} "Max"
    {&Sep} "Pend"
    {&Sep} "MsgRecv"
    {&Sep} "MsgSent"
    {&Sep} "ByteRecv"
    {&Sep} "ByteSent"
    {&Sep} "RecRecv"
    {&Sep} "RecSent"
    {&Sep} "QryRecv"
    {&Sep} "TimeSlice"
  SKIP.

  FOR EACH tt_Servers NO-LOCK,

      EACH bCurrServerStat NO-LOCK
     WHERE bCurrServerStat.SnapshotID EQ vCurrSnapshotID
       AND bCurrServerStat.uServer-Id EQ tt_Servers.uServer-Id:

    FIND FIRST bPrevServerStat NO-LOCK
         WHERE bPrevServerStat.SnapshotID EQ vPrevSnapshotID
           AND bPrevServerStat.uServer-Id EQ bCurrServerStat.uServer-Id
    NO-ERROR.

    IF AVAILABLE bPrevServerStat THEN
    ASSIGN vInterval  =  vSampleInterval
           vMsgRec    = (bCurrServerStat.uServer-MsgRec -
                         bPrevServerStat.uServer-MsgRec) / vInterval
           vMsgSent   = (bCurrServerStat.uServer-MsgSent -
                         bPrevServerStat.uServer-MsgSent)   / vInterval
           vByteRec   = (bCurrServerStat.uServer-ByteRec -
                         bPrevServerStat.uServer-ByteRec)   / vInterval
           vByteSent  = (bCurrServerStat.uServer-ByteSent -
                         bPrevServerStat.uServer-ByteSent) / vInterval
           vRecRec    = (bCurrServerStat.uServer-RecRec  -
                         bPrevServerStat.uServer-RecRec) / vInterval
           vRecSent   = (bCurrServerStat.uServer-RecSent  -
                         bPrevServerStat.uServer-RecSent) / vInterval
           vQryRec    = (bCurrServerStat.uServer-QryRec  -
                         bPrevServerStat.uServer-QryRec) / vInterval
           vTimeSlice = (bCurrServerStat.uServer-TimeSlice  -
                        bPrevServerStat.uServer-TimeSlice) / vInterval
    . /* ASSIGN */
    ELSE
    FOR FIRST ttUserStat NO-LOCK
        WHERE ttUserStat.SnapshotID EQ bCurrServerStat.SnapshotID
          AND ttUserStat.UserNumber EQ tt_Servers.uServer-Num:

      ASSIGN vInterval  = 86400
                        * (vCurrDate - String2Date(ttUserStat.uConnect-Time))
                        + (vCurrTime - String2Time(ttUserStat.uConnect-Time))
             vMsgRec    = bCurrServerStat.uServer-MsgRec    / vInterval
             vMsgSent   = bCurrServerStat.uServer-MsgSent   / vInterval
             vByteRec   = bCurrServerStat.uServer-ByteRec   / vInterval
             vByteSent  = bCurrServerStat.uServer-ByteSent  / vInterval
             vRecRec    = bCurrServerStat.uServer-RecRec    / vInterval
             vRecSent   = bCurrServerStat.uServer-RecSent   / vInterval
             vQryRec    = bCurrServerStat.uServer-QryRec    / vInterval
             vTimeSlice = bCurrServerStat.uServer-TimeSlice / vInterval
      . /* ASSIGN */
    END.

    ASSIGN vStatCount = vStatCount + 1.

    PUT STREAM ServerStat UNFORMATTED
             /*Date     */ vCurrDate
      {&Sep} /*Time     */ STRING(vCurrTime,  "HH:MM:SS":U)
      {&Sep} /*Interval */ vInterval
      {&Sep} /*Srv      */ tt_Servers.uServer-Num
      {&Sep} /*PID      */ IF   tt_Servers.uServer-Pid LT 0
                           THEN tt_Servers.uServer-Pid + 65536
                           ELSE tt_Servers.uServer-Pid
      {&Sep} /*Port     */ tt_Servers.uServer-PortNum
      {&Sep} /*Logins   */ tt_Servers.uServer-Logins
      {&Sep} /*Curr     */ tt_Servers.uServer-CurrUsers
      {&Sep} /*Max      */ tt_Servers.uServer-MaxUsers
      {&Sep} /*Pend     */ tt_Servers.uServer-PendConn
      {&Sep} /*MsgRecv  */ vMsgRec
      {&Sep} /*MsgSent  */ vMsgSent
      {&Sep} /*ByteRecv */ vByteRec
      {&Sep} /*ByteSent */ vByteSent
      {&Sep} /*RecRecv  */ vRecRec
      {&Sep} /*RecSent  */ vRecSent
      {&Sep} /*QryRecv  */ vQryRec
      {&Sep} /*TimeSlice*/ vTimeSlice
    SKIP.
  END. /* FOR EACH bCurrServerStat */
  OUTPUT STREAM ServerStat CLOSE.
END PROCEDURE. /* PutServerStat */


/* ------------------------------------------------------------------------- */
PROCEDURE PutSeqStat.

  DEFINE BUFFER bPrevSequenceStat FOR ttSequenceStat.
  DEFINE BUFFER bCurrSequenceStat FOR ttSequenceStat.

  DEFINE VARIABLE vSeqUpdates AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vPrevDate2  AS DATE    NO-UNDO.
  DEFINE VARIABLE vPrevTime2  AS INTEGER NO-UNDO.
  DEFINE VARIABLE vInterval   AS INTEGER NO-UNDO.

/* If database does not contain the sequences: */
  IF NOT CAN-FIND(FIRST ttSequenceStat) THEN
  RETURN.

  OUTPUT STREAM SeqStat TO VALUE(vSeqStatFile) APPEND.

  IF SEEK(SeqStat) EQ 0 THEN
  PUT STREAM SeqStat UNFORMATTED
           "BgnDate"
    {&Sep} "BgnTime"
    {&Sep} "EndTime"
    {&Sep} "Interval"
    {&Sep} "Num"
    {&Sep} "SeqName"
    {&Sep} "Updates"
  SKIP.

  IF CAN-FIND(FIRST bPrevSequenceStat
              WHERE bPrevSequenceStat.SnapshotID EQ vPrevSnapshotID) THEN
  DO: /* It's not a first snapshot: ---------------------------------------- */

/* Per sequence: ----------------------------------------------------------- */
    FOR EACH bCurrSequenceStat NO-LOCK
       WHERE bCurrSequenceStat.SnapshotID EQ vCurrSnapshotID
         AND bCurrSequenceStat.uSeq-Num   NE {&ALL},

       FIRST bPrevSequenceStat NO-LOCK
       WHERE bPrevSequenceStat.SnapshotID EQ vPrevSnapshotID
         AND bPrevSequenceStat.uSeq-Num   EQ bCurrSequenceStat.uSeq-Num,

       FIRST tt_Sequence NO-LOCK
       WHERE tt_Sequence.uSeq-Num EQ bCurrSequenceStat.uSeq-Num:

      ASSIGN vSeqUpdates = (bCurrSequenceStat.CurrValue
                         -  bPrevSequenceStat.CurrValue)
                         /  tt_Sequence.uSeq-Incr
             vStatCount = vStatCount + 1
      . /* ASSIGN */

      ACCUMULATE vSeqUpdates (TOTAL COUNT).

      IF vSeqUpdates NE 0 THEN
      PUT STREAM SeqStat UNFORMATTED
               /*BgnDate */ vPrevDate
        {&Sep} /*BgnTime */ STRING(vPrevTime, "HH:MM:SS":U)
        {&Sep} /*EndTime */ STRING(vCurrTime, "HH:MM:SS":U)
        {&Sep} /*Interval*/ vSampleInterval
        {&Sep} /*Num     */ tt_Sequence.uSeq-Num
        {&Sep} /*SeqName */ tt_Sequence.uSeq-Name
        {&Sep} /*Updates */ vSeqUpdates / vSampleInterval
      SKIP.
    END. /* FOR FIRST bCurrSequenceStat, bPrevSequenceStat, tt_Sequence */

/* Total Updates: ---------------------------------------------------------- */
    FOR FIRST bCurrSequenceStat NO-LOCK
        WHERE bCurrSequenceStat.SnapshotID EQ vCurrSnapshotID
          AND bCurrSequenceStat.uSeq-Num EQ {&ALL},

        FIRST bPrevSequenceStat NO-LOCK
        WHERE bPrevSequenceStat.SnapshotID EQ vPrevSnapshotID
          AND bPrevSequenceStat.uSeq-Num   EQ bCurrSequenceStat.uSeq-Num:

      ASSIGN vSeqUpdates = (ACCUM TOTAL vSeqUpdates).
      PUT STREAM SeqStat UNFORMATTED
               /*BgnDate */ vPrevDate
        {&Sep} /*BgnTime */ STRING(vPrevTime, "HH:MM:SS":U)
        {&Sep} /*EndTime */ STRING(vCurrTime, "HH:MM:SS":U)
        {&Sep} /*Interval*/ vSampleInterval
        {&Sep} /*Num     */ bCurrSequenceStat.uSeq-Num
        {&Sep} /*SeqName */ "Total Updates"
        {&Sep} /*Updates */ vSeqUpdates / vSampleInterval
      SKIP.

/* Total Reads: ------------------------------------------------------------ */

/* When uSeq-Num EQ {&ALL} the CurrValue is SEQ Latch Locks.
   Each sequence update requires two locks of SEQ Latch.
   The rest is the locks created for sequence reads
*/
      ASSIGN vSeqUpdates = bCurrSequenceStat.CurrValue
                         - bPrevSequenceStat.CurrValue
                         - (ACCUM COUNT vSeqUpdates) /*Program itself reads seq*/
                         - (ACCUM TOTAL vSeqUpdates) * 2
      . /* ASSIGN */

      PUT STREAM SeqStat UNFORMATTED
               /*BgnDate */ vPrevDate
        {&Sep} /*BgnTime */ STRING(vPrevTime, "HH:MM:SS":U)
        {&Sep} /*EndTime */ STRING(vCurrTime, "HH:MM:SS":U)
        {&Sep} /*Interval*/ vSampleInterval
        {&Sep} /*Num     */ bCurrSequenceStat.uSeq-Num
        {&Sep} /*SeqName */ "Total Reads"
        {&Sep} /*Updates */ MAX(0, vSeqUpdates) / vSampleInterval
      SKIP.
    END. /* FOR FIRST bCurrSequenceStat, bPrevSequenceStat */
  END. /* IF CAN-FIND(FIRST bPrevSequenceStat) */
  ELSE
  DO: /* It's a first snapshot: -------------------------------------------- */

/* Per sequance since database creation: ----------------------------------- */
    FOR FIRST DICTDB._MstrBlk NO-LOCK:
      ASSIGN vPrevDate2 = String2Date(DICTDB._MstrBlk._MstrBlk-crdate)
             vPrevTime2 = String2Time(DICTDB._MstrBlk._MstrBlk-crdate)
             vInterval  = (vCurrDate - vPrevDate2) * 86400
                        + (vCurrTime - vPrevTime2)
      . /* ASSIGN */
    END.

    FOR EACH bCurrSequenceStat NO-LOCK
       WHERE bCurrSequenceStat.SnapshotID EQ vCurrSnapshotID
         AND bCurrSequenceStat.uSeq-Num   NE {&ALL},

       FIRST tt_Sequence NO-LOCK
       WHERE tt_Sequence.uSeq-Num EQ bCurrSequenceStat.uSeq-Num:

      ASSIGN vSeqUpdates = (bCurrSequenceStat.CurrValue
                         -  tt_Sequence.uSeq-Init)
                         /  tt_Sequence.uSeq-Incr
             vStatCount = vStatCount + 1
      . /* ASSIGN */
      ACCUMULATE vSeqUpdates (TOTAL).

      PUT STREAM SeqStat UNFORMATTED
               /*BgnDate */ vPrevDate2
        {&Sep} /*BgnTime */ STRING(vPrevTime2, "HH:MM:SS":U)
        {&Sep} /*EndTime */ STRING(vCurrTime,  "HH:MM:SS":U)
        {&Sep} /*Interval*/ vInterval
        {&Sep} /*Num     */ tt_Sequence.uSeq-Num
        {&Sep} /*SeqName */ tt_Sequence.uSeq-Name
        {&Sep} /*Updates */ vSeqUpdates / vInterval
      SKIP.
    END. /* FOR FIRST bCurrSequenceStat, tt_Sequence */

/* Total Updates since database creation: ---------------------------------- */
    FOR FIRST bCurrSequenceStat NO-LOCK
        WHERE bCurrSequenceStat.SnapshotID EQ vCurrSnapshotID
          AND bCurrSequenceStat.uSeq-Num   EQ {&ALL}:

      ASSIGN vSeqUpdates = (ACCUM TOTAL vSeqUpdates).

      PUT STREAM SeqStat UNFORMATTED
               /*BgnDate */ vPrevDate2
        {&Sep} /*BgnTime */ STRING(vPrevTime2, "HH:MM:SS":U)
        {&Sep} /*EndTime */ STRING(vCurrTime, "HH:MM:SS":U)
        {&Sep} /*Interval*/ vInterval
        {&Sep} /*Num     */ bCurrSequenceStat.uSeq-Num
        {&Sep} /*SeqName */ "Total Updates"
        {&Sep} /*Updates */ vSeqUpdates / vInterval
      SKIP.

/* Total Updates since database starup: ----------------------------------
   Treat all SEQ latch locks since db startup as sequence updates.
   Each sequence update requires two locks of SEQ Latch.
*/    ASSIGN vSeqUpdates = bCurrSequenceStat.CurrValue / 2.

      PUT STREAM SeqStat UNFORMATTED
               /*BgnDate */ vPrevDate
        {&Sep} /*BgnTime */ STRING(vPrevTime, "HH:MM:SS":U)
        {&Sep} /*EndTime */ STRING(vCurrTime, "HH:MM:SS":U)
        {&Sep} /*Interval*/ vSampleInterval
        {&Sep} /*Num     */ bCurrSequenceStat.uSeq-Num
        {&Sep} /*SeqName */ "Total Updates (estim)"
        {&Sep} /*Updates */ vSeqUpdates / vSampleInterval
      SKIP.
    END. /* FOR FIRST bCurrSequenceStat */
  END. /* IF NOT CAN-FIND(FIRST bPrevSequenceStat) */

  OUTPUT STREAM SeqStat CLOSE.
END PROCEDURE. /* PutSeqStat */


/* ------------------------------------------------------------------------- */
PROCEDURE DeleteUserStat.
/* Delete user's statistics if the user has been disconnected from database  */

  DEFINE INPUT PARAMETER ipSnapshotID AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipUserNumber LIKE _Connect._Connect-Usr NO-UNDO.

  DO TRANSACTION:
    FOR EACH ttTableStat
       WHERE ttTableStat.SnapshotID EQ ipSnapshotID
         AND ttTableStat.UserNumber EQ ipUserNumber:
      DELETE ttTableStat.
    END.

    FOR EACH ttIndexStat
       WHERE ttIndexStat.SnapshotID EQ ipSnapshotID
         AND ttIndexStat.UserNumber EQ ipUserNumber:
      DELETE ttIndexStat.
    END.

    FOR FIRST ttUserStat
        WHERE ttUserStat.SnapshotID EQ ipSnapshotID
          AND ttUserStat.UserNumber EQ ipUserNumber:
      DELETE  ttUserStat.

    END.

    FOR FIRST ttSelectedUser
        WHERE ttSelectedUser.UserNumber EQ ipUserNumber:
      DELETE  ttSelectedUser.
    END.
  END. /* TRANSACTION */
END PROCEDURE. /* DeleteUserStat */


/* ------------------------------------------------------------------------- */
PROCEDURE PutUserStat.

/* _Connect/_User-IO/_Trans/_UserLock */

  DEFINE BUFFER bCurrUserStat FOR ttUserStat.
  DEFINE BUFFER bPrevUserStat FOR ttUserStat.

  DEFINE VARIABLE vPrevDate2 AS DATE    NO-UNDO.
  DEFINE VARIABLE vPrevTime2 AS INTEGER NO-UNDO.
  DEFINE VARIABLE vInterval  AS INTEGER NO-UNDO.
  DEFINE VARIABLE vDbAccess  AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vDbRead    AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vBiRead    AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vRecLock   AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vRecWait   AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vBiClCount AS INTEGER NO-UNDO.
  DEFINE VARIABLE vRLCounter LIKE _Trans._Trans-counter NO-UNDO.
  DEFINE VARIABLE vLastTask  LIKE _MstrBlk._MstrBlk-lasttask NO-UNDO.
  DEFINE VARIABLE vTasksPerSec AS DECIMAL NO-UNDO.

  OUTPUT STREAM UserStat TO VALUE(vUserStatFile) APPEND.
  IF SEEK(UserStat) EQ 0 THEN
  PUT STREAM UserStat UNFORMATTED
           "Date"
    {&Sep} "Time"
    {&Sep} "Interval"
    {&Sep} "Usr"
    {&Sep} "UserName"
    {&Sep} "PID"
    {&Sep} "LoginTime"
    {&Sep} "Device"
    {&Sep} "Type"
    {&Sep} "Batch"
    {&Sep} "Server"
    {&Sep} "DbAccess"
    {&Sep} "DbRead"
    {&Sep} "BiRead"
    {&Sep} "TranNum"
    {&Sep} "TranState"
    {&Sep} "TranTime"
    {&Sep} "TranLen"
    {&Sep} "NumOfBiCls"
    {&Sep} "RecLock"
    {&Sep} "RecWait"
    {&Sep} "NumOfLocks"
    {&Sep} "LockList"
    {&Sep} "WaitName"
    {&Sep} "WaitInfo"
    {&Sep} "Disconnect"
    {&Sep} "Resync"
    {&Sep} "Notes"
  SKIP.

  ASSIGN vBiClCount = GetDbAttr("NumOfBiClusters":U)
         vRLCounter = GetDbAttr("CurrentRLCounter":U)
  . /* ASSIGN */

  FOR EACH bCurrUserStat NO-LOCK
     WHERE bCurrUserStat.SnapshotID EQ vCurrSnapshotID:

/* User has been disconnected from database: */
    IF  bCurrUserStat.UserNumber    NE {&ALL}
    AND bCurrUserStat.SnapshotCount NE vSnapshotCount THEN
    DO:
      RUN DeleteUserStat(INPUT vPrevSnapshotID,
                         INPUT bCurrUserStat.UserNumber).

      RUN DeleteUserStat(INPUT vCurrSnapshotID,
                         INPUT bCurrUserStat.UserNumber).
      NEXT.
    END.

    FIND FIRST bPrevUserStat NO-LOCK
         WHERE bPrevUserStat.SnapshotID EQ vPrevSnapshotID
           AND bPrevUserStat.UserNumber EQ bCurrUserStat.UserNumber
    NO-ERROR.

/* User has been disconnected from database
  and its UsrNum was re-used by new session:
*/  IF AVAILABLE bPrevUserStat
    AND (bPrevUserStat.uConnect-Time   NE bCurrUserStat.uConnect-Time
     OR  bPrevUserStat.uConnect-Device NE bCurrUserStat.uConnect-Device
     OR  bPrevUserStat.uConnect-Pid    NE bCurrUserStat.uConnect-Pid) THEN
    RUN DeleteUserStat(INPUT vPrevSnapshotID,
                       INPUT bPrevUserStat.UserNumber).

    IF bCurrUserStat.UserNumber EQ {&ALL} THEN
    ASSIGN vLastTask    = bCurrUserStat.uConnect-TransId
           vTasksPerSec = (IF AVAILABLE bPrevUserStat
                           THEN (bCurrUserStat.uConnect-TransId -
                                 bPrevUserStat.uConnect-TransId)
                           ELSE  GetDbAttr("Commits":U)
                          ) / vSampleInterval
    . /* ASSIGN */

    IF AVAILABLE bPrevUserStat THEN
    ASSIGN vInterval = vSampleInterval
           vDbAccess = (bCurrUserStat.uUserIO-DbAccess -
                        bPrevUserStat.uUserIO-DbAccess) / vInterval
           vDbRead   = (bCurrUserStat.uUserIO-DbRead -
                        bPrevUserStat.uUserIO-DbRead)   / vInterval
           vBiRead   = (bCurrUserStat.uUserIO-BiRead -
                        bPrevUserStat.uUserIO-BiRead)   / vInterval
           vRecLock  = (bCurrUserStat.uLockReq-RecLock -
                        bPrevUserStat.uLockReq-RecLock) / vInterval
           vRecWait  = (bCurrUserStat.uLockReq-RecWait  -
                        bPrevUserStat.uLockReq-RecWait) / vInterval
    . /* ASSIGN */
    ELSE
    ASSIGN vInterval = 86400
                     * (vCurrDate - String2Date(bCurrUserStat.uConnect-Time))
                     + (vCurrTime - String2Time(bCurrUserStat.uConnect-Time))
           vDbAccess = bCurrUserStat.uUserIO-DbAccess / vInterval
           vDbRead   = bCurrUserStat.uUserIO-DbRead   / vInterval
           vBiRead   = bCurrUserStat.uUserIO-BiRead   / vInterval
           vRecLock  = bCurrUserStat.uLockReq-RecLock / vInterval
           vRecWait  = bCurrUserStat.uLockReq-RecWait / vInterval
    . /* ASSIGN */

    IF CAN-DO("SELF,REMC":U, TRIM(bCurrUserStat.uConnect-Type)) THEN
    DO:
      IF vDbAccess GT vTopDbAccessToss THEN
      RUN AddToTopList("DbAccess":U,
                       bCurrUserStat.UserNumber,
                       vDbAccess).

      IF vDbRead GT vTopDbReadToss THEN
      RUN AddToTopList("DbRead":U,
                       bCurrUserStat.UserNumber,
                       vDbRead).

      IF vRecLock GT vTopLockReqToss THEN
      RUN AddToTopList("LockReq":U,
                       bCurrUserStat.UserNumber,
                       vRecLock).
    END.

    FIND FIRST ttSelectedUser NO-LOCK
         WHERE ttSelectedUser.UserNumber EQ bCurrUserStat.UserNumber
    NO-ERROR.

    ASSIGN vStatCount = vStatCount + 1.

    PUT STREAM UserStat UNFORMATTED
             /*Date      */ vCurrDate
      {&Sep} /*Time      */ STRING(vCurrTime,  "HH:MM:SS":U)
      {&Sep} /*Interval  */ vInterval
      {&Sep} /*Usr       */ bCurrUserStat.UserNumber
      {&Sep} /*UserName  */ bCurrUserStat.uConnect-Name
      {&Sep} /*PID       */ IF   bCurrUserStat.uConnect-Pid LT 0
                            THEN bCurrUserStat.uConnect-Pid + 65536
                            ELSE bCurrUserStat.uConnect-Pid
      {&Sep} /*LoginTime */ bCurrUserStat.uConnect-Time
      {&Sep} /*Device    */ bCurrUserStat.uConnect-Device
      {&Sep} /*Type      */ bCurrUserStat.uConnect-Type
      {&Sep} /*Batch     */ bCurrUserStat.uConnect-Batch
      {&Sep} /*Server    */ IF bCurrUserStat.uConnect-Type EQ "SERV":U
                            THEN bCurrUserStat.UserNumber
                            ELSE bCurrUserStat.uConnect-Server
      {&Sep} /*DbAccess  */ vDbAccess
      {&Sep} /*DbRead    */ vDbRead
      {&Sep} /*BiRead    */ vBiRead
      {&Sep} /*TranId    */ bCurrUserStat.uConnect-TransId
      {&Sep} /*TranState */ bCurrUserStat.uTrans-State
      {&Sep} /*TranTime  */ IF bCurrUserStat.uTrans-State EQ "ALLOCATED":U
                            THEN STRING(INTEGER(
                                 (vLastTask - bCurrUserStat.uConnect-TransId)
                                 / vTasksPerSec)) + " sec ago"
                            ELSE bCurrUserStat.uTrans-txtime
      {&Sep} /*TranDurati*/ bCurrUserStat.uTrans-Duration
      {&Sep} /*NumOfBiCls*/ IF bCurrUserStat.UserNumber EQ {&ALL}
                            THEN vBiClCount
                            ELSE
                            IF bCurrUserStat.uTrans-counter EQ 0
                            THEN 0
                            ELSE vRLCounter - bCurrUserStat.uTrans-counter + 1
      {&Sep} /*RecLock*/    vRecLock
      {&Sep} /*RecWait*/    vRecWait
      {&Sep} /*NumOfLocks*/ bCurrUserStat.LockCount
      {&Sep} /*LockList  */ bCurrUserStat.LockList
      {&Sep} /*WaitName  */ bCurrUserStat.uConnect-Wait
      {&Sep} /*WaitInfo  */ bCurrUserStat.uConnect-Wait1
      {&Sep} /*Disconnect*/ bCurrUserStat.uConnect-Disconnect
      {&Sep} /*Resync    */ bCurrUserStat.uConnect-Resync
      {&Sep} /*Notes     */ IF bCurrUserStat.UserNumber EQ vMyUserNumber THEN
                            "My Connection" ELSE
                            IF AVAILABLE ttSelectedUser THEN
                            ttSelectedUser.UserDesc ELSE "":U
    SKIP.
  END. /* FOR EACH bCurrUserStat */
  OUTPUT STREAM UserStat CLOSE.
END PROCEDURE. /* PutUserStat */


/* ------------------------------------------------------------------------- */
PROCEDURE PutTableStat.

  DEFINE BUFFER bPrevTableStat FOR ttTableStat.
  DEFINE BUFFER bCurrTableStat FOR ttTableStat.

  DEFINE VARIABLE vInterval    AS INTEGER NO-UNDO.
  DEFINE VARIABLE vTableRead   AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vTableUpdate AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vTableCreate AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vTableDelete AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vTableOsRead AS DECIMAL NO-UNDO.

  OUTPUT STREAM TableStat TO VALUE(vTableStatFile) APPEND.
  IF SEEK(TableStat) EQ 0 THEN
  PUT STREAM TableStat UNFORMATTED
           "Date"
    {&Sep} "Time"
    {&Sep} "Interval"
    {&Sep} "Usr"
    {&Sep} "UserName"
    {&Sep} "Tbl"
    {&Sep} "TableName"
    {&Sep} "Area"
    {&Sep} "AreaName"
    {&Sep} "AreaInfo"
    {&Sep} "AreaSize(MB)"
    {&Sep} "Read"
    {&Sep} "Update"
    {&Sep} "Create"
    {&Sep} "Delete"
    {&Sep} "OsRead"
    {&Sep} "Notes"
    {&Sep} "B2"
    {&Sep} "LastChange"
    {&Sep} "#key"
    {&Sep} "#fld"
    {&Sep} "#kfld"
    {&Sep} "#kcmp"

  SKIP.

  FOR EACH bCurrTableStat NO-LOCK
     WHERE bCurrTableStat.SnapshotID EQ vCurrSnapshotID,

     FIRST tt_File NO-LOCK
     WHERE tt_File.uFile-Number EQ bCurrTableStat.TableNumber,

     FIRST tt_Area
     WHERE tt_Area.uArea-Number EQ tt_File.AreaNumber,

     FIRST ttUserStat NO-LOCK
     WHERE ttUserStat.SnapshotID EQ vCurrSnapshotID
       AND ttUserStat.UserNumber EQ bCurrTableStat.UserNumber

        BY bCurrTableStat.SnapshotID
        BY bCurrTableStat.UserNumber
        BY bCurrTableStat.TableNumber:

    FIND FIRST bPrevTableStat NO-LOCK
         WHERE bPrevTableStat.SnapshotID  EQ vPrevSnapshotID
           AND bPrevTableStat.UserNumber  EQ bCurrTableStat.UserNumber
           AND bPrevTableStat.TableNumber EQ bCurrTableStat.TableNumber
    NO-ERROR.

/* The difference between two snapshots: */
    IF AVAILABLE bPrevTableStat THEN
    ASSIGN vInterval    = vSampleInterval
           vTableRead   = (bCurrTableStat.TableRead    -
                           bPrevTableStat.TableRead)   / vInterval
           vTableUpdate = (bCurrTableStat.TableUpdate  -
                           bPrevTableStat.TableUpdate) / vInterval
           vTableCreate = (bCurrTableStat.TableCreate  -
                           bPrevTableStat.TableCreate) / vInterval
           vTableDelete = (bCurrTableStat.TableDelete  -
                           bPrevTableStat.TableDelete) / vInterval
           vTableOsRead = (bCurrTableStat.TableOsRead  -
                           bPrevTableStat.TableOsRead) / vInterval
    . /* ASSIGN */
    ELSE
/* User did not access the given table before the current snapshot
   or user logged into the database after the previous snapshot:
*/  ASSIGN vInterval    = IF bCurrTableStat.UserNumber EQ {&ALL}
                          THEN vSampleInterval
                          ELSE
                  86400 * (vCurrDate - String2Date(ttUserStat.uConnect-Time))
                        + (vCurrTime - String2Time(ttUserStat.uConnect-Time))
           vTableRead   = bCurrTableStat.TableRead   / vInterval
           vTableUpdate = bCurrTableStat.TableUpdate / vInterval
           vTableCreate = bCurrTableStat.TableCreate / vInterval
           vTableDelete = bCurrTableStat.TableDelete / vInterval
           vTableOsRead = bCurrTableStat.TableOsRead / vInterval
    . /* ASSIGN */

    ASSIGN vStatCount = vStatCount + 1.

    IF  vTableRead   NE 0
    OR  vTableUpdate NE 0
    OR  vTableCreate NE 0
    OR  vTableDelete NE 0
    OR (vTableOsRead NE 0 /* Could it be true if vTableRead eq 0 ? */
    AND vTableOsRead NE ?) THEN
    PUT STREAM TableStat UNFORMATTED
             /*Date      */ vCurrDate
      {&Sep} /*Time      */ STRING(vCurrTime,  "HH:MM:SS":U)
      {&Sep} /*Interval  */ vInterval
      {&Sep} /*UserNum   */ bCurrTableStat.UserNumber
      {&Sep} /*UserName  */ ttUserStat.uConnect-Name
      {&Sep} /*TableNum  */ bCurrTableStat.TableNumber
      {&Sep} /*TableName */ tt_File.uFile-Name
      {&Sep} /*Area      */ tt_Area.uArea-Number
      {&Sep} /*AreaName  */ tt_Area.uArea-Name
      {&Sep} /*AreaInfo  */ tt_Area.AreaInfo
      {&Sep} /*AreaSize  */ tt_Area.AreaSize
      {&Sep} /*Read      */ vTableRead
      {&Sep} /*Update    */ vTableUpdate
      {&Sep} /*Create    */ vTableCreate
      {&Sep} /*Delete    */ vTableDelete
      {&Sep} /*OsRead    */ vTableOsRead
      {&Sep} /*Notes     */ IF bCurrTableStat.UserNumber EQ vMyUserNumber THEN
                            "My Connection" ELSE bCurrTableStat.StatDesc
      {&Sep} /*B2        */     tt_File.InB2
      {&Sep} /*LastChange*/ '"' tt_File.LastChange '"'
      {&Sep} /*#key      */     tt_File.uNumKey
      {&Sep} /*#fld      */     tt_File.uNumFld
      {&Sep} /*kfld      */     tt_File.uNumKFld
      {&Sep} /*kcmp      */     tt_File.uNumKComp
    SKIP.
  END. /* FOR EACH bCurrTableStat */

  OUTPUT STREAM TableStat CLOSE.
END PROCEDURE. /* PutTableStat */


/* ------------------------------------------------------------------------- */
PROCEDURE PutIndexStat.

  DEFINE VARIABLE vIndexLevelsCalls   AS INTEGER NO-UNDO.

  DEFINE BUFFER bPrevIndexStat FOR ttIndexStat.
  DEFINE BUFFER bCurrIndexStat FOR ttIndexStat.

  DEFINE VARIABLE vInterval    AS INTEGER NO-UNDO.
  DEFINE VARIABLE vIndexRead   AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vIndexCreate AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vIndexDelete AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vIndexOsRead AS DECIMAL NO-UNDO.

  OUTPUT STREAM IndexStat TO VALUE(vIndexStatFile) APPEND.
  IF SEEK(IndexStat) EQ 0 THEN
  PUT STREAM IndexStat UNFORMATTED
           "Date"
    {&Sep} "Time"
    {&Sep} "Interval"
    {&Sep} "Usr"
    {&Sep} "UserName"
    {&Sep} "Idx"
    {&Sep} "TableName"
    {&Sep} "IndexName"
    {&Sep} "IndexAttr"
    {&Sep} "Area"
    {&Sep} "AreaName"
    {&Sep} "AreaInfo"
    {&Sep} "AreaSize(MB)"
    {&Sep} "Read"
    {&Sep} "Create"
    {&Sep} "Delete"
    {&Sep} "OsRead"
    {&Sep} "Notes"
    {&Sep} "B2"
    {&Sep} "RootBlock"
    {&Sep} "Levels"
  SKIP.

  FOR EACH bCurrIndexStat NO-LOCK
     WHERE bCurrIndexStat.SnapshotID EQ vCurrSnapshotID,

     FIRST tt_Index NO-LOCK
     WHERE tt_Index.uIdx-Num EQ bCurrIndexStat.IndexNumber,

     FIRST tt_File NO-LOCK
     WHERE tt_File.uFileRecid EQ tt_Index.uFile-Recid,

     FIRST tt_Area NO-LOCK
     WHERE tt_Area.uArea-Number EQ tt_Index.AreaNumber,

     FIRST ttUserStat NO-LOCK
     WHERE ttUserStat.SnapshotID EQ vCurrSnapshotID
       AND ttUserStat.UserNumber EQ bCurrIndexStat.UserNumber

        BY bCurrIndexStat.SnapshotID
        BY bCurrIndexStat.UserNumber
        BY bCurrIndexStat.IndexNumber:

    FIND FIRST bPrevIndexStat NO-LOCK
         WHERE bPrevIndexStat.SnapshotID  EQ vPrevSnapshotID
           AND bPrevIndexStat.UserNumber  EQ bCurrIndexStat.UserNumber
           AND bPrevIndexStat.IndexNumber EQ bCurrIndexStat.IndexNumber
    NO-ERROR.

/* The difference between two snapshots: */
    IF AVAILABLE bPrevIndexStat THEN
    ASSIGN vInterval    = vSampleInterval
           vIndexRead   = (bCurrIndexStat.IndexRead    -
                           bPrevIndexStat.IndexRead)   / vInterval
           vIndexCreate = (bCurrIndexStat.IndexCreate  -
                           bPrevIndexStat.IndexCreate) / vInterval
           vIndexDelete = (bCurrIndexStat.IndexDelete  -
                           bPrevIndexStat.IndexDelete) / vInterval
           vIndexOsRead = (bCurrIndexStat.IndexOsRead  -
                           bPrevIndexStat.IndexOsRead) / vInterval
    . /* ASSIGN */
    ELSE
/* User did not access the given index before the current snapshot
   or user logged into the database after the previous snapshot:
*/  ASSIGN vInterval    = IF bCurrIndexStat.UserNumber EQ {&ALL}
                          THEN vSampleInterval
                          ELSE
                  86400 * (vCurrDate - String2Date(ttUserStat.uConnect-Time))
                        + (vCurrTime - String2Time(ttUserStat.uConnect-Time))
           vIndexRead   = bCurrIndexStat.IndexRead   / vInterval
           vIndexCreate = bCurrIndexStat.IndexCreate / vInterval
           vIndexDelete = bCurrIndexStat.IndexDelete / vInterval
           vIndexOsRead = bCurrIndexStat.IndexOsRead / vInterval
    . /* ASSIGN */

/* To minimize the attempts to estimate the number of index tree levels: */
    IF  tt_Index.IndexLevels EQ ?
    AND tt_File.CanRead
    AND vIndexRead GT 0 THEN
    DO:
      ASSIGN vIndexLevelsCalls = vIndexLevelsCalls + 1.
      RUN IndexLevels(INPUT tt_File.uFile-Name,
                      INPUT tt_Index.uIndex-Name,
                     OUTPUT tt_Index.IndexLevels).
    END.

    ASSIGN vStatCount = vStatCount + 1.

    IF  vIndexRead   NE 0
    OR  vIndexCreate NE 0
    OR  vIndexDelete NE 0
    OR (vIndexOsRead NE 0 /* Could it be true if vIndexRead eq 0 ? */
    AND vIndexOsRead NE ?) THEN
    PUT STREAM IndexStat UNFORMATTED
             /*Date     */ vCurrDate
      {&Sep} /*Time     */ STRING(vCurrTime,  "HH:MM:SS":U)
      {&Sep} /*Interval */ vInterval
      {&Sep} /*UserNum  */ bCurrIndexStat.UserNumber
      {&Sep} /*UserName */ ttUserStat.uConnect-Name
      {&Sep} /*IndexNum */ bCurrIndexStat.IndexNumber
      {&Sep} /*TableName*/ tt_File.uFile-Name
      {&Sep} /*IndexName*/ tt_Index.uIndex-Name
      {&Sep} /*IndexAttr*/ tt_Index.IndexAttr
                         + MIN(":":U, tt_Index.IndexAttr, tt_Index.FieldList)
                         + tt_Index.FieldList
      {&Sep} /*Area     */ tt_Area.uArea-Number
      {&Sep} /*AreaName */ tt_Area.uArea-Name
      {&Sep} /*AreaInfo */ tt_Area.AreaInfo
      {&Sep} /*AreaSize */ tt_Area.AreaSize
      {&Sep} /*Read     */ vIndexRead
      {&Sep} /*Create   */ vIndexCreate
      {&Sep} /*Delete   */ vIndexDelete
      {&Sep} /*OsRead   */ vIndexOsRead
      {&Sep} /*Notes    */ IF bCurrIndexStat.UserNumber EQ vMyUserNumber THEN
                           "My Connection" ELSE bCurrIndexStat.StatDesc
      {&Sep} /*B2       */ tt_Index.InB2
      {&Sep} /*RootBlock*/ tt_Index.RootBlock
      {&Sep} /*Levels   */ tt_Index.IndexLevels
    SKIP.
  END. /* FOR EACH bCurrIndexStat */

  OUTPUT STREAM IndexStat CLOSE.

  IF vIndexLevelsCalls NE 0 THEN
  PUT STREAM TimingLog UNFORMATTED
           /*Date     */ TODAY
    {&Sep} /*Time     */ STRING(TIME, "HH:MM:SS":U)
    {&Sep} /*Procedure*/ "PutIndexStat.IndexLevels"
    {&Sep} /*RecCount */ vIndexLevelsCalls
    {&Sep} /*ETime    */ ?
  SKIP.
END PROCEDURE. /* PutIndexStat */


/* ------------------------------------------------------------------------- */
PROCEDURE GetAreaIOStat.

  DEFINE VARIABLE vAreaNumber LIKE _Area._Area-Number    NO-UNDO.
  DEFINE VARIABLE vExtentId1  LIKE _ActIOFile._IOFile-Id NO-UNDO.
  DEFINE VARIABLE vExtentId2  LIKE _ActIOFile._IOFile-Id NO-UNDO.

  ASSIGN vStatCount = 0.
  FOR FIRST DICTDB._ActSummary NO-LOCK,
      FIRST DICTDB._ActSpace   NO-LOCK,
      FIRST DICTDB._ActIOType  NO-LOCK
  TRANSACTION:

/* Entire Db: */
    ASSIGN vAreaNumber = {&AllObjects}.
    FIND FIRST ttAreaIOStat
         WHERE ttAreaIOStat.SnapshotID EQ vCurrSnapshotID
           AND ttAreaIOStat.AreaNumber EQ vAreaNumber
    NO-ERROR.
    IF NOT AVAILABLE ttAreaIOStat THEN
    CREATE ttAreaIOStat.
    ASSIGN ttAreaIOStat.SnapshotID  = vCurrSnapshotID
           ttAreaIOStat.AreaNumber  = vAreaNumber
           ttAreaIOStat.AreaReads   = DICTDB._ActSummary._Summary-DbReads
           ttAreaIOStat.AreaWrites  = DICTDB._ActSummary._Summary-DbWrites
           ttAreaIOStat.AreaExtends = DICTDB._ActSpace._Space-DbExd
           ttAreaIOStat.SortOrder   = vStatCount
           vStatCount               = vStatCount + 1
    . /* ASSIGN */

/* All Tables: */
    ASSIGN vAreaNumber = {&AllTables}.
    FIND FIRST ttAreaIOStat
         WHERE ttAreaIOStat.SnapshotID EQ vCurrSnapshotID
           AND ttAreaIOStat.AreaNumber EQ vAreaNumber
    NO-ERROR.
    IF NOT AVAILABLE ttAreaIOStat THEN
    CREATE ttAreaIOStat.
    ASSIGN ttAreaIOStat.SnapshotID  = vCurrSnapshotID
           ttAreaIOStat.AreaNumber  = vAreaNumber
           ttAreaIOStat.AreaReads   = DICTDB._ActIOType._IOType-DataReads
           ttAreaIOStat.AreaWrites  = DICTDB._ActIOType._IOType-DataWrts
           ttAreaIOStat.AreaExtends = ?
           ttAreaIOStat.SortOrder   = vStatCount
           vStatCount               = vStatCount + 1
    . /* ASSIGN */

/* All Indexes: */
    ASSIGN vAreaNumber = {&AllIndexes}.
    FIND FIRST ttAreaIOStat
         WHERE ttAreaIOStat.SnapshotID EQ vCurrSnapshotID
           AND ttAreaIOStat.AreaNumber EQ vAreaNumber
    NO-ERROR.
    IF NOT AVAILABLE ttAreaIOStat THEN
    CREATE ttAreaIOStat.
    ASSIGN ttAreaIOStat.SnapshotID  = vCurrSnapshotID
           ttAreaIOStat.AreaNumber  = vAreaNumber
           ttAreaIOStat.AreaReads   = DICTDB._ActIOType._IOType-IdxRds
           ttAreaIOStat.AreaWrites  = DICTDB._ActIOType._IOType-IdxWrts
           ttAreaIOStat.AreaExtends = ?
           ttAreaIOStat.SortOrder   = vStatCount
           vStatCount               = vStatCount + 1
    . /* ASSIGN */

/* All LOBs: */
    ASSIGN vAreaNumber = {&AllLOBs}.
    FIND FIRST ttAreaIOStat
         WHERE ttAreaIOStat.SnapshotID EQ vCurrSnapshotID
           AND ttAreaIOStat.AreaNumber EQ vAreaNumber
    NO-ERROR.
    IF NOT AVAILABLE ttAreaIOStat THEN
    CREATE ttAreaIOStat.
    ASSIGN ttAreaIOStat.SnapshotID  = vCurrSnapshotID
           ttAreaIOStat.AreaNumber  = vAreaNumber
           ttAreaIOStat.AreaReads   = DICTDB._ActSummary._Summary-DbReads
                                    - DICTDB._ActIOType._IOType-DataReads
                                    - DICTDB._ActIOType._IOType-IdxRds
           ttAreaIOStat.AreaWrites  = DICTDB._ActSummary._Summary-DbWrites
                                    - DICTDB._ActIOType._IOType-DataWrts
                                    - DICTDB._ActIOType._IOType-IdxWrts
           ttAreaIOStat.AreaExtends = ?
           ttAreaIOStat.SortOrder   = vStatCount
           vStatCount               = vStatCount + 1
    . /* ASSIGN */
  END. /* FOR FIRST _ActSummary, FIRST _ActSpace, FIRST _ActIOType */

/* Primary and Alternative Buffer Pools: */
  IF CAN-FIND(FIRST tt_Area WHERE tt_Area.uArea-Number EQ {&AllB2Objects}) THEN
  FOR EACH DICTDB._ActBuffer NO-LOCK
     WHERE DICTDB._ActBuffer._Buffer-Id GE 2
       AND DICTDB._ActBuffer._Buffer-Id LE 3:

    ASSIGN vAreaNumber = IF DICTDB._ActBuffer._Buffer-Id EQ 2
                         THEN {&AllB1Objects}
                         ELSE {&AllB2Objects}
    . /* ASSIGN */
    FIND FIRST ttAreaIOStat
         WHERE ttAreaIOStat.SnapshotID EQ vCurrSnapshotID
           AND ttAreaIOStat.AreaNumber EQ vAreaNumber
    NO-ERROR.
    IF NOT AVAILABLE ttAreaIOStat THEN
    CREATE ttAreaIOStat.
    ASSIGN ttAreaIOStat.SnapshotID  = vCurrSnapshotID
           ttAreaIOStat.AreaNumber  = vAreaNumber
           ttAreaIOStat.AreaReads   = DICTDB._ActBuffer._Buffer-OSRds
           ttAreaIOStat.AreaWrites  = DICTDB._ActBuffer._Buffer-OSWrts
           ttAreaIOStat.AreaExtends = ?
           ttAreaIOStat.SortOrder   = vStatCount
           vStatCount               = vStatCount + 1
    . /* ASSIGN */
  END.

  ASSIGN vExtentId2 = 1.
  FOR EACH DICTDB._Area NO-LOCK
     WHERE DICTDB._Area._Area-Number GT 1:

    ASSIGN vExtentId1 = vExtentId2
           vExtentId2 = vExtentId2 + DICTDB._Area._Area-extents
    . /* ASSIGN */

/* Don't read _ActIOFile for Secondary Recovery Areas: */
/* Session might crash with the errors if a read coincides in time
   with run of image empty:
(2329)  SYSTEM ERROR: Invalid block 0 for file db.a3, max is 0.
(612)   SYSTEM ERROR: Possible file truncation, 0 too big for database.
*/  IF DICTDB._Area._Area-Type GT {&DataArea} THEN
    NEXT.

    FIND FIRST ttAreaIOStat
         WHERE ttAreaIOStat.SnapshotID EQ vCurrSnapshotID
           AND ttAreaIOStat.AreaNumber EQ DICTDB._Area._Area-Number
    NO-ERROR.
    IF NOT AVAILABLE ttAreaIOStat THEN
    CREATE ttAreaIOStat.
    ASSIGN ttAreaIOStat.SnapshotID  = vCurrSnapshotID
           ttAreaIOStat.AreaNumber  = DICTDB._Area._Area-Number
           ttAreaIOStat.AreaReads   = 0
           ttAreaIOStat.AreaWrites  = 0
           ttAreaIOStat.AreaExtends = 0
           ttAreaIOStat.TableRead   = 0
           ttAreaIOStat.TableUpdate = 0
           ttAreaIOStat.TableCreate = 0
           ttAreaIOStat.TableDelete = 0
           ttAreaIOStat.IndexRead   = 0
           ttAreaIOStat.IndexCreate = 0
           ttAreaIOStat.IndexDelete = 0
           ttAreaIOStat.SortOrder   = vStatCount
           vStatCount               = vStatCount + 1
    . /* ASSIGN */

    FOR EACH DICTDB._ActIOFile NO-LOCK
       WHERE DICTDB._ActIOFile._IOFile-Id GT vExtentId1
         AND DICTDB._ActIOFile._IOFile-Id LE vExtentId2:

      ASSIGN ttAreaIOStat.AreaReads   = ttAreaIOStat.AreaReads
                                      + DICTDB._ActIOFile._IOFile-Reads
             ttAreaIOStat.AreaWrites  = ttAreaIOStat.AreaWrites
                                      + DICTDB._ActIOFile._IOFile-Writes
             ttAreaIOStat.AreaExtends = ttAreaIOStat.AreaExtends
                                      + DICTDB._ActIOFile._IOFile-Extends
      . /* ASSIGN */
    END. /* FOR EACH _ActIOFile */
  END. /* FOR EACH _Area */
END PROCEDURE. /* GetAreaIOStat */


/* ------------------------------------------------------------------------- */
PROCEDURE PutAreaIOStat.

  DEFINE BUFFER bCurrAreaIOStat FOR ttAreaIOStat.
  DEFINE BUFFER bPrevAreaIOStat FOR ttAreaIOStat.

  DEFINE VARIABLE vInterval    AS INTEGER NO-UNDO.
  DEFINE VARIABLE vAreaReads   AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vAreaWrites  AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vAreaExtends AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vTableRead   AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vTableUpdate AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vTableCreate AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vTableDelete AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vIndexRead   AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vIndexCreate AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vIndexDelete AS DECIMAL NO-UNDO.

/* Stats for {&AllB1Objects} is the difference
   between {&AllObjects} and {&AllB2Objects}
*/
  FOR FIRST ttAreaIOStat EXCLUSIVE
      WHERE ttAreaIOStat.SnapshotID EQ vCurrSnapshotID
        AND ttAreaIOStat.AreaNumber EQ {&AllB1Objects},

      FIRST bCurrAreaIOStat NO-LOCK
      WHERE bCurrAreaIOStat.SnapshotID EQ vCurrSnapshotID
        AND bCurrAreaIOStat.AreaNumber EQ {&AllObjects},

      FIRST bPrevAreaIOStat NO-LOCK
      WHERE bPrevAreaIOStat.SnapshotID EQ vCurrSnapshotID
        AND bPrevAreaIOStat.AreaNumber EQ {&AllB2Objects}:

    ASSIGN ttAreaIOStat.TableRead   = bCurrAreaIOStat.TableRead
                                    - bPrevAreaIOStat.TableRead
           ttAreaIOStat.TableUpdate = bCurrAreaIOStat.TableUpdate
                                    - bPrevAreaIOStat.TableUpdate
           ttAreaIOStat.TableCreate = bCurrAreaIOStat.TableCreate
                                    - bPrevAreaIOStat.TableCreate
           ttAreaIOStat.TableDelete = bCurrAreaIOStat.TableDelete
                                    - bPrevAreaIOStat.TableDelete
           ttAreaIOStat.IndexRead   = bCurrAreaIOStat.IndexRead
                                    - bPrevAreaIOStat.IndexRead
           ttAreaIOStat.IndexCreate = bCurrAreaIOStat.IndexCreate
                                    - bPrevAreaIOStat.IndexCreate
           ttAreaIOStat.IndexDelete = bCurrAreaIOStat.IndexDelete
                                    - bPrevAreaIOStat.IndexDelete
    . /* ASSIGN */
  END.

  OUTPUT STREAM AreaIOStat TO VALUE(vAreaIOStatFile) APPEND.
  IF SEEK(AreaIOStat) EQ 0 THEN
  PUT STREAM AreaIOStat UNFORMATTED
           "Date"
    {&Sep} "Time"
    {&Sep} "Interval"
    {&Sep} "Area"
    {&Sep} "AreaName"
    {&Sep} "AreaInfo"
    {&Sep} "AreaSize(MB)"
    {&Sep} "Reads"
    {&Sep} "Writes"
    {&Sep} "Extends"
    {&Sep} "TableRead"
    {&Sep} "TableUpdate"
    {&Sep} "TableCreate"
    {&Sep} "TableDelete"
    {&Sep} "IndexRead  "
    {&Sep} "IndexCreate"
    {&Sep} "IndexDelete"
  SKIP.

  FOR EACH bCurrAreaIOStat NO-LOCK
     WHERE bCurrAreaIOStat.SnapshotID EQ vCurrSnapshotID,

    FIRST tt_Area NO-LOCK
    WHERE tt_Area.uArea-Number EQ bCurrAreaIOStat.AreaNumber

        BY bCurrAreaIOStat.SortOrder:

    FIND FIRST bPrevAreaIOStat NO-LOCK
         WHERE bPrevAreaIOStat.SnapshotID EQ vPrevSnapshotID
           AND bPrevAreaIOStat.AreaNumber EQ bCurrAreaIOStat.AreaNumber
    NO-ERROR.

    IF AVAILABLE bPrevAreaIOStat THEN
    ASSIGN vAreaReads   = (bCurrAreaIOStat.AreaReads    -
                           bPrevAreaIOStat.AreaReads)   / vSampleInterval
           vAreaWrites  = (bCurrAreaIOStat.AreaWrites   -
                           bPrevAreaIOStat.AreaWrites)  / vSampleInterval
           vAreaExtends = (bCurrAreaIOStat.AreaExtends  -
                           bPrevAreaIOStat.AreaExtends) / vSampleInterval
           vTableRead   = (bCurrAreaIOStat.TableRead    -
                           bPrevAreaIOStat.TableRead)   / vSampleInterval
           vTableUpdate = (bCurrAreaIOStat.TableUpdate  -
                           bPrevAreaIOStat.TableUpdate) / vSampleInterval
           vTableCreate = (bCurrAreaIOStat.TableCreate  -
                           bPrevAreaIOStat.TableCreate) / vSampleInterval
           vTableDelete = (bCurrAreaIOStat.TableDelete  -
                           bPrevAreaIOStat.TableDelete) / vSampleInterval
           vIndexRead   = (bCurrAreaIOStat.IndexRead    -
                           bPrevAreaIOStat.IndexRead)   / vSampleInterval
           vIndexCreate = (bCurrAreaIOStat.IndexCreate  -
                           bPrevAreaIOStat.IndexCreate) / vSampleInterval
           vIndexDelete = (bCurrAreaIOStat.IndexDelete  -
                           bPrevAreaIOStat.IndexDelete) / vSampleInterval
    . /* ASSIGN */
    ELSE
    ASSIGN vAreaReads   = bCurrAreaIOStat.AreaReads   / vSampleInterval
           vAreaWrites  = bCurrAreaIOStat.AreaWrites  / vSampleInterval
           vAreaExtends = bCurrAreaIOStat.AreaExtends / vSampleInterval
           vTableRead   = bCurrAreaIOStat.TableRead   / vSampleInterval
           vTableUpdate = bCurrAreaIOStat.TableUpdate / vSampleInterval
           vTableCreate = bCurrAreaIOStat.TableCreate / vSampleInterval
           vTableDelete = bCurrAreaIOStat.TableDelete / vSampleInterval
           vIndexRead   = bCurrAreaIOStat.IndexRead   / vSampleInterval
           vIndexCreate = bCurrAreaIOStat.IndexCreate / vSampleInterval
           vIndexDelete = bCurrAreaIOStat.IndexDelete / vSampleInterval
    . /* ASSIGN */

    ASSIGN vStatCount = vStatCount + 1.

    PUT STREAM AreaIOStat UNFORMATTED
             /*Date       */ vCurrDate
      {&Sep} /*Time       */ STRING(vCurrTime,  "HH:MM:SS":U)
      {&Sep} /*Interval   */ vSampleInterval
      {&Sep} /*Area       */ IF bCurrAreaIOStat.AreaNumber LT {&ALL}
                             THEN {&ALL}
                             ELSE bCurrAreaIOStat.AreaNumber
      {&Sep} /*AreaName   */ tt_Area.uArea-Name
      {&Sep} /*AreaInfo   */ tt_Area.AreaInfo
      {&Sep} /*AreaSize   */ tt_Area.AreaSize
      {&Sep} /*Reads      */ vAreaReads
      {&Sep} /*Writes     */ vAreaWrites
      {&Sep} /*Extends    */ vAreaExtends
    . /* PUT */

    IF tt_Area.uArea-type EQ {&DataArea} THEN
    PUT STREAM AreaIOStat UNFORMATTED
      {&Sep} /*TableRead  */ vTableRead
      {&Sep} /*TableUpdate*/ vTableUpdate
      {&Sep} /*TableCreate*/ vTableCreate
      {&Sep} /*TableDelete*/ vTableDelete
      {&Sep} /*IndexRead  */ vIndexRead
      {&Sep} /*IndexCreate*/ vIndexCreate
      {&Sep} /*IndexDelete*/ vIndexDelete
    SKIP.
    ELSE
    PUT STREAM AreaIOStat UNFORMATTED
      {&Sep} /*TableRead  */
      {&Sep} /*TableUpdate*/
      {&Sep} /*TableCreate*/
      {&Sep} /*TableDelete*/
      {&Sep} /*IndexRead  */
      {&Sep} /*IndexCreate*/
      {&Sep} /*IndexDelete*/
    SKIP.

  END. /* FOR EACH bCurrAreaIOStat */

  OUTPUT STREAM AreaIOStat CLOSE.
END PROCEDURE. /* PutAreaIOStat */


/* ------------------------------------------------------------------------- */
PROCEDURE TimingLog.

  DEFINE INPUT PARAMETER ipProcedure AS CHARACTER NO-UNDO.

  PUT STREAM TimingLog UNFORMATTED
           /*Date     */ TODAY
    {&Sep} /*Time     */ STRING(TIME, "HH:MM:SS":U)
    {&Sep} /*Procedure*/ ipProcedure
    {&Sep} /*StatCount*/ vStatCount
    {&Sep} /*ETime    */ ETIME - vPrevETime
  SKIP.

  ASSIGN vPrevETime = ETIME
         vStatCount = 0
  . /* ASSIGN */
END PROCEDURE. /* TimingLog */


/* ------------------------------------------------------------------------- */
/* Main */
DEFINE VARIABLE i AS INTEGER NO-UNDO.

ASSIGN vReadOnlyConnect = CAN-DO(DBRESTRICTIONS("DICTDB"), "READ-ONLY":U)
       vLocalConnection = TRUE
       vDbHost = "":U.
DO i = NUM-ENTRIES(DBPARAM("DICTDB")) TO 1 BY -1
  WHILE vLocalConnection:
  IF ENTRY(i, DBPARAM("DICTDB")) BEGINS "-H ":U THEN
  ASSIGN vLocalConnection = FALSE
         vDbHost = SUBSTRING(ENTRY(i, DBPARAM("DICTDB")) , 3) + ".".
END.

IF vDbHost EQ "":U THEN
IF OPSYS EQ "UNIX":U THEN
DO:
  INPUT THROUGH uname -n.
  IMPORT vDbHost.
  INPUT CLOSE.
END.
ELSE
ASSIGN vDbHost = OS-GETENV("COMPUTERNAME").

FOR FIRST DICTDB._MyConnection NO-LOCK:
  ASSIGN vMyUserNumber = DICTDB._MyConnection._MyConn-UserId.
END.

FOR FIRST DICTDB._FileList NO-LOCK,
    FIRST DICTDB._MstrBlk  NO-LOCK:

  ASSIGN vLogPrefix = DICTDB._FileList._FileList-Name
         vLogPrefix = SUBSTRING(vLogPrefix, 1, LENGTH(vLogPrefix) - 3)
         vLogPrefix = REPLACE(vLogPrefix,  ":":U,  "!":U)
         vLogPrefix = REPLACE(vLogPrefix,  "/":U, "~~":U)
         vLogPrefix = REPLACE(vLogPrefix, "~\":U, "~~":U)
         vLogPrefix = TRIM(vLogPrefix, "~~":U)
         vLogPrefix = "dbmon."
                     + vDbHost + ".":U
                     + SUBSTRING(STRING(YEAR (TODAY), "9999":U), 3,2)
                     + STRING(MONTH(TODAY), "99":U)
                     + STRING(DAY  (TODAY), "99":U) + "_":U
                     + REPLACE(STRING(TIME, "HH:MM:SS":U), ":":U, "":U)
                     + ".":U
                     + vLogPrefix
/* First sample interval begins from db startup: */
         vCurrSnapshotID = ? /* This initial value does not matter */
         vCurrDate       = String2Date(DICTDB._MstrBlk._MstrBlk-oprdate)
         vCurrTime       = String2Time(DICTDB._MstrBlk._MstrBlk-oprdate)
  . /* ASSIGN */

  IF vLogPrefix BEGINS "~~":U THEN
  ASSIGN vLogPrefix = SUBSTRING(vLogPrefix, 2).
END.

/*Disable the reads of _UserLock for remote connections or for 64-bit versions:
 1) Record-length(_UserLock) varies from 2.87K for an empty record to 11.3K
    for a "fully loaded" record (when client locks 512 or more records).
    The reads of _UserLock can be slow over a network.
 2) The reads of _UserLock is very slow due to the bug in 64-bit versions.
*/

IF SESSION:BATCH-MODE THEN
/*  -param $MonIntrv,$MonCount,$Prefix */
ASSIGN vMonIntrv = INTEGER(ENTRY(1, SESSION:PARAMETER))
                   WHEN NUM-ENTRIES(SESSION:PARAMETER) GE 1
       vMonCount = INTEGER(ENTRY(2, SESSION:PARAMETER))
                   WHEN NUM-ENTRIES(SESSION:PARAMETER) GE 2
       vLogPrefix =        ENTRY(3, SESSION:PARAMETER)
                   WHEN NUM-ENTRIES(SESSION:PARAMETER) GE 3
NO-ERROR. /* ASSIGN */

ASSIGN vPrevETime = ETIME
       vStatCount = 0
. /* ASSIGN */

RUN ReadSchema(OUTPUT vTmpRecCount,
               OUTPUT vTmpRecSize).

ASSIGN vCheckUserLock = vLocalConnection AND GetDbAttr("ShmVers") LT 6400000.

ASSIGN vTableStatFile  = vLogPrefix + ".Tables.txt":U
       vIndexStatFile  = vLogPrefix + ".Indexes.txt":U
       vSeqStatFile    = vLogPrefix + ".Sequences.txt":U
       vUserStatFile   = vLogPrefix + ".Users.txt":U
       vUserCacheFile  = vLogPrefix + ".Programs.txt":U
       vServerStatFile = vLogPrefix + ".Servers.txt":U
       vAreaIOStatFile = vLogPrefix + ".AreaIO.txt":U
       vTimingLogFile  = vLogPrefix + ".Timing.log":U
. /* ASSIGN */

IF GetDbAttr("MinTableStatId":U) GT 1
OR GetDbAttr("MaxTableStatId":U) LT GetDbAttr("HighestTableId":U)
OR GetDbAttr("MinIndexStatId":U) GT 8 /* first 7 indexes are system */
OR GetDbAttr("MaxIndexStatId":U) LT GetDbAttr("HighestIndexId":U) THEN
ASSIGN  vTimingLogFile = vLogPrefix + ".Warning.txt":U.

OUTPUT STREAM TimingLog TO VALUE(vTimingLogFile).

FOR FIRST DICTDB._Connect NO-LOCK
    WHERE DICTDB._Connect._Connect-Id EQ vMyUserNumber + 1:

  PUT STREAM TimingLog UNFORMATTED
    vVersion                                          SKIP(1)
    "Client version : " PROVERSION                    SKIP
    "Db version     : " GetDbAttr("DbVers":U)         " (":U
                        GetDbAttr("DbVers":U) MOD 256 ") ":U
              "a.k.a. " DBVERSION("DICTDB")           SKIP
    "Shared mem vers: " GetDbAttr("ShmVers":U)        SKIP
    "Db restrictions: " DBRESTRICTION("DICTDB")       SKIP
    "Db codepage    : " DBCODEPAGE("DICTDB")          SKIP
    "Db collation   : " DBCOLLATION("DICTDB")         SKIP
    "Db connection  : " DBPARAM("DICTDB":U)           SKIP
    "My connection  : " DICTDB._Connect._Connect-Usr  " (":U
                        DICTDB._Connect._Connect-Name  ") ":U
                "PID: " DICTDB._Connect._Connect-Pid
           ", Device: " DICTDB._Connect._Connect-Device
       ", Login time: " DICTDB._Connect._Connect-Time SKIP
    "Session Startup: " SESSION:STARTUP-PARAMETERS    SKIP
    "Temp-table recs: " vTmpRecCount " (":U vTmpRecSize " bytes)" SKIP
    "Selected users : " vSelectedUsers                SKIP
    "Check _UserLock: " vCheckUserLock                SKIP(1)

    "TableCount    : " GetDbAttr("TableCount":U)      SKIP
    "MinTableStatId: " GetDbAttr("MinTableStatId":U)  SKIP
    "TableRangeSize: " GetDbAttr("TableRangeSize":U)  SKIP
    "MaxTableStatId: " GetDbAttr("MaxTableStatId":U)  SKIP
    "HighestTableId: " GetDbAttr("HighestTableId":U)  SKIP(1)

    "IndexCount    : " GetDbAttr("IndexCount":U)      SKIP
    "MinIndexStatId: " GetDbAttr("MinIndexStatId":U)  SKIP
    "IndexRangeSize: " GetDbAttr("IndexRangeSize":U)  SKIP
    "MaxIndexStatId: " GetDbAttr("MaxIndexStatId":U)  SKIP
    "HighestIndexId: " GetDbAttr("HighestIndexId":U)  SKIP(1)
           "Date"
    {&Sep} "Time"
    {&Sep} "Procedure"
    {&Sep} "StatCount"
    {&Sep} "ETime"
  SKIP.
END.
RUN TimingLog("ReadSchema").

ASSIGN vSnapshotCount = 0.
REPEAT:
/* Clean top-lists for new interval: */
  EMPTY TEMP-TABLE ttTopList.
  RUN CreateTopList("LongTran":U, vTopLongTranSize).
  RUN CreateTopList("DbAccess":U, vTopDbAccessSize).
  RUN CreateTopList("DbRead":U,   vTopDbReadSize).
  RUN CreateTopList("LockReq":U,  vTopLockReqSize).

  ASSIGN vPrevSnapshotID = vCurrSnapshotID
         vPrevDate       = vCurrDate
         vPrevTime       = vCurrTime
         vCurrSnapshotID = IF vCurrSnapshotID EQ 1 THEN 0 ELSE 1
         vCurrDate       = TODAY
         vCurrTime       = TIME
         vCurrETime      = ETIME
         vPrevETime      = vCurrETime
         vSampleInterval = (vCurrDate - vPrevDate) * 86400
                         + (vCurrTime - vPrevTime)
  . /* ASSIGN */

  IF SESSION:BATCH-MODE EQ FALSE THEN
  DISPLAY vSnapshotCount                  LABEL "Snapshot"
          STRING(vCurrTime, "HH:MM:SS":U) LABEL "Time"
          vSampleInterval                 LABEL "Interval"
  WITH TITLE "Monitoring " + STRING(vMonIntrv)
                   + " sec x " + STRING(vMonCount).

  RUN GetUserStat.   RUN TimingLog("GetUserStat").
  RUN GetUserIO.     RUN TimingLog("GetUserIO").
  RUN GetTranStat.   RUN TimingLog("GetTranStat").
  RUN GetLockReq.    RUN TimingLog("GetLockReq").
  RUN GetUserLock.   RUN TimingLog("GetUserLock").
  RUN GetServerStat. RUN TimingLog("GetServerStat").
  RUN GetSeqStat.    RUN TimingLog("GetSeqStat").
 /* GetAreaIOStat must run before GetTableStat/GetIndexStat: */
  RUN GetAreaIOStat. RUN TimingLog("GetAreaIOStat").
  RUN GetTableStat.  RUN TimingLog("GetTableStat").
  RUN GetIndexStat.  RUN TimingLog("GetIndexStat").

  RUN PutUserStat.  RUN TimingLog("PutUserStat").

/* PutUserStat populated the top-lists: */
  FOR EACH ttTopList NO-LOCK
     WHERE ttTopList.UserNumber NE ?:
    RUN CreateSelectedUser(ttTopList.UserNumber, ttTopList.TopListId).
  END.

  RUN GetSelectedUserStat.  RUN TimingLog("GetSelectedUserStat").
  RUN GetSelectedUserCache. RUN TimingLog("GetSelectedUserCache").
  RUN PutTableStat.         RUN TimingLog("PutTableStat").
  RUN PutIndexStat.         RUN TimingLog("PutIndexStat").
  RUN PutUserCache.         RUN TimingLog("PutUserCache").
  RUN PutSeqStat.           RUN TimingLog("PutSeqStat").
  RUN PutServerStat.        RUN TimingLog("PutServerStat").
  RUN PutAreaIOStat.        RUN TimingLog("PutAreaIOStat").

  IF vSnapshotCount EQ vMonCount THEN
  LEAVE.

  ASSIGN vSnapshotCount = vSnapshotCount + 1.
  PAUSE vMonIntrv NO-MESSAGE.
/*PAUSE vMonIntrv - ROUND((ETIME - vCurrETime) / 1000.0, 0) NO-MESSAGE.*/
END.

OUTPUT STREAM TimingLog CLOSE.
RUN DisableUserCache.
/* EMPTY TEMP-TABLE ttTopList. */

IF SESSION:BATCH-MODE THEN QUIT.

MESSAGE "See files:" vLogPrefix + "*"
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

