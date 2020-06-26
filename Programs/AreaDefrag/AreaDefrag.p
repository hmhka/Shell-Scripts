&SCOPED-DEFINE Use-RecCopy-DB FALSE
&SCOPED-DEFINE Allow-Defrag {&Use-RecCopy-DB}

RUN ScanArea({&Allow-Defrag}, 7, 1, ?).

/* ----------------------------------------------------------------------------
    File        : AreaDefrag.p
    Purpose     : Report the basic information about database storage objects.

    Syntax      : Connect all databases you need to process and run the program

    Description : The output file can be opened in Excel.

    Author(s)   : George Potemkin
    Created     : October 01, 2015
    Modified    : October 07, 2015
    Version     : 1.0

    The most recent version can be downloaded from here:
    ftp://ftp.progress-tech.ru/pub/Users/george/Programs/AreaDefrag.p

    Fixes       :

---------------------------------------------------------------------------- */

&SCOPED-DEFINE Sep "~t"

/* ******************************  Temp-tables  **************************** */

&IF {&Use-RecCopy-DB} NE TRUE &THEN
DEFINE TEMP-TABLE RecCopy NO-UNDO
  FIELD ProcessId   AS INTEGER /* PID */
  FIELD StatusId    AS INTEGER
  FIELD TransTime   AS DATETIME-TZ /*_Logging-CommitDelay  _Startup-BiDelay */
  FIELD TableNumber AS INTEGER
  FIELD OldRecid    AS INT64   /* RECID before defragmentation */
  FIELD NewRecid    AS INT64   /* RECID after defragmentation */
  FIELD RawRecord   AS RAW
  INDEX StatusId
        ProcessId
        StatusId
        TransTime
. /* DEFINE TEMP-TABLE RecCopy */
&ENDIF

DEFINE TEMP-TABLE ttArea NO-UNDO
  FIELD AreaNumber  LIKE _Area._Area-number
  FIELD RecPerBlock LIKE _Area._Area-recbits
  FIELD ClusterSize LIKE _Area._Area-clustersize
  FIELD Blocksize   LIKE _Area._Area-blocksize
  FIELD AreaHWM     LIKE _AreaStatus._AreaStatus-Hiwater
  INDEX AreaNumber  IS UNIQUE
        AreaNumber
. /* DEFINE TEMP-TABLE ttArea */

DEFINE TEMP-TABLE ttTable NO-UNDO
  FIELD TableId     AS INTEGER
  FIELD TableName   LIKE _File._File-Name
  FIELD TableNumber LIKE _File._File-Number
  FIELD Template    LIKE _File._Template
  FIELD CreateLimit LIKE _StorageObject._Create-Limit
  FIELD TossLimit   LIKE _StorageObject._Toss-Limit
  FIELD TemplSize   AS INTEGER
  FIELD hBuffer     AS HANDLE
  FIELD hQuery      AS HANDLE
  FIELD CanChange   AS LOGICAL
  INDEX TableName   IS UNIQUE
        TableName
  INDEX TableId IS UNIQUE
        TableId
. /* DEFINE TEMP-TABLE ttTable */

DEFINE TEMP-TABLE ttSizeStat NO-UNDO
  FIELD TableId  AS INTEGER
  FIELD RecSize  AS INTEGER
  FIELD RecCount AS INT64
  FIELD FragCount AS INT64
  INDEX RecSize  IS UNIQUE
        TableId
        RecSize
. /* DEFINE TEMP-TABLE ttSizeStat */

DEFINE TEMP-TABLE ttBlockSlots NO-UNDO
  FIELD TableId    AS INTEGER
  FIELD UsedSlots  AS INTEGER
  FIELD BlockCount AS INT64
  INDEX UsedSlots  IS UNIQUE
        TableId
        UsedSlots
. /* DEFINE TEMP-TABLE ttBlockSlots */

DEFINE TEMP-TABLE ttFragSlots NO-UNDO
  FIELD TableId   AS INTEGER
  FIELD FragSlot  AS INTEGER
  FIELD FragCount AS INT64
  INDEX FragSlot  IS UNIQUE
        TableId
        FragSlot
. /* DEFINE TEMP-TABLE ttFragSlots */

DEFINE TEMP-TABLE ttSkipClstr NO-UNDO
  FIELD Dbkey AS INT64
  INDEX Dbkey
        Dbkey
. /* DEFINE TEMP-TABLE ttSkipClstr */

DEFINE TEMP-TABLE ttSkipBlock NO-UNDO
  FIELD TableId AS INTEGER
  FIELD Dbkey   AS INT64
  INDEX Dbkey   IS UNIQUE
        Dbkey
. /* DEFINE TEMP-TABLE ttSkipBlock */

/* *****************************  Procedures  ****************************** */

DEFINE STREAM DUMP.

PROCEDURE RecDefrag:

  DEFINE INPUT PARAMETER ipMyPID LIKE _MyConnection._MyConn-Pid NO-UNDO.

  DEFINE VARIABLE vQuery     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE hRawRecord AS HANDLE    NO-UNDO.
  DEFINE VARIABLE vDumpFile  AS CHARACTER NO-UNDO.

/* Copy the records to "RecCopy" and delete them in the source db: */
  DO TRANSACTION:

    FOR EACH RecCopy EXCLUSIVE-LOCK
       WHERE RecCopy.ProcessId EQ ipMyPID
         AND RecCopy.StatusId  EQ 1, /* store only recid */

      FIRST ttTable NO-LOCK
      WHERE ttTable.TableNumber EQ RecCopy.TableNumber:

      /* Get the size of template record: */
      ASSIGN
        vQuery = SUBSTITUTE('FOR EACH &1 WHERE RECID(&1) EQ &2 EXCLUSIVE-LOCK',
                                    /*&1*/ ttTable.TableName,
                                    /*&2*/ STRING(RecCopy.OldRecid))
      . /* ASSIGN */

      ttTable.hQuery:QUERY-PREPARE(vQuery).
      ttTable.hQuery:QUERY-OPEN.
      ttTable.hQuery:GET-NEXT() NO-ERROR.
      ttTable.hQuery:QUERY-CLOSE.
      
      IF ttTable.hBuffer:LOCKED THEN
      DO:
        PUT UNFORMATTED NOW SPACE SUBSTITUTE(
          "Skipping defragmentaion of the locked record (table &1, recid &2).",
                                               /*&1*/ ttTable.TableName,
                                               /*&2*/ STRING(RecCopy.OldRecid))
        SKIP. /* PUT */
        ASSIGN RecCopy.StatusId = 0. /* = not in use */
        NEXT.
      END.

/* If the previously found record is not available now: */
      IF ttTable.hBuffer:RECORD-LENGTH EQ ? THEN
      DO:
        ASSIGN RecCopy.StatusId = 0. /* = not in use */
        NEXT.
      END.

      ASSIGN hRawRecord = RawRecord:HANDLE IN BUFFER RecCopy.
      IF ttTable.hBuffer:RAW-TRANSFER(TRUE, hRawRecord) THEN
      IF ttTable.hBuffer:BUFFER-DELETE THEN
      DO:
        ASSIGN RecCopy.StatusId = 2. /* = Store a fragmented record that */
        NEXT.                        /*   was deleted from a source db.  */
      END.

/* If RAW-TRANSFER or BUFFER-DELETE fails: */
      PUT UNFORMATTED NOW SPACE SUBSTITUTE(
  "Error: Failed to copy the fargmented record (table &1, recid &2). Exiting.",
                                               /*&1*/ ttTable.TableName,
                                               /*&2*/ STRING(RecCopy.OldRecid))
      SKIP. /* PUT */
      UNDO, RETURN.

    END. /* FOR EACH RecCopy, FIRST ttTable */
  END. /* DO TRANSACTION */

/* Re-create the records in the source db: */
  DO TRANSACTION:
    FOR EACH RecCopy EXCLUSIVE-LOCK
       WHERE RecCopy.ProcessId EQ ipMyPID
         AND RecCopy.StatusId  EQ 2, /* = record stored in the RecCopy */
    
      FIRST ttTable NO-LOCK
      WHERE ttTable.TableNumber EQ RecCopy.TableNumber:
    
      ttTable.hBuffer:BUFFER-CREATE.
      IF ttTable.hBuffer:RAW-TRANSFER(FALSE, hRawRecord) THEN
      ASSIGN RecCopy.TransTime = NOW
             RecCopy.StatusId  = 0  /* = can be re-used (after -Mf delay) */
             RecCopy.NewRecid  = ttTable.hBuffer:RECID
      . /* ASSIGN */
      ELSE
/* If RAW-TRANSFER fails then: */
      DO:
/* Keep the record in RecCopy: */
        ASSIGN RecCopy.StatusId = 3. /* keep copy until the problem is fixed */
               vDumpFile = SUBSTITUTE("dump.&1.&2.&3.raw",
                                              /*&1*/ LDBNAME("DCITDB"),
                                              /*&1*/ ttTable.TableName,
                                              /*&2*/ STRING(RecCopy.OldRecid))
        . /* ASSIGN */

/* Dump the record in raw format: */
        OUTPUT STREAM DUMP TO VALUE(vDumpFile).
        PUT STREAM DUMP CONTROL RecCopy.RawRecord.
        OUTPUT STREAM DUMP CLOSE.

        PUT UNFORMATTED NOW SPACE SUBSTITUTE(
      "Error: Failed to re-create the fargmented record (table &1, recid &2).",
                                               /*&1*/ ttTable.TableName,
                                               /*&2*/ STRING(RecCopy.OldRecid))
        SKIP NOW SPACE "Record dumped to " vDumpFile
        SKIP. /* PUT */

        
      END. /* RAW-TRANSFER fails */

    END. /* FOR EACH RecCopy, FIRST ttTable */
  END. /* DO TRANSACTION */
END PROCEDURE. /* RecDefrag */

/* ------------------------------------------------------------------------- */

PROCEDURE ScanArea:

  DEFINE INPUT PARAMETER ipRecDefrag     AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipAreaNumber    AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipFromOrToBlock AS INT64   NO-UNDO.
  DEFINE INPUT PARAMETER ipNumOfBlocks   AS INT64   NO-UNDO.

/*
  DEFINE INPUT PARAMETER ipFirstBlock AS INT64   NO-UNDO.
  DEFINE INPUT PARAMETER ipLastBlock  AS INT64   NO-UNDO.
*/
  
  DEFINE VARIABLE vMyPID        LIKE _MyConnection._MyConn-Pid NO-UNDO.
  DEFINE VARIABLE vMyAccessId   LIKE _UserIO._UserIO-Id        NO-UNDO.
  DEFINE VARIABLE vMyDbAccess   LIKE _UserIO._UserIO-DbAccess  NO-UNDO.
  DEFINE VARIABLE vBiDelay      LIKE _Startup._Startup-BiDelay NO-UNDO.
  DEFINE VARIABLE vMaxRecSize   AS INTEGER   NO-UNDO INITIAL 32000. /* bytes */
  DEFINE VARIABLE vReportBlock  AS INTEGER   NO-UNDO INITIAL 1000. /* blocks */
  DEFINE VARIABLE vCopyLimit    AS INTEGER   NO-UNDO INITIAL 100.  /* rec/tx */
  DEFINE VARIABLE vCopyCount    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vDefragCount  AS INT64     NO-UNDO.
  DEFINE VARIABLE vRecSize      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vUsedSize     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMaxUsedSize  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vRecPerBlock  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vRecPerClstr  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vClusterSize  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vCluster      AS INT64     NO-UNDO.
  DEFINE VARIABLE vFirstCluster AS INT64     NO-UNDO.
  DEFINE VARIABLE vLastCluster  AS INT64     NO-UNDO.
  DEFINE VARIABLE vBlock        AS INT64     NO-UNDO.
  DEFINE VARIABLE vFirstBlock   AS INT64     NO-UNDO.
  DEFINE VARIABLE vLastBlock    AS INT64     NO-UNDO.
  DEFINE VARIABLE vDbkey        AS INT64     NO-UNDO.
  DEFINE VARIABLE vRecid        AS INT64     NO-UNDO.
  DEFINE VARIABLE vSlot         AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMaxSlot      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLastSlot     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vSwitchTable  AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE vTableCount   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTableId      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vCurrTable    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vQuery        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFilePrefix   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vReportFile   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vBgnTime      AS DATETIME  NO-UNDO.
  DEFINE VARIABLE i             AS INTEGER   NO-UNDO.

  ASSIGN vBgnTime    = NOW
         vFilePrefix = ISO-DATE(vBgnTime)
         vFilePrefix = REPLACE(vFilePrefix, ":":U, ".":U)
         vFilePrefix = SUBSTRING(vFilePrefix, 1, 19)
         vFilePrefix = SUBSTITUTE("&1.area_&2.&3",
                                 /*&1*/ LDBNAME("DICTDB":U),
                                 /*&2*/ STRING(ipAreaNumber),
                                 /*&3*/ vFilePrefix)
         vReportFile = vFilePrefix + ".log"
  . /* ASSIGN */

  OUTPUT TO VALUE(vReportFile).

  FOR FIRST DICTDB._MyConnection NO-LOCK:

    ASSIGN vMyAccessId   = DICTDB._MyConnection._MyConn-UserId + 1
           vMyPID        = DICTDB._MyConnection._MyConn-Pid
    . /* ASSIGN */

    PUT UNFORMATTED
      NOW SPACE SUBSTITUTE(
      "Session PID: &1, Defrag: &2",
                  /*&1*/ STRING(DICTDB._MyConnection._MyConn-Pid),
                  /*&2*/ STRING(ipRecDefrag, "enabled/not enabled"))
    SKIP
      NOW SPACE "Working db:" DBPARAM("DICTDB":U)
    SKIP. /* PUT */

  END. /* FOR FIRST _MyConnection */

  IF LDBNAME(BUFFER RecCopy) EQ LDBNAME("DICTDB") THEN
  DO:
    PUT UNFORMATTED
      "Error: Working database contains the RecCopy table. Exiting."
    SKIP. /* PUT */
    OUTPUT CLOSE.
    QUIT.
  END.

  IF NOT CAN-FIND(FIRST DICTDB._Area
                  WHERE DICTDB._Area._Area-number EQ ipAreaNumber) THEN
  DO:
    PUT UNFORMATTED SUBSTITUTE(
      "Error: Working database does not contain the area number &1. Exiting.",
                                                  /*&1*/ STRING(ipAreaNumber))
    SKIP. /* PUT */
    OUTPUT CLOSE.
    QUIT.
  END.

  FOR FIRST DICTDB._Logging NO-LOCK,

      FIRST DICTDB._Area NO-LOCK
      WHERE DICTDB._Area._Area-number EQ ipAreaNumber,
    
      FIRST DICTDB._AreaStatus NO-LOCK
      WHERE DICTDB._AreaStatus._AreaStatus-Areanum EQ DICTDB._Area._Area-number:

    IF ipFromOrToBlock LT 0 THEN
    DO:
      PUT UNFORMATTED
"Error: ipFromOrToBlock in ScanArea() procedure can't be less than zero. Exiting."
      SKIP. /* PUT */
      OUTPUT CLOSE.
      QUIT.
    END.

    IF ipNumOfBlocks GT 0 THEN /* ipFromOrToBlock is a first block */
    ASSIGN vFirstBlock = 1
           vFirstBlock = ipFromOrToBlock WHEN ipFromOrToBlock NE ?
           vLastBlock  = vFirstBlock + ipNumOfBlocks - 1
    . /* ASSIGN */
    ELSE
    IF ipNumOfBlocks LT 0 THEN /* ipFromOrToBlock is a last block */
    ASSIGN vLastBlock  = DICTDB._AreaStatus._AreaStatus-Hiwater
           vLastBlock  = ipFromOrToBlock WHEN ipFromOrToBlock NE ?
           vFirstBlock = vLastBlock + ipNumOfBlocks
    . /* ASSIGN */
    ELSE
    IF ipNumOfBlocks EQ ? THEN /* ipFromOrToBlock is a last block */
    ASSIGN vFirstBlock = 1
           vFirstBlock = ipFromOrToBlock WHEN ipFromOrToBlock NE ?
           vLastBlock  = DICTDB._AreaStatus._AreaStatus-Hiwater
    . /* ASSIGN */
    ELSE
/* IF ipNumOfBlocks EQ 0 THEN */
    DO:
      PUT UNFORMATTED
        "Error: ipNumOfBlocks in ScanArea() procedure can't be zero. Exiting."
      SKIP. /* PUT */
      OUTPUT CLOSE.
      QUIT.
    END.

    ASSIGN vTableCount   = 0
           vCopyCount    = 0
           vDefragCount  = 0
           vBiDelay      = DICTDB._Logging._Logging-CommitDelay
           vRecPerBlock  = EXP(2, DICTDB._Area._Area-recbits)
           vRecPerClstr  = vRecPerBlock * DICTDB._Area._Area-clustersize
           vClusterSize  = DICTDB._Area._Area-clustersize
           vFirstCluster = TRUNCATE((vFirstBlock - 1) / vClusterSize, 0)
/* First cluster (starting from block 1) is owned by Master object. Skip it: */
           vFirstCluster = MAX(vFirstCluster, 1)
           vLastCluster  = TRUNCATE((vLastBlock - 1) / vClusterSize, 0)
           vLastCluster  = MAX(vFirstCluster, vLastCluster)
           vReportBlock  = vReportBlock / vClusterSize
           vReportBlock  = MAX(vReportBlock, 1)
           vFirstBlock   = vFirstCluster * vClusterSize + 1
           vLastBlock    = vLastCluster  * vClusterSize + vClusterSize - 1
           vMaxSlot      = vRecPerBlock  - 1
    . /* ASSIGN */

    PUT UNFORMATTED NOW SPACE
    SUBSTITUTE('Area: "&1":&2,&3;&4, Blocks from &5 to &6, Blocksize: &7',
              /*&1*/ DICTDB._Area._Area-name,
              /*&2*/ STRING(DICTDB._Area._Area-number),
              /*&3*/ STRING(vRecPerBlock),
              /*&4*/ STRING(DICTDB._Area._Area-clustersize),
              /*&5*/ STRING(vFirstBlock),
              /*&6*/ STRING(vLastBlock),
              /*&7*/ STRING(DICTDB._Area._Area-blocksize))
    SKIP. /* PUT */
    PUT CONTROL NULL(0).

    IF DICTDB._Area._Area-clustersize LE 1 THEN
    DO:
      PUT UNFORMATTED
        SUBSTITUTE("Area &1 (&2) is type I storage area. Exiting...",
                            /*&1*/ STRING(DICTDB._Area._Area-number),
                            /*&2*/        DICTDB._Area._Area-name)
      SKIP.
      OUTPUT CLOSE.
      QUIT.
    END.

    DO TRANSACTION:
      CREATE ttArea.
      ASSIGN ttArea.AreaNumber  = DICTDB._Area._Area-number
             ttArea.RecPerBlock = EXP(2, DICTDB._Area._Area-recbits)
             ttArea.ClusterSize = DICTDB._Area._Area-clustersize
             ttArea.Blocksize   = DICTDB._Area._Area-blocksize
             ttArea.AreaHWM     = DICTDB._AreaStatus._AreaStatus-Hiwater
             vMaxUsedSize       = DICTDB._Area._Area-blocksize - 64
      . /* ASSIGN */
    END.

    FOR EACH DICTDB._StorageObject NO-LOCK
       WHERE DICTDB._StorageObject._Area-number EQ DICTDB._Area._Area-number:

      CASE DICTDB._StorageObject._Object-type:
        WHEN 1 THEN
        FOR FIRST DICTDB._File NO-LOCK
            WHERE DICTDB._File._File-Number EQ
                  DICTDB._StorageObject._Object-number
        TRANSACTION:

          CREATE ttTable.
          ASSIGN ttTable.TableName   = DICTDB._File._File-Name
                 ttTable.TableNumber = DICTDB._File._File-Number
                 ttTable.Template    = DICTDB._File._Template
                 ttTable.CreateLimit = DICTDB._StorageObject._Create-Limit
                 ttTable.TossLimit   = DICTDB._StorageObject._Toss-Limit
                 ttTable.TableId     = vTableCount
                 vTableCount         = vTableCount + 1
          . /* ASSIGN */

          CREATE QUERY  ttTable.hQuery.
          CREATE BUFFER ttTable.hBuffer FOR TABLE ttTable.TableName.

          IF NOT ttTable.hBuffer:CAN-READ THEN
          DO:
            PUT UNFORMATTED NOW SPACE SUBSTITUTE(
          "User &1 does not have the permissions to read table &2. Exiting...",
                                  /*&1*/ USERID("DICTDB":U),
                                  /*&2*/ DICTDB._File._File-Name)
            SKIP.
            OUTPUT CLOSE.
            QUIT.
          END.

          ttTable.hBuffer:DISABLE-DUMP-TRIGGERS().
          ttTable.hBuffer:DISABLE-LOAD-TRIGGERS(TRUE). /* allow replication */
          ttTable.hQuery:SET-BUFFERS(hBuffer).

/* Get the size of template record: */
          ASSIGN 
            ttTable.CanChange = ttTable.hBuffer:CAN-CREATE
                            AND ttTable.hBuffer:CAN-DELETE
            vQuery = SUBSTITUTE('FOR EACH &1 WHERE RECID(&1) EQ &2 NO-LOCK',
                                        /*&1*/  ttTable.TableName,
                                        /*&2*/  STRING(ttTable.Template))
          . /* ASSIGN */

          IF NOT ttTable.CanChange THEN
          PUT UNFORMATTED NOW SPACE SUBSTITUTE(
          "Warning: User &1 does not have the permissions to modify table &2.",
                       /*&1*/ USERID("DICTDB":U),
                       /*&2*/ DICTDB._File._File-Name)
          SKIP.

          IF ttTable.hBuffer:HAS-LOBS THEN
          DO:
            PUT UNFORMATTED NOW SPACE SUBSTITUTE(
      "Warning: Table &1 has LOB fields. Its defragmentation will be skipped.",
                    /*&1*/ DICTDB._File._File-Name)
            SKIP.
            ASSIGN ttTable.CanChange = FALSE.
          END.

          ttTable.hQuery:QUERY-PREPARE(vQuery).
          ttTable.hQuery:QUERY-OPEN.
          ttTable.hQuery:GET-NEXT() NO-ERROR.
          ASSIGN ttTable.TemplSize = ttTable.hBuffer:RECORD-LENGTH.
          ttTable.hQuery:QUERY-CLOSE.

/* Create the records to store the statistics of record sizes: */
          DO i = 1 TO vMaxRecSize:
            CREATE ttSizeStat.
            ASSIGN ttSizeStat.TableId   = ttTable.TableId
                   ttSizeStat.RecSize   = i
                   ttSizeStat.RecCount  = 0
                   ttSizeStat.FragCount = 0
            . /* ASSIGN */
          END.
          
/* Statistics of the used slots per block: */
          DO i = 1 TO vRecPerBlock:
            CREATE ttBlockSlots.
            ASSIGN ttBlockSlots.TableId    = ttTable.TableId
                   ttBlockSlots.UsedSlots  = i
                   ttBlockSlots.BlockCount = 0
            . /* ASSIGN */
          END.

/* Statistics of the slots that store the fragmented records: */
          DO i = 0 TO vMaxSlot:
            CREATE ttFragSlots.
            ASSIGN ttFragSlots.TableId   = ttTable.TableId
                   ttFragSlots.FragSlot  = i
                   ttFragSlots.FragCount = 0
            . /* ASSIGN */
          END.
/*
Blocks in the cluster with table's object block (bkObjDbkey: 4096)

Block 129, bkDbkey 4096, bk_type 12 (Object Block)
Block 130, bkDbkey 4128, bk_type 16 (Cluster List Block)
Block 131, bkDbkey 4160, bk_type 17 (Object allocation Block)
Block 132, bkDbkey 4192, bk_type 15 (Cluster Allocation Block)
Block 133, bkDbkey 4224, bk_type  3 (Data Block)
*/        CREATE ttSkipBlock.
          ASSIGN vDbkey = DICTDB._StorageObject._Object-block
                 vDbkey = vDbkey - (vDbkey MODULO vRecPerBlock)
                 ttSkipBlock.TableId = ttTable.TableId
                 ttSkipBlock.Dbkey   = vDbkey
          . /* ASSIGN */
        END. /* WHEN 1 */

        WHEN 2 THEN
        DO TRANSACTION:
          CREATE ttSkipClstr.
          ASSIGN vDbkey = DICTDB._StorageObject._Object-block
                 vDbkey = vDbkey - (vDbkey MODULO vRecPerClstr)
                 ttSkipClstr.Dbkey = vDbkey
          . /* ASSIGN */

          CREATE ttSkipClstr.
          ASSIGN vDbkey = DICTDB._StorageObject._Object-root
                 vDbkey = vDbkey - (vDbkey MODULO vRecPerClstr)
                 ttSkipClstr.Dbkey = vDbkey
          . /* ASSIGN */
        END. /* WHEN 2 */

        OTHERWISE
        DO TRANSACTION:
          CREATE ttSkipClstr.
          ASSIGN vDbkey = DICTDB._StorageObject._Object-block
                 vDbkey = vDbkey - (vDbkey MODULO vRecPerClstr)
                 ttSkipClstr.Dbkey = vDbkey
          . /* ASSIGN */
        END. /* OTHERWISE */

      END CASE. /* DICTDB._StorageObject._Object-type */
    END. /* FOR EACH DICTDB._StorageObject */

    IF vTableCount EQ 0 THEN
    DO:
      PUT UNFORMATTED NOW SPACE
        SUBSTITUTE("There are no tables in area &1 (&2). Exiting...",
                  /*&1*/ STRING(DICTDB._Area._Area-number),
                  /*&2*/ DICTDB._Area._Area-name)
      SKIP.
      OUTPUT CLOSE.
      QUIT.
    END.

  END. /* FOR FIRST _Logging, _Area, _AreaStatus */

/* Set the current table as the one with minimal recid of template record: */
  FOR EACH ttTable NO-LOCK BY ttTable.Template:
    LEAVE.
  END.

  PUT UNFORMATTED NOW SPACE
    SUBSTITUTE('Scanning blocks from &1 to &2...',
              /*&1*/ STRING(vFirstBlock), /*&2*/ STRING(vLastBlock))
  SKIP. /* PUT */

DataCluster:
  REPEAT vCluster = vFirstCluster TO vLastCluster:

    ASSIGN vFirstBlock  = vCluster * vClusterSize
           vLastBlock   = vFirstBlock + vClusterSize - 1
           vDbkey       = vFirstBlock * vRecPerBlock
           vCurrTable   = ttTable.TableId
           vSwitchTable = vTableCount GT 1
    . /* ASSIGN */

    IF CAN-FIND(FIRST ttSkipClstr WHERE ttSkipClstr.Dbkey EQ vDbkey) THEN
    NEXT DataCluster.

/* If cluster contains the table's object block and 3 other non-data blocks: */
    FOR FIRST ttSkipBlock NO-LOCK
        WHERE ttSkipBlock.Dbkey EQ vDbkey:

      FIND FIRST ttTable NO-LOCK
           WHERE ttTable.TableId EQ ttSkipBlock.TableId.
      ASSIGN vCurrTable  = ttTable.TableId
             vFirstBlock = vFirstBlock + 4
      . /* ASSIGN */
    END.

DataBlock:
    REPEAT vBlock = vFirstBlock TO vLastBlock:
      ASSIGN vDbkey    = vBlock * vRecPerBlock
             vUsedSize = 0
             vLastSlot = ?
      . /* ASSIGN */

RecordSlot:
      REPEAT vSlot = 0 TO vMaxSlot:

        IF RETRY THEN
        DO:
          ASSIGN vTableId = vTableId + 1
                 vTableId = vTableId MODULO vTableCount
          . /* ASSIGN */

          FIND FIRST ttTable NO-LOCK
               WHERE ttTable.TableId EQ vTableId.

/* All tables were checked: */
          IF vTableId EQ vCurrTable THEN
          NEXT RecordSlot.
        END.

        ASSIGN
          vRecid = vDbkey + vSlot
          vQuery = SUBSTITUTE('FOR EACH &1 WHERE RECID(&1) EQ &2 NO-LOCK',
                       /*&1*/  ttTable.TableName, /*&2*/  STRING(vRecid))
        . /* ASSIGN */

        ttTable.hQuery:QUERY-PREPARE(vQuery).
        ttTable.hQuery:QUERY-OPEN.

        FOR FIRST DICTDB._UserIO NO-LOCK
            WHERE DICTDB._UserIO._UserIO-Id EQ vMyAccessId:
          ASSIGN vMyDbAccess = DICTDB._UserIO._UserIO-DbAccess.
        END.

        ttTable.hQuery:GET-NEXT() NO-ERROR.
        ttTable.hQuery:QUERY-CLOSE.

        ASSIGN vRecSize = hBuffer:RECORD-LENGTH.

/* Record slot is not used or does not belong to the current table: */
        IF vRecSize EQ ? THEN
        DO:
/* It's allowed to swith a table only for first record in data cluster: */
          IF vSwitchTable THEN
          UNDO RecordSlot, RETRY RecordSlot.

/* If the last checked block does not store a record and free space in
   the block is not enough to store a record at least with current (per block)
   minimum size then don't check the rest slots and go to the next block.
   It's not 100% accurate approach but it may save the time by eliminating
   the checks of the unused slots if RPB set higher than it's really required.
*/        IF vUsedSize GE vMaxUsedSize - ttTable.TossLimit
          THEN LEAVE RecordSlot.
          ELSE NEXT  RecordSlot.
        END.

/* Record is found: ----------------------------------------------------- */

/* RECORD-LENGTH() function does not count two bytes that store the size
   of a record fragment (counted by tabanalys as a part of record size)
   as well as two bytes store a record offset in the data block's header:
*/      ASSIGN vUsedSize = vUsedSize + vRecSize + 4.

        FOR FIRST DICTDB._UserIO NO-LOCK
            WHERE DICTDB._UserIO._UserIO-Id EQ vMyAccessId:
          ASSIGN vMyDbAccess  = DICTDB._UserIO._UserIO-DbAccess - vMyDbAccess
                 vLastSlot    = vSlot
                 vSwitchTable = FALSE
          . /* ASSIGN */
        END.

/* Double check for fragmented records (only for first block in the cluster):
   After the program switches a table to identify the owner of the block's
   records the _UserIO-DbAccess is /sometimes/ increased by 2 instead of by 1
   even for the non-fragmented records. Second read returns the correct value.
*/      IF vMyDbAccess GT 1 AND vBlock EQ vFirstBlock THEN
        DO:
          ttTable.hQuery:QUERY-PREPARE(vQuery).
          ttTable.hQuery:QUERY-OPEN.
          
          FOR FIRST DICTDB._UserIO NO-LOCK
              WHERE DICTDB._UserIO._UserIO-Id EQ vMyAccessId:
            ASSIGN vMyDbAccess = DICTDB._UserIO._UserIO-DbAccess.
          END.
        
          ttTable.hQuery:GET-NEXT() NO-ERROR.
          ttTable.hQuery:QUERY-CLOSE.
        
          FOR FIRST DICTDB._UserIO NO-LOCK
              WHERE DICTDB._UserIO._UserIO-Id EQ vMyAccessId:
            ASSIGN vMyDbAccess = DICTDB._UserIO._UserIO-DbAccess - vMyDbAccess.
          END.
        END.

        FOR FIRST ttSizeStat EXCLUSIVE-LOCK
            WHERE ttSizeStat.TableId EQ ttTable.TableId
              AND ttSizeStat.RecSize EQ vRecSize
        TRANSACTION:
          ASSIGN  ttSizeStat.RecCount  = ttSizeStat.RecCount  + 1
                  ttSizeStat.FragCount = ttSizeStat.FragCount + 1
                                            WHEN vMyDbAccess GT 1
          . /* ASSIGN */
        END. /* FOR FIRST ttSizeStat  */

        IF vMyDbAccess GT 1 THEN
        FOR FIRST ttFragSlots EXCLUSIVE-LOCK
            WHERE ttFragSlots.TableId EQ ttTable.TableId
              AND ttFragSlots.FragSlot EQ vLastSlot
        TRANSACTION:
          ASSIGN  ttFragSlots.FragCount = ttFragSlots.FragCount + 1.

          IF ipRecDefrag       EQ FALSE
          OR ttTable.CanChange EQ FALSE
          OR vRecSize GE vMaxUsedSize THEN
          NEXT RecordSlot.

          FIND FIRST RecCopy EXCLUSIVE-LOCK
            WHERE RecCopy.ProcessId EQ vMyPID /* INDEX StatusId */
              AND RecCopy.StatusId  EQ 0 /* = not in use */
              AND RecCopy.TransTime LT ADD-INTERVAL(NOW, - vBiDelay,
                                                    "seconds":U)
          NO-ERROR.

          IF NOT AVAILABLE RecCopy THEN
          CREATE RecCopy.
          ASSIGN RecCopy.ProcessId   = vMyPID
                 RecCopy.StatusId    = 1 /* store only recid */
                 RecCopy.TransTime   = ?
                 RecCopy.TableNumber = ttTable.TableNumber
                 RecCopy.OldRecid    = vRecid
                 RecCopy.NewRecid    = ?
                 RecCopy.RawRecord   = ?
                 vCopyCount = vCopyCount + 1
          . /* ASSIGN */

          IF vCopyCount LT vCopyLimit THEN
          NEXT RecordSlot.

          RUN RecDefrag(vMyPID).
          ASSIGN vDefragCount = vDefragCount + vCopyCount
                 vCopyCount = 0
          . /* ASSIGN */

        END. /* IF vMyDbAccess GT 1 */

      END. /* RecordSlot: REPEAT */

      FOR FIRST ttBlockSlots EXCLUSIVE-LOCK
          WHERE ttBlockSlots.TableId   EQ ttTable.TableId
            AND ttBlockSlots.UsedSlots EQ (vLastSlot  + 1)
      TRANSACTION:
        ASSIGN  ttBlockSlots.BlockCount = ttBlockSlots.BlockCount + 1.
      END. /* FOR FIRST ttSizeStat  */

    END. /* DataBlock: REPEAT */

    IF vCluster MODULO vReportBlock EQ 0 THEN
    DO:
      FOR EACH ttSizeStat NO-LOCK
          WHERE ttSizeStat.RecCount GT 0:
        ACCUMULATE ttSizeStat.RecCount (TOTAL).
      END. /* FOR FIRST ttSizeStat  */

      FOR EACH ttFragSlots NO-LOCK
         WHERE ttFragSlots.FragCount GT 0:
        ACCUMULATE ttFragSlots.FragCount (TOTAL).
      END. /* FOR FIRST ttFragSlots  */

      PUT UNFORMATTED NOW SPACE SUBSTITUTE(
        "Processed &1 blocks, &2 records. &3 fragmented record(s) found.",
                 /*&1*/ STRING(vCluster * vClusterSize),
                 /*&2*/ STRING(ACCUM TOTAL ttSizeStat.RecCount),
                 /*&3*/ STRING(ACCUM TOTAL ttFragSlots.FragCount))
      SKIP.
      PUT CONTROL NULL(0).
      PROCESS EVENTS.
    END.
  END. /* DataCluster: REPEAT */

  IF vCopyCount GT 0 THEN
  RUN RecDefrag(vMyPID).

  ASSIGN vDefragCount = vDefragCount + vCopyCount.


/* Remove a template record from total size statistics: */
  FOR EACH ttTable NO-LOCK,
     FIRST ttSizeStat EXCLUSIVE-LOCK
     WHERE ttSizeStat.TableId EQ ttTable.TableId
       AND ttSizeStat.RecSize EQ ttTable.TemplSize
  TRANSACTION:
    ASSIGN ttSizeStat.RecCount = ttSizeStat.RecCount - 1.
    DELETE OBJECT hBuffer.
    DELETE OBJECT hQuery.
  END. /* FOR EACH ttTable, FIRST ttSizeStat */

/* Report the scan summary: */
  FOR EACH ttSizeStat NO-LOCK
     WHERE ttSizeStat.RecCount GT 0:
    ACCUMULATE ttSizeStat.RecCount (TOTAL).
  END. /* FOR FIRST ttSizeStat  */
  
  FOR EACH ttFragSlots NO-LOCK
     WHERE ttFragSlots.FragCount GT 0:
    ACCUMULATE ttFragSlots.FragCount (TOTAL).
  END. /* FOR FIRST ttFragSlots  */
  
  PUT UNFORMATTED NOW SPACE SUBSTITUTE(
     "Processed &1 blocks, &2 records. &3 fragmented record(s) found.",
              /*&1*/ STRING(vLastBlock),
              /*&2*/ STRING(ACCUM TOTAL ttSizeStat.RecCount),
              /*&3*/ STRING(ACCUM TOTAL ttFragSlots.FragCount))
  SKIP NOW SPACE SUBSTITUTE("Scan completed. Elapsed time: &1",
              /*&1*/ STRING(INTERVAL(NOW, vBgnTime, 'milliseconds') / 1000.0))
  SKIP NOW SPACE SUBSTITUTE("Summary: &1 record(s) were defragmented.",
              /*&1*/ STRING(vDefragCount))
  SKIP. /* PUT */
  
  RUN ReportTableStat(vFilePrefix).
  RUN ReportSizeStat (vFilePrefix).
  RUN ReportSlotStat (vFilePrefix).


  EMPTY TEMP-TABLE ttArea       NO-ERROR.
  EMPTY TEMP-TABLE ttTable      NO-ERROR.
  EMPTY TEMP-TABLE ttSizeStat   NO-ERROR.
  EMPTY TEMP-TABLE ttBlockSlots NO-ERROR.
  EMPTY TEMP-TABLE ttFragSlots  NO-ERROR.
  EMPTY TEMP-TABLE ttSkipClstr  NO-ERROR.
  EMPTY TEMP-TABLE ttSkipBlock  NO-ERROR.

END PROCEDURE. /* ScanArea */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportTableStat:

  DEFINE INPUT PARAMETER ipFilePrefix AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vReportFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vRecSize    AS INT64     NO-UNDO.
  DEFINE VARIABLE vBlkCount   AS INT64     NO-UNDO.
  DEFINE VARIABLE vRecCount   AS INT64     NO-UNDO.
  DEFINE VARIABLE vFrgCount   AS INT64     NO-UNDO.
  DEFINE VARIABLE vUsedSlots  AS INT64     NO-UNDO.

  ASSIGN vReportFile = ipFilePrefix + ".tables.txt".

  OUTPUT TO VALUE(vReportFile).

  PUT UNFORMATTED
           "Table"
    {&Sep} "Records"
    {&Sep} "Fragments"
    {&Sep} "Deletes"
    {&Sep} "Table Size"
    {&Sep} "%Util"
    {&Sep} "Blocks"
    {&Sep} "%HWM"
    {&Sep} "Mean RPB"
    {&Sep} "Template"
    {&Sep} "MinRecSize"
    {&Sep} "MaxRecSize"
    {&Sep} "MeanRecSize"
  SKIP. /*PUT */

  FOR FIRST ttArea  NO-LOCK,
       EACH ttTable NO-LOCK:

/* Size statistics: */
    FOR EACH ttSizeStat NO-LOCK
       WHERE ttSizeStat.TableId  EQ ttTable.TableId
         AND ttSizeStat.RecCount GT 0:

      ASSIGN vRecSize = ttSizeStat.RecSize * ttSizeStat.RecCount.
      ACCUMULATE vRecSize             (TOTAL)
                 ttSizeStat.RecSize   (MIN MAX)
                 ttSizeStat.FragCount (TOTAL)
                 ttSizeStat.RecCount  (TOTAL)
      . /* ACCUMULATE */
    END. /* FOR EACH ttSizeStat */

/* Slot statistics: */
    FOR EACH ttBlockSlots NO-LOCK
       WHERE ttBlockSlots.TableId    EQ ttTable.TableId
         AND ttBlockSlots.BlockCount GT 0:

      ASSIGN vUsedSlots = ttBlockSlots.UsedSlots * ttBlockSlots.BlockCount.
      ACCUMULATE vUsedSlots (TOTAL)
                 ttBlockSlots.BlockCount (TOTAL)
      . /* ACCUMULATE */
    END. /* FOR EACH ttBlockSlots */

    ASSIGN vRecSize   = ACCUM TOTAL vRecSize
           vBlkCount  = ACCUM TOTAL ttBlockSlots.BlockCount
           vRecCount  = ACCUM TOTAL ttSizeStat.RecCount
           vFrgCount  = ACCUM TOTAL ttSizeStat.FragCount
           vUsedSlots = ACCUM TOTAL vUsedSlots
    . /* ASSIGN */

    PUT UNFORMATTED
             ttTable.TableName                  /* Table        */
      {&Sep} vRecCount                          /* Records      */
      {&Sep} vFrgCount                          /* Fragments    */
      {&Sep} vUsedSlots - vRecCount - vFrgCount - 1 /* Deletes  */
      {&Sep} vRecSize                           /* Table Size   */
      {&Sep} (vRecSize + 4 * vRecCount)         /* %Util        */
           / (ttArea.Blocksize - 64) / vBlkCount * 100.0
      {&Sep} vBlkCount                          /* Table Blocks */
      {&Sep} vBlkCount / ttArea.AreaHWM * 100.0 /* %HWM         */
      {&Sep} vUsedSlots / vBlkCount             /* Mean RPB     */
      {&Sep}              ttTable.TemplSize     /* Template     */
      {&Sep} (ACCUM MIN   ttSizeStat.RecSize)   /* MinRecSize   */ 
      {&Sep} (ACCUM MAX   ttSizeStat.RecSize)   /* MaxRecSize   */ 
      {&Sep} vRecSize / vRecCount               /* MeanRecSize  */ 
           
      SKIP. /*PUT */
  END. /* FOR EACH ttTable */

  OUTPUT CLOSE.

END PROCEDURE. /* ReportTableStat */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportSizeStat:

  DEFINE INPUT PARAMETER ipFilePrefix AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vReportFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vMinRecSize AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMaxRecSize AS INTEGER   NO-UNDO.

  ASSIGN vReportFile = ipFilePrefix + ".sizes.txt".

  OUTPUT TO VALUE(vReportFile).

  PUT UNFORMATTED
           "Table"
    {&Sep} "Rec Size"
    {&Sep} "Records"
    {&Sep} "Fragments"
  SKIP. /*PUT */

  FOR EACH ttTable NO-LOCK:

    FOR EACH ttSizeStat NO-LOCK
       WHERE ttSizeStat.TableId  EQ ttTable.TableId
         AND ttSizeStat.RecCount GT 0
          BY ttSizeStat.TableId
          BY ttSizeStat.RecSize:
      ASSIGN vMinRecSize = ttSizeStat.RecSize.
      LEAVE.
    END. /* FOR FIRST ttSizeStat  */

    FOR EACH ttSizeStat NO-LOCK
       WHERE ttSizeStat.TableId  EQ ttTable.TableId
         AND ttSizeStat.RecCount GT 0
          BY ttSizeStat.TableId DESCENDING
          BY ttSizeStat.RecSize DESCENDING:
      ASSIGN vMaxRecSize = ttSizeStat.RecSize.
      LEAVE.
    END. /* FOR FIRST ttSizeStat  */

    FOR EACH ttSizeStat NO-LOCK
       WHERE ttSizeStat.TableId EQ ttTable.TableId
         AND ttSizeStat.RecSize GE vMinRecSize
         AND ttSizeStat.RecSize LE vMaxRecSize
          BY ttSizeStat.TableId
          BY ttSizeStat.RecSize:
      PUT UNFORMATTED
               ttTable.TableName   /* Table     */
        {&Sep} ttSizeStat.RecSize  /* Rec Size  */
        {&Sep} ttSizeStat.RecCount /* Records   */
        {&Sep} ttSizeStat.FragCount /* Fragments */
      SKIP. /*PUT */
    END. /* FOR FIRST ttSizeStat  */

  END. /* FOR EACH ttTable */

  OUTPUT CLOSE.

END PROCEDURE. /* ReportSizeStat */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportSlotStat:

  DEFINE INPUT PARAMETER ipFilePrefix AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vReportFile AS CHARACTER NO-UNDO.

  ASSIGN vReportFile = ipFilePrefix + ".slots.txt".

  OUTPUT TO VALUE(vReportFile).

  PUT UNFORMATTED
           "Table"
    {&Sep} "Slots"
    {&Sep} "Count"
    {&Sep} "Type"
  SKIP. /*PUT */

  FOR EACH ttTable NO-LOCK:

    FOR EACH ttBlockSlots NO-LOCK
       WHERE ttBlockSlots.TableId    EQ ttTable.TableId
         AND ttBlockSlots.BlockCount NE 0
          BY ttBlockSlots.TableId
          BY ttBlockSlots.UsedSlots:
      PUT UNFORMATTED
               ttTable.TableName       /* Table */
        {&Sep} ttBlockSlots.UsedSlots  /* Slots */
        {&Sep} ttBlockSlots.BlockCount /* Count */
        {&Sep} "block(s)"
      SKIP. /*PUT */
    END. /* FOR FIRST ttBlockSlots  */

    FOR EACH ttFragSlots NO-LOCK
       WHERE ttFragSlots.TableId   EQ ttTable.TableId
         AND ttFragSlots.FragCount NE 0
          BY ttFragSlots.TableId
          BY ttFragSlots.FragSlot:
      PUT UNFORMATTED
               ttTable.TableName     /* Table */
        {&Sep} ttFragSlots.FragSlot  /* Slots */
        {&Sep} ttFragSlots.FragCount /* Count */
        {&Sep} "fragment(s)"
      SKIP. /*PUT */
    END. /* FOR FIRST ttFragSlots  */

  END. /* FOR EACH ttTable */

  OUTPUT CLOSE.

END PROCEDURE. /* ReportSlotStat */

/* ------------------------------------------------------------------------- */
