DEFINE VARIABLE vBaseDir AS CHARACTER NO-UNDO INITIAL
  "Directory with the input files (DbStatDump.*.txt)".

UPDATE vBaseDir LABEL "Dir" FORMAT "x(72)"
  HELP "Enter directory with input files (table/index stats)."
WITH SIDE-LABEL.

RUN ProcessDir(vBaseDir, ?, "DbStatDump~~~.*~~~.txt", "ImportDbStat":U).
RUN SetSnapshots(500 /*milliseconds*/).
RUN PutDbStat(vBaseDir).

/* ----------------------------------------------------------------------------
    File        : DbStatDiff.p
    Purpose     : Converts the dumps of the _TableStat/_IndexStat statistics
                  created by DbStatDump.p to the report per the intervals
                  between the dumps.

    Syntax      : See above.

    Description : The output file can be opened in Excel.

    Author(s)   : George Potemkin
    Created     : May 30, 2015
    Modified    : Jun 04, 2015 13:20
    Version     : 1.0

    The most recent version can be downloaded from here:
    ftp://ftp.progress-tech.ru/pub/Users/george/Programs/DbStatDiff.p

    Fixes       :

---------------------------------------------------------------------------- */

/* Statistics that can help to identify the date format in the input files: */
  DEFINE VARIABLE vMinDate AS INTEGER NO-UNDO EXTENT 3 INITIAL 2147483647.
  DEFINE VARIABLE vMaxDate AS INTEGER NO-UNDO EXTENT 3 INITIAL 0.

/* ******************************  Temp-tables  **************************** */

DEFINE TEMP-TABLE ttColumn NO-UNDO
  FIELD ColumnNumber AS INTEGER
  FIELD ColumnTitle  AS CHARACTER
  FIELD ObjectType   AS CHARACTER
  FIELD ObjectField  AS CHARACTER
  FIELD FieldType    AS CHARACTER
  INDEX ColumnNumber IS UNIQUE
        ColumnNumber
. /* TEMP-TABLE ttColumn */

DEFINE TEMP-TABLE ttSnapshot NO-UNDO
  FIELD SnapshotId    AS INTEGER
  FIELD SnapshotTime  AS CHARACTER
  FIELD SnapshotTime2 AS DATETIME
  INDEX SnapshotId    IS UNIQUE
        SnapshotId
  INDEX SnapshotTime  IS UNIQUE
        SnapshotTime
  INDEX SnapshotTime2 IS UNIQUE
        SnapshotTime2
. /* TEMP-TABLE ttSnapshot */

DEFINE TEMP-TABLE ttDb NO-UNDO
  FIELD DbId       AS INTEGER
  FIELD HostName   AS CHARACTER
  FIELD PhysName   AS CHARACTER
  INDEX DbId       IS UNIQUE
        DbId
  INDEX PhysName   IS UNIQUE
        HostName
        PhysName
. /* TEMP-TABLE ttDb */

DEFINE TEMP-TABLE ttTable NO-UNDO
  FIELD SnapshotId AS INTEGER
  FIELD DbId       AS INTEGER
  FIELD TableId    AS INTEGER
  FIELD TableName  AS CHARACTER
  FIELD NumFld     AS INTEGER
  FIELD NumKey     AS INTEGER
  FIELD AltBufPool AS CHARACTER
  FIELD AreaName   AS CHARACTER
  FIELD AreaNumber AS INTEGER
  FIELD AreaHWM    AS INT64
  FIELD RecReads   AS INT64
  FIELD RecUpdates AS INT64
  FIELD RecCreates AS INT64
  FIELD RecDeletes AS INT64
  FIELD BlockReads AS INT64
  FIELD BgnTime    AS CHARACTER
  FIELD EndTime    AS CHARACTER
  FIELD TimeLag    AS INTEGER
  FIELD BgnTime2   AS DATETIME
  FIELD EndTime2   AS DATETIME
  INDEX TableId    IS UNIQUE
        SnapshotId
        DbId
        TableId
        TableName
        BgnTime
        EndTime
. /* DEFINE TEMP-TABLE ttTable */

DEFINE TEMP-TABLE ttIndex NO-UNDO
  FIELD SnapshotId  AS INTEGER
  FIELD DbId        AS INTEGER
  FIELD TableId     AS INTEGER
  FIELD TableName   AS CHARACTER
  FIELD IndexId     AS INTEGER
  FIELD IndexName   AS CHARACTER
  FIELD NumComp     AS INTEGER
  FIELD AltBufPool  AS CHARACTER
  FIELD AreaName    AS CHARACTER
  FIELD AreaNumber  AS INTEGER
  FIELD AreaHWM     AS INT64
  FIELD IndexInfo   AS CHARACTER
  FIELD LevelCount  AS INTEGER
  FIELD IdxReads    AS INT64
  FIELD IdxCreates  AS INT64
  FIELD IdxDeletes  AS INT64
  FIELD BlockReads  AS INT64
  FIELD BlockSplits AS INT64
  FIELD BlockFree   AS INT64
  FIELD BgnTime     AS CHARACTER
  FIELD EndTime     AS CHARACTER
  FIELD TimeLag     AS INTEGER
  FIELD BgnTime2   AS DATETIME
  FIELD EndTime2   AS DATETIME
  INDEX IndexId     IS UNIQUE
        SnapshotId
        DbId
        TableName
        TableId
        IndexName
        IndexId
        BgnTime
        EndTime
. /* DEFINE TEMP-TABLE ttIndex */

DEFINE TEMP-TABLE ttSequence NO-UNDO
  FIELD SnapshotId AS INTEGER
  FIELD DbId       AS INTEGER
  FIELD SeqId      AS INTEGER
  FIELD SeqName    AS CHARACTER
  FIELD SeqUpdates AS INT64
  FIELD BgnTime    AS CHARACTER
  FIELD EndTime    AS CHARACTER
  FIELD BgnTime2   AS DATETIME
  FIELD EndTime2   AS DATETIME
  INDEX SeqId      IS UNIQUE
        SnapshotId
        DbId
        SeqId
. /* TEMP-TABLE ttSequence */

DEFINE TEMP-TABLE ttOSFile NO-UNDO
  FIELD osFileName AS CHARACTER
  FIELD osFilePath AS CHARACTER
  FIELD osFileAttr AS CHARACTER
  INDEX osFileName
        osFileName
. /* DEFINE TEMP-TABLE ttOSFile */

/* *******************************  Functions  ***************************** */

FUNCTION String2DateTime RETURNS DATETIME (INPUT ipString AS CHARACTER).

/* Input string should be in oneof the following formats:
   Tue Jul 27 12:11:45 2004
   06/27/2004 12:11:45.678[+03:00]
*/
  DEFINE VARIABLE vDateTime  AS DATETIME  NO-UNDO INITIAL ?.
  DEFINE VARIABLE vMonthList AS CHARACTER NO-UNDO
    INITIAL "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec".
  DEFINE VARIABLE vTime AS CHARACTER NO-UNDO.

  ASSIGN ipString = TRIM(REPLACE(ipString, "  ":U, " ":U))
         vDateTime = ?
  . /* ASSIGN */

  IF NUM-ENTRIES(ipString, " ":U) EQ 2
  AND ipString MATCHES "*/*/* ..:..:..*":U THEN
  ASSIGN vDateTime = DATETIME(ipString) NO-ERROR.
  ELSE

  IF NUM-ENTRIES(ipString, " ":U) EQ 5 THEN
  ASSIGN vTime = ENTRY(4, ipString, " ":U)
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

FUNCTION Excel2Integer RETURN INT64 (ipChr AS CHARACTER):
/* Converts an exponent presentation of large values to an integer:
   e.g. 8.4993E+11 to 849930000000
*/
  DEFINE VARIABLE vExp AS INTEGER NO-UNDO.
  DEFINE VARIABLE vDec AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vInt AS INT64   NO-UNDO.

  ASSIGN vInt = ?
         vExp = INDEX(ipChr, "E":U)
  . /* ASSIGN */

  IF vExp EQ 0 THEN
  ASSIGN vInt = INT64(ipChr) NO-ERROR.
  ELSE
  ASSIGN vDec = DECIMAL(SUBSTRING(ipChr, 1, vExp - 1))
         vExp = INTEGER(SUBSTRING(ipChr, vExp + 1))
         vInt = vDec * EXP(10, vExp)
  NO-ERROR. /* ASSIGN */

  RETURN vInt.
END FUNCTION. /* Excel2Integer */

/* ------------------------------------------------------------------------- */

FUNCTION myCAN-DO RETURN LOGICAL (ipList AS CHARACTER, ipItem  AS CHARACTER):
/* Works as CAN-DO but does not treat "#" and "@" as the special characters. */

  DEFINE VARIABLE vMatch AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE vMask  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE n      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i      AS INTEGER   NO-UNDO.

  ASSIGN n = NUM-ENTRIES(ipList).

  DO i = 1 TO n:
    ASSIGN vMask = ENTRY(i, ipList)
           vMatch = TRUE
    . /* ASSIGN */

    IF vMask BEGINS "!" THEN
    ASSIGN vMask = SUBSTRING(vMask, 2)
           vMatch = FALSE
    . /* ASSIGN */

    IF ipItem MATCHES vMask THEN
    RETURN vMatch.
  END.

  RETURN FALSE.
END FUNCTION. /* myCAN-DO */

/* *****************************  Procedures  ****************************** */

PROCEDURE ProcessDir:
  DEFINE INPUT PARAMETER ipInputDir    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMaxDirLevel AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipFileMask    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipProcedure   AS CHARACTER NO-UNDO.
/*
ipInputDir     - Root directory with the files to read from;
ipMaxDirLevel  - 1 = search in current dir only, ? = search in all subdirs;
ipFileMask     - Mask for input files;
*/

  ASSIGN FILE-INFO:FILE-NAME = ipInputDir.
  IF FILE-INFO:FULL-PATHNAME EQ ? THEN
  RETURN.

  ASSIGN ipInputDir = FILE-INFO:FULL-PATHNAME.

  STATUS DEFAULT SUBSTITUTE("Running &1. Please wait...", ipProcedure).

  RUN GetOSFiles(INPUT ipInputDir, INPUT ipFileMask, INPUT ipMaxDirLevel).

  FOR EACH ttOSFile NO-LOCK:
/*  DISPLAY ttOSFile.osFilePath FORMAT "x(64)". */
    RUN VALUE(ipProcedure)(ttOSFile.osFilePath).
  END. /* FOR EACH ttOSFile */

  EMPTY TEMP-TABLE ttOSFile.
  STATUS DEFAULT.

END PROCEDURE. /* ProcessDir */

/* ------------------------------------------------------------------------- */

PROCEDURE GetOSFiles.

  DEFINE INPUT PARAMETER ipRootDir     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFileMask    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMaxDirLevel AS INTEGER NO-UNDO.

  DEFINE VARIABLE vFileName AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE vFilePath AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE vFileAttr AS CHARACTER   NO-UNDO.

  ASSIGN ipMaxDirLevel = ipMaxDirLevel - 1.
  INPUT FROM OS-DIR(ipRootDir).
  REPEAT:
    IMPORT  vFileName vFilePath vFileAttr.

    IF  vFileAttr MATCHES "*D*"
    AND vFileName NE ".":U
    AND vFileName NE "..":U
    AND (ipMaxDirLevel EQ ? OR ipMaxDirLevel GT 0) THEN
    RUN GetOSFiles(INPUT vFilePath, INPUT ipFileMask, INPUT ipMaxDirLevel).
    ELSE
    IF  vFileAttr MATCHES "*F*"
    AND vFileName MATCHES ipFileMask THEN
    DO TRANSACTION:
      CREATE ttOSFile.
      ASSIGN ttOSFile.osFileName = vFileName
             ttOSFile.osFilePath = vFilePath
             ttOSFile.osFileAttr = vFileAttr
      . /* ASSIGN */
    END.
  END.
  INPUT CLOSE.
END PROCEDURE. /* GetOSFiles */

/* ------------------------------------------------------------------------- */

PROCEDURE CheckOutputFile.

  DEFINE INPUT-OUTPUT PARAMETER ioOutputFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vErrorDesc  AS CHARACTER NO-UNDO EXTENT 18 INITIAL [
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

  DEFINE VARIABLE vChoice AS LOGICAL   NO-UNDO INITIAL TRUE.
  DEFINE VARIABLE i       AS INTEGER   NO-UNDO.

  ASSIGN FILE-INFO:FILE-NAME = ioOutputFile.

  IF FILE-INFO:FULL-PATHNAME NE ? THEN
  IF FILE-INFO:FILE-TYPE MATCHES "*D*":U THEN
  DO:
    MESSAGE "The output is a directory:" SKIP
            ioOutputFile SKIP
            "Output file will not be created."
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    ASSIGN ioOutputFile = ?.
    RETURN.
  END.
  ELSE
  DO:
    ASSIGN ioOutputFile = FILE-INFO:FULL-PATHNAME.

    MESSAGE "Output file exists:"   SKIP
            ioOutputFile SKIP
            "Delete the file? (otherwise a different name will be used)"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    UPDATE vChoice.

    IF vChoice EQ TRUE /* Yes */ THEN
    DO:
      OS-DELETE VALUE(ioOutputFile).
      IF OS-ERROR EQ 0 THEN
      RETURN.
      ELSE
      MESSAGE
        "Unable to delete file:" SKIP
        ioOutputFile SKIP
        "Error:" OS-ERROR "("
        IF OS-ERROR LE EXTENT(vErrorDesc) THEN vErrorDesc[OS-ERROR]
                                          ELSE "Unmapped error" ")" SKIP
        "A different filename will be used."
      VIEW-AS ALERT-BOX WARNING BUTTONS OK.
    END.

    ASSIGN i = R-INDEX(ioOutputFile, ".":U)
            ioOutputFile = SUBSTRING(ioOutputFile, 1, i)
                         + STRING( YEAR(TODAY), "9999":U)
                 + ".":U + STRING(MONTH(TODAY),   "99":U)
                 + ".":U + STRING(  DAY(TODAY),   "99":U)
                 + "_":U + REPLACE(STRING(TIME, "HH:MM:SS":U), ":":U, ".":U)
                         + SUBSTRING(ioOutputFile, i)
    . /* ASSIGN */
    RETURN.
  END.
END PROCEDURE. /* CheckOutputFile */

/* ------------------------------------------------------------------------- */

PROCEDURE DateFormatAnalysis.

  DEFINE INPUT PARAMETER ipDate AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vItem AS INTEGER NO-UNDO.
  DEFINE VARIABLE i     AS INTEGER NO-UNDO.

  IF ipDate MATCHES "*/*/*":U THEN
  DO i = 1 TO 3:
    ASSIGN ipDate      = ENTRY(1, ipDate, " ":U)
           vItem       = INTEGER(ENTRY(i, ipDate, "/":U))
           vMinDate[i] = MIN(vMinDate[i], vItem)
           vMaxDate[i] = MAX(vMaxDate[i], vItem)
    NO-ERROR. /* ASSIGN */
  END. /* DO i = 1 TO 3 */

END PROCEDURE. /* DateFormatAnalysis */

/* ------------------------------------------------------------------------- */

PROCEDURE ImportDbStat.

  DEFINE INPUT PARAMETER ipInputFile AS CHARACTER NO-UNDO.

  &SCOPED-DEFINE Sep "~t"

  DEFINE VARIABLE vObjectTypes AS CHARACTER NO-UNDO EXTENT 10 INITIAL [
/*     Type          Masks on the imported titles */
/*1*/  "Db":U,       "*Database*,*Db*",
/*2*/  "Table":U,    "!*Area,*Table*,*Tbl*,Rec*",
/*3*/  "Index":U,    "!*Area,!Indexes,*Index*,*Idx*,Key*",
/*4*/  "Sequence":U, "*Sequence*,Seq*",
/*5*/  "Area":U,     "*Area*"
  ]. /* vObjectType */

  DEFINE VARIABLE vObjectFields AS CHARACTER NO-UNDO EXTENT 60 INITIAL [
/*     Field             Type     Masks on the imported titles*/
/*01*/ "Host":U,         "Chr":U, "*Host*",
/*02*/ "Name":U,         "Chr":U, "*Name*,Database,Db,Table,Tbl,Index,Idx,Sequence,Seq,*Area",
/*03*/ "BgnTime":U,      "Chr":U, "*From*,B*Time",
/*04*/ "EndTime":U,      "Chr":U, "To,End*Time",
/*05*/ "TableTimeLag":U, "Int":U, "T(ms)",
/*06*/ "IndexTimeLag":U, "Int":U, "I(ms)",
/*07*/ "Id":U,           "Int":U, "*~#,*id,*Num*",
/*08*/ "FieldCount":U,   "Int":U, "*Field*",
/*09*/ "IndexCount":U,   "Int":U, "Indexes,I*Count,I*Cnt",
/*10*/ "AltBufPool":U,   "Chr":U, "*B2*,Alt*",
/*11*/ "AreaHWM":U,      "Int":U, "HWM,High*",
/*12*/ "Info":U,         "Chr":U, "State,*Info*",
/*13*/ "Levels":U,       "Int":U, "*Levels*",
/*14*/ "BlkSplits":U,    "Int":U, "*Split*",
/*15*/ "BlkFree":U,      "Int":U, "*Free*,*Block*Del*",
/*16*/ "Deletes":U,      "Int":U, "*Del*",
/*17*/ "BlkReads":U,     "Int":U, "*BlkRead*",
/*18*/ "Reads":U,        "Int":U, "*Read*",
/*19*/ "Updates":U,      "Int":U, "*Upd*",
/*20*/ "Creates":U,      "Int":U, "*Create*,*crt*"
  ]. /* vObjectField */

  DEFINE VARIABLE vItem AS CHARACTER NO-UNDO EXTENT 34.

/* The columns in *.DumpStat.txt: */
  DEFINE VARIABLE vDbHostName      AS CHARACTER NO-UNDO. /* 01 */
  DEFINE VARIABLE vDbPhysName      AS CHARACTER NO-UNDO. /* 02 */
  DEFINE VARIABLE vDbBgnTime       AS CHARACTER NO-UNDO. /* 03 */
  DEFINE VARIABLE vDbEndTime       AS CHARACTER NO-UNDO. /* 04 */
  DEFINE VARIABLE vTableTimeLag    AS INTEGER   NO-UNDO. /* 05 */
  DEFINE VARIABLE vIndexTimeLag    AS INTEGER   NO-UNDO. /* 06 */
  DEFINE VARIABLE vTableId         AS INTEGER   NO-UNDO. /* 07 */
  DEFINE VARIABLE vTableName       AS CHARACTER NO-UNDO. /* 08 */
  DEFINE VARIABLE vTableFields     AS INTEGER   NO-UNDO. /* 09 */
  DEFINE VARIABLE vTableIndexes    AS INTEGER   NO-UNDO. /* 10 */
  DEFINE VARIABLE vTableAltBufPool AS CHARACTER NO-UNDO. /* 11 */
  DEFINE VARIABLE vTableAreaName   AS CHARACTER NO-UNDO. /* 12 */
  DEFINE VARIABLE vTableAreaNumber AS INTEGER   NO-UNDO. /* 13 */
  DEFINE VARIABLE vTableAreaHWM    AS INT64     NO-UNDO. /* 14 */
  DEFINE VARIABLE vTableReads      AS INT64     NO-UNDO. /* 15 */
  DEFINE VARIABLE vTableUpdates    AS INT64     NO-UNDO. /* 16 */
  DEFINE VARIABLE vTableCreates    AS INT64     NO-UNDO. /* 17 */
  DEFINE VARIABLE vTableDeletes    AS INT64     NO-UNDO. /* 18 */
  DEFINE VARIABLE vTableBlkReads   AS INT64     NO-UNDO. /* 19 */
  DEFINE VARIABLE vIndexId         AS INTEGER   NO-UNDO. /* 20 */
  DEFINE VARIABLE vIndexName       AS CHARACTER NO-UNDO. /* 21 */
  DEFINE VARIABLE vIndexFields     AS INTEGER   NO-UNDO. /* 22 */
  DEFINE VARIABLE vIndexAltBufPool AS CHARACTER NO-UNDO. /* 23 */
  DEFINE VARIABLE vIndexAreaName   AS CHARACTER NO-UNDO. /* 24 */
  DEFINE VARIABLE vIndexAreaNumber AS INTEGER   NO-UNDO. /* 25 */
  DEFINE VARIABLE vIndexAreaHWM    AS INTEGER   NO-UNDO. /* 26 */
  DEFINE VARIABLE vIndexInfo       AS CHARACTER NO-UNDO. /* 27 */
  DEFINE VARIABLE vIndexLevels     AS INTEGER   NO-UNDO. /* 28 */
  DEFINE VARIABLE vIndexReads      AS INT64     NO-UNDO. /* 29 */
  DEFINE VARIABLE vIndexCreates    AS INT64     NO-UNDO. /* 20 */
  DEFINE VARIABLE vIndexDeletes    AS INT64     NO-UNDO. /* 21 */
  DEFINE VARIABLE vIndexBlkReads   AS INT64     NO-UNDO. /* 32 */
  DEFINE VARIABLE vIndexBlkSplits  AS INT64     NO-UNDO. /* 33 */
  DEFINE VARIABLE vIndexBlkFree    AS INT64     NO-UNDO. /* 34 */
/* The columns in *.SeqDmp.txt: */
  DEFINE VARIABLE vSeqName         AS CHARACTER NO-UNDO. /* 05 */
  DEFINE VARIABLE vSeqId           AS INTEGER   NO-UNDO. /* 06 */
  DEFINE VARIABLE vSeqUpdates      AS INT64     NO-UNDO. /* 07 */

  DEFINE VARIABLE vSnapshotId  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vDbId        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vRowCount    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vCrtCount    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vColumn      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vObjectType  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vLastObject  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vObjectField AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFieldType   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vChrValue    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vIntValue    AS INT64     NO-UNDO.
  DEFINE VARIABLE vDecValue    AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vAskOnError  AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE i            AS INTEGER   NO-UNDO.

/* Examples of the column's titles:
Db Host, Db Name, From, To, T(ms), I(ms), Table, Table #, Fields, Indexes, B2,
Table Area, Area #, HWM, Index, Index #, Fields, State, B2, Index Area, Area #,
HWM, Levels, RecReads, IdxReads, RecUpdates, RecCreates, IdxCreates,
RecDeletes, IdxDeletes, RecBlkReads, IdxBlkReads, BlockSplits, BlockFree,
*/

  INPUT FROM VALUE(ipInputFile).

  ASSIGN vItem       = ?
         vObjectType = "Db":U /* First columns are about database. */
  . /* ASSIGN */

  IMPORT DELIMITER {&Sep} vItem.

  DO vColumn = 1 TO EXTENT(vItem) WHILE vItem[vColumn] NE ?:

    ASSIGN vObjectType  = "":U
           vObjectField = "":U
    . /* ASSIGN */

    DO i = 1 TO EXTENT(vObjectTypes) BY 2 WHILE vObjectType EQ "":U:
      ASSIGN vObjectType = vObjectTypes[i]
             WHEN myCAN-DO(vObjectTypes[i + 1], vItem[vColumn])
      . /* ASSIGN */
    END. /* DO i = 1 TO EXTENT(vObjectTypes) */

    ASSIGN vLastObject = vObjectType
           WHEN CAN-DO("Db,Index,Table,Sequence":U, vObjectType)
    . /* ASSIGN */

    DO i = 1 TO EXTENT(vObjectFields) BY 3 WHILE vObjectField EQ "":U:
      ASSIGN vFieldType   = vObjectFields[i + 1]
             vObjectField = vObjectFields[i]
                WHEN myCAN-DO(vObjectFields[i + 2], vItem[vColumn])
      . /* ASSIGN */
    END. /* DO i = 1 TO EXTENT(vObjectFields) */

    ASSIGN vObjectField = vObjectType + "." + vObjectField
           WHEN vObjectType NE vLastObject AND vObjectType NE "":U
    . /* ASSIGN */

    IF vObjectField NE "":U THEN
    DO TRANSACTION:
      CREATE ttColumn.
      ASSIGN ttColumn.ColumnNumber = vColumn
             ttColumn.ColumnTitle  = vItem[vColumn]
             ttColumn.ObjectType   = vLastObject
             ttColumn.ObjectField  = vObjectField
             ttColumn.FieldType    = vFieldType
      . /* ASSIGN */
    END.

  END. /* REPEAT vColumn = 1 TO EXTENT(vItem)  */

  IF NOT CAN-FIND(FIRST ttColumn) THEN
  DO:
    INPUT CLOSE.
    MESSAGE
      "Failed to parse the header of the input file:" SKIP
      ipInputFile SKIP
      "The file will be skipped."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.

  ASSIGN vSnapshotId = ? /* Shapshots will be identfied after import. */
         vRowCount = 0
         vCrtCount = 0
         vAskOnError = TRUE
  . /* ASSIGN */

ImportRow:
  REPEAT:

    ASSIGN vItem = ?.
    IMPORT DELIMITER {&Sep} vItem.

    ASSIGN vRowCount        = vRowCount + 1
           vDbHostName      = ?
           vDbPhysName      = ?
           vDbBgnTime       = ?
           vDbEndTime       = ?
           vTableTimeLag    = ?
           vIndexTimeLag    = ?
           vTableId         = ?
           vTableName       = ?
           vTableFields     = ?
           vTableIndexes    = ?
           vTableAltBufPool = ?
           vTableAreaName   = ?
           vTableAreaNumber = ?
           vTableAreaHWM    = ?
           vTableReads      = ?
           vTableUpdates    = ?
           vTableCreates    = ?
           vTableDeletes    = ?
           vTableBlkReads   = ?
           vIndexId         = ?
           vIndexName       = ?
           vIndexFields     = ?
           vIndexAltBufPool = ?
           vIndexAreaName   = ?
           vIndexAreaNumber = ?
           vIndexAreaHWM    = ?
           vIndexInfo       = ?
           vIndexLevels     = ?
           vIndexReads      = ?
           vIndexCreates    = ?
           vIndexDeletes    = ?
           vIndexBlkReads   = ?
           vIndexBlkSplits  = ?
           vIndexBlkFree    = ?
           vSeqName         = ?
           vSeqId           = ?
           vSeqUpdates      = ?
    . /* ASSIGN */

/* This loop spends most of the time: */

    FOR EACH ttColumn NO-LOCK /* index ColumnNumber */
          BY ttColumn.ColumnNumber:

      ASSIGN vChrValue = vItem[ttColumn.ColumnNumber]
             vIntValue = ?
             vDecValue = ?
      NO-ERROR. /* ASSIGN */

      ASSIGN
        vIntValue = Excel2Integer(vChrValue) WHEN ttColumn.FieldType EQ "Int":U
        vDecValue =       DECIMAL(vChrValue) WHEN ttColumn.FieldType EQ "Dec":U
      NO-ERROR.

      IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
      IF vAskOnError THEN
      DO:
        MESSAGE
          "Import from" ipInputFile        SKIP
          SUBSTITUTE('Incorrect field value: "&1", expected &2 type',
            /* &1 */ vChrValue,
            /* &2 */ ttColumn.FieldType)   SKIP
          SUBSTITUTE('Row: &1 Col: &2 ("&3" = &4.&5)',
            /* &1 */ STRING(vRowCount),
            /* &2 */ STRING(ttColumn.ColumnNumber),
            /* &3 */ ttColumn.ColumnTitle,
            /* &4 */ ttColumn.ObjectType,
            /* &5 */ ttColumn.ObjectField) SKIP
          ERROR-STATUS:GET-MESSAGE(1)      SKIP
          "Ignore only this error, ignore all errors, interrupt the import?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE vAskOnError.

        IF vAskOnError NE ? THEN
        LEAVE ImportRow.
      END.

      CASE ttColumn.ObjectType + ".":U + ttColumn.ObjectField:
        WHEN "Db.Host":U          THEN ASSIGN vDbHostName      = vChrValue.
        WHEN "Db.Name":U          THEN ASSIGN vDbPhysName      = vChrValue.
        WHEN "Db.BgnTime":U       THEN ASSIGN vDbBgnTime       = vChrValue.
        WHEN "Db.EndTime":U       THEN ASSIGN vDbEndTime       = vChrValue.
        WHEN "Db.TableTimeLag":U  THEN ASSIGN vTableTimeLag    = vIntValue.
        WHEN "Db.IndexTimeLag":U  THEN ASSIGN vIndexTimeLag    = vIntValue.
        WHEN "Table.Id":U         THEN ASSIGN vTableId         = vIntValue.
        WHEN "Table.Name":U       THEN ASSIGN vTableName       = vChrValue.
        WHEN "Table.FieldCount":U THEN ASSIGN vTableFields     = vIntValue.
        WHEN "Table.IndexCount":U THEN ASSIGN vTableIndexes    = vIntValue.
        WHEN "Table.AltBufPool":U THEN ASSIGN vTableAltBufPool = vChrValue.
        WHEN "Table.Area.Name":U  THEN ASSIGN vTableAreaName   = vChrValue.
        WHEN "Table.Area.Id":U    THEN ASSIGN vTableAreaNumber = vIntValue.
        WHEN "Table.AreaHWM":U    THEN ASSIGN vTableAreaHWM    = vIntValue.
        WHEN "Table.Reads":U      THEN ASSIGN vTableReads      = vIntValue.
        WHEN "Table.Updates":U    THEN ASSIGN vTableUpdates    = vIntValue.
        WHEN "Table.Creates":U    THEN ASSIGN vTableCreates    = vIntValue.
        WHEN "Table.Deletes":U    THEN ASSIGN vTableDeletes    = vIntValue.
        WHEN "Table.BlkReads":U   THEN ASSIGN vTableBlkReads   = vIntValue.
        WHEN "Index.Id":U         THEN ASSIGN vIndexId         = vIntValue.
        WHEN "Index.Name":U       THEN ASSIGN vIndexName       = vChrValue.
        WHEN "Index.FieldCount":U THEN ASSIGN vIndexFields     = vIntValue.
        WHEN "Index.AltBufPool":U THEN ASSIGN vIndexAltBufPool = vChrValue.
        WHEN "Index.Area.Name":U  THEN ASSIGN vIndexAreaName   = vChrValue.
        WHEN "Index.Area.Id":U    THEN ASSIGN vIndexAreaNumber = vIntValue.
        WHEN "Index.AreaHWM":U    THEN ASSIGN vIndexAreaHWM    = vIntValue.
        WHEN "Index.Info":U       THEN ASSIGN vIndexInfo       = vChrValue.
        WHEN "Index.Levels":U     THEN ASSIGN vIndexLevels     = vIntValue.
        WHEN "Index.Reads":U      THEN ASSIGN vIndexReads      = vIntValue.
        WHEN "Index.Creates":U    THEN ASSIGN vIndexCreates    = vIntValue.
        WHEN "Index.Deletes":U    THEN ASSIGN vIndexDeletes    = vIntValue.
        WHEN "Index.BlkReads":U   THEN ASSIGN vIndexBlkReads   = vIntValue.
        WHEN "Index.BlkSplits":U  THEN ASSIGN vIndexBlkSplits  = vIntValue.
        WHEN "Index.BlkFree":U    THEN ASSIGN vIndexBlkFree    = vIntValue.
        WHEN "Sequence.Name":U    THEN ASSIGN vSeqName         = vChrValue.
        WHEN "Sequence.Id":U      THEN ASSIGN vSeqId           = vIntValue.
        WHEN "Sequence.Updates":U THEN ASSIGN vSeqUpdates      = vIntValue.
      END CASE.

    END. /* FOR EACH ttColumn */

    RUN DateFormatAnalysis(vDbBgnTime).
    RUN DateFormatAnalysis(vDbEndTime).

    FIND FIRST ttDb NO-LOCK /* index PhysName */
         WHERE ttDb.HostName   EQ vDbHostName
           AND ttDb.PhysName   EQ vDbPhysName
    NO-ERROR.

    IF NOT AVAILABLE ttDb THEN
    DO TRANSACTION:
      ASSIGN vDbId = 0.
      FOR EACH ttDb NO-LOCK /* index DbId */
            BY ttDb.DbId DESCENDING:
        ASSIGN vDbId = ttDb.DbId + 1.
        LEAVE.
      END.
      CREATE ttDb.
      ASSIGN ttDb.DbId       = vDbId
             ttDb.HostName   = vDbHostName
             ttDb.PhysName   = vDbPhysName
      . /* ASSIGN */
    END. /* DO TRANSACTION*/
    ELSE
    ASSIGN vDbId = ttDb.DbId.

    IF vTableName NE ? OR vTableId NE ? THEN
    DO TRANSACTION:
      FIND FIRST ttTable NO-LOCK /* index TableId */
           WHERE ttTable.SnapshotId EQ vSnapshotId
             AND ttTable.DbId       EQ vDbId
             AND ttTable.TableId    EQ vTableId
             AND ttTable.TableName  EQ vTableName
             AND ttTable.BgnTime    EQ vDbBgnTime
             AND ttTable.EndTime    EQ vDbEndTime
      NO-ERROR.

      IF NOT AVAILABLE ttTable THEN
      CREATE ttTable.
      ASSIGN ttTable.SnapshotId = vSnapshotId
             ttTable.DbId       = vDbId
             ttTable.TableId    = vTableId
             ttTable.TableName  = vTableName
             ttTable.NumFld     = vTableFields
             ttTable.NumKey     = vTableIndexes
             ttTable.AltBufPool = vTableAltBufPool
             ttTable.AreaName   = vTableAreaName
             ttTable.AreaNumber = vTableAreaNumber
             ttTable.AreaHWM    = vTableAreaHWM
             ttTable.RecReads   = vTableReads
             ttTable.RecUpdates = vTableUpdates
             ttTable.RecCreates = vTableCreates
             ttTable.RecDeletes = vTableDeletes
             ttTable.BlockReads = vTableBlkReads
             ttTable.BgnTime    = vDbBgnTime
             ttTable.EndTime    = vDbEndTime
             ttTable.TimeLag    = vTableTimeLag
             vCrtCount = vCrtCount + 1 WHEN NEW ttTable
      . /* ASSIGN */
    END. /* DO TRANSACTION*/

    IF vIndexName NE ? OR vIndexId NE ? THEN
    DO TRANSACTION:
      FIND FIRST ttIndex NO-LOCK /* index IndexId */
           WHERE ttIndex.SnapshotId EQ vSnapshotId
             AND ttIndex.DbId       EQ vDbId
             AND ttIndex.TableName  EQ vTableName
             AND ttIndex.TableId    EQ vTableId
             AND ttIndex.IndexName  EQ vIndexName
             AND ttIndex.IndexId    EQ vIndexId
             AND ttIndex.BgnTime    EQ vDbBgnTime
             AND ttIndex.EndTime    EQ vDbEndTime
      NO-ERROR.

      IF NOT AVAILABLE ttIndex THEN
      CREATE ttIndex.
      ASSIGN ttIndex.SnapshotId  = vSnapshotId
             ttIndex.DbId        = vDbId
             ttIndex.TableId     = vTableId
             ttIndex.TableName   = vTableName
             ttIndex.IndexId     = vIndexId
             ttIndex.IndexName   = vIndexName
             ttIndex.NumComp     = vIndexFields
             ttIndex.AreaName    = vIndexAreaName
             ttIndex.AreaNumber  = vIndexAreaNumber
             ttIndex.AreaHWM     = vIndexAreaHWM
             ttIndex.IndexInfo   = vIndexInfo
             ttIndex.AltBufPool  = vIndexAltBufPool
             ttIndex.LevelCount  = vIndexLevels
             ttIndex.IdxReads    = vIndexReads
             ttIndex.IdxCreates  = vIndexCreates
             ttIndex.IdxDeletes  = vIndexDeletes
             ttIndex.BlockReads  = vIndexBlkReads
             ttIndex.BlockSplits = vIndexBlkSplits
             ttIndex.BlockFree   = vIndexBlkFree
             ttIndex.BgnTime     = vDbBgnTime
             ttIndex.EndTime     = vDbEndTime
             ttIndex.TimeLag     = vIndexTimeLag
            vCrtCount = vCrtCount + 1 WHEN NEW ttIndex
      . /* ASSIGN */
    END. /* DO TRANSACTION*/

    IF vSeqName NE ? OR vSeqId NE ? THEN
    DO TRANSACTION:
      FIND FIRST ttSequence NO-LOCK /* index SeqId */
           WHERE ttSequence.SnapshotId EQ vSnapshotId
             AND ttSequence.DbId       EQ vDbId
             AND ttSequence.SeqName    EQ vSeqName
             AND ttSequence.SeqId      EQ vSeqId
             AND ttSequence.BgnTime    EQ vDbBgnTime
             AND ttSequence.EndTime    EQ vDbEndTime
      NO-ERROR.

      IF NOT AVAILABLE ttSequence THEN
      CREATE ttSequence.
      ASSIGN ttSequence.SnapshotId = vSnapshotId
             ttSequence.DbId       = vDbId
             ttSequence.SeqId      = vSeqId
             ttSequence.SeqName    = vSeqName
             ttSequence.SeqUpdates = vSeqUpdates
             ttSequence.BgnTime    = vDbBgnTime
             ttSequence.EndTime    = vDbEndTime
             vCrtCount = vCrtCount + 1 WHEN NEW ttSequence
      . /* ASSIGN */
    END. /* DO TRANSACTION*/

  END. /* ImportRow: REPEAT */

  INPUT CLOSE.

  EMPTY TEMP-TABLE ttColumn.

  IF vRowCount GT vCrtCount THEN
  MESSAGE "File:" ipInputFile SKIP
          SUBSTITUTE("Imported &1 rows but only &2 temp records were created.",
                     STRING(vRowCount), STRING(vCrtCount))
  VIEW-AS ALERT-BOX WARNING BUTTONS OK.

END PROCEDURE. /* ImportDbStat */

/* ------------------------------------------------------------------------- */

PROCEDURE SetSnapshots.

  DEFINE INPUT PARAMETER ipTimeLag AS INTEGER     NO-UNDO.

  DEFINE VARIABLE vSnapshotId    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vOldDateFormat AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vNewDateFormat AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vEndTime       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vMinTime       AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vMaxTime       AS DATETIME  NO-UNDO.
  DEFINE VARIABLE i              AS INTEGER   NO-UNDO.

  ASSIGN vOldDateFormat = SESSION:DATE-FORMAT
         vNewDateFormat = "":U
  . /* ASSIGN */

  DO i = 1 TO 3:
    IF vMinDate[i] LT  1 THEN ASSIGN vNewDateFormat = ?. ELSE
    IF vMaxDate[i] LE 12 THEN ASSIGN vNewDateFormat = vNewDateFormat + "m":U. ELSE
    IF vMaxDate[i] LE 31 THEN ASSIGN vNewDateFormat = vNewDateFormat + "d":U.
    ELSE                      ASSIGN vNewDateFormat = vNewDateFormat + "y":U.
  END. /* DO i = 1 TO 3 */

  IF vNewDateFormat NE vOldDateFormat THEN
  IF CAN-DO("mdy,myd,dmy,dym,ymd,ydm":U, vNewDateFormat) THEN
  DO:
    ASSIGN SESSION:DATE-FORMAT = vNewDateFormat.
    MESSAGE
      SUBSTITUTE('Date format is temporarily changed from "&1" to "&2".',
                  vOldDateFormat,
                  vNewDateFormat)
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  ELSE
  IF     vNewDateFormat MATCHES "*y*y*":U /* e.g. 01/32/32 */
  OR NOT vNewDateFormat MATCHES "*m*":U   /* e.g. 13/13/2015 */ THEN
  MESSAGE
    "Some input data have an incorrect date format!" SKIP
    "Statistics:"
    STRING(vMinDate[1]) + "-":U + STRING(vMaxDate[1]) + "/":U +
    STRING(vMinDate[2]) + "-":U + STRING(vMaxDate[2]) + "/":U +
    STRING(vMinDate[3]) + "-":U + STRING(vMaxDate[3]) SKIP
    "Incorrect dates will be lost."
  VIEW-AS ALERT-BOX WARNING BUTTONS OK.

  ASSIGN   vSnapshotId = 0.
  FOR EACH ttSnapshot NO-LOCK
     WHERE ttSnapshot.SnapshotId NE ?
        BY ttSnapshot.SnapshotId DESCENDING:
    ASSIGN vSnapshotId = ttSnapshot.SnapshotId.
    LEAVE.
  END.

/* Set snapshots for ttTable ----------------------------------------------- */

  FOR EACH ttTable EXCLUSIVE-LOCK  /* index TableId (first component only) */
     WHERE ttTable.SnapshotId EQ ?
        BY ttTable.EndTime
        BY ttTable.TimeLag
  TRANSACTION:

    ASSIGN
      ttTable.BgnTime2 = String2DateTime(ttTable.BgnTime)
      ttTable.EndTime2 = String2DateTime(ttTable.EndTime)
      ttTable.EndTime2 = ADD-INTERVAL(ttTable.EndTime2, ttTable.TimeLag,
                                 "milliseconds":U) WHEN ttTable.TimeLag NE ?
      vMinTime = ADD-INTERVAL(ttTable.EndTime2, - ipTimeLag, "milliseconds":U)
      vMaxTime = ADD-INTERVAL(ttTable.EndTime2,   ipTimeLag, "milliseconds":U)
    . /* ASSIGN */

    IF ttTable.EndTime2 EQ ? THEN
    DO:
      ASSIGN vEndTime = SUBSTRING(ttTable.EndTime, 1, 19).

      FIND FIRST ttSnapshot NO-LOCK
           WHERE ttSnapshot.SnapshotTime EQ vEndTime
      NO-ERROR.

      IF NOT AVAILABLE ttSnapshot THEN
      DO:
        CREATE ttSnapshot.
        ASSIGN vSnapshotId              = vSnapshotId + 1
               ttSnapshot.SnapshotId    = vSnapshotId
               ttSnapshot.SnapshotTime  = vEndTime
               ttSnapshot.SnapshotTime2 = ?
        . /* ASSIGN */
      END.
    END. /* IF ttTable.EndTime2 EQ ? */
    ELSE
    DO:
      FIND FIRST ttSnapshot NO-LOCK
           WHERE ttSnapshot.SnapshotTime2 GE vMinTime
             AND ttSnapshot.SnapshotTime2 LT vMaxTime
      NO-ERROR.

      IF NOT AVAILABLE ttSnapshot THEN
      DO:
        CREATE ttSnapshot.
        ASSIGN vSnapshotId              = vSnapshotId + 1
               ttSnapshot.SnapshotId    = vSnapshotId
               ttSnapshot.SnapshotTime  = ttTable.EndTime
               ttSnapshot.SnapshotTime2 = ttTable.EndTime2
        . /* ASSIGN */
      END.
    END. /* IF ttTable.EndTime2 NE ? */

    ASSIGN
      ttTable.SnapshotId = ttSnapshot.SnapshotId
      ttTable.TimeLag    = INTERVAL(ttTable.EndTime2, ttSnapshot.SnapshotTime2,
                                    "milliseconds":U) WHEN ttTable.TimeLag NE ?
    . /* ASSIGN */

  END. /* FOR EACH ttTable TRANSACTION */

/* Set snapshots for ttIndex ----------------------------------------------- */

  FOR EACH ttIndex EXCLUSIVE-LOCK /* index IndexId  (first 4 components) */
     WHERE ttIndex.SnapshotId EQ ?
        BY ttIndex.EndTime
        BY ttIndex.TimeLag
  TRANSACTION:

    ASSIGN
      ttIndex.BgnTime2 = String2DateTime(ttIndex.BgnTime)
      ttIndex.EndTime2 = String2DateTime(ttIndex.EndTime)
      ttIndex.EndTime2 = ADD-INTERVAL(ttIndex.EndTime2, ttIndex.TimeLag,
                                 "milliseconds":U) WHEN ttIndex.TimeLag NE ?
      vMinTime = ADD-INTERVAL(ttIndex.EndTime2, - ipTimeLag, "milliseconds":U)
      vMaxTime = ADD-INTERVAL(ttIndex.EndTime2,   ipTimeLag, "milliseconds":U)
    . /* ASSIGN */

    IF ttIndex.EndTime2 EQ ? THEN
    DO:
      ASSIGN vEndTime = SUBSTRING(ttIndex.EndTime, 1, 19).

      FIND FIRST ttSnapshot NO-LOCK
           WHERE ttSnapshot.SnapshotTime EQ vEndTime
      NO-ERROR.

      IF NOT AVAILABLE ttSnapshot THEN
      DO:
        CREATE ttSnapshot.
        ASSIGN vSnapshotId              = vSnapshotId + 1
               ttSnapshot.SnapshotId    = vSnapshotId
               ttSnapshot.SnapshotTime  = vEndTime
               ttSnapshot.SnapshotTime2 = ?
        . /* ASSIGN */
      END.
    END. /* IF ttIndex.EndTime2 EQ ? */
    ELSE
    DO:
      FIND FIRST ttSnapshot NO-LOCK
           WHERE ttSnapshot.SnapshotTime2 GE vMinTime
             AND ttSnapshot.SnapshotTime2 LT vMaxTime
      NO-ERROR.

      IF NOT AVAILABLE ttSnapshot THEN
      DO:
        CREATE ttSnapshot.
        ASSIGN vSnapshotId              = vSnapshotId + 1
               ttSnapshot.SnapshotId    = vSnapshotId
               ttSnapshot.SnapshotTime  = ttIndex.EndTime
               ttSnapshot.SnapshotTime2 = ttIndex.EndTime2
        . /* ASSIGN */
      END.
    END. /* IF ttIndex.EndTime2 NE ? */

    ASSIGN
      ttIndex.SnapshotId = ttSnapshot.SnapshotId
      ttIndex.TimeLag    = INTERVAL(ttIndex.EndTime2, ttSnapshot.SnapshotTime2,
             "milliseconds":U) WHEN ttIndex.TimeLag NE ?
    . /* ASSIGN */

  END. /* FOR EACH ttIndex TRANSACTION */

/* Set snapshots for ttSequence ------------------------------------------- */

  FOR EACH ttSequence EXCLUSIVE-LOCK /* index TableId (first component only) */
     WHERE ttSequence.SnapshotId EQ ?
        BY ttSequence.EndTime
  TRANSACTION:

    ASSIGN
      ttSequence.BgnTime2 = String2DateTime(ttSequence.BgnTime)
      ttSequence.EndTime2 = String2DateTime(ttSequence.EndTime)
      vMinTime = ADD-INTERVAL(ttSequence.EndTime2,- ipTimeLag,"milliseconds":U)
      vMaxTime = ADD-INTERVAL(ttSequence.EndTime2,  ipTimeLag,"milliseconds":U)
    . /* ASSIGN */

    IF ttSequence.EndTime2 EQ ? THEN
    DO:
      ASSIGN vEndTime = SUBSTRING(ttSequence.EndTime, 1, 19).

      FIND FIRST ttSnapshot NO-LOCK
           WHERE ttSnapshot.SnapshotTime EQ vEndTime
      NO-ERROR.

      IF NOT AVAILABLE ttSnapshot THEN
      DO:
        CREATE ttSnapshot.
        ASSIGN vSnapshotId              = vSnapshotId + 1
               ttSnapshot.SnapshotId    = vSnapshotId
               ttSnapshot.SnapshotTime  = vEndTime
               ttSnapshot.SnapshotTime2 = ?
        . /* ASSIGN */
      END.
    END. /* IF ttSequence.EndTime2 EQ ? */
    ELSE
    DO:
      FIND FIRST ttSnapshot NO-LOCK
           WHERE ttSnapshot.SnapshotTime2 GE vMinTime
             AND ttSnapshot.SnapshotTime2 LT vMaxTime
      NO-ERROR.

      IF NOT AVAILABLE ttSnapshot THEN
      DO:
        CREATE ttSnapshot.
        ASSIGN vSnapshotId              = vSnapshotId + 1
               ttSnapshot.SnapshotId    = vSnapshotId
               ttSnapshot.SnapshotTime  = ttSequence.EndTime
               ttSnapshot.SnapshotTime2 = ttSequence.EndTime2
        . /* ASSIGN */
      END.
    END. /* IF ttSequence.EndTime2 NE ? */

    ASSIGN ttSequence.SnapshotId = ttSnapshot.SnapshotId.

  END. /* FOR EACH ttSequence TRANSACTION */

  ASSIGN SESSION:DATE-FORMAT = vOldDateFormat.

END PROCEDURE. /* SetSnapshots */

/* ------------------------------------------------------------------------- */

PROCEDURE PutDbStat.

  DEFINE INPUT PARAMETER ipOutputPrefix AS CHARACTER NO-UNDO.

/* Excel limit: rows per worksheet: 1,048,576.
   Approximate row size in vDbStatFile ~ 193 bytes.
   Approximate row size in vSeqDmpFile ~ 82 bytes.
   Rename the existing files if their size exceeds vFileSizeLimit (80 MB):
*/
  &SCOPED-DEFINE Sep "~t"

  DEFINE VARIABLE vOutputDir  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDbStatFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vSeqDmpFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i           AS INTEGER   NO-UNDO.

  DEFINE BUFFER bfTable    FOR ttTable.
  DEFINE BUFFER bfIndex    FOR ttIndex.
  DEFINE BUFFER bfSequence FOR ttSequence.

  DEFINE VARIABLE vPrevSnapshot   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vPrevTime       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vTableInterval  AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vTableReads     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vTableUpdates   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vTableCreates   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vTableDeletes   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vTableBlkReads  AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vIndexInterval  AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vIndexReads     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vIndexCreates   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vIndexDeletes   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vIndexBlkReads  AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vIndexBlkSplits AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vIndexBlkFree   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vSeqInterval    AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vSeqUpdates     AS DECIMAL   NO-UNDO.

  IF ipOutputPrefix EQ "":U
  OR ipOutputPrefix EQ ? THEN
  ASSIGN vOutputDir     = "."
         ipOutputPrefix = "DbStatDiff"
  . /* ASSIGN */
  ELSE
  DO:
    ASSIGN FILE-INFO:FILE-NAME = ipOutputPrefix.
    IF  FILE-INFO:FULL-PATHNAME NE ?
    AND FILE-INFO:FILE-TYPE MATCHES "*D*":U THEN
    ASSIGN vOutputDir     = ipOutputPrefix
           ipOutputPrefix = "DbStatDiff"
    . /* ASSIGN */
    ELSE
    ASSIGN i = R-INDEX(ipOutputPrefix,
                       IF OPSYS EQ "Unix":U THEN "/":U ELSE "~\":U)
           vOutputDir     = SUBSTRING(ipOutputPrefix, 1, i - 1)
           ipOutputPrefix = SUBSTRING(ipOutputPrefix, i + 1)
    . /* ASSIGN */
  END.

  ASSIGN FILE-INFO:FILE-NAME = vOutputDir.

  IF     FILE-INFO:FULL-PATHNAME EQ ?
  OR NOT FILE-INFO:FILE-TYPE MATCHES "*D*":U THEN
  DO:
    MESSAGE
      "The output destination does not exist or is not a directory:" SKIP
      vOutputDir SKIP
      "PutDbStat() is inable to create the output files."
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN ERROR.
  END.

  ASSIGN ipOutputPrefix = FILE-INFO:FULL-PATHNAME
                        + IF OPSYS EQ "Unix":U THEN "/":U ELSE "~\":U
                        + ipOutputPrefix
         vDbStatFile = ipOutputPrefix + ".DbStat.txt"
         vSeqDmpFile = ipOutputPrefix + ".SeqDmp.txt"
  . /* ASSIGN */

  IF NOT CAN-FIND(FIRST ttTable) THEN
  ASSIGN vDbStatFile = "":U.
  ELSE
  DO:
    RUN CheckOutputFile(INPUT-OUTPUT vDbStatFile).

    OUTPUT TO VALUE(vDbStatFile).
    PUT UNFORMATTED
             "Db Host"
      {&Sep} "Db Name"
      {&Sep} "From"
      {&Sep} "To"
      {&Sep} "Interval"
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

    ASSIGN vPrevSnapshot = ?.
    FOR EACH ttSnapshot NO-LOCK
          BY ttSnapshot.SnapshotTime:

      FOR EACH ttDb NO-LOCK, /* index PhysName */
          EACH ttTable NO-LOCK /* index TableId (first 2 components only) */
         WHERE ttTable.SnapshotId EQ ttSnapshot.SnapshotId
           AND ttTable.DbId       EQ ttDb.DbId
            BY ttDb.HostName
            BY ttDb.PhysName
            BY ttTable.AreaNumber
            BY ttTable.TableName:

        FIND FIRST bfTable NO-LOCK /* index TableId (first 2 components only) */
             WHERE bfTable.SnapshotId EQ vPrevSnapshot
               AND bfTable.DbId       EQ ttTable.DbId
               AND bfTable.TableName    EQ ttTable.TableName
               AND bfTable.TableId      EQ ttTable.TableId
               AND bfTable.BgnTime    EQ ttTable.BgnTime
        NO-ERROR.

        IF AVAILABLE bfTable THEN
        ASSIGN vPrevTime      = bfTable.EndTime
               vTableInterval = INTERVAL(ttTable.EndTime2, bfTable.EndTime2,
                                         "milliseconds":U) / 1000.0
               vTableReads    = ttTable.RecReads   - bfTable.RecReads
               vTableUpdates  = ttTable.RecUpdates - bfTable.RecUpdates
               vTableCreates  = ttTable.RecCreates - bfTable.RecCreates
               vTableDeletes  = ttTable.RecDeletes - bfTable.RecDeletes
               vTableBlkReads = ttTable.BlockReads - bfTable.BlockReads
        . /* ASSIGN */
        ELSE
        ASSIGN vPrevTime      = ttTable.BgnTime
               vTableInterval = INTERVAL(ttTable.EndTime2, ttTable.BgnTime2,
                                         "milliseconds":U) / 1000.0
               vTableReads    = ttTable.RecReads
               vTableUpdates  = ttTable.RecUpdates
               vTableCreates  = ttTable.RecCreates
               vTableDeletes  = ttTable.RecDeletes
               vTableBlkReads = ttTable.BlockReads
        . /* ASSIGN */

/* If one of the dates has an unknown value (e.g. due to an inccorect format)
   then report the absolute values rather than per sec values:
*/      IF vTableInterval NE ? THEN
        ASSIGN vTableReads    = vTableReads    / vTableInterval
               vTableUpdates  = vTableUpdates  / vTableInterval
               vTableCreates  = vTableCreates  / vTableInterval
               vTableDeletes  = vTableDeletes  / vTableInterval
               vTableBlkReads = vTableBlkReads / vTableInterval
        . /* ASSIGN */

        FOR EACH ttIndex NO-LOCK /* index IndexId  (first 4 components) */
           WHERE ttIndex.SnapshotId EQ ttTable.SnapshotId
             AND ttIndex.DbId       EQ ttTable.DbId
             AND ttIndex.TableName  EQ ttTable.TableName
             AND ttIndex.TableId    EQ ttTable.TableId
              BY ttIndex.AreaNumber
              BY ttIndex.IndexName:

          FIND FIRST bfIndex NO-LOCK /* index IndexId (first 2 components only) */
               WHERE bfIndex.SnapshotId EQ vPrevSnapshot
                 AND bfIndex.DbId       EQ ttIndex.DbId
                 AND bfIndex.TableName  EQ ttIndex.TableName
                 AND bfIndex.TableId    EQ ttIndex.TableId
                 AND bfIndex.IndexName  EQ ttIndex.IndexName
                 AND bfIndex.IndexId    EQ ttIndex.IndexId
                 AND bfIndex.BgnTime    EQ ttIndex.BgnTime
          NO-ERROR.

          IF AVAILABLE bfIndex THEN
          ASSIGN vIndexInterval  = INTERVAL(ttIndex.EndTime2, bfIndex.EndTime2,
                                            "milliseconds":U) / 1000.0
                 vIndexReads     = ttIndex.IdxReads    - bfIndex.IdxReads
                 vIndexCreates   = ttIndex.IdxCreates  - bfIndex.IdxCreates
                 vIndexDeletes   = ttIndex.IdxDeletes  - bfIndex.IdxDeletes
                 vIndexBlkReads  = ttIndex.BlockReads  - bfIndex.BlockReads
                 vIndexBlkSplits = ttIndex.BlockSplits - bfIndex.BlockSplits
                 vIndexBlkFree   = ttIndex.BlockFree   - bfIndex.BlockFree
          . /* ASSIGN */
          ELSE
          ASSIGN vIndexInterval  = INTERVAL(ttIndex.EndTime2, ttIndex.BgnTime2,
                                            "milliseconds":U) / 1000.0
                 vIndexReads     = ttIndex.IdxReads
                 vIndexCreates   = ttIndex.IdxCreates
                 vIndexDeletes   = ttIndex.IdxDeletes
                 vIndexBlkReads  = ttIndex.BlockReads
                 vIndexBlkSplits = ttIndex.BlockSplits
                 vIndexBlkFree   = ttIndex.BlockFree
          . /* ASSIGN */

/* If table's interval has an unknown value (e.g. due to an inccorect format)
   then report the absolute values for indexes as well:
*/        IF vTableInterval NE ? THEN
          ASSIGN vIndexReads     = vIndexReads     / vIndexInterval
                 vIndexCreates   = vIndexCreates   / vIndexInterval
                 vIndexDeletes   = vIndexDeletes   / vIndexInterval
                 vIndexBlkReads  = vIndexBlkReads  / vIndexInterval
                 vIndexBlkSplits = vIndexBlkSplits / vIndexInterval
                 vIndexBlkFree   = vIndexBlkFree   / vIndexInterval
          . /* ASSIGN */

          PUT UNFORMATTED
                   ttDb.HostName           /* Db Host     */
            {&Sep} ttDb.PhysName           /* Db Name     */
            {&Sep} vPrevTime               /* From        */
            {&Sep} ttSnapshot.SnapshotTime /* To          */
            {&Sep} vTableInterval          /* Interval    */
            {&Sep} ttTable.TableName       /* Table       */
            {&Sep} ttTable.TableId         /* Table #     */
            {&Sep} ttTable.NumFld          /* Fields      */
            {&Sep} ttTable.NumKey          /* Indexes     */
            {&Sep} ttTable.AltBufPool      /* B2          */
            {&Sep} ttTable.AreaName        /* Tbl Area    */
            {&Sep} ttTable.AreaNumber      /* Area #      */
            {&Sep} ttTable.AreaHWM         /* HWM         */
            {&Sep} ttIndex.IndexName       /* Index       */
            {&Sep} ttIndex.IndexId         /* Index #     */
            {&Sep} ttIndex.NumComp         /* Fields      */
            {&Sep} ttIndex.IndexInfo       /* State       */
            {&Sep} ttIndex.AltBufPool      /* B2          */
            {&Sep} ttIndex.AreaName        /* Idx Area    */
            {&Sep} ttIndex.AreaNumber      /* Area #      */
            {&Sep} ttIndex.AreaHWM         /* HWM         */
            {&Sep} ttIndex.LevelCount      /* Levels      */
            {&Sep} vTableReads             /* RecReads    */
            {&Sep} vIndexReads             /* IdxReads    */
            {&Sep} vTableUpdates           /* RecUpdates  */
            {&Sep} vTableCreates           /* RecCreates  */
            {&Sep} vIndexCreates           /* IdxCreates  */
            {&Sep} vTableDeletes           /* RecDeletes  */
            {&Sep} vIndexDeletes           /* IdxDeletes  */
            {&Sep} vTableBlkReads          /* RecBlkReads */
            {&Sep} vIndexBlkReads          /* IdxBlkReads */
            {&Sep} vIndexBlkSplits         /* BlockSplits */
            {&Sep} vIndexBlkFree           /* BlockFree   */
          SKIP . /* PUT */
        END. /* FOR EACH ttIndex */
      END. /* FOR EACH ttTable */

      ASSIGN vPrevSnapshot = ttSnapshot.SnapshotId.

    END. /* FOR EACH ttSnapshot, EACH ttDb  */

    PUT CONTROL NULL(0).
    OUTPUT CLOSE.
  END.

/*OS-DELETE VALUE(vSeqDmpFile).*/
  IF NOT CAN-FIND(FIRST ttSequence) THEN
  ASSIGN vSeqDmpFile = "":U.
  ELSE
  DO:
    RUN CheckOutputFile(INPUT-OUTPUT vSeqDmpFile).

    OUTPUT TO VALUE(vSeqDmpFile).
    PUT UNFORMATTED
             "Db Host"
      {&Sep} "Db Name"
      {&Sep} "From"
      {&Sep} "To"
      {&Sep} "Interval"
      {&Sep} "Sequence"
      {&Sep} "Seq #"
      {&Sep} "Updates"
    SKIP . /* PUT */

    ASSIGN vPrevSnapshot = ?.
    FOR EACH ttSnapshot NO-LOCK /* index SnapshotTime */
          BY ttSnapshot.SnapshotTime:

      FOR EACH ttDb       NO-LOCK , /* index PhysName */
          EACH ttSequence NO-LOCK /* index TableId (first 2 components only) */
         WHERE ttSequence.SnapshotId EQ ttSnapshot.SnapshotId
           AND ttSequence.DbId       EQ ttDb.DbId
         BY    ttDb.HostName
         BY    ttDb.PhysName
         BY IF ttSequence.SeqId NE ? THEN ttSequence.SeqName ELSE ?:

        FIND FIRST bfSequence NO-LOCK
             WHERE bfSequence.SnapshotId EQ vPrevSnapshot
               AND bfSequence.DbId       EQ ttSequence.DbId
               AND bfSequence.SeqName    EQ ttSequence.SeqName
               AND bfSequence.SeqId      EQ ttSequence.SeqId
               AND bfSequence.BgnTime    EQ ttSequence.BgnTime
        NO-ERROR.

        IF AVAILABLE bfSequence THEN
        ASSIGN
          vPrevTime = bfSequence.EndTime
          vSeqInterval = INTERVAL(ttSequence.EndTime2, bfSequence.EndTime2,
                                  "milliseconds":U) / 1000.0
          vSeqUpdates = ttSequence.SeqUpdates - bfSequence.SeqUpdates
        . /* ASSIGN */
        ELSE
        ASSIGN
          vPrevTime = ttSequence.BgnTime
          vSeqInterval = INTERVAL(ttSequence.EndTime2, ttSequence.BgnTime2,
                                  "milliseconds":U) / 1000.0
          vSeqUpdates = ttSequence.SeqUpdates
        . /* ASSIGN */

        IF vSeqInterval NE ? THEN
        ASSIGN vSeqUpdates = vSeqUpdates / vSeqInterval.

        PUT UNFORMATTED
                 ttDb.HostName           /* Db Host  */
          {&Sep} ttDb.PhysName           /* Db Name  */
          {&Sep} vPrevTime               /* From     */
          {&Sep} ttSnapshot.SnapshotTime /* To       */
          {&Sep} vSeqInterval            /* Interval */
          {&Sep} ttSequence.SeqName      /* Sequence */
          {&Sep} ttSequence.SeqId        /* Seq #    */
          {&Sep} vSeqUpdates             /* Updates  */
        SKIP. /* PUT */
      END. /* FOR EACH ttSequence */

      ASSIGN vPrevSnapshot = ttSnapshot.SnapshotId.

    END. /* FOR EACH ttSnapshot, EACH ttDb */

    PUT CONTROL NULL(0).
    OUTPUT CLOSE.
  END.

  IF vDbStatFile NE "":U THEN
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
  IF vDbStatFile NE ? OR vSeqDmpFile NE "":U THEN
  MESSAGE
    "See results in:" SKIP
    vDbStatFile SKIP
    vSeqDmpFile
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE. /* PutDbStat */

/* ------------------------------------------------------------------------- */

