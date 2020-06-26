DEFINE VARIABLE vBaseDir AS CHARACTER NO-UNDO INITIAL
/* "Basic directory with the input files".*/
  "D:\Support\VTB24\dbanalys\Recent".

UPDATE vBaseDir LABEL "Dir" FORMAT "x(72)" 
  HELP "Enter directory with input files (dbanalys and auxiliary files)."
WITH SIDE-LABEL.

RUN ProcessDir(vBaseDir, ?, "ObjectInfo*~~~.txt", "LoadObjectInfo":U).
RUN ProcessDir(vBaseDir, ?, "*~~~.df",            "LoadDfFiles":U).
RUN ProcessDir(vBaseDir, ?, "*~~~.st",            "LoadStFiles":U).
RUN ProcessDir(vBaseDir, ?, "*~~~.log",           "LoadDbanalys":U).
RUN DataMatching.
RUN DumpDbanalys(vBaseDir). /* Prefix for output files */

/* ------------------------------------------------------------------------
    File        : LoadDbanalys.p
    Purpose     : Load the output of proutil -C dbanalys

    Author(s)   : George Potemkin
    Created     : January 17, 2015
    Modified    : Febrary 28, 2015
    Version     : 1.4
    
    The most recent version can be downloaded from here:
    ftp://ftp.progress-tech.ru/pub/Users/george/Programs/LoadDbanalys.p
    
    Syntax      : See the examples above.
    
    The output files has the suffixes:
      1. Databases.txt,
      2. Areas.txt,
      3. Tables.txt,
      4. Indexes.txt,
      5. LOBs.txt,
      6. Chains.txt,
      7. Sources.txt

    Comments    : The output files can be opened in Excel.
    
    Input files:
    1. Dbanalys output files. It can be a bunch of the files:
       for the different databases,
       for the same database at the different days,
       for the similar databases from the different branches.
       
    The auxiliary files:
    2. ObjectInfo.*.txt files created by ObjectInfo.p:
    ftp://ftp.progress-tech.ru/pub/Users/george/Programs/ObjectInfo.p
    It creates a file (one for all connected databases) with information
    about all database objects. Some information (like table numbers) are
    missed in dbanalys. Without such information the reports created by
    LoadDbanalys.p will be incomplete.
    
    3. The .df and .st files.
    They are NOT required if you have the ObjectInfo.*.txt files.
    They will be used only as last resort if LoadDbanalys.p will fail to find
    the auxiliary files that exactly match the database structures in dbanalys.

    Fixes       :

V1.1 (Febrary 18, 2015): 
      Area matching now takes into account the types of the storage areas.
V1.2 (Febrary 25, 2015):
      Index (ttTable.TableId) was added to help DataMatching.
V1.3 (Febrary 26, 2015):
      String2Byte() function now uses DECIMAL instead of INTEGER;
      "Size%" column in Tables.txt now uses ttTable.BlockCount
      instead of TableSize.
V1.4 (Febrary 28, 2015):
      LoadObjectInfo is more flexible to the format changes of
      the output file from ObjectInfo.p.
------------------------------------------------------------------------ */

DEFINE TEMP-TABLE ttDatabase NO-UNDO
  FIELD SourceType   AS INTEGER
  FIELD SourceId     AS INTEGER
  FIELD SourceFile   AS CHARACTER
  FIELD SourceName   AS CHARACTER
  FIELD SourcePath   AS CHARACTER
  FIELD Location     AS CHARACTER
  FIELD Blocksize    AS INTEGER
  FIELD DbanalysDate AS CHARACTER
  FIELD DbanalysExec AS INTEGER
  FIELD ListID       AS INTEGER EXTENT 4
  FIELD UseCount     AS INTEGER
  INDEX SourceId     IS UNIQUE
        SourceId
  INDEX SourcePath
        SourceType
        SourceName
        SourcePath
. /* DEFINE TEMP-TABLE ttDatabase */

/* AREA "Table Area" : 7  BLOCK ANALYSIS */

DEFINE TEMP-TABLE ttArea NO-UNDO
  FIELD SourceId     AS INTEGER
  FIELD AreaNumber   AS INTEGER
  FIELD AreaName     AS CHARACTER
  FIELD TableCount   AS INTEGER INITIAL 0
  FIELD IndexCount   AS INTEGER INITIAL 0
  FIELD LobCount     AS INTEGER INITIAL 0
  FIELD HighBlocks   AS INT64   INITIAL 0 /*Current high water mark:         */
  FIELD RecordBlocks AS INT64   INITIAL 0 /*record block(s) found in the area*/
  FIELD IndexBlocks  AS INT64   INITIAL 0 /*index block(s) found in the area */
  FIELD LobBlocks    AS INT64   INITIAL 0 /* as an estimation                */
  FIELD RecordCount  AS INT64   INITIAL 0 /*as sum of ttTable.RecordCount*/
  FIELD TotalBlocks  AS INT64   INITIAL 0 /*block(s) found in the area.      */
  FIELD FreeBlocks   AS INT64   INITIAL 0 /*free block(s) found in the area  */
  FIELD EmptyBlocks  AS INT64   INITIAL 0 /*empty block(s) found in the area */
  FIELD ObjectBlocks AS INT64   INITIAL 0 /*object block(s) found in the area*/
  FIELD ObjectList   AS INT64   INITIAL 0 /*cluster list block(s) found in ..*/
  FIELD ClusterList  AS INT64   INITIAL 0 /*object list block(s) found in ...*/
  FIELD ClusterMap   AS INT64   INITIAL 0 /*cluster map block(s) found in ...*/
  FIELD FreeClusters AS INT64   INITIAL 0 /* found in the free cluster chain.*/
  FIELD RecPerBlock  AS INTEGER INITIAL ?
  FIELD ClusterSize  AS INTEGER INITIAL ?
  INDEX AreaNumber   IS UNIQUE
        SourceId
        AreaNumber
  INDEX AreaName     IS UNIQUE
        SourceId
        AreaName
. /* DEFINE TEMP-TABLE ttArea */

/* RECORD BLOCK SUMMARY FOR AREA "Table Area" : 7 */

DEFINE TEMP-TABLE ttTable NO-UNDO
  FIELD SourceId    AS INTEGER
  FIELD TableId     AS INTEGER /* non-unique */
  FIELD TableOwner  AS CHARACTER
  FIELD TableName   AS CHARACTER
  FIELD AreaNumber  AS INTEGER
  FIELD AreaName    AS CHARACTER
  FIELD BlockCount  AS INT64
  FIELD RecordCount AS INT64
  FIELD FragmentCnt AS INT64
  FIELD TableSize   AS INT64
  FIELD MinRecSize  AS INTEGER
  FIELD MaxRecSize  AS INTEGER
  FIELD MeanRecSize AS INTEGER
  FIELD ScatterFact AS CHARACTER
  INDEX TableName   IS UNIQUE
        SourceId
        TableName
  INDEX TableId
        SourceId
        TableId
  INDEX AreaNumber
        SourceId
        AreaNumber
. /* DEFINE TEMP-TABLE ttTable */

/* INDEX BLOCK SUMMARY FOR AREA "Index Area" : 8 */

DEFINE TEMP-TABLE ttIndex NO-UNDO
  FIELD SourceId    AS INTEGER
  FIELD IndexId     AS INTEGER
  FIELD TableOwner  AS CHARACTER
  FIELD TableName   AS CHARACTER
  FIELD IndexName   AS CHARACTER
  FIELD ActiveIndex AS LOGICAL
  FIELD IndexAttr   AS CHARACTER
  FIELD AreaNumber  AS INTEGER
  FIELD AreaName    AS CHARACTER
  FIELD FieldCount  AS INTEGER
  FIELD LevelCount  AS INTEGER
  FIELD BlockCount  AS INT64
  FIELD IndexSize   AS INT64
  INDEX IndexName   IS UNIQUE
        SourceId
        TableName
        IndexName
  INDEX IndexId     IS UNIQUE
        SourceId
        IndexId
  INDEX AreaNumber
        SourceId
        AreaNumber
. /* DEFINE TEMP-TABLE ttIndex */

/* 0 block(s) found in the free chain of Blob object 1 */

DEFINE TEMP-TABLE ttLOB NO-UNDO
  FIELD SourceId   AS INTEGER
  FIELD LobType    AS CHARACTER
  FIELD LobId      AS INTEGER
  FIELD TableOwner AS CHARACTER
  FIELD TableName  AS CHARACTER
  FIELD FieldName  AS CHARACTER
  FIELD AreaNumber AS INTEGER
  FIELD AreaName   AS CHARACTER
  FIELD LobAttr    AS CHARACTER
  FIELD BlockCount AS INT64
  INDEX LobId      IS UNIQUE
        SourceId
        LobId
. /* DEFINE TEMP-TABLE ttLOB */

/* [FREE | RM | INDEX DELETE] CHAIN ANALYSIS */
/* 505 block(s) found in the free chain of Master object 0      */
/* 0 block(s) found in the free chain of Table object 1         */
/* 1312170 block(s) found in the RM chain of Table object 1     */
/* 159 block(s) found in the free chain of Index object 8       */
/* 0 block(s) found in the Index Delete chain of Index object 8 */

DEFINE TEMP-TABLE ttChain NO-UNDO
  FIELD SourceId     AS INTEGER
  FIELD ObjectID     AS INTEGER
  FIELD ObjectOwner  AS CHARACTER
  FIELD ObjectName   AS CHARACTER
  FIELD ObjectType   AS CHARACTER
  FIELD ObjectBlocks AS INTEGER
  FIELD AreaNumber   AS INTEGER
  FIELD AreaName     AS CHARACTER
  FIELD ChainType    AS CHARACTER /*FREECHN,RMCHN,LOCKCHN */
  FIELD ChainBlocks  AS INTEGER
  INDEX ObjectChain  IS UNIQUE
        SourceId
        AreaNumber
        ObjectType
        ObjectID
        ChainType
  INDEX ObjectID
        SourceId
        ObjectID
. /* DEFINE TEMP-TABLE ttArea */

DEFINE TEMP-TABLE ttList NO-UNDO
  FIELD ListId     AS INTEGER
  FIELD SourceName AS CHARACTER
  FIELD ListType   AS INTEGER /* 1 = UsedAreas, 2 = AllAreas, 3 = OnlyNames */
  FIELD SortList   AS CHARACTER CASE-SENSITIVE
  INDEX ListId     IS UNIQUE
        SourceName
        ListType
        ListId
. /* DEFINE TEMP-TABLE ttList */

DEFINE TEMP-TABLE ttText NO-UNDO
  FIELD TextId    AS CHARACTER
  FIELD TextNum   AS INTEGER
  FIELD TextValue AS CHARACTER
  FIELD TextType  AS INTEGER
  INDEX TextNum   IS UNIQUE
        TextType
        TextNum
  INDEX TextValue IS UNIQUE
        TextType
        TextValue
. /* DEFINE TEMP-TABLE ttText */

DEFINE TEMP-TABLE ttOSFile NO-UNDO
  FIELD osFileName AS CHARACTER
  FIELD osFilePath AS CHARACTER
  FIELD osFileAttr AS CHARACTER
  INDEX osFileName
        osFileName
. /* DEFINE TEMP-TABLE ttOSFile */

DEFINE VARIABLE vTime1 AS INTEGER NO-UNDO INITIAL ?.
DEFINE VARIABLE vTime2 AS INTEGER NO-UNDO.
DEFINE VARIABLE vTime3 AS INTEGER NO-UNDO.

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

FUNCTION String2ExcelDate RETURNS CHARACTER (INPUT ipString AS CHARACTER).

/* Input string must have a format like: Tue Jul 27 12:11:45 2004 */
/* Return the value in format: DD:MM:YYYYY HH:MM:SS (eg 27/07/2004 12:11:45)*/

  DEFINE VARIABLE vMonthList AS CHARACTER NO-UNDO
    INITIAL "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec".

  ASSIGN ipString = TRIM(REPLACE(ipString, "  ":U, " ":U))
         ipString =
             /* Day   */ STRING(INTEGER(ENTRY(3, ipString, " ":U)))
   + "/":U + /* Month */ STRING(LOOKUP (ENTRY(2, ipString, " ":U), vMonthList))
   + "/":U + /* Year  */ STRING(INTEGER(ENTRY(5, ipString, " ":U)))
   + " ":U + /* Time  */ ENTRY(4, ipString, " ":U)
  NO-ERROR.

  RETURN ipString.

END FUNCTION. /* String2ExcelDate */

/* ------------------------------------------------------------------------- */

FUNCTION String2Byte RETURNS INT64 (INPUT ipString AS CHARACTER).

  DEFINE VARIABLE vSize AS INT64 NO-UNDO INITIAL ?.

  ASSIGN vSize = INTEGER(SUBSTRING(ipString, 1, LENGTH(ipString) - 1))
                 WHEN ipString MATCHES "*B":U
         vSize = DECIMAL(SUBSTRING(ipString, 1, LENGTH(ipString) - 1))
               * 1024.0
                 WHEN ipString MATCHES "*K":U
         vSize = DECIMAL(SUBSTRING(ipString, 1, LENGTH(ipString) - 1))
               * 1048576.0
                 WHEN ipString MATCHES "*M":U
         vSize = DECIMAL(SUBSTRING(ipString, 1, LENGTH(ipString) - 1))
               * 1073741824.0
                 WHEN ipString MATCHES "*G":U
         vSize = DECIMAL(SUBSTRING(ipString, 1, LENGTH(ipString) - 1))
               * 1099511627776.0
                 WHEN ipString MATCHES "*T":U
  NO-ERROR. /* ASSIGN */

  RETURN vSize.

END FUNCTION. /* String2Byte */

/* ------------------------------------------------------------------------- */

FUNCTION GetTextId RETURN CHARACTER (ipText AS CHARACTER, ipType AS INTEGER):

/* TextId returns the unique two character identificator for input text.
   The uniqueness is guaranteed for 64516 (=254^2) text values.
*/
  DEFINE VARIABLE vTextNum AS INTEGER NO-UNDO.
  DEFINE VARIABLE vChrCode AS INTEGER NO-UNDO.

  FOR FIRST ttText NO-LOCK
      WHERE ttText.TextType  EQ ipType
        AND ttText.TextValue EQ ipText:
    RETURN  ttText.TextId.
  END. /* FOR FIRST ttText */

  ASSIGN vTextNum = 0.
  FOR EACH ttText NO-LOCK
     WHERE ttText.TextType EQ ipType
        BY ttText.TextType DESCENDING
        BY ttText.TextNum  DESCENDING:
    ASSIGN vTextNum = ttText.TextNum + 1.
    LEAVE.
  END.

  DO TRANSACTION:
    CREATE ttText.
    ASSIGN ttText.TextNum   = vTextNum
           ttText.TextType  = ipType
           ttText.TextValue = ipText
           ttText.TextId    = "":U
    . /* ASSIGN */
  END.

/* vChrCode = MOD 254:
  1) to avoid the NULL byte;
  2) to reserve "~001" as a list delimiter or a "padding" constant.
*/
  DO WHILE vTextNum GT 0 TRANSACTION:
     ASSIGN vChrCode = vTextNum MOD 254
            ttText.TextId = CHR(vChrCode + 2) + ttText.TextId
            vTextNum = (vTextNum - vChrCode) / 254
     . /* ASSIGN */
  END.

/* Add the "padding" value: */
  DO WHILE LENGTH(ttText.TextId) LT 2 TRANSACTION:
     ASSIGN ttText.TextId = "~001" + ttText.TextId.
  END.

  RETURN ttText.TextId.

END FUNCTION. /* GetTextId */

/* ------------------------------------------------------------------------- */

FUNCTION GetListId RETURN INTEGER (ipType AS INTEGER, ipSource AS CHARACTER):

/* GetListId returns the unique identificator for new SortList.
   ttList record with new SortList should be created with ListId EQ 0.
*/
  DEFINE BUFFER bufList FOR ttList.
  DEFINE VARIABLE vListCount AS INTEGER NO-UNDO.

  FIND FIRST bufList EXCLUSIVE-LOCK
       WHERE bufList.SourceName EQ ipSource
         AND bufList.ListType   EQ ipType
         AND bufList.ListId     EQ 0
  NO-ERROR.

/* ttList with ListId EQ 0 should be created befor GetListId() call: */
  IF NOT AVAILABLE bufList THEN
  RETURN ?.

  ASSIGN vListCount = 1.
  FOR EACH ttList NO-LOCK /* scan by SortList */
     WHERE ttList.SourceName EQ ipSource
       AND ttList.ListType   EQ ipType
       AND ttList.ListId     GT bufList.ListId /* = 0 */ :

    ASSIGN vListCount = vListCount + 1.
    IF ttList.SortList NE bufList.SortList THEN
    NEXT.

    DELETE bufList.
    RETURN ttList.ListId.
  END.

  ASSIGN bufList.ListId = vListCount.
  RETURN bufList.ListId.

END FUNCTION. /* GetListId */

/* ------------------------------------------------------------------------- */

PROCEDURE AddToList:
  DEFINE INPUT PARAMETER ipType   AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipItem   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipSource AS CHARACTER NO-UNDO.

/* ttList.ListId EQ 0 is for ttList.SortList under construction.
   Later the GetListId() function will change it to the unique value.
*/
  FOR FIRST ttList EXCLUSIVE-LOCK
      WHERE ttList.SourceName EQ ipSource
        AND ttList.ListType   EQ ipType
        AND ttList.ListId     EQ 0
  TRANSACTION:
    ASSIGN  ttList.SortList = ttList.SortList + GetTextId(ipItem, ipType).
    RETURN.
  END.

  DO TRANSACTION:
    CREATE ttList.
    ASSIGN ttList.SourceName = ipSource
           ttList.ListType   = ipType
           ttList.ListId     = 0
           ttList.SortList = GetTextId(ipItem, ipType)
    . /* ASSIGN */
  END.
END PROCEDURE. /* AddToList */


/* ------------------------------------------------------------------------- */

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

  IF vTime1 EQ ? THEN
  ASSIGN vTime1 = ETIME.

  ASSIGN FILE-INFO:FILE-NAME = ipInputDir.
  IF FILE-INFO:FULL-PATHNAME EQ ? THEN
  RETURN.

  ASSIGN ipInputDir = FILE-INFO:FULL-PATHNAME.

  STATUS DEFAULT SUBSTITUTE("Running &1. Please wait...", ipProcedure).

  RUN GetOSFiles(INPUT ipInputDir, INPUT ipFileMask, INPUT ipMaxDirLevel).

  FOR EACH ttOSFile NO-LOCK:
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

PROCEDURE LoadDbanalys.

  DEFINE INPUT PARAMETER ipInputFile AS CHARACTER NO-UNDO.

  &SCOPED-DEFINE SourceType 0

  DEFINE VARIABLE vSlash       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDirName     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vSourceId    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vDbPath      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDbName      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vBlockSize   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vDate        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vLine        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vItem        AS CHARACTER NO-UNDO EXTENT 9.
  DEFINE VARIABLE vAreaName    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vAreaNumber  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vObjectId    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vObjectType  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vChainBlocks AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vChainType   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vTableId     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTableOwner  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vTableName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vIndexId     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vIndexName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vPrevName    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vLastName    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vActiveIndex AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE i           AS INTEGER   NO-UNDO.

/* vDirName is a directory name (without path) of input file.
   It might represent, for example, a branch where dbanalys came from.
   The value will be saved as ttDatabase.Location.
*/
  ASSIGN vSlash   = IF OPSYS EQ "Unix":U THEN "/":U ELSE "~\":U
         i        = NUM-ENTRIES(ipInputFile, vSlash)
         vDirName = ENTRY(i - 1, ipInputFile, vSlash)
  . /* ASSIGN */

  INPUT FROM VALUE(ipInputFile).

/* Load the header of dbanalys:
Database: /path/to/database
Blocksize: 4096
RecSpaceSearchDepth: 5
Options: chanalys ixanalys tabanalys
Date: Fri Jan 16 17:47:06 2015
*/
ImportHeader:
  REPEAT:
    ASSIGN vLine = "":U.
    IMPORT UNFORMATTED vLine.
    IF vLine EQ "":U  THEN
    NEXT.
/*
Database: /path/to/database
*/  IF vLine MATCHES "Database: *":U THEN
    ASSIGN i = INDEX(vLine, " ":U)
           vDbPath = SUBSTRING(vLine, i + 1)
           i = MAX(R-INDEX(vDbPath, "/":U), R-INDEX(vDbPath, "~\":U))
           vDbName = SUBSTRING(vDbPath, i + 1)
           vDbPath = SUBSTRING(vDbPath, 1,
                        LENGTH(vDbPath) - LENGTH(vDbName) - 1)
    NO-ERROR. /* ASSIGN */
    ELSE

/*
Blocksize: 4096
*/  IF vLine MATCHES "Blocksize: *":U THEN
    ASSIGN i = INDEX(vLine, " ":U)
           vBlockSize = INTEGER(SUBSTRING(vLine, i + 1))
    NO-ERROR. /* ASSIGN */
    ELSE

/*
Date: Fri Jan 16 17:47:06 2015
*/  IF vLine MATCHES "Date: *":U THEN
    DO TRANSACTION:
      ASSIGN i = INDEX(vLine, " ":U)
             vDate = SUBSTRING(vLine, i + 1)
             vSourceId = 0
      . /* ASSIGN */

      FOR EACH ttDatabase NO-LOCK /* INDEX SourceId */
            BY ttDatabase.SourceId DESCENDING:
        ASSIGN vSourceId = ttDatabase.SourceId + 1.
        LEAVE.
      END.

      CREATE ttDatabase.
      ASSIGN ttDatabase.SourceType   = {&SourceType}
             ttDatabase.SourceId     = vSourceId
             ttDatabase.SourceFile   = ipInputFile
             ttDatabase.SourceName   = vDbName
             ttDatabase.SourcePath   = vDbPath
             ttDatabase.Location     = vDirName
             ttDatabase.Blocksize    = vBlockSize
             ttDatabase.DbanalysDate = vDate
             ttDatabase.DbanalysExec = ?
             ttDatabase.UseCount     = 0
             ttDatabase.ListId       = ?
      . /* ASSIGN */

      LEAVE ImportHeader.
    END. /* DO TRANSACTION */
  END. /* REPEAT */

/* If the input file does not have the dbanalys header: */
  IF SEEK(INPUT) EQ ? THEN
  RETURN.

Chanalys:
  REPEAT:

    ASSIGN vLine = "":U.
    IMPORT UNFORMATTED vLine.
    ASSIGN vLine = TRIM(vLine).
    PROCESS EVENTS.

    IF vLine EQ "":U  THEN
    NEXT Chanalys.

/*
CHAIN ANALYSIS FOR AREA "Control Area" : 1
*/  IF vLine MATCHES "CHAIN ANALYSIS FOR AREA *":U THEN
    DO TRANSACTION:
      ASSIGN i = NUM-ENTRIES(vLine, " ":U)
             vAreaNumber = INTEGER(ENTRY(i, vLine, " ":U))
             i = i  - 2
             vAreaName = ENTRY(i, vLine, " ":U)
      . /* ASSIGN */

/* Ignore "Control Area": */
      IF vAreaNumber LT 6 THEN
      NEXT Chanalys.

      DO WHILE i GT 5:
        ASSIGN i = i  - 1
               vAreaName = ENTRY(i, vLine, " ":U) + " ":U + vAreaName
       . /* ASSIGN */
      END.
      ASSIGN vAreaName = TRIM(vAreaName, "~"":U).

      FIND FIRST ttArea /* INDEX AreaNumber */
           WHERE ttArea.SourceId   EQ ttDatabase.SourceId
             AND ttArea.AreaNumber EQ vAreaNumber
      NO-ERROR.
      IF NOT AVAILABLE ttArea THEN
      CREATE ttArea.
      ASSIGN ttArea.SourceId   = ttDatabase.SourceId
             ttArea.AreaNumber = vAreaNumber
             ttArea.AreaName   = vAreaName
      . /* ASSIGN */

      NEXT Chanalys.
    END. /* DO TRANSACTION */

/* 0 cluster(s) found in the free cluster chain.   */
    IF vLine MATCHES "* cluster(s) found in the free cluster chain.*":U THEN
    FOR FIRST ttArea /* INDEX AreaNumber */
        WHERE ttArea.SourceId   EQ ttDatabase.SourceId
          AND ttArea.AreaNumber EQ vAreaNumber:
      ASSIGN  ttArea.FreeClusters = INTEGER(ENTRY(1, vLine, " ":U)) NO-ERROR.
      NEXT Chanalys.
    END.

/* 191702527 block(s) found in the area.           */
    IF vLine MATCHES "* block(s) found in the area.*":U THEN
    FOR FIRST ttArea /* INDEX AreaNumber */
        WHERE ttArea.SourceId   EQ ttDatabase.SourceId
          AND ttArea.AreaNumber EQ vAreaNumber:
      ASSIGN  ttArea.TotalBlocks = INTEGER(ENTRY(1, vLine, " ":U)) NO-ERROR.
      NEXT Chanalys.
    END.

/* Current high water mark: 191702527              */
    IF vLine MATCHES "*Current high water mark: *":U THEN
    FOR FIRST ttArea /* INDEX AreaNumber */
        WHERE ttArea.SourceId   EQ ttDatabase.SourceId
          AND ttArea.AreaNumber EQ vAreaNumber:
      ASSIGN  i = NUM-ENTRIES(vLine, " ":U)
              ttArea.HighBlocks =  INTEGER(ENTRY(i, vLine, " ":U))
      NO-ERROR.
      NEXT Chanalys.
    END.

/*     505 free block(s) found in the area         */
    IF vLine MATCHES "* free block(s) found in the area*":U THEN
    FOR FIRST ttArea /* INDEX AreaNumber */
        WHERE ttArea.SourceId   EQ ttDatabase.SourceId
          AND ttArea.AreaNumber EQ vAreaNumber:
      ASSIGN  ttArea.FreeBlocks =  INTEGER(ENTRY(1, vLine, " ":U)) NO-ERROR.
      NEXT Chanalys.
    END.

/*     191702012 record block(s) found in the area */
    IF vLine MATCHES "* record block(s) found in the area*":U THEN
    FOR FIRST ttArea /* INDEX AreaNumber */
        WHERE ttArea.SourceId   EQ ttDatabase.SourceId
          AND ttArea.AreaNumber EQ vAreaNumber:
      ASSIGN  ttArea.RecordBlocks =  INTEGER(ENTRY(1, vLine, " ":U)) NO-ERROR.
      NEXT Chanalys.
    END.

/*     0 index block(s) found in the area          */
    IF vLine MATCHES "* index block(s) found in the area*":U THEN
    FOR FIRST ttArea /* INDEX AreaNumber */
        WHERE ttArea.SourceId   EQ ttDatabase.SourceId
          AND ttArea.AreaNumber EQ vAreaNumber:
      ASSIGN  ttArea.IndexBlocks =  INTEGER(ENTRY(1, vLine, " ":U)) NO-ERROR.
      NEXT Chanalys.
    END.

/*     0 empty block(s) found in the area          */
    IF vLine MATCHES "* empty block(s) found in the area*":U THEN
    FOR FIRST ttArea /* INDEX AreaNumber */
        WHERE ttArea.SourceId   EQ ttDatabase.SourceId
          AND ttArea.AreaNumber EQ vAreaNumber:
      ASSIGN  ttArea.EmptyBlocks =  INTEGER(ENTRY(1, vLine, " ":U)) NO-ERROR.
      NEXT Chanalys.
    END.

/*     2 object block(s) found in the area         */
    IF vLine MATCHES "* object block(s) found in the area*":U THEN
    FOR FIRST ttArea /* INDEX AreaNumber */
        WHERE ttArea.SourceId   EQ ttDatabase.SourceId
          AND ttArea.AreaNumber EQ vAreaNumber:
      ASSIGN  ttArea.ObjectBlocks =  INTEGER(ENTRY(1, vLine, " ":U)) NO-ERROR.
      NEXT Chanalys.
    END.

/*     2 cluster list block(s) found in the area   */
    IF vLine MATCHES "* cluster list block(s) found in the area*":U THEN
    FOR FIRST ttArea /* INDEX AreaNumber */
        WHERE ttArea.SourceId   EQ ttDatabase.SourceId
          AND ttArea.AreaNumber EQ vAreaNumber:
      ASSIGN  ttArea.ClusterList =  INTEGER(ENTRY(1, vLine, " ":U)) NO-ERROR.
      NEXT Chanalys.
    END.

/*     1 object list block(s) found in the area    */
    IF vLine MATCHES "* object list block(s) found in the area*":U THEN
    FOR FIRST ttArea /* INDEX AreaNumber */
        WHERE ttArea.SourceId   EQ ttDatabase.SourceId
          AND ttArea.AreaNumber EQ vAreaNumber:
      ASSIGN  ttArea.ObjectList =  INTEGER(ENTRY(1, vLine, " ":U)) NO-ERROR.
      NEXT Chanalys.
    END.

/*     2 cluster map block(s) found in the area    */
    IF vLine MATCHES "* cluster map block(s) found in the area*":U THEN
    FOR FIRST ttArea /* INDEX AreaNumber */
        WHERE ttArea.SourceId   EQ ttDatabase.SourceId
          AND ttArea.AreaNumber EQ vAreaNumber:
      ASSIGN  ttArea.ClusterMap =  INTEGER(ENTRY(1, vLine, " ":U)) NO-ERROR.
      NEXT Chanalys.
    END.


/* 505 block(s) found in the free chain of Master object 0      */
/* 0 block(s) found in the free chain of Table object 1         */
/* 1312170 block(s) found in the RM chain of Table object 1     */
/* 159 block(s) found in the free chain of Index object 8       */
/* 0 block(s) found in the Index Delete chain of Index object 8 */
/* 0 block(s) found in the free chain of Blob object 1          */
/* 14129042 block(s) found in the RM chain of Blob object 1 */
    IF vLine MATCHES "* block(s) found in the * chain of * object *":U THEN
    DO TRANSACTION:
      ASSIGN i = NUM-ENTRIES(vLine, " ":U)
             vObjectId     = INTEGER(ENTRY(i, vLine, " ":U))
             vObjectType   = ENTRY(i - 2, vLine, " ":U)
             vChainBlocks  = INTEGER(ENTRY(1, vLine, " ":U))
             vChainType    = ENTRY(6, vLine, " ":U)
      NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
      NEXT Chanalys.

      ASSIGN vChainType = "Index-Delete":U
        WHEN vChainType EQ "INDEX":U.


      CREATE ttChain.
      ASSIGN ttChain.SourceId     = ttDatabase.SourceId
             ttChain.ObjectID     = vObjectId
             ttChain.ObjectOwner  = ?
             ttChain.ObjectName   = ?
             ttChain.ObjectBlocks = ?
             ttChain.ObjectType   = vObjectType
             ttChain.AreaNumber   = vAreaNumber
             ttChain.AreaName     = vAreaName
             ttChain.ChainType    = vChainType
             ttChain.ChainBlocks  = vChainBlocks
      . /* ASSIGN */

/* Only chain analys reports info about the LOBs:
   The object type is always "Blob" even for the CLOB fields.
   There are two chain types for LOBs - "free" and "RM":
0 block(s) found in the free chain of Blob object 1
14129042 block(s) found in the RM chain of Blob object 1
   Let's count the LOBs based on the free chains.
*/
      IF vObjectType NE "BLOB":U OR vChainType NE "FREE":U THEN
      NEXT Chanalys.

      CREATE ttLob.
      ASSIGN ttLOB.SourceId   = ttDatabase.SourceId
             ttLOB.AreaNumber = vAreaNumber
             ttLOB.AreaName   = vAreaName
             ttLOB.LobId      = vObjectId
             ttLOB.LobType    = ?
             ttLOB.TableOwner = ?
             ttLOB.TableName  = ?
             ttLOB.FieldName  = ?
      . /* ASSIGN */

      FOR FIRST ttArea /* INDEX AreaNumber */
          WHERE ttArea.SourceId   EQ ttDatabase.SourceId
            AND ttArea.AreaNumber EQ vAreaNumber:
        ASSIGN  ttArea.LobCount = ttArea.LobCount + 1.
      END.

      NEXT Chanalys.
    END. /* * block(s) found in the * chain of * object * */

/* RECORD BLOCK SUMMARY FOR AREA "Schema Area" : 6 */
    IF vLine MATCHES "RECORD BLOCK SUMMARY FOR AREA *":U THEN
    DO:
      ASSIGN i = NUM-ENTRIES(vLine, " ":U)
             vAreaNumber = INTEGER(ENTRY(i, vLine, " ":U))
             i = i - 2
             vAreaName = ENTRY(i, vLine, " ":U)
      . /* ASSIGN */

/* Ignore "Control Area": */
      IF vAreaNumber LT 6 THEN
      NEXT Chanalys.

      DO WHILE i GT 6:
        ASSIGN i = i - 1
               vAreaName = ENTRY(i, vLine, " ":U) + " ":U + vAreaName
        . /* ASSIGN */
      END.
      ASSIGN vAreaName = TRIM(vAreaName, "~"":U).

      FIND FIRST ttArea /* INDEX AreaNumber */
           WHERE ttArea.SourceId   EQ ttDatabase.SourceId
             AND ttArea.AreaNumber EQ vAreaNumber
      NO-ERROR.
      IF NOT AVAILABLE ttArea THEN
      CREATE ttArea.
      ASSIGN ttArea.SourceId   = ttDatabase.SourceId
             ttArea.AreaNumber = vAreaNumber
             ttArea.AreaName   = vAreaName
      . /* ASSIGN */

/* Skip the header:
-------------------------------------------------------
                           -Record Size (B)-    ---Fragments--- Scatter
Table    Records    Size   Min   Max  Mean         Count Factor  Factor
*/    DO i = 1 TO 3:
        IMPORT ^.
      END.

Tabanalys:
      REPEAT:
        ASSIGN vItem = "":U.
        IMPORT vItem.

        IF vItem[1] EQ "":U
        OR vItem[1] BEGINS "---":U THEN
        LEAVE Tabanalys.

        ASSIGN vTableName  = vItem[1]
               i = 0
        . /* ASSIGN */

        IF NUM-ENTRIES(vTableName, ".":U) EQ 2 THEN
        ASSIGN vTableOwner = ENTRY(1, vTableName, ".":U)
               vTableName  = ENTRY(2, vTableName, ".":U)
        . /* ASSIGN */
        ELSE
        ASSIGN vTableOwner = "":U.

/* In old Progress versions the line can be splitted after long table name: */
        IF vItem[2] EQ "":U THEN
        DO:
          ASSIGN i = 1. /* shift the fields by one */
          IMPORT vItem.
        END.

/* Ignore metaschema tables: */
        IF vTableName BEGINS "_":U THEN
        NEXT Tabanalys.

/* Unexpected number of items per line: */
        IF vItem[9 - i] EQ "":U THEN
        LEAVE Tabanalys.

/*
RECORD BLOCK SUMMARY FOR AREA "area" : 7
-------------------------------------------------------
                                 -Record Size (B)-     ---Fragments--- Scatter
Table          Records    Size   Min   Max  Mean          Count Factor  Factor
PUB.table   4232953795  668.3G    43 19243   169     4233060260    1.0     1.0
1           2           3         4  5       6       7             8       9
*/
        CREATE ttTable.
        ASSIGN ttTable.SourceId    = ttDatabase.SourceId
               ttTable.TableOwner  = vTableOwner
               ttTable.TableName   = vTableName
               ttTable.TableId     = ?
               ttTable.AreaNumber  = vAreaNumber
               ttTable.AreaName    = vAreaName
               ttTable.BlockCount  = ?
               ttTable.RecordCount =       INT64(vItem[2 - i])
               ttTable.FragmentCnt =       INT64(vItem[7 - i])
               ttTable.TableSize   = String2Byte(vItem[3 - i])
               ttTable.MinRecSize  =     INTEGER(vItem[4 - i])
               ttTable.MaxRecSize  =     INTEGER(vItem[5 - i])
               ttTable.MeanRecSize =     INTEGER(vItem[6 - i])
               ttTable.ScatterFact =             vItem[9 - i]
        NO-ERROR. /* ASSIGN */
        
        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
        DELETE ttTable.

        FOR FIRST ttArea /* INDEX AreaNumber */
            WHERE ttArea.SourceId   EQ ttDatabase.SourceId
              AND ttArea.AreaNumber EQ vAreaNumber:
          ASSIGN  ttArea.TableCount  = ttArea.TableCount + 1
                  ttArea.RecordCount = ttArea.RecordCount
                                     + INT64(vItem[2 - i])
          NO-ERROR. /* ASSIGN */
        END.

      END. /* Tabanalys: REPEAT */

      NEXT  Chanalys.
    END. /* RECORD BLOCK SUMMARY FOR AREA */


/* INDEX BLOCK SUMMARY FOR AREA "Schema Area" : 6 */
    IF vLine MATCHES "INDEX BLOCK SUMMARY FOR AREA *":U THEN
    DO:
      ASSIGN i = NUM-ENTRIES(vLine, " ":U)
             vAreaNumber = INTEGER(ENTRY(i, vLine, " ":U))
             i = i - 2
             vAreaName = ENTRY(i, vLine, " ":U)
      . /* ASSIGN */

/* Ignore "Control Area": */
      IF vAreaNumber LT 6 THEN
      NEXT Chanalys.

      DO WHILE i GT 6:
        ASSIGN i = i - 1
               vAreaName = ENTRY(i, vLine, " ":U) + " ":U + vAreaName
        . /* ASSIGN */
      END.
      ASSIGN vAreaName = TRIM(vAreaName, "~"":U).

      FIND FIRST ttArea /* INDEX AreaNumber */
           WHERE ttArea.SourceId   EQ ttDatabase.SourceId
             AND ttArea.AreaNumber EQ vAreaNumber
      NO-ERROR.
      IF NOT AVAILABLE ttArea THEN
      CREATE ttArea.
      ASSIGN ttArea.SourceId   = ttDatabase.SourceId
             ttArea.AreaNumber = vAreaNumber
             ttArea.AreaName   = vAreaName
      . /* ASSIGN */

/* Skip the header:
-------------------------------------------------------
Table        Index  Fields Levels         Blocks    Size  % Util  Factor
*/    DO i = 1 TO 2:
        IMPORT ^.
      END.

      ASSIGN vTableName = ?
             vPrevName  = ?
             vLastName  = ?
      . /* ASSIGN */
Ixanalys:
      REPEAT:

        ASSIGN vItem = "":U.
        IMPORT vItem.

        IF vItem[1] EQ "":U
        OR vItem[1] BEGINS "---":U THEN
        LEAVE Ixanalys.

/* 8 items per line is the standard for index statistics: */
        IF vItem[8] NE "":U THEN
        ASSIGN i = 0  /* shift the fields by one */
               vPrevName  = vLastName
               vTableName = vLastName
               vIndexName = vItem[1]
        . /* ASSIGN */
        ELSE
/* 1 item per line is either a table name or a long index name : */
        IF vItem[2] EQ "":U THEN
        DO:
          ASSIGN vPrevName = vLastName
                 vLastName = vItem[1]
          . /* ASSIGN */
          NEXT Ixanalys.
        END.
        ELSE
        IF vItem[7] NE "":U THEN
        ASSIGN i = 1  /* shift the fields by one */
               vTableName = vPrevName
               vIndexName = vLastName
               vLastName  = vTableName
        . /* ASSIGN */
        ELSE
/* Unexpected number of items per line in ixanalys: */
        LEAVE Ixanalys.

/* Unexpected structure of ixanalys: */
        IF vTableName EQ ? THEN
        LEAVE Ixanalys.

        IF vIndexName MATCHES "*(inactive)":U THEN
        ASSIGN vIndexName   = ENTRY(1, vIndexName, "(":U)
               vActiveIndex = FALSE
        . /* ASSIGN */
        ELSE
        ASSIGN vActiveIndex = TRUE.

        IF NUM-ENTRIES(vTableName, ".":U) EQ 2 THEN
        ASSIGN vTableOwner = ENTRY(1, vTableName, ".":U)
               vTableName  = ENTRY(2, vTableName, ".":U)
        . /* ASSIGN */
        ELSE
        ASSIGN vTableOwner = "":U.

/* Ignore metaschema tables: */
        IF vTableName BEGINS "_":U THEN
        NEXT Ixanalys.
/*
INDEX BLOCK SUMMARY FOR AREA "area" : 8
-------------------------------------------------------
Table          Index  Fields Levels         Blocks    Size  % Util  Factor
PUB.table
  index1           9       3      5        6331211   10.0G    41.8     2.2
  index2          10       3      4        4258647    8.3G    51.7     2.0
  index3           8       6      5       47352557  111.9G    62.3     1.8
  index4          11       2      4       12508185   26.2G    55.3     1.9
  1               2        3      4       5          6        7        8
*/      CREATE ttIndex.
        ASSIGN ttIndex.SourceId    = ttDatabase.SourceId
               ttIndex.AreaNumber  = vAreaNumber
               ttIndex.AreaName    = vAreaName
               ttIndex.TableOwner  = vTableOwner
               ttIndex.TableName   = vTableName
               ttIndex.IndexName   = vIndexName
               ttIndex.ActiveIndex = vActiveIndex
               ttIndex.IndexId     =     INTEGER(vItem[2 - i])
               ttIndex.FieldCount  =     INTEGER(vItem[3 - i])
               ttIndex.LevelCount  =     INTEGER(vItem[4 - i])
               ttIndex.BlockCount  =     INTEGER(vItem[5 - i])
               ttIndex.IndexSize   = String2Byte(vItem[6 - i])
        NO-ERROR. /* ASSIGN */

        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
        DELETE ttIndex.

        FOR FIRST ttArea /* INDEX AreaNumber */
            WHERE ttArea.SourceId   EQ ttDatabase.SourceId
              AND ttArea.AreaNumber EQ vAreaNumber:
          ASSIGN  ttArea.IndexCount = ttArea.IndexCount + 1.
        END.

      END. /* Ixanalys: REPEAT */

      NEXT Chanalys.
    END. /* INDEX BLOCK SUMMARY FOR AREA */

/* database analysis complete Fri Jan 16 19:37:47 2015 */
    IF vLine MATCHES "database analysis complete *":U THEN
    DO TRANSACTION:

      DO i = 1 TO 3:
        ASSIGN vLine = TRIM(SUBSTRING(vLine, INDEX(vLine, " ":U) + 1)).
      END.

      ASSIGN ttDatabase.DbanalysExec =
             INTERVAL(String2DateTime(vLine),
                      String2DateTime(ttDatabase.DbanalysDate), "seconds":U)
      . /* ASSIGN */

      LEAVE Chanalys.
    END.

  END. /* Chanalys: REPEAT */

  INPUT CLOSE.

END PROCEDURE. /* LoadDbanalys */


/* ------------------------------------------------------------------------- */

PROCEDURE DataMatching.

  DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vSourceType  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vRecPerBlock AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vBytPerBlock AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vUnusedSpace AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vRestBlocks  AS INT64     NO-UNDO.
  DEFINE VARIABLE vMaxBlocks   AS INT64     NO-UNDO.
  DEFINE VARIABLE vItem        AS CHARACTER NO-UNDO.

  DEFINE BUFFER bfDatabase FOR ttDatabase.
  DEFINE BUFFER bfTable    FOR ttTable.
  DEFINE BUFFER bfIndex    FOR ttIndex.
  DEFINE BUFFER bfArea     FOR ttArea.
  DEFINE BUFFER bfLOB      FOR ttLOB.

  ASSIGN vTime2 = ETIME
         vTime1 = vTime2 - vTime1.
  STATUS DEFAULT SUBSTITUTE("Running &1. Please wait...",
                                  /* &1 */ ENTRY(1, PROGRAM-NAME(1), " ":U)).

/* Part 1: Check the internal relations in the loaded dbanalys data: ------ */

/* Rough estimation of the unsued space in each block: */
  ASSIGN vUnusedSpace =  56 /* block header in SAT2 */
                      + 150 /* CreateLimit */
                   /* + 2 * vRecPerBlock (record offset directory) */
  . /* ASSIGN */
  FOR EACH ttDatabase NO-LOCK
     WHERE ttDatabase.SourceType EQ 0, /* = only for dbanalys */

      EACH ttArea EXCLUSIVE-LOCK /* INDEX AreaNumber */
     WHERE ttArea.SourceId EQ ttDatabase.SourceId:

    PROCESS EVENTS.

/* if an area contains only one table then set TableId
   based on the chain's ObjectID : */
    IF ttArea.TableCount EQ 1 THEN
    FOR FIRST ttTable EXCLUSIVE-LOCK /* INDEX AreaNumber */
        WHERE ttTable.SourceId   EQ ttArea.SourceId
          AND ttTable.AreaNumber EQ ttArea.AreaNumber,

        FIRST ttChain NO-LOCK
        WHERE ttChain.SourceId   EQ ttArea.SourceId
          AND ttChain.AreaNumber EQ ttArea.AreaNumber
          AND ttChain.ObjectType EQ "TABLE":U
    TRANSACTION:
      ASSIGN ttTable.TableId = ttChain.ObjectID.
    END.

/* Total table size per area: */
    FOR EACH ttTable NO-LOCK /* INDEX AreaNumber */
       WHERE ttTable.SourceId   EQ ttArea.SourceId
         AND ttTable.AreaNumber EQ ttArea.AreaNumber
    TRANSACTION:
      ACCUMULATE ttTable.TableSize (TOTAL).
    END. /* FOR EACH ttTable  */

    ASSIGN vRestBlocks   = ttArea.RecordBlocks
/* Average number of the table bytes per block (if no LOBs): */
             vBytPerBlock  = IF ttArea.RecordBlocks EQ 0 THEN 0.0 ELSE
                  ((ACCUM TOTAL ttTable.TableSize) / ttArea.RecordBlocks)
      . /* ASSIGN */

/* If the area does not have the LOB objects then the table block count is
   the table size divided by the average number of bytes per block in the area:
*/  IF ttArea.LobCount EQ 0 THEN
    FOR EACH ttTable EXCLUSIVE-LOCK /* INDEX AreaNumber */
       WHERE ttTable.SourceId   EQ ttArea.SourceId
         AND ttTable.AreaNumber EQ ttArea.AreaNumber
    TRANSACTION:
      ASSIGN ttTable.BlockCount = IF vBytPerBlock EQ 0.0 THEN 0 ELSE
                               ROUND(ttTable.TableSize / vBytPerBlock, 0)
            vRestBlocks = 0
      . /* ASSIGN */
    END. /* FOR EACH ttTable EXCLUSIVE */
    ELSE

/* If the area contains the LOB objects then
   the table block count is estimated as a table size divided by block size:
*/  FOR EACH ttTable EXCLUSIVE-LOCK /* INDEX AreaNumber */
       WHERE ttTable.SourceId   EQ ttArea.SourceId
         AND ttTable.AreaNumber EQ ttArea.AreaNumber
    TRANSACTION:
      IF ttTable.RecordCount EQ 0 THEN
      ASSIGN ttTable.BlockCount = 0.
      ELSE
      ASSIGN vRecPerBlock = (ttDatabase.Blocksize - vUnusedSpace)
                          / ttTable.MeanRecSize
             vRecPerBlock = (ttDatabase.Blocksize - vUnusedSpace
                                                  - 2 * vRecPerBlock)
                          / ttTable.MeanRecSize
             ttTable.BlockCount = ttTable.RecordCount / vRecPerBlock
             vRestBlocks  = MAX(vRestBlocks - ttTable.BlockCount, 0)
      . /* ASSIGN */
    END. /* FOR EACH ttTable EXCLUSIVE */

/* If the area contains only one LOB object then: */
    IF ttArea.LobCount EQ 1 THEN
    FOR FIRST ttLOB EXCLUSIVE-LOCK /* INDEX ObjectChain */
        WHERE ttLOB.SourceId   EQ ttArea.SourceId
          AND ttLOB.AreaNumber EQ ttArea.AreaNumber
    TRANSACTION:
      ASSIGN ttLOB.BlockCount = vRestBlocks.
    END. /* FOR FIRST ttLOB  */

    DO TRANSACTION:
      ASSIGN ttArea.LobBlocks = vRestBlocks.
    END.

  END. /* FOR EACH ttDatabase, EACH ttArea */

/* Part 2: Get ListId for ttDatabase: ------------------------------------- */

/* Create the list of the database objects to find the matches
   between the different sources (dbanalys, *.st, *.df, ObjectInfo*.txt)

   To set ttTable.TableId, ttLOB.TableName and ttLOB.FieldName
   Dbanalys and DF should match by
     TableAreaName:TableName:TableId list (only where ttArea.TableCount eq 1)
    +IndexAreaName:TableName:IndexName:IndexId

   If the previous match failed then
   to set ttLOB.TableName and ttLOB.FieldName (only where ttArea.LobCount eq 1)
   Dbanalys and OI/DF should match by
     TableAreaName:TableName
    +IndexAreaName:TableName:IndexName

   To set the area attributes
   Dbanalys and OI/ST should match by
     AreaName:AreaNumber list (including the unused areas)
*/
  FOR EACH ttDatabase NO-LOCK:

    DEFINE VARIABLE vList AS LONGCHAR NO-UNDO EXTENT 4.
    ASSIGN vList = "".

    FOR EACH ttArea NO-LOCK /* INDEX AreaNumber + sorting by AreaName */
       WHERE ttArea.SourceId EQ ttDatabase.SourceId
          BY ttArea.AreaName:

      PROCESS EVENTS.

/* The list of the table's areas/names/IDs: */
      IF ttArea.TableCount EQ 1 THEN
      FOR FIRST ttTable NO-LOCK /* sorting by TableName */
          WHERE ttTable.SourceId   EQ ttArea.SourceId
            AND ttTable.AreaNumber EQ ttArea.AreaNumber:

        ASSIGN vItem = ttTable.AreaName + ":":U + ttTable.TableName.
    /*  RUN AddToList(2, vItem, ttDatabase.SourceName). */
        ASSIGN vList[2] = vList[2] + vItem.

        ASSIGN vItem = vItem + ":":U + STRING(ttTable.TableId).
     /* RUN AddToList(1, vItem, ttDatabase.SourceName). */
        ASSIGN vList[2] = vList[2] + vItem.

      END. /* FOR EACH ttTable */

/* The list of the index' areas/names/IDs: */
      FOR EACH ttIndex NO-LOCK /* sorting by TableName and IndexName */
         WHERE ttIndex.SourceId   EQ ttArea.SourceId
           AND ttIndex.AreaNumber EQ ttArea.AreaNumber
            BY ttIndex.TableName
            BY ttIndex.IndexName:

        ASSIGN vItem = ttIndex.AreaName  + ":":U +
                       ttIndex.TableName + ":":U +
                       ttIndex.IndexName
        . /* ASSIGN */
     /* RUN AddToList(2, vItem, ttDatabase.SourceName). */
        ASSIGN vList[2] = vList[2] + ":":U + vItem.

        ASSIGN vItem = vItem  + ":":U + STRING(ttIndex.IndexId).
     /* RUN AddToList(1, vItem, ttDatabase.SourceName). */
        ASSIGN vList[1] = vList[1] + ":":U + vItem.

      END. /* FOR EACH ttIndex */

/* The list of the area's names/numbers: */
      ASSIGN vItem = ttArea.AreaName + ":":U + STRING(ttArea.AreaNumber)
/* Is the storage area type 1 or type 2 ? */
             vItem = vItem + ":":U + STRING(
               CAN-FIND(FIRST ttChain
                        WHERE ttChain.SourceId   EQ ttArea.SourceId
                          AND ttChain.AreaNumber EQ ttArea.AreaNumber),"2/1":U)
             WHEN ttDatabase.SourceType EQ 0 /* dbanalys   */
             vItem = vItem + ":":U + STRING(ttArea.ClusterSize GT 1, "2/1":U)
             WHEN ttDatabase.SourceType EQ 1 /* ObjectInfo */
               OR ttDatabase.SourceType EQ 3 /* *.st file  */
      . /* ASSIGN */

      IF ttArea.TableCount NE 0
      OR ttArea.IndexCount NE 0
      OR ttArea.LobCount   NE 0 THEN
   /* RUN AddToList(3, vItem, ttDatabase.SourceName).*//* = the "used" areas */
      ASSIGN vList[3] = vList[3] + ":":U + vItem.

   /* RUN AddToList(4, vItem, ttDatabase.SourceName).*//* = for all areas */
      ASSIGN vList[4] = vList[4] + ":":U + vItem.

    END. /* FOR EACH ttArea */

    DO i = 1 TO EXTENT(ttDatabase.ListID) TRANSACTION:
   /* ASSIGN ttDatabase.ListID[i] = GetListId(i, ttDatabase.SourceName). */

      ASSIGN ttDatabase.ListID[i] = ?.
      IF vList[i] EQ "":U THEN
      NEXT.

      ASSIGN vItem = STRING(MD5-DIGEST(vList[i]))
             vList[i] = "":U
      . /* ASSIGN */
      RUN AddToList(i, vItem, "MD5":U).
      ASSIGN ttDatabase.ListID[i] = GetListId(i, "MD5":U).
    END.

  END. /* FOR EACH ttDatabase NO-LOCK */

/* Part 3: Modify the loaded dbanalys data using auxiliary information: --- */

  FOR EACH ttDatabase EXCLUSIVE-LOCK
     WHERE ttDatabase.SourceType EQ 0: /* = Dbanalys */

DataMatching:
    REPEAT vSourceType = 1 TO 2: /* OI files first, then DF files */

/* Database structures with strong matches (including the matched object IDs)*/
      FOR EACH bfDatabase NO-LOCK
         WHERE bfDatabase.SourceType EQ vSourceType
           AND bfDatabase.ListID[1]  EQ ttDatabase.ListID[1]: /* strong list */

        PROCESS EVENTS.

        FOR EACH ttTable EXCLUSIVE-LOCK
           WHERE ttTable.SourceId EQ ttDatabase.SourceId
             AND ttTable.TableId  EQ ?,

           FIRST bfTable NO-LOCK
           WHERE bfTable.SourceId  EQ bfDatabase.SourceId
             AND bfTable.TableName EQ ttTable.TableName
        TRANSACTION:
          ASSIGN ttTable.TableId = bfTable.TableId.
        END. /* FOR EACH ttTable EXCLUSIVE-LOCK, FIRST bfTable */

        FOR EACH ttLob EXCLUSIVE-LOCK
           WHERE ttLob.SourceId  EQ ttDatabase.SourceId
             AND ttLob.TableName EQ ?,

           FIRST bfLob NO-LOCK
           WHERE bfLob.SourceId EQ bfDatabase.SourceId
             AND bfLob.LobId    EQ ttLob.LobId
        TRANSACTION:
          ASSIGN ttLOB.LobType    = bfLOB.LobType
                 ttLOB.TableOwner = bfLOB.TableOwner
                 ttLOB.TableName  = bfLOB.TableName
                 ttLOB.FieldName  = bfLOB.FieldName
          . /* ASSIGN  */
        END. /* FOR EACH ttLob EXCLUSIVE-LOCK, FIRST bfLob */

        DO TRANSACTION:
          ASSIGN ttDatabase.UseCount = 1
                 bfDatabase.UseCount = bfDatabase.UseCount + 1.
        END.
      END. /* FOR EACH bfDatabase (strong matches) */

/* Database structures with weak matches (only area's and object's names): */
      FOR EACH bfDatabase NO-LOCK
         WHERE bfDatabase.SourceType EQ vSourceType
           AND bfDatabase.SourceType GT 0
           AND bfDatabase.ListID[2]  EQ ttDatabase.ListID[2]: /* weak list */

        FOR EACH ttArea NO-LOCK
           WHERE ttArea.SourceId EQ ttDatabase.SourceId
             AND ttArea.LobCount EQ 1,

           FIRST ttLob EXCLUSIVE-LOCK
           WHERE ttLob.SourceId   EQ ttDatabase.SourceId
             AND ttLob.AreaNumber EQ ttLob.AreaNumber,

           FIRST bfLob NO-LOCK
           WHERE bfLob.SourceId EQ bfDatabase.SourceId
             AND bfLob.AreaNumber EQ ttLob.AreaNumber
        TRANSACTION:
          ASSIGN ttLOB.LobType    = bfLOB.LobType
                 ttLOB.TableOwner = bfLOB.TableOwner
                 ttLOB.TableName  = bfLOB.TableName
                 ttLOB.FieldName  = bfLOB.FieldName
          . /* ASSIGN  */
        END. /* FOR EACH ttLob EXCLUSIVE-LOCK, FIRST bfLob */

        FOR EACH ttIndex EXCLUSIVE-LOCK /* INDEX AreaNumber */
           WHERE ttIndex.SourceId EQ ttDatabase.SourceId,

           FIRST bfIndex NO-LOCK /* INDEX DetailId */
           WHERE bfIndex.SourceId   EQ bfDatabase.SourceId
             AND bfIndex.TableName  EQ ttIndex.TableName
             AND bfIndex.IndexName  EQ ttIndex.IndexName
        TRANSACTION:
          ASSIGN ttIndex.IndexAttr = bfIndex.IndexAttr.
        END. /* FOR EACH ttIndex EXCLUSIVE-LOCK, FIRST bfIndex */

        DO TRANSACTION:
          ASSIGN bfDatabase.UseCount = bfDatabase.UseCount + 1
                                  WHEN ttDatabase.UseCount EQ 0
          . /*Increment UseCount only it was not incremented by strong match*/
        END.

        LEAVE DataMatching.

      END. /* FOR EACH bfDatabase (weak matches) */
    END. /* DataMatching: REPEAT vSourceType = OI to DF */

AreaMatching:
    REPEAT vSourceType = 3 TO 1 BY -2: /* ST files first, then OI files */

      FOR EACH bfDatabase NO-LOCK
         WHERE bfDatabase.SourceType EQ vSourceType
          AND (bfDatabase.ListID[3]  EQ ttDatabase.ListID[3]   /* used areas */
           OR  bfDatabase.ListID[4]  EQ ttDatabase.ListID[4]): /* all areas  */

        FOR EACH ttArea EXCLUSIVE-LOCK
           WHERE ttArea.SourceId EQ ttDatabase.SourceId,

           FIRST bfArea NO-LOCK
           WHERE bfArea.SourceId   EQ bfDatabase.SourceId
             AND bfArea.AreaNumber EQ ttArea.AreaNumber
             AND bfArea.AreaName   EQ ttArea.AreaName
        TRANSACTION:
/* Add the area's attributes to the area name: */
          ASSIGN ttArea.AreaName = bfArea.AreaName
                  + ":":U + STRING(bfArea.AreaNumber)
                  + ",":U + STRING(bfArea.RecPerBlock)
                  + ";":U + STRING(bfArea.ClusterSize)
          . /* ASSIGN */
        END. /* FOR EACH ttArea EXCLUSIVE-LOCK, FIRST bfArea */

        DO TRANSACTION:
          ASSIGN bfDatabase.UseCount = bfDatabase.UseCount + 1
                                  WHEN bfDatabase.SourceType EQ 3
          . /* Increment UseCount only for the ST files */
        END.

        IF bfDatabase.ListID[4] EQ ttDatabase.ListID[4] THEN
        LEAVE AreaMatching.

      END. /* FOR EACH bfDatabase NO-LOCK */
    END. /* AreaMatching: REPEAT vSourceType = OI to ST */
  END. /* FOR EACH ttDatabase EXCLUSIVE-LOCK WHERE SourceType EQ 0 */

/* Part 4: Set relations between objects and the chains : ----------------- */

  FOR EACH ttDatabase NO-LOCK
     WHERE ttDatabase.SourceType EQ 0, /* = Dbanalys */

      EACH ttChain EXCLUSIVE-LOCK /* INDEX ObjectChain (twice) */
     WHERE ttChain.SourceId EQ ttDatabase.SourceId:

    CASE ttChain.ObjectType:

      WHEN "TABLE":U THEN
      FOR FIRST ttTable NO-LOCK
          WHERE ttTable.SourceId EQ ttChain.SourceId
            AND ttTable.TableId  EQ ttChain.ObjectID
      TRANSACTION:
        ASSIGN  ttChain.ObjectOwner  = ttTable.TableOwner
                ttChain.ObjectName   = ttTable.TableName
                ttChain.ObjectBlocks = ttTable.BlockCount
        . /* ASSIGN */
      END. /* FOR FIRST ttTable */

      WHEN "INDEX":U THEN
      FOR FIRST ttIndex NO-LOCK
          WHERE ttIndex.SourceId EQ ttChain.SourceId
            AND ttIndex.IndexId  EQ ttChain.ObjectID
      TRANSACTION:
        ASSIGN  ttChain.ObjectOwner  = ttIndex.TableOwner
                ttChain.ObjectName   = ttIndex.TableName + ".":U +
                                       ttIndex.IndexName
                ttChain.ObjectBlocks = ttIndex.BlockCount
        . /* ASSIGN */
      END. /* FOR FIRST ttIndex */

      WHEN "BLOB":U THEN
      FOR FIRST ttLob NO-LOCK
          WHERE ttLob.SourceId EQ ttChain.SourceId
            AND ttLob.LobId    EQ ttChain.ObjectID
      TRANSACTION:
        ASSIGN  ttChain.ObjectType   = ttLOB.LobType
                ttChain.ObjectOwner  = ttLOB.TableOwner
                ttChain.ObjectName   = ttLOB.TableName + ".":U +
                                       ttLOB.FieldName
                ttChain.ObjectBlocks = ttLOB.BlockCount
        . /* ASSIGN */
      END. /* FOR FIRST ttLob */

      WHEN "Master":U THEN
      FOR FIRST ttArea NO-LOCK
          WHERE ttArea.SourceId   EQ ttChain.SourceId
            AND ttArea.AreaNumber EQ ttChain.AreaNumber
      TRANSACTION:
        ASSIGN ttChain.ObjectOwner  = "PUB":U
               ttChain.ObjectName   = "Master Object"
               ttChain.ObjectBlocks = ttArea.HighBlocks
        . /* ASSIGN */
      END. /* FOR FIRST ttArea  */

    END CASE. /* ttChain.ObjectType */
  END. /* FOR EACH ttDatabase  */

  PROCESS EVENTS.
  ASSIGN vTime2 = ETIME - vTime2.
  STATUS DEFAULT.

END PROCEDURE. /* DataMatching */

/* ------------------------------------------------------------------------- */

PROCEDURE DumpDbanalys.

  DEFINE INPUT PARAMETER ipOutPrefix AS CHARACTER NO-UNDO.
/* ipOutPrefix is the prefix for output files. */

  &SCOPED-DEFINE Sep "~t"

  DEFINE VARIABLE vSuffixList AS CHARACTER NO-UNDO INITIAL
    "Databases.txt,Areas.txt,Tables.txt,Indexes.txt,LOBs.txt,Chains.txt":U.

  DEFINE BUFFER LK_Chain FOR ttChain.
  DEFINE BUFFER RM_Chain FOR ttChain.
  DEFINE BUFFER FR_Chain FOR ttChain.

  DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vOutputDir  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFilePrefix AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vSlash      AS CHARACTER NO-UNDO.

  ASSIGN vSlash = IF OPSYS EQ "Unix":U THEN "/":U ELSE "~\":U.

  IF NOT CAN-FIND(FIRST ttDatabase WHERE ttDatabase.SourceType EQ 0) THEN
  DO:
    MESSAGE "Failed to find dbanalys files!"
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
  END.

  ASSIGN vTime3 = ETIME.
  STATUS DEFAULT SUBSTITUTE("Running &1. Please wait...",
                                  /* &1 */ ENTRY(1, PROGRAM-NAME(1), " ":U)).

/* ipOutPrefix can specify the output directory: */
  ASSIGN ipOutPrefix = ".":U WHEN ipOutPrefix EQ ? OR ipOutPrefix EQ "":U
         vOutputDir  = ipOutPrefix
         vFilePrefix = ?
         FILE-INFO:FILE-NAME = ipOutPrefix
  . /* ASSIGN */
  IF FILE-INFO:FULL-PATHNAME EQ ?
  OR NOT FILE-INFO:FILE-TYPE MATCHES "*D*":U THEN
  DO:
/* ipOutPrefix can specify the output directory plus a file prefix: */
    ASSIGN i = NUM-ENTRIES(ipOutPrefix, vSlash)
           vFilePrefix = ENTRY(i, ipOutPrefix, vSlash)
           i = LENGTH(ipOutPrefix) - LENGTH(vFilePrefix) - 1
           vOutputDir = SUBSTRING(ipOutPrefix, 1, MAX(0, i))
    . /* ASSIGN */
    ASSIGN vOutputDir = ".":U WHEN vOutputDir EQ "":U
           FILE-INFO:FILE-NAME = vOutputDir
    . /* ASSIGN */

    IF FILE-INFO:FULL-PATHNAME EQ ?
    OR NOT FILE-INFO:FILE-TYPE MATCHES "*D*":U THEN
    DO:
       MESSAGE "Output directory does not exist!" SKIP
               vOutputDir
       VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       RETURN ERROR.
    END.

  END.
  ELSE
  ASSIGN i = NUM-ENTRIES(FILE-INFO:FULL-PATHNAME, vSlash)
         vFilePrefix = ENTRY(i, FILE-INFO:FULL-PATHNAME, vSlash) + ".Dbanalys"
  . /* ASSIGN */

  ASSIGN ipOutPrefix = FILE-INFO:FULL-PATHNAME + vSlash + vFilePrefix + ".":U.


/* Change the "location": */
  FOR EACH ttDatabase NO-LOCK
     WHERE ttDatabase.SourceType EQ 0
  BREAK BY ttDatabase.Location:
    IF FIRST-OF(ttDatabase.Location) THEN
    ACCUMULATE "Location":U (COUNT).
  END.

  IF (ACCUM COUNT "Location":U) EQ 1 THEN
  FOR EACH ttDatabase NO-LOCK
     WHERE ttDatabase.SourceType EQ 0:
    ASSIGN ttDatabase.Location = SUBSTRING(ttDatabase.SourceFile,
                                   R-INDEX(ttDatabase.SourceFile,
                                           vSlash) + 1)
    . /* ASSIGN */

/* RSHB filename patterns: */
    IF ttDatabase.Location MATCHES "*-........-dbanalys~~~.txt":U
    OR ttDatabase.Location MATCHES "*~~~.........-dbanalys~~~.txt":U THEN
    ASSIGN ttDatabase.Location = SUBSTRING(ttDatabase.Location,  1,
                                   LENGTH(ttDatabase.Location) - 22)
    . /* ASSIGN */
  END.

/* ------------------------------------------------------------------------ */

  OUTPUT TO VALUE(ipOutPrefix + "Databases.txt").

  PUT UNFORMATTED
           "Time Stamp"   /* Date of dbanalys start   */
    {&Sep} "Location"     /* Location                 */
    {&Sep} "Db Name"      /* Db name                  */
    {&Sep} "ID1"          /* ID of strong object list */
    {&Sep} "ID2"          /* ID of weak object list   */
    {&Sep} "ID3"          /* ID of the used area list */
    {&Sep} "Match"        /* Aux information was used */
    {&Sep} "Db Path"      /* Path to the database     */
    {&Sep} "Exec"         /* Duration of dbanalys     */
    {&Sep} "Blocksize"    /* Db Blocksize             */
    {&Sep} "Areas"        /* Area Count               */
    {&Sep} "Total Blocks" /* Total Number of Blocks   */
    {&Sep} "Tables"       /* Table Count              */
    {&Sep} "RM Blocks"    /* Record Blocks            */
    {&Sep} "RM%"          /* Percentage of RM Blocks  */
    {&Sep} "Indexes"      /* Index Count              */
    {&Sep} "IX Blocks"    /* Index Blocks             */
    {&Sep} "IX%"          /* Percentage of IX Blocks  */
    {&Sep} "LOBs"         /* LOB Count                */
    {&Sep} "Dbanalys"     /* Dbanalys File            */
  SKIP. /* PUT */

  FOR EACH ttDatabase NO-LOCK
     WHERE ttDatabase.SourceType EQ 0
        BY ttDatabase.Location
        BY ttDatabase.SourceName:

    FOR EACH ttArea NO-LOCK
       WHERE ttArea.SourceId EQ ttDatabase.SourceId:
      ACCUMULATE ttArea.HighBlocks   (TOTAL)
                 ttArea.RecordBlocks (TOTAL)
                 ttArea.IndexBlocks  (TOTAL)
                 ttArea.TableCount   (TOTAL)
                 ttArea.IndexCount   (TOTAL)
                 ttArea.LobCount     (TOTAL)
                 "AREA":U            (COUNT)
      . /* ACCUMULATE */
    END.

    PUT UNFORMATTED
       String2ExcelDate(ttDatabase.DbanalysDate) /* Time Stamp    */
      {&Sep} ttDatabase.Location                 /* Location      */
      {&Sep} ttDatabase.SourceName               /* Db Name       */
      {&Sep} ttDatabase.ListID[1]                /* ID1           */
      {&Sep} ttDatabase.ListID[2]                /* ID2           */
      {&Sep} ttDatabase.ListID[3]                /* ID3           */
      {&Sep} ttDatabase.UseCount GT 0            /* Mod           */
      {&Sep} ttDatabase.SourcePath               /* Db Path       */
      {&Sep} ttDatabase.DbanalysExec             /* Exec          */
      {&Sep} ttDatabase.Blocksize                /* Blocksize     */
      {&Sep} ACCUM COUNT "AREA":U                /* Areas         */
      {&Sep} ACCUM TOTAL ttArea.HighBlocks       /* Total Blocks  */
      {&Sep} ACCUM TOTAL ttArea.TableCount       /* Tables        */
      {&Sep} ACCUM TOTAL ttArea.RecordBlocks     /* RM Blocks     */
      {&Sep} INTEGER((ACCUM TOTAL ttArea.RecordBlocks) /
                     (ACCUM TOTAL ttArea.HighBlocks) * 100.0)
      {&Sep} ACCUM TOTAL ttArea.IndexCount       /* Indexes       */
      {&Sep} ACCUM TOTAL ttArea.IndexBlocks      /* IX Blocks     */
      {&Sep} INTEGER((ACCUM TOTAL ttArea.IndexBlocks) /
                     (ACCUM TOTAL ttArea.HighBlocks) * 100.0)
      {&Sep} ACCUM TOTAL ttArea.LobCount         /* LOBs          */
      {&Sep} ttDatabase.SourceFile               /* Dbanalys file */
    SKIP. /* PUT */
  END.

  OUTPUT CLOSE. /* *.Databases.txt */
  PROCESS EVENTS.

/* ------------------------------------------------------------------------- */

  OUTPUT TO VALUE(ipOutPrefix + "Areas.txt").

  PUT UNFORMATTED
           "Time Stamp"    /* Date of dbanalys start                     */
    {&Sep} "Location"      /* Location                                   */
    {&Sep} "Db Name"       /* Db name                                    */
    {&Sep} "Area"          /* Area Name                                  */
    {&Sep} "Area #"        /* Area Number                                */
    {&Sep} "Records"       /* Record Count                               */
    {&Sep} "RPB"           /* Records Per Block                          */
    {&Sep} "Tables"        /* Table Count                                */
    {&Sep} "Indexes"       /* Index Count                                */
    {&Sep} "LOBs"          /* LOB Count                                  */
    {&Sep} "HWM"           /* Current high water mark                    */
    {&Sep} "RM Blocks"     /* record block(s) found in the area          */
    {&Sep} "Index Blocks"  /* index block(s) found in the area           */
    {&Sep} "LOB Blocks"    /* free block(s) found in the area            */
    {&Sep} "Total Blocks"  /* Total number of blocks found in the area   */
    {&Sep} "Free Blocks"   /* free block(s) found in the area            */
    {&Sep} "Empty Blocks"  /* empty block(s) found in the area           */
    {&Sep} "Free Clusters" /* cluster(s) found in the free cluster chain */
    {&Sep} "Object Blocks" /* object block(s) found in the area          */
    {&Sep} "Cluster List"  /* cluster list block(s) found in the area    */
    {&Sep} "Cluster Map"   /* cluster map block(s) found in the area     */
    {&Sep} "Object List"   /* object list block(s) found in the area     */
  SKIP. /* PUT */

  FOR EACH ttDatabase NO-LOCK
     WHERE ttDatabase.SourceType EQ 0,

      EACH ttArea NO-LOCK
     WHERE ttArea.SourceId EQ ttDatabase.SourceId

        BY ttArea.HighBlocks DESCENDING
        BY ttArea.AreaNumber:

    PUT UNFORMATTED
       String2ExcelDate(ttDatabase.DbanalysDate) /* Time Stamp  */
      {&Sep} ttDatabase.Location     /* Location                */
      {&Sep} ttDatabase.SourceName   /* Db Name                 */
      {&Sep} ttArea.AreaName         /* Area                    */
      {&Sep} ttArea.AreaNumber       /* Area #                  */
      {&Sep} ttArea.RecordCount      /* Records                 */
      {&Sep} IF ttArea.RecordCount EQ 0 THEN 0 ELSE
        INTEGER(ttArea.RecordCount / ttArea.RecordBlocks) /* RPB*/
      {&Sep} ttArea.TableCount       /* Tables                  */
      {&Sep} ttArea.IndexCount       /* Indexes                 */
      {&Sep} ttArea.LobCount         /* LOBs                    */
      {&Sep} ttArea.HighBlocks       /* Current high water mark */
      {&Sep} ttArea.RecordBlocks     /* Record Blocks           */
      {&Sep} ttArea.IndexBlocks      /* Index Blocks            */
      {&Sep} ttArea.LobBlocks        /* LOB Blocks              */
      {&Sep} ttArea.TotalBlocks      /* Total number of blocks  */
      {&Sep} ttArea.FreeBlocks       /* Free Blocks             */
      {&Sep} ttArea.EmptyBlocks      /* Empty Blocks            */
      {&Sep} ttArea.FreeClusters     /* Free Cluster Chain      */
      {&Sep} ttArea.ObjectBlocks     /* Object Blocks           */
      {&Sep} ttArea.ClusterList      /* Cluster List Blocks     */
      {&Sep} ttArea.ClusterMap       /* Cluster Map Blocks      */
      {&Sep} ttArea.ObjectList       /* Object List Blocks      */
    SKIP. /* PUT */
  END. /* FOR EACH ttArea, FIRST ttDatabase */

  OUTPUT CLOSE. /* *.Areas.txt */
  PROCESS EVENTS.

/* ------------------------------------------------------------------------- */

  OUTPUT TO VALUE(ipOutPrefix + "Chains.txt").

  PUT UNFORMATTED
           "Time Stamp"    /* Date of dbanalys start                    */
    {&Sep} "Location"      /* Location                                  */
    {&Sep} "Db Name"       /* Db Name                                   */
    {&Sep} "Area"          /* Area Name                                 */
    {&Sep} "Area #"        /* Area Number                               */
    {&Sep} "Object Type"   /* "Master", "Table" or "Index"              */
    {&Sep} "Owner"         /* Object Owner                              */
    {&Sep} "Object"        /* Object Name                               */
    {&Sep} "Object #"      /* Object ID                                 */
    {&Sep} "Chain Type"    /* FREECHN, RMCHN, LOCKCHN / free, RM, Index */
    {&Sep} "Chain Blocks"  /* block(s) found in the chain               */
    {&Sep} "Object Blocks" /* Object Blocks                             */
  SKIP. /* PUT */

  FOR EACH ttDatabase NO-LOCK
     WHERE ttDatabase.SourceType EQ 0,

      EACH ttChain NO-LOCK
     WHERE ttChain.SourceId EQ ttDatabase.SourceId,

     FIRST ttArea NO-LOCK
     WHERE ttArea.SourceId   EQ ttChain.SourceId
       AND ttArea.AreaNumber EQ ttChain.AreaNumber

        BY ttChain.ChainBlocks DESCENDING
        BY ttChain.ObjectID:

    PUT UNFORMATTED
       String2ExcelDate(ttDatabase.DbanalysDate) /* Time Stamp    */
      {&Sep} ttDatabase.Location                 /* Location      */
      {&Sep} ttDatabase.SourceName               /* Db name       */
      {&Sep} ttArea.AreaName                     /* Area          */
      {&Sep} ttArea.AreaNumber                   /* Area #        */
      {&Sep} ttChain.ObjectType                  /* Object Type   */
      {&Sep} ttChain.ObjectOwner                 /* Owner         */
      {&Sep} ttChain.ObjectName                  /* Object        */
      {&Sep} ttChain.ObjectID                    /* Object #      */
      {&Sep} ttChain.ChainType                   /* Chain Type    */
      {&Sep} ttChain.ChainBlocks                 /* Chain Blocks  */
      {&Sep} ttChain.ObjectBlocks                /* Object Blocks */
    SKIP. /* PUT */
  END. /* FOR EACH ttChain, FIRST ttDatabase, FIRST ttArea */

  OUTPUT CLOSE. /* *.Chains.txt */
  PROCESS EVENTS.

/* ------------------------------------------------------------------------- */

  OUTPUT TO VALUE(ipOutPrefix + "Tables.txt").

  PUT UNFORMATTED
           "Time Stamp"  /* Date of dbanalys start  */
    {&Sep} "Location"    /* Location                */
    {&Sep} "Db Name"     /* Db Name                 */
    {&Sep} "Area"        /* Area Name               */
    {&Sep} "Area #"      /* Area Number             */
    {&Sep} "Owner"       /* Table Owner             */
    {&Sep} "Table Name"  /* Table Name              */
    {&Sep} "Table #"     /* Table Id                */
    {&Sep} "Records"     /* Record Count            */
    {&Sep} "Fragments"   /* Fragment Count          */
    {&Sep} "Frag%"       /* Percentage              */
    {&Sep} "Blocks"      /* Block Count             */
    {&Sep} "RM Chain"    /* Blocks on RM Chain      */
    {&Sep} "Free Chain"  /* Blocks on Free Chain    */
    {&Sep} "RPB"         /* Records Per Block       */
    {&Sep} "Table Size"  /* Table Size              */
    {&Sep} "Size%"       /* Table Size to Area Size */
    {&Sep} "Indexes"     /* Index Count             */
    {&Sep} "Index Size"  /* Index Size              */
    {&Sep} "Index/Table" /* Index Size / Table Size */
    {&Sep} "Min"         /* Min Record Size         */
    {&Sep} "Max"         /* Max Record Size         */
    {&Sep} "Mean"        /* Mean Record Size        */
    {&Sep} "Scatter"     /* Scatter Factor          */
  SKIP. /* PUT */

  FOR EACH ttDatabase NO-LOCK
     WHERE ttDatabase.SourceType EQ 0,

     EACH ttTable NO-LOCK
     WHERE ttTable.SourceId EQ ttDatabase.SourceId,

     FIRST ttArea NO-LOCK
     WHERE ttArea.SourceId   EQ ttTable.SourceId
       AND ttArea.AreaNumber EQ ttTable.AreaNumber

        BY ttTable.TableSize DESCENDING
        BY ttTable.TableName:

    FOR EACH ttIndex NO-LOCK
       WHERE ttIndex.SourceId   EQ ttTable.SourceId
         AND ttIndex.TableOwner EQ ttTable.TableOwner
         AND ttIndex.TableName  EQ ttTable.TableName:
      ACCUMULATE ttIndex.IndexSize (TOTAL COUNT).
    END. /* FOR EACH ttIndex */

    FIND FIRST RM_Chain NO-LOCK
         WHERE RM_Chain.SourceId   EQ ttTable.SourceId
           AND RM_Chain.AreaNumber EQ ttTable.AreaNumber
           AND RM_Chain.ObjectType EQ "TABLE":U
           AND RM_Chain.ObjectID   EQ ttTable.TableId
           AND RM_Chain.ChainType  EQ "RM":U
    NO-ERROR.

    FIND FIRST FR_Chain NO-LOCK
         WHERE FR_Chain.SourceId   EQ ttTable.SourceId
           AND FR_Chain.AreaNumber EQ ttTable.AreaNumber
           AND FR_Chain.ObjectType EQ "TABLE":U
           AND FR_Chain.ObjectID   EQ ttTable.TableId
           AND FR_Chain.ChainType  EQ "FREE":U
    NO-ERROR.

    PUT UNFORMATTED
       String2ExcelDate(ttDatabase.DbanalysDate)    /* Time Stamp     */
      {&Sep} ttDatabase.Location                    /* Location       */
      {&Sep} ttDatabase.SourceName                  /* Db Name        */
      {&Sep} ttArea.AreaName                        /* Area           */
      {&Sep} ttArea.AreaNumber                      /* Area #         */
      {&Sep} ttTable.TableOwner                     /* Owner          */
      {&Sep} ttTable.TableName                      /* Table          */
      {&Sep} ttTable.TableId                        /* Table #        */
      {&Sep} ttTable.RecordCount                    /* Records        */
      {&Sep} ttTable.FragmentCnt - ttTable.RecordCount
      {&Sep} IF ttTable.RecordCount EQ 0 THEN 0.0 ELSE
             ROUND((ttTable.FragmentCnt - ttTable.RecordCount)
                  / ttTable.RecordCount * 100.0, 2)
      {&Sep} ttTable.BlockCount                     /* Blocks         */
      {&Sep} IF AVAILABLE RM_Chain THEN RM_Chain.ChainBlocks ELSE ?
      {&Sep} IF AVAILABLE FR_Chain THEN FR_Chain.ChainBlocks ELSE ?
      {&Sep} IF ttTable.BlockCount EQ 0 THEN 0 ELSE /* RPB            */
             INTEGER(ttTable.RecordCount / ttTable.BlockCount)
      {&Sep} ttTable.TableSize                      /* Table Size     */
      {&Sep} ROUND(ttTable.BlockCount / ttArea.HighBlocks * 100.0, 2)
      {&Sep} ACCUM COUNT ttIndex.IndexSize          /* Indexes        */
      {&Sep} ACCUM TOTAL ttIndex.IndexSize          /* Index Size     */
      {&Sep} IF ttTable.TableSize EQ 0 THEN 0.0 ELSE
             ROUND((ACCUM TOTAL ttIndex.IndexSize)  /* Index/Table    */
                              / ttTable.TableSize, 2)
      {&Sep} ttTable.MinRec                         /* Min            */
      {&Sep} ttTable.MaxRec                         /* Max            */
      {&Sep} ttTable.MeanRec                        /* Mean           */
      {&Sep} ttTable.Scatter                        /* Scatter Factor */
    SKIP. /* PUT */
  END. /* FOR EACH ttDatabase, EACH ttArea, EACH ttTable */

  OUTPUT CLOSE. /* *.Tables.txt */
  PROCESS EVENTS.

/* ------------------------------------------------------------------------- */

  OUTPUT TO VALUE(ipOutPrefix + "Indexes.txt").

  PUT UNFORMATTED
           "Time Stamp"  /* Dbanalys Date/Time              */
    {&Sep} "Location"    /* Location                        */
    {&Sep} "Db Name"     /* Db Name                         */
    {&Sep} "Area"        /* Area Name                       */
    {&Sep} "Area #"      /* Area Number                     */
    {&Sep} "Owner"       /* Table Owner                     */
    {&Sep} "Table"       /* Table Name                      */
    {&Sep} "Table #"     /* Table Name                      */
    {&Sep} "Records"     /* Record Count                    */
    {&Sep} "Index Name"  /* Index Name                      */
    {&Sep} "Index #"     /* Index Id                        */
    {&Sep} "State"       /* {U]nique, [[W]ord or [I]nactive */
    {&Sep} "Fields"      /* Index Fields                    */
    {&Sep} "Levels"      /* Index Levels                    */
    {&Sep} "Blocks"      /* Index Blocks                    */
    {&Sep} "Blocks %"    /* Index Blocks to Area Blocks     */
    {&Sep} "Lock Chain"  /* Blocks on Index-Delete Chain    */
    {&Sep} "Free Chain"  /* Blocks on Free Chain            */
    {&Sep} "Keys/Block"  /* Index Keys per Block            */
    {&Sep} "Key Size"    /* Mean Size of Index Keys         */
    {&Sep} "Size"        /* Index Size                      */
    {&Sep} "Util"        /* % Util of Index Blocks          */
  SKIP. /* PUT */

  FOR EACH ttDatabase NO-LOCK
     WHERE ttDatabase.SourceType EQ 0,

      EACH ttIndex NO-LOCK
     WHERE ttIndex.SourceId EQ ttDatabase.SourceId,

     FIRST ttArea NO-LOCK
     WHERE ttArea.SourceId   EQ ttIndex.SourceId
       AND ttArea.AreaNumber EQ ttIndex.AreaNumber,

     FIRST ttTable NO-LOCK
     WHERE ttTable.SourceId   EQ ttIndex.SourceId
       AND ttTable.TableOwner EQ ttIndex.TableOwner
       AND ttTable.TableName  EQ ttIndex.TableName

        BY ttIndex.BlockCount DESCENDING
        BY ttIndex.TableName
        BY ttIndex.IndexName:

    FIND FIRST LK_Chain NO-LOCK
         WHERE LK_Chain.SourceId   EQ ttIndex.SourceId
           AND LK_Chain.AreaNumber EQ ttIndex.AreaNumber
           AND LK_Chain.ObjectType EQ "INDEX":U
           AND LK_Chain.ObjectID   EQ ttIndex.IndexId
           AND LK_Chain.ChainType  EQ "Index-Delete":U
    NO-ERROR.

    FIND FIRST FR_Chain NO-LOCK
         WHERE FR_Chain.SourceId   EQ ttIndex.SourceId
           AND FR_Chain.AreaNumber EQ ttIndex.AreaNumber
           AND FR_Chain.ObjectType EQ "INDEX":U
           AND FR_Chain.ObjectID   EQ ttIndex.IndexId
           AND FR_Chain.ChainType  EQ "FREE":U
    NO-ERROR.

    PUT UNFORMATTED
       String2ExcelDate(ttDatabase.DbanalysDate) /* Time Stamp */
      {&Sep} ttDatabase.Location                 /* Location   */
      {&Sep} ttDatabase.SourceName               /* Db Name    */
      {&Sep} ttArea.AreaName                     /* Area       */
      {&Sep} ttArea.AreaNumber                   /* Area #     */
      {&Sep} ttIndex.TableOwner                  /* Owner      */
      {&Sep} ttIndex.TableName                   /* Table      */
      {&Sep} ttTable.TableId                     /* Table #    */
      {&Sep} ttTable.RecordCount                 /* Records    */
      {&Sep} ttIndex.IndexName                   /* Index      */
      {&Sep} ttIndex.IndexId                     /* Index #    */
      {&Sep} IF NOT ttIndex.ActiveIndex THEN "I" ELSE
             ttIndex.IndexAttr                   /* State      */
      {&Sep} ttIndex.FieldCount                  /* Fields     */
      {&Sep} ttIndex.LevelCount                  /* Levels     */
      {&Sep} ttIndex.BlockCount                  /* Blocks     */
      {&Sep} ROUND(ttIndex.BlockCount / ttArea.HighBlocks * 100.0, 2)
      {&Sep} IF AVAILABLE LK_Chain THEN LK_Chain.ChainBlocks ELSE ?
      {&Sep} IF AVAILABLE FR_Chain THEN FR_Chain.ChainBlocks ELSE ?
      {&Sep} IF NOT ttIndex.ActiveIndex  THEN ? ELSE /* Keys/Block */
             IF ttTable.RecordCount EQ 0 
             OR ttIndex.IndexSize   LE 3 THEN 0 ELSE
             IF ttIndex.BlockCount  GT 2 THEN
             ROUND(ttTable.RecordCount / ttIndex.BlockCount, 0)
             ELSE
             ROUND(ttTable.RecordCount / ttIndex.IndexSize
                                       * ttDatabase.Blocksize, 0)
      {&Sep} IF NOT ttIndex.ActiveIndex  THEN ? ELSE /* Key Size */
             IF ttTable.RecordCount EQ 0 
             OR ttIndex.IndexSize   LE 3 THEN 0 ELSE
             ROUND(ttIndex.IndexSize / ttTable.RecordCount, 2)
      {&Sep} ttIndex.IndexSize                   /* Size       */
      {&Sep} ROUND(ttIndex.IndexSize             /* Util       */
                 / ttIndex.BlockCount
                 / ttDatabase.Blocksize
                 * 100.0, 1)
    SKIP. /* PUT */
  END. /* FOR EACH ttDatabase, EACH ttArea, EACH ttIndex */

  OUTPUT CLOSE. /* *.Indexes.txt */
  PROCESS EVENTS.

/* ------------------------------------------------------------------------ */

  OUTPUT TO VALUE(ipOutPrefix + "LOBs.txt").

  PUT UNFORMATTED
           "Time Stamp"  /* Dbanalys Date/Time        */
    {&Sep} "Location"    /* Location                  */
    {&Sep} "Db Name"     /* Db Name                   */
    {&Sep} "Area"        /* Area Name                 */
    {&Sep} "Area #"      /* Area Number               */
    {&Sep} "Owner"       /* Table Owner               */
    {&Sep} "Table"       /* Table Name                */
    {&Sep} "Table #"     /* Table Id                  */
    {&Sep} "Field"       /* Field Name                */
    {&Sep} "LOB #"       /* LOB Id                    */
    {&Sep} "Type"        /* LOB Type                  */
    {&Sep} "Records"     /* Record Count              */
    {&Sep} "Blocks"      /* LOB Blocks                */
    {&Sep} "Blocks %"    /* LOB Blocks to Area Blocks */
    {&Sep} "RM Chain"    /* Blocks on RM Chain        */
    {&Sep} "RM %"        /* RM Blocks to LOB Blocks   */
    {&Sep} "Free Chain"  /* Blocks on Free Chain      */
    {&Sep} "Mean Size"   /* Field Size per Field      */
  SKIP. /* PUT */

  FOR EACH ttDatabase NO-LOCK
     WHERE ttDatabase.SourceType EQ 0,

      EACH ttLob NO-LOCK
     WHERE ttLob.SourceId EQ ttDatabase.SourceId,

     FIRST ttArea NO-LOCK
     WHERE ttArea.SourceId   EQ ttLob.SourceId
       AND ttArea.AreaNumber EQ ttLob.AreaNumber,

     FIRST RM_Chain NO-LOCK
     WHERE RM_Chain.SourceId   EQ ttLob.SourceId
      AND  RM_Chain.ObjectId   EQ ttLob.LobId
      AND  RM_Chain.ChainType  EQ "RM":U,

     FIRST FR_Chain NO-LOCK
     WHERE FR_Chain.SourceId   EQ ttLob.SourceId
       AND RM_Chain.ObjectId   EQ ttLob.LobId
       AND FR_Chain.ChainType  EQ "FREE":U

        BY ttDatabase.SourceId
        BY ttArea.AreaNumber
        BY RM_Chain.ObjectID:

    FIND FIRST ttTable NO-LOCK
         WHERE ttTable.SourceId   EQ ttLob.SourceId
           AND ttTable.TableOwner EQ ttLob.TableOwner
           AND ttTable.TableName  EQ ttLob.TableName
    NO-ERROR.

    PUT UNFORMATTED
       String2ExcelDate(ttDatabase.DbanalysDate)       /* Time Stamp */
      {&Sep} ttDatabase.Location                       /* Location   */
      {&Sep} ttDatabase.SourceName                     /* Db Name    */
      {&Sep} ttArea.AreaName                           /* Area       */
      {&Sep} ttArea.AreaNumber                         /* Area #     */
      {&Sep} ttLob.TableOwner                          /* Owner      */
      {&Sep} ttLob.TableName                           /* Table      */
      {&Sep} IF AVAILABLE ttTable THEN ttTable.TableId /* Table #    */
                                  ELSE ?
      {&Sep} ttLob.FieldName                           /* Field      */
      {&Sep} ttLob.LobId                               /* LOB #      */
      {&Sep} ttLob.LobType                             /* Type       */
      {&Sep} IF AVAILABLE ttTable THEN ttTable.RecordCount
                                  ELSE ?               /* Records    */
      {&Sep} ttLob.BlockCount                          /* Blocks     */
      {&Sep} ROUND(ttLob.BlockCount / ttArea.HighBlock * 100.0, 0)
      {&Sep} RM_Chain.ChainBlocks                      /* RM Chain   */
      {&Sep} IF RM_Chain.ChainBlocks EQ 0 THEN 0 ELSE  /* RM %       */
             INTEGER(RM_Chain.ChainBlocks / ttLob.BlockCount * 100.0)
      {&Sep} FR_Chain.ChainBlocks                      /* Free Chain */
      {&Sep} IF NOT AVAILABLE ttTable    THEN ? ELSE
             IF ttTable.RecordCount EQ 0 THEN 0 ELSE   /* Mean Size  */
             INTEGER(ttLob.BlockCount
                   * ttDatabase.BlockSize
                   / ttTable.RecordCount)
    SKIP. /* PUT */
  END. /* FOR EACH ttDatabase, EACH ttArea, EACH RM_Chain */

  OUTPUT CLOSE. /* *.LOBs.txt */
  PROCESS EVENTS.

  IF NOT CAN-FIND(FIRST ttLob) THEN
  OS-DELETE VALUE(ipOutPrefix + "LOBs.txt").

/* ------------------------------------------------------------------------ */

  OUTPUT TO VALUE(ipOutPrefix + "Sources.txt").

  PUT UNFORMATTED
           "Location" /* Location                                */
    {&Sep} "Db Name"  /* Db name                                 */
    {&Sep} "Type"     /* Source Type                             */
    {&Sep} "Tables"   /* Table Count                             */
    {&Sep} "Indexes"  /* Index Count                             */
    {&Sep} "LOBs"     /* LOB Count                               */
    {&Sep} "Areas"    /* Area Count                              */
    {&Sep} "ID1"      /* ID of db object list with object IDs    */
    {&Sep} "ID2"      /* ID of db object list without object IDs */
    {&Sep} "ID3"      /* ID of the used area list                */
    {&Sep} "ID4"      /* ID of the all area list                 */
    {&Sep} "Used"     /* How many times the source was used      */
    {&Sep} "Source"   /* Source File                             */
  SKIP. /* PUT */

  FOR EACH ttDatabase NO-LOCK
        BY ttDatabase.SourceName
        BY ttDatabase.SourceType
        BY ttDatabase.Location:

    FOR EACH ttArea NO-LOCK
       WHERE ttArea.SourceId EQ ttDatabase.SourceId:
      ACCUMULATE ttArea.TotalBlocks  (TOTAL)
                 ttArea.RecordBlocks (TOTAL)
                 ttArea.IndexBlocks  (TOTAL)
                 ttArea.TableCount   (TOTAL)
                 ttArea.IndexCount   (TOTAL)
                 ttArea.LobCount     (TOTAL)
                 "AREA":U            (COUNT)
      . /* ACCUMULATE */
    END.

    PUT UNFORMATTED
             ttDatabase.Location           /* Location */
      {&Sep} ttDatabase.SourceName         /* Db Name  */
      {&Sep} IF ttDatabase.SourceType EQ 0 THEN "DA" ELSE
             IF ttDatabase.SourceType EQ 1 THEN "OI" ELSE
             IF ttDatabase.SourceType EQ 2 THEN "DF" ELSE
             IF ttDatabase.SourceType EQ 3 THEN "ST" ELSE
        STRING (ttDatabase.SourceType)     /* Type     */
      {&Sep} ACCUM TOTAL ttArea.TableCount /* Tables   */
      {&Sep} ACCUM TOTAL ttArea.IndexCount /* Indexes  */
      {&Sep} ACCUM TOTAL ttArea.LobCount   /* LOBs     */
      {&Sep} ACCUM COUNT "AREA":U          /* Areas    */
      {&Sep} ttDatabase.ListID[1]          /* ID1      */
      {&Sep} ttDatabase.ListID[2]          /* ID2      */
      {&Sep} ttDatabase.ListID[3]          /* ID3      */
      {&Sep} ttDatabase.ListID[4]          /* ID4      */
      {&Sep} ttDatabase.UseCount           /* Used     */
      {&Sep} ttDatabase.SourceFile         /* Source   */
    SKIP. /* PUT */
  END. /* FOR EACH ttDatabase */

  OUTPUT CLOSE. /* *.Sources.txt */
  PROCESS EVENTS.

/* ------------------------------------------------------------------------ */

  FOR EACH ttDatabase NO-LOCK
     WHERE ttDatabase.SourceType EQ 0
  BREAK BY ttDatabase.ListID[1]:
    ACCUMULATE "Dbanalys":U (COUNT).
    IF FIRST-OF(ttDatabase.ListID[1]) THEN
    ACCUMULATE "Needed":U (COUNT).
  END.

  FOR EACH ttDatabase NO-LOCK
     WHERE ttDatabase.SourceType GT 0:
    ACCUMULATE "AuxInfo":U (COUNT).
    IF ttDatabase.UseCount GT 0 THEN
    ACCUMULATE "UsedInfo":U (COUNT).
  END.

  HIDE ALL NO-PAUSE.
  ASSIGN vTime3 = ETIME - vTime3.
  STATUS DEFAULT.

  MESSAGE
    ACCUM COUNT "Dbanalys":U "dbanalys files were processed." SKIP
    ACCUM COUNT "AuxInfo":U  "auxiliary files were loaded."   SKIP
    ACCUM COUNT "Needed":U   "auxiliary files are needed."    SKIP
    ACCUM COUNT "UsedInfo":U "auxiliary files were used."     SKIP
    "See the result in" ipOutPrefix + "*.txt":U               SKIP
    vSuffixList                                               SKIP
    " Load Time:" vTime1 / 1000.0                             SKIP
    "Match Time:" vTime2 / 1000.0                             SKIP
    " Dump Time:" vTime3 / 1000.0
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE. /* DumpDbanalys */


/* ------------------------------------------------------------------------- */

PROCEDURE LoadObjectInfo:

  DEFINE INPUT PARAMETER ipInputFile AS CHARACTER NO-UNDO.

  &SCOPED-DEFINE SourceType 1

  DEFINE VARIABLE iHost         AS INTEGER   NO-UNDO. /* Host        */
  DEFINE VARIABLE iDbName       AS INTEGER   NO-UNDO. /* Db Name     */
  DEFINE VARIABLE iAreaName     AS INTEGER   NO-UNDO. /* Area        */
  DEFINE VARIABLE iAreaNumber   AS INTEGER   NO-UNDO. /* Area #      */
  DEFINE VARIABLE vAreaNumber   AS INTEGER   NO-UNDO. /* Area #      */
  DEFINE VARIABLE iRecPerBlock  AS INTEGER   NO-UNDO. /* RPB         */
  DEFINE VARIABLE vRecPerBlock  AS INTEGER   NO-UNDO. /* RPB         */
  DEFINE VARIABLE iClusterSize  AS INTEGER   NO-UNDO. /* CLS         */
  DEFINE VARIABLE vClusterSize  AS INTEGER   NO-UNDO. /* CLS         */
  DEFINE VARIABLE iObjectType   AS INTEGER   NO-UNDO. /* Object Type */
  DEFINE VARIABLE iOwnerName    AS INTEGER   NO-UNDO. /* Owner       */
  DEFINE VARIABLE iTableName    AS INTEGER   NO-UNDO. /* Table       */
  DEFINE VARIABLE iObjectName   AS INTEGER   NO-UNDO. /* Object      */
  DEFINE VARIABLE iObjectNumber AS INTEGER   NO-UNDO. /* Object #    */
  DEFINE VARIABLE vObjectNumber AS INTEGER   NO-UNDO. /* Object #    */
  DEFINE VARIABLE iObjectInfo   AS INTEGER   NO-UNDO. /* Object Info */
  DEFINE VARIABLE vItem         AS CHARACTER NO-UNDO EXTENT 18.
  DEFINE VARIABLE i             AS INTEGER   NO-UNDO.

  INPUT FROM VALUE(ipInputFile).

/* Parse the header of ObjectInfo file: */
  IMPORT DELIMITER "~t" vItem.

  DO i = EXTENT(vItem) TO 1 BY -1:
    CASE vItem[i]:
      WHEN "Host":U        THEN ASSIGN iHost         = i.
      WHEN "Db Name":U     THEN ASSIGN iDbName       = i.
      WHEN "Area":U        THEN ASSIGN iAreaName     = i.
      WHEN "Area #":U      THEN ASSIGN iAreaNumber   = i.
      WHEN "RPB":U         THEN ASSIGN iRecPerBlock  = i.
      WHEN "CLS":U         THEN ASSIGN iClusterSize  = i.
      WHEN "Object Type":U THEN ASSIGN iObjectType   = i.
      WHEN "Owner":U       THEN ASSIGN iOwnerName    = i.
      WHEN "Table":U       THEN ASSIGN iTableName    = i.
      WHEN "Object":U      THEN ASSIGN iObjectName   = i.
      WHEN "Object #":U    THEN ASSIGN iObjectNumber = i.
      WHEN "Object Info":U THEN ASSIGN iObjectInfo   = i.
    END CASE.
  END.

  IF iHost         EQ 0
  OR iDbName       EQ 0
  OR iAreaName     EQ 0
  OR iAreaNumber   EQ 0
  OR iObjectType   EQ 0
  OR iOwnerName    EQ 0
  OR iTableName    EQ 0
  OR iObjectName   EQ 0
  OR iObjectNumber EQ 0
  OR iObjectInfo   EQ 0 THEN
  RETURN.

ImportLine:
  REPEAT ON ENDKEY UNDO, LEAVE:

    ASSIGN vItem         = "":U
           vRecPerBlock  = ?
           vClusterSize  = ?
    . /* ASSIGN */

/* Format of the files generated by ObjectInfo.p: */
    IMPORT DELIMITER "~t" vItem.

    ASSIGN vAreaNumber   = INTEGER(vItem[iAreaNumber])
           vObjectNumber = INTEGER(vItem[iObjectNumber])
           vRecPerBlock  = INTEGER(vItem[iRecPerBlock]) WHEN iRecPerBlock GT 0
           vClusterSize  = INTEGER(vItem[iClusterSize]) WHEN iClusterSize GT 0
    NO-ERROR. /* ASSIGN */

    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    NEXT ImportLine.

/* ttDatabase for vDbName: */

    FIND FIRST ttDatabase NO-LOCK /* INDEX SourceName */
         WHERE ttDatabase.SourceType EQ {&SourceType}
           AND ttDatabase.SourceName EQ vItem[iDbName]
           AND ttDatabase.SourceFile EQ ipInputFile
    NO-ERROR.

    IF NOT AVAILABLE ttDatabase THEN
    DO TRANSACTION:
      ASSIGN i = 0.
      FOR EACH ttDatabase NO-LOCK /* INDEX SourceId */
            BY ttDatabase.SourceId DESCENDING:
        ASSIGN i = ttDatabase.SourceId + 1.
        LEAVE.
      END.

      CREATE ttDatabase.
      ASSIGN ttDatabase.SourceType = {&SourceType}
             ttDatabase.SourceId   = i
             ttDatabase.SourceFile = ipInputFile
             ttDatabase.SourceName = vItem[iDbName]
             ttDatabase.SourcePath = ?
             ttDatabase.Location   = vItem[iHost]
             ttDatabase.UseCount   = 0
             ttDatabase.ListId     = ?
      . /* ASSIGN */
    END. /* IF NOT AVAILABLE ttDatabase */

    FIND FIRST ttArea NO-LOCK
         WHERE ttArea.SourceId   EQ ttDatabase.SourceId
           AND ttArea.AreaNumber EQ vAreaNumber
    NO-ERROR.

    IF NOT AVAILABLE ttArea THEN
    DO TRANSACTION:
      CREATE ttArea.
      ASSIGN ttArea.SourceId    = ttDatabase.SourceId
             ttArea.AreaNumber  = vAreaNumber
             ttArea.AreaName    = vItem[iAreaName]
             ttArea.RecPerBlock = vRecPerBlock
             ttArea.ClusterSize = vClusterSize
             ttArea.TableCount  = 0
             ttArea.IndexCount  = 0
             ttArea.LobCount    = 0
        . /* ASSIGN */
      END. /* IF NOT AVAILABLE ttArea */

    CASE vItem[iObjectType]:
      WHEN "TABLE":U THEN
      DO TRANSACTION:
        CREATE ttTable.
        ASSIGN ttTable.SourceId   = ttArea.SourceId
               ttTable.AreaNumber = ttArea.AreaNumber
               ttTable.AreaName   = ttArea.AreaName
               ttTable.TableOwner = vItem[iOwnerName]
               ttTable.TableName  = vItem[iTableName]
               ttTable.TableId    = vObjectNumber
               ttArea.TableCount  = ttArea.TableCount + 1
        . /* ASSIGN */
      END. /* "TABLE" */

      WHEN "INDEX":U THEN
      DO TRANSACTION:

        CREATE ttIndex.
        ASSIGN ttIndex.SourceId   = ttArea.SourceId
               ttIndex.AreaNumber = ttArea.AreaNumber
               ttIndex.AreaName   = ttArea.AreaName
               ttIndex.TableOwner = vItem[iOwnerName]
               ttIndex.TableName  = vItem[iTableName]
               ttIndex.IndexName  = vItem[iObjectName]
               ttIndex.IndexId    = vObjectNumber
               ttIndex.IndexAttr  = vItem[iObjectInfo]
               ttArea.IndexCount  = ttArea.IndexCount + 1
        . /* ASSIGN */
      END. /* "INDEX" */

      WHEN "BLOB":U OR
      WHEN "CLOB":U THEN
      DO TRANSACTION:
        CREATE ttLob.
        ASSIGN ttLOB.SourceId   = ttArea.SourceId
               ttLOB.AreaNumber = ttArea.AreaNumber
               ttLOB.AreaName   = ttArea.AreaName
               ttLOB.TableOwner = vItem[iOwnerName]
               ttLOB.TableName  = vItem[iTableName]
               ttLOB.FieldName  = vItem[iObjectName]
               ttLOB.LobId      = vObjectNumber
               ttLOB.LobType    = vItem[iObjectType]
               ttArea.LobCount  = ttArea.LobCount + 1
        . /* ASSIGN */
      END. /* "BLOB" or "CLOB" */
    END CASE.
  END. /* ImportLine: REPEAT */

  INPUT CLOSE.

/* If nothing was imported: */
  IF NOT CAN-FIND(FIRST ttArea NO-LOCK
                  WHERE ttArea.SourceId EQ ttDatabase.SourceId) THEN
  DO TRANSACTION:
    DELETE ttDatabase.
  END.

END PROCEDURE. /* LoadObjectInfo */


/* ------------------------------------------------------------------------- */

PROCEDURE LoadDfFiles:

  DEFINE INPUT PARAMETER ipInputFile AS CHARACTER NO-UNDO.

  &SCOPED-DEFINE SourceType 2

  DEFINE VARIABLE vDirName      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDbName       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vSlash        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vItem         AS CHARACTER NO-UNDO EXTENT 7.
  DEFINE VARIABLE vAreaName     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vCurrType     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vPrevType     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vSourceId     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTableId      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vIndexId      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLobId        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vFoundPrimary AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE i             AS INTEGER   NO-UNDO.

/* DbName: ipInputFile = "path/to/<DbName>.df */
  ASSIGN vSlash   = IF OPSYS EQ "Unix":U THEN "/":U ELSE "~\":U
         i        = NUM-ENTRIES(ipInputFile, vSlash)
         vDirName = ENTRY(i - 1, ipInputFile, vSlash)
         vDbName  = ENTRY(i, ipInputFile, vSlash)
         vDbName  = ENTRY(1, vDbName, ".":U)
         vFoundPrimary = FALSE
         vCurrType = ?
         vPrevType = ?
         vTableId  = ?
         vIndexId  = ?
         vSourceId = 0
  . /* ASSIGN */

/* New ttDatabase.SourceId: */
  FOR EACH ttDatabase NO-LOCK /* INDEX SourceId */
        BY ttDatabase.SourceId DESCENDING:
    ASSIGN vSourceId = ttDatabase.SourceId + 1.
    LEAVE.
  END.

  DO TRANSACTION:
    CREATE ttDatabase.
    ASSIGN ttDatabase.SourceType = {&SourceType}
           ttDatabase.SourceId   = vSourceId
           ttDatabase.SourceFile = ipInputFile
           ttDatabase.SourceName = vDbName
           ttDatabase.Location   = vDirName
           ttDatabase.SourcePath = ?
           ttDatabase.UseCount   = 0
           ttDatabase.ListId     = ?
    . /* ASSIGN */
  END.

  INPUT FROM VALUE(ipInputFile).

ImportLine:
  REPEAT ON ENDKEY UNDO, LEAVE:

    ASSIGN vItem = "":U.
    IMPORT vItem.

    IF vItem[1] EQ "PRIMARY":U THEN
    DO:
      ASSIGN vFoundPrimary = TRUE.
      NEXT ImportLine.
    END.

/* UNIQUE or WORD (index attributes): */
    IF CAN-DO("UNIQUE,WORD":U, vItem[1]) AND vCurrType EQ "INDEX":U THEN
    FOR LAST ttIndex EXCLUSIVE-LOCK /* INDEX DetailName */
       WHERE ttIndex.SourceId EQ ttDatabase.SourceId
         AND ttIndex.IndexId  EQ vIndexId
    TRANSACTION:
      ASSIGN ttIndex.IndexAttr = SUBSTRING(vItem[1], 1, 1).
      NEXT ImportLine.
    END. /* IF vItem[1] EQ "UNIQUE OR "WORD" */

/* AREA "Area Name" */
    IF vItem[1] EQ "AREA":U
    OR vItem[1] EQ "LOB-AREA":U THEN
    DO TRANSACTION:

      ASSIGN vAreaName = vItem[2].

      FIND FIRST ttArea
           WHERE ttArea.SourceId EQ ttDatabase.SourceId
             AND ttArea.AreaName EQ vAreaName
      NO-ERROR.

      IF NOT AVAILABLE ttArea THEN
      DO:
/* Assign the "fake" area number to provide the index uniqueness: */
        ASSIGN i = 7.
        IF vAreaName EQ "Schema Area":U THEN
        ASSIGN i = 6.
        ELSE
        FOR EACH ttArea NO-LOCK
           WHERE ttArea.SourceId   EQ ttDatabase.SourceId
              BY ttArea.SourceId   DESCENDING
              BY ttArea.AreaNumber DESCENDING:
          ASSIGN i = ttArea.AreaNumber + 1.
          LEAVE.
        END.

        CREATE ttArea.
        ASSIGN ttArea.SourceId   = ttDatabase.SourceId
               ttArea.AreaNumber = i
               ttArea.AreaName   = vAreaName
        . /* ASSIGN */
      END. /* IF NOT AVAILABLE ttArea */

/* Assign the area name to the most recently created object : */
      CASE vCurrType:

        WHEN "TABLE":U THEN
        FOR LAST ttTable EXCLUSIVE-LOCK
           WHERE ttTable.SourceId EQ ttDatabase.SourceId
             AND ttTable.TableId  EQ vTableId:
          ASSIGN ttTable.AreaName   = ttArea.AreaName
                 ttTable.AreaNumber = ttArea.AreaNumber
                 ttArea.TableCount  = ttArea.TableCount + 1
          . /* ASSIGN */
        END. /* WHEN "TABLE" */

        WHEN "INDEX":U THEN
        FOR LAST ttIndex EXCLUSIVE-LOCK
           WHERE ttIndex.SourceId EQ ttDatabase.SourceId
             AND ttIndex.IndexId  EQ vIndexId:
          ASSIGN ttIndex.AreaName   = ttArea.AreaName
                 ttIndex.AreaNumber = ttArea.AreaNumber
                 ttArea.IndexCount  = ttArea.IndexCount + 1
          . /* ASSIGN */
        END. /* WHEN "INDEX" */

        WHEN "BLOB":U OR WHEN "CLOB":U THEN
        FOR LAST ttLOB EXCLUSIVE-LOCK
           WHERE ttLOB.SourceId EQ ttDatabase.SourceId
             AND ttLOB.LobId    EQ vLobId:
          ASSIGN ttLOB.AreaName   = ttArea.AreaName
                 ttLOB.AreaNumber = ttArea.AreaNumber
                 ttArea.LobCount  = ttArea.LobCount + 1
          . /* ASSIGN */
        END. /* WHEN "BLOB" OR "CLOB" */

      END CASE. /* vCurrType */

      NEXT ImportLine.
    END. /* "AREA" or "LOB-AREA" */

    IF vItem[1] NE "ADD":U THEN
    NEXT ImportLine.

/* ADD SEQUENCE "Sequence"
   ADD TABLE "Table"
   ADD FIELD "Field" OF "Table" AS blob
   ADD INDEX "Index" ON "Table"
*/
    ASSIGN vPrevType = vCurrType
           vCurrType = vItem[2]
    . /* ASSIGN */

    IF vCurrType EQ "FIELD":U THEN
    IF NOT CAN-DO("BLOB,CLOB":U, vItem[7]) THEN
    NEXT ImportLine.
    ELSE
    DO TRANSACTION:

/* ADD FIELD "Field" OF "Table" AS blob/clob */

/* _Field._Fld-stlen begins from 1: */
      ASSIGN vLobId = 1.
      FOR EACH ttLOB NO-LOCK /* INDEX DetailId */
         WHERE ttLOB.SourceId EQ ttDatabase.SourceId
            BY ttLOB.SourceId DESCENDING
            BY ttLOB.LobId    DESCENDING:
        ASSIGN vLobId = ttLOB.LobId + 1.
        LEAVE.
      END.

      CREATE ttLOB.
      ASSIGN ttLOB.SourceId  = ttDatabase.SourceId
             ttLOB.LobId     = vLobId
             ttLOB.TableName = vItem[5]
             ttLOB.FieldName = vItem[3]
             ttLOB.LobType   = vItem[7]
             ttLOB.AreaName  = ?
      . /* ASSIGN */

      NEXT ImportLine.
    END. /* "ADD FIELD" */


/* Does the previous table have the "default" index?
   Table can have the word indexes but they can't be PRIMARY.  */
    IF  vCurrType NE "INDEX":U
    AND vPrevType EQ "INDEX":U
    AND NOT vFoundPrimary THEN
    FOR LAST ttTable NO-LOCK
       WHERE ttTable.SourceId EQ ttDatabase.SourceId
         AND ttTable.TableId  EQ vTableId
    TRANSACTION:

/* _Index._Idx-num from 1 to 7 are used by the indexes of the system tables: */
      ASSIGN vIndexId = 8.
      FOR EACH ttIndex NO-LOCK /* INDEX DetailId */
         WHERE ttIndex.SourceId EQ ttDatabase.SourceId
            BY ttIndex.SourceId DESCENDING
            BY ttIndex.IndexId  DESCENDING:
        ASSIGN vIndexId = ttIndex.IndexId + 1.
        LEAVE.
      END.

      IF vIndexId GE 994 AND vIndexId LE 1093 THEN
      ASSIGN vIndexId = 1094.

      CREATE ttIndex.
      ASSIGN ttIndex.SourceId   = ttDatabase.SourceId
             ttIndex.IndexId    = vIndexId
             ttIndex.TableName  = ttTable.TableName
             ttIndex.IndexName  = "default"
             ttIndex.AreaNumber = 6
             ttIndex.AreaName   = "Schema Area":U
             ttIndex.IndexAttr  = "D":U
      . /* ASSIGN */

      FIND FIRST ttArea
           WHERE ttArea.SourceId   EQ ttDatabase.SourceId
             AND ttArea.AreaNumber EQ ttIndex.AreaNumber
      NO-ERROR.

      IF NOT AVAILABLE ttArea THEN
      CREATE ttArea.
      ASSIGN ttArea.SourceId   = ttDatabase.SourceId
             ttArea.AreaNumber = ttIndex.AreaNumber
             ttArea.AreaName   = ttIndex.AreaName
             ttArea.IndexCount = ttArea.IndexCount  + 1
      . /* ASSIGN */

    END. /* the "default" index */

    CASE vCurrType:

      WHEN "SEQUENCE":U THEN
      NEXT ImportLine.

      WHEN "INDEX":U THEN
      DO TRANSACTION:
/*
_Index._Idx-num from 1 to 7 are used by the indexes of the system tables:
1 _File._File-Name
2 _Field._File/Field
3 _Field._Field-Name
4 _Field._Field-Position
5 _Index._File/Index
6 _Index-Field._Index/Number
7 _Index-Field._Field
*/      ASSIGN vIndexId = 8.
        FOR EACH ttIndex NO-LOCK /* INDEX DetailId */
           WHERE ttIndex.SourceId EQ ttDatabase.SourceId
              BY ttIndex.SourceId DESCENDING
              BY ttIndex.IndexId  DESCENDING:
          ASSIGN vIndexId = ttIndex.IndexId + 1.
          LEAVE.
        END.
/*
_Idx-num from 994 to 1093 are used by the indexes of the system tables.
994 _View._View-Name   (verified for Progress versions from 10.2B through 11.4)
995 _View-Col._View-Col
...
1092 _Collation._Collation-Seq
1093 _Word-rule._Wr-Number
*/      IF vIndexId GE 994 AND vIndexId LE 1093 THEN
        ASSIGN vIndexId = 1094.

/* ADD INDEX "Index" ON "Table" */
        CREATE ttIndex.
        ASSIGN ttIndex.SourceId   = ttDatabase.SourceId
               ttIndex.IndexId    = vIndexId
               ttIndex.TableName  = vItem[5]
               ttIndex.IndexName  = vItem[3]
               ttIndex.AreaName   = ?
               ttIndex.IndexAttr  = "":U
        . /* ASSIGN */
      END. /* WHEN "INDEX" */

      WHEN "TABLE":U THEN
      DO TRANSACTION:
/* _File._File-Number begins from 1. */
        ASSIGN vTableId = 1.
        FOR EACH ttTable NO-LOCK /* INDEX DetailId */
           WHERE ttTable.SourceId EQ ttDatabase.SourceId
              BY ttTable.SourceId DESCENDING
              BY ttTable.TableId  DESCENDING:
          ASSIGN vTableId = ttTable.TableId + 1.
          LEAVE.
        END.

/* ADD TABLE "Table" */
        CREATE ttTable.
        ASSIGN ttTable.SourceId   = ttDatabase.SourceId
               ttTable.TableId    = vTableId
               ttTable.TableName  = vItem[3]
               ttTable.AreaName   = ?
               vFoundPrimary      = FALSE
        . /* ASSIGN */
      END. /* WHEN "TABLE" */
    END CASE. /* vCurrType */

  END. /* ImportLine: REPEAT */

  INPUT CLOSE.

/* If the input file has the incorrect contents: */
  IF NOT CAN-FIND(FIRST ttArea
                  WHERE ttArea.SourceId EQ ttDatabase.SourceId) THEN
  DO TRANSACTION:
    DELETE ttDatabase.
  END.

END PROCEDURE. /* LoadDfFiles */


/* ------------------------------------------------------------------------- */

PROCEDURE LoadStFiles:

  DEFINE INPUT PARAMETER ipInputFile AS CHARACTER NO-UNDO.

  &SCOPED-DEFINE SourceType 3

  DEFINE VARIABLE vDirName     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDbName      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDbPath      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vSlash       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vAreaNumber  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vAreaName    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vAreaInfo    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vRecPerBlock AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vClusterSize AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLine        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i            AS INTEGER   NO-UNDO.

/* DbName: ipInputFile = "path/to/<DbName>.st */
  ASSIGN vSlash   = IF OPSYS EQ "Unix":U THEN "/":U ELSE "~\":U
         i        = NUM-ENTRIES(ipInputFile, vSlash)
         vDirName = ENTRY(i - 1, ipInputFile, vSlash)
         vDbName  = ENTRY(i, ipInputFile, vSlash)
         vDbName  = ENTRY(1, vDbName, ".":U)
         vDbPath  = ?
         i = 0
  . /* ASSIGN */

/* New ttDatabase.SourceId: */
  FOR EACH ttDatabase NO-LOCK /* INDEX SourceId */
        BY ttDatabase.SourceId DESCENDING:
    ASSIGN i = ttDatabase.SourceId + 1.
    LEAVE.
  END.

  DO TRANSACTION:
    CREATE ttDatabase.
    ASSIGN ttDatabase.SourceType = {&SourceType}
           ttDatabase.SourceId   = i
           ttDatabase.SourceFile = ipInputFile
           ttDatabase.SourceName = vDbName
           ttDatabase.Location   = vDirName
           ttDatabase.SourcePath = ?
           ttDatabase.ListId     = ?
           ttDatabase.UseCount   = 0
    . /* ASSIGN */
  END.

  INPUT FROM VALUE(ipInputFile).

ImportLine:
  REPEAT TRANSACTION:

    ASSIGN vLine = "".
    IMPORT UNFORMATTED vLine.

/* d "Area Name":7,64;512 /path/to/dbname_7.d1 */
    IF NOT vLine BEGINS "d ":U
    OR NOT vLine MATCHES "*:*":U THEN
    NEXT ImportLine.

    ASSIGN vLine =      TRIM(vLine)
           vLine = SUBSTRING(vLine, 3)
           vLine =      TRIM(vLine)
           i     =     INDEX(vLine, ":":U)
        /* vAreaInfo = "Area Name":Num,RPB;CLS */
           vAreaInfo = SUBSTRING(vLine, 1,
                           INDEX(vLine + " ":U, " ":U, i + 1) - 1)
           vDbPath   = SUBSTRING(vLine, LENGTH(vAreaInfo) + 2)
                       WHEN vDbPath EQ ?
           vAreaName = SUBSTRING(vAreaInfo, 1, i - 1)
           vAreaName = TRIM(vAreaName, "~"":U)
        /* vAreaInfo = Num,RPB;CLS */
           vAreaInfo = SUBSTRING(vAreaInfo, i + 1)
           i         = INDEX(vAreaInfo + ",":U, ",":U)
           vAreaNumber  = INTEGER(SUBSTRING(vAreaInfo, 1, i - 1))
        /* vAreaInfo = RPB;CLS */
           vAreaInfo    = SUBSTRING(vAreaInfo, i + 1)
           i            = INDEX(vAreaInfo + ";":U, ";":U)
           vRecPerBlock = INTEGER(SUBSTRING(vAreaInfo, 1, i - 1))
           vClusterSize = INTEGER(SUBSTRING(vAreaInfo,    i + 1))
    NO-ERROR.

    IF ERROR-STATUS:NUM-MESSAGES GT 0
    OR CAN-FIND(FIRST ttArea /* INDEX AreaNumber */
                WHERE ttArea.SourceId   EQ ttDatabase.SourceId
                  AND ttArea.AreaNumber EQ vAreaNumber) THEN
    NEXT ImportLine.

    CREATE ttArea.
    ASSIGN ttArea.SourceId    = ttDatabase.SourceId
           ttArea.AreaNumber  = vAreaNumber
           ttArea.AreaName    = vAreaName
           ttArea.RecPerBlock = vRecPerBlock
           ttArea.ClusterSize = vClusterSize
    . /* ASSIGN */

  END. /* ImportLine: REPEAT */

  INPUT CLOSE.

/* If the input file has the incorrect contents: */
  IF NOT CAN-FIND(FIRST ttArea
                  WHERE ttArea.SourceId EQ ttDatabase.SourceId) THEN
  DO TRANSACTION:
    DELETE ttDatabase.
  END.
  ELSE
  IF vDbPath NE ? THEN
  DO TRANSACTION:
    ASSIGN vDbPath = TRIM(vDbPath)
           ttDatabase.SourcePath = IF vDbPath MATCHES "*~~~.d1":U
                                   THEN SUBSTRING(vDbPath, 1,  MAX(
                                          R-INDEX(vDbPath,  "/":U),
                                          R-INDEX(vDbPath, "~\":U), 2) - 2)
                                   ELSE           vDbPath
    . /* ASSIGN */
  END.

END PROCEDURE. /* LoadStFiles */

/* ------------------------------------------------------------------------- */


