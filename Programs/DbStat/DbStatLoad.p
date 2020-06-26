DEFINE VARIABLE vBaseDir AS CHARACTER NO-UNDO INITIAL
/* "Basic directory with the input files".*/
  "D:\Proapps\LoadDbStat\Test".

ETIME(TRUE).
RUN DeleteAll.
MESSAGE "DeleteAll" ETIME "milliseconds" VIEW-AS ALERT-BOX INFO BUTTONS OK.

UPDATE vBaseDir LABEL "Dir" FORMAT "x(72)" 
  HELP "Enter directory with input files (dbanalys and auxiliary files)."
WITH SIDE-LABEL.

ETIME(TRUE).
RUN ProcessDir(vBaseDir, ?, "*~~~.*",           "Dbanalys":U).
MESSAGE "Load time:" ETIME "milliseconds" VIEW-AS ALERT-BOX INFO BUTTONS OK.

/*
RUN ProcessDir(vBaseDir, ?, "sports~~~.df",            "DfFile":U).
RUN ProcessDir(vBaseDir, ?, "*~~~.st",            "StFile":U).
RUN ProcessDir(vBaseDir, ?, "viewB2.*.txt",       "ViewB2":U).
RUN ProcessDir(vBaseDir, ?, "*statistics*.txt",   "ProstrctStat":U).
RUN ProcessDir(vBaseDir, ?, "ObjectInfo*~~~.txt", "ObjectInfo":U).
*/
/*RUN DisplayReport(0).*/

/* ------------------------------------------------------------------------
    File        : LoadDbStat.p
    Purpose     : Load the output of proutil -C dbanalys
                  and the output of other Progress utilities
                  into the statdb databases.

    Author(s)   : George Potemkin
    Created     : January 17, 2015
    Modified    : October 22, 2015
    Version     : 2.0
    
    The most recent version can be downloaded from here:
    ftp://ftp.progress-tech.ru/pub/Users/george/Programs/DbStatLoad.p
    
    Syntax      : See the examples above.
    
------------------------------------------------------------------------ */

/* Field separator to use in the lists to set MD5 signature: */
&SCOPED-DEFINE MD5Sep ".":U

/* ******************************  Temp-tables  **************************** */

DEFINE TEMP-TABLE ttOSFile NO-UNDO
  FIELD osFileName AS CHARACTER
  FIELD osFilePath AS CHARACTER
  FIELD osFileAttr AS CHARACTER
  INDEX osFileName
        osFileName
. /* DEFINE TEMP-TABLE ttOSFile */

DEFINE TEMP-TABLE ttListMap NO-UNDO
  FIELD LineNumber AS INTEGER
  FIELD NumOfLines AS INTEGER
  FIELD ObjectId   AS INTEGER
  FIELD ObjectType AS INTEGER
  INDEX LineNumber IS UNIQUE
        LineNumber
. /* DEFINE TEMP-TABLE ttListMap */

/* *******************************  Variables  ***************************** */

/* gvLine: the last line imported from dbanalys file. */
DEFINE VARIABLE gvLine   AS CHARACTER NO-UNDO.

/* gvReread: Re-read the last line.
For example, if a procedure reads a list that does not end by line-stopper.
It will stop reading the list if new line does not match a list pattern.
But new line should be parsed by another procedure.
*/
DEFINE VARIABLE gvReread AS LOGICAL NO-UNDO INITIAL FALSE.

/* gvTime: timestamp of the last imported line. */
DEFINE VARIABLE gvTime AS CHARACTER NO-UNDO.

/* gvLgProc: fragment of the line from .lg file that identify a process. */
DEFINE VARIABLE gvLgProc AS CHARACTER NO-UNDO.

/* gvLgMsg#: the message number of the line imported from .lg file */
DEFINE VARIABLE gvLgMsg# AS CHARACTER NO-UNDO.

/* ------------------------------------------------------------------------- */

FUNCTION GetText RETURN CHARACTER:

  DEFINE VARIABLE vText AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i     AS INTEGER   NO-UNDO.

  REPEAT ON ENDKEY UNDO, LEAVE:

    /*PROCESS EVENTS.*/
    IF gvReread THEN
    ASSIGN gvReread = FALSE. /* The current value of gvLine will not changed.*/
    ELSE
    DO:
      ASSIGN gvLine = "":U NO-ERROR. /* Clearing error status */
      IMPORT UNFORMATTED gvLine.
    END.

/* Cut off a timestamp if it exists: --------------------------------------- */

    ASSIGN gvTime = ENTRY(1, gvLine, " ":U).

/* If the source file is dbanalys output with addon timestamps:
   17:02:34 CHAIN ANALYSIS FOR AREA "Control Area": 1 
   or
  [2015/09/11@08:28:48.811+0300] P-25271 T-1 I DBANALYS : (451)  Dbanalys session begin for on .
  [2015/09/18@08:00:01.526-0400] P-13238 T-1 I Usr    24: (7129) Usr 24 set name to Dbanalys.
*/
    IF gvTime MATCHES "*:*:*":U THEN
    DO:
      ASSIGN i        = LENGTH(gvTime)
             vText    = TRIM(SUBSTRING(gvLine, i + 2))
             gvLgProc = "":U
      . /* ASSIGN */

/* If the source file is db log: */
/* [2015/09/11@08:28:48.811+0300] P-25271    T-1    I DBANALYS : (451)  Text */
      IF vText MATCHES "P-* T-* I *: (*) *":U THEN
      ASSIGN i        = INDEX(vText, ")":U)
/* P-25271    T-1    I DBANALYS : (451) */
             gvLgProc = SUBSTRING(vText, 1, i - 1)
             vText    = TRIM(SUBSTRING(vText, i + 2))
             i        = INDEX(gvLgProc, "(":U)
             gvLgMsg# = SUBSTRING(gvLgProc, i + 1)
/* P-25271    T-1    I DBANALYS : */
             gvLgProc = SUBSTRING(gvLgProc, 1, i - 2)
      . /* ASSIGN */
    END.
    ELSE
    ASSIGN vText    = TRIM(gvLine)
           gvTime   = ?
           gvLgProc = "":U
           gvLgMsg# = "":U
    . /* ASSIGN */

/* Skip the empty lines: */
    IF vText NE "":U THEN
    RETURN vText.
  END.

  RETURN ?.

END FUNCTION. /* GetText */

/* ------------------------------------------------------------------------- */

PROCEDURE DeleteAll:

  DEFINE VARIABLE vDelete AS LOGICAL NO-UNDO INITIAL FALSE.

  IF CAN-FIND(FIRST DbStat) THEN
  MESSAGE "Delete the previous statitsics?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
  TITLE "" UPDATE vDelete.

  IF NOT vDelete THEN
  RETURN.

  STATUS DEFAULT "Running DeleteAll. Please wait...".

  FOR EACH DbStatError:
    DELETE DbStatError.
  END.

  FOR EACH TableStat:
    DELETE TableStat.
  END.

  FOR EACH IndexStat:
    DELETE IndexStat.
  END.

  FOR EACH LobStat:
    DELETE LobStat.
  END.

  FOR EACH IndexField:
    DELETE IndexField.
  END.

  FOR EACH ChainStat:
    DELETE ChainStat.
  END.

  FOR EACH ChainBlock:
    DELETE ChainBlock.
  END.

  FOR EACH AreaStat:
    DELETE AreaStat.
  END.

  FOR EACH DbStat:
    DELETE DbStat.
  END.

  FOR EACH DbHost:
    DELETE DbHost.
  END.

  FOR EACH DbAlias:
    DELETE DbAlias.
  END.

END PROCEDURE. /* DeleteAll */

/* ------------------------------------------------------------------------- */

PROCEDURE DisplayReport:

  DEFINE INPUT PARAMETER ipStatId AS INTEGER NO-UNDO.

  FOR FIRST DbStat NO-LOCK
      WHERE DbStat.StatId EQ ipStatId:

    DISPLAY DbStat EXCEPT SchemaMD5 AreaMD5 PhaseName PhaseTime
    WITH TITLE " DbStat " SIDE-LABELS.

    HIDE ALL.
    FOR EACH DbStatError OF DbStat NO-LOCK
    WITH TITLE " DbStatError " + DbStat.Db_Name SIDE-LABELS:
      DISPLAY DbStatError.
      DISPLAY SKIP "-------------------------------------------------------".
    END.
    
    FOR EACH AreaStat OF DbStat NO-LOCK:

      DISPLAY AreaStat EXCEPT SchemaMD5 PhaseName PhaseTime
      WITH TITLE " AreaStat " + DbStat.Db_Name SIDE-LABELS.

      HIDE ALL.
      FOR EACH ChainStat OF AreaStat NO-LOCK
      WITH TITLE " ChainStat " + DbStat.Db_Name SIDE-LABELS:
        DISPLAY ChainStat.
        DISPLAY SKIP "-------------------------------------------------------".
      END.

      HIDE ALL.
      FOR EACH  TableStat OF AreaStat NO-LOCK
      WHERE NOT TableStat.TableName BEGINS "_"
      WITH TITLE " TableStat " + DbStat.Db_Name SIDE-LABELS:
        DISPLAY TableStat.
        DISPLAY SKIP "-------------------------------------------------------".
      END.

      HIDE ALL.
      FOR EACH  IndexStat OF AreaStat NO-LOCK
      WHERE NOT IndexStat.TableName BEGINS "_"
      WITH TITLE " IndexStat " + DbStat.Db_Name SIDE-LABELS:
        DISPLAY IndexStat EXCEPT IndexField.
        DISPLAY SKIP "-------------------------------------------------------".
      END.

      HIDE ALL.
      FOR EACH LobStat OF AreaStat NO-LOCK
      WITH TITLE " LobStat " + DbStat.Db_Name SIDE-LABELS:
        DISPLAY LobStat.
        DISPLAY SKIP "-------------------------------------------------------".
      END.

    END.
  END. /* FOR FIRST DbStat */

END PROCEDURE. /* DisplayReport */

/* ------------------------------------------------------------------------- */

/* String2DateTime() function accepts the strings that represent only time
   (like 16:04:13) or the strings that contain the dates
   (like 2015/08/08@16:04:13.372).
   The function retuns the date/time even if the input string contains
   the time only. It will use the date from the most recent input with
   fully specified date/time. 
   The gvLastFullTime variable stores the last fully specified date/time.
*/
DEFINE NEW GLOBAL SHARED VARIABLE gvLastFullTime AS DATETIME NO-UNDO INITIAL ?.

FUNCTION String2DateTime RETURNS DATETIME (INPUT ipString AS CHARACTER).

/* Input string must have a format like:
   16:04:13

database.lg:
   [2015/08/08@16:04:13.372-0400]
   also possible:
   [2014/06/13@[2014/06/13@09:18:19.096+0600] P-21937      T-1     I ABL

clientlog:
   [13/08/23@10:09:51.721+0400]
   
promon/dbanalys:
   Tue Feb  5 11:28:14 2013

admserv.log:
  [11/10/14 2:00:37 PM]
  [2014/11/15@18:22:07.114+0400]

NS1.ns.log
  [14/11/15@18:22:29.535+0400]
  [14/12/25@08:17:34.927+0400]

cmdplugin.log
  main>(Nov 15, 2014 18:22:15:722)

iso-date
2015-10-20T10:25:01.664+03:00
*/
  DEFINE VARIABLE vMonthList AS CHARACTER NO-UNDO INITIAL
                           "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec":U.

  DEFINE VARIABLE vWeekday AS CHARACTER NO-UNDO INITIAL
                                               "Mon,Tue,Wed,Thu,Fri,Sat,Sun":U.
  
  DEFINE VARIABLE vDateTime AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vBaseDate AS DATE      NO-UNDO.
  DEFINE VARIABLE vYear     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMonth    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vDay      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vHour     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMin      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vSec      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMsec     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vDate     AS DATE      NO-UNDO.
  DEFINE VARIABLE vItem     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE n         AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i         AS INTEGER   NO-UNDO.

  ASSIGN ipString  = TRIM(ipString)
         ipString  = TRIM(ipString, "[]":U)
         ipString  = REPLACE(ipString, "@":U, " ":U)
         vDateTime = ?
         vYear     = ?
         vMonth    = ?
         vDay      = ?
         vHour     = ?
  . /* ASSIGN */

/* ISO-DATE: */
  IF ipString MATCHES "....-..-..T..:..:..*":U THEN
  ASSIGN ipString  = REPLACE(SUBSTRING(ipString, 1, 10), "-":U, "/":U)
                   + " ":U + SUBSTRING(ipString, 12)
  . /* ASSIGN */

  ASSIGN n = NUM-ENTRIES(ipString, " ":U).
  DO i = 1 TO n:

    ASSIGN vItem = ENTRY(i, ipString, " ":U).

    IF vItem MATCHES "*:*:*":U THEN
    ASSIGN vHour = INTEGER(ENTRY(1, vItem, ":":U))
           vMin  = INTEGER(ENTRY(2, vItem, ":":U))
           vItem =         ENTRY(3, vItem, ":":U)
           vSec  = INTEGER(ENTRY(1, vItem, ".":U))
           vItem = IF NUM-ENTRIES(  vItem, ".":U) GE 2 THEN
                   ENTRY(2, vItem, ".":U) ELSE "0":U
           vMsec = INTEGER(SUBSTRING(vItem, 1, 3))
    NO-ERROR.  
    ELSE

    IF vItem MATCHES "*/*/*":U THEN
    ASSIGN vYear  = INTEGER(ENTRY(1, vItem, "/":U))
           vMonth = INTEGER(ENTRY(2, vItem, "/":U))
           vDay   = INTEGER(ENTRY(3, vItem, "/":U))
    NO-ERROR. /* ASSIGN */
    ELSE

    IF LENGTH(vItem) EQ 3 THEN
    ASSIGN vMonth = LOOKUP(vItem, vMonthList)
               WHEN LOOKUP(vItem, vMonthList) GT 0
    . /* ASSIGN */
    ELSE

    IF LOOKUP(vItem, "AM,PM":U) GT 0 THEN
    ASSIGN vHour = vHour MOD 12
           vHour = vHour  +  12 WHEN vItem EQ "PM":U
    . /* ASSIGN */
    ELSE

    IF vItem NE "":U AND TRIM(vItem, "0123456789":U) EQ "":U THEN
    IF LENGTH(vItem) EQ 4 THEN
    ASSIGN vYear = INTEGER(vItem).

    ELSE
    IF LENGTH(vItem) LE 2 THEN
    ASSIGN vDay  = INTEGER(vItem).
  END.

  IF vHour EQ ? THEN
  RETURN ?.

/* For year in the short format: */
  IF vYear LT 100 THEN 
  ASSIGN vYear = vYear + 100 WHEN vYear LT (SESSION:YEAR-OFFSET MOD 100)
         vYear = vYear + SESSION:YEAR-OFFSET - SESSION:YEAR-OFFSET MOD 100
  . /* ASSIGN */

/* Full time: date + time */
  IF  vYear  NE ?
  AND vMonth NE ?
  AND vDay   NE ? THEN
  ASSIGN vDateTime = DATETIME(vMonth, vDay, vYear, vHour, vMin, vSec, vMsec)
         gvLastFullTime = vDateTime
  NO-ERROR. /* ASSIGN */
  ELSE
  DO:
/* otherwise use the date of last full time or TODAY: */
    ASSIGN vDateTime = NOW
           vDateTime = gvLastFullTime WHEN gvLastFullTime NE ?
           vDateTime = DATETIME(DATE(vDateTime), vHour * 3600000 +
                                                 vMin  *   60000 +
                                                 vSec  *    1000 +
                                                 vMsec)
    . /* ASSIGN */

    IF vDateTime LT gvLastFullTime THEN
    ASSIGN vDateTime = ADD-INTERVAL(vDateTime, 1, "days").
  END.

  RETURN vDateTime.

END FUNCTION. /* String2DateTime */

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

FUNCTION GetDbId RETURNS INTEGER (ipHostName AS CHARACTER,
                                   ipDbName  AS CHARACTER,
                                   ipDbPath  AS CHARACTER,
                                   ipDesc    AS CHARACTER):

  DEFINE VARIABLE vHostId AS INTEGER NO-UNDO.
  DEFINE VARIABLE vDbId   AS INTEGER NO-UNDO.

/* Can we use the previously created DbHost and DbAlias records? */
  FOR EACH DbHost NO-LOCK
     WHERE DbHost.HostName EQ ipHostName,

      EACH DbAlias NO-LOCK
     WHERE DbAlias.HostId  EQ DbHost.HostId,
    
     FIRST DbStat NO-LOCK
     WHERE DbStat.Db_Id   EQ DbAlias.Db_Id
       AND DbStat.Db_Name EQ ipDbName
       AND DbStat.Db_Path EQ ipDbPath

        BY DbHost.Priority  DESCENDING
        BY DbAlias.Priority DESCENDING:

    RETURN DbAlias.Db_Id.
  END.

/* Can we use the previously created DbHost record? */
  ASSIGN vHostId = ?.

  FOR EACH DbHost NO-LOCK
     WHERE DbHost.HostName EQ ipHostName
        BY DbHost.Priority DESCENDING:
    ASSIGN vHostId = DbHost.HostId.
    LEAVE.
  END.

/* Otherwise create new DbHost and DbAlias records: */
  IF vHostId EQ ? THEN
  DO TRANSACTION:
    ASSIGN vHostId = 0.
    FOR EACH DbHost NO-LOCK
          BY DbHost.HostId DESCENDING:
      ASSIGN vHostId = DbHost.HostId + 1.
      LEAVE.
    END.
  
    CREATE DbHost.
    ASSIGN DbHost.HostId     = vHostId
           DbHost.HostName   = ipHostName
           DbHost.HostAlias  = ipHostName
           DbHost.HostDesc   = ipDesc
           DbHost.Priority   = 0
           DbHost.CreateTime = NOW
    . /* ASSIGN */
  END.

  ASSIGN vDbId = 0.
  FOR EACH DbAlias NO-LOCK
        BY DbAlias.Db_Id DESCENDING:
    ASSIGN vDbId = DbAlias.Db_Id + 1.
    LEAVE.
  END.

  DO TRANSACTION:
    CREATE DbAlias.
    ASSIGN DbAlias.HostId     = vHostId
           DbAlias.Db_Id      = vDbId
           DbAlias.Db_Name    = ipDbName
           DbAlias.Db_Path    = ipDbPath
           DbAlias.Db_Desc    = ipDesc
           DbAlias.Priority   = 0
           DbAlias.CreateTime = NOW
    . /* ASSIGN */
  END. /* DO TRANSACTION */

  RETURN vDbId.

END FUNCTION. /* GetDbId */


/* ------------------------------------------------------------------------- */

FUNCTION GetStatId RETURNS INTEGER (
                  ipHostName  AS CHARACTER,
                  ipDbPath    AS CHARACTER,
                  ipBgnTime   AS DATETIME,
                  ipInputFile AS CHARACTER):

  DEFINE VARIABLE vStatId    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vHostName  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDbId      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vDbName    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDbPath    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDirName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vInputTime AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vSlash     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i          AS INTEGER   NO-UNDO.

/* Split ipDbPath into vDbName and directory name: */
  ASSIGN i = MAX(R-INDEX(ipDbPath, "/":U),
                 R-INDEX(ipDbPath, "~\":U)).
  IF i GT 0 THEN
  ASSIGN vDbName = TRIM(SUBSTRING(ipDbPath, i + 1))
         vDbPath = TRIM(SUBSTRING(ipDbPath, 1, i - 1))
  . /* ASSIGN */
  ELSE
  ASSIGN vDbName = ?
         vDbPath = TRIM(vDbPath)
  . /* ASSIGN */

  ASSIGN FILE-INFO:FILE-NAME = ipInputFile
         vInputTime  = DATETIME(FILE-INFO:FILE-MOD-DATE,
                                FILE-INFO:FILE-MOD-TIME * 1000)
         ipInputFile = FILE-INFO:FULL-PATHNAME
         vSlash      = IF OPSYS EQ "Unix":U THEN "/":U ELSE "~\":U
         i           = NUM-ENTRIES(ipInputFile, vSlash)
         vDirName = IF i GE 2 THEN ENTRY(i - 1, ipInputFile, vSlash) ELSE "":U
       /* DbName: ipInputFile = "path/to/<DbName>.st */
         vDbName  = ENTRY(i, ipInputFile, vSlash) WHEN vDbName EQ ?
                                                    OR vDbName EQ "":U
         vDbName  = ENTRY(1, vDbName, ".":U)
         vHostName = IF ipHostName EQ ? THEN vDirName ELSE ipHostName
  . /* ASSIGN */

/* Dbanalys can be loaded from a file with the addon timestamps.
   If dbanalys utility used a modified promsgs file then some of messages
   were written to db log where they got the accurate timestamps.
   Db log file can be used to update the existent statistics:
*/
  IF ipBgnTime NE ? THEN
  FOR EACH DbStat EXCLUSIVE-LOCK
     WHERE DbStat.Db_Host EQ vHostName
       AND DbStat.Db_Path EQ vDbPath
       AND DbStat.Db_Name EQ vDbName
       AND DbStat.BgnTime GT ADD-INTERVAL(ipBgnTime, -2, "seconds":U)
       AND DbStat.BgnTime LT ADD-INTERVAL(ipBgnTime, +2, "seconds":U)
        BY ABS(INTERVAL(DbStat.BgnTime, ipBgnTime,  "milliseconds":U))
  TRANSACTION:
    ASSIGN DbStat.SourceFile = ipInputFile
           DbStat.SourceTime = vInputTime
    . /* ASSIGN */
    RETURN DbStat.StatId.
  END.

  ASSIGN vDbId = GetDbId(vHostName,
                         vDbName,
                         vDbPath,
                         ipInputFile) /* as description */
  . /* ASSIGN */

  ASSIGN   vStatId = 0.
  FOR EACH DbStat NO-LOCK /* INDEX StatId */
        BY DbStat.StatId DESCENDING:
    ASSIGN vStatId = DbStat.StatId + 1.
    LEAVE.
  END.

  DO TRANSACTION:
    CREATE DbStat.
    ASSIGN DbStat.StatId     = vStatId
           DbStat.Db_Id      = vDbId
           DbStat.Db_Name    = vDbName
           DbStat.Db_Path    = vDbPath
           DbStat.Db_Host    = vHostName
           DbStat.SourceType = ENTRY(1, PROGRAM-NAME(2), " ":U) /* Caller */
           DbStat.SourceFile = ipInputFile
           DbStat.SourceTime = vInputTime
           DbStat.CreateTime = NOW
    . /* ASSIGN */
  END.

  RETURN vStatId.

END FUNCTION. /* GetStatId */

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

PROCEDURE ProstrctStat:

/* Load the outputs of prostrct statistics */

  DEFINE INPUT PARAMETER ipInputFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vStatId       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vDbName       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDbPath       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vNumAreas     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vAreaNumber   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vAreaName     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vAreaInfo     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vExtentName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDbBlockSize  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vRecPerBlock  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vClusterSize  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vActiveBlocks AS INT64     NO-UNDO.
  DEFINE VARIABLE vDataBlocks   AS INT64     NO-UNDO.
  DEFINE VARIABLE vFreeBlocks   AS INT64     NO-UNDO.
  DEFINE VARIABLE vEmptyBlocks  AS INT64     NO-UNDO.
  DEFINE VARIABLE vTotalBlocks  AS INT64     NO-UNDO.
  DEFINE VARIABLE vOffset       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLine         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vLineNumber   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMD5List      AS LONGCHAR  NO-UNDO.

  INPUT FROM VALUE(ipInputFile).
  ASSIGN vDbName      = ?
         vDbPath      = ?
         vDbBlockSize = ?
         vExtentName  = " ":U /* empty line will not match a space */
         vLineNumber  = 0
  . /* ASSIGN */

/* Parsing errors are ignored for header: 
   Input file will be ignored when program fails to get the key information.
*/
ImportHeader:
  REPEAT:

    ASSIGN vLine = "":U.
    IMPORT UNFORMATTED vLine.
    ASSIGN vLine = TRIM(vLine)
           vLineNumber = vLineNumber + 1
    . /* ASSIGN */

    IF vLine EQ "":U THEN
    NEXT ImportHeader.

/* Database: sports */
    IF vLine BEGINS "Database: ":U THEN
    ASSIGN vDbName     = ENTRY(2, vLine, " ":U)
           vExtentName = SUBSTITUTE("*&1~~~.db *":U, vDbName)
    . /* ASSIGN */
    ELSE

/* Primary data block size: 4096 */
    IF vLine BEGINS "Primary data block size: ":U THEN
    ASSIGN vDbBlockSize = INTEGER(ENTRY(5, vLine, " ":U)) NO-ERROR.
    ELSE

/* Statistics for Area: Control Area
      D:\dir with spaces\sports.db  32768
*/  IF vLine MATCHES vExtentName THEN
    DO:
      ASSIGN i       =   R-INDEX(vLine, " ":U)
             vDbPath = SUBSTRING(vLine, 1, i - 1) /* Cut off file size */
             vDbPath = TRIM(vDbPath)
      NO-ERROR. /* ASSIGN */
      
/* Skip Control Area: */
      DO WHILE NOT vLine MATCHES "*Cluster size: *":U:
        IMPORT UNFORMATTED vLine.
        ASSIGN vLineNumber = vLineNumber + 1.
      END.

      ASSIGN vOffset = SEEK(INPUT).
      LEAVE ImportHeader.
    END. /* IF vLine MATCHES vExtentName */
  END. /* ImportHeader: REPEAT */

  INPUT CLOSE.

  IF vDbName EQ ?
  OR vDbPath EQ ? THEN
  RETURN.

/* Create new vStatId: */
  ASSIGN vStatId = GetStatId(? /*Host*/, vDbPath, ? /*Time*/, ipInputFile).
  
  IF vStatId EQ ? THEN
  RETURN.

  ASSIGN vExtentName   = SUBSTITUTE("*&1_*~~~.d1 *":U, vDbName)
         vAreaNumber   = ?
         vAreaName     = ?
         vRecPerBlock  = ?
         vClusterSize  = ?
         vTotalBlocks  = ?
         vActiveBlocks = ?
         vDataBlocks   = ?
         vFreeBlocks   = ?
         vEmptyBlocks  = ?
         vMD5List      = "":U
  . /* ASSIGN */

  INPUT FROM VALUE(ipInputFile).
  SEEK INPUT TO vOffset.

ImportData:
  REPEAT:

    ASSIGN vLine = "":U.
    IMPORT UNFORMATTED vLine.
    ASSIGN vLine = TRIM(vLine)
           vLineNumber = vLineNumber + 1
    . /* ASSIGN */

    IF vLine EQ "":U THEN
    NEXT ImportData.

/*  Statistics for Area: Schema Area */
    IF vLine BEGINS "Statistics for Area: ":U THEN
    ASSIGN vAreaName = TRIM(SUBSTRING(vLine, 22)).
    ELSE

/*   Files in Area: Info Area
     D:\dir with spaces\sports_7.d1  131072
*/  IF vLine MATCHES vExtentName THEN
    ASSIGN i     =   R-INDEX(vLine, ".d1":U)
           vLine = SUBSTRING(vLine, 1, i - 1)
           i     =   R-INDEX(vLine, "_":U)
           vLine = SUBSTRING(vLine, i + 1)
           vAreaNumber = INTEGER(vLine)
    NO-ERROR. /* ASSIGN */
    ELSE

/*  Active blocks: 276 */
/*    Data blocks: 276 */
/*    Free blocks: 0   */
/*   Empty blocks: 12  */
/*   Total blocks: 288 */
/*  Extent blocks: 1   */
/*  Records/Block: 32  */
/*   Cluster size: 1   */

    IF vLine BEGINS "Active blocks: ":U THEN
    ASSIGN vActiveBlocks = INT64(SUBSTRING(vLine, 16)) NO-ERROR.
    ELSE

    IF vLine BEGINS "Data blocks: ":U THEN
    ASSIGN vDataBlocks   = INT64(SUBSTRING(vLine, 14)) NO-ERROR.
    ELSE

    IF vLine BEGINS "Free blocks: ":U THEN
    ASSIGN vFreeBlocks   = INT64(SUBSTRING(vLine, 14)) NO-ERROR.
    ELSE

    IF vLine BEGINS "Empty blocks: ":U THEN
    ASSIGN vEmptyBlocks  = INT64(SUBSTRING(vLine, 15)) NO-ERROR.
    ELSE

    IF vLine BEGINS "Total blocks: ":U THEN
    ASSIGN vTotalBlocks  = INT64(SUBSTRING(vLine, 15)) NO-ERROR.
    ELSE

    IF vLine BEGINS "Records/Block: ":U THEN
    ASSIGN vRecPerBlock  = INT64(SUBSTRING(vLine, 16)) NO-ERROR.
    ELSE

    IF vLine BEGINS "Cluster size: ":U THEN
    DO TRANSACTION:
      ASSIGN vClusterSize  = INT64(SUBSTRING(vLine, 15)) NO-ERROR.
      
      CREATE AreaStat.
      ASSIGN AreaStat.StatId      = vStatId
             AreaStat.AreaNumber  = IF vAreaName EQ "Schema Area":U THEN 6 ELSE
                                    vAreaNumber
             AreaStat.AreaName    = vAreaName
             AreaStat.RecPerBlock = vRecPerBlock
             AreaStat.ClusterSize = vClusterSize
             AreaStat.TotalBlocks = vTotalBlocks
             AreaStat.HighBlock   = vActiveBlocks
             AreaStat.DataBlocks  = vDataBlocks
             AreaStat.FreeBlocks  = vFreeBlocks
             AreaStat.EmptyBlocks = vEmptyBlocks
             vNumAreas = vNumAreas + 1
             vMD5List  = vMD5List + {&MD5Sep} + STRING(vAreaNumber)
                                  + {&MD5Sep} + STRING(vClusterSize)
                                  + {&MD5Sep} + vAreaName
             vAreaNumber   = ?
             vAreaName     = ?
             vRecPerBlock  = ?
             vClusterSize  = ?
             vTotalBlocks  = ?
             vActiveBlocks = ?
             vDataBlocks   = ?
             vFreeBlocks   = ?
             vEmptyBlocks  = ?
      NO-ERROR. /* ASSIGN */
    END. /* IF vLine BEGINS "Cluster size: " */

    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    LEAVE ImportData.

  END. /* ImportData: REPEAT */

  INPUT CLOSE.

/* If the import was interrupted due to the parsing errors: */
  DO i = 1 TO ERROR-STATUS:NUM-MESSAGES
  TRANSACTION:
    CREATE DbStatError.
    ASSIGN DbStatError.StatId       = vStatId
           DbStatError.AreaNumber   = vAreaNumber
           DbStatError.ErrorNumber  = ERROR-STATUS:GET-NUMBER(i)
           DbStatError.ErrorMessage = ERROR-STATUS:GET-MESSAGE(i)
           DbStatError.ErrorSource  = vLine
    . /* ASSIGN */
  END.

  IF vNumAreas GT 0 THEN
  FOR FIRST DbStat EXCLUSIVE-LOCK
      WHERE DbStat.StatId EQ vStatId
  TRANSACTION:
    ASSIGN DbStat.DbBlockSize = vDbBlockSize
           DbStat.AreaMD5     = MD5-DIGEST(vMD5List)
           vMD5List = "":U
    . /* ASSIGN */
  END. /* FOR FIRST DbStat */

END PROCEDURE. /* ProstrctStat */

/* ------------------------------------------------------------------------- */

PROCEDURE ViewB2:

/* Load the outputs of proutil -C viewB2 */

  DEFINE INPUT PARAMETER ipInputFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vStatId        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vDbPath        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vNumAreas      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vAreaNumber    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vAreaName      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vAltBufferPool AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE vLine          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vLineNumber    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vObjectNumber  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vObjectType    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vObjectName    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vSecondName    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i              AS INTEGER   NO-UNDO.

/* Parsing errors are ignored for header: 
   Input file will be ignored when program fails to get the key information.
*/
  ASSIGN vDbPath = ?.

  INPUT FROM VALUE(ipInputFile).

ImportHeader:
  REPEAT:

    IMPORT UNFORMATTED vLine.
    ASSIGN vLine = TRIM(vLine).

/* Area and object buffer pool assignment for database D:\Proapps\sports: */
    IF NOT vLine 
    BEGINS "Area and object buffer pool assignment for database ":U THEN
    NEXT ImportHeader.

    ASSIGN vDbPath =  SUBSTRING(vLine, 53)
           vDbPath =       TRIM(vDbPath)
           vDbPath = RIGHT-TRIM(vDbPath, ":":U)
    . /* ASSIGN */
    LEAVE ImportHeader.
  END. /* REPEAT WHILE vDbPath EQ ? */

  INPUT CLOSE.

/* If header is not found: */
  IF vDbPath EQ ? THEN
  RETURN.

/* Create new vStatId: */
  ASSIGN vStatId = GetStatId(? /*Host*/, vDbPath, ? /*Time*/, ipInputFile).

  IF vStatId EQ ? THEN
  RETURN.

  INPUT FROM VALUE(ipInputFile).

  ASSIGN vLineNumber = 0.

ImportData:
  REPEAT:

    ASSIGN vLine = "":U.
    IMPORT UNFORMATTED vLine.
    ASSIGN vLine = TRIM(vLine)
           vLineNumber = vLineNumber + 1
    NO-ERROR. /* Clearing error status */

    IF vLine EQ "":U THEN
    NEXT.

    PROCESS EVENTS.

/*  Area  6: "Schema Area" - Primary Buffer Pool */
    IF vLine MATCHES "Area *: * - * Buffer Pool":U THEN
    DO TRANSACTION:
      ASSIGN i = INDEX(vLine, ":":U)
             vAreaNumber = INTEGER(SUBSTRING(vLine, 6, i - 6))
             vLine       =    TRIM(SUBSTRING(vLine, i + 2))
             i = R-INDEX(vLine, "-":U)
             vAreaName = TRIM(SUBSTRING(vLine, 1, i - 2))
             vAreaName = TRIM(vAreaName, "~"")
             vLine     = TRIM(SUBSTRING(vLine, i + 2))
             vAltBufferPool = vLine EQ "Alternate Buffer Pool":U
      NO-ERROR. /* ASSIGN */

      IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
      LEAVE ImportData.

      CREATE AreaStat.
      ASSIGN AreaStat.StatId        = vStatId
             AreaStat.AreaNumber    = vAreaNumber
             AreaStat.AreaName      = vAreaName
             AreaStat.AltBufferPool = vAltBufferPool
             vNumAreas = vNumAreas + 1
      NO-ERROR. /* ASSIGN */

      IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
      LEAVE ImportData.
    END. /* DO TRANSACTION */
    ELSE

/*
Object Enablement   Type  Object Id: Name
-----------------  ------ ---------------
Alternate           Table       2:  PUB.Customer
Default             Table       4:  PUB.Order
Default             Index      12:  CustNum (PUB.Customer)
Default              Lob        1:  CustLOB (PUB.Customer)
*/  IF vLine BEGINS "Alternate":U
    OR vLine BEGINS "Default":U THEN
    DO:
      ASSIGN vAltBufferPool = vLine BEGINS "Alternate":U
             vLine =  TRIM(SUBSTRING(vLine, 11))
             i     =           INDEX(vLine, " ":U)
/* Table       2:  PUB.Customer */
             vObjectType = SUBSTRING(vLine, 1, i - 1)
             vLine =  TRIM(SUBSTRING(vLine, i + 1))
             i     =           INDEX(vLine, ":":U)
/* 2:  PUB.Customer */
             vObjectNumber = INTEGER(SUBSTRING(vLine, 1, i - 1))
             vLine =  TRIM(SUBSTRING(vLine, i + 2))
/* CustNum (PUB.Customer) */
             vObjectName = ENTRY(1, vLine, " ":U)
/* (PUB.Customer) */
             vSecondName = TRIM(SUBSTRING(vLine, LENGTH(vObjectName) + 2))
             vSecondName = TRIM(vSecondName, "()":U)
      NO-ERROR. /* ASSIGN */

      IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
      LEAVE ImportData.

      IF vObjectNumber GT 0 THEN
      CASE vObjectType:

        WHEN "Table":U THEN
        DO TRANSACTION:
          CREATE TableStat.
          ASSIGN TableStat.StatId        = vStatId     
                 TableStat.AreaNumber    = vAreaNumber   
                 TableStat.TableNumber   = vObjectNumber
                 TableStat.TableOwner    = ENTRY(1, vObjectName, ".":U)
                 TableStat.TableName     = ENTRY(2, vObjectName, ".":U)
                 TableStat.AltBufferPool = vAltBufferPool
                 AreaStat.NumTables      = AreaStat.NumTables + 1
          . /* ASSIGN */
        END.

        WHEN "Index":U THEN
        DO TRANSACTION:
/* _Idx-num from 1 to 7 and from 994 to 1093 are used by 
    the indexes of the system tables:
*/        IF vObjectNumber GE 1   AND vObjectNumber LE 7 
          OR vObjectNumber GE 994 AND vObjectNumber LE 1093  THEN
          NEXT ImportData.

          CREATE IndexStat.
          ASSIGN IndexStat.StatId        = vStatId     
                 IndexStat.AreaNumber    = vAreaNumber   
                 IndexStat.TableOwner    = ENTRY(1, vSecondName, ".":U)
                 IndexStat.TableName     = ENTRY(2, vSecondName, ".":U)
                 IndexStat.IndexNumber   = vObjectNumber
                 IndexStat.IndexName     = vObjectName
                 IndexStat.AltBufferPool = vAltBufferPool
                 AreaStat.NumIndexes     = AreaStat.NumIndexes + 1
          . /* ASSIGN */
        END.

        WHEN "Lob":U THEN
        DO TRANSACTION:
          CREATE LobStat.
          ASSIGN LobStat.StatId        = vStatId  
                 LobStat.AreaNumber    = vAreaNumber
                 LobStat.TableOwner    = ENTRY(1, vSecondName, ".":U)
                 LobStat.TableName     = ENTRY(2, vSecondName, ".":U)
                 LobStat.LobNumber     = vObjectNumber
                 LobStat.LobName       = vObjectName
                 LobStat.AltBufferPool = vAltBufferPool
                 AreaStat.NumLobs      = AreaStat.NumLobs + 1
          . /* ASSIGN */
        END.

      END CASE. /* vObjectType */
    END. /* IF vLine BEGINS "Alternate":U OR "Default":U */
  END. /* ImportData: REPEAT */

  INPUT CLOSE.

/* If the import was interrupted due to the parsing errors: */
  DO i = 1 TO ERROR-STATUS:NUM-MESSAGES
  TRANSACTION:
    CREATE DbStatError.
    ASSIGN DbStatError.StatId       = vStatId
           DbStatError.AreaNumber   = vAreaNumber
           DbStatError.ErrorNumber  = ERROR-STATUS:GET-NUMBER(i)
           DbStatError.ErrorMessage = ERROR-STATUS:GET-MESSAGE(i)
           DbStatError.ErrorSource  = vLine
    . /* ASSIGN */
  END.

END PROCEDURE. /* ViewB2 */

/* ------------------------------------------------------------------------- */

PROCEDURE StFile:

  DEFINE INPUT PARAMETER ipInputFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vStatId      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vDbName      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDbPath      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vNumAreas    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vAreaNumber  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vAreaName    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vAreaInfo    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vRecPerBlock AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vClusterSize AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLine        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vLineNumber  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vExtension   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMD5List     AS LONGCHAR  NO-UNDO.

/* Input file will be ignored when program fails to get the key information: */
  ASSIGN vDbPath = ?.

  INPUT FROM VALUE(ipInputFile).

ImportHeader:
  REPEAT:

    ASSIGN vLine = "":U.
    IMPORT UNFORMATTED vLine.
    ASSIGN vLine = TRIM(vLine).

/* d "Area Name":7,64;512 /path/to/dbname_7.d1
   d "My Area":12,32;64 !"/dir with spaces/dbname_12.d1"
*/  IF vLine MATCHES "d *:*":U THEN
    LEAVE ImportHeader.
  END. /* ImportHeader: REPEAT */

  INPUT CLOSE.

  IF vLine MATCHES "d *:*":U THEN
  ASSIGN vLine = SUBSTRING(vLine, 3)
         vLine =      TRIM(vLine)
         i     =     INDEX(vLine, ":":U)
         vAreaName = SUBSTRING(vLine, 1, i - 1)
         vAreaName = TRIM(vAreaName, "~"")
/* Cut off the area info: d "Area Name":Num,RPB;CLS */
         i     = INDEX(vLine + " ":U, " ":U, i + 1)
         vLine = TRIM(SUBSTRING(vLine, i + 1))
         vDbPath = IF vLine BEGINS "!~""
                   THEN SUBSTRING(vLine, 3,
                          R-INDEX(vLine, "~"":U) - 3)
                   ELSE  ENTRY(1, vLine, " ":U)
         i =  NUM-ENTRIES(vDbPath, ".":U)
         vExtension = ENTRY(i, vDbPath, ".":U)
         vExtension = RIGHT-TRIM(vExtension, "0123456789":U)
  NO-ERROR. /* ASSIGN */

  IF vDbPath EQ ? THEN
  RETURN.

/* If db file extension is d1, d2 etc then vDbPath includes an extent name. */
  IF vExtension EQ "d":U THEN /* Name is either dbname.d1 or dbname_area.d1: */
  ASSIGN i = R-INDEX(vDbPath, IF vAreaName EQ "Schema Area":U
                              THEN ".":U
                              ELSE "_":U)
         vDbPath = SUBSTRING(vDbPath, 1, i - 1)
  NO-ERROR. /* ASSIGN */
  ELSE
/* If line in the .st file contains only path then add a trailing slash: */
  ASSIGN vDbPath = RIGHT-TRIM(vDbPath, "~\/":U) + "/":U.

/* Create new vStatId: */
  ASSIGN vStatId = GetStatId(? /*Host*/, vDbPath, ? /*Time*/, ipInputFile).

  IF vStatId EQ ? THEN
  RETURN.

  INPUT FROM VALUE(ipInputFile).

  ASSIGN vMD5List = "":U.
         vLineNumber = 0
  . /* ASSIGN  */

ImportData:
  REPEAT TRANSACTION:

    ASSIGN vLine = "":U.
    IMPORT UNFORMATTED vLine.
    ASSIGN vLine = TRIM(vLine)
           vLineNumber = vLineNumber + 1
    . /* ASSIGN  */

/* d "Area Name":7,64;512 /path/to/dbname_7.d1 */
    IF NOT vLine BEGINS "d ":U
    OR NOT vLine MATCHES "*:*":U THEN
    NEXT ImportData.

    ASSIGN vLine = SUBSTRING(vLine, 3)
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

    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    LEAVE ImportData.

    IF CAN-FIND(FIRST AreaStat /* INDEX AreaNumber */
                WHERE AreaStat.StatId     EQ vStatId
                  AND AreaStat.AreaNumber EQ vAreaNumber) THEN
    NEXT ImportData.

    CREATE AreaStat.
    ASSIGN AreaStat.StatId      = vStatId
           AreaStat.AreaNumber  = vAreaNumber
           AreaStat.AreaName    = vAreaName
           AreaStat.RecPerBlock = vRecPerBlock
           AreaStat.ClusterSize = vClusterSize
           vNumAreas = vNumAreas + 1
           vMD5List  = vMD5List + ":":U + STRING(vAreaNumber)
                                + ":":U + STRING(vClusterSize)
                                + ":":U + vAreaName
    . /* ASSIGN */

  END. /* ImportData: REPEAT */

  INPUT CLOSE.

/* If the import was interrupted due to the parsing errors: */
  DO i = 1 TO ERROR-STATUS:NUM-MESSAGES
  TRANSACTION:
    CREATE DbStatError.
    ASSIGN DbStatError.StatId       = vStatId
           DbStatError.AreaNumber   = vAreaNumber
           DbStatError.ErrorNumber  = ERROR-STATUS:GET-NUMBER(i)
           DbStatError.ErrorMessage = ERROR-STATUS:GET-MESSAGE(i)
           DbStatError.ErrorSource  = vLine
    . /* ASSIGN */
  END.

  IF vNumAreas GT 0 THEN
  FOR FIRST DbStat EXCLUSIVE-LOCK
      WHERE DbStat.StatId EQ vStatId
  TRANSACTION:
    ASSIGN DbStat.AreaMD5 = MD5-DIGEST(vMD5List)
           vMD5List = "":U
    . /* ASSIGN */
  END. /* FOR FIRST DbStat */

END PROCEDURE. /* StFile */

/* ------------------------------------------------------------------------- */

PROCEDURE DfFile:

/* Load a Data Definition (.df) file.
   Program will assign the object numbers in the order of the object's
   appearance imitating a load of .df file by Data Dictionary.
 */
  DEFINE INPUT PARAMETER ipInputFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vStatId       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vItem         AS CHARACTER NO-UNDO EXTENT 7.
  DEFINE VARIABLE vLineNumber   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vAreaName     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vCurrType     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vPrevType     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vAreaNumber   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTableNumber  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vIndexNumber  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLobNumber    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vObjectName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vSecondName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFoundPrimary AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE vMD5List      AS LONGCHAR  NO-UNDO.

/* Create new vStatId: */
  ASSIGN vStatId = GetStatId(? /*Host*/, ? /*DbPath*/,? /*Time*/, ipInputFile).

  IF vStatId EQ ? THEN
  RETURN.

/* The numbers of the db objects mentioned on the recent lines of .df file */
  ASSIGN vLineNumber   = 0
         vCurrType     = ?
         vPrevType     = ?
         vTableNumber  = ?
         vIndexNumber  = ?
         vAreaNumber   = 6
         vAreaName     = "Schema Area":U
         vFoundPrimary = FALSE
  . /* ASSIGN */

/* Create "Schema Area" in advance: */
  DO TRANSACTION:
    CREATE AreaStat.
    ASSIGN AreaStat.StatId     = vStatId
           AreaStat.AreaNumber = vAreaNumber
           AreaStat.AreaName   = vAreaName
    . /* ASSIGN */
  END.

  INPUT FROM VALUE(ipInputFile).

ImportData:
  REPEAT ON ENDKEY UNDO, LEAVE:

    ASSIGN vItem = "":U.
    IMPORT vItem.
    ASSIGN vLineNumber = vLineNumber + 1.

/* AREA "Area Name" */
    IF vItem[1] EQ "AREA":U
    OR vItem[1] EQ "LOB-AREA":U THEN
    DO TRANSACTION:

      ASSIGN vAreaName = vItem[2].

      FIND FIRST AreaStat
           WHERE AreaStat.StatId   EQ vStatId
             AND AreaStat.AreaName EQ vAreaName
      NO-ERROR.

      IF NOT AVAILABLE AreaStat THEN
      DO:
/* Assign the "fake" area number to provide the index uniqueness: */
        ASSIGN vAreaNumber = 7.
        FOR EACH AreaStat NO-LOCK
           WHERE AreaStat.StatId EQ vStatId
              BY AreaStat.StatId     DESCENDING
              BY AreaStat.AreaNumber DESCENDING:
          ASSIGN vAreaNumber = AreaStat.AreaNumber + 1.
          LEAVE.
        END.

        CREATE AreaStat.
        ASSIGN AreaStat.StatId     = vStatId
               AreaStat.AreaNumber = vAreaNumber
               AreaStat.AreaName   = vAreaName
        NO-ERROR. /* Clearing error status */
      END. /* IF NOT AVAILABLE AreaStat */

/* Assign the area name to the most recently created object : */
      CASE vCurrType:

        WHEN "TABLE":U THEN
        FOR LAST TableStat EXCLUSIVE-LOCK
           WHERE TableStat.StatId      EQ vStatId
             AND TableStat.TableNumber EQ vTableNumber:
          ASSIGN TableStat.AreaNumber = AreaStat.AreaNumber.
        END. /* WHEN "TABLE" */

        WHEN "INDEX":U THEN
        FOR LAST IndexStat EXCLUSIVE-LOCK
           WHERE IndexStat.StatId      EQ vStatId
             AND IndexStat.IndexNumber EQ vIndexNumber:
          ASSIGN IndexStat.AreaNumber = AreaStat.AreaNumber.
        END. /* WHEN "INDEX" */

        WHEN "LOB":U THEN
        FOR LAST LobStat EXCLUSIVE-LOCK
           WHERE LobStat.StatId    EQ vStatId
             AND LobStat.LobNumber EQ vLobNumber:
          ASSIGN LobStat.AreaNumber = AreaStat.AreaNumber.
        END. /* WHEN "LOB" */

      END CASE. /* vCurrType */

      NEXT ImportData.
    END. /* DO TRANSACTION: "AREA" or "LOB-AREA" */

/*
UPDATE TABLE "Customer"
  BUFFER-POOL "Alternate"
  
UPDATE INDEX "Name" OF "Customer"
  BUFFER-POOL "Alternate"
*/  IF vItem[1] EQ "UPDATE":U THEN
    DO TRANSACTION:

      ASSIGN vPrevType   = vCurrType
             vCurrType   = vItem[2]
             vObjectName = vItem[3]
             vSecondName = vItem[5]
             vItem       = "":U
      . /* ASSIGN */

      REPEAT WHILE vItem[1] EQ "":U:
        IMPORT vItem.
        ASSIGN vLineNumber = vLineNumber + 1.
      END.

      IF vItem[1] NE "BUFFER-POOL":U
      OR vItem[2] NE "Alternate":U THEN
      NEXT ImportData.

      CASE vCurrType:

        WHEN "TABLE":U THEN
        FOR LAST TableStat EXCLUSIVE-LOCK
           WHERE TableStat.StatId     EQ vStatId
             AND IndexStat.TableOwner EQ "PUB":U
             AND TableStat.TableName  EQ vObjectName:
          ASSIGN TableStat.AltBufferPool = TRUE.
        END. /* WHEN "TABLE" */

        WHEN "INDEX":U THEN
        FOR LAST IndexStat EXCLUSIVE-LOCK
           WHERE IndexStat.StatId     EQ vStatId
             AND IndexStat.TableOwner EQ "PUB":U
             AND IndexStat.TableName  EQ vSecondName
             AND IndexStat.IndexName  EQ vObjectName:
          ASSIGN IndexStat.AltBufferPool = TRUE.
        END. /* WHEN "INDEX" */

        WHEN "LOB":U THEN
        FOR LAST LobStat EXCLUSIVE-LOCK
           WHERE LobStat.StatId     EQ vStatId
             AND LobStat.TableOwner EQ "PUB":U
             AND LobStat.TableName  EQ vSecondName
             AND LobStat.LobName    EQ vObjectName:
          ASSIGN LobStat.AltBufferPool = TRUE.
        END. /* WHEN "LOB" */

      END CASE. /* vCurrType */

      NEXT ImportData.
    END. /* DO TRANSACTION: "AREA" or "LOB-AREA" */

    IF vCurrType EQ "INDEX":U THEN
    IF vItem[1] EQ "PRIMARY":U THEN
    DO:
      ASSIGN vFoundPrimary = TRUE.
      NEXT ImportData.
    END.
    ELSE

/* UNIQUE or WORD (index attributes): */
    IF CAN-DO("UNIQUE,WORD":U, vItem[1]) THEN
    FOR LAST IndexStat EXCLUSIVE-LOCK
       WHERE IndexStat.StatId      EQ vStatId
         AND IndexStat.IndexNumber EQ vIndexNumber
    TRANSACTION:
      ASSIGN IndexStat.IndexAttr = SUBSTRING(vItem[1], 1, 1).
      NEXT ImportData.
    END. /* IF vItem[1] EQ "UNIQUE OR "WORD" */

/*LOB-SIZE 100M
  CLOB-CODEPAGE "1251"
  CLOB-COLLATION "RUSSIAN"
  CLOB-TYPE 2
*/  IF vCurrType EQ "LOB":U THEN
    FOR LAST LobStat EXCLUSIVE-LOCK
       WHERE LobStat.StatId    EQ vStatId
         AND LobStat.LobNumber EQ vLobNumber
    TRANSACTION:
      CASE vItem[1]:
        WHEN "CLOB-TYPE":U      THEN ASSIGN LobStat.LobAttr      = vItem[2].
        WHEN "LOB-SIZE":U       THEN ASSIGN LobStat.LobSize      = vItem[2].
        WHEN "CASE-SENSITIVE":U THEN ASSIGN LobStat.LobCase      = vItem[1].
        WHEN "CLOB-CODEPAGE":U  THEN ASSIGN LobStat.LobCharset   = vItem[2].
        WHEN "CLOB-COLLATION":U THEN ASSIGN LobStat.LobCollation = vItem[2].
      END. /* WHEN "LOB" */
    END. /* DO TRANSACTION */

/* ADD SEQUENCE "Sequence"
   ADD TABLE "Table"
   ADD FIELD "Field" OF "Table" AS blob
   ADD INDEX "Index" ON "Table"
*/
    IF vItem[1] NE "ADD":U THEN
    NEXT ImportData.

    ASSIGN vPrevType   = vCurrType
           vCurrType   = vItem[2]
           vObjectName = vItem[3]
           vSecondName = vItem[5]
    . /* ASSIGN */

    IF vCurrType EQ "FIELD":U THEN
    IF NOT CAN-DO("BLOB,CLOB":U, vItem[7]) THEN
    NEXT ImportData.
    ELSE
    DO TRANSACTION:

/* ADD FIELD "Field" OF "Table" AS blob/clob */

/* _Field._Fld-stlen begins from 1: */
      ASSIGN vCurrType  = "LOB":U
             vLobNumber = 1
      . /* ASSIGN */
      FOR EACH LobStat NO-LOCK /* INDEX DetailId */
         WHERE LobStat.StatId EQ vStatId
            BY LobStat.StatId    DESCENDING
            BY LobStat.LobNumber DESCENDING:
        ASSIGN vLobNumber = LobStat.LobNumber + 1.
        LEAVE.
      END.

      CREATE LobStat.
      ASSIGN LobStat.StatId     = vStatId
             LobStat.AreaNumber = 6 /* will be updated by AREA tag  */
             LobStat.LobNumber  = vLobNumber
             LobStat.TableOwner = "PUB":U
             LobStat.TableName  = vSecondName
             LobStat.LobName    = vObjectName
             LobStat.LobType    = vItem[7]
      . /* ASSIGN */

      NEXT ImportData.
    END. /* "ADD FIELD" */

/* Does the previous table have the "default" index?
   Table can have the word indexes but they can't be PRIMARY.  */
    IF  vCurrType NE "INDEX":U
    AND vPrevType EQ "INDEX":U
    AND NOT vFoundPrimary THEN
    FOR LAST TableStat NO-LOCK
       WHERE TableStat.StatId      EQ vStatId
         AND TableStat.TableNumber EQ vTableNumber
    TRANSACTION:

/* _Index._Idx-num from 1 to 7 are used by the indexes of the system tables: */
      ASSIGN vIndexNumber = 8.
      FOR EACH IndexStat NO-LOCK /* INDEX DetailId */
         WHERE IndexStat.StatId EQ vStatId
            BY IndexStat.StatId      DESCENDING
            BY IndexStat.IndexNumber DESCENDING:
        ASSIGN vIndexNumber = IndexStat.IndexNumber + 1.
        LEAVE.
      END.

      IF vIndexNumber GE 994 AND vIndexNumber LE 1093 THEN
      ASSIGN vIndexNumber = 1094.

      CREATE IndexStat.
      ASSIGN IndexStat.StatId      = vStatId
             IndexStat.IndexNumber = vIndexNumber
             IndexStat.TableOwner  = "PUB":U
             IndexStat.TableName   = TableStat.TableName
             IndexStat.IndexName   = "default"
             IndexStat.AreaNumber  = 6
             IndexStat.IndexAttr   = "":U
      . /* ASSIGN */

    END. /* the "default" index */

    CASE vCurrType:

      WHEN "SEQUENCE":U THEN
      NEXT ImportData.

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
*/      ASSIGN vIndexNumber = 8.
        FOR EACH IndexStat NO-LOCK /* INDEX DetailId */
           WHERE IndexStat.StatId EQ vStatId
              BY IndexStat.StatId       DESCENDING
              BY IndexStat.IndexNumber  DESCENDING:
          ASSIGN vIndexNumber = IndexStat.IndexNumber + 1.
          LEAVE.
        END.
/*
_Idx-num from 994 to 1093 are used by the indexes of the system tables.
994 _View._View-Name   (verified for Progress versions from 10.2B through 11.4)
995 _View-Col._View-Col
...
1092 _Collation._Collation-Seq
1093 _Word-rule._Wr-Number
*/      IF vIndexNumber GE 994 AND vIndexNumber LE 1093 THEN
        ASSIGN vIndexNumber = 1094.

/* ADD INDEX "Index" ON "Table" */
        CREATE IndexStat.
        ASSIGN IndexStat.StatId      = vStatId
               IndexStat.AreaNumber  = 6 /* will be updated by AREA tag  */
               IndexStat.IndexNumber = vIndexNumber
               IndexStat.TableOwner  = "PUB":U
               IndexStat.TableName   = vSecondName
               IndexStat.IndexName   = vObjectName
               IndexStat.IndexAttr   = "":U
        . /* ASSIGN */
      END. /* WHEN "INDEX" */

      WHEN "TABLE":U THEN
      DO TRANSACTION:
/* _File._File-Number begins from 1. */
        ASSIGN vTableNumber = 1.
        FOR EACH TableStat NO-LOCK /* INDEX DetailId */
           WHERE TableStat.StatId EQ vStatId
              BY TableStat.StatId      DESCENDING
              BY TableStat.TableNumber DESCENDING:
          ASSIGN vTableNumber = TableStat.TableNumber + 1.
          LEAVE.
        END.

/* ADD TABLE "Table" */
        CREATE TableStat.
        ASSIGN TableStat.StatId      = vStatId
               TableStat.AreaNumber  = 6 /* will be updated by AREA tag  */
               TableStat.TableOwner  = "PUB":U
               TableStat.TableNumber = vTableNumber
               TableStat.TableName   = vObjectName
               vFoundPrimary         = FALSE
        . /* ASSIGN */
      END. /* WHEN "TABLE" */
    END CASE. /* vCurrType */

  END. /* ImportData: REPEAT */

  INPUT CLOSE.

  FOR EACH AreaStat EXCLUSIVE-LOCK
     WHERE AreaStat.StatId EQ vStatId:

    ACCUMULATE "Area":U (COUNT).

    ASSIGN vMD5List = "".

    FOR EACH TableStat OF AreaStat NO-LOCK
       WHERE TableStat.StatId EQ vStatId:
      ACCUMULATE "Table":U (COUNT).
      ASSIGN vMD5List = vMD5List + {&MD5Sep} + TableStat.TableOwner
                                 + {&MD5Sep} + TableStat.TableName
      . /*ASSIGN */
    END.
    
    FOR EACH IndexStat OF AreaStat NO-LOCK
       WHERE IndexStat.StatId EQ vStatId:
      ACCUMULATE "Index":U (COUNT).
      ASSIGN vMD5List = vMD5List + {&MD5Sep} + IndexStat.TableOwner
                                 + {&MD5Sep} + IndexStat.TableName
                                 + {&MD5Sep} + IndexStat.IndexName
      . /*ASSIGN */
    END.
    
    FOR EACH LobStat OF AreaStat NO-LOCK
       WHERE LobStat.StatId EQ vStatId:
      ACCUMULATE "LOB":U (COUNT).
      ASSIGN vMD5List = vMD5List + {&MD5Sep} + LobStat.TableOwner
                                 + {&MD5Sep} + LobStat.TableName
                                 + {&MD5Sep} + LobStat.LobName
      . /*ASSIGN */
    END.

    ASSIGN AreaStat.NumTables  = ACCUMULATE COUNT "Table":U
           AreaStat.NumIndexes = ACCUMULATE COUNT "Index":U
           AreaStat.NumLobs    = ACCUMULATE COUNT "LOB":U
           AreaStat.SchemaMD5  = MD5-DIGEST(vMD5List)
    . /* ASSIGN */
  END.

  ASSIGN vMD5List = "".

/* If the input file has the incorrect contents: */

END PROCEDURE. /* DfFile */

/* ------------------------------------------------------------------------- */

FUNCTION GetAreaNumber RETURN INTEGER(ipStatId AS INTEGER,
                                      ipText   AS CHARACTER):

  DEFINE VARIABLE vText       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vAreaName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vAreaNumber AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i           AS INTEGER   NO-UNDO.

/* msg 10097: AREA "%s" : %d  BLOCK ANALYSIS */
  IF ipText MATCHES "AREA *:* BLOCK ANALYSIS":U THEN
  ASSIGN i = LENGTH(ipText)
         vText = SUBSTRING(ipText, 6, i - 20)
  . /* ASSIGN */
  ELSE

/* msg 14179: CHAIN ANALYSIS FOR AREA "%s" : %l
   msg 14212: RECORD BLOCK SUMMARY FOR AREA "%s" : %l
   msg 14225: INDEX BLOCK SUMMARY FOR AREA "%s" : %l
   msg 16656: Summary for AREA "%s": %l
   msg 16666: SUMMARY FOR AREA "%s": %l
*/  
  IF ipText MATCHES "* FOR AREA *:*":U THEN
  ASSIGN i         = INDEX(ipText, " FOR AREA ":U)
         vText     = SUBSTRING(ipText, i + 10)
         vText     = TRIM(vText)
  . /* ASSIGN */
  ELSE

/* If ipText does not match any patten above then: */
  DO TRANSACTION:
    CREATE DbStatError.
    ASSIGN DbStatError.StatId       = ipStatId
           DbStatError.AreaNumber   = ?
           DbStatError.ErrorNumber  = -1
           DbStatError.ErrorSource  = ipText
           DbStatError.ErrorMessage = 
          "Error: GetAreaNumber function was called for a wrong line."
    . /* ASSIGN */
    RETURN ?.
  END. /* DO TRANSACTION */
      
/* vText = "%s" : %l */
  ASSIGN i           = R-INDEX(vText, ":":U)
         vAreaName   = SUBSTRING(vText, 1, i - 1)
         vAreaName   = TRIM(vAreaName, "~" ":U)
         vText       = SUBSTRING(vText, i + 1)
         vText       = TRIM(vText)
         vAreaNumber = INTEGER(vText)
  NO-ERROR. /* ASSIGN */

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ?.

  FIND FIRST AreaStat NO-LOCK
       WHERE AreaStat.StatId     EQ ipStatId
         AND AreaStat.AreaNumber EQ vAreaNumber
  NO-ERROR.

  IF AVAILABLE AreaStat THEN
  IF AreaStat.AreaName EQ vAreaName THEN
  RETURN vAreaNumber.
  ELSE
  DO TRANSACTION:
    CREATE DbStatError.
    ASSIGN DbStatError.StatId       = ipStatId
           DbStatError.AreaNumber   = vAreaNumber
           DbStatError.ErrorNumber  = -2
           DbStatError.ErrorSource  = ipText
           DbStatError.ErrorMessage = SUBSTITUTE(
         'Warning: Name mistmatch for the same area number (&1): "&2" vs "&2"',
                                      /*&1*/ STRING(vAreaNumber), 
                                      /*&2*/ vAreaName,
                                      /*&3*/ AreaStat.AreaName)
    . /* ASSIGN */
    RETURN vAreaNumber.
  END.

/*IF NOT AVAILABLE AreaStat THEN */
  DO TRANSACTION:
    CREATE AreaStat.
    ASSIGN AreaStat.StatId     = ipStatId
           AreaStat.AreaNumber = vAreaNumber
           AreaStat.AreaName   = vAreaName
    NO-ERROR. /* Clearing error status */
  END. /* DO TRANSACTION */

/* If dbanalys was done for the whole database (not just for one area)
   then create a "pseudo" area to accumualte the totals per database:
*/
  IF vAreaNumber EQ 6 AND CAN-FIND(
     FIRST AreaStat
     WHERE AreaStat.StatId     EQ ipStatId  /* if there are at least     */
       AND AreaStat.AreaNumber EQ 1) THEN   /* two areas (1 and 6) then: */
  DO TRANSACTION:
    CREATE AreaStat.
    ASSIGN AreaStat.StatId     = ipStatId
           AreaStat.AreaNumber = 0
           AreaStat.AreaName   = "Total per database"
    . /* ASSIGN */
  END. /* DO TRANSACTION */

  RETURN vAreaNumber.

END FUNCTION. /* GetAreaNumber */

/* ------------------------------------------------------------------------- */

PROCEDURE GetBlockCount:
  
  DEFINE INPUT PARAMETER ipStatId     AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipAreaNumber AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipText       AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vCount  AS INT64     NO-UNDO.
  DEFINE VARIABLE vExpect AS INT64     NO-UNDO.
  DEFINE VARIABLE vType   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vItem   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i       AS INTEGER   NO-UNDO.

  DEFINE BUFFER bAreaStat FOR AreaStat.
/*
msg 14181: Current high water mark: %J%r
*/
  IF ipText BEGINS "Current high water mark: ":U THEN
  ASSIGN i       = NUM-ENTRIES(ipText, " ":U)
         vItem   = ENTRY(i, ipText, " ":U)
         vCount  = INT64(vItem)
         vType   = "HWM":U
         vExpect = ?
  NO-ERROR. /* ASSIGN */
  ELSE
/*
msg 10097: AREA "%s" : %d  BLOCK ANALYSIS

For the whole  database: -------------------

msg 10130: %J block(s) found in the area.%r | Total blocks
msg 14182: %J free block(s) found in the area
msg 14183: %J record block(s) found in the area
msg 14184: %J index block(s) found in the area
msg 14185: %J empty block(s) found in the area
msg 14186: %J object block(s) found in the area       | bk_type 12, OBJECT / OBJECTBLK
msg 14187: %J cluster list block(s) found in the area | bk_type 16, CLISTBLK / CLUSTER_MAP_BLK
msg 14188: %J object list block(s) found in the area  | bk_type 14, OBJLISTBLK / OBJECT_LIST_BLK
msg 14189: %J cluster map block(s) found in the area  | bk_type 15, Cluster Map Block / CLUSTER MAPBLK

For a single area: -------------------------

msg 10130: %J block(s) found in the area.%r

msg  3906: BLOCK ANALYSIS
           DATABASE BLOCK ANALYSIS:

msg  3920: %J master block(s) found in the database.%r             | bk_type  1
msg  6706: %J area block(s) found in the database.                 | bk_type  9
msg  6708: %J control block(s) found in the database.              | bk_type 13
msg  6710: %J object block(s) found in the database.               | bk_type 12
msg 14197: %J cluster list block(s) found in the database.%r       | bk_type 16
msg 14198: %J cluster allocation block(s) found in the database.%r | bk_type 15
msg 14199: %J object block(s) found in the database.%r             | bk_type 12
msg 14200: %J object list block(s) found in the database.%r        | bk_type 14
msg 14201: %J object allocation block(s) found in the database.%r  | bk_type 17
msg 14202: %J row allocation block(s) found in the storage area.%r | bk_type 18

For a single area Type 1: ------------------

msg  9394: %J master block(s) found in the storage area.%r | Only in "Schema Area"
msg  9395: %J area block(s) found in the storage area.
msg  9396: %J control block(s) found in the storage area.  | Only in "Control Area"
msg  9397: %J object block(s) found in the storage area.

For a single area Type 2: ------------------

msg  9395: %J area block(s) found in the storage area.                 | bk_type  9
msg 14191: %J cluster list block(s) found in the storage area.%r       | bk_type 16
msg 14192: %J cluster allocation block(s) found in the storage area.%r | bk_type 15
msg 14193: %J object block(s) found in the storage area.%r             | bk_type 12
msg 14194: %J object list block(s) found in the storage area.%r        | bk_type 14
msg 14195: %J object allocation block(s) found in the storage area.%r  | bk_type 17
msg 14196: %J row allocation block(s) found in the storage area.%r     | bk_type 18

msg 2314: DATABASE SUMMARY

msg  3930: %J free block(s) found in the database.%r
msg  3931: %J index table block(s) found in the database.%r
msg  3932: %J sequence block(s) found in the database.%r
msg  3933: %J empty block(s) found in the database.%r
msg  3934: %j unknown type block(s) found in the database.%r (3934)
msg  3935: %j total blocks found in the database.%r

msg  6129: 5)   RECORD BLOCK SUMMARY

msg  3921: %J RM block(s) found in the database.
msg  9398: %J RM block(s) found in the storage area.

msg  3925: INDEX BLOCK SUMMARY

msg  3923: %J index block(s) found in the database.
msg  9399: %J index block(s) found in the storage area.
*/
  ASSIGN vItem   = ENTRY(1, ipText, " ":U)
         vCount  = INT64(vItem)
/* index block(s) found in the storage area. */
         vItem   = SUBSTRING(ipText, LENGTH(vItem) + 2)
         i       = INDEX(vItem, "block":U)
         vType   = TRIM(SUBSTRING(vItem, 1, i - 1))
         vExpect = ?
         ipAreaNumber = 0 WHEN ipText MATCHES "* found in the database*":U
  NO-ERROR. /* ASSIGN */

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN.
/*
- 1 master block(s) found in the database.
- 7 area block(s) found in the database.
- 1 control block(s) found in the database.
+ 2 object block(s) found in the database.
- 17 cluster list block(s) found in the database.
- 17 cluster allocation block(s) found in the database.
+ 17 object block(s) found in the database.
+ 7 object list block(s) found in the database.
- 17 object allocation block(s) found in the database.
+ 4539 free block(s) found in the database.
- 0 index table block(s) found in the database.
- 1 sequence block(s) found in the database.
+ 93084 empty block(s) found in the database.
+ 127143 total blocks found in the database.

- 1 area block(s) found in the storage area.
- 4 cluster list block(s) found in the storage area.
- 4 cluster allocation block(s) found in the storage area.
+ 4 object block(s) found in the storage area.
+ 1 object list block(s) found in the storage area.
- 4 object allocation block(s) found in the storage area.
- 0 row allocation block(s) found in the storage area.
+ 25500 RM block(s) found in the storage area.
+ 1 free block(s) found in the storage area.
+ 75424 empty block(s) found in the storage area.
+ 100943 total blocks found in the storage area.

+    1 free block(s) found in the area
+    25500 record block(s) found in the area
+    0 index block(s) found in the area
+    75424 empty block(s) found in the area
+    4 object block(s) found in the area
-    4 cluster list block(s) found in the area
+    1 object list block(s) found in the area
-    4 cluster map block(s) found in the area
*/

  FOR FIRST AreaStat EXCLUSIVE-LOCK
      WHERE AreaStat.StatId     EQ ipStatId
        AND AreaStat.AreaNumber EQ ipAreaNumber
  TRANSACTION:
    CASE vType:
      WHEN "HWM":U            THEN ASSIGN AreaStat.HighBlock        = vCount.
      WHEN "":U OR
      WHEN "total":U          THEN ASSIGN AreaStat.TotalBlocks      = vCount.
      WHEN "RM":U OR
      WHEN "record":U         THEN ASSIGN AreaStat.DataBlocks       = vCount.
      WHEN "index":U          THEN ASSIGN AreaStat.IndexBlocks      = vCount.
      WHEN "free":U           THEN ASSIGN AreaStat.FreeBlocks       = vCount.
      WHEN "empty":U          THEN ASSIGN AreaStat.EmptyBlocks      = vCount.
      WHEN "object list":U    THEN ASSIGN AreaStat.ObjectListBlocks = vCount.
      WHEN "object":U         THEN ASSIGN AreaStat.ObjectBlocks     = vCount.
      WHEN "cluster list":U   THEN ASSIGN AreaStat.ObjectBlocks     = vCount.
/* "cluster list" count comes before "cluster allocation" (aka "cluster map"),
   "object allocation" and "object" blocks.
   All of them should equate each other.
   Only "object" blocks are implemeneted and used.
*/

/* Reported by dbanalys or might be reported but should be ignored: */
      WHEN "object allocation":U  OR
      WHEN "cluster allocation":U OR
      WHEN "cluster map":U    THEN ASSIGN vExpect = AreaStat.ObjectBlocks.
      WHEN "master":U         THEN ASSIGN vExpect =
                       IF ipAreaNumber EQ 0 OR ipAreaNumber EQ 6 THEN 1 ELSE 0.
      WHEN "sequence":U       THEN ASSIGN vExpect =
                       IF ipAreaNumber EQ 0 OR ipAreaNumber EQ 6 THEN 1 ELSE 0.
      WHEN "control":U        THEN ASSIGN vExpect =
                       IF ipAreaNumber EQ 0 OR ipAreaNumber EQ 1 THEN 1 ELSE 0.
      WHEN "row allocation":U THEN ASSIGN vExpect = 0.
      WHEN "index table":U    THEN ASSIGN vExpect = 0.
      WHEN "area":U THEN /* 1 area block in each storage area type II */
      IF ipAreaNumber NE 0 THEN
      ASSIGN vExpect = IF AreaStat.ObjectListBlocks EQ 0 THEN 0 ELSE 1.
      ELSE
      DO:
/* Expect the number of the  type II storage areas: */
        ASSIGN vExpect = 0.
        FOR EACH bAreaStat NO-LOCK
           WHERE bAreaStat.StatId           EQ ipStatId
             AND bAreaStat.AreaNumber       GT 6
             AND bAreaStat.ObjectListBlocks GT 0:
          ASSIGN vExpect = vExpect + 1.
        END.
      END.

      OTHERWISE
/*
msg  3934: %j unknown type block(s) found in the database.%r (3934)
*/    DO:
        CREATE DbStatError.
        ASSIGN DbStatError.StatId       = ipStatId
               DbStatError.AreaNumber   = ipAreaNumber
               DbStatError.ErrorNumber  = 3934
               DbStatError.ErrorSource  = gvLine
               DbStatError.ErrorMessage = "Warning: Unexpected block type."
        . /* ASSIGN */
        RETURN.
      END. /* OTHERWISE */
    END CASE.

    IF vExpect NE ? THEN
    IF vExpect NE vCount THEN
    DO:
      CREATE DbStatError.
      ASSIGN DbStatError.StatId       = ipStatId
             DbStatError.AreaNumber   = ipAreaNumber
             DbStatError.ErrorNumber  = -4
             DbStatError.ErrorSource  = gvLine
             DbStatError.ErrorMessage =
                    SUBSTITUTE("Warning: Expected &1 blocks.", STRING(vExpect))
      . /* ASSIGN */

    END. /* IF vCount NE vExpect */
  END. /* FOR FIRST AreaStat */

END PROCEDURE. /* GetBlockCount */

/* ------------------------------------------------------------------------- */

PROCEDURE GetChainList:

  DEFINE INPUT PARAMETER ipStatId      AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipAreaNumber  AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipObjectId    AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipObjectType  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipChainType   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipObjectGroup AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipListOffset  AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vEndLine    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vEndOffset  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vListTitle  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vListTime   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vListBlocks AS INT64     NO-UNDO.
  DEFINE VARIABLE vBlockCount AS INT64     NO-UNDO.
  DEFINE VARIABLE vCurrDbkey  AS INT64     NO-UNDO.
  DEFINE VARIABLE vPrevDbkey  AS INT64     NO-UNDO.
  DEFINE VARIABLE vSeekPath   AS INT64     NO-UNDO.
  DEFINE VARIABLE vMinDbkey   AS INT64     NO-UNDO.
  DEFINE VARIABLE vMaxDbkey   AS INT64     NO-UNDO.
  DEFINE VARIABLE vText       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vItem1      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vItem2      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vItem3      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vItem4      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i           AS INTEGER   NO-UNDO.

  IF ipListOffset EQ ? THEN
  RETURN.

/* -------------- LIST OF CHAIN BLOCKS: Body --------------------------------*/
/*
msg #14157: LIST OF FREE CLUSTER CHAIN BLOCKS
msg # 3880: LIST OF FREE CHAIN BLOCKS
msg # 3897: LIST OF RM CHAIN BLOCKS
msg # 7283: LIST OF INDEX DELETE CHAIN BLOCKS

  LIST OF FREE CLUSTER CHAIN BLOCKS
                next
        dbkey   free
        1546784         1546848
        1546848         0
2 cluster(s) found in the free cluster chain.

          LIST OF FREE CHAIN BLOCKS
                next
        dbkey   free
        7       0
1 block(s) found in the free chain of Master object 0

          LIST OF RM CHAIN BLOCKS
                free    # free
        dbkey   space   slots   hold
        768     7948    60      1
        832     8124    64      0
        896     8124    64      0
        960     8108    64      0
4 block(s) found in the RM chain of Table object 1

          LIST OF INDEX DELETE CHAIN BLOCKS
                next
        dbkey   Block
        177015616       0
1 block(s) found in the Index Delete chain of Index object 1107
*/

  ASSIGN vEndLine   = gvLine
         vEndOffset = SEEK(INPUT)
         vListTitle = SUBSTITUTE("LIST OF &1 BLOCKS":U, ipChainType)
  . /* ASSIGN */

  /* Return to a few bytes before the "LIST OF * CHAIN BLOCKS" header: */
  SEEK INPUT TO MAX(ipListOffset - 80, 0).

FindHeader:
  REPEAT WHILE SEEK(INPUT) LT vEndOffset:
    IF GetText() EQ vListTitle THEN
    LEAVE FindHeader.
  END. /* FindHeader: REPEAT */
  
  IF SEEK(INPUT) GE vEndOffset THEN
  FOR FIRST ChainStat NO-LOCK /* INDEX ObjectNumber */
      WHERE ChainStat.StatId       EQ ipStatId
        AND ChainStat.ChainType    EQ ipChainType
        AND ChainStat.ObjectType   EQ ipObjectType
        AND ChainStat.ObjectNumber EQ ipObjectId
        AND ChainStat.ObjectGroup  EQ ipObjectGroup
  TRANSACTION:
    CREATE DbStatError.
    ASSIGN DbStatError.StatId       = ipStatId
           DbStatError.AreaNumber   = ChainStat.AreaNumber
           DbStatError.ErrorNumber  = -5
           DbStatError.ErrorSource  = vEndLine
           DbStatError.ErrorMessage = SUBSTITUTE(
               "Error: Failed to find ~"&1~" at offset &2. Current offset: &3",
                                      /*&1*/ vListTitle,
                                      /*&2*/ STRING(ipListOffset),
                                      /*&3*/ STRING(vEndOffset))
    . /* ASSIGN */
    RETURN.
  END. /* DO TRANSACTION */

/* Time of "LIST OF * BLOCKS" header: */
  ASSIGN vListTime = gvTime.

/* All LIST headers constist of two lines but let's identify the beginning of
   the list as a first line with the integer value:
*/
SkipHeader:
  REPEAT WHILE SEEK(INPUT) LT vEndOffset:
    ASSIGN vText  = GetText()
           vItem1 = ENTRY(1, vText, " ":U)
    . /* ASSIGN */
    IF TRIM(vItem1, "0123456789":U) EQ "":U THEN
    LEAVE SkipHeader.
  END. /* SkipHeader: REPEAT */
  
/* The messages in the .lg file may contain the head and bottom of the list
   but not the body of the list. Ignore such situations:
*/
  IF SEEK(INPUT) GE vEndOffset THEN
  RETURN.

  ASSIGN vCurrDbkey  = INT64(vItem1)
         vPrevDbkey  = vCurrDbkey
         vMinDbkey   = vCurrDbkey
         vMaxDbkey   = vCurrDbkey
         vListBlocks = 0
         vSeekPath   = 0
  . /* ASSIGN */

ImportChain:
  REPEAT WHILE SEEK(INPUT) LT vEndOffset
  TRANSACTION:

    ASSIGN vText  = TRIM(SUBSTRING(vText, LENGTH(vItem1) + 2))
           vItem2 = ENTRY(1, vText, " ":U)
    . /* ASSIGN */

    IF TRIM(vItem2, "0123456789":U) NE "":U THEN
    LEAVE ImportChain.

    CREATE ChainBlock.
    ASSIGN ChainBlock.StatId     = ipStatId
           ChainBlock.ChainOrder = ChainStat.ChainOrder
           ChainBlock.BlockOrder = vListBlocks
           ChainBlock.BlockDbkey = vCurrDbkey
           vListBlocks = vListBlocks + 1
           vSeekPath   = vSeekPath + ABS(vCurrDbkey - vPrevDbkey)
           vMaxDbkey   = MAX(vCurrDbkey, vMaxDbkey)
           vMinDbkey   = MIN(vCurrDbkey, vMinDbkey)
      . /* ASSIGN */
    . /* ASSIGN */

/*        LIST OF RM CHAIN BLOCKS
                free    # free
        dbkey   space   slots   hold
        768     7948    60      1
        832     8124    64      0
        896     8124    64      0
        960     8108    64      0
4 block(s) found in the RM chain of Table object 1
*/  IF ipChainType EQ "RM chain":U THEN
    ASSIGN vText  = TRIM(SUBSTRING(vText, LENGTH(vItem2) + 2))
           vItem3 = ENTRY(1, vText, " ":U)
           vText  = TRIM(SUBSTRING(vText, LENGTH(vItem3) + 2))
           vItem4 = ENTRY(1, vText, " ":U)
/* Attributes of RM block: */
           ChainBlock.BlockSpace   = INTEGER(vItem2)
           ChainBlock.BlockEntries = INTEGER(vItem3)
           ChainBlock.BlockAttr1   = INTEGER(vItem4)
           ChainBlock.BlockAttr2   = 0
    NO-ERROR. /* ASSIGN */

    ASSIGN vText  = GetText()
           vItem1 = ENTRY(1, vText, " ":U)
           vPrevDbkey = vCurrDbkey
           vCurrDbkey = INT64(vItem1)
    NO-ERROR. /* ASSIGN */

/* In case if the real list is the shorter than the bottom line said: */
    IF ERROR-STATUS:NUM-MESSAGES GT 0 OR vText EQ ? THEN
    LEAVE ImportChain.

  END. /* ImportChain: REPEAT */

  IF  SEEK(INPUT) NE ?
  AND SEEK(INPUT) NE vEndOffset THEN
  SEEK INPUT TO vEndOffset.

  FOR FIRST ChainStat EXCLUSIVE-LOCK /* INDEX ObjectNumber */
      WHERE ChainStat.StatId       EQ ipStatId
        AND ChainStat.AreaNumber   EQ ipAreaNumber
        AND ChainStat.ChainType    EQ ipChainType
        AND ChainStat.ObjectType   EQ ipObjectType
        AND ChainStat.ObjectNumber EQ ipObjectId
        AND ChainStat.ObjectGroup  EQ ipObjectGroup
  TRANSACTION:
    ASSIGN ChainStat.SeekPath = vSeekPath
           ChainStat.MinDbkey = vMinDbkey
           ChainStat.MaxDbkey = vMaxDbkey
           ChainStat.BgnTime  = String2DateTime(vListTime) WHEN vListTime NE ?
           ChainStat.EndTime  = String2DateTime(gvTime)    WHEN gvTime    NE ?
    . /* ASSIGN */

    IF vListBlocks EQ ChainStat.ChainBlocks THEN
    RETURN.

    CREATE DbStatError.
    ASSIGN DbStatError.StatId       = ipStatId
           DbStatError.AreaNumber   = ChainStat.AreaNumber
           DbStatError.ErrorNumber  = -6
           DbStatError.ErrorSource  = vEndLine
           DbStatError.ErrorMessage = SUBSTITUTE(
                               "Block count mistmatch: found &1, expected &2.",
                               /*&1*/ STRING(vListBlocks),
                               /*&2*/ STRING(ChainStat.ChainBlocks))
    . /* ASSIGN */
  END. /* FOR FIRST ChainStat */

END PROCEDURE. /* GetChainList */

/* ------------------------------------------------------------------------- */

PROCEDURE GetChainStatV10:

  DEFINE INPUT PARAMETER ipStatId     AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipAreaNumber AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipText       AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipListOffset AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vChainOrder AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vEndOffset  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vObjectId   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vObjectType AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vChainType  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vBlockCount AS INT64     NO-UNDO.
  DEFINE VARIABLE vItem       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
/*
msg 14155: %J block(s) found in the free chain of %s object %i
msg 14172: %J block(s) found in the RM chain of %s object %i
msg 14176: %J block(s) found in the Index Delete chain of %s object %i
*/
  IF ipText MATCHES "* block(s) found in the * chain of * object *":U THEN
  ASSIGN vItem       = ENTRY(1, ipText, " ":U)
         vBlockCount = INT64(vItem)
         i           = INDEX(ipText, " found in the ":U)
/* free chain of %s object %i */
         vItem       = TRIM(SUBSTRING(ipText, i + 14))
         i           = INDEX(vItem, " chain of ":U)
         vChainType  = TRIM(SUBSTRING(vItem, 1, i + 5))
/* %s object %i */
         vItem       = TRIM(SUBSTRING(  vItem, i + 10))
         vObjectType =         ENTRY(1, vItem, " ":U)
         vObjectId   = INTEGER(ENTRY(3, vItem, " ":U))
  NO-ERROR. /* ASSIGN */
  ELSE
/*
msg  3891: %J block(s) found in the free chain.%r
msg  3905: %J block(s) found in the RM chain.%r
msg  7270: %J block(s) found in the Index Delete chain.%r
*/
  IF ipText MATCHES "* block(s) found in the * chain*":U THEN
  ASSIGN vItem       = ENTRY(1, ipText, " ":U)
         vBlockCount = INT64(vItem)
         i           = INDEX(ipText, " found in the ":U)
/* free chain of %s object %i */
         vItem       = TRIM(SUBSTRING(ipText, i + 14))
         i           = INDEX(vItem, " chain":U)
         vChainType  = TRIM(SUBSTRING(vItem, 1, i + 5))
         vObjectType = "area":U /* objectType 8 as owner of ACO Object Block */
         vObjectId   = 0
  NO-ERROR. /* ASSIGN */
  ELSE

/*
msg #14161: %J cluster(s) found in the free cluster chain.
*/
  IF ipText MATCHES "* cluster(s) found in the free cluster chain*":U THEN
  ASSIGN vItem       = ENTRY(1, ipText, " ":U)
         vBlockCount = INT64(ENTRY(1, vItem, " ":U))
         vChainType  = "free cluster chain":U
         vObjectType = "area":U
         vObjectId   = 0
  NO-ERROR. /* ASSIGN */
  ELSE

/* If ipText does not match any patten above then: */
  DO TRANSACTION:
    CREATE DbStatError.
    ASSIGN DbStatError.StatId       = ipStatId
           DbStatError.AreaNumber   = ipAreaNumber
           DbStatError.ErrorNumber  = -7
           DbStatError.ErrorSource  = gvLine
           DbStatError.ErrorMessage = 
          "Error: GetChainStatV10 procedure was called for a wrong line."
    . /* ASSIGN */
    RETURN.
  END. /* DO TRANSACTION */
      
  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN.

/* Free chains of Table/Blob objects should be empty: */
  IF vChainType EQ "free chain":U
  AND LOOKUP(vObjectType, "Table,Blob":U) GT 0 THEN
  IF vBlockCount EQ 0 THEN
  RETURN.
  ELSE
  DO TRANSACTION:
    CREATE DbStatError.
    ASSIGN DbStatError.StatId       = ipStatId
           DbStatError.AreaNumber   = ipAreaNumber
           DbStatError.ErrorNumber  = -13
           DbStatError.ErrorSource  = gvLine
           DbStatError.ErrorMessage = 
     SUBSTITUTE("Warning: a non-empty free chain is unexpected for &1 object.",
                                                            /*&1*/ vObjectType)
    . /* ASSIGN */
/* Nevertheless create ChainStat: */
  END. /* DO TRANSACTION */

  DO TRANSACTION:

/* In case if the statistics report was previously loaded: */
    FIND FIRST ChainStat EXCLUSIVE-LOCK /* INDEX ObjectNumber */
         WHERE ChainStat.StatId       EQ ipStatId
           AND ChainStat.ChainType    EQ vChainType
           AND ChainStat.ObjectType   EQ vObjectType
           AND ChainStat.ObjectNumber EQ vObjectId
           AND ChainStat.ObjectGroup  EQ "":U
           AND ChainStat.AreaNumber   EQ ipAreaNumber
    NO-ERROR.       
           
    IF NOT AVAILABLE ChainStat THEN
    DO:
/* Sequantial chain number in the current dbanalys: */
      ASSIGN vChainOrder = 0.

      FOR EACH ChainStat NO-LOCK
         WHERE ChainStat.StatId EQ ipStatId
            BY ChainStat.StatId     DESCENDING
            BY ChainStat.ChainOrder DESCENDING:
        ASSIGN vChainOrder = ChainStat.ChainOrder + 1.
        LEAVE.
      END.

      CREATE ChainStat.
      ASSIGN ChainStat.ChainOrder   = vChainOrder WHEN NEW ChainStat
             ChainStat.StatId       = ipStatId
             ChainStat.AreaNumber   = ipAreaNumber
             ChainStat.ChainType    = vChainType
             ChainStat.ObjectType   = vObjectType
             ChainStat.ObjectNumber = vObjectId
             ChainStat.ObjectGroup  = "":U
      NO-ERROR. /* Clearing error status */

      IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
      RETURN.
    END.

    ASSIGN ChainStat.ChainBlocks = vBlockCount
           ChainStat.EndTime     = String2DateTime(gvTime) WHEN gvTime NE ?
    . /* ASSIGN */

  END. /* DO TRANSACTION */

/* If the chain list was not previously loaded: */
  IF ipListOffset NE ? AND NOT CAN-FIND(FIRST ChainBlock OF ChainStat) THEN
  RUN GetChainList(ipStatId, 
                   ipAreaNumber,
                   vObjectId, 
                   vObjectType, 
                   vChainType, 
                   "":U, /* ObjectGroup */
                   ipListOffset).

END PROCEDURE. /* GetChainStatV10 */

/* ------------------------------------------------------------------------- */

PROCEDURE GetChainStatV11:

  DEFINE INPUT PARAMETER ipStatId     AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipAreaNumber AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipChainType  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipListOffset AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vChainOrder  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vObjectId    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vObjectType  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vObjectName  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vObjectGroup AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vTableOwner  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vTableName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vListTitle   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vListTime    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vListOffset  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vBlockCount  AS INT64     NO-UNDO.
  DEFINE VARIABLE vText        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vItem        AS CHARACTER NO-UNDO EXTENT 4.
  DEFINE VARIABLE i            AS INTEGER   NO-UNDO.

/* Examples of shain statistics in Progress V11:

FREE CHAIN ANALYSIS
-------------------


Number of          Object    Object                     Partition/Tenant/Group
Blocks             Type
------------------------------------------------------------------------------
505                Master    --:0    
0                  Table     PUB.theTable:1             C:Initial:0
0                  Table     PUB.theTable:1             P:IntField-11:1

RM CHAIN ANALYSIS
---------------------------


Number of          Object    Object                     Partition/Tenant/Group
Blocks             Type
------------------------------------------------------------------------------
2045948            Table     PUB.theTable:1             C:Initial:0
1024               Table     PUB.theTable:1             P:IntField-11:1

INDEX DELETE CHAIN ANALYSIS
---------------------------


AREA "Table Area": 7  BLOCK ANALYSIS
-------------------------------------------------

3910655 block(s) found in the area.


But only the headers will be available In db log file:

FREE CHAIN ANALYSIS
Number of          Object    Object                     Partition/Tenant/Group
Blocks             Type
RM CHAIN ANALYSIS
Number of          Object    Object                     Partition/Tenant/Group
Blocks             Type
INDEX DELETE CHAIN ANALYSIS
AREA "Table Area": 7  BLOCK ANALYSIS
3910655 block(s) found in the area.
*/

  ASSIGN vListTitle  = SUBSTITUTE("LIST OF &1 BLOCKS":U, ipChainType)
         vListOffset = ipListOffset
         vListTime   = ?
  . /* ASSIGN */

ChainStat:
  REPEAT:

    ASSIGN vText  = GetText().

    IF vText EQ ? THEN /* On end of input file: */
    RETURN.

/* Skip the header: */
    IF vText MATCHES "Blocks * Type":U
    OR vText BEGINS "----":U THEN
    NEXT ChainStat.

/* Chain statistics can interleave with the lists of chain blocks (in chanalys)

Number of          Object    Object                     Partition/Tenant/Group
Blocks             Type
------------------------------------------------------------------------------
0                  Index     PUB.theTable.SeqId64:8
          LIST OF FREE CHAIN BLOCKS
                next
        dbkey   free
        130973952       130974016
...
        131006336       0
507                Index     PUB.theTable.LocalIdx2:13

          LIST OF RM CHAIN BLOCKS
                free    # free
        dbkey   space   slots   hold
        1535232         7880    57      1
        1535296         7286    0       57
1724               Table     PUB.tbl:2    
*/

/* Skip "LIST OF * CHAIN BLOCKS" title: */
    IF vText EQ vListTitle THEN
    DO:
      ASSIGN vListOffset = SEEK(INPUT)
             vListTime   = gvTime
      . /* ASSIGN */
      NEXT ChainStat.
    END.

/* Skip "LIST OF * CHAIN BLOCKS" header: */
    IF vListOffset NE ? THEN
    IF vText EQ     "next":U 
    OR vText BEGINS "free ":U
    OR vText BEGINS "dbkey ":U THEN
    NEXT ChainStat.

    DO i = 1 TO 4:
      ASSIGN vItem[i] = ENTRY(1, vText, " ":U)
             vText  = TRIM(SUBSTRING(vText, LENGTH(vItem[i]) + 2))
      . /* ASSIGN */
    END.

/* Starting from this point the first item should be an integer: */
    IF TRIM(vItem[1], "0123456789":U) NE "":U THEN
    LEAVE ChainStat.

/* Skip "LIST OF * CHAIN BLOCKS" body (the second item is an integer): */
    IF vListOffset NE ? THEN
    IF TRIM(vItem[2], "0123456789":U) EQ "":U THEN
    NEXT ChainStat.

/* If the items do not match the pattern of chain statistics:
507                Index     PUB.theTable.LocalIdx2:13
*/  IF LOOKUP(vItem[2], "Master,Table,Index,Blob":U) EQ 0 /* object type */
    OR NUM-ENTRIES(vItem[3], ":":U) NE 2 THEN             /* Name:Id     */
    LEAVE ChainStat.

    ASSIGN vBlockCount  =            INT64(vItem[1])
           vObjectType  =                  vItem[2]
           vObjectName  =         ENTRY(1, vItem[3], ":":U)
           vObjectId    = INTEGER(ENTRY(2, vItem[3], ":":U))
           vObjectGroup = IF vItem[4] EQ ":0":U THEN "":U ELSE vItem[4]
    NO-ERROR. /* ASSIGN */

    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    RETURN.

    CASE vObjectType:

      WHEN "Master":U THEN ASSIGN vObjectName = "":U. /* do nothing */

      WHEN "Table":U  THEN
      DO TRANSACTION:

         ASSIGN vTableOwner = ENTRY(1, vObjectName, ".":U)
                vTableName  = ENTRY(2, vObjectName, ".":U)
         . /* ASSIGN */

         FIND FIRST TableStat EXCLUSIVE-LOCK
              WHERE TableStat.StatId     EQ ipStatId
                AND TableStat.TableOwner EQ vTableOwner
                AND TableStat.TableName  EQ vTableName
                AND TableStat.TableGroup EQ vObjectGroup
         NO-ERROR.

         IF NOT AVAILABLE TableStat THEN
         CREATE TableStat.
         ASSIGN TableStat.StatId      = ipStatId   
                TableStat.TableOwner  = vTableOwner
                TableStat.TableName   = vTableName 
                TableStat.TableGroup  = vObjectGroup
                TableStat.TableNumber = vObjectId
                TableStat.AreaNumber  = ipAreaNumber
         NO-ERROR. /* Clearing error status */
      END. /* WHEN "Table":U */
      
      WHEN "Index":U  THEN
      DO TRANSACTION:

         ASSIGN vTableOwner = ENTRY(1, vObjectName, ".":U)
                vTableName  = ENTRY(2, vObjectName, ".":U)
                vObjectName = ENTRY(3, vObjectName, ".":U)
         . /* ASSIGN */

         FIND FIRST IndexStat EXCLUSIVE-LOCK
              WHERE IndexStat.StatId     EQ ipStatId
                AND IndexStat.TableOwner EQ vTableOwner
                AND IndexStat.TableName  EQ vTableName
                AND IndexStat.IndexName  EQ vObjectName
                AND IndexStat.IndexGroup EQ vObjectGroup
         NO-ERROR.

         IF NOT AVAILABLE IndexStat THEN
         CREATE IndexStat. /* GetChainStatV11 */
         ASSIGN IndexStat.StatId      = ipStatId   
                IndexStat.TableOwner  = vTableOwner
                IndexStat.TableName   = vTableName
                IndexStat.IndexName   = vObjectName
                IndexStat.IndexGroup  = vObjectGroup
                IndexStat.IndexNumber = vObjectId
                IndexStat.AreaNumber  = ipAreaNumber
         NO-ERROR. /* Clearing error status */
      END. /* WHEN "Index":U */
           
      WHEN "Blob":U  THEN
      DO TRANSACTION:

         ASSIGN vTableOwner = ENTRY(1, vObjectName, ".":U)
                vTableName  = ENTRY(2, vObjectName, ".":U)
                vObjectName = ENTRY(3, vObjectName, ".":U)
         . /* ASSIGN */

         FIND FIRST LobStat EXCLUSIVE-LOCK
              WHERE LobStat.StatId     EQ ipStatId
                AND LobStat.TableOwner EQ vTableOwner
                AND LobStat.TableName  EQ vTableName
                AND LobStat.LobName    EQ vObjectName
                AND LobStat.LobGroup   EQ vObjectGroup
         NO-ERROR.

         IF NOT AVAILABLE LobStat THEN
         CREATE LobStat.
         ASSIGN LobStat.StatId     = ipStatId   
                LobStat.TableOwner = vTableOwner
                LobStat.TableName  = vTableName
                LobStat.LobName    = vObjectName
                LobStat.LobGroup   = vObjectGroup
                LobStat.LobNumber  = vObjectId
                LobStat.AreaNumber = ipAreaNumber
         NO-ERROR. /* Clearing error status */
      END. /* WHEN "Blob":U */
      
    END CASE.

/* Free chains of Table/Blob objects should be empty: */
    IF ipChainType EQ "free chain":U
    AND LOOKUP(vObjectType, "Table,Blob":U) GT 0 THEN
    IF vBlockCount EQ 0 THEN
    RETURN.
    ELSE
    DO TRANSACTION:
      CREATE DbStatError.
      ASSIGN DbStatError.StatId       = ipStatId
             DbStatError.AreaNumber   = ipAreaNumber
             DbStatError.ErrorNumber  = -13
             DbStatError.ErrorSource  = gvLine
             DbStatError.ErrorMessage = 
     SUBSTITUTE("Warning: a non-empty free chain is unexpected for &1 object.",
                                                            /*&1*/ vObjectType)
      . /* ASSIGN */
/* Nevertheless create ChainStat: */
    END. /* DO TRANSACTION */

    DO TRANSACTION:

/* If the statistics report was previously loaded: */
      FIND FIRST ChainStat EXCLUSIVE-LOCK
           WHERE ChainStat.StatId       EQ ipStatId
             AND ChainStat.ChainType    EQ ipChainType
             AND ChainStat.ObjectType   EQ vObjectType
             AND ChainStat.ObjectNumber EQ vObjectId
             AND ChainStat.ObjectGroup  EQ vObjectGroup
             AND ChainStat.AreaNumber   EQ ipAreaNumber
      NO-ERROR.       
             
      IF NOT AVAILABLE ChainStat THEN
      DO:
        ASSIGN vChainOrder = 0.
        FOR EACH ChainStat NO-LOCK
           WHERE ChainStat.StatId EQ ipStatId
              BY ChainStat.StatId     DESCENDING
              BY ChainStat.ChainOrder DESCENDING:
          ASSIGN vChainOrder = ChainStat.ChainOrder + 1.
          LEAVE.
        END.

        CREATE ChainStat.
        ASSIGN ChainStat.StatId       = ipStatId
               ChainStat.ChainOrder   = vChainOrder
               ChainStat.ChainType    = ipChainType
               ChainStat.ObjectType   = vObjectType
               ChainStat.ObjectNumber = vObjectId
               ChainStat.ObjectGroup  = vObjectGroup
               ChainStat.AreaNumber   = ipAreaNumber
        NO-ERROR.

        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
        RETURN.
      END.

      ASSIGN ChainStat.ChainBlocks = vBlockCount
             ChainStat.BgnTime = String2DateTime(vListTime) WHEN vListTime NE ?
             ChainStat.EndTime = String2DateTime(gvTime)    WHEN gvTime    NE ? 
      . /* ASSIGN */

    END. /* DO TRANSACTION */

/* If the statistics report was NOT previously loaded: */
    IF vListOffset NE ? AND NOT CAN-FIND(FIRST ChainBlock OF ChainStat) THEN
    RUN GetChainList(ipStatId,
                     ipAreaNumber,
                     vObjectId,
                     vObjectType,
                     ipChainType,
                     vObjectGroup,
                     ipListOffset).

    ASSIGN vListOffset = ?
           vListTime   = ?
    . /* ASSIGN */
  END. /* ChainStat: REPEAT */

/* The ChainStat loop was left after reading a line with unexpected contents.
   Next GetText() will re-read the last line:
*/
  ASSIGN gvReread = TRUE.

END PROCEDURE. /* GetChainStatV11 */

/* ------------------------------------------------------------------------- */
        
PROCEDURE GetTableStat:

  DEFINE INPUT PARAMETER ipStatId     AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipAreaNumber AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vTableOwner  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vTableName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vTableGroup  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vText        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vItem        AS CHARACTER NO-UNDO EXTENT 8.
  DEFINE VARIABLE nItem        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vRecCount    AS   INT64   NO-UNDO.
  DEFINE VARIABLE vFragCount   AS   INT64   NO-UNDO.
  DEFINE VARIABLE vMinRecSize  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMaxRecSize  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMeanRecSize AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vScatterFact AS DECIMAL   NO-UNDO.

/* The starting line is the last line of table's header:

RECORD BLOCK SUMMARY FOR AREA "Schema Area" : 6
-------------------------------------------------------
                           -Record Size (B)-    ---Fragments--- Scatter
Table    Records    Size   Min   Max  Mean         Count Factor  Factor
PUB.table      0    0.0B     0     0     0             0    0.0     0.0
*/
TableLine:
  REPEAT TRANSACTION:

    ASSIGN vText = GetText().

    IF vText EQ ? THEN /* On end of input file: */
    RETURN.

/* The table ends by the dash line:
            -----------------------------------------------------------------
Subtotals:
*/  IF vText BEGINS "---":U THEN
    RETURN.

    ASSIGN vTableName  = ENTRY(1, vText, " ":U)
           i = LENGTH(vTableName)
           vText = TRIM(SUBSTRING(vText, i + 2))
           nItem = 0
    . /* ASSIGN */

    CASE NUM-ENTRIES(vTableName, ".":U):
      WHEN 1 THEN ASSIGN vTableGroup = "":U
                         vTableOwner = "PUB":U
                  . /* ASSIGN */
      WHEN 2 THEN ASSIGN vTableGroup = "":U
                         vTableOwner = ENTRY(1, vTableName, ".":U)
                         vTableName  = ENTRY(2, vTableName, ".":U)
                  . /* ASSIGN */
      WHEN 3 THEN ASSIGN vTableGroup = ENTRY(3, vTableName, ".":U)
                         vTableGroup = IF vTableGroup = "Initial(Composite)":U
                                       THEN "C:Initial:0":U
                                       ELSE "P:":U + vTableGroup
                         vTableOwner = ENTRY(1, vTableName, ".":U)
                         vTableName  = ENTRY(2, vTableName, ".":U)
                  . /* ASSIGN */
      OTHERWISE
      DO:
        CREATE DbStatError.
        ASSIGN DbStatError.StatId       = ipStatId
               DbStatError.AreaNumber   = ipAreaNumber
               DbStatError.ErrorNumber  = -8
               DbStatError.ErrorSource  = gvLine
               DbStatError.ErrorMessage = 
                         "Error in tabanalys: Unexpected format of table name:"
                         + vTableName
        . /* ASSIGN */
      END.
    END CASE.

/* Tabanalys in Progress versions before V10.2B06 breaks a line for a table
   with the long name and its statistics continues on the next line:
   http://knowledgebase.progress.com/articles/Article/000033756
*/  
    DO WHILE nItem LT 8:
      ASSIGN vText = GetText() WHEN vText EQ "":U
             nItem = nItem + 1
             vItem[nItem] = ENTRY(1, vText, " ":U)
             vText = TRIM(SUBSTRING(vText, LENGTH(vItem[nItem]) + 2))
      . /* ASSIGN */
    END.

    IF vText EQ ? THEN /* On end of input file: */
    RETURN.

/* Unexpected number of items per line: */
    IF vText NE "":U THEN
    DO:
      CREATE DbStatError.
      ASSIGN DbStatError.StatId       = ipStatId
             DbStatError.AreaNumber   = ipAreaNumber
             DbStatError.ErrorNumber  = -9
             DbStatError.ErrorSource  = gvLine
             DbStatError.ErrorMessage = 
                         "Warning: Unexpected number of the items in tabanalys"
      . /* ASSIGN */
    END.

/*
RECORD BLOCK SUMMARY FOR AREA "area" : 7
-------------------------------------------------------
                                 -Record Size (B)-     ---Fragments--- Scatter
Table          Records    Size   Min   Max  Mean          Count Factor  Factor
PUB.table   4232953795  668.3G    43 19243   169     4233060260    1.0     1.0
1           2           3         4  5       6       7             8       9
*/  ASSIGN vRecCount    =   INT64(vItem[1])
           vMinRecSize  = INTEGER(vItem[3])
           vMaxRecSize  = INTEGER(vItem[4])
           vMeanRecSize = INTEGER(vItem[5])
           vFragCount   =   INT64(vItem[6])
           vScatterFact = DECIMAL(vItem[8])
    NO-ERROR. /* ASSIGN */

    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    RETURN.
/*
/* Table and index count per database as record count in _File and _Index: */
    IF ipAreaNumber EQ 6 THEN
    FOR FIRST AreaStat EXCLUSIVE-LOCK
      WHERE AreaStat.StatId     EQ ipStatId
        AND AreaStat.AreaNumber EQ ipAreaNumber:
      CASE vTableName:
        WHEN "_File":U  THEN ASSIGN AreaStat.NumTables = vRecCount.
        WHEN "_Index":U THEN ASSIGN AreaStat.NumIndexes = vRecCount.
      END CASE.
    END. /* FOR FIRST AreaStat */

/* Ignore the metaschema tables: */
    IF ipAreaNumber LE 6 THEN
    IF vTableName BEGINS "_":U THEN
    NEXT TableLine.
*/
    FIND FIRST TableStat EXCLUSIVE-LOCK
         WHERE TableStat.StatId     EQ ipStatId
           AND TableStat.TableOwner EQ vTableOwner
           AND TableStat.TableName  EQ vTableName
           AND TableStat.AreaNumber EQ ipAreaNumber
    NO-ERROR.
           
    IF NOT AVAILABLE TableStat THEN
    CREATE TableStat.
    ASSIGN TableStat.StatId      = ipStatId
           TableStat.TableOwner  = vTableOwner
           TableStat.TableName   = vTableName
           TableStat.TableGroup  = vTableGroup
           TableStat.AreaNumber  = ipAreaNumber
           TableStat.RecCount    = vRecCount
           TableStat.FragCount   = vFragCount - vRecCount
           TableStat.SizeInChar  =             vItem[2]
           TableStat.TableSize   = String2Byte(vItem[2])
           TableStat.MinRecSize  = vMinRecSize 
           TableStat.MaxRecSize  = vMaxRecSize 
           TableStat.MeanRecSize = vMeanRecSize
           TableStat.ScatterFact = vScatterFact
    NO-ERROR. /* Clearing error status */
    
    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    RETURN.
    
  END. /* TableLine: REPEAT TRANSACTION */

END PROCEDURE. /* GetTableStat */

/* ------------------------------------------------------------------------- */

PROCEDURE GetIndexStat:
  DEFINE INPUT PARAMETER ipStatId     AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipAreaNumber AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vTableOwner  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vTableName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vGroupName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vIndexNumber AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vIndexName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vIndexGroup  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vActiveIndex AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE vIndexFields AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vIndexLevels AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vIndexBlocks AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vSizeInChar  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vIndexUtil   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vText        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vItem1       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE nItem        AS INTEGER   NO-UNDO. /*<= last used vItem[i] */
  DEFINE VARIABLE vItem        AS CHARACTER NO-UNDO EXTENT 8.
  DEFINE VARIABLE nName        AS INTEGER   NO-UNDO. /*<= last used vName[i] */
  DEFINE VARIABLE vName        AS CHARACTER NO-UNDO EXTENT 3.

/* The starting line is the last line of table's header:
Table                      Index  Fields Levels  Blocks    Size  % Util  Factor

Examples:

INDEX BLOCK SUMMARY FOR AREA "index area" : 8
-------------------------------------------------------
Table                      Index  Fields Levels  Blocks    Size  % Util  Factor
PUB.table  
  index1                      10       3      5 9254912   18.0G    51.2     2.0
  index-with-long-name(inactive)
                              11       2      1       1    3.0B     0.1     1.0

INDEX BLOCK SUMMARY FOR SHARED OBJECTS:
--------------------------------------------
Table                      Index  Fields Levels  Blocks    Size  % Util  Factor
PUB.theTable
  IntField
    _Partition-Internal-Value
                           -1558       1      1       1  227.0B     2.8     1.0
    DateField:0(inactive)     12       1      3   46647  360.2M    99.2     1.0
    IntField                  10       1
      IntField-1:3                            2     215    1.6M    98.2     1.0
      IntField-10:2                           2     215    1.6M    98.2     1.0
      IntField-11:1                           2     216    1.6M    97.8     1.0
    Rand64:0                  11       1      3  260801    1.4G    68.2     1.6
    SeqId64:0                  8       1      3  204776    1.0G    64.9     1.7
    
Table                      Index  Fields Levels  Blocks    Size  % Util  Factor
PUB.theTable
  IntField
    IntField                  10       1
      IntField-5:7                            2     215    1.6M    98.2     1.0
*/
  ASSIGN nItem = 0    /* <= last used index in vItem array */
         nName = 0    /* <= last used index in vName array */
  NO-ERROR.           /* <= clearing error status    */

IndexLine:
  REPEAT TRANSACTION:

    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    RETURN.

    ASSIGN vText = GetText().

    IF vText EQ ? THEN /* On end of input file: */
    RETURN.

/* In V11 the index list ends by the dash line:
            -----------------------------------------------------------------
Totals:
*/  IF vText BEGINS "---":U THEN
    RETURN.

/* Parse the line by items: */
    ASSIGN nItem = 0. /* <= number of vItem */
    DO WHILE nItem LT EXTENT(vItem) AND vText NE "":U:
      ASSIGN nItem = nItem + 1
             vItem[nItem] = ENTRY(1, vText, " ":U)
             vText = TRIM(SUBSTRING(vText, LENGTH(vItem[nItem]) + 2))
      . /* ASSIGN */
    END. /* DO WHILE */

/* In V11 the index list ends by the next "INDEX BLOCK SUMMARY" title.       */
/* Stop reading the index list if the 2nd item on the line is not an integer:*/
    IF TRIM(vItem[2], "0123456789":U) NE "":U THEN
    DO:
      ASSIGN gvReread = TRUE.
      RETURN.
    END.

/* Line conatins more than 8 items: */
    IF vText NE "":U THEN
    DO:
      CREATE DbStatError.
      ASSIGN DbStatError.StatId       = ipStatId
             DbStatError.AreaNumber   = ipAreaNumber
             DbStatError.ErrorNumber  = -10
             DbStatError.ErrorSource  = gvLine
             DbStatError.ErrorMessage = "Warning: Too many items in ixanalys."
      . /* ASSIGN */
    END. /* OTHERWISE */

    CASE nItem:
      WHEN 1 THEN
      DO:
/* Only a table name or a partition policy name can be alone on the line.
   Or a long index name followed by 7 items on the next line.
   vName is the FIFO stack to store such names:
*/      ASSIGN nName = nName + 1
               vName[nName] = vItem[1]
        . /* ASSIGN */
        NEXT IndexLine.
      END. /* WHEN 1 */

      WHEN 2 THEN /* ------------------------------------------------------- */
      DO:
/* Index with long name and partitioning:
Table                      Index  Fields Levels  Blocks    Size  % Util  Factor
PUB.theTable
  IntField
    IntField901234567890123456789012
==>                           10       1
      IntField-1:3                            2     215    1.6M    98.2     1.0
*/      ASSIGN vIndexName   = vName[nName] /*Remove last name from the stack:*/
               nName        = nName - 1
               vIndexNumber = INTEGER(vItem[1])
               vIndexFields = INTEGER(vItem[2])
        NO-ERROR. /* ASSIGN */
        NEXT IndexLine.
      END. /* WHEN 2 */

      WHEN 3 THEN /* ------------------------------------------------------- */
      DO:
/* Common information for local indexex
Table                      Index  Fields Levels  Blocks    Size  % Util  Factor
PUB.theTable
  IntField
==> IntField                  10       1
      IntField-1:3                            2     215    1.6M    98.2     1.0
*/      ASSIGN vIndexName   =         vItem[1]
               vIndexNumber = INTEGER(vItem[2])
               vIndexFields = INTEGER(vItem[3])
        NO-ERROR. /* ASSIGN */
        NEXT IndexLine.
      END.

      WHEN 6 THEN /* ------------------------------------------------------- */
/* Common information for local indexe:
Table                      Index  Fields Levels  Blocks    Size  % Util  Factor
PUB.theTable
  IntField
    IntField                  10       1
==>   IntField-1:3                            2     215    1.6M    98.2     1.0
*/    ASSIGN vIndexGroup  = "P:":U + vItem[1]
             vIndexLevels =  INTEGER(vItem[2])
             vIndexBlocks =  INTEGER(vItem[3])
             vSizeInChar  =          vItem[4]
             vIndexUtil   =  DECIMAL(vItem[5])
      NO-ERROR. /* ASSIGN */

      WHEN 7 THEN /* ------------------------------------------------------- */
/* Index with long name:
Table                      Index  Fields Levels  Blocks    Size  % Util  Factor
PUB.table  
  index-with-long-name(inactive)
==>                           11       2      1       1    3.0B     0.1     1.0
*/    ASSIGN vIndexName   = vName[nName] /*Remove last name from the stack:*/
             nName        = nName - 1
             vIndexGroup  = "":U
             vIndexNumber = INTEGER(vItem[1])
             vIndexFields = INTEGER(vItem[2])
             vIndexLevels = INTEGER(vItem[3])
             vIndexBlocks = INTEGER(vItem[4])
             vSizeInChar  =         vItem[5]
             vIndexUtil   = DECIMAL(vItem[6])
      NO-ERROR. /* ASSIGN */

      WHEN 8 THEN /* ------------------------------------------------------- */
/* The most cases will have all 8 items on the same line:
Table                      Index  Fields Levels  Blocks    Size  % Util  Factor
PUB.table  
  index1                      10       3      5 9254912   18.0G    51.2     2.0
*/    ASSIGN vIndexGroup  = "":U
             vIndexName   =         vItem[1]
             vIndexNumber = INTEGER(vItem[2])
             vIndexFields = INTEGER(vItem[3])
             vIndexLevels = INTEGER(vItem[4])
             vIndexBlocks = INTEGER(vItem[5])
             vSizeInChar  =         vItem[6]
             vIndexUtil   = DECIMAL(vItem[7])
      NO-ERROR. /* ASSIGN */

      OTHERWISE
      DO:
        CREATE DbStatError.
        ASSIGN DbStatError.StatId       = ipStatId
               DbStatError.AreaNumber   = ipAreaNumber
               DbStatError.ErrorNumber  = -11
               DbStatError.ErrorSource  = gvLine
               DbStatError.ErrorMessage =
                               "Error in ixanalys: Unexpected number of items."
        . /* ASSIGN */
        RETURN.
      END. /* OTHERWISE */
    END CASE. /* nItem */

    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    RETURN.

/* vName stack can store one or two "unused" named: */
    IF nName EQ 1 THEN
    ASSIGN vTableName = vName[nName].
    ELSE
    IF nName EQ 2 THEN
    ASSIGN vTableName = vName[1]
           vGroupName = vName[2]
    . /* ASSIGN */
    ELSE
    IF nName GT 2 THEN
    DO:
      ASSIGN vText = "".
      DO WHILE nName GT 0:
        ASSIGN vText = " ":U + vName[nName] + vText
               nName = nName - 1
        . /* ASSIGN */
      END.
      CREATE DbStatError.
      ASSIGN DbStatError.StatId       = ipStatId
             DbStatError.AreaNumber   = ipAreaNumber
             DbStatError.ErrorNumber  = -12
             DbStatError.ErrorSource  = gvLine
             DbStatError.ErrorMessage =
                      "Error in ixanalys: can't interpret the names:" + vText
      . /* ASSIGN */
      RETURN.
    END. /* IF nName GT 2 */

    IF nName GT 0 THEN
    IF NUM-ENTRIES(vTableName, ".":U) EQ 2 THEN
    ASSIGN vTableOwner = ENTRY(1, vTableName, ".":U)
           vTableName  = ENTRY(2, vTableName, ".":U)
    . /* ASSIGN */
    ELSE 
    ASSIGN vTableOwner = "PUB":U WHEN vTableOwner EQ "":U.

/* "Empty" the vName stack: */
    ASSIGN nName = 0.
/*
Table                      Index  Fields Levels  Blocks    Size  % Util  Factor
PUB.theTable
  IntField
    DateField:0(inactive)     12       1      3   46647  360.2M    99.2     1.0
    Rand64:0                  11       1      3  260801    1.4G    68.2     1.6
    SeqId64:0                  8       1      3  204776    1.0G    64.9     1.7
*/  IF NUM-ENTRIES(vIndexName, ":":U) EQ 2 THEN
    ASSIGN vIndexName  = ENTRY(1, vIndexName, ":":U).

/* Ignore metaschema indexes: */
/*  IF vTableName BEGINS "_":U AND ipAreaNumber LE 6 THEN
    NEXT IndexLine.
*/
    IF vIndexName MATCHES "*(inactive)":U THEN
    ASSIGN vIndexName   = SUBSTRING(vIndexName, 1, LENGTH(vIndexName) - 10)
           vActiveIndex = FALSE
    . /* ASSIGN */
    ELSE
    ASSIGN vActiveIndex = TRUE.

    FIND FIRST IndexStat EXCLUSIVE-LOCK /* INDEX IndexName + AreaNumber */
         WHERE IndexStat.StatId     EQ ipStatId
           AND IndexStat.TableOwner EQ vTableOwner
           AND IndexStat.TableName  EQ vTableName
           AND IndexStat.IndexName  EQ vIndexName
           AND IndexStat.IndexGroup EQ vIndexGroup
           AND IndexStat.AreaNumber EQ ipAreaNumber
    NO-ERROR.       

    IF NOT AVAILABLE IndexStat THEN
    CREATE IndexStat. /* GetIndexStat */
    ASSIGN IndexStat.StatId      = ipStatId
           IndexStat.AreaNumber  = ipAreaNumber
           IndexStat.TableOwner  = vTableOwner
           IndexStat.TableName   = vTableName
           IndexStat.IndexName   = vIndexName
           IndexStat.IndexGroup  = vIndexGroup
           IndexStat.ActiveIndex = vActiveIndex
           IndexStat.IndexNumber = vIndexNumber
           IndexStat.IndexFields = vIndexFields
           IndexStat.IndexLevels = vIndexLevels
           IndexStat.IndexBlocks = vIndexBlocks
           IndexStat.IndexSize   = String2Byte(vSizeInChar)
           IndexStat.SizeInChar  = vSizeInChar
           IndexStat.IndexUtil   = vIndexUtil 
    NO-ERROR. /* Clearing error status */

  END. /* IndexLine: REPEAT TRANSACTION */

END PROCEDURE. /* GetIndexStat */

/* ------------------------------------------------------------------------- */

PROCEDURE Dbanalys:

  DEFINE INPUT PARAMETER ipInputFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vOptionList AS CHARACTER NO-UNDO INITIAL
                                      "dbanalys,chanalys,ixanalys,tabanalys":U.
  DEFINE VARIABLE vStatId     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vStatIdList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vSegment    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vSetTime    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDbPath     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vBlockSize  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vOptions    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDate       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vAreaNumber AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vAreaName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vChainType  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vText       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vMsg#       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMyProc     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vListOffset AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i           AS INTEGER   NO-UNDO.

/*
The structure of dbanalys output:

msg  3867: 1)   PROGRESS Database Analysis
msg  3868:      Database: %s
msg  8361:      Blocksize: %i
msg 14178:      RecSpaceSearchDepth: %d
msg  3869:      Options: %s %s
msg  6172:      Options: %s %s %s
msg  3870:      Date: %s%r
msg 14179: 2)   CHAIN ANALYSIS FOR AREA "%s" : %l
msg 14163: 2.1) FREE CLUSTER CHAIN ANALYSIS
msg 14157:      LIST OF FREE CLUSTER CHAIN BLOCKS
msg 14161:      %J cluster(s) found in the free cluster chain.
msg  3873: 2.2) FREE CHAIN ANALYSIS [(3873)]
msg  3880:      LIST OF FREE CHAIN BLOCKS
msg 14155:      %J block(s) found in the free chain of %s object %i
msg 16573:      %rNumber of          Object    Object    Partition/Tenant/Group
msg 16576:      %rNumber of          Object    Object
msg 16574:      Blocks             Type
msg  3892: 2.3) RM CHAIN ANALYSIS [(3892)]
msg  3897:      LIST OF RM CHAIN BLOCKS
msg 14172:      %J block(s) found in the RM chain of %s object %i
msg  7269: 2.4) INDEX DELETE CHAIN ANALYSIS
msg  7283:      LIST OF INDEX DELETE CHAIN BLOCKS
msg 14176:      %J block(s) found in the Index Delete chain of %s object %i
msg 10097: 3)   AREA "%s" : %d  BLOCK ANALYSIS
msg  3906: 4)   BLOCK ANALYSIS [(3906)] <- moved to the end at V11
msg  6129: 5)   RECORD BLOCK SUMMARY [(6129)]
msg 14212:      RECORD BLOCK SUMMARY FOR AREA "%s" : %l
msg 16667:      RECORD BLOCK SUMMARY FOR SHARED TABLES
msg 16668:      RECORD BLOCK SUMMARY FOR %s %s: %l
msg 16656:      Summary for AREA "%s": %l
msg  9894:      Subtotals:      %s
msg  6130:      Totals:         %s
msg  3921:      %J RM block(s) found in the database.
msg  3922:      %l.%l% of the RM block space is used.
msg  3925: 6)   INDEX BLOCK SUMMARY [(3925)]
msg 14225:      INDEX BLOCK SUMMARY FOR AREA "%s" : %l
msg 16657:      INDEX BLOCK SUMMARY FOR SHARED OBJECTS:
msg 16658:      INDEX BLOCK SUMMARY FOR %s: %s
msg  9894:      Subtotals:      %s
msg  6130:      Totals:         %s
msg  3923:      %J index block(s) found in the database.
msg  3922:      %l.%l% of the RM block space is used.
msg  2314: 7)   DATABASE SUMMARY
msg 16666:      SUMMARY FOR AREA "%s": %l
msg 16659:   ?? SUMMARY FOR SHARED OBJECTS:
      V11: 8)   AREA BLOCK ANALYSIS:
      V11: 8)   DATABASE BLOCK ANALYSIS:
                Total          %s
msg  3937:      Database analysis complete %s%r
*/
  INPUT FROM VALUE(ipInputFile).

/* Fields from dbanalys header (PROGRESS Database Analysis): */
  ASSIGN vSegment   = "PROGRESS Database Analysis":U
         vMyProc    = ?
         vDbPath    = ?
         vBlockSize = ?
         vOptions   = ?
         vDate      = ?
         vSetTime   = ?
         vStatId    = ?
         vStatIdList = "":U
  . /* ASSIGN */

ImportLine:
  REPEAT:

    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    LEAVE ImportLine.

    ASSIGN vText = GetText().

    IF vText EQ ? THEN
    LEAVE ImportLine.

/*
MESSAGE "Debug1" SKIP
"Text:" vText SKIP
"Time:" gvTime SKIP
"Line:" gvLine SKIP
"Offset:" SEEK(INPUT) SKIP
"StatId:" vStatId SKIP
"AreaNumber:" vAreaNumber SKIP
"Segment:" vSegment SKIP
"LgMsg#:" gvLgMsg# SKIP
"LgProc:" gvLgProc SKIP
"MyProc:" vMyProc
VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/

/* The warnings/errors issued by dbanalys: --------------------------------- */

    IF vText MATCHES "* (*)":U THEN
    CASE ENTRY(NUM-ENTRIES(vText, " ":U), vText, " ":U):
/* In old Progress versions some segment titles had the numbers: */
      WHEN "(3873)":U OR   /* FREE CHAIN ANALYSIS (3873)  */
      WHEN "(3906)":U OR   /* BLOCK ANALYSIS (3906)       */
      WHEN "(3892)":U OR   /* RM CHAIN ANALYSIS (3892)    */
      WHEN "(3906)":U OR   /* BLOCK ANALYSIS (3906)       */
      WHEN "(3925)":U OR   /* INDEX BLOCK SUMMARY (3925)  */
      WHEN "(6129)":U THEN /* RECORD BLOCK SUMMARY (6129) */
      ASSIGN vText = SUBSTRING(vText, 1, LENGTH(vText) - 7).

/*[Warning] database in use - reported statistics are approximations. (2486) */
      WHEN "(2486)":U THEN
      FOR FIRST DbStat EXCLUSIVE-LOCK
          WHERE DbStat.StatId EQ vStatId:
        ASSIGN  DbStat.SourceType = DbStat.SourceType + ",online":U.
        NEXT ImportLine.
      END.

      OTHERWISE
/*[Warning] database not properly closed, proceeding. (3871)
  [Warning] Analysis performed on database that was not properly closed. (3936)
  [Warning] RM block found that should be in the RM free chain. (2802)
*/    DO TRANSACTION:
        ASSIGN i     = NUM-ENTRIES(vText, " ":U)
               vText = ENTRY(i, vText, " ":U)
               vText = TRIM(vText, "()":U)
               vMsg# = INTEGER(vText)
        NO-ERROR. /* ASSIGN */

        CREATE DbStatError.
        ASSIGN DbStatError.StatId       = vStatId
               DbStatError.AreaNumber   = vAreaNumber
               DbStatError.ErrorNumber  = vMsg#
               DbStatError.ErrorSource  = gvLine
               DbStatError.ErrorMessage = "Dbanalys error"
        . /* ASSIGN */
        NEXT ImportLine.
      END. /* FOR FIRST AreaStat */
    END CASE. /* message number */
    ELSE

/* The end of the current dbanalys: ---------------------------------------- */

/* msg  3937: database analysis complete %s%r */
    IF vText BEGINS "database analysis complete":U
    OR
/* (334) Chanalys session end.
   (453) Logout by Dbanalys on batch. 
*/  (vMyProc EQ gvLgProc AND LOOKUP(gvLgMsg#, "334,453":U) GT 0) THEN
    FOR FIRST DbStat EXCLUSIVE-LOCK
        WHERE DbStat.StatId EQ vStatId:
/* Messages from db log file (## 334 and 453) will have gvTime ne ?.
   If message # 3937 have gvTime then gvTime has a priority over the time
   reported in the message itself. Otherwise use the time from the message:
*/    ASSIGN  gvTime = SUBSTRING(vText, 28) WHEN gvTime EQ ?
              DbStat.EndTime = String2DateTime(gvTime)
              DbStat.RunInterval = INTERVAL(DbStat.EndTime,
                                            DbStat.BgnTime,
                                           "milliseconds":U) / 1000.0

              vStatIdList = vStatIdList + MIN(vStatIdList, ",":U)
                          + STRING(vStatId)
/* The load of the current dbanalys is finished. Look for the next one: */
              vSegment   = "PROGRESS Database Analysis":U
              vMyProc    = ?
              vStatId    = ?
              vDbPath    = ?
              vBlockSize = ?
              vOptions   = ?
              vDate      = ?
              vSetTime   = ?
      . /* ASSIGN */
    END.

/* The segments of dbanalys output: ---------------------------------------- */

/* msg  3867: 1)   PROGRESS Database Analysis */
    IF vText EQ "PROGRESS Database Analysis":U THEN
    ASSIGN vSegment    = vText
           vSetTime    = "None":U
           vStatId     = ?
           vAreaNumber = ?
    . /* ASSIGN */
    ELSE

/* msg 14179: CHAIN ANALYSIS FOR AREA "%s" : %l */
    IF vText BEGINS "CHAIN ANALYSIS FOR AREA ":U THEN
    ASSIGN vSegment = "AREA CHAIN ANALYSIS":U
           vSetTime = "AreaBegin":U
           vAreaNumber = GetAreaNumber(vStatId, vText)
    . /* ASSIGN */
    ELSE

/* msg 10097: AREA "%s" : %d  BLOCK ANALYSIS */
    IF vText MATCHES "AREA * BLOCK ANALYSIS":U THEN
    ASSIGN vSegment = "AREA BLOCK ANALYSIS":U
           vSetTime = "AreaPhase":U
           vAreaNumber = GetAreaNumber(vStatId, vText)
    . /* ASSIGN */
    ELSE

/* V11 for single area dbanalys: */
    IF vText EQ "AREA BLOCK ANALYSIS:":U THEN
    ASSIGN vSegment = "BLOCK ANALYSIS":U
           vSetTime = "AreaPhase":U
    . /* ASSIGN */
    ELSE

/* msg 14212: RECORD BLOCK SUMMARY FOR AREA "%s" : %l */
    IF vText BEGINS "RECORD BLOCK SUMMARY FOR AREA ":U THEN
    ASSIGN vSegment = "RECORD BLOCK SUMMARY":U
           vSetTime = IF vText BEGINS vSegment THEN "None":U ELSE "DbPhase":U
           vAreaNumber = GetAreaNumber(vStatId, vText)
    . /* ASSIGN */
    ELSE

/* msg 14225: INDEX BLOCK SUMMARY FOR AREA "%s" : %l */
    IF vText BEGINS "INDEX BLOCK SUMMARY FOR AREA ":U THEN
    ASSIGN vSegment = "INDEX BLOCK SUMMARY":U
           vSetTime = IF vText BEGINS vSegment THEN "None":U ELSE "DbPhase":U
           vAreaNumber = GetAreaNumber(vStatId, vText)
    . /* ASSIGN */
    ELSE

/* msg 14212: RECORD BLOCK SUMMARY FOR AREA "%s" : %l */
    IF vText EQ "RECORD BLOCK SUMMARY":U
    OR vText EQ "INDEX BLOCK SUMMARY":U THEN
    ASSIGN vSegment = vText
           vSetTime = "StatPhase":U
    . /* ASSIGN */
    ELSE

    IF vText EQ "DATABASE SUMMARY":U              /* V10 */
    OR vText EQ "BLOCK ANALYSIS":U                /* V10 for database */
    OR vText EQ "DATABASE BLOCK ANALYSIS:":U THEN /* V11 for database */
    ASSIGN vSegment = "BLOCK ANALYSIS":U
           vSetTime = "StatPhase":U
    . /* ASSIGN */

/* Starting time of new dbanalys' segment: */
    IF gvTime NE ? THEN
    CASE vSetTime:
    
      WHEN "AreaBegin":U THEN /* ---------------------------- */
      FOR FIRST AreaStat EXCLUSIVE-LOCK
          WHERE AreaStat.StatId     EQ vStatId
            AND AreaStat.AreaNumber EQ vAreaNumber
      TRANSACTION:
         ASSIGN AreaStat.BgnTime = String2DateTime(gvTime).
      END.
    
      WHEN "AreaPhase":U THEN /* ---------------------------- */
      FOR FIRST AreaStat EXCLUSIVE-LOCK
          WHERE AreaStat.StatId     EQ vStatId
            AND AreaStat.AreaNumber EQ vAreaNumber
      TRANSACTION:
        DO i = 1 TO EXTENT(AreaStat.PhaseTime):
          IF AreaStat.PhaseTime[i] EQ ?
          OR AreaStat.PhaseName[i] EQ vSegment THEN
          ASSIGN AreaStat.PhaseTime[i] = String2DateTime(gvTime)
                 AreaStat.PhaseName[i] = vSegment
                 i = ? /* to leave the DO loop */
          . /* ASSIGN */
        END. /* DO */
      END. /* FOR FIRST AreaStat */
    
      WHEN "StatPhase":U THEN /* ---------------------------- */
      FOR FIRST DbStat EXCLUSIVE-LOCK
          WHERE DbStat.StatId EQ vStatId:
        DO i = 1 TO EXTENT(DbStat.PhaseTime):
          IF DbStat.PhaseTime[i] EQ ?
          OR DbStat.PhaseName[i] EQ vSegment THEN
          ASSIGN DbStat.PhaseTime[i] = String2DateTime(gvTime)
                 DbStat.PhaseName[i] = vSegment
                 i = ? /* to leave the DO loop */
          . /* ASSIGN */
        END. /* DO */
      END. /* FOR FIRST DbStat */
    END CASE.

/* SetTime is defined only when the current line opnes new segment: */
    IF vSetTime NE ? THEN
    DO:
      ASSIGN vSetTime = ?.
      NEXT ImportLine.
    END.
    
/* Parsing dbanalys by segments: ------------------------------------------- */

/* List of the dbanalys' segments:

    "PROGRESS Database Analysis"
    "AREA CHAIN ANALYSIS"
    "AREA BLOCK ANALYSIS"
    "RECORD BLOCK SUMMARY"
    "INDEX BLOCK SUMMARY"
    "BLOCK ANALYSIS"
*/
    CASE vSegment:

      WHEN "PROGRESS Database Analysis":U THEN /* -------------------------- */
      DO:
/* msg  3868: Database: %s */
        IF vText BEGINS "Database: ":U /* /path/to/database */
/* msg (4235) Physical Database Name (-db): dbpath */
        OR vText BEGINS "Physical Database Name (-db): ":U THEN
        ASSIGN i       = INDEX(vText, ":":U)
               vDbPath = SUBSTRING(vText, i + 2)
               vDbPath = TRIM(vDbPath)
        . /* ASSIGN */
        ELSE

/* msg  8361: Blocksize: %i */
        IF vText BEGINS "Blocksize: ":U /* 8192 */
/* msg (6573) Database Blocksize (-blocksize): 8192  */
        OR vText BEGINS "Database Blocksize (-blocksize): ":U THEN
        ASSIGN i          = NUM-ENTRIES(vText, " ":U)
               vBlockSize = INTEGER(ENTRY(i, vText, " ":U))
        . /* ASSIGN */
        ELSE
    
/* msg  3869: Options: %s %s
   msg  6172: Options: %s %s %s
*/      IF vText BEGINS "Options: ":U /* chanalys ixanalys tabanalys */ THEN
        ASSIGN vOptions = TRIM(SUBSTRING(vText, 10)).
        ELSE

/* msg  3870: Date: %s%r */
        IF vText BEGINS "Date: ":U /* Fri Jan 16 17:47:06 2015 */ THEN
        ASSIGN vDate = SUBSTRING(vText, 7).
        ELSE

        IF gvLgProc NE "":U THEN
        CASE gvLgMsg#:
/* msg  (451) Dbanalys session begin for <user> on <ttyxxx>. */
          WHEN "451":U THEN
          IF gvLgMsg# EQ "451":U
          AND LOOKUP(ENTRY(1, vText, " ":U), vOptionList) GT 0 THEN
          ASSIGN vDate    = gvTime
                 vOptions = ENTRY(1, vText, " ":U)
          . /* ASSIGN */

/* msg (7129) Usr 5 set name to Idxanalys. */
          WHEN "7129":U THEN
          IF vText MATCHES " *analys.":U THEN
          ASSIGN vDate    = gvTime
                 i        = NUM-ENTRIES(vText, " ":U)
                 vOptions = ENTRY(i, vText, " ":U)
                 vOptions = TRIM(vOptions, ".":U)
          . /* ASSIGN */
        END CASE.

        IF vDbPath NE ? AND vDate NE ? THEN
        DO:
          ASSIGN
            vDate = gvTime WHEN gvLgProc NE "":U /* Time from .lg file */
            vOptions = vOptionList    + SUBSTRING(vOptions, 9)
                          WHEN vOptions BEGINS "dbanalys":U
            vOptions = "chanalys,":U  + SUBSTRING(vOptions, 9)
                          WHEN vOptions BEGINS "chanalys":U
            vOptions = "ixanalys,":U  + SUBSTRING(vOptions, 9)
                          WHEN vOptions BEGINS "ixanalys":U
            vOptions = "tabanalys,":U + SUBSTRING(vOptions, 10)
                          WHEN vOptions BEGINS "tabanalys":U
            vOptions = vOptionList    + SUBSTRING(vOptions, 28)
                          WHEN vOptions BEGINS "chanalys ixanalys tabanalys":U
            vStatId = GetStatId(? /*Host*/, vDbPath, String2DateTime(vDate),
                                ipInputFile)
          . /* ASSIGN */

          IF vStatId NE ? THEN
          FOR FIRST DbStat EXCLUSIVE-LOCK
              WHERE DbStat.StatId EQ vStatId:
            ASSIGN  DbStat.DbBlockSize = vBlockSize
                    DbStat.BgnTime     = String2DateTime(vDate)
                    DbStat.SourceType  = vOptions
                    vMyProc            = gvLgProc WHEN gvLgProc NE "":U
            . /* ASSIGN */
          END. /* FOR FIRST DbStat */

          ASSIGN vListOffset = ?
                 vBlockSize  = ?
                 vOptions    = ?
                 vDbPath     = ?
                 vDate       = ?
          . /* ASSIGN */
        END. /* IF vDbPath NE ? AND vDate NE ? */

        NEXT ImportLine.
      END. /* PROGRESS Database Analysis */

      WHEN "AREA CHAIN ANALYSIS":U THEN /* --------------------------------- */
/*
msg 14157: LIST OF FREE CLUSTER CHAIN BLOCKS
*/      IF vText EQ "LIST OF FREE CLUSTER CHAIN BLOCKS":U THEN
        ASSIGN vListOffset = SEEK(INPUT).
        ELSE
/*
msg #14161: %J cluster(s) found in the free cluster chain.
*/      IF vText MATCHES "* cluster(s) found in the free cluster chain*":U THEN
        DO:
           RUN GetChainStatV10(vStatId, vAreaNumber, vText, vListOffset).
           ASSIGN vListOffset = ?.
        END.
        ELSE
/*
msg  3873: FREE CHAIN ANALYSIS
msg  3892: RM CHAIN ANALYSIS
msg  7269: INDEX DELETE CHAIN ANALYSIS
*/      IF vText MATCHES "* CHAIN ANALYSIS":U THEN
        ASSIGN i           = LENGTH(vText)
               vChainType  = SUBSTRING(vText, 1, i - 9)
        . /* ASSIGN */
        ELSE
/*
msg  3880: LIST OF FREE CHAIN BLOCKS
msg  3897: LIST OF RM CHAIN BLOCKS
msg  7283: LIST OF INDEX DELETE CHAIN BLOCKS
*/      IF vText MATCHES "LIST OF * CHAIN BLOCKS":U THEN
        ASSIGN i           = LENGTH(vText)
               vChainType  = SUBSTRING(vText, 9, i - 15)
               vListOffset = SEEK(INPUT)
        . /* ASSIGN */
        ELSE
/*
msg  3891: %J block(s) found in the free chain.%r
msg 14155: %J block(s) found in the free chain of %s object %i
msg  3905: %J block(s) found in the RM chain.%r
msg 14172: %J block(s) found in the RM chain of %s object %i
msg  7270: %J block(s) found in the Index Delete chain.%r
msg 14176: %J block(s) found in the Index Delete chain of %s object %i
*/      IF vText MATCHES "* block(s) found in the * chain*":U THEN
        DO:
           RUN GetChainStatV10(vStatId, vAreaNumber, vText, vListOffset).
           ASSIGN vListOffset = ?.
        END.

        ELSE
/*
msg 16576: Number of          Object    Object
msg 16573: Number of          Object    Object * Partition/Tenant/Group
*/      IF vText MATCHES "Number of * Object * Object*":U THEN
        DO:
           RUN GetChainStatV11(vStatId, vAreaNumber, vChainType, vListOffset).
           ASSIGN vListOffset = ?.
        END.

/*    WHEN "AREA CHAIN ANALYSIS":U */
        
      WHEN "AREA BLOCK ANALYSIS":U THEN /* --------------------------------- */
/*
msg 10130: %J block(s) found in the area.%r           | Total blocks
msg 14181 Current high water mark: %J%r
msg 14182: %J free block(s) found in the area
msg 14183: %J record block(s) found in the area
msg 14184: %J index block(s) found in the area
msg 14185: %J empty block(s) found in the area
msg 14186: %J object block(s) found in the area       | bk_type 12
msg 14187: %J cluster list block(s) found in the area | bk_type 16
msg 14188: %J object list block(s) found in the area  | bk_type 14
msg 14189: %J cluster map block(s) found in the area  | bk_type 15

For a single area: -------------------------

msg 10130: %J block(s) found in the area.%r
msg 14181: Current high water mark: %J%r
*/      IF vText MATCHES "* block(s) found in the area*":U
        OR vText BEGINS "Current high water mark:":U THEN
        DO:
          IF gvTime NE ? THEN
          FOR FIRST AreaStat EXCLUSIVE-LOCK
              WHERE AreaStat.StatId     EQ vStatId
                AND AreaStat.AreaNumber EQ vAreaNumber
          TRANSACTION:
             ASSIGN AreaStat.EndTime = String2DateTime(gvTime).
          END.

          RUN GetBlockCount(vStatId, vAreaNumber, vText).
        END.
   /* WHEN "AREA BLOCK ANALYSIS":U  */

      WHEN "RECORD BLOCK SUMMARY":U THEN /* -------------------------------- */
/*
msg 14212: RECORD BLOCK SUMMARY FOR AREA "%s" : %l
*/      IF vText MATCHES "RECORD BLOCK SUMMARY FOR AREA *: *" THEN
        ASSIGN vAreaNumber = GetAreaNumber(vStatId, vText).
        ELSE
/*
msg  6132: Table   Records  Size  Min  Max  Mean  Count Factor  Factor
*/      IF vText MATCHES "Table * Records * Size * Min * Max * Mean *":U THEN
        RUN GetTableStat(vStatId, vAreaNumber).
        ELSE
/*
msg  3921: %J RM block(s) found in the database.
msg  9398: %J RM block(s) found in the storage area.
*/      IF vText MATCHES "* RM block(s) found in the *":U THEN
        DO:
          RUN GetBlockCount(vStatId, vAreaNumber, vText).
          ASSIGN vSegment = "BLOCK ANALYSIS":U.
        END.
   /* WHEN "RECORD BLOCK SUMMARY":U */

      WHEN "INDEX BLOCK SUMMARY":U THEN /* --------------------------------- */
/*
msg 14225: INDEX BLOCK SUMMARY FOR AREA "%s" : %l
*/      IF vText MATCHES "INDEX BLOCK SUMMARY FOR AREA *: *" THEN
        ASSIGN vAreaNumber = GetAreaNumber(vStatId, vText).
        ELSE
/*
msg  2313: %sIndex    Fields Levels          Blocks    Size  % Util  Factor
*/      IF vText MATCHES "*Index * Fields Levels * Blocks * Size *Util*":U THEN
        RUN GetIndexStat(vStatId, vAreaNumber).
        ELSE
/*
msg  3923: %J index block(s) found in the database.
msg  9399: %J index block(s) found in the storage area.
*/      IF vText MATCHES "* index block(s) found in the *":U THEN
        DO:
          RUN GetBlockCount(vStatId, vAreaNumber, vText).
          ASSIGN vSegment = "BLOCK ANALYSIS":U.
        END.
   /* WHEN "INDEX BLOCK SUMMARY":U ----------------------------------------- */

      WHEN "BLOCK ANALYSIS":U THEN /* -------------------------------------- */
/*
msg  3920: %J master block(s) found in the database.%r             | bk_type  1
msg  6706: %J area block(s) found in the database.                 | bk_type  9
msg  6708: %J control block(s) found in the database.              | bk_type 13
msg  6710: %J object block(s) found in the database.               | bk_type 12
msg 14197: %J cluster list block(s) found in the database.%r       | bk_type 16
msg 14198: %J cluster allocation block(s) found in the database.%r | bk_type 15
msg 14199: %J object block(s) found in the database.%r             | bk_type 12
msg 14200: %J object list block(s) found in the database.%r        | bk_type 14
msg 14201: %J object allocation block(s) found in the database.%r  | bk_type 17
msg 14202: %J row allocation block(s) found in the storage area.%r | bk_type 18

For a single area Type 1: ------------------

msg  9394: %J master block(s) found in the storage area.%r | in "Schema Area"
msg  9395: %J area block(s) found in the storage area.
msg  9396: %J control block(s) found in the storage area.  | in "Control Area"
msg  9397: %J object block(s) found in the storage area.

For a single area Type 2: ------------------

msg  9395: %J area block(s) found in the storage area.                 | bk_type  9
msg 14191: %J cluster list block(s) found in the storage area.%r       | bk_type 16
msg 14192: %J cluster allocation block(s) found in the storage area.%r | bk_type 15
msg 14193: %J object block(s) found in the storage area.%r             | bk_type 12
msg 14194: %J object list block(s) found in the storage area.%r        | bk_type 14
msg 14195: %J object allocation block(s) found in the storage area.%r  | bk_type 17
msg 14196: %J row allocation block(s) found in the storage area.%r     | bk_type 18

msg  3930: %J free block(s) found in the database.%r
msg  3931: %J index table block(s) found in the database.%r
msg  3932: %J sequence block(s) found in the database.%r
msg  3933: %J empty block(s) found in the database.%r
msg  3934: %j unknown type block(s) found in the database.%r (3934)
msg  3935: %j total blocks found in the database.%r
*/
        IF vText MATCHES "* block* found in the *":U THEN
        RUN GetBlockCount(vStatId, vAreaNumber, vText).
   /* WHEN "BLOCK ANALYSIS":U */

    END CASE. /* vSegment */

  END. /* ImportLine: REPEAT */

  INPUT CLOSE.

/* If the parsing of input file was interrupted due to the errors: */
  DO i = 1 TO ERROR-STATUS:NUM-MESSAGES
  TRANSACTION:
    CREATE DbStatError.
    ASSIGN DbStatError.StatId       = vStatId
           DbStatError.AreaNumber   = vAreaNumber
           DbStatError.ErrorNumber  = ERROR-STATUS:GET-NUMBER(1)
           DbStatError.ErrorSource  = gvLine
           DbStatError.ErrorMessage = ERROR-STATUS:GET-MESSAGE(1)
    . /* ASSIGN */
  END.

/* When the whole input file is processed the do the post load processing: */
  DO i = 1 TO NUM-ENTRIES(vStatIdList):
    ASSIGN vStatId = INTEGER(ENTRY(i, vStatIdList)).
    RUN PostLoadProcessing(vStatId).
  END.
  

END PROCEDURE. /* Dbanalys */

/* ------------------------------------------------------------------------- */

PROCEDURE PostLoadProcessing:

  DEFINE INPUT PARAMETER ipStatId AS INTEGER NO-UNDO.

  DEFINE VARIABLE vGroupEnabled AS LOGICAL NO-UNDO.
  DEFINE VARIABLE vBgnTime AS DATETIME NO-UNDO.
  DEFINE VARIABLE vEndTime AS DATETIME NO-UNDO.
  DEFINE VARIABLE vCount   AS INTEGER  NO-UNDO.
  DEFINE VARIABLE i        AS INTEGER  NO-UNDO.


  FIND FIRST DbStat NO-LOCK
       WHERE DbStat.StatId EQ ipStatId
  NO-ERROR.
  
  IF NOT AVAILABLE DbStat THEN
  RETURN.

  ASSIGN vGroupEnabled = FALSE.

/* Set AreaStat.NumTables ----------------- */
  FOR EACH AreaStat OF DbStat EXCLUSIVE-LOCK
     WHERE AreaStat.AreaNumber GE 6:

/* Table Count: */
    ASSIGN vCount = 0.

    ACCUMULATE AreaStat.HighBlock (TOTAL COUNT).

    IF LOOKUP("tabanalys":U, DbStat.SourceType) GT 0 THEN
    FOR EACH TableStat NO-LOCK
       WHERE TableStat.StatId     EQ AreaStat.StatId
         AND TableStat.AreaNumber EQ AreaStat.AreaNumber:

/* Ignore the metaschema tables: */
      IF  TableStat.AreaNumber EQ 6
      AND TableStat.TableName BEGINS "_":U THEN
      NEXT.

      ASSIGN vCount = vCount + 1
             vGroupEnabled = TRUE WHEN TableStat.TableGroup NE "":U
      . /* ASSIGN */
    END. /* FOR EACH TableStat  */
    ELSE

    IF LOOKUP("chanalys":U, DbStat.SourceType) GT 0 THEN
    FOR EACH ChainStat NO-LOCK
       WHERE ChainStat.StatId     EQ AreaStat.StatId
         AND ChainStat.AreaNumber EQ AreaStat.AreaNumber
         AND ChainStat.ChainType  EQ "RM chain":U
         AND ChainStat.ObjectType EQ "Table":U:
      ASSIGN vCount = vCount + 1.
    END. /* FOR EACH ChainStat  */
    ELSE

/* Chanalys of storage area type 2: */
    IF AreaStat.ObjectBlocks GT 1 THEN
    ASSIGN vCount = AreaStat.ObjectBlocks - AreaStat.NumIndexes - 1
               WHEN AreaStat.DataBlocks GT 0
    . /* ASSIGN */
    ELSE
/* Chanalys of storage area type 1: */
    ASSIGN vCount = ?.

    ASSIGN AreaStat.NumTables = vCount.

/* Index Count: */
    ASSIGN vCount = 0.

    IF LOOKUP("ixanalys":U, DbStat.SourceType) GT 0 THEN
    FOR EACH  IndexStat NO-LOCK
       WHERE  IndexStat.StatId     EQ AreaStat.StatId
         AND  IndexStat.AreaNumber EQ AreaStat.AreaNumber:

/* Ignore the indexes of system tables: */
      IF  IndexStat.IndexNumber LE 7
      OR (IndexStat.IndexNumber GE 994 AND IndexStat.IndexNumber LE 1093) THEN
      NEXT.
      
      ASSIGN vCount = vCount + 1
             vGroupEnabled = TRUE WHEN IndexStat.IndexGroup NE "":U
      . /* ASSIGN */
    END. /* FOR EACH IndexStat  */
    ELSE

    IF LOOKUP("chanalys":U, DbStat.SourceType) GT 0 THEN
    FOR EACH ChainStat NO-LOCK
       WHERE ChainStat.StatId     EQ AreaStat.StatId
         AND ChainStat.AreaNumber EQ AreaStat.AreaNumber
         AND ChainStat.ChainType  EQ "free chain":U
         AND ChainStat.ObjectType EQ "Index":U:
      ASSIGN vCount = vCount + 1
             vGroupEnabled = TRUE WHEN ChainStat.ObjectGroup NE "":U
      . /* ASSIGN */
    END. /* FOR EACH ChainStat  */
    ELSE

/* Tabanalys of storage area type 2: */
    IF AreaStat.ObjectBlocks GT 1 THEN
    ASSIGN vCount = AreaStat.ObjectBlocks - AreaStat.NumTables - 1
               WHEN AreaStat.IndexBlocks GT 0
    . /* ASSIGN */
    ELSE
/* Tabanalys of storage area type 1: */
    ASSIGN vCount = ?.

    ASSIGN AreaStat.NumIndexes = vCount.

/* Blob Count: */
    ASSIGN vCount = 0.

    IF LOOKUP("chanalys":U, DbStat.SourceType) GT 0 THEN
    FOR EACH ChainStat NO-LOCK
       WHERE ChainStat.StatId     EQ AreaStat.StatId
         AND ChainStat.AreaNumber EQ AreaStat.AreaNumber
         AND ChainStat.ChainType  EQ "RM chain":U
         AND ChainStat.ObjectType EQ "Blob":U:
      ASSIGN vCount = vCount + 1
             vGroupEnabled = TRUE WHEN ChainStat.ObjectGroup NE "":U
      . /* ASSIGN */
    END. /* FOR EACH ChainStat  */

    ASSIGN AreaStat.NumLobs = vCount.

/* Set TableStat.DataBlocks: */
    IF AreaStat.NumTables EQ 1 THEN
    FOR FIRST TableStat EXCLUSIVE-LOCK
        WHERE TableStat.StatId     EQ AreaStat.StatId
          AND TableStat.AreaNumber EQ AreaStat.AreaNumber:

      FIND FIRST ChainStat NO-LOCK /* INDEX ObjectNumber */
           WHERE ChainStat.StatId      EQ TableStat.StatId
             AND ChainStat.AreaNumber  EQ TableStat.AreaNumber
             AND ChainStat.ChainType   EQ "RM chain":U
             AND ChainStat.ObjectType  EQ "Table":U
             AND ChainStat.ObjectGroup EQ TableStat.TableGroup
      NO-ERROR.

      ASSIGN TableStat.DataBlocks  = AreaStat.DataBlocks
                                WHEN AreaStat.NumLobs EQ 0 
             TableStat.TableNumber = ChainStat.ObjectNumber
                      WHEN AVAILABLE ChainStat
      . /* ASSIGN */
    END.
    ELSE

/* Set LobStat.DataBlocks: */
    IF AreaStat.NumLobs EQ 1 AND AreaStat.NumTables EQ 0 THEN
    FOR FIRST LobStat EXCLUSIVE-LOCK
        WHERE LobStat.StatId     EQ AreaStat.StatId
          AND LobStat.AreaNumber EQ AreaStat.AreaNumber:
      ASSIGN  LobStat.DataBlocks =  AreaStat.DataBlocks.
    END.

  END. /* FOR EACH AreaStat */

/* Set AreaStat.ClusterSize and maybe AreaStat.RecPerBlock ----------------- */

  IF LOOKUP("chanalys":U, DbStat.SourceType) GT 0 THEN
  FOR EACH AreaStat OF DbStat EXCLUSIVE-LOCK
     WHERE AreaStat.AreaNumber GT 0:

    ASSIGN AreaStat.ClusterSize = 1
           AreaStat.EmptyBlocks = AreaStat.TotalBlocks - AreaStat.HighBlock
      WHEN AreaStat.EmptyBlocks EQ ?
    . /* ASSIGN */

/* "Control Area" and "Schema Area": */
    IF AreaStat.AreaNumber LE 6 THEN
    ASSIGN AreaStat.RecPerBlock = IF DbStat.DbBlockSize EQ 8192 THEN 64 ELSE 32.
    ELSE
    FOR FIRST ChainStat OF AreaStat NO-LOCK
        WHERE ChainStat.ChainType    EQ "free chain":U
          AND ChainStat.ObjectType   EQ "Master":U
          AND ChainStat.ObjectNumber EQ 0:

      IF ChainStat.ChainBlocks GE 64 THEN
      ASSIGN AreaStat.ClusterSize = 512.
      ELSE
      IF AreaStat.ObjectListBlocks NE ? THEN
/* Blocks in the first and the only cluster of Master object:

Block 1, bkDbkey n/a, bk_type 254 (Extent Header Block)
Block 2, bkDbkey  32, bk_type   9 (Area Block)
Block 3, bkDbkey  64, bk_type  12 (Object Block)
Block 4, bkDbkey  96, bk_type  16 (Cluster List Block)
Block 5, bkDbkey 128, bk_type  17 (Object allocation Block)
Block 6, bkDbkey 160, bk_type  15 (Cluster Allocation Block)
Block 7, bkDbkey 192, bk_type  14 (Object List Block)
Block 8, bkDbkey 224, bk_type   4 (Free Block)
*/    ASSIGN  
        AreaStat.ClusterSize = ChainStat.ChainBlocks /* <= free blocks */
                             + AreaStat.ObjectListBlocks
                             + 6
/* If there are many Object List Blocks then they can use a few clusters: */
        AreaStat.ClusterSize = IF AreaStat.ClusterSize LT 64  THEN 8  ELSE
                               IF AreaStat.ClusterSize LT 512 THEN 64 ELSE 512
      . /* ASSIGN */

/* If "LIST OF FREE CHAIN BLOCKS" was loaded then: */

      IF ChainStat.MaxDbkey NE ? THEN
      ASSIGN AreaStat.RecPerBlock = 
                      ROUND(ChainStat.MaxDbkey / (AreaStat.ClusterSize - 1), 0)
      . /* ASSIGN */
    END. /* FOR FIRST ChainStat */

/* Cosmetic changes of the unknown values to the zero values: */
    FOR EACH ChainStat OF AreaStat EXCLUSIVE-LOCK
       WHERE ChainStat.ChainBlocks EQ 0:

      ASSIGN ChainStat.SeekPath = 0
             ChainStat.MinDbkey = 0
             ChainStat.MaxDbkey = 0
      . /* ASSIGN */
    END. /* FOR EACH ChainStat OF AreaStat  */

  END. /* FOR EACH AreaStat */
  ELSE

/* If chanalys is not available: */
  FOR EACH AreaStat OF DbStat EXCLUSIVE-LOCK
     WHERE AreaStat.AreaNumber GT 0:

/* Storage area type 1: */
    IF AreaStat.ObjectListBlocks EQ 0 THEN
    ASSIGN AreaStat.ClusterSize = 1.
    ELSE

/* If there are no indexes then all free blocks belong to Master object
   or to Free Cluster chain. In last case the number of free blocks great
   than a cluster size:
*/  IF AreaStat.NumIndexes EQ 0 THEN
    DO:
      ASSIGN vCount = AreaStat.FreeBlocks + AreaStat.ObjectListBlocks + 6.
      CASE vCount:
        WHEN 8 OR WHEN 64 OR WHEN 512 THEN
        ASSIGN AreaStat.ClusterSize = vCount.
      END CASE.
    END.
    ELSE

/* Small number of empty blocks may exist for cluster size 8: */
    IF  AreaStat.EmptyBlocks GE 8
    AND AreaStat.EmptyBlocks LE 56 THEN
    ASSIGN AreaStat.ClusterSize = 8.
  END. /* FOR EACH AreaStat */

/* Set "Total per database": --------------------------------------------- */
  FOR EACH AreaStat OF DbStat NO-LOCK
     WHERE AreaStat.AreaNumber GE 6:
    ACCUMULATE AreaStat.NumTables  (TOTAL)
               AreaStat.NumIndexes (TOTAL)
               AreaStat.NumLobs    (TOTAL)
    . /* ACCUMULATE */
  END.

/* Total per database: */
  FOR EACH AreaStat OF DbStat EXCLUSIVE-LOCK
     WHERE AreaStat.AreaNumber EQ 0:
    ASSIGN AreaStat.NumTables   = ACCUM TOTAL AreaStat.NumTables
           AreaStat.NumIndexes  = ACCUM TOTAL AreaStat.NumIndexes
           AreaStat.NumLobs     = ACCUM TOTAL AreaStat.NumLobs
           AreaStat.HighBlock   = ACCUM TOTAL AreaStat.HighBlock
           AreaStat.BgnTime     = DbStat.BgnTime
           AreaStat.EndTime     = DbStat.EndTime
           AreaStat.RunInterval = DbStat.RunInterval
    . /* ASSIGN */
  END.

/* Set AreaStat.BgnTime if AreaStat.EndTime is known: */
  ASSIGN vBgnTime = DbStat.BgnTime.
  FOR EACH AreaStat OF DbStat EXCLUSIVE-LOCK
     WHERE AreaStat.AreaNumber GT 0
        BY AreaStat.StatId
        BY AreaStat.AreaNumber:
    ASSIGN AreaStat.BgnTime = vBgnTime WHEN AreaStat.BgnTime EQ ?
           vBgnTime = AreaStat.EndTime
    . /* ASSIGN */

  END. /* FOR EACH AreaStat */

/* Set AreaStat.EndTime if AreaStat.BgnTime is known: */
  ASSIGN vEndTime = DbStat.EndTime.
  FOR EACH AreaStat OF DbStat EXCLUSIVE-LOCK
     WHERE AreaStat.AreaNumber GT 0
        BY AreaStat.StatId     DESCENDING
        BY AreaStat.AreaNumber DESCENDING:
    ASSIGN AreaStat.EndTime = vEndTime WHEN AreaStat.EndTime EQ ?
           vEndTime = AreaStat.BgnTime
           AreaStat.RunInterval = INTERVAL(AreaStat.EndTime,
                                           AreaStat.BgnTime,
                                           "milliseconds":U) / 1000.0
    . /* ASSIGN */
  END. /* FOR EACH AreaStat */

  FOR EACH AreaStat OF DbStat NO-LOCK:

/* Set ChainStat.BgnTime if ChainStat.EndTime is known as in V10
where ChainStat.EndTime is the time of message:
%J block(s) found in the <ChainType> chain of %s object %i
*/
    ASSIGN vBgnTime = AreaStat.BgnTime.
    FOR EACH ChainStat OF AreaStat EXCLUSIVE-LOCK
          BY ChainStat.StatId
          BY ChainStat.ChainOrder:
      ASSIGN ChainStat.BgnTime = vBgnTime WHEN ChainStat.BgnTime EQ ?
             vBgnTime = ChainStat.EndTime
      . /* ASSIGN */
    END. /* FOR EACH ChainStat */

/* Set ChainStat.EndTime if ChainStat.BgnTime is known as in V11
where ChainStat.BgnTime is the time of:
LIST OF <ChainType> CHAIN BLOCKS
*/  ASSIGN vEndTime = AreaStat.EndTime.
/* Last ChainStat ends just before "AREA BLOCK ANALYSIS" phase: */
    REPEAT i = 1 TO EXTENT(AreaStat.PhaseName)
      WHILE AreaStat.PhaseTime[i] NE ?:
      IF AreaStat.PhaseName[i] NE "AREA BLOCK ANALYSIS":U THEN
      NEXT.
      ASSIGN vEndTime = AreaStat.PhaseTime[i].
      LEAVE.
    END.

    FOR EACH ChainStat OF AreaStat EXCLUSIVE-LOCK
          BY ChainStat.StatId     DESCENDING
          BY ChainStat.ChainOrder DESCENDING:
      ASSIGN ChainStat.EndTime = vEndTime WHEN ChainStat.EndTime EQ ?
             vEndTime = ChainStat.BgnTime
             ChainStat.RunInterval = INTERVAL(ChainStat.EndTime,
                                              ChainStat.BgnTime,
                                             "milliseconds":U) / 1000.0
      . /* ASSIGN */
    END. /* FOR EACH ChainStat */

  END. /* FOR EACH AreaStat */

  FOR FIRST DbStat EXCLUSIVE-LOCK
      WHERE DbStat.StatId EQ ipStatId
  TRANSACTION:
    ASSIGN  DbStat.NumAreas = ACCUM COUNT AreaStat.HighBlock.
  END. /* FOR FIRST DbStat */

END PROCEDURE. /* PostLoadProcessing */

/* ------------------------------------------------------------------------- */
