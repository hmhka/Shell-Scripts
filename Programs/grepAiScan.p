RUN LoadAreaInfo("path/to/your_db.st").

RUN grepAiScan(
  "/path/to/dir",  /* Input directory in which AI scan files are stored.     */
  ?,               /* MaxDirLevel: 1 - current dir only, ? - infinitely deep */
  "ai_scans~~~.*", /* Mask for input scan file names.                        */
  ?,               /* Output file name. Default: "<InputDir>.grepAiScan.txt" */
  "*",             /* Selection by TridList: Trid1,Trid2,...                 */
  "",              /*       or by DbkeyList: Area1:dbkey1,Area2:dbkey2,...   */
  "",              /*       or by  UserList: User1,User2,...                 */
  "00:00:00",      /* TimeFrom - time range of the notes to report.          */
  "24:00:00"       /* TimeTo                                                 */
).

/*------------------------------------------------------------------------
    Program     : grepAiScan.p
    Purpose     : Grep the specified transactions from ai scan output.

    Syntax      : See the examples above.

    Author(s)   : George Potemkin
    Created     : Jun 15, 2013
    Modified    : Feb 23, 2015 23:00
    Version     : 2.0.2

    Description :
To select the transactions you can specify the lists of Trids, Dbkeys or Users.
All three lists can use the masks as in CAN-DO function.
The unknown value of UserList will select all transactions that begun before
the processed AI files (in other words, when RL_TBGN notes were not found).
The result of selection by lists will be combined by a logical OR operator.
Selection by DbkeyList: all notes of the selected transactions will be reported.
It can be a bad idea to select all transactions - the output file can have
too much rows to be opened in Excel.
The output file is a tab separated list of note's attributes.
You can specify its name or it will be created based on the specified lists.
To mimimize the size of the output file you can specify the time frame:
TimeFrom, TimeTo.

Columns in the output file:
"AiNum"  - AI sequence number (Seqno) of the imported AI scan file;
"Line"   - Line number of the note in AI scan file;
"Note"   - Note number (count) inside the current transaction;
"Date"   - Date when the note was created;
"Time"   - Estimated time when the note was created;
"Gap"    - Time gap between any transaction notes arround the current note.
           It's an upper limit for precision of the note's time estimations;
"Intrv"  - Time interval since transaction's begin time (or since first note);
"Idle"   - Time interval since previous note of the same transaction.
           For RL_TEND rows the value in "Idle" column is the total idle time.
           So these rows provide the summary per transaction:
           note count per transaction, its duration and its total idle time;
"Trid"   - Transaction ID;
"Userid" - Userid who created the transaction;
"Area"   - Area number in the note (message 12529);
"Dbkey"  - Dbkey in the note (message 12529);
"UpdCtr" - Update counter (message 12529);
"Code"   - Note type  (message 12528).

Estimated execution time:
 2.5 MB/sec of AI scans or 4 times slower than 4GL "grep".
 The -tmpbsize 8 is recommended. Large -Bt or the -T on SSD can help as well.

------------------------------------------------------------------------ */

DEFINE STREAM ScanReport.

/* Trid: Trid code = NoteCode version = 2 (12528) */
DEFINE TEMP-TABLE ttNoteType NO-UNDO
  FIELD NoteCode AS INTEGER   /* Integer ID */
  FIELD NoteType AS CHARACTER /* Note type (RL_*) */
  INDEX NoteCode IS UNIQUE
        NoteCode
  INDEX NoteType IS UNIQUE
        NoteType
. /* DEFINE TEMP-TABLE ttNoteType */

/* Trid: TRID area = AREA   dbkey = DBKEY   update counter = UPDCTR (12529) */
DEFINE TEMP-TABLE ttNote NO-UNDO
  FIELD AiSeqNum   AS INTEGER  /* Seqno of current AI file                  */
  FIELD NoteId     AS INTEGER  /* Id (sequential number) of the note        */
  FIELD NoteTrid   AS INTEGER  /* Transaction ID in recovery note           */
  FIELD NoteCode   AS INTEGER  /* Code of note type                         */
  FIELD NoteArea   AS INTEGER  /* Area in recovery note                     */
  FIELD NoteDbkey  AS INT64    /* Dbkey in recovery note                    */
  FIELD NoteUpdCtr AS INTEGER  /* Update counter in recovery note           */
  FIELD NoteTime   AS DATETIME /* Time of tran note before the current note */
  FIELD TimeGap    AS INTEGER  /* Interval between two nearest tran notes   */
  FIELD TranId1    AS INTEGER  /* Id of tran note before the current note   */
  FIELD TranId2    AS INTEGER  /* Id of tran note after the current note    */
  INDEX LineNum IS UNIQUE
        AiSeqNum
        NoteId
  INDEX NoteTrid
        NoteTrid
. /* DEFINE TEMP-TABLE ttNote */

/* Trid: TRID User Id: USERID (12531) */
DEFINE TEMP-TABLE ttTran NO-UNDO
  FIELD Trid       AS INTEGER                  /* Transaction ID            */
  FIELD UserName   AS CHARACTER INITIAL ?      /* Userid who created tran   */
  FIELD IsSelected AS LOGICAL   INITIAL FALSE  /* Notes match the selection */
  INDEX Trid IS UNIQUE
        Trid
. /* DEFINE TEMP-TABLE ttTran */

DEFINE TEMP-TABLE ttOSFile NO-UNDO
  FIELD osFileName AS CHARACTER
  FIELD osFilePath AS CHARACTER
  FIELD osFileAttr AS CHARACTER
  INDEX osFileName
        osFileName
. /* DEFINE TEMP-TABLE ttOSFile */

DEFINE TEMP-TABLE ttArea NO-UNDO
  FIELD AreaNumber AS INTEGER
  FIELD AreaInfo   AS CHARACTER FORMAT "x(32)" /* LoadAreaInfo(dbname.st)*/
  INDEX AreaNumber IS UNIQUE
        AreaNumber
. /* DEFINE TEMP-TABLE ttArea */

DEFINE VARIABLE vFrameTitle AS CHARACTER NO-UNDO.

FORM vCnt  AS INTEGER   FORMAT ">>9"   LABEL "#"
     vFile AS CHARACTER FORMAT "x(24)" LABEL "File"
     vSize AS CHARACTER FORMAT "x(10)" LABEL "Size"
     vLoad AS CHARACTER FORMAT "x(8)"  LABEL "Load"
     vTime AS CHARACTER FORMAT "x(8)"  LABEL "Time"
WITH FRAME StatusFrame TITLE vFrameTitle 16 DOWN.

/* -------------------------------------------------------------------------- */

PROCEDURE LoadAreaInfo.
  DEFINE INPUT PARAMETER ipStructureFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vArea AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLine AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i     AS INTEGER   NO-UNDO.

  INPUT FROM VALUE(ipStructureFile).
  REPEAT TRANSACTION:
    ASSIGN vLine = "".
    IMPORT UNFORMATTED vLine.

/* d "Area Name":7,64;512 /path/to/dbname_7.d1 */
    IF NOT vLine BEGINS "d ":U
    OR NOT vLine MATCHES "*:*":U THEN
    NEXT.

    ASSIGN vLine = SUBSTRING(vLine, 3)
           vLine =      TRIM(vLine)
           i     =     INDEX(vLine, ":":U) + 1 /* first space after ":" */
           vLine = SUBSTRING(vLine, 1, INDEX(vLine + " ":U, " ":U, i) - 1)
           vArea = INTEGER(SUBSTRING(vLine, i,
                           MIN(INDEX(vLine + ",":U, ",":U),
                               INDEX(vLine + ";":U, ";":U)) - i))
   NO-ERROR.

   IF ERROR-STATUS:NUM-MESSAGES GT 0
   OR CAN-FIND(ttArea WHERE ttArea.AreaNumber EQ vArea) THEN
   NEXT.

   CREATE ttArea.
   ASSIGN ttArea.AreaNumber = vArea
          ttArea.AreaInfo   = vLine
   . /* ASSIGN */
  END.
END PROCEDURE. /* LoadAreaInfo */

/* ------------------------------------------------------------------------- */

PROCEDURE grepAiScan:

/* Path to the directory thar contains ai scan  files: */
  DEFINE INPUT PARAMETER ipInputDir AS CHARACTER NO-UNDO.

/* How deep to scan in subdirectories?
   Unknown value means to scan all subdirectories of vRootDir.
   vMaxDirLevel 1 means to scan vRootDir directory only. */
  DEFINE INPUT PARAMETER ipMaxDirLevel AS INTEGER NO-UNDO.

/* Process the files whose names match the specified mask: */
  DEFINE INPUT PARAMETER ipFileMask AS CHARACTER  NO-UNDO.

/* Name for the output file. It will be created in ipInputDir
   if the file location is not specified explicitly.
   If the file already exists it'll be DELETED. */
  DEFINE INPUT PARAMETER ipOutputFile AS CHARACTER NO-UNDO.

/* Grep the transactions with Trid from the specified list (CAN-DO): */
  DEFINE INPUT PARAMETER ipTridList AS CHARACTER NO-UNDO.

/* Grep the notes with "area:dbkey" pair from the specified list (CAN-DO): */
  DEFINE INPUT PARAMETER ipDbkeyList AS CHARACTER NO-UNDO.

/* Grep the transactions created by userid from the specified list (CAN-DO): */
  DEFINE INPUT PARAMETER ipUserList AS CHARACTER NO-UNDO.

/* Process the notes only for the specified time range: */
  DEFINE INPUT PARAMETER ipTimeFrom AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipTimeTo   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vPrevETime AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vFileCount AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNoteCount AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vOutputDir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vSlash     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i          AS INTEGER   NO-UNDO.

/* The lists (except UserList) with unknown values are treated as empty: */
  ASSIGN ipTridList  = "":U WHEN ipTridList  EQ ?
         ipDbkeyList = "":U WHEN ipDbkeyList EQ ?
         ipTridList  = REPLACE(ipTridList , " ":U, "":U)
         ipDbkeyList = REPLACE(ipDbkeyList, " ":U, "":U)
         ipUserList  = REPLACE(ipUserList,  " ":U, "":U)
         ipTimeFrom  = "00:00:00":U WHEN ipTimeFrom EQ ?
         ipTimeTo    = "24:00:00":U WHEN ipTimeTo   EQ ?
         vSlash = IF OPSYS EQ "Unix":U THEN "/":U ELSE "~\":U
         FILE-INFO:FILE-NAME = ipInputDir
  . /* ASSIGN */

  IF ipTridList + ipDbkeyList + ipUserList EQ "":U THEN
  DO:
    MESSAGE
      "At least one of the lists (Trid, Dbkey or User) should be non-empty."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    QUIT.
  END.

  IF FILE-INFO:FULL-PATHNAME EQ ? THEN
  DO:
    MESSAGE ipInputDir " does not exist!"
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    QUIT.
  END.

  ASSIGN ipInputDir = FILE-INFO:FULL-PATHNAME.

  IF ipOutputFile EQ ? THEN /* Default output file name: */
  ASSIGN
    vOutputDir = ipInputDir
    ipOutputFile = SUBSTRING(ipInputDir, R-INDEX(ipInputDir, vSlash) + 1)
                 + ".AiScan.Grep"
                 + (IF ipTridList EQ "":U THEN "":U ELSE
                 ".Trid_" + REPLACE(ipTridList, "*":U, "ALL":U))
/* DbkeyList: */
                 + (IF ipDbkeyList EQ "":U THEN "":U ELSE
                   ".Dbkey_" + REPLACE(ipDbkeyList, "*":U, "ALL":U))
/* UserList:  */
                 + (IF ipUserList EQ "":U THEN "":U ELSE
                    IF ipUserList EQ ?    THEN
                   ".User_unknown"       ELSE
                   ".User_" + REPLACE(ipUserList, "*":U, "ALL":U))
/* Time Range: */
                 + (IF  ipTimeFrom EQ "00:00:00":U
                    AND ipTimeTo   EQ "24:00:00":U THEN "":U ELSE
                   ".Time_" + REPLACE(ipTimeFrom + "-'" + ipTimeTo,":":U,"":U))
                 + ".txt":U
  . /* ASSIGN */
  ELSE
  ASSIGN i = R-INDEX(ipOutputFile, vSlash)
         vOutputDir = IF i EQ 0 THEN ipInputDir
                                ELSE SUBSTRING(ipOutputFile, 1, i - 1)
         ipOutputFile = SUBSTRING(ipOutputFile, i + 1)
  . /* ASSIGN */

  ASSIGN FILE-INFO:FILE-NAME = vOutputDir.
  IF FILE-INFO:FULL-PATHNAME EQ ? THEN
  DO:
    MESSAGE "Output directory does not exist!" SKIP vOutputDir
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    QUIT.
  END.

  ASSIGN ipOutputFile = FILE-INFO:FULL-PATHNAME + vSlash + ipOutputFile.

  RUN GetOSFiles(INPUT ipInputDir, INPUT ipFileMask, INPUT ipMaxDirLevel).

  ASSIGN vFileCount = 0.
  FOR EACH ttOSFile NO-LOCK:
    ASSIGN vFileCount = vFileCount + 1.
  END. /* FOR EACH ttOSFile */

  IF vFileCount EQ 0 THEN
  DO:
    MESSAGE
      SUBSTITUTE("No files (&1) found in dir:", ipFileMask) SKIP ipInputDir
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    QUIT.
  END.

  ASSIGN vFrameTitle =
         SUBSTITUTE("Processing &1 file(s)...", STRING(vFileCount)).

  OUTPUT STREAM ScanReport TO VALUE(ipOutputFile).

  PAUSE 0 BEFORE-HIDE.
  ASSIGN vPrevETime = ETIME
         vFileCount = 0
         vNoteCount = 0
  . /* ASSIGN */
  FOR EACH ttOSFile
     WHERE ttOSFile.osFilePath NE ipOutputFile
        BY ttOSFile.osFileName:

    ASSIGN vFileCount          = vFileCount + 1
           FILE-INFO:FILE-NAME = ttOSFile.osFilePath
    . /* ASSIGN */

    DISPLAY vFileCount          @ vCnt
            ttOSFile.osFileName @ vFile
            STRING(INTEGER(FILE-INFO:FILE-SIZE / 1024), ">,>>>,>>9K") @ vSize
            "0%":U              @ vLoad
            "":U                @ vTime
    WITH FRAME StatusFrame TITLE vFrameTitle.

    RUN ImportAiScan(INPUT ttOSFile.osFilePath,
                     INPUT ipTridList,
                     INPUT ipDbkeyList,
                     INPUT ipUserList,
                     INPUT ipTimeFrom,
                     INPUT ipTimeTo).

    DOWN WITH FRAME StatusFrame.
  END. /* FOR EACH ttOSFile */

  FOR EACH ttTran
     WHERE ttTran.IsSelected EQ TRUE
        BY ttTran.Trid:
    RUN ReportTran(ttTran.Trid, ipTimeFrom, ipTimeTo).
  END.

  OUTPUT STREAM ScanReport CLOSE.

  ASSIGN FILE-INFO:FILE-NAME = ipOutputFile.
  MESSAGE
    SUBSTITUTE("Total execution time: &1 sec",
               STRING(INTEGER((ETIME - vPrevETime) / 1000)), "HH:MM:SS":U) SKIP
    "File :" FILE-INFO:FULL-PATHNAME SKIP
    "Size :" TRIM(STRING(FILE-INFO:FILE-SIZE / 1024, ">>>,>>>,>>9K"))
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE. /* grepAiScan */

/* ------------------------------------------------------------------------- */

FUNCTION String2DateTime RETURNS DATETIME (INPUT ipString AS CHARACTER).
/* Input string must have a format like: Tue Jul 27 12:11:45 2004 */
  DEFINE VARIABLE vDateTime  AS DATETIME  NO-UNDO INITIAL ?.
  DEFINE VARIABLE vMonthList AS CHARACTER NO-UNDO
    INITIAL "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec".
  DEFINE VARIABLE vTime AS CHARACTER NO-UNDO.

  ASSIGN /*ipString = TRIM(REPLACE(ipString, "  ":U, " ":U))*/
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
/* Output string in format: Tue Jul 27 12:11:45 2004 */
  DEFINE VARIABLE vMonthList AS CHARACTER NO-UNDO
    INITIAL "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec".

  DEFINE VARIABLE vDayList AS CHARACTER NO-UNDO
    INITIAL "Sun,Mon,Tue,Wed,Thu,Fri,Sat".

  DEFINE VARIABLE vDate   AS DATE      NO-UNDO.
  DEFINE VARIABLE vTime   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vString AS CHARACTER NO-UNDO.

  ASSIGN vDate = DATE(ipDateTime)
         vTime = INTERVAL (ipDateTime, DATETIME(vDate), "seconds")
         vString = ENTRY(WEEKDAY(vDate), vDayList)
         + " ":U + ENTRY(MONTH(vDate), vMonthList)
         + " ":U + STRING(DAY(vDate), ">9":U)
         + " ":U + STRING(vTime, "HH:MM:SS":U)
         + " ":U + STRING(YEAR(vDate))
  NO-ERROR.
  RETURN vString.
END FUNCTION. /* DateTime2String */

/* ------------------------------------------------------------------------- */

PROCEDURE GetOSFiles.

  DEFINE INPUT PARAMETER ipInputDir     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFileMask    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMaxDirLevel AS INTEGER NO-UNDO.

  DEFINE VARIABLE vFileName AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE vFilePath AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE vFileAttr AS CHARACTER   NO-UNDO.

  ASSIGN ipMaxDirLevel = ipMaxDirLevel - 1.
  INPUT FROM OS-DIR(ipInputDir).
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

PROCEDURE ImportAiScan.

  DEFINE INPUT PARAMETER ipInputFile AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipTridList  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipDbkeyList AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipUserList  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipTimeFrom  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipTimeTo    AS CHARACTER NO-UNDO.

/* The estimation of the average length of line in ai scan: */
  DEFINE VARIABLE vLineLength AS DECIMAL   NO-UNDO INITIAL 60.0.
  DEFINE VARIABLE vFileSize   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vReportLine AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vPrevETime  AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vAiNewTime  AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vAiSeqNum   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLastTranId AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLastTime   AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vPrevTime   AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vTimeGap    AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vLine         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vItem         AS CHARACTER NO-UNDO EXTENT 13.
  DEFINE VARIABLE vNoteCount    AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vNoteTrid   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNoteArea   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNoteDbkey  AS INT64     NO-UNDO.
  DEFINE VARIABLE vNoteUpdCtr AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNoteCode   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNoteType   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vCharDbkey  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vUserid     AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  ASSIGN FILE-INFO:FILE-NAME = ipInputFile
         vFileSize   = FILE-INFO:FILE-SIZE / 100.0
         vReportLine = MAX(1000, vFileSize / vLineLength)
         vReportLine = MIN(vReportLine, 10000)
  . /*ASSIGN */

  INPUT FROM VALUE(ipInputFile).

/* Header of AI scan: ------------------------------------------------------ */

  ASSIGN vAiSeqNum   = ?
         vAiNewTime  = ?
         vNoteCount  = 0
         vPrevETime  = ETIME
  . /* ASSIGN */
  REPEAT:
    ASSIGN vLine = "":U.
    IMPORT UNFORMATTED vLine.
    ASSIGN vLine = TRIM(vLine).

    IF vLine BEGINS "Trid: ":U THEN
    LEAVE.

/*  This is aimage file number AISEQNUM since the last AIMAGE BEGIN. (1642) */
    IF vLine MATCHES "* (1642)":U THEN
    REPEAT i = NUM-ENTRIES(vLine, " ":U) - 1 TO 1 BY -1:
      ASSIGN vAiSeqNum = INTEGER(ENTRY(i, vLine, " ":U)) NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES EQ 0 THEN
      LEAVE.
    END.
    ELSE

/*  Last AIMAGE BEGIN Mon Feb 23 18:24:57 2015 (1640) 
    Last AIMAGE NEW Sat Nov 30 08:00:01 2013 (1641) */
    IF vLine MATCHES "* (1640)":U 
    OR vLine MATCHES "* (1641)":U THEN
    ASSIGN vLine = SUBSTRING(vLine, LENGTH(vLine) - 30, 24)
           vLine = TRIM(REPLACE(vLine, "  ":U, " ":U))
           vAiNewTime = String2DateTime(vLine)
    . /* ASSIGN */
    ELSE

    IF vLine BEGINS "Trid: ":U THEN
    LEAVE.

  END. /* REPEAT */

  IF SEEK(INPUT) EQ ? OR vAiSeqNum EQ ? OR vAiNewTime EQ ? THEN
  DO:
    MESSAGE
      "Failed to parse the AI header of" ipInputFile SKIP
      "Processing interrupted."
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    QUIT.
  END.

  FOR FIRST ttNote NO-LOCK
      WHERE ttNote.AiSeqNum EQ vAiSeqNum:
    MESSAGE
      SUBSTITUTE("AI SeqNo &1 was found in file &2",
                  vAiSeqNum, ipInputFile)                    SKIP
      "Last AIMAGE NEW:" DateTime2String(vAiNewTime)         SKIP
      "but the file with that number was already processed." SKIP
      "Processing interrupted."
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    QUIT.
  END.

/* Main body of AI scan: ------------------------------------------------------

Trid: 0 code = RL_AIEXT version = 2 (12528)
Trid: 0 dbkey = 0  update counter = 0 (12530)
Trid: 1796235913 code = RL_CXREM version = 2 (12528)
Trid: 1796235913 area = 10   dbkey = 69353255   update counter = 2295 (12529)
Trid: 1796235913 code = RL_LSTMOD version = 2 (12528)
Trid: 1796235913 area = 6   dbkey = 64   update counter = 99363 (12529)
Trid: 1796235913 code = RL_CXREM version = 2 (12528)
Trid: 1796235913 area = 10   dbkey = 74881282   update counter = 18790 (12529)
*/
  ASSIGN vLastTime = vAiNewTime.
  SEEK INPUT TO 0.

  ImportLine:
  REPEAT:
    ASSIGN vItem = "":U.
    IMPORT vItem.
    PROCESS EVENTS.

    IF vItem[1] NE "Trid:":U THEN
    NEXT.

    IF vNoteCount MOD vReportLine EQ 0 THEN
    DISPLAY
      STRING(SEEK(INPUT) / vFileSize, ">>9%")                    @ vLoad
      STRING(INTEGER((ETIME - vPrevETime) / 1000), "HH:MM:SS":U) @ vTime
    WITH FRAME StatusFrame.

/*  1   2    3  4     5       6    7 8    9
Trid: TRID code = NOTETYPE version = 1 (12528)
*/  IF vItem[9] EQ "(12528)":U THEN
    DO:
      ASSIGN vNoteType     = vItem[5]
             vNoteTrid     = INTEGER(vItem[2])
      . /* ASSIGN */

/* vNoteCode is an integer equivalent of vNoteType: */
      FIND FIRST ttNoteType NO-LOCK
           WHERE ttNoteType.NoteType EQ vNoteType
      NO-ERROR.
      IF AVAILABLE ttNoteType THEN
      ASSIGN vNoteCode = ttNoteType.NoteCode.
      ELSE
      DO TRANSACTION:
        ASSIGN vNoteCode = 0.
        FOR EACH ttNoteType NO-LOCK
              BY ttNoteType.NoteCode DESCENDING:
          ASSIGN vNoteCode = ttNoteType.NoteCode + 1.
          LEAVE.
        END.
        CREATE ttNoteType.
        ASSIGN ttNoteType.NoteCode = vNoteCode
               ttNoteType.NoteType = vNoteType
        . /* ASSIGN */
      END.
    END. /* Trid: <Trid> code = <name> version = <version> (12528) */
    ELSE

/* "System" notes: --------------------------------------------------------- */

/*  1   2    3   4 5    6       7    8 9   10
Trid: TRID dbkey = 0  update counter = 0 (12530)
*/  IF vItem[10] EQ "(12530)":U THEN
    DO TRANSACTION:

/*   IF vNoteTrid NE INTEGER(vItem[2]) THEN ... */

      FIND FIRST ttTran
           WHERE ttTran.Trid EQ vNoteTrid
      NO-ERROR.
      IF NOT AVAILABLE ttTran THEN
      DO:
        CREATE ttTran.
        ASSIGN ttTran.Trid = vNoteTrid
               ttTran.IsSelected = CAN-DO(ipTridList, vItem[2])
        . /* ASSIGN */
      END.

/* New service notes of Tran: ---------------------------------------------- */
      CREATE ttNote.
      ASSIGN ttNote.AiSeqNum   = vAiSeqNum
             ttNote.NoteId     = vNoteCount
             ttNote.NoteTrid   = vNoteTrid
             ttNote.NoteCode   = vNoteCode
             ttNote.NoteArea   = 0
             ttNote.NoteDbkey  = 0
             ttNote.NoteUpdCtr = 0
             ttNote.NoteTime   = vLastTime
             ttNote.TimeGap    = 0
             ttNote.TranId1    = vLastTranId
             vNoteCount        = vNoteCount + 1
      . /* ASSIGN */

      CASE vNoteType:
        WHEN "RL_TBGN":U  THEN
          ASSIGN ttTran.UserName   = vUserid
                 ttTran.IsSelected = ttTran.IsSelected OR
                              CAN-DO(ipUserList, vUserid) WHEN ipUserList NE ?
          . /* ASSIGN */

        WHEN "RL_TEND":U  THEN
          IF ttTran.IsSelected
          THEN RUN ReportTran(ttTran.Trid, ipTimeFrom, ipTimeTo).
          ELSE RUN DeleteTran(ttTran.Trid).
      END CASE.

    END. /* Trid: TRID dbkey = 0  update counter = 0 (12530) */
    ELSE

/* "Update" notes: --------------------------------------------------------- */

/*  1   2    3  4   5      6   7   8        9      10   11  12     13
Trid: TRID area = AREA   dbkey = DBKEY   update counter = UPDCTR (12529)
*/  IF vItem[13] EQ "(12529)":U THEN
    DO TRANSACTION:

/*   IF vNoteTrid NE INTEGER(vItem[2]) THEN ... need a sanity check? */

      ASSIGN vNoteArea   = INTEGER(vItem[5])
             vNoteDbkey  = INT64  (vItem[8])
             vCharDbkey  = vItem[5] + ":":U + vItem[8]
             vNoteUpdCtr = INTEGER(vItem[12])
      NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
      NEXT ImportLine.

      FIND FIRST ttTran
           WHERE ttTran.Trid EQ vNoteTrid
      NO-ERROR.
      IF NOT AVAILABLE ttTran THEN
      DO:
        CREATE ttTran.
        ASSIGN ttTran.Trid       = vNoteTrid
               ttTran.IsSelected = ipUserList EQ ? OR
                            CAN-DO(ipTridList, vItem[2])
        . /* ASSIGN */
      END.

/* New update note of Tran: ------------------------------------------------ */
      CREATE ttNote.
      ASSIGN ttNote.AiSeqNum   = vAiSeqNum
             ttNote.NoteId     = vNoteCount
             ttNote.NoteTrid   = vNoteTrid
             ttNote.NoteCode   = vNoteCode
             ttNote.NoteArea   = vNoteArea
             ttNote.NoteDbkey  = vNoteDbkey
             ttNote.NoteUpdCtr = vNoteUpdCtr
             ttNote.NoteTime   = vLastTime
             ttNote.TimeGap    = 0
             ttNote.TranId1    = vLastTranId
             ttTran.IsSelected = ttTran.IsSelected OR
                                 CAN-DO(ipDbkeyList, vCharDbkey)
             vNoteCount        = vNoteCount + 1
      . /* ASSIGN */

    END. /* Trid: <n> area = <n>  dbkey = <n>  update counter = <n> (12529) */
    ELSE

/*  1   2   3   4  5     6       7     8
Trid: TRID Sat Nov 30 08:00:01 2013. (2598)
*/  IF vItem[8] EQ "(2598)":U THEN
    DO:
      ASSIGN vPrevTime = vLastTime
             vLastTime = String2DateTime(
                         vItem[3] + " " + vItem[4] + " " + vItem[5] + " "
                       + vItem[6] + " " + TRIM(vItem[7], ".":U))
             vTimeGap = INTERVAL(vLastTime, vPrevTime, "seconds":U)
      . /* ASSIGN */

      IF vTimeGap GT 0 THEN
      FOR EACH ttNote
         WHERE ttNote.AiSeqNum EQ vAiSeqNum
           AND ttNote.NoteId   GT vLastTranId:

        ASSIGN ttNote.TimeGap = vTimeGap
               ttNote.TranId2 = vNoteCount
        . /* ASSIGN */
      END.

      ASSIGN vLastTranId = vNoteCount.
    END.
    ELSE

/* 1    2    3   4     5      6
Trid: TRID User Id: USERID (12531)
*/  IF vItem[6] EQ "(12531)":U THEN
    ASSIGN vUserid = vItem[5].

  END. /* ImportLine: REPEAT */

  INPUT CLOSE.

  DISPLAY
    "100%":U @ vLoad
    STRING(INTEGER((ETIME - vPrevETime) / 1000), "HH:MM:SS":U) @ vTime
  WITH FRAME StatusFrame.

END PROCEDURE. /* ImportAiScan */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportTran:

  DEFINE INPUT PARAMETER ipTrid      AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipTimeFrom  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipTimeTo    AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vNoteCount  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vBgnTime    AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vNoteTime   AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vPrevTime   AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vIdleTime   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vReportDate AS DATE      NO-UNDO.
  DEFINE VARIABLE vReportTime AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vCharTime   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vInterval   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vUserid     AS CHARACTER NO-UNDO.

&SCOPED-DEFINE Sep "~t"

  IF SEEK(ScanReport) EQ 0 THEN
  PUT STREAM ScanReport UNFORMATTED
           "AiNum"    /* AI sequence number (Seqno)                          */
    {&Sep} "Line"     /* Line number of the note in AI scan file             */
    {&Sep} "Note"     /* Note number inside the current transaction          */
    {&Sep} "Date"     /* Date when the note was created                      */
    {&Sep} "Time"     /* Estimated time when the note was created            */
    {&Sep} "Gap"      /* Time gap between tran notes arround the current note*/
    {&Sep} "Intrv"    /* Time interval since transaction's begin time        */
    {&Sep} "Idle"     /* Time interval since previous note of the same tran  */
    {&Sep} "Trid"     /* Transaction ID                                      */
    {&Sep} "Userid"   /* Userid who created the transaction                  */
    {&Sep} "Area"     /* Area number in the note (message 12529)             */
    {&Sep} "AreaInfo" /* Area info from .st file                             */
    {&Sep} "Dbkey"    /* Dbkey in the note (message 12529)                   */
    {&Sep} "UpdCtr"   /* Update counter (message 12529)                      */
    {&Sep} "Code"     /* Note type  (message 12528)                          */
  SKIP. /* PUT STREAM ScanReport */

  FOR FIRST ttTran
      WHERE ttTran.Trid EQ ipTrid
  TRANSACTION:
    ASSIGN vUserid = ttTran.UserName.
    DELETE ttTran.
  END.

  ASSIGN vBgnTime   = ?
         vNoteCount = 0
  . /* ASSIGN */
  FOR EACH ttNote
     WHERE ttNote.NoteTrid EQ ipTrid,

     FIRST ttNoteType NO-LOCK
     WHERE ttNoteType.NoteCode EQ ttNote.NoteCode

        BY ttNote.AiSeqNum
        BY ttNote.NoteId
  TRANSACTION:

    ASSIGN
      vNoteCount  = vNoteCount + 1
      vPrevTime   = vNoteTime
      vNoteTime   = IF ttNote.TimeGap LE 0
                    THEN ttNote.NoteTime
                    ELSE ttNote.NoteTime + INTEGER(
                          ttNote.TimeGap * 1000.0
                       * (ttNote.NoteId  - ttNote.TranId1)
                       / (ttNote.TranId2 - ttNote.TranId1))
      vBgnTime    = vNoteTime WHEN vBgnTime EQ ?
      vInterval   = INTERVAL(vNoteTime, vBgnTime,  "seconds":U)
      vIdleTime   = INTERVAL(vNoteTime, vPrevTime, "seconds":U)
      vReportDate = DATE(vNoteTime)
      vReportTime = INTERVAL(vNoteTime, DATETIME(vReportDate), "seconds":U)
      vCharTime   = STRING(vReportTime, "HH:MM:SS":U)
    . /* ASSIGN */

    IF vIdleTime LE 1
    OR vIdleTime EQ ?
    THEN ASSIGN vIdleTime = 0.
    ELSE ACCUMULATE vIdleTime (TOTAL).

    FIND FIRST ttArea NO-LOCK
         WHERE ttArea.AreaNumber EQ ttNote.NoteArea
    NO-ERROR.

    IF vCharTime GE ipTimeFrom AND vCharTime LE ipTimeTo THEN
    PUT STREAM ScanReport UNFORMATTED
             /*AiNum   */ ttNote.AiSeqNum
      {&Sep} /*Line    */ ttNote.NoteId
      {&Sep} /*Note    */ vNoteCount
      {&Sep} /*Date    */ vReportDate
      {&Sep} /*Time    */ vCharTime
      {&Sep} /*Gap     */ ttNote.TimeGap
      {&Sep} /*Intrv   */ vInterval
      {&Sep} /*Idle    */ IF ttNoteType.NoteType EQ "RL_TEND":U
                          THEN ACCUM TOTAL vIdleTime
                          ELSE vIdleTime
      {&Sep} /*Trid    */ ttNote.NoteTrid
      {&Sep} /*Userid  */ vUserid
      {&Sep} /*Area    */ ttNote.NoteArea
      {&Sep} /*AreaInfo*/ IF AVAILABLE ttArea THEN ttArea.AreaInfo ELSE "":U
      {&Sep} /*Dbkey   */ ttNote.NoteDbkey
      {&Sep} /*UpdCtr  */ ttNote.NoteUpdCtr
      {&Sep} /*Code    */ ttNoteType.NoteType
    SKIP. /* PUT STREAM ScanReport */

    DELETE ttNote.

  END. /* FOR EACH ttNote */

END PROCEDURE. /* ReportTran */

/* ------------------------------------------------------------------------- */

PROCEDURE DeleteTran:

  DEFINE INPUT PARAMETER ipTrid AS INTEGER NO-UNDO.

  FOR FIRST ttTran
      WHERE ttTran.Trid EQ ipTrid
  TRANSACTION:
    DELETE ttTran.
  END.

  FOR EACH ttNote
     WHERE ttNote.NoteTrid EQ ipTrid
  TRANSACTION:
    DELETE ttNote.
  END. /* FOR EACH ttNote */

END PROCEDURE. /* DeleteTran */

/* ------------------------------------------------------------------------- */

