RUN ReadAiDir(
  "/path/to/dir",  /* Input directory in which AI scan files are stored.     */
  ?,               /* MaxDirLevel: 1 - current dir only, ? - infinitely deep */
  "ai_scans~~~.*", /* Mask for input scan file names.                        */
  ?                /* Output file. Default: "<InputDir>.AiScanStat.Files.txt"*/
).

/* ----------------------------------------------------------------------------
    Program     : AiScanInfo.p
    Purpose     : Read information from the headers and tailers of the ai scans
                  and check the consistency of the files:
                  the same AIMAGE BEGIN, the sequential AiSeqNum and
                  the same numbers of transactions on the border of AI files.

    Syntax      : See the examples above.

    Description : The output file can be opened in Excel.

    Author(s)   : George Potemkin
    Created     : Febrary 27, 2014
    Modified    : Febrary 28, 2014 13:00
    Version     : 1.0

---------------------------------------------------------------------------- */

DEFINE STREAM FilesReport.

DEFINE TEMP-TABLE ttAiFile
  FIELD AiSeqNum   AS INTEGER   /* AI sequence number (Seqno)       */
  FIELD AiFileName AS CHARACTER /* Name of AI scan file             */
  FIELD AiFileSize AS DECIMAL   /* Size of AI scan file             */
  FIELD AiBegTime  AS CHARACTER /* Time of aimage begin             */
  FIELD AiOpenTime AS CHARACTER /* Time of db restart               */
  FIELD AiNewTime  AS CHARACTER /* Time of aimage new               */
  FIELD NoteCount  AS INTEGER   /* Total note count in AI scan      */
  FIELD TBgnCount  AS INTEGER   /* Transactions started (1635)      */
  FIELD TEndCount  AS INTEGER   /* Transactions completed (11138)   */
  FIELD TranAtBgn  AS INTEGER   /* Transactions at beginning (3785) */
  FIELD TranAtEnd  AS INTEGER   /* Transactions at the end (1636)   */
  INDEX AiSeqNum
        AiSeqNum
. /* DEFINE TEMP-TABLE ttAiFile */

DEFINE TEMP-TABLE ttOSFile NO-UNDO /* used by GetOSFiles() procedure */
  FIELD osFileName AS CHARACTER
  FIELD osFilePath AS CHARACTER
  FIELD osFileAttr AS CHARACTER
  INDEX osFileName
        osFileName
. /* DEFINE TEMP-TABLE ttOSFile */

/* ------------------------------------------------------------------------- */

PROCEDURE ImportAiScan.

  DEFINE INPUT PARAMETER ipInputFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vFileName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFileSize   AS DECIMAL   NO-UNDO.

  DEFINE VARIABLE vAiBegTime  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vAiNewTime  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vAiOpenTime AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE vAiSeqNum   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNoteCount  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTBgnCount  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTEndCount  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTranAtBgn  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTranAtEnd  AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vLine       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i           AS INTEGER NO-UNDO.

  ASSIGN FILE-INFO:FILE-NAME = ipInputFile
         vFileSize = FILE-INFO:FILE-SIZE
         vFileName = IF OPSYS EQ "Unix":U THEN "/":U ELSE "~\":U /*slash*/
         vFileName = SUBSTRING(FILE-INFO:FULL-PATHNAME,
                       R-INDEX(FILE-INFO:FULL-PATHNAME, vFileName) + 1)
  . /*ASSIGN */

  INPUT FROM VALUE(ipInputFile).

/* Header of AI scan: ---------------------------------------------------------

    Last AIMAGE BEGIN Thu Jun 17 21:44:19 2010 (1640)
    Last AIMAGE NEW Sat Nov 30 08:00:01 2013 (1641)
    This is aimage file number 59926 since the last AIMAGE BEGIN. (1642)
    This file was last opened for output on Sat Nov 30 08:00:01 2013. (1643)
*/
  ASSIGN vAiSeqNum   = ?
         vAiBegTime  = ?
         vAiNewTime  = ?
         vAiOpenTime = ?
  . /* ASSIGN */
  REPEAT:
    ASSIGN vLine = "":U.
    IMPORT UNFORMATTED vLine.
    ASSIGN vLine = TRIM(vLine).

    IF vLine BEGINS "Trid: ":U THEN
    LEAVE.

/* Last AIMAGE BEGIN Thu Jun 17 21:44:19 2010 (1640) */
    IF vLine MATCHES "* (1640)":U THEN
    ASSIGN vAiBegTime = SUBSTRING(vLine, LENGTH(vLine) - 30, 24).
    ELSE

/* Last AIMAGE NEW Sat Nov 30 08:00:01 2013 (1641) */
    IF vLine MATCHES "* (1641)":U THEN
    ASSIGN vAiNewTime = SUBSTRING(vLine, LENGTH(vLine) - 30, 24).
    ELSE

/* This file was last opened for output on Sat Nov 30 08:00:01 2013. (1643) */
    IF vLine MATCHES "* (1643)":U THEN
    ASSIGN vAiOpenTime = SUBSTRING(vLine, LENGTH(vLine) - 31, 24).
    ELSE

/* This is aimage file number 59926 since the last AIMAGE BEGIN. (1642) */
    IF vLine MATCHES "* (1642)":U THEN
    REPEAT i = NUM-ENTRIES(vLine, " ":U) - 1 TO 1 BY -1:
      ASSIGN vAiSeqNum = INTEGER(ENTRY(i, vLine, " ":U)) NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES EQ 0 THEN
      LEAVE.
    END.
  END. /* REPEAT */

  IF vAiSeqNum   EQ ?
  OR vAiBegTime  EQ ?
  OR vAiNewTime  EQ ?
  OR vAiOpenTime EQ ? THEN
  DO:
    MESSAGE
      "Failed to parse the AI header of" vFileName SKIP
      "Processing interrupted."
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    QUIT.
  END.

  IF SEEK(INPUT) EQ ? THEN
  DO:
    MESSAGE
      "Unexpected structure of" vFileName SKIP
      "Processing interrupted."
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    QUIT.
  END.

/* Move to the tailer: */
  SEEK INPUT TO END.
  SEEK INPUT TO SEEK(INPUT) - 512.

/* Tail of AI scan: -----------------------------------------------------------

6523036 notes were processed. (1634)
2 in-flight transactions. (3785)
1670 transactions were started. (1635)
1669 transactions were completed. (11138)
At the end of the .ai file, 3 transactions were still active. (1636)
*/
  ASSIGN vNoteCount = ?
         vTranAtBgn = ?
         vTranAtEnd = ?
         vTBgnCount = ?
         vTEndCount = ?
  . /* ASSIGN */
  REPEAT:

    ASSIGN vLine = "":U.
    IMPORT UNFORMATTED vLine.

    IF vLine BEGINS "Trid: ":U THEN
    NEXT.

/* 6523036 notes were processed. (1634) */
    IF vLine MATCHES "* (1634)":U THEN
    REPEAT i = 1 TO NUM-ENTRIES(vLine, " ":U) - 1:
      ASSIGN vNoteCount = INTEGER(ENTRY(i, vLine, " ":U)) NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES EQ 0 THEN
      LEAVE.
    END.
    ELSE

/* 2 in-flight transactions. (3785) */
    IF vLine MATCHES "* (3785)":U THEN
    REPEAT i = 1 TO NUM-ENTRIES(vLine, " ":U) - 1:
      ASSIGN vTranAtBgn = INTEGER(ENTRY(i, vLine, " ":U)) NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES EQ 0 THEN
      LEAVE.
    END.
    ELSE

/* 1670 transactions were started. (1635) */
    IF vLine MATCHES "* (1635)":U THEN
    REPEAT i = 1 TO NUM-ENTRIES(vLine, " ":U) - 1:
      ASSIGN vTBgnCount = INTEGER(ENTRY(i, vLine, " ":U)) NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES EQ 0 THEN
      LEAVE.
    END.
    ELSE

/* 1669 transactions were completed. (11138) */
    IF vLine MATCHES "* (11138)":U THEN
    REPEAT i = 1 TO NUM-ENTRIES(vLine, " ":U) - 1:
      ASSIGN vTEndCount = INTEGER(ENTRY(i, vLine, " ":U)) NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES EQ 0 THEN
      LEAVE.
    END.
    ELSE

/* At the end of the .ai file, 3 transactions were still active. (1636) */
    IF vLine MATCHES "* (1636)":U THEN
    REPEAT i = 1 TO NUM-ENTRIES(vLine, " ":U) - 1:
      ASSIGN vTranAtEnd = INTEGER(ENTRY(i, vLine, " ":U)) NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES EQ 0 THEN
      LEAVE.
    END.

  END. /* REPEAT */

  INPUT CLOSE.

  DO TRANSACTION:
    CREATE ttAiFile.
    ASSIGN ttAiFile.AiSeqNum     = vAiSeqNum
           ttAiFile.AiFileName   = vFileName
           ttAiFile.AiFileSize   = vFileSize
           ttAiFile.AiBegTime    = vAiBegTime
           ttAiFile.AiOpenTime   = vAiOpenTime
           ttAiFile.AiNewTime    = vAiNewTime
           ttAiFile.NoteCount    = vNoteCount
           ttAiFile.TBgnCount    = vTBgnCount
           ttAiFile.TEndCount    = vTEndCount
           ttAiFile.TranAtBgn    = vTranAtBgn
           ttAiFile.TranAtEnd    = vTranAtEnd
    . /* ASSIGN */
  END. /* TRANSACTION */

END PROCEDURE. /* ImportAiScan */


/* ------------------------------------------------------------------------- */

PROCEDURE FileReports:

  &SCOPED-DEFINE Sep "~t"

  DEFINE VARIABLE vError AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vPrevRecid AS RECID NO-UNDO.
  DEFINE BUFFER  bfPrevFile FOR ttAiFile.

  IF SEEK(FilesReport) EQ 0 THEN /* ---------------------------------------- */
  PUT STREAM FilesReport UNFORMATTED
           "AiScan"      /* Name of AI file scan                             */
    {&Sep} "Size"        /* Size of AI scan                                  */
    {&Sep} "AiNum"       /* AI sequence number (Seqno)                       */
    {&Sep} "BeginTime"   /* Time of rfutil aimage begin                      */
    {&Sep} "NewTime"     /* Time of rfutil aimage new                        */
    {&Sep} "OpenTime"    /* Time of db restart (if/when AI was current)      */
    {&Sep} "Notes"       /* Note count in AI scan                            */
    {&Sep} "TranStart"   /* Number of transactions started in AI file        */
    {&Sep} "TranCommit"  /* Number of transactions committed in AI file      */
    {&Sep} "TranAtBgn"   /* Number of the in-flight transactions (at begin)  */
    {&Sep} "TranAtEnd"   /* Number of transactions still active at the end   */
  SKIP. /* PUT STREAM FilesReport */

  ASSIGN vPrevRecid = ?
         vError = "":U
  . /* ASSIGN */
  FOR EACH ttAiFile NO-LOCK
        BY ttAiFile.AiSeqNum:

    IF vPrevRecid NE ? THEN
    FOR FIRST bfPrevFile NO-LOCK WHERE RECID(bfPrevFile) EQ vPrevRecid:
      ASSIGN
        vError = vError + ", Mismatched AIMAGE BEGIN time!"
                             WHEN ttAiFile.AiBegTime NE bfPrevFile.AiBegTime
        vError = vError + ", AiSeqNum are not sequential!"
                             WHEN ttAiFile.AiSeqNum  NE bfPrevFile.AiSeqNum + 1
        vError = vError + ", Mismatched numbers of transactions!"
                             WHEN ttAiFile.TranAtBgn NE bfPrevFile.TranAtEnd
      . /* ASSIGN */
    END. /* FOR FIRST bfPrevFile */

    PUT STREAM FilesReport UNFORMATTED
             /*AiScan    */ ttAiFile.AiFileName
      {&Sep} /*Size      */ ttAiFile.AiFileSize
      {&Sep} /*AiNum     */ ttAiFile.AiSeqNum
      {&Sep} /*BeginTime */ ttAiFile.AiBegTime
      {&Sep} /*NewTime   */ ttAiFile.AiNewTime
      {&Sep} /*OpenTime  */ IF ttAiFile.AiOpenTime NE ttAiFile.AiNewTime THEN
                            ttAiFile.AiOpenTime ELSE "":U
      {&Sep} /*Notes     */ ttAiFile.NoteCount
      {&Sep} /*TranStart */ ttAiFile.TBgnCount
      {&Sep} /*TranCommit*/ ttAiFile.TEndCount
      {&Sep} /*TranAtBgn */ ttAiFile.TranAtBgn
      {&Sep} /*TranAtEnd */ ttAiFile.TranAtEnd
    . /* PUT STREAM FilesReport */

    IF vError NE "":U THEN
    DO:
      ASSIGN vError = SUBSTRING(vError, 3).
      PUT STREAM FilesReport UNFORMATTED {&Sep} vError.

      MESSAGE
        vError                                 SKIP(1)
        "Previous file:" bfPrevFile.AiFileName SKIP
        " Current file:"   ttAiFile.AiFileName SKIP
        " AIMAGE BEGIN:" bfPrevFile.AiBegTime  SKIP
        " AIMAGE BEGIN:"   ttAiFile.AiBegTime  SKIP
        " AIMAGE   NEW:" bfPrevFile.AiNewTime  SKIP
        " AIMAGE   NEW:"   ttAiFile.AiNewTime  SKIP
        "     AiSeqNum:" bfPrevFile.AiSeqNum   SKIP
        "     AiSeqNum:"   ttAiFile.AiSeqNum   SKIP
        " Trans at end:" bfPrevFile.TranAtEnd  SKIP
        " Trans at bgn:"   ttAiFile.TranAtBgn
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    END. /* IF vError NE "" */

    PUT STREAM FilesReport UNFORMATTED SKIP.

    ASSIGN vPrevRecid = RECID(ttAiFile)
           vError = "":U.
  END. /* FOR EACH ttAiFile  */

END PROCEDURE. /* FileReports */

/* ------------------------------------------------------------------------- */

PROCEDURE ReadAiDir.
  DEFINE INPUT PARAMETER ipInputDir    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMaxDirLevel AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipFileMask    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipOutputFile  AS CHARACTER NO-UNDO.
  /*
ipInputDir     - Root directory with the files to read from;
ipMaxDirLevel  - 1 = search in current dir only, ? = search in all subdirs;
ipFileMask     - Mask for input files;
ipOutputDir    - Directory for report files. Default is ipInputDir;
ipOutPrefix    - Prefix for output files. Default is "AiScanStat".
*/
  DEFINE VARIABLE vOutputDir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFileCount AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vSlash     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i          AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vErrorDesc AS CHARACTER NO-UNDO EXTENT 18 INITIAL [
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

  ASSIGN vSlash = IF OPSYS EQ "Unix":U THEN "/":U ELSE "~\":U
         FILE-INFO:FILE-NAME = ipInputDir
  . /* ASSIGN */
  IF FILE-INFO:FULL-PATHNAME EQ ? THEN
  DO:
    MESSAGE "Input directory does not exist!" SKIP ipInputDir
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    QUIT.
  END.

  ASSIGN ipInputDir = FILE-INFO:FULL-PATHNAME.

  IF ipOutputFile EQ ? THEN
  ASSIGN vOutputDir = ipInputDir
         ipOutputFile = SUBSTRING(ipInputDir, R-INDEX(ipInputDir, vSlash) + 1)
                      + ".AiScanStat.Files.txt"
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
  FOR EACH ttOSFile NO-LOCK WHERE
       NOT ttOSFile.osFilePath MATCHES ipOutputFile:
    ASSIGN vFileCount = vFileCount + 1.
  END. /* FOR EACH ttOSFile */

  IF vFileCount EQ 0 THEN
  DO:
    MESSAGE
      SUBSTITUTE("No files (&1) found in dir:", ipFileMask) SKIP ipInputDir
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    QUIT.
  END.

  PAUSE 0 BEFORE-HIDE.

  FOR EACH ttOSFile NO-LOCK WHERE
       NOT ttOSFile.osFilePath MATCHES ipOutputFile:
    RUN ImportAiScan(INPUT ttOSFile.osFilePath).
  END. /* FOR EACH ttOSFile */

  OUTPUT STREAM FilesReport  TO VALUE(ipOutputFile).
  RUN FileReports.
  OUTPUT STREAM FilesReport CLOSE.

  BELL.
  MESSAGE SUBSTITUTE("Checked &1 file(s)", vFileCount) SKIP
         "Report in the file:" ipOutputFile
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE. /* ReadAiDir */

/* ------------------------------------------------------------------------- */

PROCEDURE GetOSFiles.

  DEFINE INPUT PARAMETER ipInputDir    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFileMask    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMaxDirLevel AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vFileName AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFilePath AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFileAttr AS CHARACTER NO-UNDO.

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

