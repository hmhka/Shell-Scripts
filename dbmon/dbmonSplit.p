RUN DbMonSplit("path/to/dir", 1, ?). /* See description below */

/* ----------------------------------------------------------------------------
    File        : dbmonSplit.p
    Purpose     : Split promon output file into sections per promon menus.

    Author(s)   : George Potemkin
    Created     : Aug 08, 2013
    Modified    : Jun 22, 2016
    Version     : 1.1
    
    The most recent version can be downloaded from here:
    ftp://ftp.progress-tech.ru/pub/Users/george/Scripts/dbmon/
    
    Syntax      : See the examples above.
    
    Input parameters:
    1 = RootDir: root directory for the promon's log files;
    2 = MaxDepth: descend  at most levels of the specified directory.
        MaxDepth = 1 - process the contents of the specified directory only.
        MaxDepth = ? - process all subdirectories;
    3 = FileMask: mask of the promon's log files.
        Empty or unknown value means the default: '*~~.promon~~.*log'
*/

/* ------------------------------------------------------------------------- */

DEFINE TEMP-TABLE ttOSFile NO-UNDO
  FIELD osFileName AS CHARACTER
  FIELD osFilePath AS CHARACTER
  FIELD osFileAttr AS CHARACTER
  INDEX osFileName
        osFileName
. /* DEFINE TEMP-TABLE ttOSFile */

/* ------------------------------------------------------------------------- */

PROCEDURE GetOSFiles.

  DEFINE INPUT PARAMETER ipRootDir     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFileMask    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMaxDirLevel AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vFileName AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFilePath AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFileAttr AS CHARACTER NO-UNDO.

  ASSIGN ipMaxDirLevel = ipMaxDirLevel - 1.
  INPUT FROM OS-DIR(ipRootDir).
  REPEAT:
    IMPORT  vFileName vFilePath vFileAttr.

    IF  vFileAttr MATCHES "*D*" 
    AND vFileName NE ".":U
    AND vFileName NE "..":U 
    AND (ipMaxDirLevel EQ ? OR ipMaxDirLevel GT 0) THEN
    RUN GetOSFiles(INPUT vFilePath,
                   INPUT ipFileMask,
                   INPUT ipMaxDirLevel).
    ELSE
    IF  vFileAttr MATCHES "*F*"
    AND vFileName MATCHES ipFileMask
    THEN
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

DEFINE STREAM InpFile.
DEFINE STREAM OutFile.

DEFINE TEMP-TABLE ttHeader NO-UNDO
  FIELD HeaderLine AS INTEGER
  FIELD HeaderText AS CHARACTER
  INDEX HeaderLine
        HeaderLine
. /* DEFINE TEMP-TABLE ttHeader */

/* ------------------------------------------------------------------------- */

PROCEDURE SplitPromonOutput.

  DEFINE INPUT PARAMETER ipInputFile AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE vOutputFile   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vOutputPrefix AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vNeedHeader   AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE vInputLine    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vInputText    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vPromonMenu   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDirName      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDbName       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vSlash        AS CHARACTER   NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  ASSIGN 
     vSlash = IF OPSYS MATCHES "UNIX" THEN "/":U ELSE "~\":U
     FILE-INFO:FILE-NAME = ipInputFile
  . /* ASSIGN */
  IF FILE-INFO:FULL-PATHNAME EQ ? THEN RETURN.
/*
The expected format of an input file name:
1. dbmon.hosthame.date_time.dbname.promon.log
2. dbname.promon.date_time.log
*/
  ASSIGN i = R-INDEX(ipInputFile, vSlash)
         vDirName = SUBSTRING(ipInputFile, 1, i) /* with a trailing slash */
         vOutputPrefix  = SUBSTRING(ipInputFile, i + 1)
         i = R-INDEX(vOutputPrefix, ".":U) /* Cut off a file extension */
         vOutputPrefix  = SUBSTRING(vOutputPrefix, 1, i - 1)
         i = NUM-ENTRIES(vOutputPrefix, ".":U)
  . /* ASSIGN */

/* dbmon.hosthame.date_time.dbname.promon.log */
  IF i GE 2 AND ENTRY(i, vOutputPrefix, ".":U) EQ "promon":U THEN
  ASSIGN vDbName  = ENTRY(i - 1, vOutputPrefix, ".":U)
         i = R-INDEX(vOutputPrefix, ".promon":U )
         vOutputPrefix  = SUBSTRING(vOutputPrefix, 1, i - 1)
         vDirName = vDirName + vDbName
  . /* ASSIGN */
  ELSE
/* dbname.promon.date_time.log */
  IF i EQ 3 AND ENTRY(2, vOutputPrefix, ".":U) EQ "promon":U THEN
  ASSIGN vDbName  = ENTRY(1, vOutputPrefix, ".":U)
         vOutputPrefix = "dbmon."
                       + ENTRY(3, vOutputPrefix, ".":U) + "."
                       + vDbName
         vDirName = vDirName + vDbName
  . /* ASSIGN */

  ASSIGN vOutputPrefix = vDirName + vSlash + vOutputPrefix + ".".

/* Create subdirectory using db name (if possible): */
  ASSIGN FILE-INFO:FILE-NAME = vDirName.

  IF FILE-INFO:FULL-PATHNAME EQ ? THEN
  DO:
    OS-CREATE-DIR VALUE(FILE-INFO:FILE-NAME).

    IF OS-ERROR NE 0 THEN
    DO:
      MESSAGE
        "Failed to create directory" vDirName
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN.
    END.
  END.

  INPUT STREAM InpFile FROM VALUE(ipInputFile).
  ASSIGN vInputLine = 0.
  EMPTY TEMP-TABLE ttHeader.

HeaderLines:
  REPEAT TRANSACTION:
    ASSIGN vInputText = "":U.
    IMPORT STREAM InpFile UNFORMATTED vInputText.
    ASSIGN vInputLine = vInputLine + 1.

    IF vInputText EQ "":U OR vInputText EQ ".":U THEN
    LEAVE.

    CREATE ttHeader.
    ASSIGN ttHeader.HeaderLine = vInputLine
           ttHeader.HeaderText = vInputText
    . /* ASSIGN */
  END. /* REPEAT TRANSACTION */

/* The whole input file was scanned but the end of header was not found.
   The input file does not have a header.
   Re-read the file from s scratch as promon data.
*/
  IF SEEK(InpFile) EQ ? THEN
  DO:
    EMPTY TEMP-TABLE ttHeader.
    CREATE ttHeader.
    ASSIGN ttHeader.HeaderLine = 0
           ttHeader.HeaderText = ipInputFile
    . /* ASSIGN */

    INPUT STREAM InpFile CLOSE.
    INPUT STREAM InpFile FROM VALUE(ipInputFile).
  END.

  ASSIGN vPromonMenu = ?.

InputLine:
  REPEAT:
    IMPORT STREAM InpFile UNFORMATTED vInputText.
    ASSIGN vInputLine = vInputLine + 1
           vInputText = RIGHT-TRIM(vInputText)
    . /* ASSIGN */

/* Ignore empty lines:*/
    IF vInputText EQ "" THEN
    NEXT InputLine.

/* The id of this process is 10595. (1408) */
    IF vInputText MATCHES "* (1408)" THEN
    NEXT InputLine.

/* New promon screen:
04/05/16        Activity: Summary
*/  IF vInputText MATCHES "../../.. *" THEN
    ASSIGN vPromonMenu = TRIM(SUBSTRING(vInputText, 10)).
    ELSE
/*
13:17:03        Adjust Latch Options
*/  IF vInputText MATCHES "..:..:.. *" THEN
    ASSIGN vPromonMenu = TRIM(SUBSTRING(vInputText, 10)).
    ELSE
/*  
Shared Resources:
Database Status:
*/  IF vInputText BEGINS "Shared Resources:":U
    OR vInputText BEGINS "Database Status:":U
    OR vInputText BEGINS "User Control:":U THEN
    ASSIGN vPromonMenu = SUBSTRING(vInputText, 1, INDEX(vInputText,":":U) - 1).

    IF vPromonMenu NE ? THEN
    DO:
      ASSIGN i = INDEX(vPromonMenu, " by user number for all tenants":U).
      ASSIGN vPromonMenu = SUBSTRING(vPromonMenu, 1, i - 1) WHEN i GT 0
             vPromonMenu =   REPLACE(vPromonMenu, ":":U, "":U)
             vPromonMenu =   REPLACE(vPromonMenu, "/":U, "":U)
             vPromonMenu =   REPLACE(vPromonMenu, " ":U, "_":U)

             vOutputFile = vOutputPrefix + vPromonMenu + ".log":U
             vPromonMenu = ?
      . /* ASSIGN */

      IF SEEK(OutFile) NE ? THEN
      PUT STREAM OutFile UNFORMATTED SKIP(1).
      
      OUTPUT STREAM OutFile CLOSE.
      OUTPUT STREAM OutFile TO VALUE(vOutputFile) APPEND.
      
      IF SEEK(OutFile) EQ 0 THEN
      DO:
        FOR EACH ttHeader NO-LOCK
              BY ttHeader.HeaderLine:
          PUT STREAM OutFile UNFORMATTED ttHeader.HeaderText SKIP.
        END. /* FOR EACH ttHeader */
      
        PUT STREAM OutFile UNFORMATTED ".":U SKIP.
      END. /* IF SEEK(OutFile) */

/* Menu title: */
      PUT STREAM OutFile UNFORMATTED vInputText SKIP.

/* Line after menu title: */
      IMPORT STREAM InpFile UNFORMATTED vInputText.
      ASSIGN vInputLine = vInputLine + 1
             vInputText = RIGHT-TRIM(vInputText)
      . /* ASSIGN */
    END. /* vPromonMenu */

    IF SEEK(OutFile) EQ ? THEN
    DO:
      ASSIGN vOutputFile = vOutputPrefix + vPromonMenu + "Unknown_Menu.log":U.
      OUTPUT STREAM OutFile TO VALUE(vOutputFile) APPEND.
    END. /* IF vPromonMenu = "Unknown" */

    IF vInputText NE "":U THEN
    PUT STREAM OutFile UNFORMATTED vInputText SKIP.
  END. /* InputLine */

  INPUT  STREAM InpFile CLOSE.
  OUTPUT STREAM OutFile CLOSE.

END PROCEDURE. /* SplitPromonOutput */

/* ------------------------------------------------------------------------- */

PROCEDURE DbMonSplit:

  DEFINE INPUT PARAMETER ipRootDir  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMaxDepth AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipFileMask AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vFileCount AS INTEGER NO-UNDO.

  ASSIGN 
    ipFileMask = "*~~.promon~~.*log" WHEN ipFileMask EQ "" OR ipFileMask EQ ?
     FILE-INFO:FILE-NAME = ipRootDir.
  IF FILE-INFO:FULL-PATHNAME EQ ? THEN
  DO:
    MESSAGE ipRootDir " does not exist!"
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    QUIT.
  END.
  
  RUN GetOSFiles(INPUT ipRootDir,
                 INPUT ipFileMask,
                 INPUT ipMaxDepth).
  
  PAUSE 0 BEFORE-HIDE.
  ASSIGN vFileCount = 0.
  FOR EACH ttOSFile NO-LOCK:
    ASSIGN vFileCount = vFileCount + 1.
    DISPLAY
      vFileCount          FORMAT ">>9"   LABEL "#"
      ttOSFile.osFileName FORMAT "x(64)" LABEL "File".
    PROCESS EVENTS.
    RUN SplitPromonOutput(INPUT ttOSFile.osFilePath).
  END.

END PROCEDURE. /* DbMonSplit */

/* ------------------------------------------------------------------------- */
