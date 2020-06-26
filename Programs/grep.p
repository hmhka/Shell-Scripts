/*
RUN grepFile(
  "/path/to/database.lg", /* input file */
  "output.txt", /* output file */
  "* error*",   /* search mask  */
  4,            /* lines before */
  4,            /* lines after  */
  FALSE,        /* append to the output file */
  ?             /* starting position in the name to print to output file */ 
).              /* unknown: don't print the input file name */
*/
RUN grepDir(
  "/path/to/dir_with_files_to_search",
  ?,            /* MaxDirLevel: 1 - cureent only, ? - unlimited. */
  "*~~~.lg",    /* file mask    */ 
  "output.txt", /* output file  */
  "* error*",   /* search mask  */
  4,            /* lines before */
  4             /* lines after  */
).

/* ------------------------------------------------------------------------
    File        : grep.p
    Purpose     : Searches for the lines matching to the given mask.


    Syntax      : See the examples above.

    Description : The output file can be opened in Excel.

    Author(s)   : George Potemkin
    Created     : July 13, 2013
    Modified    : July 13, 2013
------------------------------------------------------------------------ */

DEFINE VARIABLE vFrameTitle AS CHARACTER   NO-UNDO.

FORM vFile AS CHARACTER FORMAT "x(41)" LABEL "File"
     vSize AS CHARACTER FORMAT "x(10)" LABEL "Size"
     vTime AS CHARACTER FORMAT "x(12)" LABEL "Processed"
WITH FRAME grepFile TITLE vFrameTitle 16 DOWN.

DEFINE STREAM OutputFile.

DEFINE TEMP-TABLE ttOSFile NO-UNDO
  FIELD osFileName AS CHARACTER
  FIELD osFilePath AS CHARACTER
  FIELD osFileAttr AS CHARACTER
  INDEX osFileName
        osFileName
. /* DEFINE TEMP-TABLE ttOSFile */

/* -------------------------------------------------------------------------- */

PROCEDURE grepFile.
  DEFINE INPUT PARAMETER ipInputFile    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipOutputFile   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipSearchMask   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipLinesBefore  AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipLinesAfter   AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipOutputAppend AS LOGICAL   NO-UNDO.
  DEFINE INPUT PARAMETER ipNamePosition AS INTEGER   NO-UNDO.
/*
ipInputFile    - the file to read from;
ipOutputFile   - the file to write to;
ipSearchMask   - searches for the lines matching to the given mask;
ipLinesBefore  - the number of lines to display before the matching line;
ipLinesAfter   - the number of lines to display after the matching line;
ipOutputAppend - append to the existent output file (yes/no);
ipNamePosition - starting possition of input file name to print
                 together with the line numbers to the output file.
                 Unknown value means: don't print the name and line numbers.
*/
  
/* The estimation of the average length of line in db log: */
  DEFINE VARIABLE vLineLength AS DECIMAL   NO-UNDO INITIAL 135.0.
  DEFINE VARIABLE vDispName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFileName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFileSize   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vReportLine AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vSlash      AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vLineStack  AS CHARACTER NO-UNDO EXTENT.
  DEFINE VARIABLE vLine       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vLineCount  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vCurrCount  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
  
  &SCOPED-DEFINE Sep "~t"

  ASSIGN FILE-INFO:FILE-NAME = ipInputFile.
  IF FILE-INFO:FULL-PATHNAME EQ ?
  OR NOT FILE-INFO:FILE-TYPE MATCHES "*F*":U 
  OR NOT FILE-INFO:FILE-TYPE MATCHES "*R*":U THEN
  DO:
    MESSAGE
      "File does not exist or is not readable!" SKIP
      ipInputFile SKIP
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN ERROR.
  END.

  ASSIGN vSlash = IF OPSYS EQ "Unix":U THEN "/":U ELSE "~\":U
         vFileSize = FILE-INFO:FILE-SIZE / 100.0
         vReportLine = MAX(100, vFileSize / vLineLength)
         EXTENT(vLineStack) = 1 + ipLinesBefore + ipLinesAfter
         vFileName = SUBSTRING(ipInputFile, ipNamePosition)
         vDispName = IF LENGTH(ipInputFile) LE 41 THEN ipInputFile ELSE
                     SUBSTRING(ipInputFile, 1, 18) + "...":U +
                     SUBSTRING(ipInputFile, LENGTH(ipInputFile) - 19)
         vFrameTitle = "grepFile: " + ipSearchMask
  . /*ASSIGN */

  IF INDEX(ipOutputFile, vSlash) EQ 0 THEN
  ASSIGN ipOutputFile = SUBSTRING(ipInputFile, 1, R-INDEX(ipInputFile, vSlash))
                      + ipOutputFile
  . /* ASSIGN */

  DISPLAY vDispName @ vFile
          STRING(INTEGER(vFileSize / 10.24), ">,>>>,>>9K") @ vSize
          "0%"      @ vTime
  WITH FRAME grepFile TITLE vFrameTitle.

  ETIME(TRUE).
  
  INPUT FROM VALUE(ipInputFile).

  IF ipOutputAppend THEN
  OUTPUT STREAM OutputFile TO VALUE(ipOutputFile) APPEND. ELSE
  OUTPUT STREAM OutputFile TO VALUE(ipOutputFile).

  ASSIGN vLineStack = "":U
         vLineCount = 0.

  ImportLine:
  REPEAT:
    ASSIGN vLine = "":U.
    IMPORT UNFORMATTED vLine.
    PROCESS EVENTS.
  
    ASSIGN vLineCount = vLineCount + 1
           vLineStack[vLineCount MOD EXTENT(vLineStack) + 1] = vLine
    . /* ASSIGN */

    IF vLineCount MOD vReportLine EQ 0 THEN
    DISPLAY STRING(MAX(SEEK(INPUT), 0) / vFileSize, ">>9%") @ vTime
    WITH FRAME grepFile.
  
    IF NOT vLine MATCHES ipSearchMask THEN
    NEXT ImportLine.
  
    ASSIGN vCurrCount = vLineCount.
    REPEAT i = 1 TO ipLinesAfter:
      ASSIGN vLine = "":U.
      IMPORT UNFORMATTED vLine.
      ASSIGN vLineCount = vLineCount + 1
             vLineStack[vLineCount MOD EXTENT(vLineStack) + 1] = vLine
      . /* ASSIGN */
    END.

    DO i = MAX(1, vCurrCount - ipLinesBefore) TO vCurrCount + ipLinesAfter:
      IF vFileName NE ? THEN
      PUT STREAM OutputFile UNFORMATTED vFileName {&Sep} i {&Sep}.
      PUT STREAM OutputFile UNFORMATTED vLineStack[i MOD EXTENT(vLineStack) + 1]
      SKIP.
    END.
    IF EXTENT(vLineStack) GT 1 THEN
    PUT STREAM OutputFile UNFORMATTED SKIP(1).

    IF SEEK(OutputFile) EQ ? THEN
    LEAVE.
  END.
  OUTPUT STREAM OutputFile CLOSE.
  INPUT CLOSE.

  DISPLAY STRING(TRUNCATE(ETIME / 1000.0, 2), ">>>>9.99 sec") @ vTime
  WITH FRAME grepFile.
END PROCEDURE. /* grepFile */

/* -------------------------------------------------------------------------- */

PROCEDURE grepDir.
  DEFINE INPUT PARAMETER ipRootDir      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMaxDirLevel  AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipFileMask     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipOutputFile   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipSearchMask     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipLinesBefore  AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipLinesAfter   AS INTEGER   NO-UNDO.
/*
ipRootDir      - root directory with the files to read from;
ipMaxDirLevel  - how deep to search in subdirectories (1: current only, ?: unlimited);
ipFileMask     - the mask for the files;
ipOutputFile   - the file to write to;
ipSearchMask   - searches for the lines matching to the given mask;
ipLinesBefore  - the number of lines to display before the matching line;
ipLinesAfter   - the number of lines to display after the matching line;
*/

  DEFINE VARIABLE vOutputDir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vSlash     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vError     AS LOGICAL   NO-UNDO.
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

  ASSIGN FILE-INFO:FILE-NAME = ipRootDir
         vSlash = IF OPSYS EQ "Unix":U THEN "/":U ELSE "~\":U
         vError = TRUE
  . /* ASSIGN */
  
  IF FILE-INFO:FULL-PATHNAME EQ ? THEN
  MESSAGE "Input directory does not exist!" SKIP
           ipRootDir 
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  ELSE
  ASSIGN ipRootDir = FILE-INFO:FULL-PATHNAME
         vError    = FALSE.

  IF vError THEN
  RETURN ERROR.
  
  IF INDEX(ipOutputFile, vSlash) EQ 0
  THEN ASSIGN ipOutputFile = FILE-INFO:FULL-PATHNAME + vSlash + ipOutputFile.
  ELSE
  DO:
/* Check the directory of output file: */
    ASSIGN vOutputDir = SUBSTRING(ipOutputFile, 1,
                          R-INDEX(ipOutputFile, vSlash) - 1)
           FILE-INFO:FILE-NAME = vOutputDir
           vError = TRUE
    . /* ASSIGN */

    IF FILE-INFO:FULL-PATHNAME NE ? THEN
    ASSIGN vError = FALSE.
    ELSE
    MESSAGE "Output dir" vOutputDir " does not exist!"
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.

    IF vError THEN
    RETURN ERROR.
  END.

/* Check the output file: */
  ASSIGN FILE-INFO:FILE-NAME = ipOutputFile
         vError = TRUE.

  IF FILE-INFO:FULL-PATHNAME EQ ? THEN
  ASSIGN vError = FALSE.
  ELSE
  IF  NOT FILE-INFO:FILE-TYPE MATCHES "*F*":U
  AND NOT FILE-INFO:FILE-TYPE MATCHES "*W*":U THEN
  MESSAGE 
    "The output either not a file or you don't have the write permissions"
  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  ELSE
  DO:
    MESSAGE "The output file exists:" SKIP
             FILE-INFO:FULL-PATHNAME  SKIP
            "Do you want to delete it?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL    
      TITLE "Replace or Append?" UPDATE vChoice AS LOGICAL.

    CASE vChoice:
      WHEN TRUE THEN
      DO:
        OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).
        IF OS-ERROR EQ 0 THEN
        ASSIGN vError = FALSE.
        ELSE
        MESSAGE 
          "Unable to delete" FILE-INFO:FULL-PATHNAME SKIP
          "Error:" OS-ERROR "("
          IF OS-ERROR LE EXTENT(vErrorDesc) THEN vErrorDesc[OS-ERROR]
                                            ELSE "Unmapped error" ")"
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      END.

      WHEN FALSE THEN
      ASSIGN vError = FALSE.

    END CASE.
  END.

  IF vError THEN
  RETURN ERROR.

  RUN GetOSFiles(INPUT ipRootDir, INPUT ipFileMask, INPUT ipMaxDirLevel).
  
  PAUSE 0 BEFORE-HIDE.
  FOR EACH ttOSFile
     WHERE ttOSFile.osFilePath NE ipOutputFile:

    RUN grepFile(ttOSFile.osFilePath,  /* input file        */
                 ipOutputFile,         /* output file       */
                 ipSearchMask,         /* search mask       */
                 ipLinesBefore,        /* lines before      */
                 ipLinesAfter ,        /* lines after       */
                 TRUE,                 /* output append     */
                 LENGTH(ipRootDir) + 2 /* starting position */
                ).

    DOWN WITH FRAME grepFile.

  END. /* FOR EACH ttOSFile */
END PROCEDURE. /* grepDir */

/* -------------------------------------------------------------------------- */

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

/* -------------------------------------------------------------------------- */
