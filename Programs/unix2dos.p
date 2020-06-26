DEFINE VARIABLE vRootDir AS CHARACTER   NO-UNDO INITIAL 
  "D:\vwRHEL5\Share\test".

DEFINE VARIABLE vExtList AS CHARACTER NO-UNDO INITIAL "log,txt,lg,df,lic".
DEFINE VARIABLE vMaxDirLevel AS INTEGER NO-UNDO INITIAL ?.

DEFINE TEMP-TABLE ttOSFile NO-UNDO
  FIELD osFileName AS CHARACTER
  FIELD osFilePath AS CHARACTER
  FIELD osFileAttr AS CHARACTER
  INDEX osFileName
        osFileName
. /* DEFINE TEMP-TABLE ttOSFile */

/* -------------------------------------------------------------------------- */
PROCEDURE GetOSFiles.

  DEFINE INPUT PARAMETER ipRootDir     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipExtList     AS CHARACTER NO-UNDO.
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
                   INPUT ipExtList,
                   INPUT ipMaxDirLevel).
    ELSE
    IF  vFileAttr MATCHES "*F*"
    AND CAN-DO(ipExtList, SUBSTRING(vFileName, R-INDEX(vFileName, ".":U) + 1))
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

/* -------------------------------------------------------------------------- */
DEFINE STREAM InpFile.
DEFINE STREAM OutFile.

PROCEDURE Unix2Dos:
  DEFINE INPUT PARAMETER ipInputFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFileLine  AS CHARACTER NO-UNDO.
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

  INPUT  STREAM InpFile FROM VALUE(ipInputFile).
  OUTPUT STREAM OutFile TO   VALUE(ipInputFile + ".tmp").
  REPEAT:
      ASSIGN vFileLine = "".
      IMPORT STREAM InpFile UNFORMATTED vFileLine.
      IF vFileLine NE ""
      THEN PUT STREAM OutFile UNFORMATTED vFileLine SKIP.
      ELSE PUT STREAM OutFile SKIP(1).
  END.
  INPUT  STREAM InpFile CLOSE.
  OUTPUT STREAM OutFile CLOSE.
  OS-DELETE VALUE(ipInputFile).
  OS-RENAME VALUE(ipInputFile + ".tmp") VALUE(ipInputFile).
  IF OS-ERROR NE 0 THEN
  MESSAGE 
    ipInputFile SKIP
    "Error:" OS-ERROR SKIP
    IF OS-ERROR LE EXTENT(vErrorDesc) THEN vErrorDesc[OS-ERROR]
                                      ELSE "Unmapped error"
  VIEW-AS ALERT-BOX INFO BUTTONS OK.
END PROCEDURE. /* Unix2Dos */


/* ----------------------------- Main procedure ---------------------------- */
DEFINE VARIABLE vFileCount AS INTEGER NO-UNDO.

ASSIGN FILE-INFO:FILE-NAME = vRootDir.
IF FILE-INFO:FULL-PATHNAME EQ ? THEN
DO:
  MESSAGE vRootDir " does not exist!"
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  QUIT.
END.

RUN GetOSFiles(INPUT vRootDir,
               INPUT vExtList,
               INPUT vMaxDirLevel).

PAUSE 0 BEFORE-HIDE.
ASSIGN vFileCount = 0.
FOR EACH ttOSFile NO-LOCK:
  ASSIGN vFileCount = vFileCount + 1.
  DISPLAY
    vFileCount          FORMAT ">>9"   LABEL "#"
    ttOSFile.osFileName FORMAT "x(64)" LABEL "File".
  PROCESS EVENTS.
  RUN Unix2Dos(INPUT ttOSFile.osFilePath).
END.
