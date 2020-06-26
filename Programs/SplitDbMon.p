DEFINE VARIABLE vRootDir AS CHARACTER   NO-UNDO INITIAL 
  "D:\vwRHEL5\Share\test".

DEFINE VARIABLE vFileMask AS CHARACTER NO-UNDO INITIAL "*~.promon~.*~.log".
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
    AND vFileName MATCHES vFileMask
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

PROCEDURE SplitPromonOutput.
  DEFINE INPUT PARAMETER ipInputFile AS CHARACTER  NO-UNDO.

  DEFINE VARIABLE vOutputFile      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vOutputPrefix    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vOutputExtension AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vHeaderLine AS INTEGER   NO-UNDO INITIAL 6.
  DEFINE VARIABLE vHeaderText AS CHARACTER NO-UNDO EXTENT 6.
  DEFINE VARIABLE vNeedHeader AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE vInputLine  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vInputText  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vPromonMenu AS CHARACTER NO-UNDO INITIAL ?.
  DEFINE VARIABLE vFileName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDirName    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDbName     AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  ASSIGN FILE-INFO:FILE-NAME = ipInputFile.
  IF FILE-INFO:FULL-PATHNAME EQ ? THEN RETURN.
    
  ASSIGN i = R-INDEX(ipInputFile, 
                     IF OPSYS MATCHES "WIN*" THEN "~\":U ELSE "/":U)
         vDirName  = SUBSTRING(ipInputFile, 1, i - 1) /*without a tailed slash*/
         vFileName = SUBSTRING(ipInputFile, i)        /* begins with a slash  */
         i = INDEX(vFileName, ".":U)
         vDbName = SUBSTRING(vFileName, 1, MAX(0, i - 1)). /*begins with slash*/

/* Create subdirectory using db name (if possible): */
  IF LENGTH(vDbName) GT 2 THEN
  DO:
    ASSIGN FILE-INFO:FILE-NAME = vDirName + vDbName.
    IF FILE-INFO:FULL-PATHNAME EQ ? THEN
    OS-CREATE-DIR VALUE(FILE-INFO:FILE-NAME).
    
    ASSIGN FILE-INFO:FILE-NAME = vDirName + vDbName.
    ASSIGN vDirName = FILE-INFO:FULL-PATHNAME
                 WHEN FILE-INFO:FULL-PATHNAME NE ?
                  AND FILE-INFO:FILE-TYPE BEGINS "D" .
  END.

  ASSIGN i = R-INDEX(vFileName, ".":U)
         vOutputPrefix    = vDirName + SUBSTRING(vFileName, 1, i)
         vOutputExtension = SUBSTRING(vFileName, i).

  INPUT STREAM InpFile FROM VALUE(ipInputFile).
  ASSIGN vInputLine = 0.
  OUTPUT STREAM OutFile TO VALUE(vOutputPrefix + "Status" + vOutputExtension).

InputLine:
  REPEAT:
    IMPORT STREAM InpFile UNFORMATTED vInputText.
    ASSIGN vInputLine = vInputLine + 1.

    IF vInputLine LE vHeaderLine THEN
    DO:
/* Header is a first vHeaderLine lines or it ends by an empty line.
*/    IF vInputText EQ ""
      THEN ASSIGN vHeaderLine = vInputLine - 1.
      ELSE ASSIGN vHeaderText[vInputLine] = vInputText.
      NEXT InputLine.
    END.

/* Ignore empty lines:*/
    IF vInputText EQ "" THEN
    NEXT InputLine.

/* New promon screen:
*/  IF SUBSTRING(vInputText, 1, 9) MATCHES "../../.. " THEN
    DO:
      ASSIGN vPromonMenu = TRIM(SUBSTRING(vInputText, 10))
             vPromonMenu = REPLACE(vPromonMenu, ":":U, "":U)
             vPromonMenu = REPLACE(vPromonMenu, "/":U, "":U)
             vPromonMenu = REPLACE(vPromonMenu, " ":U, "_":U)
             vOutputFile = vOutputPrefix + vPromonMenu + vOutputExtension
             FILE-INFO:FILE-NAME = vOutputFile
             vNeedHeader = FILE-INFO:FULL-PATHNAME EQ ?.
      OUTPUT STREAM OutFile CLOSE.
      OUTPUT STREAM OutFile TO VALUE(vOutputFile) APPEND.

      IF vNeedHeader THEN
      DO i = 1 TO vHeaderLine:
        PUT STREAM OutFile UNFORMATTED vHeaderText[i] SKIP.
      END.
      PUT STREAM OutFile UNFORMATTED SKIP(1).
    END.

    IF vPromonMenu EQ ? THEN
    DO:
      ASSIGN vPromonMenu = "Status":U
             vOutputFile = vOutputPrefix + vPromonMenu + vOutputExtension
             FILE-INFO:FILE-NAME = vOutputFile
             vNeedHeader = FILE-INFO:FULL-PATHNAME EQ ?.
      OUTPUT STREAM OutFile CLOSE.
      OUTPUT STREAM OutFile TO VALUE(vOutputFile) APPEND.

      IF vNeedHeader THEN
      DO i = 1 TO vHeaderLine:
        PUT STREAM OutFile UNFORMATTED vHeaderText[i] SKIP.
      END.
      PUT STREAM OutFile UNFORMATTED SKIP(1).
    END.

    PUT STREAM OutFile UNFORMATTED vInputText SKIP.
  END. /* InputLine */

  INPUT STREAM InpFile CLOSE.
  OUTPUT STREAM OutFile CLOSE.
END PROCEDURE. /* SplitPromonOutput */


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
               INPUT vFileMask,
               INPUT vMaxDirLevel).

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

