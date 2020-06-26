DEFINE VARIABLE vLineBreak AS INTEGER   NO-UNDO INITIAL 200000.
DEFINE VARIABLE vPrecision AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE vCommand   AS CHARACTER NO-UNDO.
DEFINE VARIABLE vDbName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE vLine      AS CHARACTER NO-UNDO.
DEFINE VARIABLE vCount     AS INTEGER   NO-UNDO.
DEFINE VARIABLE vLineCount AS INTEGER   NO-UNDO.
DEFINE VARIABLE vInitTime  AS INTEGER   NO-UNDO.
DEFINE VARIABLE vPrevTime  AS INTEGER   NO-UNDO.
DEFINE VARIABLE vCurrTime  AS INTEGER   NO-UNDO.
DEFINE VARIABLE vInterval  AS INTEGER   NO-UNDO.

DEFINE STREAM DbLog.

ASSIGN vCommand = SESSION:PARAMETER.

IF vCommand EQ "":U THEN
DO:
  PUT UNFORMATTED "You should specify rfutil command with the -param." SKIP.
  QUIT.
END.

ASSIGN vLine = SUBSTRING(vCommand, 1, INDEX(vCommand, " -C ":U)).
       
REPEAT vCount = NUM-ENTRIES(vLine, " ":U) TO 2 BY -1:
  ASSIGN vDbName = ENTRY(vCount, vLine, " ":U)
         vDbName = ENTRY(1, vDbName, ".":U)
  . /* ASSIGN */
  IF vDbName BEGINS "-":U THEN
  NEXT.

  FILE-INFO:FILE-NAME = vDbName + ".lg":U.
  IF FILE-INFO:FULL-PATHNAME NE ? THEN
  LEAVE.
END.

IF FILE-INFO:FULL-PATHNAME EQ ? THEN
DO:
  PUT UNFORMATTED "Failed to find dbname in " vCommand SKIP.
  QUIT.
END.

INPUT STREAM DbLog FROM VALUE(FILE-INFO:FULL-PATHNAME).
SEEK STREAM DbLog TO END.

ASSIGN vCurrTime  = MTIME
       vInitTime  = vCurrTime
       vLineCount = 0
       vLineBreak = vLineBreak - 1
. /* ASSIGN */

PUT UNFORMATTED 
  /* Time    */ STRING(TIME, "HH:MM:SS":U) SPACE
  /* Interval*/ 0          SPACE
  /* Lines   */ 0          SPACE
  /* Rate    */ "Starting" SPACE
  /* Message */ vCommand   SKIP
. /* PUT     */

INPUT THROUGH VALUE(vCommand).

REPEAT:
  IMPORT UNFORMATTED vLine.
  IF vLine BEGINS "Trid: ":U THEN
  LEAVE.
  ASSIGN vLineCount = vLineCount + 1.
  PUT UNFORMATTED
    /* Time    */ STRING(TIME, "HH:MM:SS":U) SPACE
    /* Interval*/ 0          SPACE
    /* Lines   */ vLineCount SPACE
    /* Rate    */ "Header:"  SPACE
    /* Message */ vLine      SKIP
  . /* PUT     */
END.

ASSIGN vPrevTime = vCurrTime
       vCurrTime = MTIME
       vInterval = vCurrTime - vPrevTime
. /* ASSIGN */

PUT UNFORMATTED 
  /* Time    */ STRING(TIME, "HH:MM:SS":U) SPACE
  /* Interval*/ vInterval  SPACE
  /* Lines   */ vLineCount SPACE
  /* Rate    */ "Started:" SPACE
  /* Message */ vCommand   SKIP
. /* PUT     */

REPEAT:
  IMPORT STREAM DbLog UNFORMATTED vLine.
  PUT UNFORMATTED vLine SKIP.
END.

INPUT STREAM DbLog FROM VALUE(FILE-INFO:FULL-PATHNAME).
SEEK STREAM DbLog TO END.

MainLoop:
REPEAT ON ENDKEY UNDO, LEAVE MainLoop:

  REPEAT vCount = 1 TO vLineBreak ON ENDKEY UNDO, LEAVE MainLoop:
    IMPORT UNFORMATTED ^.
  END.

  IMPORT UNFORMATTED vLine.

  ASSIGN vPrevTime  = vCurrTime
         vCurrTime  = MTIME
         vInterval  = vCurrTime - vPrevTime
         vLineCount = vLineCount + vCount
  . /* ASSIGN */

  PUT UNFORMATTED 
    /* Time    */ STRING(TIME, "HH:MM:SS":U) SPACE
    /* Interval*/ vInterval  SPACE
    /* Lines   */ vLineCount SPACE
    /* Rate    */ ROUND(vCount / vInterval, vPrecision) SPACE
    /* Message */ vLine      SKIP
  . /* PUT */
END.

INPUT CLOSE.

ASSIGN vInterval  = vCurrTime - vInitTime
       vLineCount = vLineCount + vCount
. /* ASSIGN */
PUT UNFORMATTED 
  /* Time    */ STRING(TIME, "HH:MM:SS":U) SPACE
  /* Interval*/ vInterval   SPACE
  /* Lines   */ vLineCount  SPACE
  /* Rate    */ ROUND(vLineCount / vInterval, vPrecision) SPACE
  /* Message */ "Finished" SKIP
. /* PUT     */

REPEAT:
  IMPORT STREAM DbLog UNFORMATTED vLine.
  PUT UNFORMATTED vLine SKIP.
END.

OUTPUT CLOSE.
QUIT.
