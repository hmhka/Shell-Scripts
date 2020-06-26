DEFINE VARIABLE vMinNoteCount AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE vInputList    AS CHARACTER NO-UNDO
  INITIAL "scan.1697.log,scan.1698.log".

DEFINE VARIABLE vErrorLog   AS CHARACTER NO-UNDO INITIAL "aistat.error.log".
DEFINE VARIABLE vErrorCount AS INTEGER NO-UNDO INITIAL 0.
/* Variable vErrorCount is "global" for this procedure. */

DEFINE VARIABLE i AS INTEGER NO-UNDO.

/* vInputStream identifies the input stream.
   The variable is used by ErrorLog procedure.
*/
DEFINE VARIABLE vInputStream AS CHARACTER NO-UNDO.

DO i = 1 TO NUM-ENTRIES(vInputList):
  ASSIGN vInputStream = ENTRY(i, vInputList).
  INPUT FROM VALUE(vInputStream).
  RUN ImportAiStat(vMinNoteCount).
  INPUT CLOSE.
END.

IF vErrorCount GT 0 THEN
MESSAGE "Errors:" vErrorCount SKIP "See in file:" vErrorLog
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
     
RUN StatReports.

MESSAGE "StatReports done!"
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

/* ----------------------------------------------------------- */
DEFINE TEMP-TABLE ttTranStat NO-UNDO
  FIELD Trid        AS INTEGER                 /* Trid: (2598) */
  FIELD TranIdx     AS INTEGER  /* transaction index =  (1638) */
  FIELD TranBgnTime AS CHARACTER FORMAT "x(24)"      /* (2598) */
  FIELD TranEndTime AS CHARACTER FORMAT "x(24)"      /* (2598) */
  FIELD TranUser    AS CHARACTER            /* User Id: (2599) */
  FIELD NoteCount   AS INTEGER
  INDEX Trid        AS PRIMARY UNIQUE
        Trid
  INDEX TranIdx
        TranIdx.

DEFINE TEMP-TABLE ttCodeStat NO-UNDO
  FIELD Trid        AS INTEGER
  FIELD CodeName    AS CHARACTER /* code = (1637) */
  FIELD NoteCount   AS INTEGER
  INDEX TranCode    AS PRIMARY UNIQUE
        Trid
        CodeName.

DEFINE TEMP-TABLE ttAreaStat NO-UNDO
  FIELD Trid        AS INTEGER
  FIELD CodeName    AS CHARACTER /* code = (1637) */
  FIELD AreaNum     AS INTEGER
  FIELD NoteCount   AS INTEGER   /* area = (9016) */
  INDEX TranCode    AS PRIMARY UNIQUE
        Trid
        CodeName
        AreaNum.


/* vLineNumber stores the current line number in input stream.
   The variable is shared between ErrorLog and ImportAiStat procedures.
   It's used for information purposes only.
*/
DEFINE VARIABLE vLineNumber AS INTEGER NO-UNDO.

/*------------------------------------------------------------------*/
/* Error Log: */
PROCEDURE ErrorLog.
  DEFINE INPUT PARAMETER ipErrorDesc  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipErrorData  AS CHARACTER NO-UNDO.

/* Acceptable number of the errors:*/
  DEFINE VARIABLE vMaxErrorCount AS INTEGER NO-UNDO INITIAL 100.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  ASSIGN vErrorCount = vErrorCount + 1.
  OUTPUT TO VALUE(vErrorLog) APPEND.

  IF vErrorCount EQ 1 THEN
  PUT UNFORMATTED SKIP(3) FILL("-", 72) SKIP.

  PUT UNFORMATTED
    TODAY SPACE STRING(TIME,"HH:MM:SS") SPACE
    "Count: " vErrorCount  ", ":U
    "Stream:" vInputStream ", ":U
    "Line: "  vLineNumber  ", ":U
    "Offset: " SEEK(INPUT) SKIP
    SPACE(18) "Error: " ipErrorDesc SKIP.

  IF ipErrorData NE "" AND
     ipErrorData NE ? THEN
  PUT UNFORMATTED
    SPACE(18) "Data: " ipErrorData SKIP.

  DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
    PUT UNFORMATTED 
      SPACE(18) ERROR-STATUS:GET-MESSAGE(i) SKIP.
  END.

  OUTPUT CLOSE.

  IF vErrorCount GE vMaxErrorCount THEN
  DO:
    MESSAGE "Reach max number of the errors:" vErrorCount  SKIP
             "See log: "  vErrorLog
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    OS-COMMAND SILENT VALUE("START " + vErrorLog).
    QUIT.
  END.
END.

/*------------------------------------------------------------------*/
PROCEDURE ImportAiStat.
  DEFINE INPUT PARAMETER ipMinNoteCount AS INTEGER NO-UNDO.

  DEFINE VARIABLE vInputLine  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vTranIdx  AS INTEGER   NO-UNDO INITIAL ?.
  DEFINE VARIABLE vCodeName AS CHARACTER NO-UNDO INITIAL ?.
  DEFINE VARIABLE vTrid     AS INTEGER   NO-UNDO INITIAL ?.
  DEFINE VARIABLE vTime     AS CHARACTER NO-UNDO INITIAL ?.
  DEFINE VARIABLE vAreaNum  AS INTEGER   NO-UNDO INITIAL ?.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  ASSIGN vLineNumber = 0.
  ETIME(TRUE).

InputLine:
  REPEAT:
    IMPORT UNFORMATTED vInputLine.
    ASSIGN 
      vInputLine  = TRIM(vInputLine)
      vLineNumber = vLineNumber + 1.

    IF vLineNumber MOD 1000 EQ 0 THEN
    DO:
      PAUSE 0.
      DISPLAY
        vInputStream FORMAT "x(32)"                LABEL "Stream"
        vLineNumber  FORMAT "->,>>>,>>>,>>9"       LABEL "Line"
        SEEK(INPUT)  FORMAT "->,>>>,>>>,>>9 bytes" LABEL "Offset"
        ETIME        FORMAT ">>>,>>9 ms"           LABEL "Time"
      WITH FRAME InputStatus
           TITLE "Input Status"
           SIDE-LABELS 1 DOWN 1 COLUMN.
    END.

/*------------------------------------------------------------------*/
/* code = <name> (1637) */
    IF vInputLine MATCHES "* (1637)" THEN
    DO:
      ASSIGN
        vInputLine = SUBSTRING(vInputLine, 1, LENGTH(vInputLine) - 7)
        vInputLine = SUBSTRING(vInputLine, INDEX(vInputLine, "=":U) + 2)
        vCodeName  = TRIM(vInputLine)
      NO-ERROR.

      IF NOT vCodeName BEGINS "RL_":U THEN
      DO:
        RUN ErrorLog("code = <name> (1637):", vCodeName).
        ASSIGN vCodeName = ?.
      END.

      NEXT InputLine.
    END. /* code = <name> (1637) */

/*------------------------------------------------------------------*/
/* transaction index = <number> (1638) */
    IF vInputLine MATCHES "* (1638)" THEN
    DO:
/* Note KB-P72241: 
   rfutil does not show transaction index within message id 1638
   By default transaction index is reported as short (2-byte) integer 
   while Trid is long (4-byte) integer.
   Workarround is to restore a real transaction number based on Trid
   in message 2598 for /last/ transaction with the same transaction index.
*/
      ASSIGN
        vInputLine = SUBSTRING(vInputLine, 1, LENGTH(vInputLine) - 7)
        vInputLine = SUBSTRING(vInputLine, INDEX(vInputLine, "=":U) + 2)
        vTranIdx   = INTEGER  (vInputLine)
      NO-ERROR.

      IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
      DO:
        RUN ErrorLog("transaction index = <number> (1638):", vInputLine).
        ASSIGN vTrid = ?.
        NEXT InputLine.
      END.

      IF vCodeName EQ ? THEN
      NEXT InputLine.

      ASSIGN vTrid = ?.
      FOR EACH ttTranStat
         WHERE ttTranStat.TranIdx  EQ vTranIdx
            BY ttTranStat.Trid DESCENDING:
        ASSIGN ttTranStat.NoteCount = ttTranStat.NoteCount + 1
               vTrid = ttTranStat.Trid.
      END.

/* If a root record in ttTranStat does not yet exist: */
      IF vTrid EQ ? THEN
      DO:
        CREATE ttTranStat.
        ASSIGN ttTranStat.TranIdx  = vTranIdx
               ttTranStat.Trid     = vTranIdx
               vTrid               = vTranIdx.
      END.
/* If the note with same code and the Tris already exists: */
/*
**      FOR FIRST ttCodeStat
**          WHERE ttCodeStat.Trid     EQ vTrid
**            AND ttCodeStat.CodeName EQ vCodeName:
**         ASSIGN ttCodeStat.NoteCount = ttCodeStat.NoteCount + 1.
**         NEXT InputLine.
**      END.
**
/* If it's a first note with given code and Trid: */
**      CREATE ttCodeStat.
**      ASSIGN ttCodeStat.Trid      = vTrid
**             ttCodeStat.CodeName  = vCodeName
**             ttCodeStat.NoteCount = 1.
**      NEXT InputLine.
*/
    END. /* transaction index = <number> (1638) */

/*------------------------------------------------------------------*/
/* Trid: <num> <time>. (2598) */
    IF vInputLine MATCHES "* (2598)" THEN
    DO:
      ASSIGN
        vInputLine = SUBSTRING(vInputLine, 1, LENGTH(vInputLine) - 7)
        vInputLine = SUBSTRING(vInputLine, INDEX(vInputLine, ":":U) + 2)
        vInputLine = TRIM     (vInputLine)
        i = INDEX(vInputLine, " ":U)
        vTrid = INTEGER(SUBSTRING(vInputLine, 1, i - 1))
        vTime = TRIM(SUBSTRING(vInputLine, i + 1))
      NO-ERROR.

      IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
      DO:
        RUN ErrorLog("Trid: <num> <time>. (2598):", vInputLine).
        ASSIGN vTrid = ?
               vTime =?.
      END.

      IF vTrid EQ ? THEN
      NEXT InputLine.

      FIND FIRST ttTranStat
           WHERE ttTranStat.Trid EQ vTrid
      NO-ERROR.
        
      IF NOT AVAILABLE ttTranStat THEN
      IF vTranIdx EQ ? THEN
      NEXT InputLine.
      ELSE
      FOR EACH ttTranStat
          WHERE ttTranStat.TranIdx EQ vTranIdx
             BY ttTranStat.Trid DESCENDING:
        LEAVE.
      END.

      IF NOT AVAILABLE ttTranStat THEN
      DO:
        CREATE ttTranStat.
        ASSIGN ttTranStat.Trid    = vTrid
               ttTranStat.TranIdx = vTranIdx.
      END.

      ASSIGN
        ttTranStat.Trid        = vTrid
        ttTranStat.TranBgnTime = vTime WHEN vCodeName EQ "RL_TBGN":U
        ttTranStat.TranEndTime = vTime WHEN vCodeName EQ "RL_TEND":U
      . /* ASSIGN */

      IF vCodeName NE "RL_TEND":U OR
         ttTranStat.NoteCount GE ipMinNoteCount THEN
      NEXT InputLine.

/* If transaction is committed and it tunes out to have a few notes
   then delete all related information to free space in temp-tables.
   We are looking for the relatively large transactions
   (at least ipMinNoteCount notes per transaction).
*/
      FOR EACH ttCodeStat
         WHERE ttCodeStat.Trid EQ vTrid:
        DELETE ttCodeStat.
      END.

      FOR EACH ttAreaStat
         WHERE ttAreaStat.Trid EQ vTrid:
        DELETE ttAreaStat.
      END.

      DELETE ttTranStat.

      NEXT InputLine.
    END. /* Trid: <num> <time>. (2598) */

/*------------------------------------------------------------------*/
/* User Id: <name>. (2599) */
    IF vInputLine MATCHES "* (2599)" THEN
    FOR FIRST ttTranStat
        WHERE ttTranStat.Trid EQ vTrid:

      ASSIGN
        vInputLine = SUBSTRING(vInputLine, 1, LENGTH(vInputLine) - 7)
        vInputLine = SUBSTRING(vInputLine, INDEX(vInputLine, ":":U) + 2)
        ttTranStat.TranUser = TRIM(vInputLine).

      NEXT InputLine.
    END. /* User Id: <name>. (2599) */

/*------------------------------------------------------------------*/
/* area = <area>   dbkey = <dbkey>   update counter = <number> (9016) */
/*
    IF vInputLine MATCHES "* (9016)" THEN
    DO:
      IF vTrid EQ ? THEN
      NEXT InputLine.

      ASSIGN
        vInputLine = SUBSTRING(vInputLine, 1, LENGTH(vInputLine) - 7)
        vInputLine = SUBSTRING(vInputLine, INDEX(vInputLine, "=":U) + 2)
        vInputLine = TRIM     (vInputLine)
        vAreaNum   = INTEGER(ENTRY(1, vInputLine, " ":U))
      NO-ERROR.
   
      IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
      DO:
        RUN ErrorLog("area = <area> (9016):", vInputLine).
        ASSIGN vAreaNum = ?.
        NEXT InputLine.
      END.

      FOR FIRST ttAreaStat
           WHERE ttAreaStat.Trid     EQ vTrid
             AND ttAreaStat.CodeName EQ vCodeName
             AND ttAreaStat.AreaNum  EQ vAreaNum:

         ASSIGN ttAreaStat.NoteCount = ttAreaStat.NoteCount + 1.
         NEXT InputLine.
      END.

    /* if not available ttAreaStat: */
      CREATE ttAreaStat.
      ASSIGN ttAreaStat.Trid      = vTrid
             ttAreaStat.CodeName  = vCodeName
             ttAreaStat.AreaNum   = vAreaNum
             ttAreaStat.NoteCount = 1.

      NEXT InputLine.
    END. /* area = <area> (9016) */
*/
  END. /* REPEAT */
END PROCEDURE. /* ImportAiStat */

/*------------------------------------------------------------------*/
PROCEDURE StatReports:
  DEFINE VARIABLE vTranStatReport AS CHARACTER  NO-UNDO INITIAL "TranStat.txt".
  DEFINE VARIABLE vCodeStatReport AS CHARACTER  NO-UNDO INITIAL "CodeStat.txt".
  DEFINE VARIABLE vAreaStatReport AS CHARACTER  NO-UNDO INITIAL "AreaStat.txt".

  OUTPUT TO VALUE(vTranStatReport).
  PUT UNFORMATTED
    "Trid"  "~t"
    "Begin" "~t"
    "End"   "~t"
    "User"  "~t"
    "Notes" SKIP.

  FOR EACH ttTranStat BY ttTranStat.Trid:
    PUT UNFORMATTED
      ttTranStat.Trid        "~t"
      ttTranStat.TranBgnTime "~t"
      ttTranStat.TranEndTime "~t"
      ttTranStat.TranUser    "~t"
      ttTranStat.NoteCount   SKIP.
  END.
  OUTPUT CLOSE.

  OUTPUT TO VALUE(vCodeStatReport).
  PUT UNFORMATTED
    "Trid"  "~t"
    "Code" "~t"
    "Notes" SKIP.

/*
  FOR EACH ttCodeStat BY ttCodeStat.Trid:
    PUT UNFORMATTED
      ttCodeStat.Trid      "~t"
      ttCodeStat.CodeName  "~t"
      ttCodeStat.NoteCount SKIP.
  END.
  OUTPUT CLOSE.

  OUTPUT TO VALUE(vAreaStatReport).
  PUT UNFORMATTED
    "Trid"  "~t"
    "Code"  "~t"
    "Area"  "~t"
    "Notes" SKIP.

  FOR EACH ttAreaStat BY ttAreaStat.Trid:
    PUT UNFORMATTED
      ttAreaStat.Trid      "~t"
      ttAreaStat.CodeName  "~t"
      ttAreaStat.AreaNum   "~t"
      ttAreaStat.NoteCount SKIP.
  END.
*/
  OUTPUT CLOSE.
END PROCEDURE. /* StatReports */
/*------------------------------------------------------------------*/

