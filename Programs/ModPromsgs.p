/*------------------------------------------------------------------------
    File        : ModPromsgs.p
    Purpose     : Create a modified copy of the current promsgs file:
                  Add the "%B" tag to the specified messages so they
                  will be written also to db log with timestamps.

    Syntax      : See below the list of the modified messages.

    Description : Program creates the promsgs.mod file.

    Inspired by Dan Foreman's loadmsg.p from Database Administration Guide.

    Author(s)   : George Potemkin
    Created     : Aug 25, 2015
    Modified    : Sep 27, 2015
    Version     : 1.2.2
*/
DEFINE VARIABLE vSrcPromsg AS CHARACTER NO-UNDO.
DEFINE VARIABLE vModPromsg AS CHARACTER NO-UNDO.
ASSIGN vSrcPromsg = SEARCH(PROMSGS)
       vModPromsg = "promsgs.dba"
. /* ASSIGN */

/* It'd be enough to specify the message number and "*" as mask for msg text.
   The exact message text (as it exists in the original promsgs file) is used
   only for double check: are we going to modify the intended message?
   Note: message text may slightly vary between Progress versions (e.g. spaces)
*/
/* Dbanalys messages: */

RUN ModMsg( 3867, '%rPROGRESS Database Analysis%r').
RUN ModMsg( 3868, 'Database: %s').
RUN ModMsg( 3869, '*Options: %s %s').
RUN ModMsg( 6172, 'Options: %s %s %s').
RUN ModMsg( 3870, 'Date: %s%r').
RUN ModMsg( 8361, 'Blocksize: %i').
RUN ModMsg( 3937, 'Database analysis complete %s%r').

RUN ModMsg(14179, '%rCHAIN ANALYSIS FOR AREA "%s"*').
RUN ModMsg(14163, '%rFREE CLUSTER CHAIN ANALYSIS%r*').
RUN ModMsg(14161, '%J cluster(s) found in the free cluster chain.%r').

RUN ModMsg( 3892, '%rRM CHAIN ANALYSIS').
RUN ModMsg( 3897, '* LIST OF RM CHAIN BLOCKS').
RUN ModMsg( 3905, '%J block(s) found in the RM chain.%r').
RUN ModMsg(14172, '%J block(s) found in the RM chain of %s object %i%r').

RUN ModMsg( 3873, '%rFREE CHAIN ANALYSIS').
RUN ModMsg( 3880, '* LIST OF FREE CHAIN BLOCKS').
RUN ModMsg( 3891, '%J block(s) found in the free chain.%r').
RUN ModMsg(14155, '%J block(s) found in the free chain of %s object %i%r').

RUN ModMsg( 7269, '%rINDEX DELETE CHAIN ANALYSIS').
RUN ModMsg( 7283, '* LIST OF INDEX DELETE CHAIN BLOCKS').
RUN ModMsg( 7270, '%J block(s) found in the Index Delete chain.%r').
RUN ModMsg(14176, '%J block(s) found in the Index Delete chain of %s object*').

RUN ModMsg(10097, '%rAREA "%s"* BLOCK ANALYSIS').
RUN ModMsg(10130, '%J block(s) found in the area.%r').

/*V11:*/
RUN ModMsg(16573, "%rNumber of * Object * Object *").
RUN ModMsg(16574, "Blocks * Type").
RUN ModMsg(16576, "%rNumber of * Object * Object").
RUN ModMsg(16579, "%r%s BLOCK ANALYSIS:").


RUN ModifyPromsgs(vSrcPromsg, vModPromsg).

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttMsg NO-UNDO
  FIELD ModMsgTag  AS CHARACTER
  FIELD MsgNumber  AS INTEGER
  FIELD MsgText    AS CHARACTER /* can be a mask for MATCHES comparison */
  FIELD MatchCount AS INTEGER
  INDEX MsgNumber  IS UNIQUE
        MsgNumber
. /* DEFINE TEMP-TABLE ttMsg */


/* ******************************  Procedures  ***************************** */

PROCEDURE ModMsg.

  DEFINE INPUT PARAMETER ipMsgNumber AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipMsgText   AS CHARACTER NO-UNDO.

  DO TRANSACTION:
    CREATE ttMsg.
    ASSIGN ttMsg.ModMsgTag  = "%B":U
           ttMsg.MsgNumber  = ipMsgNumber
           ttMsg.MsgText    = ipMsgText
           ttMsg.MatchCount = 0
    . /* ASSIGN */
  END.
END PROCEDURE. /* ModMsg */

/* ------------------------------------------------------------------------- */

PROCEDURE ModifyPromsgs.

  DEFINE INPUT PARAMETER ipSrcPromsgs AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipModPromsgs AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vMaxMsgSize  AS INTEGER   NO-UNDO INITIAL 81.
  DEFINE VARIABLE vPromsgsSize AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vModPromsgs  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDirName     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vChrMsg      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vRawMsg      AS RAW       NO-UNDO.
  DEFINE VARIABLE vMsgNumber   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMsgContinue AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vUpdateCount AS INTEGER   NO-UNDO.

  DEFINE BUFFER bfMsgCont FOR ttMsg.

  FILE-INFO:FILE-NAME = ipSrcPromsgs.
  IF FILE-INFO:FULL-PATHNAME EQ ?
  OR NOT FILE-INFO:FILE-TYPE MATCHES "*F*":U THEN
  DO:
    MESSAGE SUBSTITUTE("File &1 does not exist.", ipSrcPromsgs)
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
  END.

  ASSIGN vPromsgsSize = FILE-INFO:FILE-SIZE
         vDirName = SUBSTRING(ipModPromsgs, 1,
                  MAX(R-INDEX(ipModPromsgs, "/":U),
                      R-INDEX(ipModPromsgs, "~\":U)))
         vModPromsgs = SUBSTRING(ipModPromsgs, LENGTH(vDirName) + 1)
  . /* ASSIGN */

  IF vDirName EQ "":U THEN /* create ModPromsgs new SrcPromsgs: */
  ASSIGN vDirName = SUBSTRING(FILE-INFO:FILE-NAME, 1,
                      R-INDEX(FILE-INFO:FULL-PATHNAME,
                              IF OPSYS EQ "Unix":U THEN "/":U ELSE "~\":U))
  . /* ASSIGN */

  FILE-INFO:FILE-NAME = vDirName.
  IF FILE-INFO:FULL-PATHNAME EQ ?
  OR NOT FILE-INFO:FILE-TYPE MATCHES "*D*":U THEN
  DO:
    MESSAGE SUBSTITUTE("Directory &1 does not exist.", vDirName)
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
  END.

  ASSIGN vModPromsgs = vDirName + vModPromsgs.

  FILE-INFO:FILE-NAME = vModPromsgs.
  IF FILE-INFO:FULL-PATHNAME NE ? THEN
  DO:
    MESSAGE SUBSTITUTE("File &1 already exists.", vModPromsgs) SKIP
            "Remove or rename the previously modified file."
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
  END.

  ASSIGN LENGTH(vRawMsg) = vMaxMsgSize
         vUpdateCount    = 0
         vMsgNumber      = 0
  . /* ASSIGN */

  INPUT FROM VALUE(ipSrcPromsgs) BINARY.
  OUTPUT TO  VALUE(vModPromsgs) BINARY.

  /* Skip first record; it's a header of promsgs file: */
  IMPORT UNFORMATTED vRawMsg.
  PUT        CONTROL vRawMsg.

  REPEAT:
    IMPORT UNFORMATTED vRawMsg .
    ASSIGN vChrMsg      = GET-STRING(vRawMsg, 1)
           vMsgNumber   = vMsgNumber + 1
           vMsgContinue = "":U
    . /* ASSIGN */

/* Long texts (longer than 81 byte) are broken into the fragments.
   First 6 characters in the text is the "number" of next fragment.
   Fragment's offset is the "number" multiplied by 81:
*/  IF LENGTH(vChrMsg) EQ vMaxMsgSize
    AND TRIM(SUBSTRING(vChrMsg, 1, 6), "0123456789":U) EQ "":U THEN
    DO:
      ASSIGN vMsgContinue = SUBSTRING(vChrMsg, 1, 6)
             vChrMsg      = SUBSTRING(vChrMsg, 7)
      . /* ASSIGN */

/* If the specified message numbers are not used in current Progress version:*/
      FOR EACH ttMsg EXCLUSIVE-LOCK
         WHERE ttMsg.MsgNumber EQ INTEGER(vMsgContinue)
      TRANSACTION:
        DELETE ttMsg.
      END.
    END.

/* Don't allow to add the tag if the message already begins with special tag */
    IF NOT (vChrMsg BEGINS "%B":U  /*Write to both the log file and screen.*/
         OR vChrMsg BEGINS "%L":U  /*Write to the log file only. */
         OR vChrMsg BEGINS "%G":U) /*Crash process and generate a core file.*/
    THEN
UpdateMsg:
    FOR EACH ttMsg EXCLUSIVE-LOCK
       WHERE ttMsg.MsgNumber EQ vMsgNumber
    TRANSACTION:

/* It's a message that we want to modify? */
      IF NOT vChrMsg MATCHES ttMsg.MsgText THEN
      DO:
        MESSAGE
          "Mismatched text for message #" vMsgNumber SKIP
          " Expecting:" ttMsg.MsgText SKIP
          "Found text:" vChrMsg SKIP
          "The message will not be updated."
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        LEAVE UpdateMsg.
      END.

/* Modified message text: */
      ASSIGN vChrMsg = vMsgContinue + ttMsg.ModMsgTag + vChrMsg.

/* The end of current fragment should be added to the next existent fragment:*/
      IF vMsgContinue NE "":U THEN
      DO:
        CREATE bfMsgCont.
        ASSIGN bfMsgCont.ModMsgTag  = SUBSTRING(vChrMsg, vMaxMsgSize + 1)
               bfMsgCont.MsgNumber  = INTEGER(vMsgContinue)
               bfMsgCont.MsgText    = "*":U
               vChrMsg = SUBSTRING(vChrMsg, 1, vMaxMsgSize)
        . /* ASSIGN */
      END.
      ELSE
/* Does the size of the modified non-fragmented message exceed vMaxMsgSize? */
      IF LENGTH(vChrMsg) GT vMaxMsgSize THEN
/* If the message does not have a tag at the end: */
      IF INDEX(vChrMsg, "%":U, vMaxMsgSize) EQ 0 THEN
      ASSIGN vChrMsg = SUBSTRING(vChrMsg, 1, vMaxMsgSize).
      ELSE
      DO:
        MESSAGE
          "Can't modify the message number" vMsgNumber SKIP
          "because its text is too long and it has a tag at the end:" SKIP
          vChrMsg
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
        LEAVE UpdateMsg.
      END.

/* Change the current message fragment: */
      PUT-STRING(vRawMsg, 1) = vChrMsg.

/* PUT-STRING increases the size of vRawMsg by one zero byte
   when length(vRawMsg) eq length(vChrMsg). Re-set the size:
*/    IF LENGTH(vRawMsg) GT vMaxMsgSize THEN
      LENGTH(vRawMsg) = vMaxMsgSize.

      ASSIGN vUpdateCount = vUpdateCount + 1.
    END. /* FOR FIRST ttMsg */

    PUT CONTROL vRawMsg.

  END. /* REPEAT */
  INPUT  CLOSE.
  OUTPUT CLOSE.
  ASSIGN LENGTH(vRawMsg) = 0.

  FOR EACH ttMsg NO-LOCK:
    ACCUMULATE "Msg" (COUNT).
  END.

  FILE-INFO:FILE-NAME = vModPromsgs.
  IF FILE-INFO:FULL-PATHNAME NE ? THEN
  IF FILE-INFO:FILE-SIZE NE vPromsgsSize THEN
  DO:
    MESSAGE "Mismatch of file sizes:"            SKIP
            "Original file:" vPromsgsSize        SKIP
            "Modified file:" FILE-INFO:FILE-SIZE SKIP
            "The modified file will be deleted/"
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).
  END.
  ELSE
  IF vUpdateCount EQ 0 THEN
  DO:
    OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).
    MESSAGE "No updates were done."
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  END.
  ELSE
  MESSAGE vUpdateCount "of" (ACCUM COUNT "Msg") "update(s) created." SKIP
          "The modified promsgs file was created:" SKIP
          vModPromsgs
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE. /* ModifyPromsgs */

