/*------------------------------------------------------------------------
  File        : DecodeIdxKeys.p
  Purpose     : Decode the field values of index components reported by errors:
  
(8781) SYSTEM ERROR: Index <index-num>  (<owner-name>.<table-name>, <index-name>): key <key> recid <DBKEY> is out of sequence.
(8783) SYSTEM ERROR: Index <index-num>  (<owner-name>.<table-name>, <index-name>): couldn't find key <key> recid <DBKEY>.
(8827) Index <index number> (<owner name>.<table name>, <index name>):  Added key <index value> recid <record identifier>.
(8828) Index <index number> (<owner name>.<table name>, <index neme>):  Deleted key <key value> recid <record identifier>.
(8829) Index <index number> (<owner name>.<table name>, <index name>):  Found invalid key <key value> recid <record identifier>.
(8870) Index <index name> (<owner name>.<table name>, <index name>):  Added key <index value> recid <record identifier>

For example:
IDXFIX   : (8783)  SYSTEM ERROR: Index 64 (PUB.DecTable, Dec2): 
 couldn't find key <^<DD><CB><FF>> recid 1097.
In this case the decimal field Dec2 is a component of index 64
and the field value is -1.123.

  Syntax      : RUN DecodeIdxKeys.p
  Description : 
  Program checks the log of the working database (DICTDB)
  and creates two output files:
  1) DecodeIdxKeys.<db.dbCreateDate.Time.TxLastTran>.Report.<codepage>.txt
     Open file in Excel.
  2) DecodeIdxKeys.<db.dbCreateDate.Time.TxLastTran>.Errors.txt
     It's a kind of grep of the errors above.
     
  Author      : George Potemkin
  Created     : May 25, 2012
  Modified    : May 23, 2014
  Vesrion     : 1.2
1.2: The messages are linked to the indexes by <table-name>.<index-name>
     rather than by <index-num>. It allows to use a database with the structure
     that is different from one where the errors were found.
  ----------------------------------------------------------------------*/

&SCOPED-DEFINE WrkCodePage "undefined":U
&SCOPED-DEFINE Sep "~t"

DEFINE VARIABLE vDumpAuxFiles AS LOGICAL NO-UNDO INITIAL NO.

DEFINE VARIABLE vPrefix     AS CHARACTER NO-UNDO INITIAL "DecodeIdxKeys.".
DEFINE VARIABLE vCollDump   AS CHARACTER NO-UNDO.
DEFINE VARIABLE vIdxDfDump  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vDbLogFile  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vErrorLog   AS CHARACTER NO-UNDO.
DEFINE VARIABLE vReportFile AS CHARACTER NO-UNDO.

/* ------------------------------------------------------------------------- */

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE vDbCodePage  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vDbUpperCase AS INTEGER   NO-UNDO EXTENT 255.

DEFINE TEMP-TABLE ttCollationTable NO-UNDO
  FIELD CharCode   AS INTEGER FORMAT ">>9" LABEL "Cod"
  FIELD SortWeight AS INTEGER FORMAT "999" LABEL "Srt"
  INDEX CharCode AS UNIQUE
        CharCode
  INDEX SortWeight
        SortWeight
. /* DEFINE TEMP-TABLE ttCollationTable */

DEFINE TEMP-TABLE ttIndex NO-UNDO
  FIELD IndexNum   LIKE _Index._Idx-num
  FIELD TableName  LIKE _File._File-name
  FIELD IndexName  LIKE _Index._Index-name
  FIELD IsUnique   LIKE _Index._Unique
  FIELD IsActive   LIKE _Index._Active
  FIELD IsWordidx  LIKE _Index._Wordidx
  FIELD FieldCount LIKE _Index._num-comp
  FIELD FieldNames AS CHARACTER
  FIELD FieldTypes AS CHARACTER
  INDEX IndexNum IS UNIQUE
        IndexNum
. /* DEFINE TEMP-TABLE ttIndex */

DEFINE TEMP-TABLE ttIndexKey NO-UNDO
  FIELD Rec-Id      AS CHARACTER
  FIELD TableName   AS CHARACTER
  FIELD IndexName   AS CHARACTER
  FIELD IndexNum    AS CHARACTER
  FIELD ErrorNum    AS CHARACTER
  FIELD IndexKey    AS CHARACTER
  FIELD FieldCount  AS INTEGER
  FIELD FieldValue1 AS CHARACTER EXTENT 16
  FIELD FieldValue2 AS CHARACTER EXTENT 16
  FIELD LogTime     AS CHARACTER

  INDEX RecidIndexNum
        Rec-Id
        IndexNum
. /* DEFINE TEMP-TABLE ttIndexKey */

DEFINE TEMP-TABLE ttField NO-UNDO
  FIELD TableName  AS CHARACTER
  FIELD Rec-Id     AS CHARACTER
  FIELD FieldName  AS CHARACTER
  FIELD FieldType  AS CHARACTER
  FIELD FieldValue AS CHARACTER FORMAT "x(32)"
  FIELD LgMsgValue AS CHARACTER FORMAT "x(32)"
  FIELD IndexList  AS CHARACTER
  FIELD ErrorList  AS CHARACTER
  FIELD AmbigValue AS LOGICAL

  INDEX FieldValue
        Rec-Id    
        TableName
        FieldName 
        FieldValue
. /* DEFINE TEMP-TABLE ttField */


/* ------------------------------------------------------------------------- */
FUNCTION HexToDec RETURNS INTEGER (ipHex AS CHARACTER).
  DEFINE VARIABLE vHexList AS CHARACTER NO-UNDO INITIAL "0123456789ABCDEF".
  DEFINE VARIABLE vNum  AS INTEGER NO-UNDO.
  DEFINE VARIABLE vItem AS INTEGER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE n AS INTEGER NO-UNDO.

  ASSIGN vNum = 0
         n = LENGTH(ipHex).
  DO i = 1 TO n:
    ASSIGN vItem = INDEX(vHexList, SUBSTRING(ipHex, i, 1)) - 1.

    IF vItem LT 0 THEN
    RETURN ?.

    ASSIGN vNum = vNum * 16 + vItem.
  END.
  RETURN vNum.
END FUNCTION. /* HexToDec */


/* ------------------------------------------------------------------------- */
FUNCTION String2TimeStamp RETURNS CHARACTER (INPUT ipString AS CHARACTER).
/* Input string must have a format like: Tue Jul 27 12:11:45 2004 */
  DEFINE VARIABLE vDate AS DATE    NO-UNDO INITIAL ?.
  DEFINE VARIABLE vString    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vMonthList AS CHARACTER NO-UNDO
    INITIAL "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec".

  ASSIGN
    ipString = TRIM(REPLACE(ipString, "  ":U, " ":U))
    ipString = 
    /*Year: */ ENTRY(5,ipString," ":U) +
    /*Month:*/ STRING(LOOKUP (ENTRY(2,ipString," ":U),vMonthList), "99":U) +
    /*Day:  */ STRING(INTEGER(ENTRY(3,ipString," ":U)), "99":U) + ".":U +
    /*Time: */ REPLACE(ENTRY(4,ipString," ":U), ":":U, "":U)
  NO-ERROR.
  IF ERROR-STATUS:NUM-MESSAGES GT 0
  THEN RETURN "":U.
  ELSE RETURN ipString.
END FUNCTION. /* String2TimeStamp */


/* ------------------------------------------------------------------------- */
DEFINE NEW SHARED STREAM ddl.

PROCEDURE DumpCollationTables.
  DEFINE INPUT PARAMETER ipTranDfFile AS CHARACTER   NO-UNDO.
  OUTPUT STREAM ddl TO VALUE(ipTranDfFile).
  FIND DICTDB._Db NO-LOCK WHERE DICTDB._Db._Db-local EQ TRUE.
  RUN prodict/dump/_dmpdefs.p ("c", RECID(DICTDB._Db), "n").
  OUTPUT STREAM ddl CLOSE.
END PROCEDURE. /* DumpCollationTables */

/* ------------------------------------------------------------------------- */
PROCEDURE ReadCollationTable.

  DEFINE OUTPUT PARAMETER opDbCodePage AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vCollationId AS INTEGER   NO-UNDO INITIAL 1.
  DEFINE VARIABLE vUpperCaseId AS INTEGER   NO-UNDO INITIAL 3.
/* 1 = CASE-INSENSITIVE-SORT
   2 = CASE-SENSITIVE-SORT
*/
  DEFINE VARIABLE vCharCode AS INTEGER NO-UNDO.

/* CODEPAGE-NAME:         _Db-xl-name    */
/* CASE-INSENSITIVE-SORT: _Db-collate[1] */
/* UPPERCASE-MAP:         _Db-collate[3] */

   FOR FIRST DICTDB._Db NO-LOCK WHERE DICTDB._Db._Db-local EQ TRUE:

     ASSIGN opDbCodePage = DICTDB._Db._Db-xl-name.

     IF LENGTH(DICTDB._Db._Db-collate[vCollationId], "raw":U) NE 256 THEN
     DO:
       MESSAGE 
         "ReadCollationTable: length of _Db-collate[" vCollationId "] is" 
         LENGTH(DICTDB._Db._Db-collate[vCollationId], "raw":U)
         "but should be 256"
         VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       RETURN ERROR.
     END.
     DO TRANSACTION vCharCode = 0 TO 255:
       CREATE ttCollationTable.
       ASSIGN ttCollationTable.CharCode   = vCharCode
              ttCollationTable.SortWeight = 
              GETBYTE(DICTDB._Db._Db-collate[vCollationId], vCharCode + 1)
       . /* ASSIGN */
     END. /* DO TRANSACTION */

/* UPPERCASE-MAP: */
     IF LENGTH(DICTDB._Db._Db-collate[vUpperCaseId], "raw":U) NE 256
     THEN DO vCharCode = 1 TO 255:
       ASSIGN vDbUpperCase[vCharCode] = 
              ASC(UPPER( CHR(vCharCode, {&WrkCodePage}) ), {&WrkCodePage})
       . /* ASSIGN */
     END.
     ELSE DO vCharCode = 1 TO 255:
       ASSIGN vDbUpperCase[vCharCode] = 
              GETBYTE(DICTDB._Db._Db-collate[vUpperCaseId], vCharCode + 1)
       . /* ASSIGN */
     END.
   END. /* FOR FIRST DICTDB._Db */
END PROCEDURE. /* ReadCollationTable */


/* ------------------------------------------------------------------------- */
PROCEDURE LoadCollationTable.
  DEFINE INPUT  PARAMETER ipTranDfFile AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opDbCodePage AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vKeyword  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vCharCode AS INTEGER NO-UNDO.
  DEFINE VARIABLE vItem AS INTEGER NO-UNDO EXTENT 16.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  
  INPUT FROM VALUE(ipTranDfFile).
  REPEAT:
    ASSIGN vKeyword = "".
    IMPORT vKeyword opDbCodePage.
    IF vKeyword EQ "CODEPAGE-NAME":U THEN
    LEAVE.
  END.
  
  IF SEEK(INPUT) NE ? THEN
  REPEAT:
    ASSIGN vKeyword = "".
    IMPORT vKeyword.
    IF vKeyword EQ "CASE-INSENSITIVE-SORT":U THEN
    LEAVE.
  END.
  
  ASSIGN vCharCode = 0.
  IF SEEK(INPUT) NE ? THEN
  REPEAT:
    ASSIGN vKeyword = "".
    IMPORT vKeyword vItem.
    IF NOT vKeyword BEGINS "/*":U THEN
    LEAVE.
    DO TRANSACTION i = 1 TO EXTENT(vItem):
      CREATE ttCollationTable.
      ASSIGN ttCollationTable.CharCode   = vCharCode
             ttCollationTable.SortWeight = vItem[i]
             vCharCode = vCharCode + 1
      . /* ASSIGN */
    END.
  END.
  INPUT CLOSE.
  IF vCharCode NE 256 THEN
  DO:
    MESSAGE "Failed to load _tran.df, the number of codes:" vCharCode
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN ERROR.
  END.
END PROCEDURE. /* LoadCollationTable */


/* ------------------------------------------------------------------------- */
PROCEDURE ReadIndexDefinitions.
  DEFINE VARIABLE vFieldNames AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE vFieldTypes AS CHARACTER   NO-UNDO.
  
  FOR FIRST DICTDB._Db NO-LOCK WHERE DICTDB._Db._Db-local EQ TRUE,
       EACH DICTDB._File  OF DICTDB._Db NO-LOCK
      WHERE DICTDB._File._File-Number LT 32768   /* include Metadata Tables */
        AND DICTDB._File._File-Number GT -16385, /* but exclude VSTs */
       EACH DICTDB._Index OF DICTDB._File NO-LOCK:
   
    ASSIGN vFieldNames = "":U
           vFieldTypes = "":U.

    FOR EACH DICTDB._Index-Field OF DICTDB._Index NO-LOCK,
       FIRST DICTDB._Field OF DICTDB._Index-Field NO-LOCK
          BY DICTDB._Index-Field._Index-Seq:
  
      ASSIGN
        vFieldNames = vFieldNames + DICTDB._Field._Field-name + ","
        vFieldTypes = vFieldTypes + DICTDB._Field._Data-Type  + ",".
    END.
  
    CREATE ttIndex.
    ASSIGN ttIndex.IndexNum   = DICTDB._Index._Idx-num
           ttIndex.TableName  = DICTDB._File._File-name
           ttIndex.IndexName  = DICTDB._Index._Index-name
           ttIndex.IsUnique   = DICTDB._Index._Unique
           ttIndex.IsActive   = DICTDB._Index._Active
           ttIndex.IsWordidx  = DICTDB._Index._Wordidx
           ttIndex.FieldCount = DICTDB._Index._num-comp
           ttIndex.FieldNames = SUBSTRING(vFieldNames,1,LENGTH(vFieldNames) - 1)
           ttIndex.FieldTypes = SUBSTRING(vFieldTypes,1,LENGTH(vFieldTypes) - 1)
    . /* ASSIGN */
  END.
END PROCEDURE. /* ReadIndexDefinitions */


/* ------------------------------------------------------------------------- */
PROCEDURE DumpIndexDefinitions.
  DEFINE INPUT PARAMETER ipIdxDfDump AS CHARACTER NO-UNDO.

  OUTPUT TO VALUE(ipIdxDfDump).
  FOR EACH ttIndex NO-LOCK:
    EXPORT ttIndex.
  END.
  OUTPUT CLOSE.
END PROCEDURE. /* DumpIndexDefinitions */


/* ------------------------------------------------------------------------- */
PROCEDURE LoadIndexDefinitions.
  DEFINE INPUT PARAMETER ipIdxDfDump AS CHARACTER NO-UNDO.

  INPUT FROM VALUE(ipIdxDfDump).
  REPEAT:
    CREATE ttIndex.
    IMPORT ttIndex.
  END.
  DELETE ttIndex.
  INPUT CLOSE.
END PROCEDURE. /* LoadIndexDefinitions */


/* ------------------------------------------------------------------------- */
PROCEDURE KeysFromDbLog.
  DEFINE INPUT PARAMETER ipDbLogFile AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipErrorLog  AS CHARACTER NO-UNDO.

/* Program parses the messages:
[2012/01/23@18:57:48.473+0400] P-8913140    T-1     I IDXFIX   : 
(8781) SYSTEM ERROR: Index<index-num>  (<owner-name>.<table-name>, <index-name>): key <key> recid <DBKEY> is out of sequence.
(8783) SYSTEM ERROR: Index 13 (PUB.op-entry, entry-date): couldn't find key <<83>><2455903><<BB>><<83><91><A5><99><81><A3><95>7><183179585><1> recid 395611985.
(8827) Index 13 (PUB.op-entry, entry-date):  Added key <<83>><2455903><<BB>><<83><91><A5><99><81><A3><95>7><183179585><1> recid 395611985.
(8828) Index <index number> (<owner name>.<table name>, <index neme>):  Deleted key <key value> recid <record identifier>.
(8829) Index 13 (PUB.op-entry, entry-date):  Found invalid key <<83>><2455903><<BB>><<89>0051<B3><9D><8D>><183179585><1> recid 395611985.
(8870) Index <index name> (<owner name>.<table name>, <index name>):  Added key <index value> recid <record identifier>.
*/
  DEFINE VARIABLE vIndexKey     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vIndexMsgItem AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vIndexNumItem AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vIndexKeyItem AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vItem         AS CHARACTER NO-UNDO EXTENT 100.
  DEFINE VARIABLE vName         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vRecid        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vMsgCount     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vErrCount     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i AS INTEGER  NO-UNDO.

  INPUT FROM VALUE(ipDbLogFile).
  OUTPUT TO VALUE(ipErrorLog).

  ASSIGN vMsgCount = 0
         vErrCount = 0.

ReadDbLog:
  REPEAT:
    ASSIGN vItem = "".
    IMPORT vItem.

    IF vItem[6] MATCHES "(*)":U THEN
    ASSIGN vIndexMsgItem = 6.
    ELSE
    IF vItem[7] MATCHES "(*)":U THEN
    ASSIGN vIndexMsgItem = 7.
    ELSE
    NEXT ReadDbLog.

    CASE vItem[vIndexMsgItem]:
      WHEN "(8781)" THEN
/*(8781) SYSTEM ERROR: Index <index-num>  (<owner-name>.<table-name>, <index-name>): key <key> recid <DBKEY> is out of sequence.*/
      ASSIGN vIndexNumItem = vIndexMsgItem + 4
             vIndexKeyItem = vIndexMsgItem + 8.

      WHEN "(8783)"  THEN
/*(8783) SYSTEM ERROR: Index <index-num>  (<owner-name>.<table-name>, <index-name>): couldn't find key <key> recid <DBKEY>.*/
      ASSIGN vIndexNumItem = vIndexMsgItem + 4
             vIndexKeyItem = vIndexMsgItem + 10.

      WHEN "(8827)" THEN
/*(8827) Index <index number> (<owner name>.<table name>, <index name>):  Added key <index value> recid <record identifier>.*/
      ASSIGN vIndexNumItem = vIndexMsgItem + 3
             vIndexKeyItem = vIndexMsgItem + 7.

      WHEN "(8828)"  THEN
/*(8828) Index <index number> (<owner name>.<table name>, <index neme>):  Deleted key <key value> recid <record identifier>.*/
      ASSIGN vIndexNumItem = vIndexMsgItem + 2
             vIndexKeyItem = vIndexMsgItem + 7.

      WHEN "(8829)" THEN
/*(8829) Index <index number> (<owner name>.<table name>, <index name>):  Found invalid key <key value> recid <record identifier>.*/
      ASSIGN vIndexNumItem = vIndexMsgItem + 2
             vIndexKeyItem = vIndexMsgItem + 8.

      WHEN "(8870)"  THEN
/*(8870) Index <index name> (<owner name>.<table name>, <index name>):  Added key <index value> recid <record identifier>*/
      ASSIGN vIndexNumItem = vIndexMsgItem + 2
             vIndexKeyItem = vIndexMsgItem + 7.

      OTHERWISE NEXT ReadDbLog.

    END CASE.

    ASSIGN vMsgCount = vMsgCount + 1
           vIndexKey = vItem[vIndexKeyItem].
    DO i = vIndexKeyItem + 1 TO EXTENT(vItem)
      WHILE vItem[i] NE "recid":U:
      ASSIGN vIndexKey = vIndexKey + " ":U + vItem[i].
    END.
    ASSIGN vRecid = vItem[i + 1].

    DO i = 1 TO EXTENT(vItem)
      WHILE vItem[i] NE "":U:
      PUT UNFORMATTED vItem[i] SPACE.
    END.

    DO TRANSACTION:
      CREATE ttIndexKey.
      ASSIGN ttIndexKey.Rec-Id   = TRIM(vRecid, ".":U)
             ttIndexKey.IndexNum = vItem[vIndexNumItem]
             ttIndexKey.ErrorNum = TRIM(vItem[vIndexMsgItem], "()":U)
             ttIndexKey.IndexKey = vIndexKey
             ttIndexKey.LogTime  = vItem[1]

/* SYSTEM ERROR: Index <index-num> (<owner-name>.<table-name>, <index-name>):*/
             vName = vItem[vIndexNumItem + 1]
             vName = ENTRY(2, vName, ".":U)
             vName = TRIM(vName, ",":U)
             ttIndexKey.TableName = vName

             vName = vItem[vIndexNumItem + 2]
             vName = TRIM(vName, "):":U)
             ttIndexKey.IndexName = vName
      . /* ASSIGN */

      RUN ParseIndexKey(INPUT vIndexKey,
                       OUTPUT ttIndexKey.FieldCount,
                       OUTPUT ttIndexKey.FieldValue1,
                       OUTPUT ttIndexKey.FieldValue2).
    END. /* DO TRANSACTION */

    FIND FIRST ttIndex NO-LOCK
         WHERE ttIndex.TableName EQ ttIndexKey.TableName
           AND ttIndex.IndexName EQ ttIndexKey.IndexName
    NO-ERROR.

    IF NOT AVAILABLE ttIndex THEN
    DO:
      ASSIGN vErrCount = vErrCount + 1.
      PUT UNFORMATTED SKIP SUBSTITUTE(
        "Error: Index &1.&2 not found.",
                     /*1*/ ttIndexKey.TableName,
                     /*2*/ ttIndexKey.IndexName)
      SKIP(1).
      NEXT ReadDbLog.
    END.

    IF ttIndex.IndexNum NE INTEGER(ttIndexKey.IndexNum) THEN
    PUT UNFORMATTED SKIP SUBSTITUTE(
       "Warning: Mismatched index numbers. Log: &1, Schema: &2",
                                    /*1*/ ttIndexKey.IndexNum, 
                                    /*2*/ STRING(ttIndex.IndexNum))
    SKIP(1).

    IF ttIndex.FieldCount NE ttIndexKey.FieldCount THEN
    DO:
      ASSIGN vErrCount = vErrCount + 1.
      PUT UNFORMATTED SKIP SUBSTITUTE(
        "Error: Mismatched number of fields. Expected: &1, Found: &2",
                                    /*1*/ STRING(ttIndex.FieldCount), 
                                    /*2*/ STRING(ttIndexKey.FieldCount))
      SKIP(1).
    END.
    PUT UNFORMATTED SKIP(1).

  END. /* REPEAT: IMPORT */
  INPUT CLOSE.
  OUTPUT CLOSE.

  IF vMsgCount EQ 0 THEN
  DO:
    MESSAGE
      SUBSTITUTE("Index error messages were not found in &1.",
                 /*1*/ ipDbLogFile)                                  SKIP
      "Expected messages:"                                           SKIP
      "(8781) SYSTEM ERROR: Index : key  recid  is out of sequence." SKIP
      "(8783) SYSTEM ERROR: Index : couldn't find key  recid ."      SKIP
      "(8827) Index :  Added key  recid ."                           SKIP
      "(8828) Index :  Deleted key  recid ."                         SKIP
      "(8829) Index :  Found invalid key  recid ."                   SKIP
      "(8870) Index :  Added key  recid ."
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN ERROR.
  END.
  ELSE
  IF vErrCount GT 0 THEN
  MESSAGE
    SUBSTITUTE("Failed to process &1 of &2 index error messages.", 
               /*1*/ STRING(vErrCount), /*2*/ STRING(vMsgCount)) SKIP
    "See" ipErrorLog
    VIEW-AS ALERT-BOX WARNING BUTTONS OK.

END PROCEDURE. /* KeysFromDbLog */


/* ------------------------------------------------------------------------- */
FUNCTION DecodeChrKey RETURNS CHARACTER (ipFieldValue AS CHARACTER).

  DEFINE VARIABLE vFieldPos   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vFieldValue AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vCharValue  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vCodeValue  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vCodeCount  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vCodeArray  AS INTEGER   NO-UNDO EXTENT 256.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  ASSIGN vFieldValue = "".
  DO vFieldPos = LENGTH(ipFieldValue) TO 1 BY -1:

    ASSIGN vCodeCount = 0
           vCharValue = "":U.
/* Create the list of characters that have the same weight: */
CollationTable:
    FOR EACH ttCollationTable NO-LOCK
       WHERE ttCollationTable.SortWeight EQ
             ASC(SUBSTRING(ipFieldValue, vFieldPos, 1), {&WrkCodePage}):

      ASSIGN vCodeValue = vDbUpperCase[ttCollationTable.CharCode].
      DO i = 1 TO vCodeCount:
        IF vCodeValue EQ vCodeArray[i] THEN
        NEXT CollationTable.
      END.
      ASSIGN vCodeCount = vCodeCount + 1
             vCodeArray[vCodeCount] = vCodeValue
             vCharValue = vCharValue + CHR(vCodeValue, {&WrkCodePage}).
    END. /* FOR EACH ttCollationTable */

/* If more than one characters have the same weight then put them in [] */
    ASSIGN vFieldValue = (IF vCodeCount EQ 0 THEN "[?]":U ELSE
                          IF vCodeCount GT 1 THEN "[":U + vCharValue + "]":U
                                             ELSE vCharValue)
                       + vFieldValue.
  END. /* DO vFieldPos = */

  RETURN vFieldValue.
  
END FUNCTION. /* DecodeChrKey */


/* ------------------------------------------------------------------------- */
FUNCTION DecodeDecKey RETURNS CHARACTER (ipString AS CHARACTER).

  DEFINE VARIABLE vDecPos AS INTEGER     NO-UNDO.
  DEFINE VARIABLE vNegative AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE vPosition AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLoNibble AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vHiNibble AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vDecString   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDigit    AS CHARACTER NO-UNDO EXTENT 16
    INITIAL ['', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ?].


  ASSIGN vDecPos   = ASC(SUBSTRING(ipString, 1, 1), {&WrkCodePage})
         vNegative = vDecPos LT 160 /* 160=0xA0 95=255-160 */
         vDecPos   = IF vNegative THEN 95 - vDecPos ELSE vDecPos - 160.

  ASSIGN vDecString = "":U.
  DO vPosition = LENGTH(ipString) TO 2 BY -1:
    ASSIGN vHiNibble  = ASC(SUBSTRING(ipString, vPosition, 1), {&WrkCodePage})
           vHiNibble  = 255 - vHiNibble WHEN vNegative
           vLoNibble  = vHiNibble MOD 16
           vHiNibble  = TRUNCATE(vHiNibble / 16, 0)
           vDecString = vDigit[vHiNibble + 1]
                      + vDigit[vLoNibble + 1]
                      + vDecString
    . /* ASSIGN */
  END.

  ASSIGN vDecString = vDecString + FILL("0":U, vDecPos - LENGTH(vDecString))
                   WHEN vDecPos GT LENGTH(vDecString)
         vDecString = SUBSTRING(vDecString, 1, vDecPos) + "."
                 + SUBSTRING(vDecString, vDecPos + 1)
         vDecString = "-" + vDecString
                   WHEN vNegative.

  RETURN IF vDecString MATCHES "*0":U 
         THEN SUBSTRING(vDecString, 1, LENGTH(vDecString) - 1)
         ELSE vDecString.
END FUNCTION. /* DecodeDecKey */


/* ------------------------------------------------------------------------- */
PROCEDURE ParseIndexKey.
  DEFINE  INPUT PARAMETER ipIndexKey    AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opFieldCount  AS INTEGER   NO-UNDO.
  DEFINE OUTPUT PARAMETER opFieldValue1 AS CHARACTER NO-UNDO EXTENT 16.
  DEFINE OUTPUT PARAMETER opFieldValue2 AS CHARACTER NO-UNDO EXTENT 16.

  DEFINE VARIABLE vFieldValue AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vHexChars   AS CHARACTER NO-UNDO INITIAL "1234567890ABCDEF":U.
  DEFINE VARIABLE vCurrPos    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vFldBgn     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vFldEnd     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vHexBgn     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vChar       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vKeyLength  AS INTEGER  NO-UNDO.

  ASSIGN
    opFieldCount = 0
    vFldBgn = 0
    vHexBgn = 0
    vFieldValue = ""
    vKeyLength  = LENGTH(ipIndexKey)
  . /* ASSIGN */

  REPEAT vCurrPos = 1 TO vKeyLength:

    ASSIGN vChar = SUBSTRING(ipIndexKey, vCurrPos, 1).

    CASE vChar:
      WHEN "<":U THEN
      DO:
  /* If previous position is the end of field then 
     the current position is the beginning of next field:
  */    IF vCurrPos EQ vFldEnd + 1
        THEN ASSIGN vFldBgn = vCurrPos
                    vChar = "":U.
        ELSE 
        IF vHexBgn GT vFldBgn /* there was a false beginning of HEX */
        THEN ASSIGN vChar   = SUBSTRING(ipIndexKey, vHexBgn, vCurrPos - vHexBgn)
                    vHexBgn = vCurrPos.
        ELSE ASSIGN vChar   = "":U
                    vHexBgn = vCurrPos.
      END.
  /*  WHEN "<":U */

      WHEN ">":U THEN
        IF vHexBgn GT vFldBgn
        THEN
  /* Does "<*>" look like the end of hex value: */
          IF vCurrPos - vHexBgn EQ 3 AND
             TRIM(SUBSTRING(ipIndexKey, vHexBgn + 1, 2), vHexChars) EQ "":U
          THEN /* found hex value */
          ASSIGN vChar = SUBSTRING(ipIndexKey, vHexBgn + 1, 2)
                 vChar = CHR(HexToDec(vChar), {&WrkCodePage})
                 vHexBgn = 0
                 .
          ELSE /* all chars since last "<" sign are just the chars: */
          ASSIGN vChar = SUBSTRING(ipIndexKey, vHexBgn, vCurrPos - vHexBgn + 1)
                 vHexBgn = 0.
        ELSE
  /* If next position is the beginning of next field then 
     the current position is the end of current field:
  */    IF vCurrPos EQ vKeyLength 
        OR SUBSTRING(ipIndexKey, vCurrPos + 1, 1) EQ "<":U
        THEN /* it's the end of field value*/
          ASSIGN
            vFldEnd = vCurrPos
            opFieldCount = opFieldCount + 1
            opFieldValue1[opFieldCount] = SUBSTRING(ipIndexKey, 
                                 vFldBgn + 1, vFldEnd - vFldBgn - 1)
            opFieldValue2[opFieldCount] = vFieldValue
            vFieldValue = ""
            vChar = "".
     /* ELSE it's just a ">" sign. */
  /*  WHEN ">":U */

      OTHERWISE
        IF vHexBgn GT vFldBgn THEN /* the char might be a part of HEX value: */
        ASSIGN vChar = "".

    END CASE. /* vChar */

    ASSIGN vFieldValue = vFieldValue + vChar.

  END. /* REPEAT vCurrPos = */

END PROCEDURE. /* ParseIndexKey */


/* ------------------------------------------------------------------------- */
PROCEDURE ReadRecord.
  DEFINE INPUT PARAMETER ipTableName  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipRecid      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipDbCodePage AS CHARACTER NO-UNDO.

  DEFINE VARIABLE hQuery  AS HANDLE NO-UNDO.
  DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
  DEFINE VARIABLE hField  AS HANDLE NO-UNDO.
  DEFINE VARIABLE vFieldName  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFieldValue AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  CREATE QUERY hQuery.
  CREATE BUFFER hBuffer FOR TABLE ipTableName.
  hQuery:SET-BUFFERS(hBuffer).

  hQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 NO-LOCK WHERE RECID(&1) EQ &2",
                                   ipTableName, ipRecid)).
  hQuery:QUERY-OPEN.
  hQuery:GET-NEXT() NO-ERROR.

  IF NOT hQuery:QUERY-OFF-END THEN 
  DO TRANSACTION i = 1 TO hBuffer:NUM-FIELDS:

    ASSIGN hField      = hBuffer:BUFFER-FIELD(i)
           vFieldName  = hField:NAME
           vFieldValue = TRIM(STRING(hField:BUFFER-VALUE))
    . /* ASSIGN */

    FIND FIRST ttField
         WHERE ttField.Rec-Id     EQ ipRecid
           AND ttField.TableName  EQ ipTableName
           AND ttField.FieldName  EQ vFieldName
           AND ttField.FieldValue EQ vFieldValue
    NO-ERROR.

    IF NOT AVAILABLE ttField THEN
    CREATE ttField.
    ASSIGN ttField.Rec-Id     = ipRecid
           ttField.TableName  = ipTableName
           ttField.FieldName  = vFieldName
           ttField.FieldValue = CODEPAGE-CONVERT(vFieldValue, ipDbCodePage)
           ttField.IndexList  = ttField.IndexList
                              + MIN(",":U, ttField.IndexList)
                              + "recid"
    . /* ASSIGN */
  END.

  hQuery:QUERY-CLOSE.
  DELETE WIDGET hQuery.
  DELETE WIDGET hBuffer.
  IF VALID-HANDLE(hField) THEN
  DELETE WIDGET hField.
END PROCEDURE. /* ReadRecord */


/* ------------------------------------------------------------------------- */
PROCEDURE IndexKeyReport.

  DEFINE INPUT PARAMETER ipReportFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vRecid      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vTableName  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vIndexName  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFieldNum   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vFieldName  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFieldType  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFieldValue AS CHARACTER NO-UNDO.

  DEFINE BUFFER bttIndexKey FOR ttIndexKey.
  DEFINE BUFFER bttField    FOR ttField.

  OUTPUT TO VALUE(ipReportFile) NO-CONVERT.
  PUT UNFORMATTED
           "Table"
    {&Sep} "Recid"
    {&Sep} "Field"
    {&Sep} "Type"
    {&Sep} "Value"
    {&Sep} "LgMsg"
    {&Sep} "Ambig"
    {&Sep} "#Idx"
    {&Sep} "IdxLst"
    {&Sep} "ErrLst"
  SKIP.

  ASSIGN vRecid = "".

RecidLoop:
  REPEAT:

    FIND FIRST bttIndexKey NO-LOCK
         WHERE bttIndexKey.Rec-Id GT vRecid /* INDEX RecidIndexNum */
    NO-ERROR.

    IF NOT AVAILABLE bttIndexKey THEN
    LEAVE RecidLoop.

    ASSIGN vRecid = bttIndexKey.Rec-Id.

    FOR EACH ttIndexKey NO-LOCK
       WHERE ttIndexKey.Rec-Id EQ vRecid:

      FIND FIRST ttIndex NO-LOCK
           WHERE ttIndex.TableName EQ ttIndexKey.TableName
             AND ttIndex.IndexName EQ ttIndexKey.IndexName
      NO-ERROR.

      IF AVAILABLE ttIndex THEN
      ASSIGN vTableName = ttIndex.TableName
             vIndexName = ttIndex.IndexName.
      ELSE
      ASSIGN vTableName = ?
             vIndexName = "Idx#" + ttIndexKey.IndexNum.
      
      DO TRANSACTION vFieldNum = 1 TO ttIndexKey.FieldCount:

        IF AVAILABLE ttIndex AND vFieldNum LE ttIndex.FieldCount
        THEN ASSIGN vFieldName = ENTRY(vFieldNum, ttIndex.FieldNames)
                    vFieldType = ENTRY(vFieldNum, ttIndex.FieldTypes).
        ELSE ASSIGN vFieldName = vIndexName + "fld#" + STRING(vFieldNum)
                    vFieldType = ?.

        ASSIGN vFieldValue = ttIndexKey.FieldValue2[vFieldNum].
        CASE vFieldType:

          WHEN "DATE":U THEN
          ASSIGN vFieldValue = STRING(DATE(INTEGER(vFieldValue))) NO-ERROR.
          WHEN "DECIMAL":U THEN
          ASSIGN vFieldValue = DecodeDecKey(vFieldValue).

          WHEN "CHARACTER":U OR WHEN ? THEN
          ASSIGN vFieldValue = DecodeChrKey(vFieldValue).

        END CASE.

        FIND FIRST ttField
             WHERE ttField.Rec-Id     EQ vRecid
               AND ttField.FieldName  EQ vFieldName
               AND ttField.FieldValue EQ vFieldValue
        NO-ERROR.

        IF NOT AVAILABLE ttField THEN
        CREATE ttField.
        ASSIGN ttField.Rec-Id     = vRecid
               ttField.TableName  = vTableName
               ttField.FieldName  = vFieldName
               ttField.FieldType  = vFieldType
               ttField.FieldValue = vFieldValue
               ttField.LgMsgValue = ttIndexKey.FieldValue1[vFieldNum]
               ttField.IndexList  = ttField.IndexList
                                  + MIN(",":U, ttField.IndexList)
                                  + vIndexName
               WHEN NOT CAN-DO(ttField.IndexList, vIndexName)
               ttField.ErrorList  = ttField.ErrorList
                                  + MIN(",":U, ttField.ErrorList)
                                  + ttIndexKey.ErrorNum
               WHEN NOT CAN-DO(ttField.ErrorList, ttIndexKey.ErrorNum)
        . /* ASSIGN */
      END.

      IF vTableName NE ? AND NOT CAN-FIND(
         FIRST ttField
         WHERE ttField.Rec-Id    EQ vRecid    
           AND ttField.TableName EQ vTableName
           AND CAN-DO(ttField.IndexList, "recid":U))
      THEN RUN ReadRecord(vTableName, vRecid, vDbCodePage).

    END. /* FOR EACH ttIndexKey */

/* Set ttField.AmbigValue: TRUE if there are more than one value per recid. */
    FOR EACH ttField
       WHERE ttField.Rec-Id EQ vRecid:

      ASSIGN ttField.AmbigValue = CAN-FIND(
      FIRST bttField
      WHERE bttField.TableName  EQ ttField.TableName
        AND bttField.Rec-Id     EQ ttField.Rec-Id
        AND bttField.FieldName  EQ ttField.FieldName
        AND bttField.FieldValue NE ttField.FieldValue).
    END. /* FOR EACH ttField */

    FOR EACH ttField NO-LOCK
       WHERE ttField.Rec-Id EQ vRecid
          BY ttField.Rec-Id
          BY ttField.TableName
          BY ttField.AmbigValue DESCENDING
          BY ttField.FieldName:

      PUT UNFORMATTED
               /* Table  */ ttField.TableName 
        {&Sep} /* Recid  */ ttField.Rec-Id    
        {&Sep} /* Field  */ ttField.FieldName 
        {&Sep} /* Type   */ ttField.FieldType 
        {&Sep} /* Value  */ ttField.FieldValue
        {&Sep} /* LgMsg  */ ttField.LgMsgValue
        {&Sep} /* Ambig  */ ttField.AmbigValue
        {&Sep} /* #Idx   */ NUM-ENTRIES(ttField.IndexList)
        {&Sep} /* IdxLst */ ttField.IndexList 
        {&Sep} /* Errors */ ttField.ErrorList
      SKIP.
    END. /* FOR EACH ttField */
  END. /* RecidLoop: REPEAT */

  OUTPUT CLOSE.

END PROCEDURE. /* IndexKeyReport */


/* ------------------------------------------------------------------------- */
FOR FIRST DICTDB._MstrBlk NO-LOCK:
  ASSIGN
    vPrefix = vPrefix
            + LDBNAME("DICTDB") + ".":U
            + String2TimeStamp(_MstrBlk._MstrBlk-crdate)
            + ".Tx":U + STRING(_MstrBlk._MstrBlk-lasttask) + "."
    vCollDump   = vPrefix + "_tran.df"
    vIdxDfDump  = vPrefix + "Indexes.d"
    vErrorLog   = vPrefix + "Errors.log"
    vReportFile = vPrefix + "Report.txt" /* it will be reset later */
  . /* ASSIGN */
END.

FOR FIRST DICTDB._FileList NO-LOCK
    WHERE DICTDB._FileList._FileList-Name MATCHES "*~~.db":

  ASSIGN vDbLogFile = SUBSTRING(DICTDB._FileList._FileList-Name, 1, 
                         LENGTH(DICTDB._FileList._FileList-Name) - 3) + ".lg".
/*
  ASSIGN vDbLogFile = 
"D:\Support\VTB24\2012.01.23 - 1422 Index could not be deleted\PSTS\Vrn\op-entry.vrn.2012.01.24.lg"
*/
  FILE-INFO:FILE-NAME = vDbLogFile.
  IF FILE-INFO:FULL-PATHNAME EQ ? THEN
  DO:
    MESSAGE "Db log not found:" vDbLogFile
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    QUIT.
  END.
END.

RUN ReadCollationTable(OUTPUT vDbCodePage).
IF vDumpAuxFiles THEN
RUN DumpCollationTables(INPUT vCollDump).
/*
EMPTY TEMP-TABLE ttCollationTable.
ASSIGN vCollDump =
"D:\Support\VTB24\2012.01.23 - 1422 Index could not be deleted\PSTS\Vrn\op-entry.vrn.2012.04.11._tran.df".
RUN LoadCollationTable(INPUT vCollDump, OUTPUT vDbCodePage).
*/
RUN ReadIndexDefinitions.
IF vDumpAuxFiles THEN
RUN DumpIndexDefinitions(INPUT vIdxDfDump).
/*
EMPTY TEMP-TABLE ttIndex.
RUN LoadIndexDefinitions(INPUT vIdxDfDump).
*/
RUN KeysFromDbLog(INPUT vDbLogFile, INPUT vErrorLog).

IF ERROR-STATUS:ERROR THEN
RETURN.

ASSIGN vReportFile = vPrefix + "Report." + STRING(vDbCodePage) + ".txt".
RUN IndexKeyReport(INPUT vReportFile).

FILE-INFO:FILE-NAME = vReportFile.
MESSAGE
  "See result in" FILE-INFO:FULL-PATHNAME SKIP
  "Codepage:" vDbCodePage
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

