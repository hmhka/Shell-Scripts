/* ----------------------------------------------------------------------------
    File        : ObjectInfo.p
    Purpose     : Report the basic information about database storage objects.

    Syntax      : Connect all databases you need to process and run the program

    Description : The output file can be opened in Excel.

    Author(s)   : George Potemkin
    Created     : January 17, 2015
    Modified    : March 01, 2015
    Version     : 2.5

    The most recent version can be downloaded from here:
    ftp://ftp.progress-tech.ru/pub/Users/george/Programs/ObjectInfo.p

    Fixes       :

V2.1: Report a physical database name rather than its logical name.
V2.2: Added a dbkey of sequence block.
      Its "Object Info" column stores the metaschema timestamp.
V2.2: The name of output file includes the most recent metaschema timestamp
      rather than the current date/time. So the file name will be changed
      only if the structure of one of the databases will be changed.
      Added TIMEZONE to the table's LastChange.
V2.3: The name of output file includes the timestamp of the most recent change
      of the application's table (_File._Last-Change). Note that metaschema
      timestamp (_MstrBlk-timestamp) will be changed if you edit the list of
      security administrators (_File._Can-create for _File where _File-Name
      eq "_User") but _File._Last-Change will not be updated.
V2.4: The name of output file includes the timestamp of the most recent change
      of the application's table (_File._Last-Change) and md5 digest of output
      file except the "sequence" lines. In other words the file name will be
      changed if there are any changes in db objects.
      The "sequence" lines contains two timestamps:
      1. Db metaschema timestamp ("Object Info" column);
      2. The current time when program was run ("Object Attr" column).
      These timestamps are not participate in the calcualtion of md5 digest.
V2.5 (Febrary 27, 2015):
      Removed the "HWM" column.
      Added the "Object CRC" column (_File._CRC, _Index._Idx-CRC).
      Added the sequences.

---------------------------------------------------------------------------- */

/* ******************************  Temp-tables  **************************** */

DEFINE TEMP-TABLE tt_Db NO-UNDO
  FIELD mb_crdate    LIKE _MstrBlk._MstrBlk-crdate
  FIELD mb_timestamp LIKE _MstrBlk._MstrBlk-timestamp
  FIELD mb_dbvers    LIKE _MstrBlk._MstrBlk-dbvers
  FIELD mb_dbrecid   LIKE _File._Db-recid
  FIELD DbGuid       LIKE _Db._Db-guid
  FIELD ObjectBlock  LIKE _StorageObject._Object-block
  FIELD CreateLimit  LIKE _StorageObject._Create-Limit
  FIELD TossLimit    LIKE _StorageObject._Toss-Limit
 /* _MstrBlk-lasttask */
. /* TEMP-TABLE tt_Db */

DEFINE TEMP-TABLE tt_Sequence NO-UNDO
  FIELD SeqNumber LIKE _Sequence._Seq-Num  
  FIELD SeqOwner  LIKE _Sequence._Seq-Owner
  FIELD SeqName   LIKE _Sequence._Seq-Name
  FIELD SeqInit   LIKE _Sequence._Seq-Init
  FIELD SeqIncr   LIKE _Sequence._Seq-Incr
  INDEX SeqNumber IS UNIQUE
        SeqNumber
. /* TEMP-TABLE tt_Sequence */

DEFINE TEMP-TABLE tt_Area NO-UNDO
  FIELD AreaNumber      LIKE _Area._Area-Number
  FIELD AreaName        LIKE _Area._Area-Name
  FIELD AreaType        LIKE _Area._Area-type
  FIELD AreaBlockSize   LIKE _Area._Area-blocksize
  FIELD AreaRecPerBlock LIKE _Area._Area-recbits
  FIELD AreaClusterSize LIKE _Area._Area-clustersize
  FIELD AreaAttrib      LIKE _Area._Area-attrib
  INDEX AreaNumber IS UNIQUE
        AreaNumber
. /* TEMP-TABLE tt_Area */

DEFINE TEMP-TABLE tt_File NO-UNDO
  FIELD TableNumber LIKE _File._File-Number
  FIELD TableOwner  LIKE _File._Owner
  FIELD TableName   LIKE _File._File-Name
  FIELD TableFields LIKE _File._numfld
  FIELD TableCRC    LIKE _File._CRC
  FIELD Template    LIKE _File._Template
  FIELD PrimeIndex  LIKE _File._Prime-Index
  FIELD dft-pk      LIKE _File._dft-pk
  FIELD FileRecid   LIKE _Index._File-recid
  FIELD LastChange  LIKE _File._Last-Change
  FIELD NumKey      LIKE _File._numkey
  FIELD NumFld      LIKE _File._numfld
  FIELD NumKFld     LIKE _File._numkfld
  FIELD NumKComp    LIKE _File._numkcomp
  INDEX TableNumber IS UNIQUE
        TableNumber
  INDEX FileRecid   IS UNIQUE
        FileRecid
. /* TEMP-TABLE tt_File */

DEFINE TEMP-TABLE tt_Index NO-UNDO
  FIELD IndexNumber LIKE _Index._Idx-Num
  FIELD IndexOwner  LIKE _Index._Idxowner
  FIELD IndexName   LIKE _Index._Index-Name
  FIELD IndexFields LIKE _Index._num-comp
  FIELD FileRecid   LIKE _Index._File-recid
  FIELD IndexActive LIKE _Index._Active
  FIELD IndexUnique LIKE _Index._Unique
  FIELD IndexCRC    LIKE _Index._Idx-CRC
  FIELD WordIdx     LIKE _Index._Wordidx
  FIELD NumComp     LIKE _Index._num-comp
  INDEX IndexNumber IS UNIQUE
        IndexNumber
. /* TEMP-TABLE tt_Index */

DEFINE TEMP-TABLE tt_Field NO-UNDO
  FIELD FileRecid      LIKE _Field._File-recid
  FIELD FieldName      LIKE _Field._Field-Name
  FIELD DataType       LIKE _Field._Data-Type
  FIELD Fld-stdtype    LIKE _Field._Fld-stdtype
  FIELD Fld-stlen      LIKE _Field._Fld-stlen      /* Object number */
  FIELD Fld-misc2      LIKE _Field._Fld-misc2      /* LOB-Size       */
  FIELD Fld-case       LIKE _Field._Fld-case       /* case-sensitive */
  FIELD FieldCharset   LIKE _Field._Charset        /* CLOB-CODEPAGE  */
  FIELD FieldCollation LIKE _Field._Collation      /* CLOB-COLLATION */
  FIELD Attributes1    LIKE _Field._Attributes1    /* CLOB-TYPE      */
  INDEX Fld-stdtype IS UNIQUE
        Fld-stdtype
. /* TEMP-TABLE tt_Field */

DEFINE TEMP-TABLE tt_StorageObject NO-UNDO
  FIELD ObjectType   LIKE _StorageObject._Object-type
  FIELD ObjectNumber LIKE _StorageObject._Object-number
  FIELD AreaNumber   LIKE _StorageObject._Area-number
  FIELD ObjectBlock  LIKE _StorageObject._Object-block
  FIELD ObjectRoot   LIKE _StorageObject._Object-root
  FIELD ObjectAttrib LIKE _StorageObject._Object-attrib
  FIELD CreateLimit  LIKE _StorageObject._Create-Limit
  FIELD TossLimit    LIKE _StorageObject._Toss-Limit
  INDEX ObjectNumber IS UNIQUE
        ObjectType
        ObjectNumber
  INDEX AreaNumber
        AreaNumber
. /* TEMP-TABLE tt_StorageObject */


/* *******************************  Functions  ***************************** */

FUNCTION XOR RETURNS CHARACTER (ipInt1 AS INTEGER, ipInt2 AS INTEGER):

  DEFINE VARIABLE vBitList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i        AS INTEGER   NO-UNDO.

  ASSIGN vBitList = "":U.
  DO i = 32 TO 1 BY -1:
    ASSIGN vBitList = vBitList + IF GET-BITS(ipInt1, i, 1) EQ 1
                                 OR GET-BITS(ipInt2, i, 1) EQ 1
                                 THEN "1":U
                                 ELSE "0":U
    . /* ASSIGN */
  END.
  RETURN LEFT-TRIM(vBitList, "0":U).

END FUNCTION. /* XOR */

/* ------------------------------------------------------------------------- */

FUNCTION Integer2DateTime RETURNS DATETIME (INPUT ipLastChange AS INTEGER).

  RETURN ADD-INTERVAL(DATETIME(1, 1, 1970, 0, 0, 0, 0),
                      ipLastChange + TIMEZONE * 60,
                      "seconds":U).

END FUNCTION. /* Integer2DateTime */

/* ------------------------------------------------------------------------- */

FUNCTION String2DateTime RETURNS DATETIME (INPUT ipString AS CHARACTER).
/* Input string must have a format like: Tue Jul 27 12:11:45 2004 */

  DEFINE VARIABLE vDateTime  AS DATETIME  NO-UNDO INITIAL ?.
  DEFINE VARIABLE vMonthList AS CHARACTER NO-UNDO
    INITIAL "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec".
  DEFINE VARIABLE vTime AS CHARACTER NO-UNDO.

  ASSIGN ipString = TRIM(REPLACE(ipString, "  ":U, " ":U))
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
/* Retun the input datetime in format "YYYY.MM.DD_HH.MM.SS" */

  DEFINE VARIABLE vDate AS DATE    NO-UNDO.
  DEFINE VARIABLE vTime AS INTEGER NO-UNDO.

  ASSIGN vDate = DATE(ipDateTime)
         vTime = INTERVAL(ipDateTime, DATETIME(vDate), "seconds":U)
  NO-ERROR.

  RETURN      STRING( YEAR(vDate), "9999":U)
    + ".":U + STRING(MONTH(vDate),   "99":U)
    + ".":U + STRING(  DAY(vDate),   "99":U)
/*  + "_":U + REPLACE(STRING(vTime, "HH:MM:SS":U), ":":U, ".":U) */
  . /* RETURN */

END FUNCTION. /* DateTime2String */

/* ------------------------------------------------------------------------- */

FUNCTION DateTime2Excel RETURNS CHARACTER (INPUT ipDateTime AS DATETIME).

/* Return the value has format: DD:MM:YYYYY HH:MM:SS (eg 27/07/2004 12:11:45)*/


  DEFINE VARIABLE vDate AS DATE    NO-UNDO.
  DEFINE VARIABLE vTime AS INTEGER NO-UNDO.

  ASSIGN vDate = DATE(ipDateTime)
         vTime = INTERVAL(ipDateTime, DATETIME(vDate), "seconds":U)
  NO-ERROR.

  RETURN      /* Day   */ STRING( YEAR(vDate), "9999":U)
    + "/":U + /* Month */ STRING(MONTH(vDate),   "99":U)
    + "/":U + /* Year  */ STRING(  DAY(vDate),   "99":U)
    + " ":U + /* Time  */ STRING(vTime, "HH:MM:SS":U)
  . /* RETURN */
END FUNCTION. /* DateTime2Excel */

/* ------------------------------------------------------------------------- */

FUNCTION LkHostName RETURNS CHARACTER (ipLkFile AS CHARACTER):

  DEFINE VARIABLE vHostName AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vLkSize   AS INTEGER   NO-UNDO INITIAL 38.
  DEFINE VARIABLE vRawData  AS RAW       NO-UNDO.

/* Try to open .lk file for reads: */
  DO ON ERROR UNDO, RETRY:
    IF RETRY THEN
    RETURN ERROR SUBSTITUTE("Unable to read &1 (Progress error &2)",
                  /* &1 */  ipLkFile,
                  /* &1 */  STRING(_Msg(1))).
    INPUT FROM VALUE(ipLkFile) BINARY.
  END.
/* Read .lk file: */
  LENGTH(vRawData) = vLkSize.
  IMPORT UNFORMATTED vRawData.
  INPUT CLOSE.
  ASSIGN vHostName = GET-STRING(vRawData, 9)
         LENGTH(vRawData) = 0.

  RETURN vHostName.
END FUNCTION. /* LkHostName */

/* ------------------------------------------------------------------------- */

FUNCTION LocalHostName RETURNS CHARACTER:
  DEFINE VARIABLE vHostName AS CHARACTER NO-UNDO INITIAL 'unknown'.
  DEFINE VARIABLE vTcpName  AS CHARACTER NO-UNDO INITIAL ''.
  DEFINE VARIABLE vLength   AS INTEGER   NO-UNDO INITIAL 100.
  DEFINE VARIABLE vReturn   AS INTEGER   NO-UNDO INITIAL 0.

  IF OPSYS EQ "WIN32" THEN
  DO: /* Call Win32 routine to get host name */
    RUN gethostname(OUTPUT vTcpName,
                    INPUT  vLength,
                    OUTPUT vReturn).
    IF vReturn EQ 0 THEN
    ASSIGN vHostName = ENTRY(1, vTcpName, CHR(0)).
  END.
  ELSE DO:
    /* get UNIX host name */
    INPUT THROUGH uname -n.
    IMPORT vHostName.
    INPUT CLOSE.
  END.

  RETURN vHostName.
END FUNCTION. /* LocalHostName */

/* ------------------------------------------------------------------------- */

FUNCTION DbParamHostName RETURNS CHARACTER (ipLDBName AS CHARACTER).

  DEFINE VARIABLE vParam AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i      AS INTEGER   NO-UNDO.

  DO i = 1 TO NUM-ENTRIES(DBPARAM(ipLDBName)):
    ASSIGN vParam = ENTRY(i, DBPARAM(ipLDBName)).

    IF vParam BEGINS "-H ":U THEN
    RETURN SUBSTRING(vParam, 4).
  END.

  RETURN ?.
END FUNCTION. /* DbParamHostName */

/* *****************************  Procedures  ****************************** */

PROCEDURE gethostname EXTERNAL "wsock32.dll" :
   DEFINE OUTPUT PARAMETER p-Hostname AS CHARACTER.
   DEFINE INPUT  PARAMETER p-Length   AS LONG.
   DEFINE RETURN PARAMETER p-Return   AS LONG.
END PROCEDURE. /* gethostname */

/* ------------------------------------------------------------------------- */

PROCEDURE GetMetaschema.

  DEFINE VARIABLE hQuery  AS HANDLE NO-UNDO. /*Query handle*/
  DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO. /*Buffer handle*/


/* Get _Area: -------- */
  CREATE BUFFER hBuffer FOR TABLE "DICTDB._Area":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE(
  "FOR EACH DICTDB._Area NO-LOCK WHERE _Area-Number GE 6 AND _Area-type EQ 6":U)
  NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

  REPEAT WHILE NOT hQuery:QUERY-OFF-END
  TRANSACTION:
    CREATE tt_Area.
    ASSIGN
tt_Area.AreaNumber      = hBuffer:BUFFER-FIELD("_Area-Number":U   ):BUFFER-VALUE()
tt_Area.AreaName        = hBuffer:BUFFER-FIELD("_Area-Name":U     ):BUFFER-VALUE()
tt_Area.AreaType        = hBuffer:BUFFER-FIELD("_Area-type"       ):BUFFER-VALUE()
tt_Area.AreaBlockSize   = hBuffer:BUFFER-FIELD("_Area-blocksize"  ):BUFFER-VALUE()
tt_Area.AreaRecPerBlock = EXP(2,
                          hBuffer:BUFFER-FIELD("_Area-recbits"    ):BUFFER-VALUE())
tt_Area.AreaClusterSize = hBuffer:BUFFER-FIELD("_Area-clustersize"):BUFFER-VALUE()
tt_Area.AreaAttrib      = hBuffer:BUFFER-FIELD("_Area-attrib"     ):BUFFER-VALUE()
    . /* ASSIGN */
    hQuery:GET-NEXT().
  END. /* REPEAT */
  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.


/* Get _MstrBlk:  -------- */
  CREATE BUFFER hBuffer FOR TABLE "DICTDB._MstrBlk":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH DICTDB._MstrBlk NO-LOCK":U)
  NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  IF hQuery:QUERY-OFF-END THEN
  RETURN ERROR.

  DO TRANSACTION:
    CREATE tt_Db.
    ASSIGN
tt_Db.mb_crdate    = hBuffer:BUFFER-FIELD("_MstrBlk-crdate":U   ):BUFFER-VALUE()
tt_Db.mb_timestamp = hBuffer:BUFFER-FIELD("_MstrBlk-timestamp":U):BUFFER-VALUE()
tt_Db.mb_dbvers    = hBuffer:BUFFER-FIELD("_MstrBlk-dbvers":U   ):BUFFER-VALUE()
    . /* ASSIGN */
  END. /* DO TRANSACTION */
  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.


/* Get _Db:  -------- */
  CREATE BUFFER hBuffer FOR TABLE "DICTDB._Db":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH DICTDB._Db NO-LOCK WHERE _Db-local EQ TRUE":U)
  NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  IF hQuery:QUERY-OFF-END THEN
  RETURN ERROR.

  DO TRANSACTION:
    ASSIGN tt_Db.mb_dbrecid = hBuffer:RECID
           tt_Db.DbGuid     = hBuffer:BUFFER-FIELD("_Db-guid":U):BUFFER-VALUE()
    . /* ASSIGN */
  END. /* DO TRANSACTION */
  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.


/* Get _Sequence:  -------- */
  CREATE BUFFER hBuffer FOR TABLE "DICTDB._Sequence":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH DICTDB._Sequence NO-LOCK":U)
  NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END
  TRANSACTION:
    CREATE tt_Sequence.
    ASSIGN
    tt_Sequence.SeqNumber = hBuffer:BUFFER-FIELD("_Seq-Num":U  ):BUFFER-VALUE()
    tt_Sequence.SeqOwner  = hBuffer:BUFFER-FIELD("_Seq-Owner":U):BUFFER-VALUE()
    tt_Sequence.SeqName   = hBuffer:BUFFER-FIELD("_Seq-Name":U ):BUFFER-VALUE()
    tt_Sequence.SeqInit   = hBuffer:BUFFER-FIELD("_Seq-Init":U ):BUFFER-VALUE()
    tt_Sequence.SeqIncr   = hBuffer:BUFFER-FIELD("_Seq-Incr":U ):BUFFER-VALUE()
    . /* ASSIGN */
    hQuery:GET-NEXT().
  END. /* REPEAT */
  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.


/* Get _File:  -------- */
  CREATE BUFFER hBuffer FOR TABLE "DICTDB._File":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE(
"FOR EACH DICTDB._File NO-LOCK WHERE _File-Number GT 0 AND _File-Number LT 32768":U)
  NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END
  TRANSACTION:
    CREATE tt_File.
    ASSIGN
tt_File.TableNumber = hBuffer:BUFFER-FIELD("_File-Number":U):BUFFER-VALUE()
tt_File.TableOwner  = hBuffer:BUFFER-FIELD("_Owner":U      ):BUFFER-VALUE()
tt_File.TableName   = hBuffer:BUFFER-FIELD("_File-Name":U  ):BUFFER-VALUE()
tt_File.TableFields = hBuffer:BUFFER-FIELD("_numfld":U     ):BUFFER-VALUE()
tt_File.TableCRC    = hBuffer:BUFFER-FIELD("_CRC":U        ):BUFFER-VALUE()
tt_File.Template    = hBuffer:BUFFER-FIELD("_Template":U   ):BUFFER-VALUE()
tt_File.PrimeIndex  = hBuffer:BUFFER-FIELD("_Prime-Index":U):BUFFER-VALUE()
tt_File.FileRecid   = hBuffer:RECID
tt_File.LastChange  = hBuffer:BUFFER-FIELD("_Last-Change":U):BUFFER-VALUE()
tt_File.NumKey      = hBuffer:BUFFER-FIELD("_numkey":U     ):BUFFER-VALUE()
tt_File.NumFld      = hBuffer:BUFFER-FIELD("_numfld":U     ):BUFFER-VALUE()
tt_File.NumKFld     = hBuffer:BUFFER-FIELD("_numkfld":U    ):BUFFER-VALUE()
tt_File.NumKComp    = hBuffer:BUFFER-FIELD("_numkcomp":U   ):BUFFER-VALUE()
    . /* ASSIGN */
    hQuery:GET-NEXT().
  END. /* REPEAT */
  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.


/* Get _Index:  -------- */
  CREATE BUFFER hBuffer FOR TABLE "DICTDB._Index":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH DICTDB._Index NO-LOCK WHERE _Idx-Num GT 0":U)
  NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  hQuery:QUERY-OPEN().
  REPEAT
  TRANSACTION:

    hQuery:GET-NEXT().

    IF hQuery:QUERY-OFF-END THEN
    LEAVE.

    IF NOT CAN-FIND(FIRST tt_File
                    WHERE tt_File.TableNumber GT 0
                      AND tt_File.TableNumber LT 32768
                      AND tt_File.FileRecid   EQ
                    hBuffer:BUFFER-FIELD("_File-recid":U):BUFFER-VALUE()) THEN
    NEXT.

    CREATE tt_Index.
    ASSIGN
    tt_Index.IndexNumber = hBuffer:BUFFER-FIELD("_Idx-Num":U   ):BUFFER-VALUE()
    tt_Index.IndexOwner  = hBuffer:BUFFER-FIELD("_Idxowner":U  ):BUFFER-VALUE()
    tt_Index.IndexName   = hBuffer:BUFFER-FIELD("_Index-Name":U):BUFFER-VALUE()
    tt_Index.IndexFields = hBuffer:BUFFER-FIELD("_num-comp":U  ):BUFFER-VALUE()
    tt_Index.FileRecid   = hBuffer:BUFFER-FIELD("_File-recid":U):BUFFER-VALUE()
    tt_Index.IndexActive = hBuffer:BUFFER-FIELD("_Active":U    ):BUFFER-VALUE()
    tt_Index.IndexUnique = hBuffer:BUFFER-FIELD("_Unique":U    ):BUFFER-VALUE()
    tt_Index.IndexCRC    = hBuffer:BUFFER-FIELD("_Idx-CRC":U   ):BUFFER-VALUE()
    tt_Index.WordIdx     = hBuffer:BUFFER-FIELD("_Wordidx":U   ):BUFFER-VALUE()
    tt_Index.NumComp     = hBuffer:BUFFER-FIELD("_num-comp":U  ):BUFFER-VALUE()
    . /* ASSIGN */
  END. /* REPEAT */
  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.


/* Get _Field:  -------- */
  CREATE BUFFER hBuffer FOR TABLE "DICTDB._Field":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE(
    "FOR EACH DICTDB._Field NO-LOCK WHERE CAN-DO('BLOB,CLOB':U, _Data-Type)":U)
  NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END
  TRANSACTION:

    CREATE tt_Field.
    ASSIGN
tt_Field.FileRecid      = hBuffer:BUFFER-FIELD("_File-recid":U ):BUFFER-VALUE()
tt_Field.FieldName      = hBuffer:BUFFER-FIELD("_Field-Name":U ):BUFFER-VALUE()
tt_Field.DataType       = hBuffer:BUFFER-FIELD("_Data-Type":U  ):BUFFER-VALUE()
tt_Field.Fld-stdtype    = hBuffer:BUFFER-FIELD("_Fld-stdtype":U):BUFFER-VALUE()
tt_Field.Fld-stlen      = hBuffer:BUFFER-FIELD("_Fld-stlen":U  ):BUFFER-VALUE()
tt_Field.Fld-misc2      = hBuffer:BUFFER-FIELD("_Fld-misc2":U  ):BUFFER-VALUE()
tt_Field.Fld-case       = hBuffer:BUFFER-FIELD("_Fld-case":U   ):BUFFER-VALUE()
tt_Field.FieldCharset   = hBuffer:BUFFER-FIELD("_Charset":U    ):BUFFER-VALUE()
tt_Field.FieldCollation = hBuffer:BUFFER-FIELD("_Collation":U  ):BUFFER-VALUE()
tt_Field.Attributes1    = hBuffer:BUFFER-FIELD("_Attributes1":U):BUFFER-VALUE()
    . /* ASSIGN */
    hQuery:GET-NEXT().
  END. /* REPEAT */
  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.


/* Get _StorageObject:  -------- */
  CREATE BUFFER hBuffer FOR TABLE "DICTDB._StorageObject":U.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE(
"FOR EACH DICTDB._StorageObject NO-LOCK WHERE _Object-number GE -1 AND _Object-associate GT 0":U)
  NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  RETURN ERROR.

  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().

/* First _StorageObject is for the _File table: */
  DO TRANSACTION:
    ASSIGN
    tt_Db.ObjectBlock  = hBuffer:BUFFER-FIELD("_Object-block":U):BUFFER-VALUE()
    tt_Db.CreateLimit  = hBuffer:BUFFER-FIELD("_Create-Limit":U):BUFFER-VALUE()
    tt_Db.TossLimit    = hBuffer:BUFFER-FIELD("_Toss-Limit":U  ):BUFFER-VALUE()
    . /* ASSIGN */
  END.

  hQuery:GET-NEXT().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END
  TRANSACTION:

    CREATE tt_StorageObject.
    ASSIGN
tt_StorageObject.ObjectType   = hBuffer:BUFFER-FIELD("_Object-type":U  ):BUFFER-VALUE()
tt_StorageObject.ObjectNumber = hBuffer:BUFFER-FIELD("_Object-number":U):BUFFER-VALUE()
tt_StorageObject.AreaNumber   = hBuffer:BUFFER-FIELD("_Area-number":U  ):BUFFER-VALUE()
tt_StorageObject.ObjectBlock  = hBuffer:BUFFER-FIELD("_Object-block":U ):BUFFER-VALUE()
tt_StorageObject.ObjectRoot   = hBuffer:BUFFER-FIELD("_Object-root":U  ):BUFFER-VALUE()
tt_StorageObject.ObjectAttrib = hBuffer:BUFFER-FIELD("_Object-attrib":U):BUFFER-VALUE()
tt_StorageObject.CreateLimit  = hBuffer:BUFFER-FIELD("_Create-Limit":U ):BUFFER-VALUE()
tt_StorageObject.TossLimit    = hBuffer:BUFFER-FIELD("_Toss-Limit":U   ):BUFFER-VALUE()
    . /* ASSIGN */
    hQuery:GET-NEXT().
  END. /* REPEAT */
  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.

END PROCEDURE. /* GetMetaschema */

/* ------------------------------------------------------------------------- */

PROCEDURE PutObjectInfo.

  DEFINE INPUT  PARAMETER ipOutputFile  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opDbTimeStamp AS DATETIME  NO-UNDO.
  DEFINE OUTPUT PARAMETER opMD5Digest   AS CHARACTER NO-UNDO.

  &SCOPED-DEFINE Sep "~t"

  DEFINE VARIABLE vHostName     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vPDbName      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vTableOwner   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vTableName    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vObjectName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vObjectType   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vObjectFields AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vObjectInfo   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vObjectAttr   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vObjectCRC    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vObjectRoot LIKE _StorageObject._Object-root NO-UNDO.
  DEFINE VARIABLE vLastChange LIKE _File._Last-Change NO-UNDO.
  DEFINE VARIABLE hFileMemPtr   AS MEMPTR    NO-UNDO.
  DEFINE VARIABLE vMD5Offset    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i             AS INTEGER   NO-UNDO.

  ASSIGN vHostName = DbParamHostName("DICTDB":U)
         vPDbName  = PDBNAME("DICTDB":U).

  ASSIGN vHostName = LocalHostName()
    WHEN vHostName EQ ?
      OR vHostName EQ "localhost":U

         vPDbName = SUBSTRING(vPDbName, 1, LENGTH(vPDbName) - 3)
    WHEN vPDbName MATCHES "*.db":U.

  ASSIGN vHostName = LkHostName(vPDbName + ".lk":U)
    WHEN vHostName EQ ?
         i = MAX(R-INDEX(vPDbName, "/":U), R-INDEX(vPDbName, "~\":U))
         vPDbName = SUBSTRING(vPDbName, i + 1)
  . /* ASSIGN */

  OUTPUT TO VALUE(ipOutputFile) APPEND.
  IF SEEK(OUTPUT) EQ 0 THEN
  PUT UNFORMATTED
           "Host"
    {&Sep} "Db Name"
    {&Sep} "Area"
    {&Sep} "Area #"
    {&Sep} "RPB"
    {&Sep} "CLS"
    {&Sep} "Object Type"
    {&Sep} "Owner"
    {&Sep} "Table"
    {&Sep} "Object"
    {&Sep} "Object #"
    {&Sep} "Fields"
    {&Sep} "Object Info"
    {&Sep} "Object Attr"
    {&Sep} "Object CRC"
    {&Sep} "Object Block"
    {&Sep} "Template/Root"
    {&Sep} "Create Limit"
    {&Sep} "Toss Limit"
  SKIP.

  FOR FIRST tt_Area NO-LOCK /* INDEX AreaNumber */
      WHERE tt_Area.AreaNumber EQ 6:

    FOR FIRST tt_Db NO-LOCK:

      ASSIGN opDbTimeStamp = String2DateTime(tt_Db.mb_timestamp).

      PUT UNFORMATTED
               /* Host          */ vHostName
        {&Sep} /* Db Name       */ vPDbName /* LDBNAME("DICTDB":U) */
        {&Sep} /* Area          */ tt_Area.AreaName
        {&Sep} /* Area #        */ tt_Area.AreaNumber
        {&Sep} /* RPB           */ tt_Area.AreaRecPerBlock
        {&Sep} /* CLS           */ tt_Area.AreaClusterSize
        {&Sep} /* Object Type   */ "Schema":U
        {&Sep} /* Owner         */ "":U
        {&Sep} /* Table         */ tt_Db.DbGuid
        {&Sep} /* Object        */ DateTime2Excel(
                                   String2DateTime(tt_Db.mb_crdate))
        {&Sep} /* Object #      */ "":U
        {&Sep} /* Fields        */ "":U
        {&Sep} /* Object Info   */ DateTime2Excel(opDbTimeStamp)
        {&Sep} /* Object Attr   */ DateTime2Excel(DATETIME(TODAY, TIME * 1000))
        {&Sep} /* Object CRC    */ "":U
        {&Sep} /* Object Block  */ tt_Db.ObjectBlock
        {&Sep} /* Template/Root */ tt_Db.mb_dbrecid
        {&Sep} /* Create Limit  */ tt_Db.CreateLimit
        {&Sep} /* Toss Limit    */ tt_Db.TossLimit
      SKIP.

    END. /* FOR FIRST tt_Db */

    ASSIGN vMD5Offset  = SEEK(OUTPUT)
           vObjectRoot = tt_Area.AreaRecPerBlock * 3
  . /* ASSIGN */

    FOR EACH tt_Sequence NO-LOCK
          BY tt_Sequence.SeqNumber:
      PUT UNFORMATTED
               /* Host          */ vHostName
        {&Sep} /* Db Name       */ vPDbName
        {&Sep} /* Area          */ tt_Area.AreaName
        {&Sep} /* Area #        */ tt_Area.AreaNumber
        {&Sep} /* RPB           */ tt_Area.AreaRecPerBlock
        {&Sep} /* CLS           */ tt_Area.AreaClusterSize
        {&Sep} /* Object Type   */ "Sequence":U
        {&Sep} /* Owner         */ tt_Sequence.SeqOwner
        {&Sep} /* Table         */ "":U
        {&Sep} /* Object        */ tt_Sequence.SeqName
        {&Sep} /* Object #      */ tt_Sequence.SeqNumber
        {&Sep} /* Fields        */ "":U
        {&Sep} /* Object Info   */ tt_Sequence.SeqInit
        {&Sep} /* Object Attr   */ tt_Sequence.SeqIncr
        {&Sep} /* Object CRC    */ "":U
        {&Sep} /* Object Block  */ "":U
        {&Sep} /* Template/Root */ vObjectRoot
        {&Sep} /* Create Limit  */ "":U
        {&Sep} /* Toss Limit    */ "":U
      SKIP.
    END. /* FOR EACH tt_Sequence  */
  END. /* FOR FIRST tt_Area */

  ASSIGN vLastChange = 0.

StorageObject:
  FOR EACH tt_Area NO-LOCK /* INDEX AreaNumber */
     WHERE tt_Area.AreaNumber GE 6
       AND tt_Area.AreaType   EQ 6,

      EACH tt_StorageObject NO-LOCK /* INDEX AreaNumber */
     WHERE tt_StorageObject.AreaNumber   EQ tt_Area.AreaNumber
       AND tt_StorageObject.ObjectNumber GT 0

        BY tt_Area.AreaNumber
        BY tt_StorageObject.ObjectType
        BY tt_StorageObject.ObjectNumber:

    ASSIGN vObjectAttr = XOR(tt_Area.AreaAttrib,tt_StorageObject.ObjectAttrib)
           vTableOwner = ?
           vTableName  = ?
           vObjectName = ?
           vObjectType = ?
           vObjectInfo = ?
           vObjectRoot = ?
    . /* ASSIGN */

    CASE tt_StorageObject.ObjectType:
/* Table: ------------------------------------------------------------------ */
      WHEN 1 THEN
      FOR FIRST tt_File NO-LOCK /* INDEX TableNumber */
          WHERE tt_File.TableNumber EQ tt_StorageObject.ObjectNumber:

        ASSIGN
          vTableOwner   = tt_File.TableOwner
          vTableName    = tt_File.TableName
          vObjectName   = "":U
          vObjectType   = "Table"
          vObjectRoot   = tt_File.Template
          vObjectFields = tt_File.TableFields
          vObjectInfo = DateTime2Excel(Integer2DateTime(tt_File.LastChange))
          vObjectCRC  = STRING(tt_File.TableCRC)
          vLastChange =  tt_File.LastChange WHEN
          vLastChange LT tt_File.LastChange
        . /* ASSIGN */
      END. /* WHEN 1 (_File) */

/* Index: ------------------------------------------------------------------ */
      WHEN 2 THEN
      FOR FIRST tt_Index NO-LOCK /* INDEX IndexNumber */
          WHERE tt_Index.IndexNumber EQ tt_StorageObject.ObjectNumber,

          FIRST tt_File NO-LOCK /* INDEX FileRecid */
          WHERE tt_File.FileRecid EQ tt_Index.FileRecid
            AND tt_File.TableNumber GT 0
             OR tt_File.TableNumber LT 32768:

        ASSIGN vTableOwner   = tt_File.TableOwner
               vTableName    = tt_File.TableName
               vObjectName   = tt_Index.IndexName
               vObjectType   = "Index"
               vObjectFields = tt_Index.IndexFields
               vObjectInfo = IF NOT tt_Index.IndexActive THEN "I":U ELSE
                             IF tt_File.dft-pk           THEN "D":U ELSE
                             IF tt_Index.IndexUnique     THEN "U":U ELSE
                             IF tt_Index.WordIdx GT 0    THEN "W":U ELSE "":U
               vObjectRoot = tt_StorageObject.ObjectRoot
               vObjectCRC  = STRING(tt_Index.IndexCRC)
        . /* ASSIGN */
      END. /* WHEN 2(_Index) */

/* LOBs: ------------------------------------------------------------------- */
      WHEN 3 THEN
      FOR FIRST tt_Field NO-LOCK /* INDEX Fld-stdtype */
          WHERE tt_Field.fld-stlen EQ tt_StorageObject.ObjectNumber,

          FIRST tt_File NO-LOCK /* INDEX FileRecid */
          WHERE tt_File.FileRecid EQ tt_Field.FileRecid:
/*
        IF tt_Field.DataType EQ "CLOB":U THEN
        ASSIGN
          vObjectInfo = tt_Field.FieldCharset     /* CLOB-CODEPAGE  */ + ",":U
                      + tt_Field.FieldCollation   /* CLOB-COLLATION */ + ",":U
/* Attributes1: if _Field._Charset EQ _Db._Db-xl-name then 1 else 2 */
                      + STRING(tt_Field.Attributes1)   /* CLOB-TYPE */
                      + (IF tt_Field.Fld-case  THEN ",case" ELSE "":U) + ",":U
                      +     tt_Field.Fld-Misc2[1]
        . /* ASSIGN */
        ELSE
        ASSIGN vObjectInfo = tt_Field.Fld-Misc2[1].
*/
        ASSIGN vTableOwner   = tt_File.TableOwner
               vTableName    = tt_File.TableName
               vObjectName   = tt_Field.FieldName
               vObjectType   = tt_Field.DataType
               vObjectInfo   = tt_Field.Fld-Misc2[1]
               vObjectFields = 1
               vObjectRoot   = ?
               vObjectCRC    = "":U
        . /* ASSIGN */
      END. /* WHEN 3 (_Field) */
    END CASE.

    PUT UNFORMATTED
             /* Host          */ vHostName
      {&Sep} /* Db Name       */ vPDbName /* LDBNAME("DICTDB":U) */
      {&Sep} /* Area          */ tt_Area.AreaName
      {&Sep} /* Area #        */ tt_Area.AreaNumber
      {&Sep} /* RPB           */ tt_Area.AreaRecPerBlock
      {&Sep} /* CLS           */ tt_Area.AreaClusterSize
      {&Sep} /* Object Type   */ vObjectType
      {&Sep} /* Owner         */ vTableOwner
      {&Sep} /* Table         */ vTableName
      {&Sep} /* Object        */ vObjectName
      {&Sep} /* Object #      */ tt_StorageObject.ObjectNumber
      {&Sep} /* Fields        */ vObjectFields
      {&Sep} /* Object Info   */ vObjectInfo
      {&Sep} /* Object Attr   */ vObjectAttr
      {&Sep} /* Object CRC    */ vObjectCRC
      {&Sep} /* Object Block  */ tt_StorageObject.ObjectBlock
      {&Sep} /* Template/Root */ vObjectRoot
      {&Sep} /* Create Limit  */ IF vObjectType EQ "Index":U THEN "":U ELSE
                                  STRING(tt_StorageObject.CreateLimit)
      {&Sep} /* Toss Limit    */ IF vObjectType EQ "Index":U THEN "":U ELSE
                                  STRING(tt_StorageObject.TossLimit)
    SKIP.

  END. /* FOR EACH _StorageObject, FIRST _Area */

  OUTPUT CLOSE.

  EMPTY TEMP-TABLE tt_Db.
  EMPTY TEMP-TABLE tt_Sequence.
  EMPTY TEMP-TABLE tt_Area.
  EMPTY TEMP-TABLE tt_File.
  EMPTY TEMP-TABLE tt_Index.
  EMPTY TEMP-TABLE tt_Field.
  EMPTY TEMP-TABLE tt_StorageObject.

  ASSIGN FILE-INFO:FILE-NAME = ipOutputFile
  SET-SIZE(hFileMemPtr) = FILE-INFO:FILE-SIZE - vMD5Offset.
  INPUT FROM VALUE(FILE-INFO:FULL-PATHNAME) BINARY NO-MAP NO-CONVERT.
  SEEK INPUT TO vMD5Offset.
  IMPORT hFileMemPtr.
  INPUT CLOSE.
  ASSIGN opMD5Digest = opMD5Digest + HEX-ENCODE(MD5-DIGEST(hFileMemPtr))
         SET-SIZE(hFileMemPtr) = 0
  . /* ASSIGN */

  IF vLastChange GT 0 THEN
  ASSIGN opDbTimeStamp = Integer2DateTime(vLastChange).

END PROCEDURE. /* PutObjectInfo */

/* ------------------------------------------------------------------------- */

/* ******************************  Main block  **************************** */

DEFINE VARIABLE vMaxTimeStamp AS DATETIME  NO-UNDO.
DEFINE VARIABLE vDbTimeStamp  AS DATETIME  NO-UNDO.
DEFINE VARIABLE vHostName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vPDbName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE vOutputFile1  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vOutputFile2  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vDbId         AS INTEGER   NO-UNDO.
DEFINE VARIABLE vMD5Digest    AS CHARACTER NO-UNDO.

DEFINE VARIABLE vErrorDesc    AS CHARACTER NO-UNDO EXTENT 18 INITIAL [
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


/* Hostname of the first connected database: */
ASSIGN vHostName = DbParamHostName("DICTDB":U)
       vPDbName  = PDBNAME("DICTDB":U).

ASSIGN vHostName = LocalHostName()
  WHEN vHostName EQ ?
    OR vHostName EQ "localhost":U

       vPDbName = SUBSTRING(vPDbName, 1, LENGTH(vPDbName) - 3)
  WHEN vPDbName MATCHES "*.db":U.

ASSIGN vHostName = "":U WHEN vHostName EQ ?
       vOutputFile2 = "ObjectInfo." + vHostName + ".&1.txt":U
       vOutputFile1 = SUBSTITUTE(vOutputFile2,
                      /* &1*/ DateTime2String(DATETIME(TODAY, TIME * 1000)))
. /* ASSIGN */

ASSIGN vMaxTimeStamp = DATETIME(1, 1, 1970, 0, 0, 0, 0).
REPEAT vDbID = 1 TO NUM-DBS:
  CREATE ALIAS DICTDB FOR DATABASE VALUE(LDBNAME(vDbID)).
  RUN GetMetaschema.
  RUN PutObjectInfo(INPUT  vOutputFile1,
                    OUTPUT vDbTimeStamp,
                    OUTPUT vMD5Digest).

  IF vMaxTimeStamp LT vDbTimeStamp
  OR vMaxTimeStamp EQ ? THEN
  ASSIGN vMaxTimeStamp = vDbTimeStamp.
END.

ASSIGN vMD5Digest   = LC(ENCODE(vMD5Digest))
                 /* = HEX-ENCODE(MD5-DIGEST(vMD5Digest)) */
       vOutputFile2 = SUBSTITUTE(vOutputFile2,
              /* &1*/ DateTime2String(vMaxTimeStamp) + ".":U + vMD5Digest)
. /* ASSIGN */

ASSIGN FILE-INFO:FILE-NAME = vOutputFile2.
IF FILE-INFO:FULL-PATHNAME NE ? THEN
DO:
  OS-DELETE VALUE(FILE-INFO:FULL-PATHNAME).
  IF OS-ERROR GT 0 THEN
  MESSAGE
    "Unable to delete file" FILE-INFO:FULL-PATHNAME SKIP
    "Error:" OS-ERROR "("
    IF OS-ERROR LE EXTENT(vErrorDesc) THEN vErrorDesc[OS-ERROR]
                                      ELSE "Unmapped error" ")"
  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
END.

OS-RENAME VALUE(vOutputFile1) VALUE(vOutputFile2).
IF OS-ERROR GT 0 THEN
MESSAGE
  SUBSTITUTE("Unable to rename &1 to &2", vOutputFile1, vOutputFile2) SKIP
  "Error:" OS-ERROR "("
  IF OS-ERROR LE EXTENT(vErrorDesc) THEN vErrorDesc[OS-ERROR]
                                    ELSE "Unmapped error" ")"
VIEW-AS ALERT-BOX ERROR BUTTONS OK.
ELSE
ASSIGN vOutputFile1 = vOutputFile2.

ASSIGN FILE-INFO:FILE-NAME = vOutputFile1.
MESSAGE "See" FILE-INFO:FULL-PATHNAME
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

