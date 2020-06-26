FUNCTION SeekRatio RETURN DECIMAL (
         ipTableName AS CHARACTER,
         ipIndexName AS CHARACTER,
         ipReadLimit AS INTEGER) FORWARD.

DEFINE VARIABLE vSeekRatio AS DECIMAL NO-UNDO.

FOR EACH DICTDB._File NO-LOCK
   WHERE /*DICTDB._File._File-Name EQ "customer"
     AND*/ DICTDB._File._File-Number GT 0
     AND DICTDB._File._File-Number LT 32768,

    EACH DICTDB._Index OF DICTDB._File NO-LOCK
   WHERE DICTDB._Index._Active  EQ TRUE
     AND DICTDB._Index._Wordidx EQ ?:

  DISPLAY 
    DICTDB._File._File-Name   FORMAT "x(25)"
    DICTDB._Index._Index-Name FORMAT "x(25)"
    SeekRatio(DICTDB._File._File-Name,
              DICTDB._Index._Index-Name,
              10000) LABEL "Ratio"  FORMAT ">>>,>>>,>>9.99"
  WITH TITLE "Seek Raio".
END.

/* ------------------------------------------------------------------------
    File        : SeekRatio.p
    Purpose     : Check the index logical scatter factors.
    Syntax      : See the examples above.
    Description : The logical scatter factor is defined as the ratio of
                  the seek path to dbkey range where:
                  seek path = sum of abs(current_dbkey - previous_dbkey)
                  and
                  dbkey range = max_dbkey - min_dbkey

    Author(s)   : George Potemkin
    Created     : Nov 12, 2015
    Modified    : Nov 12, 2015
    Version     : 1.0
------------------------------------------------------------------------ */


/* ------------------------------------------------------------------------- */

FUNCTION SeekRatio RETURN DECIMAL (
         ipTableName AS CHARACTER,
         ipIndexName AS CHARACTER,
         ipReadLimit AS INTEGER).

  DEFINE VARIABLE vSeekPath  AS INT64   NO-UNDO.
  DEFINE VARIABLE vRecCount  AS INT64   NO-UNDO.
  DEFINE VARIABLE vRecid     AS INT64   NO-UNDO.
  DEFINE VARIABLE vCurrDbkey AS INT64   NO-UNDO.
  DEFINE VARIABLE vPrevDbkey AS INT64   NO-UNDO.
  DEFINE VARIABLE vMinDbkey  AS INT64   NO-UNDO.
  DEFINE VARIABLE vMaxDbkey  AS INT64   NO-UNDO.
  DEFINE VARIABLE vRPB       AS INTEGER NO-UNDO.
  DEFINE VARIABLE vWordIndex AS INTEGER NO-UNDO.
  DEFINE VARIABLE vActiveIdx AS LOGICAL NO-UNDO.
  DEFINE VARIABLE hQuery     AS HANDLE  NO-UNDO.
  DEFINE VARIABLE hBuffer    AS HANDLE  NO-UNDO.
  DEFINE VARIABLE hBuffer1   AS HANDLE  NO-UNDO.
  DEFINE VARIABLE hBuffer2   AS HANDLE  NO-UNDO.
  DEFINE VARIABLE hBuffer3   AS HANDLE  NO-UNDO.
  DEFINE VARIABLE hBuffer4   AS HANDLE  NO-UNDO.

  &SCOPED-DEFINE Pool WIDGET-POOL "SeekRatio":U
  
  CREATE {&Pool}.


  ASSIGN vSeekPath = ?.

MainBlock:
  DO:
    CREATE BUFFER hBuffer FOR TABLE ipTableName IN {&Pool} NO-ERROR.

    IF ERROR-STATUS:NUM-MESSAGES GT 0 OR NOT hBuffer:CAN-READ THEN
    LEAVE MainBlock.

    CREATE BUFFER hBuffer1 FOR TABLE hBuffer:DBNAME + "._File":U          IN {&Pool}.
    CREATE BUFFER hBuffer2 FOR TABLE hBuffer:DBNAME + "._Index":U         IN {&Pool}.
    CREATE BUFFER hBuffer3 FOR TABLE hBuffer:DBNAME + "._StorageObject":U IN {&Pool}.
    CREATE BUFFER hBuffer4 FOR TABLE hBuffer:DBNAME + "._Area":U          IN {&Pool}.
    CREATE QUERY  hQuery                                                  IN {&Pool}.

    hQuery:SET-BUFFERS(hBuffer1, hBuffer2, hBuffer3, hBuffer4).
    hQuery:QUERY-PREPARE(SUBSTITUTE(
  "FOR EACH  &1._File NO-LOCK":U
   + " WHERE &1._File._File-Name EQ '&2',":U

   + " EACH  &1._Index NO-LOCK OF &1._File":U
   + " WHERE &1._Index._Index-Name EQ '&3',":U

   + " EACH  &1._StorageObject NO-LOCK":U
   + " WHERE &1._StorageObject._Db-recid      EQ &1._File._Db-recid":U
   + "   AND &1._StorageObject._Object-type   EQ 1":U
   + "   AND &1._StorageObject._Object-number EQ &1._File._File-number,":U

   + " EACH  &1._Area NO-LOCK":U
   + " WHERE &1._Area._Area-number EQ &1._StorageObject._Area-number":U,

      /* &1 */ hBuffer:DBNAME,
      /* &2 */ ipTableName,
      /* &3 */ ipIndexName))
    NO-ERROR.

    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    LEAVE MainBlock.

    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    IF NOT hQuery:QUERY-OFF-END THEN
    ASSIGN vRPB = hBuffer4:BUFFER-FIELD("_Area-recbits":U):BUFFER-VALUE()
           vRPB = EXP(2, vRPB)
           vWordIndex = hBuffer2:BUFFER-FIELD("_Wordidx":U ):BUFFER-VALUE()
           vActiveIdx = hBuffer2:BUFFER-FIELD("_Active":U  ):BUFFER-VALUE()
           vRecid     = hBuffer1:BUFFER-FIELD("_Template":U):BUFFER-VALUE()
    . /* ASSIGN */

    hQuery:QUERY-CLOSE().

    IF vWordIndex GT 0 OR NOT vActiveIdx THEN
    LEAVE MainBlock.

    DELETE OBJECT hQuery   NO-ERROR.
    DELETE OBJECT hBuffer1 NO-ERROR.
    DELETE OBJECT hBuffer2 NO-ERROR.
    DELETE OBJECT hBuffer3 NO-ERROR.
    DELETE OBJECT hBuffer4 NO-ERROR.

  /* ipTableName USE-INDEX ipIndexName: */
    CREATE QUERY hQuery IN {&Pool}.
    hQuery:SET-BUFFERS(hBuffer).
    hQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH &1 NO-LOCK USE-INDEX &2":U,
                                           /* &1 */ ipTableName,
                                           /* &2 */ ipIndexName)) NO-ERROR.

    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    LEAVE MainBlock.

    hQuery:QUERY-OPEN().

    ASSIGN vRecCount  = 0
           vSeekPath  = 0
           vPrevDbkey = vRecid - (vRecid MODULO vRPB)
           vMinDbkey  = vPrevDbkey
           vMaxDbkey  = vPrevDbkey
    . /* ASSIGN */

    REPEAT WHILE vRecCount LT ipReadLimit:
      hQuery:GET-NEXT().
      IF hQuery:QUERY-OFF-END THEN
      LEAVE.

      ASSIGN vRecCount  = vRecCount + 1
             vRecid     = hBuffer:RECID
             vCurrDbkey = vRecid - (vRecid MODULO vRPB)
             vMinDbkey  = MIN(vMinDbkey, vCurrDbkey)
             vMaxDbkey  = MAX(vMaxDbkey, vCurrDbkey)
             vSeekPath  = vSeekPath + ABSOLUTE(vCurrDbkey - vPrevDbkey)
             vPrevDbkey = vCurrDbkey
      . /* ASSIGN */
    END.
  END.

  DELETE {&Pool} NO-ERROR.
  CASE vSeekPath:
    WHEN 0 OR WHEN ? THEN RETURN vSeekPath.
    OTHERWISE             RETURN vSeekPath / (vMaxDbkey - vMinDbkey).
  END CASE.

END FUNCTION. /* SeekRatio */

/* ------------------------------------------------------------------------- */


