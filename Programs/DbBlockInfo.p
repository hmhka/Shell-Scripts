DEFINE VARIABLE vAreaList AS CHARACTER NO-UNDO INITIAL "17".

DEFINE VARIABLE vDbkeyList AS CHARACTER NO-UNDO INITIAL  "78300160".


DEFINE VARIABLE vBlockTypeName AS CHARACTER NO-UNDO EXTENT 18 INITIAL [
/*  1 */ "Master",
/*  2 */ "Index",
/*  3 */ "Data",
/*  4 */ "Free",
/*  5 */ "Index Anchor Table",
/*  6 */ "Sequence Value",
/*  7 */ "Empty",
/*  8 */ "Parameter",
/*  9 */ "Area",
/* 10 */ "Object Directory",
/* 11 */ "Extent List",
/* 12 */ "Object",
/* 13 */ "Control",
/* 14 */ "Object List",
/* 15 */ "Cluster Map",
/* 16 */ "Object Cluster",
/* 17 */ "Block Map",
/* 18 */ "Row Space"].

DEFINE VARIABLE vBlockTypeShortName AS CHARACTER NO-UNDO EXTENT 18 INITIAL [
/*  1 */ "Mastr",
/*  2 */ "Index",
/*  3 */ "Data",
/*  4 */ "Free",
/*  5 */ "Anch",
/*  6 */ "SeqBl",
/*  7 */ "Empty",
/*  8 */ "Param",
/*  9 */ "Area",
/* 10 */ "ObDir",
/* 11 */ "ExLst",
/* 12 */ "ObjBl",
/* 13 */ "CtrBl",
/* 14 */ "ObLst",
/* 15 */ "ClMap",
/* 16 */ "ObClu",
/* 17 */ "BlMap",
/* 18 */ "RowSp"].

DEFINE VARIABLE vChainTypeName AS CHARACTER NO-UNDO EXTENT 4 INITIAL [
/*   0 */ "FreeChn",
/*   1 */ "RM Chn",
/*   2 */ "LockChn",
/* 127 */ "NoChain"].

DEFINE VARIABLE vAreaNumber  AS INTEGER NO-UNDO.
DEFINE VARIABLE vDbkey       AS INTEGER NO-UNDO.
DEFINE VARIABLE vObjectType  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vObjectName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vExtentFile  AS CHARACTER NO-UNDO.
DEFINE VARIABLE v_bk_dbkey   AS INTEGER NO-UNDO. /* Block Address (dbkey) */
DEFINE VARIABLE v_bk_type    AS INTEGER NO-UNDO. /* Block Type */
DEFINE VARIABLE v_bk_frchn   AS INTEGER NO-UNDO. /* Chain Type */
DEFINE VARIABLE v_bk_incr    AS INTEGER NO-UNDO. /* Incremental Backup Counter */
DEFINE VARIABLE v_bk_nextf   AS INTEGER NO-UNDO. /* Chain Next Block (dbkey) */
DEFINE VARIABLE v_bk_updctr  AS INTEGER NO-UNDO. /* Vlock Version Number */

DEFINE VARIABLE v_rm_numdir  AS INTEGER NO-UNDO. /* No. of Recs */
DEFINE VARIABLE v_rm_freedir AS INTEGER NO-UNDO. /* Free Slots */
DEFINE VARIABLE v_rm_free    AS INTEGER NO-UNDO. /* No. Bytes Free Space */

DEFINE VARIABLE v_ih_top     AS INTEGER NO-UNDO. /* Root Flag*/
DEFINE VARIABLE v_ih_bot     AS INTEGER NO-UNDO. /* Leaf Flag*/
DEFINE VARIABLE v_ih_ixnum   AS INTEGER NO-UNDO. /* Index Number */
DEFINE VARIABLE v_ih_nment   AS INTEGER NO-UNDO. /* No. Index Entries */
DEFINE VARIABLE v_ih_lnent   AS INTEGER NO-UNDO. /* No. Bytes Used */

DEFINE VARIABLE v_objectId   AS INTEGER NO-UNDO.
DEFINE VARIABLE v_objectType AS INTEGER NO-UNDO.

DEFINE STREAM BlkReport.
DEFINE VARIABLE vStreamOpened AS LOGICAL NO-UNDO EXTENT 3 INITIAL FALSE.
DEFINE VARIABLE vPutHeader    AS LOGICAL NO-UNDO.
&SCOPED-DEFINE Separator ";"

DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE j AS INTEGER NO-UNDO.

/* Test code ON
/* You can choose any index to test the program: */
&SCOPED-DEFINE TABLE Customer
&SCOPED-DEFINE INDEX NAME


/* Set "input" parameters as an example: */
FOR FIRST _File NO-LOCK
    WHERE _File-Name EQ "{&TABLE}",

  FIRST _Index OF _File NO-LOCK
  WHERE _Index-Name EQ "{&INDEX}",

  FIRST _StorageObject
  WHERE _StorageObject._Db-recid      EQ _File._Db-recid
    AND _StorageObject._Object-type   EQ 2 /* Index */
    AND _StorageObject._Object-number EQ _Index._idx-num:

  ASSIGN
    vAreaNumber = _StorageObject._Area-number
    vDbkey      = _StorageObject._Object-root
  .  /* ASSIGN */
END.

RUN GetBlockInfo(
    INPUT vAreaNumber,
    INPUT vDbkey,
    OUTPUT vExtentFile,
    OUTPUT v_bk_dbkey,
    OUTPUT v_bk_type,
    OUTPUT v_bk_frchn,
    OUTPUT v_bk_incr,
    OUTPUT v_bk_nextf,
    OUTPUT v_bk_updctr,

    OUTPUT v_objectId,
    OUTPUT v_objectType,

    OUTPUT v_rm_numdir,
    OUTPUT v_rm_freedir,
    OUTPUT v_rm_free,

    OUTPUT v_ih_top,
    OUTPUT v_ih_bot,
    OUTPUT v_ih_ixnum,
    OUTPUT v_ih_nment,
    OUTPUT v_ih_lnent
). /* RUN GetBlockInfo */

MESSAGE
  "IdxNum:       " _Index._idx-num   SKIP
  "Area:         " vAreaNumber SKIP
  "v_ih_ixnum:   " v_ih_ixnum  SKIP
  "v_objectId:   " v_objectId  SKIP

  "Dbkey:        " vDbkey      SKIP
  "Block Dbkey:  " v_bk_dbkey (v_bk_dbkey EQ vDbkey) SKIP
  "Block Type:   " v_bk_type   SKIP
  "v_objectType: " v_objectType SKIP

  "v_bk_frchn:   " v_bk_frchn  SKIP 
  "v_bk_incr:    " v_bk_incr   SKIP 
  "v_bk_nextf:   " v_bk_nextf  SKIP
  "v_bk_updctr:  " v_bk_updctr SKIP
                
  "v_ih_top:     " v_ih_top    SKIP
  "v_ih_bot:     " v_ih_bot    SKIP
  "v_ih_nment:   " v_ih_nment  SKIP 
  "v_ih_lnent:   " v_ih_lnent  SKIP

VIEW-AS ALERT-BOX INFO BUTTONS OK.
Test code OFF */

REPEAT i = 1 TO NUM-ENTRIES(vAreaList):

  ASSIGN vAreaNumber = INTEGER (ENTRY(i, vAreaList)) NO-ERROR.

  IF ERROR-STATUS:NUM-MESSAGES GT 0
  OR NOT CAN-FIND (FIRST DICTDB._Area 
                   WHERE DICTDB._Area._Area-Number EQ vAreaNumber) THEN
  NEXT.

  REPEAT j = 1 TO NUM-ENTRIES(vDbkeyList):

    ASSIGN vDbkey = INTEGER (ENTRY(j, vDbkeyList)) NO-ERROR.

    IF ERROR-STATUS:NUM-MESSAGES GT 0
    OR vDbkey LE 0 THEN
    NEXT.

    RUN GetBlockInfo(
        INPUT  vAreaNumber,
        INPUT  vDbkey,

        OUTPUT vExtentFile,
        OUTPUT v_bk_dbkey,
        OUTPUT v_bk_type,
        OUTPUT v_bk_frchn,
        OUTPUT v_bk_incr,
        OUTPUT v_bk_nextf,
        OUTPUT v_bk_updctr,

        OUTPUT v_objectId,
        OUTPUT v_objectType,

        OUTPUT v_rm_numdir,
        OUTPUT v_rm_freedir,
        OUTPUT v_rm_free,

        OUTPUT v_ih_top,
        OUTPUT v_ih_bot,
        OUTPUT v_ih_ixnum,
        OUTPUT v_ih_nment,
        OUTPUT v_ih_lnent
    ). /* RUN BlockInfo */

    RUN GetObjectName(
      INPUT  v_objectType,
      INPUT  v_objectId,
      INPUT  v_bk_type,
      INPUT  v_ih_ixnum,

      OUTPUT vObjectType,
      OUTPUT vObjectName
    ). /* RUN GetObjectName */

    DISPLAY
      vAreaNumber            FORMAT ">>>9"       LABEL "Area"
      vDbkey                 FORMAT ">>>>>>>>>9" LABEL "Dbkey"
     (vDbkey EQ v_bk_dbkey)  FORMAT "!/"         LABEL ""
      v_bk_type              FORMAT ">>9"        LABEL "Blk"
     (IF v_bk_type GE 1 AND v_bk_type LE EXTENT(vBlockTypeShortName)
      THEN vBlockTypeShortName[v_bk_type]
      ELSE
      IF v_bk_type EQ 254
      THEN "ExtHead"
      ELSE ?) @
      vBlockTypeShortName[1] FORMAT "x(5)"       LABEL "Type"
      v_objectType           FORMAT ">>9"        LABEL "Obj"
      vObjectType            FORMAT "x(4)"       LABEL "Type"
      v_objectId             FORMAT ">>>>9"      LABEL "ObjId"
      v_ih_ixnum             FORMAT ">>>>9"      LABEL "IdxId"
      vObjectName            FORMAT "x(32)"      LABEL "Object Name"
    . /* DISPLAY */

    CASE v_bk_type:
   /* ---------------------------- */
      WHEN 2 /* index block */ THEN
      DO:
        OUTPUT STREAM BlkReport CLOSE.
        OUTPUT STREAM BlkReport TO
          VALUE(LDBNAME("DICTDB") + ".IdxBlkReport.csv") APPEND.
        ASSIGN vPutHeader = NOT vStreamOpened[v_bk_type]
               vStreamOpened[v_bk_type] = TRUE.
      END.
   /* ---------------------------- */
      WHEN 3 /* data block */ THEN
      DO: 
        OUTPUT STREAM BlkReport CLOSE.
        OUTPUT  STREAM BlkReport TO
          VALUE(LDBNAME("DICTDB") + ".DatBlkReport.csv") APPEND.
        ASSIGN vPutHeader = NOT vStreamOpened[v_bk_type]
               vStreamOpened[v_bk_type] = TRUE.
      END.
    /* ---------------------------- */
      OTHERWISE
      DO: 
        OUTPUT STREAM BlkReport CLOSE.
        OUTPUT STREAM BlkReport TO
          VALUE(LDBNAME("DICTDB") + ".OthBlkReport.csv") APPEND.
        ASSIGN vPutHeader = NOT vStreamOpened[1]
               vStreamOpened[1] = TRUE.
      END.
   /* ---------------------------- */
    END CASE. /* v_bk_type */
  
    IF vPutHeader THEN
    PUT STREAM BlkReport UNFORMATTED
      "Extent"      {&Separator}
      "LastTran"    {&Separator}
      "Area"        {&Separator}
      "Dbkey"       {&Separator}
      "Found"       {&Separator}
      ""            {&Separator}
      "BlkTypeId"   {&Separator}
      "BlkType"     {&Separator}
      "ChainId"     {&Separator}
      "ChainType"   {&Separator}
      "NextInChain" {&Separator}
      "BackupCtr"   {&Separator}
      "UpdateCtr"   {&Separator}
      "ObjTypeId"   {&Separator}
      "ObjType"     {&Separator}
      "ObjId"       {&Separator}
      "ObjName"   
    . /* PUT */
  
    IF vPutHeader THEN
    CASE v_bk_type:
   /* ---------------------------- */
      WHEN 2 /* index block */ THEN
      PUT STREAM BlkReport UNFORMATTED
                     {&Separator}
        "RootFlag"    {&Separator}
        "LeafFlag"    {&Separator}
        "IdxNum"      {&Separator}
        "IdxEntries"  {&Separator}
        "BytesUsed"   SKIP.
   /* ---------------------------- */
      WHEN 3 /* data block */ THEN
      PUT STREAM BlkReport UNFORMATTED
                      {&Separator}
        "UsedSlots"   {&Separator}
        "FreeSlots"   {&Separator}
        "FreeBytes"   SKIP.
   /* ---------------------------- */
      OTHERWISE
      PUT STREAM BlkReport UNFORMATTED SKIP.
   /* ---------------------------- */
    END CASE. /* v_bk_type */
  
  
    FIND FIRST _DbStatus NO-LOCK.
    
    PUT STREAM BlkReport UNFORMATTED
    /* Extent     */ vExtentFile                  {&Separator}
    /* LastTran   */ _DbStatus._DbStatus-LastTran {&Separator}
    /* Area       */ vAreaNumber                  {&Separator}
    /* Dbkey      */ vDbkey                       {&Separator}
    /* Found      */ v_bk_dbkey                   {&Separator}
    /*            */ (IF vDbkey NE v_bk_dbkey 
                      THEN "!"
                      ELSE "")                    {&Separator}
    /* BlkTypeId  */ v_bk_type                    {&Separator}
    /* BlkType    */ (IF  v_bk_type GE 1
                      AND v_bk_type LE EXTENT(vBlockTypeShortName)
                      THEN vBlockTypeName[v_bk_type]
                      ELSE "?")                   {&Separator}
    /* ChainId    */ v_bk_frchn                   {&Separator}
    /* ChainType  */ (IF v_bk_frchn GE 0 AND v_bk_frchn LE 2
                      THEN vChainTypeName[v_bk_frchn + 1]
                      ELSE
                      IF v_bk_frchn EQ 127
                      THEN vChainTypeName[4]
                      ELSE "?")                   {&Separator} 
    /* NextInChain*/ v_bk_nextf                   {&Separator}
    /* BackupCtr  */ v_bk_incr                    {&Separator}
    /* UpdateCtr  */ v_bk_updctr                  {&Separator}
    /* ObjTypeId  */ v_objectType                 {&Separator}
    /* ObjType    */ vObjectType                  {&Separator}
    /* ObjId      */ v_objectId                   {&Separator}
    /* ObjName    */ vObjectName.
  
    CASE v_bk_type:
   /* ---------------------------- */
      WHEN 2 /* index block */ THEN
      PUT STREAM BlkReport UNFORMATTED {&Separator}
        /* RootFlag   */ v_ih_top      {&Separator}
        /* LeafFlag   */ v_ih_bot      {&Separator}
        /* IdxNum     */ v_ih_ixnum    {&Separator}
        /* IdxEntries */ v_ih_nment    {&Separator}
        /* BytesUsed  */ v_ih_lnent  SKIP.
   /* ---------------------------- */
      WHEN 3 /* data block */ THEN
      PUT STREAM BlkReport UNFORMATTED {&Separator}
        /* UsedSlots */ v_rm_numdir    {&Separator}
        /* FreeSlots */ v_rm_freedir   {&Separator}
        /* FreeBytes */ v_rm_free    SKIP.
   /* ---------------------------- */
      OTHERWISE
      PUT STREAM BlkReport UNFORMATTED SKIP.
   /* ---------------------------- */
    END CASE. /* v_bk_type */

  END. /* REPEAT vDbkey */
END. /* DO vAreaNumber */


IF vStreamOpened[1] OR vStreamOpened[2] OR vStreamOpened[3] THEN
DO:
  OUTPUT STREAM BlkReport CLOSE.
  OUTPUT STREAM BlkReport TO VALUE(LDBNAME("DICTDB") + ".Status.csv") APPEND.
  PUT STREAM BlkReport UNFORMATTED
    "Database"     {&Separator}
    "Version"      {&Separator}
    "Date/Time"    {&Separator}
    "BlockSize"    {&Separator}
    "LastTran"     {&Separator}
    "CreateDate"   {&Separator}
    "SchemaDate"   {&Separator}
    "LastOpened"   {&Separator}
    "LastStarted"  {&Separator}
    "LastFullBkp"  {&Separator}
    "IncrBkpCtr"   {&Separator}
    "LastIncBkp"   SKIP.

  FIND FIRST DICTDB._FileList NO-LOCK.
  FIND FIRST DICTDB._DbStatus NO-LOCK.

  PUT STREAM BlkReport UNFORMATTED
    /* Database    */ DICTDB._FileList._FileList-Name           {&Separator}
    /* Version     */ DICTDB._DbStatus._DbStatus-DbVers MOD 256 {&Separator}
    /* Date/Time   */ STRING(TODAY,"99/99/9999") SPACE STRING(TIME, "HH:MM:SS")
                                                                {&Separator}
    /* BlockSize   */ DICTDB._DbStatus._DbStatus-DbBlkSize      {&Separator}
    /* LastTran    */ DICTDB._DbStatus._DbStatus-LastTran       {&Separator}
    /* CreateDate  */ DICTDB._DbStatus._DbStatus-CreateDate     {&Separator}
    /* SchemaDate  */ DICTDB._DbStatus._DbStatus-CacheStamp     {&Separator}
    /* LastOpened  */ DICTDB._DbStatus._DbStatus-LastOpen       {&Separator}
    /* LastStarted */ DICTDB._DbStatus._DbStatus-starttime      {&Separator}
    /* LastFullBkp */ DICTDB._DbStatus._DbStatus-fbDate         {&Separator}
    /* IncrBkpCtr  */ DICTDB._DbStatus._DbStatus-ibSeq          {&Separator}
    /* LastIncBkp  */ DICTDB._DbStatus._DbStatus-ibDate         SKIP.

  OUTPUT STREAM BlkReport CLOSE.
END.

/* -------------------------------------------------------------------------  */
PROCEDURE GetObjectName.

  DEFINE INPUT PARAMETER ip_objectType AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ip_objectId AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ip_bk_type    AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ip_ih_ixnum   AS INTEGER   NO-UNDO.

  DEFINE OUTPUT PARAMETER opObjectType AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opObjectName AS CHARACTER NO-UNDO.

  ASSIGN
    opObjectType = ?
    opObjectName = ?.

  CASE ip_objectType:
/*
#define DSMOBJECT_TABLE  1  /* table object */
#define DSMOBJECT_INDEX  2  /* index object */
#define DSMOBJECT_BLOB   3  /* blob object */
*/
    WHEN 1 /* table object */ THEN
    FOR FIRST DICTDB._File NO-LOCK
        WHERE DICTDB._File._File-Number EQ ip_objectId:

        ASSIGN opObjectType = "TBL":U
               opObjectName = DICTDB._File._File-Name.
    END.
 /* ------------------------------ */
    WHEN 2 /* index object */ THEN
    FOR FIRST DICTDB._Index NO-LOCK
        WHERE DICTDB._Index._Idx-num EQ ip_objectId,

        FIRST DICTDB._File OF DICTDB._Index NO-LOCK:

        ASSIGN opObjectType = "IDX":U
               opObjectName = DICTDB._File._File-Name + "."
                            + DICTDB._Index._Index-Name.
    END.
 /* ------------------------------ */
    WHEN 3 /* blob object */ THEN
    FOR FIRST DICTDB._Field NO-LOCK
        WHERE DICTDB._Field._Data-Type EQ "blob"
           OR DICTDB._Field._Data-Type EQ "clob"
          AND DICTDB._Field._Fld-stlen EQ ip_objectId,

        FIRST DICTDB._File OF DICTDB._Field NO-LOCK:

        ASSIGN opObjectType = DICTDB._Field._Data-Type
               opObjectName = DICTDB._File._File-Name + "."
                            + DICTDB._Field._Field-Name.
    END.
  END CASE. /* ip_objectType */

  IF opObjectName EQ ? THEN
  CASE ip_bk_type:
      WHEN 3 /* data block */ THEN
          ASSIGN opObjectType = "DATA":U.
   /* ------------------------------ */
      WHEN 2 /* index block */ THEN
      FOR FIRST DICTDB._Index NO-LOCK
          WHERE DICTDB._Index._Idx-num EQ ip_ih_ixnum,

          FIRST DICTDB._File OF DICTDB._Index NO-LOCK:

          ASSIGN opObjectType = "IDX":U
                 opObjectName = DICTDB._File._File-Name + "."
                              + DICTDB._Index._Index-Name.
      END.
  END CASE. /* ip_bk_type */

END PROCEDURE. /* GetObjectName */

/* -------------------------------------------------------------------------  */

PROCEDURE GetBlockInfo.
  DEFINE INPUT PARAMETER ipAreaNumber   AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipDbkey        AS INTEGER NO-UNDO.

  DEFINE OUTPUT PARAMETER opExtentFile    AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER op_bk_dbkey   AS INTEGER NO-UNDO. /* Block Address (dbkey) */
  DEFINE OUTPUT PARAMETER op_bk_type    AS INTEGER NO-UNDO. /* Block Type */
  DEFINE OUTPUT PARAMETER op_bk_frchn   AS INTEGER NO-UNDO. /* Chain Type */
  DEFINE OUTPUT PARAMETER op_bk_incr    AS INTEGER NO-UNDO. /* Incremental Backup Counter */
  DEFINE OUTPUT PARAMETER op_bk_nextf   AS INTEGER NO-UNDO. /* Chain Next Block (dbkey) */
  DEFINE OUTPUT PARAMETER op_bk_updctr  AS INTEGER NO-UNDO. /* Vlock Version Number */

  DEFINE OUTPUT PARAMETER op_objectId   AS INTEGER NO-UNDO. /* _StorageObject._Object-Number */
  DEFINE OUTPUT PARAMETER op_objectType AS INTEGER NO-UNDO. /* _StorageObject._Object-Type */

  DEFINE OUTPUT PARAMETER op_rm_numdir  AS INTEGER NO-UNDO. /* No. of Recs */
  DEFINE OUTPUT PARAMETER op_rm_freedir AS INTEGER NO-UNDO. /* Free Slots */
  DEFINE OUTPUT PARAMETER op_rm_free    AS INTEGER NO-UNDO. /* No. Bytes Free Space */

  DEFINE OUTPUT PARAMETER op_ih_top     AS INTEGER NO-UNDO. /* Root Flag*/
  DEFINE OUTPUT PARAMETER op_ih_bot     AS INTEGER NO-UNDO. /* Leaf Flag*/
  DEFINE OUTPUT PARAMETER op_ih_ixnum   AS INTEGER NO-UNDO. /* Index Number */
  DEFINE OUTPUT PARAMETER op_ih_nment   AS INTEGER NO-UNDO. /* No. Index Entries */
  DEFINE OUTPUT PARAMETER op_ih_lnent   AS INTEGER NO-UNDO. /* No. Bytes Used */

  DEFINE VARIABLE v_bkHeaderSize AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vPrevOffset    AS INT64   NO-UNDO.
  DEFINE VARIABLE vNextOffset    AS INT64   NO-UNDO.
  DEFINE VARIABLE vBlockOffset   AS INT64   NO-UNDO.
  DEFINE VARIABLE vExtentSize    AS INT64   NO-UNDO.
  DEFINE VARIABLE vRawHeader     AS RAW       NO-UNDO.

  ASSIGN
/* Standart block header  */
    op_bk_dbkey    = ?
    op_bk_type     = ?
    op_bk_frchn    = ?
    op_bk_incr     = ?
    op_bk_nextf    = ?
    op_bk_updctr   = ?

  /* SAT2 block header */
    v_bkHeaderSize = 16
    op_objectId    = ?
    op_objectType  = ?

  /* Index block header */
    op_ih_top      = ?
    op_ih_bot      = ?
    op_ih_ixnum    = ?
    op_ih_nment    = ?
    op_ih_lnent    = ?

  /* Data block header */
    op_rm_numdir   = ?
    op_rm_freedir  = ?
    op_rm_free     = ?
  . /* ASSIGN */

  FIND FIRST DICTDB._Area NO-LOCK
       WHERE DICTDB._Area._Area-number EQ ipAreaNumber NO-ERROR.

  IF NOT AVAILABLE DICTDB._Area THEN
  RETURN.

  ASSIGN vNextOffset = 0
         vBlockOffset = INT64(ipDbkey / EXP( 2, DICTDB._Area._Area-recbits))
                      * DICTDB._Area._Area-blocksize
  .  /* ASSIGN */

  FOR EACH DICTDB._AreaExtent NO-LOCK
     WHERE DICTDB._AreaExtent._Area-number EQ DICTDB._Area._Area-number
        BY DICTDB._AreaExtent._Extent-number:

  /* _FileList._FileList returns full path while
     _AreaExtent._Extent-path can be relative. */
    FIND FIRST DICTDB._FileList NO-LOCK
      WHERE DICTDB._FileList._FileList-Name MATCHES "*":U +
           SUBSTRING(DICTDB._AreaExtent._Extent-path, 3).

    ASSIGN
      opExtentFile =  DICTDB._FileList._FileList-Name
      vExtentSize  = (DICTDB._FileList._FileList-Size - 1) * 1024
   /* vExtentSize does not include a header block */
      vPrevOffset = vNextOffset
      vNextOffset = vNextOffset + vExtentSize
    . /* ASSIGN */

    IF vBlockOffset LT vNextOffset THEN /* Bingo! */ LEAVE.

  END. /* EACH _AreaExtent */

  ASSIGN vBlockOffset = vBlockOffset - vPrevOffset.

  INPUT FROM VALUE(opExtentFile) BINARY.
  SEEK INPUT TO vBlockOffset.
  ASSIGN LENGTH(vRawHeader) = 1024.
  IMPORT UNFORMATTED vRawHeader.
  INPUT CLOSE.

  ASSIGN
    /* Standart block header  */
    op_bk_dbkey   = GET-LONG(vRawHeader,  1)
    op_bk_type    = GET-BYTE(vRawHeader,  5)
    op_bk_frchn   = GET-BYTE(vRawHeader,  6)
    op_bk_incr    = GET-SHORT(vRawHeader, 7)
    op_bk_nextf   = GET-LONG(vRawHeader,  9)
    op_bk_updctr  = GET-LONG(vRawHeader, 13)
  . /* ASSIGN */

/* Storage area type II: */
  IF DICTDB._Area._Area-clustersize GT 1 THEN
  ASSIGN
  /* SAT2 block header */
    v_bkHeaderSize = GET-SHORT(vRawHeader, 19)
    op_objectId    = GET-SHORT(vRawHeader, 21)
    op_objectType  = GET-SHORT(vRawHeader, 23)
  . /* ASSIGN */

  CASE op_bk_type:
    WHEN 2 THEN
    ASSIGN
    /* Index block header */
      op_ih_top   = GET-BYTE(vRawHeader,  v_bkHeaderSize +  1)
      op_ih_bot   = GET-BYTE(vRawHeader,  v_bkHeaderSize +  2)
      op_ih_ixnum = GET-SHORT(vRawHeader, v_bkHeaderSize +  3)
      op_ih_nment = GET-SHORT(vRawHeader, v_bkHeaderSize +  9)
      op_ih_lnent = GET-SHORT(vRawHeader, v_bkHeaderSize + 11)
    . /* ASSIGN */

    WHEN 3 THEN
    ASSIGN
    /* Data block header */
      op_rm_numdir  = GET-BYTE(vRawHeader,  v_bkHeaderSize + 1)
      op_rm_freedir = GET-BYTE(vRawHeader,  v_bkHeaderSize + 2)
      op_rm_free    = GET-SHORT(vRawHeader, v_bkHeaderSize + 3)
    . /* ASSIGN */
  END CASE. /* op_bk_type */

  ASSIGN LENGTH(vRawHeader) = 0.

END PROCEDURE. /* GetBlockInfo */


