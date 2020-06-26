/*------------------------------------------------------------------------
    File        : IdxRootBlockInfo.p
    Purpose     : Index Root Block Information.

    Syntax      :

    Description :

    Author(s)   : George Potemkin
    Created     : Jul 30, 2011
    Modified    : Aug 02, 2011
    Notes       : 
    Aug 01, 2011: added "Levels" and area's and index attributes.
    Aug 02, 2011: added "AreaHWM"; 
                  fixed "too large to fit in INTEGER" with extents larger 2GB;
                  set "Levels" to 1 if the root block is a leaf block.
 "Blocks" reported by ixanalys seems to equal 
 "TotalBlocks" - "NumBlocksOnChain_FREECHN" - 4
  ----------------------------------------------------------------------*/
  
DEFINE VARIABLE vReportPrefix AS CHARACTER NO-UNDO INITIAL "IdxRootBlockInfo".
&SCOPED-DEFINE Separator "~t"

DEFINE TEMP-TABLE IdxBlockInfo NO-UNDO
  FIELD TableName  AS CHARACTER
  FIELD IndexName  AS CHARACTER
  FIELD IndexNum   AS INTEGER
  FIELD IndexAttr  AS CHARACTER
  FIELD AreaNum    AS INTEGER
  FIELD RootDbkey  AS RECID   /* Dbkey of Index Root Block    */
  FIELD Levels     AS INTEGER /* Index Tree Levels            */
  FIELD RootFlag   AS INTEGER /* 1 = Root Block               */
  FIELD LeafFlag   AS INTEGER /* 1 = Leaf Block               */
  FIELD IndexKeys  AS INTEGER /* No. Index Entries            */
  FIELD BytesUsed  AS INTEGER /* No. Byte Used                */
  FIELD BackupCtr  AS INTEGER /* Backup Counter               */
  FIELD UpdateCtr  AS INTEGER /* Block Version Number         */
  FIELD HeaderSize AS INTEGER /* Block Header Size            */
  FIELD CheckSum   AS INTEGER /* Block Check Sum              */
  FIELD ObjDbkey   AS RECID   /* Object Block (type 12) Dbkey */
  FIELD SerialNumber             AS INT64
  FIELD TotalBlocks              AS INT64
  FIELD HiWaterBlock             AS INT64
  FIELD NumBlocksOnChain_FREECHN AS INT64
  FIELD NumBlocksOnChain_LOCKCHN AS INT64
  FIELD ErrorMsg   AS CHARACTER

  INDEX IndexName  IS UNIQUE
        TableName
        IndexName
. /* DEFINE TEMP-TABLE IdxBlockInfo */


DEFINE VARIABLE i AS INTEGER NO-UNDO.

DO i = NUM-ENTRIES(DBPARAM("DICTDB")) TO 1 BY -1:
  IF NOT ENTRY(i, DBPARAM("DICTDB")) BEGINS "-H" THEN
  NEXT.

  MESSAGE 
    "Database" LDBNAME("DICTDB") "is connected in remote mode." SKIP
    "The program can be used only for self-service connections."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  QUIT.
END.

FOR EACH DICTDB._File NO-LOCK WHERE _File-Number GT 0 AND _File-Number LT 32768,

  EACH DICTDB._Index OF DICTDB._File NO-LOCK,

  FIRST DICTDB._StorageObject
  WHERE DICTDB._StorageObject._Db-recid      EQ _File._Db-recid
    AND DICTDB._StorageObject._Object-type   EQ 2 /* Index */
    AND DICTDB._StorageObject._Object-number EQ _Index._idx-num:

  CREATE IdxBlockInfo.
  ASSIGN IdxBlockInfo.TableName = DICTDB._File._File-Name
         IdxBlockInfo.IndexName = DICTDB._Index._Index-Name
         IdxBlockInfo.IndexNum  = DICTDB._Index._Idx-Num
         IdxBlockInfo.IndexAttr =
           (IF DICTDB._Index._Active THEN "":U ELSE "i":U) +
           (IF DICTDB._File._Prime-Index EQ RECID(DICTDB._Index)
            THEN "p":U ELSE "":U) +
           (IF DICTDB._Index._Unique THEN "u":U ELSE "":U) +
           (IF DICTDB._Index._Wordidx GT 0 THEN "w":U ELSE "":U) +
           "c" + STRING(DICTDB._Index._num-comp)
         IdxBlockInfo.AreaNum   = DICTDB._StorageObject._Area-Number
         IdxBlockInfo.RootDbkey = DICTDB._StorageObject._Object-Root
  . /* ASSIGN */

  RUN GetIdxRootBlockInfo(INPUT IdxBlockInfo.AreaNum,
                          INPUT IdxBlockInfo.RootDbkey).

  IF IdxBlockInfo.LeafFlag EQ 1 THEN
  ASSIGN IdxBlockInfo.Levels = 1.
  ELSE
  IF  DICTDB._Index._Wordidx  NE 1
  AND DICTDB._Index._num-comp GT 0 /* exclude "default" index */ THEN
  RUN IndexLevels(INPUT  DICTDB._File._File-Name,
                  INPUT  DICTDB._Index._Index-Name,
                  OUTPUT IdxBlockInfo.Levels).
  ELSE
  ASSIGN IdxBlockInfo.Levels = ?.
END.

OUTPUT TO VALUE(vReportPrefix  + ".":U + LDBNAME("DICTDB") + ".txt":U).
PUT UNFORMATTED "TableName" 
  {&Separator}  "IndexName" 
  {&Separator}  "IndexNum" 
  {&Separator}  "IndexAttr"
  {&Separator}  "AreaName" 
  {&Separator}  "AreaNum" 
  {&Separator}  "AreaClSz"
  {&Separator}  "AreaRPB"
  {&Separator}  "AreaHWM"
  {&Separator}  "Dbkey"
  {&Separator}  "Levels"
  {&Separator}  "LeafFlag "  /* 1 = Leaf Block */
  {&Separator}  "BackupCtr"  /* Backup Counter */
  {&Separator}  "UpdateCtr"  /* Block Version Number */
  {&Separator}  "IndexKeys"  /* No. Index Entries */
  {&Separator}  "BytesUsed"  /* No. Byte Used */
  {&Separator}  "KeySize"    /* Average size of index keys in root block */
  {&Separator}  "MaxKeys"    /* Max number of index keys per block based on their size */
/* For Storage Area Type 2: */
  {&Separator}  "CheckSum"  /* Block Check Sum */
  {&Separator}  "ObjDbkey"  /* Object Block (type 12) Dbkey */
  {&Separator}  "SerialNumber" 
  {&Separator}  "TotalBlocks" 
  {&Separator}  "HiWaterBlock" 
  {&Separator}  "NumBlocksOnChain_FREECHN" 
  {&Separator}  "NumBlocksOnChain_LOCKCHN" 
  {&Separator}  "Errors"
SKIP.

FOR EACH IdxBlockInfo NO-LOCK,

  FIRST DICTDB._Area NO-LOCK
  WHERE DICTDB._Area._Area-Number EQ IdxBlockInfo.AreaNum,
  
  FIRST DICTDB._AreaStatus NO-LOCK
  WHERE DICTDB._AreaStatus._AreaStatus-AreaNum EQ DICTDB._Area._Area-Number:

  PUT UNFORMATTED /*TableName*/ IdxBlockInfo.TableName
    {&Separator}  /*IndexName*/ IdxBlockInfo.IndexName
    {&Separator}  /*IndexNum */ IdxBlockInfo.IndexNum
    {&Separator}  /*IndexAttr*/ IdxBlockInfo.IndexAttr
    {&Separator}  /*AreaName */ DICTDB._Area._Area-Name
    {&Separator}  /*AreaNum  */ IdxBlockInfo.AreaNum
    {&Separator}  /*AreaClSz */ DICTDB._Area._Area-clustersize
    {&Separator}  /*AreaRPB */  EXP(2, DICTDB._Area._Area-recbits)
    {&Separator}  /*AreaHWM */  DICTDB._AreaStatus._AreaStatus-Hiwater

    {&Separator}  /*Dbkey    */ IdxBlockInfo.RootDbkey
    
    {&Separator}  /*Levels   */ IdxBlockInfo.Levels 
    {&Separator}  /*LeafFlag */ IdxBlockInfo.LeafFlag
    {&Separator}  /*BackupCtr*/ IdxBlockInfo.BackupCtr
    {&Separator}  /*UpdateCtr*/ IdxBlockInfo.UpdateCtr
    {&Separator}  /*IndexKeys*/ IdxBlockInfo.IndexKeys
    {&Separator}  /*BytesUsed*/ IdxBlockInfo.BytesUsed
    {&Separator}  /*KeySize  */    
         IF IdxBlockInfo.IndexKeys EQ 0 THEN 0 ELSE
         TRUNCATE(DECIMAL(IdxBlockInfo.BytesUsed) / IdxBlockInfo.IndexKeys, 1)
    {&Separator}  /*MaxKeys*/    
        INTEGER((DICTDB._Area._Area-blocksize - IdxBlockInfo.HeaderSize)
                / IdxBlockInfo.BytesUsed
                * IdxBlockInfo.IndexKeys)
  /* For Storage Area Type 2 only: */
    {&Separator}  /*CheckSum*/                 IdxBlockInfo.CheckSum
    {&Separator}  /*ObjDbkey*/                 IdxBlockInfo.ObjDbkey
    {&Separator}  /*SerialNumber*/             IdxBlockInfo.SerialNumber            
    {&Separator}  /*TotalBlocks */             IdxBlockInfo.TotalBlocks             
    {&Separator}  /*HiWaterBlock*/             IdxBlockInfo.HiWaterBlock            
    {&Separator}  /*NumBlocksOnChain_FREECHN*/ IdxBlockInfo.NumBlocksOnChain_FREECHN
    {&Separator}  /*NumBlocksOnChain_LOCKCHN*/ IdxBlockInfo.NumBlocksOnChain_LOCKCHN
    {&Separator}  /*Errors*/                   IdxBlockInfo.ErrorMsg                
  SKIP.
END.
OUTPUT CLOSE.

FILE-INFO:FILE-NAME = vReportPrefix  + ".":U + LDBNAME("DICTDB") + ".txt":U.
MESSAGE "See" FILE-INFO:FULL-PATHNAME
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

/* -------------------------------------------------------------------------  */
PROCEDURE GetIdxRootBlockInfo.
  DEFINE INPUT  PARAMETER ipAreaNumber AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER ipRootDbkey  AS RECID   NO-UNDO.

  DEFINE VARIABLE vRawBlockHeader  AS RAW     NO-UNDO.
  DEFINE VARIABLE vBlockHeaderSize AS INTEGER NO-UNDO.
/* Any Blocks: */
  DEFINE VARIABLE v_bk_type    AS INTEGER NO-UNDO. /* Block Type */
  DEFINE VARIABLE v_bk_frchn   AS INTEGER NO-UNDO. /* Chain Type */
  DEFINE VARIABLE v_bk_incr    AS INTEGER NO-UNDO.
  DEFINE VARIABLE v_bk_updctr  AS INTEGER NO-UNDO.
  DEFINE VARIABLE v_objectId   AS INTEGER NO-UNDO. /* _StorageObject._Object-Number */
  DEFINE VARIABLE v_objectType AS INTEGER NO-UNDO. /* _StorageObject._Object-Type */
  DEFINE VARIABLE v_bkCheckSum AS INTEGER NO-UNDO.
  DEFINE VARIABLE v_bkObjDbkey AS RECID   NO-UNDO.

/*Storage Block: */
  DEFINE VARIABLE v_chainFirst_FREECHN          AS INT64 NO-UNDO.
  DEFINE VARIABLE v_chainFirst_RMCHN            AS INT64 NO-UNDO.
  DEFINE VARIABLE v_chainFirst_LOCKCHN          AS INT64 NO-UNDO.
  DEFINE VARIABLE v_numBlocksOnChainOld_FREECHN AS INT64 NO-UNDO.
  DEFINE VARIABLE v_numBlocksOnChainOld_RMCHN   AS INT64 NO-UNDO.
  DEFINE VARIABLE v_numBlocksOnChainOld_LOCKCHN AS INT64 NO-UNDO.
  DEFINE VARIABLE v_chainLast_FREECHN           AS INT64 NO-UNDO.
  DEFINE VARIABLE v_chainLast_RMCHN             AS INT64 NO-UNDO.
  DEFINE VARIABLE v_chainLast_LOCKCHN           AS INT64 NO-UNDO.
  DEFINE VARIABLE v_firstFreeCluster            AS INT64 NO-UNDO.
  DEFINE VARIABLE v_lastFreeCluster             AS INT64 NO-UNDO.
  DEFINE VARIABLE v_NumBlocksOnChain_RMCHN      AS INT64 NO-UNDO.

  DEFINE VARIABLE vErrorMsg   AS CHARACTER NO-UNDO.

/* Read Index Block (bk_type 2): */
  RUN GetBlockHeader(INPUT ipAreaNumber,
                     INPUT ipRootDbkey,
                     OUTPUT vRawBlockHeader,
                     OUTPUT vBlockHeaderSize,
                     OUTPUT v_bk_type,
                     OUTPUT v_bk_frchn,
                     OUTPUT IdxBlockInfo.BackupCtr /*bk_incr*/,
                     OUTPUT IdxBlockInfo.UpdateCtr /*bk_updctr*/,
                     OUTPUT IdxBlockInfo.CheckSum  /*bkCheckSum*/,
                     OUTPUT v_objectId,
                     OUTPUT v_objectType,
                     OUTPUT IdxBlockInfo.ObjDbkey  /*bkObjDbkey*/,
                     OUTPUT vErrorMsg).

  ASSIGN IdxBlockInfo.HeaderSize = vBlockHeaderSize.

  IF vErrorMsg NE "" THEN
  ASSIGN IdxBlockInfo.ErrorMsg = IdxBlockInfo.ErrorMsg + vErrorMsg + "|".

  IF v_bk_type NE 2 THEN
  ASSIGN IdxBlockInfo.ErrorMsg = IdxBlockInfo.ErrorMsg +
         "Wrong bk_type: " + STRING(v_bk_type) + "|".

  IF v_bk_frchn NE 127 /*NOCHN*/ THEN
  ASSIGN IdxBlockInfo.ErrorMsg = IdxBlockInfo.ErrorMsg +
         "Wrong bk_frchn: " + STRING(v_bk_frchn) + "|".

/* For Storage Area Type 2 only:*/
  IF v_objectId NE 0 AND v_objectId NE IdxBlockInfo.IndexNum THEN
  ASSIGN IdxBlockInfo.ErrorMsg = IdxBlockInfo.ErrorMsg +
         "Wrong objectId: " + STRING(v_objectId) + "|".

  IF v_objectId NE 0 AND v_objectType NE 2 THEN
  ASSIGN IdxBlockInfo.ErrorMsg = IdxBlockInfo.ErrorMsg +
         "Wrong objectType: " + STRING(v_objectType) + "|".

/* Stop processing because of the wrong data: */
  IF vErrorMsg NE "":U 
  OR v_bk_type NE 2 THEN
  RETURN.

  RUN GetIdxBlockInfo(INPUT  vRawBlockHeader,
                      INPUT  vBlockHeaderSize,
                      OUTPUT IdxBlockInfo.RootFlag,   /* op_ih_top */
                      OUTPUT IdxBlockInfo.LeafFlag,   /* op_ih_bot */
                      OUTPUT v_objectId,              /* op_ih_ixnum */
                      OUTPUT IdxBlockInfo.IndexKeys,  /* op_ih_nment */
                      OUTPUT IdxBlockInfo.BytesUsed). /* op_ih_lnent */

  IF IdxBlockInfo.RootFlag NE 1 THEN
  ASSIGN IdxBlockInfo.ErrorMsg = IdxBlockInfo.ErrorMsg +
         "Wrong ih_top: " + STRING(IdxBlockInfo.RootFlag) + "|".

  IF v_objectId NE IdxBlockInfo.IndexNum THEN
  ASSIGN IdxBlockInfo.ErrorMsg = IdxBlockInfo.ErrorMsg +
         "Wrong ih_ixnum: " + STRING(v_objectId) + "|".

/* Stop processing because of the wrong data: */
  IF v_objectId NE IdxBlockInfo.IndexNum THEN
  RETURN.

/* For Storage Area Type 2 only: */
  IF IdxBlockInfo.ObjDbkey EQ 0 THEN
  RETURN.

/* Read Object Block (bk_type 12): */
  RUN GetBlockHeader(INPUT ipAreaNumber,
                     INPUT IdxBlockInfo.ObjDbkey,
                     OUTPUT vRawBlockHeader,
                     OUTPUT vBlockHeaderSize,
                     OUTPUT v_bk_type,
                     OUTPUT v_bk_frchn,
                     OUTPUT v_bk_incr,
                     OUTPUT v_bk_updctr,
                     OUTPUT v_bkCheckSum,
                     OUTPUT v_objectId,
                     OUTPUT v_objectType,
                     OUTPUT v_bkObjDbkey,
                     OUTPUT vErrorMsg).

  IF vErrorMsg NE "" THEN
  ASSIGN IdxBlockInfo.ErrorMsg = IdxBlockInfo.ErrorMsg + vErrorMsg + "|".

  IF v_bk_type NE 12 THEN
  ASSIGN IdxBlockInfo.ErrorMsg = IdxBlockInfo.ErrorMsg +
         "Wrong bk_type in Object Block: " + STRING(v_bk_type) + "|".

  IF v_bk_frchn NE 127 /*NOCHN*/ THEN
  ASSIGN IdxBlockInfo.ErrorMsg = IdxBlockInfo.ErrorMsg +
         "Wrong bk_frchn in Object Block: " + STRING(v_bk_frchn) + "|".

  IF v_objectId NE IdxBlockInfo.IndexNum THEN
  ASSIGN IdxBlockInfo.ErrorMsg = IdxBlockInfo.ErrorMsg +
         "Wrong objectId in Object Block: " + STRING(v_objectId) + "|".

  IF v_objectType NE 2 THEN
  ASSIGN IdxBlockInfo.ErrorMsg = IdxBlockInfo.ErrorMsg +
         "Wrong objectType in Object Block: " + STRING(v_objectType) + "|".

  IF v_bkObjDbkey NE IdxBlockInfo.ObjDbkey THEN
  ASSIGN IdxBlockInfo.ErrorMsg = IdxBlockInfo.ErrorMsg +
         "Wrong bkObjDbkey in Object Block: " + STRING(v_bkObjDbkey) + "|".

/* Stop processing because of the wrong data: */
  IF vErrorMsg  NE "":U
  OR v_bk_type  NE 12
  OR v_objectId NE IdxBlockInfo.IndexNum THEN
  RETURN.

  RUN GetObjBlockInfo(INPUT  vRawBlockHeader,
                      INPUT  vBlockHeaderSize,
                      OUTPUT v_chainFirst_FREECHN,
                      OUTPUT v_chainFirst_RMCHN,
                      OUTPUT v_chainFirst_LOCKCHN,
                      OUTPUT v_numBlocksOnChainOld_FREECHN,
                      OUTPUT v_numBlocksOnChainOld_RMCHN,
                      OUTPUT v_numBlocksOnChainOld_LOCKCHN,
                      OUTPUT v_chainLast_FREECHN,
                      OUTPUT v_chainLast_RMCHN,
                      OUTPUT v_chainLast_LOCKCHN,
                      OUTPUT v_objectId,
                      OUTPUT v_objectType,
                      OUTPUT IdxBlockInfo.SerialNumber /*serialNumber*/,
                      OUTPUT v_firstFreeCluster,
                      OUTPUT v_lastFreeCluster,
                      OUTPUT IdxBlockInfo.TotalBlocks /*totalBlocks*/,
                      OUTPUT IdxBlockInfo.HiWaterBlock /*hiWaterBlock*/,
                      OUTPUT IdxBlockInfo.NumBlocksOnChain_FREECHN,
                      OUTPUT v_NumBlocksOnChain_RMCHN,
                      OUTPUT IdxBlockInfo.NumBlocksOnChain_LOCKCHN).

  ASSIGN LENGTH(vRawBlockHeader) = 0.

END PROCEDURE. /* GetIdxRootBlockInfo */

/* -------------------------------------------------------------------------  */
PROCEDURE GetBlockHeader.
  DEFINE INPUT  PARAMETER ipAreaNumber AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER ipDbkey      AS RECID   NO-UNDO.
  DEFINE OUTPUT PARAMETER opRawHeader  AS RAW     NO-UNDO.
  DEFINE OUTPUT PARAMETER opHeaderSize AS INTEGER NO-UNDO.

/* Standart Block Header:  */

/* Dec offset after BlockHeaderSize / Field size */
  DEFINE VARIABLE         /*000/4*/ v_bk_dbkey    AS INTEGER NO-UNDO. /* Block Address (32-bit dbkey) */
  DEFINE OUTPUT PARAMETER /*004/1*/ op_bk_type    AS INTEGER NO-UNDO. /* Block Type */
  DEFINE OUTPUT PARAMETER /*005/1*/ op_bk_frchn   AS INTEGER NO-UNDO. /* Chain Type */
  DEFINE OUTPUT PARAMETER /*006/2*/ op_bk_incr    AS INTEGER NO-UNDO. /* Incremental Backup Counter */
  DEFINE VARIABLE         /*008/4*/ v_bk_nextf    AS INTEGER NO-UNDO. /* Chain Next Block (dbkey) */
  DEFINE OUTPUT PARAMETER /*012/4*/ op_bk_updctr  AS INTEGER NO-UNDO. /* Block Version Number */

/* Type 2 Block Header:  */

  DEFINE OUTPUT PARAMETER /*016/2*/ op_bkCheckSum   AS INTEGER NO-UNDO.
  DEFINE VARIABLE         /*018/2*/ v_bkHeaderSize  AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER /*020/2*/ op_objectId     AS INTEGER NO-UNDO. /* _StorageObject._Object-Number */
  DEFINE OUTPUT PARAMETER /*022/2*/ op_objectType   AS INTEGER NO-UNDO. /* _StorageObject._Object-Type */
  DEFINE OUTPUT PARAMETER /*024/8*/ op_bkObjDbkey   AS RECID   NO-UNDO.
  DEFINE VARIABLE         /*032/8*/ v_bkDbkey       AS RECID   NO-UNDO.
  DEFINE VARIABLE         /*040/8*/ op_bkNextf      AS RECID   NO-UNDO.
  DEFINE VARIABLE         /*048/8*/ v_bkLastBiNote  AS INT64   NO-UNDO.
  DEFINE OUTPUT PARAMETER           op_ErrorMsg     AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vIsSAT2      AS LOGICAL   NO-UNDO INITIAL ?.
  DEFINE VARIABLE vExtentFile  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vExtentId    AS INTEGER   NO-UNDO.  
  DEFINE VARIABLE vExtentSize  LIKE _FileList._FileList-Size NO-UNDO.
  DEFINE VARIABLE vBlockOffset AS INT64     NO-UNDO. /* from begining of db (in KB) */
  DEFINE VARIABLE vPrevOffset  AS INT64     NO-UNDO.
  DEFINE VARIABLE vNextOffset  AS INT64     NO-UNDO.

FindExtent:
  FOR FIRST DICTDB._Area NO-LOCK
      WHERE DICTDB._Area._Area-number EQ ipAreaNumber:

    ASSIGN
      vExtentId    = 0
      vNextOffset  = 0
      vIsSAT2      = (DICTDB._Area._Area-clustersize GT 1)
      vBlockOffset = (INT64(ipDbkey) / EXP( 2, DICTDB._Area._Area-recbits))
                   * (DICTDB._Area._Area-blocksize / 1024)
    .  /* ASSIGN */

/* Find _FileList where _FileList-Name matches _Extent-path.
  _FileList-Name always returns full path while _Extent-path can be relative.
*/  FOR EACH DICTDB._AreaExtent NO-LOCK
       WHERE DICTDB._AreaExtent._Area-number EQ DICTDB._Area._Area-number
          BY DICTDB._AreaExtent._Extent-number /*use index _AreaExtent-Area */:

      ASSIGN vExtentId = vExtentId + 1.
/* The records in _FileList and _AreaExtent used to be in the same order: */
      FIND FIRST DICTDB._FileList NO-LOCK
           WHERE DICTDB._FileList._FileList-Id EQ vExtentId
      NO-ERROR.

/* ...otherwise do a full scan for the matches: */
      IF NOT AVAILABLE DICTDB._FileList
      OR NOT DICTDB._FileList._FileList-Name MATCHES
      ("*" + DICTDB._AreaExtent._Extent-path)
      THEN
      FIND FIRST DICTDB._FileList NO-LOCK
           WHERE DICTDB._FileList._FileList-Name MATCHES
          ("*" + DICTDB._AreaExtent._Extent-path)
      NO-ERROR.
      IF NOT AVAILABLE DICTDB._FileList THEN
      DO:
        MESSAGE 
         "Failed to find _FileList that matches" 
          DICTDB._AreaExtent._Extent-path SKIP
         "Execution is interrupted."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN ERROR.
      END.
        
      ASSIGN
        vExtentFile = DICTDB._FileList._FileList-Name
        vExtentSize = DICTDB._FileList._FileList-Size - 1
     /* vExtentSize does not include a header block */
        vPrevOffset = vNextOffset
        vNextOffset = vNextOffset + vExtentSize
      . /* ASSIGN */

      IF vBlockOffset LT vNextOffset THEN /* Bingo! */ LEAVE FindExtent.

    END. /* FOR EACH DICTDB._AreaExtent OF DICTDB._Area */
  END. /* FOR FIRST DICTDB._Area  */

  INPUT FROM VALUE(vExtentFile) BINARY.
  SEEK INPUT TO (vBlockOffset - vPrevOffset) * 1024.
  ASSIGN LENGTH(opRawHeader) = 1024.
  IMPORT UNFORMATTED opRawHeader.
  INPUT CLOSE.

/* Standart Block Header: */
  ASSIGN
    opHeaderSize  = 16                    /* Offset/Width */
    v_bk_dbkey    = GET-LONG(opRawHeader,  1) /* 00/2 */
    op_bk_type    = GET-BYTE(opRawHeader,  5) /* 04/1 */
    op_bk_frchn   = GET-BYTE(opRawHeader,  6) /* 05/1 */
    op_bk_incr    = GET-SHORT(opRawHeader, 7) /* 06/2 */
    v_bk_nextf    = GET-LONG(opRawHeader,  9) /* 08/4 */
    op_bk_updctr  = GET-LONG(opRawHeader, 13) /* 12/4 */
    v_bkDbkey     = v_bk_dbkey
    op_bkNextf    = v_bk_nextf
    op_bkObjDbkey = 0
  . /* ASSIGN */

/* Storage area type II: */
  IF vIsSAT2 THEN
  ASSIGN                                    /* Offset/Width */
    op_bkCheckSum   = GET-SHORT(opRawHeader, 17) /* 16/2 */
    v_bkHeaderSize  = GET-SHORT(opRawHeader, 19) /* 18/2 */
    opHeaderSize    = v_bkHeaderSize
    op_objectId     = GET-SHORT(opRawHeader, 21) /* 20/2 */
    op_objectType   = GET-SHORT(opRawHeader, 23) /* 22/2 */
    op_bkObjDbkey   = GET-INT64(opRawHeader, 25) /* 24/8 */
    v_bkDbkey       = GET-INT64(opRawHeader, 33) /* 32/8 */
    op_bkNextf      = GET-INT64(opRawHeader, 41) /* 40/8 */
    v_bkLastBiNote  = GET-INT64(opRawHeader, 49) /* 48/8 */
  . /* ASSIGN */

  IF ipDbkey NE v_bkDbkey THEN
  ASSIGN op_ErrorMsg = "Wrong dbkey: " + STRING(v_bkDbkey).

END PROCEDURE. /* GetBlockHeader */

/* -------------------------------------------------------------------------  */
PROCEDURE GetIdxBlockInfo.
  DEFINE INPUT PARAMETER ipRawHeader  AS RAW     NO-UNDO.
  DEFINE INPUT PARAMETER ipHeaderSize AS INTEGER NO-UNDO.

/* Block Type / bk_type : 2 / Index     */
/* Chain Type / bk_frchn: 127 / NoChain */

/* Dec offset after BlockHeaderSize / Field size */
  DEFINE OUTPUT PARAMETER /*000/1*/ op_ih_top   AS INTEGER NO-UNDO. /* Root Flag*/
  DEFINE OUTPUT PARAMETER /*001/1*/ op_ih_bot   AS INTEGER NO-UNDO. /* Leaf Flag*/
  DEFINE OUTPUT PARAMETER /*002/2*/ op_ih_ixnum AS INTEGER NO-UNDO. /* Index Number */
  DEFINE OUTPUT PARAMETER /*008/2*/ op_ih_nment AS INTEGER NO-UNDO. /* No. Index Entries */
  DEFINE OUTPUT PARAMETER /*010/2*/ op_ih_lnent AS INTEGER NO-UNDO. /* No. Bytes Used */

/* Index Block Header: */
  ASSIGN                                               /* Offset/Width */
    op_ih_top   = GET-BYTE(ipRawHeader,  ipHeaderSize +  1) /*000/1*/  
    op_ih_bot   = GET-BYTE(ipRawHeader,  ipHeaderSize +  2) /*001/1*/  
    op_ih_ixnum = GET-SHORT(ipRawHeader, ipHeaderSize +  3) /*002/2*/  
    op_ih_nment = GET-SHORT(ipRawHeader, ipHeaderSize +  9) /*008/2*/  
    op_ih_lnent = GET-SHORT(ipRawHeader, ipHeaderSize + 11) /*010/2*/ 
  . /* ASSIGN */

END PROCEDURE. /* GetIdxBlockInfo */

/* -------------------------------------------------------------------------  */
PROCEDURE GetDataBlockInfo.
  DEFINE INPUT PARAMETER ipRawHeader  AS RAW     NO-UNDO.
  DEFINE INPUT PARAMETER ipHeaderSize AS INTEGER NO-UNDO.

/* Block Type / bk_type : 3 / Data Block */

/* Dec offset after BlockHeaderSize / Field size */
 DEFINE OUTPUT PARAMETER /*000/1*/ op_numdir  AS INTEGER NO-UNDO.
 DEFINE OUTPUT PARAMETER /*001/1*/ op_freedir AS INTEGER NO-UNDO.
 DEFINE OUTPUT PARAMETER /*002/2*/ op_free    AS INTEGER NO-UNDO.
 DEFINE OUTPUT PARAMETER /*004/2*/ op_dir     AS INTEGER NO-UNDO.
 DEFINE OUTPUT PARAMETER opRecidLocks         AS INTEGER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

/* Object Block Header: */
  ASSIGN                                                /* Offset/Width */
    op_numdir  = GET-BYTE (ipRawHeader, ipHeaderSize + 001) /*000/1*/
    op_freedir = GET-BYTE (ipRawHeader, ipHeaderSize + 002) /*001/1*/
    op_free    = GET-SHORT(ipRawHeader, ipHeaderSize + 003) /*002/2*/
    opRecidLocks = 0
  . /* ASSIGN */
 
  DO i = 1 TO op_numdir:
    ASSIGN op_dir = GET-SHORT(ipRawHeader, ipHeaderSize + 2 + i * 2).
    IF op_dir NE 0 AND op_dir GT 8192 THEN
    ASSIGN opRecidLocks = opRecidLocks + 1.
  END.
/* Draft version. Need more accurate parsing:
   4 bits - flags, 12 bits - record offset
*/
END PROCEDURE. /* GetDataBlockInfo */

/* -------------------------------------------------------------------------  */
PROCEDURE GetObjBlockInfo.
  DEFINE INPUT PARAMETER ipRawHeader  AS RAW     NO-UNDO.
  DEFINE INPUT PARAMETER ipHeaderSize AS INTEGER NO-UNDO.

/* Block Type / bk_type : 12 / Object Block */
/* Chain Type / bk_frchn: 127 / NoChain     */

/* Dec offset after BlockHeaderSize / Field size */
  DEFINE OUTPUT PARAMETER /*000/8*/ op_chainFirst_FREECHN          AS INT64 NO-UNDO.
  DEFINE OUTPUT PARAMETER /*008/8*/ op_chainFirst_RMCHN            AS INT64 NO-UNDO.
  DEFINE OUTPUT PARAMETER /*016/8*/ op_chainFirst_LOCKCHN          AS INT64 NO-UNDO.
  DEFINE OUTPUT PARAMETER /*024/8*/ op_numBlocksOnChainOld_FREECHN AS INT64 NO-UNDO.
  DEFINE OUTPUT PARAMETER /*032/8*/ op_numBlocksOnChainOld_RMCHN   AS INT64 NO-UNDO.
  DEFINE OUTPUT PARAMETER /*040/8*/ op_numBlocksOnChainOld_LOCKCHN AS INT64 NO-UNDO.
  DEFINE OUTPUT PARAMETER /*048/8*/ op_chainLast_FREECHN           AS INT64 NO-UNDO.
  DEFINE OUTPUT PARAMETER /*056/8*/ op_chainLast_RMCHN             AS INT64 NO-UNDO.
  DEFINE OUTPUT PARAMETER /*064/8*/ op_chainLast_LOCKCHN           AS INT64 NO-UNDO.
  DEFINE OUTPUT PARAMETER /*072/2*/ op_objectId                    AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER /*074/2*/ op_objectType                  AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER /*080/8*/ op_serialNumber                AS INT64 NO-UNDO.
  DEFINE OUTPUT PARAMETER /*088/8*/ op_firstFreeCluster            AS INT64 NO-UNDO.
  DEFINE OUTPUT PARAMETER /*096/8*/ op_lastFreeCluster             AS INT64 NO-UNDO.
  DEFINE OUTPUT PARAMETER /*104/8*/ op_totalBlocks                 AS INT64 NO-UNDO.
  DEFINE OUTPUT PARAMETER /*112/8*/ op_hiWaterBlock                AS INT64 NO-UNDO.
  DEFINE OUTPUT PARAMETER /*120/8*/ op_numBlocksOnChain_FREECHN    AS INT64 NO-UNDO.
  DEFINE OUTPUT PARAMETER /*128/8*/ op_numBlocksOnChain_RMCHN      AS INT64 NO-UNDO.
  DEFINE OUTPUT PARAMETER /*136/8*/ op_numBlocksOnChain_LOCKCHN    AS INT64 NO-UNDO.

/* Object Block Header: */
  ASSIGN                                                                   /* Offset/Width */
    op_chainFirst_FREECHN          = GET-INT64(ipRawHeader, ipHeaderSize + 001) /*000/8*/
    op_chainFirst_RMCHN            = GET-INT64(ipRawHeader, ipHeaderSize + 009) /*008/8*/
    op_chainFirst_LOCKCHN          = GET-INT64(ipRawHeader, ipHeaderSize + 017) /*016/8*/
    op_numBlocksOnChainOld_FREECHN = GET-INT64(ipRawHeader, ipHeaderSize + 025) /*024/8*/
    op_numBlocksOnChainOld_RMCHN   = GET-INT64(ipRawHeader, ipHeaderSize + 033) /*032/8*/
    op_numBlocksOnChainOld_LOCKCHN = GET-INT64(ipRawHeader, ipHeaderSize + 041) /*040/8*/
    op_chainLast_FREECHN           = GET-INT64(ipRawHeader, ipHeaderSize + 049) /*048/8*/
    op_chainLast_RMCHN             = GET-INT64(ipRawHeader, ipHeaderSize + 057) /*056/8*/
    op_chainLast_LOCKCHN           = GET-INT64(ipRawHeader, ipHeaderSize + 065) /*064/8*/
    op_objectId                    = GET-SHORT(ipRawHeader, ipHeaderSize + 073) /*072/2*/
    op_objectType                  = GET-SHORT(ipRawHeader, ipHeaderSize + 075) /*074/2*/
    op_serialNumber                = GET-INT64(ipRawHeader, ipHeaderSize + 081) /*080/8*/
    op_firstFreeCluster            = GET-INT64(ipRawHeader, ipHeaderSize + 089) /*088/8*/
    op_lastFreeCluster             = GET-INT64(ipRawHeader, ipHeaderSize + 097) /*096/8*/
    op_totalBlocks                 = GET-INT64(ipRawHeader, ipHeaderSize + 105) /*104/8*/
    op_hiWaterBlock                = GET-INT64(ipRawHeader, ipHeaderSize + 113) /*112/8*/
    op_numBlocksOnChain_FREECHN    = GET-INT64(ipRawHeader, ipHeaderSize + 121) /*120/8*/
    op_numBlocksOnChain_RMCHN      = GET-INT64(ipRawHeader, ipHeaderSize + 129) /*128/8*/
    op_numBlocksOnChain_LOCKCHN    = GET-INT64(ipRawHeader, ipHeaderSize + 137) /*136/8*/
  . /* ASSIGN */

END PROCEDURE. /* GetObjBlockInfo */

/* -------------------------------------------------------------------------  */
PROCEDURE IndexLevels.
  DEFINE INPUT  PARAMETER ipTableName AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipIndexName AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opIndexLevels AS INTEGER NO-UNDO.

  DEFINE VARIABLE vUserIO-DbAccess LIKE _UserIO._UserIO-DbAccess NO-UNDO.
  DEFINE VARIABLE qh AS HANDLE NO-UNDO. /*Query handle*/
  DEFINE VARIABLE bh AS HANDLE NO-UNDO. /*buffer handle*/

  CREATE BUFFER bh FOR TABLE ipTableName.
  CREATE QUERY qh.
  qh:SET-BUFFERS(bh).
  qh:QUERY-PREPARE(
  "FOR EACH " + ipTableName + " NO-LOCK NO-PREFETCH USE-INDEX " + ipIndexName).
  qh:QUERY-OPEN().
/* First read can cause the extra db requests:*/
  qh:GET-NEXT().
  
/* Stat before record read: */
  FIND FIRST DICTDB._MyConnection NO-LOCK.
  FIND FIRST DICTDB._UserIO NO-LOCK 
       WHERE DICTDB._UserIO._UserIO-Usr EQ DICTDB._MyConnection._MyConn-UserId.
  ASSIGN vUserIO-DbAccess = _UserIO._UserIO-DbAccess.

/* Test read: */
  qh:GET-LAST().

/* Stat after record read: */
  FIND FIRST DICTDB._UserIO NO-LOCK 
     WHERE DICTDB._UserIO._UserIO-Usr EQ DICTDB._MyConnection._MyConn-UserId.

  ASSIGN opIndexLevels = DICTDB._UserIO._UserIO-DbAccess - vUserIO-DbAccess
                       - (IF qh:QUERY-OFF-END THEN 0 ELSE 1).
  DELETE OBJECT qh NO-ERROR.
  DELETE OBJECT bh NO-ERROR.
END PROCEDURE. /* IndexLevels */

/* -------------------------------------------------------------------------  */
