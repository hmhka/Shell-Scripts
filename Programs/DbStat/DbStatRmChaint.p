RUN ReportRmChains("").

/* ----------------------------------------------------------------------------
    File        : DbStatRmChains.p
    Purpose     : 

    Author(s)   : George Potemkin
    Created     : Novermber 21, 2015
    Modified    : Novermber 21, 2015
    Version     : 1.0
    
    The most recent version can be downloaded from here:
    ftp://ftp.progress-tech.ru/pub/Users/george/Programs/DbStatRmChains.p
    
    Syntax      : See the examples above.
    
---------------------------------------------------------------------------- */

DEFINE TEMP-TABLE ttRmChain NO-UNDO
  FIELD Db_Name     AS CHARACTER
  FIELD Db_Path     AS CHARACTER
  FIELD Db_Host     AS CHARACTER
  FIELD AreaNumber  AS INTEGER
  FIELD TableNumber AS INTEGER
  INDEX UniqueKey   IS UNIQUE
        Db_Name    
        Db_Path    
        Db_Host    
        AreaNumber 
        TableNumber
. /* DEFINE TEMP-TABLE ttRmChain */

&SCOPED-DEFINE Sep "~t"

DEFINE STREAM RemovedBlks.
DEFINE STREAM UpdatedBlks.
DEFINE STREAM AddedBlocks.

/* ------------------------------------------------------------------------- */

PROCEDURE ReportRmChains.

/* The prefix for output files: */
  DEFINE INPUT PARAMETER ipOutPrefix AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vReportPrefix  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vRemovedBlocks AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vUpdatedBlocks AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vAddedBlocks   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vBlockSummary  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vPrevListEnd   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vPrevMaxDbkey  AS INT64     NO-UNDO.
  
  DEFINE BUFFER DbStat1     FOR DbStat.
  DEFINE BUFFER DbStat2     FOR DbStat.
  DEFINE BUFFER AreaStat1   FOR AreaStat.
  DEFINE BUFFER AreaStat2   FOR AreaStat.
  DEFINE BUFFER ChainStat1  FOR ChainStat.
  DEFINE BUFFER ChainStat2  FOR ChainStat.
  DEFINE BUFFER ChainBlock1 FOR ChainBlock.
  DEFINE BUFFER ChainBlock2 FOR ChainBlock.
  
  FOR EACH DbStat NO-LOCK
     WHERE LOOKUP("chanalys":U, DbStat.SourceType) GT 0,

      EACH ChainStat NO-LOCK
     WHERE ChainStat.ChainType EQ "RM chain"
       AND CAN-FIND(FIRST ChainBlock OF ChainStat)
  TRANSACTION:

    FIND FIRST ttRmChain EXCLUSIVE-LOCK
         WHERE ttRmChain.Db_Name     EQ DbStat.Db_Name    
           AND ttRmChain.Db_Path     EQ DbStat.Db_Path    
           AND ttRmChain.Db_Host     EQ DbStat.Db_Host    
           AND ttRmChain.AreaNumber  EQ ChainStat.AreaNumber 
           AND ttRmChain.TableNumber EQ ChainStat.ObjectNumber
    NO-ERROR.
    IF NOT AVAILABLE ttRmChain THEN
    CREATE ttRmChain.
    ASSIGN ttRmChain.Db_Name     = DbStat.Db_Name        
           ttRmChain.Db_Path     = DbStat.Db_Path        
           ttRmChain.Db_Host     = DbStat.Db_Host        
           ttRmChain.AreaNumber  = ChainStat.AreaNumber  
           ttRmChain.TableNumber = ChainStat.ObjectNumber
    . /* ASSIGN */
  END. /* FOR EACH DbStat NO-LOCK, EACH ChainStat */

  FOR EACH ttRmChain NO-LOCK:

/* The most recent DbStat with chain blocks: */
    FOR EACH DbStat2 NO-LOCK
       WHERE LOOKUP("chanalys":U, DbStat2.SourceType) GT 0
         AND DbStat2.Db_Name EQ ttRmChain.Db_Name
         AND DbStat2.Db_Path EQ ttRmChain.Db_Path
         AND DbStat2.Db_Host EQ ttRmChain.Db_Host,

       FIRST ChainStat2 OF DbStat2 NO-LOCK
       WHERE ChainStat2.ChainType    EQ "RM chain"
         AND ChainStat2.AreaNumber   EQ ttRmChain.AreaNumber 
         AND ChainStat2.ObjectNumber EQ ttRmChain.TableNumber,

       FIRST ChainBlock2 OF ChainStat2 NO-LOCK,
          
       FIRST AreaStat2 OF DbStat2 NO-LOCK
       WHERE AreaStat2.AreaNumber  EQ ChainStat2.AreaNumber
         AND AreaStat2.ClusterSize LT ChainStat2.ChainBlocks
/* Do not report the short chains  ^^^ */

          BY DbStat2.BgnTime DESCENDING:
      LEAVE.
    END. /* FOR EACH DbStat2,
               FIRST ChainStat2 OF DbStat2,
               FIRST ChainBlock2 OF ChainStat2,
               FIRST AreaStat2 OF DbStat2 */

    /* The previous DbStat with chain blocks: */
    FOR EACH DbStat1 NO-LOCK
       WHERE LOOKUP("chanalys":U, DbStat1.SourceType) GT 0
         AND DbStat1.StatId  NE DbStat2.StatId
         AND DbStat1.Db_Name EQ DbStat2.Db_Name
         AND DbStat1.Db_Path EQ DbStat2.Db_Path
         AND DbStat1.Db_Host EQ DbStat2.Db_Host,

       FIRST ChainStat1 OF DbStat1 NO-LOCK
       WHERE ChainStat1.ChainType    EQ ChainStat2.ChainType
         AND ChainStat1.AreaNumber   EQ ChainStat2.AreaNumber 
         AND ChainStat1.ObjectNumber EQ ChainStat2.ObjectNumber,

       FIRST ChainBlock1 OF ChainStat1 NO-LOCK,

       FIRST AreaStat1 OF DbStat1 NO-LOCK
       WHERE AreaStat1.AreaNumber EQ ChainStat1.AreaNumber

          BY DbStat1.BgnTime DESCENDING:
      LEAVE.
    END. /* FOR EACH DbStat1,
               FIRST ChainStat1 OF DbStat1,
               FIRST ChainBlock1 OF ChainStat1,
               FIRST AreaStat1 OF DbStat1 */

/* Report RM chains with at least two snapshots: */
    IF NOT AVAILABLE DbStat1 THEN
    NEXT.

    ASSIGN vReportPrefix = SUBSTITUTE(
             "RM.chain.&1.&2.area.&3.tbl.&4.&5-&6",
                  /*&1*/ DbStat1.Db_Host,
                  /*&2*/ DbStat1.Db_Name,
                  /*&3*/ STRING(ChainStat1.AreaNumber),
                  /*&4*/ STRING(ChainStat1.ObjectNumber),
                  /*&5*/ SUBSTRING(ISO-DATE(DbStat1.BgnTime), 1, 19),
                  /*&6*/ SUBSTRING(ISO-DATE(DbStat2.BgnTime), 1, 19))
           vReportPrefix = REPLACE(vReportPrefix, ":":U, ".":U)
           vReportPrefix = ipOutPrefix + MIN(ipOutPrefix, ".":U)
                         + vReportPrefix
           vRemovedBlocks = vReportPrefix + ".removed.txt"
           vUpdatedBlocks = vReportPrefix + ".updated.txt"
           vAddedBlocks   = vReportPrefix + ".added.txt"
           vBlockSummary  = vReportPrefix + ".summary.txt"
    . /* ASSIGN */

    OUTPUT STREAM RemovedBlks TO VALUE(vRemovedBlocks).
    PUT STREAM RemovedBlks UNFORMATTED
             "Order1"
      {&Sep} "Dbkey"
      {&Sep} "Free Space"
      {&Sep} "Free Slots"
      {&Sep} "Slot Holds"
    SKIP. /* PUT */

    OUTPUT STREAM UpdatedBlks TO VALUE(vUpdatedBlocks).
    PUT STREAM UpdatedBlks UNFORMATTED
             "Order1"
      {&Sep} "Order2"
      {&Sep} "To End"
      {&Sep} "Dbkey"
      {&Sep} "Free Space"
      {&Sep} "Space Upd"
      {&Sep} "Free Slots"
      {&Sep} "Slots Upd"
      {&Sep} "Slot Holds"
      {&Sep} "Holds Upd"
    SKIP. /* PUT */

    OUTPUT STREAM AddedBlocks TO VALUE(vAddedBlocks).
    PUT STREAM AddedBlocks UNFORMATTED
             "Order2"
      {&Sep} "Dbkey"
      {&Sep} "Free Space"
      {&Sep} "Free Slots"
      {&Sep} "Slot Holds"
      {&Sep} "To End"
      {&Sep} "Extend"
    SKIP. /* PUT */


    ASSIGN vPrevMaxDbkey = ?.
    FOR EACH ChainBlock1 OF ChainStat1 NO-LOCK
          BY ChainBlock1.StatId     DESCENDING
          BY ChainBlock1.ChainOrder DESCENDING
          BY ChainBlock1.BlockDbkey DESCENDING:
      ASSIGN vPrevMaxDbkey = ChainBlock1.BlockDbkey.
      LEAVE.
    END. /* FOR EACH (last) ChainBlock1 */


    ASSIGN vPrevListEnd = ?.
    FOR EACH ChainBlock1 OF ChainStat1 NO-LOCK,
       FIRST ChainBlock2 OF ChainStat2 NO-LOCK
       WHERE ChainBlock2.BlockDbkey EQ ChainBlock1.BlockDbkey
          BY ChainBlock1.StatId     DESCENDING
          BY ChainBlock1.ChainOrder DESCENDING
          BY ChainBlock1.BlockOrder DESCENDING:
      ASSIGN vPrevListEnd = ChainBlock2.BlockOrder.
      LEAVE.
    END. /* FOR EACH (last) ChainBlock1, FIRST ChainBlock2 */

    FOR EACH ChainBlock1 OF ChainStat1 NO-LOCK
          BY ChainBlock1.StatId
          BY ChainBlock1.ChainOrder
          BY ChainBlock1.BlockOrder:

      FIND FIRST ChainBlock2 OF ChainStat2 NO-LOCK
           WHERE ChainBlock2.BlockDbkey EQ ChainBlock1.BlockDbkey
      NO-ERROR.

      IF NOT AVAILABLE ChainBlock2 THEN
      DO:
        ACCUMULATE "Removed":U (COUNT).
        PUT STREAM RemovedBlks UNFORMATTED 
                 ChainBlock1.BlockOrder   /* Order1     */
          {&Sep} ChainBlock1.BlockDbkey   /* Dbkey      */
          {&Sep} ChainBlock1.BlockSpace   /* Free Space */
          {&Sep} ChainBlock1.BlockEntries /* Free Slots */
          {&Sep} ChainBlock1.BlockAttr1   /* Slot Holds */
        SKIP. /* PUT */
        NEXT.
      END. /* Deleted */

      IF ChainBlock1.BlockSpace   NE ChainBlock2.BlockSpace  
      OR ChainBlock1.BlockEntries NE ChainBlock2.BlockEntries
      OR ChainBlock1.BlockAttr1   NE ChainBlock2.BlockAttr1
      OR ChainBlock2.BlockOrder   GT vPrevListEnd   
        THEN
      DO:
        ACCUMULATE "Updated":U (COUNT).
        PUT STREAM UpdatedBlks UNFORMATTED
                 ChainBlock1.BlockOrder   /* Order1     */
          {&Sep} ChainBlock2.BlockOrder   /* Order2     */
          {&Sep} ChainBlock2.BlockOrder GT vPrevListEnd /* To End */
          {&Sep} ChainBlock1.BlockDbkey   /* Dbkey      */
          {&Sep} ChainBlock1.BlockSpace   /* Free Space */
          {&Sep} ChainBlock2.BlockSpace   /* Space Upd  */
               - ChainBlock1.BlockSpace
          {&Sep} ChainBlock1.BlockEntries /* Free Slots */
          {&Sep} ChainBlock2.BlockEntries /* Slots Upd  */
               - ChainBlock1.BlockEntries
          {&Sep} ChainBlock1.BlockAttr1   /* Slot Holds */
          {&Sep} ChainBlock2.BlockAttr1   /* Holds Upd  */
               - ChainBlock1.BlockAttr1
        SKIP. /* PUT */
      END. /* Changed */

    END. /* FOR EACH ChainBlock1 */
    
    OUTPUT STREAM RemovedBlks CLOSE.
    OUTPUT STREAM UpdatedBlks CLOSE.

    IF (ACCUM COUNT "Removed":U) EQ 0 THEN
    OS-DELETE VALUE(vRemovedBlocks) NO-ERROR.

    IF (ACCUM COUNT "Updated":U) EQ 0 THEN
    OS-DELETE VALUE(vUpdatedBlocks) NO-ERROR.

    FOR EACH ChainBlock2 OF ChainStat2 NO-LOCK
      WHERE NOT CAN-FIND(
       FIRST ChainBlock1 OF ChainStat1
       WHERE ChainBlock1.BlockDbkey EQ ChainBlock2.BlockDbkey)

          BY ChainBlock2.StatId
          BY ChainBlock2.ChainOrder
          BY ChainBlock2.BlockOrder:

      ACCUMULATE "Added":U (COUNT).
      PUT STREAM AddedBlocks UNFORMATTED
               ChainBlock2.BlockOrder                  /* Order2     */
        {&Sep} ChainBlock2.BlockDbkey                  /* Dbkey      */
        {&Sep} ChainBlock2.BlockSpace                  /* Free Space */
        {&Sep} ChainBlock2.BlockEntries                /* Free Slots */
        {&Sep} ChainBlock2.BlockAttr1                  /* Slot Holds */
        {&Sep} ChainBlock2.BlockOrder GT vPrevListEnd  /* To End     */
        {&Sep} ChainBlock2.BlockDbkey GT vPrevMaxDbkey /* Extend     */
      SKIP. /* PUT */
    END. /* FOR EACH ChainStat2 */

    OUTPUT STREAM AddedBlocks CLOSE.

    IF (ACCUM COUNT "Added":U) EQ 0 THEN
    OS-DELETE VALUE(vAddedBlocks) NO-ERROR.

    FOR EACH ChainBlock2 OF ChainStat2 NO-LOCK
          BY ChainBlock2.StatId     DESCENDING
          BY ChainBlock2.ChainOrder DESCENDING
          BY ChainBlock2.BlockOrder DESCENDING:
      LEAVE.
    END. /* FOR EACH (last) ChainBlock1, FIRST ChainBlock2 */

    OUTPUT TO value(vBlockSummary).
    
    PUT UNFORMATTED 
      "Area: " SUBSTITUTE('"&1":&2,&3;&4',
                          /*&1*/ AreaStat1.AreaName,
                          /*&2*/ AreaStat1.AreaNumber,
                          /*&3*/ AreaStat1.RecPerBlock,
                          /*&4*/ AreaStat1.ClusterSize) SPACE
      "Blocksize: " DbStat1.DbBlockSize  SKIP
      "Table # " ChainStat1.ObjectNumber SKIP
      "Blocks on RM chain: " ChainStat1.ChainBlocks " at " DbStat1.BgnTime SKIP
      "Blocks on RM chain: " ChainStat2.ChainBlocks " at " DbStat2.BgnTime SKIP
      "Blocks changed    : " ACCUM COUNT "Updated":U SKIP
      "Blocks removed    : " ACCUM COUNT "Removed":U SKIP
      "Blocks added      : " ACCUM COUNT "Added":U   SKIP
      "Moved to the end  : " ChainBlock2.BlockOrder - vPrevListEnd SKIP
      "Chain increased   : " ChainStat2.ChainBlocks - ChainStat1.ChainBlocks SKIP
      "HWM increased     : " AreaStat2.HighBlock - AreaStat1.HighBlock
    . /* PUT */

    IF ChainStat2.ChainBlocks - ChainStat1.ChainBlocks 
    NE (ACCUM COUNT "Added":U) - (ACCUM COUNT "Removed":U) THEN
    PUT UNFORMATTED "Warning: Block count mismatch!" SKIP.

    OUTPUT CLOSE.
  END. /* FOR EACH ttRmChain */

END PROCEDURE. /* ReportRmChains */

