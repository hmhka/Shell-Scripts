RUN ReportClusterStat("Levin").

DEFINE TEMP-TABLE ttArea NO-UNDO
  FIELD AreaNumber AS INTEGER
  INDEX AreaNumber IS UNIQUE
        AreaNumber
. /* DEFINE TEMP-TABLE ttArea */

/* ------------------------------------------------------------------------- */

&SCOPED-DEFINE Sep "~t"

/* ------------------------------------------------------------------------- */

PROCEDURE ReportClusterStat.

/* The prefix for output files: */
  DEFINE INPUT PARAMETER ipOutPrefix AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vReportFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vPhaseTime  AS DATETIME  NO-UNDO.
  DEFINE VARIABLE i           AS INTEGER   NO-UNDO.

  EMPTY TEMP-TABLE ttArea.

  FOR EACH DbStat NO-LOCK,
      EACH AreaStat  OF DbStat NO-LOCK,
     FIRST ChainStat OF AreaStat NO-LOCK
     WHERE ChainStat.ChainType   EQ "free cluster chain":U
       AND ChainStat.ChainBlocks GT 0
  TRANSACTION:

    IF CAN-FIND(FIRST ttArea
                WHERE ttArea.AreaNumber EQ AreaStat.AreaNumber) THEN
    NEXT.

    CREATE ttArea.
    ASSIGN ttArea.AreaNumber = AreaStat.AreaNumber.
  END.

  IF NOT CAN-FIND(FIRST ttArea) THEN
  RETURN.

  ASSIGN ipOutPrefix = "dbstat":U WHEN ipOutPrefix EQ ? OR ipOutPrefix EQ "":U
         vReportFile = ipOutPrefix + ".free.clusters.txt"
  . /* ASSIGN */

  OUTPUT TO VALUE(vReportFile).

  PUT UNFORMATTED
           "Dbanalys time"  /* Begin time of dbanalys                   */
    {&Sep} "Area"           /* Area Name                                */
    {&Sep} "Area #"         /* Area Number                              */
    {&Sep} "RPB"            /* Records Per Block                        */
    {&Sep} "Cluster Size"   /* Cluster Size                             */
    {&Sep} "Free Clusters"  /* Blocks found in the free cluster chain   */
    {&Sep} "%HWM"           /* Percentage of area's high water mark     */
    {&Sep} "%Free"          /* Percentage of free blocks                */
    {&Sep} "Chain Scan"     /* Duration of CHAIN ANALYSIS FOR AREA phase*/
    {&Sep} "Block Scan"     /* Duration of AREA BLOCK ANALYSIS phase    */
    {&Sep} "HWM"            /* Area's High Water Mark                   */
    {&Sep} "IX Blocks"      /* Index blocks                             */
    {&Sep} "Indexes"        /* Index count                              */
    {&Sep} "RM Blocks"      /* Record blocks                            */
    {&Sep} "Tables"         /* Table count                              */
    {&Sep} "LOBs"           /* LOB count                                */
  SKIP. /* PUT */

  FOR EACH ttArea NO-LOCK:

    FOR EACH DbStat NO-LOCK,
        EACH AreaStat OF DbStat NO-LOCK
       WHERE AreaStat.AreaNumber EQ ttArea.AreaNumber,
       FIRST ChainStat OF AreaStat NO-LOCK
       WHERE ChainStat.ChainType EQ "free cluster chain":U
          BY DbStat.BgnTime:

      ASSIGN vPhaseTime = ?.
      REPEAT i = 1 TO EXTENT(AreaStat.PhaseName)
        WHILE AreaStat.PhaseTime[i] NE ?:
        IF AreaStat.PhaseName[i] NE "AREA BLOCK ANALYSIS":U THEN
        NEXT.
        ASSIGN vPhaseTime = AreaStat.PhaseTime[i].
        LEAVE.
      END.

      PUT UNFORMATTED
               ISO-DATE(DbStat.BgnTime)        /* Dbanalys time  */
        {&Sep} AreaStat.AreaName               /* Area           */
        {&Sep} AreaStat.AreaNumber             /* Area #         */
        {&Sep} AreaStat.RecPerBlock            /* RPB            */
        {&Sep} AreaStat.ClusterSize            /* Cluster Size   */
        {&Sep} ChainStat.ChainBlocks           /* Free Clusters  */
        {&Sep} ChainStat.ChainBlocks           /* %HWM           */
             * AreaStat.ClusterSize
             / AreaStat.HighBlock * 100.0
        {&Sep} ChainStat.ChainBlocks           /* %Free          */
             * AreaStat.ClusterSize
             / AreaStat.FreeBlocks * 100.0
        {&Sep} ChainStat.RunInterval           /* Chain Scan     */
        {&Sep} INTERVAL(AreaStat.EndTime,      /* Block Scan    */
                        vPhaseTime, "milliseconds":U) / 1000.0
        {&Sep} AreaStat.HighBlock              /* HWM           */
        {&Sep} AreaStat.IndexBlocks            /* IX Blocks     */
        {&Sep} AreaStat.NumIndexes             /* Indexes       */
        {&Sep} AreaStat.DataBlocks             /* RM Blocks     */
        {&Sep} AreaStat.NumTables              /* Tables        */
        {&Sep} AreaStat.NumLobs                /* LOBs          */
      SKIP. /* PUT */
    END. /* FOR EACH DbStat, EACH AreaStat OF DbStat, FIRST ChainStat OF */
  END. /* FOR EACH ttArea */

  OUTPUT CLOSE.

END PROCEDURE. /* ReportAreaStat */

/* ------------------------------------------------------------------------- */


