RUN ReportDbStat("Levin.chanalys", TRUE).

/* ----------------------------------------------------------------------------
    File        : DbStatReport.p
    Purpose     : Create the reports based on data in the statdb databases.

    Author(s)   : George Potemkin
    Created     : September 28, 2015
    Modified    : September 28, 2015
    Version     : 1.0
    
    The most recent version can be downloaded from here:
    ftp://ftp.progress-tech.ru/pub/Users/george/Programs/DbStatReport.p
    
    Syntax      : See the examples above.
    
---------------------------------------------------------------------------- */

&SCOPED-DEFINE Sep "~t"

/* ------------------------------------------------------------------------- */

PROCEDURE ReportDbStat.

/* The prefix for output files: */
  DEFINE INPUT PARAMETER ipOutPrefix AS CHARACTER NO-UNDO.

/* Append output to the existent files: */
  DEFINE INPUT PARAMETER ipAppend    AS LOGICAL   NO-UNDO.

  DEFINE BUFFER bDbStat FOR DbStat.

  DEFINE VARIABLE vSubPrefix  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vReportFile AS CHARACTER NO-UNDO.
  ASSIGN ipOutPrefix = "dbstat":U WHEN ipOutPrefix EQ ? OR ipOutPrefix EQ "":U
         vReportFile = ipOutPrefix + ".databases.txt"
  . /* ASSIGN */

  IF ipAppend
  THEN OUTPUT TO VALUE(vReportFile) APPEND.
  ELSE OUTPUT TO VALUE(vReportFile).

  IF SEEK(OUTPUT) EQ 0 THEN
  PUT UNFORMATTED
           "Db Host"          /* Db host                                  */
    {&Sep} "Db Name"          /* Db name                                  */
    {&Sep} "Db Path"          /* Path to the database                     */
    {&Sep} "Analysis Date"    /* Date of dbanalys start                   */
    {&Sep} "Exec Time"        /* Duration of dbanalys                     */
    {&Sep} "Online"           /* Duration of dbanalys                     */
    {&Sep} "Blocksize"        /* Db blocksize                             */
    {&Sep} "Group"            /* Tenancy/Partitions enabled               */
    {&Sep} "Areas"            /* Area count                               */
    {&Sep} "Total Blocks"     /* Total number of blocks found in the area */
    {&Sep} "HWM"              /* Sum of area's HighBlock                  */
    {&Sep} "Free Blocks"      /* free block(s) found in the area          */
    {&Sep} "Empty Blocks"     /* empty block(s) found in the area         */
    {&Sep} "Object List"      /* object list block(s) found in the area   */
    {&Sep} "Object Blocks"    /* object block(s) found in the area        */
    {&Sep} "Tables"           /* Table count                              */
    {&Sep} "LOBs"             /* LOB count                                */
    {&Sep} "RM Blocks"        /* Record blocks                            */
    {&Sep} "RM%"              /* Percentage of RM Blocks                  */
    {&Sep} "Indexes"          /* Index count                              */
    {&Sep} "IX Blocks"        /* Index blocks                             */
    {&Sep} "IX%"              /* Percentage of IX blocks                  */
    {&Sep} "Schema timestamp" /* Time of the most recent table's change   */
    {&Sep} "Errors"           /* Number of dbanalys errors                */
    {&Sep} "Dbanalys"         /* Dbanalys file                            */
  SKIP. /* PUT */

  FOR EACH DbStat              NO-LOCK,
     FIRST DbAlias  OF DbStat  NO-LOCK,
     FIRST DbHost   OF DbAlias NO-LOCK
        BY DbHost.HostName
        BY DbAlias.Db_Name
        BY DbStat.BgnTime:

    FOR EACH DbStatError NO-LOCK:
      ACCUMULATE "Errors":U (COUNT).
    END.

    FIND FIRST AreaStat OF DbStat NO-LOCK
         WHERE AreaStat.AreaNumber EQ 0
    NO-ERROR.
    IF NOT AVAILABLE AreaStat THEN
    FIND FIRST AreaStat OF DbStat NO-LOCK.

    PUT UNFORMATTED
             DbHost.HostName              /* Db Host          */
      {&Sep} DbAlias.Db_Name              /* Db Name          */
      {&Sep} DbAlias.Db_Path              /* Db Path          */
      {&Sep} ISO-DATE(DbStat.BgnTime)     /* Analysis Date    */
      {&Sep} DbStat.RunInterval           /* Exec Time        */
      {&Sep} LOOKUP("online", DbStat.SourceType) GT 0 /* Online */
      {&Sep} DbStat.DbBlockSize           /* Blocksize        */
      {&Sep} STRING(DbStat.GroupEnabled, "Enabled/":U) /* Group */
      {&Sep} DbStat.NumAreas              /* Areas            */
      {&Sep} AreaStat.TotalBlocks         /* Total Blocks     */
      {&Sep} AreaStat.HighBlock           /* HWM              */
      {&Sep} AreaStat.FreeBlocks          /* Free Blocks      */
      {&Sep} AreaStat.EmptyBlocks         /* Empty Blocks     */
      {&Sep} AreaStat.ObjectListBlocks    /* Object List      */
      {&Sep} AreaStat.ObjectBlocks        /* Object Blocks    */
      {&Sep} AreaStat.NumTables           /* Tables           */
      {&Sep} AreaStat.NumLobs             /* LOBs             */
      {&Sep} AreaStat.DataBlocks          /* RM Blocks        */
      {&Sep} ROUND(AreaStat.DataBlocks / AreaStat.HighBlock * 100.0, 1) /*RM%*/
      {&Sep} AreaStat.NumIndexes          /* Indexes          */
      {&Sep} AreaStat.IndexBlocks         /* IX Blocks        */
      {&Sep} ROUND(AreaStat.IndexBlocks / AreaStat.HighBlock * 100.0,1) /*IX%*/
      {&Sep} DbStat.DbSchemaTime          /* Schema timestamp */
      {&Sep} ACCUM COUNT "Errors":U       /* Errors           */
      {&Sep} DbStat.SourceFile            /* Dbanalys file    */
    SKIP. /* PUT */

  END. /* FOR EACH DbStat, FIRST DbAlias OF DbStat, FIRST DbHost OF DbAlias */

  OUTPUT CLOSE.

  FOR EACH bDbStat NO-LOCK,
     FIRST DbAlias  OF bDbStat NO-LOCK,
     FIRST DbHost   OF DbAlias NO-LOCK:

    IF ipAppend THEN
    ASSIGN vSubPrefix = ipOutPrefix.
    ELSE
    ASSIGN /* ISO-DATE: YYYY-MM-DDTHH:MM:SS.SSS */
           vSubPrefix = ISO-DATE(bDbStat.BgnTime)
           vSubPrefix = REPLACE(vSubPrefix, ":":U, ".":U)
           vSubPrefix = SUBSTRING(vSubPrefix, 1, 19)
           vSubPrefix = ipOutPrefix     + ".":U
                      + DbHost.HostName + ".":U
                      + DbAlias.Db_Name + ".":U
                      + vSubPrefix
    . /* ASSIGN */

    RUN ReportAreaStat (bDbStat.StatId, vSubPrefix, ipAppend).
    RUN ReportTableStat(bDbStat.StatId, vSubPrefix, ipAppend).
    RUN ReportIndexStat(bDbStat.StatId, vSubPrefix, ipAppend).
    RUN ReportChainStat(bDbStat.StatId, vSubPrefix, ipAppend).
  END.

END PROCEDURE. /* ReportDbStat */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportAreaStat.

  DEFINE INPUT PARAMETER ipStatId    AS INTEGER   NO-UNDO.
/* The prefix for output files: */
  DEFINE INPUT PARAMETER ipOutPrefix AS CHARACTER NO-UNDO.
/* Append output to the existent files: */
  DEFINE INPUT PARAMETER ipAppend    AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE vReportFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vPhaseTime  AS DATETIME  NO-UNDO.
  DEFINE VARIABLE i           AS INTEGER   NO-UNDO.

  ASSIGN ipOutPrefix = "dbstat":U WHEN ipOutPrefix EQ ? OR ipOutPrefix EQ "":U
         vReportFile = ipOutPrefix + ".areas.txt"
  . /* ASSIGN */

  IF ipAppend
  THEN OUTPUT TO VALUE(vReportFile) APPEND.
  ELSE OUTPUT TO VALUE(vReportFile).

  IF SEEK(OUTPUT) EQ 0 THEN
  PUT UNFORMATTED
           "Db Host"       /* Db host                                  */
    {&Sep} "Db Name"       /* Db name                                  */
    {&Sep} "Analysis Date" /* Date of dbanalys start                   */
    {&Sep} "Area"          /* Area Name                                */
    {&Sep} "Area #"        /* Area Number                              */
    {&Sep} "RPB"           /* Records Per Block                        */
    {&Sep} "Cluster Size"  /* Cluster Size                             */
    {&Sep} "AltBuf"        /* Alternative Buffer Pool                  */
    {&Sep} "Chain Scan"    /* Duration of CHAIN ANALYSIS FOR AREA phase*/
    {&Sep} "Block Scan"    /* Duration of AREA BLOCK ANALYSIS phase    */
    {&Sep} "Total Blocks"  /* Total number of blocks found in the area */
    {&Sep} "HWM"           /* Area's High Water Mark                   */
    {&Sep} "Object List"   /* object list block(s) found in the area   */
    {&Sep} "Object Blocks" /* object block(s) found in the area        */
    {&Sep} "Tables"        /* Table count                              */
    {&Sep} "Records"       /* Total number of records in the area      */
    {&Sep} "Fragments"     /* Total number of record fragments         */
    {&Sep} "RM Blocks"     /* Record blocks                            */
    {&Sep} "RM%"           /* Percentage of RM Blocks                  */
    {&Sep} "LOBs"          /* LOB count                                */
    {&Sep} "Indexes"       /* Index count                              */
    {&Sep} "IX Blocks"     /* Index blocks                             */
    {&Sep} "IX%"           /* Percentage of IX blocks                  */
    {&Sep} "Free Blocks"   /* free block(s) found in the area          */
    {&Sep} "Empty Blocks"  /* empty block(s) found in the area         */
  SKIP. /* PUT */

  FOR FIRST DbStat NO-LOCK
      WHERE DbStat.StatId EQ ipStatId,
      FIRST DbAlias  OF DbStat  NO-LOCK,
      FIRST DbHost   OF DbAlias NO-LOCK,
       EACH AreaStat OF DbStat  NO-LOCK
         BY AreaStat.StatId
         BY AreaStat.AreaNumber: /* INDEX AreaNumber */

    FOR EACH TableStat OF AreaStat NO-LOCK:
      ACCUMULATE TableStat.RecCount  (TOTAL)
                 TableStat.FragCount (TOTAL)
      . /* ACCUMULATE */
    END.

    ASSIGN vPhaseTime = ?.
    REPEAT i = 1 TO EXTENT(AreaStat.PhaseName)
      WHILE AreaStat.PhaseTime[i] NE ?:
      IF AreaStat.PhaseName[i] NE "AREA BLOCK ANALYSIS":U THEN
      NEXT.
      ASSIGN vPhaseTime = AreaStat.PhaseTime[i].
      LEAVE.
    END.

    PUT UNFORMATTED
             DbHost.HostName              /* Db Host          */
      {&Sep} DbAlias.Db_Name              /* Db Name          */
      {&Sep} ISO-DATE(DbStat.BgnTime)     /* Analysis Date    */
      {&Sep} AreaStat.AreaName               /* Area          */
      {&Sep} AreaStat.AreaNumber             /* Area #        */
      {&Sep} AreaStat.RecPerBlock            /* RPB           */
      {&Sep} AreaStat.ClusterSize            /* Cluster Size  */
      {&Sep} AreaStat.AltBufferPool          /* AltBuf        */
      {&Sep} INTERVAL(vPhaseTime,            /* Chain Scan    */
                      AreaStat.BgnTime, "milliseconds":U) / 1000.0
      {&Sep} INTERVAL(AreaStat.EndTime,      /* Block Scan    */
                      vPhaseTime, "milliseconds":U) / 1000.0
      {&Sep} AreaStat.TotalBlocks            /* Total Blocks  */
      {&Sep} AreaStat.HighBlock              /* HWM           */
      {&Sep} AreaStat.ObjectListBlocks       /* Object List   */
      {&Sep} AreaStat.ObjectBlocks           /* Object Blocks */
      {&Sep} AreaStat.NumTables              /* Tables        */
      {&Sep} ACCUM TOTAL TableStat.RecCount  /* Records       */
      {&Sep} ACCUM TOTAL TableStat.FragCount /* Fragments     */
      {&Sep} AreaStat.DataBlocks             /* RM Blocks     */
      {&Sep} ROUND(AreaStat.DataBlocks / AreaStat.HighBlock * 100.0, 1) /*RM%*/
      {&Sep} AreaStat.NumLobs                /* LOBs          */
      {&Sep} AreaStat.NumIndexes             /* Indexes       */
      {&Sep} AreaStat.IndexBlocks            /* IX Blocks     */
      {&Sep} ROUND(AreaStat.IndexBlocks / AreaStat.HighBlock * 100.0, 1) /*IX%*/
      {&Sep} AreaStat.FreeBlocks             /* Free Blocks   */
      {&Sep} AreaStat.EmptyBlocks            /* Empty Blocks  */
    SKIP. /* PUT */
  END. /* FOR EACH AreaStat */

  OUTPUT CLOSE.

END PROCEDURE. /* ReportAreaStat */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportTableStat.

  DEFINE INPUT PARAMETER ipStatId    AS INTEGER   NO-UNDO.
/* The prefix for output files: */
  DEFINE INPUT PARAMETER ipOutPrefix AS CHARACTER NO-UNDO.
/* Append output to the existent files: */
  DEFINE INPUT PARAMETER ipAppend    AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE vReportFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vChainBlocks AS INT64    NO-UNDO.

  IF NOT CAN-FIND(FIRST TableStat WHERE TableStat.StatId EQ ipStatId) THEN
  RETURN.

  ASSIGN ipOutPrefix = "dbstat":U WHEN ipOutPrefix EQ ? OR ipOutPrefix EQ "":U
         vReportFile = ipOutPrefix + ".tables.txt"
  . /* ASSIGN */

  IF ipAppend
  THEN OUTPUT TO VALUE(vReportFile) APPEND.
  ELSE OUTPUT TO VALUE(vReportFile).

  IF SEEK(OUTPUT) EQ 0 THEN
  PUT UNFORMATTED
           "Db Host"       /* Db host                               */
    {&Sep} "Db Name"       /* Db name                               */
    {&Sep} "Analysis Date" /* Date of dbanalys start                */
    {&Sep} "Area"          /* Area Name                             */
    {&Sep} "Area #"        /* Area Number                           */
    {&Sep} "RPB"           /* Records Per Block                     */
    {&Sep} "Cluster Size"  /* Cluster Size                          */
    {&Sep} "Table #"       /* Table Number                          */
    {&Sep} "Owner"         /* Table Owner                           */
    {&Sep} "Table Name"    /* Table Name                            */
    {&Sep} "Table Group"   /* Group                                 */
    {&Sep} "Last Change"   /* Last Change                           */
    {&Sep} "Records"       /* Records                               */
    {&Sep} "Fragments"     /* Fragments                             */
    {&Sep} "Frag%"         /* Frag%                                 */
    {&Sep} "Table Size"    /* Table Size in bytes                   */
    {&Sep} "Size"          /* Table Size in the units (K,M,G,T)     */
    {&Sep} "%HWM"          /* Percentage of the area's HWM          */
    {&Sep} "Indexes"       /* Number of indexes                     */
    {&Sep} "Index Size"    /* Total size of table's indexes         */
    {&Sep} "Index/Table"   /* Index Size / Table Size               */
    {&Sep} "Fields"        /* Number of table's fields              */
    {&Sep} "Template"      /* Size of template records              */
    {&Sep} "Min Rec Size"  /* Minimal Record Size                   */
    {&Sep} "Max Rec Size"  /* Maximum Record Size                   */
    {&Sep} "Mean Rec Size" /* Mean Record Size                      */
    {&Sep} "Real RPB"      /* Record per Block                      */
    {&Sep} "Create Limit"  /* Create Limit                          */
    {&Sep} "Toss Limit"    /* Toss Limit                            */
    {&Sep} "Data Blocks"   /* Number of data blocks used by table   */
    {&Sep} "RM Chain"      /* Number of blocks on RM chain          */
    {&Sep} "%RM Chain"     /* Persentage of data blocks on RM chain */
    {&Sep} "Scatter Fact"  /* Scatter Factor                        */
    {&Sep} "ABP"           /* Alternative Buffer Pool               */
  SKIP. /* PUT */


  FOR FIRST DbStat NO-LOCK
      WHERE DbStat.StatId EQ ipStatId,
      FIRST DbAlias   OF DbStat   NO-LOCK,
      FIRST DbHost    OF DbAlias  NO-LOCK,
       EACH AreaStat  OF DbStat   NO-LOCK,
       EACH TableStat OF AreaStat NO-LOCK
         BY AreaStat.StatId
         BY AreaStat.AreaNumber /* INDEX AreaNumber */
         BY TableStat.TableName:

    FOR EACH IndexStat NO-LOCK
       WHERE IndexStat.StatId     EQ AreaStat.StatId
         AND IndexStat.TableName  EQ TableStat.TableName /* INDEX IndexName */
         AND IndexStat.TableOwner EQ TableStat.TableOwner:
      ACCUMULATE IndexStat.IndexBlocks (TOTAL).
    END.

    ASSIGN vChainBlocks = ?.
    FOR FIRST ChainStat NO-LOCK /* INDEX ObjectNumber */
        WHERE ChainStat.StatId       EQ TableStat.StatId
          AND ChainStat.AreaNumber   EQ TableStat.AreaNumber
          AND ChainStat.ChainType    EQ "RM Chain":U
          AND ChainStat.ObjectType   EQ "Table":U
          AND ChainStat.ObjectNumber EQ TableStat.TableNumber
          AND ChainStat.ObjectGroup  EQ TableStat.TableGroup:
      ASSIGN vChainBlocks = ChainStat.ChainBlocks.
    END.

    PUT UNFORMATTED
             DbHost.HostName                     /* Db Host       */
      {&Sep} DbAlias.Db_Name                     /* Db Name       */
      {&Sep} ISO-DATE(DbStat.BgnTime)            /* Analysis Date */
      {&Sep} AreaStat.AreaName                   /* Area          */
      {&Sep} AreaStat.AreaNumber                 /* Area #        */
      {&Sep} AreaStat.RecPerBlock                /* RPB           */
      {&Sep} AreaStat.ClusterSize                /* Cluster Size  */
      {&Sep} TableStat.TableNumber               /* Table #       */
      {&Sep} TableStat.TableOwner                /* Owner         */
      {&Sep} TableStat.TableName                 /* Table Name    */
      {&Sep} TableStat.TableGroup                /* Table Group   */
      {&Sep} TableStat.LastChange                /* Last Change   */
      {&Sep} TableStat.RecCount                  /* Records       */
      {&Sep} TableStat.FragCount                 /* Fragments     */
      {&Sep} IF TableStat.RecCount GT 0 THEN     /* Frag%         */
             TableStat.FragCount                 
           / TableStat.RecCount * 100 ELSE 0.0    
      {&Sep} TableStat.TableSize                 /* Table Size    */
      {&Sep} TableStat.SizeInChar                /* Size          */
      {&Sep} TableStat.DataBlocks                /* %HWM"         */
           / AreaStat.HighBlock * 100.0
      {&Sep} TableStat.NumKeys                   /* Indexes       */
      {&Sep} (ACCUM TOTAL IndexStat.IndexBlocks) /* Index Size    */
      {&Sep} (ACCUM TOTAL IndexStat.IndexBlocks) /* Index/Table   */
           / TableStat.TableSize
      {&Sep} TableStat.NumFields                 /* Fields        */
      {&Sep} TableStat.TemplateSize              /* Template      */
      {&Sep} TableStat.MinRecSize                /* Min Rec Size  */
      {&Sep} TableStat.MaxRecSize                /* Max Rec Size  */
      {&Sep} TableStat.MeanRecSize               /* Mean Rec Size */
      {&Sep} TableStat.RecCount                  /* Real RPB      */
           / TableStat.DataBlocks                
      {&Sep} TableStat.CreateLimit               /* Create Limit  */
      {&Sep} TableStat.TossLimit                 /* Toss Limit    */
      {&Sep} TableStat.DataBlocks                /* Data Blocks   */
      {&Sep} vChainBlocks                        /* RM Chain      */
      {&Sep} vChainBlocks                        /* %RM Chain     */
           / TableStat.DataBlocks * 100.0        
      {&Sep} TableStat.ScatterFact               /* Scatter Fact  */
      {&Sep} TableStat.AltBufferPool             /* ABP           */
    SKIP. /* PUT */                                      
  END. /* FOR EACH AreaStat, EACH TableStat OF AreaStat */

  OUTPUT CLOSE.

END PROCEDURE. /* ReportTableStat */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportIndexStat.

  DEFINE INPUT PARAMETER ipStatId    AS INTEGER   NO-UNDO.
/* The prefix for output files: */
  DEFINE INPUT PARAMETER ipOutPrefix AS CHARACTER NO-UNDO.
/* Append output to the existent files: */
  DEFINE INPUT PARAMETER ipAppend    AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE vReportFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vRecords    AS INT64     NO-UNDO.
  DEFINE VARIABLE vTableSize  AS INT64     NO-UNDO.
  DEFINE VARIABLE vFreeBlocks AS INT64     NO-UNDO.
  DEFINE VARIABLE vLockBlocks AS INT64     NO-UNDO.

  IF NOT CAN-FIND(FIRST IndexStat WHERE IndexStat.StatId EQ ipStatId) THEN
  RETURN.

  ASSIGN ipOutPrefix = "dbstat":U WHEN ipOutPrefix EQ ? OR ipOutPrefix EQ "":U
         vReportFile = ipOutPrefix + ".indexes.txt"
  . /* ASSIGN */

  IF ipAppend
  THEN OUTPUT TO VALUE(vReportFile) APPEND.
  ELSE OUTPUT TO VALUE(vReportFile).

  IF SEEK(OUTPUT) EQ 0 THEN
  PUT UNFORMATTED
           "Db Host"       /* Db host                                */
    {&Sep} "Db Name"       /* Db name                                */
    {&Sep} "Analysis Date" /* Date of dbanalys start                 */
    {&Sep} "Area"          /* Area Name                              */
    {&Sep} "Area #"        /* Area Number                            */
    {&Sep} "Cluster Size"  /* Cluster Size                           */
    {&Sep} "Index #"       /* Index Number                           */
    {&Sep} "Owner"         /* Table Owner                            */
    {&Sep} "Table Name"    /* Table Name                             */
    {&Sep} "Index Name"    /* Index Name                             */
    {&Sep} "Index Group"   /* Group                                  */
    {&Sep} "State"         /* Index Attributes                       */
    {&Sep} "Records"       /* Records                                */
    {&Sep} "Table Size"    /* Table Size                             */
    {&Sep} "Index Size"    /* Index Size                             */
    {&Sep} "Index Blocks"  /* Index Blocks                           */
    {&Sep} "%HWM"          /* Percentage of the area size            */
    {&Sep} "%Util"         /* Index Block Utilization                */
    {&Sep} "Keys/Block"    /* Index keys per block                   */
    {&Sep} "Free Chain"    /* Number of blocks on Free chain         */
    {&Sep} "Lock Chain"    /* Number of blocks on Index Delete chain */
    {&Sep} "Index Levels"  /* Index Levels                           */
    {&Sep} "Fields"        /* Number of index's fields               */
    {&Sep} "ABP"           /* Alternative Buffer Pool                */
  SKIP. /* PUT */

  FOR FIRST DbStat NO-LOCK
      WHERE DbStat.StatId EQ ipStatId,
      FIRST DbAlias   OF DbStat   NO-LOCK,
      FIRST DbHost    OF DbAlias  NO-LOCK,
       EACH AreaStat  OF DbStat   NO-LOCK,
       EACH IndexStat OF AreaStat NO-LOCK
        BY AreaStat.StatId
        BY AreaStat.AreaNumber /* INDEX AreaNumber */
        BY IndexStat.TableName
        BY IndexStat.IndexName
        BY IndexStat.IndexGroup:

    ASSIGN vRecords   = ?
           vTableSize = ?
    . /* ASSIGN */
    FOR FIRST TableStat NO-LOCK /* INDEX TableName */
        WHERE TableStat.StatId     EQ IndexStat.StatId
          AND TableStat.TableName  EQ IndexStat.TableName
          AND TableStat.TableGroup EQ IndexStat.IndexGroup
          AND TableStat.TableOwner EQ IndexStat.TableOwner:
      ASSIGN vRecords   = TableStat.RecCount
             vTableSize = TableStat.TableSize
      . /* ASSIGN */
    END.

    ASSIGN vFreeBlocks = ?.
    FOR FIRST ChainStat NO-LOCK /* INDEX ObjectNumber */
        WHERE ChainStat.StatId       EQ IndexStat.StatId
          AND ChainStat.AreaNumber   EQ IndexStat.AreaNumber
          AND ChainStat.ChainType    EQ "Free Chain":U
          AND ChainStat.ObjectType   EQ "Index":U
          AND ChainStat.ObjectNumber EQ IndexStat.IndexNumber
          AND ChainStat.ObjectGroup  EQ IndexStat.IndexGroup:
      ASSIGN vFreeBlocks = ChainStat.ChainBlocks.
    END.

    ASSIGN vLockBlocks = ?.
    FOR FIRST ChainStat NO-LOCK /* INDEX ObjectNumber */
        WHERE ChainStat.StatId       EQ IndexStat.StatId
          AND ChainStat.AreaNumber   EQ IndexStat.AreaNumber
          AND ChainStat.ChainType    EQ "Index Delete Chain":U
          AND ChainStat.ObjectType   EQ "Index":U
          AND ChainStat.ObjectNumber EQ IndexStat.IndexNumber
          AND ChainStat.ObjectGroup  EQ IndexStat.IndexGroup:
      ASSIGN vLockBlocks = ChainStat.ChainBlocks.
    END.

    PUT UNFORMATTED
             DbHost.HostName                  /* Db Host       */
      {&Sep} DbAlias.Db_Name                  /* Db Name       */
      {&Sep} ISO-DATE(DbStat.BgnTime)         /* Analysis Date */
      {&Sep} AreaStat.AreaName                /* Area          */
      {&Sep} AreaStat.AreaNumber              /* Area #        */
      {&Sep} AreaStat.ClusterSize             /* Cluster Size  */
      {&Sep} IndexStat.IndexNumber            /* Index #       */
      {&Sep} IndexStat.TableOwner             /* Owner         */
      {&Sep} IndexStat.TableName              /* Table Name    */
      {&Sep} IndexStat.IndexName              /* Index Name    */
      {&Sep} IndexStat.IndexGroup             /* Index Group   */
      {&Sep} IF IndexStat.ActiveIndex THEN    /* State         */
             IndexStat.IndexAttrib    ELSE "Inactive"
      {&Sep} vRecords                         /* Records       */
      {&Sep} TableSize                        /* Table Size    */
      {&Sep} IndexStat.IndexSize              /* Index Size    */
      {&Sep} IndexStat.IndexBlocks            /* Index Blocks  */
      {&Sep} IndexStat.IndexBlocks            /* %HWM          */
           / AreaStat.HighBlock * 100.0
      {&Sep} IndexStat.IndexUtil              /* %Util         */
      {&Sep} vRecords / IndexStat.IndexBlocks /* Keys/Block    */
      {&Sep} vFreeBlocks                      /* Free Chain    */
      {&Sep} vLockBlocks                      /* Delete Chain  */
      {&Sep} IndexStat.IndexLevels            /* Index Levels  */
      {&Sep} IndexStat.IndexFields            /* Fields        */
      {&Sep} IndexStat.AltBufferPool          /* ABP           */
    SKIP. /* PUT */                                      
  END. /* FOR EACH AreaStat, EACH IndexStat OF AreaStat */

  OUTPUT CLOSE.

END PROCEDURE. /* ReportIndexStat */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportChainStat.

  DEFINE INPUT PARAMETER ipStatId    AS INTEGER   NO-UNDO.
/* The prefix for output files: */
  DEFINE INPUT PARAMETER ipOutPrefix AS CHARACTER NO-UNDO.
/* Append output to the existent files: */
  DEFINE INPUT PARAMETER ipAppend    AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE vReportFile   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vTableOwner   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vTableName    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vObjectName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vObjectGroup  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vObjectBlocks AS INT64     NO-UNDO.

  IF NOT CAN-FIND(FIRST ChainStat WHERE ChainStat.StatId EQ ipStatId) THEN
  RETURN.
  
  ASSIGN ipOutPrefix = "dbstat":U WHEN ipOutPrefix EQ ? OR ipOutPrefix EQ "":U
         vReportFile = ipOutPrefix + ".chains.txt"
  . /* ASSIGN */

  IF ipAppend
  THEN OUTPUT TO VALUE(vReportFile) APPEND.
  ELSE OUTPUT TO VALUE(vReportFile).

  IF SEEK(OUTPUT) EQ 0 THEN
  PUT UNFORMATTED
           "Db Host"       /* Db host                */
    {&Sep} "Db Name"       /* Db name                */
    {&Sep} "Analysis Date" /* Date of dbanalys start */
    {&Sep} "Area"          /* Area Name              */
    {&Sep} "Area #"        /* Area Number            */
    {&Sep} "RPB"           /* Records Per Block      */
    {&Sep} "Cluster Size"  /* Cluster Size           */
    {&Sep} "Chain Type"    /* Chain Type             */
    {&Sep} "Object #"      /* Object Number          */
    {&Sep} "Object Type"   /* Object Type            */
    {&Sep} "Owner"         /* Table Owner            */
    {&Sep} "Table Name"    /* Table Name             */
    {&Sep} "Object Name"   /* Object Name            */
    {&Sep} "Object Group"  /* Group                  */
    {&Sep} "Object Blocks" /* Object Blocks          */
    {&Sep} "Chain Blocks"  /* Chain Blocks           */
    {&Sep} "SeekPath"      /* Seek Path              */
    {&Sep} "MinDbkey"      /* Min Dbkey              */
    {&Sep} "MaxDbkey"      /* Max Dbkey              */
    {&Sep} "Scan Time"     /* Time to scan the chain */

  SKIP. /* PUT */

  FOR FIRST DbStat NO-LOCK
      WHERE DbStat.StatId EQ ipStatId,
      FIRST DbAlias   OF DbStat   NO-LOCK,
      FIRST DbHost    OF DbAlias  NO-LOCK,
       EACH AreaStat  OF DbStat   NO-LOCK,
       EACH ChainStat OF AreaStat NO-LOCK /* INDEX ObjectNumber */
         BY AreaStat.StatId
         BY AreaStat.AreaNumber /* INDEX AreaNumber */
         BY ChainStat.ObjectNumber:

    ASSIGN vTableOwner   = ?
           vTableName    = ?
           vObjectName   = ?
           vObjectBlocks = ?
    . /* ASSIGN */

    CASE ChainStat.ObjectType:
      WHEN "Table":U THEN
      FOR FIRST TableStat NO-LOCK /* INDEX TableNumber */
          WHERE TableStat.StatId      EQ ChainStat.StatId
            AND TableStat.TableNumber EQ ChainStat.ObjectNumber
            AND TableStat.TableGroup  EQ ChainStat.ObjectGroup:
        ASSIGN vTableOwner   = TableStat.TableOwner
               vTableName    = TableStat.TableName
               vObjectName   = "":U
               vObjectBlocks = TableStat.DataBlocks
        . /* ASSIGN */
      END. /* Table */

      WHEN "Index":U THEN /* INDEX IndexNumber */
      FOR FIRST IndexStat NO-LOCK
          WHERE IndexStat.StatId      EQ ChainStat.StatId
            AND IndexStat.IndexNumber EQ ChainStat.ObjectNumber
            AND IndexStat.IndexGroup  EQ ChainStat.ObjectGroup:
        ASSIGN vTableOwner   = IndexStat.TableOwner
               vTableName    = IndexStat.TableName
               vObjectName   = IndexStat.IndexName
               vObjectBlocks = IndexStat.IndexBlocks
        . /* ASSIGN */
      END. /* Index */

      WHEN "Blob":U THEN
      FOR FIRST LobStat NO-LOCK /* INDEX LobNumber */
          WHERE LobStat.StatId    EQ ChainStat.StatId
            AND LobStat.LobNumber EQ ChainStat.ObjectNumber
            AND LobStat.LobGroup  EQ ChainStat.ObjectGroup:
        ASSIGN vTableOwner   = LobStat.TableOwner
               vTableName    = LobStat.TableName
               vObjectName   = LobStat.LobName
               vObjectBlocks = LobStat.DataBlocks
        . /* ASSIGN */
      END. /* Blob */

      WHEN "area":U OR
      WHEN "Master":U THEN
      ASSIGN vTableOwner   = "":U
             vTableName    = "":U
             vObjectName   = "":U
             vObjectBlocks = AreaStat.HighBlock
      . /* ASSIGN */

    END CASE. /* ChainStat.ObjectType */

    PUT UNFORMATTED
             DbHost.HostName          /* Db Host       */
      {&Sep} DbAlias.Db_Name          /* Db Name       */
      {&Sep} ISO-DATE(DbStat.BgnTime) /* Analysis Date */
      {&Sep} AreaStat.AreaName        /* Area          */
      {&Sep} AreaStat.AreaNumber      /* Area #        */
      {&Sep} AreaStat.RecPerBlock     /* RPB           */
      {&Sep} AreaStat.ClusterSize     /* Cluster Size  */
      {&Sep} ChainStat.ChainType      /* Chain Type    */
      {&Sep} ChainStat.ObjectNumber   /* Object #      */
      {&Sep} ChainStat.ObjectType     /* Object Type   */
      {&Sep} vTableOwner              /* Owner         */
      {&Sep} vTableName               /* Table Name    */
      {&Sep} vObjectName              /* Object Name   */
      {&Sep} ChainStat.ObjectGroup    /* Object Group  */
      {&Sep} vObjectBlocks            /* Object Blocks */
      {&Sep} ChainStat.ChainBlocks    /* Chain Blocks  */
      {&Sep} ChainStat.SeekPath       /* SeekPath      */
      {&Sep} ChainStat.MinDbkey       /* MinDbkey      */
      {&Sep} ChainStat.MaxDbkey       /* MaxDbkey      */
      {&Sep} ChainStat.RunInterval    /* Scan Time     */
    SKIP. /* PUT */                                      
  END. /* FOR EACH AreaStat, EACH ChainStat OF AreaStat */

  OUTPUT CLOSE.

END PROCEDURE. /* ReportChainStat */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportLobStat.

  DEFINE INPUT PARAMETER ipStatId    AS INTEGER   NO-UNDO.
/* The prefix for output files: */
  DEFINE INPUT PARAMETER ipOutPrefix AS CHARACTER NO-UNDO.
/* Append output to the existent files: */
  DEFINE INPUT PARAMETER ipAppend    AS LOGICAL   NO-UNDO.

  DEFINE VARIABLE vReportFile   AS CHARACTER NO-UNDO.
  
  IF NOT CAN-FIND(FIRST LobStat WHERE LobStat.StatId EQ ipStatId) THEN
  RETURN.
  
  ASSIGN ipOutPrefix = "dbstat":U WHEN ipOutPrefix EQ ? OR ipOutPrefix EQ "":U
         vReportFile = ipOutPrefix + ".lobs.txt"
  . /* ASSIGN */

  IF ipAppend
  THEN OUTPUT TO VALUE(vReportFile) APPEND.
  ELSE OUTPUT TO VALUE(vReportFile).

  IF SEEK(OUTPUT) EQ 0 THEN
  PUT UNFORMATTED
           "Db Host"       /* Db host                 */
    {&Sep} "Db Name"       /* Db name                 */
    {&Sep} "Analysis Date" /* Date of dbanalys start  */
    {&Sep} "Area"         /* Area Name                */
    {&Sep} "Area #"       /* Area Number              */
    {&Sep} "RPB"          /* Records Per Block        */
    {&Sep} "Cluster Size" /* Cluster Size             */
    {&Sep} "Owner"        /* Table Owner              */
    {&Sep} "Table"        /* Table Name               */
    {&Sep} "Field"        /* Field Name               */
    {&Sep} "LOB #"        /* LOB Id                   */
    {&Sep} "Type"         /* LOB Type                 */
    {&Sep} "Records"      /* Record Count             */
    {&Sep} "Blocks"       /* LOB Blocks               */
    {&Sep} "%HWM"         /* LOB Blocks to area's HWM */
    {&Sep} "RM Chain"     /* Blocks on RM Chain       */
    {&Sep} "Mean Size"    /* Field Size per Field     */
  SKIP. /* PUT */

  OUTPUT CLOSE.

END PROCEDURE. /* ReportLobStat */

/* ------------------------------------------------------------------------ */

