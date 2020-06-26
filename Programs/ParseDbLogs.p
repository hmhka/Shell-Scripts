RUN ParseDbLogs(
/*     UsrList */ "",
/*     UsrTime */ "[2013/07/19@11:10",
/*   UsrDbName */ "bank",
/*   GroupList */ "!Ignore,*",
/*    FileMask */ "*~~~.lg",
/* MaxDirLevel */ ?,
/*  LogRootDir */ "/path/to/dir_with_db.lg"
).

/* ------------------------------------------------------------------------
    File        : ParseDbLogs.p
    Purpose     : Parse the messages from database logs.
    Syntax      : See the examples above.
    Description : The output files can be opened in Excel.

    Author(s)   : George Potemkin
    Created     : July 13, 2013
    Modified    : July 18, 2013
V1.0 as of July 18, 2013: the program is now compatible with V10.1
V1.1 as of July 19, 2013: beta version
V1.2 as of July 21, 2013: 
1. Optimized the usage of ttGroupMask;
2. New UsrNum() function;
3. New output file (ParseDbLogs.Statitics.txt) for debugging purposes;
4. Minor changes of group definitions.
------------------------------------------------------------------------ */

/* The messages imported from db logs: */
DEFINE TEMP-TABLE ttMsg NO-UNDO
  FIELD GroupId AS INTEGER
  FIELD DbId    AS INTEGER
  FIELD MsgTime AS CHARACTER
  FIELD MsgPID  AS CHARACTER
  FIELD MsgTID  AS CHARACTER
  FIELD MsgType AS CHARACTER
  FIELD SrvType AS CHARACTER
  FIELD SrvNum  AS CHARACTER
  FIELD MsgNum  AS CHARACTER
  FIELD MsgText AS CHARACTER
  FIELD UsrNum  AS CHARACTER /*the usernum the message text is talking about*/
  FIELD MsgLine AS INTEGER
  INDEX UniqueIdx IS UNIQUE
        GroupId
        MsgTime
        MsgLine
        DbId
  INDEX Group0Idx /* IS UNIQUE */
        GroupId
        DbId   
        UsrNum
        MsgTime
        MsgLine
  INDEX PrevMsgIdx
        GroupId
        DbId   
        SrvType
        SrvNum 
. /* DEFINE TEMP-TABLE ttMsg */

/* The databases whose logs were processed: */
DEFINE TEMP-TABLE ttDatabase NO-UNDO
  FIELD DbId AS INTEGER
  FIELD DbNm AS CHARACTER
  INDEX DbId IS UNIQUE
        DbId
  INDEX DbNm IS UNIQUE
        DbNm
. /* DEFINE TEMP-TABLE ttDatabase */

/* The groups of the messages: */
DEFINE TEMP-TABLE ttGroup NO-UNDO
  FIELD GroupId     AS INTEGER   FORMAT "->9"
  FIELD GroupName   AS CHARACTER FORMAT "x(16)"
  FIELD StatGroup   AS LOGICAL   FORMAT "Stat/List" INITIAL FALSE
  FIELD ActiveGroup AS LOGICAL   FORMAT "Active/Inactive" 
  FIELD MsgCount    AS INTEGER /* the number of messages defined in group */
  FIELD LogCount    AS INTEGER /* the number of messages found in db logs */
  FIELD MskCount    AS INTEGER /* the number of times the masks were used */
  FIELD TimeSize    AS INTEGER   /* only for Stat groups */
  FIELD MaxCount    AS INTEGER   /* only for Stat groups */
  FIELD UsrTime     AS CHARACTER /* only for Sessions group */
  FIELD UsrDbId     AS INTEGER   /* only for Sessions group */
  INDEX GroupId     IS UNIQUE
        GroupId
  INDEX StatGroup
        StatGroup
  INDEX GroupName
        GroupName
. /* DEFINE TEMP-TABLE ttGroup */

/* The group masks: */
DEFINE TEMP-TABLE ttGroupMask NO-UNDO
  FIELD GroupId  AS INTEGER
  FIELD Accept   AS LOGICAL FORMAT "Accept/Reject"
  FIELD Mask     AS CHARACTER
  FIELD MskCount AS INTEGER /* the number of times the mask returned TRUE */
  INDEX Accept
        GroupId
        Accept
. /* DEFINE TEMP-TABLE ttGroupMask */

/* The messages that below to a group: */
DEFINE TEMP-TABLE ttGroupMsg NO-UNDO
  FIELD GroupId AS INTEGER
  FIELD MsgNum  AS CHARACTER
  FIELD MsgText AS CHARACTER /* used only for the messages of Stat group */
  FIELD MsgCol  AS INTEGER   /* used only for the messages of Stat group */
  INDEX MsgNum  IS UNIQUE
        GroupId
        MsgNum
  INDEX MsgText
        GroupId
        MsgText
. /* DEFINE TEMP-TABLE ttGroupMsg */

&SCOPED-DEFINE MaxSize 256

/* The statistics of the messages in stat groups: */
DEFINE TEMP-TABLE ttMsgStat NO-UNDO
  FIELD GroupId  AS INTEGER
  FIELD DbId     AS INTEGER
  FIELD MsgDate  AS CHARACTER
  FIELD MsgTime  AS CHARACTER
  FIELD TotCount AS INTEGER
  FIELD MsgCount AS INTEGER EXTENT {&MaxSize}  INITIAL [0]
  FIELD UserList AS CHARACTER
  INDEX Idx4Report IS UNIQUE
        GroupId
        MsgDate
        MsgTime
        DbId
. /* DEFINE TEMP-TABLE ttMsgStat */

DEFINE TEMP-TABLE ttOSFile NO-UNDO
  FIELD osFileName AS CHARACTER
  FIELD osFilePath AS CHARACTER
  FIELD osFileAttr AS CHARACTER
  FIELD osFileSize AS INTEGER
  FIELD osFileTime AS INTEGER
  FIELD osFileLine AS INTEGER
  FIELD DbId       AS INTEGER
  INDEX osFileName
        osFileName
. /* DEFINE TEMP-TABLE ttOSFile */

DEFINE VARIABLE vFrameTitle AS CHARACTER NO-UNDO INITIAL " ParseDbLogs: ".

FORM vFile AS CHARACTER FORMAT "x(41)" LABEL "File"
     vSize AS CHARACTER FORMAT "x(10)" LABEL "Size"
     vTime AS CHARACTER FORMAT "x(12)" LABEL "Processed"
WITH FRAME ProgressFrame TITLE vFrameTitle 16 DOWN.

/* ------------------------------------------------------------------------- */

PROCEDURE ParseDbLogs.

  DEFINE INPUT PARAMETER ipUsrList  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipUsrTime  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipUsrDbNm  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipGrpList  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFileMask AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMaxDirLevel AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipInputDir AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vDbId AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vDbNm AS CHARACTER NO-UNDO.

  ASSIGN FILE-INFO:FILE-NAME = ipInputDir.
  
  IF FILE-INFO:FULL-PATHNAME EQ ? THEN
  DO:
    MESSAGE "Input directory does not exist!" SKIP
             ipInputDir 
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN ERROR.
  END.

  RUN GetOSFiles(INPUT ipInputDir, INPUT ipFileMask, INPUT ipMaxDirLevel).

  ASSIGN vDbId = 0.
  FOR EACH ttOSFile EXCLUSIVE
        BY ttOSFile.osFileName
  TRANSACTION:
  
    ASSIGN FILE-INFO:FILE-NAME = ttOSFile.osFilePath.
    IF FILE-INFO:FILE-SIZE EQ 0 THEN
    DO:
      DELETE ttOSFile.
      NEXT.
    END.

    ASSIGN ttOSFile.osFileSize = FILE-INFO:FILE-SIZE.

    ASSIGN vDbNm = ENTRY(1, ttOSFile.osFileName, ".":U).

    FIND FIRST ttDatabase NO-LOCK
         WHERE ttDatabase.DbNm EQ vDbNm
    NO-ERROR.

    IF AVAILABLE ttDatabase THEN
    ASSIGN ttOSFile.DbId = vDbId.
    ELSE
    DO:
      CREATE ttDatabase.
      ASSIGN ttDatabase.DbNm = vDbNm
             ttDatabase.DbId = vDbId
             ttOSFile.DbId   = vDbId
             vDbId           = vDbId + 1
      . /* ASSIGN */
    END.
  END. /* FOR EACH ttOSFile */

  IF vDbId EQ 0 THEN
  DO:
    MESSAGE SUBSTITUTE("There are no &1 files in the input directory:",
              /* &1 */ ipFileMask) SKIP
             ipInputDir 
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN ERROR.
  END.

  ASSIGN vDbId = ?.
  FOR FIRST ttDatabase NO-LOCK
      WHERE ttDatabase.DbNm EQ ipUsrDbNm:
    ASSIGN vDbId = ttDatabase.DbId.
  END.

  IF ipUsrList NE "":U AND ipUsrList NE ? THEN
  IF vDbId EQ ? THEN
  DO:
    MESSAGE
      SUBSTITUTE("Log of &1 db was not found in the input directory:",
       /* &1 */  ipUsrDbNm) SKIP
      ipInputDir             SKIP
      SUBSTITUTE("User list (&1) will be ignored.",
       /* &1 */  ipUsrList) SKIP
      "Would you like to process anyway?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    UPDATE vChoice AS LOGICAL.

    IF vChoice EQ TRUE
    THEN ASSIGN ipUsrList = "":U.
    ELSE RETURN ERROR.
  END.

  IF ipUsrList NE "":U
  OR ipGrpList NE "":U THEN
  RUN CreateGroups(
        /* UsrList */ ipUsrList,
        /* UsrTime */ ipUsrTime,
        /* UsrDbId */ vDbId,
        /* GrpList */ ipGrpList).
  ELSE
  DO:
    MESSAGE "User list and group list are empty." SKIP
      "There are nothing to process"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.

  IF ipUsrList NE "":U THEN
  ASSIGN vFrameTitle = SUBSTITUTE(" ParseDbLogs: &1 in &2 ",
                         /* &1 */ ipUsrList,
                         /* &2 */ ipUsrDbNm)
  . /* ASSIGN */

  FOR EACH ttOSFile NO-LOCK
        BY ttOSFile.osFileName:
    RUN ImportMsgs(ttOSFile.osFilePath, ttOSFile.DbId).
  END. /* FOR EACH ttOSFile */

  RUN ReportMsgs(ipInputDir /* dir for output files*/).

END PROCEDURE. /* ParseDbLogs */

/* ------------------------------------------------------------------------- */

PROCEDURE CreateGroups:

 DEFINE INPUT PARAMETER ipUsrList AS CHARACTER NO-UNDO.
 DEFINE INPUT PARAMETER ipUsrTime AS CHARACTER NO-UNDO.
 DEFINE INPUT PARAMETER ipUsrDbId AS INTEGER   NO-UNDO.
 DEFINE INPUT PARAMETER ipGrpList AS CHARACTER NO-UNDO.
 /*ipGrpList: "Startups,Warnings,Logins,Network,AfterImage,Ignore" */

/* Tips for creating the message lists:
1) The unknown value (?) terminates the list. All elements of an array
   since the element with the unknown value will be ignored.
2) The negative and zero values will be skipped from the list.
   If you need temporarily to exclude a message from a list then
   just add the minus sing to its number.
*/
&SCOPED-DEFINE MaxSize 256

DEFINE VARIABLE vEmptyList AS INTEGER NO-UNDO EXTENT {&MaxSize} INITIAL [?].

DEFINE VARIABLE vIgnoreList AS INTEGER NO-UNDO EXTENT {&MaxSize} INITIAL [
451,   /*<session-type> session begin for <user> on <ttyxxx>.             */
708,   /*Userid is now <name>.                                            */
1015,  /*Database <name> was disconnected.                                */
1423,  /*There is no server for database <dbname>.                        */
2251,  /*Destroyed user  pid .                                            */
2520,  /*Stopped.                                                         */
3694,  /*SIGTERM received.                                                */
5512,  /*Previous message sent on behalf of user <user number>.           */
5646,  /*Started on port <port> using <network-type>, pid <pid>.          */
7129,  /*Usr <User> set name to <Name>.                                   */
10429, /*The user failed to connect to database <database> with error <error> in <functionname>.*/
10717, /*The Fathom Replication Utility cannot connect to database <name>.*/
10996, /*Database statistics (<statistic type>) updated for table <name>. */
14658, /*Previous message sent on behalf of user <user number>, server pid <process id>, broker pid <prodess id>. (5512)*/
12699, /*Database <name> Options: <name>                                  */
?]. /* vIgnoreList */
 
DEFINE VARIABLE vLoginsList AS INTEGER NO-UNDO EXTENT {&MaxSize} INITIAL [
452,   /*Login by <user> on <ttyxxx>.                        */
742,   /*Login usernum <num>, userid <name>, on <tty/node>   */
8873,  /*Login usernum , remote SQL client.                  */
453,   /*Logout by <user> on <ttyxxx>.                       */
739,   /*Logout usernum <num>, userid <name>, on <tty/node>. */
562,   /*HANGUP signal received.                             */
298,   /*KILL signal received.                               */
2252,  /*Begin transaction backout                           */
2253,  /*Transaction backout completed.                      */
794,   /*Usernum <num> terminated abnormally.                */
1166,  /* Server disconnecting user <num>.                   */
?].    /* vLoginsList */

DEFINE VARIABLE vNetworkList AS INTEGER NO-UNDO EXTENT {&MaxSize} INITIAL [
796,   /*Error writing msg, socket=<n> errno=<n> usernum=<n> disconnected.*/
2266,  /*Expected user <user> received user <usernum>.                    */
12454, /*Server <server-num> has <num> unresolved pending connections(s). Please check port <port-number>.*/
12455, /*Clearing pending connections from server <server-number>.        */
2268, /*Invalid message was received for an attempted login.              */
792,  /*Message received with bad usernum.                                */
4006, /*SYSTEM ERROR: Got message <msgcode> for disconnected user <num>   */
/*(-----) TCP/IP write error occurred with errno 32                       */
?].    /* vNetworkList */

DEFINE VARIABLE vAfterImageList AS INTEGER NO-UNDO EXTENT {&MaxSize} INITIAL [
/* RFUTIL: */
3774,  /*Backup after-image extent and mark it as empty.                 */
3776,  /*Backup ai extent and mark it as empty.                          */
3777,  /*Switched to ai extent <anme>                                    */
3778,  /*This is after-image file number <n> since the last AIMAGE BEGIN */
3789,  /*Marked after-image extent EMPTY.                                */
13154, /*Marked after-image extent  ARCHIVED.                            */
11805, /*Unlocking after-image file <n> and locking ALL FULL after-image files beginning with file <n>.*/
13199, /*After-image extent <name> has been copied to <name>.            */
13231, /*From this point forward all after-image extents will be archived to <name>.*/
?]. /* vAfterImageList */

DEFINE VARIABLE vBackupList AS INTEGER NO-UNDO EXTENT {&MaxSize} INITIAL [
/* BACKUP: */
1074,  /*Wrote a total of <blocks> backup blocks using <bytes> bytes of media.*/
1361,  /*Incremental backup started.                                       */
1362,  /*Full backup started.                                              */
1363,  /*Incremental backup successfully completed.                        */
1364,  /*Full backup successfully completed.                               */
1365,  /*Database copied from <database-name>.                             */
1366,  /*Incremental restore of sequence <number> started.                 */
1367,  /*Incremental restore of sequence <number> completed.               */
1368,  /*Full restore started.                                             */
1369,  /*Full restore completed.                                           */
3664,  /*Incremental backup started.                                       */
3665,  /*<num> bi blocks will be dumped.                                   */
3666,  /*<num> out of <num> blocks in <database-name> will be dumped.      */
3667,  /*This will require <num> bytes of backup media.                    */
3668,  /*This backup will require a minimum of <number> blocks to restore. */
3739,  /*Backup estimate complete.                                         */
3740,  /*Backup complete.                                                  */
3751,  /*Verify pass started.                                              */
3752,  /*Full verify pass started.                                         */
3758,  /*Full verify successful.                                           */
3762,  /*Using device/file <device-name>.                                  */
3763,  /*Started restoring volume <number>.                                */
5431,  /*Wrote a total of <blocks> backup blocks using <Mb> megabytes of media.*/
5432,  /*This will require <num> megabytes of backup media.                */
5459,  /*Begin backup of Before Image file(s).                             */
5460,  /*End backup of Before Image file(s).                               */
5461,  /*Begin backup of Data file(s).                                     */
5462,  /*End backup of Data file(s).                                       */
6678,  /*NOTE: Backup media estimates done without database scan.          */
6680,  /*Estimate for uncompressed full backup is provided.                */
6686,  /*<num> active blocks out of <num> blocks in <name> will be dumped. */
6688,  /*<num> BI blocks will be dumped.                                   */
6689,  /*Backup requires  blocks ( Mb) of media.                           */
6699,   /*Restore would require  blocks ( Mb) of media.                    */
6758,  /*Backup for <name> verified ok.                                    */
6759,  /*This is a  backup of .                                            */
6760,  /*This backup was taken <time>.                                     */
6761,  /*It is based on the full backup of .                               */
6762,  /*It is sequence <sequence number>, based on the incremental of . */
6763, /*It will require a minimum of <number> blocks to restore.           */
9284,  /*Wrote a total of <blocks> backup blocks using <size+identifier> of media.*/
9285,  /*Backup requires an estimated <size+identifier> of media.          */
9286,  /*Restore would require an estimated  db blocks using <size+identifier> of media.*/
12850, /*Backup blocks will be written to <device/file>.                   */
13268, /*Database backup method to use during transition (backup-method) : <name>.*/
13625, /*Wrote a total of <blocks> backup blocks using <size+identifier> of media.*/
13786, /*This backup was also used to enable after-image on the source database. */
14256, /*The Replication Server has been notified that an online backup is about to be performed.*/
14258, /*The online backup of this database has completed normally.        */
15108, /*The online backup has finished backing up the BI.  The Replication Agent will now begin recovery synchronization followed by normal processing.*/
15681, /*Restore is extending Storage Area <areanum> by <neededblocks> blocks */
?]. /* vBackupList */

DEFINE VARIABLE vStartupList AS INTEGER NO-UNDO EXTENT {&MaxSize} INITIAL [
/* BROKER  0: ----------------------------------------------------- */
333,  /*Multi-user session begin.                                   */
4234, /*Progress OpenEdge Release <VERSION_NUMBER> on <OS_PLATFORM>.*/
4235, /*Physical Database Name (-db): <full-path/database-name>.    */
4236, /*Database Type (-dt): PROGRESS.                              */
4237, /*Force Access (-F): <Enabled/Not Enabled>.                   */
4238, /*Direct I/O (-directio): <Enabled/Not Enabled/Not Supported>.*/
/*-----, Number of LRU force skips (-lruskips): 0                   */
4239, /*Number of Database Buffers (-B): <num>.                     */
4240, /*Excess Shared Memory Size (-Mxs): <num>.                    */
4241, /*Current Size of Lock Table (-L): <#>.                       */
4242, /*Hash Table Entries (-hash): <num>.                          */
4243, /*Current Spin Lock Tries (-spin): <num>.                     */
4244, /*Crash Recovery (-i): <Enabled/Disabled>.                    */
4245, /*Delay of Before-Image Flush (-Mf): <num>.                   */
4246, /*Before-Image File Name (-g): <full-path/bi-file-name>       */
4247, /*Before-Image File I/O (-r -R): <Reliable/Not Reliable>.     */
4249, /*Before-Image Truncate Interval (-G): <num>.                 */
4250, /*Before-Image Cluster Size: <num>.                           */
4251, /*Before-Image Block Size: <num>.                             */
4252, /*Number of Before-Image Buffers (-bibufs): <num>.            */
4253, /*After-Image File Name (-a): <full-path/after-image-filename>*/
4254, /*After-Image Stall (-aistall): <Enabled/Not Enabled>.        */
4255, /*After-Image Block Size: <num>.                              */
4256, /*Number of After-Image Buffers (-aibufs): <num>.             */
4257, /*Maximum Number of Clients Per Server (-Ma): <num>.          */
4258, /*Maximum Number of Servers (-Mn): <num>.                     */
4259, /*Minimum Clients Per Server (-Mi): <num>.                    */
/*-----, Delay first prefetch message (-prefetchDelay): Disabled    */
/*-----, Minimum records in prefetch ms (-prefetchNumRecs): <num>   */
4260,  /*Maximum Number of Users (-n): <num>.                       */
4261,  /*Host Name (-H): <database-server-host>.                    */
4262,  /*Service Name (-S): <service-name>.                         */
4263,  /*Network Type (-N): TCP.                                    */
4264,  /*Character Set (-cpinternal): <name>.                       */
4265,  /*Stream (-cpstream): <name>.                                */
4281,  /*Server started by <user-id> on <tty-name>.                 */
4282,  /*Parameter File: <parameter-file-name>.                     */
5326,  /*Begin Physical Redo Phase at <current address> .           */
5327,  /*Physical Redo Phase Completed at <lastredo address>, <note count> changes redone.*/
5328,  /*Begin Logical Undo Phase,  <number of live transactions> incomplete transactions.*/
5329,  /*Logical Undo Phase Complete.                               */
5330,  /*Begin Physical Undo Phase,  <number of live transactions> transactions at  <bi address>.*/
5331,  /*Physical Undo Phase Completed at  <bi address>.            */
5644,  /*Started for <service-name> using <network-type>, pid <pid>.*/
5645,  /*This is an additional broker for this protocol.            */
5646,  /*Started on port <port> using <network-type>, pid <pid>.    */
5647,  /*Maximum Servers Per Broker (-Mpb): <num>.                  */
5648,  /*Minimum Port for Auto Servers (-minport): <num>.           */
5649,  /*Maximum Port for Auto Servers (-maxport): <num>.           */
6526,  /*Number of Semaphore Sets (-semsets): .                     */
6551,  /*BI File Threshold Stall (-bistall): Enabled.               */
6552,  /*BI File Threshold Stall (-bistall): Disabled.              */
6573,  /*Database Blocksize (-blocksize): <num>.                    */
6574,  /*Started using pid: <num>.                                  */
7161,  /*Physical Redo Phase Completed at blk <blk> off <off> upd <upd>.*/
7162,  /*Begin Logical Undo Phase, <number> incomplete transactions are being backed out.*/
7163,  /*Begin Physical Undo  transactions at block  offset         */
8527,  /*Storage object cache size (-omsize):                       */
8836, /*Connecting to Admin Server on port <port>.                  */
9238,  /*BI File Threshold size (-bithold): <size+identifier>.      */
9336,  /*Created shared memory with segment_id: <number>            */
9422,  /*Maximum private buffers per user (-Bpmax): <num>.          */
9426,  /*Large database file access has been enabled.               */
10013, /*The shared memory segment is locked in memory.             */
10014, /*The shared memory segment is not locked in memory.         */
10357, /*Pending client connection timeout (-PendConnTimeout): <num>*/
10392, /*Database <name> is being replicated from database <name> on host <name>.*/
10471, /*Database connections have been enabled.                    */
10489, /*Fathom Replication Agent <name> is beginning Startup Synchronization at block <number>*/
10490, /*Fathom Replication Agent <name> is beginning normal processing at block <number>.*/
10501, /*The Fathom Replication Agent has been successfully started as PID <pid>.*/
10535, /*Enhanced Read-Only mode (-ERO): Enabled                    */
10545, /*Connections to this database will not be allowed until all Database Services started have completed their startup and initialisation. */
10668, /*The Source and Target databases are synchronized.          */
11231, /*Logical Undo Phase begin at Block <blknum> Offset <offset>.*/
12080, /*Begin Replication Redo Phase for <num> live transactions from <current address . */
12081, /*Replication Redo Phase completed at block , offset .       */
12095, /*Logical Undo Phase completed at block <blknum>, offset <offset>.*/
12423, /*Physical Redo Aging set to <ageTime>.                      */
12458, /*Maximum Number of Clients (-n): <num>.                     */
12812, /*BIW writer delay (-bwdelay): <num>                         */
12813, /*Allowed index cursors (-c): <num>.                         */
12814, /*Group delay (-groupdelay): <num>.                          */
12815, /*Lock table hash table size (-lkhash): <num>                */
12816, /*Maxport (-maxport): <num>                                  */
12817, /*Minport (-minport): <num>                                  */
12818, /*Message Buffer Size (-Mm): <num>                           */
12819, /*Servers per Protocol (-Mp): <num>                          */
12820, /*Maximum Servers per Broker (-Mpb): <num>                   */
12821, /*Use muxlatches (-mux): <num>                               */
12822, /*Read Only (-RO): <num>                                     */
12823, /*Semaphore Sets (-semsets): <num>                           */
13547, /*At end of Physical redo, transaction table size is 16384.  */
13869, /*Database Service Manager - Service(s) to start (-DBService) : <names>*/
13870, /*Database Service Manager - IPC Queue Size (-pica) : <value>*/
13871, /*After-image Management Archival Method : <name>.           */
13872, /*After-image Management Archival Interval (-aiarcinterval) : <num>. */
13873, /*After-image Management Archival Directory List (-aiarcdir) : <name>.*/
13874, /*Create After-image Management Archival Directory(s) (-aiarcdircreate) : <yes/no>.*/
13875, /*This database is enabled for OpenEdge Replication as a <source/target> database.*/
13896, /*TXE Commit lock skip limit (-TXESkipLimit): <num>.         */
13924, /*Maximum Shared Memory Segment Size (-shmsegsize) <num> Mb. */
13953, /*Maximum Area Number (-maxArea): <num>.                     */
14268, /*TCP/IP Version (-ipver) : <IPVersion>                      */

15218, /*Encryption cache size (-ecsize): <num>                     */
15219, /*Encryption enabled:                                        */
15321, /*Before Image Log Initialization at block <biblk> offset <bioffset>.*/
15824, /* enabled:                                                  */
/* Login Brokers: ------------------------------------------------- */
5644,  /*Started for <service-name> using <network-type>, pid <pid>.*/ 
5645,  /*This is an additional broker for this protocol.            */
8863,  /*This broker supports 4GL server groups only.               */
8864,  /*This broker supports SQL server groups only.               */
8865,  /*This broker supports both 4GL and SQL server groups.       */
/* Shutdown: ------------------------------------------------------ */
334,   /*<Multi/Single> session end.                                */
542,   /*Server shutdown started by root on batch.                  */
2248,  /*Begin normal shutdown                                      */
2249,  /*Begin ABNORMAL shutdown code                               */
2250,  /*Database killed by <user> on <tty-name>                    */
2261, /*Sending signal <signal number> to <num> connected user(s).  */
2263,  /*Resending shutdown request to <n> user(s).                 */
5316,  /*Emergency shutdown initiated...                            */
10485, /*The Fathom Replication Server is shutting down.            */
10486, /*Shutdown of the Fathom Replication Server is complete.     */
15109, /*At Database close the number of live transactions is <n>.  */
15193, /*The normal shutdown of the database will continue for 10 Min 0 Sec if required. */
15194, /*Database activity did not finish before the shutdown timeout expired so the database is performing an immediate shutdown.*/
15743, /*Before Image Log Completion at Block <n> Offset <n>.       */
/*-----  Removed shared memory with segment_id: 4194323             */
/* BIW, AIW, APW, WDOG: ------------------------------------------- */
2518,  /*Started.                                                   */
2519,  /*Disconnected.                                              */
/* AIMGT: --------------------------------------------------------- */
13194,	/*The after-image manager is beginning.                     */
/* RPLS: ---------------------------------------------------------- */
10326, /*<sectionname> Properties                                   */
10327, /*Database Name (database): <dbname>                         */
10328, /*Listener Maximum Port Number (listener-maxport): <num>.    */
10329, /*Listener Minimum Port Number (listener-minport): <num>.    */
10330, /*Transition Method (transition): manual, synchronous or asynchronous.*/
10331, /*Transition Timeout (transition-timeout): <num>.            */
10332, /*Control Agent (control-agent) : <name>.                    */
10333, /*Host Name (host): <nnme>.                                  */
10334, /*Port (port): <name>.                                       */
10335, /*Critical (critical): <num>.                                */
10336, /*Replication Method (replication-method): <name>.           */
10337, /*Maximum Message Length (maximum-message): <num>            */
10338, /*Agent Name (name) : <name>.                                */
10500, /*The Fathom Replication Server successfully started as PID <pid>.*/
10661, /*The Fathom Replication Server is beginning recovery for agent <agent-name>.*/
10842, /*Connecting to Fathom Replication Agent <agentname>.        */
11715, /*Minimum Polling Delay (minumum-polling-delay): <value>.    */
11716, /*Maximum Polling Delay (maximum-polling-delay): <value>.    */
11717, /*Defer Agent Startup (defer-agent-startup): <num>.          */
11718, /*Defer Agent Startup (defer-agent-startup): Not Active.     */
12685, /*Agent Shutdown Action (agent-shutdown-action) : <name>.    */
13261, /*Database-role (database-role) : <name>.                    */
13262, /*Responsibility (responsibility) : <name>.                  */
13263, /*Restart database after transition (restart-after-transition) : <name>.*/
13264, /*<name> startup arguments (<name>) : <arguments>.           */
13265, /*Automatically begin after-imaging during transition (auto-begin-ai) : <name>.*/
13266, /*Automatically add after-image areas during transition (auto-add-ai-areas) : <name>.*/
13267, /*Structure file that contains the after-image area definitions to automatically add (ai-structure-file) : <name>.*/
13268, /*Database backup method to use during transition (backup-method) : <name>.*/
13269, /*<name> arguments (<name>) : <name>.                        */
13270, /*The <name> property is required when the backup-method property is <name>.*/
14249, /*TCP/IP Version (ipver): <ipv4|ipv6>.                       */
10507, /*The Fathom Replication Server has successfully connected to the Fathom Replication Agent <agent-name> on host <hostname>.*/
11251, /*The Replication Server successfully connected to all of it's configured Agents.*/
10436, /*The source database <databasename> and the target database <databasename> on host <hostname> are synchronized.*/
10508, /*Beginning Fathom Replication synchronization for the Fathom Replication Agent <agent-name>.*/
10436, /*The source database <databasename> and the target database <databasename> on host <hostname> are synchronized.*/
10698, /*The Fathom Replication Server will shutdown but the source database will remain active.*/
10699, /*The Fathom Replication Agent agent1 is requesting to be terminated.*/
10700, /*The Fathom Replication Agent agent1 is being terminated.   */
10700, /*The Fathom Replication Agent agent1 is being terminated.   */
10702, /*All Fathom Replication Agents have been terminated or have ended.*/
10505, /*The Fathom Replication Server is ending.                   */
10715, /*Connect Timeout (connect-timeout): <num>.                  */
10819, /*The Fathom Replication property file is being processed.   */
12231, /*Replication Keep Alive (repl-keep-alive): <num>.           */
12233, /*Schema Lock Action (schema-lock-action) : <name>.          */
/* RPLA: ---------------------------------------------------------- */
12688, /*The Replication Server has been terminated or the Source database has been shutdown.  The Agents will enter PRE-TRANSITION, waiting for re-connection from the Replication Server.*/
10482, /*The Fathom Replication Agent is shutting down.*/
10506, /*The Fathom Replication Agent agent1 is ending.*/
10483, /*Shutdown of the Fathom Replication Agent is complete.*/
10501, /*The Fathom Replication Agent has been successfully started as PID 62783760.*/
/* FMAGEN: -------------------------------------------------------- */
14262, /*Successfully connected to AdminServer on port <PORT> using TCP/IP <IPVersion> address <IPAddr>*/
8846,  /*Registered with Admin Server.                              */
?]. /* vStartupList */

DEFINE VARIABLE vWarningList AS INTEGER NO-UNDO EXTENT {&MaxSize} INITIAL [
5407,  /*WARNING: -nb exceeded. Automatically increasing from <old> to <new>.*/
5408,  /*WARNING: -l exceeded. Automatically increasing from <old> to <new>. */
5409,  /*WARNING: -mmax exceeded. Automatically increasing from <old> to <new>.*/
5410,  /*WARNING: -D limit has been exceeded; automatically increasing to    */
/* Minus ("-") means to skip temporarily a number from the list: */
-10840, /*WARNING: Width of data is greater than <database>.<table>.<field> (rowid <rowid>) _width.*/
-13435, /*bkxtn: WARNING: Area: <Number> extent <Name> has reached the <percentage> percent threshold for block usage - current block hiwater <maxAreaBlock>.*/
?]. /* vWarningList */

/* Reject/Accept masks are the comma separated lists of masks that will
   be applied to the messages without error number: (-----).
   The masks with empty or unknown values will be ignored.
*/
  RUN CreateListGroup("Ignore",
    /*     MsgList */ vIgnoreList,
    /* RejectMasks */ "*error*,*warning*":U, /* just in case */
    /* AcceptMasks */ "Sending signal *,PROMO*,Disconnected due to shutdown of primary database:*").
  
  RUN CreateListGroup("StartupList",
    /*     MsgList */ vStartupList,
    /* RejectMasks */ "Sending signal *":U,
    /* AcceptMasks */ "BROKER,RPLS,* enabled*":U).

  RUN CreateListGroup("AfterImageList",
    /*     MsgList */ vAfterImageList,
    /* RejectMasks */ "":U,
    /* AcceptMasks */ "":U).
  
  RUN CreateListGroup("BackupList",
    /*     MsgList */ vBackupList,
    /* RejectMasks */ "":U,
    /* AcceptMasks */ "":U).
  
  RUN CreateListGroup("SQLSrvList",
    /*     MsgList */ vEmptyList,
    /* RejectMasks */ "":U,
    /* AcceptMasks */ "SQLSRV":U).
  
  RUN CreateListGroup("WarningList",
    /*     MsgList */ vWarningList,
    /* RejectMasks */ "":U,
    /* AcceptMasks */ "*warning*":U).

/* TimeSize - first digits of time to be used as the stat interval. 
   TimeSize = 5 => 12:45[:58] => the stat interval is a minute.
   MaxCount - the total only for first MaxCount messages in the list.
   MaxCount = ? => All columns contribute to the total.
   MaxCount = 0 => Don't create the "Total" column.
   MaxCount > 0 => Only first MaxCount columns contribute to the total.
*/
  RUN CreateStatGroup("WarningStat",
    /*     MsgList */ vWarningList,
    /* RejectMasks */ "":U,
    /* AcceptMasks */ "*warning*":U,
    /*    TimeSize */ 5,
    /*    MaxCount */ ?).

  RUN CreateStatGroup("LoginsStat",
    /*     MsgList */ vLoginsList,
    /* RejectMasks */ "":U,
    /* AcceptMasks */ "Received RECONNECT from WTB,Login by *~~~.":U, /*PROMO1013: (-----) Login by root.*/
    /*    TimeSize */ 5,
    /*    MaxCount */ 5).
  
  RUN CreateStatGroup("NetworkStat",
    /*   MsgList   */ vNetworkList,
    /* RejectMasks */ "":U,
    /* AcceptMasks */ "TCP/IP write error occurred with errno *":U,
    /*    TimeSize */ 5,
    /*    MaxCount */ 3).

/* Sessions Group is the messages from sessions specified by user list */
 DO TRANSACTION:
   CREATE ttGroup.
   ASSIGN ttGroup.GroupId     = 0
          ttGroup.GroupName   = "Sessions"
          ttGroup.ActiveGroup = FALSE
          ttGroup.UsrTime     = IF ipUsrTime BEGINS "[":U
                                THEN          ipUsrTime
                                ELSE ("[":U + ipUsrTime)
          ttGroup.UsrDbId     = ipUsrDbId
          ttGroup.LogCount    = 0
   . /* ASSIGN */.

   IF ipUsrList NE "":U THEN
   DO:
     CREATE ttGroupMask.
     ASSIGN ttGroupMask.GroupId = ttGroup.GroupId
            ttGroupMask.Accept  = TRUE
            ttGroupMask.Mask    = ipUsrList
            ttGroup.ActiveGroup = TRUE
     . /* ASSIGN */.
   END.
 END.


/* Servers Group - the last messages of the servers.
   It will be used to link UserNum to the messaged reported by SRV.
*/
  DO TRANSACTION:
    CREATE ttGroup.
    ASSIGN ttGroup.GroupId   = -1
           ttGroup.GroupName = "Servers"
           ttGroup.LogCount  = 0
    . /* ASSIGN */        
  END.

/* Errors Group - the messages that don't below to the groups above. */
  DO TRANSACTION:
    CREATE ttGroup.
    ASSIGN ttGroup.GroupId   = ?
           ttGroup.GroupName = "Errors"
           ttGroup.LogCount  = 0
    . /* ASSIGN */        
  END.

/* Ignore the groups that are not on the ipGrpList */
  FOR EACH ttGroup
     WHERE ttGroup.GroupId GT 0
       AND ttGroup.GroupId NE ?
  TRANSACTION:
    ASSIGN ttGroup.ActiveGroup = CAN-DO(ipGrpList, ttGroup.GroupName).
  END.

END PROCEDURE. /* CreateGroups */

/* ------------------------------------------------------------------------- */

FUNCTION MsgText RETURNS CHARACTER(ipMsgNum AS INTEGER).
  DEFINE VARIABLE vMsgNum  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMsgText AS CHARACTER NO-UNDO.

  ASSIGN vMsgText = SEARCH("prohelp/msgdata/msg1")
         vMsgText = SUBSTRING(vMsgText, 1, LENGTH(vMsgText) - 1)
         vMsgNum  = TRUNCATE((ipMsgNum - 1) / 50, 0) + 1
         vMsgText = vMsgText + STRING(vMsgNum)
         FILE-INFO:FILE-NAME = vMsgText
  . /* ASSIGN */
  
/* If MsgNum is too high for current Progress version: */
  IF FILE-INFO:FULL-PATHNAME EQ ? THEN
  RETURN "(" + STRING(ipMsgNum) + ")".

  INPUT FROM VALUE(FILE-INFO:FULL-PATHNAME).
  REPEAT:
    ASSIGN vMsgNum  = ?
           vMsgText = ?.
    IMPORT vMsgNum vMsgText ^.
    IF vMsgNum EQ ipMsgNum THEN
    LEAVE.
  END.
  INPUT CLOSE.
  RETURN vMsgText.
END FUNCTION. /* MsgText */

/* ------------------------------------------------------------------------- */

PROCEDURE CreateListGroup:

  DEFINE INPUT PARAMETER ipGroupName   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMsgList     AS INTEGER   NO-UNDO EXTENT {&MaxSize}.
  DEFINE INPUT PARAMETER ipRejectMasks AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipAcceptMasks AS CHARACTER NO-UNDO.

/* Reject/Accept masks are the comma separated lists of masks that will be
   applied to the messages without error number: (-----).
   The masks with empty or unknown values will be ignored.
*/
  DEFINE VARIABLE vGroupId  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMsgMask  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vMsgNum   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vMsgCount AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i         AS INTEGER   NO-UNDO.

  ASSIGN vGroupId = 1.
  FOR EACH ttGroup NO-LOCK
        BY ttGroup.GroupId DESCENDING:
    ASSIGN vGroupId = ttGroup.GroupId + 1.
    LEAVE.
  END.

/* ipAcceptMasks */
  REPEAT i = NUM-ENTRIES(ipAcceptMasks) TO 1 BY -1
  TRANSACTION:

    ASSIGN vMsgMask = TRIM(ENTRY(i, ipAcceptMasks)).
    IF vMsgMask EQ "":U THEN
    NEXT.

    CREATE ttGroupMask.
    ASSIGN ttGroupMask.GroupId = vGroupId
           ttGroupMask.Accept  = TRUE
           ttGroupMask.Mask    = vMsgMask
    . /* ASSIGN */
  END.

/* ipRejectMasks*/
  REPEAT i = NUM-ENTRIES(ipRejectMasks) TO 1 BY -1
  TRANSACTION:

    ASSIGN vMsgMask = TRIM(ENTRY(i, ipRejectMasks)).
    IF vMsgMask EQ "":U THEN
    NEXT.

    CREATE ttGroupMask.
    ASSIGN ttGroupMask.GroupId = vGroupId
           ttGroupMask.Accept  = FALSE
           ttGroupMask.Mask    = vMsgMask
    . /* ASSIGN */
  END.

/* ttGroupMsg */
  ASSIGN vMsgCount = 0.
  REPEAT i = 1 TO {&MaxSize}
  WHILE ipMsgList[i] NE ?      /* the unknown value terminates the list */
  TRANSACTION:
    IF  ipMsgList[i] LE 0 THEN /* non-positive values are skipped from list */
    NEXT.
      
    ASSIGN vMsgNum = STRING(ipMsgList[i]).

    IF CAN-FIND(ttGroupMsg
          WHERE ttGroupMsg.GroupId EQ vGroupId
            AND ttGroupMsg.MsgNum  EQ vMsgNum) THEN
    NEXT.

    CREATE ttGroupMsg.
    ASSIGN ttGroupMsg.GroupId = vGroupId
           ttGroupMsg.MsgNum  = vMsgNum
           ttGroupMsg.MsgCol  = i
           vMsgCount = vMsgCount + 1
    . /* ASSIGN */
/* In V10.1B:
 An attempt was made to reference the user-defined function or method
 'MsgText' in an ASSIGN type statement after an assignment to key field
  'ttGroupMsg.MsgNum'. (7955)
*/  ASSIGN ttGroupMsg.MsgText = MsgText(ipMsgList[i]).
  END. /* REPEAT i = 1 TO {&MaxSize} */

  DO TRANSACTION:
    CREATE ttGroup.
    ASSIGN ttGroup.GroupId     = vGroupId
           ttGroup.GroupName   = ipGroupName
           ttGroup.StatGroup   = FALSE
           ttGroup.ActiveGroup = FALSE
           ttGroup.MsgCount    = vMsgCount
           ttGroup.LogCount    = 0
    . /* ASSIGN */
  END. /* DO TRANSACTION */
END PROCEDURE. /* CreateListGroup */

/* ------------------------------------------------------------------------- */

PROCEDURE CreateStatGroup:

  DEFINE INPUT PARAMETER ipGroupName   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMsgList     AS INTEGER   NO-UNDO EXTENT {&MaxSize}.
  DEFINE INPUT PARAMETER ipRejectMasks AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipAcceptMasks AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipTimeSize    AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipMaxCount    AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vGroupId AS INTEGER NO-UNDO.
  DEFINE VARIABLE vMsgNum  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i        AS INTEGER NO-UNDO.


  RUN CreateListGroup(ipGroupName,
                     ipMsgList,
                     ipRejectMasks,
                     ipAcceptMasks).

  FOR FIRST ttGroup
      WHERE ttGroup.GroupName EQ ipGroupName
  TRANSACTION:
    ASSIGN ttGroup.StatGroup = TRUE            /*123456789012+TimeSize*/
           ttGroup.TimeSize  = ipTimeSize + 12 /*[2013/07/08@14:31:21.*/
           ttGroup.MaxCount  = ipMaxCount
           vGroupId = ttGroup.GroupId
    . /* ASSIGN */
  END. /* DO TRANSACTION */
END PROCEDURE. /* CreateStatGroup */

/* ------------------------------------------------------------------------- */

FUNCTION GroupAccept RETURNS LOGICAL (
  INPUT ipGroupId AS INTEGER,
  INPUT ipValue   AS CHARACTER).

/* Check the reject masks first, then the explicit accepts: */
  FOR EACH ttGroupMask
     WHERE ttGroupMask.GroupId EQ ipGroupId
        BY ttGroupMask.Accept: 

    IF NOT CAN-DO(ttGroupMask.Mask, ipValue) THEN
    NEXT.

    ASSIGN ttGroupMask.MskCount = ttGroupMask.MskCount + 1.
    RETURN ttGroupMask.Accept.
  END.
   
  RETURN FALSE.
END FUNCTION. /* GroupAccept */

/* ------------------------------------------------------------------------- */

PROCEDURE CopyUsrToPrevMsg.
  DEFINE INPUT PARAMETER ipDbId   AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipSrvNum AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipUsrNum AS CHARACTER NO-UNDO.

  DEFINE BUFFER bPrevMsg FOR ttMsg.
  DEFINE VARIABLE vInGroup0 AS LOGICAL NO-UNDO.
/*
For any messages followed by 5512 or 14658:
(14658) Previous message sent on behalf of user <user number>, server pid <process id>, broker pid <prodess id>. (5512)

For TCP/IP and socket errors followed by 796:
(796)   Error writing msg, socket=<n> errno=<n> usernum=<n> disconnected.
*/
/* Find prev message of the same server: */
  FOR FIRST bPrevMsg EXCLUSIVE /* index PrevMsgIdx */
      WHERE bPrevMsg.GroupId EQ -1
        AND bPrevMsg.DbId    EQ ipDbId
        AND bPrevMsg.SrvType EQ "SRV":U
        AND bPrevMsg.SrvNum  EQ ipSrvNum
  TRANSACTION:

/* Sanity check: if UsrNum was already updated? */
    IF bPrevMsg.SrvNum NE bPrevMsg.UsrNum THEN
    RETURN.

    ASSIGN bPrevMsg.UsrNum = ipUsrNum
           vInGroup0 = FALSE.

/* Update all copies of the message: */
    FOR EACH ttGroup NO-LOCK
       WHERE ttGroup.GroupId GE 0,
    
       FIRST ttMsg EXCLUSIVE /* index UniqueIdx */
       WHERE ttMsg.GroupId EQ ttGroup.GroupId
         AND ttMsg.DbId    EQ bPrevMsg.DbId
         AND ttMsg.MsgTime EQ bPrevMsg.MsgTime
         AND ttMsg.MsgLine EQ bPrevMsg.MsgLine:
      ASSIGN ttMsg.UsrNum = ipUsrNum
             vInGroup0 = TRUE WHEN ttGroup.GroupId EQ 0.
    END.

/* If new ipUsrNum is in UsrList: */
    IF NOT vInGroup0 THEN
    FOR FIRST ttGroup NO-LOCK
        WHERE ttGroup.GroupId EQ 0
          AND ttGroup.UsrDbId EQ ipDbId
        WHILE GroupAccept(ttGroup.GroupId, ipUsrNum):
   
      CREATE ttMsg.
      ASSIGN ttMsg.GroupId = ttGroup.GroupId
             ttMsg.DbId    = bPrevMsg.DbId   
             ttMsg.MsgTime = bPrevMsg.MsgTime
             ttMsg.MsgPID  = bPrevMsg.MsgPID 
             ttMsg.MsgTID  = bPrevMsg.MsgTID 
             ttMsg.MsgType = bPrevMsg.MsgType
             ttMsg.SrvType = bPrevMsg.SrvType
             ttMsg.SrvNum  = bPrevMsg.SrvNum 
             ttMsg.MsgNum  = bPrevMsg.MsgNum 
             ttMsg.MsgText = bPrevMsg.MsgText
             ttMsg.UsrNum  = bPrevMsg.UsrNum 
             ttMsg.MsgLine = bPrevMsg.MsgLine
             ttGroup.LogCount = ttGroup.LogCount + 1
      . /* ASSIGN */
    END. /* FOR FIRST ttGroup WHERE GroupId EQ 0 */
  END. /* FOR FIRST bPrevMsg WHERE GroupId EQ -1 */
END PROCEDURE. /* CopyUsrToPrevMsg */

/* ------------------------------------------------------------------------- */

FUNCTION CopyUsrFromPrevMsg RETURNS CHARACTER (
  ipDbId   AS INTEGER,
  ipSrvNum AS CHARACTER).
/*
For messages:
(2252)  Begin transaction backout.
(2253)  Transaction backout completed.
*/
/* Find prev message of the same server: */
  FOR FIRST ttMsg NO-LOCK /* index PrevMsgIdx */
      WHERE ttMsg.GroupId EQ -1
        AND ttMsg.DbId    EQ ipDbId
        AND ttMsg.SrvType EQ "SRV":U
        AND ttMsg.SrvNum  EQ ipSrvNum:
    RETURN ttMsg.UsrNum.
  END.

  RETURN ipSrvNum.
END FUNCTION. /* CopyUsrFromPrevMsg */

/* ------------------------------------------------------------------------- */

PROCEDURE CheckMsg.

  DEFINE INPUT PARAMETER ipDbId    AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipMsgTime AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMsgPID  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMsgTID  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMsgType AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipSrvType AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipSrvNum  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMsgNum  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMsgText AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipUsrNum  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMsgLine AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vUserId     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vIndex      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMsgMask    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vGroupFound AS LOGICAL   NO-UNDO.

/* Store the most recent server's messages (Type = "SRV")
   as the special group (GroupId -1).
   These records will be used to identify UsrNum.
   See CopyUsrToPrevMsg and CopyUsrFromPrevMsg.
*/
  IF ipSrvType EQ "SRV":U THEN
  FOR FIRST ttGroup
      WHERE ttGroup.GroupId EQ -1
  TRANSACTION:
    FIND FIRST ttMsg EXCLUSIVE /* index PrevMsgIdx */
         WHERE ttMsg.GroupId EQ ttGroup.GroupId
           AND ttMsg.DbId    EQ ipDbId
           AND ttMsg.SrvType EQ ipSrvType
           AND ttMsg.SrvNum  EQ ipSrvNum
    NO-ERROR.
    IF NOT AVAILABLE ttMsg THEN
    CREATE ttMsg.
    ASSIGN ttMsg.GroupId = -1
           ttMsg.DbId    = ipDbId   
           ttMsg.MsgTime = ipMsgTime
           ttMsg.MsgPID  = ipMsgPID 
           ttMsg.MsgTID  = ipMsgTID 
           ttMsg.MsgType = ipMsgType
           ttMsg.SrvType = ipSrvType
           ttMsg.SrvNum  = ipSrvNum 
           ttMsg.MsgNum  = ipMsgNum 
           ttMsg.MsgText = ipMsgText
           ttMsg.UsrNum  = ipUsrNum 
           ttMsg.MsgLine = ipMsgLine
           ttGroup.LogCount = ttGroup.LogCount + 1 WHEN NEW ttMsg
    . /* ASSIGN */
  END.

/* Sessions Group: */
Group0:
  FOR FIRST ttGroup NO-LOCK
      WHERE ttGroup.GroupId     EQ 0
        AND ttGroup.ActiveGroup EQ TRUE
        AND ttGroup.UsrDbId     EQ ipDbId
         
      WHILE GroupAccept(ttGroup.GroupId, ipUsrNum):

/* Try to identify UserName (a.k.a. Userid): */
    ASSIGN vUserId  = ?.
    CASE ipMsgNum:
/*(452)   Login by <user> on <ttyxxx>.  */
/*(453)   Logout by <user> on <ttyxxx>. */
      WHEN "452" OR WHEN "453" THEN
        ASSIGN vIndex  = NUM-ENTRIES(ipMsgText, " ":U)
               vUserId = ENTRY(vIndex - 2, ipMsgText, " ":U)
        . /* ASSIGN */

/*(742)   Login usernum <num>, userid <name>, on <tty/node>   */
/*(739)   Logout usernum <num>, userid <name>, on <tty/node>. */
      WHEN "742" OR WHEN "739" THEN
        ASSIGN vIndex  = INDEX(ipMsgText + " userid ":U, " userid ":U)
               vUserId = TRIM(SUBSTRING(ipMsgText, vIndex + 8))
               vUserId = ENTRY(1, ipMsgText, " ":U)
               vUserId = TRIM(vUserId, ",":U)
        . /* ASSIGN */

/*(708)   Userid is now <name>. */
      WHEN "708" THEN
        ASSIGN vIndex  = NUM-ENTRIES(ipMsgText, " ":U)
               vUserId = ENTRY(vIndex, ipMsgText, " ":U)
               vUserId = TRIM(vUserId, ".":U)
        . /* ASSIGN */
    END CASE. /* ipMsgNum */

/* If Userid matches UsrList then add ipUsrNum to UsrList: */
    IF  vUserId NE ?    /*If it's the messages above and not */ 
    AND vUserId NE "":U /*the failure of the messages parsing*/ THEN
    IF GroupAccept(ttGroup.GroupId, vUserId) THEN
    FOR FIRST ttGroupMask EXCLUSIVE
        WHERE ttGroupMask.GroupId EQ ttGroup.GroupId
          AND ttGroupMask.Accept  EQ TRUE
    TRANSACTION:
      ASSIGN  ttGroupMask.Mask = ttGroupMask.Mask + "," + ipUsrNum.
    END.

/* If it's not a session we are looking for then nothing else to do: */
    IF  NOT GroupAccept(ttGroup.GroupId, ipSrvNum)
    AND NOT GroupAccept(ttGroup.GroupId, ipUsrNum) THEN
    LEAVE Group0.

/* Session with one of UsrNum we are looking for is found!       */
/* Check timeframe of the user's session compared to UsrTime:   */
/*(452)   Login by <user> on <ttyxxx>.                           */
/*(742)   Login usernum <num>, userid <name>, on <tty/node>      */
/*(8873)  Login usernum , remote SQL client.                     */
/*(5646)  Started on port <port> using <network-type>, pid <pid>.*/
    IF CAN-DO("452,742,8873,5646":U, ipMsgNum) THEN
    IF  ttGroup.UsrTime NE ?
    AND ttGroup.UsrTime NE "":U THEN
/* Previous session ended before UsrTime: */
    IF ttGroup.UsrTime GE ipMsgTime THEN 
    FOR EACH ttMsg EXCLUSIVE /* Group0Idx */
       WHERE ttMsg.GroupId EQ ttGroup.GroupId
         AND ttMsg.DbId    EQ ttGroup.UsrDbId
         AND ttMsg.UsrNum  EQ ipUsrNum
    TRANSACTION:
      DELETE ttMsg.
    END.
    ELSE
/* New session begun after UsrTime: */
    FOR FIRST ttGroupMask EXCLUSIVE
        WHERE ttGroupMask.GroupId EQ ttGroup.GroupId
          AND ttGroupMask.Accept  EQ TRUE
    TRANSACTION: /* Reject ipUsrNum */
      ASSIGN  ttGroupMask.Mask = "!" + ipUsrNum + "," + ttGroupMask.Mask.
      LEAVE Group0.
    END.

/* Add the message to the "sessions" group: */
    DO TRANSACTION:
      CREATE ttMsg.
      ASSIGN ttMsg.GroupId = ttGroup.GroupId
             ttMsg.DbId    = ipDbId
             ttMsg.MsgTime = ipMsgTime
             ttMsg.MsgPID  = ipMsgPID 
             ttMsg.MsgTID  = ipMsgTID 
             ttMsg.MsgType = ipMsgType
             ttMsg.SrvNum  = ipSrvNum 
             ttMsg.SrvType = ipSrvType
             ttMsg.MsgNum  = ipMsgNum 
             ttMsg.MsgText = ipMsgText
             ttMsg.UsrNum  = ipUsrNum
             ttMsg.MsgLine = ipMsgLine
             ttGroup.LogCount = ttGroup.LogCount + 1
      . /* ASSIGN */
    END.
  END. /* FOR FIRST ttGroup WHERE GroupId EQ 0 */

  ASSIGN vGroupFound = FALSE.

/* ttGroup WHERE GroupId GT 0 */
  FOR EACH ttGroup NO-LOCK
     WHERE ttGroup.GroupId GT 0
       AND ttGroup.GroupId NE ?:

/*  IF ipMsgNum EQ "-----" */
    IF TRIM(ipMsgNum, "-":U) EQ "":U THEN
    DO:

      ASSIGN ttGroup.MskCount = ttGroup.MskCount + 1.

      IF GroupAccept(ttGroup.GroupId, ipMsgText)
      OR GroupAccept(ttGroup.GroupId, ipSrvType) THEN
      DO:
        ASSIGN vGroupFound = TRUE
               ttGroup.LogCount = ttGroup.LogCount + 1
        . /* ASSIGN */

        IF NOT ttGroup.ActiveGroup THEN
        NEXT.
        
        IF ttGroup.StatGroup THEN
        RUN CreateMsgStat(ttGroup.GroupId,
                          ipDbId,
                          SUBSTRING(ipMsgTime, 1, ttGroup.TimeSize),
                          ipSrvType,
                          ipSrvNum,
                          ipUsrNum,
                          ipMsgNum,
                          ipMsgText).
        ELSE
        DO TRANSACTION:
          CREATE ttMsg.
          ASSIGN ttMsg.GroupId = ttGroup.GroupId
                 ttMsg.DbId    = ipDbId
                 ttMsg.MsgTime = ipMsgTime
                 ttMsg.MsgPID  = ipMsgPID 
                 ttMsg.MsgTID  = ipMsgTID 
                 ttMsg.MsgType = ipMsgType
                 ttMsg.SrvNum  = ipSrvNum 
                 ttMsg.SrvType = ipSrvType
                 ttMsg.MsgNum  = ipMsgNum 
                 ttMsg.MsgText = ipMsgText
                 ttMsg.UsrNum  = ipUsrNum
                 ttMsg.MsgLine = ipMsgLine
          . /* ASSIGN */
        END.  /* DO TRANSACTION */
      END. /* IF GroupAccept */
    END. /* IF ipMsgNum EQ "-----" */
    ELSE
/* IF ipMsgNum NE "-----" */
/* Find the groups the message number belongs to: */
    FOR FIRST ttGroupMsg NO-LOCK
        WHERE ttGroupMsg.GroupId EQ ttGroup.GroupId
          AND ttGroupMsg.MsgNum  EQ ipMsgNum:

      ASSIGN vGroupFound = TRUE
             ttGroup.LogCount = ttGroup.LogCount + 1
      . /* ASSIGN */

      IF NOT ttGroup.ActiveGroup THEN
      NEXT.

      IF ttGroup.StatGroup THEN
      RUN CreateMsgStat(ttGroup.GroupId,
                        ipDbId,
                        SUBSTRING(ipMsgTime, 1, ttGroup.TimeSize),
                        ipSrvType,
                        ipSrvNum,
                        ipUsrNum,
                        ipMsgNum,
                        ipMsgText).
      ELSE
      DO TRANSACTION:
        CREATE ttMsg.
        ASSIGN ttMsg.GroupId = ttGroup.GroupId
               ttMsg.DbId    = ipDbId
               ttMsg.MsgTime = ipMsgTime
               ttMsg.MsgPID  = ipMsgPID 
               ttMsg.MsgTID  = ipMsgTID 
               ttMsg.MsgType = ipMsgType
               ttMsg.SrvNum  = ipSrvNum 
               ttMsg.SrvType = ipSrvType
               ttMsg.MsgNum  = ipMsgNum 
               ttMsg.MsgText = ipMsgText
               ttMsg.UsrNum  = ipUsrNum
               ttMsg.MsgLine = ipMsgLine
        . /* ASSIGN */
      END. /* DO TRANSACTION */
    END. /* FOR FIRST ttGroupMsg */
  END. /* FOR EACH ttGroup WHERE GroupId GT 0 */

/* Message was not found in any group: */
  IF NOT vGroupFound THEN
  FOR FIRST ttGroup
      WHERE ttGroup.GroupId EQ ?
  TRANSACTION:
    CREATE ttMsg.
    ASSIGN ttMsg.GroupId = ttGroup.GroupId 
           ttMsg.DbId    = ipDbId
           ttMsg.MsgTime = ipMsgTime
           ttMsg.MsgPID  = ipMsgPID 
           ttMsg.MsgTID  = ipMsgTID 
           ttMsg.MsgType = ipMsgType
           ttMsg.SrvNum  = ipSrvNum 
           ttMsg.SrvType = ipSrvType
           ttMsg.MsgNum  = ipMsgNum 
           ttMsg.MsgText = ipMsgText
           ttMsg.UsrNum  = ipUsrNum
           ttMsg.MsgLine = ipMsgLine
           ttGroup.LogCount = ttGroup.LogCount + 1
    . /* ASSIGN */
  END. /* DO TRANSACTION */

END PROCEDURE. /* CheckMsg */

/* ------------------------------------------------------------------------- */

PROCEDURE CreateMsgStat:

  DEFINE INPUT PARAMETER ipGroupId AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipDbId    AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipMsgTime AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipSrvType AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipSrvNum  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipUsrNum  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMsgNum  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMsgText AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vDate   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vTime   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vUser   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vServ   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vColumn AS INTEGER     NO-UNDO.

  ASSIGN vDate = SUBSTRING(ipMsgTime, 2, 10)
         vTime = SUBSTRING(ipMsgTime, 13)
         vServ = ipSrvType + ipSrvNum
         vUser = vServ
         vUser = vServ  + "/" + ipUsrNum WHEN ipSrvNum NE ipUsrNum
         vColumn = ?
  . /* ASSIGN */

  FOR FIRST ttGroup
      WHERE ttGroup.GroupId EQ ipGroupId:

/* If message does not have a number then seach by MsgText: */
/*  IF ipMsgNum EQ "-----" */
    IF TRIM(ipMsgNum, "-":U) EQ "":U THEN
    DO:
      FIND FIRST ttGroupMsg NO-LOCK
           WHERE ttGroupMsg.GroupId EQ ipGroupId
             AND ttGroupMsg.MsgText EQ ipMsgText
      NO-ERROR.

      IF NOT AVAILABLE ttGroupMsg THEN
      DO TRANSACTION:
/* The maximum number of the columns is already reached: */
        IF ttGroup.MsgCount EQ EXTENT(ttMsgStat.MsgCount) THEN
        RETURN.

/* Create new column: */
        CREATE ttGroupMsg.
        ASSIGN ttGroup.MsgCount   = ttGroup.MsgCount + 1
               ttGroupMsg.GroupId = ipGroupId
               ttGroupMsg.MsgNum  = ?
               ttGroupMsg.MsgText = ipMsgText
               ttGroupMsg.MsgCol  = ttGroup.MsgCount
        . /* ASSIGN */       
      END. /* DO TRANSACTION */

      ASSIGN vColumn = ttGroupMsg.MsgCol.
    END. /*  IF ipMsgNum EQ "-----" */
    ELSE
/* If message has a number then seach by MsgNum: */
    FOR FIRST ttGroupMsg NO-LOCK
        WHERE ttGroupMsg.GroupId EQ ipGroupId
          AND ttGroupMsg.MsgNum  EQ ipMsgNum:

      ASSIGN vColumn = ttGroupMsg.MsgCol.

    END. /* FOR FIRST ttGroupMsg */

/* For vColumn do: */
    DO TRANSACTION:
      FIND FIRST ttMsgStat
           WHERE ttMsgStat.GroupId EQ ipGroupId
             AND ttMsgStat.DbId    EQ ipDbId
             AND ttMsgStat.MsgDate EQ vDate
             AND ttMsgStat.MsgTime EQ vTime
      NO-ERROR.
      IF NOT AVAILABLE ttMsgStat THEN
      CREATE ttMsgStat.
      ASSIGN ttMsgStat.GroupId  = ipGroupId
             ttMsgStat.DbId     = ipDbId
             ttMsgStat.MsgDate  = vDate
             ttMsgStat.MsgTime  = vTime
             ttMsgStat.TotCount = ttMsgStat.TotCount + 1
                                  WHEN ttGroup.MaxCount EQ ?
                                    OR ttGroup.MaxCount GE vColumn
             ttMsgStat.MsgCount[vColumn] = ttMsgStat.MsgCount[vColumn] + 1
      . /* ASSIGN */
      IF ttMsgStat.UserList BEGINS "???":U /*The list is marked as "too long"*/
      OR CAN-DO(ttMsgStat.UserList, vUser) THEN
      RETURN.

/* If server is already in the list then replace it by user: */
      ASSIGN vColumn = INDEX(ttMsgStat.UserList + ",":U, vServ + ",":U).
      IF vColumn EQ 0 THEN
      ASSIGN ttMsgStat.UserList = ttMsgStat.UserList + ",":U + vUser.
      ELSE SUBSTRING (ttMsgStat.UserList, vColumn, LENGTH(vServ)) = vUser.

/* Mark the list as "too long": */
      ASSIGN ttMsgStat.UserList = "???":U + ttMsgStat.UserList
                                WHEN LENGTH(ttMsgStat.UserList) GT 10000
      . /* ASSIGN */        
    END. /* DO TRANSACTION */
  END. /* FOR FIRST ttGroup WHERE GroupId EQ ipGroupId */
END PROCEDURE. /* CreateMsgStat */

/* ------------------------------------------------------------------------- */

/* ------------------------------------------------------------------------- */

FUNCTION UsrNum RETURNS CHARACTER (
  INPUT ipMsgNum  AS CHARACTER,
  INPUT ipMsgText AS CHARACTER).
/* Search for usernum in the message text: */

/* The list of messages where UsrNum is the first integer item: */
  DEFINE VARIABLE vMsgList1 AS CHARACTER NO-UNDO INITIAL
"5512,14658,742,739,8873,6796,12092,741,1166,2525,2526,2527,2522,5026,2523,~
 5027,12454,12455,2028,2266,2267,2351,2705,2709,2901,3710,3723,3724,3727,~
 4504,4514,4550,5134,5135,5136,5137,5140,5141,6799,12472,13173,13214,13492,~
 14399,14400,14829,15093,15110,15767,15932,16451"
  . /* vMsgList1 */
/*
5512  Previous message sent on behalf of user <user number>. (5512)
14658 Previous message sent on behalf of user <user number>, server pid <process id>, broker pid <prodess id>. (5512) (14658)
742   Login usernum <num>, userid <name>, on <tty/node>. (742)
739   Logout usernum <num>, userid <name>, on <tty/node>. (739)
8873  Login usernum <num>, remote SQL client. (8873)
6796  User <user number> disconnect initiated. (6796)
12092 User <user number> disconnect initiated. (12092)
741   Disconnecting user <number>. (741)
1166  Server disconnecting user <num>. (1166)
2525  Disconnecting dead server <number>. (2525)
2526  Disconnecting client <number> of dead server <number>. (2526)
2527  Disconnecting dead user <number>. (2527)
2522  User <num> died holding <num> shared memory locks. (2522)
5026  User <num> died holding <num> shared memory locks. (5026)
2523  User <num> died with <num> buffers locked. (2523)
5027  User <num> died with <num> buffers locked. (5027)
12454 Server <server-num> has <num> unresolved pending connections(s). Please check port <port-number>. (12454)
12455 Clearing pending connections from server <server-number>. (12455)
2028  Could not log out user <user-num> name <user-name> on <device>. (2028)
2266  Expected user <user> received user <usernum>. (2266)
2267  Invalid message was received for user <user>. (2267)
2351  DosPeek failed on usr <number>, error = <number>. (2351)
2705  SYSTEM ERROR: User <user-num> is queued in two queues. (2705)
2709  SYSTEM ERROR: User <user-num> already has a suspended message. (2709)
2901  Server disconnecting user <num>: client cannot handle the database word-break rules (2901)
3710  SYSTEM ERROR: bad latch wake usr <num> latch <latch-num> waiting for <lock-num>. (3710)
3723  mtusrwake: wrong queue, usr <num>, queue <num>, waiting for <lock>. (3723)
3724  mtusrwake: user <num> already waked, queue <num> wake <num>. (3724)
3727  Can not resync transaction of user <num> on tty <terminal>. (3727)
4504  Timeout waiting login user <num>. (4504)
4514  pnsur bad usernum <num>, msg's nssnum <num>, msgcode <code>. (4514)
4550  pnusr bas usernum <n>, msg nssnum <n>, msgcode <code> (4550)
5134  SYSTEM ERROR: cxgetcur: user <user number> trying to use a free cursor (5134)
5135  SYSTEM ERROR: cxgetcur: user <user number> trying to use user <user number>'s cursor (5135)
5136  SYSTEM ERROR: cxgetcur: user <user number> trying to use cursor for wrong db (5136)
5137  SYSTEM ERROR: gauxcurs: cursor error for user <user number> (5137)
5140  SYSTEM ERROR: cxdlc: user <user number> freeing a released cursor (5140)
5141  SYSTEM ERROR: cxdlc: Attempt by user <user number> to free user <user number>'s cursor (5141)
6799  User <num> not logged on . (6799)
12472 Rejecting login - server <svr num>(<server pid>) has reached its maximum number of <max clients> client connections. (12472)
13173 SYSTEM ERROR: Usr <usrnum> died holding biadd latch. (13173)
13214 SYSTEM ERROR: User <usrnum> died holding the AI Management latch. (13214)
13492 SYSTEM ERROR: User <usrnum> died holding the Database Service Manager latch. (13492)
14399 Login usernum <num>, federated SQL Agent (14399)
14400 Login usernum <num>, federated SQL Client (14400)
14829 SYSTEM ERROR: An invalid key length was encountered from user <NUM> at <NUM> in <filename>, msgcode=, rowid=, fileno=, msglen=. (14829)
15093 SYSTEM ERROR: This server has too many open cursors so the cursor creation attempt by user <NUM> at line <NUM> in <NUM>, msgcode <NUM>, ROWID <NUM>, table <num>, index <NUM>.   (15093)
15110 User <num>, process <num> is not responding to the shutdown request. (15110)
15767 SYSTEM ERROR: User <usrnum> died holding the <latch-name> latch. (15767)
15932 Unable to allocate shared memory for this user's database request statement cache.  Statement caching for user <NUM> is being deactivated. (15932)
16451 Disconnecting user <NUM> of tenant <NUM>. (16451)
*/

/* The list of messages where UsrNum is not the first integer item: */
  DEFINE VARIABLE vMsgList2 AS CHARACTER NO-UNDO INITIAL
"796,797,1007,1162,1280,1949,1951,4006,12093,12608,12692"
  . /* vMsgList2 */
/*
796   Error writing msg, socket=<n> errno=<n> usernum=<n> disconnected. (796)
797   Partial write, socket=<n> msglen=<n> ret=<n> user <n> disconnected. (797)
1007  SYSTEM ERROR: unexpected wakeup from wait code <number>, user <user-number> (1007)
1162  Error writing message, errno=<num> usernum=<num> disconnected. (1162)
1280  Connection timed out on socket=<n> for usernum <n>, attempt disconnect. (1280)
1949  Send error <num> usr <usrnum> conid <conid>. (1949)
1951  Receive error <num> usr <usrnum> state <state>. (1951)
4006  SYSTEM ERROR: Got message <msgcode> for disconnected user <user-num> (4006)
12093 Notifying server <server>, connection <connection> to terminate remote user <user> (12093)
12608 SYSTEM ERROR: lkIntentLockGet found table <table-num> lock not in txn <trid> list usr <usrnum> (12608)
12692 SYSTEM ERROR: lktend missing table <table num> lock in user <usernum> or XAtask <JTA tran id> list (12692)
*/
/* Patterns to search for UsrNum in vMsgList2 (the spaces are important!): */
  DEFINE VARIABLE vUsrPattens AS CHARACTER NO-UNDO EXTENT 3 INITIAL
                 [" Usernum ":U, " User ":U, " Usr ":U].

  DEFINE VARIABLE vItem AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vNum  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE n     AS INTEGER   NO-UNDO.

  IF CAN-DO(vMsgList1, ipMsgNum) THEN
  DO:
    ASSIGN n = NUM-ENTRIES(ipMsgText, " ":U).
    REPEAT i = 2 TO n:
      ASSIGN vItem = ENTRY(i, ipMsgText, " ":U).
      IF vItem EQ "":U THEN
      NEXT.
      
      ASSIGN vNum  = INTEGER(vItem) NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
      NEXT.

      RETURN TRIM(vItem, ",.":U).
    END. /* REPEAT i = 2 TO n */
  END. /* IF CAN-DO(vMsgList1, ipMsgNum) */
  ELSE
/* Try to identify vUsrNum by the pattens: */
  IF CAN-DO(vMsgList2, ipMsgNum)
  OR TRIM(ipMsgNum, "-":U) EQ "":U THEN
  DO:
    ASSIGN n = EXTENT(vUsrPattens)
           ipMsgText = " ":U + REPLACE(ipMsgText, "=":U, " ":U).
    REPEAT i = 1 TO n:
      ASSIGN vNum = INDEX(ipMsgText, vUsrPattens[i]).

      IF vNum EQ 0 THEN
      NEXT. /* vUsrPattens */
    
      ASSIGN vNum  = vNum + LENGTH(vUsrPattens[i]) - 1
             vItem = TRIM(SUBSTRING(ipMsgText, vNum))
             vItem  = ENTRY(1, vItem, " ":U)
      . /* ASSIGN */

      IF vItem EQ "":U THEN
      NEXT. /* vUsrPattens */

      ASSIGN vNum  = INTEGER(vItem) NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
      NEXT. /* vUsrPattens */

/* Pattern followed by a number is found. */
      RETURN TRIM(vItem, ",.":U).
    END. /* REPEAT i = 1 TO n */
  END. /* IF CAN-DO(vMsgList2, ipMsgNum) */

/* UsrNum was not found: */
  RETURN ?.
END FUNCTION. /* UsrNum */

/* ------------------------------------------------------------------------- */

PROCEDURE ImportMsgs.

  DEFINE INPUT PARAMETER ipInputFile AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipDbId      AS INTEGER   NO-UNDO.
  
/* The estimation of the average length of line in db log: */
  DEFINE VARIABLE vLineLength AS DECIMAL   NO-UNDO INITIAL 135.0.
  DEFINE VARIABLE vDispName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFileSize   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vReportLine AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vUsrNum     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vText       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vIndex      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vIgnoreUsrs AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vLine       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vMsgLine    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMsgTime    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vMsgPID     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vMsgTID     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vMsgType    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vSrvType    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vSrvNum     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vMsgNum     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vMsgText    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vMsgMask    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i           AS INTEGER   NO-UNDO.


  ASSIGN FILE-INFO:FILE-NAME = ipInputFile
         vFileSize = FILE-INFO:FILE-SIZE / 100.0
         vReportLine = MAX(1000, vFileSize / vLineLength)
         vDispName = IF LENGTH(ipInputFile) LE 41 THEN ipInputFile ELSE
                     SUBSTRING(ipInputFile, 1, 18) + "...":U +
                     SUBSTRING(ipInputFile, LENGTH(ipInputFile) - 19)
  . /* ASSIGN */

  IF FILE-INFO:FILE-SIZE EQ 0 THEN
  RETURN.

  DISPLAY vDispName @ vFile
          STRING(INTEGER(vFileSize / 10.24), ">,>>>,>>9K") @ vSize
          "0%"      @ vTime
  WITH FRAME ProgressFrame TITLE vFrameTitle.

  ETIME(TRUE).
  PAUSE 0 BEFORE-HIDE.

  INPUT FROM VALUE(ipInputFile).

ImportLine:
  REPEAT:
    ASSIGN vLine = "":U vMsgLine = vMsgLine + 1.
    IMPORT UNFORMATTED vLine.
    PROCESS EVENTS.
  
    IF vMsgLine MOD vReportLine EQ 0 THEN
    DISPLAY STRING(MAX(SEEK(INPUT), 0) / vFileSize, ">>9%") @ vTime
    WITH FRAME ProgressFrame.

    IF NOT vLine MATCHES "[..../../..@..:..:..~~~.*":U THEN
    NEXT ImportLine.
/* 
[2013/06/20@11:29:27.689+0400] P-48496826   T-1     I APW   458: (2519)  Disconnected.
*/  ASSIGN
      vLine = TRIM(vLine)

/* Msg Time: */
      vMsgTime = ENTRY(1, vLine, " ":U)
      vMsgText = TRIM(SUBSTRING(vLine, LENGTH(vMsgTime) + 2))
/* Msg PID: */
      vMsgPID  = ENTRY(1, vMsgText, " ":U)
      vMsgText = TRIM(SUBSTRING(vMsgText, LENGTH(vMsgPID) + 2))
/* Thread: */
      vMsgTID  = ENTRY(1, vMsgText, " ":U)
      vMsgText = TRIM(SUBSTRING(vMsgText, LENGTH(vMsgTID) + 2))
/* Msg Type: */
      vMsgType = ENTRY(1, vMsgText, " ":U)
      vMsgText = TRIM(SUBSTRING(vMsgText, LENGTH(vMsgType) + 2))
/* User's Type and Num: */
/* Possible cases:
I SHUT 1010: (453)   Logout by root on batch.
I PROMO1009: (453)   Logout by root on batch. 
I RPLU       (1423)  There is no server for database
I          : (-----) 
I          : (453)   Logout by root on /dev/pts/0.
*/    vIndex   = INDEX(vMsgText, ":":U)
      vIndex   = IF vIndex GT 0 THEN vIndex ELSE
                 INDEX(vMsgText, " ":U)
      vSrvNum  = SUBSTRING(vMsgText, 1, vIndex - 1)
      vMsgText = TRIM(SUBSTRING(vMsgText, vIndex + 1))
/* SrvType & vSrvNum could be "PROMON462:" or "APW   458:" */
      vSrvType = SUBSTRING(vSrvNum, 1, LENGTH(TRIM(vSrvNum, " 0123456789:")))
      vSrvNum  = TRIM(SUBSTRING(vSrvNum, LENGTH(vSrvType) + 1))
/* Msg Num and Msg Text: */
      vMsgNum  = ENTRY(1, vMsgText, " ":U)
      vMsgText = TRIM(SUBSTRING(vMsgText, LENGTH(vMsgNum) + 2))
    . /* ASSIGN */
/*
IF vMsgType NE "I" THEN
DO:
  OUTPUT TO VALUE("MsgType.txt") APPEND.
  PUT UNFORMATTED vLine SKIP.
  OUTPUT CLOSE.
END.
*/

/* Cut off the brackets in message number: */
    IF vMsgNum MATCHES "(*)" THEN
    ASSIGN vMsgNum = SUBSTRING(vMsgNum, 2, LENGTH(vMsgNum) - 2).
    
/* For some MsgNum copy UsrNum from previous message:
(2252)  Begin transaction backout.
(2253)  Transaction backout completed.

(2266)  Expected user <user> received user <num>. 
(2268)  Invalid message was received for an attempted
*/  IF CAN-DO("2252,2253,2268":U, vMsgNum) THEN
    ASSIGN vUsrNum = CopyUsrFromPrevMsg(ipDbId, vSrvNum).
    ELSE
/* Search for usernum in the message text: */
    ASSIGN vUsrNum = UsrNum(vMsgNum, vMsgText).

    IF vUsrNum EQ ? THEN
    ASSIGN vUsrNum = vSrvNum.
    ELSE
/* Copy UsrNum to a previous message: */
/* 5512,14658 => any previous message
(14658) Previous message sent on behalf of user <user>, server pid <pid>, broker pid <pid>. (5512)
(5512)  Previous message sent on behalf of user <user>.
*/  IF CAN-DO("5512,14658":U, vMsgNum) THEN
    DO:
      RUN CopyUsrToPrevMsg(ipDbId, vSrvNum, vUsrNum).
      NEXT ImportLine. /* Skip the message*/
    END. ELSE
/*
796 => TCP/IP write error
(-----) TCP/IP write error occurred with errno <num>
(796)   Error writing msg, socket=<n> errno=<n> usernum=<n> disconnected.

2266 => 792
(792)   Message received with bad usernum. 
(2266)  Expected user 9 received user 0.
*/  IF CAN-DO("796,2266":U, vMsgNum) THEN
    RUN CopyUsrToPrevMsg(ipDbId, vSrvNum, vUsrNum).

    RUN CheckMsg(ipDbId,
                 vMsgTime,
                 vMsgPID,
                 vMsgTID,
                 vMsgType,
                 vSrvType,
                 vSrvNum,
                 vMsgNum,
                 vMsgText,
                 vUsrNum,
                 vMsgLine).
  END. /* ImportLine: REPEAT */
  INPUT CLOSE.

  FOR FIRST ttOSFile
      WHERE ttOSFile.osFilePath EQ ipInputFile
  WITH FRAME ProgressFrame:
    ASSIGN ttOSFile.osFileTime = ETIME
           ttOSFile.osFileLine = vMsgLine - 1.
    DISPLAY STRING(TRUNCATE(ETIME / 1000.0, 2), ">>>>9.99 sec") @ vTime.
    DOWN.
  END.
END PROCEDURE. /* ImportMsgs */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportMsgs.

  DEFINE INPUT PARAMETER ipOutputDir AS CHARACTER NO-UNDO.

/* Limit on the number of output files - if too many sessions match ipUsrList*/
  DEFINE VARIABLE vOutputLimit AS INTEGER  NO-UNDO INITIAL 100.
  DEFINE VARIABLE vOutputCount AS INTEGER  NO-UNDO.
  
  DEFINE BUFFER bUsr FOR ttMsg.
  DEFINE VARIABLE vUsrNum AS CHARACTER   NO-UNDO.

  DEFINE VARIABLE vFilePrefix AS CHARACTER NO-UNDO INITIAL "ParseDbLogs".
  DEFINE VARIABLE vOutputFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vMinMsgTime AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vMaxMsgTime AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDbName     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDbColumn   AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE vUserList   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
  
  &SCOPED-DEFINE Sep "~t"

  ASSIGN FILE-INFO:FILE-NAME = ipOutputDir.

  
  ASSIGN ipOutputDir = (IF FILE-INFO:FULL-PATHNAME EQ ?
                        OR NOT FILE-INFO:FILE-TYPE MATCHES "*D*":U
                        THEN ".":U
                        ELSE FILE-INFO:FULL-PATHNAME
                       ) + IF OPSYS EQ "Unix":U THEN "/":U ELSE "~\":U
         vOutputCount = 0
  . /* ASSIGN */

  FOR FIRST ttGroup NO-LOCK
      WHERE ttGroup.GroupId EQ 0,

    FIRST ttDatabase NO-LOCK
    WHERE ttDatabase.DbId EQ ttGroup.UsrDbId:

    ASSIGN vUsrNum = "":U.
    REPEAT:
      FIND FIRST bUsr NO-LOCK
           WHERE bUsr.GroupId EQ ttGroup.GroupId
             AND bUsr.DbId    EQ ttGroup.UsrDbId
             AND bUsr.UsrNum  GT vUsrNum
      NO-ERROR.
      IF NOT AVAILABLE bUsr THEN
      LEAVE.

      ASSIGN vUsrNum = bUsr.UsrNum.
      
      FOR EACH ttMsg NO-LOCK
         WHERE ttMsg.GroupId EQ ttGroup.GroupId
           AND ttMsg.DbId    EQ ttGroup.UsrDbId
           AND ttMsg.UsrNum  EQ vUsrNum
            BY ttMsg.MsgTime:
        ASSIGN vMinMsgTime = SUBSTRING(ttMsg.MsgTime, 2, 19)
               vMinMsgTime = REPLACE(vMinMsgTime, "@":U, "_":U)
               vMinMsgTime = REPLACE(vMinMsgTime, "/":U,  "":U)
               vMinMsgTime = REPLACE(vMinMsgTime, ":":U,  "":U)
        . /* ASSIGN */
        LEAVE.
      END.

      FOR EACH ttMsg NO-LOCK
         WHERE ttMsg.GroupId EQ ttGroup.GroupId
           AND ttMsg.DbId    EQ ttGroup.UsrDbId
           AND ttMsg.UsrNum  EQ vUsrNum
            BY ttMsg.MsgTime DESCENDING:
        ASSIGN vMaxMsgTime = SUBSTRING(ttMsg.MsgTime, 2, 19)
               vMaxMsgTime = REPLACE(vMaxMsgTime, "@":U, "_":U)
               vMaxMsgTime = REPLACE(vMaxMsgTime, "/":U,  "":U)
               vMaxMsgTime = REPLACE(vMaxMsgTime, ":":U,  "":U)
        . /* ASSIGN */
        LEAVE.
      END.
      
      ASSIGN vMaxMsgTime = SUBSTRING(vMaxMsgTime, 10)
                      WHEN SUBSTRING(vMaxMsgTime, 1, 9)
                        EQ SUBSTRING(vMinMsgTime, 1, 9)
             vOutputFile = ipOutputDir
                         + SUBSTITUTE("&1.&2.USR_&3.&4-&5.txt",
                             /* &1 */ vFilePrefix,
                             /* &2 */ ttDatabase.DbNm,
                             /* &3 */ vUsrNum,
                             /* &4 */ vMinMsgTime,
                             /* &5 */ vMaxMsgTime)
             vOutputCount = vOutputCount + 1
      . /* ASSIGN */

      IF vOutputCount GT vOutputLimit THEN
      DO:
        MESSAGE 
          SUBSTITUTE("The number of output files exceeded the limit (&1)",
                     STRING(vOutputLimit)) SKIP
          "Increase the limit or edit the user list"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
      END.
      
      OUTPUT TO VALUE(vOutputFile).
      PUT UNFORMATTED
               "Time"
        {&Sep} "PID "
        {&Sep} "TID "
        {&Sep} "T"
        {&Sep} "SrvType"
        {&Sep} "SrvNum "
     /* {&Sep} "Usr" */
        {&Sep} "MsgNum" 
        {&Sep} "MsgText"
      SKIP.

      FOR EACH ttMsg NO-LOCK /* index Group0Idx */
         WHERE ttMsg.GroupId EQ ttGroup.GroupId
           AND ttMsg.DbId    EQ ttGroup.UsrDbId
           AND ttMsg.UsrNum  EQ vUsrNum
            BY ttMsg.MsgTime
            BY ttMsg.MsgLine:

        PUT UNFORMATTED
                 ttMsg.MsgTime
          {&Sep} ttMsg.MsgPID 
          {&Sep} ttMsg.MsgTID 
          {&Sep} ttMsg.MsgType
          {&Sep} ttMsg.SrvType
          {&Sep} ttMsg.SrvNum 
       /* {&Sep} ttMsg.UsrNum */
          {&Sep} "(" ttMsg.MsgNum ")"
          {&Sep} ttMsg.MsgText
        SKIP.
      END. /* FOR EACH ttMsg */

      OUTPUT CLOSE.

    END. /* REPEAT: FIRST bUsr */
  END. /* FOR FIRST ttGroup WHERE GroupId EQ 0, FIRST ttDatabase */


  FIND FIRST ttDatabase NO-LOCK NO-ERROR.
  IF NOT AVAILABLE ttDatabase THEN
  RETURN.

  ASSIGN vDbName   = ttDatabase.DbNm
         vDbColumn = FALSE.
  FIND NEXT ttDatabase NO-LOCK NO-ERROR.
  IF AVAILABLE ttDatabase THEN
  ASSIGN vDbName   = "AllDbs":U
         vDbColumn = TRUE.

/* Msg Group reports: ------------------------------------------------ */

  FOR EACH ttGroup NO-LOCK
     WHERE ttGroup.GroupId   NE 0
       AND ttGroup.StatGroup EQ FALSE:

    ASSIGN vMinMsgTime = ?.
    FOR EACH ttMsg NO-LOCK
       WHERE ttMsg.GroupId EQ ttGroup.GroupId
          BY ttMsg.MsgTime:
      ASSIGN vMinMsgTime = SUBSTRING(ttMsg.MsgTime, 2, 19)
             vMinMsgTime = REPLACE(vMinMsgTime, "@":U, "_":U)
             vMinMsgTime = REPLACE(vMinMsgTime, "/":U,  "":U)
             vMinMsgTime = REPLACE(vMinMsgTime, ":":U,  "":U)
      . /* ASSIGN */
      LEAVE.
    END.

    IF vMinMsgTime EQ ? THEN /* no ttMsg in current group */
    NEXT.

    FOR EACH ttMsg NO-LOCK
       WHERE ttMsg.GroupId EQ ttGroup.GroupId
          BY ttMsg.MsgTime DESCENDING:
      ASSIGN vMaxMsgTime = SUBSTRING(ttMsg.MsgTime, 2, 19)
             vMaxMsgTime = REPLACE(vMaxMsgTime, "@":U, "_":U)
             vMaxMsgTime = REPLACE(vMaxMsgTime, "/":U,  "":U)
             vMaxMsgTime = REPLACE(vMaxMsgTime, ":":U,  "":U)
      . /* ASSIGN */
      LEAVE.
    END.

    ASSIGN vMaxMsgTime = SUBSTRING(vMaxMsgTime, 10)
                    WHEN SUBSTRING(vMaxMsgTime, 1, 9)
                      EQ SUBSTRING(vMinMsgTime, 1, 9)
           vOutputFile = ipOutputDir
                       + SUBSTITUTE("&1.&2.&3.&4-&5.txt",
                           /* &1 */ vFilePrefix,
                           /* &2 */ vDbName,
                           /* &3 */ ttGroup.GroupName,
                           /* &4 */ vMinMsgTime,
                           /* &5 */ vMaxMsgTime)
           vOutputCount = vOutputCount + 1
    . /* ASSIGN */

    OUTPUT TO VALUE(vOutputFile).
    IF vDbColumn THEN
    PUT UNFORMATTED "Database" {&Sep}.
    PUT UNFORMATTED
             "Time"
      {&Sep} "PID"
      {&Sep} "TID"
      {&Sep} "Type"
      {&Sep} "SrvType"
      {&Sep} "SrvNum" 
      {&Sep} "UsrNum"
      {&Sep} "MsgNum" 
      {&Sep} "MsgText"
    SKIP.

    FOR EACH ttMsg NO-LOCK
       WHERE ttMsg.GroupId EQ ttGroup.GroupId,
      
       FIRST ttDatabase NO-LOCK
       WHERE ttDatabase.DbId EQ ttMsg.DbId

          BY ttMsg.MsgTime:

      IF vDbColumn THEN
      PUT UNFORMATTED ttDatabase.DbNm {&Sep}.
      PUT UNFORMATTED
               ttMsg.MsgTime
        {&Sep} ttMsg.MsgPID 
        {&Sep} ttMsg.MsgTID 
        {&Sep} ttMsg.MsgType
        {&Sep} ttMsg.SrvType
        {&Sep} ttMsg.SrvNum
        {&Sep} ttMsg.UsrNum
        {&Sep} "(" ttMsg.MsgNum ")" 
        {&Sep} ttMsg.MsgText
      SKIP.
    END. /* FOR EACH ttMsg */
    OUTPUT CLOSE.
  END. /* FOR EACH ttGroup NO-LOCK WHERE StatGroup EQ FALSE*/

/* Count Group reports: ------------------------------------------------ */

  FOR EACH ttGroup NO-LOCK
     WHERE ttGroup.StatGroup   EQ TRUE
       AND ttGroup.ActiveGroup EQ TRUE
       AND ttGroup.GroupName   NE "":U:

    ASSIGN vMinMsgTime = ?.
    FOR EACH ttMsgStat NO-LOCK
       WHERE ttMsgStat.GroupId EQ ttGroup.GroupId
          BY ttMsgStat.MsgDate
          BY ttMsgStat.MsgTime:

      ASSIGN vMinMsgTime = REPLACE(ttMsgStat.MsgDate, "/":U,  "":U) + "_":U
                         + REPLACE(ttMsgStat.MsgTime, ":":U,  "":U)
      . /* ASSIGN */
      LEAVE.
    END.
    IF vMinMsgTime EQ ? THEN  /* no ttMsgStats in current group */
    NEXT.

    FOR EACH ttMsgStat NO-LOCK
       WHERE ttMsgStat.GroupId EQ ttGroup.GroupId
          BY ttMsgStat.MsgDate DESCENDING
          BY ttMsgStat.MsgTime DESCENDING:

      ASSIGN vMaxMsgTime = REPLACE(ttMsgStat.MsgDate, "/":U,  "":U) + "_":U
                         + REPLACE(ttMsgStat.MsgTime, ":":U,  "":U)
      . /* ASSIGN */
      LEAVE.
    END.

    ASSIGN vMaxMsgTime = SUBSTRING(vMaxMsgTime, 10)
                    WHEN SUBSTRING(vMaxMsgTime, 1, 9)
                      EQ SUBSTRING(vMinMsgTime, 1, 9)
           vOutputFile = ipOutputDir
                       + SUBSTITUTE("&1.&2.&3.&4-&5.txt",
                           /* &1 */ vFilePrefix,
                           /* &2 */ vDbName,
                           /* &3 */ ttGroup.GroupName,
                           /* &4 */ vMinMsgTime,
                           /* &5 */ vMaxMsgTime)
           vOutputCount = vOutputCount + 1
    . /* ASSIGN */

    OUTPUT TO VALUE(vOutputFile).

    IF vDbColumn THEN
    PUT UNFORMATTED "Database" {&Sep}.
    PUT UNFORMATTED "Date"     {&Sep} "Time".

/* MaxCount = ? => All columns contribute to the total.
   MaxCount = 0 => Don't create the "Total" column.
   MaxCount > 0 => Only first MaxCount columns contribute to the total.
*/  IF ttGroup.MaxCount EQ ? THEN
    PUT UNFORMATTED {&Sep} "Total".
    ELSE
    IF ttGroup.MaxCount GT 0 THEN
    PUT UNFORMATTED {&Sep} "Total"  "|":U ttGroup.MaxCount.

    FOR EACH ttGroupMsg NO-LOCK
       WHERE ttGroupMsg.GroupId EQ ttGroup.GroupId
          BY ttGroupMsg.MsgCol:

      PUT UNFORMATTED {&Sep} ttGroupMsg.MsgText.
    END. /* FOR EACH ttGroupMsg */
    PUT UNFORMATTED
      {&Sep} "UserList"
      {&Sep} "UserList"
    SKIP.

    FOR EACH ttMsgStat NO-LOCK
       WHERE ttMsgStat.GroupId EQ ttGroup.GroupId,
      
       FIRST ttDatabase NO-LOCK
       WHERE ttDatabase.DbId EQ ttMsgStat.DbId

          BY ttMsgStat.MsgDate
          BY ttMsgStat.MsgTime:

      IF vDbColumn THEN
      PUT UNFORMATTED ttDatabase.DbNm {&Sep}.
      PUT UNFORMATTED
               ttMsgStat.MsgDate
        {&Sep} ttMsgStat.MsgTime.
      IF ttGroup.MaxCount EQ 0 THEN . ELSE
      PUT UNFORMATTED {&Sep} ttMsgStat.TotCount.

      FOR EACH ttGroupMsg NO-LOCK
         WHERE ttGroupMsg.GroupId EQ ttGroup.GroupId
            BY ttGroupMsg.MsgCol:
        PUT UNFORMATTED {&Sep} ttMsgStat.MsgCount[ttGroupMsg.MsgCol].
      END.
      ASSIGN vUserList = SUBSTRING(ttMsgStat.UserList, 2).
      PUT UNFORMATTED
        {&Sep} NUM-ENTRIES(vUserList)
        {&Sep} vUserList
      SKIP.
    END. /* FOR EACH ttMsgStat */
    OUTPUT CLOSE.
  END. /* FOR EACH ttGroup NO-LOCK WHERE StatGroup EQ TRUE*/

  ASSIGN vOutputFile = ipOutputDir + vFilePrefix + ".Statitics.txt"
         vOutputCount = vOutputCount + 1
  . /* ASSIGN */

  OUTPUT TO VALUE(vOutputFile).
  PUT UNFORMATTED
    TODAY SPACE STRING(TIME, "HH:MM:SS":U)     SKIP
    "Imported files:"                          SKIP
    "     Lines  Size(KB)    Time KB/Sec File" SKIP
    "---------- --------- ------- ------ -------------------------------------"
  SKIP.

  FOR EACH ttOSFile NO-LOCK
        BY ttOSFile.osFileName:
    PUT ttOSFile.osFileLine        FORMAT ">>,>>>,>>9" SPACE
        ttOSFile.osFileSize / 1024 FORMAT ">,>>>,>>9"  SPACE
        ttOSFile.osFileTime / 1000 FORMAT ">,>>9.9"    SPACE
        IF ttOSFile.osFileTime LE 0 THEN ? ELSE
        ttOSFile.osFileSize / ttOSFile.osFileTime / 1.024
                                   FORMAT ">>>9.9"     SPACE.
    PUT UNFORMATTED
       ttOSFile.osFilePath
    SKIP.
  END.

  FOR EACH ttMsg NO-LOCK:
    ACCUMULATE "ttMsg":U (COUNT).
  END.

  FOR EACH ttMsgStat NO-LOCK:
    ACCUMULATE "ttMsgStat":U (COUNT).
  END.

  PUT UNFORMATTED                                SKIP(1)
    "Temp-tables:"                               SKIP
    "ttMsg    :" SPACE ACCUM COUNT "ttMsg":U     SKIP
    "ttMsgStat:" SPACE ACCUM COUNT "ttMsgStat":U SKIP(1)
    "Group statistics:"                          SKIP
    "Name             ID Active   Type        Msg        Log        Msk" SKIP
    "---------------- -- -------- ----------- ---------- ---------- ----------"
  SKIP.
  FOR EACH ttGroup NO-LOCK
     WHERE ttGroup.LogCount GT 0:
    PUT
      ttGroup.GroupName
      ttGroup.GroupId                      SPACE
      ttGroup.ActiveGroup                  SPACE
      ttGroup.StatGroup                    SPACE
      ttGroup.MsgCount FORMAT ">>,>>>,>>9" SPACE
      ttGroup.LogCount FORMAT ">>,>>>,>>9" SPACE
      ttGroup.MskCount FORMAT ">>,>>>,>>9"
    SKIP.
  END.

  PUT UNFORMATTED       SKIP(1)
    "Mask statistics:"  SKIP
    "Group            ID Accept      Count Mask" SKIP
    "---------------- -- -------- -------- ---------------------------------"
  SKIP.
  FOR EACH ttGroupMask NO-LOCK,
     FIRST ttGroup     NO-LOCK
     WHERE ttGroup.GroupId EQ ttGroupMask.GroupId:
    PUT ttGroup.GroupName
        ttGroup.GroupId      SPACE
        ttGroupMask.Accept   SPACE
        ttGroupMask.MskCount FORMAT ">>,>>>,>>9" SPACE.
    PUT UNFORMATTED
        ttGroupMask.Mask
    SKIP.
  END.
  OUTPUT CLOSE.

  IF NOT SESSION:BATCH-MODE THEN
  MESSAGE
    vOutputCount "output file(s) created in direcory:" SKIP
    ipOutputDir
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE. /* ReportMsgs */

/* -------------------------------------------------------------------------- */

PROCEDURE GetOSFiles.

  DEFINE INPUT PARAMETER ipRootDir     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFileMask    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMaxDirLevel AS INTEGER NO-UNDO.

  DEFINE VARIABLE vFileName AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE vFilePath AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE vFileAttr AS CHARACTER   NO-UNDO.

  ASSIGN ipMaxDirLevel = ipMaxDirLevel - 1.
  INPUT FROM OS-DIR(ipRootDir).
  REPEAT:
    IMPORT  vFileName vFilePath vFileAttr.

    IF  vFileAttr MATCHES "*D*" 
    AND vFileName NE ".":U
    AND vFileName NE "..":U 
    AND (ipMaxDirLevel EQ ? OR ipMaxDirLevel GT 0) THEN
    RUN GetOSFiles(INPUT vFilePath, INPUT ipFileMask, INPUT ipMaxDirLevel).
    ELSE
    IF  vFileAttr MATCHES "*F*"
    AND vFileName MATCHES ipFileMask THEN
    DO TRANSACTION:
      CREATE ttOSFile.
      ASSIGN ttOSFile.osFileName = vFileName
             ttOSFile.osFilePath = vFilePath
             ttOSFile.osFileAttr = vFileAttr
      . /* ASSIGN */
    END.
  END.
  INPUT CLOSE.
END PROCEDURE. /* GetOSFiles */

/* ------------------------------------------------------------------------- */
