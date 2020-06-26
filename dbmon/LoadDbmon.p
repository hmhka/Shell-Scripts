DEFINE VARIABLE vRootDir   AS CHARACTER NO-UNDO. /* Directory with log files */
DEFINE VARIABLE vFileMasks AS CHARACTER NO-UNDO. /* Mask for the file names  */
DEFINE VARIABLE vMaxDepth  AS INTEGER   NO-UNDO. /* Max depth in directory   */
ASSIGN
/* Set the default directory that contains the promon's log files: */
  vRootDir = "/path/to/dir"

/* Set a comma separated list of the masks for the files to load: */
  vFileMasks =  "*.promon.*_*.log" /* dbmon.sh Release 2.0 */
     /* 1 */ + ",*.promon.log"     /* dbmon.sh Release 3.0 */
/* The special log files (dbmon.sh Release 3.0): */
     /* 2 */ + ",*.latches.log"
     /* 3 */ + ",*.status_Active_Transactions.log"
     /* 4 */ + ",*.status_Blocked_Clients.log"
     /* 5 */ + ",*.status_Buffer_Lock_Queue.log"
     /* 6 */ + ",*.status_Buffer_Locks.log"
/*  vFileMasks = "*.debug.txt"*/

/* How deep to search in the directory: the unknown value means no limit. */
  vMaxDepth  = ?
. /* ASSIGN */

/* ---------------------------------------------------------------------------
    File        : LoadDbmon.p
    Purpose     : Load data from the promon's log files.
    Syntax      : Run it!
    Description : The output files can be opened in Excel (tab separeted cols).
*.StatusProperties.txt - the properties (names and values) on the Status menus.
*.ActivityProperties.txt - the activity counters on the Activity menus.
*.StaticProperties.txt - Status properties (like db startup parameters) and
                          zeroed zeroed activity counters that are not changing
                          during the monitoring interval.
*.<Menu_Name>.txt - one output file per 'Array' type menus (like "Checkpoints")
                    that have more than one property's value per line.

    Author(s)   : George Potemkin
    Created     : Dec 06, 2017
    Modified    : Jan 29, 2018 (the first 'final' version)
---------------------------------------------------------------------------- */

RUN ChooseDir(INPUT-OUTPUT vRootDir,
              INPUT-OUTPUT vFileMasks,
              INPUT-OUTPUT vMaxDepth).

IF vRootDir EQ ? THEN
RETURN.

RUN LoadPromonLogs(vRootDir, vFileMasks, vMaxDepth).

/* ************************************************************************* */

&SCOPED-DEFINE IgnoreErrors YES

DEFINE STREAM LargeReport.

DEFINE TEMP-TABLE ttOSFile NO-UNDO
  FIELD osFileName AS CHARACTER
  FIELD osFilePath AS CHARACTER
  FIELD osFileAttr AS CHARACTER
  INDEX osFileName
        osFileName
. /* DEFINE TEMP-TABLE ttOSFile */

DEFINE TEMP-TABLE ttMenu NO-UNDO
  FIELD MenuId    AS INTEGER
  FIELD MenuName  AS CHARACTER /* Menu title like "Activity: Summary".   */
  FIELD MenuTime  AS DATETIME  /* Date/Time of the last menu output.     */
  FIELD ArraySize AS INTEGER   /* The number of the columns in an 'Array'*/
  FIELD ArrayHead AS CHARACTER /* A header of the 'Array' type menus.    */
  INDEX MenuId    IS UNIQUE
        MenuId
  INDEX MenuName  IS UNIQUE
        MenuName
. /* DEFINE TEMP-TABLE ttMenu */

DEFINE TEMP-TABLE ttSect NO-UNDO
  FIELD MenuId    AS INTEGER
  FIELD SectId    AS INTEGER
  FIELD SectName  AS CHARACTER /* Name of section in promon's screen.     */
  INDEX SectId    IS UNIQUE
        MenuId
        SectId
  INDEX SectName  IS UNIQUE
        MenuId
        SectName
. /* DEFINE TEMP-TABLE ttSect */

/* Promon's screens devided in the sections:
Activity: Buffer Cache => Database Buffer Pool
                          Primary Buffer Pool
                          Alternate Buffer Pool
Activity: I/O Operations by Type => reads/writes
Status: Broker Startup Parameters => Login brokers
Activity: Servers => Activity for Server #
*/
DEFINE TEMP-TABLE ttDatabase NO-UNDO
  FIELD DatabaseId   AS INTEGER
  FIELD DatabaseHost AS CHARACTER
  FIELD DatabaseName AS CHARACTER
  FIELD DatabasePath AS CHARACTER
  INDEX DatabaseId   IS UNIQUE
        DatabaseId
  INDEX DatabaseName IS UNIQUE
        DatabaseName
        DatabaseHost
        DatabasePath
. /* DEFINE TEMP-TABLE ttDatabase */

DEFINE TEMP-TABLE ttSnapshot NO-UNDO
  FIELD DatabaseId   AS INTEGER
  FIELD InputLogId   AS INTEGER
  FIELD SnapshotId   AS INTEGER
  FIELD SnapshotTime AS DATETIME
  FIELD StatInterval AS INTEGER
/*FIELD SinceStartup AS LOGICAL*/
  INDEX SnapshotId   IS UNIQUE
        DatabaseId
        InputLogId
        SnapshotId
  INDEX SnapshotTime
        DatabaseId
        InputLogId
        SnapshotTime
. /* DEFINE TEMP-TABLE ttSnapshot */

/* The properties: the field names in the menus. */
DEFINE TEMP-TABLE ttProperty NO-UNDO
  FIELD MenuId       AS INTEGER
  FIELD PropertyId   AS INTEGER /* eq PropertyName + PropertyRow */
  FIELD PropertyName AS CHARACTER
  FIELD PropertyRow  AS INTEGER
  FIELD PropertyType AS CHARACTER /* List: "Status,Activity,Resource,Latch" */
  INDEX PropertyId   IS UNIQUE
        MenuId
        PropertyId
  INDEX PropertyName IS UNIQUE
        MenuId
        PropertyName
        PropertyRow
. /* DEFINE TEMP-TABLE ttProperty */

/* The 'Static' properties that do not change at any snapshots: */
DEFINE TEMP-TABLE ttStatic NO-UNDO
  FIELD DatabaseId    AS INTEGER
  FIELD MenuId        AS INTEGER
  FIELD SectId        AS INTEGER
  FIELD PropertyId    AS INTEGER
  FIELD PropertyValue AS CHARACTER
  FIELD SnapshotCount AS INTEGER
  INDEX ReportOrder   IS UNIQUE
        DatabaseId
        MenuId
        SectId
        PropertyId
. /* DEFINE TEMP-TABLE ttStatic */

/* 'Status' properties are represented by their current values.
    Like 'Status: BI Log' or 'Status: Startup Parameters' menus.
*/
DEFINE TEMP-TABLE ttStatus NO-UNDO
  FIELD DatabaseId    AS INTEGER
  FIELD InputLogId    AS INTEGER
  FIELD SnapshotId    AS INTEGER
  FIELD MenuId        AS INTEGER
  FIELD SectId        AS INTEGER
  FIELD PropertyId    AS INTEGER
  FIELD PropertyValue AS CHARACTER
  FIELD SnapshotTime  AS DATETIME
  INDEX ReportOrder   IS UNIQUE
        DatabaseId
        InputLogId
        SnapshotId
        MenuId
        SectId
        PropertyId
. /* DEFINE TEMP-TABLE ttStatus */

/* 'Activity' properties are represented by a single value.
Example: 'Activity: BI Log' menu
*/
DEFINE TEMP-TABLE ttActivity NO-UNDO
  FIELD DatabaseId    AS INTEGER
  FIELD InputLogId    AS INTEGER
  FIELD SnapshotId    AS INTEGER
  FIELD MenuId        AS INTEGER
  FIELD SectId        AS INTEGER
  FIELD PropertyId    AS INTEGER
  FIELD PropertyValue AS DECIMAL EXTENT 2 /* Total and Per Sec */
  INDEX ReportOrder   IS UNIQUE
        DatabaseId
        InputLogId
        SnapshotId
        MenuId
        SectId
        PropertyId
. /* DEFINE TEMP-TABLE ttActivity */

/* 'Resource' properties are represented by two values: Requests and Waits.
Activity: Resource Queues
Activity: TXE Lock Activity
*/
DEFINE TEMP-TABLE ttResource NO-UNDO
  FIELD DatabaseId  AS INTEGER
  FIELD InputLogId  AS INTEGER
  FIELD SnapshotId  AS INTEGER
  FIELD MenuId      AS INTEGER /* no SectId */
  FIELD PropertyId  AS INTEGER
  FIELD Requests    AS DECIMAL EXTENT 2 /* Total and Per Sec */
  FIELD Waits       AS DECIMAL EXTENT 2
  INDEX ReportOrder IS UNIQUE
        DatabaseId
        InputLogId
        SnapshotId
        MenuId
        PropertyId
. /* DEFINE TEMP-TABLE ttResource */

DEFINE TEMP-TABLE ttLatch NO-UNDO
  FIELD DatabaseId  AS INTEGER
  FIELD InputLogId  AS INTEGER
  FIELD SnapshotId  AS INTEGER
  FIELD MenuId      AS INTEGER /* no SectId */
  FIELD PropertyId  AS INTEGER
  FIELD LatchOwner  AS CHARACTER
  FIELD LatchLock   AS DECIMAL EXTENT 2
  FIELD LatchWait   AS DECIMAL /* Naps/Sec  */
/* 2. Enable latch activity data collection: */
  FIELD LatchBusy   AS DECIMAL /* Busy/Sec  = one digit number */
  FIELD LatchSpin   AS DECIMAL /* Spins/Sec (Spins are greater than Locks by a few times*/
/* Added in V10.2B06: */
  FIELD MaxNapTime  AS INTEGER /* Nap Max HWM: multiple of the -nap */
  FIELD MaxNapCount AS INTEGER /* Nap Max Total: how many times HWM was reached */
  INDEX ReportOrder IS UNIQUE
        DatabaseId
        InputLogId
        SnapshotId
        MenuId
        PropertyId
. /* DEFINE TEMP-TABLE ttLatch */

/* The "Array" type menus e.g. "Checkpoints" or "Status: Active Transactions"*/
/* Max number of the columns in "Array" menus is 18 in 'Checkpoints' V11.7   */
&SCOPED-DEFINE ArraySize 20

DEFINE TEMP-TABLE ttArray NO-UNDO
  FIELD DatabaseId   AS INTEGER
  FIELD InputLogId   AS INTEGER
  FIELD SnapshotId   AS INTEGER
  FIELD MenuId       AS INTEGER /* no SectId */
  FIELD ArrayRow     AS INTEGER
  FIELD ArrayValue   AS CHARACTER EXTENT {&ArraySize}
  FIELD ArraySize    AS INTEGER
  FIELD SnapshotTime AS DATETIME
  INDEX ReportOrder  IS UNIQUE
        DatabaseId
        InputLogId
        SnapshotId
        MenuId
        ArrayRow
. /* DEFINE TEMP-TABLE ttArray */

/* ttMenu.ArrayHead parsed by columns: */
DEFINE TEMP-TABLE ttColumn NO-UNDO
  FIELD MenuId     AS INTEGER
  FIELD ColumnId   AS INTEGER
  FIELD ColumnName AS CHARACTER
  INDEX ColumnId   IS UNIQUE
        MenuId
        ColumnId
  INDEX ColumnName IS UNIQUE
        MenuId
        ColumnName
. /* DEFINE TEMP-TABLE ttColumn */

/* Statistics per snapshots for some 'Array' type menus: */
/* Used by procedures:
StatOfBlockedClients
StatOfBufferLockQueue
StatOfBufferLocks
StatOfActiveTransactions
*/
DEFINE TEMP-TABLE ttArraySet NO-UNDO
  FIELD SetCount AS INTEGER
  FIELD SetValue AS CHARACTER EXTENT {&ArraySize}
  INDEX SetCount
        SetCount
. /* DEFINE TEMP-TABLE ttActTransStat */

/* ------------------------------------------------------------------------- */

PROCEDURE ChooseDir:

  DEFINE INPUT-OUTPUT PARAMETER ioDirectory AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ioFileMasks AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER ioMaxDepth  AS INTEGER   NO-UNDO.

  CREATE WIDGET-POOL.

  DEFINE VARIABLE hWindow AS WIDGET-HANDLE NO-UNDO.
  DEFINE BUTTON btnCancel SIZE 11 BY 1.
  DEFINE BUTTON btnDir    SIZE 11 BY 1.
  DEFINE BUTTON btnOK     SIZE 11 BY 1.


  DEFINE FRAME ChooseDir
    ioDirectory FORMAT "X(256)":U LABEL "Root Dir"
                VIEW-AS FILL-IN SIZE 51 BY 1 AT ROW 1.5 COL 12 COLON-ALIGNED
    ioFileMasks FORMAT "X(256)":U LABEL "File Masks"
                VIEW-AS FILL-IN SIZE 51 BY 1 AT ROW 3   COL 12 COLON-ALIGNED
    ioMaxDepth  LABEL "Max Depth" FORMAT "9" AT ROW 4.5 COL 12 COLON-ALIGNED
    btnDir      LABEL "Browse"               AT ROW 1.5 COL 67
                HELP "Choose Directory with promon logs"
    btnOK       LABEL "OK"                   AT ROW 3   COL 67
    btnCancel   LABEL "Cancel"               AT ROW 4.5 COL 67
  WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 10
         DEFAULT-BUTTON btnOK CANCEL-BUTTON btnCancel.

  IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW hWindow ASSIGN
         HIDDEN             = YES
         TITLE              = "Choose Directory"
         HEIGHT             = 5
         WIDTH              = 80
         MAX-HEIGHT         = 48
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 48
         VIRTUAL-WIDTH      = 384
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = YES
  . /* CREATE */
  ELSE
  ASSIGN hWindow = CURRENT-WINDOW.

  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(hWindow) THEN 
  ASSIGN hWindow:HIDDEN = no.
  
  ON END-ERROR OF hWindow
  OR ENDKEY    OF hWindow ANYWHERE DO:
    IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
  END.
  
  ON WINDOW-CLOSE OF hWindow
  DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
  END.

  ON CLOSE OF THIS-PROCEDURE
  DO:
    IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(hWindow)
    THEN DELETE WIDGET hWindow.
    IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
  END.
  
  ON CHOOSE OF btnOK IN FRAME ChooseDir
  DO:
    ASSIGN FILE-INFO:FILE-NAME = ioDirectory:SCREEN-VALUE IN FRAME ChooseDir.
    IF FILE-INFO:FULL-PATHNAME EQ ? THEN
    DO:
      MESSAGE
        SUBSTITUTE("Directory &1  does not exist!",
                   ioDirectory:SCREEN-VALUE IN FRAME ChooseDir)
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN NO-APPLY.
    END.
    
    IF NOT FILE-INFO:FILE-TYPE MATCHES "*D*":U THEN
    DO:
      MESSAGE
        SUBSTITUTE("&1  is not a directory!",
                   ioDirectory:SCREEN-VALUE IN FRAME ChooseDir)
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN NO-APPLY.
    END.

    ASSIGN ioDirectory:SCREEN-VALUE IN FRAME ChooseDir = FILE-INFO:FULL-PATHNAME
           ioDirectory = FILE-INFO:FULL-PATHNAME
/* To specify a period (".") as a literal character
   we  must enter a tilde (~) before the character. 
   A period (".") in a mask matches any single character.
   If we specify the match pattern as a literal quoted string in a procedure
   file, you must enter each tilde as a double tilde ( ~ ~ ). The first tilde
   escapes the second tilde. So "~~.":U in a mask will match exactly a dot.
*/         ioFileMasks = REPLACE(ioFileMasks, ".":U, "~~.":U)
                        WHEN NOT ioFileMasks MATCHES "*~~~~.*":U
    . /* ASSIGN */
    APPLY "WINDOW-CLOSE" TO FRAME ChooseDir.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
  END.

  ON CHOOSE OF btnCancel IN FRAME ChooseDir
  DO:
    ASSIGN ioDirectory = ?.
    APPLY "WINDOW-CLOSE" TO FRAME ChooseDir.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
  END.

  ON CHOOSE OF btnDir IN FRAME ChooseDir
  DO:
    DEFINE VARIABLE vOKpressed AS LOGICAL NO-UNDO INITIAL TRUE.

    SYSTEM-DIALOG GET-DIR ioDirectory
      INITIAL-DIR ioDirectory
      TITLE " Choose Directory with Promon Logs "
    UPDATE vOKpressed. 

    IF vOKpressed THEN
    ASSIGN ioDirectory:SCREEN-VALUE IN FRAME ChooseDir = ioDirectory.
  END.

  ASSIGN CURRENT-WINDOW                = hWindow 
         THIS-PROCEDURE:CURRENT-WINDOW = hWindow
  . /* ASSIGN */
  
  PAUSE 0 BEFORE-HIDE.
  
  DO ON ERROR   UNDO, RETURN
     ON END-KEY UNDO, RETURN:
    DISPLAY ioDirectory ioFileMasks ioMaxDepth
    WITH FRAME ChooseDir IN WINDOW hWindow.
    ENABLE ioDirectory ioFileMasks ioMaxDepth btnDir btnOK btnCancel
    WITH FRAME ChooseDir IN WINDOW hWindow.
    VIEW hWindow.
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
  END.

END PROCEDURE. /* ChooseDir */

/* ------------------------------------------------------------------------- */

PROCEDURE CreateWindow:

  DEFINE INPUT PARAMETER ipHeight AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipWidth  AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipTitle  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vWidget AS HANDLE NO-UNDO.

  IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW vWidget ASSIGN
         HIDDEN             = YES
         TITLE              = ipTitle
         HEIGHT             = ipHeight
         WIDTH              = ipWidth
         MAX-HEIGHT         = 24
         MAX-WIDTH          = ipWidth
         VIRTUAL-HEIGHT     = 24
         VIRTUAL-WIDTH      = ipWidth
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
  ELSE
/*IF SESSION:DISPLAY-TYPE = "TTY" */
  ASSIGN vWidget = CURRENT-WINDOW.

  ON WINDOW-CLOSE OF vWidget /* Progress status */
  DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
  END.

  ON CLOSE OF THIS-PROCEDURE
  DO:
    IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(vWidget) THEN 
    DELETE WIDGET vWidget.
  END.

  ASSIGN CURRENT-WINDOW                = vWidget 
         THIS-PROCEDURE:CURRENT-WINDOW = vWidget
  . /* ASSIGN */

END PROCEDURE. /* CreateWindow */

/* ------------------------------------------------------------------------- */

PROCEDURE DeleteWindow:

  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(CURRENT-WINDOW) THEN 
  DELETE WIDGET CURRENT-WINDOW.

END PROCEDURE. /* DeleteWindow */

/* ------------------------------------------------------------------------- */

PROCEDURE GetOSFiles.

  DEFINE INPUT PARAMETER ipRootDir     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFileMask    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMaxDirDepth AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vFileName AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE vFilePath AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE vFileAttr AS CHARACTER   NO-UNDO.

  ASSIGN ipMaxDirDepth = ipMaxDirDepth - 1.
  INPUT FROM OS-DIR(ipRootDir).
  REPEAT:
    IMPORT  vFileName vFilePath vFileAttr.

    IF  vFileAttr MATCHES "*D*"
    AND vFileName NE ".":U
    AND vFileName NE "..":U
    AND (ipMaxDirDepth EQ ? OR ipMaxDirDepth GT 0) THEN
    RUN GetOSFiles(INPUT vFilePath, INPUT ipFileMask, INPUT ipMaxDirDepth).
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

FUNCTION FleaDetected RETURNS LOGICAL (INPUT ipLine AS CHARACTER):
/*
Solution: 17962
Title: Why am I being "kicked out" of PROMON?
Written by Mark Meachen 20/05/98

The R&D section of promon will exit, after writing the message
"Progress Monitor Session End." to stdout, under the following
conditions:

* the shutdown flag in shared memory is set. The message
  "The database is being shut down." is also written to stdout.

* the usrtodie flag in shared memory is set for the promon user.
  The message "The database has been disconnected." is also written to
  stdout.

* the "x" or "X" command has been entered when input is requested.

* end of file is detected on stdin when input is requested. This may
  occur for several reasons:

   - promon was being fed input from a file and eof was reached
   - ctrl-d was entered
   - rlogin or telnet session terminated or window closed
     (in this case, several signals may also be generated
     and one of them might cause it to stop).

* a signal like HUP, QUIT, or TERM was received. In this case
  there will not be the normal session end message to stdout.

* a couple of internal errors are detected.

Progress Software Technical Support Note #17962
---------------------------------------------------------------------------

Messages:
The database is being shutdown. (1384)
This message is written to the user's terminal if the user is accessing
a multi-user database and that database server is shutdown.

Database <name> was disconnected. (1015)
The named database was disconnected and is no longer available to
your PROGRESS session.  You might be to reaccess the database by issuing
a CONNECT statement. There are several possible reasons the database was
disconnected. For example, the database server may have been shut down,
or the PROSHUT program was used to disconnect you.

_mprshut:
The database is being shut down.
The database has been disconnected.
OpenEdge Monitor Session End.

---------------------------------------------------------------------------

Example:
OpenEdge Release 11.7.1 as of Wed Jun 14 18:25:48 EDT 2017

12/21/
Database /path/to/dbname was disconnected. (1015)
17        Activity: I/O Operations by File
08:02:26        12/21/17 07:57 to 12/21/17 08:02 (5 min 1 sec)

                                    Total         Per Min          Per Sec          Per Tx

/path/to/dbname.db
Reads                                   0               0             0.00            0.00
Writes                                  0               0             0.00            0.00
Extends                                 0               0             0.00            0.00

The database has been disconnected.

OpenEdge Monitor Session End.
*/
  IF ipLine MATCHES "Database * was disconnected. (1015)":U THEN
  RETURN TRUE.

  CASE ipLine:
    WHEN "The database is being shutdown. (1384)":U OR
    WHEN "The database is being shut down.":U       OR
    WHEN "The database has been disconnected.":U    OR
    WHEN "OpenEdge Monitor Session End.":U          OR
    WHEN "Progress Monitor Session End":U         THEN
    RETURN TRUE.
  END CASE. /* ipLine */

  RETURN FALSE.

END FUNCTION. /* FleaDetected */

/* ------------------------------------------------------------------------- */

FUNCTION Str2DateTime RETURNS DATETIME (ipDate AS CHARACTER,
                                        ipTime AS CHARACTER):
  /* ipDate: MM/DD/[CC]YY  ipTime: HH:MM:SS */

  DEFINE VARIABLE vDateTime   AS DATETIME  NO-UNDO INITIAL ?.
  DEFINE VARIABLE vYearOffset AS CHARACTER NO-UNDO INITIAL "":U.
  ASSIGN vYearOffset = "20":U WHEN LENGTH(ENTRY(3, ipDate, "/":U)) EQ 2
         vDateTime = DATETIME(
                    /* month   */ INTEGER(ENTRY(1, ipDate, "/":U)),
                    /* day     */ INTEGER(ENTRY(2, ipDate, "/":U)),
                    /* year    */ INTEGER(vYearOffset
                                        + ENTRY(3, ipDate, "/":U)),
                    /* hours   */ INTEGER(ENTRY(1, ipTime, ":":U)),
                    /* minutes */ INTEGER(ENTRY(2, ipTime, ":":U)),
                    /* seconds */ INTEGER(ENTRY(3, ipTime, ":":U)))
  NO-ERROR. /* ASSIGN */
  RETURN vDateTime.

END FUNCTION. /* DateTime2Str */

/* ------------------------------------------------------------------------- */

FUNCTION DateTime2Str RETURNS CHARACTER (ipDateTime AS DATETIME):

  DEFINE VARIABLE vDateTime2Str AS CHARACTER NO-UNDO.
  ASSIGN vDateTime2Str = ISO-DATE(ipDateTime)
         vDateTime2Str = SUBSTRING(vDateTime2Str, 1, 10) + " ":U
                       + SUBSTRING(vDateTime2Str, 12, 8)
  . /* ASSIGN */
  RETURN vDateTime2Str.
END FUNCTION. /* DateTime2Str */

/* ------------------------------------------------------------------------- */

FUNCTION String2Dec RETURNS DECIMAL (INPUT ipString AS CHARACTER).
/*
01/06/18        Activity: Space Allocation
11:41:17        01/06/18 11:41 to 01/06/18 11:41 (6 sec)
                    Total    Per Min          Per Sec          Per Tx
Alloc from free  16777216T    163840P 3074457345618258432.00 771120477957927.88
*/
  DEFINE VARIABLE vResult AS DECIMAL   NO-UNDO INITIAL ?.
  DEFINE VARIABLE vUnit   AS CHARACTER NO-UNDO INITIAL "":U.

  ASSIGN ipString = TRIM(ipString)
         vUnit    = LEFT-TRIM(ipString, "1234567890.":U)
         ipString = SUBSTRING(ipString, 1, LENGTH(ipString) - LENGTH(vUnit))
         vResult  = DECIMAL(ipString)
  NO-ERROR. /* ASSIGN */

&IF NOT {&IgnoreErrors} &THEN
  IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  MESSAGE SUBSTITUTE("String2Dec(&1):":U, ipString) ERROR-STATUS:GET-MESSAGE(1)
  VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF

  CASE vUnit:
    WHEN "K":U THEN RETURN vResult * 1024.0.
    WHEN "M":U THEN RETURN vResult * 1048576.0.
    WHEN "G":U THEN RETURN vResult * 1073741824.0.
    WHEN "T":U THEN RETURN vResult * 1099511627776.0.
    WHEN "P":U THEN RETURN vResult * 1125899906842624.0.
    OTHERWISE       RETURN vResult.
  END CASE. /* vUnit */

END FUNCTION. /* String2Dec */

/* ------------------------------------------------------------------------- */

FUNCTION Bytes2Str RETURN CHARACTER (INPUT ipSize AS INT64):

  DEFINE VARIABLE vDecimal AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vMultiple AS INTEGER NO-UNDO.

  ASSIGN vDecimal = ipSize
         vMultiple = 1
  . /* ASSIGN */
  DO WHILE ROUND(vDecimal, 0) GE 1000.0:
    ASSIGN vDecimal = vDecimal / 1024.0
           vMultiple = vMultiple + 1
    . /* ASSIGN */
  END. /* DO WHILE */

  RETURN RIGHT-TRIM(SUBSTRING(STRING(vDecimal), 1, 3), ".":U)
       + SUBSTRING("BKMGTP", vMultiple, 1).
END FUNCTION. /* Bytes2Str */

/* ------------------------------------------------------------------------- */

FUNCTION MenuId RETURNS INTEGER (INPUT ipMenuName AS CHARACTER):

  FOR FIRST ttMenu NO-LOCK /* INDEX MenuName */
      WHERE ttMenu.MenuName EQ ipMenuName:
    RETURN  ttMenu.MenuId.
  END.

  RETURN ?.

END FUNCTION. /* MenuId */

/* ------------------------------------------------------------------------- */

FUNCTION SectId RETURNS INTEGER (INPUT ipMenuId   AS INTEGER,
                                 INPUT ipSectName AS CHARACTER):

  DEFINE VARIABLE vSectId AS INTEGER NO-UNDO.

  IF ipMenuId EQ ? THEN
  RETURN ?.

  FOR FIRST ttSect NO-LOCK /* INDEX SectName */
      WHERE ttSect.MenuId   EQ ipMenuId
        AND ttSect.SectName EQ ipSectName:
    RETURN  ttSect.SectId.
  END.

  ASSIGN vSectId = 0.
  FOR EACH ttSect NO-LOCK /* INDEX SectId */
     WHERE ttSect.MenuId EQ ipMenuId
        BY ttSect.SectId DESCENDING:
    ASSIGN vSectId = ttSect.SectId + 1.
    LEAVE.
  END.

  DO TRANSACTION:
    CREATE ttSect.
    ASSIGN ttSect.MenuId   = ipMenuId
           ttSect.SectId   = vSectId
           ttSect.SectName = ipSectName
    . /* ASSIGN */
  END.

  RETURN vSectId.

END FUNCTION. /* SectId */

/* ------------------------------------------------------------------------- */

FUNCTION PropertyId RETURNS INTEGER (INPUT ipPropertyType AS CHARACTER,
                                     INPUT ipMenuId       AS INTEGER,
                                     INPUT ipPropertyName AS CHARACTER,
                                     INPUT ipPropertyRow  AS INTEGER).

  DEFINE VARIABLE vPropertyId AS INTEGER NO-UNDO.

  FOR FIRST ttProperty EXCLUSIVE-LOCK /* INDEX PropertyName */
      WHERE ttProperty.MenuId       EQ ipMenuId
        AND ttProperty.PropertyName EQ ipPropertyName
        AND ttProperty.PropertyRow  EQ ipPropertyRow:
    ASSIGN  ttProperty.PropertyType = ttProperty.PropertyType + ",":U
                                    + ipPropertyType
            WHEN LOOKUP(ipPropertyType, ttProperty.PropertyType) EQ 0
    . /* ASSIGN */
    RETURN  ttProperty.PropertyId.
  END. /* FOR FIRST ttProperty */

  ASSIGN vPropertyId = 0.
  FOR EACH ttProperty NO-LOCK
        BY ttProperty.PropertyId DESCENDING:
    ASSIGN vPropertyId = ttProperty.PropertyId + 1.
    LEAVE.
  END.

  DO TRANSACTION:
    CREATE ttProperty.
    ASSIGN ttProperty.MenuId       = ipMenuId
           ttProperty.PropertyId   = vPropertyId
           ttProperty.PropertyName = ipPropertyName
           ttProperty.PropertyRow  = ipPropertyRow
           ttProperty.PropertyType = ipPropertyType
    . /* ASSIGN */
  END. /* DO TRANSACTION */

  RETURN vPropertyId.

END FUNCTION. /* PropertyId */

/* ------------------------------------------------------------------------- */

FUNCTION ColumnId RETURNS INTEGER (INPUT ipMenuId     AS INTEGER,
                                   INPUT ipColumnName AS CHARACTER).

  DEFINE VARIABLE vColumnId AS INTEGER NO-UNDO.

  FOR FIRST ttColumn NO-LOCK /* INDEX ColumnName */
      WHERE ttColumn.MenuId     EQ ipMenuId
        AND ttColumn.ColumnName EQ ipColumnName:
    RETURN  ttColumn.ColumnId.
  END. /* FOR FIRST ttColumn */

  RETURN ?.

END FUNCTION. /* ColumnId */

/* ------------------------------------------------------------------------- */

FUNCTION ArraySize RETURNS INTEGER (ipMenuId AS INTEGER,
                                    ipArray  AS CHARACTER EXTENT {&ArraySize}):

  DEFINE VARIABLE vArraySize AS INTEGER NO-UNDO.

  ASSIGN vArraySize = 0.
  DO WHILE vArraySize LT {&ArraySize} AND ipArray[vArraySize + 1] NE "":U
  TRANSACTION:
    CREATE ttColumn.
    ASSIGN vArraySize = vArraySize + 1
           ttColumn.MenuId     = ipMenuId
           ttColumn.ColumnId   = vArraySize
           ttColumn.ColumnName = ipArray[vArraySize]
    . /* ASSIGN */
  END. /* DO WHILE vArraySize LT {&ArraySize} */

  RETURN vArraySize.

END FUNCTION. /* ArrayHead */

/* ------------------------------------------------------------------------- */

FUNCTION HeaderSize RETURNS INTEGER (ipMenuId AS INTEGER,
                                     ipHeader AS CHARACTER):

/* Header's field names with the spaces: */
  DEFINE VARIABLE vName AS CHARACTER NO-UNDO EXTENT 37 INITIAL [
  /* 01 */ "APW Q",
  /* 02 */ "BI Cluster",
  /* 03 */ "BI RRead",
  /* 04 */ "BI RReads",
  /* 05 */ "BI RWries",
  /* 06 */ "BI RWrites",
  /* 07 */ "BI Write",
  /* 08 */ "BI Writes",
  /* 09 */ "Block Dels",
  /* 10 */ "Cache Update",
  /* 11 */ "CPT Q",
  /* 12 */ "Crd Nam",
  /* 13 */ "Crd Tx Id",
  /* 14 */ "DB Write",
  /* 15 */ "DB Writes",
  /* 16 */ "Extent Name",
  /* 17 */ "Last poll time",
  /* 18 */ "Login  Time",
  /* 19 */ "Login time",
  /* 20 */ "Num Txns",
  /* 21 */ "Per i/o",
  /* 22 */ "Per Min",
  /* 23 */ "Per Sec",
  /* 24 */ "Per Tx",
  /* 25 */ "Poll time",
  /* 26 */ "Remote Address",
  /* 27 */ "Schema Timestamp",
  /* 28 */ "Start time",
  /* 29 */ "Sync Time",
  /* 30 */ "Tenant Id",
  /* 31 */ "Trans id",
  /* 32 */ "Trans State",
  /* 33 */ "Tx start time",
  /* 34 */ "User Cache",
  /* 35 */ "User Count",
  /* 36 */ "Wait Info",
  /* 37 */ "":U]. /* a column's name is the next word in a header's line. */

  DEFINE VARIABLE vHeaderSize AS INTEGER NO-UNDO.
  DEFINE VARIABLE i           AS INTEGER NO-UNDO.

  ASSIGN vHeaderSize = 0.
  FOR EACH ttColumn NO-LOCK
    WHERE ttColumn.MenuId EQ ipMenuId:
    ASSIGN vHeaderSize = vHeaderSize + 1.
  END. /* FOR EACH ttColumn */

ParseHeader:
  REPEAT WHILE ipHeader NE "":U:
/* If a header contains the known field with spaces: */
    DO i = 1 TO EXTENT(vName):
/* Last vName is empty and it's used to create ttColumn
   with the name as a single word:
*/    IF ipHeader BEGINS vName[i] THEN
      DO TRANSACTION:
        CREATE ttColumn.
        ASSIGN i = INDEX(ipHeader + " ":U, " ":U, LENGTH(vName[i]) + 1)
               vHeaderSize = vHeaderSize + 1
               ttColumn.MenuId     = ipMenuId
               ttColumn.ColumnId   = vHeaderSize
               ttColumn.ColumnName = SUBSTRING(ipHeader, 1, i - 1)
               ipHeader = TRIM(SUBSTRING(ipHeader, i + 1))
        . /* ASSIGN */
        NEXT ParseHeader.
      END. /* DO TRANSACTION */
    END. /* DO i = 1 TO EXTENT(vName) */
  END. /* ParseHeader: REPEAT WHILE ipHeader NE "" */

  RETURN vHeaderSize.

END FUNCTION. /* HeaderSize */

/* ------------------------------------------------------------------------- */

FUNCTION Add2Header RETURNS INTEGER (ipMenuId AS INTEGER,
                                     ipColumn AS INTEGER,
                                     ipHeader AS CHARACTER):
/* ipHeader - a comma separated list of the columns to add.
   ipColumn - a starting position for new columns.
*/
  DEFINE VARIABLE vHeaderSize AS INTEGER NO-UNDO.
  DEFINE VARIABLE vNewColumns AS INTEGER NO-UNDO.
  DEFINE VARIABLE i           AS INTEGER NO-UNDO.

  ASSIGN vNewColumns = NUM-ENTRIES(ipHeader)
         vHeaderSize = 0
  . /* ASSIGN */
  FOR EACH ttColumn NO-LOCK
    WHERE ttColumn.MenuId EQ ipMenuId:
    ASSIGN vHeaderSize = vHeaderSize + 1.
  END. /* FOR EACH ttColumn */

  DO i = vHeaderSize TO ipColumn BY -1:
    FOR FIRST ttColumn EXCLUSIVE-LOCK
      WHERE ttColumn.MenuId   EQ ipMenuId
        AND ttColumn.ColumnId EQ i:
      ASSIGN ttColumn.ColumnId = ttColumn.ColumnId + vNewColumns.
    END. /* FOR FIRST ttColumn */
  END. /* DO i = vHeaderSize TO ipColumn */

  DO i = 1 TO vNewColumns
  TRANSACTION:
    CREATE ttColumn.
    ASSIGN ttColumn.MenuId     = ipMenuId
           ttColumn.ColumnId   = ipColumn
           ttColumn.ColumnName = ENTRY(i, ipHeader)
           ipColumn = ipColumn + 1
    . /* ASSIGN */
  END. /* DO i = 1 TO vNewColumns */

  RETURN vHeaderSize + vNewColumns.

END FUNCTION. /* Add2Header */

/* ------------------------------------------------------------------------- */

FUNCTION StatInterval RETURNS INTEGER (INPUT ipString AS CHARACTER).
/*
Convert text in the brackets to the time interval:
04:00:26        10/15/17 04:10 to 11/01/17 04:00 (407 hrs 50 min)
04:30:27        11/01/17 04:00 to 11/01/17 04:30 (30 min 0 sec)
19:45:56        11/30/17 19:45 to 11/30/17 19:45 (3 sec)

Format:
%-16s%s to %s (%u hrs %u min)
%-16s%s to %s (%u min %u sec)
%-16s%s to %s (%u sec)
*/
  DEFINE VARIABLE vInterval AS INTEGER NO-UNDO.
  DEFINE VARIABLE i         AS INTEGER NO-UNDO.

  IF NOT ipString MATCHES "*(* *)*":U THEN
  RETURN ?.

  ASSIGN ipString = SUBSTRING(ipString, R-INDEX(ipString, "(":U) + 1)
         vInterval = 0
         i = INDEX(ipString, "hrs":U)
  . /* ASSIGN */

  IF i GT 0 THEN
  IF i LT 3 THEN RETURN ?. ELSE
  ASSIGN vInterval = INTEGER(SUBSTRING(ipString, 1, i - 2)) * 3600
         ipString = SUBSTRING(ipString, i + 4)
  . /* ASSIGN */

  ASSIGN i = INDEX(ipString, "min":U).

  IF i GT 0 THEN
  IF i LT 3 THEN RETURN ?. ELSE
  ASSIGN vInterval = vInterval + INTEGER(SUBSTRING(ipString, 1, i - 2)) * 60
         ipString = SUBSTRING(ipString, i + 4)
  . /* ASSIGN */

  ASSIGN i = INDEX(ipString, "sec":U).

  IF i GT 0 THEN
  IF i LT 3 THEN RETURN ?. ELSE
  ASSIGN vInterval = vInterval + INTEGER(SUBSTRING(ipString, 1, i - 2)).

  RETURN vInterval.

END FUNCTION. /* StatInterval */

/* ------------------------------------------------------------------------- */

PROCEDURE LoadPromonLogs:

  DEFINE INPUT PARAMETER ipRootDir      AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFileMaskList AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMaxDirDepth  AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vOutputPrefix AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vInputLogId   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vRootDirSize  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vETime        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vProcList     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vProcName     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDispName     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE hWidget       AS HANDLE    NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  ASSIGN FILE-INFO:FILE-NAME = ipRootDir.
  IF FILE-INFO:FULL-PATHNAME EQ ? THEN
  DO:
    MESSAGE
      SUBSTITUTE("Directory &1  does not exist!", ipRootDir)
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
  END.

  IF NOT FILE-INFO:FILE-TYPE MATCHES "*D*":U THEN
  DO:
    MESSAGE
      SUBSTITUTE("&1  is not a directory!", ipRootDir)
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
  END.

  DO i = 1 TO NUM-ENTRIES(ipFileMaskList):
    RUN GetOSFiles(INPUT FILE-INFO:FULL-PATHNAME,
                   INPUT ENTRY(i, ipFileMaskList),
                   INPUT ipMaxDirDepth).
  END.

/* vOutputPrefix = "/" + basename ipRootDir: */
  ASSIGN vOutputPrefix = SUBSTRING(FILE-INFO:FULL-PATHNAME,
                           R-INDEX(FILE-INFO:FULL-PATHNAME,
                         IF OPSYS EQ "Unix":U THEN "/":U ELSE "~\":U))
  . /* ASSIGN */

  IF NOT FILE-INFO:FILE-TYPE MATCHES "*W*":U THEN
  DO:
    ASSIGN FILE-INFO:FILE-NAME = ".":U.
    MESSAGE
      SUBSTITUTE("You do not have permissions to write to &1", ipRootDir) SKIP
      SUBSTITUTE("The output files will be created in &1",
                                                       FILE-INFO:FULL-PATHNAME)
    VIEW-AS ALERT-BOX WARNING BUTTONS OK.
  END.

/* vOutputPrefix = ipRootDir "/" + basename ipRootDir + ".promon.": */
  ASSIGN vOutputPrefix = FILE-INFO:FULL-PATHNAME + vOutputPrefix + ".promon."
         vRootDirSize = LENGTH(FILE-INFO:FULL-PATHNAME) + 2
         vInputLogId = 0
  . /* ASSIGN */

  OUTPUT TO VALUE (vOutputPrefix + "LogFiles.txt").
  PUT UNFORMATTED "LogId":U "~t" "FilePath":U SKIP.
  FOR EACH ttOSFile:
    ASSIGN vInputLogId = vInputLogId + 1.
    PUT UNFORMATTED vInputLogId "~t" ttOSFile.osFilePath SKIP.
  END.
  OUTPUT CLOSE.

  IF vInputLogId EQ 0 THEN
  DO:
    OS-DELETE VALUE(vOutputPrefix + "LogFiles.txt").

    MESSAGE 
      "No files matched the specified masks were found in directory:" SKIP
      ipRootDir SKIP
      "Maximum depth to search in directory was" ipMaxDirDepth SKIP
      "List of the masks for file names:" SKIP
      ipFileMaskList
    VIEW-AS ALERT-BOX ERROR.

    RETURN.
  END.

  PAUSE 0 BEFORE-HIDE.
  DEFINE FRAME StatusFrame
    vNum  AS INTEGER   VIEW-AS TEXT FORMAT   ">>9" AT ROW 1 COL  1 LABEL  "#  ":U
    vSize AS CHARACTER VIEW-AS TEXT FORMAT  "x(4)" AT ROW 1 COL  5 LABEL "Size":U
    vTime AS CHARACTER VIEW-AS TEXT FORMAT  "x(4)" AT ROW 1 COL 11 LABEL "Time":U
    vText AS CHARACTER VIEW-AS TEXT FORMAT "x(64)" AT ROW 1 COL 16 LABEL
                                                              "Promon Log File":U
  WITH 20 DOWN NO-LABELS NO-UNDERLINE NO-BOX THREE-D.


  RUN CreateWindow(14, 80, SUBSTITUTE(" Loading &1 Promon Log Files ", 
                                      STRING(vInputLogId))).
  VIEW CURRENT-WINDOW.
  VIEW FRAME StatusFrame IN WINDOW CURRENT-WINDOW.

  ASSIGN vInputLogId = 0.
  FOR EACH ttOSFile WITH FRAME StatusFrame:
    ASSIGN vInputLogId = vInputLogId + 1
           vDispName = SUBSTRING(ttOSFile.osFilePath, vRootDirSize)
           vDispName = IF LENGTH(vDispName) LE 64 THEN vDispName ELSE
                       SUBSTRING(vDispName, 1, 30) + "...":U +
                       SUBSTRING(vDispName, LENGTH(vDispName) - 30)
           FILE-INFO:FILE-NAME = ttOSFile.osFilePath
           hWidget = vTime:HANDLE
           vETime = ETIME
    . /*ASSIGN */
  
    DISPLAY vInputLogId @ vNum 
            Bytes2Str(FILE-INFO:FILE-SIZE) @ vSize
            "":U        @ vTime
            vDispName   @ vText.
    . /* DISPLAY */
    COLOR DISPLAY Gray vText.

    RUN LoadPromonLog(vInputLogId, ttOSFile.osFilePath, hWidget).

    COLOR DISPLAY Black vText.
    ASSIGN hWidget:SCREEN-VALUE = SUBSTRING(STRING((ETIME - vETime) / 1000.0), 1, 4).

    IF FRAME-LINE(StatusFrame) = FRAME-DOWN(StatusFrame)
    THEN UP FRAME-LINE(StatusFrame) - 1.
    ELSE DOWN 1.

    PROCESS EVENTS.
  END. /* FOR EACH ttOSFile */
  HIDE ALL.
  RUN DeleteWindow.

  DEFINE FRAME ProcessFrame
    vNum  VIEW-AS TEXT FORMAT   ">>9" AT ROW 1 COL  1 LABEL  "#  ":U
    vTime VIEW-AS TEXT FORMAT  "x(4)" AT ROW 1 COL  5 LABEL "Time":U
    vText VIEW-AS TEXT FORMAT "x(32)" AT ROW 1 COL 10 LABEL "Procedure":U
  WITH 15 DOWN NO-LABELS NO-UNDERLINE NO-BOX THREE-D.

  RUN CreateWindow(11, 43, " Running: ").
  VIEW CURRENT-WINDOW.
  VIEW FRAME ProcessFrame IN WINDOW CURRENT-WINDOW.

  ASSIGN vProcList =  "ProcessFirstSnapshots":U
          /*  2 */ + ",ProcessOperationsByType":U
          /*  3 */ + ",ProcessOperationsByFile":U
          /*  4 */ + ",ProcessServiceManager":U
          /*  5 */ + ",CheckStaticProperties":U
          /*  6 */ + ",ReportStaticProperties":U
          /*  7 */ + ",ReportStatusProperties":U
          /*  8 */ + ",ReportActivityProperties":U
          /*  9 */ + ",ReportResourceProperties":U
          /* 10 */ + ",ReportActivityLatchCounts":U
          /* 11 */ + ",ReportArrayMenus":U
          /* 12 */ + ",ReportStatActiveTrans":U
          /* 13 */ + ",ReportStatBufferLocks":U
          /* 14 */ + ",ReportStatBlockedClients":U
          /* 15 */ + ",ReportStatBufferLockQueue":U
  . /* ASSIGN */

  DO i = 1 TO NUM-ENTRIES(vProcList) WITH FRAME ProcessFrame:
    ASSIGN vProcName = ENTRY(i, vProcList)
           vETime    = ETIME
    . /*ASSIGN */

    DISPLAY i         @ vNum 
            "":U      @ vTime
            vProcName @ vText.
    . /* DISPLAY */

    IF vProcName BEGINS "Report":U
    THEN RUN VALUE(vProcName) (vOutputPrefix).
    ELSE RUN VALUE(vProcName).

    DISPLAY
      SUBSTRING(TRIM(STRING((ETIME - vETime) / 1000.0, ">>>9.99":U)), 1, 4)
      @ vTime
    . /* DISPLAY */

    IF FRAME-LINE(ProcessFrame) = FRAME-DOWN(ProcessFrame)
    THEN UP FRAME-LINE(ProcessFrame) - 1.
    ELSE DOWN 1.

    PROCESS EVENTS.
  END. /* DO i = 1 TO ... */

  MESSAGE
    "See the files:" SKIP
    vOutputPrefix + "*.txt":U
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

  RUN DeleteWindow.

END PROCEDURE. /* LoadPromonLogs */

/* ------------------------------------------------------------------------- */

PROCEDURE LoadPromonLog:

  DEFINE INPUT PARAMETER ipInputLogId AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipInputLog   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER iphPercent   AS HANDLE    NO-UNDO.

  DEFINE BUFFER bfCurrMenu FOR ttMenu.

  DEFINE VARIABLE vZoneSize   AS INTEGER   NO-UNDO INITIAL 8192.
  DEFINE VARIABLE vFileSize   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vFleaZone   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTempFile   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDatabaseId AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vSnapshotId AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMenuName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vCurrDate   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vCurrTime   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vLogType    AS CHARACTER NO-UNDO. /* Promon/Status/Latches */
  DEFINE VARIABLE vLine       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFeed       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i           AS INTEGER   NO-UNDO.

  INPUT FROM VALUE(ipInputLog).

/* The "fleas" ("Database was disconnected" messages) used to be found close
   to the end of promon log.
   Statistics: 130 real dbmon logs with "Database was disconnected. (1015)"
               Average offset from the end: 2285 bytes.
               9 offsets are greater than 4K
               Max offset is 7991 bytes.
   So let's set the size of Quarantine Zone to 8192.
*/
  SEEK INPUT TO END.
  ASSIGN vFileSize = SEEK(INPUT)
         vFleaZone = vFileSize - vZoneSize
         vTempFile = SESSION:TEMP-DIRECTORY + "LoadDbmon.tmp":U
  . /* ASSIGN */
  SEEK INPUT TO 0.

  RUN ParseLogHeader(INPUT  ipInputLog,
                     INPUT  ipInputLogId,
                     OUTPUT vDatabaseId,
                     OUTPUT vSnapshotId,
                     OUTPUT vCurrDate,
                     OUTPUT vCurrTime,
                     OUTPUT vLogType).

  IF vLogType NE "Main":U THEN
  DO:
    RUN LoadSpecialLog(vDatabaseId, ipInputLogId, vLogType).
    INPUT CLOSE.
    RETURN.
  END.
  
/* Reset the menu's last date/time: */
  FOR EACH ttMenu NO-LOCK:
    ASSIGN ttMenu.MenuTime = ?.
  END.

ImportLine:
  REPEAT WHILE SEEK(INPUT) NE ?: /* import from promon output file */

/* When promon was kicked out from database then the message 1015
   will break a line in promon's log. Example of the broken log:
12/21/
Database /path/to/dbname was disconnected. (1015)
17        Activity: I/O Operations by File

  But it should be:
12/21/17        Activity: I/O Operations by File

Flea Killer: copy the tail of input file to a temp file and remove the "fleas".
*/  IF SEEK(INPUT) GT vFleaZone THEN
    DO:
      OUTPUT TO VALUE(vTempFile).

      ASSIGN vFleaZone = ? /* To create Quarantine Zone only once per file.*/
             vLine = "":U
      . /* ASSIGN */
      REPEAT ON ENDKEY UNDO, LEAVE:
        PUT UNFORMATTED vLine.

        ASSIGN vLine = "":U.
        IMPORT UNFORMATTED vLine.

        IF FleaDetected(vLine) THEN
        ASSIGN vLine = "":U /* skip the current line   */
               vFeed = "":U /* skip the next line feed */
        . /* ASSIGN */
        ELSE
        ASSIGN vLine = vFeed + vLine
               vFeed = "~n":U
        . /* ASSIGN */
      END. /* REPEAT */

      PUT UNFORMATTED vLine "~n":U.

      OUTPUT CLOSE.
      INPUT  CLOSE. /* already closed ON ENDKEY */
      INPUT FROM VALUE(vTempFile).
/* Now the IMPORT will be done from temp file instead of the original one. */
    END. /* IF SEEK(INPUT) GT vFleaZone */

    ASSIGN vLine = "":U.
    IMPORT UNFORMATTED vLine.

    IF vLine EQ "":U THEN
    NEXT ImportLine.

    ASSIGN vMenuName = "":U.

/* Promon High Level:
    1.  User Control
    2.  Locking and Waiting Statistics
    3.  Block Access
    4.  Record Locking Table
    5.  Activity
    6.  Shared Resources
    7.  Database Status
    8.  Shut Down Database
    9.  Currently Connected Tenants
*/  IF vLine BEGINS "User Control:":U
    OR vLine BEGINS "Shared Resources:":U
    OR vLine BEGINS "Database Status:":U THEN
    ASSIGN i         = INDEX(vLine, ":":U)
           vMenuName = SUBSTRING(vLine, 1, i - 1)
    . /* ASSIGN */
    ELSE

/* 09:16:50        Adjust Latch Options */
    IF vLine MATCHES "..:..:.. *":U THEN
    ASSIGN vCurrTime =      SUBSTRING(vLine, 1, 8)
           vMenuName = TRIM(SUBSTRING(vLine, 10))
    . /* ASSIGN */
    ELSE
/*
12/20/17        Activity: Summary
12/19/17        Status: Database
%-16sActivity: %s
%-16sStatus: %s %s %s
*/  IF vLine MATCHES "../../.. *":U THEN
    DO:
      ASSIGN vCurrDate =      SUBSTRING(vLine, 1, 8)
             vMenuName = TRIM(SUBSTRING(vLine, 10))
      . /* ASSIGN */

      IMPORT UNFORMATTED vLine.

      IF NOT vLine MATCHES "..:..:..*":U THEN
      NEXT ImportLine. /* It's an unexpected situation. */

      ASSIGN vCurrTime =  SUBSTRING(vLine, 1, 8)
             vLine = TRIM(SUBSTRING(vLine, 10))
      . /* ASSIGN */
/*
12/20/17        Activity: Summary
09:16:50        12/20/17 09:13 to 12/20/17 09:16 (3 min 46 sec)
or
12/31/17        OpenEdge Release 11 Monitor (R&D)
15:40:57        View Database-Request Statement Cache

12/31/17        OpenEdge Release 11 Monitor (R&D)
15:56:53        Check Active Transaction Status
*/    IF vLine MATCHES "../../.. ..:.. to ../../.. ..:.. (* *)":U
      THEN ASSIGN vLine = SUBSTRING(vLine, 34). /* "(sample interval)" */
      ELSE ASSIGN vMenuName = vLine WHEN vLine NE "":U.
    END. /* IF vLine MATCHES "../../.. *" */

    IF vMenuName EQ "":U THEN
    NEXT ImportLine.

/* Cut off "by user number for all tenants": */
/*
12/31/17        Status: Active Transactions by user number for all tenants
*/  ASSIGN i = INDEX(vMenuName, " by user number":U).
    ASSIGN vMenuName = TRIM(SUBSTRING(vMenuName, 1, i - 1)) WHEN i GT 1
           i = vSnapshotId.

/* Create bfCurrMenu when needed and set vSnapshotId: */

    RUN SetCurrMenu(INPUT  vDatabaseId,
                    INPUT  ipInputLogId,
                    INPUT  vMenuName,
                    INPUT  vCurrDate,
                    INPUT  vCurrTime,
                    INPUT  vLine,       /* "(sample interval)" */
                    OUTPUT vSnapshotId,
                    BUFFER bfCurrMenu).

/* Display the percentage of current load with StatusFrame frame: */
    IF vSnapshotId NE i AND vSnapshotId MOD 10 EQ 0 THEN
    ASSIGN iphPercent:SCREEN-VALUE = RIGHT-TRIM(SUBSTRING(STRING(
                      SEEK(INPUT) / vFileSize * 100.0), 1, 3), ".":U) + "%":U
    . /* ASSIGN */

    CASE vMenuName:
/* Activity: Summary: */
      WHEN /* R&D/2/1. */  "Activity: Summary":U THEN
      RUN LoadActivitySummary(INPUT  vDatabaseId,
                              INPUT ipInputLogId,
                              INPUT  vSnapshotId,
                              BUFFER bfCurrMenu).

/* 'Latch' menu: */
      WHEN /* R&D/6/11. */ "Activity: Latch Counts":U THEN
      RUN LoadActivityLatchCounts(INPUT  vDatabaseId,
                                  INPUT ipInputLogId,
                                  INPUT  vSnapshotId,
                                  BUFFER bfCurrMenu).

/* 'Resource' menus: */
      WHEN /* R&D/6/8. */  "Activity: Resource Queues":U     OR
      WHEN /* R&D/6/9. */  "Activity: TXE Lock Activity":U THEN
      RUN LoadResourceMenu(INPUT  vDatabaseId,
                           INPUT ipInputLogId,
                           INPUT  vSnapshotId,
                           BUFFER bfCurrMenu).

/* 'Array' menus: */
      WHEN /* promon/1.*/  "User Control":U                                OR
/*  1. Status Displays ... ----------------------------------------------- */
      WHEN /* R&D/1/3. */  "Status: Servers":U                             OR
      WHEN /* R&D/1/5. */  "Status: Files":U                               OR
      WHEN /* R&D/1/6. */  "Status: Lock Table":U                          OR
      WHEN /* R&D/1/14.*/  "Status: Shared Memory Segments":U              OR
      WHEN /* R&D/1/15.*/  "Status: AI Extents":U                          OR
      WHEN /* R&D/1/17.*/  "Status: Servers By Broker":U                   OR
      WHEN /* R&D/1/19.*/  "Status: Schema Locks & Wait Queue":U           OR
/*  1/4. Processes/Clients ... ------------------------------------------- */
      WHEN /* R&D/1/4/1.*/ "Status: All Processes":U                       OR
      WHEN /* R&D/1/4/2.*/ "Status: Blocked Clients":U                     OR
      WHEN /* R&D/1/4/3.*/ "Status: Active Transactions":U                 OR
      WHEN /* R&D/1/4/4.*/ "Status: Local Clients":U                       OR
      WHEN /* R&D/1/4/5.*/ "Status: Batch Clients":U                       OR
      WHEN /* R&D/1/4/6.*/ "Status: Remote Clients":U                      OR
      WHEN /* R&D/1/4/7.*/ "Status: Background Processes":U                OR
      WHEN /* R&D/1/4/8.*/ "Currently Connected Tenants":U                 OR
      WHEN /* R&D/1/4/9.*/ "Status: User Notification Processes":U         OR
/*  1/18. Client Database-Request Statement Cache ... -------------------- */
      WHEN /* R&D/1/18/7. */ "View Database-Request Statement Cache":U     OR
/*   !!! Warning:  In V10.2B "Database statement cache locks" is # 18/9. */
      WHEN /* R&D/1/18/10.*/ "Database statement cache locks":U            OR
/*  3. Other Displays ...   ---------------------------------------------- */
      WHEN /* R&D/3/2. */  "I/O Operations by Process":U                   OR
      WHEN /* R&D/3/3. */  "Lock Requests By User":U                       OR
      WHEN /* R&D/3/4. */  "Checkpoints":U                                 OR
      WHEN /* R&D/3/5. */  "Activity: I/O Operations by User by Table":U   OR
      WHEN /* R&D/3/6. */  "Activity: I/O Operations by User by Index":U   OR
      WHEN /* R&D/3/7. */  "Status: Total Locks per User":U                OR
/*  4. Administrative Functions Menu ------------------------------------- */
      WHEN /* R&D/4/1. */  "Check Active Transaction Status":U             OR
      WHEN /* R&D/4/13.*/  "Status: Caches":U                              OR
/*  6. This menu is not here --------------------------------------------- */
      WHEN /* R&D/6/1. */  "Status: Cache Entries":U                       OR
      WHEN /* R&D/6/2. */  "Status: Hash Chain":U                          OR
      WHEN /* R&D/6/3. */  "Status: Page Writer Queue":U                   OR
      WHEN /* R&D/6/4. */  "Status: Lru Chains":U                          OR
      WHEN /* R&D/6/5. */  "Status: Locked Buffers":U                      OR
      WHEN /* R&D/6/6. */  "Status: Buffer Locks":U                        OR
      WHEN /* R&D/6/7. */  "Status: Buffer Use Counts":U                   OR
      WHEN /* R&D/6/13.*/  "Activity: I/O Wait Time by Type":U             OR
      WHEN /* R&D/6/15.*/  "Status: Buffer Lock Queue":U                   OR
      WHEN /* R&D/6/16.*/  "Semaphores":U                                THEN
      RUN LoadArrayMenu (INPUT  vDatabaseId,
                         INPUT ipInputLogId,
                         INPUT  vSnapshotId,
                         BUFFER bfCurrMenu).

/* Status menus without "Status:" prefix in their names: */
      WHEN /* promon/6.*/  "Shared Resources:":U                           OR
      WHEN /* promon/7.*/  "Database Status:":U                            OR
/*  4. Administrative Functions Menu ------------------------------------- */
      WHEN /* R&D/4/4. */ "Adjust Latch Options":U                         OR
      WHEN /* R&D/4/5. */ "Adjust Page Writer Options":U                   OR
      WHEN /* R&D/4/7. */ "Server Options":U                               OR
      WHEN /* R&D/4/8. */ "Enable/Disable block level consistency check":U OR
/*  6. This menu is not here --------------------------------------------- */
      WHEN /* R&D/6/10.*/ "Adjust TXE Options":U                       THEN
      RUN LoadStatusMenu(INPUT  vDatabaseId,
                         INPUT ipInputLogId,
                         INPUT  vSnapshotId,
                         BUFFER bfCurrMenu).

      OTHERWISE
      IF vMenuName BEGINS "Status: ":U THEN
      RUN LoadStatusMenu(INPUT  vDatabaseId,
                         INPUT ipInputLogId,
                         INPUT  vSnapshotId,
                         BUFFER bfCurrMenu).

      ELSE
      IF vMenuName BEGINS "Activity: ":U  THEN
      RUN LoadActivityMenu(INPUT  vDatabaseId,
                           INPUT ipInputLogId,
                           INPUT  vSnapshotId,
                           BUFFER bfCurrMenu).
    END CASE. /* vMenuName */

    PROCESS EVENTS.

  END. /* ImportLine: REPEAT WHILE SEEK(INPUT) NE ? */
  INPUT CLOSE.

  OS-DELETE VALUE(vTempFile).

END PROCEDURE. /* LoadPromonLog */

/* ------------------------------------------------------------------------- */

PROCEDURE ParseLogHeader:

  DEFINE  INPUT PARAMETER ipInputLog   AS CHARACTER NO-UNDO.
  DEFINE  INPUT PARAMETER ipInputLogId AS INTEGER   NO-UNDO.
  DEFINE OUTPUT PARAMETER opDatabaseId AS INTEGER   NO-UNDO.
  DEFINE OUTPUT PARAMETER opSnapshotId AS INTEGER   NO-UNDO.
  DEFINE OUTPUT PARAMETER opLogDate    AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opLogTime    AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opLogType    AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vDbHost   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDbName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDbPath   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDateTime AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vOffset   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLine     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i         AS INTEGER   NO-UNDO.
  DEFINE VARIABLE n         AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMonths   AS CHARACTER NO-UNDO INITIAL
                           "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec":U.
  ASSIGN opLogType = "Main":U /* default value */
         vDbHost   = ?
         vDbName   = ?
         opLogDate = ?
         opLogTime = ?
  . /* ASSIGN */

/* Parse the log file name as a draft source of information: */
  ASSIGN i     = MAX(R-INDEX(ipInputLog, "/":U), R-INDEX(ipInputLog, "~\":U))
         vLine = SUBSTRING(ipInputLog, i + 1)
         n     = NUM-ENTRIES(vLine, ".":U).
/* 
Log naming convention in dbmon V3:
dbmon.<dbhost>.<yymmdd_hhmmss>.<dbname>.promon.log
dbmon.<dbhost>.<yymmdd_hhmmss>.<dbname>.latches.log
dbmon.<dbhost>.<yymmdd_hhmmss>.<dbname>.<status_Buffer_Lock_Queue>.log

Note: dbhost can contain the dots (".") 
*/
  IF n GE 6
  AND ENTRY(1, vLine, ".":U) EQ "dbmon":U
  AND ENTRY(n - 3, vLine, ".":U) MATCHES "......_......":U THEN
  ASSIGN opLogType = ENTRY(n - 1, vLine, ".":U)
         vDbName   = ENTRY(n - 2, vLine, ".":U)
         opLogTime = ENTRY(n - 3, vLine, ".":U)
         vDbHost   = ENTRY(    2, vLine, ".":U)
/*       vDbHost   = SUBSTRING(vLine, 7, LENGTH(vLine)
/*          File extension ("log"): */ - LENGTH(ENTRY(n, vLine, ".":U))
/*       <status_Buffer_Lock_Queue> */ - LENGTH(opLogType)
/*  <yymmdd_hhmmss>.<dbname> + dots */ - LENGTH(vDbName) - 23)
*/        opLogType = ENTRY(1, opLogType, "_":U)
  . /* ASSIGN */
  ELSE
/*
Log naming convention in dbmon V2:
<dbname>.promon.<yymmdd_hhmmss>.log
<dbname>.promon.<yymmdd_hhmmss>.<Status_Buffer_Lock_Queue>.log
*/
  IF n GE 4
  AND ENTRY(2, vLine, ".":U) EQ "promon":U
  AND ENTRY(3, vLine, ".":U) MATCHES "......_......":U THEN
  ASSIGN vDbHost   = ?
         vDbName   = ENTRY(1, vLine, ".":U)  WHEN vDbName EQ ?
         opLogTime = ENTRY(3, vLine, ".":U)
  NO-ERROR. /* ASSIGN */

/* vTime = "yymmdd_hhmmss": */
  ASSIGN opLogDate = SUBSTITUTE("&1/&2/&3":U, /* to "month-day-year" order */
                   /* month  */ SUBSTRING(opLogTime,  3, 2),
                   /* day    */ SUBSTRING(opLogTime,  5, 2),
                   /* year   */ SUBSTRING(opLogTime,  1, 2))
         opLogTime = SUBSTITUTE("&1:&2:&3":U,
                  /* hours   */ SUBSTRING(opLogTime,  8, 2),
                  /* minutes */ SUBSTRING(opLogTime, 10, 2),
                  /* seconds */ SUBSTRING(opLogTime, 12, 2))
         vOffset = SEEK(INPUT)
         n = 0
  . /* ASSIGN */

/* Parse first 10 lines of the log file (it's expected to be a header): */
  IF vOffset NE ? THEN
  REPEAT ON ENDKEY UNDO, LEAVE WHILE n LT 10:

    ASSIGN vLine = "":U.
    IMPORT UNFORMATTED vLine.
    ASSIGN vLine = TRIM(vLine) n = n + 1.

    IF vLine EQ ".":U
    OR vLine EQ "":U THEN /* the end of the log's header */
    DO:
      ASSIGN vOffset = SEEK(INPUT).
      LEAVE.
    END.
/*
dbmon.sh Release 3.1.4, Dec 16, 2017
Main promon
   DB: /path/to/dbname
 Date: Wed Dec 20 09:16:49 2017
 Host: AIX hostname 1 7 00CF14F74C00
Intrv: 2 sec
Count: 10
Total: 20 sec
.

Promon (1800 sec x 46) started at Mon Dec 11 04:00:22 2017
dbmon.sh Release 2.0, Mar 23, 2012
OpenEdge Release 10.2B0864  as of Wed Sep 28 09:50:54 EDT 2016
Database: /path/to/dbname
Host: AIX hostname 1 7 00CF14C74C00
*/
    IF vLine MATCHES "Promon * started at *":U THEN
    ASSIGN vLine = "Date: ":U 
                 + SUBSTRING(vLine, INDEX(vLine, " started at ":U) + 12)
    . /* ASSIGN */

/* Date: Wed Dec 20 09:16:49 2017 */
    IF vLine BEGINS "Date: ":U THEN
    ASSIGN vLine = TRIM(SUBSTRING(vLine, 7))
           vLine = REPLACE(vLine, "  ":U, " ":U)
           opLogTime = ENTRY(4, vLine, " ":U)
           opLogDate = SUBSTITUTE("&1/&2/&3":U, /* to "month-day-year" order */
           /* month */ STRING(LOOKUP(ENTRY(2, vLine, " ":U), vMonths), "99":U),
           /* day   */ STRING(INTEGER(ENTRY(3, vLine, " ":U)), "99":U),
           /* year  */ ENTRY(5, vLine, " ":U))
    NO-ERROR. /* ASSIGN */
    ELSE

/*  DB: /path/to/dbname */
    IF vLine BEGINS "DB: ":U
    OR vLine BEGINS "Database: ":U THEN
    ASSIGN vDbName = ENTRY(2, vLine, " ":U).
    ELSE

/* Dbmon log header:
Host: AIX progress-tst1 1 7 00CF14F74C00 */
    IF vLine BEGINS "Host: ":U THEN
    ASSIGN vDbHost = ENTRY(3, vLine, " ":U).
    ELSE

    IF vLine BEGINS "Status promon":U  THEN
    ASSIGN opLogType = "Status":U.
    ELSE 

    IF vLine BEGINS "Latch promon":U  THEN
    ASSIGN opLogType = "Latches":U.
    ELSE

    IF vLine BEGINS "Main promon":U  THEN
    ASSIGN opLogType = "Main":U.

  END. /* REPEAT WHILE n LT 10 */

/* If still failed to find some of the fields in the log's header
   then parse the contents of the log as the last resort :
*/
  IF vOffset GT 0 THEN
  REPEAT ON ENDKEY UNDO, LEAVE
  WHILE vDbHost   EQ ?
     OR vDbName   EQ ?
     OR opLogTime EQ ?:

    ASSIGN vLine = "":U.
    IMPORT UNFORMATTED vLine.
    ASSIGN vLine = TRIM(vLine).

/* Shared Resources or Status: Startup Parameters:
Host Name (-H): Not Enabled */
    IF vLine BEGINS "Host Name (-H): ":U THEN
    ASSIGN vDbHost = TRIM(ENTRY(2, vLine, ":":U)) WHEN vDbHost EQ ?. /* !!! */
    ELSE
/*
12/19/17        Status: Files
                                       Size Extend
File name                              (KB) (KB)
/path/to/dbname.db                      640 512

12/19/17        Activity: I/O Operations by File
                        Total         Per Min          Per Sec          Per Tx
/path/to/dbname.db
*/  IF vLine + " ":U MATCHES "*~~~.db *":U THEN
    ASSIGN i        = INDEX(vLine + " ":U, ".db ":U)
           vLine    = SUBSTRING(vLine, 1, i - 1)
           i        = NUM-ENTRIES(vLine, " ":U)
           vDbName = ENTRY(i, vLine, " ":U)      WHEN vDbName EQ ? /* !!! */
      . /* ASSIGN */
    ELSE

/* 12/19/17        Status: Database */
    IF vLine MATCHES "../../.. *":U THEN
    ASSIGN opLogDate = SUBSTRING(vLine, 1, 8)    WHEN opLogDate EQ ?. /* !!! */
    ELSE

/* 14:30:39        Adjust Monitor Options */
    IF vLine MATCHES "..:..:.. *":U THEN
    ASSIGN opLogTime = SUBSTRING(vLine, 1, 8)    WHEN opLogTime EQ ?. /* !!! */

  END. /* REPEAT WHILE parse the contents of the log */

/* Re-open the file if it was closed: */
  IF vOffset GT 0 THEN
  DO:
    IF SEEK(INPUT) EQ ? THEN
    INPUT FROM VALUE(ipInputLog). 
    SEEK INPUT TO vOffset. /* reposition to the end of log header */
  END.

  ASSIGN vDateTime = DATETIME(
                /* month   */ INTEGER(SUBSTRING(opLogDate, 1, 2)),
                /* day     */ INTEGER(SUBSTRING(opLogDate, 4, 2)),
                /* year    */ INTEGER(SUBSTRING(opLogDate, 7, 2)) + 2000,
                /* hours   */ INTEGER(SUBSTRING(opLogTime, 1, 2)),
                /* minutes */ INTEGER(SUBSTRING(opLogTime, 4, 2)),
                /* seconds */ INTEGER(SUBSTRING(opLogTime, 7, 2)))
  NO-ERROR. /* ASSIGN */

  IF opLogTime EQ ? OR ERROR-STATUS:NUM-MESSAGES GT 0 THEN
  DO:
    MESSAGE 
      "Failed to find a date/time in" ipInputLog SKIP
      "Found:" opLogDate opLogTime SKIP
      "Execution is interrupted!"
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    QUIT.
  END.

  ASSIGN i = MAX(R-INDEX(vDbName, "/":U), R-INDEX(vDbName, "~\":U)).
  IF i GT 1 THEN
  ASSIGN vDbPath = SUBSTRING(vDbName, 1, i - 1)
         vDbName = SUBSTRING(vDbName, i + 1)
  . /* ASSIGN */
  ELSE
  ASSIGN vDbPath = ?.

/* If ttDatabase already exists: */
  FIND FIRST ttDatabase NO-LOCK /* INDEX DatabaseName */
       WHERE ttDatabase.DatabaseName EQ vDbName
         AND ttDatabase.DatabaseHost EQ vDbHost
         AND ttDatabase.DatabasePath EQ vDbPath
  NO-ERROR.

  IF NOT AVAILABLE ttDatabase THEN
  DO TRANSACTION:
    ASSIGN opDatabaseId = 0.
    FOR EACH ttDatabase NO-LOCK
          BY ttDatabase.DatabaseId DESCENDING:
      ASSIGN opDatabaseId = ttDatabase.DatabaseId + 1.
      LEAVE.
    END. /* FOR EACH ttDatabase  */

    CREATE ttDatabase.
    ASSIGN ttDatabase.DatabaseId   = opDatabaseId
           ttDatabase.DatabaseHost = vDbHost
           ttDatabase.DatabaseName = vDbName
           ttDatabase.DatabasePath = vDbPath
    . /* ASSIGN */
  END. /* DO TRANSACTION */

  ASSIGN opDatabaseId = ttDatabase.DatabaseId
         opSnapshotId = 0
  . /* ASSIGN */

/* Create new snapshot: */
  DO TRANSACTION:
    CREATE ttSnapshot.
    ASSIGN ttSnapshot.DatabaseId   = opDatabaseId
           ttSnapshot.InputLogId   = ipInputLogId
           ttSnapshot.SnapshotId   = opSnapshotId
           ttSnapshot.SnapshotTime = Str2DateTime(opLogDate, opLogTime)
           ttSnapshot.StatInterval = ?
    . /* ASSIGN */
  END. /* DO TRANSACTION */

END PROCEDURE. /* ParseLogHeader */

/* ------------------------------------------------------------------------- */

PROCEDURE SetCurrMenu:
  
  DEFINE  INPUT PARAMETER ipDatabaseId AS INTEGER   NO-UNDO.
  DEFINE  INPUT PARAMETER ipInputLogId AS INTEGER   NO-UNDO.
  DEFINE  INPUT PARAMETER ipMenuName   AS CHARACTER NO-UNDO.
  DEFINE  INPUT PARAMETER ipMenuDate   AS CHARACTER NO-UNDO.
  DEFINE  INPUT PARAMETER ipMenuTime   AS CHARACTER NO-UNDO.
  DEFINE  INPUT PARAMETER ipSample     AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opSnapshotId AS INTEGER   NO-UNDO.
  DEFINE PARAMETER BUFFER bfCurrMenu   FOR TEMP-TABLE ttMenu.

  DEFINE VARIABLE vMenuId       AS INTEGER  NO-UNDO.
  DEFINE VARIABLE vMenuTime     AS DATETIME NO-UNDO.
  DEFINE VARIABLE vSnapshotTime AS DATETIME NO-UNDO.
  DEFINE VARIABLE vMinDateTime  AS DATETIME NO-UNDO.
  DEFINE VARIABLE vMaxDateTime  AS DATETIME NO-UNDO.
  DEFINE VARIABLE vStatInterval AS INTEGER     NO-UNDO.

  ASSIGN vMenuTime = Str2DateTime(ipMenuDate, ipMenuTime).

  IF vMenuTime EQ ? THEN
  DO:
    MESSAGE
      "SetCurrMenu: Error parsing the menu's date/time:" SKIP
      ERROR-STATUS:GET-MESSAGE(1) SKIP
      "Menu:" ipMenuName          SKIP
      "Date:" ipMenuDate          SKIP
      "Time:" ipMenuTime          SKIP
      "Execution is interrupted!"
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    QUIT.
  END.

/* If it's not the first appearance of menu */
  FIND FIRST bfCurrMenu EXCLUSIVE-LOCK
       WHERE bfCurrMenu.MenuName EQ ipMenuName
  NO-ERROR.

  IF AVAILABLE bfCurrMenu THEN
  IF bfCurrMenu.MenuTime NE ? THEN
  DO:
    ASSIGN
      vStatInterval = IF ipSample MATCHES "*(* *)*":U 
                      THEN StatInterval(ipSample)
                      ELSE INTERVAL(vMenuTime, bfCurrMenu.MenuTime, "seconds":U)
/* Time range: a half of StatInterval before and after MenuTime: */
      vMinDateTime = ADD-INTERVAL(vMenuTime, - vStatInterval * 500 , "milliseconds":U)
      vMaxDateTime = ADD-INTERVAL(vMenuTime,   vStatInterval * 500 , "milliseconds":U)
      bfCurrMenu.MenuTime = vMenuTime
    . /* ASSIGN */

/* Choose an existing snapshot closest to a datetime of the menu: */
    FOR EACH ttSnapshot EXCLUSIVE-LOCK
       WHERE ttSnapshot.DatabaseId   EQ ipDatabaseId
         AND ttSnapshot.InputLogId   GE ipInputLogId
         AND ttSnapshot.SnapshotTime GE vMinDateTime
         AND ttSnapshot.SnapshotTime LE vMaxDateTime
/* Sorting to find an existing snapshot closest to a datetime of the menu
   Though it's expected there is only one snapshot in the range:
*/  BY ABS(INTERVAL(ttSnapshot.SnapshotTime, vMenuTime, "seconds":U)):
      ASSIGN ttSnapshot.StatInterval = vStatInterval
        WHEN ttSnapshot.StatInterval EQ ?
      . /* ASSIGN */
      ASSIGN opSnapshotId =  ttSnapshot.SnapshotId.
      RETURN.
    END. /* FOR EACH ttSnapshot */

/* If the existing snapshots do not match the criterions then create new one:*/
    ASSIGN   opSnapshotId = 0.
    FOR EACH ttSnapshot NO-LOCK /* INDEX SnapshotId */
       WHERE ttSnapshot.DatabaseId EQ ipDatabaseId
         AND ttSnapshot.InputLogId EQ ipInputLogId
          BY ttSnapshot.SnapshotId DESCENDING:
      ASSIGN opSnapshotId = ttSnapshot.SnapshotId + 1.
      LEAVE.
    END.

    DO TRANSACTION:
      CREATE ttSnapshot.
      ASSIGN ttSnapshot.DatabaseId   = ipDatabaseId
             ttSnapshot.InputLogId   = ipInputLogId
             ttSnapshot.SnapshotId   = opSnapshotId
             ttSnapshot.SnapshotTime = vMenuTime
             ttSnapshot.StatInterval = vStatInterval
           /*ttSnapshot.SinceStartup = FALSE*/
      . /* ASSIGN */
    END.

    RETURN.
  END. /* DO TRANSACTION */
/* 
  It's the first appearance of menu then and SnapshotId should be the last one.
*/
  ELSE
/*IF bfCurrMenu.MenuTime EQ ? THEN 
  It's the first appearance of the current menu for a new database:
*/
  ASSIGN bfCurrMenu.MenuTime = vMenuTime.

  ELSE
/*IF NOT AVAILABLE bfCurrMenu THEN
  It's the first appearance of menu:
*/
  DO:
    ASSIGN vMenuId = 0.
    FOR EACH ttMenu NO-LOCK /* INDEX MenuId */
          BY ttMenu.MenuId DESCENDING:
      ASSIGN vMenuId = ttMenu.MenuId + 1.
      LEAVE.
    END. /* FOR EACH ttMenu */

    CREATE bfCurrMenu.
    ASSIGN bfCurrMenu.MenuId   = vMenuId
           bfCurrMenu.MenuName = ipMenuName
           bfCurrMenu.MenuTime = vMenuTime
    . /* ASSIGN */

    CREATE ttSect.
    ASSIGN ttSect.MenuId   = vMenuId
           ttSect.SectId   = 0
           ttSect.SectName = "":U
    . /* ASSIGN */
  END. /* IF NOT AVAILABLE bfCurrMenu */

/* Choose the last SnapshotId: */
  FOR EACH ttSnapshot EXCLUSIVE-LOCK
     WHERE ttSnapshot.DatabaseId EQ ipDatabaseId
       AND ttSnapshot.InputLogId EQ ipInputLogId
  BY ABS(INTERVAL(ttSnapshot.SnapshotTime, vMenuTime, "seconds":U))
        BY ttSnapshot.SnapshotId DESCENDING:

    ASSIGN opSnapshotId = ttSnapshot.SnapshotId
           ttSnapshot.StatInterval = StatInterval(ipSample)
      WHEN ttSnapshot.StatInterval EQ ?
       AND ipSample MATCHES "*(* *)*":U 
    . /* ASSIGN */
    RETURN.
  END. /* FOR EACH ttSnapshot */

/* 
It was expected that there is at least SnapshotId 0 created by ParseLogHeader:
*/
  MESSAGE
    "Internal error in SetCurrMenu: ttSnapshot was not found!" SKIP
    "Execution is interrupted!"
  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  QUIT.

END PROCEDURE. /* SetCurrMenu */

/* ------------------------------------------------------------------------- */

PROCEDURE LoadStatusMenu:

  DEFINE INPUT PARAMETER  ipDatabaseId AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER  ipInputLogId AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER  ipSnapshotId AS INTEGER NO-UNDO.
  DEFINE PARAMETER BUFFER bfCurrMenu   FOR TEMP-TABLE ttMenu.

  DEFINE VARIABLE vPropertyName AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vPropertyRow  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vSectId       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vOffset       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLine         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i             AS INTEGER   NO-UNDO.

  ASSIGN vSectId = 0
         vPropertyRow = 0
  . /* ASSIGN */

ImportLine:
  REPEAT ON ENDKEY UNDO, RETURN:

    ASSIGN vLine = "":U vOffset = SEEK(INPUT).
    IMPORT UNFORMATTED vLine.
    ASSIGN vLine = TRIM(vLine).

/* Ignore all empty lines before the property's rows.
   Stop import after a first empty line below the property's rows.
*/  IF vLine EQ "":U THEN
    IF vPropertyRow GT 0 THEN RETURN.
                         ELSE NEXT ImportLine.

/* Stop import if the messages:
*** There are no client/server parameters for the primary broker. ***
*** Two phase commit is not enabled. ***
*** Before-image logging is not enabled. ***
*** After-image logging is not enabled. ***
*** Latch timing is not enabled. ***
*** I/O timing is not enabled. ***
**** No schema locks outstanding.
*/  IF vLine BEGINS "**":U THEN
    RETURN.

/* Stop import if the messages:
Latch Options cannot be adjusted without an Enterprise RDBMS License.
Server performance options cannot be adjusted without an Enterprise RDBMS License.
*/  IF vLine MATCHES "* License*":U THEN
    RETURN.

/* Stop import after a header of a next promon menu: */
    IF vLine MATCHES "../../..*":U
    OR vLine MATCHES "..:..:..*":U THEN
    DO:
      ASSIGN vOffset = MAX(vOffset - 8, 0). /* Padding 8 bytes */
      SEEK INPUT TO vOffset. /* Return to a line back*/
      RETURN.
    END.

/* Ignore the lines without ": ". Examples:
		Current consistency check status:
Registered Database Service Objects
Name                             Rdy Status  Messages Locked by
OpenEdge Replication Server       Y  RUN         3265
OpenEdge RDBMS                    Y  REG            0
*/  IF INDEX(vLine, ": ":U) LT 2 THEN
    NEXT ImportLine.

    ASSIGN vPropertyRow  = vPropertyRow + 1.

    CASE bfCurrMenu.MenuName:
      WHEN "Status: Broker Startup Parameters":U THEN
/* Broker: 0 Pid: 40501296 Logins: 0 Pend: 0 Connected: 0
*/    IF vLine MATCHES "Broker: * Pid: * Logins: * Pend: * Connected: *":U THEN
      DO:
/* Use a broker number as a section name: */
        ASSIGN vSectId = SectId(bfCurrMenu.MenuId, ENTRY(2, vLine, " ":U)).

/* Pid: 40501296 Logins: 0 Pend: 0 Connected: 0 */
        DO i = NUM-ENTRIES(vLine, " ":U) /* = 10 */ TO 4 BY -2
        TRANSACTION:
          CREATE ttStatus. 
          ASSIGN vPropertyName = RIGHT-TRIM(ENTRY(i - 1, vLine, " ":U), ":":U)
                 ttStatus.DatabaseId    = ipDatabaseId
                 ttStatus.InputLogId    = ipInputLogId
                 ttStatus.SnapshotId    = ipSnapshotId
                 ttStatus.InputLogId    = ipInputLogId
                 ttStatus.SnapshotTime  = bfCurrMenu.MenuTime
                 ttStatus.MenuId        = bfCurrMenu.MenuId
                 ttStatus.SectId        = vSectId
                 ttStatus.PropertyValue = ENTRY(i, vLine, " ":U)
                 ttStatus.PropertyId    = PropertyId(
                  "Status":U, bfCurrMenu.MenuId, vPropertyName, vPropertyRow)
          NO-ERROR. /* ASSIGN */

&IF NOT {&IgnoreErrors} &THEN
          IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
          MESSAGE
            SUBSTITUTE("Proc: ~"&1~", line: &2 (AAAA), error:",
            /*&1*/ ENTRY(1, PROGRAM-NAME(1), " ":U), /*&2*/ "{&LINE-NUMBER}") SKIP
            ERROR-STATUS:GET-MESSAGE(1)     SKIP
            "Database:" ipDatabaseId        SKIP
            "LogId:"    ipInputLogId        SKIP
            "Snapshot:" ipSnapshotId        SKIP
            "Time:"     bfCurrMenu.MenuTime SKIP
            "Menu:"     bfCurrMenu.MenuName SKIP
            "MenuId:"   bfCurrMenu.MenuId   SKIP
            "SectId:"   vSectId             SKIP
            "Row:"      vPropertyRow        SKIP
            "Line:"     vLine               SKIP
            "Property:" vPropertyName       SKIP
          VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
          IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
          DELETE ttStatus.
        END. /* DO i = */

        NEXT ImportLine.

      END. /* WHEN "Status: Broker Startup Parameters":U */
    END CASE. /* ipMenuName */

/* Total buffers: 20002 */

    DO TRANSACTION:
      CREATE ttStatus.
      ASSIGN i = INDEX(vLine, ": ":U)
             vPropertyName = TRIM(SUBSTRING(vLine, 1, i - 1))
  /* Adjust TXE Options
     Adjust Latch Options
     Adjust Page Writer Options
     Adjust Governor options
     Enable/Disable block level consistency check
     Server Options
  Their options begin with a number:
   1. Spins before timeout: 10000
   1. -MemCheck: disabled
  */         vPropertyName = LEFT-TRIM(vPropertyName, "0123456789. -":U)
             ttStatus.DatabaseId    = ipDatabaseId
             ttStatus.InputLogId    = ipInputLogId
             ttStatus.SnapshotId    = ipSnapshotId
             ttStatus.SnapshotTime  = bfCurrMenu.MenuTime
             ttStatus.MenuId        = bfCurrMenu.MenuId
             ttStatus.SectId        = vSectId
             ttStatus.PropertyValue = TRIM(SUBSTRING(vLine, i + 2))
             ttStatus.PropertyId    = PropertyId(
              "Status":U, bfCurrMenu.MenuId, vPropertyName, vPropertyRow)
      NO-ERROR. /* ASSIGN */

&IF NOT {&IgnoreErrors} &THEN
        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
        MESSAGE
          SUBSTITUTE("Proc: ~"&1~", line: &2 (BBBB), error:",
          /*&1*/ ENTRY(1, PROGRAM-NAME(1), " ":U), /*&2*/ "{&LINE-NUMBER}") SKIP
          ERROR-STATUS:GET-MESSAGE(1)     SKIP
          "Database:" ipDatabaseId        SKIP
          "LogId:"    ipInputLogId        SKIP
          "Snapshot:" ipSnapshotId        SKIP
          "Time:"     bfCurrMenu.MenuTime SKIP
          "Menu:"     bfCurrMenu.MenuName SKIP
          "MenuId:"   bfCurrMenu.MenuId   SKIP
          "SectId:"   vSectId             SKIP
          "Row:"      vPropertyRow        SKIP
          "Line:"     vLine               SKIP
          "Property:" vPropertyName       SKIP
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
        DELETE ttStatus.
    END. /* DO TRANSACTION */
  END. /* ImportLine: REPEAT */
END PROCEDURE. /* LoadStatusMenu */

/* ------------------------------------------------------------------------- */

PROCEDURE LoadActivityMenu:

  DEFINE INPUT PARAMETER  ipDatabaseId AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER  ipInputLogId AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER  ipSnapshotId AS INTEGER NO-UNDO.
  DEFINE PARAMETER BUFFER bfCurrMenu   FOR TEMP-TABLE ttMenu.

  DEFINE VARIABLE vPropertyName AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vPropertyRow  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vSectId       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vEmptyLines   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLastName     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLastItem     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vValueCnt     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vOffset       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vHead         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vItem         AS CHARACTER NO-UNDO EXTENT 20.

/*
11/01/17        Activity: Performance Indicators
                        Total         Per Min          Per Sec          Per Tx
Skip the header:
*/
  ASSIGN vHead = "":U.
  REPEAT ON ENDKEY UNDO, RETURN:
    IMPORT UNFORMATTED vHead.
    IF vHead MATCHES "*Total * Per Min * Per Sec * Per Tx*":U THEN
    LEAVE.
  END.

  ASSIGN vSectId      = 0
         vEmptyLines  = 0
         vPropertyRow = 0
  . /* ASSIGN */

ImportLine:
  REPEAT ON ENDKEY UNDO, RETURN:
    ASSIGN vItem = "":U vOffset = SEEK(INPUT).
    IMPORT vItem.

/* Ignore a single empty line anywhere.
   Stop import after two sequential empty lines:
*/  IF vItem[1] EQ "":U THEN
    DO:
      ASSIGN vEmptyLines = vEmptyLines + 1.
      IF vEmptyLines GE 2 THEN RETURN.

      NEXT ImportLine.
    END.

/* Stop import after a header of a next promon menu: */
    IF vItem[1] MATCHES "../../..":U
    OR vItem[1] MATCHES "..:..:..":U THEN
    DO:
      ASSIGN vOffset = MAX(vOffset - 8, 0). /* Padding 8 bytes */
      SEEK INPUT TO vOffset. /* Return to a line back*/
      RETURN.
    END.

/* The rows in Activity menu should have 4 last columns with the numbers:

03/15/15        Activity: Lock Table
09:25:01        12/05/14 05:12 to 03/15/15 09:25 (2404 hrs 13 min)

                                 Total      Per Min       Per Sec       Per Tx

Requests:
    Share                     38586603K      273912       4565.21         3.03
    Intent Share              69810937          484          8.07         0.01
    Exclusive                 26464442K      187862       3131.03         2.07
    Intent Exclusive            422704K        3001         50.01         0.03
    Share Intent Excl             2776            0          0.00         0.00
    Upgrade                     384137K        2727         45.45         0.03
    Record Get Lock           22767286K      161617       2693.61         1.79
    Table Lock                  440520K        3127         52.12         0.03
    Record Lock               42271619K      300071       5001.18         3.31

Grants:
    Share                     38615380K      274117       4568.61         3.03
    Intent Share              69810986          484          8.07         0.01
    Exclusive                 25737262K      182700       3044.99         2.02
    Intent Exclusive            422710K        3001         50.01         0.03
    Share Intent Excl             2776            0          0.00         0.00
    Upgrade                     382940K        2718         45.31         0.03
    Record Get Lock           22766596K      161612       2693.53         1.79
    Table Lock                  440502K        3127         52.12         0.03
    Record Lock               41526608K      294782       4913.04         3.26

Waits:
    Share                   64052129            444          7.40         0.00
    Intent Share                   0              0          0.00         0.00
    Exclusive                 728352K          5170         86.17         0.06
    Intent Exclusive               0              0          0.00         0.00
    Share Intent Excl              0              0          0.00         0.00
    Upgrade                     1040              0          0.00         0.00
    Record Get Lock          1349503              9          0.16         0.00
    Table Lock                     0              0          0.00         0.00
    Record Lock               727869K          5167         86.11         0.06

Requests Cancelled              4074              0          0.00         0.00
Downgrades                   1368334K          9713        161.89         0.11
Redundant Requests          15969813K        113364       1889.40         1.25
Shared Find                  9759231K         69277       1154.62         0.77
Exclusive Find              10065973K         71455       1190.91         0.79
*/

    ASSIGN vPropertyRow = vPropertyRow + 1
           vEmptyLines = 0
           vLastItem  = 1
    . /* ASSIGN */

ParseItem:
    REPEAT WHILE vLastItem LT EXTENT(vItem) AND vItem[vLastItem] NE "":U:

      ASSIGN vLastName     = vLastItem
             vPropertyName = vItem[vLastName]
      . /* ASSIGN */
      DO vLastItem = vLastName + 1 TO EXTENT(vItem)
      WHILE RIGHT-TRIM(vItem[vLastItem], "1234567890.KMGTP%":U) NE "":U:
        ASSIGN vLastName     = vLastItem
               vPropertyName = vPropertyName + " ":U + vItem[vLastName]
        . /* ASSIGN */
      END.

      ASSIGN vValueCnt = 0.
      DO vLastItem = vLastName + 1 TO EXTENT(vItem)
      WHILE vItem[vLastItem] NE "":U
        AND RIGHT-TRIM(vItem[vLastItem], "1234567890.KMGTP%":U) EQ "":U:
        ASSIGN vValueCnt = vValueCnt + 1.
      END.

      IF vValueCnt EQ 0 THEN
/* It's a new section in the Activity menu.
Example:
12/19/17        Activity: Buffer Cache
Database Buffer Pool
Primary Buffer Pool
Alternate Buffer Pool
*/    ASSIGN
        vPropertyRow = 0
        vSectId = SectId(bfCurrMenu.MenuId, RIGHT-TRIM(vPropertyName, ":":U))
      . /* ASSIGN */
      ELSE
      IF vValueCnt LE 2 THEN
      IF bfCurrMenu.MenuName EQ "Activity: Servers":U THEN
/* Activity for Server => SectName

12/11/17        Activity: Servers
04:00:22        12/10/17 02:20 to 12/11/17 04:00 (25 hrs 40 min)

                                 Total      Per Min       Per Sec       Per Tx

Messages received                    0            0          0.00         0.00
Messages sent                        0            0          0.00         0.00
Bytes received                       0            0          0.00         0.00
Bytes sent                           0            0          0.00         0.00
Records received                     0            0          0.00         0.00
Records sent                         0            0          0.00         0.00
Queries received                     0            0          0.00         0.00
Time slices                          0            0          0.00         0.00

Activity for Server 1
12/11/17        Activity: Servers
04:00:22        12/10/17 02:20 to 12/11/17 04:00 (25 hrs 40 min)

                                 Total      Per Min       Per Sec       Per Tx

Messages received               747511          485          8.09         0.17
Messages sent                   381929          248          4.13         0.08
Bytes received                  153951K      102374       1706.24        34.99
Bytes sent                    83257366        54067        901.11        18.48
Records received                   228            0          0.00         0.00
Records sent                    378874          246          4.10         0.08
Queries received                  1557            1          0.02         0.00
Time slices                       8775            6          0.09         0.00

Activity for Server 2
*/    DO:
/* Use a server number as a section name: */
        ASSIGN vSectId = SectId(bfCurrMenu.MenuId, vItem[vLastName + 1]).

        FOR EACH ttActivity EXCLUSIVE-LOCK /* INDEX ReportOrder */
           WHERE ttActivity.DatabaseId EQ ipDatabaseId
             AND ttActivity.InputLogId EQ ipInputLogId
             AND ttActivity.SnapshotId EQ ipSnapshotId
             AND ttActivity.MenuId     EQ bfCurrMenu.MenuId
             AND ttActivity.SectId     EQ 0:
          ASSIGN ttActivity.SectId = vSectId NO-ERROR.

&IF NOT {&IgnoreErrors} &THEN
          IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
          MESSAGE
            SUBSTITUTE("Proc: ~"&1~", line: &2 (CCCC), error:",
            /*&1*/ ENTRY(1, PROGRAM-NAME(1), " ":U),
            /*&2*/ "{&LINE-NUMBER}")        SKIP
            ERROR-STATUS:GET-MESSAGE(1)     SKIP
            "Database:" ipDatabaseId        SKIP
            "LogId:"    ipInputLogId        SKIP
            "Snapshot:" ipSnapshotId        SKIP
            "Time:"     bfCurrMenu.MenuTime SKIP
            "Menu:"     bfCurrMenu.MenuName SKIP
            "MenuId:"   bfCurrMenu.MenuId   SKIP
            "Sect:" vPropertyName vItem[vLastName + 1] SKIP
          VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
          IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
          LEAVE.
        END. /* FOR EACH ttActivity */
/* Activity for Server N is the last line on menu screen: */
        RETURN.
      END. /* IF ipMenuName EQ "Activity: Servers" */
      ELSE
/* It's a "Status" property in the Activity menu:
Examples:
12/19/17        Activity: Page Writers
Number of APWs: 4

12/19/17        Activity: Buffer Cache
Database buffer pool hit ratio:  95 %
*/    DO TRANSACTION: /* when vValueCnt LE 2 */
        CREATE ttStatus.
        ASSIGN vPropertyName = RIGHT-TRIM(vPropertyName, ":":U)
               ttStatus.DatabaseId    = ipDatabaseId
               ttStatus.InputLogId    = ipInputLogId
               ttStatus.SnapshotId    = ipSnapshotId
               ttStatus.SnapshotTime  = bfCurrMenu.MenuTime
               ttStatus.MenuId        = bfCurrMenu.MenuId
               ttStatus.SectId        = vSectId
               ttStatus.PropertyValue = vItem[vLastName + 1]
               ttStatus.PropertyValue = ttStatus.PropertyValue + " "
                                      + vItem[vLastName + 2] WHEN vValueCnt GE 2
               ttStatus.PropertyId    = PropertyId(
                "Status":U, bfCurrMenu.MenuId, vPropertyName, vPropertyRow)
               vLastName = vLastItem
        NO-ERROR. /* ASSIGN */

&IF NOT {&IgnoreErrors} &THEN
        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
        MESSAGE
          SUBSTITUTE("Proc: ~"&1~", line: &2 (DDDD), error:",
          /*&1*/ ENTRY(1, PROGRAM-NAME(1), " ":U), /*&2*/ "{&LINE-NUMBER}") SKIP
          ERROR-STATUS:GET-MESSAGE(1)     SKIP
          "Database:" ipDatabaseId        SKIP
          "LogId:"    ipInputLogId        SKIP
          "Snapshot:" ipSnapshotId        SKIP
          "Time:"     bfCurrMenu.MenuTime SKIP
          "Menu:"     bfCurrMenu.MenuName SKIP
          "MenuId:"   bfCurrMenu.MenuId   SKIP
          "SectId:"   vSectId             SKIP
          "Row:"      vPropertyRow        SKIP
          "Line:"     vItem[1] vItem[2] vItem[3] vItem[4] vItem[5] vItem[6] SKIP
          "Property:" vPropertyName       SKIP
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN /* This should not happen */
        DELETE ttStatus.                     /* but just in case.      */
      END. /* IF vValueCnt GE 1 AND vValueCnt LE 2 */
      ELSE
/*    IF vValueCnt GE 3 THEN */
/* It's a "Status" property in the Activity menu:
12/19/17        Activity: Buffer Cache
                                Total     Per Min      Per Sec      Per Tx
Logical reads                    2150         506         8.43        0.13
*/    DO TRANSACTION:
        CREATE ttActivity.
        ASSIGN ttActivity.DatabaseId       = ipDatabaseId
               ttActivity.InputLogId       = ipInputLogId
               ttActivity.SnapshotId       = ipSnapshotId
               ttActivity.MenuId           = bfCurrMenu.MenuId
               ttActivity.SectId           = vSectId
               ttActivity.PropertyValue[1] = String2Dec(vItem[vLastName + 1])
               ttActivity.PropertyValue[2] = DECIMAL   (vItem[vLastName + 3])
               ttActivity.PropertyId       = PropertyId(
                "Activity":U, bfCurrMenu.MenuId, vPropertyName, vPropertyRow)
        NO-ERROR. /* ASSIGN */
/*
Dbmon uses "Activity: I/O Operations by File" for sampling.
It creates the menu's outputs with same date/time.
Anyways I/O statistics for .db file is changing only at db startup
or after running prostrct addonline. Skip this statistics:
*/      &IF NOT {&IgnoreErrors} &THEN
        IF ERROR-STATUS:NUM-MESSAGES GT 0 
        AND bfCurrMenu.MenuName NE "Activity: I/O Operations by File":U 
          THEN
        MESSAGE
          SUBSTITUTE("Proc: ~"&1~", line: &2 (EEEE), error:",
          /*&1*/ ENTRY(1, PROGRAM-NAME(1), " ":U), /*&2*/ "{&LINE-NUMBER}") SKIP
          ERROR-STATUS:GET-MESSAGE(1)     SKIP
          "Database:" ipDatabaseId        SKIP
          "LogId:"    ipInputLogId        SKIP
          "Snapshot:" ipSnapshotId        SKIP
          "Time:"     bfCurrMenu.MenuTime SKIP
          "Menu:"     bfCurrMenu.MenuName SKIP
          "MenuId:"   bfCurrMenu.MenuId   SKIP
          "SectId:"   vSectId             SKIP
          "Row:"      vPropertyRow        SKIP
          "Line:"     vItem[1] vItem[2] vItem[3] vItem[4] vItem[5] vItem[6] SKIP
          "Property:" vPropertyName   SKIP
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
        DO:
          DELETE ttActivity.
          RETURN.
        END.

        LEAVE ParseItem. /* Ingore anything in vItem */
      END. /* IF vValueCnt GE 3  */
    END. /* ParseItem: REPEAT */
  END. /* ImportLine: REPEAT */

END PROCEDURE. /* LoadActivityMenu */

/* ------------------------------------------------------------------------- */

PROCEDURE LoadResourceMenu:

  DEFINE INPUT PARAMETER  ipDatabaseId AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER  ipInputLogId AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER  ipSnapshotId AS INTEGER NO-UNDO.
  DEFINE PARAMETER BUFFER bfCurrMenu   FOR TEMP-TABLE ttMenu.

  DEFINE VARIABLE vPropertyName AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vPropertyRow  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vEmptyLines   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLastName     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLastItem     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vValueCnt     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vOffset       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vHead         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vItem         AS CHARACTER NO-UNDO EXTENT 20.
  DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLockType     AS CHARACTER NO-UNDO EXTENT 4
    INITIAL ["Share", "Update", "Commit", "Excl"].
/*
03/15/15        Activity: Resource Queues
09:25:01        12/05/14 05:12 to 03/15/15 09:25 (2404 hrs 13 min)

Queue               - Requests -     ------- Waits -------   --- Wait usec ---
                    Total   /Sec     Total   /Sec      Pct     /Req    /Wait

03/15/15        Activity: TXE Lock Activity
09:25:01        12/05/14 05:12 to 03/15/15 09:25 (2404 hrs 13 min)

                 ------------ Requests ----  ----------------- Waits ---------
                                Total  /Sec                 Total  /Sec    Pct
Skip the header:
*/
  ASSIGN vHead = "":U.
  REPEAT ON ENDKEY UNDO, RETURN:
    IMPORT UNFORMATTED vHead.
    IF vHead MATCHES "*Total * /Sec * Total * /Sec * Pct*":U THEN
    LEAVE.
  END.

  ASSIGN vEmptyLines  = 0
         vPropertyRow = 0
  . /* ASSIGN */

/*
03/15/15        Activity: Resource Queues
09:25:01        12/05/14 05:12 to 03/15/15 09:25 (2404 hrs 13 min)

Queue               - Requests -     ------- Waits -------   --- Wait usec ---
                    Total   /Sec     Total   /Sec      Pct     /Req    /Wait

Shared Memory           0      0         0      0     0.00        0        0
Record Lock     43056939008   4975    414538      0     0.00        0        0
Schema Lock             0      0         0      0     0.00        0        0
Trans Commit    115520880     13      6839      0     0.00        0        0
DB Buf I Lock   5804334080    671  16519262      2     0.00        0        0
Record Get      23314946048   2694   1349640      0     0.00        0        0
DB Buf Read     5336923648    617         0      0     0.00        0        0
DB Buf Write    292333248     34         0      0     0.00        0        0
DB Buf Backup           0      0         0      0     0.00        0        0
DB Buf S Lock   775193296896  89564  48916596      6     0.00        0        0
DB Buf X Lock   5705388544    659  42063704      5     0.01        0        0
DB Buf Avail            0      0         0      0     0.00        0        0
DB Buf S Lock LRU2      0      0         0      0     0.00        0        0
DB Buf X Lock LRU2      0      0         0      0     0.00        0        0
DB Buf Write LRU2       0      0         0      0     0.00        0        0
BI Buf Read        801176      0         0      0     0.00        0        0
BI Buf Write    111982840     13  15043064      2     0.13        0        0
AI Buf Read          2864      0         0      0     0.00        0        0
AI Buf Write     90696480     10   3578484      0     0.04        0        0
TXE Share Lock  6372649984    736        21      0     0.00        0        0
TXE Update Lock 547939840     63   1740037      0     0.00        0        0
TXE Commit Lock 1228721280    142   8342896      1     0.01        0        0
TXE Excl Lock         103      0         1      0     0.01        0        0
Repl TEND Ack           0      0         0      0     0.00        0        0
DBSQ Send Lock          0      0         0      0     0.00        0        0
TDE Buffer              0      0         0      0     0.00        0        0
Statement Cache         0      0         0      0     0.00        0        0


03/15/15        Activity: TXE Lock Activity
09:25:01        12/05/14 05:12 to 03/15/15 09:25 (2404 hrs 13 min)

                 ------------ Requests ----  ----------------- Waits ---------
                                Total  /Sec                 Total  /Sec    Pct

Record Create               264678274    30                     7     0   0.00
Record Modify              2950767962   340               1278168     0   0.04
Record Delete               242586612    28                281075     0   0.12
Key Add                    1702478972   196                     1     0   0.00
Key Delete                 1719236140   198                134584     0   0.01
Sequence Update              30369027     3                     0     0   0.00
Other                        10473054     1                 46355     0   0.44
Prostrct Add                        0     0                     0     0   0.00
Replication Enable                  0     0                     0     0   0.00

 Share Locks               6372650085   736                    21     0   0.00
Update Locks                547939853    63               1740037     0   0.32
Commit Locks               1228721254   141               8342896     0   0.68
  Excl Locks                      103     0                     1     0   0.97
       Total               8149311295   941              10082955     1   0.12

# OpenEdge Release 10.2B0864  as of Wed Sep 28 09:50:54 EDT 2016
# OpenEdge Release 11.3.2 as of Mon Jan 27 16:10:46 EST 2014
Upgrade Requests:            188502610   Rate:    21    Pct:  2.96

# OpenEdge Release 11.5.1 as of Wed May  6 19:02:33 EDT 2015
Upgrade Requests                       1     0      Pct: 50.00
TXE Lock Retries                       0     0  Average:  0.00

Current  Share:     0 Update:     0 Commit:     0 Excl:     0
Current   Wait:     0   Wait:     0   Wait:     0 Wait:     0
*/
ImportLine:
  REPEAT ON ENDKEY UNDO, RETURN:

    ASSIGN vItem = "":U vOffset = SEEK(INPUT).
    IMPORT vItem.

/* Ignore all empty lines before the property's rows.
   Stop import after two sequential empty lines.
*/  IF vItem[1] EQ "":U THEN
    DO:
      ASSIGN vEmptyLines = vEmptyLines + 1.
      IF vEmptyLines GE 2 THEN RETURN.
      NEXT ImportLine.
    END.

/* Stop import after a header of a next promon menu: */
    IF vItem[1] MATCHES "../../..":U
    OR vItem[1] MATCHES "..:..:..":U THEN
    DO:
      ASSIGN vOffset = MAX(vOffset - 8, 0). /* Padding 8 bytes */
      SEEK INPUT TO vOffset. /* Return to a line back*/
      RETURN.
    END.

/* Reset the counter of the empty lines after any non-empty line: */
    ASSIGN vEmptyLines  = 0
           vPropertyRow = vPropertyRow + 1
    . /* ASSIGN */

/* Status properties:
Current  Share:     0 Update:     0 Commit:     0 Excl:     0
Current   Wait:     0   Wait:     0   Wait:     0 Wait:     0
*/  IF vItem[1] EQ "Current":U THEN
    DO:
      ASSIGN vPropertyName = " Locks" WHEN vItem[2] EQ "Share:":U
             vPropertyName = " Waits" WHEN vItem[2] EQ "Wait:":U
      . /* ASSIGN */

      DO TRANSACTION i = 1 TO 4:
        CREATE ttStatus.
        ASSIGN ttStatus.DatabaseId    = ipDatabaseId
               ttStatus.InputLogId    = ipInputLogId
               ttStatus.SnapshotId    = ipSnapshotId
               ttStatus.SnapshotTime  = bfCurrMenu.MenuTime
               ttStatus.MenuId        = bfCurrMenu.MenuId
               ttStatus.SectId        = 0
               ttStatus.PropertyValue = vItem[i * 2 + 1]
               ttStatus.PropertyId    = PropertyId("Status":U, 
                 bfCurrMenu.MenuId, vLockType[i] + vPropertyName, vPropertyRow)
        NO-ERROR. /* ASSIGN */

&IF NOT {&IgnoreErrors} &THEN
        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
        MESSAGE
          SUBSTITUTE("Proc: ~"&1~", line: &2 (FFFF), error:",
          /*&1*/ ENTRY(1, PROGRAM-NAME(1), " ":U), /*&2*/ "{&LINE-NUMBER}") SKIP
          ERROR-STATUS:GET-MESSAGE(1)     SKIP
          "Database:" ipDatabaseId        SKIP
          "LogId:"    ipInputLogId        SKIP
          "Snapshot:" ipSnapshotId        SKIP
          "Time:"     bfCurrMenu.MenuTime SKIP
          "Menu:"     bfCurrMenu.MenuName SKIP
          "MenuId:"   bfCurrMenu.MenuId   SKIP
          "Row:"      vPropertyRow        SKIP
          "Line:"     vItem[1] vItem[2] vItem[3] vItem[4] vItem[5] vItem[6] SKIP
          "Property:" vPropertyName       SKIP
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN /* This should not happen */
        DELETE ttStatus.                       /* but just in case.      */
      END. /* DO i = 1 TO 4 */

      IF vItem[1] EQ "Wait:":U THEN RETURN. ELSE
      NEXT ImportLine.
    END.

    ASSIGN vLastItem  = 1.


ParseItem:
    REPEAT WHILE vLastItem LT EXTENT(vItem) AND vItem[vLastItem] NE "":U:


      ASSIGN vLastName     = vLastItem
             vPropertyName = vItem[vLastName]
      . /* ASSIGN */
      DO vLastItem = vLastName + 1 TO EXTENT(vItem)
      WHILE RIGHT-TRIM(vItem[vLastItem], "1234567890.":U) NE "":U:
        ASSIGN vLastName     = vLastItem
               vPropertyName = vPropertyName + " ":U + vItem[vLastName]
        . /* ASSIGN */
      END.

      ASSIGN vValueCnt = 0.
      DO vLastItem = vLastName + 1 TO EXTENT(vItem)
      WHILE vItem[vLastItem] NE "":U
        AND RIGHT-TRIM(vItem[vLastItem], "1234567890.":U) EQ "":U:
        ASSIGN vValueCnt = vValueCnt + 1.
      END.

      IF vValueCnt LT 2 THEN
/* vValueCnt EQ 0 is unexpected for both "Resource" menus.
The properties with vValueCnt EQ 1 are ignoring:
Upgrade Requests:            188502610   Rate:    21    Pct:  2.96
*/    NEXT ImportLine.
      ELSE

      IF vValueCnt EQ 2 THEN
/* An "Activity" property in the "Activity: TXE Lock Activity" menu:
                    ------------ Requests ----
                                   Total  /Sec
Upgrade Requests                       1     0      Pct: 50.00
TXE Lock Retries                       0     0  Average:  0.00
*/    DO TRANSACTION:
        CREATE ttActivity.
        ASSIGN ttActivity.DatabaseId       = ipDatabaseId
               ttActivity.InputLogId       = ipInputLogId
               ttActivity.SnapshotId       = ipSnapshotId
               ttActivity.MenuId           = bfCurrMenu.MenuId
               ttActivity.SectId           = 0
               ttActivity.PropertyValue[1] = DECIMAL(vItem[vLastName + 1])
               ttActivity.PropertyValue[2] = DECIMAL(vItem[vLastName + 2])
               ttActivity.PropertyId       = PropertyId(
                "Activity":U, bfCurrMenu.MenuId, vPropertyName, vPropertyRow)
        NO-ERROR.  /* ASSIGN */

&IF NOT {&IgnoreErrors} &THEN
        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
        MESSAGE
          SUBSTITUTE("Proc: ~"&1~", line: &2 (GGGG), error:",
          /*&1*/ ENTRY(1, PROGRAM-NAME(1), " ":U), /*&2*/ "{&LINE-NUMBER}") SKIP
          ERROR-STATUS:GET-MESSAGE(1)     SKIP
          "Database:" ipDatabaseId        SKIP
          "LogId:"    ipInputLogId        SKIP
          "Snapshot:" ipSnapshotId        SKIP
          "Time:"     bfCurrMenu.MenuTime SKIP
          "Menu:"     bfCurrMenu.MenuName SKIP
          "MenuId:"   bfCurrMenu.MenuId   SKIP
          "Row:"      vPropertyRow        SKIP
          "Line:"     vItem[1] vItem[2] vItem[3] vItem[4] vItem[5] vItem[6] SKIP
          "Property:" vPropertyName       SKIP
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN /* This should not happen */
        DELETE ttActivity.                     /* but just in case.      */
      END. /* IF vValueCnt EQ 2 */
      ELSE

      IF vValueCnt GE 4 THEN
/* It's a "Resource" property:
03/15/15        Activity: Resource Queues
Queue               - Requests -     ------- Waits -------   --- Wait usec ---
                    Total   /Sec     Total   /Sec      Pct     /Req    /Wait
Shared Memory           0      0         0      0     0.00        0        0

03/15/15        Activity: TXE Lock Activity
                 ------------ Requests ----  ----------------- Waits ---------
                                Total  /Sec                 Total  /Sec    Pct
Record Create               264678274    30                     7     0   0.00
*/    DO TRANSACTION:
        CREATE ttResource.
        ASSIGN ttResource.DatabaseId   = ipDatabaseId
               ttResource.InputLogId   = ipInputLogId
               ttResource.SnapshotId   = ipSnapshotId
               ttResource.MenuId       = bfCurrMenu.MenuId
               ttResource.Requests[1]  = DECIMAL(vItem[vLastName + 1])
               ttResource.Requests[2]  = DECIMAL(vItem[vLastName + 2])
               ttResource.Waits   [1]  = DECIMAL(vItem[vLastName + 3])
               ttResource.Waits   [2]  = DECIMAL(vItem[vLastName + 4])
               ttResource.PropertyId   = PropertyId(
                "Resource":U, bfCurrMenu.MenuId, vPropertyName, vPropertyRow)
        NO-ERROR. /* ASSIGN */

&IF NOT {&IgnoreErrors} &THEN
        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
        MESSAGE
          SUBSTITUTE("Proc: ~"&1~", line: &2 (HHHH), error:",
          /*&1*/ ENTRY(1, PROGRAM-NAME(1), " ":U), /*&2*/ "{&LINE-NUMBER}") SKIP
          ERROR-STATUS:GET-MESSAGE(1)     SKIP
          "Database:" ipDatabaseId        SKIP
          "LogId:"    ipInputLogId        SKIP
          "Snapshot:" ipSnapshotId        SKIP
          "Time:"     bfCurrMenu.MenuTime SKIP
          "Menu:"     bfCurrMenu.MenuName SKIP
          "MenuId:"   bfCurrMenu.MenuId   SKIP
          "Row:"      vPropertyRow        SKIP
          "Line:"     vItem[1] vItem[2] vItem[3] vItem[4] vItem[5] vItem[6] SKIP
          "Property:" vPropertyName       SKIP
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN /* This should not happen */
        DELETE ttResource.                     /* but just in case.      */
      END. /* IF vValueCnt GE 4  */

      NEXT ImportLine. /* Parse only one property per line */

    END. /* ParseItem: REPEAT */
  END. /* ImportLine: REPEAT */

END PROCEDURE. /* LoadResourceMenu */

/* ------------------------------------------------------------------------- */

PROCEDURE LoadArrayMenu:

  DEFINE INPUT PARAMETER  ipDatabaseId AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER  ipInputLogId AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER  ipSnapshotId AS INTEGER NO-UNDO.
  DEFINE PARAMETER BUFFER bfCurrMenu   FOR TEMP-TABLE ttMenu.

/* ArrayMenu can import the output from:
  promon/1. User Control
1. Status Displays ... ---------------------------------------------
  R&D/1/3.   Status: Servers
  R&D/1/5.   Status: Files
  R&D/1/6.   Status: Lock Table
  R&D/1/14.  Status: Shared Memory Segments
  R&D/1/15.  Status: AI Extents
  R&D/1/17.  Status: Servers By Broker
  R&D/1/19.  Status: Schema Locks & Wait Queue
1/4. Processes/Clients ... -----------------------------------------
  R&D/1/4/1. Status: All Processes
  R&D/1/4/2. Status: Blocked Clients
  R&D/1/4/3. Status: Active Transactions
  R&D/1/4/4. Status: Local Clients
  R&D/1/4/5. Status: Batch Clients
  R&D/1/4/6. Status: Remote Clients
  R&D/1/4/7. Status: Background Processes
  R&D/1/4/8. Currently Connected Tenants
  R&D/1/4/9. Status: User Notification Processes
1/18. Client Database-Request Statement Cache ... ------------------
  R&D/1/18/7.  View Database-Request Statement Cache
  R&D/1/18/10. Database statement cache locks !!! Warning: it's # 9. in V10.2B
3. Other Displays ...   --------------------------------------------
  R&D/3/2.   I/O Operations by Process
  R&D/3/3.   Lock Requests By User
  R&D/3/4.   Checkpoints
  R&D/3/5.   Activity: I/O Operations by User by Table
  R&D/3/6.   Activity: I/O Operations by User by Index
  R&D/3/7.   Status: Total Locks per User
4. Administrative Functions Menu -----------------------------------
  R&D/4/1.   Check Active Transaction Status
  R&D/4/13.  Status: Caches
6. This menu is not here -------------------------------------------
  R&D/6/1.   Status: Cache Entries
  R&D/6/2.   Status: Hash Chain
  R&D/6/3.   Status: Page Writer Queue
  R&D/6/4.   Status: Lru Chains
  R&D/6/5.   Status: Locked Buffers
  R&D/6/6.   Status: Buffer Locks
  R&D/6/7.   Status: Buffer Use Counts
  R&D/6/13.  Activity: I/O Wait Time by Type
  R&D/6/15.  Status: Buffer Lock Queue
  R&D/6/16.  Semaphores
*/
  DEFINE VARIABLE vDigits      AS CHARACTER NO-UNDO INITIAL "1234567890.":U.
  DEFINE VARIABLE vPropertyRow AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vEmptyLines  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vOffset      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vArray       AS CHARACTER NO-UNDO EXTENT {&ArraySize}.
  DEFINE VARIABLE vArraySize   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLine        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vItem        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vRest        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vHead        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vCurr        AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNext        AS INTEGER   NO-UNDO.

/* Get a header (the column's names) of "array" menu:
   The header is the one or two last non-empty lines before the property rows.
   A property row has at least two columns. One of the columns is a number.
*/
  ASSIGN vEmptyLines = 0.

ImportLine:
  REPEAT ON ENDKEY UNDO, LEAVE ImportLine:
/*
IMPORT statement ignores hyphen (dash).
If an input data line contains an unquoted hyphen in place of a data value
then the corresponding field is skipped.
Promon uses the hyphens for the date/time fields.
Workaround: use IMPORT UNFORMATTED and parse an input line by the items: 
*/  ASSIGN vLine = "":U.
    IMPORT UNFORMATTED vLine.

    IF vLine EQ "":U THEN
    DO:
      ASSIGN vEmptyLines = vEmptyLines + 1.
      IF vEmptyLines GT 1
      THEN LEAVE ImportLine.
      ELSE NEXT  ImportLine.
    END.

    IF vLine MATCHES "*---*":U THEN
/* Ignore the "dash" header lines:

R&D/3/2.   I/O Operations by Process
                  -------- Database ------     ---- BI -----     ---- AI -----
  Usr Name        Access     Read    Write     Read    Write     Read    Write

R&D/3/3.   Lock Requests By User
  Usr User        --- Record ---    ---- Trans ---    --- Schema ---
  Num Name        Locks    Waits    Locks    Waits    Locks    Waits

R&D/4/13.  Status: Caches
 ID        Name      Size(kb)  Entries  Hash      Hits        Misses
--- ---------------- -------- -------- ------ ------------ ------------
*/  NEXT.

    ASSIGN vEmptyLines = 0
           vArraySize  = 0
           vArray      = "":U
           vRest = TRIM(vLine)
           vItem = "":U
    . /* ASSIGN */

/* Parse the line by items using the spaces as a separator: */
    REPEAT WHILE vArraySize LT {&ArraySize} AND vRest NE "":U:
      ASSIGN vItem = ENTRY(1, vRest, " ":U)
             vRest = TRIM(SUBSTRING(vRest, LENGTH(vItem) + 1))
             vArraySize = vArraySize + 1
             vArray[vArraySize] = vItem
      . /* ASSIGN */
    END. /* REPEAT WHILE vArraySize LT {&ArraySize) */

/* Array should have at least 3 columns: */
    IF vArraySize LT 2 THEN
    NEXT ImportLine. /* It's an unexpected input line. */

/* If none of the first two items is an integer then it's a header: */
    IF  TRIM(vArray[1], vDigits) NE "":U
    AND TRIM(vArray[2], vDigits) NE "":U THEN
    IF vPropertyRow GT 0 THEN
/* Some "Array" menus may contain a extra line at the end:

R&D/1/19.  Status: Schema Locks & Wait Queue
  Usr   Name                Locked     Queued
    5  George               SHR
  Total Locks Granted - SHR: 1     EXCL: 0                  <= Ignore this line

R&D/4/13.  Status: Caches
 ID        Name      Size(kb)  Entries  Hash      Hits        Misses
--- ---------------- -------- -------- ------ ------------ ------------
  1 CDC Primary           100        0    317            0            0
  2 CDC Secondary         100        0    317            0            0
Enter Cache ID to reset total hits and total misses         <= Ignore this line

01/02/18        Status: Shared Memory Segments
Seg       Id         Size          Used         Free
 1   54525952     18580128     16490936      2089192

Shm needed:     18579496                                    <= Ignore this line
The "Shm needed" only appears in the "debghb" mode.
It's a calculation of how much shared memory Progress thinks it will
need to accommodate all of the data structures that reside in shared memory.
*/    LEAVE ImportLine.
    ELSE
/*  IF vPropertyRow EQ 0 THEN */
    DO:
/* If a header consists of many rows then keep a row with more characters: */
      ASSIGN vHead = vLine WHEN LENGTH(REPLACE(vLine, " ":U, "":U))
                             GT LENGTH(REPLACE(vHead, " ":U, "":U))
      . /* ASSIGN */
      NEXT ImportLine.
    END.
/*
If vArraySize GE 3 and one of the first two items is an integer then
it's a property row:
*/  ASSIGN vPropertyRow = vPropertyRow + 1
           vEmptyLines  = 0
    . /* ASSIGN */

/* Exceptions for menus: */
    CASE bfCurrMenu.MenuName:
      WHEN "Status: Servers":U THEN
/*
03/15/15        Status: Servers
Sv                                                   Pend.   Cur.   Max.   Port
No    Pid  Type       Protocol               Logins  Users  Users  Users    Num
0   6012 Login                                  0      0      0      4      0
1  22766 Login      TCP                    215345      0      0     15  17996
2  22845 Auto       TCP                      1233      1      4     15   1030
3      0 Inactive                               0      0      0      0      0
Ignore a pimary broker (if it's not a login broker) and "Inactive" slots.
Tip: their "Protocol" column is empty:
*/    IF vArray[4] NE "TCP":U THEN
      NEXT ImportLine. /* Ignore the line. */
    END CASE. /* ipMenuName */

/* Check for the date/time pairs (treats them as a single field).

Example:
12/11/17        Status: Active Transactions
Usr Name     Type       Login time     Tx start time  Trans id Trans State
11986 user1    REMC/ABL  12/10/17 15:09 -              596708950  Begin
12215 user2    REMC/ABL  12/11/17 02:39 12/11/17 04:00 599854969  Active
*/
    ASSIGN vNext = 3. /* Check the items starting from the third one. */
    DO WHILE vNext LT vArraySize:
      ASSIGN vCurr = vNext
             vNext = vNext + 1
      . /* ASSIGN */
      IF  vArray[vCurr] MATCHES "../../..":U
      AND vArray[vNext] MATCHES "..:..*":U THEN
      DO:
        ASSIGN vArray[vCurr] = vArray[vCurr] + " ":U + vArray[vNext]
               vArraySize    = vArraySize - 1
        . /* ASSIGN */
/* Shift the items to the left by one: */
        DO vCurr = vNext TO vArraySize:
          ASSIGN vArray[vCurr] = vArray[vCurr + 1].
        END. /* DO vCurr = vNext TO vArraySize */
      END. /* IF date/time value */

/* Empty old last item: */
      ASSIGN vArray[vArraySize + 1] = "":U.
    END. /* DO WHILE vNext LT vArraySize */

/* Check for "Phase 1" or "Phase 2" as "Trans State": */

    ASSIGN vNext = 5. /* Check the items starting from the fifth one. */
    IF bfCurrMenu.MenuName EQ "Status: Active Transactions":U THEN
    DO WHILE vNext LT vArraySize:
      ASSIGN vCurr = vNext
             vNext = vNext + 1
      . /* ASSIGN */
      IF  vArray[vCurr] EQ "Phase":U
      AND TRIM(vArray[vNext], "12":U) EQ "":U THEN
      DO:
        ASSIGN vArray[vCurr] = vArray[vCurr] + " ":U + vArray[vNext]
               vArraySize    = vArraySize - 1
        . /* ASSIGN */
/* Shift the items to the left by one: */
        DO vCurr = vNext TO vArraySize:
          ASSIGN vArray[vCurr] = vArray[vCurr + 1].
        END. /* DO vCurr = vNext TO vArraySize */

/* Empty old last item: */
        ASSIGN vArray[vArraySize + 1] = "":U.
      END. /* IF "Phase 1" or "Phase 2" values */
    END. /* DO WHILE vNext LT vArraySize */
  
    IF vArraySize GE 3 THEN /* "Array" menu should have at least 3 columns. */
    DO TRANSACTION:
      CREATE ttArray.
      ASSIGN ttArray.DatabaseId   = ipDatabaseId
             ttArray.InputLogId   = ipInputLogId
             ttArray.SnapshotId   = ipSnapshotId
             ttArray.SnapshotTime = bfCurrMenu.MenuTime
             ttArray.MenuId       = bfCurrMenu.MenuId
             ttArray.ArrayRow     = vPropertyRow
             ttArray.ArrayValue   = vArray
             ttArray.ArraySize    = vArraySize
      NO-ERROR. /* ASSIGN */

&IF NOT {&IgnoreErrors} &THEN
      IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
      MESSAGE
        SUBSTITUTE("Proc: ~"&1~", line: &2 (IIII), error:",
        /*&1*/ ENTRY(1, PROGRAM-NAME(1), " ":U), /*&2*/ "{&LINE-NUMBER}") SKIP
        ERROR-STATUS:GET-MESSAGE(1)     SKIP
        "Database:" ipDatabaseId        SKIP
        "LogId:"    ipInputLogId        SKIP
        "Snapshot:" ipSnapshotId        SKIP
        "Time:"     bfCurrMenu.MenuTime SKIP
        "Menu:"     bfCurrMenu.MenuName SKIP
        "MenuId:"   bfCurrMenu.MenuId   SKIP
        "Row:"      vPropertyRow        SKIP
        "Line:" vArray[1] vArray[2] vArray[3] vArray[4] vArray[5] vArray[6] SKIP
      VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
        DELETE ttArray.

      ACCUMULATE vArraySize (MAXIMUM).
    END. /* DO TRANSACTION */
  END. /* ImportLine: REPEAT */

  ASSIGN  bfCurrMenu.ArraySize = MAX(bfCurrMenu.ArraySize, ACCUM MAX vArraySize)
          bfCurrMenu.ArrayHead = vHead
                             WHEN LENGTH(vHead) GT LENGTH(bfCurrMenu.ArrayHead)
  . /* ASSIGN */

END PROCEDURE. /* LoadArrayMenu */

/* ------------------------------------------------------------------------- */

PROCEDURE LoadSpecialLog:

  DEFINE INPUT PARAMETER ipDatabaseId AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipInputLogId AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipLogType    AS CHARACTER NO-UNDO.

/* ipLogType = "Status":
  R&D/1/4/2. Status: Blocked Clients
  R&D/1/4/3. Status: Active Transactions
  R&D/6/6.   Status: Buffer Locks
  R&D/6/15.  Status: Buffer Lock Queue
  
 ipLogType = "Latches":
 R&D/6/11.   Activity: Latch Counts
*/
  DEFINE VARIABLE vArray        AS CHARACTER NO-UNDO EXTENT {&ArraySize}.
  DEFINE VARIABLE vArrayRow     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vArraySize    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMenuId       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMenuName     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vColumn       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vPrevSnapshot AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vCurrSnapshot AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vPrevDateTime AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vCurrDateTime AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vDate         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vTime         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vLine         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vItem         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vRest         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vHead         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vCurr         AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNext         AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i             AS INTEGER   NO-UNDO.

  ASSIGN vArray = "":U
         vHead  = "":U
         vMenuName = "":U
         vMenuName = "Activity: Latch Counts":U WHEN ipLogType EQ "Latches":U
  . /* ASSIGN */
/*
01/06/18        Status: Active Transactions by user number for all tenants
Sample Date     Time       Usr:Ten   Name      Domain     Type       Login time
*/
ImportHead:
  REPEAT ON ENDKEY UNDO, RETURN:
    ASSIGN vLine = vHead.
    IMPORT UNFORMATTED vHead.
/*
01/17/18        Status: Active Transactions
Sample Date     Time       Usr:Ten   Name      Domain     Type       Login time
     0 01/17/18 08:35:30 Begin
1234567890123456789012345
*/  IF vHead MATCHES "Sample Date*Time *":U THEN
    LEAVE ImportHead.
  END. /* ImportHead: REPEAT */

  IF vLine MATCHES "../../.. *":U THEN
  ASSIGN vMenuName = TRIM(SUBSTRING(vLine, 10))
         i = INDEX(vMenuName, " by user number":U)
  . /* ASSIGN */
  
/* IF vLine contains the unexpected data: */
  IF vMenuName EQ "":U THEN
  DO:
    MESSAGE
      SUBSTITUTE("Unexpected header in the special (&1) log file:", ipLogType)
            SKIP
      vLine SKIP
      vHead SKIP
      "Skipping the file..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
    INPUT CLOSE.
    RETURN.
  END.

/* Cut off "Sample Date     Time" and "by user number for all tenants": */
  ASSIGN vHead     = SUBSTRING(vHead, 26)
         vMenuName = TRIM(SUBSTRING(vMenuName, 1, i - 1)) WHEN i GT 1
         vMenuId   = MenuId(vMenuName)
  . /* ASSIGN */

  IF vMenuId EQ ? THEN
  DO:
    ASSIGN vMenuId = 0.
    FOR EACH ttMenu NO-LOCK /* INDEX MenuId */
          BY ttMenu.MenuId DESCENDING:
      ASSIGN vMenuId = ttMenu.MenuId + 1.
      LEAVE.
    END. /* FOR EACH ttMenu */
  
/* No MenuTime, no ttSect for the menus in the special log files: */
    DO TRANSACTION:
      CREATE ttMenu.
      ASSIGN ttMenu.MenuId   = vMenuId
             ttMenu.MenuName = vMenuName
             ttMenu.MenuTime = ?
      . /* ASSIGN */
    END. /* DO TRANSACTION */
  END. /* IF vMenuId EQ ? */

  ASSIGN vPrevDateTime = ?
         vCurrSnapshot = 0
  . /* ASSIGN */

ImportLine:
  REPEAT ON ENDKEY UNDO, LEAVE ImportLine:

    ASSIGN vLine = "":U.
    IMPORT UNFORMATTED vLine.
    
    IF vLine EQ "":U THEN
    NEXT ImportLine.
/*
IMPORT statement ignores hyphen (dash).
If an input data line contains an unquoted hyphen in place of a data value
then the corresponding field is skipped.
Promon uses the hyphens for the date/time fields.
Workaround: use IMPORT UNFORMATTED and parse an input line by the items: 
*/
    IF vLine EQ "":U THEN
    NEXT  ImportLine.
  
    ASSIGN vArray = "":U 
           vArraySize = 0
           vColumn = 0
           vRest = TRIM(vLine)
           vItem = "":U
           vPrevSnapshot = vCurrSnapshot
    . /* ASSIGN */

/* Parse the line by items using the spaces as a separator: */
    REPEAT WHILE vArraySize LT {&ArraySize} AND vRest NE "":U:
      ASSIGN vItem = ENTRY(1, vRest, " ":U)
             vRest = TRIM(SUBSTRING(vRest, LENGTH(vItem) + 1))
             vColumn = vColumn + 1
      . /* ASSIGN */
      CASE vColumn:
        WHEN 1 THEN ASSIGN vCurrSnapshot = INTEGER(vItem) NO-ERROR.
        WHEN 2 THEN ASSIGN vDate = vItem.
        WHEN 3 THEN ASSIGN vTime = vItem.
        OTHERWISE   ASSIGN vArraySize = vArraySize + 1
                           vArray[vArraySize] = vItem
                    . /* ASSIGN */
      END CASE. /* vColumn */
    END. /* REPEAT WHILE vArraySize LT {&ArraySize) */

/* Array should have at least 5 columns: */
    IF vArraySize LT 5 THEN
    NEXT ImportLine.

/* Ignore "Begin" and "End" rows:
Sample Date     Time   Latch Owner   Locks/Sec Naps/Sec Interval
     0 01/17/18 08:35:30 bgn    --           0        0        1
 11350 01/17/18 11:47:38 end    --           0        0        1

Sample Date     Time             User:Ten  DBKEY Area          Hash T S Usect
     0 01/17/18 08:35:30 Begin
  2876 01/17/18 11:47:37 End
*/  IF LOOKUP(vArray[1], "bgn,begin,end":U) GT 0 THEN
    NEXT ImportLine.

/* Check for the date/time pairs (treats them as a single field).
Example
12/11/17        Status: Active Transactions
Usr Name     Type       Login time     Tx start time  Trans id Trans State
11986 user1    REMC/ABL  12/10/17 15:09 -              596708950  Begin
12215 user2    REMC/ABL  12/11/17 02:39 12/11/17 04:00 599854969  Active
*/  ASSIGN vNext = 2. /* Check the items starting from the second one. */
    DO WHILE vNext LT vArraySize:
      ASSIGN vCurr = vNext
             vNext = vNext + 1
      . /* ASSIGN */
      IF  vArray[vCurr] MATCHES "../../..":U
      AND vArray[vNext] MATCHES "..:..*":U THEN
      DO:
        ASSIGN vArray[vCurr] = vArray[vCurr] + " ":U + vArray[vNext]
               vArraySize    = vArraySize - 1
        . /* ASSIGN */
/* Shift the items to left: */
        DO vCurr = vNext TO vArraySize:
          ASSIGN vArray[vCurr] = vArray[vCurr + 1].
        END. /* DO vCurr = vNext TO vArraySize */

/* Empty old last item: */
        ASSIGN vArray[vArraySize + 1] = "":U.
      END. /* IF date/time value */
    END. /* DO WHILE vNext LT vArraySize */
  
/* Check for "Phase 1" or "Phase 2" as "Trans State": */
    ASSIGN vNext = 5. /* Check the items starting from the fifth one. */
    IF vMenuName EQ "Status: Active Transactions":U THEN
    DO WHILE vNext LT vArraySize:
      ASSIGN vCurr = vNext
             vNext = vNext + 1
      . /* ASSIGN */
      IF  vArray[vCurr] EQ "Phase":U
      AND TRIM(vArray[vNext], "12":U) EQ "":U THEN
      DO:
        ASSIGN vArray[vCurr] = vArray[vCurr] + " ":U + vArray[vNext]
               vArraySize    = vArraySize - 1
        . /* ASSIGN */
/* Shift the items to left: */
        DO vCurr = vNext TO vArraySize:
          ASSIGN vArray[vCurr] = vArray[vCurr + 1].
        END. /* DO vCurr = vNext TO vArraySize */

/* Empty old last item: */
        ASSIGN vArray[vArraySize + 1] = "":U.
      END. /* IF "Phase 1" or "Phase 2" values */
    END. /* DO WHILE vNext LT vArraySize */
  
    ASSIGN vCurrDateTime = Str2DateTime(vDate, vTime).

    IF vCurrSnapshot EQ vPrevSnapshot THEN
    ASSIGN vArrayRow = vArrayRow + 1.
    ELSE
    DO TRANSACTION:
/* Create new snapshot: */
      CREATE ttSnapshot.
      ASSIGN ttSnapshot.DatabaseId   = ipDatabaseId
             ttSnapshot.InputLogId   = ipInputLogId
             ttSnapshot.SnapshotId   = vCurrSnapshot
             ttSnapshot.SnapshotTime = vCurrDateTime
             ttSnapshot.StatInterval = INTERVAL(vCurrDateTime, vPrevDateTime, "seconds":U)
             vPrevDateTime = vCurrDateTime
             vPrevSnapshot = vCurrSnapshot
             vArrayRow = 1
      . /* ASSIGN */
    END. /* DO TRANSACTION (IF vCurrSnapshot NE vPrevSnapshot) */

    DO TRANSACTION:
      CREATE ttArray.
      ASSIGN ttArray.DatabaseId   = ipDatabaseId
             ttArray.InputLogId   = ipInputLogId
             ttArray.SnapshotId   = vCurrSnapshot
             ttArray.SnapshotTime = vCurrDateTime
             ttArray.MenuId       = vMenuId
             ttArray.ArrayRow     = vArrayRow
             ttArray.ArrayValue   = vArray
             ttArray.ArraySize    = vArraySize
      NO-ERROR. /* ASSIGN */

&IF NOT {&IgnoreErrors} &THEN
      IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
      MESSAGE
        SUBSTITUTE("Proc: ~"&1~", line: &2 (JJJJ), error:",
        /*&1*/ ENTRY(1, PROGRAM-NAME(1), " ":U), /*&2*/ "{&LINE-NUMBER}") SKIP
        ERROR-STATUS:GET-MESSAGE(1) SKIP
        "Database:" ipDatabaseId    SKIP
        "LogId:"    ipInputLogId    SKIP
        "LogType:"  ipLogType       SKIP
        "Menu:"     vMenuName       SKIP
        "MenuId:"   vMenuId         SKIP
        "Snapshot:" vCurrSnapshot   SKIP
        "Time:"     vDate vTime     SKIP
        "Row:"      vArrayRow       SKIP
        "Line:" vArray[1] vArray[2] vArray[3] vArray[4] vArray[5] vArray[6] SKIP
      VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
        DELETE ttArray.

      ACCUMULATE vArraySize (MAXIMUM).
    END. /* DO TRANSACTION */
  END. /* ImportLine: REPEAT */
  
  FOR FIRST ttMenu EXCLUSIVE-LOCK
      WHERE ttMenu.MenuId EQ vMenuId:
    ASSIGN  ttMenu.ArraySize = ACCUM MAXIMUM vArraySize
            ttMenu.ArrayHead = vHead
                   WHEN LENGTH(vHead) GT LENGTH(ttMenu.ArrayHead)
    . /* ASSIGN */
  END. /* FOR FIRST ttMenu */

END PROCEDURE. /* LoadSpecialLog */

/* ------------------------------------------------------------------------- */

PROCEDURE LoadActivitySummary:

  DEFINE INPUT PARAMETER  ipDatabaseId AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER  ipInputLogId AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER  ipSnapshotId AS INTEGER NO-UNDO.
  DEFINE PARAMETER BUFFER bfCurrMenu   FOR TEMP-TABLE ttMenu.

  DEFINE VARIABLE vPropertyRow  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vEmptyLines   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vOffset       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLine         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vName         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vValue        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vTotal        AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vPerSec       AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vColumn       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
/*
06/11/13        Activity: Summary
14:48:18        06/11/13 03:11 to 06/11/13 14:48 (11 hrs 36 min)
Event                  Total  Per Sec |Event                  Total  Per Sec

Skip the header:
*/
  REPEAT ON ENDKEY UNDO, RETURN:
    IMPORT UNFORMATTED vLine.
    IF vLine MATCHES "*Event * Total *Per Sec*|*Event * Total *Per Sec*":U THEN
    LEAVE.
  END. /* REPEAT */

  ASSIGN vEmptyLines  = 0
         vPropertyRow = 0
  . /* ASSIGN */

ImportLine:
  REPEAT ON ENDKEY UNDO, RETURN:
    ASSIGN vLine = "":U vOffset = SEEK(INPUT).
    IMPORT UNFORMATTED vLine.

/* Ignore all empty lines before the property's rows.
   Stop import after two sequential empty lines.
*/  IF vLine EQ "":U THEN
    DO:
      ASSIGN vEmptyLines = vEmptyLines + 1.
      IF vEmptyLines GE 2 THEN RETURN.
      NEXT ImportLine.
    END.

/* Stop import after a header of a next promon menu: */
    IF vLine MATCHES "../../..*":U
    OR vLine MATCHES "..:..:..*":U THEN
    DO:
      ASSIGN vOffset = MAX(vOffset - 8, 0). /* Padding 8 bytes */
      SEEK INPUT TO vOffset. /* Return to a line back*/
      RETURN.
    END.

/* Reset the counter of the empty lines after any non-empty line: */
    ASSIGN vEmptyLines  = 0
           vPropertyRow = vPropertyRow + 1
    . /* ASSIGN */
/*
11/07/17        Activity: Summary
04:00:24        10/15/17 04:09 to 11/07/17 04:00 (551 hrs 51 min)

Event                  Total  Per Sec |Event                  Total  Per Sec

Commits              400000K    206.2 |DB Reads            1632723K    841.6
Undos              12944319       6.5 |DB Writes          40786312      20.5
Record Reads         117781M  62166.1 |BI Reads              71797       0.0
Record Updates     38461644      19.4 |BI Writes          14384421       7.2
Record Creates     78165856      39.3 |AI Writes          12811568       6.4
Record Deletes      2939246       1.5 |Checkpoints             611       0.0
Record Locks        1472158K    758.8 |Flushed at chkpt          0       0.0
Record Waits           1520       0.0 |Active trans              5

Rec Lock Waits     0 %    BI Buf Waits      0 %    AI Buf Waits      0 %
Writes by APW     76 %    Writes by BIW    29 %    Writes by AIW    95 %
DB Size:        1606 GB   BI Size:       5000 MB   AI Size:         88 K
Empty blocks:   1046      Free blocks:   1554      RM chain:      1046
Buffer Hits       99 %    Primary Hits     99 %    Alternate Hits    0 %

436 Servers, 908 Users (44 Local, 864 Remote, 639 Batch), 10 Apws
*/  IF NUM-ENTRIES(vLine, "|":U) EQ 2 THEN
    DO vColumn = 1 TO 2:

/* Parse from left to right: */
      ASSIGN vName  = TRIM(ENTRY(vColumn, vLine, "|":U))
             i      = R-INDEX(vName, " ":U)
             vValue = SUBSTRING(vName, i + 1)
             vName  = TRIM(SUBSTRING(vName, 1, i - 1))
      . /* ASSIGN */

      IF vName EQ "Active trans":U THEN
      DO TRANSACTION:
        CREATE ttStatus.
        ASSIGN ttStatus.DatabaseId    = ipDatabaseId
               ttStatus.InputLogId    = ipInputLogId
               ttStatus.SnapshotId    = ipSnapshotId
               ttStatus.SnapshotTime  = bfCurrMenu.MenuTime
               ttStatus.MenuId        = bfCurrMenu.MenuId
               ttStatus.SectId        = 0
               ttStatus.PropertyValue = vValue
               ttStatus.PropertyId    = PropertyId("Status":U,
                   bfCurrMenu.MenuId, vName, vPropertyRow + (vColumn - 1) * 8)
        . /* ASSIGN */
      END. /* DO TRANSACTION: IF vName EQ "Active trans" */
      ELSE
      DO TRANSACTION: /* Activity properties */
        CREATE ttActivity.
        ASSIGN vPerSec = DECIMAL(vValue)
               i       = R-INDEX(vName, " ":U)
               vValue  = SUBSTRING(vName, i + 1)
               vTotal  = String2Dec(vValue)
               vName   = TRIM(SUBSTRING(vName, 1, i - 1))
               ttActivity.DatabaseId       = ipDatabaseId
               ttActivity.InputLogId       = ipInputLogId
               ttActivity.SnapshotId       = ipSnapshotId
               ttActivity.MenuId           = bfCurrMenu.MenuId
               ttActivity.SectId           = 0
               ttActivity.PropertyValue[1] = vTotal
               ttActivity.PropertyValue[2] = vPerSec
               ttActivity.PropertyId       = PropertyId("Activity":U,
                    bfCurrMenu.MenuId, vName, vPropertyRow + (vColumn - 1) * 8)
        NO-ERROR. /* ASSIGN */

&IF NOT {&IgnoreErrors} &THEN
        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
        MESSAGE
          SUBSTITUTE("Proc: ~"&1~", line: &2 (KKKK), error:",
          /*&1*/ ENTRY(1, PROGRAM-NAME(1), " ":U), /*&2*/ "{&LINE-NUMBER}") SKIP
          ERROR-STATUS:GET-MESSAGE(1)     SKIP
          "Database:" ipDatabaseId        SKIP
          "LogId:"    ipInputLogId        SKIP
          "Snapshot:" ipSnapshotId        SKIP
          "Time:"     bfCurrMenu.MenuTime SKIP
          "Menu:"     bfCurrMenu.MenuName SKIP
          "MenuId:"   bfCurrMenu.MenuId   SKIP
          "Row:"      vPropertyRow        SKIP
          "Line:"     vLine               SKIP
          "Property:" vName               SKIP
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
        DELETE ttActivity.
      END. /* DO TRANSACTION: Activity properties */
    END. /* DO vColumn = 1 TO 2 */

    ELSE /* IF NUM-ENTRIES(vLine, "|":U) EQ 2 */
/* It's a line with the "Status" properties:
            14 17       26                       51                    76
Rec Lock Waits     0 %    BI Buf Waits      0 %    AI Buf Waits      0 %
Writes by APW     76 %    Writes by BIW    29 %    Writes by AIW    95 %
DB Size:        1606 GB   BI Size:       5000 MB   AI Size:         88 K
Empty blocks:   1046      Free blocks:   1554      RM chain:      1046
Buffer Hits       99 %    Primary Hits     99 %    Alternate Hits    0 %

DB Size:     %7ld K       BI Size:%11ld K          AI Size:%11ld K
DB Size:     %7ld MB      BI Size:%11ld MB         AI Size:%11ld MB
DB Size:     %7ld GB      BI Size:%11ld GB         AI Size:%11ld GB
Empty blocks:%7lu      Free blocks:%7lu      RM chain:%10lu
*/  IF vLine MATCHES "*:*:*:*":U THEN
    DO vColumn = 1 TO 51 BY 25
    TRANSACTION:
      CREATE ttStatus.
      ASSIGN vName  = TRIM(SUBSTRING(vLine, vColumn, 25))
             i      = R-INDEX(vName, ":":U)
             vValue = TRIM (SUBSTRING(vName, i + 1))
             vName  = TRIM(SUBSTRING(vName, 1, i - 1))
             ttStatus.DatabaseId    = ipDatabaseId
             ttStatus.InputLogId    = ipInputLogId
             ttStatus.SnapshotId    = ipSnapshotId
             ttStatus.SnapshotTime  = bfCurrMenu.MenuTime
             ttStatus.MenuId        = bfCurrMenu.MenuId
             ttStatus.SectId        = 0
             ttStatus.PropertyValue = vValue
             ttStatus.PropertyId    = PropertyId(
              "Status":U, bfCurrMenu.MenuId, vName, vPropertyRow)
      NO-ERROR. /* ASSIGN */

&IF NOT {&IgnoreErrors} &THEN
      IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
      MESSAGE
        SUBSTITUTE("Proc: ~"&1~", line: &2 (LLLL), error:",
        /*&1*/ ENTRY(1, PROGRAM-NAME(1), " ":U), /*&2*/ "{&LINE-NUMBER}") SKIP
        ERROR-STATUS:GET-MESSAGE(1)     SKIP
        "Database:" ipDatabaseId        SKIP
        "LogId:"    ipInputLogId        SKIP
        "Snapshot:" ipSnapshotId        SKIP
        "Time:"     bfCurrMenu.MenuTime SKIP
        "Menu:"     bfCurrMenu.MenuName SKIP
        "MenuId:"   bfCurrMenu.MenuId   SKIP
        "Row:"      vPropertyRow        SKIP
        "Line:"     vLine               SKIP
        "Property:" vName               SKIP
      VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
      IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN /* This should not happen */
      DELETE ttStatus.                       /* but just in case.      */
    END. /* DO vColumn = 1 TO 51 BY 25 */
    ELSE
/*          14
Rec Lock Waits  %3.0f %%    BI Buf Waits    %3.0f %%    AI Buf Waits    %3.0f %%
Writes by APW   %3.0f %%    Writes by BIW   %3.0f %%    Writes by AIW   %3.0f %%
Buffer Hits     %3.0f %%    Primary Hits    %3.0f %%    Alternate Hits  %3.0f %%
*/  IF vLine MATCHES "* %* %* *%":U THEN
    DO vColumn = 1 TO 51 BY 25
    TRANSACTION:
      CREATE ttStatus.
      ASSIGN vName  = TRIM(SUBSTRING(vLine, vColumn, 25))
             vValue = TRIM(SUBSTRING(vName, 15))        
             vName  = TRIM(SUBSTRING(vName, 1, 14))
             ttStatus.DatabaseId    = ipDatabaseId
             ttStatus.InputLogId    = ipInputLogId
             ttStatus.SnapshotId    = ipSnapshotId
             ttStatus.SnapshotTime  = bfCurrMenu.MenuTime
             ttStatus.MenuId        = bfCurrMenu.MenuId
             ttStatus.SectId        = 0
             ttStatus.PropertyValue = vValue
             ttStatus.PropertyId    = PropertyId(
              "Status":U, bfCurrMenu.MenuId, vName, vPropertyRow)
      NO-ERROR. /* ASSIGN */

&IF NOT {&IgnoreErrors} &THEN
        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
        MESSAGE
          SUBSTITUTE("Proc: ~"&1~", line: &2 (MMMM), error:",
          /*&1*/ ENTRY(1, PROGRAM-NAME(1), " ":U), /*&2*/ "{&LINE-NUMBER}") SKIP
          ERROR-STATUS:GET-MESSAGE(1)     SKIP
          "Database:" ipDatabaseId        SKIP
          "LogId:"    ipInputLogId        SKIP
          "Snapshot:" ipSnapshotId        SKIP
          "Time:"     bfCurrMenu.MenuTime SKIP
          "Menu:"     bfCurrMenu.MenuName SKIP
          "MenuId:"   bfCurrMenu.MenuId   SKIP
          "Row:"      vPropertyRow        SKIP
          "Line:"     vLine               SKIP
          "Property:" vName               SKIP
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
      IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN /* This should not happen */
      DELETE ttStatus.                       /* but just in case.      */
    END. /* DO vColumn = 1 TO 51 BY 25 */
    ELSE
/*
436 Servers, 908 Users (44 Local, 864 Remote, 639 Batch), 10 Apws
%d Servers, %d Users (%d Local, %d Remote, %d Batch), %d Apws
*/  IF vLine MATCHES "* Servers,* Users (* Local, * Remote, * Batch), * Apws":U THEN
    DO i = 1 TO 6
    TRANSACTION:
      CREATE ttStatus.
      ASSIGN vName  = RIGHT-TRIM(ENTRY(i * 2, vLine, " ":U), ",)":U)
             vValue = ENTRY(i * 2 - 1, vLine, " ":U)
             vValue = LEFT-TRIM(vValue, "(":U) WHEN i EQ 3
             ttStatus.DatabaseId    = ipDatabaseId
             ttStatus.InputLogId    = ipInputLogId
             ttStatus.SnapshotId    = ipSnapshotId
             ttStatus.SnapshotTime  = bfCurrMenu.MenuTime
             ttStatus.MenuId        = bfCurrMenu.MenuId
             ttStatus.SectId        = 0
             ttStatus.PropertyValue = vValue
             ttStatus.PropertyId    = PropertyId(
              "Status":U, bfCurrMenu.MenuId, vName, vPropertyRow)
      NO-ERROR. /* ASSIGN */

&IF NOT {&IgnoreErrors} &THEN
      IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
      MESSAGE
        SUBSTITUTE("Proc: ~"&1~", line (LLLL): &2, error:",
        /*&1*/ ENTRY(1, PROGRAM-NAME(1), " ":U), /*&2*/ "{&LINE-NUMBER}") SKIP
        ERROR-STATUS:GET-MESSAGE(1)     SKIP
        "Database:" ipDatabaseId        SKIP
        "LogId:"    ipInputLogId        SKIP
        "Snapshot:" ipSnapshotId        SKIP
        "Time:"     bfCurrMenu.MenuTime SKIP
        "Menu:"     bfCurrMenu.MenuName SKIP
        "MenuId:"   bfCurrMenu.MenuId   SKIP
        "Row:"      vPropertyRow        SKIP
        "Line:"     vLine               SKIP
        "Property:" vName               SKIP
      VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
      IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN /* This should not happen */
      DELETE ttStatus.                       /* but just in case.      */
    END. /* DO i = 1 TO 6 */

  END. /* ImportLine: REPEAT */

END PROCEDURE. /* LoadActivitySummary */

/* ------------------------------------------------------------------------- */

PROCEDURE LoadActivityLatchCounts:

  DEFINE INPUT PARAMETER  ipDatabaseId AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER  ipInputLogId AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER  ipSnapshotId AS INTEGER NO-UNDO.
  DEFINE PARAMETER BUFFER bfCurrMenu   FOR TEMP-TABLE ttMenu.

  DEFINE VARIABLE vRow    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vOffset AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vItem   AS CHARACTER NO-UNDO EXTENT 12.
  DEFINE VARIABLE vHead   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i       AS INTEGER   NO-UNDO.
/*
03/15/15        Activity: Latch Counts
09:25:01        12/05/14 05:12 to 03/15/15 09:25 (2404 hrs 13 min)

                ----- Locks ----- ------ Busy ------ Naps   ---------- Spins ----------- ----- Nap Max -----
   Owner        Total        /Sec /Sec           Pct /Sec   /Sec       /Lock       /Busy    Total   HWM

  Skip the header:
*/
  REPEAT ON ENDKEY UNDO, RETURN:
    IMPORT UNFORMATTED vHead.
    IF vHead MATCHES "*Owner *  Total * /Sec * /Sec * Pct * /Sec*":U THEN
    LEAVE.
  END. /* REPEAT */

  ASSIGN vRow = 0.

ImportLine:
  REPEAT TRANSACTION ON ENDKEY UNDO, RETURN:
    ASSIGN vItem = "":U vOffset = SEEK(INPUT).
    IMPORT vItem.

/* Ignore all empty lines before the data rows.
   Stop import after first empty line below the data rows.
*/  IF vItem[1] EQ "":U THEN
    IF vRow EQ 0 THEN NEXT ImportLine.
                 ELSE RETURN.

/* Stop import after a header of a next promon menu: */
    IF vItem[1] MATCHES "../../..*":U
    OR vItem[1] MATCHES "..:..:..*":U THEN
    DO:
      ASSIGN vOffset = MAX(vOffset - 8, 0). /* Padding 8 bytes */
      SEEK INPUT TO vOffset. /* Return to a line back*/
      RETURN.
    END.
/*
03/15/15        Activity: Latch Counts
09:25:01        12/05/14 05:12 to 03/15/15 09:25 (2404 hrs 13 min)

                ----- Locks ----- ------ Busy ------ Naps   ---------- Spins ----------- ----- Nap Max -----
   Owner        Total        /Sec /Sec           Pct /Sec   /Sec       /Lock       /Busy    Total   HWM

MTX  --   37278056170        4307    0           0.0    4 225600           7           0        1   160
USR  --       3245744           0    0           0.0    0      0           0           0        0     0
1    2              3           4    5             6    7      8           9          10       11    12
*/  CREATE ttLatch.
    ASSIGN vRow  = vRow  + 1
           ttLatch.DatabaseId   = ipDatabaseId
           ttLatch.InputLogId   = ipInputLogId
           ttLatch.SnapshotId   = ipSnapshotId
           ttLatch.MenuId       = bfCurrMenu.MenuId
           ttLatch.LatchOwner   = vItem[2]
           ttLatch.LatchLock[1] = DECIMAL(vItem[3])  /* Locks Total   */
           ttLatch.LatchLock[2] = DECIMAL(vItem[4])  /* Locks/Sec     */
           ttLatch.LatchWait    = DECIMAL(vItem[7])  /* Naps/Sec      */
           ttLatch.LatchBusy    = DECIMAL(vItem[5])  /* Busy/Sec      */
           ttLatch.LatchSpin    = DECIMAL(vItem[8])  /* Spins/Sec     */
           ttLatch.MaxNapTime   = INTEGER(vItem[11]) /* Nap Max HWM   */
           ttLatch.MaxNapCount  = INTEGER(vItem[12]) /* Nap Max Total */
           ttLatch.PropertyId   = PropertyId(
            "Latch":U, bfCurrMenu.MenuId, vItem[1], vRow)
    NO-ERROR. /* ASSIGN */

&IF NOT {&IgnoreErrors} &THEN
    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    MESSAGE
      SUBSTITUTE("Proc: ~"&1~", line (MMMM): &2, error:",
      /*&1*/ ENTRY(1, PROGRAM-NAME(1), " ":U), /*&2*/ "{&LINE-NUMBER}") SKIP
      ERROR-STATUS:GET-MESSAGE(1)     SKIP
      "Database:" ipDatabaseId        SKIP
      "LogId:"    ipInputLogId        SKIP
      "Snapshot:" ipSnapshotId        SKIP
      "Time:"     bfCurrMenu.MenuTime SKIP
      "Menu:"     bfCurrMenu.MenuName SKIP
      "MenuId:"   bfCurrMenu.MenuId   SKIP
      "Row:"  vRow                    SKIP
      "Line:" vItem[1] vItem[2] vItem[3] vItem[4] vItem[5] vItem[6] vItem[7] SKIP
    VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
    IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
    DELETE ttLatch.
  END. /* ImportLine: REPEAT */

END PROCEDURE. /* LoadActivityLatchCounts */

/* ------------------------------------------------------------------------- */

PROCEDURE ProcessFirstSnapshots:

  DEFINE VARIABLE vMenuId  AS INTEGER NO-UNDO.
/* 
 Set StatInterval for a first snapshot since db startup.
 LoadDbmon set it based on an activity interval:
 (%u hrs %u min) or (%u min %u sec) or (%u sec)
 An accuracy used to be a minute.
 Precision of R&D/1/1. Status: Database => "It has been up for" is a second.
*/
  ASSIGN vMenuId = MenuId("Status: Database":U).
  FOR FIRST ttProperty NO-LOCK /* INDEX PropertyName */
      WHERE ttProperty.MenuId       EQ vMenuId
        AND ttProperty.PropertyName EQ "It has been up for":U,
    
       EACH ttDatabase NO-LOCK,
 
       EACH ttSnapshot EXCLUSIVE-LOCK /* any of two indexes */
      WHERE ttSnapshot.DatabaseId EQ ttDatabase.DatabaseId
        AND ttSnapshot.SnapshotId EQ 0, /* <= not in index */
 
      FIRST ttStatus NO-LOCK /* INDEX ReportOrder */
      WHERE ttStatus.DatabaseId EQ ttSnapshot.DatabaseId
        AND ttStatus.InputLogId EQ ttSnapshot.InputLogId
        AND ttStatus.SnapshotId EQ ttSnapshot.SnapshotId
        AND ttStatus.MenuId     EQ ttProperty.MenuId
        AND ttStatus.PropertyId EQ ttProperty.PropertyId:

/* It has been up for:                551:50:49 */
      ASSIGN ttSnapshot.StatInterval =
             INTEGER(ENTRY(1, ttStatus.PropertyValue, ":":U)) * 3600
           + INTEGER(ENTRY(2, ttStatus.PropertyValue, ":":U)) * 60
           + INTEGER(ENTRY(3, ttStatus.PropertyValue, ":":U))
      NO-ERROR. /* ASSIGN */

  END. /*FOR FIRST ttProperty, EACH ttDatabase, EACH ttSnapshot, FIRST ttStatus*/

END PROCEDURE. /* ProcessFirstSnapshots */

/* ------------------------------------------------------------------------- */

PROCEDURE ProcessOperationsByType:

  DEFINE VARIABLE vMenuId  AS INTEGER NO-UNDO.
  DEFINE VARIABLE vSectId  AS INTEGER NO-UNDO.
  DEFINE VARIABLE vSectId1 AS INTEGER NO-UNDO.
  DEFINE VARIABLE vSectId2 AS INTEGER NO-UNDO.
/*
       R&D/2/2. Activity: I/O Operations by Type
09:55:02        03/15/15 09:25 to 03/15/15 09:55 (30 min 0 sec)
                                Total      Per Min       Per Sec       Per Tx
Database reads                  73880         2463         41.04         0.16
  Index blocks                   6320          211          3.51         0.01
  Data blocks                   67560         2252         37.53         0.15
BI reads                          754           25          0.42         0.00
AI reads                            6            0          0.00         0.00
Total reads                     74640         2488         41.47         0.17

Database writes                661257        22042        367.37         1.47
  Index blocks                 366034        12201        203.35         0.81
  Data blocks                  295223         9841        164.01         0.65
BI writes                      175790         5860         97.66         0.39
AI writes                      161333         5378         89.63         0.36
Total writes                   998408        33280        554.67         2.21

Set the "Reads" section for first half of the rows
and the "Writes" section for second half:
*/
  ASSIGN vMenuId  = MenuId("Activity: I/O Operations by Type":U)
         vSectId1 = SectId(vMenuId, "Read Operations":U)
         vSectId2 = SectId(vMenuId, "Write Operations":U)
  . /* ASSIGN */

  FOR EACH ttProperty NO-LOCK /* 12 records */
     WHERE ttProperty.MenuId EQ vMenuId:

    ASSIGN vSectId = IF ttProperty.PropertyRow LT 6 THEN vSectId1 ELSE vSectId2.

    FOR EACH ttDatabase NO-LOCK,

        EACH ttSnapshot NO-LOCK /* any of two indexes */
       WHERE ttSnapshot.DatabaseId EQ ttDatabase.DatabaseId,

        EACH ttActivity EXCLUSIVE-LOCK /* INDEX ReportOrder */
       WHERE ttActivity.DatabaseId EQ ttSnapshot.DatabaseId
         AND ttActivity.InputLogId EQ ttSnapshot.InputLogId
         AND ttActivity.SnapshotId EQ ttSnapshot.SnapshotId
         AND ttActivity.MenuId     EQ ttProperty.MenuId:
      ASSIGN ttActivity.SectId = vSectId.
    END. /* FOR EACH ttDatabase, EACH ttSnapshot, EACH ttActivity */
  END. /* FOR EACH ttProperty */

END PROCEDURE. /* ProcessOperationsByType */

/* ------------------------------------------------------------------------- */

PROCEDURE ProcessOperationsByFile:

  DEFINE VARIABLE vArraySize             AS INTEGER NO-UNDO.

/*
 Reformat "Activity by File" menus to the 'Array' type menu:
 
12/11/17        Activity: I/O Operations by File
04:00:22        12/10/17 02:20 to 12/11/17 04:00 (25 hrs 40 min)
                                    Total         Per Min          Per Sec          Per Tx
/home2/db-acct/acct.db
Reads                                  19               0             0.00            0.00
Writes                                  0               0             0.00            0.00
Extends                                 0               0             0.00            0.00

12/11/17        Activity: I/O by File
04:00:22        12/10/17 02:20 to 12/11/17 04:00 (25 hrs 40 min)
                                    Total         Per Min          Per Sec          Per Tx
/home2/db-acct/acct.db
Buffered Reads                         22               0             0.00            0.00
Buffered Writes                         1               0             0.00            0.00
Unbuffered Reads                        0               0             0.00            0.00
Unbuffered Writes                       0               0             0.00            0.00
*/
  FOR EACH ttMenu EXCLUSIVE-LOCK
     WHERE ttMenu.MenuName EQ "Activity: I/O Operations by File":U /*R&D/2/9*/
        OR ttMenu.MenuName EQ "Activity: I/O by File":U:           /*R&D/6/14*/

    FOR EACH ttSect EXCLUSIVE-LOCK
       WHERE ttSect.MenuId   EQ ttMenu.MenuId
         AND ttSect.SectName NE "":U
    TRANSACTION:

      FOR EACH ttDatabase NO-LOCK,
  
          EACH ttSnapshot NO-LOCK /* any of two indexes */
         WHERE ttSnapshot.DatabaseId EQ ttDatabase.DatabaseId:
    
        IF NOT CAN-FIND(
          FIRST ttActivity NO-LOCK /* INDEX ReportOrder */
          WHERE ttActivity.DatabaseId EQ ttSnapshot.DatabaseId
            AND ttActivity.InputLogId EQ ttSnapshot.InputLogId
            AND ttActivity.SnapshotId EQ ttSnapshot.SnapshotId
            AND ttActivity.MenuId     EQ ttSect.MenuId
            AND ttActivity.SectId     EQ ttSect.SectId) THEN
        NEXT.

        CREATE ttArray.
        ASSIGN ttArray.DatabaseId    = ttSnapshot.DatabaseId
               ttArray.InputLogId    = ttSnapshot.InputLogId
               ttArray.SnapshotId    = ttSnapshot.SnapshotId
               ttArray.SnapshotTime  = ttSnapshot.SnapshotTime
               ttArray.MenuId        = ttSect.MenuId
               ttArray.ArrayRow      = ttSect.SectId
               ttArray.ArrayValue[1] = STRING(ttSnapshot.StatInterval)
               ttArray.ArrayValue[2] = ttSect.SectName
               vArraySize = 2
        . /* ASSIGN */
    
        FOR EACH ttActivity NO-LOCK /* INDEX ReportOrder */
           WHERE ttActivity.DatabaseId EQ ttSnapshot.DatabaseId
             AND ttActivity.InputLogId EQ ttSnapshot.InputLogId
             AND ttActivity.SnapshotId EQ ttSnapshot.SnapshotId
             AND ttActivity.MenuId     EQ ttSect.MenuId
             AND ttActivity.SectId     EQ ttSect.SectId
              BY ttActivity.PropertyId:

          ASSIGN vArraySize = vArraySize + 1 /* Total: */
                 ttArray.ArrayValue[vArraySize] = STRING(ttActivity.PropertyValue[1])
          . /* ASSIGN */
          DELETE ttActivity.
        END. /* FOR EACH ttActivity */

        ASSIGN ttArray.ArraySize = vArraySize.
      END.  /* FOR EACH ttDatabase, EACH ttSnapshot */

      DELETE ttSect.
    END. /* FOR EACH ttSect */

    ASSIGN ttMenu.ArraySize = vArraySize.
  END. /* FOR EACH ttMenu */

END PROCEDURE. /* ProcessOperationsByFile */

/* ------------------------------------------------------------------------- */

PROCEDURE ProcessServiceManager:

/*
Activity counters in R&D/1/16. Status: Database Service Manager
  Used HighWater Mark     :        3334
  Area Filled Count       :           0
  Access Count            :       31040
  Access Collisions       :           1
*/
  DEFINE VARIABLE vServiceName AS CHARACTER NO-UNDO EXTENT 4 INITIAL
    ["Used HighWater Mark":U,
     "Area Filled Count":U,
     "Access Count":U,
     "Access Collisions":U].

  DEFINE VARIABLE vCurrValue AS DECIMAL NO-UNDO.
  DEFINE VARIABLE vPrevValue AS DECIMAL NO-UNDO.

  DEFINE VARIABLE vMenuId  AS INTEGER NO-UNDO.
  DEFINE VARIABLE vSectId  AS INTEGER NO-UNDO.
  DEFINE VARIABLE i        AS INTEGER NO-UNDO.

  ASSIGN vMenuId = MenuId("Status: Database Service Manager":U)
         vSectId = SectId(vMenuId, "Activity Counters":U)
  . /* ASSIGN */

  DO i = 1 TO EXTENT(vServiceName):
    FOR FIRST ttProperty NO-LOCK /* INDEX PropertyName */
        WHERE ttProperty.MenuId       EQ vMenuId
          AND ttProperty.PropertyName EQ vServiceName[i]:

      ASSIGN  ttProperty.PropertyType = ttProperty.PropertyType + ",Activity":U.
        
      FOR EACH ttDatabase NO-LOCK,

          EACH ttSnapshot NO-LOCK /* any of two indexes */
         WHERE ttSnapshot.DatabaseId EQ ttDatabase.DatabaseId,
 
         FIRST ttStatus NO-LOCK /* INDEX ReportOrder */
         WHERE ttStatus.DatabaseId EQ ttSnapshot.DatabaseId
           AND ttStatus.InputLogId EQ ttSnapshot.InputLogId
           AND ttStatus.SnapshotId EQ ttSnapshot.SnapshotId
           AND ttStatus.MenuId     EQ ttProperty.MenuId
           AND ttStatus.PropertyId EQ ttProperty.PropertyId
 
            BY ttDatabase.DatabaseId    /* INDEX DatabaseId   */
            BY ttSnapshot.SnapshotTime  /* INDEX SnapshotTime */
      TRANSACTION:
        CREATE ttActivity.
        ASSIGN vPrevValue = 0.0         WHEN ttSnapshot.SnapshotId EQ 0
               ttActivity.DatabaseId       = ttSnapshot.DatabaseId
               ttActivity.InputLogId       = ttSnapshot.InputLogId
               ttActivity.SnapshotId       = ttSnapshot.SnapshotId
               ttActivity.MenuId           = vMenuId
               ttActivity.SectId           = vSectId
               ttActivity.PropertyId       = ttProperty.PropertyId
               vCurrValue = DECIMAL(ttStatus.PropertyValue)
               ttActivity.PropertyValue[1] = vCurrValue - vPrevValue
               ttActivity.PropertyValue[2] = ttActivity.PropertyValue[1]
                                           / ttSnapshot.StatInterval
              vPrevValue = vCurrValue
        NO-ERROR. /* ASSIGN */
      
&IF NOT {&IgnoreErrors} &THEN
        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
        MESSAGE
          SUBSTITUTE("Proc: ~"&1~", line (NNNN): &2, error:",
          /*&1*/ ENTRY(1, PROGRAM-NAME(1), " ":U), /*&2*/ "{&LINE-NUMBER}") SKIP
          ERROR-STATUS:GET-MESSAGE(1)         SKIP
          "Database:" ttSnapshot.DatabaseId   SKIP
          "LogId:"    ttSnapshot.InputLogId   SKIP
          "Snapshot:" ttSnapshot.SnapshotId   SKIP
          "    Time:" ttSnapshot.SnapshotTime SKIP
          "Property:" ttProperty.PropertyName SKIP
          "   Value:" ttProperty.PropertyName SKIP
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
        IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN /* This should not happen but */
        DELETE ttActivity.      /* if ttStatus.PropertyValue is not decimal. */
      END. /* FOR EACH ttDatabase, EACH ttSnapshot, FIRST ttStatus*/
    END. /* FIRST ttProperty */
  END. /* DO i = 1 TO EXTENT(vServiceName) */
/* Activity counters in R&D/1/16. Status: Database Service Manager */

END PROCEDURE. /* ProcessServiceManager */

/* ------------------------------------------------------------------------- */

PROCEDURE CheckStaticProperties:

  DEFINE VARIABLE vPropertyValue AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vSectId  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vIsStatic      AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE vSnapshotCount AS INTEGER   NO-UNDO.

  FOR EACH ttDatabase   NO-LOCK,

      EACH ttMenu NO-LOCK
     WHERE ttMenu.ArraySize EQ 0, /* exclude the 'Array' type menu */

      EACH ttProperty   NO-LOCK
     WHERE ttProperty.MenuId EQ ttMenu.MenuId,

      EACH ttSect NO-LOCK
     WHERE ttSect.MenuId EQ ttMenu.MenuId:

    ASSIGN vIsStatic = TRUE
           vPropertyValue = "0":U
           vSnapshotCount = 0
    . /* ASSIGN */
/*
MenuId field is for a link with ttProperty.
SectId field is for a link with ttMenu in the reports.
*/
    CASE ttProperty.PropertyType:
      WHEN "Activity":U THEN /* -------------------------------------------- */
      FOR EACH ttSnapshot NO-LOCK /* any of two indexes */
         WHERE ttSnapshot.DatabaseId EQ ttDatabase.DatabaseId,
      
          EACH ttActivity NO-LOCK /* INDEX ReportOrder */
         WHERE ttActivity.DatabaseId EQ ttSnapshot.DatabaseId
           AND ttActivity.InputLogId EQ ttSnapshot.InputLogId
           AND ttActivity.SnapshotId EQ ttSnapshot.SnapshotId
           AND ttActivity.MenuId     EQ ttSect.MenuId
           AND ttActivity.SectId     EQ ttSect.SectId
           AND ttActivity.PropertyId EQ ttProperty.PropertyId
        WHILE  vIsStatic: /* until first non-zero value: */
        ASSIGN vIsStatic = ttActivity.PropertyValue[1] EQ 0
               vSnapshotCount = vSnapshotCount + 1
        . /* ASSIGN */
      END. /* EACH ttSnapshot, EACH ttActivity */

      WHEN "Resource":U THEN /* -------------------------------------------- */
      FOR EACH ttSnapshot NO-LOCK /* any of two indexes */
         WHERE ttSnapshot.DatabaseId EQ ttDatabase.DatabaseId,
   
          EACH ttResource NO-LOCK /* INDEX ReportOrder */
         WHERE ttResource.DatabaseId EQ ttSnapshot.DatabaseId
           AND ttResource.InputLogId EQ ttSnapshot.InputLogId
           AND ttResource.SnapshotId EQ ttSnapshot.SnapshotId
           AND ttResource.MenuId     EQ ttProperty.MenuId
           AND ttResource.PropertyId EQ ttProperty.PropertyId
         WHILE vIsStatic: /* until first non-zero value: */
        ASSIGN vIsStatic = ttResource.Requests[1] EQ 0
                       AND ttResource.Waits[1]    EQ 0
               vSnapshotCount = vSnapshotCount + 1
        . /* ASSIGN */ /* Requests are always zero for "Statement Cache"
                          even though the waits are non-zero. Bug in V10.2B. */
      END. /* EACH ttSnapshot, EACH ttResource */

      WHEN "Latch":U THEN /* ----------------------------------------------- */
      FOR EACH ttSnapshot NO-LOCK /* any of two indexes */
         WHERE ttSnapshot.DatabaseId EQ ttDatabase.DatabaseId,

          EACH ttLatch NO-LOCK /* INDEX ReportOrder */
         WHERE ttLatch.DatabaseId EQ ttSnapshot.DatabaseId
           AND ttLatch.InputLogId EQ ttSnapshot.InputLogId
           AND ttLatch.SnapshotId EQ ttSnapshot.SnapshotId
           AND ttLatch.MenuId     EQ ttProperty.MenuId
           AND ttLatch.PropertyId EQ ttProperty.PropertyId
         WHILE vIsStatic: /* until first non-zero value: */
        ASSIGN vIsStatic = ttLatch.LatchOwner   EQ "--":U
                       AND ttLatch.LatchLock[1] EQ 0
                       AND ttLatch.LatchWait    EQ 0
               vSnapshotCount = vSnapshotCount + 1
        . /* ASSIGN */ /* Dan Foreman: _Latch-Lock displays zero for certain
        latches (e.g.MTL_LHT and others) even though _Latch-Wait is non-zero.
        Logged as a bug with PSC: issue# 20041202-022.
        Appears to have been fixed some time after V10.0B. */    
      END. /* EACH ttSnapshot, EACH ttLatch */

      OTHERWISE /* If the same property was registered with different types */
      DO:       /* then set ttProperty.IsStatic based on ttStatus only:     */

/* Get vPropertyValue from the first ttStatus: */
        FOR EACH ttSnapshot NO-LOCK /* any of two indexes */
           WHERE ttSnapshot.DatabaseId EQ ttDatabase.DatabaseId,
       
           FIRST ttStatus NO-LOCK /* INDEX ReportOrder*/
           WHERE ttStatus.DatabaseId EQ ttSnapshot.DatabaseId
             AND ttStatus.InputLogId EQ ttSnapshot.InputLogId
             AND ttStatus.SnapshotId EQ ttSnapshot.SnapshotId
             AND ttStatus.MenuId     EQ ttSect.MenuId
             AND ttStatus.SectId     EQ ttSect.SectId
             AND ttStatus.PropertyId EQ ttProperty.PropertyId:
          ASSIGN vPropertyValue = ttStatus.PropertyValue.
          LEAVE.
        END. /* EACH ttSnapshot, EACH ttStatus */

/* Check the difference with the first value: */
        FOR EACH ttSnapshot NO-LOCK /* any of two indexes */
           WHERE ttSnapshot.DatabaseId EQ ttDatabase.DatabaseId,
      
            EACH ttStatus NO-LOCK /* INDEX ReportOrder*/
           WHERE ttStatus.DatabaseId EQ ttSnapshot.DatabaseId
             AND ttStatus.InputLogId EQ ttSnapshot.InputLogId
             AND ttStatus.SnapshotId EQ ttSnapshot.SnapshotId
             AND ttStatus.MenuId     EQ ttSect.MenuId
             AND ttStatus.SectId     EQ ttSect.SectId
             AND ttStatus.PropertyId EQ ttProperty.PropertyId
           WHILE vIsStatic: /* until the difference is found: */
          ASSIGN vIsStatic = ttStatus.PropertyValue EQ vPropertyValue
                 vSnapshotCount = vSnapshotCount + 1
          . /* ASSIGN */
        END. /* EACH ttSnapshot, EACH ttStatus */
      END. /* OTHERWISE PropertyType is "Status" */
    END CASE. /* ttProperty.PropertyType */

    IF vIsStatic AND vSnapshotCount GT 0 THEN
    DO TRANSACTION:
      CREATE ttStatic.
      ASSIGN ttStatic.DatabaseId    = ttDatabase.DatabaseId  
             ttStatic.PropertyId    = ttProperty.PropertyId  
             ttStatic.MenuId        = ttSect.MenuId
             ttStatic.SectId        = ttSect.SectId
             ttStatic.PropertyValue = vPropertyValue
             ttStatic.SnapshotCount = vSnapshotCount
      . /* ASSIGN */
    END. /* DO TRANSACTION */
  END. /* FOR EACH ttDatabase, EACH ttProperty */

END PROCEDURE. /* CheckStaticProperties */

/* ------------------------------------------------------------------------- */

PROCEDURE NewOutputFile:

  DEFINE INPUT-OUTPUT PARAMETER ioFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vVolume AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vExtend AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vHead   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i       AS INTEGER   NO-UNDO.

/* Close the current output file and continue to write to a new one: */
  OUTPUT STREAM LargeReport CLOSE.

  INPUT FROM VALUE(ioFile).
  IMPORT UNFORMATTED vHead.
  INPUT CLOSE.

/* Cut off the file extend: */ 
  ASSIGN i  = MAX(1, R-INDEX(ioFile, ".":U))
         vExtend = SUBSTRING(ioFile, i)
         ioFile  = SUBSTRING(ioFile, 1, i - 1)
/*Cut off the volume number (if it exists): */ 
         i =  MAX(1, R-INDEX(ioFile, ".":U))
         vVolume = SUBSTRING(ioFile, i + 1)
         ioFile  = SUBSTRING(ioFile, 1, i)
  . /* ASSIGN */

  ASSIGN ioFile = ioFile
                + (IF vVolume NE "":U AND TRIM(vVolume, "1234567890":U) EQ "":U
                   THEN STRING(INTEGER(vVolume) + 1)
                   ELSE vVolume + ".2")
                + vExtend
  . /* ASSIGN */

  OUTPUT STREAM LargeReport TO VALUE(ioFile).
  PUT STREAM LargeReport UNFORMATTED vHead SKIP.

END PROCEDURE. /* NewOutputFile */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportStaticProperties:

  DEFINE INPUT PARAMETER ipOutputPrefix AS CHARACTER NO-UNDO.

  &SCOPED-DEFINE Sep "~t"

  IF NOT CAN-FIND(FIRST ttStatic) THEN
  RETURN.

  OUTPUT TO VALUE(ipOutputPrefix + "StaticProperties.txt").

  PUT UNFORMATTED "DbName"
           {&Sep} "Property"
           {&Sep} "Value"
           {&Sep} "Snapshots"
           {&Sep} "Menu"
           {&Sep} "Section"
           {&Sep} "Row"
           {&Sep} "DbHost"
           {&Sep} "DbPath"
  SKIP.

  FOR EACH ttDatabase NO-LOCK,

      EACH ttMenu     NO-LOCK
     WHERE ttMenu.ArraySize EQ 0, /* exclude the 'array' type menus */
 
      EACH ttSect     NO-LOCK
     WHERE ttSect.MenuId EQ ttMenu.MenuId,
      
      EACH ttStatic   NO-LOCK /* INDEX ReportOrder */
     WHERE ttStatic.DatabaseId EQ ttDatabase.DatabaseId
       AND ttStatic.MenuId     EQ ttSect.MenuId
       AND ttStatic.SectId     EQ ttSect.SectId,

     FIRST ttProperty NO-LOCK /* INDEX PropertyId */
     WHERE ttProperty.MenuId     EQ ttStatic.MenuId
       AND ttProperty.PropertyId EQ ttStatic.PropertyId
  
        BY ttDatabase.DatabaseName /* INDEX DatabaseName */
        BY ttDatabase.DatabaseHost
        BY ttDatabase.DatabasePath
  
        BY ttMenu.MenuName /* INDEX MenuName */
        BY ttSect.SectName:

    PUT UNFORMATTED /* DbName   */ ttDatabase.DatabaseName
             {&Sep} /* Property */ ttProperty.PropertyName
             {&Sep} /* Value    */ ttStatic.PropertyValue
             {&Sep} /* Snapshots*/ ttStatic.SnapshotCount
             {&Sep} /* Menu     */ ttMenu.MenuName
             {&Sep} /* Section  */ ttSect.SectName
             {&Sep} /* Row      */ ttProperty.PropertyRow
             {&Sep} /* DbHost   */ ttDatabase.DatabaseHost
             {&Sep} /* DbPath   */ ttDatabase.DatabasePath
    SKIP.
  END. /* FOR FIRST ttDatabase, EACH ttMenu */

  OUTPUT CLOSE.

END PROCEDURE. /* ReportStaticProperties */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportStatusProperties:

  DEFINE INPUT PARAMETER ipOutputPrefix AS CHARACTER NO-UNDO.

  &SCOPED-DEFINE Sep "~t"
  
/* Excel limits: total number of rows is 1,048,576 rows */
  DEFINE VARIABLE vRowLimit AS INTEGER NO-UNDO INITIAL 1048575.
  DEFINE VARIABLE vRowCount AS INTEGER NO-UNDO.

  DEFINE VARIABLE vOutputFile AS CHARACTER NO-UNDO.
  ASSIGN vOutputFile = ipOutputPrefix + "StatusProperties.txt".

  IF NOT CAN-FIND(FIRST ttStatus WHERE
     NOT CAN-FIND(FIRST ttStatic /* INDEX ReportOrder */
                  WHERE ttStatic.DatabaseId EQ ttStatus.DatabaseId
                    AND ttStatic.MenuId     EQ ttStatus.MenuId
                    AND ttStatic.SectId     EQ ttStatus.SectId
                    AND ttStatic.PropertyId EQ ttStatus.PropertyId)) THEN
  RETURN.

  OUTPUT STREAM LargeReport TO VALUE(vOutputFile).

  PUT STREAM LargeReport UNFORMATTED
             "DbName"
      {&Sep} "LogId"
      {&Sep} "Snapshot"
      {&Sep} "Date/Time"
      {&Sep} "Property"
      {&Sep} "Value"
      {&Sep} "Menu"
      {&Sep} "Section"
      {&Sep} "Row"
      {&Sep} "DbHost"
      {&Sep} "DbPath"
  SKIP.

  ASSIGN vRowCount = 1.

  FOR EACH ttDatabase NO-LOCK,

      EACH ttSnapshot NO-LOCK /* any of two indexes */
     WHERE ttSnapshot.DatabaseId EQ ttDatabase.DatabaseId,

      EACH ttMenu     NO-LOCK
     WHERE ttMenu.ArraySize EQ 0, /* exclude the 'array' type menus */

      EACH ttSect     NO-LOCK
     WHERE ttSect.MenuId EQ ttMenu.MenuId,

      EACH ttStatus   NO-LOCK /* INDEX ReportOrder */
     WHERE ttStatus.DatabaseId EQ ttSnapshot.DatabaseId
       AND ttStatus.InputLogId EQ ttSnapshot.InputLogId
       AND ttStatus.SnapshotId EQ ttSnapshot.SnapshotId
       AND ttStatus.MenuId     EQ ttSect.MenuId
       AND ttStatus.SectId     EQ ttSect.SectId
       AND NOT CAN-FIND(
     FIRST ttStatic           /* INDEX ReportOrder */
     WHERE ttStatic.DatabaseId EQ ttStatus.DatabaseId
       AND ttStatic.MenuId     EQ ttStatus.MenuId
       AND ttStatic.SectId     EQ ttStatus.SectId
       AND ttStatic.PropertyId EQ ttStatus.PropertyId),
 
     FIRST ttProperty NO-LOCK /* INDEX PropertyId */
     WHERE ttProperty.MenuId     EQ ttStatus.MenuId
       AND ttProperty.PropertyId EQ ttStatus.PropertyId
  
         BY ttDatabase.DatabaseName /* INDEX DatabaseName */
         BY ttDatabase.DatabaseHost
         BY ttDatabase.DatabasePath

         BY ttSnapshot.SnapshotTime

         BY ttMenu.MenuName /* INDEX MenuName */
         BY ttSect.SectName:
      /* BY ttProperty.PropertyId */
 
    ASSIGN vRowCount = vRowCount + 1.
    
    IF vRowCount GT vRowLimit THEN
    DO:
      RUN NewOutputFile(INPUT-OUTPUT vOutputFile).
      ASSIGN vRowCount = 2.
    END.

    PUT STREAM LargeReport UNFORMATTED
             /* DbName   */ ttDatabase.DatabaseName
      {&Sep} /* LogId    */ ttSnapshot.InputLogId
      {&Sep} /* Snapshot */ ttSnapshot.SnapshotId
      {&Sep} /* Date/Time*/ DateTime2Str(ttStatus.SnapshotTime)
      {&Sep} /* Property */ ttProperty.PropertyName
      {&Sep} /* Value    */ ttStatus.PropertyValue
      {&Sep} /* Menu     */ ttMenu.MenuName
      {&Sep} /* Section  */ ttSect.SectName
      {&Sep} /* Row      */ ttProperty.PropertyRow
      {&Sep} /* DbHost   */ ttDatabase.DatabaseHost
      {&Sep} /* DbPath   */ ttDatabase.DatabasePath
    SKIP.
  END. /* FOR EACH ttDatabase, EACH ttSnapshot, EACH ttMenu, EACH ttStatus */

  OUTPUT STREAM LargeReport CLOSE.

END PROCEDURE. /* ReportStatusProperties */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportActivityProperties:

  DEFINE INPUT PARAMETER ipOutputPrefix AS CHARACTER NO-UNDO.

  &SCOPED-DEFINE Sep "~t"
  
/* Excel limits: total number of rows is 1,048,576 rows */
  DEFINE VARIABLE vRowLimit AS INTEGER NO-UNDO INITIAL 1048575.
  DEFINE VARIABLE vRowCount AS INTEGER NO-UNDO.

  DEFINE VARIABLE vOutputFile AS CHARACTER NO-UNDO.
  ASSIGN vOutputFile = ipOutputPrefix + "ActivityProperties.txt".

  IF NOT CAN-FIND(FIRST ttActivity WHERE
     NOT CAN-FIND(FIRST ttStatic /* INDEX ReportOrder */
                  WHERE ttStatic.DatabaseId EQ ttActivity.DatabaseId
                    AND ttStatic.MenuId     EQ ttActivity.MenuId
                    AND ttStatic.SectId     EQ ttActivity.SectId
                    AND ttStatic.PropertyId EQ ttActivity.PropertyId)) THEN
  RETURN.

  OUTPUT STREAM LargeReport TO VALUE(vOutputFile).

  PUT STREAM LargeReport UNFORMATTED
             "DbName"
      {&Sep} "LogId"
      {&Sep} "Snapshot"
      {&Sep} "Date/Time"
      {&Sep} "Interval"
      {&Sep} "Property"
      {&Sep} "Total"
      {&Sep} "Per Sec"
      {&Sep} "Menu"
      {&Sep} "Section"
      {&Sep} "Row"
      {&Sep} "DbHost"
      {&Sep} "DbPath"
  SKIP.

  ASSIGN vRowCount = 1.

  FOR EACH ttDatabase NO-LOCK,

      EACH ttSnapshot NO-LOCK /* any of two indexes */
     WHERE ttSnapshot.DatabaseId EQ ttDatabase.DatabaseId,

      EACH ttMenu     NO-LOCK
     WHERE ttMenu.ArraySize EQ 0, /* exclude the 'array' type menus */

      EACH ttSect     NO-LOCK
     WHERE ttSect.MenuId EQ ttMenu.MenuId,

      EACH ttActivity NO-LOCK
     WHERE ttActivity.DatabaseId EQ ttSnapshot.DatabaseId
       AND ttActivity.InputLogId EQ ttSnapshot.InputLogId
       AND ttActivity.SnapshotId EQ ttSnapshot.SnapshotId
       AND ttActivity.MenuId     EQ ttSect.MenuId
       AND ttActivity.SectId     EQ ttSect.SectId
       AND NOT CAN-FIND(
     FIRST ttStatic           /* INDEX ReportOrder */
     WHERE ttStatic.DatabaseId EQ ttActivity.DatabaseId
       AND ttStatic.MenuId     EQ ttActivity.MenuId
       AND ttStatic.SectId     EQ ttActivity.SectId
       AND ttStatic.PropertyId EQ ttActivity.PropertyId),
 
     FIRST ttProperty NO-LOCK /* INDEX PropertyId */
     WHERE ttProperty.MenuId     EQ ttActivity.MenuId
       AND ttProperty.PropertyId EQ ttActivity.PropertyId
  
         BY ttDatabase.DatabaseName /* INDEX DatabaseName */
         BY ttDatabase.DatabaseHost
         BY ttDatabase.DatabasePath

         BY ttSnapshot.SnapshotTime

         BY ttMenu.MenuName /* INDEX MenuName */
         BY ttSect.SectName:
      /* BY ttProperty.PropertyId */

    ASSIGN vRowCount = vRowCount + 1.
    
    IF vRowCount GT vRowLimit THEN
    DO:
      RUN NewOutputFile(INPUT-OUTPUT vOutputFile).
      ASSIGN vRowCount = 2.
    END.

    PUT STREAM LargeReport UNFORMATTED
             /* DbName   */ ttDatabase.DatabaseName
      {&Sep} /* LogId    */ ttSnapshot.InputLogId
      {&Sep} /* Snapshot */ ttSnapshot.SnapshotId
      {&Sep} /* Date/Time*/ DateTime2Str(ttSnapshot.SnapshotTime)
      {&Sep} /* Interval */ ttSnapshot.StatInterval
      {&Sep} /* Property */ ttProperty.PropertyName
      {&Sep} /* Total    */ ttActivity.PropertyValue[1]
      {&Sep} /* Per Sec  */ ttActivity.PropertyValue[2]
      {&Sep} /* Menu     */ ttMenu.MenuName
      {&Sep} /* Section  */ ttSect.SectName
      {&Sep} /* Row      */ ttProperty.PropertyRow
      {&Sep} /* DbHost   */ ttDatabase.DatabaseHost
      {&Sep} /* DbPath   */ ttDatabase.DatabasePath
    SKIP.
  END. /* FOR EACH ttDatabase, EACH ttSnapshot, EACH ttMenu, EACH ttActivity */

  OUTPUT STREAM LargeReport CLOSE.

END PROCEDURE. /* ReportActivityProperties */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportResourceProperties:

  DEFINE INPUT PARAMETER ipOutputPrefix AS CHARACTER NO-UNDO.

  &SCOPED-DEFINE Sep "~t"
  
/* Excel limits: total number of rows is 1,048,576 rows */
  DEFINE VARIABLE vRowLimit AS INTEGER NO-UNDO INITIAL 1048575.
  DEFINE VARIABLE vRowCount AS INTEGER NO-UNDO.

  DEFINE VARIABLE vOutputFile AS CHARACTER NO-UNDO.
  ASSIGN vOutputFile = ipOutputPrefix + "ResourceProperties.txt".

  IF NOT CAN-FIND(FIRST ttResource WHERE
     NOT CAN-FIND(FIRST ttStatic /* INDEX ReportOrder */
                  WHERE ttStatic.DatabaseId EQ ttResource.DatabaseId
                    AND ttStatic.MenuId     EQ ttResource.MenuId
                    AND ttStatic.SectId     EQ 0
                    AND ttStatic.PropertyId EQ ttResource.PropertyId)) THEN
  RETURN.

  OUTPUT STREAM LargeReport TO VALUE(vOutputFile).

  PUT STREAM LargeReport UNFORMATTED
             "DbName"
      {&Sep} "LogId"
      {&Sep} "Snapshot"
      {&Sep} "Date/Time"
      {&Sep} "Interval"
      {&Sep} "Property"
      {&Sep} "Requests"
      {&Sep} "Waits"
      {&Sep} "Requests/Sec"
      {&Sep} "Waits/Sec"
      {&Sep} "Menu"
      {&Sep} "Row"
      {&Sep} "DbHost"
      {&Sep} "DbPath"
  SKIP.

  ASSIGN vRowCount = 1.

  FOR EACH ttDatabase NO-LOCK,

      EACH ttSnapshot NO-LOCK /* any of two indexes */
     WHERE ttSnapshot.DatabaseId EQ ttDatabase.DatabaseId,

      EACH ttMenu     NO-LOCK
     WHERE ttMenu.MenuName EQ "Activity: Resource Queues":U
        OR ttMenu.MenuName EQ "Activity: TXE Lock Activity":U,

      EACH ttResource NO-LOCK
     WHERE ttResource.DatabaseId EQ ttSnapshot.DatabaseId
       AND ttResource.InputLogId EQ ttSnapshot.InputLogId
       AND ttResource.SnapshotId EQ ttSnapshot.SnapshotId
       AND ttResource.MenuId     EQ ttMenu.MenuId
       AND NOT CAN-FIND(
     FIRST ttStatic           /* INDEX ReportOrder */
     WHERE ttStatic.DatabaseId EQ ttResource.DatabaseId
       AND ttStatic.MenuId     EQ ttResource.MenuId
       AND ttStatic.PropertyId EQ ttResource.PropertyId),
 
     FIRST ttProperty NO-LOCK /* INDEX PropertyId */
     WHERE ttProperty.MenuId     EQ ttResource.MenuId
       AND ttProperty.PropertyId EQ ttResource.PropertyId
  
         BY ttDatabase.DatabaseName /* INDEX DatabaseName */
         BY ttDatabase.DatabaseHost
         BY ttDatabase.DatabasePath

         BY ttSnapshot.SnapshotTime

         BY ttMenu.MenuName: /* INDEX MenuName */
      /* BY ttProperty.PropertyId */

    ASSIGN vRowCount = vRowCount + 1.
    
    IF vRowCount GT vRowLimit THEN
    DO:
      RUN NewOutputFile(INPUT-OUTPUT vOutputFile).
      ASSIGN vRowCount = 2.
    END.

    PUT STREAM LargeReport UNFORMATTED
             /* DbName      */ ttDatabase.DatabaseName
      {&Sep} /* LogId       */ ttSnapshot.InputLogId
      {&Sep} /* Snapshot    */ ttSnapshot.SnapshotId
      {&Sep} /* Date/Time   */ DateTime2Str(ttSnapshot.SnapshotTime)
      {&Sep} /* Interval    */ ttSnapshot.StatInterval
      {&Sep} /* Property    */ ttProperty.PropertyName
      {&Sep} /* Requests    */ ttResource.Requests[1]
      {&Sep} /* Waits       */ ttResource.Waits[1]
      {&Sep} /* Requests/Sec*/ ttResource.Requests[2]
      {&Sep} /* Waits/Sec   */ ttResource.Waits[2]
      {&Sep} /* Menu        */ ttMenu.MenuName
      {&Sep} /* Row         */ ttProperty.PropertyRow
      {&Sep} /* DbHost      */ ttDatabase.DatabaseHost
      {&Sep} /* DbPath      */ ttDatabase.DatabasePath
    SKIP.
  END. /* FOR EACH ttDatabase, EACH ttSnapshot, EACH ttMenu, EACH ttResource */

  OUTPUT STREAM LargeReport CLOSE.

END PROCEDURE. /* ReportResourceProperties */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportActivityLatchCounts:

  DEFINE INPUT PARAMETER ipOutputPrefix AS CHARACTER NO-UNDO.

  &SCOPED-DEFINE Sep "~t"
  
/* Excel limits: total number of rows is 1,048,576 rows */
  DEFINE VARIABLE vRowLimit AS INTEGER NO-UNDO INITIAL 1048575.
  DEFINE VARIABLE vRowCount AS INTEGER NO-UNDO.

  DEFINE VARIABLE vOutputFile AS CHARACTER NO-UNDO.
  ASSIGN vOutputFile = ipOutputPrefix + "ActivityLatchCounts.txt".

  IF NOT CAN-FIND(FIRST ttLatch WHERE
     NOT CAN-FIND(FIRST ttStatic /* INDEX ReportOrder */
                  WHERE ttStatic.DatabaseId EQ ttLatch.DatabaseId
                    AND ttStatic.MenuId     EQ ttLatch.MenuId
                    AND ttStatic.SectId     EQ 0
                    AND ttStatic.PropertyId EQ ttLatch.PropertyId)) THEN
  RETURN.

  OUTPUT STREAM LargeReport TO VALUE(vOutputFile).

  PUT STREAM LargeReport UNFORMATTED
             "DbName"
      {&Sep} "LogId"
      {&Sep} "Snapshot"
      {&Sep} "Date/Time"
      {&Sep} "Interval"
      {&Sep} "Latch"
      {&Sep} "Id"
      {&Sep} "Owner"
      {&Sep} "Locks"
      {&Sep} "Locks/Sec"
      {&Sep} "Naps/Sec"
      {&Sep} "Busy/Sec"
      {&Sep} "Spins/Sec"
      {&Sep} "Nap Max HWM"
      {&Sep} "Nap Max Total"
      {&Sep} "DbHost"
      {&Sep} "DbPath"
  SKIP.

  ASSIGN vRowCount = 1.
  
  FOR EACH ttDatabase NO-LOCK,

      EACH ttSnapshot NO-LOCK /* any of two indexes */
     WHERE ttSnapshot.DatabaseId EQ ttDatabase.DatabaseId,

     FIRST ttMenu     NO-LOCK
     WHERE ttMenu.MenuName EQ "Activity: Latch Counts":U,
 
      EACH ttLatch    NO-LOCK
     WHERE ttLatch.DatabaseId EQ ttSnapshot.DatabaseId
       AND ttLatch.InputLogId EQ ttSnapshot.InputLogId
       AND ttLatch.SnapshotId EQ ttSnapshot.SnapshotId
       AND ttLatch.MenuId     EQ ttMenu.MenuId
       AND NOT CAN-FIND(
     FIRST ttStatic           /* INDEX ReportOrder */
     WHERE ttStatic.DatabaseId EQ ttLatch.DatabaseId
       AND ttStatic.MenuId     EQ ttLatch.MenuId
       AND ttStatic.PropertyId EQ ttLatch.PropertyId),
 
     FIRST ttProperty NO-LOCK /* INDEX PropertyId */
     WHERE ttProperty.MenuId     EQ ttLatch.MenuId
       AND ttProperty.PropertyId EQ ttLatch.PropertyId
  
        BY ttDatabase.DatabaseName /* INDEX DatabaseName */
        BY ttDatabase.DatabaseHost
        BY ttDatabase.DatabasePath

        BY ttSnapshot.SnapshotTime

        BY ttMenu.MenuName: /* INDEX MenuName */
     /* BY ttProperty.PropertyId */

    ASSIGN vRowCount = vRowCount + 1.
    
    IF vRowCount GT vRowLimit THEN
    DO:
      RUN NewOutputFile(INPUT-OUTPUT vOutputFile).
      ASSIGN vRowCount = 2.
    END.

    PUT STREAM LargeReport UNFORMATTED
             /* DbName       */ ttDatabase.DatabaseName
      {&Sep} /* LogId        */ ttSnapshot.InputLogId
      {&Sep} /* Snapshot     */ ttSnapshot.SnapshotId
      {&Sep} /* Date/Time    */ DateTime2Str(ttSnapshot.SnapshotTime)
      {&Sep} /* Interval     */ ttSnapshot.StatInterval
      {&Sep} /* Latch        */ ttProperty.PropertyName
      {&Sep} /* Id           */ ttProperty.PropertyRow
      {&Sep} /* Owner        */ ttLatch.LatchOwner
      {&Sep} /* Locks        */ ttLatch.LatchLock[1]
      {&Sep} /* Locks/Sec    */ ttLatch.LatchLock[2]
      {&Sep} /* Naps/Sec     */ ttLatch.LatchWait
      {&Sep} /* Busy/Sec     */ ttLatch.LatchBusy
      {&Sep} /* Spins/Sec    */ ttLatch.LatchSpin
      {&Sep} /* Nap Max HWM  */ ttLatch.MaxNapTime
      {&Sep} /* Nap Max Total*/ ttLatch.MaxNapCount
      {&Sep} /* DbHost       */ ttDatabase.DatabaseHost
      {&Sep} /* DbPath       */ ttDatabase.DatabasePath
    SKIP.
  END. /* FOR FIRST ttDatabase, EACH ttSnapshot, EACH ttMenu, EACH ttLatch */

  OUTPUT STREAM LargeReport CLOSE.

END PROCEDURE. /* ReportActivityLatchCounts */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportArrayMenus:
  
  DEFINE INPUT PARAMETER ipOutputPrefix AS CHARACTER NO-UNDO.

  &SCOPED-DEFINE Sep "~t"
  
  DEFINE VARIABLE vOutputFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vHeadSize   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vHeadLine   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i           AS INTEGER   NO-UNDO.

/* Excel limits: total number of rows is 1,048,576 rows */
  DEFINE VARIABLE vRowLimit AS INTEGER NO-UNDO INITIAL 1048575.
  DEFINE VARIABLE vRowCount AS INTEGER NO-UNDO.

  DEFINE VARIABLE vArrayHead AS CHARACTER NO-UNDO EXTENT {&ArraySize}.

/* Converting two level headers to one level:*/

/* R&D/1/3. Status: Servers or R&D/1/17. Status: Servers By Broker: */
  DEFINE VARIABLE vStatusServers8 AS CHARACTER NO-UNDO EXTENT {&ArraySize} INITIAL
["Sv No", "Pid", "Type", "Protocol", "Logins", "Cur Users", "Max Users", "Port Num", ""].

/* "Pend Users" was added in 9.1B15: */
  DEFINE VARIABLE vStatusServers9 AS CHARACTER NO-UNDO EXTENT {&ArraySize} INITIAL
["Sv No", "Pid", "Type", "Protocol", "Logins", "Pend Users", "Cur Users", "Max Users", "Port Num", ""].

/* R&D/1/5. Status: Files */
  DEFINE VARIABLE vStatusFiles AS CHARACTER NO-UNDO EXTENT {&ArraySize} INITIAL
["File name", "Size (KB)", "Extend (KB)", ""].

/* R&D/3/2. I/O Operations by Process */
  DEFINE VARIABLE vIObyProcess9 AS CHARACTER NO-UNDO EXTENT {&ArraySize} INITIAL
["Usr", "Name", "DB Access", "DB Read", "DB Write", "BI Read", "BI Write", "AI Read", "AI Write", ""].

/* "Domain" column was added in V11: */
  DEFINE VARIABLE vIObyProcess10 AS CHARACTER NO-UNDO EXTENT {&ArraySize} INITIAL
  ["Usr:Ten", "Name", "Domain", "DB Access", "DB Read", "DB Write", "BI Read", "BI Write", "AI Read", "AI Write", ""].

/* R&D/3/3. Lock Requests By User */
  DEFINE VARIABLE vLocksByUser8 AS CHARACTER NO-UNDO EXTENT {&ArraySize} INITIAL
["Usr Num", "User Name", "Record Locks", "Record Waits", "Trans Locks", "Trans Waits", "Schema Locks", "Schema Waits", ""].

/* "Domain" was added in V11: */
  DEFINE VARIABLE vLocksByUser9 AS CHARACTER NO-UNDO EXTENT {&ArraySize} INITIAL
["Usr:Ten", "Name User", "Domain", "Record Locks", "Record Waits", "Trans Locks", "Trans Waits", "Schema Locks", "Schema Waits", ""].

/* "Record Current" and "Record HWM" were added in V11.7: */
  DEFINE VARIABLE vLocksByUser11 AS CHARACTER NO-UNDO EXTENT {&ArraySize} INITIAL
["Usr:Ten", "Name User", "Domain", "Record Locks", "Record Waits", "Record Current", "Record HWM", "Trans Locks", "Trans Waits", "Schema Locks", "Schema Waits", ""].

/* R&D/2/9. Activity: I/O Operations by File */
  DEFINE VARIABLE vOpsByFile AS CHARACTER NO-UNDO EXTENT {&ArraySize} INITIAL
["Interval", "Db File", "OS Reads", "OS Writes", "Extends", ""].

/* R&D/6/14.  Activity: I/O by File */
  DEFINE VARIABLE vIOsByFile AS CHARACTER NO-UNDO EXTENT {&ArraySize} INITIAL
["Interval", "Db File", "Buffered Reads", "Buffered Writes", "Unbuffered Reads", "Unbuffered Writes", ""].

  FOR EACH ttMenu NO-LOCK
     WHERE ttMenu.ArraySize GT 0:

    ASSIGN vOutputFile = ttMenu.MenuName
           vOutputFile = REPLACE(vOutputFile, ":":U, "":U)
           vOutputFile = REPLACE(vOutputFile, "/":U, "":U)
           vOutputFile = REPLACE(vOutputFile, " ":U, "_":U)
           vOutputFile = ipOutputPrefix + vOutputFile + ".txt":U

           vHeadLine  = TRIM(ttMenu.ArrayHead)
    . /* ASSIGN */

/* ArraySize() creates ttColumn records based on the specified array: */
    CASE ttMenu.MenuName:
      WHEN "Status: Files":U
      THEN ASSIGN vHeadSize = ArraySize(ttMenu.MenuId, vStatusFiles).

      WHEN "Activity: I/O Operations by File":U
      THEN ASSIGN vHeadSize = ArraySize(ttMenu.MenuId, vOpsByFile).

      WHEN "Activity: I/O by File":U
      THEN ASSIGN vHeadSize = ArraySize(ttMenu.MenuId, vIOsByFile).

      WHEN "Status: Servers":U OR
      WHEN "Status: Servers By Broker":U THEN
      IF ttMenu.ArraySize LE 8
      THEN ASSIGN vHeadSize = ArraySize(ttMenu.MenuId, vStatusServers8).
      ELSE ASSIGN vHeadSize = ArraySize(ttMenu.MenuId, vStatusServers9).

      WHEN "I/O Operations by Process":U THEN
      IF ttMenu.ArraySize LE 9
      THEN ASSIGN vHeadSize = ArraySize(ttMenu.MenuId, vIObyProcess9).
      ELSE ASSIGN vHeadSize = ArraySize(ttMenu.MenuId, vIObyProcess10).

      WHEN "Lock Requests By User":U THEN
      IF ttMenu.ArraySize LE 8
      THEN ASSIGN vHeadSize = ArraySize(ttMenu.MenuId, vLocksByUser8).
      ELSE
      IF ttMenu.ArraySize LE 10
      THEN ASSIGN vHeadSize = ArraySize(ttMenu.MenuId, vLocksByUser9).
      ELSE ASSIGN vHeadSize = ArraySize(ttMenu.MenuId, vLocksByUser11).

      OTHERWISE ASSIGN vHeadSize = HeaderSize(ttMenu.MenuId, vHeadLine).
    END CASE. /* ttMenu.MenuName */

/* Add the names to the untitled columns: */
IF vHeadSize LT ttMenu.ArraySize THEN
    CASE ttMenu.MenuName:
      WHEN "Status: Active Transactions":U THEN
      ASSIGN vHeadSize = Add2Header(ttMenu.MenuId, vHeadSize + 1, "Trans Flags":U).
/* [snip] Trans State <Trans Flags>
   [snip] Begin   FWD                                                     
   [snip] Phase 2 FWD                                                     
   [snip] Active  FWD                                                     
   
   Last column (with FWD as the values) is really named "Trans Flags".
   "Trans State" column is its predecessor.
*/
      WHEN "Status: Blocked Clients":U THEN
      ASSIGN vHeadSize = Add2Header(ttMenu.MenuId, vHeadSize + 1, "debghb":U).
/* [snip]  Login time     Schema Timestamp  <debghb>
   [snip] 01/06/18 11:55        1514436903   0
   [snip] 01/06/18 11:55        1514436903   0
   
   Last column is available when 'debghb' is enabled.
   Its meaning is unknown and the value seems to be always 0.
*/
      WHEN "Status: Buffer Locks":U THEN
      ASSIGN vHeadSize = Add2Header(ttMenu.MenuId, 1, "Num":U).
/*
<Num> User:Ten                    DBKEY Area                 Hash T S Usect
  1     13                          192    6                  468 S S     5
  2     17                          192    6                  468 S S     4
  3     18                          192    6                  468 S S     3
  4     23                          192    6                  468 S S     3
  
  "Num" column (first one) is missed. It exists, for example,
  in "Status: Cache Entries" or "Status: Locked Buffers"
*/
    END CASE. /* ttMenu.MenuName (the untitled columns) */

    OUTPUT STREAM LargeReport TO VALUE(vOutputFile).
    ASSIGN vRowCount = 1.

    IF ttMenu.MenuName EQ "Checkpoints":U THEN
    DO:
      PUT STREAM LargeReport UNFORMATTED          
               "DbName"
        {&Sep} "Ckpt No."
        {&Sep} "Date"
      . /* PUT */
      FOR EACH ttColumn NO-LOCK
         WHERE ttColumn.MenuId   EQ ttMenu.MenuId
           AND ttColumn.ColumnId GT 1
            BY ttColumn.ColumnId:
        PUT STREAM LargeReport UNFORMATTED {&Sep} ttColumn.ColumnName.
      END. /* FOR EACH ttColumn */

      DO i = vHeadSize + 1 TO ttMenu.ArraySize:
        PUT STREAM LargeReport UNFORMATTED {&Sep}.
      END.
      PUT STREAM LargeReport UNFORMATTED
        {&Sep} "DbHost"
        {&Sep} "DbPath"
      SKIP.

      FOR EACH ttDatabase NO-LOCK,

          EACH ttSnapshot NO-LOCK /* any of two indexes */
         WHERE ttSnapshot.DatabaseId EQ ttDatabase.DatabaseId,

          EACH ttArray    NO-LOCK
         WHERE ttArray.DatabaseId EQ ttSnapshot.DatabaseId
           AND ttArray.InputLogId EQ ttSnapshot.InputLogId
           AND ttArray.SnapshotId EQ ttSnapshot.SnapshotId
           AND ttArray.MenuId     EQ ttMenu.MenuId

         BREAK BY ttDatabase.DatabaseId
               BY INTEGER(ttArray.ArrayValue[1]) /* Ckpt No.*/
               BY ttArray.ArrayRow:

        IF  LAST-OF(ttArray.ArrayRow)
        AND LAST-OF(INTEGER(ttArray.ArrayValue[1])) THEN .
        ELSE NEXT.

        ASSIGN vRowCount = vRowCount + 1.
        
        IF vRowCount GT vRowLimit THEN
        DO:
          RUN NewOutputFile(INPUT-OUTPUT vOutputFile).
          ASSIGN vRowCount = 2.
        END.

        PUT STREAM LargeReport UNFORMATTED
                 /* DbName  */ ttDatabase.DatabaseName
          {&Sep} /* Ckpt No.*/ ttArray.ArrayValue[1]
          {&Sep} /* Date    */ DATE(ttSnapshot.SnapshotTime)
        . /* PUT */
        DO i = 2 TO ttArray.ArraySize:
          PUT STREAM LargeReport UNFORMATTED
            {&Sep} /* Value */ ttArray.ArrayValue[i]
          . /* PUT */
        END.

        DO i = ttArray.ArraySize + 1 TO vHeadSize:
          PUT STREAM LargeReport UNFORMATTED {&Sep}.
        END.

        PUT STREAM LargeReport UNFORMATTED
          {&Sep} /* DbHost */  ttDatabase.DatabaseHost
          {&Sep} /* DbPath */  ttDatabase.DatabasePath
        SKIP.

      END. /* FOR EACH ttDatabase, EACH ttSnapshot, EACH ttArray */
    END. /* IF ttMenu.MenuName EQ "Checkpoints" */
    ELSE
/*  IF ttMenu.MenuName NE "Checkpoints" THEN */
    DO:
      PUT STREAM LargeReport UNFORMATTED
               "DbName"
        {&Sep} "LogId"
        {&Sep} "Snapshot"
        {&Sep} "Date/Time"
      . /* PUT */

      FOR EACH ttColumn NO-LOCK
         WHERE ttColumn.MenuId   EQ ttMenu.MenuId
            BY ttColumn.ColumnId:
        PUT STREAM LargeReport UNFORMATTED {&Sep} ttColumn.ColumnName.
      END. /* FOR EACH ttColumn */

/* If menu header is shorter than the arrays of the values: */
      DO i = vHeadSize + 1 TO ttMenu.ArraySize:
        PUT STREAM LargeReport UNFORMATTED {&Sep} "Col" + STRING(i).
      END.
      PUT STREAM LargeReport UNFORMATTED
        {&Sep} "DbHost"
        {&Sep} "DbPath"
      SKIP.

      FOR EACH ttDatabase NO-LOCK,

          EACH ttSnapshot NO-LOCK /* any of two indexes */
         WHERE ttSnapshot.DatabaseId EQ ttDatabase.DatabaseId,

          EACH ttArray    NO-LOCK
         WHERE ttArray.DatabaseId EQ ttSnapshot.DatabaseId
           AND ttArray.InputLogId EQ ttSnapshot.InputLogId
           AND ttArray.SnapshotId EQ ttSnapshot.SnapshotId
           AND ttArray.MenuId     EQ ttMenu.MenuId

            BY ttDatabase.DatabaseId
            BY ttSnapshot.SnapshotTime
            BY ttArray.ArrayRow:

        ASSIGN vRowCount = vRowCount + 1.
    
        IF vRowCount GT vRowLimit THEN
        DO:
          RUN NewOutputFile(INPUT-OUTPUT vOutputFile).
          ASSIGN vRowCount = 2.
        END.

        PUT STREAM LargeReport UNFORMATTED
                 /* DbName   */ ttDatabase.DatabaseName
          {&Sep} /* LogId    */ ttSnapshot.InputLogId
          {&Sep} /* Snapshot */ ttSnapshot.SnapshotId
          {&Sep} /* Date/Time*/ DateTime2Str(ttArray.SnapshotTime)
        . /* PUT */
        DO i = 1 TO ttArray.ArraySize:
          PUT STREAM LargeReport UNFORMATTED {&Sep} ttArray.ArrayValue[i].
        END.

/* If the array of the values is shorter than menu header: */
        DO i = ttArray.ArraySize + 1 TO vHeadSize:
          PUT STREAM LargeReport UNFORMATTED {&Sep}.
        END.

        PUT STREAM LargeReport UNFORMATTED
          {&Sep} /* DbHost */  ttDatabase.DatabaseHost
          {&Sep} /* DbPath */  ttDatabase.DatabasePath
        SKIP.

      END. /* FOR EACH ttDatabase, EACH ttSnapshot, EACH ttArray */
    END. /* IF ttMenu.MenuName NE "Checkpoints" */

    OUTPUT STREAM LargeReport CLOSE.

  END. /* FOR EACH ttMenu */

END PROCEDURE. /* ReportArrayMenus */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportStatActiveTrans:
  
  DEFINE INPUT PARAMETER ipOutputPrefix AS CHARACTER NO-UNDO.

  &SCOPED-DEFINE Sep "~t"
  
  DEFINE VARIABLE vOutputFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vMenuId     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTransId    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTransState AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTransFlags AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTridRange  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTridSpeed  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLastTrid   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vPrevTrid   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vPrevTime   AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vInterval   AS INTEGER   NO-UNDO.

  ASSIGN vMenuId     = MenuId("Status: Active Transactions":U)
         vOutputFile = ipOutputPrefix + "Status_Active_Transactions.stat.txt":U
         vTransId    = ColumnId(vMenuId, "Trans id":U)
         vTransState = ColumnId(vMenuId, "Trans State":U)
         vTransFlags = ColumnId(vMenuId, "Trans Flags":U)
  . /* ASSIGN */
/*
Trans State:
  None (Dead)
  Begin (Allocated)
  Active (doing stuff)
  Prep (Preparing)
  Phase 1 (Prepared)
  Phase 2 (Committing)
 
Trans Flags:
  FWD
  UNDO
*/

  IF vMenuId EQ ?
  OR NOT CAN-FIND(FIRST ttArray WHERE ttArray.MenuId EQ vMenuId) THEN
  RETURN.

  OUTPUT TO VALUE(vOutputFile).

  PUT UNFORMATTED
           "DbName"
    {&Sep} "LogId"
    {&Sep} "Snapshot"
    {&Sep} "Date/Time"
    {&Sep} "Trid Range"
    {&Sep} "Trids/Sec"
    {&Sep} "Count"
    {&Sep} "Sub-count"
    {&Sep} "Trans State"
    {&Sep} "Trans Flags"
    {&Sep} "DbHost"
    {&Sep} "DbPath"
  SKIP.

  FOR EACH ttDatabase NO-LOCK,

      EACH ttSnapshot NO-LOCK
     WHERE ttSnapshot.DatabaseId EQ ttDatabase.DatabaseId

  BREAK BY ttDatabase.DatabaseName
        BY ttSnapshot.InputLogId
        BY ttSnapshot.SnapshotTime:

    IF FIRST-OF(ttSnapshot.InputLogId) THEN
    ASSIGN vPrevTrid = ?
           vPrevTime = ?
    . /* ASSIGN */

    IF NOT CAN-FIND(
       FIRST ttArray NO-LOCK
       WHERE ttArray.DatabaseId EQ ttDatabase.DatabaseId
         AND ttArray.InputLogId EQ ttSnapshot.InputLogId
         AND ttArray.SnapshotId EQ ttSnapshot.SnapshotId
         AND ttArray.MenuId     EQ vMenuId) THEN
    NEXT.

    EMPTY TEMP-TABLE ttArraySet.

    FOR EACH ttArray NO-LOCK
       WHERE ttArray.DatabaseId EQ ttDatabase.DatabaseId
         AND ttArray.InputLogId EQ ttSnapshot.InputLogId
         AND ttArray.SnapshotId EQ ttSnapshot.SnapshotId
         AND ttArray.MenuId     EQ vMenuId
    BREAK BY ttArray.ArrayValue[vTransState]
          BY ttArray.ArrayValue[vTransFlags]:

      ACCUMULATE "Set":U (SUB-COUNT BY ttArray.ArrayValue[vTransFlags]).

      ACCUMULATE INTEGER(ttArray.ArrayValue[vTransId]) (MINIMUM MAXIMUM COUNT).

      IF LAST-OF(ttArray.ArrayValue[vTransFlags]) THEN
      DO TRANSACTION:
        CREATE ttArraySet.
        ASSIGN ttArraySet.SetValue[vTransState] = ttArray.ArrayValue[vTransState]
               ttArraySet.SetValue[vTransFlags] = ttArray.ArrayValue[vTransFlags]
               ttArraySet.SetCount =
                   ACCUM SUB-COUNT BY ttArray.ArrayValue[vTransFlags] "Set":U
        . /* ASSIGN */
      END. /* DO TRANSACTION */
    END. /* FOR EACH ttArray */

    ASSIGN vInterval = INTERVAL(ttSnapshot.SnapshotTime, vPrevTime, "seconds":U)
           vPrevTime = ttSnapshot.SnapshotTime
           vTridRange = (ACCUM MAXIMUM INTEGER(ttArray.ArrayValue[vTransId]))
                      - (ACCUM MINIMUM INTEGER(ttArray.ArrayValue[vTransId]))
           vLastTrid  =  ACCUM MAXIMUM INTEGER(ttArray.ArrayValue[vTransId])
           vTridSpeed = (vLastTrid - vPrevTrid) / vInterval
           vPrevTrid   = vLastTrid
    . /* ASSIGN */

    FOR EACH ttArraySet NO-LOCK
          BY ttArraySet.SetCount DESCENDING:
      PUT UNFORMATTED
               /* DbName      */ ttDatabase.DatabaseName
        {&Sep} /* LogId       */ ttSnapshot.InputLogId
        {&Sep} /* Snapshot    */ ttSnapshot.SnapshotId
        {&Sep} /* Date/Time   */ DateTime2Str(ttSnapshot.SnapshotTime)
        {&Sep} /* Trid Range  */ vTridRange
        {&Sep} /* Trids/Sec   */ vTridSpeed
        {&Sep} /* Trans Count */ ACCUM COUNT INTEGER(ttArray.ArrayValue[vTransId])
        {&Sep} /* Sub-Count   */ ttArraySet.SetCount
        {&Sep} /* Trans State */ ttArraySet.SetValue[vTransState]
        {&Sep} /* Trans Flags */ ttArraySet.SetValue[vTransFlags]
        {&Sep} /* DbHost      */ ttDatabase.DatabaseHost
        {&Sep} /* DbPath      */ ttDatabase.DatabasePath
      SKIP.
    END. /* FOR EACH ttArraySet */
  END. /* FOR EACH ttDatabase, EACH ttSnapshot */

  OUTPUT CLOSE.

END PROCEDURE. /* ReportStatActiveTrans */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportStatBufferLocks:
  
  DEFINE INPUT PARAMETER ipOutputPrefix AS CHARACTER NO-UNDO.

  &SCOPED-DEFINE Sep "~t"
/*
01/23/18        Status: Buffer Locks by user number for all tenants
<Num>  User:Ten                    DBKEY Area                 Hash T S Usect
    1   114                        38336   23               339952 I X     1
*/
  DEFINE VARIABLE vOutputFile  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vMenuId      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vUser        AS INTEGER   NO-UNDO. /* "User:Ten" column */
  DEFINE VARIABLE vDbkey       AS INTEGER   NO-UNDO. /* "DBKEY" column */
  DEFINE VARIABLE vAreaNum     AS INTEGER   NO-UNDO. /* "Area" column */
  DEFINE VARIABLE vBlockType   AS INTEGER   NO-UNDO. /* "T" column */
  DEFINE VARIABLE vBlockStatus AS INTEGER   NO-UNDO. /* "S" column */
  DEFINE VARIABLE vUsect       AS INTEGER   NO-UNDO. /* "Usect" column */
  DEFINE VARIABLE vArrayCount  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vDbkeyCount  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vUserCount   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLockCount   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vIsEmpty     AS LOGICAL   NO-UNDO.

  ASSIGN vMenuId      = MenuId("Status: Buffer Locks":U)
         vOutputFile  = ipOutputPrefix + "Status_Buffer_Locks.stat.txt":U
         vUser        = ColumnId(vMenuId, "User:Ten":U)
         vAreaNum     = ColumnId(vMenuId, "Area":U)
         vDbkey       = ColumnId(vMenuId, "DBKEY":U)
         vBlockType   = ColumnId(vMenuId, "T":U)
         vBlockStatus = ColumnId(vMenuId, "S":U)
         vUsect       = ColumnId(vMenuId, "Usect":U)
  . /* ASSIGN */
  ASSIGN vUser        = ColumnId(vMenuId, "User":U) WHEN vUser EQ ?.
/*
Block Type ("T" column)
D Data block
I Index block
M Master block
O Object block
S Sequence block
F Free block
E Empty block

Block Status (the “S” column label)
S Share Lock
I Share Lock with Intent to modify.
X Exclusive Lock
R I/O state (either being read or written)
B On Online Backup Queue (probkup online)
*/

  IF vMenuId EQ ?
  OR NOT CAN-FIND(FIRST ttArray WHERE ttArray.MenuId EQ vMenuId) THEN
  RETURN.

  OUTPUT TO VALUE(vOutputFile).

  PUT UNFORMATTED
           "DbName"
    {&Sep} "LogId"
    {&Sep} "Snapshot"
    {&Sep} "Date/Time"
    {&Sep} "Count"
    {&Sep} "Users"
    {&Sep} "Dbkeys"
    {&Sep} "Sub-Count"
    {&Sep} "Area"
    {&Sep} "T"
    {&Sep} "S"
    {&Sep} "Usect"
    {&Sep} "DbHost"
    {&Sep} "DbPath"
  SKIP.

  FOR EACH ttDatabase NO-LOCK,

      EACH ttSnapshot NO-LOCK
     WHERE ttSnapshot.DatabaseId EQ ttDatabase.DatabaseId
        BY ttDatabase.DatabaseName
        BY ttSnapshot.SnapshotTime:

/* Count the unique User numbers: */
    ASSIGN vUserCount  = 0.
    FOR EACH ttArray NO-LOCK
       WHERE ttArray.DatabaseId EQ ttSnapshot.DatabaseId
         AND ttArray.InputLogId EQ ttSnapshot.InputLogId
         AND ttArray.SnapshotId EQ ttSnapshot.SnapshotId
         AND ttArray.MenuId     EQ vMenuId
    BREAK BY ttArray.ArrayValue[vUser]:
      IF FIRST-OF(ttArray.ArrayValue[vUser]) THEN
      ASSIGN vUserCount = vUserCount  + 1.
    END. /* FOR EACH ttArray */

/* Count the unique Area+DBKEY pairs: */
    ASSIGN vDbkeyCount = 0.
    FOR EACH ttArray NO-LOCK
       WHERE ttArray.DatabaseId EQ ttSnapshot.DatabaseId
         AND ttArray.InputLogId EQ ttSnapshot.InputLogId
         AND ttArray.SnapshotId EQ ttSnapshot.SnapshotId
         AND ttArray.MenuId     EQ vMenuId
    BREAK BY ttArray.ArrayValue[vAreaNum]
          BY ttArray.ArrayValue[vDbkey]:
      IF FIRST-OF(ttArray.ArrayValue[vDbkey]) THEN
      ASSIGN vDbkeyCount = vDbkeyCount + 1.
    END. /* FOR EACH ttArray */

/* Create and count the sub-sets by Area+T+S+Usect*/
    ASSIGN vArrayCount = 0.
    EMPTY TEMP-TABLE ttArraySet.

    FOR EACH ttArray NO-LOCK
       WHERE ttArray.DatabaseId EQ ttSnapshot.DatabaseId
         AND ttArray.InputLogId EQ ttSnapshot.InputLogId
         AND ttArray.SnapshotId EQ ttSnapshot.SnapshotId
         AND ttArray.MenuId     EQ vMenuId
    BREAK BY ttArray.ArrayValue[vAreaNum]
          BY ttArray.ArrayValue[vBlockType]
          BY ttArray.ArrayValue[vBlockStatus]
          BY ttArray.ArrayValue[vUsect]:
  
      ASSIGN vArrayCount = vArrayCount + 1.
      ACCUMULATE "Set":U (SUB-COUNT BY ttArray.ArrayValue[vUsect]).

      IF LAST-OF(ttArray.ArrayValue[vUsect]) THEN
      DO TRANSACTION:
        CREATE ttArraySet.
        ASSIGN ttArraySet.SetValue[vAreaNum    ] = ttArray.ArrayValue[vAreaNum    ]
               ttArraySet.SetValue[vBlockType  ] = ttArray.ArrayValue[vBlockType  ]
               ttArraySet.SetValue[vBlockStatus] = ttArray.ArrayValue[vBlockStatus]
               ttArraySet.SetValue[vUsect      ] = ttArray.ArrayValue[vUsect      ]
               ttArraySet.SetCount = 
                          ACCUM SUB-COUNT BY ttArray.ArrayValue[vUsect] "Set":U
        . /* ASSIGN */
      END. /* DO TRANSACTION */
    END. /* FOR EACH ttArray */

    FOR EACH ttArraySet NO-LOCK
          BY ttArraySet.SetCount DESCENDING:
      PUT UNFORMATTED
               /* DbName    */ ttDatabase.DatabaseName
        {&Sep} /* LogId     */ ttSnapshot.InputLogId
        {&Sep} /* Snapshot  */ ttSnapshot.SnapshotId
        {&Sep} /* Date/Time */ DateTime2Str(ttSnapshot.SnapshotTime)
        {&Sep} /* Count     */ vArrayCount
        {&Sep} /* Users     */ vUserCount
        {&Sep} /* Dbkeys    */ vDbkeyCount
        {&Sep} /* Sub-Count */ ttArraySet.SetCount
        {&Sep} /* Area      */ ttArraySet.SetValue[vAreaNum    ]
        {&Sep} /* T         */ ttArraySet.SetValue[vBlockType  ]
        {&Sep} /* S         */ ttArraySet.SetValue[vBlockStatus]
        {&Sep} /* Usect     */ ttArraySet.SetValue[vUsect      ]
        {&Sep} /* DbHost    */ ttDatabase.DatabaseHost
        {&Sep} /* DbPath    */ ttDatabase.DatabasePath
      SKIP.
    END. /* FOR EACH ttArraySet */
  END. /* FOR EACH ttDatabase, EACH ttSnapshot */

  OUTPUT CLOSE.

  ASSIGN vOutputFile  = ipOutputPrefix + "Status_Buffer_Locks.MultiLocks.txt":U
  . /* ASSIGN */

  IF vMenuId EQ ?
  OR NOT CAN-FIND(FIRST ttArray WHERE ttArray.MenuId EQ vMenuId) THEN
  RETURN.

  OUTPUT TO VALUE(vOutputFile).

  PUT UNFORMATTED
           "DbName"
    {&Sep} "DbHost"
    {&Sep} "DbPath"
    {&Sep} "LogId"
    {&Sep} "Snapshot"
    {&Sep} "Date/Time"
    {&Sep} "User:Tent"
    {&Sep} "Locks"
    {&Sep} "DBKEY1"
    {&Sep} "Area1"
    {&Sep} "T1"
    {&Sep} "S1"
    {&Sep} "Usect1"
    {&Sep} "DBKEY2"
    {&Sep} "Area2"
    {&Sep} "T2"
    {&Sep} "S2"
    {&Sep} "Usect2"
    {&Sep} "DBKEY3"
    {&Sep} "Area3"
    {&Sep} "T3"
    {&Sep} "S3"
    {&Sep} "Usect3"
    {&Sep} "DBKEY4"
    {&Sep} "Area4"
    {&Sep} "T4"
    {&Sep} "S4"
    {&Sep} "Usect4"
  SKIP.

  ASSIGN vIsEmpty = TRUE.

  FOR EACH ttDatabase NO-LOCK,

      EACH ttSnapshot NO-LOCK
     WHERE ttSnapshot.DatabaseId EQ ttDatabase.DatabaseId
        BY ttDatabase.DatabaseName
        BY ttSnapshot.SnapshotTime:

/* More than one buffer locks per User number: */

    FOR EACH ttArray NO-LOCK
       WHERE ttArray.DatabaseId EQ ttSnapshot.DatabaseId
         AND ttArray.InputLogId EQ ttSnapshot.InputLogId
         AND ttArray.SnapshotId EQ ttSnapshot.SnapshotId
         AND ttArray.MenuId     EQ vMenuId
    BREAK BY ttArray.ArrayValue[vUser]:

/* Only one buffer locks per User number: */
      IF FIRST-OF(ttArray.ArrayValue[vUser])
      AND LAST-OF(ttArray.ArrayValue[vUser]) THEN
      NEXT.

      ACCUMULATE "Locks":U (SUB-COUNT BY ttArray.ArrayValue[vUser]).

      IF FIRST-OF(ttArray.ArrayValue[vUser]) THEN
      EMPTY TEMP-TABLE ttArraySet.
      
      DO TRANSACTION:
        CREATE ttArraySet.
        ASSIGN vUserCount = vUserCount + 1
               ttArraySet.SetValue[vDbkey      ] = ttArray.ArrayValue[vDbkey      ]
               ttArraySet.SetValue[vAreaNum    ] = ttArray.ArrayValue[vAreaNum    ]
               ttArraySet.SetValue[vBlockType  ] = ttArray.ArrayValue[vBlockType  ]
               ttArraySet.SetValue[vBlockStatus] = ttArray.ArrayValue[vBlockStatus]
               ttArraySet.SetValue[vUsect      ] = ttArray.ArrayValue[vUsect      ]
               ttArraySet.SetCount =
                         ACCUM SUB-COUNT BY ttArray.ArrayValue[vUser] "Locks":U
        . /* ASSIGN */
      END. /* DO TRANSACTION */

      IF LAST-OF(ttArray.ArrayValue[vUser]) THEN
      DO:
        PUT UNFORMATTED
                 /* DbName    */ ttDatabase.DatabaseName
          {&Sep} /* DbHost    */ ttDatabase.DatabaseHost
          {&Sep} /* DbPath    */ ttDatabase.DatabasePath
          {&Sep} /* LogId     */ ttSnapshot.InputLogId
          {&Sep} /* Snapshot  */ ttSnapshot.SnapshotId
          {&Sep} /* Date/Time */ DateTime2Str(ttSnapshot.SnapshotTime)
          {&Sep} /* User:Tent */ ttArray.ArrayValue[vUser]
          {&Sep} /* Locks     */ 
                         ACCUM SUB-COUNT BY ttArray.ArrayValue[vUser] "Locks":U
        . /* PUT */

        FOR EACH ttArraySet NO-LOCK
              BY ttArraySet.SetCount:
          PUT UNFORMATTED
            {&Sep} /* DBKEY   */ ttArraySet.SetValue[vDbkey      ]
            {&Sep} /* Area    */ ttArraySet.SetValue[vAreaNum    ]
            {&Sep} /* T       */ ttArraySet.SetValue[vBlockType  ]
            {&Sep} /* S       */ ttArraySet.SetValue[vBlockStatus]
            {&Sep} /* Usect   */ ttArraySet.SetValue[vUsect      ]
          . /* PUT */
        END. /* FOR EACH ttArraySet */

        PUT UNFORMATTED SKIP.
        ASSIGN vIsEmpty = FALSE.

      END. /* IF LAST-OF(ttArray.ArrayValue[vUser]) */
    END. /* FOR EACH ttArray */
  END. /* FOR EACH ttDatabase, EACH ttSnapshot */

  OUTPUT CLOSE.
  IF vIsEmpty THEN
  OS-DELETE VALUE(vOutputFile).

END PROCEDURE. /* ReportStatBufferLocks */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportStatBlockedClients:
  
  DEFINE INPUT PARAMETER ipOutputPrefix AS CHARACTER NO-UNDO.

  &SCOPED-DEFINE Sep "~t"
/*
01/23/18        Status: Blocked Clients by user number for all tenants
  Usr:Ten   Name      Domain     Type      Wait            Wait Info  ...
  115       root          -4      SELF/ABL  REC           34781:1     ...
*/  
  DEFINE VARIABLE vOutputFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vMenuId     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vWait       AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vWaitInfo   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vInfoCount  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vArrayCount AS INTEGER   NO-UNDO.

  ASSIGN vMenuId     = MenuId("Status: Blocked Clients":U)
         vOutputFile = ipOutputPrefix + "Status_Blocked_Clients.stat.txt":U
         vWait       = ColumnId(vMenuId, "Wait":U)
         vWaitInfo   = ColumnId(vMenuId, "Wait Info":U)
  . /* ASSIGN */
/*
Wait:
--       Shared Memory
REC      Record Lock
SCH      Schema Lock
TRAN     Trans Commit
DBSI     DB Buf I Lock
RGET     Record Get
DBRD     DB Buf Read
DBWR     DB Buf Write
DBBK     DB Buf Backup        (not implemented)
BKSH     DB Buf S Lock
BKEX     DB Buf X Lock
NOBF     DB Buf Avail
BKS2     DB Buf S Lock LRU2
BKX2     DB Buf X Lock LRU2
DBW2     DB Buf Write LRU2
BIRD     BI Buf Read
BIWR     BI Buf Write    
AIRD     AI Buf Read
AIWR     AI Buf Write
TXES     TXE Share Lock       (added in V9.1A)
TXEU     TXE Update Lock
TXEC     TXE Commit Lock
TXEX     TXE Excl Lock        (added in V9.1A)
SRPL     Repl TEND Ack        (added in V9.1D) OE Replication Transaction END Acknowledgement
DBSQ     DBSQ Send Lock       (added in V10.1C)
BTDE     TDE Buffer           (added in V10.2B)
STCA     Statement Cache Lock (added in V10.2B)

Wait Info:
BKEX   824032512:7  (11.7)
BKEX   1728901632   (10.2B)
TXEU   2
TXEC   3            (10.2B)
TXEC   3:0          (11.7)
*/

  IF vMenuId EQ ?
  OR NOT CAN-FIND(FIRST ttArray WHERE ttArray.MenuId EQ vMenuId) THEN
  RETURN.

  OUTPUT TO VALUE(vOutputFile).

  PUT UNFORMATTED
           "DbName"
    {&Sep} "LogId"
    {&Sep} "Snapshot"
    {&Sep} "Date/Time"
    {&Sep} "Count"
    {&Sep} "Wait+Info"
    {&Sep} "Sub-Count"
    {&Sep} "Wait"
    {&Sep} "DbHost"
    {&Sep} "DbPath"
  SKIP.

  FOR EACH ttDatabase NO-LOCK,

      EACH ttSnapshot NO-LOCK
     WHERE ttSnapshot.DatabaseId EQ ttDatabase.DatabaseId
        BY ttDatabase.DatabaseName
        BY ttSnapshot.SnapshotTime:

    EMPTY TEMP-TABLE ttArraySet.
    ASSIGN vArrayCount = 0
           vInfoCount  = 0
    . /* ASSIGN */

/* Count Wait+WaitInfo and sub-sets by Wait: */
    FOR EACH ttArray NO-LOCK
       WHERE ttArray.DatabaseId EQ ttSnapshot.DatabaseId
         AND ttArray.InputLogId EQ ttSnapshot.InputLogId
         AND ttArray.SnapshotId EQ ttSnapshot.SnapshotId
         AND ttArray.MenuId     EQ vMenuId
    BREAK BY ttArray.ArrayValue[vWait]
          BY ttArray.ArrayValue[vWaitInfo]:

      ASSIGN vArrayCount = vArrayCount + 1
             vInfoCount  = vInfoCount  + 1
                           WHEN FIRST-OF(ttArray.ArrayValue[vWaitInfo])
      . /* ASSIGN */
      ACCUMULATE "Set":U (SUB-COUNT BY ttArray.ArrayValue[vWait]).

      IF LAST-OF(ttArray.ArrayValue[vWait]) THEN
      DO TRANSACTION:
        CREATE ttArraySet.
        ASSIGN ttArraySet.SetValue[vWait] = ttArray.ArrayValue[vWait]
               ttArraySet.SetCount =
                          ACCUM SUB-COUNT BY ttArray.ArrayValue[vWait] "Set":U
        . /* ASSIGN */
      END. /* DO TRANSACTION */
    END. /* FOR EACH ttArray */

    FOR EACH ttArraySet NO-LOCK
          BY ttArraySet.SetCount DESCENDING:
      PUT UNFORMATTED
               /* DbName    */ ttDatabase.DatabaseName
        {&Sep} /* LogId     */ ttSnapshot.InputLogId
        {&Sep} /* Snapshot  */ ttSnapshot.SnapshotId
        {&Sep} /* Date/Time */ DateTime2Str(ttSnapshot.SnapshotTime)
        {&Sep} /* Count     */ vArrayCount
        {&Sep} /* Wait+Info */ vInfoCount
        {&Sep} /* Sub-Count */ ttArraySet.SetCount
        {&Sep} /* Wait      */ ttArraySet.SetValue[vWait]
        {&Sep} /* DbHost    */ ttDatabase.DatabaseHost
        {&Sep} /* DbPath    */ ttDatabase.DatabasePath
      SKIP.
    END. /* FOR EACH ttArraySet */
  END. /* FOR EACH ttDatabase, EACH ttSnapshot */

  OUTPUT CLOSE.

END PROCEDURE. /* ReportStatBlockedClients */

/* ------------------------------------------------------------------------- */

PROCEDURE ReportStatBufferLockQueue:
  
  DEFINE INPUT PARAMETER ipOutputPrefix AS CHARACTER NO-UNDO.

  &SCOPED-DEFINE Sep "~t"
/*
01/23/18        Status: Buffer Lock Queue by user number for all tenants
  Usr:Ten                  DBKEY Area T         Status           Type     Usect
*/  
  DEFINE VARIABLE vOutputFile AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vMenuId     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vDbkey      AS INTEGER   NO-UNDO. /* "DBKEY" column */
  DEFINE VARIABLE vAreaNum    AS INTEGER   NO-UNDO. /* "Area" column */
  DEFINE VARIABLE vBlockType  AS INTEGER   NO-UNDO. /* "T" column */
  DEFINE VARIABLE vStatus     AS INTEGER   NO-UNDO. /* "Status" column */
  DEFINE VARIABLE vLockType   AS INTEGER   NO-UNDO. /* "Type" column */
  DEFINE VARIABLE vUsect      AS INTEGER   NO-UNDO. /* "Usect" column */
  DEFINE VARIABLE vDbkeyCount AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vArrayCount AS INTEGER   NO-UNDO.

  ASSIGN vMenuId     = MenuId("Status: Buffer Lock Queue":U)
         vOutputFile = ipOutputPrefix + "Status_Buffer_Lock_Queue.stat.txt":U
         vDbkey      = ColumnId(vMenuId, "DBKEY":U)
         vAreaNum    = ColumnId(vMenuId, "Area":U)
         vBlockType  = ColumnId(vMenuId, "T":U)
         vStatus     = ColumnId(vMenuId, "Status":U)
         vLockType   = ColumnId(vMenuId, "Type":U)
         vUsect      = ColumnId(vMenuId, "Usect":U)
  . /* ASSIGN */
/*
Status:
LOCKED
WAITING

Lock/Wait Type:
FREE          ???
SHARE         Share locked
INTENT        Share locked with Intent to modify
EXCL          Exclusively locked
TIO           I/O state (either being read or written)
BACKQ         On Online Backup Queue (not implemented)  
INTENTWAIT    DB Buf I Lock                                                                         
RGWAIT        Record Get                                                                            
READWAIT      DB Buf Read                                                                           
WRITEWAIT     DB Buf Write                                                                          
BACKWAIT      DB Buf Backup        (not implemented)                                                
SHAREWAIT     DB Buf S Lock                                                                         
EXCLWAIT      DB Buf X Lock                                                                         
NOBUFWAIT     DB Buf Avail                                                                          
SHAREWAIT2    DB Buf S Lock LRU2                                                                    
EXCLWAIT2     DB Buf X Lock LRU2                                                                    
WRITEWAIT2    DB Buf Write LRU2                                                                     
BIREADWAIT    BI Buf Read                                                                           
BIWRITEWAIT   BI Buf Write                                                                          
AIREADWAIT    AI Buf Read                                                                           
AIWRITEWAIT   AI Buf Write                                                                          
TXSWAIT       TXE Share Lock       (added in V9.1A)                                                 
TXBWAIT       TXE Update Lock                                                                       
TXEWAIT       TXE Commit Lock                                                                       
TXXWAIT       TXE Excl Lock        (added in V9.1A)                                                 
REPLWAIT      Repl TEND Ack        (added in V9.1D) OE Replication Transaction END Acknowledgement
DBSQWAIT      DBSQ Send Lock       (added in V10.1C)                                                
ENSPBUFWAIT   TDE Buffer           (added in V10.2B)                                                
STCACHEWAIT   Statement Cache Lock (added in V10.2B)                                                
*/
  IF vMenuId EQ ?
  OR NOT CAN-FIND(FIRST ttArray WHERE ttArray.MenuId EQ vMenuId) THEN
  RETURN.

  OUTPUT TO VALUE(vOutputFile).

  PUT UNFORMATTED
           "DbName"
    {&Sep} "LogId"
    {&Sep} "Snapshot"
    {&Sep} "Date/Time"
    {&Sep} "Count"
    {&Sep} "Dbkeys"
    {&Sep} "Sub-Count"
    {&Sep} "Area"
    {&Sep} "T"
    {&Sep} "Type"
    {&Sep} "Usect"
    {&Sep} "DbHost"
    {&Sep} "DbPath"
  SKIP.

  FOR EACH ttDatabase NO-LOCK,

      EACH ttSnapshot NO-LOCK
     WHERE ttSnapshot.DatabaseId EQ ttDatabase.DatabaseId
        BY ttDatabase.DatabaseName
        BY ttSnapshot.SnapshotTime:

    ASSIGN vArrayCount = 0
           vDbkeyCount = 0
    . /* ASSIGN */

    FOR EACH ttArray NO-LOCK
       WHERE ttArray.DatabaseId EQ ttSnapshot.DatabaseId
         AND ttArray.InputLogId EQ ttSnapshot.InputLogId
         AND ttArray.SnapshotId EQ ttSnapshot.SnapshotId
         AND ttArray.MenuId     EQ vMenuId
    BREAK BY ttArray.ArrayValue[vAreaNum]
          BY ttArray.ArrayValue[vDbkey]:
      
      ASSIGN vArrayCount = vArrayCount + 1
             vDbkeyCount = vDbkeyCount + 1
                           WHEN FIRST-OF(ttArray.ArrayValue[vDbkey])
      . /* ASSIGN */
    END. /* FOR EACH ttArray */

/* Usect is empty when Status WAITING or SHAREWAIT: */
    EMPTY TEMP-TABLE ttArraySet.

    FOR EACH ttArray NO-LOCK
       WHERE ttArray.DatabaseId EQ ttSnapshot.DatabaseId
         AND ttArray.InputLogId EQ ttSnapshot.InputLogId
         AND ttArray.SnapshotId EQ ttSnapshot.SnapshotId
         AND ttArray.MenuId     EQ vMenuId
         AND ttArray.ArraySize  LT vUsect
    BREAK BY ttArray.ArrayValue[vAreaNum]
          BY ttArray.ArrayValue[vDbkey]
          BY ttArray.ArrayValue[vBlockType]
          BY ttArray.ArrayValue[vLockType]:

      ACCUMULATE "Set":U (SUB-COUNT BY ttArray.ArrayValue[vLockType]).

      IF LAST-OF(ttArray.ArrayValue[vLockType]) THEN
      DO TRANSACTION:
        CREATE ttArraySet.
        ASSIGN ttArraySet.SetValue[vAreaNum  ] = ttArray.ArrayValue[vAreaNum]
               ttArraySet.SetValue[vBlockType] = ttArray.ArrayValue[vBlockType]
               ttArraySet.SetValue[vLockType ] = ttArray.ArrayValue[vLockType]
               ttArraySet.SetValue[vUsect    ] = "dbkey ":U
                                                  + ttArray.ArrayValue[vDbkey]
               ttArraySet.SetCount = 
                      ACCUM SUB-COUNT BY ttArray.ArrayValue[vLockType] "Set":U
        . /* ASSIGN */
      END. /* DO TRANSACTION */
    END. /* FOR EACH ttArray */

/* Status other than WAITING: */

    FOR EACH ttArray NO-LOCK
       WHERE ttArray.DatabaseId EQ ttSnapshot.DatabaseId
         AND ttArray.InputLogId EQ ttSnapshot.InputLogId
         AND ttArray.SnapshotId EQ ttSnapshot.SnapshotId
         AND ttArray.MenuId     EQ vMenuId
         AND ttArray.ArraySize  GE vUsect /*'Usect' for LOCKED type */
    BREAK BY ttArray.ArrayValue[vAreaNum]
          BY ttArray.ArrayValue[vBlockType]
          BY ttArray.ArrayValue[vLockType]
          BY ttArray.ArrayValue[vUsect]:

      ACCUMULATE "Set":U (SUB-COUNT BY ttArray.ArrayValue[vUsect]).

      IF LAST-OF(ttArray.ArrayValue[vUsect]) THEN
      DO TRANSACTION:
        CREATE ttArraySet.
        ASSIGN ttArraySet.SetValue[vAreaNum  ] = ttArray.ArrayValue[vAreaNum]
               ttArraySet.SetValue[vBlockType] = ttArray.ArrayValue[vBlockType]
               ttArraySet.SetValue[vLockType ] = ttArray.ArrayValue[vLockType]
               ttArraySet.SetValue[vUsect    ] = ttArray.ArrayValue[vUsect]
               ttArraySet.SetCount =
                          ACCUM SUB-COUNT BY ttArray.ArrayValue[vUsect] "Set":U
        . /* ASSIGN */
      END. /* DO TRANSACTION */
    END. /* FOR EACH ttArray */

    FOR EACH ttArraySet NO-LOCK
          BY ttArraySet.SetCount DESCENDING:
      PUT UNFORMATTED
               /* DbName    */ ttDatabase.DatabaseName
        {&Sep} /* LogId     */ ttSnapshot.InputLogId
        {&Sep} /* Snapshot  */ ttSnapshot.SnapshotId
        {&Sep} /* Date/Time */ DateTime2Str(ttSnapshot.SnapshotTime)
        {&Sep} /* Count     */ vArrayCount
        {&Sep} /* Dbkeys    */ vDbkeyCount
        {&Sep} /* Sub-Count */ ttArraySet.SetCount
        {&Sep} /* Area      */ ttArraySet.SetValue[vAreaNum  ]
        {&Sep} /* T         */ ttArraySet.SetValue[vBlockType]
        {&Sep} /* Type      */ ttArraySet.SetValue[vLockType ]
        {&Sep} /* Usect     */ ttArraySet.SetValue[vUsect    ]
        {&Sep} /* DbHost    */ ttDatabase.DatabaseHost
        {&Sep} /* DbPath    */ ttDatabase.DatabasePath
      SKIP.
    END. /* FOR EACH ttArraySet */
  END. /* FOR EACH ttDatabase, EACH ttSnapshot */

  OUTPUT CLOSE.

END PROCEDURE. /* ReportStatBufferLockQueue */

/* ------------------------------------------------------------------------- */

/*
PAUSE BEFORE-HIDE.
FOR EACH ttStatus  NO-LOCK,

  FIRST ttProperty NO-LOCK
  WHERE ttProperty.MenuId EQ ttStatus.MenuId,

  FIRST ttMenu NO-LOCK
  WHERE ttMenu.MenuId EQ ttStatus.MenuId,

  FIRST ttSect NO-LOCK
  WHERE ttSect.SectId EQ ttStatus.SectId:
DISPLAY 
  ttMenu.MenuId ttMenu.MenuName FORMAT "x(40)" SKIP
  ttSect.SectId ttSect.SectName FORMAT "x(40)" SKIP
  ttProperty.PropertyName LABEL "Name"  FORMAT "x(64)" SKIP
  ttStatus.PropertyValue  LABEL "Value" FORMAT "x(64)" 
WITH SIDE-LABELS TITLE "debug" .
END.
*/

/* ------------------------------------------------------------------------- */

PROCEDURE DumpAll:

  &SCOPED-DEFINE Sep "~t"
  
  DEFINE VARIABLE i AS INTEGER     NO-UNDO.

  IF CAN-FIND(FIRST ttOSFile) THEN
  DO:
    OUTPUT TO VALUE("debug.ttOSFile.txt").
    PUT UNFORMATTED 
             "LogId"
      {&Sep} "osFileName"
      {&Sep} "osFilePath"
      {&Sep} "osFileAttr"
    SKIP.
    ASSIGN i = 0.
    FOR EACH ttOSFile:
      ASSIGN i = i + 1.
      PUT UNFORMATTED i
        {&Sep} ttOSFile.osFileName
        {&Sep} ttOSFile.osFilePath
        {&Sep} ttOSFile.osFileAttr
      SKIP.
    END.
    OUTPUT CLOSE.
  END. /* ttOSFile */

  IF CAN-FIND(FIRST ttMenu) THEN
  DO:
    OUTPUT TO VALUE("debug.ttMenu.txt").
    PUT UNFORMATTED 
             "MenuId   "
      {&Sep} "MenuName "
      {&Sep} "MenuTime "
      {&Sep} "ArraySize"
      {&Sep} "ArrayHead"
    SKIP.
    FOR EACH ttMenu:
      PUT UNFORMATTED 
               ttMenu.MenuId   
        {&Sep} ttMenu.MenuName 
        {&Sep} ttMenu.MenuTime 
        {&Sep} ttMenu.ArraySize
        {&Sep} ttMenu.ArrayHead
      SKIP.
    END.
    OUTPUT CLOSE.
  END. /* ttMenu */

  IF CAN-FIND(FIRST ttSect) THEN
  DO:
    OUTPUT TO VALUE("debug.ttSect.txt").
    PUT UNFORMATTED 
             "MenuId  "
      {&Sep} "SectId  "
      {&Sep} "SectName"
    SKIP.
    FOR EACH ttSect:
      PUT UNFORMATTED 
               ttSect.MenuId  
        {&Sep} ttSect.SectId  
        {&Sep} ttSect.SectName
      SKIP.
    END.
    OUTPUT CLOSE.
  END. /* ttSect */

  IF CAN-FIND(FIRST ttDatabase) THEN
  DO:
    OUTPUT TO VALUE("debug.ttDatabase.txt").
    PUT UNFORMATTED 
             "DatabaseId  "
      {&Sep} "DatabaseHost"
      {&Sep} "DatabaseName"
      {&Sep} "DatabasePath"
    SKIP.
    FOR EACH ttDatabase:
      PUT UNFORMATTED 
               ttDatabase.DatabaseId  
        {&Sep} ttDatabase.DatabaseHost
        {&Sep} ttDatabase.DatabaseName
        {&Sep} ttDatabase.DatabasePath
      SKIP.
    END.
    OUTPUT CLOSE.
  END. /* ttDatabase */

  IF CAN-FIND(FIRST ttSnapshot) THEN
  DO:
    OUTPUT TO VALUE("debug.ttSnapshot.txt").
    PUT UNFORMATTED 
             "DatabaseId  "
      {&Sep} "InputLogId  "
      {&Sep} "SnapshotId  "
      {&Sep} "SnapshotTime"
      {&Sep} "StatInterval"
    SKIP.
    FOR EACH ttSnapshot:
      PUT UNFORMATTED 
               ttSnapshot.DatabaseId  
        {&Sep} ttSnapshot.InputLogId  
        {&Sep} ttSnapshot.SnapshotId  
        {&Sep} ttSnapshot.SnapshotTime
        {&Sep} ttSnapshot.StatInterval
      SKIP.
    END.
    OUTPUT CLOSE.
  END. /* ttSnapshot */

  IF CAN-FIND(FIRST ttProperty) THEN
  DO:
    OUTPUT TO VALUE("debug.ttProperty.txt").
    PUT UNFORMATTED 
             "MenuId      "
      {&Sep} "PropertyId  "
      {&Sep} "PropertyName"
      {&Sep} "PropertyRow "
      {&Sep} "PropertyType"
    SKIP.
    FOR EACH ttProperty:
      PUT UNFORMATTED 
               ttProperty.MenuId      
        {&Sep} ttProperty.PropertyId  
        {&Sep} ttProperty.PropertyName
        {&Sep} ttProperty.PropertyRow 
        {&Sep} ttProperty.PropertyType
      SKIP.
    END.
    OUTPUT CLOSE.
  END. /* ttProperty */

  IF CAN-FIND(FIRST ttStatic) THEN
  DO:
    OUTPUT TO VALUE("debug.ttStatic.txt").
    PUT UNFORMATTED 
             "DatabaseId   "
      {&Sep} "MenuId       "
      {&Sep} "SectId       "
      {&Sep} "PropertyId   "
      {&Sep} "PropertyValue"
      {&Sep} "SnapshotCount"
    SKIP.
    FOR EACH ttStatic:
      PUT UNFORMATTED 
               ttStatic.DatabaseId   
        {&Sep} ttStatic.MenuId       
        {&Sep} ttStatic.SectId       
        {&Sep} ttStatic.PropertyId   
        {&Sep} ttStatic.PropertyValue
        {&Sep} ttStatic.SnapshotCount
      SKIP.
    END.
    OUTPUT CLOSE.
  END. /* ttStatic */

  IF CAN-FIND(FIRST ttStatus) THEN
  DO:
    OUTPUT TO VALUE("debug.ttStatus.txt").
    PUT UNFORMATTED 
             "DatabaseId   "
      {&Sep} "InputLogId   "
      {&Sep} "SnapshotId   "
      {&Sep} "MenuId       "
      {&Sep} "SectId       "
      {&Sep} "PropertyId   "
      {&Sep} "PropertyValue"
      {&Sep} "SnapshotTime "
    SKIP.
    FOR EACH ttStatus:
      PUT UNFORMATTED 
               ttStatus.DatabaseId   
        {&Sep} ttStatus.InputLogId   
        {&Sep} ttStatus.SnapshotId   
        {&Sep} ttStatus.MenuId       
        {&Sep} ttStatus.SectId       
        {&Sep} ttStatus.PropertyId   
        {&Sep} ttStatus.PropertyValue
        {&Sep} ttStatus.SnapshotTime 
      SKIP.
    END.
    OUTPUT CLOSE.
  END. /* ttStatus */

  IF CAN-FIND(FIRST ttActivity) THEN
  DO:
    OUTPUT TO VALUE("debug.ttActivity.txt").
    PUT UNFORMATTED 
             "DatabaseId   "
      {&Sep} "InputLogId   "
      {&Sep} "SnapshotId   "
      {&Sep} "MenuId       "
      {&Sep} "SectId       "
      {&Sep} "PropertyId   "
      {&Sep} "PropertyValue"
    SKIP.
    FOR EACH ttActivity:
      PUT UNFORMATTED 
               ttActivity.DatabaseId   
        {&Sep} ttActivity.InputLogId   
        {&Sep} ttActivity.SnapshotId   
        {&Sep} ttActivity.MenuId       
        {&Sep} ttActivity.SectId       
        {&Sep} ttActivity.PropertyId   
        {&Sep} ttActivity.PropertyValue[1]
      SKIP.
    END.
    OUTPUT CLOSE.
  END. /* ttActivity */

  IF CAN-FIND(FIRST ttResource) THEN
  DO:
    OUTPUT TO VALUE("debug.ttResource.txt").
    PUT UNFORMATTED 
             "DatabaseId"
      {&Sep} "InputLogId"
      {&Sep} "SnapshotId"
      {&Sep} "MenuId    "
      {&Sep} "PropertyId"
      {&Sep} "Requests  "
      {&Sep} "Waits     "
    SKIP.
    FOR EACH ttResource:
      PUT UNFORMATTED 
               ttResource.DatabaseId
        {&Sep} ttResource.InputLogId
        {&Sep} ttResource.SnapshotId
        {&Sep} ttResource.MenuId    
        {&Sep} ttResource.PropertyId
        {&Sep} ttResource.Requests[1]
        {&Sep} ttResource.Waits[1]
      SKIP.
    END.
    OUTPUT CLOSE.
  END. /* ttResource */

  IF CAN-FIND(FIRST ttLatch) THEN
  DO:
    OUTPUT TO VALUE("debug.ttLatch.txt").
    PUT UNFORMATTED 
             "DatabaseId"
      {&Sep} "InputLogId"
      {&Sep} "SnapshotId"
      {&Sep} "MenuId    "
      {&Sep} "PropertyId"
      {&Sep} "LatchOwner"
      {&Sep} "LatchLock "
      {&Sep} "LatchWait "
    SKIP.
    FOR EACH ttLatch:
      PUT UNFORMATTED 
               ttLatch.DatabaseId
        {&Sep} ttLatch.InputLogId
        {&Sep} ttLatch.SnapshotId
        {&Sep} ttLatch.MenuId    
        {&Sep} ttLatch.PropertyId
        {&Sep} ttLatch.LatchOwner
        {&Sep} ttLatch.LatchLock[1]
        {&Sep} ttLatch.LatchWait 
      SKIP.
    END.
    OUTPUT CLOSE.
  END. /* ttLatch */

  IF CAN-FIND(FIRST ttArray) THEN
  DO:
    OUTPUT TO VALUE("debug.ttArray.txt").
    PUT UNFORMATTED 
             "DatabaseId  "
      {&Sep} "InputLogId  "
      {&Sep} "SnapshotId  "
      {&Sep} "MenuId      "
      {&Sep} "ArrayRow    "
      {&Sep} "ArraySize   "
      {&Sep} "SnapshotTime"
    . /* PUT */
    DO i = 1 TO {&ArraySize}:
      PUT UNFORMATTED {&Sep} "Value" i.
    END.
    PUT UNFORMATTED SKIP.
    

    FOR EACH ttArray:
      PUT UNFORMATTED 
               ttArray.DatabaseId  
        {&Sep} ttArray.InputLogId  
        {&Sep} ttArray.SnapshotId  
        {&Sep} ttArray.MenuId      
        {&Sep} ttArray.ArrayRow    
        {&Sep} ttArray.ArraySize   
        {&Sep} ttArray.SnapshotTime
      . /* PUT */
      DO i = 1 TO {&ArraySize}:
        PUT UNFORMATTED {&Sep} ttArray.ArrayValue[i].
      END.
      PUT UNFORMATTED SKIP.
    END.
    OUTPUT CLOSE.
  END. /* ttArray */

  IF CAN-FIND(FIRST ttColumn) THEN
  DO:
    OUTPUT TO VALUE("debug.ttColumn.txt").
    PUT UNFORMATTED 
             "MenuId    "
      {&Sep} "ColumnId  "
      {&Sep} "ColumnName"
    SKIP.
    FOR EACH ttColumn:
      PUT UNFORMATTED 
               ttColumn.MenuId    
        {&Sep} ttColumn.ColumnId  
        {&Sep} ttColumn.ColumnName
      SKIP.
    END.
    OUTPUT CLOSE.
  END. /* ttColumn */

END PROCEDURE. /* DumpAll */


