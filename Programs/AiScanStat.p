RUN LoadAreaInfo("path/to/your_db.st").

RUN ReadAiDir(
  "/path/to/dir",  /* Input directory in which AI scan files are stored.     */
  ?,               /* MaxDirLevel: 1 - current dir only, ? - infinitely deep */
  "ai_scans~~~.*", /* Mask for input scan file names.                        */
  ?,               /* Output directory. Default value is <Input dir>.        */
  ?                /* Prefix for output files. Program will creates 8 files  */
).                 /* Default prefix is "<InputDir>.AiScanStat"              */

/* ------------------------------------------------------------------------
    Program     : AiScanStat.p
    Purpose     : Various statistics based on AI scans.
                  AI scans should be from Progress V10.1A or higher.

    Syntax      : See the examples above.

    Description : The output file can be opened in Excel.

    Author(s)   : George Potemkin
    Created     : January 03, 2014
    Modified    : March 16, 2014 12:00
    Version     : 3.1.1

Estimated execution time:
 1.6 MB/sec of AI scans (1GB ~ 700 sec) or 6 times slower than 4GL "grep".
 2/3 of time is the "Load" phase and 1/3 is the "Report" phase.
Required resources:
 DBI size is 1/3 of the largest AI scan (~350 MB for 1GB of AI scan).
 The -tmpbsize 8 is recommended. Large -Bt or the -T on SSD will speed up.
 Note that the *.Dbkey.txt and *.Trans.txt output files can be huge.
 To reduce their size use the vDbkeyUpdLimit, vTranTimeLimit, vTranNoteLimit:

Fixes:
March 16, 2014: The "OldAtEnd" values in Files report were incorrect.

------------------------------------------------------------------------ */

/* Parameters to minimize the size of the reports: */
DEFINE VARIABLE vDbkeyUpdLimit AS INTEGER NO-UNDO INITIAL  10. /* Updates */
DEFINE VARIABLE vTranTimeLimit AS INTEGER NO-UNDO INITIAL   2. /* Sec     */
DEFINE VARIABLE vTranNoteLimit AS INTEGER NO-UNDO INITIAL 100. /* Notes   */
/* Parameters for data presentation: */
DEFINE VARIABLE vNoteListThold AS DECIMAL NO-UNDO INITIAL 0.3.
DEFINE VARIABLE vNoteListDigit AS INTEGER NO-UNDO INITIAL   2.
DEFINE VARIABLE vRoundDigits   AS INTEGER NO-UNDO INITIAL   2.
/*
vDbkeyUpdLimit - the limit of update note counts to report dbkey statistics.
---
vTranTimeLimit, vTranNoteLimit - the limits for transaction duration and
note count per transaction to report transaction statistics.
---
vNoteListThold, vNoteListDigit - to find the similarity in transactions.
The list of the notes per transaction will be sorted by note counts descending:
Note1:Area1*Count1,...,NoteN:AreaN*CountN
where Count1 >= Count2 >= ... CountN

With vNoteListThold & vNoteListDigit the list will be represented as:
(Note1:Area1*Fact1,...)*SubTrans,NoteM:AreaM*CountM,...,NoteN:AreaN*CountN
where
Fact1=Count1/SubTrans is a decimal with vNoteListDigit decimals.
NoteThold=SubTrans*vNoteListThold is threshold for note counts outside brackets.
NoteThold >= CountM >= ... CountN

SubTrans is the number of sub-transactions.
Algorithm to estimate SubTrans can be changed later.
---
vRoundDigits - the number of digits for decimal values in the reports.
*/

&SCOPED-DEFINE TotalStat 0
&SCOPED-DEFINE Sep "~t"

DEFINE VARIABLE vFrameTitle AS CHARACTER   NO-UNDO.

FORM vCnt  AS INTEGER   FORMAT ">>9"   LABEL "#"
     vFile AS CHARACTER FORMAT "x(24)" LABEL "File"
     vSize AS CHARACTER FORMAT "x(10)" LABEL "Size"
     vLoad AS CHARACTER FORMAT "x(8)"  LABEL "Load"
     vTime AS CHARACTER FORMAT "x(8)"  LABEL "Report"
WITH FRAME StatusFrame TITLE vFrameTitle 16 DOWN.

DEFINE STREAM FilesReport.
DEFINE STREAM AreasReport.
DEFINE STREAM NotesReport.
DEFINE STREAM DbkeyReport.
DEFINE STREAM CkptsReport.
DEFINE STREAM TransReport.
DEFINE STREAM UsersReport.
DEFINE STREAM PerSecReport.

DEFINE TEMP-TABLE ttAiFile
  FIELD AiSeqNum     AS INTEGER            /* AI sequence number (Seqno)    */
  FIELD AiFileName   AS CHARACTER          /* Name of AI scan file          */
  FIELD AiFileSize   AS DECIMAL  INITIAL ? /* Size of AI scan file          */
  FIELD LineCount    AS INTEGER  INITIAL ? /* Number of lines in AI scan    */
  FIELD AiBegTime    AS DATETIME INITIAL ? /* Time of aimage begin          */
  FIELD AiOpenTime   AS DATETIME INITIAL ? /* Time of db restart            */
  FIELD AiNewTime    AS DATETIME INITIAL ? /* Time of aimage new            */
  FIELD LastTime     AS DATETIME INITIAL ? /* Time of last transation note  */
  FIELD AiInterval   AS INTEGER  INITIAL ? /* between AiNewTime & LastTime  */
  FIELD NoteCount    AS INTEGER  INITIAL ? /* Total note count in AI scan   */
  FIELD UpdtCount    AS INTEGER  INITIAL ? /* Update note count in AI scan  */
  FIELD TranCount    AS INTEGER  INITIAL ? /* Count of RL_TBGN notes        */
  FIELD MinTrid      AS INTEGER  INITIAL ? /* Min Trid found in AI scan     */
  FIELD MaxTrid      AS INTEGER  INITIAL ? /* Max Trid found in AI scan     */
  FIELD TranAtBgn    AS INTEGER  INITIAL ? /* Trans at beginning (3785)     */
  FIELD TranAtEnd    AS INTEGER  INITIAL ? /* Trans at the end (1636)       */
  FIELD OldAtEnd     AS INTEGER  INITIAL ? /* Active from beginning to end  */
  FIELD AveActTran   AS DECIMAL  INITIAL 0 /* Ave num of simultaneous trans */
  FIELD MaxActTran   AS INTEGER  INITIAL 0 /* Max num of simultaneous trans */
  FIELD MaxActTime   AS DATETIME INITIAL ? /* Time when num of tran was max */
  FIELD TotTimeGap   AS INTEGER  INITIAL 0
  FIELD MaxTimeGap   AS INTEGER  INITIAL 0
  FIELD AveTimeGap   AS DECIMAL  INITIAL 0
  FIELD LastCkpTime  AS DATETIME INITIAL ? /* Stat to add to next checkpoint */
  FIELD CkpNoteCount AS INTEGER  INITIAL ?
  FIELD CkpUpdtCount AS INTEGER  INITIAL ?
  FIELD CkpTranCount AS INTEGER  INITIAL ?
  INDEX AiSeqNum IS UNIQUE
        AiSeqNum
. /* DEFINE TEMP-TABLE ttAiFile */

/* Trid: Trid code = NoteCode version = 2 (12528) */
DEFINE TEMP-TABLE ttNoteType NO-UNDO
  FIELD NoteCode AS INTEGER   /* Integer ID */
  FIELD NoteType AS CHARACTER /* Note type (RL_*) */
  INDEX NoteCode IS UNIQUE
        NoteCode
  INDEX NoteType IS UNIQUE
        NoteType
. /* DEFINE TEMP-TABLE ttNoteType */

/*
Trid: TRID dbkey = 0  update counter = 0 (12530)
Trid: TRID area = AREA   dbkey = DBKEY   update counter = UPDCTR (12529)
*/
DEFINE TEMP-TABLE ttNote NO-UNDO
  FIELD NoteArea    AS INTEGER
  FIELD NoteDbkey   AS INT64
  FIELD NoteCode    AS INTEGER
  FIELD NoteTrid    AS INTEGER
  FIELD NoteCount   AS INTEGER
  FIELD FirstUpdCtr AS INTEGER  /* Update counter of the first note */
  FIELD LastUpdCtr  AS INTEGER  /* Update counter of the last note  */
  FIELD FirstTime   AS DATETIME /* Time of the first note           */
  FIELD LastTime    AS DATETIME /* Time of the last note            */
  INDEX PrimaryKey IS UNIQUE
        NoteArea
        NoteDbkey
        NoteTrid
        NoteCode
  INDEX NoteTrid
        NoteTrid
. /* DEFINE TEMP-TABLE ttNote */
/*
ttNote records accumulate the count of notes by Area+Dbkey+Code+Trid.
ttNote is a main contributor to the size of DBI file.
ImportAiScan() can create the millions of records in ttNote.
MainReports() will empty ttNote.
IOW, the max size of ttNote is defined by the largest AI scan.

PrimaryKey index will be used to create statistics accumulated by dbkey.
NoteTrid index will be used in transaction statistics report.
*/

/* Auxiliary table to accumulate and to sort the note counts */
DEFINE TEMP-TABLE ttNoteList NO-UNDO
  FIELD ListId    AS INTEGER /* a.k.a. list type: per Tran, per Area, per Db */
  FIELD NoteArea  AS INTEGER
  FIELD NoteType  AS CHARACTER
  FIELD NoteCount AS INTEGER
  INDEX PrimaryKey IS UNIQUE
        ListId
        NoteArea
        NoteType
        NoteCount
. /* DEFINE TEMP-TABLE ttNoteList */
/*
The records created for ListId will be deleted immediately after sorting
except for the uncommitted transactions.
IOW, the number of records in ttNoteList will be small.
ListId, NoteArea have sense only for the note list per transaction
but nevertheless they're used with other types of the note lists.
*/

DEFINE TEMP-TABLE ttArea NO-UNDO
  FIELD AreaNumber   AS INTEGER
  FIELD AreaInfo     AS CHARACTER FORMAT "x(32)" /* LoadAreaInfo(dbname.st)*/
  FIELD UsedSeqCount AS INTEGER INITIAL 0  /* "Used" sequence count per area*/ 
  FIELD AveSeqPath   AS DECIMAL INITIAL 0.0
  FIELD MaxSeqPath   AS INTEGER INITIAL 0
  INDEX AreaNumber IS UNIQUE
        AreaNumber
. /* DEFINE TEMP-TABLE ttArea */
/*
UsedSeqCount, AveSeqPath, MaxSeqPath fields are accumulated by area.
The terms see below in comments for ttTran.
*/

DEFINE TEMP-TABLE ttTran NO-UNDO
  FIELD Trid          AS INTEGER             /* Transaction ID               */
  FIELD UserName      AS CHARACTER INITIAL ? /* Userid that started the tran */
  FIELD BgnTime       AS DATETIME  INITIAL ? /* Transaction begin time       */
  FIELD EndTime       AS DATETIME  INITIAL ? /* Transaction end time         */
  FIELD AiSeqNum      AS INTEGER   INITIAL ? /* AI file with first tran notes*/
  FIELD LastTime      AS DATETIME  INITIAL ? /* Time of the most recent note */
  FIELD IdleTime      AS INTEGER   INITIAL 0 /* Idle time inside transaction */
  FIELD MaxGTime      AS INTEGER   INITIAL 0 /* Max gap time (idle interval) */
  FIELD GapCount      AS INTEGER   INITIAL 0 /* Gap count of gaps */
  FIELD NoteCount     AS INTEGER   INITIAL 0 /* Note count per transaction   */
  FIELD UpdtCount     AS INTEGER   INITIAL 0 /* Update note count            */
  FIELD ConcCount     AS INTEGER   INITIAL 0 /* Concurrent update count      */
  FIELD PrevNoteCount AS INTEGER   INITIAL 0 /* Count before the current AI  */
  FIELD PrevIdleTime  AS INTEGER   INITIAL 0 /* Idle before the current AI   */
  FIELD UsedSeqCount  AS INTEGER   INITIAL 0 /* "Used" sequence count        */
  FIELD LastSeqIndex  AS INTEGER   INITIAL ? /* Last ttTran.UpdtCount for seq*/
  FIELD AveSeqPath    AS DECIMAL   INITIAL 0.0
  FIELD MaxSeqPath    AS INTEGER   INITIAL 0
  INDEX Trid IS UNIQUE
        Trid
. /* DEFINE TEMP-TABLE ttTran */
/*
UsedSeqCount, AveSeqPath, MaxSeqPath fields:
The sequence increments are often used to create an unique ID in the records.
The terms below are used to identify the dependency between these db changes:

The "linked" sequence increments:
RL_SEINC notes followed by RL_RMCR notes inside the same transaction.

The "sequence path":
The number of the "update" notes in between RL_SEINC and RL_RMCR notes.
It could be, for example, the removing of a recid lock (place holder).
The "sequence path" estimates the reliability of the "link".

The "used" sequence increments:
If all RL_RMCR notes for the same dbkey are "linked" to RL_SEINC notes
then all these RL_SEINC notes are "used".
Note that the "used" condition of is too strong - some RL_RMCR notes can
represent the record's fragments rather than the records themselves.
*/

DEFINE TEMP-TABLE ttUser NO-UNDO
  FIELD AiSeqNum     AS INTEGER               /* Seqno of current AI file   */
  FIELD UserName     AS CHARACTER INITIAL ?   /* Userid that started trans  */
  FIELD TranCount    AS INTEGER   INITIAL 0   /* Commits in current AI file */
  FIELD NoteCount    AS INTEGER   INITIAL 0   /* Inside the current AI file */
  FIELD AveNoteCount AS DECIMAL   INITIAL 0.0 /* For committed transactions */
  FIELD MaxNoteCount AS INTEGER   INITIAL 0   /* For committed transactions */
  FIELD TranTime     AS INTEGER   INITIAL 0   /* Inside the current AI file */
  FIELD AveTranTime  AS DECIMAL   INITIAL 0.0 /* For committed transactions */
  FIELD MaxTranTime  AS INTEGER   INITIAL 0   /* For committed transactions */
  FIELD IdleTime     AS INTEGER   INITIAL 0   /* Inside the current AI file */
  FIELD FirstTime    AS DATETIME  INITIAL ?   /* Inside the current AI file */
  FIELD LastTime     AS DATETIME  INITIAL ?   /* Inside the current AI file */
  INDEX PrimaryKey IS UNIQUE
        AiSeqNum
        UserName
. /* DEFINE TEMP-TABLE ttUser */
/*
NoteCount, IdleTime - the sum of activity of all sessions with the given userid
inside the current AI file only. IdleTime can be higher than the user's time
(interval between FirstTime and LastTime) as well as the whole AI interval.

TranCount - the number of transactions committed during the current AI file.
It can be, for example, zero while NoteCount is non-zero.

AveNoteCount, MaxNoteCount, AveTranTime, MaxTranTime:
the statistics per the committed transactions (TranCount) only
no matter when the transactions begun. IOW, they can include
the user's activity outside the current AI file (AiSeqNum).

FirstTime, LastTime - the times of first and last notes for userid
inside the current AI file.
*/

/* Trid: 0 code = RL_INMEM version = 3 (12528) */
DEFINE TEMP-TABLE ttCkpt NO-UNDO
  FIELD AiSeqNum   AS INTEGER  /* Seqno of the current AI file              */
  FIELD CkptId     AS INTEGER  /* Id inside the current AI file             */
  FIELD CkptTime   AS DATETIME /* Estimated timestamp of INMEM note         */
  FIELD TimeGap    AS INTEGER  /* Time between two tran notes arround INMEM */
  FIELD NoteCount  AS INTEGER  /* Note count between two INMEM notes        */
  FIELD UpdtCount  AS INTEGER  /* Updt count between two INMEM notes        */
  FIELD TranCount  AS INTEGER  /* Tran count between two INMEM notes        */
  INDEX PrimaryKey IS UNIQUE
        AiSeqNum
        CkptId
. /* DEFINE TEMP-TABLE ttCkpt */
/*
The first note in bi cluster is RL_INMEM - the list of the active transactions.
The same note is written to AI file.
Timestamp of the note is a starting time of checkpoint.

TimeGap - a precision of timestamp estimation (it's really much more accurate).

NoteCount, UpdtCount, TranCount - the counters between checkpoints.
The counters after last checkpoint in current AI file will be stored in
ttAiFile table and will be added to the first checkpoint in the next AI file.
*/

DEFINE TEMP-TABLE ttOSFile NO-UNDO /* used by GetOSFiles() procedure */
  FIELD osFileName AS CHARACTER
  FIELD osFilePath AS CHARACTER
  FIELD osFileAttr AS CHARACTER
  INDEX osFileName
        osFileName
. /* DEFINE TEMP-TABLE ttOSFile */

/* ------------------------------------------------------------------------- */

FUNCTION Percent RETURNS DECIMAL (
  INPUT ipValue1 AS DECIMAL,
  INPUT ipValue2 AS DECIMAL).

  RETURN IF ipValue1 EQ 0 THEN 0.0 ELSE
         IF ipValue2 LE 0 THEN  ?  ELSE
         ROUND(ipValue1 / ipValue2 * 100.0, 0).
END FUNCTION. /* Percent */

/* ------------------------------------------------------------------------- */

FUNCTION String2DateTime RETURNS DATETIME (INPUT ipString AS CHARACTER).
/* Input string must have a format like: Tue Jul 27 12:11:45 2004 */
  DEFINE VARIABLE vDateTime  AS DATETIME  NO-UNDO INITIAL ?.
  DEFINE VARIABLE vMonthList AS CHARACTER NO-UNDO
    INITIAL "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec".
  DEFINE VARIABLE vTime AS CHARACTER NO-UNDO.

  ASSIGN /*ipString = TRIM(REPLACE(ipString, "  ":U, " ":U))*/
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
/* Output string in format: Tue Jul 27 12:11:45 2004 */
  DEFINE VARIABLE vMonthList AS CHARACTER NO-UNDO
    INITIAL "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec".

  DEFINE VARIABLE vDayList AS CHARACTER NO-UNDO
    INITIAL "Sun,Mon,Tue,Wed,Thu,Fri,Sat".

  DEFINE VARIABLE vDate   AS DATE      NO-UNDO.
  DEFINE VARIABLE vTime   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vString AS CHARACTER NO-UNDO.

  ASSIGN vDate = DATE(ipDateTime)
         vTime = INTERVAL (ipDateTime, DATETIME(vDate), "seconds")
         vString = ENTRY(WEEKDAY(vDate), vDayList)
         + " ":U + ENTRY(MONTH(vDate), vMonthList)
         + " ":U + STRING(DAY(vDate), ">9":U)
         + " ":U + STRING(vTime, "HH:MM:SS":U)
         + " ":U + STRING(YEAR(vDate))
  NO-ERROR.
  RETURN vString.
END FUNCTION. /* DateTime2String */

/* -------------------------------------------------------------------------- */

FUNCTION NoteCount RETURNS INTEGER (
  INPUT ipListId   AS INTEGER,
  INPUT ipNoteArea AS INTEGER,
  INPUT ipNoteType AS CHARACTER).

  FOR FIRST ttNoteList NO-LOCK
      WHERE ttNoteList.ListId   EQ ipListId
        AND ttNoteList.NoteArea EQ ipNoteArea
        AND ttNoteList.NoteType EQ ipNoteType:
    RETURN  ttNoteList.NoteCount.
  END.

/* Record not found: */
  RETURN 0.

END FUNCTION. /* NoteCount */

/* -------------------------------------------------------------------------- */

PROCEDURE LoadAreaInfo.
  DEFINE INPUT PARAMETER ipStructureFile AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vArea AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLine AS CHARACTER NO-UNDO.
  DEFINE VARIABLE i     AS INTEGER   NO-UNDO.

  INPUT FROM VALUE(ipStructureFile).
  REPEAT TRANSACTION:
    ASSIGN vLine = "".
    IMPORT UNFORMATTED vLine.

/* d "Area Name":7,64;512 /path/to/dbname_7.d1 */
    IF NOT vLine BEGINS "d ":U
    OR NOT vLine MATCHES "*:*":U THEN
    NEXT.

    ASSIGN vLine = SUBSTRING(vLine, 3)
           vLine =      TRIM(vLine)
           i     =     INDEX(vLine, ":":U) + 1 /* first space after ":" */
           vLine = SUBSTRING(vLine, 1, INDEX(vLine + " ":U, " ":U, i) - 1)
           vArea = INTEGER(SUBSTRING(vLine, i,
                           MIN(INDEX(vLine + ",":U, ",":U),
                               INDEX(vLine + ";":U, ";":U)) - i))
   NO-ERROR.

   IF ERROR-STATUS:NUM-MESSAGES GT 0
   OR CAN-FIND(ttArea WHERE ttArea.AreaNumber EQ vArea) THEN
   NEXT.

   CREATE ttArea.
   ASSIGN ttArea.AreaNumber = vArea
          ttArea.AreaInfo   = vLine
   . /* ASSIGN */
  END.
END PROCEDURE. /* LoadAreaInfo */

/* -------------------------------------------------------------------------- */

PROCEDURE ImportAiScan.

  DEFINE INPUT PARAMETER ipInputFile AS CHARACTER NO-UNDO.

/* The estimation of the average length of line in ai scan: */
  DEFINE VARIABLE vLineLength AS DECIMAL   NO-UNDO INITIAL 60.0.
  DEFINE VARIABLE vFileName   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFileSize   AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vReportLine AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vReportDate AS DATE      NO-UNDO.
  DEFINE VARIABLE vReportTime AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vPrevETime  AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vAiBegTime  AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vAiNewTime  AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vAiOpenTime AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vAiSeqNum   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLastTime   AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vPrevTime   AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vInterval   AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vLine         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vItem         AS CHARACTER NO-UNDO EXTENT 13.
  DEFINE VARIABLE vLineCount    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNoteCount    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vUpdtCount    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vDbkeyCount   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNoReuseCount AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vIsCkpt        AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE vCkptId        AS INTEGER   NO-UNDO. /* inside AI file     */
  DEFINE VARIABLE vCkpNoteCount1 AS INTEGER   NO-UNDO. /*between tran & INMEM*/
  DEFINE VARIABLE vCkpNoteCount2 AS INTEGER   NO-UNDO. /* between tran notes */
  DEFINE VARIABLE vCkpNoteCount  AS INTEGER   NO-UNDO. /* as vNoteCount      */
  DEFINE VARIABLE vCkpUpdtCount  AS INTEGER   NO-UNDO. /* as vUpdtCount      */
  DEFINE VARIABLE vCkpTranCount  AS INTEGER   NO-UNDO. /* as vTBgnCount      */
  DEFINE VARIABLE vGapTBgnCount  AS INTEGER   NO-UNDO. /* as vTBgnCount      */
  DEFINE VARIABLE vGapTEndCount  AS INTEGER   NO-UNDO. /* as vTEndCount      */
  DEFINE VARIABLE vGapNoteCount  AS INTEGER   NO-UNDO. /* as vNoteCount      */
  DEFINE VARIABLE vGapUpdtCount  AS INTEGER   NO-UNDO. /* as vUpdtCount      */
  DEFINE VARIABLE vGapMaxActTran AS INTEGER   NO-UNDO. /* as vCurActTran     */
  DEFINE VARIABLE vGapMinActTran AS INTEGER   NO-UNDO. /* as vCurActTran     */
  DEFINE VARIABLE vTimeGap       AS INTEGER   NO-UNDO. /* between tran notes */
  DEFINE VARIABLE vEvent         AS CHARACTER NO-UNDO. /* Db event during gap*/
  DEFINE VARIABLE vIdleTime      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLastCkpTime   AS DATETIME  NO-UNDO.

  DEFINE VARIABLE vNoteTrid   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNoteArea   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNoteDbkey  AS INT64     NO-UNDO.
  DEFINE VARIABLE vNoteUpdCtr AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNoteCode   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNoteType   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vNoteList   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vBlockType  AS CHARACTER NO-UNDO.

  DEFINE VARIABLE vUserid      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vTBgnCount   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTEndCount  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTranAtBgn   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTranAtEnd   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vOldAtEnd    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vCurActTran  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vAveActTran  AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vMaxActTran  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMaxActTime  AS DATETIME  NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  ASSIGN FILE-INFO:FILE-NAME = ipInputFile
         vFileSize   = FILE-INFO:FILE-SIZE / 100.0
         vReportLine = MAX(1000, vFileSize / vLineLength)
         vReportLine = MIN(vReportLine, 10000)
         vFileName = IF OPSYS EQ "Unix":U THEN "/":U ELSE "~\":U /*slash*/
         vFileName = SUBSTRING(FILE-INFO:FULL-PATHNAME,
                       R-INDEX(FILE-INFO:FULL-PATHNAME, vFileName) + 1)
  . /*ASSIGN */

  INPUT FROM VALUE(ipInputFile).

/* Header of AI scan: ---------------------------------------------------------

    Last AIMAGE BEGIN Thu Jun 17 21:44:19 2010 (1640)
    Last AIMAGE NEW Sat Nov 30 08:00:01 2013 (1641)
    This is aimage file number 59926 since the last AIMAGE BEGIN. (1642)
    This file was last opened for output on Sat Nov 30 08:00:01 2013. (1643)
*/
  ASSIGN vAiSeqNum   = ?
         vAiBegTime  = ?
         vAiNewTime  = ?
         vAiOpenTime = ?
         vLineCount  = 0
         vPrevETime  = ETIME
  . /* ASSIGN */
  REPEAT:
    ASSIGN vLine = "":U.
    IMPORT UNFORMATTED vLine.
    ASSIGN vLine = TRIM(vLine).

    IF vLine BEGINS "Trid: ":U THEN
    LEAVE.

    IF vLine MATCHES "* (1640)":U THEN
    ASSIGN vLine = SUBSTRING(vLine, LENGTH(vLine) - 30, 24)
           vLine = TRIM(REPLACE(vLine, "  ":U, " ":U))
           vAiBegTime = String2DateTime(vLine)
    . /* ASSIGN */
    ELSE
    IF vLine MATCHES "* (1641)":U THEN
    ASSIGN vLine = SUBSTRING(vLine, LENGTH(vLine) - 30, 24)
           vLine = TRIM(REPLACE(vLine, "  ":U, " ":U))
           vAiNewTime = String2DateTime(vLine)
    . /* ASSIGN */
    ELSE
    IF vLine MATCHES "* (1643)":U THEN
    ASSIGN vLine = SUBSTRING(vLine, LENGTH(vLine) - 31, 24)
           vLine = TRIM(REPLACE(vLine, "  ":U, " ":U))
           vAiOpenTime = String2DateTime(vLine)
    . /* ASSIGN */
    ELSE
    IF vLine MATCHES "* (1642)":U THEN
    REPEAT i = NUM-ENTRIES(vLine, " ":U) - 1 TO 1 BY -1:
      ASSIGN vAiSeqNum = INTEGER(ENTRY(i, vLine, " ":U)) NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES EQ 0 THEN
      LEAVE.
    END.
  END. /* REPEAT */

  IF vAiSeqNum   EQ ?
  OR vAiBegTime  EQ ?
  OR vAiNewTime  EQ ?
  OR vAiOpenTime EQ ? THEN
  DO:
    MESSAGE
      "Failed to parse the AI header of" vFileName SKIP
      "Processing interrupted."
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    QUIT.
  END.

/* Check the uniqueness of vAiSeqNum: */
  FOR FIRST ttAiFile NO-LOCK
      WHERE ttAiFile.AiSeqNum EQ vAiSeqNum:
    MESSAGE SUBSTITUTE(
      "AI SeqNo &1 was found in file &2", vAiSeqNum, vFileName) SKIP
      "but the file with that number was already processed."    SKIP
      "Previous file:" ttAiFile.AiFileName                      SKIP
      "AIMAGE BEGIN:" DateTime2String(vAiBegTime) "vs"
              DateTime2String(ttAiFile.AiBegTime)               SKIP
      "AIMAGE   NEW:" DateTime2String(vAiNewTime) "vs"
              DateTime2String(ttAiFile.AiNewTime)               SKIP
      "Processing interrupted."
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    QUIT.
  END.

  IF SEEK(INPUT) EQ ? THEN
  DO:
    MESSAGE
      "Unexpected structure of" vFileName SKIP
      "Processing interrupted."
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    QUIT.
  END.

/* Move to the tailer: */
  SEEK INPUT TO END.
  SEEK INPUT TO SEEK(INPUT) - 512.

/* Tail of AI scan: -----------------------------------------------------------

6523036 notes were processed. (1634)
2 in-flight transactions. (3785)
1670 transactions were started. (1635)
1669 transactions were completed. (11138)
At the end of the .ai file, 3 transactions were still active. (1636)
*/
  ASSIGN vTranAtBgn = ?
         vTranAtEnd = ?
  . /* ASSIGN */
  REPEAT:

    ASSIGN vLine = "":U.
    IMPORT UNFORMATTED vLine.

    IF vLine BEGINS "Trid: ":U THEN
    NEXT.

    IF vLine MATCHES "* (3785)":U THEN
    REPEAT i = 1 TO NUM-ENTRIES(vLine, " ":U) - 1:
      ASSIGN vTranAtBgn = INTEGER(ENTRY(i, vLine, " ":U)) NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES EQ 0 THEN
      LEAVE.
    END.
    ELSE

    IF vLine MATCHES "* (1636)":U THEN
    REPEAT i = 1 TO NUM-ENTRIES(vLine, " ":U) - 1:
      ASSIGN vTranAtEnd = INTEGER(ENTRY(i, vLine, " ":U)) NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES EQ 0 THEN
      LEAVE.
    END.
  END. /* REPEAT */

/* Main body of AI scan: ------------------------------------------------------

Trid: 0 code = RL_AIEXT version = 2 (12528)
Trid: 0 dbkey = 0  update counter = 0 (12530)
Trid: 1796235913 code = RL_CXREM version = 2 (12528)
Trid: 1796235913 area = 10   dbkey = 69353255   update counter = 2295 (12529)
Trid: 1796235913 code = RL_LSTMOD version = 2 (12528)
Trid: 1796235913 area = 6   dbkey = 64   update counter = 99363 (12529)
Trid: 1796235913 code = RL_CXREM version = 2 (12528)
Trid: 1796235913 area = 10   dbkey = 74881282   update counter = 18790 (12529)
*/
  ASSIGN vLineCount    = 0
         vNoteCount    = 0
         vUpdtCount    = 0
         vCkpTranCount = 0
         vCkpNoteCount = 0
         vCkpUpdtCount = 0
         vGapTBgnCount = 0
         vGapNoteCount = 0
         vGapUpdtCount = 0
         vCkptId       = 0
         vPrevTime     = vAiNewTime
         vLastTime     = vAiNewTime
         vOldAtEnd     = vTranAtBgn
         vTBgnCount    = 0
         vTEndCount    = 0
         vCurActTran   = vTranAtBgn
         vAveActTran   = vTranAtBgn
         vMaxActTran   = vTranAtBgn
         vMaxActTime   = vAiNewTime
        vGapMaxActTran = vTranAtBgn
        vGapMinActTran = vTranAtBgn
  . /* ASSIGN */

  IF SEEK(PerSecReport) EQ 0 THEN  /* -------------------------------------- */
  PUT STREAM PerSecReport UNFORMATTED
           "AiNum"     /* AI sequence number (Seqno)                         */
    {&Sep} "Date"      /* Date of transaction notes                          */
    {&Sep} "Time"      /* Time of transaction notes                          */
    {&Sep} "Intr"      /* Interval (1 second or time gap between tran notes) */
    {&Sep} "Notes"     /* Note count during the interval                     */
    {&Sep} "Updates"   /* Update notes count during interval                 */
    {&Sep} "TranBgn"   /* RL_TBGN note count during interval                 */
    {&Sep} "TranEnd"   /* RL_TEND note count during interval                 */
    {&Sep} "MinTran"   /* Minimum number of simultaneous transactions        */
    {&Sep} "MaxTran"   /* Maximum number of simultaneous transactions        */
    {&Sep} "Event"     /* Db events during interval (if any)                 */
  SKIP. /* PUT */

/* Reopen the file after reading its tailer: */
  INPUT FROM VALUE(ipInputFile).

  ImportLine:
  REPEAT:
    ASSIGN vLineCount = vLineCount + 1
           vItem = "":U.
    IMPORT vItem.
    PROCESS EVENTS.

    IF vItem[1] NE "Trid:":U THEN
    NEXT.

    IF vLineCount MOD vReportLine EQ 0 THEN
    DISPLAY
      STRING(SEEK(INPUT) / vFileSize, ">>9%")                    @ vLoad
      STRING(INTEGER((ETIME - vPrevETime) / 1000), "HH:MM:SS":U) @ vTime
    WITH FRAME StatusFrame.

/*  1   2    3  4     5       6    7 8    9
Trid: TRID code = NOTETYPE version = 1 (12528)
*/  IF vItem[9] EQ "(12528)":U THEN
    DO:
      ASSIGN vNoteCount = vNoteCount + 1
             vNoteType     = vItem[5]
             vNoteTrid     = INTEGER(vItem[2])
      . /* ASSIGN */

/* vNoteCode is an integer equivalent of vNoteType: */
      FIND FIRST ttNoteType NO-LOCK
           WHERE ttNoteType.NoteType EQ vNoteType
      NO-ERROR.
      IF AVAILABLE ttNoteType THEN
      ASSIGN vNoteCode = ttNoteType.NoteCode.
      ELSE
      DO TRANSACTION:
        ASSIGN vNoteCode = 0.
        FOR EACH ttNoteType NO-LOCK
              BY ttNoteType.NoteCode DESCENDING:
          ASSIGN vNoteCode = ttNoteType.NoteCode + 1.
          LEAVE.
        END.
        CREATE ttNoteType.
        ASSIGN ttNoteType.NoteCode = vNoteCode
               ttNoteType.NoteType = vNoteType
        . /* ASSIGN */
      END.
    END. /* Trid: <Trid> code = <name> version = <version> (12528) */
    ELSE

/* "System" notes: --------------------------------------------------------- */

/*  1   2    3   4 5    6       7    8 9   10
Trid: TRID dbkey = 0  update counter = 0 (12530)
*/  IF vItem[10] EQ "(12530)":U THEN
    DO TRANSACTION:

/*   IF vNoteTrid NE INTEGER(vItem[2]) THEN ... */

      FIND FIRST ttTran
           WHERE ttTran.Trid EQ vNoteTrid
      NO-ERROR.
      IF NOT AVAILABLE ttTran THEN
      CREATE ttTran.
      IF NEW ttTran THEN
      ASSIGN ttTran.Trid      = vNoteTrid
             ttTran.NoteCount = 1
             ttTran.AiSeqNum  = vAiSeqNum
             ttTran.LastTime  = vLastTime
             vIdleTime = INTERVAL(vLastTime, vAiNewTime, "seconds":U)
      . /* ASSIGN */
      ELSE
      ASSIGN vIdleTime = IF ttTran.LastTime GE vPrevTime THEN 0 ELSE
                         INTERVAL(vLastTime, ttTran.LastTime, "seconds":U) - 1
             ttTran.NoteCount = ttTran.NoteCount + 1
             ttTran.LastTime  = vLastTime
      . /* ASSIGN */

      IF vIdleTime GT 0 THEN
      ASSIGN ttTran.IdleTime = ttTran.IdleTime + vIdleTime
             ttTran.MaxGTime = MAX(ttTran.MaxGTime, vIdleTime)
             ttTran.GapCount = ttTran.GapCount + 1
      . /* ASSIGN */
/*
 "Clocks work" = db has the transaction notes per each second.
   In this case the estimation of ttTran.IdleTime is rather accurate.
 "Clocks don't work" - for example, the current transaction is the only one.
   For all its notes we have only one "time mark" (in RL_TBGN note).
   Hence we can't estimate the idle time inside the transaction.
 Middle case #1: between previous and current notes of the transaction there
   is only one "time mark": ttTran.LastTime = vPrevTime LT vLastTime.
   There is a time gap between the transaction notes (vLastTime - vPrevTime)
   but it does not mean that the current transaction was idle.
   Let's treat the case as "clocks does not work" and the idle time is zero.
 Middle case #2: between previous and current notes of the transaction there
   are at least two "time marks": ttTran.LastTime LT vPrevTime LE vLastTime.
   Transaction idle time equals at least to (vLastTime - vPrevTime).
   But let's treat the case as "clocks work" and
   let's estimate the idle time as (vLastTime - ttTran.LastTime).
 Middle case <N>: It's possible to check the number of "time marks" between
   the notes of current transaction (e.g. ttTran.LastMark as vTBgnCount)
   but it will require an extra field in ttTran.
   It's enough to use the cases 1 & 2.
*/
      CASE vNoteType:
        WHEN "RL_TBGN":U  THEN
        DO:
          ACCUMULATE vNoteTrid (MIN MAX).
          ASSIGN ttTran.UserName = vUserid
                 ttTran.BgnTime  = vLastTime
                 ttTran.IdleTime = 0
                 ttTran.MaxGTime = 0
                 ttTran.GapCount = 0
                 ttTran.PrevIdleTime = 0
                 ttTran.PrevNoteCount = 0
                 vMaxActTime     = vLastTime WHEN vCurActTran EQ vMaxActTran
                 vCurActTran     = vCurActTran + 1
                 vTBgnCount      = vTBgnCount  + 1
                 vGapMaxActTran  = MAX(vGapMaxActTran, vCurActTran)
                 vGapMinActTran  = MIN(vGapMinActTran, vCurActTran)
                 vMaxActTran     = MAX(   vMaxActTran, vCurActTran)
                 vAveActTran     = vAveActTran + (vCurActTran - vAveActTran)
                                               / (vTBgnCount + vTEndCount)
          . /* ASSIGN */
          NEXT ImportLine. /* Don't need to create ttNote */
        END.

        WHEN "RL_TEND":U  THEN
        DO:
          ASSIGN ttTran.EndTime = vLastTime
                 vOldAtEnd   = vOldAtEnd - 1 WHEN ttTran.AiSeqNum NE vAiSeqNum
                                               OR ttTran.BgnTime  EQ ?
                 vCurActTran = vCurActTran - 1
                 vTEndCount  = vTEndCount  + 1
              vGapMaxActTran = MAX(vGapMaxActTran, vCurActTran)
              vGapMinActTran = MIN(vGapMinActTran, vCurActTran)
                 vAveActTran = vAveActTran + (vCurActTran - vAveActTran)
                                           / (vTBgnCount + vTEndCount)
          . /* ASSIGN */
          NEXT ImportLine. /* Don't need to create ttNote */
        END.

        WHEN "RL_INMEM":U THEN
        DO:
          CREATE ttCkpt.
          ASSIGN ttCkpt.AiSeqNum  = vAiSeqNum
                 vCkptId          = vCkptId + 1
                 ttCkpt.CkptId    = vCkptId
                 ttCkpt.CkptTime  = vLastTime
                 ttCkpt.NoteCount = vNoteCount - vCkpNoteCount
                 ttCkpt.UpdtCount = vUpdtCount - vCkpUpdtCount
                 ttCkpt.TranCount = vTBgnCount - vCkpTranCount
                 vCkpNoteCount1   = vNoteCount - vGapNoteCount - 1
                 vCkpNoteCount2   = 0
                 vCkpNoteCount    = vNoteCount
                 vCkpUpdtCount    = vUpdtCount
                 vCkpTranCount    = vTBgnCount
                 vLastCkpTime     = ttCkpt.CkptTime
          . /* ASSIGN */
        END.

      END CASE.

/* The list of the "service" notes per Tran: ------------------------------- */
      FIND FIRST ttNote
           WHERE ttNote.NoteArea    EQ 0
             AND ttNote.NoteDbkey   EQ 0
             AND ttNote.NoteCode    EQ vNoteCode
             AND ttNote.NoteTrid    EQ vNoteTrid
      NO-ERROR.
      IF NOT AVAILABLE ttNote THEN
      CREATE ttNote.
      ASSIGN ttNote.NoteArea    = 0
             ttNote.NoteDbkey   = 0
             ttNote.NoteCode    = vNoteCode
             ttNote.NoteTrid    = vNoteTrid
             ttNote.NoteCount   = ttNote.NoteCount + 1
             ttNote.FirstUpdCtr = 0
             ttNote.LastUpdCtr  = 0
             ttNote.FirstTime   = vLastTime WHEN NEW ttNote
             ttNote.LastTime    = vLastTime
      . /* ASSIGN */
    END. /* Trid: TRID dbkey = 0  update counter = 0 (12530) */
    ELSE

/* "Update" notes: --------------------------------------------------------- */

/*  1   2    3  4   5      6   7   8        9      10   11  12     13
Trid: TRID area = AREA   dbkey = DBKEY   update counter = UPDCTR (12529)
*/  IF vItem[13] EQ "(12529)":U THEN
    DO TRANSACTION:

/*   IF vNoteTrid NE INTEGER(vItem[2]) THEN ... need a sanity check? */

      ASSIGN vUpdtCount  = vUpdtCount + 1
             vNoteArea   = INTEGER(vItem[5])
             vNoteDbkey  = INT64  (vItem[8])
             vNoteUpdCtr = INTEGER(vItem[12])
      NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES GT 0 THEN
      NEXT ImportLine.

      FIND FIRST ttArea EXCLUSIVE-LOCK
           WHERE ttArea.AreaNumber EQ vNoteArea
      NO-ERROR.
      IF NOT AVAILABLE ttArea THEN
      CREATE ttArea.
      ASSIGN ttArea.AreaNumber = vNoteArea.

/* The list of notes per Tran: --------------------------------------------- */
      FIND FIRST ttTran
           WHERE ttTran.Trid EQ vNoteTrid
      NO-ERROR.
      IF NOT AVAILABLE ttTran THEN
      CREATE ttTran.
      IF NEW ttTran THEN
      ASSIGN ttTran.Trid      = vNoteTrid
             ttTran.NoteCount = 1
             ttTran.UpdtCount = 1
             ttTran.AiSeqNum  = vAiSeqNum
             ttTran.LastTime  = vLastTime
             vIdleTime = INTERVAL(vLastTime, vAiNewTime, "seconds":U)
      . /* ASSIGN */
      ELSE
      ASSIGN vIdleTime = IF ttTran.LastTime GE vPrevTime THEN 0 ELSE
                         INTERVAL(vLastTime, ttTran.LastTime, "seconds":U) - 1
             ttTran.NoteCount = ttTran.NoteCount + 1
             ttTran.UpdtCount = ttTran.UpdtCount + 1
             ttTran.LastTime = vLastTime

      . /* ASSIGN */

      IF vIdleTime GT 0 THEN
      ASSIGN ttTran.IdleTime = ttTran.IdleTime + vIdleTime
             ttTran.MaxGTime = MAX(ttTran.MaxGTime, vIdleTime)
             ttTran.GapCount = ttTran.GapCount + 1
      . /* ASSIGN */

      FIND FIRST ttNote
           WHERE ttNote.NoteArea  EQ vNoteArea
             AND ttNote.NoteDbkey EQ vNoteDbkey
             AND ttNote.NoteCode  EQ vNoteCode
             AND ttNote.NoteTrid  EQ vNoteTrid
      NO-ERROR.
      IF NOT AVAILABLE ttNote THEN
      CREATE ttNote.
      ASSIGN ttNote.NoteArea    = vNoteArea
             ttNote.NoteDbkey   = vNoteDbkey
             ttNote.NoteCode    = vNoteCode
             ttNote.NoteTrid    = vNoteTrid
             ttNote.NoteCount   = ttNote.NoteCount + 1
             ttNote.FirstUpdCtr = vNoteUpdCtr WHEN NEW ttNote
             ttNote.LastUpdCtr  = vNoteUpdCtr
             ttNote.FirstTime   = vLastTime   WHEN NEW ttNote
             ttNote.LastTime    = vLastTime
      . /* ASSIGN */

/* Link the recent RL_SEINC to the current RL_RMCR note: */
      CASE vNoteType:
        WHEN "RL_SEINC":U THEN
        ASSIGN ttTran.LastSeqIndex = ttTran.UpdtCount.

        WHEN "RL_RMCR":U THEN
        IF ttTran.LastSeqIndex NE ? THEN
        DO:
/* Distance: the number of notes between the pair of RL_SEINC and RL_RMCR */
          ASSIGN i = ttTran.UpdtCount - ttTran.LastSeqIndex - 1
                 ttTran.LastSeqIndex = ?

                 ttTran.UsedSeqCount = ttTran.UsedSeqCount + 1
                 ttTran.AveSeqPath = ttTran.AveSeqPath
                              + (i - ttTran.AveSeqPath) / ttTran.UsedSeqCount
                 ttTran.MaxSeqPath = MAX(ttTran.MaxSeqPath, i)
          . /* ASSIGN */

/* Create a fake note with type "RL_SEINC":U for the current RM block: */
          FIND FIRST ttNoteType NO-LOCK
               WHERE ttNoteType.NoteType EQ "RL_SEINC":U.
          ASSIGN vNoteCode = ttNoteType.NoteCode.

          FIND FIRST ttNote
               WHERE ttNote.NoteArea    EQ vNoteArea
                 AND ttNote.NoteDbkey   EQ vNoteDbkey
                 AND ttNote.NoteCode    EQ vNoteCode
                 AND ttNote.NoteTrid    EQ vNoteTrid
          NO-ERROR.
          IF NOT AVAILABLE ttNote THEN
          CREATE ttNote.
          ASSIGN ttNote.NoteArea    = vNoteArea
                 ttNote.NoteDbkey   = vNoteDbkey
                 ttNote.NoteCode    = vNoteCode
                 ttNote.NoteTrid    = vNoteTrid
                 ttNote.NoteCount   = ttNote.NoteCount + 1
                 ttNote.FirstUpdCtr = ? /* it means a fake note */
                 ttNote.LastUpdCtr  = ?
                 ttNote.FirstTime   = vLastTime WHEN NEW ttNote
                 ttNote.LastTime    = vLastTime
          . /* ASSIGN */
        END. /* IF ttTran.LastSeqIndex NE ? */
      END CASE. /* vNoteType */

    END. /* Trid: <n> area = <n>  dbkey = <n>  update counter = <n> (12529) */
    ELSE

/*  1   2   3   4  5     6       7     8
Trid: TRID Sat Nov 30 08:00:01 2013. (2598)
*/  IF vItem[8] EQ "(2598)":U THEN
    DO:
      ASSIGN vPrevTime    = vLastTime
             vLastTime    = String2DateTime(
                             vItem[3] + " " + vItem[4] + " " + vItem[5] + " "
                           + vItem[6] + " " + TRIM(vItem[7], ".":U))
             vTimeGap      = INTERVAL(vLastTime, vPrevTime, "seconds":U)
             vIsCkpt       = FALSE
      . /* ASSIGN */

/* First timestamp after RL_INMEM note: */
      IF vCkpNoteCount1 GT vCkpNoteCount2 THEN
      FOR FIRST ttCkpt
          WHERE ttCkpt.AiSeqNum EQ vAiSeqNum
            AND ttCkpt.CkptId   EQ vCkptId
      TRANSACTION:
         ASSIGN vCkpNoteCount2  = vGapNoteCount
                ttCkpt.TimeGap  = vTimeGap
                ttCkpt.CkptTime = ttCkpt.CkptTime
                + INTEGER(vTimeGap * 1000.0 * vCkpNoteCount1 / vCkpNoteCount2)
/* Db restart: */
                ttCkpt.CkptTime = vAiOpenTime WHEN vAiOpenTime GT vPrevTime
                                               AND vAiOpenTime LE vLastTime
                vIsCkpt         = TRUE
                vLastCkpTime = ttCkpt.CkptTime
         . /* ASSIGN */
      END. /* FOR FIRST ttCkpt */

/* === Tran/Time Gap Report ================================================ */

      IF vTimeGap EQ 0 THEN
      NEXT ImportLine.

      ASSIGN
        vReportDate = DATE(vPrevTime)
        vReportTime = INTERVAL(vPrevTime, DATETIME(vReportDate), "seconds":U)
        vGapTBgnCount = vTBgnCount - vGapTBgnCount
        vGapTEndCount = vTEndCount - vGapTEndCount
        vGapNoteCount = vNoteCount - vGapNoteCount
        vGapUpdtCount = vUpdtCount - vGapUpdtCount
        vEvent = "":U
        vEvent = vEvent + ",Ckp"     WHEN vIsCkpt
        vEvent = vEvent + ",AiNew"   WHEN vAiNewTime  EQ vPrevTime
        vEvent = vEvent + ",Restart" WHEN vAiOpenTime GT vPrevTime
                                      AND vAiOpenTime LE vLastTime
        vEvent = SUBSTRING(vEvent, 2)
      . /* ASSIGN */

/* For FilesReport: */
      IF vTimeGap GT 1 THEN
      ACCUMULATE vTimeGap - 1 (AVERAGE TOTAL MAXIMUM).

      ASSIGN vInterval = INTERVAL(vLastTime, vAiNewTime, "seconds":U)
      . /* ASSIGN  */

      PUT STREAM PerSecReport UNFORMATTED
               /*AiNum    */ vAiSeqNum
        {&Sep} /*Date     */ vReportDate
        {&Sep} /*Time     */ STRING(vReportTime, "HH:MM:SS":U)
        {&Sep} /*Intr     */ vTimeGap
        {&Sep} /*Notes    */ vGapNoteCount
        {&Sep} /*Updates  */ vGapUpdtCount
        {&Sep} /*TranBgn  */ vGapTBgnCount
        {&Sep} /*TranEnd  */ vGapTEndCount
        {&Sep} /*MinTran  */ vGapMinActTran
        {&Sep} /*MaxTran  */ vGapMaxActTran
        {&Sep} /*Event    */ vEvent
      SKIP. /* PUT */

      ASSIGN vGapTBgnCount  = vTBgnCount
             vGapTEndCount  = vTEndCount
             vGapNoteCount  = vNoteCount
             vGapUpdtCount  = vUpdtCount
             vGapMinActTran = vCurActTran
             vGapMaxActTran = 0
      . /* ASSIGN */

    END. /* Trid: <num> <time>. (2598) */
    ELSE

/* 1    2    3   4     5      6
Trid: TRID User Id: USERID (12531)
*/  IF vItem[6] EQ "(12531)":U THEN
    ASSIGN vUserid = vItem[5].

  END. /* ImportLine: REPEAT */

  INPUT CLOSE.

  DO TRANSACTION:
    CREATE ttAiFile.
    ASSIGN ttAiFile.AiSeqNum     = vAiSeqNum
           ttAiFile.AiFileName   = vFileName
           ttAiFile.AiFileSize   = vFileSize / 10.24 /*KB*/
           ttAiFile.LineCount    = vLineCount
           ttAiFile.AiBegTime    = vAiBegTime
           ttAiFile.AiOpenTime   = vAiOpenTime
           ttAiFile.AiNewTime    = vAiNewTime
           ttAiFile.LastTime     = vLastTime
           ttAiFile.AiInterval   = INTERVAL(vLastTime, vAiNewTime, "seconds":U)
           ttAiFile.NoteCount    = vNoteCount
           ttAiFile.UpdtCount    = vUpdtCount
           ttAiFile.TranCount    = vTBgnCount
           ttAiFile.MinTrid      = ACCUM MIN vNoteTrid
           ttAiFile.MaxTrid      = ACCUM MAX vNoteTrid
           ttAiFile.TranAtBgn    = vTranAtBgn
           ttAiFile.TranAtEnd    = vTranAtEnd
           ttAiFile.OldAtEnd     = vOldAtEnd
           ttAiFile.AveActTran   = vAveActTran
           ttAiFile.MaxActTran   = vMaxActTran
           ttAiFile.MaxActTime   = vMaxActTime
           ttAiFile.TotTimeGap   = ACCUM TOTAL   vTimeGap - 1
/* ACCUM MAXIMUM returns ? if there were no loops, replace the value by 0: */
           ttAiFile.MaxTimeGap   = IF ttAiFile.TotTimeGap EQ 0 THEN 0 ELSE 
                                   ACCUM MAXIMUM vTimeGap - 1
           ttAiFile.AveTimeGap   = ACCUM AVERAGE vTimeGap - 1
           ttAiFile.CkpNoteCount = vNoteCount - vCkpNoteCount
           ttAiFile.CkpUpdtCount = vUpdtCount - vCkpUpdtCount
           ttAiFile.CkpTranCount = vTBgnCount - vCkpTranCount
           ttAiFile.LastCkpTime  = vLastCkpTime
    . /* ASSIGN */
  END. /* TRANSACTION */


  DISPLAY STRING(INTEGER((ETIME - vPrevETime) / 1000), "HH:MM:SS":U) @ vLoad
          "Wait..." @ vTime
  WITH FRAME StatusFrame.

  ASSIGN vPrevETime = ETIME.
  RUN MainReports(vAiSeqNum).

  DISPLAY STRING(INTEGER((ETIME - vPrevETime) / 1000), "HH:MM:SS":U) @ vTime
  WITH FRAME StatusFrame.

END PROCEDURE. /* ImportAiScan */


/* ------------------------------------------------------------------------- */

PROCEDURE MainReports:

  DEFINE INPUT PARAMETER ipAiSeqNum AS INTEGER NO-UNDO.

  DEFINE BUFFER bfNewList FOR ttNoteList.
  DEFINE BUFFER bfUser FOR ttUser.

  &SCOPED-DEFINE  DbkeyList -2
  &SCOPED-DEFINE   AreaList -1
  &SCOPED-DEFINE AiFileList  0

  DEFINE VARIABLE vTranTime     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vIdleTime     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTranAlloc    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTBgnCount    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNoteCount    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vConcCount    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vUpdtCount    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMaxNoteCount AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vMaxTrid      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vFirstUpdCtr  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vLastUpdCtr   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vAiFirstTime  AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vFirstTime    AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vLastTime     AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vInterval     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNoReuseCount AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vUsedSeqCount AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNoteList     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vNoteList1    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vNoteList2    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vSubTrans     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTranSeqs     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNoteThold    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vExtended     AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE vBlockType    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vDbkeyCount   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vDecDbkey     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vAveDbkey     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vDevDbkey     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vMinDbkey     AS INT64     NO-UNDO.
  DEFINE VARIABLE vMaxDbkey     AS INT64     NO-UNDO.
  DEFINE VARIABLE vNoteFact     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vUserCount    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTopUserName  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vTopNoteCount AS INTEGER   NO-UNDO.

  IF SEEK(AreasReport) EQ 0 THEN /* --------------------------------------- */
  PUT STREAM AreasReport UNFORMATTED
           "AiScan"    /* Name of AI scan file                            */
    {&Sep} "AiNum"     /* AI sequence number (Seqno)                      */
    {&Sep} "NewTime"   /* Time of rfutil aimage new                       */
    {&Sep} "Interval"  /* AI file interval                                */
    {&Sep} "Area"      /* Area number                                     */
    {&Sep} "AreaInfo"  /* Area info from .st file                         */
    {&Sep} "MinDbkey"  /* Minimum dbkey of updated blocks                 */
    {&Sep} "MaxDbkey"  /* Maximum dbkey of updated blocks                 */
    {&Sep} "X"         /* Area is extended (RL_XTDB) => MaxDbkey eq HWM   */
    {&Sep} "AveDbkey"  /* Average dbkey of updated blocks                 */
    {&Sep} "DevDbkey"  /* Standard deviation of the dbkeys                */
    {&Sep} "%DevDbkey" /* Percentage of the maximum dbkey                 */
    {&Sep} "Updates"   /* Count of the "update" notes per area            */
    {&Sep} "%Updates"  /* Percentage of the "update" notes per AI file    */
    {&Sep} "Dbkeys"    /* Count of unique dbkeys                          */
    {&Sep} "NoReuse"   /* Count of dbkeys that were updated only once     */
    {&Sep} "%NoReuse"  /* Percentage of the count of unique dbkeys        */
    {&Sep} "AveReuse"  /* Average number of updates per "reusable" dbkeys */
    {&Sep} "Trans"     /* Number of transactions updated the area         */
    {&Sep} "%Trans"    /* Percentage of the total number of transactions  */
    {&Sep} "UsedSeqs"  /* Count of the "used" sequence increments         */
    {&Sep} "NoteList"  /* List of notes per area                          */
  SKIP. /* PUT STREAM AreasReport */

  IF SEEK(DbkeyReport) EQ 0 THEN  /* --------------------------------------- */
  PUT STREAM DbkeyReport UNFORMATTED
           "AiScan"      /* Name of AI scan file                            */
    {&Sep} "AiNum"       /* AI sequence number (Seqno)                      */
    {&Sep} "NewTime"     /* Time of rfutil aimage new                       */
    {&Sep} "AiIntrvl"    /* AI file interval                                */
    {&Sep} "Area"        /* Area number                                     */
    {&Sep} "AreaInfo"    /* Area info from .st file                         */
    {&Sep} "Dbkey"       /* Dbkey from transaction's notes                  */
    {&Sep} "T"           /* Block type: Sequence, Data, Index or Other      */
    {&Sep} "FirstUpdCtr" /* Update counter in the first note of dbkey       */
    {&Sep} "LastUpdCtr"  /* Update counter in the last note of dbkey        */
    {&Sep} "FirstTime"   /* Time of the first dbkey note                    */
    {&Sep} "UpdIntrvl"   /* Interval between the first and last dbkey notes */
    {&Sep} "Updates"     /* Number of dbkey updates                         */
    {&Sep} "Upd/sec"     /* Updates per second                              */
    {&Sep} "ConcUpd"     /* Concurrent block updates (a.k.a. competition)   */
    {&Sep} "Trans"       /* Number of transactions that updated dbkey       */
    {&Sep} "Trid"        /* Example of transaction that updated dbkey       */
    {&Sep} "NoteList"    /* Sorted list of the notes that update dbkey      */
  SKIP. /* PUT STREAM DbkeyReport */

  IF SEEK(NotesReport) EQ 0 THEN /* --------------------------------------- */
  PUT STREAM NotesReport UNFORMATTED
           "AiScan"    /* Name of AI scan file       */
    {&Sep} "AiNum"     /* AI sequence number (Seqno) */
    {&Sep} "NewTime"   /* Time of rfutil aimage new  */
    {&Sep} "Interval"  /* AI file interval           */
    {&Sep} "Area"      /* Area number                */
    {&Sep} "AreaInfo"  /* Area info from .st file    */
    {&Sep} "Type"      /* Note type: RL_*            */
    {&Sep} "Count"     /* Note count                 */
  SKIP. /* PUT STREAM NotesReport */

/* The most early time in all AI scans
   to replace ttTran.BgnTime when it's unknwon: */
  FOR EACH ttAiFile NO-LOCK
        BY ttAiFile.AiSeqNum:
    ASSIGN vAiFirstTime = ttAiFile.AiNewTime.
  END.

  FIND FIRST ttAiFile NO-LOCK
       WHERE ttAiFile.AiSeqNum EQ ipAiSeqNum.

  FOR EACH ttArea NO-LOCK:

    ASSIGN vDbkeyCount   = 0
           vNoReuseCount = 0
           vAveDbkey     = 0.0
           vDevDbkey     = 0.0
    . /* ASSIGN */

    FOR EACH ttNote NO-LOCK
       WHERE ttNote.NoteArea EQ ttArea.AreaNumber
    BREAK BY ttNote.NoteArea
          BY ttNote.NoteDbkey
          BY ttNote.NoteTrid:

      IF FIRST-OF(ttNote.NoteDbkey) THEN
      ASSIGN vTBgnCount    = 0
             vMaxNoteCount = 0
             vMaxTrid      = ?
             vUsedSeqCount = 0
      . /* ASSIGN */

      IF FIRST-OF(ttNote.NoteTrid) THEN
      ASSIGN vTBgnCount = vTBgnCount + 1.

      IF ttNote.FirstUpdCtr EQ ? THEN /* it's a fake RL_SEINC note */
      ASSIGN vUsedSeqCount = vUsedSeqCount + ttNote.NoteCount.
      ELSE /* if it's not a fake RL_SEINC note: */
      ACCUMULATE
  /* BY NoteTrid */
        ttNote.NoteCount   (SUB-TOTAL BY ttNote.NoteTrid)
        ttNote.FirstUpdCtr (SUB-MIN   BY ttNote.NoteTrid)
        ttNote.LastUpdCtr  (SUB-MAX   BY ttNote.NoteTrid)
  /* BY NoteDbkey */
        ttNote.NoteCount   (SUB-TOTAL BY ttNote.NoteDbkey)
        ttNote.FirstUpdCtr (SUB-MIN   BY ttNote.NoteDbkey)
        ttNote.LastUpdCtr  (SUB-MAX   BY ttNote.NoteDbkey)
        ttNote.FirstTime   (SUB-MIN   BY ttNote.NoteDbkey)
        ttNote.LastTime    (SUB-MAX   BY ttNote.NoteDbkey)
      . /* ACCUMULATE */

/* Accumulate note's statistics per dbkey: */
      FOR FIRST ttNoteType NO-LOCK
          WHERE ttNoteType.NoteCode EQ ttNote.NoteCode
      TRANSACTION:
        FIND FIRST ttNoteList
             WHERE ttNoteList.ListId   EQ {&DbkeyList} /*Sum by dbkey's trans*/
               AND ttNoteList.NoteArea EQ ttNote.NoteArea
               AND ttNoteList.NoteType EQ ttNoteType.NoteType
        NO-ERROR.
        IF NOT AVAILABLE ttNoteList THEN
        CREATE ttNoteList.
        ASSIGN ttNoteList.ListId    = {&DbkeyList}
               ttNoteList.NoteArea  = ttNote.NoteArea
               ttNoteList.NoteType  = ttNoteType.NoteType
               ttNoteList.NoteCount = ttNoteList.NoteCount + ttNote.NoteCount
        . /* ASSIGN */
      END.

      IF LAST-OF(ttNote.NoteTrid) THEN
      FOR FIRST ttTran EXCLUSIVE
          WHERE ttTran.Trid EQ ttNote.NoteTrid
      TRANSACTION:
        ASSIGN
          vNoteCount   = ACCUM SUB-TOTAL BY ttNote.NoteTrid ttNote.NoteCount
          vFirstUpdCtr = ACCUM SUB-MIN   BY ttNote.NoteTrid ttNote.FirstUpdCtr
          vLastUpdCtr  = ACCUM SUB-MAX   BY ttNote.NoteTrid ttNote.LastUpdCtr
          vConcCount   = vLastUpdCtr - vFirstUpdCtr + 1 - vNoteCount
          ttTran.ConcCount = ttTran.ConcCount + vConcCount
        . /* ASSIGN */

        IF vNoteCount GT vMaxNoteCount THEN
        ASSIGN vMaxTrid      = ttNote.NoteTrid
               vMaxNoteCount = vNoteCount
        . /* ASSIGN */

        ACCUMULATE vConcCount (SUB-TOTAL BY ttNote.NoteDbkey).
      END.

/* ====== DbkeyReport ====================================================== */

      IF LAST-OF(ttNote.NoteDbkey) THEN
      DO:
        ASSIGN
          vNoteCount   = ACCUM SUB-TOTAL BY ttNote.NoteDbkey ttNote.NoteCount
          vFirstUpdCtr = ACCUM SUB-MIN   BY ttNote.NoteDbkey ttNote.FirstUpdCtr
          vLastUpdCtr  = ACCUM SUB-MAX   BY ttNote.NoteDbkey ttNote.LastUpdCtr
          vConcCount   = ACCUM SUB-TOTAL BY ttNote.NoteDbkey vConcCount
          vFirstTime   = ACCUM SUB-MIN   BY ttNote.NoteDbkey ttNote.FirstTime
          vLastTime    = ACCUM SUB-MAX   BY ttNote.NoteDbkey ttNote.LastTime
          vInterval    = INTERVAL(vLastTime, vFirstTime, "seconds":U)
          vDbkeyCount  = vDbkeyCount   + 1
          vDecDbkey    = ttNote.NoteDbkey
          vAveDbkey    = vAveDbkey + (vDecDbkey - vAveDbkey) / vDbkeyCount
          vDevDbkey    = vDevDbkey + (vDecDbkey * vDecDbkey - vDevDbkey)
                                   /  vDbkeyCount
          vBlockType   = "O"
          vNoteList    = "":U
        . /* ASSIGN */

/* WHEN expressions are evaluated at the beginning of the assignment: */
        ASSIGN vNoReuseCount = vNoReuseCount + 1 WHEN vNoteCount EQ 1.

/* UsedSeqCount is reliable if the number of RL_RMCR and fake RL_SEINC notes
   are equal for the same block:
*/      FOR FIRST ttNoteList NO-LOCK
            WHERE ttNoteList.ListId   EQ {&DbkeyList}
              AND ttNoteList.NoteArea EQ ttNote.NoteArea
              AND ttNoteList.NoteType EQ "RL_RMCR":U:
          ASSIGN vUsedSeqCount = 0 WHEN vUsedSeqCount NE ttNoteList.NoteCount.
        END.

  /* Copy the dbkey's NoteStat to the area's NoteStat: */
        FOR EACH ttNoteList NO-LOCK
           WHERE ttNoteList.ListId   EQ {&DbkeyList}
             AND ttNoteList.NoteArea EQ ttNote.NoteArea
        BREAK BY ttNoteList.NoteCount DESCENDING
              BY ttNoteList.NoteType
        TRANSACTION:

          FIND FIRST bfNewList
               WHERE bfNewList.ListId   EQ {&AreaList} /* Sum by dbkeys */
                 AND bfNewList.NoteArea EQ ttNoteList.NoteArea
                 AND bfNewList.NoteType EQ ttNoteList.NoteType
          NO-ERROR.
          IF NOT AVAILABLE bfNewList THEN
          CREATE bfNewList.
          ASSIGN bfNewList.ListId    = {&AreaList}
                 bfNewList.NoteArea  = ttNoteList.NoteArea
                 bfNewList.NoteType  = ttNoteList.NoteType
                 bfNewList.NoteCount = bfNewList.NoteCount
                                     + ttNoteList.NoteCount
                 vNoteList = vNoteList + ",":U + ttNoteList.NoteType + "*":U
                                       +  STRING(ttNoteList.NoteCount)
          . /* ASSIGN */

          CASE SUBSTRING(ttNoteList.NoteType, 1, 5):
            WHEN "RL_RM":U THEN ASSIGN vBlockType = "D".
            WHEN "RL_CX":U THEN ASSIGN vBlockType = "I".
            WHEN "RL_IX":U THEN ASSIGN vBlockType = "I".
            WHEN "RL_SE":U THEN ASSIGN vBlockType = "S" /*not for fake notes*/
                                  WHEN vBlockType EQ "O"
                                   AND ttNoteList.NoteArea EQ 6.
          END CASE.

          DELETE ttNoteList.
        END. /* FOR EACH ttNoteList */

/* ====== DbkeyReport ====================================================== */

        IF vNoteCount   GE vDbkeyUpdLimit /* only "reusable" dbkeys */
        OR vFirstUpdCtr LE 0 /* and newborn dbkeys */ THEN
        PUT STREAM DbkeyReport UNFORMATTED
                 /*AiScan    */ ttAiFile.AiFileName
          {&Sep} /*SeqNum    */ ttAiFile.AiSeqNum
          {&Sep} /*NewTime   */ Datetime2String(ttAiFile.AiNewTime)
          {&Sep} /*AiIntrvl  */ ttAiFile.AiInterval
          {&Sep} /*Area      */ ttNote.NoteArea
          {&Sep} /*AreaInfo  */ ttArea.AreaInfo
          {&Sep} /*Dbkey     */ ttNote.NoteDbkey
          {&Sep} /*T         */ vBlockType
          {&Sep} /*FirstUpdCt*/ vFirstUpdCtr
          {&Sep} /*LastUpdCtr*/ vLastUpdCtr
          {&Sep} /*FirstTime */ Datetime2String(vFirstTime)
          {&Sep} /*UpdIntrvl */ vInterval
          {&Sep} /*Updates   */ vNoteCount
          {&Sep} /*Upd/sec   */ ROUND(vNoteCount / MAX(vInterval, 1),
                                      vRoundDigits)
          {&Sep} /*ConcUpd   */ vConcCount
          {&Sep} /*Trans     */ vTBgnCount
          {&Sep} /*Trid      */ vMaxTrid
          {&Sep} /*NoteList  */ SUBSTRING(vNoteList, 2)
        SKIP. /* PUT STREAM DbkeyReport */

        ACCUMULATE ttNote.NoteDbkey (MIN MAX)
                   vNoteCount       (TOTAL)
                   vUsedSeqCount    (TOTAL)
        . /* ACCUMULATE by Area (for AreasReport) */

        PROCESS EVENTS.

      END. /* IF LAST-OF(ttNote.NoteDbkey) */

    END. /* FOR EACH ttNote */

/* ====== AreasReport ====================================================== */

    ASSIGN vNoteCount    = ACCUM TOTAL vNoteCount
           vUsedSeqCount = ACCUM TOTAL vUsedSeqCount
           vMinDbkey     = ACCUM MIN   ttNote.NoteDbkey
           vMaxDbkey     = ACCUM MAX   ttNote.NoteDbkey
           vDevDbkey     = SQRT(vDevDbkey - vAveDbkey * vAveDbkey)
           vExtended     = NoteCount({&AreaList}, ttNote.NoteArea, "RL_XTDB":U)
                           GT 0
           vNoteList     = "":U
           vTBgnCount    = 0
    . /* ASSIGN */

/* The query will create a temp index for sorting. It will be used only once.
   It's faster than to create a normal (persistent) index. */
    FOR EACH ttNote NO-LOCK
       WHERE ttNote.NoteArea EQ ttArea.AreaNumber
    BREAK BY ttNote.NoteArea
          BY ttNote.NoteTrid:
      IF FIRST-OF(ttNote.NoteTrid) THEN
      ASSIGN vTBgnCount = vTBgnCount + 1.
    END.

/* Copy Area's NoteStat to the AiFile's NoteStat: */
    FOR EACH ttNoteList NO-LOCK
       WHERE ttNoteList.ListId   EQ {&AreaList}
         AND ttNoteList.NoteArea EQ ttNote.NoteArea
    BREAK BY ttNoteList.NoteCount DESCENDING
          BY ttNoteList.NoteType
    TRANSACTION:

      ASSIGN vNoteList = vNoteList + ",":U + ttNoteList.NoteType + "*":U
                                   +  STRING(ttNoteList.NoteCount)
      . /* ASSIGN */

/* Ignore the fake RL_SEINC notes: */
      IF  ttNoteList.NoteType EQ "RL_SEINC":U
      AND ttNoteList.NoteArea NE 6 THEN
      NEXT.

      FIND FIRST bfNewList
           WHERE bfNewList.ListId   EQ {&AiFileList} /* Sum by areas and   */
             AND bfNewList.NoteArea EQ {&AiFileList} /* by "service" notes */
             AND bfNewList.NoteType EQ ttNoteList.NoteType
      NO-ERROR.
      IF NOT AVAILABLE bfNewList THEN
      CREATE bfNewList.
      ASSIGN bfNewList.ListId    = {&AiFileList}
             bfNewList.NoteArea  = {&AiFileList}
             bfNewList.NoteType  = ttNoteList.NoteType
             bfNewList.NoteCount = bfNewList.NoteCount + ttNoteList.NoteCount
      . /* ASSIGN */

/* ====== NotesReport ====================================================== */

      PUT STREAM NotesReport UNFORMATTED
               /*AiScan  */ ttAiFile.AiFileName
        {&Sep} /*AiNum   */ ttAiFile.AiSeqNum
        {&Sep} /*NewTime */ Datetime2String(ttAiFile.AiNewTime)
        {&Sep} /*Interval*/ ttAiFile.AiInterval
        {&Sep} /*Area    */ ttNoteList.NoteArea
        {&Sep} /*AreaInfo*/ IF AVAILABLE ttArea THEN ttArea.AreaInfo ELSE "":U
        {&Sep} /*Type    */ ttNoteList.NoteType
        {&Sep} /*Count   */ ttNoteList.NoteCount
      SKIP. /* PUT STREAM NotesReport */

      DELETE ttNoteList. /* from {&AreaList} */

    END. /* FOR EACH ttNoteList EQ {&AreaList} */

/* ====== AreasReport ====================================================== */

    PUT STREAM AreasReport UNFORMATTED
             /*AiScan   */ ttAiFile.AiFileName
      {&Sep} /*AiNum    */ ttAiFile.AiSeqNum
      {&Sep} /*NewTime  */ Datetime2String(ttAiFile.AiNewTime)
      {&Sep} /*Intrvl   */ ttAiFile.AiInterval
      {&Sep} /*Area     */ ttNote.NoteArea
      {&Sep} /*AreaInfo */ IF AVAILABLE ttArea THEN ttArea.AreaInfo ELSE "":U
      {&Sep} /*MinDbkey */ vMinDbkey
      {&Sep} /*MaxDbkey */ vMaxDbkey
      {&Sep} /*X        */ IF vExtended THEN "X":U ELSE "":U
      {&Sep} /*AveDbkey */ ROUND(vAveDbkey, 0)
      {&Sep} /*DevDbkey */ ROUND(vDevDbkey, 0)
      {&Sep} /*%DevDbkey*/ Percent(vDevDbkey, vMaxDbkey)
      {&Sep} /*Updates  */ vNoteCount  /* only updates */
      {&Sep} /*%Updates */ Percent(vNoteCount, ttAiFile.UpdtCount)
      {&Sep} /*Dbkeys   */ vDbkeyCount
      {&Sep} /*NoReuse  */ vNoReuseCount
      {&Sep} /*%NoReuse */ Percent(vNoReuseCount, vDbkeyCount)
      {&Sep} /*AveReuse */ IF vDbkeyCount LE vNoReuseCount THEN 0.0 ELSE
                           ROUND((vNoteCount  - vNoReuseCount)
                               / (vDbkeyCount - vNoReuseCount), vRoundDigits)
      {&Sep} /*Trans    */ vTBgnCount
      {&Sep} /*%Trans   */ Percent(vTBgnCount, ttAiFile.TranCount)
      {&Sep} /*UsedSeqs */ vUsedSeqCount
      {&Sep} /*NoteList */  SUBSTRING(vNoteList, 2)
    SKIP. /* PUT STREAM AreasReport */

/* For AreasReport: */
    ACCUMULATE vDbkeyCount   (TOTAL)
               vNoReuseCount (TOTAL)
               vUsedSeqCount (TOTAL)
    . /* ACCUMULATE */

  END. /* FOR EACH ttArea */

/* ====== AreasReport: Total by AI file ==================================== */

  ASSIGN vDbkeyCount   = ACCUM TOTAL vDbkeyCount
         vNoReuseCount = ACCUM TOTAL vNoReuseCount
         vUsedSeqCount = ACCUM TOTAL vUsedSeqCount
         vNoteList     = "":U
  . /* ASSIGN */

/* Accumulate statistics of the "system" notes (the sum by Trids): */
  FOR EACH ttNote NO-LOCK
     WHERE ttNote.NoteArea EQ 0,

     FIRST ttNoteType NO-LOCK
     WHERE ttNoteType.NoteCode EQ ttNote.NoteCode
  TRANSACTION:
    FIND FIRST ttNoteList
         WHERE ttNoteList.ListId   EQ {&AiFileList}
           AND ttNoteList.NoteArea EQ {&AiFileList}
           AND ttNoteList.NoteType EQ ttNoteType.NoteType
    NO-ERROR.
    IF NOT AVAILABLE ttNoteList THEN
    CREATE ttNoteList.
    ASSIGN ttNoteList.ListId    = {&AiFileList}
           ttNoteList.NoteArea  = {&AiFileList}
           ttNoteList.NoteType  = ttNoteType.NoteType
           ttNoteList.NoteCount = ttNoteList.NoteCount + ttNote.NoteCount
    . /* ASSIGN */
  END.

  FOR EACH ttNoteList NO-LOCK
     WHERE ttNoteList.ListId   EQ {&AiFileList}
       AND ttNoteList.NoteArea EQ {&AiFileList}
  BREAK BY ttNoteList.NoteCount DESCENDING
        BY ttNoteList.NoteType:

    ASSIGN vNoteList = vNoteList + ",":U + ttNoteList.NoteType + "*":U
                                 +  STRING(ttNoteList.NoteCount)
    . /* ASSIGN */

/* ====== NotesReport: Total by AI file ==================================== */

    PUT STREAM NotesReport UNFORMATTED
             /*AiScan  */ ttAiFile.AiFileName
      {&Sep} /*AiNum   */ ttAiFile.AiSeqNum
      {&Sep} /*NewTime */ Datetime2String(ttAiFile.AiNewTime)
      {&Sep} /*Interval*/ ttAiFile.AiInterval
      {&Sep} /*Area    */ "Total"
      {&Sep} /*AreaInfo*/ "Total"
      {&Sep} /*Type    */ ttNoteList.NoteType
      {&Sep} /*Count   */ ttNoteList.NoteCount
    SKIP. /* PUT STREAM NotesReport */

    DELETE ttNoteList. /* from {&AiFileList} */

  END. /* FOR EACH ttNoteList */

  ASSIGN vNoteList = SUBSTRING(vNoteList, 2).

/* ====== AreasReport: Total by AI file ==================================== */

  PUT STREAM AreasReport UNFORMATTED
           /*AiScan   */ ttAiFile.AiFileName
    {&Sep} /*AiNum    */ ttAiFile.AiSeqNum
    {&Sep} /*NewTime  */ Datetime2String(ttAiFile.AiNewTime)
    {&Sep} /*Intrvl   */ ttAiFile.AiInterval
    {&Sep} /*Area     */ "Total"
    {&Sep} /*AreaInfo */ "Total"
    {&Sep} /*MinDbkey */ "n/a"
    {&Sep} /*MaxDbkey */ "n/a"
    {&Sep} /*X        */ ""
    {&Sep} /*AveDbkey */ "n/a"
    {&Sep} /*DevDbkey */ "n/a"
    {&Sep} /*%DevDbkey*/ "n/a"
    {&Sep} /*Updates  */ ttAiFile.UpdtCount
    {&Sep} /*%Updates */ Percent(ttAiFile.UpdtCount, ttAiFile.NoteCount)
    {&Sep} /*Dbkeys   */ vDbkeyCount
    {&Sep} /*NoReuse  */ vNoReuseCount
    {&Sep} /*%NoReuse */ Percent(vNoReuseCount, vDbkeyCount)
    {&Sep} /*AveReuse */ IF vDbkeyCount LE vNoReuseCount THEN 0.0 ELSE
                         ROUND((ttAiFile.UpdtCount - vNoReuseCount)
                             / (vDbkeyCount - vNoReuseCount), vRoundDigits)
    {&Sep} /*Trans    */ ttAiFile.TranCount
    {&Sep} /*%Trans   */ "100%"
    {&Sep} /*UsedSeqs */ vUsedSeqCount
    {&Sep} /*NoteList */ vNoteList
  SKIP. /* PUT STREAM AreasReport */

/* ====== TransReport ====================================================== */

  IF SEEK(TransReport) EQ 0 THEN /* ---------------------------------------- */
  PUT STREAM TransReport UNFORMATTED
           "Trid"       /* Transaction ID                                    */
    {&Sep} "Userid"     /* Userid of the user that started the transaction   */
    {&Sep} "AiNum"      /* AI sequence number (Seqno)                        */
    {&Sep} "BegTime"    /* Time when the transaction became active (RL_TBGN) */
    {&Sep} "EndTime"    /* Time when the transaction was committed (RL_TEND) */
    {&Sep} "Len"        /* Transaction duration in seconds (=EndTime-BgnTime)*/
    {&Sep} "Idle"       /* Total idle time during transaction                */
    {&Sep} "%Idle"      /* Percentage of the transaction's duration          */
    {&Sep} "MaxGap"     /* Maximum continuous interval with no tran activity */
    {&Sep} "GapCount"   /* The number of idle intervals during transaction   */
    {&Sep} "SeqIncr"    /* Number of sequence increments during transaction  */
    {&Sep} "SeqUsed"    /* Number of the "used" sequence increments          */
    {&Sep} "AveSeqPath" /* Average "sequence path" (from RL_SEINC to RL_RMCR)*/
    {&Sep} "MaxSeqPath" /* Maximum length of "sequence path"                 */
    {&Sep} "Updates"    /* Number of "update" notes per transaction          */
    {&Sep} "%Updates"   /* Percentage of the number of notes per transaction */
    {&Sep} "Notes"      /* Number of notes per transaction                   */
    {&Sep} "SubTrans"   /* Number of sub-transactions                        */
    {&Sep} "Notes/Sub"  /* Average number of notes per sub-transaction       */
    {&Sep} "NoteList"   /* Sorted list of the notes per transaction          */
  SKIP. /* PUT STREAM TransReport */

  ASSIGN vUserCount = 0.
  FOR EACH ttTran NO-LOCK
     WHERE ttTran.Trid GT 0
        BY ttTran.Trid:

    DO TRANSACTION:
      FIND FIRST ttUser
           WHERE ttUser.AiSeqNum EQ ttAiFile.AiSeqNum
             AND ttUser.UserName EQ ttTran.UserName
      NO-ERROR.
      IF NOT AVAILABLE ttUser THEN
      CREATE ttUser.

      IF NEW ttUser THEN
      ASSIGN vUserCount       = vUserCount + 1
             ttUser.AiSeqNum  = ttAiFile.AiSeqNum
             ttUser.UserName  = ttTran.UserName
      . /* ASSIGN  */

      ASSIGN vFirstTime = IF  ttTran.BgnTime EQ ? THEN ttAiFile.AiNewTime ELSE
                          MAX(ttTran.BgnTime,          ttAiFile.AiNewTime)
      . /* ASSIGN  */

/* If transaction did not end in the current AI file: */
      IF ttTran.EndTime EQ ? THEN
      ASSIGN  vLastTime = ttAiFile.LastTime
              vTranTime = INTERVAL(vLastTime, vFirstTime, "seconds":U)
              vIdleTime = INTERVAL(vLastTime, ttTran.LastTime, "seconds":U)
        ttTran.IdleTime = ttTran.IdleTime + vIdleTime
        ttTran.MaxGTime = MAX(ttTran.MaxGTime, vIdleTime)
        ttTran.GapCount = ttTran.GapCount + 1
        ttTran.LastTime = vLastTime
      . /* ASSIGN  */
      ELSE
/* If transaction was committed in the current AI file: */
      ASSIGN vLastTime = ttTran.EndTime
             vTranTime = INTERVAL(vLastTime, vFirstTime, "seconds":U)

             ttUser.TranCount    = ttUser.TranCount + 1

             ttUser.MaxTranTime  = MAX(ttUser.MaxTranTime, vTranTime)
             ttUser.AveTranTime  = ttUser.AveTranTime
                   + (vTranTime  - ttUser.AveTranTime) / ttUser.TranCount

             ttUser.MaxNoteCount = MAX(ttUser.MaxNoteCount, ttTran.NoteCount)
             ttUser.AveNoteCount = ttUser.AveNoteCount +
               (ttTran.NoteCount - ttUser.AveNoteCount) / ttUser.TranCount
      . /* ASSIGN  */

/* vTranTime = the part of transaction time inside the current AI file: */
      ASSIGN ttUser.TranTime  = ttUser.TranTime + vTranTime

             vIdleTime           = ttTran.IdleTime - ttTran.PrevIdleTime
             ttTran.PrevIdleTime = ttTran.IdleTime
             ttUser.IdleTime     = ttUser.IdleTime + vIdleTime

             vNoteCount       = ttTran.NoteCount - ttTran.PrevNoteCount
             ttTran.PrevNoteCount = ttTran.NoteCount
             ttUser.NoteCount = ttUser.NoteCount + vNoteCount

             ttUser.FirstTime = IF NEW ttUser THEN vFirstTime ELSE
                             MIN(ttUser.FirstTime, vFirstTime)
             ttUser.LastTime  = IF NEW ttUser THEN vLastTime ELSE
                             MAX(ttUser.LastTime,  vLastTime)

/* vTranTime = full transaction duration (committed or not): */
             vTranTime = INTERVAL(ttTran.EndTime,
                               IF ttTran.BgnTime EQ ? THEN vAiFirstTime ELSE
                                  ttTran.BgnTime, "seconds":U)
      . /* ASSIGN  */

    END. /* TRANSACTION */

/* If transaction is committed but but too small for the report then:
*/  IF   vTranTime       NE ?
    AND  vTranTime       LT vTranTimeLimit
    AND ttTran.NoteCount LT vTranNoteLimit THEN
    DO TRANSACTION:
      DELETE ttTran.
      NEXT. /* ttTran */
    END.

    FOR EACH ttNote
       WHERE ttNote.NoteTrid EQ ttTran.Trid
         AND ttNote.FirstUpdCtr NE ?: /* ignore the fake notes */

/* Accumulate note's statistics per transaction: */
      FOR FIRST ttNoteType NO-LOCK
          WHERE ttNoteType.NoteCode EQ ttNote.NoteCode
      TRANSACTION:
        FIND FIRST ttNoteList
             WHERE ttNoteList.ListId   EQ ttTran.Trid
               AND ttNoteList.NoteArea EQ ttNote.NoteArea
               AND ttNoteList.NoteType EQ ttNoteType.NoteType
        NO-ERROR.
        IF NOT AVAILABLE ttNoteList THEN
        CREATE ttNoteList.
        ASSIGN ttNoteList.ListId    = ttTran.Trid
               ttNoteList.NoteArea  = ttNote.NoteArea
               ttNoteList.NoteType  = ttNoteType.NoteType
               ttNoteList.NoteCount = ttNoteList.NoteCount + ttNote.NoteCount
        . /* ASSIGN */
      END. /* FOR FIRST ttNoteType */
    END. /* FOR EACH ttNote of ttTran */

/* Postpone the reporting of the "incomplete" transactions: */
    IF ttTran.EndTime EQ ? THEN
    NEXT.

    ASSIGN vSubTrans = 0.
    FOR EACH ttNoteList NO-LOCK
       WHERE ttNoteList.ListId EQ ttTran.Trid
         AND ttNoteList.NoteType BEGINS "RL_RM":U:
      ASSIGN vSubTrans = MAX(vSubTrans, ttNoteList.NoteCount).
    END.

    ASSIGN vTranSeqs  =     NoteCount(ttTran.Trid, 6 ,"RL_SEINC":U)
           vSubTrans  = MAX(NoteCount(ttTran.Trid, 0, "RL_TMSAVE":U),
                            NoteCount(ttTran.Trid, 0, "RL_LOGOP_START":U),
                            NoteCount(ttTran.Trid, 0, "RL_LOGOP_END":U),
                            vTranSeqs, 1) WHEN vSubTrans EQ 0
           vNoteFact  = vSubTrans * vNoteListThold
           vNoteThold = IF vNoteFact GT 0.5 THEN ROUND(vNoteFact, 0) ELSE ?
           vNoteList1 = "":U
           vNoteList2 = "":U
    . /* ASSIGN */

    FOR EACH ttNoteList NO-LOCK
       WHERE ttNoteList.ListId EQ ttTran.Trid
          BY ttNoteList.NoteCount DESCENDING
          BY ttNoteList.NoteArea
          BY ttNoteList.NoteType:

      IF ttNoteList.NoteCount GT vNoteThold THEN
      ASSIGN
        vNoteFact = ROUND(ttNoteList.NoteCount / vSubTrans, vNoteListDigit)
        vNoteList1 = vNoteList1 + ",":U + ttNoteList.NoteType
                   + (IF ttNoteList.NoteArea EQ 0 THEN "":U
                      ELSE ":":U + STRING(ttNoteList.NoteArea)
                     ) +
                     (IF vNoteFact EQ 1.0 THEN "":U
                      ELSE "*":U + STRING(vNoteFact))
      . /* ASSIGN */
      ELSE
      ASSIGN
        vNoteCount = ttNoteList.NoteCount
        vNoteList2 = vNoteList2 + ",":U + ttNoteList.NoteType
                   + (IF ttNoteList.NoteArea EQ 0 THEN "":U
                      ELSE ":":U + STRING(ttNoteList.NoteArea)
                     ) +
                     (IF vNoteCount EQ 1 THEN "":U
                      ELSE "*":U + STRING(vNoteCount))
      . /* ASSIGN */

      DELETE ttNoteList. /* of ttTran.Trid */

    END. /* FOR EACH ttNoteList */

    ASSIGN vNoteList1 = SUBSTRING(vNoteList1, 2)
           vNoteList2 = SUBSTRING(vNoteList2, 2)
    . /* ASSIGN */

/* ====== TransReport ====================================================== */

    PUT STREAM TransReport UNFORMATTED
             /*Trid      */ ttTran.Trid
      {&Sep} /*Userid    */ ttTran.UserName
      {&Sep} /*AiNum     */ ttTran.AiSeqNum
      {&Sep} /*BegTime   */ DateTime2String(ttTran.BgnTime)
      {&Sep} /*EndTime   */ DateTime2String(ttTran.EndTime)
      {&Sep} /*Len       */ vTranTime
      {&Sep} /*Idle      */ ttTran.IdleTime
      {&Sep} /*%Idle     */ Percent(ttTran.IdleTime, vTranTime)
      {&Sep} /*MaxGap    */ ttTran.MaxGTime
      {&Sep} /*GapCount  */ ttTran.GapCount
      {&Sep} /*SeqIncr   */ vTranSeqs
      {&Sep} /*SeqUsed   */ ttTran.UsedSeqCount
      {&Sep} /*AveSeqPath*/ ROUND(ttTran.AveSeqPath, vRoundDigits)
      {&Sep} /*MaxSeqPath*/ ttTran.MaxSeqPath
      {&Sep} /*Updates   */ ttTran.UpdtCount
      {&Sep} /*%Updates  */ Percent(ttTran.UpdtCount, ttTran.NoteCount)
      {&Sep} /*Notes     */ ttTran.NoteCount
      {&Sep} /*SubTrans  */ vSubTrans
      {&Sep} /*Notes/Sub */ ROUND(ttTran.NoteCount / vSubTrans, vRoundDigits)
      {&Sep} /*NoteList  */ IF vNoteList1 EQ "":U THEN vNoteList2 ELSE
                            "(":U + vNoteList1 + ")*":U + STRING(vSubTrans)
                            + MIN(",":U, vNoteList2) + vNoteList2
    SKIP. /* PUT STREAM TransReport */

/* For FilesReport: */
/* Expecting: vTranTime NE ? */
    ACCUMULATE vTranTime        (AVERAGE MAXIMUM)
               ttTran.NoteCount (AVERAGE MAXIMUM)
               vSubTrans        (AVERAGE MAXIMUM)
    . /* ACCUMULATE */

    DELETE ttTran.

    PROCESS EVENTS.

  END. /* FOR EACH ttTran */

/* ====== UsersReport ====================================================== */

  IF SEEK(UsersReport) EQ 0 THEN /* ---------------------------------------- */
  PUT STREAM UsersReport UNFORMATTED
           "AiScan"    /* Name of AI scan file                               */
    {&Sep} "AiNum"     /* AI sequence number (Seqno)                         */
    {&Sep} "AiIntr"    /* AI file interval                                   */
    {&Sep} "Userid"    /* Userid of the user that started the transactions   */
    {&Sep} "FirstTime" /* First time of user's activity inside AI file       */
    {&Sep} "LastTime"  /* Last time of user's  activity inside AI file       */
    {&Sep} "UserTime"  /* Interval of user's activity (=LastTime-FirstTime)  */
    {&Sep} "%UserTime" /* Percentage of the AI interval                      */
    {&Sep} "TranTime"  /* Total duration of user's transactions              */
    {&Sep} "%TranTime" /* Percentage of AI interval (can be great than 100%) */
    {&Sep} "IdleTime"  /* Total idle times of user's transactions            */
    {&Sep} "%IdleTime" /* Percentage of TranTime                             */
    {&Sep} "Trans"     /* Number of commits inside the current AI file       */
    {&Sep} "%Trans"    /* Percentage of the total number of commits in AI    */
    {&Sep} "Notes"     /* Total number of notes in the user's transactions   */
    {&Sep} "%Notes"    /* Percentage of the total note count per all users   */
    {&Sep} "AveTime"   /* Average duration of transactions committed in AI   */
    {&Sep} "MaxTime"   /* Maximum duration of transactions committed in AI   */
    {&Sep} "AveNotes"  /* Average note count per committed transaction       */
    {&Sep} "MaxNotes"  /* Maximum note count per committed transaction       */
  SKIP. /* PUT STREAM UsersReport */

  ASSIGN vTopNoteCount = 0.
  FOR EACH ttUser NO-LOCK
     WHERE ttUser.AiSeqNum EQ ttAiFile.AiSeqNum
        BY ttUser.AiSeqNum
        BY ttUser.UserName
  TRANSACTION:

    ASSIGN vInterval = INTERVAL(ttUser.LastTime, ttUser.FirstTime, "seconds":U).

/* ====== UsersReport ====================================================== */

    PUT STREAM UsersReport UNFORMATTED
             /*AiScan   */ ttAiFile.AiFileName
      {&Sep} /*AiNum    */ ttAiFile.AiSeqNum
      {&Sep} /*AiIntr   */ ttAiFile.AiInterval
      {&Sep} /*Userid   */ ttUser.UserName
      {&Sep} /*FirstTime*/ DateTime2String(ttUser.FirstTime)
      {&Sep} /*LastTime */ DateTime2String(ttUser.LastTime)
      {&Sep} /*UserTime */ vInterval
      {&Sep} /*%UserTime*/ Percent(vInterval, ttAiFile.AiInterval)
      {&Sep} /*TranTime */ ttUser.TranTime
      {&Sep} /*%TranTime*/ Percent(ttUser.TranTime, ttAiFile.AiInterval)
      {&Sep} /*IdleTime */ ttUser.IdleTime
      {&Sep} /*%IdleTime*/ Percent(ttUser.IdleTime, ttUser.TranTime)
      {&Sep} /*Trans    */ ttUser.TranCount
      {&Sep} /*%Trans   */ Percent(ttUser.TranCount, ttAiFile.TranCount)
      {&Sep} /*Notes    */ ttUser.NoteCount
      {&Sep} /*%Notes   */ Percent(ttUser.NoteCount, ttAiFile.NoteCount)
      {&Sep} /*AveTime  */ ROUND(ttUser.AveTranTime, vRoundDigits)
      {&Sep} /*MaxTime  */ ttUser.MaxTranTime
      {&Sep} /*AveNotes */ ROUND(ttUser.AveNoteCount, vRoundDigits)
      {&Sep} /*MaxNotes */ ttUser.MaxNoteCount
    SKIP. /* PUT STREAM UsersReport */

/* For FilesReport: */
    IF ttUser.NoteCount GT vTopNoteCount THEN
    ASSIGN vTopUserName  = ttUser.UserName
           vTopNoteCount = ttUser.NoteCount
    . /* ASSIGN */

/* Accumulate user's activity stats by AiSeqNum: */
    FIND FIRST bfUser
         WHERE bfUser.AiSeqNum EQ {&TotalStat}
           AND bfUser.UserName EQ ttUser.UserName
    NO-ERROR.
    IF NOT AVAILABLE bfUser THEN
    CREATE bfUser.
    IF NEW bfUser THEN
    ASSIGN bfUser.AiSeqNum     = {&TotalStat}
           bfUser.UserName     = ttUser.UserName
           bfUser.TranCount    = ttUser.TranCount
           bfUser.FirstTime    = ttUser.FirstTime
           bfUser.LastTime     = ttUser.LastTime
           bfUser.TranTime     = ttUser.TranTime
           bfUser.IdleTime     = ttUser.IdleTime
           bfUser.NoteCount    = ttUser.NoteCount
           bfUser.AveTranTime  = ttUser.AveTranTime
           bfUser.MaxTranTime  = ttUser.MaxTranTime
           bfUser.AveNoteCount = ttUser.AveNoteCount
           bfUser.MaxNoteCount = ttUser.MaxNoteCount
    . /* ASSIGN */
    ELSE
    ASSIGN bfUser.TranCount    = bfUser.TranCount    + ttUser.TranCount
           bfUser.FirstTime    = MIN(bfUser.FirstTime, ttUser.FirstTime)
           bfUser.LastTime     = MAX(bfUser.LastTime,  ttUser.LastTime)
           bfUser.TranTime     = bfUser.TranTime  + ttUser.TranTime
           bfUser.IdleTime     = bfUser.IdleTime  + ttUser.IdleTime
           bfUser.NoteCount    = bfUser.NoteCount + ttUser.NoteCount
           vNoteFact           = bfUser.NoteCount / ttUser.NoteCount
           bfUser.AveTranTime  = bfUser.AveTranTime +
          (ttUser.AveTranTime  - bfUser.AveTranTime) / vNoteFact
           bfUser.MaxTranTime  = MAX(bfUser.MaxTranTime, ttUser.MaxTranTime)
           bfUser.AveNoteCount = bfUser.AveNoteCount +
          (ttUser.AveNoteCount - bfUser.AveNoteCount) / vNoteFact
           bfUser.MaxNoteCount = MAX(bfUser.MaxNoteCount, ttUser.MaxNoteCount)
    . /* ASSIGN */

    DELETE ttUser.

  END. /* FOR EACH ttUser  */

/* === FilesReport ========================================================= */

  FOR EACH ttCkpt NO-LOCK
     WHERE ttCkpt.AiSeqNum EQ ttAiFile.AiSeqNum:
    ACCUMULATE "Ckpts" (COUNT).
  END.
  ASSIGN vTranAlloc = ttAiFile.MaxTrid - ttAiFile.MinTrid - ttAiFile.TranCount.

  IF SEEK(FilesReport) EQ 0 THEN /* ---------------------------------------- */
  PUT STREAM FilesReport UNFORMATTED
           "AiScan"      /* Name of AI scan file                             */
    {&Sep} "Lines"       /* Line count in AI scan                            */
    {&Sep} "AiNum"       /* AI sequence number (Seqno)                       */
    {&Sep} "BeginTime"   /* Time of rfutil aimage begin                      */
    {&Sep} "NewTime"     /* Time of rfutil aimage new                        */
    {&Sep} "OpenTime"    /* Time of db restart (if/when AI was current)      */
    {&Sep} "LastTime"    /* Time of last transaction note in AI scan         */
    {&Sep} "Interval"    /* AI file interval                                 */
    {&Sep} "Notes"       /* Note count in AI scan                            */
    {&Sep} "Notes/sec"   /* Notes per sec                                    */
    {&Sep} "Updates"     /* "Update" note count                              */
    {&Sep} "%Updates"    /* Percent to the note count                        */
    {&Sep} "Dbkeys"      /* Number of updated db blocks                      */
    {&Sep} "NoReuse"     /* Number of blocks updated only one time           */
    {&Sep} "%NoReuse"    /* Percentage of the note count                     */
    {&Sep} "AveReuse"    /* Number of updates for blocks with more one update*/
/* Checkpoints: */
    {&Sep} "Ckpts"       /* Number of checkpoints during current AI interval */
/* Time Gaps: */
    {&Sep} "AveGap"      /* Average time gap between transaction notes       */
    {&Sep} "MaxGap"      /* Maximum time gap between transaction notes       */
    {&Sep} "%GapTime"    /* Sum of time gaps as percentage of the AI interval*/
/* Transactions: */
    {&Sep} "MinTrid"     /* Minimal transaction ID found in the current AI   */
    {&Sep} "MaxTrid"     /* Maximum transaction ID found in the current AI   */
    {&Sep} "TranStart"   /* Number of transactions started in AI file        */
    {&Sep} "Tran/sec"    /* Number of transactions per sec                   */
    {&Sep} "TranAlloc"   /* Number of allocated but not active transactions  */
    {&Sep} "%TranAlloc"  /* Percentage of the Trid range (= MaxTrid-MinTrid) */
    {&Sep} "TranAtBgn"   /* Number of the in-flight transactions (at begin)  */
    {&Sep} "TranAtEnd"   /* Number of transactions still active at the end   */
    {&Sep} "OldAtEnd"    /* Number of old transactions still active at end   */
    {&Sep} "AveActTran"  /* Average number of simultaneous transactions      */
    {&Sep} "MaxActTran"  /* Maximum number of simultaneous transactions      */
    {&Sep} "MaxActTranAt" /*Time when maximum was reached                    */
    {&Sep} "Users"       /* Number of logins that created transactions       */
    {&Sep} "TopUser"     /* Userid who created the most number of notes      */
    {&Sep} "%TopNotes"   /* Note count of TopUser as percentage of the total */
    {&Sep} "AveTranTime" /* Average duration of committed transactions       */
    {&Sep} "MaxTranTime" /* Maximum duration of committed transactions       */
    {&Sep} "AveTranSize" /* Average note count per committed transactions    */
    {&Sep} "MaxTranSize" /* Maximum note count per committed transactions    */
    {&Sep} "AveSubTrans" /* Average number of sub-transactions               */
    {&Sep} "MaxSubTrans" /* Maximum number of sub-transactions               */
/* Note list per AI file: */
    {&Sep} "NoteList"    /* Sorted list of notes per AI file                 */
  SKIP. /* PUT STREAM FilesReport */

  PUT STREAM FilesReport UNFORMATTED
           /*AiScan   */ ttAiFile.AiFileName
    {&Sep} /*Lines    */ ttAiFile.LineCount
    {&Sep} /*AiNum    */ ttAiFile.AiSeqNum
    {&Sep} /*BeginTime*/ DateTime2String(ttAiFile.AiBegTime)
    {&Sep} /*NewTime  */ DateTime2String(ttAiFile.AiNewTime)
    {&Sep} /*OpenTime */ IF ttAiFile.AiOpenTime EQ ttAiFile.AiNewTime THEN "":U
                    ELSE DateTime2String(ttAiFile.AiOpenTime)
    {&Sep} /*LastTime */ DateTime2String(ttAiFile.LastTime)
    {&Sep} /*Interval */ ttAiFile.AiInterval
    {&Sep} /*Notes    */ ttAiFile.NoteCount
    {&Sep} /*Notes/sec*/ ROUND(ttAiFile.NoteCount / ttAiFile.AiInterval,
                               vRoundDigits)
    {&Sep} /*Updates  */ ttAiFile.UpdtCount
    {&Sep} /*%Updates */ Percent(ttAiFile.UpdtCount, ttAiFile.NoteCount)
    {&Sep} /*Dbkeys   */ vDbkeyCount

    {&Sep} /*NoReuse  */ vNoReuseCount
    {&Sep} /*%NoReuse */ Percent(vNoReuseCount, vDbkeyCount)
    {&Sep} /*AveReuse */ IF vDbkeyCount LE vNoReuseCount THEN 0.0 ELSE
                         ROUND((ttAiFile.UpdtCount - vNoReuseCount)
                             / (vDbkeyCount - vNoReuseCount), vRoundDigits)
/* Checkpoints: */
    {&Sep} /*Ckpts    */ ACCUM COUNT "Ckpts"
/* Time gaps: */
    {&Sep} /*AveGap   */ ROUND(ttAiFile.AveTimeGap, vRoundDigits)
    {&Sep} /*MaxGap   */ ttAiFile.MaxTimeGap
    {&Sep} /*%GapTime */ Percent(ttAiFile.TotTimeGap, ttAiFile.AiInterval)
/* Transactions: */
    {&Sep} /*MinTrid    */ ttAiFile.MinTrid
    {&Sep} /*MaxTrid    */ ttAiFile.MaxTrid
    {&Sep} /*TranStart  */ ttAiFile.TranCount
    {&Sep} /*Tran/sec   */ ROUND(ttAiFile.TranCount / ttAiFile.AiInterval,
                                 vRoundDigits)
    {&Sep} /*TranAlloc  */ vTranAlloc
    {&Sep} /*%TranAlloc */ Percent(vTranAlloc,
                                   ttAiFile.MaxTrid - ttAiFile.MinTrid)
    {&Sep} /*TranAtBgn  */ ttAiFile.TranAtBgn
    {&Sep} /*TranAtEnd  */ ttAiFile.TranAtEnd
    {&Sep} /*OldAtEnd   */ ttAiFile.OldAtEnd
    {&Sep} /*AveActTran */ ROUND(ttAiFile.AveActTran, vRoundDigits)
    {&Sep} /*MaxActTran */ ttAiFile.MaxActTran
    {&Sep} /*MaxActTrnAt*/ DateTime2String(ttAiFile.MaxActTime)
    {&Sep} /*Users      */ vUserCount
    {&Sep} /*TopUser    */ vTopUserName
    {&Sep} /*%TopNotes  */ Percent(vTopNoteCount, ttAiFile.NoteCount)
    {&Sep} /*AveTranTime*/ ROUND(ACCUM AVERAGE vTranTime, vRoundDigits)
    {&Sep} /*MaxTranTime*/       ACCUM MAXIMUM vTranTime
    {&Sep} /*AveTranSize*/ ROUND(ACCUM AVERAGE ttTran.NoteCount, vRoundDigits)
    {&Sep} /*MaxTranSize*/       ACCUM MAXIMUM ttTran.NoteCount
    {&Sep} /*AveSubTrans*/ ROUND(ACCUM AVERAGE vSubTrans, vRoundDigits)
    {&Sep} /*MaxSubTrans*/       ACCUM MAXIMUM vSubTrans
/* Note list per AI file*/
    {&Sep} /*Notes      */ vNoteList
  SKIP. /* PUT STREAM FilesReport */

/* Flush the output buffers (you can read the files while scanning next ai):*/
  PUT STREAM FilesReport  CONTROL NULL(0).
  PUT STREAM AreasReport  CONTROL NULL(0).
  PUT STREAM NotesReport  CONTROL NULL(0).
  PUT STREAM DbkeyReport  CONTROL NULL(0).
  PUT STREAM TransReport  CONTROL NULL(0).
  PUT STREAM PerSecReport CONTROL NULL(0).

  EMPTY TEMP-TABLE ttNote.

END PROCEDURE. /* MainReports */

/* ------------------------------------------------------------------------- */

PROCEDURE RestReports:

  DEFINE VARIABLE vInterval    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vAiInterval  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTranTime    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTBgnCount   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNoteCount   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNoteList    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vNoteList1   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vNoteList2   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vSubTrans    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vTranSeqs    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNoteThold   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vNoteFact    AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE vPrevCkpTime AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vAiFirstTime AS DATETIME  NO-UNDO.
  DEFINE VARIABLE vAiLastTime  AS DATETIME  NO-UNDO.

  FOR EACH ttAiFile NO-LOCK
        BY ttAiFile.AiSeqNum:
    ASSIGN vAiFirstTime = ttAiFile.AiNewTime.
    LEAVE.
  END.

  FOR EACH ttAiFile NO-LOCK
        BY ttAiFile.AiSeqNum DESCENDING:
    ASSIGN vAiLastTime = ttAiFile.LastTime.
    LEAVE.
  END.


/* ====== TransReport ====================================================== */

/* Only transactions with ttTran.BgnTime eq ? or ttTran.EndTime eq ?.
   Their notes are already accumulated in ttNoteList records.
*/
  FOR EACH ttTran NO-LOCK
     WHERE ttTran.Trid GT 0
        BY ttTran.Trid:

    ASSIGN vSubTrans = 0.
    FOR EACH ttNoteList NO-LOCK
       WHERE ttNoteList.ListId EQ ttTran.Trid
         AND ttNoteList.NoteType BEGINS "RL_RM":U:
      ASSIGN vSubTrans = MAX(vSubTrans, ttNoteList.NoteCount).
    END.

    ASSIGN vTranSeqs  =     NoteCount(ttTran.Trid, 6 ,"RL_SEINC":U)
           vSubTrans  = MAX(NoteCount(ttTran.Trid, 0, "RL_TMSAVE":U),
                            NoteCount(ttTran.Trid, 0, "RL_LOGOP_START":U),
                            NoteCount(ttTran.Trid, 0, "RL_LOGOP_END":U),
                            vTranSeqs, 1) WHEN vSubTrans EQ 0
           vNoteFact  = vSubTrans * vNoteListThold
           vNoteThold = IF vNoteFact GT 0.5 THEN ROUND(vNoteFact, 0) ELSE ?
           vNoteList1 = "":U
           vNoteList2 = "":U
    . /* ASSIGN */

    FOR EACH ttNoteList NO-LOCK
       WHERE ttNoteList.ListId EQ ttTran.Trid
          BY ttNoteList.NoteCount DESCENDING
          BY ttNoteList.NoteArea
          BY ttNoteList.NoteType:

      IF ttNoteList.NoteCount GT vNoteThold THEN
      ASSIGN
        vNoteFact = ROUND(ttNoteList.NoteCount / vSubTrans, vNoteListDigit)
        vNoteList1 = vNoteList1 + ",":U + ttNoteList.NoteType
                   + (IF ttNoteList.NoteArea EQ 0 THEN "":U
                      ELSE ":":U + STRING(ttNoteList.NoteArea)
                     ) +
                     (IF vNoteFact EQ 1.0 THEN "":U
                      ELSE "*":U + STRING(vNoteFact))
      . /* ASSIGN */
      ELSE
      ASSIGN
        vNoteCount = ttNoteList.NoteCount
        vNoteList2 = vNoteList2 + ",":U + ttNoteList.NoteType
                   + (IF ttNoteList.NoteArea EQ 0 THEN "":U
                      ELSE ":":U + STRING(ttNoteList.NoteArea)
                     ) +
                     (IF vNoteCount EQ 1 THEN "":U
                      ELSE "*":U + STRING(vNoteCount))
      . /* ASSIGN */

      DELETE ttNoteList. /* of ttTran.Trid */

    END. /* FOR EACH ttNoteList */

    ASSIGN vNoteList1 = SUBSTRING(vNoteList1, 2)
           vNoteList2 = SUBSTRING(vNoteList2, 2)
           vTranTime  = INTERVAL(
                  IF ttTran.EndTime NE ? THEN ttTran.EndTime ELSE vAiLastTime ,
                  IF ttTran.BgnTime NE ? THEN ttTran.BgnTime ELSE vAiFirstTime,
                                 "seconds":U)
    . /* ASSIGN */

/* ====== TransReport ====================================================== */

    PUT STREAM TransReport UNFORMATTED
             /*Trid      */ ttTran.Trid
      {&Sep} /*Userid    */ ttTran.UserName
      {&Sep} /*AiNum     */ ttTran.AiSeqNum
      {&Sep} /*BegTime   */ DateTime2String(ttTran.BgnTime)
      {&Sep} /*EndTime   */ DateTime2String(ttTran.EndTime)
      {&Sep} /*Len       */ vTranTime
      {&Sep} /*Idle      */ ttTran.IdleTime
      {&Sep} /*%Idle     */ Percent(ttTran.IdleTime, vTranTime)
      {&Sep} /*MaxGap    */ ttTran.MaxGTime
      {&Sep} /*GapCount  */ ttTran.GapCount
      {&Sep} /*SeqIncr   */ vTranSeqs
      {&Sep} /*SeqUsed   */ ttTran.UsedSeqCount
      {&Sep} /*AveSeqPath*/ ROUND(ttTran.AveSeqPath, vRoundDigits)
      {&Sep} /*MaxSeqPath*/ ttTran.MaxSeqPath
      {&Sep} /*Updates   */ ttTran.UpdtCount
      {&Sep} /*%Updates  */ Percent(ttTran.UpdtCount, ttTran.NoteCount)
      {&Sep} /*Notes     */ ttTran.NoteCount
      {&Sep} /*SubTrans  */ vSubTrans
      {&Sep} /*Notes/Sub */ ROUND(ttTran.NoteCount / vSubTrans, vRoundDigits)
      {&Sep} /*NoteList  */ IF vNoteList1 EQ "":U THEN vNoteList2 ELSE
                            "(":U + vNoteList1 + ")*":U + STRING(vSubTrans)
                            + MIN(",":U, vNoteList2) + vNoteList2
    SKIP. /* PUT STREAM TransReport */

  END. /* FOR EACH ttTran */

/* ====== UsersReport ====================================================== */

  FOR EACH ttAiFile NO-LOCK:
    ACCUMULATE ttAiFile.AiInterval (TOTAL COUNT)
               ttAiFile.NoteCount  (TOTAL)
               ttAiFile.TranCount  (TOTAL)
    . /* ACCUMULATE */
  END. /* FOR EACH ttAiFile */
  ASSIGN vAiInterval = ACCUM TOTAL ttAiFile.AiInterval
         vTBgnCount  = ACCUM TOTAL ttAiFile.NoteCount
         vNoteCount  = ACCUM TOTAL ttAiFile.TranCount
  . /* ASSIGN */

  IF (ACCUM COUNT ttAiFile.AiInterval) GT 1 THEN
  FOR EACH ttUser NO-LOCK
     WHERE ttUser.AiSeqNum EQ {&TotalStat}
        BY ttUser.AiSeqNum
        BY ttUser.UserName:

    ASSIGN vInterval = INTERVAL(ttUser.LastTime, ttUser.FirstTime, "seconds":U).

    PUT STREAM UsersReport UNFORMATTED
             /*AiScan   */ "Total"
      {&Sep} /*AiNum    */ "Total"
      {&Sep} /*AiIntr   */ vAiInterval
      {&Sep} /*Userid   */ ttUser.UserName
      {&Sep} /*FirstTime*/ DateTime2String(ttUser.FirstTime)
      {&Sep} /*LastTime */ DateTime2String(ttUser.LastTime)
      {&Sep} /*UserTime */ vInterval
      {&Sep} /*%UserTime*/ Percent(vInterval, vAiInterval)
      {&Sep} /*TranTime */ ttUser.TranTime
      {&Sep} /*%TranTime*/ Percent(ttUser.TranTime, vAiInterval)
      {&Sep} /*IdleTime */ ttUser.IdleTime
      {&Sep} /*%IdleTime*/ Percent(ttUser.IdleTime, ttUser.TranTime)
      {&Sep} /*Trans    */ ttUser.TranCount
      {&Sep} /*%Trans   */ Percent(ttUser.TranCount, vTBgnCount)
      {&Sep} /*Notes    */ ttUser.NoteCount
      {&Sep} /*%Notes   */ Percent(ttUser.NoteCount, vNoteCount)
      {&Sep} /*AveTime  */ ROUND(ttUser.AveTranTime, vRoundDigits)
      {&Sep} /*MaxTime  */ ttUser.MaxTranTime
      {&Sep} /*AveNotes */ ROUND(ttUser.AveNoteCount, vRoundDigits)
      {&Sep} /*MaxNotes */ ttUser.MaxNoteCount
    SKIP. /* PUT STREAM UsersReport */

  END. /* FOR EACH ttUser NO-LOCK*/

/* ====== CkptsReport ====================================================== */

  IF SEEK(CkptsReport) EQ 0 THEN /* ---------------------------------------- */
  PUT STREAM CkptsReport UNFORMATTED
           "AiScan"    /* Name of AI scan file                               */
    {&Sep} "AiNum"     /* AI sequence number (Seqno)                         */
    {&Sep} "CkptId"    /* Checkpoint's ID inside the current AI file         */
    {&Sep} "CkptTime"  /* Checkpoint starting time                           */
    {&Sep} "Delta"     /* Estimated accuracy of checkpoint time              */
    {&Sep} "Interval"  /* Time interval between checkpoints                  */
    {&Sep} "Notes"     /* Total number of notes created between checkpoints  */
    {&Sep} "Notes/sec" /* Notes per sec                                      */
    {&Sep} "Updates"   /* Number of update notes created between checkpoints */
    {&Sep} "%Updates"  /* Percentage of the total notes count                */
    {&Sep} "Trans"     /* Number of transactions started between checkpoints */
  SKIP. /* PUT */

  ASSIGN vPrevCkpTime = ?.

  FOR EACH ttCkpt NO-LOCK
        BY ttCkpt.AiSeqNum
        BY ttCkpt.CkptId:

/* Add statistics from the previous AI scans: */
    IF ttCkpt.CkptId EQ 1 THEN
    FOR EACH ttAiFile NO-LOCK
       WHERE ttAiFile.AiSeqNum LT ttCkpt.AiSeqNum
          BY ttAiFile.AiSeqNum DESCENDING
    TRANSACTION:
      ASSIGN ttCkpt.NoteCount = ttCkpt.NoteCount + ttAiFile.CkpNoteCount
             ttCkpt.UpdtCount = ttCkpt.UpdtCount + ttAiFile.CkpUpdtCount
             ttCkpt.TranCount = ttCkpt.TranCount + ttAiFile.CkpTranCount
             vPrevCkpTime     = ttAiFile.LastCkpTime
      . /* ASSIGN */

      IF vPrevCkpTime NE ? THEN
      LEAVE.

      ASSIGN vPrevCkpTime = ttAiFile.AiNewTime.
    END.

    FIND FIRST ttAiFile NO-LOCK
         WHERE ttAiFile.AiSeqNum EQ ttCkpt.AiSeqNum.

    ASSIGN vPrevCkpTime = ttAiFile.AiNewTime WHEN vPrevCkpTime EQ ?
           vInterval = INTERVAL(ttCkpt.CkptTime, vPrevCkpTime, "seconds":U)
           vPrevCkpTime = ttCkpt.CkptTime
    . /* ASSIGN */

/* ======= CkptsReport ===================================================== */

    PUT STREAM CkptsReport UNFORMATTED
             /*AiScan   */ ttAiFile.AiFileName
      {&Sep} /*AiNum    */ ttAiFile.AiSeqNum
      {&Sep} /*CkptId   */ ttCkpt.CkptId
      {&Sep} /*CkptTime */ DateTime2String(ttCkpt.CkptTime)
      {&Sep} /*Delta    */ ttCkpt.TimeGap
      {&Sep} /*Interval */ vInterval
      {&Sep} /*Notes    */ ttCkpt.NoteCount
      {&Sep} /*Notes/sec*/ ROUND(ttCkpt.NoteCount / vInterval, 2)
      {&Sep} /*Updates  */ ttCkpt.UpdtCount
      {&Sep} /*%Updates */ Percent(ttCkpt.UpdtCount, ttCkpt.NoteCount)
      {&Sep} /*Trans    */ ttCkpt.TranCount
    SKIP. /* PUT */

  END. /* FOR EACH ttCkpt */

  EMPTY TEMP-TABLE ttTran.
  EMPTY TEMP-TABLE ttUser.
  EMPTY TEMP-TABLE ttCkpt.

END PROCEDURE. /* RestReports */

/* ------------------------------------------------------------------------- */

PROCEDURE ReadAiDir.
  DEFINE INPUT PARAMETER ipInputDir    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMaxDirLevel AS INTEGER   NO-UNDO.
  DEFINE INPUT PARAMETER ipFileMask    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipOutputDir   AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipOutPrefix   AS CHARACTER NO-UNDO.
  /*
ipInputDir     - Root directory with the files to read from;
ipMaxDirLevel  - 1 = search in current dir only, ? = search in all subdirs;
ipFileMask     - Mask for input files;
ipOutputDir    - Directory for report files. Default is ipInputDir;
ipOutPrefix    - Prefix for output files. Default is "AiScanStat".
*/

  DEFINE VARIABLE vPrevETime AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vFileCount AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vSlash     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vErrorDesc AS CHARACTER NO-UNDO EXTENT 18 INITIAL [
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

  ASSIGN FILE-INFO:FILE-NAME = ipInputDir.
  IF FILE-INFO:FULL-PATHNAME EQ ? THEN
  DO:
    MESSAGE "Input directory does not exist!" SKIP
             ipInputDir
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    QUIT.
  END.
  ELSE
  ASSIGN ipInputDir = FILE-INFO:FULL-PATHNAME. /*+ vSlash.*/

  IF ipOutputDir NE ? THEN
  ASSIGN FILE-INFO:FILE-NAME = ipOutputDir.
  IF FILE-INFO:FULL-PATHNAME EQ ? THEN
  DO:
    MESSAGE "Output directory does not exist!" SKIP
             ipOutputDir
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    QUIT.
  END.

  ASSIGN vSlash = IF OPSYS EQ "Unix":U THEN "/":U ELSE "~\":U
         ipOutputDir = FILE-INFO:FULL-PATHNAME
         ipOutPrefix = SUBSTRING(ipInputDir, R-INDEX(ipInputDir, vSlash) + 1)
                     + ".AiScanStat" WHEN ipOutPrefix EQ ?
         ipOutPrefix = ipOutputDir + vSlash + ipOutPrefix + ".":U
  . /* ASSIGN */

  RUN GetOSFiles(INPUT ipInputDir, INPUT ipFileMask, INPUT ipMaxDirLevel).

  ASSIGN vFileCount = 0.
  FOR EACH ttOSFile NO-LOCK WHERE
       NOT ttOSFile.osFilePath MATCHES ipOutPrefix + "*~~~.txt":
    ASSIGN vFileCount = vFileCount + 1.
  END. /* FOR EACH ttOSFile */

  IF vFileCount EQ 0 THEN
  DO:
    MESSAGE
      SUBSTITUTE("No files (&1) found in dir:", ipFileMask) SKIP ipInputDir
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    QUIT.
  END.

  ASSIGN vFrameTitle = SUBSTITUTE("Importing &1 file(s)...",
                                  STRING(vFileCount)).

  OUTPUT STREAM FilesReport  TO VALUE(ipOutPrefix + "Files.txt").
  OUTPUT STREAM AreasReport  TO VALUE(ipOutPrefix + "Areas.txt").
  OUTPUT STREAM NotesReport  TO VALUE(ipOutPrefix + "Notes.txt").
  OUTPUT STREAM DbkeyReport  TO VALUE(ipOutPrefix + "Dbkey.txt").
  OUTPUT STREAM TransReport  TO VALUE(ipOutPrefix + "Trans.txt").
  OUTPUT STREAM UsersReport  TO VALUE(ipOutPrefix + "Users.txt").
  OUTPUT STREAM PerSecReport TO VALUE(ipOutPrefix + "PerSec.txt").

  PAUSE 0 BEFORE-HIDE.

  ASSIGN vPrevETime = ETIME
         vFileCount = 0.
  FOR EACH ttOSFile NO-LOCK WHERE
       NOT ttOSFile.osFilePath MATCHES ipOutPrefix + "*~~~.txt":

    ASSIGN vFileCount          = vFileCount + 1
           FILE-INFO:FILE-NAME = ttOSFile.osFilePath
    . /* ASSIGN */

    DISPLAY vFileCount          @ vCnt
            ttOSFile.osFileName @ vFile
            STRING(INTEGER(FILE-INFO:FILE-SIZE / 1024), ">,>>>,>>9K") @ vSize
            "0%"                @ vLoad
            "0s"                @ vTime
    WITH FRAME StatusFrame TITLE vFrameTitle.

    RUN ImportAiScan(INPUT ttOSFile.osFilePath).

    DOWN WITH FRAME StatusFrame.

  END. /* FOR EACH ttOSFile */

  OUTPUT STREAM CkptsReport TO VALUE(ipOutPrefix + "Ckpts.txt").

  RUN RestReports.

  OUTPUT STREAM FilesReport  CLOSE.
  OUTPUT STREAM AreasReport  CLOSE.
  OUTPUT STREAM NotesReport  CLOSE.
  OUTPUT STREAM DbkeyReport  CLOSE.
  OUTPUT STREAM TransReport  CLOSE.
  OUTPUT STREAM UsersReport  CLOSE.
  OUTPUT STREAM CkptsReport  CLOSE.
  OUTPUT STREAM PerSecReport CLOSE.

  BELL.
  MESSAGE SUBSTITUTE("Total execution time: &1 sec",
          STRING(INTEGER((ETIME - vPrevETime) / 1000)), "HH:MM:SS":U) SKIP
         "Result files:" ipOutPrefix + "*.txt"
  VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE. /* ReadAiDir */

/* -------------------------------------------------------------------------- */

PROCEDURE GetOSFiles.

  DEFINE INPUT PARAMETER ipInputDir    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFileMask    AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMaxDirLevel AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vFileName AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFilePath AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vFileAttr AS CHARACTER NO-UNDO.

  ASSIGN ipMaxDirLevel = ipMaxDirLevel - 1.
  INPUT FROM OS-DIR(ipInputDir).
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

/* -------------------------------------------------------------------------- */

