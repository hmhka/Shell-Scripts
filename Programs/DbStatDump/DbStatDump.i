/*------------------------------------------------------------------------
    File        : DbStatDump.i
    Purpose     :

    Syntax      :

    Description : Define the shared temp-tables to store the most recent stats

    Author(s)   : George Potemkin
    Created     : Jun 05, 2010
    Modified    : Jul 01, 2010
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&GLOBAL-DEFINE Sep "~t"

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) GT 10.2
OR (DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) EQ 10.2
    AND SUBSTRING(PROVERSION, INDEX(PROVERSION,".") + 2, 1) GE "B":U)
&THEN
&GLOBAL-DEFINE Version_GE_102B TRUE
&ENDIF

DEFINE {1} SHARED TEMP-TABLE ttDbStat NO-UNDO
  FIELD DbHost      AS CHARACTER
  FIELD DbPath      LIKE _FileList._FileList-Name
  FIELD DbStartTime LIKE _MstrBlk._MstrBlk-oprdate
  FIELD DbTimeStamp LIKE _MstrBlk._MstrBlk-TimeStamp INITIAL ?

  FIELD SnapshotTimeStamp AS CHARACTER

  FIELD DbAccesses  LIKE _ActSummary._Summary-DbAccesses

  FIELD RecRead     LIKE _ActRecord._Record-RecRead
  FIELD RecUpdate   LIKE _ActRecord._Record-RecUpd
  FIELD RecCreate   LIKE _ActRecord._Record-RecCreat
  FIELD RecDelete   LIKE _ActRecord._Record-RecDel

  FIELD BytesRead   LIKE _ActRecord._Record-BytesRead
  FIELD BytesUpdate LIKE _ActRecord._Record-BytesUpd
  FIELD BytesCreate LIKE _ActRecord._Record-BytesCreat
  FIELD BytesDelete LIKE _ActRecord._Record-BytesDel

  FIELD IdxRead     LIKE _ActIndex._Index-Find
  FIELD IdxCreate   LIKE _ActIndex._Index-Create
  FIELD IdxDelete   LIKE _ActIndex._Index-Delete

  FIELD DbReads     LIKE _ActSummary._Summary-DbReads
  FIELD DbWrites    LIKE _ActSummary._Summary-DbWrites
  FIELD DatDbReads  LIKE _ActIOType._IOType-DataReads
  FIELD DatDbWrites LIKE _ActIOType._IOType-DataWrts
  FIELD IdxDbReads  LIKE _ActIOType._IOType-IdxRds
  FIELD IdxDbWrites LIKE _ActIOType._IOType-IdxWrts

  FIELD BiWrites    LIKE _ActBILog._BiLog-TotalWrts
  FIELD BiBytesWrtn LIKE _ActBILog._BiLog-BytesWrtn
  FIELD BiNotesWrtn LIKE _ActBILog._BiLog-RecWriten
  FIELD TransComm   LIKE _ActSummary._Summary-TransComm

/*  _Latch._Latch-lock & _Latch._Latch-wait are 32-bit integer 
but _Resrc._Resrc-lock & _Resrc._Resrc-wait are 64-bit integer.
Sum of _Latch-lock can easily overflow 2 billion limit.
I don't want to declare these fields as int64 
because the program will not with the earlier Progress versions. */
  FIELD LatchLock   LIKE _Resrc._Resrc-lock /* _Latch._Latch-lock */
  FIELD LatchWait   LIKE _Resrc._Resrc-wait /* _Latch._Latch-wait */
  FIELD ResrcLock   LIKE _Resrc._Resrc-lock
  FIELD ResrcWait   LIKE _Resrc._Resrc-wait
  FIELD SemWaits    LIKE _ActOther._Other-Wait
  FIELD TxeLock     LIKE _TxeLock._Txe-Locks EXTENT 0
  FIELD TxeWait     LIKE _TxeLock._Txe-Waits EXTENT 0

  INDEX DbStatIdx IS UNIQUE
        DbHost
        DbPath
. /* TEMP-TABLE ttDbStat */


DEFINE {1} SHARED TEMP-TABLE ttAreaStat NO-UNDO
  FIELD DbHost      AS CHARACTER
  FIELD DbPath      LIKE _FileList._FileList-Name
  FIELD AreaNumber  LIKE _Area._Area-number
  FIELD AreaName    LIKE _Area._Area-Name
  FIELD AreaHWM     LIKE _AreaStatus._AreaStatus-Hiwater
  FIELD AreaReads   LIKE _ActIOFile._IOFile-Reads
  FIELD AreaWrites  LIKE _ActIOFile._IOFile-Writes
  FIELD TableRead   LIKE _TableStat._TableStat-Read
  FIELD TableUpdate LIKE _TableStat._TableStat-Update
  FIELD TableCreate LIKE _TableStat._TableStat-Create
  FIELD TableDelete LIKE _TableStat._TableStat-Delete
  FIELD IndexRead   LIKE _IndexStat._IndexStat-Read
  FIELD IndexCreate LIKE _IndexStat._IndexStat-Create
  FIELD IndexDelete LIKE _IndexStat._IndexStat-Delete

  INDEX AreaStatIdx IS UNIQUE
        DbHost
        DbPath
        AreaNumber
. /* TEMP-TABLE ttAreaStat */


DEFINE {1} SHARED TEMP-TABLE ttTableStat NO-UNDO
  FIELD DbHost      AS CHARACTER
  FIELD DbPath      LIKE _FileList._FileList-Name
  FIELD TableNumber LIKE _File._File-Number
  FIELD TableName   LIKE _File._File-Name /* The field is not used */
  FIELD TableRead   LIKE _TableStat._TableStat-Read
  FIELD TableUpdate LIKE _TableStat._TableStat-Update
  FIELD TableCreate LIKE _TableStat._TableStat-Create
  FIELD TableDelete LIKE _TableStat._TableStat-Delete
&IF DEFINED(Version_GE_102B)
&THEN
  FIELD TableOsRead LIKE _TableStat._TableStat-OsRead
&ENDIF

  INDEX TableStatIdx IS UNIQUE
        DbHost
        DbPath
        TableNumber
. /* TEMP-TABLE ttTableStat */

/*
ttTableStat.TableName, ttIndexStat.TableName and ttIndexStat.IndexName
These fields are not used to create the reports.
But data in these fields can be usefull if someone is going to convert data
from DbStatDump.LastSnapshot.d to the stats report since db startup.
*/

DEFINE {1} SHARED TEMP-TABLE ttIndexStat NO-UNDO
  FIELD DbHost      AS CHARACTER
  FIELD DbPath      LIKE _FileList._FileList-Name
  FIELD IndexNumber LIKE _Index._Idx-Num
  FIELD TableName   LIKE _File._File-Name   /* The field is not used */
  FIELD IndexName   LIKE _Index._Index-Name /* The field is not used */
  FIELD IndexRead   LIKE _IndexStat._IndexStat-Read
  FIELD IndexCreate LIKE _IndexStat._IndexStat-Create
  FIELD IndexDelete LIKE _IndexStat._IndexStat-Delete
&IF DEFINED(Version_GE_102B)
&THEN
  FIELD IndexOsRead LIKE _IndexStat._IndexStat-OsRead
&ENDIF

  INDEX IndexStatIdx IS UNIQUE
        DbHost
        DbPath
        IndexNumber
. /* TEMP-TABLE ttIndexStat */


DEFINE {1} SHARED TEMP-TABLE ttResrcStat NO-UNDO
  FIELD DbHost    AS CHARACTER
  FIELD DbPath    LIKE _FileList._FileList-Name
  FIELD ResrcType AS CHARACTER
  FIELD ResrcName LIKE _Resrc._Resrc-Name
  FIELD ResrcLock LIKE _Resrc._Resrc-lock
  FIELD ResrcWait LIKE _Resrc._Resrc-wait

  INDEX ResrcStatIdx IS UNIQUE
      DbHost
      DbPath
      ResrcType
      ResrcName
. /* TEMP-TABLE ttResrcStat */
