/* ------------------------------------------------------------------------
    File        : LruShot.p
    Purpose     : Protect promon/proshut utilities against a locked LRU latch.
    Syntax      : Run it once before using promon/proshut.
    Description : 
    
    Set highest possible values of "Skips" to the blocks in buffer pool.
    Accessing these blocks will not acquire the LRU latch next 2 billions times.
    The target blocks are the ones used by proshut and promon utilities.

    Program re-reads the blocks the (-lruskips + 1) times.
    When their "Skips" counter that was less or equal the initial -lruskips
    will become zero Progress will re-set it to the new (maximum) value.

   promon/R&D/debghb/6/1. Status: Cache Entries
   promon/R&D/debghb/6/4. Status: Lru Chains
   
Num   DBKEY Area   Hash T S Usect Flags   Updctr   Lsn Chkpnt  Lru      Skips
 33      64    1    139 O       0 L            4     0      0    0 2147483647
 34      64    6    824 O       0 L         1385     0      0    0 2147483647
 35      64    7     74 O       0 L           43     0      0    0 2147483647
 36      64    8    211 O       0 L           10     0      0    0 2147483647
 37       2    9    348 O       0 L            6     0      0    0 2147483647
 38       2   10    485 O       0 L            6     0      0    0 2147483647
 39      64   11    622 O       0 L            6     0      0    0 2147483647
  
    Progress version should be 10.2B06 or higher.
    VST tables in a database should be updated (proutil db -C updatevst).

    Author(s)   : George Potemkin
    Created     : May 14, 2016
    Modified    : May 14, 2016
------------------------------------------------------------------------ */

PAUSE 0 BEFORE-HIDE.
RUN Skips4DbRecid.      /* promon and proshut reads _Db record */
RUN Skips4Index("_User":U, "_User-sql-only-user":U). /* promon */
RUN Skips4ACOBlocks.                                 /* promon */
/*RUN Skips4Connection.*/

/* *******************************  Functions  ***************************** */

FUNCTION SetLruSkipsTo RETURNS INTEGER (ipNewLruSkips AS INTEGER).
/* Returns the current value of the -lruskips and re-set it to a new value. */

  DEFINE VARIABLE vOldLruSkips AS INTEGER NO-UNDO INITIAL 0.

  FOR FIRST DICTDB._Startup EXCLUSIVE-LOCK:
    ASSIGN vOldLruSkips = DICTDB._Startup._Startup-LRU-Skips
           DICTDB._Startup._Startup-LRU-Skips = ipNewLruSkips
    . /* ASSIGN */
  END.
  RETURN vOldLruSkips.
END FUNCTION. /* SetLruSkipsTo */

/* *****************************  Procedures  ****************************** */

PROCEDURE Skips4DbRecid:

/* Promon/proshut reads the _Db record (but not the rest of metaschema).
   Its recid is stored in master block as mb_dbrecid.
*/
  DEFINE VARIABLE vMaxLruSkips AS INTEGER NO-UNDO INITIAL 2147483647.
  DEFINE VARIABLE vMinLruSkips AS INTEGER NO-UNDO INITIAL 100000.
  DEFINE VARIABLE vOldLruSkips AS INTEGER NO-UNDO.
  DEFINE VARIABLE vAccess      AS INTEGER NO-UNDO.
  DEFINE VARIABLE vRowid       AS ROWID   NO-UNDO.

  FIND FIRST DICTDB._Db NO-LOCK WHERE DICTDB._Db._Db-local EQ TRUE.
/*
  MESSAGE "mb_dbrecid:" RECID(DICTDB._Db).
*/
  ASSIGN vRowid = ROWID(DICTDB._Db)
         vOldLruSkips = SetLruSkipsTo(vMaxLruSkips)
  . /* ASSIGN */
  ETIME(TRUE).

  IF vOldLruSkips LE vMinLruSkips THEN
  DO vAccess = 0 TO vOldLruSkips:
    FIND FIRST DICTDB._Db NO-LOCK WHERE ROWID(DICTDB._Db) EQ vRowid.
  END. /* DO vAccess */

  ASSIGN vMaxLruSkips = SetLruSkipsTo(vOldLruSkips).
/*
  MESSAGE "etime:" ETIME "msec lruskips:" vOldLruSkips.
*/
END PROCEDURE. /* Skips4DbRecid */

/* -------------------------------------------------------------------------- */

PROCEDURE Skips4ACOBlocks:

/* Promon reads the ACO object blocks when you enter Activity screen
   or R&D level or any action (A, L, R, S, U, Z) on Activity screens.
   ACO (Area Control Object) object block = bk_type 12 + objectId 0,
   It's the block 3 in each area.
   The contents of the ACO blocks is reported by prostrct statistics
   but it reads the blocks from a disk rather than from db buffer pool
   even if database is online.
   _AreaStatus VST table reads the ACO blocks from db buffer pool.
   For information: it addinionally reads all blocks from Control Area.
*/
  DEFINE VARIABLE vMaxLruSkips AS INTEGER NO-UNDO INITIAL 2147483647.
  DEFINE VARIABLE vMinLruSkips AS INTEGER NO-UNDO INITIAL 10000.
  DEFINE VARIABLE vOldLruSkips AS INTEGER NO-UNDO.
  DEFINE VARIABLE vAccess      AS INTEGER NO-UNDO.

/* Just for information: */
/*
  FOR EACH DICTDB._Area NO-LOCK
     WHERE DICTDB._Area._Area-type EQ 6:
    MESSAGE SUBSTITUTE('ACO dbkey: &1, area &2 "&3"':U,
              /* &1 */ STRING(2 * EXP(2, DICTDB._Area._Area-recbits)),
              /* &2 */ STRING(DICTDB._Area._Area-number),
              /* &3 */ STRING(DICTDB._Area._Area-name)).
  END.
  ETIME(TRUE).
*/
  ASSIGN vOldLruSkips = SetLruSkipsTo(vMaxLruSkips).

  IF vOldLruSkips LE vMinLruSkips THEN
  DO vAccess = 0 TO vOldLruSkips:
    FOR EACH DICTDB._AreaStatus NO-LOCK
       WHERE DICTDB._AreaStatus._AreaStatus-Rmnum NE ?:
    END.
  END. /* DO vAccess */

  ASSIGN vMaxLruSkips = SetLruSkipsTo(vOldLruSkips).
/*
  MESSAGE "etime:" ETIME "msec lruskips:" vOldLruSkips.
*/
END PROCEDURE. /* Skips4ACOBlocks */

/* -------------------------------------------------------------------------- */

PROCEDURE Skips4Index:

/* Promon reads the blocks of the _User._User-sql-only-user index
   even though it totally ignores the _User table for authorization.
*/
  DEFINE INPUT PARAMETER ipTableName LIKE _File._File-Name NO-UNDO.
  DEFINE INPUT PARAMETER ipIndexName LIKE _Index._Index-Name NO-UNDO.
  
  DEFINE VARIABLE vMaxLruSkips AS INTEGER NO-UNDO INITIAL 2147483647.
  DEFINE VARIABLE vMinLruSkips AS INTEGER NO-UNDO INITIAL 100000.
  DEFINE VARIABLE vOldLruSkips AS INTEGER NO-UNDO.
  DEFINE VARIABLE vAccess      AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE hQuery  AS HANDLE NO-UNDO. /*Query handle*/
  DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO. /*Buffer handle*/

  FOR FIRST DICTDB._File NO-LOCK
      WHERE DICTDB._File._File-Name EQ ipTableName,

      FIRST DICTDB._Index OF DICTDB._File NO-LOCK
      WHERE DICTDB._Index._Index-Name EQ ipIndexName,
    
      FIRST DICTDB._StorageObject NO-LOCK
      WHERE DICTDB._StorageObject._Db-recid EQ DICTDB._File._Db-recid
        AND DICTDB._StorageObject._Object-type EQ 2
        AND DICTDB._StorageObject._Object-number EQ DICTDB._Index._Idx-num:
/*
    MESSAGE SUBSTITUTE("Root dbkey: &1, area: &2, index: &3.&4":U,
              /* &1 */ STRING(DICTDB._StorageObject._Object-root),
              /* &2 */ STRING(DICTDB._StorageObject._Area-number),
              /* &3 */ ipTableName,
              /* &4 */ ipIndexName)
    . /* MESSAGE */
*/
    CREATE BUFFER hBuffer FOR TABLE "DICTDB.":U + ipTableName.
    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS(hBuffer).
    hQuery:QUERY-PREPARE(SUBSTITUTE("FOR EACH DICTDB.&1 NO-LOCK USE-INDEX &2":U,
                                    ipTableName, ipIndexName)).

    ASSIGN vOldLruSkips = SetLruSkipsTo(vMaxLruSkips).
    ETIME(TRUE).

    IF vOldLruSkips LE vMinLruSkips THEN
    DO vAccess = 0 TO vOldLruSkips:
      hQuery:QUERY-OPEN().
      hQuery:GET-FIRST().
      hQuery:QUERY-CLOSE().
    END.

    ASSIGN vMaxLruSkips = SetLruSkipsTo(vOldLruSkips).
/*
    MESSAGE "etime:" ETIME "msec lruskips:" vOldLruSkips.
*/
    DELETE OBJECT hQuery  NO-ERROR.
    DELETE OBJECT hBuffer NO-ERROR.

  END. /* FOR FIRST _File, FIRST _Index, FIRST _StorageObject */

/* The alternate static solution: */
  &IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") - 1)) GE 11
  &THEN
/*
  ASSIGN vOldLruSkips = SetLruSkipsTo(vMaxLruSkips).

  IF vOldLruSkips LE vMinLruSkips THEN
  DO vAccess = 0 TO vOldLruSkips:
    CAN-FIND(FIRST DICTDB._User USE-INDEX _User-sql-only-user).
  END.
  ASSIGN vMaxLruSkips = SetLruSkipsTo(vOldLruSkips).
*/  
  &ENDIF

END PROCEDURE. /* Skips4Index */

/* -------------------------------------------------------------------------- */

PROCEDURE Skips4Connection:

/* Set the maximum skips for all blocks accessed a database connection.
   It might take awhile especially if there are a lot of database objects
   and the -lruskips is already set to a high value.
   For example, time to connect a sports db is approximately 0.1 sec.
*/
  DEFINE VARIABLE vMaxLruSkips AS INTEGER NO-UNDO INITIAL 2147483647.
  DEFINE VARIABLE vMinLruSkips AS INTEGER NO-UNDO INITIAL 100.
  DEFINE VARIABLE vOldLruSkips AS INTEGER NO-UNDO.
  DEFINE VARIABLE vAccess      AS INTEGER NO-UNDO.

  DEFINE VARIABLE vDbConnect AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vLdbName   AS CHARACTER NO-UNDO.

  ASSIGN vLdbName   = LDBNAME("DICTDB") + "tmp":U
         vDbConnect = REPLACE(DBPARAM("DICTDB"), ",":U, " ":U)
                    + " -ld ":U + vLdbName
         vOldLruSkips = SetLruSkipsTo(vMaxLruSkips)
  . /* ASSIGN */
/*
  MESSAGE "Connection to" LDBNAME("DICTDB").
  ETIME(TRUE).
*/
  IF vOldLruSkips LE vMinLruSkips THEN
  REPEAT vAccess = 0 TO vOldLruSkips:
    CONNECT VALUE(vDbConnect) NO-ERROR.
    IF NOT CONNECTED(vLdbName) THEN
    LEAVE.
    DISCONNECT VALUE(vLdbName).
  END. /* DO vAccess */

  ASSIGN vMaxLruSkips = SetLruSkipsTo(vOldLruSkips).
/*
  MESSAGE "etime:" ETIME "msec lruskips:" vOldLruSkips.
*/
END PROCEDURE. /* Skips4Connection */

/* -------------------------------------------------------------------------- */
