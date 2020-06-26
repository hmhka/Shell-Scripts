DEFINE VARIABLE vMyUserNumber   AS INTEGER NO-UNDO.
DEFINE VARIABLE vFirstStatID    AS INTEGER NO-UNDO.
DEFINE VARIABLE vLastStatID     AS INTEGER NO-UNDO.
DEFINE VARIABLE vTableRangeSize AS INTEGER NO-UNDO.
DEFINE VARIABLE vHighestTableId AS INTEGER NO-UNDO.
DEFINE VARIABLE vIndexRangeSize AS INTEGER NO-UNDO.
DEFINE VARIABLE vHighestIndexId AS INTEGER NO-UNDO.

FOR FIRST DICTDB._MyConnection NO-LOCK:
  ASSIGN vMyUserNumber = DICTDB._MyConnection._MyConn-UserId.
END.

FOR LAST DICTDB._TableStat NO-LOCK:
  ASSIGN vTableRangeSize = RECID(DICTDB._TableStat).
END.

FOR EACH DICTDB._File NO-LOCK
      BY DICTDB._File._File-Number DESCENDING:
  ASSIGN vHighestTableId = DICTDB._File._File-Number.
  LEAVE.
END.

ASSIGN vFirstStatID = vTableRangeSize * vMyUserNumber
       vLastStatID  = vTableRangeSize + vFirstStatID
       vFirstStatID = vFirstStatID + 1
. /* ASSIGN */

FOR EACH  DICTDB._UserTableStat NO-LOCK
   WHERE  DICTDB._UserTableStat._UserTableStat-id      GE vFirstStatID
     AND  DICTDB._UserTableStat._UserTableStat-id      LE vLastStatID
     AND (DICTDB._UserTableStat._UserTableStat-Read    NE 0
       OR DICTDB._UserTableStat._UserTableStat-Update  NE 0
       OR DICTDB._UserTableStat._UserTableStat-Create  NE 0
       OR DICTDB._UserTableStat._UserTableStat-Delete  NE 0)
    WHILE DICTDB._UserTableStat._UserTableStat-Num     LE vHighestTableId:
  DISPLAY DICTDB._UserTableStat._UserTableStat-Num
          DICTDB._UserTableStat._UserTableStat-Read
          DICTDB._UserTableStat._UserTableStat-Update
          DICTDB._UserTableStat._UserTableStat-Create
          DICTDB._UserTableStat._UserTableStat-Delete
  . /* DISPLAY */
END. /* FOR EACH _UserTableStat */

FOR LAST DICTDB._IndexStat NO-LOCK:
  ASSIGN vIndexRangeSize = RECID(DICTDB._IndexStat).
END.

FOR EACH DICTDB._Index NO-LOCK
      BY DICTDB._Index._Idx-Num DESCENDING:
  ASSIGN vHighestIndexId = DICTDB._Index._Idx-Num.
  LEAVE.
END.

ASSIGN vFirstStatID = vIndexRangeSize * vMyUserNumber
       vLastStatID  = vIndexRangeSize + vFirstStatID
       vFirstStatID = vFirstStatID + 1
. /* ASSIGN */

FOR EACH  DICTDB._UserIndexStat NO-LOCK
   WHERE  DICTDB._UserIndexStat._UserIndexStat-id      GE vFirstStatID
     AND  DICTDB._UserIndexStat._UserIndexStat-id      LE vLastStatID
     AND (DICTDB._UserIndexStat._UserIndexStat-Read    NE 0
       OR DICTDB._UserIndexStat._UserIndexStat-Create  NE 0
       OR DICTDB._UserIndexStat._UserIndexStat-Delete  NE 0)
    WHILE DICTDB._UserIndexStat._UserIndexStat-Num     LE vHighestIndexId:
  DISPLAY DICTDB._UserIndexStat._UserIndexStat-Num
          DICTDB._UserIndexStat._UserIndexStat-Read
          DICTDB._UserIndexStat._UserIndexStat-Create
          DICTDB._UserIndexStat._UserIndexStat-Delete
  . /* DISPLAY */
END. /* FOR EACH DICTDB._UserIndexStat */

