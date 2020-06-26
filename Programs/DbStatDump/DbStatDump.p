/*------------------------------------------------------------------------
    File        : DbStatDump.p
    Purpose     : Dump _TableStat/_IndexStat for all connected dbs.

    Syntax      :

    Description :

    Author(s)   : George Potemkin
    Created     : May 23, 2010
    Modified    : Jul 01, 2010
    Notes       : Use DbStatDump2.p (should be in PROPATH).
  ----------------------------------------------------------------------*/
/*PROPATH = PROPATH + ",D:\Proapps\WRK\DbStatDump".*/

/* Full path to the file that stores the last snapshot: */
DEFINE VARIABLE vLastSnapshot AS CHARACTER   NO-UNDO
  INITIAL "./DbStatDump.LastSnapshot.d":U.

DEFINE VARIABLE vErrorsLogFile AS CHARACTER NO-UNDO.

DEFINE STREAM DumpFile.

/* Define the shared temp-tables to store the most recent stats: */
{DbStatDump.i NEW}

/* *****************************  Functions  ************************** */

FUNCTION GetLocalHostName RETURNS CHARACTER:
  DEFINE VARIABLE vHostName AS CHARACTER NO-UNDO INITIAL 'unknown'.
  DEFINE VARIABLE vTcpName  AS CHARACTER NO-UNDO INITIAL ''.
  DEFINE VARIABLE vLength   AS INTEGER   NO-UNDO INITIAL 100.
  DEFINE VARIABLE vReturn   AS INTEGER   NO-UNDO INITIAL 0.

  IF OPSYS EQ "WIN32" THEN
  DO: /* Call Win32 routine to get host name */
    RUN gethostname(OUTPUT vTcpName,
                    INPUT  vLength,
                    OUTPUT vReturn).
    IF vReturn EQ 0 THEN
    ASSIGN vHostName = ENTRY(1, vTcpName, CHR(0)).
  END.
  ELSE DO:
    /* get UNIX host name */
    INPUT THROUGH uname -n.
    IMPORT vHostName.
    INPUT CLOSE.
  END.

  RETURN vHostName.
END FUNCTION. /* GetLocalHostName */

/* ***************************  Procedures  *************************** */

PROCEDURE gethostname EXTERNAL "wsock32.dll" :
   DEFINE OUTPUT PARAMETER p-Hostname AS CHARACTER.
   DEFINE INPUT  PARAMETER p-Length   AS LONG.
   DEFINE RETURN PARAMETER p-Return   AS LONG.
END PROCEDURE.

/* -------------------------------------------------------------------- */

PROCEDURE LoadDbStat.

  DEFINE VARIABLE vCurrSeek AS INTEGER NO-UNDO.
  DEFINE VARIABLE vPrevSeek AS INTEGER NO-UNDO.

  DEFINE VARIABLE vLastSnapshotDir AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vSlash AS CHARACTER NO-UNDO.
  ASSIGN vSlash = IF OPSYS EQ "Unix":U THEN "/":U ELSE "~\":U.

  IF SESSION:PARAMETER NE "":U THEN
  ASSIGN vLastSnapshot = SESSION:PARAMETER.

  ASSIGN
    vLastSnapshotDir = SUBSTRING(vLastSnapshot, 1,
                     MAX(R-INDEX(vLastSnapshot, "/":U), 
                         R-INDEX(vLastSnapshot, "~\":U)) - 1)
    vLastSnapshot = SUBSTRING(vLastSnapshot, 
                       LENGTH(vLastSnapshotDir) + 1)
  . /* ASSIGN */

  IF vLastSnapshotDir EQ "":U THEN
  ASSIGN vLastSnapshotDir = ".":U.

  IF vLastSnapshot BEGINS "/":U
  OR vLastSnapshot BEGINS "~\":U THEN
  ASSIGN vLastSnapshot = SUBSTRING(vLastSnapshot, 2).

  IF vLastSnapshot EQ "":U THEN
  ASSIGN vLastSnapshot = "DbStatDump.LastSnapshot.d":U.

  ASSIGN FILE-INFO:FILE-NAME = vLastSnapshotDir.
  IF  FILE-INFO:FULL-PATHNAME NE ?
  AND FILE-INFO:FILE-TYPE MATCHES "*D*":U
  THEN ASSIGN vLastSnapshot = FILE-INFO:FULL-PATHNAME + vSlash + vLastSnapshot.
  ELSE 
  DO:
    ASSIGN vLastSnapshot = ".":U + vSlash + vLastSnapshot.
    OUTPUT TO VALUE(vErrorsLogFile) APPEND.
    PUT UNFORMATTED
      "Dir " vLastSnapshotDir " does not exist. "
      "Last snapshot will be stored in " vLastSnapshot SKIP.
    OUTPUT CLOSE.
  END.
  
  ASSIGN vCurrSeek = ?
         vPrevSeek = ?.
  FILE-INFO:FILE-NAME = vLastSnapshot.
  IF  FILE-INFO:FULL-PATHNAME NE ?
  AND FILE-INFO:FILE-TYPE MATCHES "*F*":U
  AND FILE-INFO:FILE-TYPE MATCHES "*R*":U
  THEN DO:
    INPUT STREAM DumpFile FROM VALUE(FILE-INFO:FULL-PATHNAME).

/* Import previous DbStat: */
    REPEAT:
      CREATE ttDbStat.
      IMPORT STREAM DumpFile DELIMITER {&Sep}
             ttDbStat.
    END.
    DELETE   ttDbStat.

/* Import previous AreaStat: */
    IF SEEK(DumpFile) NE ? THEN
    REPEAT:
      CREATE ttAreaStat.
      IMPORT STREAM DumpFile DELIMITER {&Sep}
             ttAreaStat.
    END.
    DELETE   ttAreaStat.

/* Import previous TableStat: */
    IF SEEK(DumpFile) NE ? THEN
    REPEAT:
      CREATE ttTableStat.
      IMPORT STREAM DumpFile DELIMITER {&Sep}
             ttTableStat.
    END.
    DELETE   ttTableStat.

/* Import previous IndexStat: */
    IF SEEK(DumpFile) NE ? THEN
    REPEAT:
      CREATE ttIndexStat.
      IMPORT STREAM DumpFile DELIMITER {&Sep}
             ttIndexStat.
    END.
    DELETE   ttIndexStat.

/* Import previous ResrcStat: */
    IF SEEK(DumpFile) NE ? THEN
    REPEAT:
      CREATE ttResrcStat.
      IMPORT STREAM DumpFile DELIMITER {&Sep}
             ttResrcStat.
    END.
    DELETE   ttResrcStat.

    ASSIGN vCurrSeek = SEEK(DumpFile).
    IF vCurrSeek NE ? THEN
    DO ON ENDKEY UNDO, LEAVE:
      IMPORT STREAM DumpFile vPrevSeek.
    END.
    INPUT STREAM DumpFile CLOSE.

    IF vCurrSeek NE vPrevSeek THEN
    DO:
      OUTPUT TO VALUE(vErrorsLogFile) APPEND.
      PUT UNFORMATTED
        FILE-INFO:FULL-PATHNAME
        " has wrong size: now " vCurrSeek ", was " vPrevSeek SKIP.
      OUTPUT CLOSE.
    END.
  END. /* If LastSnapshot exists */
END PROCEDURE. /* LoadDbStat */

/* -------------------------------------------------------------------- */

PROCEDURE DumpDbStat.

  DEFINE VARIABLE vCurrSeek AS INTEGER NO-UNDO.

/* Export DbStat: */
  OUTPUT STREAM DumpFile TO VALUE (vLastSnapshot).
  FOR EACH ttDbStat NO-LOCK:
    EXPORT STREAM DumpFile DELIMITER {&Sep}
           ttDbStat.
  END.
  PUT STREAM DumpFile UNFORMATTED ".":U SKIP.

/* Export AreaStat: */
  FOR EACH ttAreaStat NO-LOCK:
    EXPORT STREAM DumpFile DELIMITER {&Sep}
           ttAreaStat.
  END.
  PUT STREAM DumpFile UNFORMATTED ".":U SKIP.

/* Export TableStat: */
  FOR EACH ttTableStat NO-LOCK
     WHERE ttTableStat.TableRead   NE 0
        OR ttTableStat.TableUpdate NE 0
        OR ttTableStat.TableCreate NE 0
        OR ttTableStat.TableDelete NE 0:
    EXPORT STREAM DumpFile DELIMITER {&Sep}
           ttTableStat.
  END.
  PUT STREAM DumpFile UNFORMATTED ".":U SKIP.

/* Export IndexStat: */
  FOR EACH ttIndexStat NO-LOCK
     WHERE ttIndexStat.IndexRead   NE 0
        OR ttIndexStat.IndexCreate NE 0
        OR ttIndexStat.IndexDelete NE 0:
    EXPORT STREAM DumpFile DELIMITER {&Sep}
           ttIndexStat.
  END.
  PUT STREAM DumpFile UNFORMATTED ".":U SKIP.

/* Export ResrcStat: */
  FOR EACH ttResrcStat NO-LOCK
     WHERE ttResrcStat.ResrcLock NE 0
        OR ttResrcStat.ResrcWait NE 0:
    EXPORT STREAM DumpFile DELIMITER {&Sep}
           ttResrcStat.
  END.
  PUT STREAM DumpFile UNFORMATTED ".":U          SKIP.
  ASSIGN vCurrSeek = SEEK(DumpFile).
  PUT STREAM DumpFile UNFORMATTED vCurrSeek SKIP.
  OUTPUT STREAM DumpFile CLOSE.

END PROCEDURE. /* DumpDbStat */

/* ***************************  Main Block  *************************** */

DEFINE VARIABLE vTimeStamp     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vDbID AS INTEGER NO-UNDO.


ASSIGN
  vTimeStamp = STRING(YEAR(TODAY))
             + STRING(MONTH(TODAY),"99":U)
             + STRING(DAY(TODAY),"99":U) + "_":U
             + REPLACE(STRING(TIME, "HH:MM:SS":U), ":":U, "":U)
  vErrorsLogFile = "DbStatDump.":U + vTimeStamp + ".Errors.log":U
. /* ASSIGN */

RUN LoadDbStat.

REPEAT vDbID = 1 TO NUM-DBS:
  CREATE ALIAS DICTDB FOR DATABASE VALUE(LDBNAME(vDbID)).
  RUN DbStatDump2.p( GetLocalHostName(), vTimeStamp).
END.

RUN DumpDbStat.
