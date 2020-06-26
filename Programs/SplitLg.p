DEFINE VARIABLE vInpFile  AS CHARACTER NO-UNDO
  INITIAL "D:\Support\Host\2008.03.18\bank\bank.lg".
DEFINE VARIABLE vOutFile  AS CHARACTER NO-UNDO.
DEFINE VARIABLE vPartNum  AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE vLine     AS CHARACTER NO-UNDO.

ASSIGN vOutFile = vInpFile + "." + STRING(vPartNum,"999") + ".begining.lg".

INPUT FROM VALUE(vInpFile).
OUTPUT  TO VALUE(vOutFile).
REPEAT:
  IMPORT UNFORMATTED vLine.
  IF vLine MATCHES "* (333) *" THEN
  DO:
/* 12345678901234567890
   [2008/03/18@18:16:42.981+0500] P-175287     T--1071947776 I
    BROKER  0: (333)   Multi-user session begin. */
    OUTPUT CLOSE.
    ASSIGN vPartNum = vPartNum + 1.
    ASSIGN vOutFile = vInpFile + "." + STRING(vPartNum,"999") + ".":U +
      SUBSTRING(vLine,  2,4) + 
      SUBSTRING(vLine,  7,2) + 
      SUBSTRING(vLine, 10,2) + ".":U +
      SUBSTRING(vLine, 13,2) + 
      SUBSTRING(vLine, 16,2) + 
      SUBSTRING(vLine, 19,2) + ".lg":U.
    OUTPUT TO VALUE(vOutFile).
  END.
  PUT UNFORMATTED vLine SKIP.
END.
OUTPUT CLOSE.
INPUT CLOSE.
