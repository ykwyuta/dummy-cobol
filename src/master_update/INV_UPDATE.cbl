       IDENTIFICATION DIVISION.
       PROGRAM-ID. INV_UPDATE.
       AUTHOR. Jules.
      *
      * Subprogram for the master update sample. It receives an
      * item code and quantity, then updates the stock in the
      * indexed master file.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ITEM-MASTER-FILE ASSIGN TO "data/master_update/MASTER_ITEM.IDX"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS IM-ITEM-CODE
               FILE STATUS IS FS-ITEM.

       DATA DIVISION.
       FILE SECTION.
           COPY "ITEMREC.CPY".

       WORKING-STORAGE SECTION.
       01  FS-ITEM                  PIC X(2).
       01  WS-CALC-QTY              PIC S9(7).

       LINKAGE SECTION.
       01  LK-ITEM-CODE             PIC X(8).
       01  LK-QTY                   PIC S9(5) SIGN IS LEADING SEPARATE.

       PROCEDURE DIVISION USING LK-ITEM-CODE, LK-QTY.
       MAIN-PROCEDURE.
           OPEN I-O ITEM-MASTER-FILE.

           IF FS-ITEM NOT = "00"
               DISPLAY "SUB: ERROR OPENING MASTER FILE: " FS-ITEM
               GO TO END-PROGRAM
           END-IF.

           MOVE LK-ITEM-CODE TO IM-ITEM-CODE.

           READ ITEM-MASTER-FILE
               INVALID KEY
                   DISPLAY "SUB: ITEM NOT FOUND. CODE: " LK-ITEM-CODE
                           ", STATUS: " FS-ITEM
                   GO TO END-UPDATE
           END-READ.

           MOVE LK-QTY TO WS-CALC-QTY.
           COMPUTE IM-STOCK-QTY = IM-STOCK-QTY - WS-CALC-QTY.

           REWRITE IM-RECORD
               INVALID KEY
                   DISPLAY "SUB: ERROR REWRITING RECORD. CODE: "
                           LK-ITEM-CODE ", STATUS: " FS-ITEM
           END-REWRITE.

       END-UPDATE.
           CLOSE ITEM-MASTER-FILE.

       END-PROGRAM.
           EXIT PROGRAM.
