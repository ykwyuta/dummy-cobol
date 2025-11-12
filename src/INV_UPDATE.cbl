       IDENTIFICATION DIVISION.
       PROGRAM-ID. INV_UPDATE.
       AUTHOR. Jules.
      *
      * This is a subprogram to update the stock quantity in the
      * item master file. It is called by the main program.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ITEM-MASTER-FILE ASSIGN TO "data/MASTER_ITEM.IDX"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS IM-ITEM-CODE
               FILE STATUS IS ITEM-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
           COPY "ITEMREC.CPY".

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  ITEM-FILE-STATUS     PIC X(2).
       01  WS-CALC-QTY              PIC S9(7).

       LINKAGE SECTION.
       01  LK-ITEM-CODE             PIC X(8).
       01  LK-QTY                   PIC S9(5) SIGN IS LEADING SEPARATE.

       PROCEDURE DIVISION USING LK-ITEM-CODE, LK-QTY.
       MAIN-PROCEDURE.
           OPEN I-O ITEM-MASTER-FILE.

           IF ITEM-FILE-STATUS NOT = "00"
               DISPLAY "ERROR OPENING MASTER FILE IN SUB: "
                       ITEM-FILE-STATUS
               GO TO END-PROGRAM
           END-IF.

           MOVE LK-ITEM-CODE TO IM-ITEM-CODE.

           READ ITEM-MASTER-FILE
               INVALID KEY
                   DISPLAY "SUB: ITEM NOT FOUND. CODE: " LK-ITEM-CODE
                           ", STATUS: " ITEM-FILE-STATUS
                   GO TO END-UPDATE
           END-READ.

           MOVE LK-QTY TO WS-CALC-QTY.
           COMPUTE IM-STOCK-QTY = IM-STOCK-QTY - WS-CALC-QTY.

           REWRITE IM-RECORD
               INVALID KEY
                   DISPLAY "SUB: ERROR REWRITING RECORD. CODE: "
                           LK-ITEM-CODE ", STATUS: " ITEM-FILE-STATUS
           END-REWRITE.

       END-UPDATE.
           CLOSE ITEM-MASTER-FILE.

       END-PROGRAM.
           EXIT PROGRAM.
