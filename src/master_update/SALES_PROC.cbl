       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALES-PROC.
       AUTHOR. Jules.
      *
      * Main program for the master update sample. It reads daily
      * sales data, validates it, writes valid records to a history
      * file, and calls a subprogram to update the master file.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALES-FILE ASSIGN TO DSN-SALES
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-SALES.
           SELECT HISTORY-FILE ASSIGN TO DSN-HISTORY
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-HISTORY.
           SELECT ERROR-FILE ASSIGN TO DSN-ERROR
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-ERROR.
           SELECT ITEM-MASTER-FILE ASSIGN TO DSN-ITEM
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS IM-ITEM-CODE
               FILE STATUS IS FS-ITEM.

       DATA DIVISION.
       FILE SECTION.
           COPY "SALESREC.CPY".
           COPY "ITEMREC.CPY".

       WORKING-STORAGE SECTION.
       01  WS-FLAGS.
           05  WS-SALES-EOF-FLAG    PIC X VALUE 'N'.
               88  IS-SALES-EOF     VALUE 'Y'.
           05  WS-VALIDATION-FLAG   PIC X.
               88  IS-VALID         VALUE 'Y'.

       01  WS-FILE-STATUS.
           05  FS-SALES             PIC X(2).
           05  FS-HISTORY           PIC X(2).
           05  FS-ERROR             PIC X(2).
           05  FS-ITEM              PIC X(2).
       01  DSN-FIELDS.
           05 DSN-SALES         PIC X(34)
              VALUE "data/master_update/DAILY_SALES.DAT".
           05 DSN-HISTORY       PIC X(36)
              VALUE "data/master_update/SALES_HISTORY.DAT".
           05 DSN-ERROR         PIC X(35)
              VALUE "data/master_update/ERROR_SALES.LST".
           05 DSN-ITEM          PIC X(35)
              VALUE "data/master_update/MASTER_ITEM.IDX".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT SALES-FILE.
           OPEN OUTPUT HISTORY-FILE ERROR-FILE.

           PERFORM UNTIL IS-SALES-EOF
               READ SALES-FILE
                   AT END
                       SET IS-SALES-EOF TO TRUE
                   NOT AT END
                       PERFORM VALIDATE-AND-PROCESS
               END-READ
           END-PERFORM.

           CLOSE SALES-FILE HISTORY-FILE ERROR-FILE.
           STOP RUN.

       VALIDATE-AND-PROCESS.
           MOVE "Y" TO WS-VALIDATION-FLAG.

           IF SD-ITEM-CODE = SPACES
               MOVE "Item code is blank." TO ER-MESSAGE
               MOVE "N" TO WS-VALIDATION-FLAG
           ELSE IF SD-QTY <= 0
               MOVE "Sales quantity is not positive." TO ER-MESSAGE
               MOVE "N" TO WS-VALIDATION-FLAG
           END-IF.

           IF IS-VALID
              PERFORM WRITE-TO-HISTORY
              PERFORM UPDATE-INVENTORY
           ELSE
              PERFORM WRITE-TO-ERROR
           END-IF.

       WRITE-TO-HISTORY.
           MOVE SD-RECORD TO HISTORY-RECORD.
           WRITE HISTORY-RECORD.

       WRITE-TO-ERROR.
           MOVE SD-RECORD TO ER-ORIGINAL-RECORD.
           WRITE ERROR-RECORD.

       UPDATE-INVENTORY.
           CALL "INV_UPDATE" USING SD-ITEM-CODE, SD-QTY.

       END PROGRAM SALES-PROC.
