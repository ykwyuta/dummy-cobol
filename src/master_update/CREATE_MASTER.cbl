       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREATE-MASTER.
       AUTHOR. Jules.
      *
      * This program reads a sequential master data file and
      * creates an indexed master file from it. It's a utility
      * for the master update sample.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-MASTER-FILE ASSIGN TO "data/master_update/initial_master.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ITEM-MASTER-FILE ASSIGN TO "data/master_update/MASTER_ITEM.IDX"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS IM-ITEM-CODE
               FILE STATUS IS ITEM-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  SEQ-MASTER-FILE.
       01  SEQ-MASTER-RECORD.
           05  SM-ITEM-CODE         PIC X(8).
           05  SM-ITEM-NAME         PIC X(30).
           05  SM-STOCK-QTY         PIC S9(7).
           05  SM-UNIT-PRICE        PIC 9(7)V99.

       COPY "ITEMREC.CPY".

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  ITEM-FILE-STATUS     PIC X(2).
       01  WS-EOF-FLAG              PIC X VALUE 'N'.
           88  IS-EOF               VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT SEQ-MASTER-FILE.
           OPEN OUTPUT ITEM-MASTER-FILE.

           IF ITEM-FILE-STATUS NOT = "00"
              DISPLAY "ERROR OPENING MASTER FILE: " ITEM-FILE-STATUS
              GO TO END-PROGRAM
           END-IF.

           PERFORM UNTIL IS-EOF
               READ SEQ-MASTER-FILE
                   AT END
                       SET IS-EOF TO TRUE
                   NOT AT END
                       PERFORM PROCESS-RECORD
               END-READ
           END-PERFORM.

       END-PROGRAM.
           CLOSE SEQ-MASTER-FILE
                 ITEM-MASTER-FILE.
           STOP RUN.

       PROCESS-RECORD.
           MOVE SM-ITEM-CODE TO IM-ITEM-CODE.
           MOVE SM-ITEM-NAME TO IM-ITEM-NAME.
           MOVE SM-STOCK-QTY TO IM-STOCK-QTY.
           MOVE SM-UNIT-PRICE TO IM-UNIT-PRICE.

           WRITE IM-RECORD
               INVALID KEY
                   DISPLAY "ERROR WRITING MASTER: " IM-ITEM-CODE
                   DISPLAY "FILE STATUS: " ITEM-FILE-STATUS
           END-WRITE.
