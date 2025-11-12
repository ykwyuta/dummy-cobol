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
           SELECT SEQ-MASTER-FILE ASSIGN TO DSN-SEQ-MASTER
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-SEQ-MASTER.

           SELECT ITEM-MASTER-FILE ASSIGN TO DSN-ITEM-MASTER
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS IM-ITEM-CODE
               FILE STATUS IS FS-ITEM-MASTER.

       DATA DIVISION.
       FILE SECTION.
       FD  SEQ-MASTER-FILE.
       01  SEQ-MASTER-RECORD    PIC X(54).
       FD  ITEM-MASTER-FILE.
       01  ITEM-MASTER-RECORD   PIC X(54).


       WORKING-STORAGE SECTION.
       COPY "ITEMREC.CPY".

       01  WS-FILE-STATUS.
           05  FS-SEQ-MASTER    PIC X(2).
           05  FS-ITEM-MASTER   PIC X(2).

       01  DSN-FIELDS.
           05 DSN-SEQ-MASTER    PIC X(36)
              VALUE "data/master_update/initial_master.dat".
           05 DSN-ITEM-MASTER   PIC X(33)
              VALUE "data/master_update/MASTER_ITEM.IDX".

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

       END PROGRAM CREATE-MASTER.
