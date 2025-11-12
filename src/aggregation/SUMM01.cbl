       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUMM01.
       AUTHOR. JULES.
       DATE-WRITTEN. 2025/11/11.
      ******************************************************************
      * PROGRAM: SUMM01
      * PURPOSE: AGGREGATE SALES DATA BY DEPARTMENT
      * INPUT:   INPUT-SUMM.DAT (DEPARTMENT CODE, SALES AMOUNT)
      * OUTPUT:  OUTPUT-SUMM.DAT (DEPARTMENT CODE, TOTAL SALES)
      *          CONTROL-LIST.DAT (CONTROL TOTALS)
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "data/aggregation/INPUT_SUMM.DA"
      -    "T"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS FS-INPUT.
           SELECT OUTPUT-FILE ASSIGN TO "data/aggregation/OUTPUT_SUMM.D"
      -    "AT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS FS-OUTPUT.
           SELECT CONTROL-LIST ASSIGN TO "data/aggregation/CONTROL_LIST"
      -    ".DAT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS FS-CONTROL.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD.
           05  DEPT-CODE-I      PIC X(2).
           05  SALES-AMOUNT-I   PIC 9(8).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD.
           05  DEPT-CODE-O      PIC X(2).
           05  FILLER           PIC X(1) VALUE ','.
           05  TOTAL-SALES-O    PIC 9(10).

       FD  CONTROL-LIST.
       01  CONTROL-RECORD     PIC X(80).


       WORKING-STORAGE SECTION.
       01  FILE-STATUS-CODES.
           05  FS-INPUT         PIC X(2).
               88 FS-INPUT-OK   VALUE "00".
               88 FS-INPUT-EOF  VALUE "10".
           05  FS-OUTPUT        PIC X(2).
               88 FS-OUTPUT-OK  VALUE "00".
           05  FS-CONTROL       PIC X(2).
               88 FS-CONTROL-OK VALUE "00".

       01  WORK-AREAS.
           05  PREV-DEPT-CODE   PIC X(2) VALUE SPACES.
           05  DEPT-TOTAL       PIC S9(10) VALUE 0.
           05  GRAND-TOTAL      PIC S9(12) VALUE 0.
           05  INPUT-REC-COUNT  PIC 9(5) VALUE 0.
           05  OUTPUT-REC-COUNT PIC 9(5) VALUE 0.

       01  REPORT-HEADER.
           05 FILLER PIC X(80) VALUE "        SALES AGGREGATION REPORT".

       01  REPORT-TOTALS.
           05 FILLER           PIC X(20) VALUE "INPUT RECORD COUNT: ".
           05 RPT-IN-COUNT     PIC ZZZZ9.
           05 FILLER           PIC X(21) VALUE " OUTPUT RECORD COUNT: ".
           05 RPT-OUT-COUNT    PIC ZZZZ9.
           05 FILLER           PIC X(15) VALUE " GRAND TOTAL: ".
           05 RPT-GRAND-TOTAL  PIC ZZZ,ZZZ,ZZ9.

       PROCEDURE DIVISION.
       1000-MAIN.
           PERFORM 2000-INITIALIZE.
           PERFORM 3000-PROCESS-RECORDS UNTIL FS-INPUT-EOF.
           PERFORM 4000-TERMINATE.
           STOP RUN.

       2000-INITIALIZE.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE, CONTROL-LIST.
           IF NOT FS-INPUT-OK
               DISPLAY "ERROR OPENING INPUT FILE: " FS-INPUT
               STOP RUN
           END-IF.
           IF NOT FS-OUTPUT-OK
               DISPLAY "ERROR OPENING OUTPUT FILE: " FS-OUTPUT
               STOP RUN
           END-IF.
           IF NOT FS-CONTROL-OK
                DISPLAY "ERROR OPENING CONTROL LIST: " FS-CONTROL
                STOP RUN
           END-IF.
           PERFORM 9000-READ-INPUT.

       3000-PROCESS-RECORDS.
           IF PREV-DEPT-CODE = SPACES
               MOVE DEPT-CODE-I TO PREV-DEPT-CODE
           END-IF.

           IF DEPT-CODE-I NOT = PREV-DEPT-CODE
               PERFORM 3100-WRITE-DEPT-TOTAL
           END-IF.

           ADD SALES-AMOUNT-I TO DEPT-TOTAL.
           PERFORM 9000-READ-INPUT.


       3100-WRITE-DEPT-TOTAL.
           MOVE PREV-DEPT-CODE TO DEPT-CODE-O.
           MOVE DEPT-TOTAL TO TOTAL-SALES-O.
           WRITE OUTPUT-RECORD.
           ADD 1 TO OUTPUT-REC-COUNT.
           ADD DEPT-TOTAL TO GRAND-TOTAL.
           MOVE 0 TO DEPT-TOTAL.
           MOVE DEPT-CODE-I TO PREV-DEPT-CODE.


       4000-TERMINATE.
           PERFORM 3100-WRITE-DEPT-TOTAL. *> Write last department
           PERFORM 5000-WRITE-CONTROL-TOTALS.
           CLOSE INPUT-FILE, OUTPUT-FILE, CONTROL-LIST.

       5000-WRITE-CONTROL-TOTALS.
            WRITE CONTROL-RECORD FROM REPORT-HEADER.
            MOVE INPUT-REC-COUNT TO RPT-IN-COUNT.
            MOVE OUTPUT-REC-COUNT TO RPT-OUT-COUNT.
            MOVE GRAND-TOTAL TO RPT-GRAND-TOTAL.
            WRITE CONTROL-RECORD FROM REPORT-TOTALS.

       9000-READ-INPUT.
           READ INPUT-FILE.
           IF FS-INPUT-OK
               ADD 1 TO INPUT-REC-COUNT
           END-IF.
