       IDENTIFICATION DIVISION.
       PROGRAM-ID. SELECT01.
       AUTHOR. JULES.
       DATE-WRITTEN. 2025/11/11.
      ******************************************************************
      * PROGRAM: SELECT01
      * PURPOSE: SELECT RECORDS BASED ON A CONDITION
      * INPUT:   INPUT-SELECT.DAT (DEPARTMENT CODE, EMPLOYEE INFO)
      * OUTPUT:  OUTPUT-SELECT.DAT (SELECTED RECORDS)
      *          ERROR-SELECT.LST (RECORDS THAT DO NOT MATCH)
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "data/INPUT_SELECT.DAT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS FS-INPUT.
           SELECT OUTPUT-FILE ASSIGN TO "data/OUTPUT_SELECT.DAT"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS FS-OUTPUT.
           SELECT ERROR-FILE ASSIGN TO "data/ERROR_SELECT.LST"
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS IS FS-ERROR.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD.
           05  DEPT-CODE-I      PIC X(2).
           05  EMP-ID-I         PIC X(5).
           05  EMP-NAME-I       PIC X(20).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD      PIC X(27).

       FD  ERROR-FILE.
       01  ERROR-RECORD.
           05  ERROR-REC-DATA   PIC X(27).
           05  FILLER           PIC X(1) VALUE SPACES.
           05  ERROR-MSG        PIC X(20).


       WORKING-STORAGE SECTION.
       01  FILE-STATUS-CODES.
           05  FS-INPUT         PIC X(2).
               88 FS-INPUT-OK   VALUE "00".
               88 FS-INPUT-EOF  VALUE "10".
           05  FS-OUTPUT        PIC X(2).
               88 FS-OUTPUT-OK  VALUE "00".
           05  FS-ERROR         PIC X(2).
               88 FS-ERROR-OK   VALUE "00".

       01  WORK-AREAS.
           05  SELECT-DEPT-CODE PIC X(2) VALUE '01'.
           05  INPUT-REC-COUNT  PIC 9(5) VALUE 0.
           05  OUTPUT-REC-COUNT PIC 9(5) VALUE 0.
           05  ERROR-REC-COUNT  PIC 9(5) VALUE 0.


       PROCEDURE DIVISION.
       1000-MAIN.
           PERFORM 2000-INITIALIZE.
           PERFORM 3000-PROCESS-RECORDS UNTIL FS-INPUT-EOF.
           PERFORM 4000-TERMINATE.
           STOP RUN.

       2000-INITIALIZE.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE, ERROR-FILE.
           IF NOT FS-INPUT-OK
               DISPLAY "ERROR OPENING INPUT FILE: " FS-INPUT
               STOP RUN
           END-IF.
           IF NOT FS-OUTPUT-OK
               DISPLAY "ERROR OPENING OUTPUT FILE: " FS-OUTPUT
               STOP RUN
           END-IF.
           IF NOT FS-ERROR-OK
                DISPLAY "ERROR OPENING ERROR FILE: " FS-ERROR
                STOP RUN
           END-IF.
           PERFORM 9000-READ-INPUT.

       3000-PROCESS-RECORDS.
           IF DEPT-CODE-I = SELECT-DEPT-CODE
               PERFORM 3100-WRITE-OUTPUT
           ELSE
               PERFORM 3200-WRITE-ERROR
           END-IF.
           PERFORM 9000-READ-INPUT.


       3100-WRITE-OUTPUT.
           WRITE OUTPUT-RECORD FROM INPUT-RECORD.
           ADD 1 TO OUTPUT-REC-COUNT.

       3200-WRITE-ERROR.
           MOVE INPUT-RECORD TO ERROR-REC-DATA.
           MOVE "INVALID DEPT CODE" TO ERROR-MSG.
           WRITE ERROR-RECORD.
           ADD 1 TO ERROR-REC-COUNT.


       4000-TERMINATE.
           DISPLAY "PROCESSING COMPLETE".
           DISPLAY "INPUT RECORDS: " INPUT-REC-COUNT.
           DISPLAY "OUTPUT RECORDS: " OUTPUT-REC-COUNT.
           DISPLAY "ERROR RECORDS: " ERROR-REC-COUNT.
           CLOSE INPUT-FILE, OUTPUT-FILE, ERROR-FILE.

       9000-READ-INPUT.
           READ INPUT-FILE.
           IF FS-INPUT-EOF
               CONTINUE
           ELSE
               IF FS-INPUT-OK
                   ADD 1 TO INPUT-REC-COUNT
               ELSE
                   DISPLAY "ERROR READING INPUT FILE: " FS-INPUT
                   MOVE "99" TO FS-INPUT *> FORCE EOF
               END-IF
           END-IF.
