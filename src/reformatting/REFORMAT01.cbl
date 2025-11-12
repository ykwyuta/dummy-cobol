       IDENTIFICATION DIVISION.
       PROGRAM-ID. REFORMAT01.
       AUTHOR. JULES.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO DSN-INPUT
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-INPUT.
           SELECT OUTPUT-FILE ASSIGN TO DSN-OUTPUT
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-OUTPUT.

       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD.
           05 IN-ID         PIC 9(5).
           05 IN-NAME       PIC X(20).
           05 IN-PHONE      PIC 9(9).

       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD.
           05 OUT-NAME      PIC X(20).
           05 OUT-ID        PIC 9(5).
           05 OUT-PHONE     PIC 9(9).

       WORKING-STORAGE SECTION.
       01  FILE-STATUS-CODES.
           05  FS-INPUT         PIC X(2).
               88 FS-INPUT-OK   VALUE "00".
               88 FS-INPUT-EOF  VALUE "10".
           05  FS-OUTPUT        PIC X(2).
               88 FS-OUTPUT-OK  VALUE "00".

       01  DSN-FIELDS.
           05 DSN-INPUT         PIC X(36)
              VALUE "data/reformatting/INPUT-REFORMAT.DAT".
           05 DSN-OUTPUT        PIC X(37)
              VALUE "data/reformatting/OUTPUT-REFORMAT.DAT".

       01  WS-EOF-FLAG      PIC X VALUE 'N'.
           88 WS-EOF               VALUE 'Y'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.

           PERFORM UNTIL WS-EOF
               READ INPUT-FILE
                   AT END
                       SET WS-EOF TO TRUE
                   NOT AT END
                       PERFORM PROCESS-RECORD
               END-READ
           END-PERFORM.

           CLOSE INPUT-FILE.
           CLOSE OUTPUT-FILE.

           STOP RUN.

       PROCESS-RECORD.
           MOVE IN-NAME TO OUT-NAME.
           MOVE IN-ID TO OUT-ID.
           MOVE IN-PHONE TO OUT-PHONE.
           WRITE OUTPUT-RECORD.
