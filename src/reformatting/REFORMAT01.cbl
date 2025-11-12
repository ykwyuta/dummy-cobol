       IDENTIFICATION DIVISION.
       PROGRAM-ID. REFORMAT01.
       AUTHOR. JULES.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO
               "data/reformatting/INPUT-REFORMAT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO
               "data/reformatting/OUTPUT-REFORMAT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

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
