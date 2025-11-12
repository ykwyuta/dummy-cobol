
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MATCH01.
       AUTHOR. Jules.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MASTER-IN ASSIGN TO "data/matching/MASTER.DAT"
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT TRAN-IN ASSIGN TO "data/matching/TRAN.DAT"
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT NEW-MASTER-OUT ASSIGN TO "data/matching/NEWMAST.DAT"
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ERROR-REPORT ASSIGN TO "data/matching/ERROR.LST"
                  ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD MASTER-IN.
       01 MASTER-REC.
          05 M-KEY PIC 9(5).
          05 M-DATA PIC X(20).

       FD TRAN-IN.
       01 TRAN-REC.
          05 T-KEY PIC 9(5).
          05 T-MODE PIC X(1).
          05 T-DATA PIC X(20).

       FD NEW-MASTER-OUT.
       01 NEW-MASTER-REC.
          05 NM-KEY PIC 9(5).
          05 NM-DATA PIC X(20).

       FD ERROR-REPORT.
       01 ERROR-REC PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-EOF-FLAGS.
          05 WS-MASTER-EOF PIC X(1) VALUE 'N'.
          05 WS-TRAN-EOF PIC X(1) VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT MASTER-IN, TRAN-IN
                OUTPUT NEW-MASTER-OUT, ERROR-REPORT.

           PERFORM READ-MASTER.
           PERFORM READ-TRAN.

           PERFORM UNTIL WS-MASTER-EOF = 'Y' AND WS-TRAN-EOF = 'Y'
              EVALUATE TRUE
                 WHEN M-KEY < T-KEY AND WS-MASTER-EOF = 'N'
                    PERFORM WRITE-NEW-MASTER
                    PERFORM READ-MASTER
                 WHEN M-KEY > T-KEY AND WS-TRAN-EOF = 'N'
                    PERFORM PROCESS-NEW-RECORD
                    PERFORM READ-TRAN
                 WHEN M-KEY = T-KEY
                    PERFORM PROCESS-MATCH
                    PERFORM READ-MASTER
                    PERFORM READ-TRAN
                 WHEN WS-MASTER-EOF = 'N'
                    PERFORM WRITE-NEW-MASTER
                    PERFORM READ-MASTER
                 WHEN WS-TRAN-EOF = 'N'
                    PERFORM PROCESS-NEW-RECORD
                    PERFORM READ-TRAN
              END-EVALUATE
           END-PERFORM.

           CLOSE MASTER-IN, TRAN-IN, NEW-MASTER-OUT, ERROR-REPORT.
           STOP RUN.

       READ-MASTER.
           READ MASTER-IN
              AT END MOVE 'Y' TO WS-MASTER-EOF
                     MOVE HIGH-VALUE TO M-KEY
           END-READ.

       READ-TRAN.
           READ TRAN-IN
              AT END MOVE 'Y' TO WS-TRAN-EOF
                     MOVE HIGH-VALUE TO T-KEY
           END-READ.

       WRITE-NEW-MASTER.
           MOVE M-KEY TO NM-KEY.
           MOVE M-DATA TO NM-DATA.
           WRITE NEW-MASTER-REC.

       PROCESS-NEW-RECORD.
           IF T-MODE = 'A'
              MOVE T-KEY TO NM-KEY
              MOVE T-DATA TO NM-DATA
              WRITE NEW-MASTER-REC
           ELSE
              STRING "ERROR:Tran key " T-KEY " not found"
                 INTO ERROR-REC
              WRITE ERROR-REC
           END-IF.

       PROCESS-MATCH.
           EVALUATE T-MODE
              WHEN 'U'
                 MOVE T-KEY TO NM-KEY
                 MOVE T-DATA TO NM-DATA
                 WRITE NEW-MASTER-REC
              WHEN 'D'
                 CONTINUE
              WHEN OTHER
                 STRING "ERROR:Invalid mode " T-MODE " for key " T-KEY
                    INTO ERROR-REC
                 WRITE ERROR-REC
           END-EVALUATE.

       END PROGRAM MATCH01.
