       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORT01.
       AUTHOR. Jules.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "data/INPUT-SORT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "data/OUTPUT-SORT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT SORT-WORK-FILE ASSIGN TO "SORTWORK.TMP".
      *
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD            PIC X(80).
      *
       FD  OUTPUT-FILE.
       01  OUTPUT-RECORD           PIC X(80).
      *
       SD  SORT-WORK-FILE.
       01  SORT-WORK-RECORD.
           05  SORT-KEY            PIC X(05).
           05  SORT-DATA           PIC X(75).
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           SORT SORT-WORK-FILE
               ON ASCENDING KEY SORT-KEY
               USING INPUT-FILE
               GIVING OUTPUT-FILE.
      *
           STOP RUN.
      *
       END PROGRAM SORT01.
