       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALES-PROC.
       AUTHOR. Jules.
      *
      * This is the main program. It processes daily sales data,
      * validates records, writes them to a history file, and
      * calls a subprogram to update the inventory master.
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "FILEDEF.CPY".

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
           05  ITEM-FILE-STATUS     PIC X(2).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT SALES-FILE.
           OPEN OUTPUT HISTORY-FILE.
           OPEN OUTPUT ERROR-FILE.

           PERFORM UNTIL IS-SALES-EOF
               READ SALES-FILE
                   AT END
                       SET IS-SALES-EOF TO TRUE
                   NOT AT END
                       PERFORM VALIDATE-AND-PROCESS
               END-READ
           END-PERFORM.

       END-PROGRAM.
           CLOSE SALES-FILE
                 HISTORY-FILE
                 ERROR-FILE.
           STOP RUN.

       VALIDATE-AND-PROCESS.
           MOVE "Y" TO WS-VALIDATION-FLAG.
      * Data Validation
           IF SD-ITEM-CODE = SPACES
               MOVE "Item code is blank." TO ER-MESSAGE
               MOVE "N" TO WS-VALIDATION-FLAG
           ELSE IF SD-QTY <= 0
               MOVE "Sales quantity is not positive." TO ER-MESSAGE
               MOVE "N" TO WS-VALIDATION-FLAG
           END-IF.

      * Process based on validation result
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
