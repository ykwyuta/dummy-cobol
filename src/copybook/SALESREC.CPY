       *> SALESREC.CPY
       *> This copybook defines the layout of the sales data record.

       FD  SALES-FILE.
       01  SD-RECORD.
           05  SD-ITEM-CODE         PIC X(8).
           05  SD-SALE-DATE         PIC 9(8).
           05  SD-QTY               PIC S9(5) SIGN IS LEADING SEPARATE.
           05  SD-PRICE             PIC 9(7)V99.

       FD  HISTORY-FILE.
       01  HISTORY-RECORD           PIC X(31).

       FD  ERROR-FILE.
       01  ERROR-RECORD.
           05  ER-ORIGINAL-RECORD   PIC X(31).
           05  FILLER               PIC X(2).
           05  ER-MESSAGE           PIC X(50).
