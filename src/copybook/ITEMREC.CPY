       *> ITEMREC.CPY
       *> Defines the record layout for the item master file.
       FD  ITEM-MASTER-FILE.
       01  IM-RECORD.
           05  IM-ITEM-CODE         PIC X(8).
           05  IM-ITEM-NAME         PIC X(30).
           05  IM-STOCK-QTY         PIC S9(7).
           05  IM-UNIT-PRICE        PIC 9(7)V99.
