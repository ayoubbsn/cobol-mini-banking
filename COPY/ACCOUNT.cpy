           05  ACCT-ID             PIC 9(05).
           05  ACCT-NAME           PIC X(30).
           05  ACCT-BALANCE        PIC S9(09)V99 COMP-3.
           05  ACCT-STATUS         PIC X(01).
               88 ACCT-ACTIVE      VALUE 'A'.
               88 ACCT-CLOSED      VALUE 'C'.
               88 ACCT-FROZEN      VALUE 'F'.
           05  ACCT-OPEN-DATE      PIC 9(08).
           05  FILLER              PIC X(20).
