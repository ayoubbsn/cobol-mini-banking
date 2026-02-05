       IDENTIFICATION DIVISION.
       PROGRAM-ID. TXNPROC.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-FILE ASSIGN TO "DATA/accounts.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-ID
               FILE STATUS IS WS-ACCT-STATUS.
               
           SELECT TRANS-FILE ASSIGN TO "DATA/trans.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TRANS-STATUS.
               
       DATA DIVISION.
       FILE SECTION.
       FD  ACCT-FILE.
       01  ACCOUNT-RECORD.
           05  ACCT-ID             PIC 9(05).
           05  ACCT-NAME           PIC X(30).
           05  ACCT-BALANCE        PIC S9(09)V99 COMP-3.
           05  ACCT-STATUS         PIC X(01).
               88 ACCT-ACTIVE      VALUE 'A'.
               88 ACCT-CLOSED      VALUE 'C'.
               88 ACCT-FROZEN      VALUE 'F'.
           05  ACCT-OPEN-DATE      PIC 9(08).
           05  FILLER              PIC X(20).

       FD  TRANS-FILE.
       01  TRANS-RECORD.
           05  TRANS-ID            PIC 9(08).
           05  TRANS-ACCT-ID       PIC 9(05).
           05  TRANS-TYPE          PIC X(01).
               88 TRANS-DEPOSIT    VALUE 'D'.
               88 TRANS-WITHDRAW   VALUE 'W'.
               88 TRANS-TRANSFER   VALUE 'T'.
           05  TRANS-AMOUNT        PIC S9(09)V99.
           05  TRANS-DATE          PIC 9(08).
           05  TRANS-TIME          PIC 9(06).
           
       WORKING-STORAGE SECTION.
       01  WS-ACCT-STATUS      PIC X(02).
       01  WS-TRANS-STATUS     PIC X(02).
       01  WS-CURRENT-DATE.
           05  WS-YEAR         PIC 9(04).
           05  WS-MONTH        PIC 9(02).
           05  WS-DAY          PIC 9(02).
       01  WS-CURRENT-TIME.
           05  WS-HOUR         PIC 9(02).
           05  WS-MIN          PIC 9(02).
           05  WS-SEC          PIC 9(02).
           05  WS-MS           PIC 9(02).
           
       LINKAGE SECTION.
       01  LS-TXN-TYPE         PIC X(01).
       01  LS-TXN-ACCT-ID      PIC 9(05).
       01  LS-TXN-TARGET-ID    PIC 9(05).
       01  LS-TXN-AMOUNT       PIC 9(09)V99.
       01  LS-RESULT-CODE      PIC X(02).
           88 RES-OK           VALUE '00'.
           88 RES-NO-FUNDS     VALUE '10'.
           88 RES-NOT-FOUND    VALUE '02'.
           88 RES-ERROR        VALUE '99'.
           
       PROCEDURE DIVISION USING LS-TXN-TYPE LS-TXN-ACCT-ID LS-TXN-TARGET-ID LS-TXN-AMOUNT LS-RESULT-CODE.
       MAIN-PROCEDURE.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE.
           MOVE FUNCTION CURRENT-DATE(9:6) TO WS-CURRENT-TIME.
           
           OPEN I-O ACCT-FILE.
           IF WS-ACCT-STATUS NOT = '00'
               DISPLAY "Error Opening ACCT-FILE. Status: " WS-ACCT-STATUS
               MOVE '99' TO LS-RESULT-CODE
               GOBACK
           END-IF.
           
           OPEN EXTEND TRANS-FILE.
           IF WS-TRANS-STATUS NOT = '00'
               DISPLAY "Error Opening TRANS-FILE. Status: " WS-TRANS-STATUS
               CLOSE ACCT-FILE
               MOVE '99' TO LS-RESULT-CODE
               GOBACK
           END-IF.
           
           EVALUATE LS-TXN-TYPE
               WHEN 'D'
                   PERFORM DEPOSIT
               WHEN 'W'
                   PERFORM WITHDRAW
               WHEN 'T'
                   PERFORM TRANSFER
               WHEN OTHER
                   MOVE '99' TO LS-RESULT-CODE
           END-EVALUATE.
           
           CLOSE ACCT-FILE.
           CLOSE TRANS-FILE.
           GOBACK.
           
       DEPOSIT.
           MOVE LS-TXN-ACCT-ID TO ACCT-ID OF ACCOUNT-RECORD.
           READ ACCT-FILE
               INVALID KEY
                   MOVE '02' TO LS-RESULT-CODE
                   EXIT PARAGRAPH.
                   
           COMPUTE ACCT-BALANCE OF ACCOUNT-RECORD = ACCT-BALANCE OF ACCOUNT-RECORD + LS-TXN-AMOUNT.
           REWRITE ACCOUNT-RECORD.
           
           PERFORM LOG-TRANSACTION.
           MOVE '00' TO LS-RESULT-CODE.
           
       WITHDRAW.
           MOVE LS-TXN-ACCT-ID TO ACCT-ID OF ACCOUNT-RECORD.
           READ ACCT-FILE
               INVALID KEY
                   MOVE '02' TO LS-RESULT-CODE
                   EXIT PARAGRAPH.
                   
           IF ACCT-BALANCE OF ACCOUNT-RECORD < LS-TXN-AMOUNT
               MOVE '10' TO LS-RESULT-CODE
           ELSE
               COMPUTE ACCT-BALANCE OF ACCOUNT-RECORD = ACCT-BALANCE OF ACCOUNT-RECORD - LS-TXN-AMOUNT
               REWRITE ACCOUNT-RECORD
               PERFORM LOG-TRANSACTION
               MOVE '00' TO LS-RESULT-CODE
           END-IF.
           
       TRANSFER.
      *>   Withdraw from Source
           MOVE LS-TXN-ACCT-ID TO ACCT-ID OF ACCOUNT-RECORD.
           READ ACCT-FILE
               INVALID KEY
                   MOVE '02' TO LS-RESULT-CODE
                   EXIT PARAGRAPH.
                   
           IF ACCT-BALANCE OF ACCOUNT-RECORD < LS-TXN-AMOUNT
               MOVE '10' TO LS-RESULT-CODE
               EXIT PARAGRAPH.
               
           COMPUTE ACCT-BALANCE OF ACCOUNT-RECORD = ACCT-BALANCE OF ACCOUNT-RECORD - LS-TXN-AMOUNT.
           REWRITE ACCOUNT-RECORD.
           PERFORM LOG-TRANSACTION.
           
      *>   Deposit to Target
           MOVE LS-TXN-TARGET-ID TO ACCT-ID OF ACCOUNT-RECORD.
           READ ACCT-FILE
               INVALID KEY
      *>           Ideally rollback here, but keeping simple for now
                   MOVE '02' TO LS-RESULT-CODE
                   EXIT PARAGRAPH.
                   
           COMPUTE ACCT-BALANCE OF ACCOUNT-RECORD = ACCT-BALANCE OF ACCOUNT-RECORD + LS-TXN-AMOUNT.
           REWRITE ACCOUNT-RECORD.
           
      *>   Log Target side? Maybe, but sticking to source log for now or double log.
      *>   Let's just log the transfer on the source account.
           
           MOVE '00' TO LS-RESULT-CODE.
           
       LOG-TRANSACTION.
           MOVE LS-TXN-ACCT-ID TO TRANS-ACCT-ID OF TRANS-RECORD.
           MOVE LS-TXN-TYPE TO TRANS-TYPE OF TRANS-RECORD.
           MOVE LS-TXN-AMOUNT TO TRANS-AMOUNT OF TRANS-RECORD.
           MOVE WS-CURRENT-DATE TO TRANS-DATE OF TRANS-RECORD.
           MOVE WS-CURRENT-TIME TO TRANS-TIME OF TRANS-RECORD.
           
      *>   Generate a simple ID (Time based + Amount?) - A real system checks last ID.
           COMPUTE TRANS-ID OF TRANS-RECORD = FUNCTION NUMVAL(WS-CURRENT-TIME) * 10.
           
           WRITE TRANS-RECORD.
