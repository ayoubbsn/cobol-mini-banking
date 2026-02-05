       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTMGT.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-FILE ASSIGN TO "DATA/accounts.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS ACCT-ID
               FILE STATUS IS WS-ACCT-STATUS.
               
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
           
       WORKING-STORAGE SECTION.
       01  WS-ACCT-STATUS      PIC X(02).
       
       LINKAGE SECTION.
       01  LS-OPERATION        PIC X(01).
           88 OP-CREATE        VALUE 'C'.
           88 OP-INQUIRY       VALUE 'I'.
           
       01  LS-ACCOUNT-DATA.
           05  L-ACCT-ID             PIC 9(05).
           05  L-ACCT-NAME           PIC X(30).
           05  L-ACCT-BALANCE        PIC S9(09)V99 COMP-3.
           05  L-ACCT-STATUS         PIC X(01).
               88 L-ACCT-ACTIVE      VALUE 'A'.
               88 L-ACCT-CLOSED      VALUE 'C'.
               88 L-ACCT-FROZEN      VALUE 'F'.
           05  L-ACCT-OPEN-DATE      PIC 9(08).
           05  L-FILLER              PIC X(20).
           
       01  LS-RESULT-CODE      PIC X(02).
           88 RES-OK           VALUE '00'.
           88 RES-DUP          VALUE '01'.
           88 RES-NOT-FOUND    VALUE '02'.
           88 RES-ERROR        VALUE '99'.
           
       PROCEDURE DIVISION USING LS-OPERATION LS-ACCOUNT-DATA LS-RESULT-CODE.
       MAIN-PROCEDURE.
           OPEN I-O ACCT-FILE.
           IF WS-ACCT-STATUS = "35" THEN
               OPEN OUTPUT ACCT-FILE
               CLOSE ACCT-FILE
               OPEN I-O ACCT-FILE
           END-IF.
           
           EVALUATE TRUE
               WHEN OP-CREATE
                   PERFORM CREATE-ACCOUNT
               WHEN OP-INQUIRY
                   PERFORM INQUIRY-ACCOUNT
               WHEN OTHER
                   MOVE '99' TO LS-RESULT-CODE
           END-EVALUATE.
           
           CLOSE ACCT-FILE.
           GOBACK.
           
       CREATE-ACCOUNT.
           MOVE L-ACCT-ID TO ACCT-ID OF ACCOUNT-RECORD.
           MOVE L-ACCT-NAME TO ACCT-NAME OF ACCOUNT-RECORD.
           MOVE L-ACCT-BALANCE TO ACCT-BALANCE OF ACCOUNT-RECORD.
           MOVE L-ACCT-STATUS TO ACCT-STATUS OF ACCOUNT-RECORD.
           MOVE L-ACCT-OPEN-DATE TO ACCT-OPEN-DATE OF ACCOUNT-RECORD.
           
           WRITE ACCOUNT-RECORD.
           
           EVALUATE WS-ACCT-STATUS
               WHEN "00"
                   MOVE '00' TO LS-RESULT-CODE
               WHEN "22"
                   MOVE '01' TO LS-RESULT-CODE
               WHEN OTHER
                   MOVE '99' TO LS-RESULT-CODE
           END-EVALUATE.

       INQUIRY-ACCOUNT.
           MOVE L-ACCT-ID TO ACCT-ID OF ACCOUNT-RECORD.
           READ ACCT-FILE KEY IS ACCT-ID
           
           EVALUATE WS-ACCT-STATUS
               WHEN "00"
                   MOVE ACCT-ID OF ACCOUNT-RECORD TO L-ACCT-ID
                   MOVE ACCT-NAME OF ACCOUNT-RECORD TO L-ACCT-NAME
                   MOVE ACCT-BALANCE OF ACCOUNT-RECORD TO L-ACCT-BALANCE
                   MOVE ACCT-STATUS OF ACCOUNT-RECORD TO L-ACCT-STATUS
                   MOVE ACCT-OPEN-DATE OF ACCOUNT-RECORD TO L-ACCT-OPEN-DATE
                   MOVE '00' TO LS-RESULT-CODE
               WHEN "23"
                   MOVE '02' TO LS-RESULT-CODE
               WHEN OTHER
                   MOVE '99' TO LS-RESULT-CODE
           END-EVALUATE.
