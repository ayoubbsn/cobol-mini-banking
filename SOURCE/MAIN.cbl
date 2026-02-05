       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CHOICE           PIC X(01).
       01  WS-EXIT-FLAG        PIC X(01) VALUE 'N'.
       
       01  WS-ACCOUNT-DATA.
           05  ACCT-ID             PIC 9(05).
           05  ACCT-NAME           PIC X(30).
           05  ACCT-BALANCE        PIC S9(09)V99 COMP-3.
           05  ACCT-STATUS         PIC X(01).
               88 ACCT-ACTIVE      VALUE 'A'.
               88 ACCT-CLOSED      VALUE 'C'.
               88 ACCT-FROZEN      VALUE 'F'.
           05  ACCT-OPEN-DATE      PIC 9(08).
           05  FILLER              PIC X(20).
           
       01  WS-TXN-ARGS.
           05  WS-TXN-TYPE         PIC X(01).
           05  WS-TXN-ACCT-ID      PIC 9(05).
           05  WS-TXN-TARGET-ID    PIC 9(05).
           05  WS-TXN-AMOUNT       PIC 9(09)V99.
           
       01  WS-RESULT-CODE      PIC X(02).
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM UNTIL WS-EXIT-FLAG = 'Y'
               DISPLAY "========================================"
               DISPLAY "      COBOL BANKING SYSTEM v1.0"
               DISPLAY "========================================"
               DISPLAY "1. Create Account"
               DISPLAY "2. Account Inquiry"
               DISPLAY "3. Deposit"
               DISPLAY "4. Withdraw"
               DISPLAY "5. Transfer"
               DISPLAY "6. Exit"
               DISPLAY "Enter Selection: " WITH NO ADVANCING
               ACCEPT WS-CHOICE
               
               EVALUATE WS-CHOICE
                   WHEN '1'
                       PERFORM DO-CREATE
                   WHEN '2'
                       PERFORM DO-INQUIRY
                   WHEN '3'
                       PERFORM DO-DEPOSIT
                   WHEN '4'
                       PERFORM DO-WITHDRAW
                   WHEN '5'
                       PERFORM DO-TRANSFER
                   WHEN '6'
                       MOVE 'Y' TO WS-EXIT-FLAG
                   WHEN OTHER
                       DISPLAY "Invalid Option!"
               END-EVALUATE
               
               DISPLAY "Press Enter to continue..."
               ACCEPT WS-CHOICE
               call "system" using "cls"
           END-PERFORM.
           
           STOP RUN.
           
       DO-CREATE.
           DISPLAY "Create New Account".
           DISPLAY "Enter ID (5 digits): " WITH NO ADVANCING.
           ACCEPT ACCT-ID OF WS-ACCOUNT-DATA.
           DISPLAY "Enter Name (30 chars): " WITH NO ADVANCING.
           ACCEPT ACCT-NAME OF WS-ACCOUNT-DATA.
           MOVE 0 TO ACCT-BALANCE OF WS-ACCOUNT-DATA.
           MOVE 'A' TO ACCT-STATUS OF WS-ACCOUNT-DATA.
           MOVE FUNCTION CURRENT-DATE(1:8) TO ACCT-OPEN-DATE OF WS-ACCOUNT-DATA.
           
           CALL "ACCTMGT" USING BY CONTENT "C" 
                                BY CONTENT WS-ACCOUNT-DATA 
                                BY REFERENCE WS-RESULT-CODE.
                                
           IF WS-RESULT-CODE = '00'
               DISPLAY "Account Created Successfully!"
           ELSE
               DISPLAY "Error Creating Account. Code: " WS-RESULT-CODE
           END-IF.

       DO-INQUIRY.
           DISPLAY "Account Inquiry".
           DISPLAY "Enter ID (5 digits): " WITH NO ADVANCING.
           ACCEPT ACCT-ID OF WS-ACCOUNT-DATA.
           
           CALL "ACCTMGT" USING BY CONTENT "I" 
                                BY REFERENCE WS-ACCOUNT-DATA 
                                BY REFERENCE WS-RESULT-CODE.
                                
           IF WS-RESULT-CODE = '00'
               DISPLAY "Name:    " ACCT-NAME OF WS-ACCOUNT-DATA
               DISPLAY "Balance: " ACCT-BALANCE OF WS-ACCOUNT-DATA
               DISPLAY "Status:  " ACCT-STATUS OF WS-ACCOUNT-DATA
           ELSE
               DISPLAY "Account Not Found or Error. Code: " WS-RESULT-CODE
           END-IF.

       DO-DEPOSIT.
           MOVE 'D' TO WS-TXN-TYPE.
           PERFORM GET-TXN-COMMON-INPUT.
           
           CALL "TXNPROC" USING BY CONTENT WS-TXN-TYPE
                                BY CONTENT WS-TXN-ACCT-ID
                                BY CONTENT WS-TXN-TARGET-ID
                                BY CONTENT WS-TXN-AMOUNT
                                BY REFERENCE WS-RESULT-CODE.
           PERFORM SHOW-RESULT.

       DO-WITHDRAW.
           MOVE 'W' TO WS-TXN-TYPE.
           PERFORM GET-TXN-COMMON-INPUT.
           
           CALL "TXNPROC" USING BY CONTENT WS-TXN-TYPE
                                BY CONTENT WS-TXN-ACCT-ID
                                BY CONTENT WS-TXN-TARGET-ID
                                BY CONTENT WS-TXN-AMOUNT
                                BY REFERENCE WS-RESULT-CODE.
           PERFORM SHOW-RESULT.

       DO-TRANSFER.
           MOVE 'T' TO WS-TXN-TYPE.
           DISPLAY "Source Account ID: " WITH NO ADVANCING.
           ACCEPT WS-TXN-ACCT-ID.
           DISPLAY "Target Account ID: " WITH NO ADVANCING.
           ACCEPT WS-TXN-TARGET-ID.
           DISPLAY "Amount: " WITH NO ADVANCING.
           ACCEPT WS-TXN-AMOUNT.
           
           CALL "TXNPROC" USING BY CONTENT WS-TXN-TYPE
                                BY CONTENT WS-TXN-ACCT-ID
                                BY CONTENT WS-TXN-TARGET-ID
                                BY CONTENT WS-TXN-AMOUNT
                                BY REFERENCE WS-RESULT-CODE.
           PERFORM SHOW-RESULT.

       GET-TXN-COMMON-INPUT.
           DISPLAY "Account ID: " WITH NO ADVANCING.
           ACCEPT WS-TXN-ACCT-ID.
           MOVE 0 TO WS-TXN-TARGET-ID.
           DISPLAY "Amount: " WITH NO ADVANCING.
           ACCEPT WS-TXN-AMOUNT.
           
       SHOW-RESULT.
           IF WS-RESULT-CODE = '00'
               DISPLAY "Transaction Successful!"
           ELSE
               IF WS-RESULT-CODE = '10'
                   DISPLAY "Insufficient Funds!"
               ELSE
                   DISPLAY "Error: " WS-RESULT-CODE
               END-IF
           END-IF.
