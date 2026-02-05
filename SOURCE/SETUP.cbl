       IDENTIFICATION DIVISION.
       PROGRAM-ID. SETUP.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-OPERATION        PIC X(01) VALUE 'C'.
       01  WS-ACCOUNT-DATA.
           05  WS-ACCT-ID             PIC 9(05) VALUE 10001.
           05  WS-ACCT-NAME           PIC X(30) VALUE 'TEST USER'.
           05  WS-ACCT-BALANCE        PIC S9(09)V99 COMP-3 VALUE 1000.00.
           05  WS-ACCT-STATUS         PIC X(01) VALUE 'A'.
           05  WS-ACCT-OPEN-DATE      PIC 9(08) VALUE 20230101.
           05  WS-FILLER              PIC X(20) VALUE SPACES.
       01  WS-RESULT-CODE      PIC X(02).
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Creating Test Account 10001...".
           
           CALL "ACCTMGT" USING BY CONTENT WS-OPERATION
                                BY REFERENCE WS-ACCOUNT-DATA
                                BY REFERENCE WS-RESULT-CODE.
                                
           IF WS-RESULT-CODE = '00'
               DISPLAY "Account Created Successfully."
           ELSE
               DISPLAY "Error Creating Account. Code: " WS-RESULT-CODE
           END-IF.
           
           STOP RUN.
