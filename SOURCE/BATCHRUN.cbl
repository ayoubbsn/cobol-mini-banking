       IDENTIFICATION DIVISION.
       PROGRAM-ID. BATCHRUN.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BATCH-FILE ASSIGN TO "DATA/batch_input.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-BATCH-STATUS.
               
       DATA DIVISION.
       FILE SECTION.
       FD  BATCH-FILE.
       01  BATCH-RECORD.
           05  B-TYPE          PIC X(01).
           05  B-ACCT-ID       PIC 9(05).
           05  B-TARGET-ID     PIC 9(05).
           05  B-AMOUNT        PIC 9(09)V99.
           
       WORKING-STORAGE SECTION.
       01  WS-BATCH-STATUS     PIC X(02).
       01  WS-EOF              PIC X(01) VALUE 'N'.
       
       01  WS-RESULT-CODE      PIC X(02).
       
       01  WS-COUNTERS.
           05  WS-TOTAL-READ   PIC 9(05) VALUE 0.
           05  WS-SUCCESS      PIC 9(05) VALUE 0.
           05  WS-FAIL         PIC 9(05) VALUE 0.
           
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Starting Batch Processing...".
           
           OPEN INPUT BATCH-FILE.
           IF WS-BATCH-STATUS NOT = "00"
               DISPLAY "Error opening batch file. Status: " WS-BATCH-STATUS
               STOP RUN
           END-IF.
           
           PERFORM UNTIL WS-EOF = 'Y'
               READ BATCH-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       PERFORM PROCESS-LINE
               END-READ
           END-PERFORM.
           
           CLOSE BATCH-FILE.
           
           DISPLAY "Batch Processing Complete.".
           DISPLAY "Total Processed: " WS-TOTAL-READ.
           DISPLAY "Successful:      " WS-SUCCESS.
           DISPLAY "Failed:          " WS-FAIL.
           
           STOP RUN.
           
       PROCESS-LINE.
           ADD 1 TO WS-TOTAL-READ.
           
           CALL "TXNPROC" USING BY CONTENT B-TYPE
                                BY CONTENT B-ACCT-ID
                                BY CONTENT B-TARGET-ID
                                BY CONTENT B-AMOUNT
                                BY REFERENCE WS-RESULT-CODE.
                                
           IF WS-RESULT-CODE = '00'
               ADD 1 TO WS-SUCCESS
               DISPLAY "Line " WS-TOTAL-READ ": Success"
           ELSE
               ADD 1 TO WS-FAIL
               DISPLAY "Line " WS-TOTAL-READ ": Failed (Code " WS-RESULT-CODE ")"
           END-IF.
