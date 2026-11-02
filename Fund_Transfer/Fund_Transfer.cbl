       IDENTIFICATION DIVISION.
       PROGRAM-ID. FUNDXFER.
       AUTHOR. RAJA S.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNT-FILE ASSIGN TO "accounts.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNT-FILE.
       01  ACCOUNT-LINE            PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS          PIC XX.
       01  WS-EOF-FLAG             PIC X VALUE "N".
           88  END-OF-FILE         VALUE "Y".
       01  WS-VALID-FLAG           PIC X VALUE "Y".
           88  TRANSFER-OK         VALUE "Y".
           88  TRANSFER-FAILED     VALUE "N".

     
       01  WS-ACCOUNT-TABLE.
           05  WS-ACCT-ENTRY OCCURS 100 TIMES.
               10  T-ACCT-NO       PIC X(10).
               10  T-NAME          PIC X(20).
               10  T-TYPE          PIC X.
               10  T-BALANCE       PIC S9(9)V99.
               10  T-STATUS        PIC X.

       01  WS-COUNT                PIC 999 VALUE 0.
       01  WS-I                    PIC 999.
       01  WS-SRC-IDX              PIC 999 VALUE 0.
       01  WS-DST-IDX              PIC 999 VALUE 0.

      
       01  WS-ACCT-NO              PIC X(10).
       01  WS-NAME                 PIC X(20).
       01  WS-TYPE                 PIC X.
       01  WS-BAL-TEXT             PIC X(14).
       01  WS-STATUS               PIC X.

     
       01  WS-FROM-ACCT            PIC X(10).
       01  WS-TO-ACCT              PIC X(10).
       01  WS-AMOUNT-TEXT          PIC X(14).
       01  WS-AMOUNT               PIC 9(9)V99 VALUE 0.

       01  WS-ERROR-MSG            PIC X(40).
       01  WS-EDIT                 PIC Z(8)9.99.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-LOAD-ACCOUNTS
           PERFORM 2000-GET-TRANSFER-REQUEST
           PERFORM 3000-VALIDATE-TRANSFER
           IF TRANSFER-OK
               PERFORM 4000-DO-TRANSFER
               PERFORM 5000-SAVE-ACCOUNTS
               PERFORM 6000-SHOW-SUCCESS
           ELSE
               DISPLAY "TRANSFER FAILED : "
                       FUNCTION TRIM(WS-ERROR-MSG)
           END-IF
           STOP RUN.

       1000-LOAD-ACCOUNTS.
           OPEN INPUT ACCOUNT-FILE
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "Cannot open accounts.dat, status "
                       WS-FILE-STATUS
               STOP RUN
           END-IF
           PERFORM UNTIL END-OF-FILE
               READ ACCOUNT-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM 1100-STORE-IN-TABLE
               END-READ
           END-PERFORM
           CLOSE ACCOUNT-FILE.

       1100-STORE-IN-TABLE.
           IF ACCOUNT-LINE NOT = SPACES AND WS-COUNT < 100
               UNSTRING ACCOUNT-LINE DELIMITED BY "|"
                   INTO WS-ACCT-NO WS-NAME WS-TYPE
                        WS-BAL-TEXT WS-STATUS
               END-UNSTRING
               ADD 1 TO WS-COUNT
               MOVE WS-ACCT-NO TO T-ACCT-NO(WS-COUNT)
               MOVE WS-NAME    TO T-NAME(WS-COUNT)
               MOVE WS-TYPE    TO T-TYPE(WS-COUNT)
               MOVE WS-STATUS  TO T-STATUS(WS-COUNT)
               COMPUTE T-BALANCE(WS-COUNT) =
                       FUNCTION NUMVAL(WS-BAL-TEXT)
           END-IF.

       2000-GET-TRANSFER-REQUEST.
           DISPLAY "From Account : " WITH NO ADVANCING
           ACCEPT WS-FROM-ACCT
           DISPLAY "To Account   : " WITH NO ADVANCING
           ACCEPT WS-TO-ACCT
           DISPLAY "Amount       : " WITH NO ADVANCING
           ACCEPT WS-AMOUNT-TEXT
           COMPUTE WS-AMOUNT = FUNCTION NUMVAL(WS-AMOUNT-TEXT).

       3000-VALIDATE-TRANSFER.
           SET TRANSFER-OK TO TRUE
           MOVE SPACES TO WS-ERROR-MSG
           PERFORM 3100-FIND-ACCOUNTS

           EVALUATE TRUE
               WHEN WS-AMOUNT NOT > 0
                   SET TRANSFER-FAILED TO TRUE
                   MOVE "AMOUNT MUST BE GREATER THAN ZERO"
                       TO WS-ERROR-MSG
               WHEN WS-FROM-ACCT = WS-TO-ACCT
                   SET TRANSFER-FAILED TO TRUE
                   MOVE "SOURCE AND DESTINATION ARE THE SAME"
                       TO WS-ERROR-MSG
               WHEN WS-SRC-IDX = 0
                   SET TRANSFER-FAILED TO TRUE
                   MOVE "SOURCE ACCOUNT NOT FOUND" TO WS-ERROR-MSG
               WHEN WS-DST-IDX = 0
                   SET TRANSFER-FAILED TO TRUE
                   MOVE "DESTINATION ACCOUNT NOT FOUND"
                       TO WS-ERROR-MSG
               WHEN T-STATUS(WS-SRC-IDX) NOT = "A"
                   SET TRANSFER-FAILED TO TRUE
                   MOVE "SOURCE ACCOUNT INACTIVE" TO WS-ERROR-MSG
               WHEN T-STATUS(WS-DST-IDX) NOT = "A"
                   SET TRANSFER-FAILED TO TRUE
                   MOVE "DESTINATION ACCOUNT INACTIVE"
                       TO WS-ERROR-MSG
               WHEN T-BALANCE(WS-SRC-IDX) < WS-AMOUNT
                   SET TRANSFER-FAILED TO TRUE
                   MOVE "INSUFFICIENT BALANCE" TO WS-ERROR-MSG
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       3100-FIND-ACCOUNTS.
           MOVE 0 TO WS-SRC-IDX
           MOVE 0 TO WS-DST-IDX
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-COUNT
               IF T-ACCT-NO(WS-I) = WS-FROM-ACCT
                   MOVE WS-I TO WS-SRC-IDX
               END-IF
               IF T-ACCT-NO(WS-I) = WS-TO-ACCT
                   MOVE WS-I TO WS-DST-IDX
               END-IF
           END-PERFORM.

       4000-DO-TRANSFER.
           SUBTRACT WS-AMOUNT FROM T-BALANCE(WS-SRC-IDX)
           ADD WS-AMOUNT TO T-BALANCE(WS-DST-IDX).

       5000-SAVE-ACCOUNTS.
           OPEN OUTPUT ACCOUNT-FILE
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "Cannot rewrite accounts.dat, status "
                       WS-FILE-STATUS
               STOP RUN
           END-IF
           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > WS-COUNT
               PERFORM 5100-WRITE-ONE-ACCOUNT
           END-PERFORM
           CLOSE ACCOUNT-FILE.

       5100-WRITE-ONE-ACCOUNT.
           MOVE T-BALANCE(WS-I) TO WS-EDIT
           MOVE SPACES TO ACCOUNT-LINE
           STRING T-ACCT-NO(WS-I)             DELIMITED BY SIZE
                  "|"                         DELIMITED BY SIZE
                  FUNCTION TRIM(T-NAME(WS-I)) DELIMITED BY SIZE
                  "|"                         DELIMITED BY SIZE
                  T-TYPE(WS-I)                DELIMITED BY SIZE
                  "|"                         DELIMITED BY SIZE
                  FUNCTION TRIM(WS-EDIT)      DELIMITED BY SIZE
                  "|"                         DELIMITED BY SIZE
                  T-STATUS(WS-I)              DELIMITED BY SIZE
               INTO ACCOUNT-LINE
           END-STRING
           WRITE ACCOUNT-LINE.

       6000-SHOW-SUCCESS.
           MOVE WS-AMOUNT TO WS-EDIT
           DISPLAY " "
           DISPLAY "------ FUND TRANSFER ------"
           DISPLAY "Transfer Successful"
           DISPLAY "From Account : " WS-FROM-ACCT
           DISPLAY "To Account   : " WS-TO-ACCT
           DISPLAY "Amount       : " FUNCTION TRIM(WS-EDIT)
           MOVE T-BALANCE(WS-SRC-IDX) TO WS-EDIT
           DISPLAY "New balance (from) : " FUNCTION TRIM(WS-EDIT)
           MOVE T-BALANCE(WS-DST-IDX) TO WS-EDIT
           DISPLAY "New balance (to)   : " FUNCTION TRIM(WS-EDIT)
           DISPLAY "--------------------------".
