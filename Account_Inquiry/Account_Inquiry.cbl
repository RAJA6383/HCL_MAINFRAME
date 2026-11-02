       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTINQ.
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
       01  WS-FOUND-FLAG           PIC X VALUE "N".
           88  ACCOUNT-FOUND       VALUE "Y".

       01  WS-SEARCH-ACCT          PIC X(10).

    
       01  WS-ACCT-NO              PIC X(10).
       01  WS-CUST-NAME            PIC X(20).
       01  WS-ACCT-TYPE            PIC X.
       01  WS-BAL-TEXT             PIC X(12).
       01  WS-STATUS               PIC X.

       01  WS-BALANCE              PIC 9(7)V99.
       01  WS-BALANCE-OUT          PIC Z(6)9.99.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-GET-ACCOUNT-NUMBER
           PERFORM 2000-SEARCH-ACCOUNT
           PERFORM 3000-SHOW-RESULT
           STOP RUN.

       1000-GET-ACCOUNT-NUMBER.
           DISPLAY "Enter account number : " WITH NO ADVANCING
           ACCEPT WS-SEARCH-ACCT.

       2000-SEARCH-ACCOUNT.
           OPEN INPUT ACCOUNT-FILE
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "Cannot open accounts.dat, status "
                       WS-FILE-STATUS
               STOP RUN
           END-IF

           PERFORM UNTIL END-OF-FILE OR ACCOUNT-FOUND
               READ ACCOUNT-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM 2100-CHECK-RECORD
               END-READ
           END-PERFORM

           CLOSE ACCOUNT-FILE.

       2100-CHECK-RECORD.
           IF ACCOUNT-LINE NOT = SPACES
               UNSTRING ACCOUNT-LINE DELIMITED BY "|"
                   INTO WS-ACCT-NO
                        WS-CUST-NAME
                        WS-ACCT-TYPE
                        WS-BAL-TEXT
                        WS-STATUS
               END-UNSTRING
               IF WS-ACCT-NO = WS-SEARCH-ACCT
                   SET ACCOUNT-FOUND TO TRUE
               END-IF
           END-IF.

       3000-SHOW-RESULT.
           EVALUATE TRUE
               WHEN NOT ACCOUNT-FOUND
                   DISPLAY "ACCOUNT NOT FOUND"
               WHEN WS-STATUS NOT = "A"
                   DISPLAY "ACCOUNT INACTIVE"
               WHEN OTHER
                   COMPUTE WS-BALANCE = FUNCTION NUMVAL(WS-BAL-TEXT)
                   MOVE WS-BALANCE TO WS-BALANCE-OUT
                   DISPLAY "------ ACCOUNT BALANCE ------"
                   DISPLAY "Account Number : " WS-ACCT-NO
                   DISPLAY "Customer Name  : "
                           FUNCTION TRIM(WS-CUST-NAME)
                   DISPLAY "Balance        : "
                           FUNCTION TRIM(WS-BALANCE-OUT)
                   DISPLAY "----------------------------"
           END-EVALUATE.
