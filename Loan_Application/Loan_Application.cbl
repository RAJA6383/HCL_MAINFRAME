       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOANAPP.
       AUTHOR. RAJA S.
     
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOAN-FILE ASSIGN TO "loan_applications.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  LOAN-FILE.
       01  LOAN-RECORD             PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS          PIC XX.
       01  WS-MORE-FLAG            PIC X VALUE "Y".
           88  ADD-ANOTHER         VALUE "Y" "y".
       01  WS-VALID-FLAG           PIC X VALUE "Y".
           88  INPUT-VALID         VALUE "Y".
           88  INPUT-INVALID       VALUE "N".

       01  WS-CUSTOMER-ID          PIC X(10).
       01  WS-AMOUNT-TEXT          PIC X(12).
       01  WS-LOAN-AMOUNT          PIC 9(8)V99 VALUE 0.
       01  WS-LOAN-TYPE            PIC X(10).

       01  WS-MAX-LOAN             PIC 9(8)V99 VALUE 1000000.00.
       01  WS-APP-NUMBER           PIC 9(6) VALUE 120044.
       01  WS-AMOUNT-EDIT          PIC Z(7)9.99.
       01  WS-ERROR-MSG            PIC X(50).

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-OPEN-OUTPUT-FILE
           PERFORM UNTIL NOT ADD-ANOTHER
               PERFORM 2000-GET-APPLICATION
               PERFORM 3000-VALIDATE-APPLICATION
               IF INPUT-VALID
                   PERFORM 4000-SAVE-APPLICATION
               ELSE
                   DISPLAY "Application rejected: "
                           FUNCTION TRIM(WS-ERROR-MSG)
               END-IF
               DISPLAY " "
               DISPLAY "Another application? (Y/N) : "
                       WITH NO ADVANCING
               ACCEPT WS-MORE-FLAG
           END-PERFORM
           CLOSE LOAN-FILE
           STOP RUN.

     
       1000-OPEN-OUTPUT-FILE.
           OPEN EXTEND LOAN-FILE
           IF WS-FILE-STATUS = "35"
               OPEN OUTPUT LOAN-FILE
           END-IF
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "Cannot open loan file, status "
                       WS-FILE-STATUS
               STOP RUN
           END-IF.

       2000-GET-APPLICATION.
           DISPLAY "Customer ID : " WITH NO ADVANCING
           ACCEPT WS-CUSTOMER-ID
           DISPLAY "Loan Amount : " WITH NO ADVANCING
           ACCEPT WS-AMOUNT-TEXT
           DISPLAY "Loan Type (HOME/CAR/PERSONAL) : "
                   WITH NO ADVANCING
           ACCEPT WS-LOAN-TYPE
           MOVE FUNCTION UPPER-CASE(WS-LOAN-TYPE) TO WS-LOAN-TYPE
           COMPUTE WS-LOAN-AMOUNT = FUNCTION NUMVAL(WS-AMOUNT-TEXT).

       3000-VALIDATE-APPLICATION.
           SET INPUT-VALID TO TRUE
           MOVE SPACES TO WS-ERROR-MSG
           EVALUATE TRUE
               WHEN WS-CUSTOMER-ID = SPACES
                   SET INPUT-INVALID TO TRUE
                   MOVE "CUSTOMER ID CANNOT BE BLANK" TO WS-ERROR-MSG
               WHEN WS-LOAN-AMOUNT NOT > 0
                   SET INPUT-INVALID TO TRUE
                   MOVE "LOAN AMOUNT MUST BE GREATER THAN ZERO"
                       TO WS-ERROR-MSG
               WHEN WS-LOAN-AMOUNT > WS-MAX-LOAN
                   SET INPUT-INVALID TO TRUE
                   MOVE "LOAN AMOUNT EXCEEDS 10,00,000 LIMIT"
                       TO WS-ERROR-MSG
               WHEN WS-LOAN-TYPE NOT = "HOME"
                AND WS-LOAN-TYPE NOT = "CAR"
                AND WS-LOAN-TYPE NOT = "PERSONAL"
                   SET INPUT-INVALID TO TRUE
                   MOVE "LOAN TYPE MUST BE HOME, CAR OR PERSONAL"
                       TO WS-ERROR-MSG
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.

       4000-SAVE-APPLICATION.
           ADD 1 TO WS-APP-NUMBER
           MOVE WS-LOAN-AMOUNT TO WS-AMOUNT-EDIT

           MOVE SPACES TO LOAN-RECORD
           STRING WS-APP-NUMBER                DELIMITED BY SIZE
                  "|"                          DELIMITED BY SIZE
                  FUNCTION TRIM(WS-CUSTOMER-ID) DELIMITED BY SIZE
                  "|"                          DELIMITED BY SIZE
                  FUNCTION TRIM(WS-AMOUNT-EDIT) DELIMITED BY SIZE
                  "|"                          DELIMITED BY SIZE
                  FUNCTION TRIM(WS-LOAN-TYPE)  DELIMITED BY SIZE
                  "|PENDING"                   DELIMITED BY SIZE
               INTO LOAN-RECORD
           END-STRING
           WRITE LOAN-RECORD
           IF WS-FILE-STATUS NOT = "00"
               DISPLAY "Write failed, status " WS-FILE-STATUS
               STOP RUN
           END-IF

           DISPLAY " "
           DISPLAY "------ LOAN APPLICATION ------"
           DISPLAY "Application ID : " WS-APP-NUMBER
           DISPLAY "Customer ID    : " FUNCTION TRIM(WS-CUSTOMER-ID)
           DISPLAY "Loan Amount    : " FUNCTION TRIM(WS-AMOUNT-EDIT)
           DISPLAY "Loan Type      : " FUNCTION TRIM(WS-LOAN-TYPE)
           DISPLAY "Status         : PENDING"
           DISPLAY "----------------------------".
