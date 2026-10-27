       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCVALID.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-ACCOUNT-NUMBER PIC X(15).
       01 WS-VALID-FLAG PIC X VALUE 'Y'.
       01 WS-COUNTER PIC 9(2).

       PROCEDURE DIVISION.

           ACCEPT WS-ACCOUNT-NUMBER

           IF WS-ACCOUNT-NUMBER(1:10) IS NUMERIC
               IF WS-ACCOUNT-NUMBER(11:5) = SPACES
                   DISPLAY "VALID"
               ELSE
                   DISPLAY "INVALID"
               END-IF
           ELSE
               DISPLAY "INVALID"
           END-IF

           STOP RUN.