       IDENTIFICATION DIVISION.
       PROGRAM-ID. CCFRAUD.
       AUTHOR. RAJA S.
      
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TXN-FILE ASSIGN TO "transactions.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-IN-STATUS.
           SELECT STATUS-FILE ASSIGN TO "txn_status.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  TXN-FILE.
       01  TXN-LINE                PIC X(80).
       FD  STATUS-FILE.
       01  STATUS-LINE             PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-IN-STATUS            PIC XX.
       01  WS-OUT-STATUS           PIC XX.
       01  WS-EOF-FLAG             PIC X VALUE "N".
           88  END-OF-FILE         VALUE "Y".

       01  WS-TXN-ID               PIC X(10).
       01  WS-CARD-NO              PIC X(16).
       01  WS-AMOUNT-TEXT          PIC X(12).
       01  WS-AMOUNT               PIC 9(8)V99.
       01  WS-AMOUNT-EDIT          PIC Z(7)9.99.
       01  WS-STATUS-TEXT          PIC X(8).

       01  WS-FRAUD-LIMIT          PIC 9(8)V99 VALUE 100000.00.
       01  WS-APPROVED-COUNT       PIC 9(5) VALUE 0.
       01  WS-SUSPECT-COUNT        PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-OPEN-FILES
           PERFORM UNTIL END-OF-FILE
               READ TXN-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM 2000-PROCESS-TRANSACTION
               END-READ
           END-PERFORM
           PERFORM 3000-SHOW-TOTALS
           CLOSE TXN-FILE STATUS-FILE
           STOP RUN.

       1000-OPEN-FILES.
           OPEN INPUT TXN-FILE
           IF WS-IN-STATUS NOT = "00"
               DISPLAY "Cannot open transactions.dat, status "
                       WS-IN-STATUS
               STOP RUN
           END-IF
           OPEN OUTPUT STATUS-FILE
           IF WS-OUT-STATUS NOT = "00"
               DISPLAY "Cannot open txn_status.dat, status "
                       WS-OUT-STATUS
               STOP RUN
           END-IF.

       2000-PROCESS-TRANSACTION.
           IF TXN-LINE NOT = SPACES
               UNSTRING TXN-LINE DELIMITED BY "|"
                   INTO WS-TXN-ID WS-CARD-NO WS-AMOUNT-TEXT
               END-UNSTRING
               COMPUTE WS-AMOUNT = FUNCTION NUMVAL(WS-AMOUNT-TEXT)

               IF WS-AMOUNT > WS-FRAUD-LIMIT
                   MOVE "SUSPECT" TO WS-STATUS-TEXT
                   ADD 1 TO WS-SUSPECT-COUNT
               ELSE
                   MOVE "APPROVED" TO WS-STATUS-TEXT
                   ADD 1 TO WS-APPROVED-COUNT
               END-IF

               PERFORM 2100-WRITE-STATUS-RECORD
               DISPLAY FUNCTION TRIM(WS-TXN-ID) " - "
                       FUNCTION TRIM(WS-STATUS-TEXT)
           END-IF.

       2100-WRITE-STATUS-RECORD.
           MOVE WS-AMOUNT TO WS-AMOUNT-EDIT
           MOVE SPACES TO STATUS-LINE
           STRING FUNCTION TRIM(WS-TXN-ID)      DELIMITED BY SIZE
                  "|"                           DELIMITED BY SIZE
                  FUNCTION TRIM(WS-CARD-NO)     DELIMITED BY SIZE
                  "|"                           DELIMITED BY SIZE
                  FUNCTION TRIM(WS-AMOUNT-EDIT) DELIMITED BY SIZE
                  "|"                           DELIMITED BY SIZE
                  FUNCTION TRIM(WS-STATUS-TEXT) DELIMITED BY SIZE
               INTO STATUS-LINE
           END-STRING
           WRITE STATUS-LINE.

       3000-SHOW-TOTALS.
           DISPLAY " "
           DISPLAY "Approved Transactions : " WS-APPROVED-COUNT
           DISPLAY "Suspect Transactions  : " WS-SUSPECT-COUNT.
