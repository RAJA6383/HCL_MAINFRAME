       IDENTIFICATION DIVISION.
       PROGRAM-ID. AUDITAPP.
       AUTHOR. RAJA S.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT NEW-TXN-FILE ASSIGN TO "new_txns.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-NEW-STATUS.
           SELECT AUDIT-FILE ASSIGN TO "audit.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-AUDIT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  NEW-TXN-FILE.
       01  NEW-TXN-REC             PIC X(80).
       FD  AUDIT-FILE.
       01  AUDIT-REC               PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-NEW-STATUS           PIC XX.
       01  WS-AUDIT-STATUS         PIC XX.
       01  WS-EOF-FLAG             PIC X VALUE "N".
           88  END-OF-FILE         VALUE "Y".
       01  WS-APPENDED-COUNT       PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-OPEN-FILES
           PERFORM UNTIL END-OF-FILE
               READ NEW-TXN-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM 2000-APPEND-RECORD
               END-READ
           END-PERFORM
           CLOSE NEW-TXN-FILE AUDIT-FILE
           DISPLAY "Records appended to audit file : "
                   WS-APPENDED-COUNT
           STOP RUN.

       1000-OPEN-FILES.
           OPEN INPUT NEW-TXN-FILE
           IF WS-NEW-STATUS NOT = "00"
               DISPLAY "Cannot open new_txns.dat, status "
                       WS-NEW-STATUS
               STOP RUN
           END-IF

           OPEN EXTEND AUDIT-FILE
           IF WS-AUDIT-STATUS = "35"
               DISPLAY "audit.dat not found, creating a new one."
               OPEN OUTPUT AUDIT-FILE
           END-IF
           IF WS-AUDIT-STATUS NOT = "00"
               DISPLAY "Cannot open audit.dat, status "
                       WS-AUDIT-STATUS
               STOP RUN
           END-IF.

       2000-APPEND-RECORD.
           IF NEW-TXN-REC NOT = SPACES
               WRITE AUDIT-REC FROM NEW-TXN-REC
               IF WS-AUDIT-STATUS = "00"
                   ADD 1 TO WS-APPENDED-COUNT
               ELSE
                   DISPLAY "Write error on audit.dat, status "
                           WS-AUDIT-STATUS
               END-IF
           END-IF.
