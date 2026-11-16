       IDENTIFICATION DIVISION.
       PROGRAM-ID. VARLEN.
       AUTHOR. RAJA S.
     
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT IN-FILE ASSIGN TO "varrec_in.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-IN-STATUS.
           SELECT OUT-FILE ASSIGN TO "varrec_out.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.

       DATA DIVISION.
       FILE SECTION.
      
       FD  IN-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 80 CHARACTERS
               DEPENDING ON WS-IN-LEN.
       01  IN-REC                  PIC X(80).

      
       FD  OUT-FILE
           RECORD IS VARYING IN SIZE FROM 5 TO 80 CHARACTERS
               DEPENDING ON WS-OUT-LEN.
       01  OUT-REC                 PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-IN-STATUS            PIC XX.
       01  WS-OUT-STATUS           PIC XX.
       01  WS-EOF-FLAG             PIC X VALUE "N".
           88  END-OF-FILE         VALUE "Y".

       01  WS-IN-LEN               PIC 999 VALUE 0.
       01  WS-OUT-LEN              PIC 999 VALUE 0.
       01  WS-MIN-LEN              PIC 999 VALUE 5.
       01  WS-MAX-LEN              PIC 999 VALUE 80.

       01  WS-READ-COUNT           PIC 9(4) VALUE 0.
       01  WS-WRITE-COUNT          PIC 9(4) VALUE 0.
       01  WS-BAD-COUNT            PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-OPEN-FILES
           PERFORM UNTIL END-OF-FILE
               READ IN-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM 2000-HANDLE-RECORD
               END-READ
           END-PERFORM
           CLOSE IN-FILE OUT-FILE
           DISPLAY " "
           DISPLAY "Records read    : " WS-READ-COUNT
           DISPLAY "Records written : " WS-WRITE-COUNT
           DISPLAY "Bad length      : " WS-BAD-COUNT
           STOP RUN.

       1000-OPEN-FILES.
           OPEN INPUT IN-FILE
           IF WS-IN-STATUS NOT = "00"
               DISPLAY "Cannot open varrec_in.dat, status "
                       WS-IN-STATUS
               STOP RUN
           END-IF
           OPEN OUTPUT OUT-FILE
           IF WS-OUT-STATUS NOT = "00"
               DISPLAY "Cannot open varrec_out.dat, status "
                       WS-OUT-STATUS
               STOP RUN
           END-IF.

       2000-HANDLE-RECORD.
           ADD 1 TO WS-READ-COUNT
           IF WS-IN-LEN < WS-MIN-LEN OR WS-IN-LEN > WS-MAX-LEN
               ADD 1 TO WS-BAD-COUNT
               DISPLAY "Record " WS-READ-COUNT
                       " skipped, invalid length " WS-IN-LEN
           ELSE
               PERFORM 2100-COPY-RECORD
           END-IF.

       2100-COPY-RECORD.
      
           MOVE SPACES TO OUT-REC
           MOVE IN-REC(1:WS-IN-LEN) TO OUT-REC(1:WS-IN-LEN)
           MOVE WS-IN-LEN TO WS-OUT-LEN
           WRITE OUT-REC
           IF WS-OUT-STATUS = "00"
               ADD 1 TO WS-WRITE-COUNT
               DISPLAY "Record " WS-READ-COUNT " length "
                       WS-IN-LEN " : " IN-REC(1:WS-IN-LEN)
           ELSE
               DISPLAY "Write error, status " WS-OUT-STATUS
           END-IF.
