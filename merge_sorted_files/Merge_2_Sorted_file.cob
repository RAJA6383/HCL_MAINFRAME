       IDENTIFICATION DIVISION.
       PROGRAM-ID. MRGFILE.
       AUTHOR. RAJA S.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMP-FILE-1 ASSIGN TO "emp_file1.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS-1.
           SELECT EMP-FILE-2 ASSIGN TO "emp_file2.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS-2.
           SELECT MERGED-FILE ASSIGN TO "emp_merged.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-STATUS-OUT.

       DATA DIVISION.
       FILE SECTION.
       FD  EMP-FILE-1.
       01  REC-1                   PIC X(60).
       FD  EMP-FILE-2.
       01  REC-2                   PIC X(60).
       FD  MERGED-FILE.
       01  MERGED-REC              PIC X(60).

       WORKING-STORAGE SECTION.
       01  WS-STATUS-1             PIC XX.
       01  WS-STATUS-2             PIC XX.
       01  WS-STATUS-OUT           PIC XX.

      
       01  WS-KEY-1                PIC X(10).
       01  WS-KEY-2                PIC X(10).

       01  WS-WRITTEN-COUNT        PIC 9(5) VALUE 0.
       01  WS-DUPLICATE-COUNT      PIC 9(5) VALUE 0.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-OPEN-FILES
           PERFORM 2000-READ-FILE-1
           PERFORM 2100-READ-FILE-2

           PERFORM UNTIL WS-KEY-1 = HIGH-VALUES
                     AND WS-KEY-2 = HIGH-VALUES
               EVALUATE TRUE
                   WHEN WS-KEY-1 < WS-KEY-2
                       PERFORM 3000-WRITE-FROM-FILE-1
                       PERFORM 2000-READ-FILE-1
                   WHEN WS-KEY-2 < WS-KEY-1
                       PERFORM 3100-WRITE-FROM-FILE-2
                       PERFORM 2100-READ-FILE-2
                   WHEN OTHER
      *                Same ID in both files: keep File 1, skip File 2
                       PERFORM 3000-WRITE-FROM-FILE-1
                       ADD 1 TO WS-DUPLICATE-COUNT
                       PERFORM 2000-READ-FILE-1
                       PERFORM 2100-READ-FILE-2
               END-EVALUATE
           END-PERFORM

           CLOSE EMP-FILE-1 EMP-FILE-2 MERGED-FILE
           DISPLAY "Total records written : " WS-WRITTEN-COUNT
           DISPLAY "Duplicate IDs found   : " WS-DUPLICATE-COUNT
           STOP RUN.

       1000-OPEN-FILES.
           OPEN INPUT EMP-FILE-1
           IF WS-STATUS-1 NOT = "00"
               DISPLAY "Cannot open emp_file1.dat, status "
                       WS-STATUS-1
               STOP RUN
           END-IF
           OPEN INPUT EMP-FILE-2
           IF WS-STATUS-2 NOT = "00"
               DISPLAY "Cannot open emp_file2.dat, status "
                       WS-STATUS-2
               STOP RUN
           END-IF
           OPEN OUTPUT MERGED-FILE
           IF WS-STATUS-OUT NOT = "00"
               DISPLAY "Cannot open emp_merged.dat, status "
                       WS-STATUS-OUT
               STOP RUN
           END-IF.

       2000-READ-FILE-1.
           READ EMP-FILE-1
               AT END
                   MOVE HIGH-VALUES TO WS-KEY-1
               NOT AT END
                   UNSTRING REC-1 DELIMITED BY "|" INTO WS-KEY-1
                   END-UNSTRING
           END-READ.

       2100-READ-FILE-2.
           READ EMP-FILE-2
               AT END
                   MOVE HIGH-VALUES TO WS-KEY-2
               NOT AT END
                   UNSTRING REC-2 DELIMITED BY "|" INTO WS-KEY-2
                   END-UNSTRING
           END-READ.

       3000-WRITE-FROM-FILE-1.
           WRITE MERGED-REC FROM REC-1
           ADD 1 TO WS-WRITTEN-COUNT.

       3100-WRITE-FROM-FILE-2.
           WRITE MERGED-REC FROM REC-2
           ADD 1 TO WS-WRITTEN-COUNT.
