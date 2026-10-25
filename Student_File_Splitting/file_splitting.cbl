       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUSPLIT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT STUDENT-INFILE
               ASSIGN TO "STUDIN.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-IN-STATUS.

           SELECT UNDERGRAD-FILE
               ASSIGN TO "UGOUT.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-UG-STATUS.

           SELECT GRADUATE-FILE
               ASSIGN TO "PGOUT.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-PG-STATUS.

       DATA DIVISION.
       FILE SECTION.

       FD STUDENT-INFILE.
       01 STUDENT-IN-REC PIC X(17).

       FD UNDERGRAD-FILE.
       01 UG-REC PIC X(17).

       FD GRADUATE-FILE.
       01 PG-REC PIC X(17).

       WORKING-STORAGE SECTION.

       01 WS-IN-STATUS PIC XX.
       01 WS-UG-STATUS PIC XX.
       01 WS-PG-STATUS PIC XX.

       01 WS-EOF PIC X VALUE 'N'.

       01 WS-DEGREE-CODE PIC X(3).

       01 WS-INVALID-DEGREE-CNT PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.

       MAIN-PARA.

           OPEN INPUT STUDENT-INFILE

           IF WS-IN-STATUS NOT = '00'
               DISPLAY 'INPUT FILE ERROR: ' WS-IN-STATUS
               STOP RUN
           END-IF

           OPEN OUTPUT UNDERGRAD-FILE

           IF WS-UG-STATUS NOT = '00'
               DISPLAY 'UG OUTPUT ERROR: ' WS-UG-STATUS
               CLOSE STUDENT-INFILE
               STOP RUN
           END-IF

           OPEN OUTPUT GRADUATE-FILE

           IF WS-PG-STATUS NOT = '00'
               DISPLAY 'PG OUTPUT ERROR: ' WS-PG-STATUS
               CLOSE STUDENT-INFILE
               CLOSE UNDERGRAD-FILE
               STOP RUN
           END-IF

           PERFORM UNTIL WS-EOF = 'Y'

               READ STUDENT-INFILE

               EVALUATE WS-IN-STATUS

                   WHEN '00'
                       MOVE STUDENT-IN-REC(12:3)
                         TO WS-DEGREE-CODE
                       PERFORM ROUTE-STUDENT

                   WHEN '10'
                       MOVE 'Y' TO WS-EOF

                   WHEN OTHER
                       DISPLAY 'READ ERROR: ' WS-IN-STATUS
                       MOVE 'Y' TO WS-EOF

               END-EVALUATE

           END-PERFORM

           CLOSE STUDENT-INFILE
           CLOSE UNDERGRAD-FILE
           CLOSE GRADUATE-FILE

           DISPLAY 'INVALID DEGREE COUNT: '
                   WS-INVALID-DEGREE-CNT

           DISPLAY 'FILE SPLITTING COMPLETED'

           STOP RUN.

       ROUTE-STUDENT.

           EVALUATE WS-DEGREE-CODE

               WHEN 'UG '
                   WRITE UG-REC FROM STUDENT-IN-REC

               WHEN 'PG '
                   WRITE PG-REC FROM STUDENT-IN-REC

               WHEN 'PHD'
                   WRITE PG-REC FROM STUDENT-IN-REC

               WHEN OTHER
                   ADD 1 TO WS-INVALID-DEGREE-CNT

           END-EVALUATE.
           