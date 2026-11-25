       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPSORT.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT EMP-INFILE
           ASSIGN TO "EMPLOYEE.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       SELECT EMP-OUTFILE
           ASSIGN TO "SORTED-EMPLOYEE.txt"
           ORGANIZATION IS LINE SEQUENTIAL.

       SELECT EMP-SORT-FILE
           ASSIGN TO "SORT-WORK".

       DATA DIVISION.
       FILE SECTION.

       FD EMP-INFILE.
       01 EMP-IN-REC PIC X(20).

       FD EMP-OUTFILE.
       01 EMP-OUT-REC PIC X(20).

       SD EMP-SORT-FILE.
       01 SORT-EMP-REC.
          05 SORT-EMP-ID PIC X(4).
          05 SORT-EMP-NAME PIC X(10).
          05 SORT-SALARY PIC 9(6).

       WORKING-STORAGE SECTION.

       01 WS-EOF PIC X VALUE 'N'.

       01 WS-IN-REC.
          05 WS-EMP-ID PIC X(4).
          05 WS-EMP-NAME PIC X(10).
          05 WS-SALARY PIC X(6).

       PROCEDURE DIVISION.

       MAIN-PARA.
           OPEN INPUT EMP-INFILE

           SORT EMP-SORT-FILE
               ON ASCENDING KEY SORT-SALARY
               INPUT PROCEDURE READ-EMPLOYEES
               OUTPUT PROCEDURE WRITE-EMPLOYEES

           CLOSE EMP-INFILE

           DISPLAY 'EMPLOYEE SORT COMPLETED'

           STOP RUN.

       READ-EMPLOYEES.
           PERFORM UNTIL WS-EOF = 'Y'

               READ EMP-INFILE
                   AT END
                       MOVE 'Y' TO WS-EOF
                   NOT AT END
                       MOVE EMP-IN-REC TO WS-IN-REC

                       MOVE WS-EMP-ID TO SORT-EMP-ID
                       MOVE WS-EMP-NAME TO SORT-EMP-NAME
                       MOVE WS-SALARY TO SORT-SALARY

                       RELEASE SORT-EMP-REC
               END-READ

           END-PERFORM.

       WRITE-EMPLOYEES.
           OPEN OUTPUT EMP-OUTFILE

           MOVE 'N' TO WS-EOF

           PERFORM UNTIL WS-EOF = 'Y'

              RETURN EMP-SORT-FILE
                   AT END
                   MOVE 'Y' TO WS-EOF
              NOT AT END
                 MOVE SORT-EMP-REC TO EMP-OUT-REC
                 WRITE EMP-OUT-REC
           END-RETURN

           END-PERFORM

           CLOSE EMP-OUTFILE.