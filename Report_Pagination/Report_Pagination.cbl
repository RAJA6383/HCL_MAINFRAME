       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPRPT.
       AUTHOR. RAJA S.
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMP-FILE ASSIGN TO "employees.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-IN-STATUS.
           SELECT REPORT-FILE ASSIGN TO "emp_report.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-OUT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  EMP-FILE.
       01  EMP-LINE                PIC X(80).
       FD  REPORT-FILE.
       01  REPORT-LINE             PIC X(40).

       WORKING-STORAGE SECTION.
       01  WS-IN-STATUS            PIC XX.
       01  WS-OUT-STATUS           PIC XX.
       01  WS-EOF-FLAG             PIC X VALUE "N".
           88  END-OF-FILE         VALUE "Y".

       01  WS-LINES-PER-PAGE       PIC 99 VALUE 20.
     
       01  WS-LINE-COUNT           PIC 99 VALUE 20.
       01  WS-PAGE-NO              PIC 99 VALUE 0.
       01  WS-REC-COUNT            PIC 9(4) VALUE 0.

       01  WS-EMP-ID               PIC X(10).
       01  WS-EMP-NAME             PIC X(15).
       01  WS-SALARY-TEXT          PIC X(10).
       01  WS-SALARY               PIC 9(6).

       01  HEADING-LINE-1.
           05  FILLER              PIC X(15) VALUE "EMPLOYEE REPORT".
           05  FILLER              PIC X(12) VALUE SPACES.
           05  FILLER              PIC X(5)  VALUE "PAGE ".
           05  H1-PAGE-NO          PIC Z9.

       01  HEADING-LINE-2.
           05  FILLER              PIC X(11) VALUE "EMP ID".
           05  FILLER              PIC X(17) VALUE "NAME".
           05  FILLER              PIC X(6)  VALUE "SALARY".

       01  HEADING-LINE-3          PIC X(34) VALUE ALL "-".

       01  DETAIL-LINE.
           05  DL-EMP-ID           PIC X(11).
           05  DL-EMP-NAME         PIC X(17).
           05  DL-SALARY           PIC Z(5)9.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-OPEN-FILES
           PERFORM UNTIL END-OF-FILE
               READ EMP-FILE
                   AT END
                       SET END-OF-FILE TO TRUE
                   NOT AT END
                       PERFORM 2000-PRINT-EMPLOYEE
               END-READ
           END-PERFORM
           CLOSE EMP-FILE REPORT-FILE
           DISPLAY "Report written to emp_report.txt"
           DISPLAY "Employees printed : " WS-REC-COUNT
           DISPLAY "Pages printed     : " WS-PAGE-NO
           STOP RUN.

       1000-OPEN-FILES.
           OPEN INPUT EMP-FILE
           IF WS-IN-STATUS NOT = "00"
               DISPLAY "Cannot open employees.dat, status "
                       WS-IN-STATUS
               STOP RUN
           END-IF
           OPEN OUTPUT REPORT-FILE
           IF WS-OUT-STATUS NOT = "00"
               DISPLAY "Cannot open emp_report.txt, status "
                       WS-OUT-STATUS
               STOP RUN
           END-IF.

       2000-PRINT-EMPLOYEE.
           IF EMP-LINE NOT = SPACES
               UNSTRING EMP-LINE DELIMITED BY "|"
                   INTO WS-EMP-ID WS-EMP-NAME WS-SALARY-TEXT
               END-UNSTRING
               COMPUTE WS-SALARY = FUNCTION NUMVAL(WS-SALARY-TEXT)

               IF WS-LINE-COUNT >= WS-LINES-PER-PAGE
                   PERFORM 3000-PRINT-HEADINGS
               END-IF

               MOVE WS-EMP-ID   TO DL-EMP-ID
               MOVE WS-EMP-NAME TO DL-EMP-NAME
               MOVE WS-SALARY   TO DL-SALARY
               WRITE REPORT-LINE FROM DETAIL-LINE
                   AFTER ADVANCING 1 LINE
               ADD 1 TO WS-LINE-COUNT
               ADD 1 TO WS-REC-COUNT
           END-IF.

       3000-PRINT-HEADINGS.
           ADD 1 TO WS-PAGE-NO
           MOVE WS-PAGE-NO TO H1-PAGE-NO
           WRITE REPORT-LINE FROM HEADING-LINE-1
               AFTER ADVANCING PAGE
           MOVE SPACES TO REPORT-LINE
           WRITE REPORT-LINE AFTER ADVANCING 1 LINE
           WRITE REPORT-LINE FROM HEADING-LINE-2
               AFTER ADVANCING 1 LINE
           WRITE REPORT-LINE FROM HEADING-LINE-3
               AFTER ADVANCING 1 LINE
           MOVE 0 TO WS-LINE-COUNT.
