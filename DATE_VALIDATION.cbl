IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.

DATA DIVISION.
WORKING-STORAGE SECTION.

01 YYYYMMDD PIC 9(8).
01 DD       PIC 99.
01 MM       PIC 99.
01 YYYY     PIC 9(4).
01 LEAP     PIC X VALUE "N".
01 RESULT   PIC 9.
01 MAX-DAY  PIC 99.

PROCEDURE DIVISION.

    DISPLAY "ENTER DATE IN YYYYMMDD FORMAT:"
    ACCEPT YYYYMMDD

    MOVE YYYYMMDD(1:4) TO YYYY
    MOVE YYYYMMDD(5:2) TO MM
    MOVE YYYYMMDD(7:2) TO DD

    MOVE "N" TO LEAP
    MOVE 0 TO RESULT
    MOVE 0 TO MAX-DAY

    IF FUNCTION MOD(YYYY, 400) = 0
        MOVE "Y" TO LEAP
    ELSE
        IF FUNCTION MOD(YYYY, 100) = 0
            MOVE "N" TO LEAP
        ELSE
            IF FUNCTION MOD(YYYY, 4) = 0
                MOVE "Y" TO LEAP
            END-IF
        END-IF
    END-IF

    EVALUATE MM
        WHEN 1
        WHEN 3
        WHEN 5
        WHEN 7
        WHEN 8
        WHEN 10
        WHEN 12
            MOVE 31 TO MAX-DAY

        WHEN 4
        WHEN 6
        WHEN 9
        WHEN 11
            MOVE 30 TO MAX-DAY

        WHEN 2
            IF LEAP = "Y"
                MOVE 29 TO MAX-DAY
            ELSE
                MOVE 28 TO MAX-DAY
            END-IF

        WHEN OTHER
            MOVE 0 TO MAX-DAY
    END-EVALUATE

    IF YYYY > 0 AND MM >= 1 AND MM <= 12
        IF DD >= 1 AND DD <= MAX-DAY
            MOVE 1 TO RESULT
        END-IF
    END-IF

    IF RESULT = 1
        DISPLAY "VALID DATE INPUT"
    ELSE
        DISPLAY "INVALID DATE INPUT"
    END-IF

    STOP RUN.
