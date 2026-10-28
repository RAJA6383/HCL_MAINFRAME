       IDENTIFICATION DIVISION.
       PROGRAM-ID. BALRANK.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 WS-N PIC 9(4).
       01 WS-I PIC 9(4).
       01 WS-LIMIT PIC 9(4).
       01 WS-BAL-OUT PIC Z(8)9.

       01 WS-CUSTOMER-TABLE.
          05 WS-CUSTOMER OCCURS 1000 TIMES.
             10 WS-CUST-ID PIC X(10).
             10 WS-CUST-BAL PIC 9(9).

       PROCEDURE DIVISION.

           ACCEPT WS-N

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-N

               ACCEPT WS-CUST-ID(WS-I)
               ACCEPT WS-CUST-BAL(WS-I)

           END-PERFORM

           SORT WS-CUSTOMER
               ON DESCENDING KEY WS-CUST-BAL
               ON ASCENDING KEY WS-CUST-ID

           IF WS-N < 3
               MOVE WS-N TO WS-LIMIT
           ELSE
               MOVE 3 TO WS-LIMIT
           END-IF

           PERFORM VARYING WS-I FROM 1 BY 1
               UNTIL WS-I > WS-LIMIT

               MOVE WS-CUST-BAL(WS-I)
                 TO WS-BAL-OUT

               DISPLAY WS-CUST-ID(WS-I) " "
                   FUNCTION TRIM(WS-BAL-OUT)

           END-PERFORM

           STOP RUN.