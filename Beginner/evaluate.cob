       IDENTIFICATION DIVISION.
       PROGRAM-ID. EVALUATE-EXAMPLE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 DAY-NAME         PIC X(10) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "What day is it? Day number: 3".
           EVALUATE 3
               WHEN 1
                   MOVE "Monday" TO DAY-NAME
               WHEN 2
                   MOVE "Tuesday" TO DAY-NAME
               WHEN 3
                   MOVE "Wednesday" TO DAY-NAME
               WHEN 4
                   MOVE "Thursday" TO DAY-NAME
               WHEN 5
                   MOVE "Friday" TO DAY-NAME
               WHEN 6
                   MOVE "Saturday" TO DAY-NAME
               WHEN 7
                   MOVE "Sunday" TO DAY-NAME
               WHEN OTHER
                   MOVE "Unknown" TO DAY-NAME
           END-EVALUATE.
           DISPLAY "Today is: " DAY-NAME.
           STOP RUN.
