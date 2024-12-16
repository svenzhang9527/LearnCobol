       IDENTIFICATION DIVISION.
       PROGRAM-ID. EVALUATE-EXAMPLE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 DAY-OF-WEEK      PIC 9 VALUE 3.
           01 DAY-NAME         PIC X(10) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "What day is it? Day number: " DAY-OF-WEEK
           EVALUATE TRUE
               WHEN DAY-OF-WEEK = 1
                   MOVE "Monday" TO DAY-NAME
               WHEN DAY-OF-WEEK = 2
                   MOVE "Tuesday" TO DAY-NAME
               WHEN DAY-OF-WEEK = 3
                   MOVE "Wednesday" TO DAY-NAME
               WHEN DAY-OF-WEEK = 4
                   MOVE "Thursday" TO DAY-NAME
               WHEN DAY-OF-WEEK = 5
                   MOVE "Friday" TO DAY-NAME
               WHEN DAY-OF-WEEK = 6
                   MOVE "Saturday" TO DAY-NAME
               WHEN DAY-OF-WEEK = 7
                   MOVE "Sunday" TO DAY-NAME
               WHEN OTHER
                   MOVE "Unknown" TO DAY-NAME
           END-EVALUATE
           DISPLAY "Today is: " DAY-NAME
           STOP RUN.
