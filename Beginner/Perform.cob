       IDENTIFICATION DIVISION.
      *程序的名称
       PROGRAM-ID. perform.
      *作者信息  
       AUTHOR. svenzhang.  

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 COUNTER     PIC 9(2) VALUE 1.

       PROCEDURE DIVISION.
           PERFORM UNTIL COUNTER > 5        *> 循环直到 COUNTER 大于 5
               DISPLAY 'count: ' COUNTER
               ADD 1 TO COUNTER
           END-PERFORM.

           STOP RUN.
