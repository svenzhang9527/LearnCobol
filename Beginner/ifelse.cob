       IDENTIFICATION DIVISION.
      *程序的名称
       PROGRAM-ID. ifelse.
      *作者信息  
       AUTHOR. svenzhang.  

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1        PIC 9(3).

       PROCEDURE DIVISION.
           MOVE 50 TO NUM1.               *> 给 NUM1 赋值
           
           IF NUM1 > 100 THEN             *> 判断 NUM1 是否大于100
               DISPLAY 'NUM1 大于 100'
           ELSE
               DISPLAY 'NUM1 小于等于 100'
           END-IF.
           
           STOP RUN.
