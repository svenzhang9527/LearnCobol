       IDENTIFICATION DIVISION.
      *程序的名称
       PROGRAM-ID. fileTest.
      *作者信息  
       AUTHOR. svenzhang.  

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1        PIC 9(5) COMP.     *> 使用 COMP 格式存储5位整数

       PROCEDURE DIVISION.
           MOVE 12345 TO NUM1.           *> 将数值赋给 NUM1
           DISPLAY 'NUM1 = ' NUM1.       *> 显示 NUM1
           STOP RUN.
