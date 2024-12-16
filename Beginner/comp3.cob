       IDENTIFICATION DIVISION.
      *程序的名称
       PROGRAM-ID. fileTest.
      *作者信息  
       AUTHOR. svenzhang.  

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM2        PIC 9(5) COMP-3.    *> 使用 COMP-3 格式存储数值（压缩格式）

       PROCEDURE DIVISION.
           MOVE 54321 TO NUM2.            *> 将数值赋给 NUM2
           DISPLAY 'NUM2 = ' NUM2.        *> 显示 NUM2（注意：显示时会自动转换为可读格式）
           STOP RUN.

