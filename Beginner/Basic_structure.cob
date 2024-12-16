       IDENTIFICATION DIVISION.
      *程序的名称
       PROGRAM-ID. fileTest.
      *作者信息  
       AUTHOR. svenzhang.  
              
       DATA DIVISION.                *> 数据部分
       WORKING-STORAGE SECTION.      *> 工作存储区
       01 NUM1        PIC 9(3).        *> 定义一个整数变量，最大值为999
       01 NUM2        PIC 9(3).        *> 定义另一个整数变量
       01 RESULT      PIC 9(5).        *> 定义一个结果变量，最大值为99999
       01 NAME        PIC X(20).       *> 定义一个字符串变量，长度为20

       PROCEDURE DIVISION.
           MOVE 100 TO NUM1.           *> 给 NUM1 赋值
           MOVE 200 TO NUM2.           *> 给 NUM2 赋值
           ADD NUM1 TO NUM2 GIVING RESULT.  *> 将 NUM1 和 NUM2 相加，结果存入 RESULT
           DISPLAY 'RESULT = ' RESULT.  *> 输出结果
           STOP RUN.
