       IDENTIFICATION DIVISION.
      *程序的名称
       PROGRAM-ID. fileTest.
      *作者信息  
       AUTHOR. svenzhang.  

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    将输入文件绑定到名为 'input.txt' 的文件
           SELECT inputFile ASSIGN TO 'input.txt' 
      *    指定文件的组织方式为逐行顺序
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
      *文件描述符，定义输入文件
       FD  inputFile. 
      *定义文件记录的结构
       01 inputFileRecord. 
      *记录数据的字段，最多 80 个字符
           05 recordData PIC X(80). 

       WORKING-STORAGE SECTION.
      * 定义工作存储变量 WS-VAR1，初始值为 1000
       01 WS-VAR1       PIC 9(4) VALUE 1000. 
      * 定义工作存储变量 WS-NAME，初始值为 'John Doe'
       01 WS-NAME       PIC X(20) VALUE 'John Doe'. 
      * 定义工作存储变量 WS-COUNTER，初始值为 0
       01 WS-COUNTER    PIC 9(3) VALUE 0. 
      *定义文件结束标志，初始值为 'NO'
       01 EOF           PIC X(3) VALUE 'NO'.

       PROCEDURE DIVISION.
       BEGIN.
      *显示程序启动的消息
           DISPLAY 'Starting Program...'. 
      *    打开输入文件以供读取
           OPEN INPUT inputFile 
      *    循环读取文件，直到遇到文件末尾
           PERFORM READ-FILE UNTIL EOF = 'YES' 
      *    关闭输入文件
           CLOSE inputFile 
      *    显示程序结束的消息
           DISPLAY 'Program Finished.'. 
      *    终止程序的执行
           STOP RUN. 

       READ-FILE.
      *从文件中读取一条记录
           READ inputFile INTO inputFileRecord 
      *    如果到达文件末尾，将 EOF 设置为 'YES'
               AT END MOVE 'YES' TO EOF  
           NOT AT END
      *    显示读取的记录内容
               DISPLAY 'Read record: ' recordData  
      *        计数器递增
               ADD 1 TO WS-COUNTER  
           END-READ.
      *     程序结束标记
       END PROGRAM fileTest. 
