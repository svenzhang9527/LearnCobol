       IDENTIFICATION DIVISION.                                          *> 程序标识部分，指定程序的名称
       PROGRAM-ID. BatchAccountProcessor.  *> 程序名称

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.  *> 输入输出部分，定义输入和输出文件
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "ACCOUNT_INPUT.DAT"  *> 输入文件分配给 "ACCOUNT_INPUT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.  *> 文件组织形式为逐行存储

           SELECT OUTPUT-FILE ASSIGN TO "ACCOUNT_OUTPUT.DAT"  *> 输出文件分配给 "ACCOUNT_OUTPUT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.  *> 文件组织形式为逐行存储

       DATA DIVISION.  *> 数据定义部分，声明所有数据结构
       FILE SECTION.
       FD  INPUT-FILE.  *> 输入文件描述符
       01  ACCOUNT-RECORD.  *> 定义账户记录结构
           05 ACCOUNT-ID            PIC 9(4).  *> 账户ID，4位数字
           05 ACCOUNT-NAME          PIC X(10).  *> 账户名称，最大10个字符
           05 ACCOUNT-BALANCE       PIC 9(7)V99.  *> 账户余额，最大7位整数和2位小数

       FD  OUTPUT-FILE.  *> 输出文件描述符
       01  OUTPUT-RECORD.  *> 定义输出记录结构
           05 ACCT-ID            PIC 9(4).  *> 账户ID，4位数字
           05 ACCT-NAME          PIC X(10).  *> 账户名称，最大10个字符
           05 FINAL-ACCOUNT-BALANCE PIC 9(7)V99.  *> 最终账户余额，最大7位整数和2位小数

       WORKING-STORAGE SECTION.  *> 工作存储部分，用于存储临时数据
       01  WS-EOF                  PIC X VALUE 'N'.  *> EOF标志，初始化为'N'（未到文件末尾）
       01  WS-INTEREST-RATE        PIC 9V9(2) VALUE 0.03.  *> 利率，设为0.03（3%）
       01  WS-MANAGEMENT-FEE       PIC 9(2)V99 VALUE 10.00.  *> 管理费用，设为10.00
       01  WS-TOTAL-BALANCE        PIC 9(9)V99 VALUE 0.  *> 总余额，初始化为0
       01  WS-NEW-BALANCE          PIC 9(7)V99.  *> 新的账户余额，用于存储计算后的余额
       01  TEMP-RESULT             PIC 9(7)V99.  *> 定义 TEMP-RESULT 作为一个 7 位数字带小数的变量

       PROCEDURE DIVISION.  *> 处理逻辑部分，包含程序的具体执行流程

       BEGIN-PROGRAM.
           OPEN INPUT INPUT-FILE  *> 打开输入文件以读取数据
                OUTPUT OUTPUT-FILE.  *> 打开输出文件以写入数据

           PERFORM UNTIL WS-EOF = 'Y'  *> 循环读取文件，直到到达文件末尾
               READ INPUT-FILE
                   AT END  *> 到达文件末尾时
                       MOVE 'Y' TO WS-EOF  *> 设置 EOF 标志为 'Y' 表示文件结束
                   NOT AT END  *> 如果不是文件末尾
                       PERFORM CALCULATE-NEW-BALANCE  *> 计算新的账户余额
                       PERFORM APPLY-MANAGEMENT-FEE  *> 应用管理费用
                       PERFORM WRITE-OUTPUT-RECORD  *> 将输出记录写入文件
                       PERFORM UPDATE-TOTAL-BALANCE  *> 更新总余额
               END-READ
           END-PERFORM.

           PERFORM DISPLAY-TOTAL-BALANCE.  *> 显示总余额

           CLOSE INPUT-FILE OUTPUT-FILE.  *> 关闭输入和输出文件
           STOP RUN.  *> 结束程序执行

       CALCULATE-NEW-BALANCE.
           COMPUTE WS-NEW-BALANCE = ACCOUNT-BALANCE +  *> 计算新的账户余额
               (ACCOUNT-BALANCE * WS-INTEREST-RATE).  *> 利用利率计算

           *> 使用临时变量保存计算结果
           COMPUTE TEMP-RESULT = ACCOUNT-BALANCE * WS-INTEREST-RATE.
           DISPLAY 'WS-NEW-BALANCE: ' WS-NEW-BALANCE.
           DISPLAY 'ACCOUNT-BALANCE: ' ACCOUNT-BALANCE.
           DISPLAY 'WS-INTEREST-RATE: ' WS-INTEREST-RATE.
           DISPLAY 'INTEREST AMOUNT: ' TEMP-RESULT.
           
           *> 强制格式化为2位小数
           IF WS-NEW-BALANCE NOT = FUNCTION NUMVAL(WS-NEW-BALANCE) THEN
               DISPLAY 'Error in balance calculation: ' WS-NEW-BALANCE
           END-IF.


       APPLY-MANAGEMENT-FEE.
           IF WS-NEW-BALANCE < 10000.00  *> 如果新余额小于10000.00
               COMPUTE WS-NEW-BALANCE = WS-NEW-BALANCE - 
               WS-MANAGEMENT-FEE.  *> 扣除管理费用
       
       WRITE-OUTPUT-RECORD.
           MOVE ACCOUNT-ID TO ACCT-ID.  *> 将账户ID写入输出记录
           MOVE ACCOUNT-NAME TO ACCT-NAME.  *> 将账户名称写入输出记录
           MOVE WS-NEW-BALANCE TO FINAL-ACCOUNT-BALANCE.  *> 将新的账户余额写入输出记录
      *    DISPLAY 'WS-NEW-BALANCE: ' WS-NEW-BALANCE

           WRITE OUTPUT-RECORD.  *> 将输出记录写入文件

       UPDATE-TOTAL-BALANCE.
           ADD WS-NEW-BALANCE TO WS-TOTAL-BALANCE.  *> 将新余额添加到总余额中

       DISPLAY-TOTAL-BALANCE.
           DISPLAY "TOTAL BALANCE AFTER PROCESSING: " WS-TOTAL-BALANCE.  *> 显示处理后的总余额
