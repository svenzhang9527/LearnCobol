       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERFORM-EXAMPLE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 COUNTER          PIC 99 VALUE 0.   *> 修正 COUNTER 范围为两位数
       01 TOTAL            PIC 9(3) VALUE 0. *> 确保 TOTAL 可以存储三位数

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           DISPLAY "Start calculating the sum from 1 to 10"  *> 输出起始提示
           PERFORM CALCULATE-SUM
               VARYING COUNTER FROM 1 BY 1
               UNTIL COUNTER > 10                           *> 循环从 1 到 10
           DISPLAY "The total sum is: " TOTAL               *> 输出最终总和
           STOP RUN.                                        *> 停止程序

       CALCULATE-SUM.
           ADD COUNTER TO TOTAL.                            *> 将 COUNTER 累加到 TOTAL
