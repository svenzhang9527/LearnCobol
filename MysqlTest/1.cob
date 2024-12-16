       IDENTIFICATION DIVISION.
       PROGRAM-ID. CICSDEMO.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-MESSAGE       PIC X(50) VALUE 'Hello, CICS World!'.
       PROCEDURE DIVISION.

       CICS-START.
           DISPLAY 'Starting CICS Program...'   *>/* 输出开始消息 */ 发送消息到 CICS 客户端
           EXEC CICS SEND TEXT(WS-MESSAGE)   
           END-EXEC.
           DISPLAY 'CICS Program Finished!'   *>/* 输出结束消息 */
           STOP RUN.
