       IDENTIFICATION DIVISION.
       PROGRAM-ID. IndexedFileExample.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT idxFile ASSIGN TO 'INDEXED.TXT'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS idxKey
               FILE STATUS IS fileStatus.

       DATA DIVISION.
       FILE SECTION.
       FD  idxFile.
       01  idxRecord.
           05 idxKey        PIC X(10).
           05 idxData       PIC X(30).

       WORKING-STORAGE SECTION.
       01 fileStatus      PIC XX.
       01 idxCounter      PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.
       *> 打开文件进行写入
       OPEN OUTPUT idxFile
           IF fileStatus NOT = '00'
               DISPLAY "Error opening file for writing. File Status: " fileStatus
               STOP RUN
           END-IF.

       *> 写入 5 条记录
       PERFORM write-record UNTIL idxCounter = 5
       CLOSE idxFile.

       *> 打开文件进行读取
       OPEN I-O idxFile
           IF fileStatus NOT = '00'
               DISPLAY "Error opening file for reading. File Status: " fileStatus
               STOP RUN
           END-IF.

       *> 读取特定的记录
       PERFORM read-record UNTIL idxCounter = 5
       CLOSE idxFile.

       STOP RUN.

       write-record.
           ADD 1 TO idxCounter.
           MOVE 'Index-' TO idxKey.
           STRING idxCounter DELIMITED BY SIZE INTO idxKey
           MOVE 'This is indexed record ' TO idxData.
           STRING idxCounter DELIMITED BY SIZE INTO idxData
           WRITE idxRecord
               INVALID KEY
                   DISPLAY "Error writing record. File Status: " fileStatus
                   STOP RUN
           END-IF.
           DISPLAY "Writing record: " idxKey " - " idxData.  *> 这里打印写入的记录

       read-record.
           MOVE 'Index-2' TO idxKey.  *> 读取特定索引记录
           DISPLAY "Searching for: " idxKey  *> 打印正在搜索的索引
           READ idxFile INVALID KEY
               DISPLAY "Record not found."
           NOT INVALID KEY
               DISPLAY "Record: " idxKey " - " idxData
           END-READ.
