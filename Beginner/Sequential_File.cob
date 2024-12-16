       IDENTIFICATION DIVISION.
       PROGRAM-ID. SequentialFileExample.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT seqFile ASSIGN TO 'SEQUENCE.TXT'
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS fileStatus.

       DATA DIVISION.
       FILE SECTION.
       FD  seqFile.
       01  seqRecord.
           05 seqKey        PIC X(10).
           05 seqData       PIC X(30).

       WORKING-STORAGE SECTION.
       01 fileStatus      PIC XX.
       01 endOfFile       PIC X VALUE 'N'.
       01 seqCounter      PIC 9(3) VALUE 0.

       PROCEDURE DIVISION.

       *> 打开文件进行写入
       OPEN OUTPUT seqFile
           IF fileStatus NOT = '00'
               DISPLAY "Error opening file for writing"
               STOP RUN
           END-IF.

       PERFORM write-record UNTIL seqCounter = 5
       CLOSE seqFile.

       *> 打开文件进行读取
       OPEN INPUT seqFile
           IF fileStatus NOT = '00'
               DISPLAY "Error opening file for reading"
               STOP RUN
           END-IF.

       PERFORM read-record UNTIL endOfFile = 'Y'
       CLOSE seqFile.

       STOP RUN.

       write-record.
           ADD 1 TO seqCounter.
           MOVE 'Record-' TO seqKey.
           STRING seqCounter DELIMITED BY SIZE INTO seqKey
           MOVE 'This is record number ' TO seqData.
           STRING seqCounter DELIMITED BY SIZE INTO seqData
           WRITE seqRecord.

       read-record.
           READ seqFile INTO seqRecord
               AT END
                   MOVE 'Y' TO endOfFile
               NOT AT END
                   DISPLAY "Record: " seqKey " - " seqData
           END-READ.
