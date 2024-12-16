       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-QUERY.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       EXEC SQL INCLUDE SQLCA END-EXEC.
       01  EMPLOYEE-RECORD.
           05 EMPLOYEE-ID   PIC X(10).
           05 EMPLOYEE-NAME PIC X(20).

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "STARTING THE EMPLOYEE QUERY PROGRAM.".

           *> CONNECT TO DATABASE
           EXEC SQL 
               CONNECT TO 'mysql://root:Aa123456@localhost:3309/licai'
           END-EXEC.

           IF SQLCODE NOT = 0
               DISPLAY "ERROR CONNECTING TO DATABASE."
               DISPLAY "SQLCODE: " SQLCODE.
               DISPLAY "SQLERRMC: " SQLERRMC.
               STOP RUN
           END-IF.

           * DECLARE CURSOR
           EXEC SQL
               DECLARE C1 CURSOR FOR
                   SELECT employee_id, employee_name FROM employees
           END-EXEC.

           * OPEN CURSOR
           EXEC SQL
               OPEN C1
           END-EXEC.

           PERFORM UNTIL SQLCODE NOT = 0
               * FETCH ROW
               EXEC SQL
                   FETCH C1 INTO :EMPLOYEE-ID, :EMPLOYEE-NAME
               END-EXEC.

               IF SQLCODE = 0
                   DISPLAY "EMPLOYEE ID: " EMPLOYEE-ID ", NAME: " EMPLOYEE-NAME.
               ELSE
                   DISPLAY "NO MORE RECORDS".
               END-IF.
           END-PERFORM.

           * CLOSE CURSOR
           EXEC SQL
               CLOSE C1
           END-EXEC.

           * DISCONNECT FROM DATABASE
           EXEC SQL
               DISCONNECT CURRENT
           END-EXEC.

           DISPLAY "ENDING THE EMPLOYEE QUERY PROGRAM.".
           STOP RUN.



