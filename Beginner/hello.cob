       IDENTIFICATION DIVISION.
       PROGRAM-ID. HelloWorld.
       author. svenzhang
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DUMMY PIC X.
       PROCEDURE DIVISION.
           DISPLAY "Hello, World!".
           ACCEPT WS-DUMMY.
           STOP RUN.
