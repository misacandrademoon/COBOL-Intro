                 IDENTIFICATION DIVISION.
       PROGRAM-ID. RELATIVE-EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RELATIVE-FILE ASSIGN TO 'relativo.dat'
               ORGANIZATION IS RELATIVE
               ACCESS MODE IS DYNAMIC
               RELATIVE KEY IS RECORD-NUM.

       DATA DIVISION.
       FILE SECTION.
       FD RELATIVE-FILE.
       01 RELATIVE-RECORD.
           05 IDNUM   PIC 9(5).
           05 DA    PIC X(20).

       WORKING-STORAGE SECTION.
       01 RECORD-NUM     PIC 9(5).
       01 OPTION         PIC X VALUE SPACE.
       01 INPUT-DATA     PIC X(20).

       PROCEDURE DIVISION.
       MAIN.
           OPEN I-O RELATIVE-FILE

           PERFORM UNTIL OPTION = 'F'
               DISPLAY "Enter record number (1–99999): "
               ACCEPT RECORD-NUM
               DISPLAY "DATA: "
               ACCEPT INPUT-DATA
               MOVE RECORD-NUM TO IDNUM
               MOVE INPUT-DATA TO DA
               WRITE RELATIVE-RECORD INVALID KEY
                   DISPLAY "Error writing record"
               END-WRITE
               DISPLAY "Do you want to continue? (F to finish)"
               ACCEPT OPTION
           END-PERFORM

           CLOSE RELATIVE-FILE
           STOP RUN.
