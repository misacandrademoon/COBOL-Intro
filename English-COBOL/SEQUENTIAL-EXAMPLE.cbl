       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEQUENTIAL-EXAMPLE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQUENTIAL-FILE ASSIGN TO 'datos.dat'
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
           FD SEQUENTIAL-FILE.
       01  RECORD.
           05 IDNUM     PIC 9(5).
           05 NAME      PIC X(30).

            WORKING-STORAGE SECTION.
       01   END-OF-DATA       PIC X VALUE 'N'.
        88 NO-MORE-DATA   VALUE 'S'.

       01 CURRENT-ID        PIC 9(5) VALUE 1.
       01 CURRENT-NAME      PIC X(30).

           PROCEDURE DIVISION.
            MAIN.
           INITIALIZE END-OF-DATA.
           OPEN OUTPUT SEQUENTIAL-FILE.
           PERFORM UNTIL NO-MORE-DATA
               DISPLAY "Name: "
               ACCEPT CURRENT-NAME
               IF CURRENT-NAME = "FIN"
                   MOVE 'S' TO END-OF-DATA
               ELSE
                   MOVE CURRENT-ID TO IDNUM
                   MOVE CURRENT-NAME TO NAME
                   WRITE RECORD
                   ADD 1 TO CURRENT-ID
               END-IF
           END-PERFORM.
           CLOSE SEQUENTIAL-FILE.
           STOP RUN.
