      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
        IDENTIFICATION DIVISION.
       PROGRAM-ID. EJEMPLO-SECUENCIAL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-SECUENCIAL ASSIGN TO 'datos.dat'
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ARCHIVO-SECUENCIAL.
       01  REGISTRO.
           05 IDNUM      PIC 9(5).
           05 NOMBRE     PIC X(30).

       WORKING-STORAGE SECTION.
       01 FIN-DE-DATOS      PIC X VALUE 'N'.
          88 NO-HAY-MAS     VALUE 'S'.

       01 ID-ACTUAL         PIC 9(5) VALUE 1.
       01 NOMBRE-ACTUAL     PIC X(30).

       PROCEDURE DIVISION.
       PRINCIPAL.
           INITIALIZE FIN-DE-DATOS.
           OPEN OUTPUT ARCHIVO-SECUENCIAL.
           PERFORM UNTIL NO-HAY-MAS
               DISPLAY "Nombre: "
               ACCEPT NOMBRE-ACTUAL
               IF NOMBRE-ACTUAL = "FIN"
                   MOVE 'S' TO FIN-DE-DATOS
               ELSE
                   MOVE ID-ACTUAL TO IDNUM
                   MOVE NOMBRE-ACTUAL TO NOMBRE
                   WRITE REGISTRO
                   ADD 1 TO ID-ACTUAL
               END-IF
           END-PERFORM.
           CLOSE ARCHIVO-SECUENCIAL.
           STOP RUN.
