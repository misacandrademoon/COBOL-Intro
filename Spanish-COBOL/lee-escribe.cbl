      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
                 IDENTIFICATION DIVISION.
       PROGRAM-ID. lee-escribe.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-ENTRADA ASSIGN TO './entrada.txt'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT ARCHIVO-SALIDA ASSIGN TO './salida.txt'
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ARCHIVO-ENTRADA.
       01 REGISTRO-ENTRADA PIC X(80).

       FD ARCHIVO-SALIDA.
       01 REGISTRO-SALIDA PIC X(80).

       WORKING-STORAGE SECTION.
       77 FIN-ARCHIVO PIC X VALUE 'N'.
          88 FIN-DE-ARCHIVO VALUE 'S'.
          88 NO-FIN-DE-ARCHIVO VALUE 'N'.

       PROCEDURE DIVISION.
       INICIO.
           OPEN INPUT ARCHIVO-ENTRADA
           OPEN OUTPUT ARCHIVO-SALIDA

           PERFORM LEER-ARCHIVO
           PERFORM PROCESAR-REGISTROS UNTIL FIN-DE-ARCHIVO

           CLOSE ARCHIVO-ENTRADA
           CLOSE ARCHIVO-SALIDA

           DISPLAY "PROCESO COMPLETADO."
           STOP RUN.

       LEER-ARCHIVO.
           READ ARCHIVO-ENTRADA
               AT END SET FIN-DE-ARCHIVO TO TRUE
               NOT AT END SET NO-FIN-DE-ARCHIVO TO TRUE
           END-READ.

       PROCESAR-REGISTROS.
           MOVE REGISTRO-ENTRADA TO REGISTRO-SALIDA
           WRITE REGISTRO-SALIDA
           PERFORM LEER-ARCHIVO.
