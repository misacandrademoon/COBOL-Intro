      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
        IDENTIFICATION DIVISION.
       PROGRAM-ID. EJEMPLO-RELATIVO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-RELATIVO ASSIGN TO 'relativo.dat'
               ORGANIZATION IS RELATIVE
               ACCESS MODE IS DYNAMIC
               RELATIVE KEY IS NUM-REG.

       DATA DIVISION.
       FILE SECTION.
       FD ARCHIVO-RELATIVO.
       01  REGISTRO-REL.
           05 IDNUM   PIC 9(5).
           05 DATO    PIC X(20).

       WORKING-STORAGE SECTION.
       01 NUM-REG         PIC 9(5).
       01 OPCION          PIC X.
       01 DATO-ENTRADA    PIC X(20).
       01 ID-ACTUAL       PIC 9(5).

       PROCEDURE DIVISION.
       PRINCIPAL.
           OPEN I-O ARCHIVO-RELATIVO
           PERFORM UNTIL OPCION = 'F'
               DISPLAY "Escriba número de registro (1-99999): "
               ACCEPT NUM-REG
               DISPLAY "DATO: "
               ACCEPT DATO-ENTRADA
               MOVE NUM-REG TO IDNUM
               MOVE DATO-ENTRADA TO DATO
               WRITE REGISTRO-REL INVALID KEY
                   DISPLAY "Error escribiendo registro"
               END-WRITE
               DISPLAY "¿Desea continuar? (F para finalizar)"
               ACCEPT OPCION
           END-PERFORM
           CLOSE ARCHIVO-RELATIVO
           STOP RUN.
