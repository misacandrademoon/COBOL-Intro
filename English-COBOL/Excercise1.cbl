      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      *
      *******************************************************************
              IDENTIFICATION DIVISION.
       PROGRAM-ID. CAPTURA-PRODUCTOS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRODUCTOS-FILE ASSIGN TO "PRODUCTOS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PROD-CODIGO
               FILE STATUS IS FS-PRODUCTOS.

       DATA DIVISION.
       FILE SECTION.
       FD  PRODUCTOS-FILE.
       01  PRODUCTOS-RECORD.
           05 PROD-CODIGO       PIC X(5).
           05 PROD-NOMBRE       PIC X(20).
           05 PROD-PRECIO       PIC 9(7)V99.
           05 PROD-STOCK        PIC 9(5).

       WORKING-STORAGE SECTION.
       01  FS-PRODUCTOS         PIC XX.
           88 FS-OK             VALUE "00".
           88 FS-DUPLICADO      VALUE "22".
           88 FS-NO-ENCONTRADO  VALUE "23".

       01  WS-CONTINUAR         PIC X VALUE "S".
           88 WS-SI             VALUE "S", "s".

       01  WS-ENTRADA.
           05 WS-CODIGO         PIC X(5).
           05 WS-NOMBRE         PIC X(20).
           05 WS-PRECIO         PIC 9(7)V99.
           05 WS-STOCK          PIC 9(5).

       01  WS-PRECIO-EDITADO    PIC Z,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INICIALIZAR.
           PERFORM PROCESAR UNTIL NOT WS-SI.
           PERFORM TERMINAR.
           STOP RUN.

       INICIALIZAR.
           OPEN I-O PRODUCTOS-FILE.
           IF FS-PRODUCTOS NOT = "00" AND NOT = "05"
              DISPLAY "ERROR AL ABRIR ARCHIVO: " FS-PRODUCTOS
              STOP RUN
           END-IF.

       PROCESAR.
           DISPLAY " ".
           DISPLAY "----------------------------------".
           DISPLAY "CAPTURA DE PRODUCTOS".
           DISPLAY "----------------------------------".

           DISPLAY "CODIGO (5 caracteres): ".
           ACCEPT WS-CODIGO.

           DISPLAY "NOMBRE (20 caracteres): ".
           ACCEPT WS-NOMBRE.

           DISPLAY "PRECIO (ej: 1000000.50): ".
           ACCEPT WS-PRECIO.

           DISPLAY "STOCK (max 99999): ".
           ACCEPT WS-STOCK.

           MOVE WS-CODIGO TO PROD-CODIGO.
           MOVE WS-NOMBRE TO PROD-NOMBRE.
           MOVE WS-PRECIO TO PROD-PRECIO.
           MOVE WS-STOCK TO PROD-STOCK.

           WRITE PRODUCTOS-RECORD
               INVALID KEY
                   DISPLAY "ERROR: CODIGO DUPLICADO - " PROD-CODIGO
               NOT INVALID KEY
                   MOVE PROD-PRECIO TO WS-PRECIO-EDITADO
                   DISPLAY "PRODUCTO REGISTRADO:"
                   DISPLAY "CODIGO: " PROD-CODIGO
                   DISPLAY "NOMBRE: " PROD-NOMBRE
                   DISPLAY "PRECIO: " WS-PRECIO-EDITADO
                   DISPLAY "STOCK: " PROD-STOCK
           END-WRITE.

           DISPLAY " ".
           DISPLAY "¿DESEA CAPTURAR OTRO PRODUCTO? (S/N): ".
           ACCEPT WS-CONTINUAR.

       TERMINAR.
           CLOSE PRODUCTOS-FILE.
