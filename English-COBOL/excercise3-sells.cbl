      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
                  IDENTIFICATION DIVISION.
       PROGRAM-ID. CREATE-SELLS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRODUCTOS-FILE ASSIGN TO "PRODUCTS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PROD-CODIGO
               FILE STATUS IS FS-PRODUCTOS.

           SELECT VENTAS-FILE ASSIGN TO "SELLS.DAT"
               ORGANIZATION IS SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS FS-VENTAS.

       DATA DIVISION.
       FILE SECTION.
       FD PRODUCTOS-FILE.
       01 PRODUCTOS-RECORD.
           05 PROD-CODIGO         PIC X(05).
           05 PROD-NOMBRE         PIC X(20).
           05 PROD-PRECIO         PIC 9(07)V99.
           05 PROD-STOCK          PIC 9(05).

       FD VENTAS-FILE.
       01 VENTAS-RECORD.
           05 VENTA-CODIGO        PIC X(05).
           05 VENTA-CANTIDAD      PIC 9(05).

       WORKING-STORAGE SECTION.
       01 FS-PRODUCTOS           PIC XX.
       01 FS-VENTAS              PIC XX.
       01 WS-INDICE              PIC 9(03).
       01 WS-CONTADOR            PIC 9(03) VALUE ZERO.
       01 WS-COD-VENTA           PIC X(05).
       01 WS-CANT-VENTA          PIC 9(05).

       PROCEDURE DIVISION.
       INICIO.
           PERFORM ABRIR-ARCHIVOS
           PERFORM GENERAR-VENTAS
           PERFORM CERRAR-ARCHIVOS
           STOP RUN.

       ABRIR-ARCHIVOS.
           OPEN INPUT PRODUCTOS-FILE
           IF FS-PRODUCTOS NOT = "00"
               DISPLAY "ERROR AL ABRIR PRODUCTOS.DAT"
               STOP RUN
           END-IF

           OPEN OUTPUT VENTAS-FILE
           IF FS-VENTAS NOT = "00"
               DISPLAY "ERROR AL CREAR SELLS.DAT"
               STOP RUN
           END-IF
           .

       GENERAR-VENTAS.
           DISPLAY "GENERANDO ARCHIVO SELLS.DAT..."

           MOVE "00001" TO WS-COD-VENTA
           MOVE 5 TO WS-CANT-VENTA
           PERFORM ESCRIBIR-VENTA

           MOVE "00002" TO WS-COD-VENTA
           MOVE 10 TO WS-CANT-VENTA
           PERFORM ESCRIBIR-VENTA

           MOVE "00003" TO WS-COD-VENTA
           MOVE 3 TO WS-CANT-VENTA
           PERFORM ESCRIBIR-VENTA

           MOVE "00001" TO WS-COD-VENTA
           MOVE 2 TO WS-CANT-VENTA
           PERFORM ESCRIBIR-VENTA

           MOVE "00002" TO WS-COD-VENTA
           MOVE 1 TO WS-CANT-VENTA
           PERFORM ESCRIBIR-VENTA

           DISPLAY "TOTAL SELLS: " WS-CONTADOR
           .

       ESCRIBIR-VENTA.
       DISPLAY "Searching Code: " WS-COD-VENTA
       READ PRODUCTOS-FILE KEY IS WS-COD-VENTA
        INVALID KEY
            DISPLAY "DON´T EXIST PRODUCT: " WS-COD-VENTA
        NOT INVALID KEY
            DISPLAY "PRODUCT: " PROD-CODIGO
            MOVE WS-COD-VENTA TO VENTA-CODIGO
            MOVE WS-CANT-VENTA TO VENTA-CANTIDAD
            WRITE VENTAS-RECORD
            IF FS-VENTAS = "00"
                ADD 1 TO WS-CONTADOR
                DISPLAY "REGISTER SELL: " WS-COD-VENTA
            ELSE
                DISPLAY "ERROR WRITE SELL"
            END-IF
       END-READ.
