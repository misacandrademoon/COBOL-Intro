      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREAR-PRODUCTOS.

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
       FD PRODUCTOS-FILE.
       01 PRODUCTOS-RECORD.
       05 PROD-CODIGO PIC X(05).
       05 PROD-STOCK  PIC 9(05).

       WORKING-STORAGE SECTION.
       01 FS-PRODUCTOS PIC XX.
       88 FS-OK VALUE "00".

        01 WS-CONTADOR PIC 9(03) VALUE 1.

       01 PRODUCTOS-DATOS.
       05 PRODUCTO-ITEM OCCURS 3 TIMES.
       10 ITEM-LINE PIC X(18).

       PROCEDURE DIVISION.
       INICIO.

       MOVE "       00001    00100" TO ITEM-LINE(1)
       MOVE "       00002    00050" TO ITEM-LINE(2)
       MOVE "       00003    00200" TO ITEM-LINE(3)

       PERFORM ABRIR-ARCHIVO
       PERFORM CARGAR-DATOS
       PERFORM CERRAR-ARCHIVO
        STOP RUN.

       ABRIR-ARCHIVO.
        OPEN OUTPUT PRODUCTOS-FILE
       IF NOT FS-OK
       DISPLAY "ERROR AL ABRIR PRODUCTOS.DAT: " FS-PRODUCTOS
       STOP RUN
       END-IF.

       CARGAR-DATOS.
       PERFORM VARYING WS-CONTADOR FROM 1 BY 1 UNTIL WS-CONTADOR > 3
       MOVE ITEM-LINE(WS-CONTADOR)(8:5) TO PROD-CODIGO
       MOVE ITEM-LINE(WS-CONTADOR)(14:5) TO PROD-STOCK
       WRITE PRODUCTOS-RECORD
          INVALID KEY
             DISPLAY "CODIGO DUPLICADO: " PROD-CODIGO
          NOT INVALID KEY
             DISPLAY "AGREGADO: " PROD-CODIGO " STOCK: " PROD-STOCK
       END-WRITE
       END-PERFORM.

       CERRAR-ARCHIVO.
       CLOSE PRODUCTOS-FILE
       DISPLAY "ARCHIVO PRODUCTOS.DAT CREADO CORRECTAMENTE".
