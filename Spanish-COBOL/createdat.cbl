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
           88 FS-ARCHIVO-NO-EXISTE VALUE "35".
           88 FS-INVALID-ORG    VALUE "30".
           88 FS-OTRO-ERROR     VALUE "10" THRU "99".

       01  WS-CONTINUAR         PIC X VALUE "S".
           88 WS-SI             VALUE "S", "s".

       01  WS-ENTRADA.
           05 WS-CODIGO         PIC X(5).
           05 WS-NOMBRE         PIC X(20).
           05 WS-PRECIO         PIC 9(7)V99.
           05 WS-STOCK         PIC 9(5).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INICIALIZAR.
           IF FS-OK
               PERFORM PROCESAR UNTIL NOT WS-SI
           END-IF.
           PERFORM TERMINAR.
           STOP RUN.

       INICIALIZAR.
           PERFORM INTENTAR-ABRIR-ARCHIVO.

       INTENTAR-ABRIR-ARCHIVO.
           OPEN I-O PRODUCTOS-FILE.
           EVALUATE TRUE
               WHEN FS-ARCHIVO-NO-EXISTE
                   PERFORM CREAR-ARCHIVO
               WHEN FS-INVALID-ORG
                   PERFORM RECREAR-ARCHIVO
               WHEN FS-OTRO-ERROR
                   DISPLAY "ERROR INESPERADO: " FS-PRODUCTOS
                   STOP RUN
           END-EVALUATE.

       CREAR-ARCHIVO.
           DISPLAY "CREANDO NUEVO ARCHIVO...".
           OPEN OUTPUT PRODUCTOS-FILE.
           CLOSE PRODUCTOS-FILE.
           OPEN I-O PRODUCTOS-FILE.
           IF NOT FS-OK
               DISPLAY "ERROR AL CREAR ARCHIVO: " FS-PRODUCTOS
               STOP RUN
           END-IF.

       RECREAR-ARCHIVO.
           DISPLAY "ARCHIVO EXISTENTE NO VALIDO, RECREANDO...".
           CLOSE PRODUCTOS-FILE.
           DELETE FILE PRODUCTOS-FILE.
           PERFORM CREAR-ARCHIVO.

       PROCESAR.
           DISPLAY " ".
           DISPLAY "CAPTURA DE PRODUCTOS".
           DISPLAY "CODIGO (5 caracteres): " WITH NO ADVANCING.
           ACCEPT WS-CODIGO.
           DISPLAY "NOMBRE (20 caracteres): " WITH NO ADVANCING.
           ACCEPT WS-NOMBRE.
           DISPLAY "PRECIO (9(7)V99): " WITH NO ADVANCING.
           ACCEPT WS-PRECIO.
           DISPLAY "STOCK (5 digitos): " WITH NO ADVANCING.
           ACCEPT WS-STOCK.

           MOVE WS-CODIGO TO PROD-CODIGO.
           MOVE WS-NOMBRE TO PROD-NOMBRE.
           MOVE WS-PRECIO TO PROD-PRECIO.
           MOVE WS-STOCK TO PROD-STOCK.

           WRITE PRODUCTOS-RECORD
               INVALID KEY
                   DISPLAY "ERROR: CODIGO DUPLICADO - " PROD-CODIGO
               NOT INVALID KEY
                   DISPLAY "PRODUCTO REGISTRADO CORRECTAMENTE"
           END-WRITE.

           DISPLAY " ".
           DISPLAY "DESEA CAPTURAR OTRO PRODUCTO? (S/N): "
              WITH NO ADVANCING.
           ACCEPT WS-CONTINUAR.

       TERMINAR.
           CLOSE PRODUCTOS-FILE.
           DISPLAY "PROGRAMA TERMINADO.".
