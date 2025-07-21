
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EJEMPLO-INDEXADO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-CLIENTES ASSIGN TO 'clientes.idx'
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CLAVE-CLIENTE.

       DATA DIVISION.
       FILE SECTION.
       FD ARCHIVO-CLIENTES.
       01 REGISTRO-CLIENTE.
           05 CLAVE-CLIENTE   PIC X(10).
           05 NOMBRE-CLIENTE  PIC X(30).
           05 TELEFONO        PIC X(15).

       WORKING-STORAGE SECTION.
       01 OPCION          PIC X.
       01 CLAVE-BUSQUEDA  PIC X(10).
       01 NOMBRE-TEMP     PIC X(30).
       01 TELEFONO-TEMP   PIC X(15).

       PROCEDURE DIVISION.
       PRINCIPAL.
           OPEN I-O ARCHIVO-CLIENTES
           PERFORM UNTIL OPCION = 'F'
               DISPLAY "A - Agregar | B - Buscar | F - Finalizar"
               ACCEPT OPCION
               EVALUATE OPCION
                   WHEN 'A'
                       DISPLAY "Clave: "
                       ACCEPT CLAVE-CLIENTE
                       DISPLAY "Nombre: "
                       ACCEPT NOMBRE-CLIENTE
                       DISPLAY "Teléfono: "
                       ACCEPT TELEFONO
                       WRITE REGISTRO-CLIENTE INVALID KEY
                           DISPLAY "Clave duplicada"
                       END-WRITE
                   WHEN 'B'
                       DISPLAY "Clave a buscar: "
                       ACCEPT CLAVE-BUSQUEDA
                       MOVE CLAVE-BUSQUEDA TO CLAVE-CLIENTE
                       READ ARCHIVO-CLIENTES
                           INVALID KEY DISPLAY "No encontrado"
                           NOT INVALID DISPLAY NOMBRE-CLIENTE " - "
                                               TELEFONO
                       END-READ
               END-EVALUATE
           END-PERFORM
           CLOSE ARCHIVO-CLIENTES
           STOP RUN.
