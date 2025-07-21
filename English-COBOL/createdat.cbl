             IDENTIFICATION DIVISION.
       PROGRAM-ID. PRODUCT-ENTRY.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRODUCTS-FILE ASSIGN TO "PRODUCTS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PROD-CODE
               FILE STATUS IS FS-PRODUCTS.

       DATA DIVISION.
       FILE SECTION.
       FD  PRODUCTS-FILE.
       01  PRODUCTS-RECORD.
           05 PROD-CODE         PIC X(5).
           05 PROD-NAME         PIC X(20).
           05 PROD-PRICE        PIC 9(7)V99.
           05 PROD-STOCK        PIC 9(5).

       WORKING-STORAGE SECTION.
       01  FS-PRODUCTS          PIC XX.
           88 FS-OK             VALUE "00".
           88 FS-DUPLICATE      VALUE "22".
           88 FS-NOT-FOUND      VALUE "23".
           88 FS-FILE-NOT-EXIST VALUE "35".
           88 FS-INVALID-ORG    VALUE "30".
           88 FS-OTHER-ERROR    VALUE "10" THRU "99".

       01  WS-CONTINUE          PIC X VALUE "Y".
           88 WS-YES            VALUE "Y", "y".

       01  WS-INPUT.
           05 WS-CODE           PIC X(5).
           05 WS-NAME           PIC X(20).
           05 WS-PRICE          PIC 9(7)V99.
           05 WS-STOCK          PIC 9(5).

           PROCEDURE DIVISION.
           MAIN-PROCEDURE.
           PERFORM INITIALIZE.
           IF FS-OK
               PERFORM PROCESS UNTIL NOT WS-YES
           END-IF.
           PERFORM TERMINATE.
           STOP RUN.

            INITIALIZE.
           PERFORM TRY-OPEN-FILE.

            TRY-OPEN-FILE.
           OPEN I-O PRODUCTS-FILE.
           EVALUATE TRUE
               WHEN FS-FILE-NOT-EXIST
                   PERFORM CREATE-FILE
               WHEN FS-INVALID-ORG
                   PERFORM RECREATE-FILE
               WHEN FS-OTHER-ERROR
                   DISPLAY "UNEXPECTED ERROR: " FS-PRODUCTS
                   STOP RUN
           END-EVALUATE.

           CREATE-FILE.
           DISPLAY "CREATING NEW FILE...".
           OPEN OUTPUT PRODUCTS-FILE.
           CLOSE PRODUCTS-FILE.
           OPEN I-O PRODUCTS-FILE.
           IF NOT FS-OK
               DISPLAY "ERROR CREATING FILE: " FS-PRODUCTS
               STOP RUN
           END-IF.

            RECREATE-FILE.
           DISPLAY "INVALID EXISTING FILE, RECREATING...".
           CLOSE PRODUCTS-FILE.
           DELETE FILE PRODUCTS-FILE.
           PERFORM CREATE-FILE.

           PROCESS.
           DISPLAY " ".
           DISPLAY "PRODUCT ENTRY".
           DISPLAY "CODE (5 characters): " WITH NO ADVANCING.
           ACCEPT WS-CODE.
           DISPLAY "NAME (20 characters): " WITH NO ADVANCING.
           ACCEPT WS-NAME.
           DISPLAY "PRICE (9(7)V99): " WITH NO ADVANCING.
           ACCEPT WS-PRICE.
           DISPLAY "STOCK (5 digits): " WITH NO ADVANCING.
           ACCEPT WS-STOCK.

           MOVE WS-CODE TO PROD-CODE.
           MOVE WS-NAME TO PROD-NAME.
           MOVE WS-PRICE TO PROD-PRICE.
           MOVE WS-STOCK TO PROD-STOCK.

           WRITE PRODUCTS-RECORD
               INVALID KEY
                   DISPLAY "ERROR: DUPLICATE CODE - " PROD-CODE
               NOT INVALID KEY
                   DISPLAY "PRODUCT SUCCESSFULLY REGISTERED"
           END-WRITE.

           DISPLAY " ".
           DISPLAY "DO YOU WANT TO ENTER ANOTHER PRODUCT? (Y/N): "
              WITH NO ADVANCING.
           ACCEPT WS-CONTINUE.

           END.
           CLOSE PRODUCTS-FILE.
           DISPLAY "PROGRAM FINISHED.".
