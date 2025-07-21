       IDENTIFICATION DIVISION.
       PROGRAM-ID. Aritmeti.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  NUMERO-1        PIC 9(3) VALUE 20.
       77  NUMERO-2        PIC 9(3) VALUE 5.
       77  RESULTADO       PIC 9(5)V99.
       77  RESTO           PIC 9(3).

       PROCEDURE DIVISION.

           DISPLAY "Numero 1: " NUMERO-1
           DISPLAY "Numero 2: " NUMERO-2

           ADD NUMERO-1 TO NUMERO-2 GIVING RESULTADO
           DISPLAY "Suma (ADD): " RESULTADO

           SUBTRACT NUMERO-2 FROM NUMERO-1 GIVING RESULTADO
           DISPLAY "Resta (SUBTRACT): " RESULTADO

           MULTIPLY NUMERO-1 BY NUMERO-2 GIVING RESULTADO
           DISPLAY "Multiplicacion (MULTIPLY): " RESULTADO

           DIVIDE NUMERO-1 BY NUMERO-2 GIVING RESULTADO REMAINDER RESTO
           DISPLAY "Division DIVIDE " NUMERO-1 " BY " NUMERO-2 " = "
                      RESULTADO "  Resto:" RESTO

           DIVIDE NUMERO-1 INTO NUMERO-2 GIVING RESULTADO REMAINDER
                                                           RESTO
           DISPLAY "Division DIVIDE " NUMERO-1 " INTO " NUMERO-2 " = "
                      RESULTADO " Resto: "    RESTO

           DISPLAY " "
           COMPUTE RESULTADO = NUMERO-1 + NUMERO-2 / NUMERO-1 * NUMERO-2
           COMPUTE RESULTADO = RESULTADO - NUMERO-1
           DISPLAY "RESULTADO (COMPUTE): " RESULTADO

           STOP RUN.
