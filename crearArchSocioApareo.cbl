      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SOCIOS
           ASSIGN TO
           "D:\linux cecilia\COBOL\archivo\archSocios.dat".

       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01  soc-reg.
           03 soc-soc pic 9(4).
           03 soc-nombre pic x(20).
           03 soc-saldo pic s9(8)v99.

       WORKING-STORAGE SECTION.
       01  cont pic 999 value zeros.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 200-ING-SOCIOS
           PERFORM UNTIL soc-soc = 0
               PERFORM 300-INGRESO-CODIGO
               PERFORM 400-PROCESO
               PERFORM 200-ING-SOCIOS
           END-PERFORM.
           PERFORM 500-FIN.

            STOP RUN.

       100-INICIO.
           DISPLAY "PROGRAMA PARA GRABAR ARCHIVO SOCIOS".
           OPEN OUTPUT SOCIOS.
       200-ING-SOCIOS.
           PERFORM 210-TRAIGO-SOCIO
           PERFORM UNTIL soc-soc >= 0 AND soc-soc < 9999
               DISPLAY "ERROR"
               PERFORM 210-TRAIGO-SOCIO
           END-PERFORM.
       210-TRAIGO-SOCIO.
           DISPLAY "INGRESE NUMERO DE CODIGO, X FIN INGRESE 0".
           ACCEPT soc-soc.
       300-INGRESO-CODIGO.
           PERFORM 320-ING-NOMBRE.
           PERFORM UNTIL soc-nombre <> SPACE
               DISPLAY "ERROR"
               PERFORM 320-ING-NOMBRE
           END-PERFORM.
       320-ING-NOMBRE.
           DISPLAY "INGRESE NOMBRE DEL SOCIO".
           ACCEPT soc-nombre.
       400-PROCESO.
           DISPLAY "INGRESE SALDO".
           ACCEPT soc-saldo.
           WRITE soc-reg.
       500-FIN.
           CLOSE SOCIOS.
       END PROGRAM YOUR-PROGRAM-NAME.
