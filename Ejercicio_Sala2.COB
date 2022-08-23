      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Cecilia_Olmos.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SOCIOS
           ASSIGN TO
           "D:\linux cecilia\COBOL\archivo\archSocios.dat".

           SELECT SOCIOS-ACT
           ASSIGN TO
           "D:\linux cecilia\COBOL\archivo\archSociosAct.dat".

           SELECT NOVEDADES
           ASSIGN TO
           "D:\linux cecilia\COBOL\archivo\archNoved.dat".

           SELECT LOG-ERRORES
           ASSIGN TO
           "D:\linux cecilia\COBOL\archivo\archErrores.dat".

       DATA DIVISION.
       FILE SECTION.
       FD  SOCIOS.
       01  soc-reg.
           03 soc-socio pic 9(4).
           03 soc-nombre pic x(20).
           03 soc-importe pic s9(8)v99.

       FD  SOCIOS-ACT.
       01  soc-act-reg.
           03 soc-act-socio pic 9(4).
           03 soc-act-nombre pic x(20).
           03 soc-act-importe pic s9(8)v99.

       FD  NOVEDADES.
       01  nov-reg.
           03 nov-socio pic 9(4).
           03 nov-movimiento pic x.
           03 nov-nombre pic x(20).
           03 nov-importe pic s9(7)v99.

       FD  LOG-ERRORES.
       01  log-reg.
           03 log-socio pic 9(4).
           03 log-movimiento pic x.
           03 log-importe pic s9(7)v99.
           03 log-error pic x(40).

       WORKING-STORAGE SECTION.
       01  w-flag-soc-act pic 9.
       01  w-flag-error pic 9.

       01  lin-guarda.
           03 filler pic x(80) value all "*".
       01  lin-titulo.
           03 filler pic x(25) value spaces.
           03 filler pic x(30) value "MOVIMIENTOS BANCO EL CORRALITO".
           03 filler pic x(25) value spaces.
       01  lin-socio-act.
           03 filler pic x(27) value spaces.
           03 filler pic x(26) value "ARCHIVO SOCIOS ACTUALIZADO".
           03 filler pic x(27) value spaces.
       01  lin-subrayado.
           03 filler pic x(27) value space.
           03 filler pic x(26) value all "-".
           03 filler pic x(27) value space.
       01  lin-soc.
           03 filler pic x(17) value spaces.
           03 filler pic x(5) value "SOCIO".
           03 filler pic x(8) value spaces.
           03 filler pic x(20) value "NOMBRE".
           03 filler pic x(8) value spaces.
           03 filler pic x(10) value "IMPORTE".
           03 filler pic x(17) value spaces.
       01  lin-val.
           03 filler pic x(17) value spaces.
           03 l-soc pic x(5) value spaces.
           03 filler pic x(8) value spaces.
           03 l-nombre pic x(20).
           03 filler pic x(8) value spaces.
           03 l-saldo pic zz.zzz.zz9,99.
           03 filler pic x(17) value spaces.
       01  lin-error.
           03 filler pic x(31) value spaces.
           03 filler pic x(18) value "ARCHIVO DE ERRORES".
           03 filler pic x(31) value spaces.
       01  lin-subrayado-errores.
           03 filler pic x(31) value space.
           03 filler pic x(18) value all "-".
           03 filler pic x(31) value space.
       01  lin-val-error.
           03 filler pic x(10) value spaces.
           03 l-soc-error pic x(5) value spaces.
           03 filler pic x(3) value spaces.
           03 l-nombre-error pic x value spaces.
           03 filler pic x(4) value spaces.
           03 l-saldo-error pic zz.zzz.zz9,99.
           03 filler pic x(5) value spaces.
           03 l-tipo-error pic x(40) value space.
           03 filler pic x(12) value space.

      ***** Flags Fin De Archivo **********************************************

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM 100-INICIO-GENERAL.
            PERFORM 150-LEER-SOCIO.
            PERFORM 260-PASO-DATOS-SOCIOS-A-ACT.
            PERFORM 151-LEER-NOVEDAD.
            PERFORM UNTIL soc-socio IS EQUALS 9999 AND
                          nov-socio IS EQUALS 9999 AND
                          soc-act-socio IS EQUALS 9999
               IF nov-socio > soc-act-socio THEN
                   PERFORM 200-PROCESO-X-MAYOR
               ELSE
                   IF nov-socio IS EQUALS soc-act-socio THEN
                       PERFORM 300-PROCESO-X-IGUAL
                   END-IF
                   IF nov-socio < soc-act-socio THEN
                       PERFORM 400-PROCESO-X-MENOR
                   END-IF
                   PERFORM 340-LEO-NOVEDADES-VER-FIN-ARCH
               END-IF
            END-PERFORM.
            PERFORM 500-FIN-GENERAL.
            STOP RUN.

       100-INICIO-GENERAL.
           PERFORM 120-ABRO-ARCHIVOS.

       120-ABRO-ARCHIVOS.
           OPEN INPUT SOCIOS.
           OPEN INPUT NOVEDADES.
           OPEN OUTPUT LOG-ERRORES.
           OPEN OUTPUT SOCIOS-ACT.

       150-LEER-SOCIO.
           READ SOCIOS AT END MOVE 9999 TO soc-socio.

       151-LEER-NOVEDAD.
           READ NOVEDADES AT END MOVE 9999 TO nov-socio.

       200-PROCESO-X-MAYOR.
           PERFORM 220-GRABO-EN-SOCIOS-ACT.
           IF nov-movimiento IS NOT EQUALS "A" THEN
               PERFORM 240-LEO-SOCIOS-VER-FIN-ARCH.
           PERFORM 260-PASO-DATOS-SOCIOS-A-ACT.

       220-GRABO-EN-SOCIOS-ACT.
           WRITE soc-act-reg.

       240-LEO-SOCIOS-VER-FIN-ARCH.
           IF soc-socio IS NOT EQUALS 9999 THEN
               PERFORM 150-LEER-SOCIO.

       260-PASO-DATOS-SOCIOS-A-ACT.
           MOVE soc-reg TO soc-act-reg.

       300-PROCESO-X-IGUAL.
           IF nov-movimiento IS EQUALS "A" THEN
              PERFORM 310-ARMO-LINEA-ERROR-AARE
              PERFORM 420-GRABO-EN-LOG-ERRORES
           ELSE
               IF nov-movimiento IS EQUALS "B" THEN
                   PERFORM 240-LEO-SOCIOS-VER-FIN-ARCH
                   PERFORM 260-PASO-DATOS-SOCIOS-A-ACT
               ELSE
                   PERFORM 350-MODIFICO-DAT-NOV-A-ACT
               END-IF
           END-IF.

       310-ARMO-LINEA-ERROR-AARE.
           MOVE soc-act-socio TO log-socio.
           MOVE "A" TO log-movimiento.
           MOVE soc-act-importe TO log-importe.
           MOVE "A.A.R.E" TO log-error.

       340-LEO-NOVEDADES-VER-FIN-ARCH.
           IF nov-socio IS NOT EQUALS 9999 THEN
               PERFORM 151-LEER-NOVEDAD.

       350-MODIFICO-DAT-NOV-A-ACT.
           IF nov-nombre IS NOT EQUALS " " THEN
               MOVE nov-nombre TO soc-nombre.
           IF nov-importe IS NOT EQUALS " " THEN
               ADD nov-importe TO soc-act-importe.

       400-PROCESO-X-MENOR.
           IF nov-movimiento IS EQUALS "A" THEN
               PERFORM 460-PASO-DAT-NOV-A-ACT
           ELSE
               IF soc-act-socio IS NOT EQUALS 9999 THEN
                   PERFORM 430-VERIFICO-ERROR-MARI-O-BARI
                   PERFORM 420-GRABO-EN-LOG-ERRORES
               END-IF
           END-IF.

       420-GRABO-EN-LOG-ERRORES.
           WRITE log-reg.

       430-VERIFICO-ERROR-MARI-O-BARI.
           IF nov-movimiento IS EQUALS "M" THEN
               PERFORM 435-ARMO-LINEA-ERROR-MARI
           END-IF.
           IF nov-movimiento IS EQUALS "B" THEN
               PERFORM 436-ARMO-LINEA-ERROR-BARI
           END-IF.

       435-ARMO-LINEA-ERROR-MARI.
           MOVE soc-act-socio TO log-socio.
           MOVE "M" TO log-movimiento.
           MOVE soc-act-importe TO log-importe.
           MOVE "M.A.R.I" TO log-error.

       436-ARMO-LINEA-ERROR-BARI.
           MOVE soc-act-socio TO log-socio.
           MOVE "B" TO log-movimiento.
           MOVE soc-act-importe TO log-importe.
           MOVE "B.A.R.I" TO log-error.

       460-PASO-DAT-NOV-A-ACT.
           MOVE nov-socio TO soc-act-socio.
           MOVE nov-nombre TO soc-act-nombre.
           MOVE nov-importe TO soc-act-importe.

       500-FIN-GENERAL.
           PERFORM 510-CIERRO-ARCHIVOS.
           PERFORM 530-MUESTRO-ARCHIVOS.


       510-CIERRO-ARCHIVOS.
           CLOSE SOCIOS.
           CLOSE NOVEDADES.
           CLOSE LOG-ERRORES.
           CLOSE SOCIOS-ACT.

       530-MUESTRO-ARCHIVOS.
           DISPLAY lin-guarda.
           DISPLAY lin-titulo.
           DISPLAY lin-guarda.
           DISPLAY lin-socio-act.
           DISPLAY lin-subrayado.
           DISPLAY lin-soc.
           PERFORM 700-MUESTRO-ACT.
           DISPLAY lin-guarda.
           DISPLAY lin-error.
           DISPLAY lin-subrayado-errores.
           PERFORM 600-MUESTRO-ERROR.


       600-MUESTRO-ERROR.
           OPEN INPUT LOG-ERRORES.
           MOVE ZERO TO w-flag-error.
           PERFORM 610-LEER-ERROR.
           PERFORM UNTIL w-flag-error IS EQUAL 1
               PERFORM 620-ARMO-LINEA-ERROR
               PERFORM 610-LEER-ERROR
           END-PERFORM.
           CLOSE LOG-ERRORES.
       610-LEER-ERROR.
           READ LOG-ERRORES AT END MOVE 1 TO w-flag-error.

       700-MUESTRO-ACT.
            OPEN INPUT SOCIOS-ACT.
            MOVE ZERO to w-flag-soc-act.
            PERFORM 710-LEER-SOCIO-ACT.
            PERFORM UNTIL w-flag-soc-act is EQUAL 1
               PERFORM 720-ARMO-LINEA-ACT
               PERFORM 710-LEER-SOCIO-ACT
            END-PERFORM.
            CLOSE SOCIOS-ACT.

       710-LEER-SOCIO-ACT.
            READ SOCIOS-ACT AT END MOVE 1 TO w-flag-soc-act.

       720-ARMO-LINEA-ACT.
           MOVE soc-act-socio TO l-soc.
           MOVE soc-act-nombre TO l-nombre.
           MOVE soc-act-importe TO l-saldo.
           DISPLAY lin-val.
       620-ARMO-LINEA-ERROR.
           MOVE log-socio TO l-soc-error.
           MOVE log-movimiento TO l-nombre-error.
           MOVE log-importe TO l-saldo-error.
           MOVE log-error TO l-tipo-error.

           DISPLAY lin-val-error.

           END PROGRAM Cecilia_Olmos.
