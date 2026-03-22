       IDENTIFICATION DIVISION.
       PROGRAM-ID. INV-MASTER-ADV.
       AUTHOR. GEMINI-AI.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-SISTEMA-ESTADO.
           05 WS-CONTADOR-REGRS      PIC 9(03) VALUE ZERO.
           05 WS-EOF-FLAG            PIC X(01) VALUE 'N'.
              88 END-OF-FILE                   VALUE 'S'.
           05 WS-INDICE              PIC 9(02) COMP.

       01  WS-TABLA-PRODUCTOS.
           05 WS-PRODUCTO-ITEM OCCURS 50 TIMES
                                INDEXED BY IDX-PROD.
              10 WS-PROD-ID          PIC X(05).
              10 WS-PROD-NOMBRE      PIC X(20).
              10 WS-PROD-STOCK       PIC S9(4) COMP-3.
              10 WS-PROD-PRECIO      PIC 9(5)V99.

       01  WS-SALIDA-FORMATEADA.
           05 DETALLE-ID             PIC X(05).
           05 FILLER                 PIC X(2)  VALUE SPACES.
           05 DETALLE-NOM            PIC X(20).
           05 FILLER                 PIC X(2)  VALUE SPACES.
           05 DETALLE-STOCK          PIC +ZZZ9.
           05 FILLER                 PIC X(2)  VALUE SPACES.
           05 DETALLE-PRECIO         PIC $ZZ.ZZ9,99.

       PROCEDURE DIVISION.
       000-CONTROL-PRINCIPAL.
           PERFORM 100-INICIALIZAR-DATOS
           PERFORM 200-PROCESAR-INVENTARIO
           PERFORM 300-MOSTRAR-REPORTE
           STOP RUN.

       100-INICIALIZAR-DATOS.
           SET IDX-PROD TO 1
           MOVE "A100" TO WS-PROD-ID(1)
           MOVE "MONITOR 24 PULG" TO WS-PROD-NOMBRE(1)
           MOVE 15 TO WS-PROD-STOCK(1)
           MOVE 150,50 TO WS-PROD-PRECIO(1)

           SET IDX-PROD TO 2
           MOVE "B200" TO WS-PROD-ID(2)
           MOVE "TECLADO MECANICO" TO WS-PROD-NOMBRE(2)
           MOVE -5 TO WS-PROD-STOCK(2)
           MOVE 45,00 TO WS-PROD-PRECIO(2).

       200-PROCESAR-INVENTARIO.
           SEARCH WS-PRODUCTO-ITEM
               AT END
                  DISPLAY "BUSQUEDA FINALIZADA"
               WHEN WS-PROD-STOCK(IDX-PROD) < 0
                  DISPLAY "ALERTA: STOCK NEGATIVO EN ID: "
                          WS-PROD-ID(IDX-PROD)
                  PERFORM 250-CORREGIR-STOCK
           END-SEARCH.

       250-CORREGIR-STOCK.
           COMPUTE WS-PROD-STOCK(IDX-PROD) =
                   FUNCTION ABS(WS-PROD-STOCK(IDX-PROD)).

       300-MOSTRAR-REPORTE.
           DISPLAY "------------------------------------------------"
           DISPLAY "ID     NOMBRE               STOCK    PRECIO     "
           DISPLAY "------------------------------------------------"
           PERFORM VARYING WS-INDICE FROM 1 BY 1
             UNTIL WS-INDICE > 2
               MOVE WS-PROD-ID(WS-INDICE)     TO DETALLE-ID
               MOVE WS-PROD-NOMBRE(WS-INDICE) TO DETALLE-NOM
               MOVE WS-PROD-STOCK(WS-INDICE)  TO DETALLE-STOCK
               MOVE WS-PROD-PRECIO(WS-INDICE) TO DETALLE-PRECIO
               DISPLAY WS-SALIDA-FORMATEADA
           END-PERFORM.