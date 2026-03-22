       IDENTIFICATION DIVISION.
       PROGRAM-ID. GESTION-MATERIA-PRO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MATERIAL-FILE ASSIGN TO "MATERIA.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS MAT-ID
               FILE STATUS IS FS-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  MATERIAL-FILE.
       01  MATERIAL-REG.
           05 MAT-ID            PIC 9(05).
           05 MAT-NOMBRE        PIC X(20).
           05 MAT-CANTIDAD      PIC 9(06).
           05 MAT-MINIMO        PIC 9(04).

       WORKING-STORAGE SECTION.
       01  WS-VARIABLES.
           05 FS-STATUS         PIC XX.
           05 WS-OPCION         PIC 9.
           05 WS-FIN            PIC X VALUE 'N'.
           05 WS-GASTO          PIC 9(06).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN I-O MATERIAL-FILE
           IF FS-STATUS = "35"
               OPEN OUTPUT MATERIAL-FILE
               PERFORM CARGA-INICIAL-BASE
               CLOSE MATERIAL-FILE
               OPEN I-O MATERIAL-FILE
           END-IF.

           PERFORM UNTIL WS-FIN = 'S'
               DISPLAY " "
               DISPLAY "=== PANEL DE CONTROL DE SUMINISTROS ==="
               DISPLAY "1. Registrar Nuevo Material"
               DISPLAY "2. Añadir Material (Setear)"
               DISPLAY "3. Gastar Material (Salida)"
               DISPLAY "4. Ver Inventario y ALERTAS"
               DISPLAY "5. Salir"
               DISPLAY "Seleccione: " WITH NO ADVANCING
               ACCEPT WS-OPCION

               EVALUATE WS-OPCION
                   WHEN 1 PERFORM ALTA-MATERIAL
                   WHEN 2 PERFORM MODIF-MATERIAL
                   WHEN 3 PERFORM GASTAR-MATERIAL
                   WHEN 4 PERFORM LISTAR-ALERTAS
                   WHEN 5 MOVE 'S' TO WS-FIN
                   WHEN OTHER DISPLAY "Opcion invalida."
               END-EVALUATE
           END-PERFORM.
           CLOSE MATERIAL-FILE
           STOP RUN.

       GASTAR-MATERIAL.
           DISPLAY "ID del material a gastar: " ACCEPT MAT-ID
           READ MATERIAL-FILE
               INVALID KEY DISPLAY "Error: No existe ese material."
               NOT INVALID KEY
                   DISPLAY "Material: " MAT-NOMBRE
                   DISPLAY "Stock actual: " MAT-CANTIDAD
                   DISPLAY "Cantidad a retirar: " ACCEPT WS-GASTO

                   IF WS-GASTO > MAT-CANTIDAD
                       DISPLAY "ERROR: No hay stock suficiente."
                   ELSE
                       SUBTRACT WS-GASTO FROM MAT-CANTIDAD
                       REWRITE MATERIAL-REG
                       DISPLAY "Gasto registrado."
                       IF MAT-CANTIDAD <= 5
                           DISPLAY "!!! AVISO: STOCK CRITICO !!!"
                       END-IF
                   END-IF
           END-READ.

       CARGA-INICIAL-BASE.
           DISPLAY "Generando materiales de base..."
           MOVE 00001 TO MAT-ID
           MOVE "HIERRO CORRUGADO" TO MAT-NOMBRE
           MOVE 000050 TO MAT-CANTIDAD
           MOVE 0010 TO MAT-MINIMO
           WRITE MATERIAL-REG
           MOVE 00002 TO MAT-ID
           MOVE "CEMENTO GRIS" TO MAT-NOMBRE
           MOVE 000004 TO MAT-CANTIDAD
           MOVE 0015 TO MAT-MINIMO
           WRITE MATERIAL-REG
           DISPLAY "Carga inicial completada.".

       ALTA-MATERIAL.
           DISPLAY "ID: " ACCEPT MAT-ID
           DISPLAY "Nombre: " ACCEPT MAT-NOMBRE
           DISPLAY "Stock: " ACCEPT MAT-CANTIDAD
           DISPLAY "Minimo: " ACCEPT MAT-MINIMO
           WRITE MATERIAL-REG
               INVALID KEY DISPLAY "ID duplicado."
               NOT INVALID KEY DISPLAY "Guardado."
           END-WRITE.

       MODIF-MATERIAL.
           DISPLAY "ID a modificar: " ACCEPT MAT-ID
           READ MATERIAL-FILE
               INVALID KEY DISPLAY "No encontrado."
               NOT INVALID KEY
                   DISPLAY "Nombre: " MAT-NOMBRE
                   DISPLAY "Nuevo stock: " ACCEPT MAT-CANTIDAD
                   REWRITE MATERIAL-REG
                   DISPLAY "Actualizado."
           END-READ.

       LISTAR-ALERTAS.
           MOVE 0 TO MAT-ID
           START MATERIAL-FILE KEY NOT LESS MAT-ID
           DISPLAY "--------------------------------------------"
           DISPLAY "ID    | NOMBRE              | STK | ESTADO"
           DISPLAY "--------------------------------------------"
           PERFORM UNTIL FS-STATUS = "10"
               READ MATERIAL-FILE NEXT
                   AT END MOVE "10" TO FS-STATUS
                   NOT AT END
                       DISPLAY MAT-ID " | " MAT-NOMBRE " | "
                               MAT-CANTIDAD " | " WITH NO ADVANCING
                       IF MAT-CANTIDAD <= 5
                           DISPLAY "CRITICO"
                       ELSE
                           IF MAT-CANTIDAD < MAT-MINIMO
                               DISPLAY "BAJO"
                           ELSE
                               DISPLAY "OK"
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           MOVE "00" TO FS-STATUS.