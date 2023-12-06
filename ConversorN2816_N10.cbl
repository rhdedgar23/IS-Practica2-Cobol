      * Conversor de numeros binarios, octales y hexadecimales a decimales
      * Elaboró Edgar Daniel Rodriguez Herrera
      
      * Columna 1-6 numeros de secuencia
      * Columna 7 reservada para comentarios
      * Columna 8-11 Area A: DIVISION headers, SECTION headers, PARAGRAPH
      * headers, PARAGRAPH names
      * Columna 12-72 Area B: codigo funcional
      * Columna 73-80 Area de identificacion; ignorada por el compilador

       IDENTIFICATION DIVISION.
       *> Se requiere el PROGRAM-ID; es el nombre del programa
       PROGRAM-ID. conversorN2816_N10.
       *> DATA DIVISION para variables globales
       DATA DIVISION.
        WORKING-STORAGE SECTION.
           *>Opcion de Menu Principal
           01 opcion PIC 9(1).
           *>Opcion de Submenu's
           01 opcion2 PIC 9(1).

           *>Contador de indices (como funcion length)
           01 cont PIC 9(3) VALUE 1. 
           *>Contador auxiliar
           01 contaux PIC 9(3) VALUE 1.
           *>Potencia para Metodo General
           01 pot PIC 9(36) VALUE 1.
           *>Numero decimal resultante
           01 num_dec PIC 9(36) VALUE 0.
           *>Numero a convertir
           *>y tabla de hasta 100 casillas de un caracter cada una
           01 BinOctDecHex. 02 indice OCCURS 100 PIC X(1).

           *>Digito de Numero Binario
           01 digito_B PIC 9(36).
               88 numero_B VALUES ARE 00 THRU 01.
           *>Digito de Numero Octal     
           01 digito_O PIC 9(36).
               88 numero_O VALUES ARE 00 THRU 07.         
           *>Digito de Numero Decimal
           01 digito_D PIC 9(1).
               88 numero_D VALUES ARE 00 THRU 09.
           *>Digito de Numero Hexadecimal     
           01 digito_H PIC X(36).
               88 numero_H VALUES ARE '0' THRU '9'. 
               88 letra_H VALUES ARE 'A' THRU 'F'. 
           01 val_H PIC 9(36) VALUE 0.

           01 residuo PIC 9(1).
           01 residuo_h PIC 9(2).
           01 Binario. 02 indice1 OCCURS 100 PIC 9(1).
           01 Octal. 02 indice2 OCCURS 100 PIC 9(1).
           01 Hexadecimal. 02 indice3 OCCURS 100 PIC X(1).

      *PROCEDURE DIVISION para prodecimiento principal     
       PROCEDURE DIVISION.
      *MENU PRINCIPAL
           MENU-PRINCIPAL.
               *> Opciones del menu
               DISPLAY 'Conversor de Sistemas de Numeracion Posicional'.
               DISPLAY 'Menu:'.
               DISPLAY '1. Conversion de N(x) -> N(10)'.
               DISPLAY '2. Conversion de N(10) -> N(x)'.
               DISPLAY '3. Salir del Programa'.

           PERFORM UNTIL opcion = 3
               DISPLAY 'Elige una de las opciones anteriores.'
               ACCEPT opcion
               EVALUATE opcion
                   WHEN 1
                       PERFORM SUBMENU1
                   WHEN 2
                       PERFORM SUBMENU2
                   WHEN 3
                       DISPLAY 'Hasta pronto!'
                   WHEN OTHER
                       DISPLAY 'Opcion Invalida!'
               END-EVALUATE
      *         PERFORM MENU-PRINCIPAL
           END-PERFORM.
           STOP RUN.
           
      * Menu Conversion N(x) a N(10)
           SUBMENU1.
               *> Opciones del submenu
               DISPLAY 'Conversor de N(x) -> N(10)'.
               DISPLAY 'Menu:'.
               DISPLAY '1. Conversion de N(2) -> N(10)'.
               DISPLAY '2. Conversion de N(8) -> N(10)'.
               DISPLAY '3. Conversion de N(16) -> N(10)'.
               DISPLAY '4. Salir del Programa'.
               
           PERFORM UNTIL opcion2 = 4
               DISPLAY 'Elige una de las opciones anteriores.'
               ACCEPT opcion2
               EVALUATE opcion2
                   WHEN 1
                       PERFORM CONVERSOR_N2_N10
                   WHEN 2
                       PERFORM CONVERSOR_N8_N10
                   WHEN 3
                       PERFORM CONVERSOR_N16_N10
                   WHEN 4
                       DISPLAY 'Hasta pronto!'
                   WHEN OTHER
                       DISPLAY 'Opcion Invalida!'
               END-EVALUATE
      *         PERFORM MENU-PRINCIPAL
           END-PERFORM.
           STOP RUN.

      * Menu Conversion N(10) a N(x)
           SUBMENU2.
               *> Opciones del submenu
               DISPLAY 'Conversor de N(10) -> N(x)'.
               DISPLAY 'Menu:'.
               DISPLAY '1. Conversion de N(10) -> N(2)'.
               DISPLAY '2. Conversion de N(10) -> N(8)'.
               DISPLAY '3. Conversion de N(10) -> N(16)'.
               DISPLAY '4. Salir del Programa'.
           
           PERFORM UNTIL opcion2 = 4
               DISPLAY 'Elige una de las opciones anteriores.'
               ACCEPT opcion2
               EVALUATE opcion2
                   WHEN 1
                       PERFORM CONVERSOR_N10_N2
                   WHEN 2
                       PERFORM CONVERSOR_N10_N8
                   WHEN 3
                       PERFORM CONVERSOR_N10_N16
                   WHEN 4
                       DISPLAY 'Hasta pronto!'
                   WHEN OTHER
                       DISPLAY 'Opcion Invalida!'
               END-EVALUATE
      *         PERFORM MENU-PRINCIPAL
           END-PERFORM.
           STOP RUN.

      *Conversor N2 a N10
           CONVERSOR_N2_N10.
               *>Reseteamos num_dec y pot
               MOVE 0 TO num_dec.
               MOVE 1 TO pot.
               DISPLAY 'Ingresa el numero binario de tamaño 100 maximo'.
               ACCEPT BinOctDecHex.
               MOVE 1 TO cont.
               *>Contamos la longitud del numero ingresado 
               PERFORM UNTIL indice(cont) = ' '
                   ADD 1 TO cont
               END-PERFORM
               *>Nos posicionamos en la ultima casilla
               SUBTRACT 1 FROM cont.
               *>De la ultima casilla hasta la primera
               PERFORM UNTIL cont = 0
                   *>pasamos el digito binario a la variable digito_B
                   MOVE indice(cont) to digito_B
                   *>checamos si es parte del sistema binario
                   IF NOT numero_B THEN
                       DISPLAY 'ERROR: El digito no ' WITH NO ADVANCING
                       DISPLAY 'pertenece al Sistema Binario'
                       GO TO CONVERSOR_N2_N10
                   *> si lo es, 
                   ELSE
                       *>realizamos el metodo general
                       COMPUTE num_dec = num_dec + (pot * digito_B)
                       MULTIPLY 2 BY pot
                       SUBTRACT 1 FROM cont 
                   END-IF
               END-PERFORM 
               DISPLAY 'El numero decimal es ' num_dec.
               GO TO MENU-PRINCIPAL.
               STOP RUN.

      *Conversor N8 a N10
           CONVERSOR_N8_N10.
               *>Reseteamos num_dec y pot
               MOVE 0 TO num_dec.
               MOVE 1 TO pot.
               DISPLAY 'Ingresa el numero octal de tamaño 100 maximo'.
               ACCEPT BinOctDecHex.
               MOVE 1 TO cont.
               *>Contamos la longitud del numero ingresado 
               PERFORM UNTIL indice(cont) = ' '
                   ADD 1 TO cont
               END-PERFORM
               *>Nos posicionamos en la ultima casilla
               SUBTRACT 1 FROM cont.
               *>De la ultima casilla hasta la primera
               PERFORM UNTIL cont = 0
                   *>pasamos el digito octal a la variable digito_O
                   MOVE indice(cont) to digito_O
                   *>checamos si es parte del sistema octal
                   IF NOT numero_O THEN
                       DISPLAY 'ERROR: El digito no ' WITH NO ADVANCING
                       DISPLAY 'pertenece al Sistema Octal'
                       GO TO CONVERSOR_N8_N10
                   *> si lo es, 
                   ELSE
                       *>realizamos el metodo general
                       COMPUTE num_dec = num_dec + (pot * digito_O)
                       MULTIPLY 8 BY pot
                       SUBTRACT 1 FROM cont 
                   END-IF
               END-PERFORM 
               DISPLAY 'El numero decimal es ' num_dec.
               GO TO MENU-PRINCIPAL.
               STOP RUN.

      *Conversor N16 a N10
           CONVERSOR_N16_N10.
               *>Reseteamos num_dec y pot
               MOVE 0 TO num_dec.
               MOVE 1 TO pot.
               DISPLAY 'Ingresa el numero ' WITH NO ADVANCING
               DISPLAY 'hexadecimal de tamaño 100 maximo'.
               ACCEPT BinOctDecHex.
               MOVE 1 TO cont.
               *>Contamos la longitud del numero ingresado 
               PERFORM UNTIL indice(cont) = ' '
                   ADD 1 TO cont
               END-PERFORM
               *>Nos posicionamos en la ultima casilla
               SUBTRACT 1 FROM cont.
               *>De la ultima casilla hasta la primera
               PERFORM UNTIL cont = 0
                   *>pasamos el digito hexadecimal a la variable digito_H
                   MOVE indice(cont) to digito_H
                   *>checamos si es parte del sistema hexadecimal
                   IF NOT (numero_H OR letra_H)  THEN
                       DISPLAY 'ERROR: El digito no ' WITH NO ADVANCING
                       DISPLAY 'pertenece al Sistema Hexadecimal'
                       *>GO TO CONVERSOR_N16_N10
                   *> si lo es,
                   *> obtenemos el valor numerico de numero_H 
                   ELSE IF numero_H THEN
                       COMPUTE val_H = FUNCTION NUMVAL(digito_H)
                       DISPLAY val_H                         
                       COMPUTE num_dec = num_dec + (pot * val_H)
                       MULTIPLY 16 BY pot
                       SUBTRACT 1 FROM cont        
                   ELSE IF letra_H THEN
                       IF digito_H = 'A' 
                           MOVE 10 TO val_H
                           DISPLAY val_H                         
                           COMPUTE num_dec = num_dec + (pot * val_H)
                           MULTIPLY 16 BY pot
                           SUBTRACT 1 FROM cont
                       ELSE IF digito_H = 'B' 
                           MOVE 11 TO val_H
                           DISPLAY val_H                         
                           COMPUTE num_dec = num_dec + (pot * val_H)
                           MULTIPLY 16 BY pot
                           SUBTRACT 1 FROM cont
                       ELSE IF digito_H = 'C' 
                           MOVE 12 TO val_H
                           DISPLAY val_H                         
                           COMPUTE num_dec = num_dec + (pot * val_H)
                           MULTIPLY 16 BY pot
                           SUBTRACT 1 FROM cont
                       ELSE IF digito_H = 'D' 
                           MOVE 13 TO val_H
                           DISPLAY val_H                         
                           COMPUTE num_dec = num_dec + (pot * val_H)
                           MULTIPLY 16 BY pot
                           SUBTRACT 1 FROM cont
                       ELSE IF digito_H = 'E' 
                           MOVE 14 TO val_H
                           DISPLAY val_H                         
                           COMPUTE num_dec = num_dec + (pot * val_H)
                           MULTIPLY 16 BY pot
                           SUBTRACT 1 FROM cont
                       ELSE IF digito_H = 'F' 
                           MOVE 15 TO val_H
                           DISPLAY val_H                         
                           COMPUTE num_dec = num_dec + (pot * val_H)
                           MULTIPLY 16 BY pot
                           SUBTRACT 1 FROM cont
                       END-IF     
               END-PERFORM 
               DISPLAY 'El numero decimal es ' num_dec.
               GO TO MENU-PRINCIPAL.
               STOP RUN.
      
      
      *Conversor N10 a N2
           CONVERSOR_N10_N2.
               *>Reseteamos num_dec y pot
               MOVE 0 TO num_dec.
               MOVE 100 TO cont.
               DISPLAY 'Ingresa el numero decimal de tamaño 100 maximo'.
               ACCEPT num_dec.
               *> para el metodo del residuo,
               *> perform hasta que el residuo se igual que 0
               PERFORM UNTIL num_dec = 0
                   COMPUTE residuo = FUNCTION MOD(num_dec, 2)
                   DISPLAY residuo
                   MOVE residuo TO indice1(cont)
                   *>DISPLAY cont
                   COMPUTE num_dec = num_dec/2
                   DISPLAY num_dec
                   SUBTRACT 1 FROM cont
               END-PERFORM
               COMPUTE cont = cont + 1
               DISPLAY 'El numero binario es: '
               PERFORM UNTIL cont > 100
                   DISPLAY indice1(cont) WITH NO ADVANCING
                   COMPUTE cont = cont + 1
               END-PERFORM
               DISPLAY ' '.
               GO TO MENU-PRINCIPAL.
               STOP RUN.
      *Conversor N10 a N8
           CONVERSOR_N10_N8.
               *>Reseteamos num_dec y pot
               MOVE 0 TO num_dec.
               MOVE 100 TO cont.
               DISPLAY 'Ingresa el numero decimal de tamaño 100 maximo'.
               ACCEPT num_dec.
               *> para el metodo del residuo,
               *> perform hasta que el residuo sea igual que 0
               PERFORM UNTIL num_dec = 0
                   COMPUTE residuo = FUNCTION MOD(num_dec, 8)
                   DISPLAY residuo
                   MOVE residuo TO indice2(cont)
                   *>DISPLAY cont
                   COMPUTE num_dec = num_dec/8
                   DISPLAY num_dec
                   SUBTRACT 1 FROM cont
               END-PERFORM
               COMPUTE cont = cont + 1
               DISPLAY 'El numero octal es: '
               PERFORM UNTIL cont > 100
                   DISPLAY indice2(cont) WITH NO ADVANCING
                   COMPUTE cont = cont + 1
               END-PERFORM
               DISPLAY ' '.
               GO TO MENU-PRINCIPAL.
               STOP RUN.

      *Conversor N10 a N16
           CONVERSOR_N10_N16.   
