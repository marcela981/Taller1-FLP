#lang eopl

;Punto 1
;En el punto número uno se pide crear una función llamada invert
;inver reciber argumentos de una lista L
;La lista de números de se debe componer por pares y estos se debe inveritr
(define invert ;Define la función invert
  (lambda (L) ;Inver toma el argumento L
    (cond
      [(null? L) L] ;Esta condición verifica si L es una lista vacía
      [else (cons     (cons  (cadar L)  (cons (caar L) '()))     (invert (cdr L)))] ;Si no es vacío...
      ;(cadar L) -> Toma el segundo elemento de la primera sublista
      ;(caar L) -> Toma el primer elemento de la primera sublista
      ;(cons (caar L) '()) -> Realiza una lista que contiene el primer elemento de la primera sublista seguido de una lista vacía.
      ;Esta operación se aplica recirsivamente a la cola de la lista, usando (invert (cdr L))
    )
  )
)

(display (invert '((a 1) (a 2) (1 b) (2 b)))) ;((1 a) (2 a) (b 1) (b 2))
(newline)
(display (invert '((5 9) (10 91) (82 7) (a e) ("hola" "Mundo")))) ;((9 5) (91 10) (7 82) (e a) ("Mundo" "hola")
(newline)
(display(invert '(("es" "racket") ("genial" "muy") (17 29) (81 o)))) ;(("racket" "es") ("muy" "genial") (29 17) (o 81))
(newline)
(display (invert '((2 1) ("Este" 3) ("el" "es") ("punto" "primer") (o W) (W o)))) ;((1 2) (3 Este) (es el) (primer punto) (W o) (o W))
(newline)
(display (invert '( (0 1) ("hola" 2) ))) ; ((1 0) (2 hola))
(newline)
(newline)

;Punto 2
;Contrato: l -> lista
;Se necesita una función que reciba una lista y esta le agregue
;un nivel más de parentesis a la lista l
(define (down lst)
  (cond
    ((null? lst) '()) ; Caso base
    ((list? (car lst))
     (cons (down (car lst)) (down (cdr lst)))) ; Si es una lista anidada, llama recursivamente a down en el primer elemento (anidado) y al resto de la lista, y luego utiliza cons para construir una nueva lista que contenga el resultado de down en el elemento anidado y el resultado de down en el resto de la lista.
    (else
     (cons (list (car lst)) (down (cdr lst)))))) ; crea una nueva lista que contiene ese elemento en una lista (añadiendo paréntesis) y el resultado de down en el resto de la lista (llamada recursiva).


;; Punto 3
;; list-set :
;; Propósito:
;; Procedimiento en el que una lista L, recibe un elemento x, se reemplaza en una posición n
;; y devuelve una nueva lista similar a L pero con el elemento
;; en la posición n (indexada desde cero) reemplazado por x.
;;

(define list-set
  (lambda (l n x)
    (letrec
        (
         (list-aux
          (lambda (laux naux xaux acc)
            (cond
              [(eqv? laux '()) empty]
              [(= acc naux) (cons xaux (list-aux (cdr laux) naux xaux (+ 1 acc)))]
              [else (cons (car laux) (list-aux (cdr laux) naux xaux (+ 1 acc)))]
              )
            )
          )
         )
      (list-aux l n x 0))
    )
  )
;; Pruebas
(display(list-set '(a b c d) 2 '(1 2)));; Retorna '(a b (1 2) d)
(newline)

(display(list-set '(a b c d) 1 '(1 2))); Retorna '(a '(1 2) c d)'
(newline)

(display(list-set '(apple banana cherry date) 2 '(x y z))) ; Retorna '(apple banana '(x y z) date)'
(newline)


;Punto 4
;Se debe elaborar la función filter-in que recibe dos argumentos, predicado y lista
;La función retorna una lista que contiene los elementos que pertenecen a la lista y satisface el predicado
;Es decir, este algoritmo imprime lo que se pide de una lista de x cosas, sea números, simbolos o strings
(define (filter-in P L) ;Se define filter-in, se agrega una función predicado y la lista de entrada
  (cond
    [(null? L) empty] ; Caso base: lista vacía, devuelve una lista vacía
    [(P (car L)) (cons (car L) (filter-in P (cdr L)))] ; Satisface el predicado, lo agregamos a la lista resultante
    [else (filter-in P (cdr L))]))          ; Si no lo omitimos y continuamos con el resto de la lista


(display (filter-in number? '(a 2 (1 3) b 7))) ; (2 7)
(newline)
(display (filter-in symbol? '(a (b c) 17 foo))) ;(a foo)
(newline)
(display (filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))) ; ("univalle" "racket" "flp")
(newline)
(display (filter-in string? '(a b u (40 56) hi (a b c) (1 2 3)))) ;()
(newline)
(display (filter-in number? '(o o "1 2 3" 0 "94" hi (a b c) (1 2 3)))) ;(0)
(newline)
(newline)

;Punto 5
;Contrato: p -> predicado, l -> lista
;Se necesita una función que reciba dos argumentos, un predicado y una lista
;la función debería retornar el indice del primer elemento que cumpla con el predicado
(define (list-index p l)
  ; Se declara una función auxiliar para llevar el contador del indice
  (define (aux p l index)
    (cond
      ((null? l) #f)
      ((p (car l)) index) ; Si el predicado se cumple devuelve el indice
      (else (aux p (cdr l) (+ index 1))) ; Si no, continua la ejecución recursivamente
      ))
  (cond
    ((null? l) #f)
    (else (aux p l 0))
    ))


;; Punto 6

;; my-map :
;; Propósito:
;;   Aplica una función func a cada elemento de una lista lst y devuelve una nueva lista
;;   que contiene los resultados de aplicar func a cada elemento de lst.
(define (my-map func lst)
  (if (null? lst)
      '()
      (cons (func (car lst))
            (my-map func (cdr lst)))))

;; swapper :
;; Propósito:
;; Intercambia las ocurrencias de dos elementos, E1 y E2, en una lista L.

(define swapper
  (lambda(E1 E2 L)
  (define (swap x)
    (cond
      [(eq? x E1) E2]
      [(eq? x E2) E1]
      [else x]))

  (my-map (lambda (x) (swap x)) L)))


;; Pruebas

(display (swapper '8 '1 '(1 2 3 5 4 6 8 7))) ; Retorna '(8 2 3 5 4 6 1 7'
(newline)

(display (swapper 'a 'b '(a b c d e))) ; Retorna '(b a c d e)'
(newline)

(display (swapper "apple" "banana" '("apple" "banana" "cherry" "date" "apple"))) ; Retorna '("banana" "apple" "cherry" "date" "banana")'
(newline)


;Punto 7
;La intención de este punto es calcular el producto cartesiano
;Esto lo hace mediante dos listas de tuplas
(define (cartesian-product L1 L2) ;cartesian-product se define y toma dos listas
  (define (combine x lst); combine toma un elemento x y una lista lst y hace uso de map, así se cre la lista de pares 
    (map (lambda (y) (list x y)) lst)); map aplica el combine a cada elemento de la lista, así se genera una lista de pares para cada elemento de L1
  
  (apply append (map (lambda (x) (combine x L2)) L1))); apply append combina todas las listas de pares en una sola lista


(display (cartesian-product '(a b c) '(x y))) ;((a x) (a y) (b x) (b y) (c x) (c y))
(newline)
(display (cartesian-product '(p q r) '(5 6 7))) ;((p 5) (p 6) (p 7) (q 5) (q 6) (q 7) (r 5) (r 6) (r 7))
(newline)
(display (cartesian-product '(1 2 3) '("hola" "nuevo" "mundo")))
(newline)
(display (cartesian-product '(0 ) '(a b c 0))) ;((0 a) (0 b) (0 c) (0 0))
(newline)
(newline)

;Punto 8
;Contrato f -> función unaria, l1 -> lista, l2 -> lista
;Se necesita una función que reciba tres argumentos: una función unaria
;y dos listas, la función debe retornar una lista de pares de l1 y l2 respectivamente
;las cuales cumplan que el resultado de la función unaria sea igual al segundo elemento del par
(define (mapping f l1 l2)
  (cond
    ((or (null? l1) (null? l2)) '()) ; Caso base
    (else
     (cond
       ((equal? (f (car l1)) (car l2)) (cons (list (car l1) (car l2)) (mapping f (cdr l1) (cdr l2)))) ;Si la cabeza de l1 evaluada en la función unaria es igual a la cabeza de l2 entonces retorna los pares y continua la ejecución recursivamente
       (else (mapping f (cdr l1) (cdr l2))))
     )))


;; Punto 9
;; inversions :
;; Propósito: Calcular el número de inversiones en una lista de números
;; Una inversión es un par de elementos (i, j) en la lista donde i < j y L[i] > L[j]. En otras palabras, una inversión ocurre cuando un número en una posición anterior en la lista es mayor que un número en una posición posterior. La función utiliza un enfoque recursivo para contar las inversiones en la lista.
;;.
(define (inversions L)
  (define inv-count 0)
  
  (define (count-inversions x xs)
    (if (null? xs)
        0
        (+ (if (< x (car xs)) 0 1) (count-inversions x (cdr xs)))))
    
  (define (count-inversions-list L)
    (if (null? L)
        0
        (+ (count-inversions (car L) (cdr L))
           (count-inversions-list (cdr L)))))
  
  
  (set! inv-count (count-inversions-list L))
  
 
  inv-count)

;; Pruebas
(display (inversions '(2 4 1 3 5)));; Retorna 3
(newline)

(display (inversions '(1 2 3 4 5))) ; Retorna 0
(newline)

(display (inversions '(3 1 3 2 1))) ; Retorna 8
(newline)


;Punto 10
;En este punto se desea elaborar una función up, que reciba una lista como entrada
;La función tiene como objetivo eliminar los parentesis de cada elemento del nivel más alto de la lista
;Si un elemento no es una, se imprime sin modificación alguna
(define (up L) ; Se define up
  (cond
    [(null? L) empty]             ; Condición, si L está vacía, devuelve una lista vacía
    [(list? (car L)) ; Si el primer elemento de L es una lista
     (append (up (car L)) (up (cdr L)))] ; Llamamos recursivamente a up en el primer elemento y en el resto de la lista, luego concatenamos los resultados
    [else ; Si el primer elemento de L no es una lista
     (cons (car L) (up (cdr L)))])) ; Agregamos el primer elemento a la salida y llamamos recursivamente a up en el resto de la lista



(display (up '((1 2) (3 4)))) ;(1 2 3 4)
(newline)
(display (up '((x (y)) z))) ;(x (y) z)
(newline)
(display (up '((1 x) (b o)))) ;(x (y) z)
(newline)


;Punto 11
;Contrato f -> función binaria, l1 -> lista, l2 -> lista
;Se necesita una función que reciba tres entradas: una función binaria, y dos listas de igual tamaño
;la función debe retornar una lista con la operación de las dos listas en su n-ésima posición 
(define (zip f l1 l2)
  (cond
    ((or (null? l1) (null? l2)) '()) ; Caso base
    (else
     (cons (f (car l1) (car l2)) (zip f (cdr l1) (cdr l2)))) ; Construyo la lista con aplicando la función unaria a la cabeza de l1 y l2 respectivamente
    ))


;; Punto 12
;; filter-acum
;; Propósito: Acumular valores dentro de un rango [a, b] basado en un filtro y una función de acumulación proporcionada.
;;   Se itera desde 'a' hasta 'b', aplicando el filtro y la función de acumulación a los elementos adecuados.

(define filter-acum
  (lambda(a b f acum filter)
    (define (iter i acc)
    (if (> i b)
        acc
        (iter (+ i 1)
              (if (filter i)
                  (f i acc)
                  acc))))
  
  (iter a acum)))


;; Pruebas
(display (filter-acum 1 15 + 0 even?)) ; Retorna 56
(newline)

(display (filter-acum 1 15 + 0 odd?)) ; Retorna 64
(newline)

(display (filter-acum 1 15 + 0 rational?)) ; Retorna 120
(newline)


;Punto 13
; Se elabora una función operate lrators lrands donde lrands es una lista de funciones binarias de tamaño n
; y lrands  es una lista de número de tamaño n+1
;La función debe retornar y aplicar sucesivamente las operaciones a los valores en lrands 
(define (operate lrators lrands) ;Define operate
  (define (apply-operations operators operands) ;operators operadores que quedan por aplicar,operands ista de operandos que quedan por operar. 
    (if (null? operators) ;Si está vació devuelve el valor resultante, operands
        (car operands) ; Aplica operators al primer y segundo operador en la lista
        (apply-operations (cdr operators) ; Llamamos recursivamente sin el primer operador
                          (cons ((car operators) (car operands) (cadr operands)) ; Aplicamos el primer operador a los dos primeros operandos
                                (cddr operands))))) ; Resto de operandos

  (if (null? lrators) ; si está vacía, no hay operadores que aplicarla
      (car lrands) ; Si no hay operadores, simplemente devolvemos el primer número
      (apply-operations lrators lrands))) ; Llamamos a apply-operations con lrators y lrands para realizar las operaciones


(display (operate (list + * + - *) '(1 2 8 4 11 6))) ;  102
(newline)
(display (operate (list *) '(4 5))) ; 20
(newline)
(display (operate (list - + - * -) '(0 10 10 3 2 6))) ; -12
(newline)
(display (operate (list - + - + *) '(10 2 68 8 20 0))) ; 0
(newline)
(newline)


;;Punto 15
;; <tree> -> <pair>
;; Proposito: Función que recibe un árbol y cuenta la cantidad de números pares e impares en ese árbol.
(define count-odd-and-even
  (lambda (tree)
    (define (count-odd-even-aux tree even-count odd-count)
      (cond
        [(null? tree) (list even-count odd-count)]
        [(even? (car tree))
         (count-odd-even-aux (cdr tree) (+ even-count 1) odd-count)]
        [else
         (count-odd-even-aux (cdr tree) even-count (+ odd-count 1))]))

    (count-odd-even-aux tree 0 0)))

;; <tree>:=()
;;           :=(<total-odd>,<total-even>))


;;Pruebas
(display (count-odd-and-even '(2 4 6 8 10))) ;; Retorna (5 0), 5 pares y 0 impares
(newline)
(display (count-odd-and-even '(1 3 5 6 7 8 0))) ;; Retorna (3 4), 3 pares y 4 impares
(newline)
(display (count-odd-and-even '()));; Retorna (0 0) ya que el árbol está vacio
(newline)



;Punto 16
;En este caso se nos entrega una gramatica que tiene como función recibir parametros
;Estos parametro son una operación binaria y retornan el resultado
; las operaciones son: suma, resta y multiplicación
;De este modo se pueden contruir expresiones binarias, esas expresiones pueden ser anidadas y se evaluan
; Ejemplo: (2 suma (3 multiplica 4)) -> La expresión es valida y cae de forma general en el segundo caso.
; Podemos ver que el ejemplo es una operación binaria, (que a su vez es un entero), seguido de una operación suma
;Seguida de otra operación binaria (Que a su vez es un entero una operación y un enero)
; La segunda operación binaria entre en el caso 4, que es una operación binaria, una operación de multiplicación seguido de un binario
;De forma recursiva podesmo utilizar esta gramatica, por tanto el ejemplo es valido.
;<OperacionB>::= <int>
            ;::= (<OperacionB> ’suma <OperacionB>)
            ;::= (<OperacionB> ’resta <OperacionB>)
            ;::= (<OperacionB> ’multiplica <OperacionB>)
;La gramatica nos indica que las operaciones binarias
;Pueden ser enteros (un numero es una operación binaria)o...
;Una operación binaria seguida de una operacón suma seguida de otra operación binaria (Se usa la suma para combinar las operaciones binarias)o...
;Una operación binaria seguida de una operacón resta seguida de otra operación binaria (Se usa la resta para combinar las operaciones binarias)o...
;Una operación binaria seguida de una operacón multiplicación seguida de otra operación binaria (Se usa la multiplicación para combinar las operaciones binarias)o...

(define (Operar-binarias operacionB); Se define la operación binaria
  (cond
    [(number? operacionB) operacionB] ; Si operacionB es un número, simplemente devolvemos ese número
    [(pair? operacionB) ; Si operacionB es una lista (operación binaria)
     (let* ([operador (cadr operacionB)]
            [operando1 (Operar-binarias (car operacionB))]
            [operando2 (Operar-binarias (caddr operacionB))])
       (cond
         [(equal? operador 'suma) (+ operando1 operando2)]
         [(equal? operador 'resta) (- operando1 operando2)]
         [(equal? operador 'multiplica) (* operando1 operando2)]
         [else ( "Operador no válido")]))]
    [else ( "Operación no válida")])) ; En caso de entrada no válida


(display (Operar-binarias 4)) ; 4
(newline)
(display (Operar-binarias '(2 suma 9))) ; 11
(newline)
(display (Operar-binarias '(2 resta 9))) ; -7
(newline)
(display (Operar-binarias '(2 multiplica 9))) ; 18
(newline)
(display (Operar-binarias '((2 multiplica 3) suma (5 resta 1)))) ; 10
(newline)
(display (Operar-binarias '( (2 multiplica (4 suma 1) )
multiplica( (2 multiplica 4) resta 1 ) ) ) )  ; 70
(newline)
(display (Operar-binarias ' (2 suma (3 multiplica 4))))
(newline)
(display (Operar-binarias ' (2 suma (3 resta( 4 suma (0 multiplica 100)))))); 1
(newline)