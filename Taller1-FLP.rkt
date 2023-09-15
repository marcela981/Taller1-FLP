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
(newline)


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

(display(list-set '(a b c d) 3 '(1 5 10)));; Retorna '(a b c (1 5 10))
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
(display (filter-in string? '(a b u (40 56) hi (a b c) (1 2 3))))
(newline)
(newline)



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
(newline)

;Punto 10
;En este punto se desea elaborar una función up, que reciba una lista como entrada
;La función tiene como objetivo eliminar los parentesis de cada elemento del nivel más alto de la lista
;Si un elemento no es una, se imprime sin modificación alguna
(define (up L) ; Se define up
  (cond
    [(null? L) empty]             ; Condición, si L está vacía, devuelve una lista vacía
    [(not (pair? (car L)))        ; Si el primer elemento no es una lista
     (cons (car L) (up (cdr L)))] ; Agrega el elemento a la salida y procesa el resto de la lista
    [else                         ; y Si no, el primer elemento es una lista
     (append (up (car L)) (up (cdr L)))])) ; Combina las salidas de procesar el primer elemento y el resto de la lista


(display (up '((1 2) (3 4)))) ;(1 2 3 4)
(newline)
(display (up '((x (y)) z))) ;(x (y) z)
(newline)
(newline)



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
(newline)