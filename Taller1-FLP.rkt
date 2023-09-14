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

