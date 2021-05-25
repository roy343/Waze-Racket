#lang racket

(provide (all-defined-out))


;;GrafoDinamico
(define grafoPrincipal '()
  )

;; Agregar ciudades para grafoDinamico
;; E: #id ciudad, grafo mixto
;; S: grafo mixto con la ciudad añadida
;; R: # ciudad vacío, literales.
(define (agregarNodo id grafo)
  (cond ( (null? grafo) 
          (list (append (list id) (list grafo)))) ;;si el grafo está vacío, añada la ciudad
        ( else
           (append grafo (list (list id '()))));; sino, agregue la ciudad al grafo      
 ))

;; Agregar caminos entre ciudades
(define (agregarRuta inicio final peso grafo)
  (agregarRutasAux inicio final peso grafo '()))

(define (agregarRutasAux inicio final peso grafo Final)
  (cond ( (null? grafo) ;;Condición base: Cuando ya se terminó de recorrer el grafo, finalice
          Final) ;;retorne el nuevo grafo generado 'grafoFinal'.
        ( else
          (cond ( (equal? inicio (caar grafo)) ;;valida si está recorriendo la ciudad de origen en el grafo
                  (agregarRutasAux inicio final peso (cdr grafo) ;;recorre la función nuevamente ahora sin el primer elemnto en el grafo
                                    (append Final ;;grafoFinal ahora es la unión del grafo en formación +
                                            (list (append (list(caar grafo)) ;;la ciudad de origen +
                                                          (list (cambiarRutas (list final peso) (cadar grafo)))))))) ;;conexiones de la ciudad incluyendo el nuevo camino
                (else
                 (agregarRutasAux inicio final peso (cdr grafo) ;;recorre la función nuevamente ahora sin el primer elemnto en el grafo
                                   (append Final ;;grafoFinal ahora es la unión del grafo en formación +
                                           (list(car grafo))) ;;el primer elemento en el grafo en desintegración.
                 )
                )
            )
          )
   )
)


;; Agrega un nodo a la lista de conexiones de una ciudad
;; E: id: nodo compuesto por ciudad y peso.
;; S: lista de nodos conexiones
;; R: nodos vacíos
(define (cambiarRutas id lista)
  (cambiarRutasAux id lista))

(define (cambiarRutasAux id lista)
  (cond ( (null? lista);;si la lista esta vacía
          (list (append lista id))) ;;agrege el nodo a la lista
        ( else ;;sino
           (append lista (list id)));; una la actual lista de conexiones con el nodo   
 ))

#|
;;Grado precargado de prueba
(define gg '( (i (a b))
              (a (i c d))
              (b (i c d))
              (c (a x b))
              (d (a b f))
              (x (c))
              (f (d))
              ))
|#

;;Indica si una ruta sin peso ha llegado al fin deseado
(define (sol? final ruta)
  (equal? final (car ruta)))

;;Indica si una ruta con peso ha llegado al fin deseado
(define (sol2? final ruta)
  (equal? final (caar ruta)))

;;Indica los vecinos de un nodo
(define (vecinos elemento grafo)
       (vecinosAux (assoc elemento grafo) elemento grafo))

(define (vecinosAux result elemento grafo)
  (cond ( (equal? result #f)
          #f)
        ( else
          (cond ( (null? (cdr result))
                  (cdr result))
                (else
                 (cadr result))))))

;;Me indica si un nodo es parte de una ruta.
(define (miembro? elemento list)
  (cond ( (null? list)
          #f)
        ( (equal? elemento (car list))
          #t)
        (else
         (miembro? elemento (cdr list)))))

;;Me indica si un nodo es parte de una ruta con peso, SIN considerar el peso.
(define (miembro2? elemento list)
  (cond ( (null? list)
          #f)
        ( (equal? (car elemento) (caar list))
          #t)
        (else
         (miembro2? elemento (cdr list)))))

;;Extiende una ruta hacia los nodos vecinos del grafo de prueba sin mostrar el peso de cada movimiento desde el inicio
(define (conectar ruta grafo)
  (conectarAux ruta '() grafo (vecinos (car ruta) grafo))
  )

(define (conectarAux ruta rutaCreada grafo vecinos)
  (cond ((null? vecinos) rutaCreada)
        (else
         (cond ((miembro? (car vecinos) ruta)
                (conectarAux ruta rutaCreada grafo (cdr vecinos)))
               (else 
                     (conectarAux ruta (append (list (cons (car vecinos) ruta)) rutaCreada) grafo (cdr vecinos))
  )))))


;;Extiende una ruta hacia los nodos vecinos sin mostrar el peso de cada movimiento desde el inicio
(define (conectar2 ruta grafo)
  (conectar2Aux ruta '() grafo (vecinos (car ruta) grafo))
  )

(define (conectar2Aux ruta rutaCreada grafo vecinos)
  (cond ((null? vecinos) rutaCreada)
        (else
         (cond ((miembro? (caar vecinos) ruta)
                (conectar2Aux ruta rutaCreada grafo (cdr vecinos)))
               (else 
                     (conectar2Aux ruta (append (list (cons (caar vecinos) ruta)) rutaCreada) grafo (cdr vecinos))
  )))))


;;Extiende una ruta hacia los nodos vecinos mostrando el peso de cada movimiento desde el inicio
(define (conectar3 ruta grafo)
  (conectar3Aux ruta '() grafo (vecinos (caar ruta) grafo))
  )

(define (conectar3Aux ruta rutaCreada grafo vecinos)
  (cond ((null? vecinos) rutaCreada)
               (else
                (cond ((miembro2? (car vecinos) ruta)
                       (conectar3Aux ruta rutaCreada grafo (cdr vecinos)))
                      (else 
                       (conectar3Aux ruta (append (list (cons (car vecinos)  ruta)) rutaCreada) grafo (cdr vecinos))
                       )))))
        
;; Revierte los elementos de un conjunto de rutas
(define (revertir rutasAux rutas)
  (cond ( (null? rutas)
          rutasAux)
        (else
         (revertir 
                      (append (list (reverse (car rutas))) rutasAux)
                      (cdr rutas)
         ))))

;; buscar todas las rutas para el grafo de prueba gg
(define (buscaPrueba inicio final grafo)
  (buscaRutasTotalesAux (list (list inicio)) final grafo '()))

(define (buscaPruebaAux rutas final grafo total)
  (cond ( (null? rutas)
          (revertir '() total))
        ( (sol? final (car rutas))
          (buscaPruebaAux (cdr rutas)
                           final
                           grafo
                           (cons (car rutas) total)))
        ( else
          (buscaPruebaAux (append (conectar (car rutas) grafo)
                                   (cdr rutas))
                           final
                           grafo
                           total))))


;;RETORNA LAS RUTAS DE UN PUNTO A OTRO SIN PESO
(define (buscaSinPeso inicio final grafo)
  (buscaSinPesoAux (list (list inicio)) final grafo '()))

(define (buscaSinPesoAux rutas final grafo total)
  (cond ( (null? rutas)
          (revertir '() total))
        ( (sol? final (car rutas))
          (buscaSinPesoAux (cdr rutas)
                           final
                           grafo
                           (cons (car rutas) total)))
        ( else
          (buscaSinPesoAux (append (conectar2 (car rutas) grafo)
                                   (cdr rutas))
                           final
                           grafo
                           total))))

;;RETORNA LAS RUTAS ACOMPAÑADOS DE SUS PESOS POR TRASLADO
(define (buscaPeso ini fin grafo)
  (buscaPesoAux (list (list (list ini '0))) fin grafo '()))

(define (buscaPesoAux rutas fin grafo total)
  (cond ( (null? rutas)
          (revertir '() total))
        ( (sol2? fin (car rutas))
          (buscaPesoAux (cdr rutas)
                           fin
                           grafo
                           (cons (car rutas) total)))
        ( else
          (buscaPesoAux (append (conectar3 (car rutas) grafo)
                                   (cdr rutas))
                           fin
                           grafo
                           total))))


;;RETORNA LISTA CON DISTANCIA TOTAL DE CADA RUTA
(define (distanciaRutas rutas)
  (distanciaRutasAux rutas '()))
 
(define (distanciaRutasAux rutas totales)
  (cond ( (null? rutas)
          totales)
        ( else
          (distanciaRutasAux (cdr rutas) (append totales (list(distanciaTotal 0 (car rutas))))))))

;;RETORNA DISTANCIA TOTAL DE RUTA
(define (distanciaTotal num ruta)
  (cond ( (null? ruta)
          num)
        (else
         (distanciaTotal (+ num (cadar ruta)) (cdr ruta)))))

;;RETORNA EL ID DE LA MENOR LISTA
(define (menorLista lista)
  (menorListaAux lista (car lista) 0))

(define (menorListaAux lista num cont)
  (cond ( (null? lista)
          cont)
        (else
         (cond ( (<= num (car lista))
                 (menorListaAux (cdr lista) num cont))
               (else
                (menorListaAux (cdr lista) (car lista) (+ cont 1)))))))

;;RETORNA RUTA MAS CORTA
(define (rutaCorta inicio final grafo)
  (rutaCortaAux (buscaPeso inicio final grafo) (buscaSinPeso inicio final grafo)))

(define (rutaCortaAux rutas rutasSinPeso)
  (cond ( (null? rutas)
          rutas)
        ( else
          (rutaCortaAux2 (menorLista (distanciaRutas rutas)) rutas rutasSinPeso))))

(define (rutaCortaAux2 num rutas rutasSinPeso)
  (cond ( (zero? num)
          (cons (car rutasSinPeso) (list (distanciaTotal 0 (car rutas)))))
        ( else
          (rutaCortaAux2 (- num 1) (cdr rutas) (cdr rutasSinPeso)))))

;;RETORNA TODAS LAS RUTAS DE UN PUNTO A OTRO CON SU PESO TOTAL
(define (buscaRutasTotales inicio final grafo)
  (ordenar (buscaRutasTotalesAux (buscaSinPeso inicio final grafo) (distanciaRutas (buscaPeso inicio final grafo)) '()) '())
  )

(define (buscaRutasTotalesAux rutas pesosRuta Final)
  (cond ( (null? rutas)
          Final)
        (else
         (buscaRutasTotalesAux (cdr rutas) (cdr pesosRuta)  (append Final (list (cons (car rutas) (list (car pesosRuta)))))))))

;;InsertaElementoOrdenado
(define (insertarOrden ele lista)
  (cond ( (null? lista)
          (list ele))
        ( (> (cadr ele) (cadar lista))
          (cons (car lista)
                (insertarOrden ele (cdr lista))))
        ( else
          (cons ele lista))))

;;Genera una lista con las rutas ordenadas
(define (ordenar rutas Ordenadas)
  (cond ( (null? rutas)
          Ordenadas)
        ( else
          (ordenar (cdr rutas) (insertarOrden (car rutas) Ordenadas)))))