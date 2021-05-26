#lang racket

(provide (all-defined-out))
#|
;Creacion del grafo de prueba
(define grafoPrueba '( (i (a b))
              (a (i c d))
              (b (i c d))
              (c (a b x))
              (d (a b f))
              (x (c))
              (f (d))
              ))
|#


;Grafo principal, es el dinámico que se crea a medida que se agragan nodos
(define grafoPrincipal '()
  )

#|
; Agregar nodos al grafo principal
; Entrada: id del nodo
; Salida: grafo creado
|#

(define (agregarNodo id grafo)
  (cond ( (null? grafo) 
          (list (append (list id) (list grafo)))) ;;si el grafo está vacío, añada la ciudad
        ( else
           (append grafo (list (list id '()))));; sino, agregue la ciudad al grafo      
 ))



#|
Función que agrega rutas entre los nodos
Entradas: nodo de inicio, nodo final, peso del camino
Salida: Grafo con rutas y pesos respectivos
|#

(define (agregarRuta inicio final peso grafo)
  (agregarRutasAux inicio final peso grafo '()))

(define (agregarRutasAux inicio final peso grafo Final)
  (cond ( (null? grafo) ;Cuando se termina de recorrer se detiene la función auxiliar
          Final) ; Devuelve un grafo Final con ruts incluidas
        ( else
          (cond ( (equal? inicio (caar grafo)) ;Se valida si se encuentra en el nodo de origen
                  (agregarRutasAux inicio final peso (cdr grafo) ;Realiza nuevamente la función ahora sin contar el nodo de origen
                                    (append Final ;Se le agrega al grafo final lo siguiente...
                                            (list (append (list(caar grafo)) ; El nodo de origen
                                                          (list (cambiarRutas (list final peso) (cadar grafo)))))))) ;;conexiones de la ciudad incluyendo el nuevo camino
                (else
                 (agregarRutasAux inicio final peso (cdr grafo) ;recorre la función nuevamente ahora sin el primer eleemnto en el grafo
                                   (append Final ;grafo Final ahora es la unión del grafo en formación y el primer elemento del grafo inicial
                                           (list(car grafo))) 
                 ))))
   ))

#|
Función que agrega un nodo a la lista de conexiones de un nod
Entrada: id de nodo 
Salida: Lista de nodos con conexiones

Esta no puede tener nodos vacíos

|#

(define (cambiarRutas id lista)
  (cambiarRutasAux id lista))

(define (cambiarRutasAux id lista)
  (cond ( (null? lista);Valida si la lista es vacía 
          (list (append lista id))) ;Si es vacía agrega el nodo a la lista
        ( else 
           (append lista (list id)));En otro caso une la lista actual de coexiones con el nodo deseado 
 ))



;Indica si el recorrido de una ruta sin peso fue exitosa
(define (sol? final ruta)
  (equal? final (car ruta))); Lo hace validando si el final es igual al primer elemento de la ruta

;Indica si el recorrido de una ruta con peso fue exitosa
(define (sol2? final ruta)
  (equal? final (caar ruta)));Al tener peso lo realiza validando el primer elemento del primer elemento de la ruta 

;Función que indica los vecinos de un nodo
(define (vecinos elemento grafo)
       (vecinosAux (assoc elemento grafo) elemento grafo))

;Funcón auxiliar de vecinos
(define (vecinosAux result elemento grafo)
  (cond ( (equal? result #f)
          #f)
        ( else
          (cond ( (null? (cdr result))
                  (cdr result))
                (else
                 (cadr result))))))

#|
Función que indica si un nodo es parte de una ruta sin peso.
|#
(define (miembro? elemento list)
  (cond ( (null? list);Cuando la lista es nula se deja de recorrer
          #f)
        ( (equal? elemento (car list));Evalúa si el elemento es igual al primer elemento de la lista
          #t)
        (else
         (miembro? elemento (cdr list)))));Si no se cumple lo anterior, realizar recursión con la lista sin el primer elemento


#|
Función que indica si un nodo es parte de una ruta con peso.
|#
(define (miembro2? elemento list)
  (cond ( (null? list)
          #f)
        ( (equal? (car elemento) (caar list));Evalúa si el elemento es igual al primer elemento del grafo
          #t)
        (else
         (miembro2? elemento (cdr list)))));Si no se cumple lo anterior, realizar recursión con la lista sin el primer elemento


#|
Función que conecta el nodo con los nodos vecinos según la ruta establecida
|#
(define (conectar ruta grafo)
  (conectarAux ruta '() grafo (vecinos (car ruta) grafo))
  )
;Función auxiliar de la conexión
(define (conectarAux ruta rutaCreada grafo vecinos)
  (cond ((null? vecinos) rutaCreada);Cuando no haya más vecinos retorna la ruta Creadad
        (else
         (cond ((miembro? (car vecinos) ruta) ;verifica si un vecino es parte de la ruta del nodo
                (conectarAux ruta rutaCreada grafo (cdr vecinos)))
               (else 
                     (conectarAux ruta (append (list (cons (car vecinos) ruta)) rutaCreada) grafo (cdr vecinos)); se crea una ruta y se continua con la lista de vecinos sin el primer elemento
  )))))

#|
Función que genera una conexión hacia los nodos vecinos sin mostrar el peso
|#
(define (conectar2 ruta grafo)
  (conectar2Aux ruta '() grafo (vecinos (car ruta) grafo))
  )
;Función auxiliar de la conexión
(define (conectar2Aux ruta rutaCreada grafo vecinos)
  (cond ((null? vecinos) rutaCreada);Cuando no haya más vecinos retorna la ruta Creadad
        (else
         (cond ((miembro? (caar vecinos) ruta);verifica si un vecino es parte de la ruta del nodo
                (conectar2Aux ruta rutaCreada grafo (cdr vecinos)))
               (else 
                     (conectar2Aux ruta (append (list (cons (caar vecinos) ruta)) rutaCreada) grafo (cdr vecinos));se crea una ruta y se continua con la lista de vecinos sin el primer elemento
  )))))


#|
Función que genera una conexión hacia los nodos vecinos tomando en cuenta el peso de los caminos
|#
(define (conectar3 ruta grafo)
  (conectar3Aux ruta '() grafo (vecinos (caar ruta) grafo))
  )
;Función auxiliar de la conexión
(define (conectar3Aux ruta rutaCreada grafo vecinos)
  (cond ((null? vecinos) rutaCreada);Cuando no haya más vecinos retorna la ruta Creadad
               (else
                (cond ((miembro2? (car vecinos) ruta);verifica si un vecino es parte de la ruta del nodo y se utiliza miembro2 cporque considera peso
                       (conectar3Aux ruta rutaCreada grafo (cdr vecinos)))
                      (else 
                       (conectar3Aux ruta (append (list (cons (car vecinos)  ruta)) rutaCreada) grafo (cdr vecinos)));se crea una ruta y se continua con la lista de vecinos sin el primer elemento
                       ))))
        
#|
Función que revierte los elementos de una ruta
|#
(define (revertir rutasAux rutas)
  (cond ( (null? rutas)
          rutasAux)
        (else
         (revertir 
                      (append (list (reverse (car rutas))) rutasAux);Se revierten los elementos de las rutas
                      (cdr rutas)
         ))))

#|
Función que busca y retorna las rutas del grafo de prueba
|#
(define (buscaPrueba inicio final grafo)
  (buscaRutasTotalesAux (list (list inicio)) final grafo '()))

;Función auxiliar de búsqueda
(define (buscaPruebaAux rutas final grafo total)
  (cond ( (null? rutas)
          (revertir '() total));Cuando las rutas son nulas, se realiza la función revertir 
        ( (sol? final (car rutas));Si se llegó al final de manera exitosa se retornan las rutas totales construidas
          (buscaPruebaAux (cdr rutas)
                           final
                           grafo
                           (cons (car rutas) total)))
        ( else
          (buscaPruebaAux (append (conectar (car rutas) grafo);en otro caso sigue realizando la recursión siguiendo con la lista de rutas sin el primer elemento
                                   (cdr rutas))
                           final
                           grafo
                           total))))


#|
Función que retorna las rutas sin peso de un grafo
|#
(define (buscaSinPeso inicio final grafo)
  (buscaSinPesoAux (list (list inicio)) final grafo '()))

;Función auxiliar de búsqueda
(define (buscaSinPesoAux rutas final grafo total)
  (cond ( (null? rutas)
          (revertir '() total));Cuando las rutas son nulas, se realiza la función revertir
        ( (sol? final (car rutas));Si se llegó al final de manera exitosa se retornan las rutas totales construidas
          (buscaSinPesoAux (cdr rutas)
                           final
                           grafo
                           (cons (car rutas) total)))
        ( else
          (buscaSinPesoAux (append (conectar2 (car rutas) grafo);en otro caso sigue realizando la recursión siguiendo con la lista de rutas sin el primer elemento
                                   (cdr rutas))
                           final
                           grafo
                           total))))


#|
Función que retorna las rutas con peso de un grafo
Entrada: nodo de inicio y nodo de final
Salida: Lista de rutas con peso del grafo
|#
(define (buscaPeso ini fin grafo)
  (buscaPesoAux (list (list (list ini '0))) fin grafo '()))
;Función auxiliar de búsqueda

(define (buscaPesoAux rutas fin grafo total)
  (cond ( (null? rutas)
          (revertir '() total));Cuando las rutas son nulas, se realiza la función revertir
        ( (sol2? fin (car rutas))
          (buscaPesoAux (cdr rutas);Si se llegó al final de manera exitosa se retornan las rutas totales construidas
                           fin
                           grafo
                           (cons (car rutas) total)))
        ( else
          (buscaPesoAux (append (conectar3 (car rutas) grafo);Se utiliza el conectar3 porque toma en cuenta el peso de la ruta
                                   (cdr rutas));en otro caso sigue realizando la recursión siguiendo con la lista de rutas sin el primer elemento
                           fin
                           grafo
                           total))))


#|
Función que retorna la distancia total de las rutas
|#
(define (distanciaRutas rutas)
  (distanciaRutasAux rutas '()))

;Función auxiliar de distancias
(define (distanciaRutasAux rutas totales)
  (cond ( (null? rutas);Cuando las rutas son nulas, se retorna la lista con las distancias totales
          totales)
        ( else
          (distanciaRutasAux (cdr rutas) (append totales (list(distanciaTotal 0 (car rutas))))))));En otro caso se realiza la recursión y la distancia del primer elemento de las rutas se concatena a la lista totales

#|
Función que retorna la distancia total de una ruta
|#
(define (distanciaTotal num ruta)
  (cond ( (null? ruta);Cuando la ruta es nula o recorrida retorna la ruta
          num)
        (else
         (distanciaTotal (+ num (cadar ruta)) (cdr ruta))))); Genera la distancia total al recorrer la ruta

#|
Función que retorna el id de la menor lista
|#
(define (menorLista lista)
  (menorListaAux lista (car lista) 0))

;Función auxiliar de la lista
(define (menorListaAux lista num cont)
  (cond ( (null? lista);Cuando se termina de recorrer 
          cont)
        (else
         (cond ( (<= num (car lista))
                 (menorListaAux (cdr lista) num cont)); Si num es menor o igual que el primer elemento de la lista entonces se continua con la lista sin el primer elemento
               (else
                (menorListaAux (cdr lista) (car lista) (+ cont 1)))))));En otro caso el contador se aumenta 

#|
Función que retorna la ruta más corta de una grafo a otro
|#
(define (rutaCorta inicio final grafo)
  (rutaCortaAux (buscaPeso inicio final grafo) (buscaSinPeso inicio final grafo)))

;Función auxiliar de ruta corta
(define (rutaCortaAux rutas rutasSinPeso)
  (cond ( (null? rutas)
          rutas);Cuando se terminan de recorrer las rutas la retorna
        ( else
          (rutaCortaAux2 (menorLista (distanciaRutas rutas)) rutas rutasSinPeso))));En otro caso se ejecuta la función auxiliar 2
;Función auxiliar de ruta corta 2
(define (rutaCortaAux2 num rutas rutasSinPeso)
  (cond ( (zero? num) ;Cuando el num = cero se detiene la recursión
          (cons (car rutasSinPeso) (list (distanciaTotal 0 (car rutas)))));Y se construye el par del primer elemento de las rutas y la distancia total de estas
        ( else
          (rutaCortaAux2 (- num 1) (cdr rutas) (cdr rutasSinPeso)))));En otro caso se reduce el num y se realiza la recursión con las listas de rutas y rutas sin peso sin el primer elemento de cada uno

#|
Función que retorna todas las rutas de un nodo a otro
|#
(define (buscaRutasTotales inicio final grafo)
  (ordenar (buscaRutasTotalesAux (buscaSinPeso inicio final grafo) (distanciaRutas (buscaPeso inicio final grafo)) '()) '())
  )
;Función auxiliar de rutas totales
(define (buscaRutasTotalesAux rutas pesosRuta Final)
  (cond ( (null? rutas)
          Final); Cuando las rutas sean recorridas por completo se retorna el final
        (else; Se construye la lista Final de rutas totales
         (buscaRutasTotalesAux (cdr rutas) (cdr pesosRuta)  (append Final (list (cons (car rutas) (list (car pesosRuta))))))))) ;Se realiza la recursión quitando el primer elemento de la lista de rutas y el peso


#|
Función que inserta elementos ordenados
|#
(define (insertarOrden ele lista)
  (cond ( (null? lista)
          (list ele))
        ( (> (cadr ele) (cadar lista));Compara los elementos a añadir
          (cons (car lista)
                (insertarOrden ele (cdr lista))));accede a los elementos de la lista y realiza la recursión y construccion del par elemento y lista sin el primer elemento
        ( else
          (cons ele lista))));En otro caso construye los pares con elemento y la lista inicial


#|
Función que inserta elementos ordenados
|#
(define (ordenar rutas Ordenadas)
  (cond ( (null? rutas)
          Ordenadas); Cuando la lista de rutas es recorrida retorna la lista de ordenadas
        ( else
          (ordenar (cdr rutas) (insertarOrden (car rutas) Ordenadas)))));En otr caso, ordena las rutas y realiza la recursión