#lang racket
(require racket/gui)
(require racket/draw/arrow)
(require "Grafos.rkt")

;Definición de variables del sistema


;GrafoDinamico
(define mapa '() )

;Lista de Coordenadas
(define coordenadas '() )

;Ciudad actual por agregar
(define nuevoNodo -1)

;Posicion en x de la ciudad por agregar
(define nuevoNodoX 0)

;Posicion en y de la ciudad por agregar
(define nuevoNodoY 0)

;Ciudad inicial donde sale el camino nuevo
(define inicialNodo -1)

;Ciudad final donde llega el camino nuevo
(define finalNodo -1)

;Peso del nuevo camino
(define pesoNuevo -1)

;Flag para saber si ya se pueden efectuar busquedas
(define prep #f)

;Destino inicial de la ruta
(define destinoInicio "")

;Destino final de la ruta
(define destinoFin "")


;Variable de estado de seleccion de rutas
(define rutaSeleccion -1)

;Estado de seleccion de rutas
(define busqueda "")


;Lista con el camino mas corto y su peso
(define caminoCorto '() )

;Lista con todos los caminos y sus pesos
(define caminosTotales '() )

;Cantidad de caminos por ruta actual
(define caminos 0)


;Lista con botones actuales
(define botones '() )


;Lista para graficar pesos
(define pesos '() )


;Defnicion de los pens con su respectivo color

(define penCeleste (make-object pen% "LIGHTBLUE" 4 'solid))
(define penAzul (make-object pen% "BLUE" 4 'solid))
(define penNegro (make-object pen% "BLACK" 2 'solid))
(define red-pen (make-object pen% "RED" 4 'solid))(define white-pen (make-object pen% "SNOW" 1 'solid))


;Brushes

(define brushNodos (make-object brush% "WHITE" 'solid)) ;4
(define blanco (make-object brush% "WHITE" 'solid))
(define brushFlechas (make-object brush% "GAINSBORO" 'solid))
(define negro2 (make-object brush% "BLACK" 'hilite))




;Funcion que dibuja las referencias en el canvas para saber donde graficar la ciudad
(define (showGrid)
  (showGridAux 0 50 800 500)
  )

;Funcion auxiliar que dibuja las referencias en el canvas para saber donde graficar el grafo
(define (showGridAux n limites posX posY)
  (cond ( (and #t (<= posX n) (<= posY n) )
          #f
         )
        (else
         (send dc set-pen white-pen)
         (send dc set-brush negro2)
         
         ;Se dibujan las lineas
         (send dc draw-line n 0 n posY)
         (send dc draw-line 0 n posX n)
         
         (showGridAux (+ n limites) limites posX posY)
         (cond ( (equal? 0 (modulo n 100))
                 ;Dibuja los numeros de las coordenadas
                 (send dc draw-text (number->string n) 0 n)
                 (send dc draw-text (number->string n) n 0)
                )))
    ))







;Funcion para agregar una nueva ciudad al mapa
(define (agregarCiudad)
  ;Se ingresa el texto del field a la varible
  (set! nuevoNodo (send agregarCiudadText get-value))
  (set! nuevoNodoX (send agregarCiudadX get-value))
  (set! nuevoNodoY (send agregarCiudadY get-value))
  
  (cond ( (equal? #f (validarCiudad nuevoNodo));Si se encuentra que la ciudad no puede ser agregada se envía un mensaje de error dentro de la funcion llamada
          #f
         )
        (else

         (cond ( (equal? #f (validarCiudadCoords nuevoNodoX nuevoNodoY))
                 ;Si se encuentra que la ciudad no puede ser agregada se envía un mensaje de error dentro de la funcion llamada
                 #f
                )
               (else
                       ;Se agrega el nuevo nodo a grafoDinamico
                       (set! mapa (agregarNodo (string->number nuevoNodo) mapa))
                       ;Se agrega el nodo y sus posiciones a coords-list
                       (set! coordenadas (setPos (string->number nuevoNodo)
                                   (string->number nuevoNodoX)
                                   (string->number nuevoNodoY)))
                       ;Informacion del agergado en instructions-text-field
                       (send TextFieldInstrucciones set-value (string-append "-> Nueva Ciudad: " nuevoNodo
                                                                       "\n     Coordenada X: " nuevoNodoX
                                                                       "\n     Coordenada Y: " nuevoNodoY
                                                                       ))
                       ;Verifica la cantidad de ciudades actuales en grafo para activar botones y funciones
                       (validarCantidad)
                

                
                       ;Para graficar todos los nodos en la lista
                       (dibujarNodos)
                       
                )))
        ))


;Funcion para verificar si la ciudad puede ser agregada al mapa
(define (validarCiudad city)
  (validarCiudadAux city mapa)
  )

;Funcion auxiliar para verificar si la ciudad puede ser agregada al mapa
(define (validarCiudadAux city graph)
  ;Verifica que la ciudad ingresada sea una ciudad valida
  (cond ( (equal? #t (validarIdNodo city) )
          ;Verifica que la ciudad ingresada sea una ciudad que no exista ya
          (cond ( (equal? #t (validarExistencia city graph) )
                  #t
                  )
                (else
                 (send TextFieldInstrucciones set-value
                       "-> La ciudad deseada ya se encuentra en al mapa.\n-> Ingrese una ciudad no existente." )
                 #f
                 )
             )
          )
        (else
         (send TextFieldInstrucciones set-value
               "-> La ciudad deseada no puede ser agregada al mapa.\n-> Ingrese una ciudad entre 0 y 99." )
         #f
         )))
  


;Funcion para verificar que la ciudad tenga el formato deseado (0 a 99)
(define (validarIdNodo city)
  (ValidarNodoCiudadAux city 99)
  )

;Funcion auxiliar para verificar que la ciudad tenga el formato deseado (0 a 99)
(define (ValidarNodoCiudadAux city n)
  (cond ( (equal? n -1)
          #f
         )
        ( (equal? city (number->string n))
          #t
         )
        (else
         (ValidarNodoCiudadAux city (- n 1))
         )))

;Funcion para verificar que el nodo existe en grafoDinamico
(define (validarExistencia ciudad graph)
  (cond ( (null? graph)
          #t
         )
        ( (equal? (string->number ciudad) (caar graph) )
          #f
         )
        (else
         (validarExistencia ciudad (cdr graph) )
         )
    )
  )

;Funcion para verificar definir los límites del canvas para dibujar los nodos
(define (validarCiudadCoords x y)
  (validarCiudadCoordsAux x y 773 483)
  )


;Funcion auxiliar para verificar que las coordenadas sea numeros dentro del canvas
(define (validarCiudadCoordsAux x y dx dy)
  (cond ( (and #t (verificarCoordenada x dx) (verificarCoordenada y dy) )
          #t
         )
        (else
         (send TextFieldInstrucciones set-value
                       "-> Las coordenadas ingresadas no pertenecen dentro\n    del mapa.
-> La coordenada 'x' debe ser un valor entre 17 y 773
    y la coordenada en 'y' entre 17 y 483." )
                 #f
                 )))


;Verifica una sola coordenada por separado
(define (verificarCoordenada a da)
  (cond ( (equal? 16 da)
          #f
         )
        ( (equal? a (number->string da))
         #t
         )
        (else
         (verificarCoordenada a (- da 1))
         )))

;Funcion para agregar el nodo con sus respectivas coordenadas 
;Ya se encuentran verificados los parametros
(define (setPos ciudad x y)
  (append coordenadas (list (list ciudad x y)) )
  )

;Funcion para verificar la cantidad de nodos agregados
;Dependiendo de esto dependen otras funcionalidades del programa
(define (validarCantidad)
  (cond ( (send botonBuscar is-enabled?)
          #t
         )
        ( (and #t (<= 4 (length mapa)) prep)
          ;Habilita el boton de busqueda y los radio-buttons
          (send textFieldInicial enable #t)
          (send textFieldFinal enable #t)
          (send botonBuscar enable #t)
          (send botonRutas enable #t)
          
         )
        ( (equal? 2 (length mapa))
          (send botonAgregarCamino enable #t)
         )))







;Funcion para agregar un camino entre dos ciudades en el mapa
(define (agregarCamino)

  ;Se ingresa el texto del field a la varible
  (set! inicialNodo (send textFieldAgregarCam get-value))
  (set! finalNodo (send agregarCaminoFinal get-value))
  (set! pesoNuevo (send agregarPesoCamino get-value))

  (cond ( (equal? #f (verificarCamino inicialNodo finalNodo))
          ;Si se encuentra que los destinos no se encuentran en el mapa
          ;Se manda un mensaje de error dentro de la funcion llamada
          #f
         )
        (else
         (cond ( (equal? #f (mismoCamino inicialNodo finalNodo))
                ;Si se encuentra que los destinos son iguales
                ;Se manda un mensaje de error dentro de la funcion llamada
                #f
                )
               (else
                (cond ( (equal? #f (verificarPeso pesoNuevo))
                 ;Si se encuentra que el peso no es adecuado al formato
                 ;Se manda un mensaje de error dentro de la funcion llamada
                 #f
                 )
               (else
                (cond ( (equal? #f (verificarExCamino inicialNodo finalNodo pesoNuevo))
                        ;Si se encuentra que el camino se repite
                        ;Se manda un mensaje de confirmacion dentro de la funcion llamada
                        #f
                        )
                      (else
                       (cond ( (equal? #f (verificarInversoCamino inicialNodo finalNodo pesoNuevo))
                               ;Si se encuentra que la distancia del camino para hacerlo de doble via no es igual a de solo una via
                               ;Se manda un mensaje de confirmacion dentro de la funcion llamada
                               #f
                              )
                             (else
                              #t
                       ;Verificar que el camino no se repita
                       ;O que si se repite solo se cambie el peso
                       (set! mapa (agregarRuta
                              (string->number inicialNodo)
                              (string->number finalNodo)
                              (string->number pesoNuevo) mapa))
                       ;Guarda el peso es una lista para ser graficado
                       (almacenarPeso (string->number inicialNodo)
                                         (string->number finalNodo)
                                         (string->number pesoNuevo))
                       ;Informacion del agergado en instructions-text-field
                       (send TextFieldInstrucciones set-value (string-append "-> Nuevo Camino: "
                                                                              inicialNodo " -> "
                                                                              finalNodo ".\n"
                                                                              "     Peso: " pesoNuevo "."
                                                                       ))
                       ;Refresh al canvas
                       (send canvasMapa refresh-now)
                       ;Redibuja el grid
                       (showGrid)
                       ;Graficacion de caminos
                       (dibujarLineas)
                       ;Se grafican de nuevos los nodos para manener las lineas por debajo
                       (dibujarNodos)
                       ;Se grafican las distancias (pesos)
                       (dibujarPesos)
                       ;Se cambia el flag para efectuar busquedas luego del primer camino
                       (cond ( (equal? #f prep)
                               (set! prep #t)
                               (validarCantidad)))
                              ))))
                ))))
    )))

;Funcion que verifica la validez del camino por agregar
(define (verificarCamino i-city f-city)
  (cond ( (equal? #f (verificarCaminoAux i-city mapa) )
        ;Cond inicial
          (send TextFieldInstrucciones set-value
              "-> La ciudad inicial seleccionada no existe.\n-> Ingrese una nueva ciudad." )
          #f )
        ( (equal? #f (verificarCaminoAux f-city mapa) )
        ;Cond final
          (send TextFieldInstrucciones set-value
              "-> La ciudad final seleccionada no existe.\n-> Ingrese una nueva ciudad." )
          #f )
        (else
         #t )
     )
  )

;Funcion auxiliar que verifica la validez del camino por agregar
(define (verificarCaminoAux city graph)
  (cond ( (null? graph)
        #f
        )
        ( (equal? city (number->string (caar graph)))
          #t
         )
        (else
         (verificarCaminoAux city (cdr graph) )
         )))

;Fucion para verificar que las rutas no sean iguales
(define (mismoCamino i-city f-city)
  (cond ( (equal? #t (equal? i-city f-city) )
        ;Cond final
          (send TextFieldInstrucciones set-value
              "-> Las ciudades deben ser diferentes.\n-> Ingrese nuevas ciudades." )
          #f )
        (else
         #t )
    )
  )

;Funcion para verificar que el peso del camino se apegue al formato de estos (1-20)
(define (verificarPeso weight)
  (cond ( (equal? #f (verificarPesoAux weight 9) )
          ;Si el peso esta mal
          (send TextFieldInstrucciones set-value
              "-> La distancia deseada no se encuentra dentro del rango\n     de 1 y 9.\n-> Ingrese una nueva distancia." )
          #f )
        (else
         ;Si el peso es adecuado para el mapa
         #t )
     )
  )


;Funcion auxiliar para verificar que el peso del camino se apegue al formato de estos (1-20)
(define (verificarPesoAux weight max)
  (cond ( (equal? 0 max)
          #f
         )
        ( (equal? weight (number->string max))
         #t
         )
        (else
         (verificarPesoAux weight (- max 1) )
         )
    )
  )


;Funcion para verificar la existencia del camino deseado
;Si este existe, se mantiene el camino y se sobreescribe el peso.
(define (verificarExCamino ciudadInicio ciudadFinal weight)
  (verificarExistenciaCamAux ciudadInicio ciudadFinal weight mapa)
  )

;Funcion auxiliar para verificar la existencia del camino deseado
(define (verificarExistenciaCamAux ciudadInicio ciudadFinal weight graph)

  (cond ( (null? graph)
          #t
         )
        ( (equal? ciudadInicio (number->string (caar graph)) )
               (verificarConexiones ciudadInicio ciudadFinal (cadar graph) )
         )
        (else
         (verificarExistenciaCamAux ciudadInicio ciudadFinal weight (cdr graph))
         )
        )
  )


;Funcion para verificar si existe el camino inverso y si el peso es igual
(define (verificarInversoCamino ciudadInicio ciudadFinal weight)

  (cond ( (equal? #t (and (equal? #t (verificarExCamino ciudadInicio ciudadFinal weight)) (equal? #t (verificarExCamino ciudadFinal ciudadInicio weight))))
         ;Si no existe ninguno de los dos caminos
          #t
         )
        ( (equal? #f (verificarExCamino ciudadFinal ciudadInicio weight))
          ;Si existe el camino inverso, puede verificarse que el peso sea igual
         (verificarExCaminoInverso ciudadInicio ciudadFinal weight pesos)
         )
        (else
         ;Si no existe el camino inverso, se grafica normal
         
         #t)
        )
  )
  
;Funcion auxiliar para verificar si existe el camino inverso y si el peso es igual
(define (verificarExCaminoInverso ciudadInicio ciudadFinal weight list)
  (cond ( (null? list)
          ;Si el peso no es igual
          (send TextFieldInstrucciones set-value
              "-> La distancia deseada debe ser igual a la del camino \n     inverso.\n-> Ingrese la distancia correspondiente." )
          #f
         )
        ( (equal? #t (and (equal? (string->number ciudadFinal) (caar list) )
                          (equal? (string->number ciudadInicio) (cadar list) )
                          (equal? (string->number weight) (caddar list) ) ) )
          ;Si el camino inverso existe y el peso es igual
          #t )
        (else
         (verificarExCaminoInverso ciudadInicio ciudadFinal weight (cdr list))
         )))

;Funcion auxiliar de la auxiliar para verificar la existencia del camino deseado
;Esta se encarga de revisar en las conexiones del nodo deseado
(define (verificarConexiones ciudadInicio ciudadFinal conections)
  (cond ( (null? conections)
          #t
         )
        ( (equal? ciudadFinal (number->string (caar conections)))
          ;Si el camino ya existe
          (send TextFieldInstrucciones set-value
              "-> Este camino  ya existe .\n-> Por favor ingrese uno nuevo." )
          #f
         )
        (else
         (verificarConexiones ciudadInicio ciudadFinal (cdr conections))
         )))

;Funcion que guarda el nuevo camino para que su peso pueda ser graficado
(define (almacenarPeso i-city f-city weight)
  (set! pesos (append pesos (list (list i-city f-city weight))))
  )









;Funcion para encontrar la coordenada deseada de un nodo
;Parametros:
;   nodo: numero de ciudad
;   pos: coordenada ("x" o "y")
(define (getCoord nodo pos)
  (get-coord-aux nodo pos coordenadas)
    )

;Funcion auxiliar para encontrar la coordenada deseada de un nodo
;Parametros:
;   nodo: numero de ciudad
;   pos: coordenada ("x" o "y")
;   list: nodes-list
(define (get-coord-aux nodo pos list)
  (cond( (null? list)
         -1)
       ( (equal? nodo (caar list))
         (cond( (equal? pos "x")
                (cadar list)
               )
              ( (equal? pos "y")
                (caddar list)
               )
              (else
               -1)
           )
        )
       (else
        (get-coord-aux nodo pos (cdr list))
        )
   )
)







;Funcion para dibujar todos los nodos al iniciar la aplicacion
;Llama a una funcion auxiliar
(define (dibujarNodos)
  (dibujarNodosAux coordenadas dc)
  )

;Funcion auxiliar para dibujar todos los nodos al iniciar la aplicacion
;Parametros:
;   list: nodes-list
;   dc: Drawing Context
(define (dibujarNodosAux list dc)
  (cond ( (null? (cdr list) )
              (dibujarCiudad dc (caar list) (cadar list) (caddar list) )
           )
        (else
             (dibujarCiudad dc (caar list) (cadar list) (caddar list) )
             (dibujarNodosAux (cdr list) dc)
         )
  )
)


;Funcion para dibujar un nodo
;Parametros:
;   dc: Drawing Context
;   node: numero de ciudad
;   x: coordenada en x
;   y: coordenada en y
(define (dibujarCiudad dc node x y)

  (cond ( (equal? (quotient node 10) 0)
          (send dc set-brush brushNodos)
         )
        ( (equal? (quotient node 10) 1)
          (send dc set-brush brushNodos)
         )
        ( (equal? (quotient node 10) 2)
          (send dc set-brush brushNodos)
         )
        ( (equal? (quotient node 10) 3)
          (send dc set-brush brushNodos)
         )
        ( (equal? (quotient node 10) 4)
          (send dc set-brush brushNodos)
         )
        ( (equal? (quotient node 10) 5)
          (send dc set-brush brushNodos)
         )
        ( (equal? (quotient node 10) 6)
          (send dc set-brush brushNodos)
         )
        ( (equal? (quotient node 10) 7)
          (send dc set-brush brushNodos)
         )
        ( (equal? (quotient node 10) 8)
          (send dc set-brush brushNodos)
         )
        ( (equal? (quotient node 10) 9)
          (send dc set-brush brushNodos)
         )
        (else
         (send dc set-brush blanco)
         )
    )

  ;Dibuja el nodo (circulo)
  (send dc set-pen penNegro)
  (send dc draw-ellipse (- x 15) (- y 15) 30 30)

  ;Muestra el respectivo numero dentro del nodo
  (send dc set-pen white-pen)

  (cond ( (>= 9 node)
          (send dc draw-text (string-append "0" (number->string node)) (- x 10) (- y 10) )
         )
        (else
         (send dc draw-text (number->string node) (- x 10) (- y 10) )
         )
        )
  
  
 
)








;Funcion para dibujar todas las lineas (inicio de aplicacion)
(define (dibujarLineas)
  (dibujarLineasAux mapa);CAMBIO
  )

;Funcion auxiliar para dibujar todas las lineas (inicio de aplicacion)
;Parametros:
;   graph: graph
(define (dibujarLineasAux graph)
  (cond ( (null? (cdr graph) )
          (dibujarPorNodo dc (caar graph) (cadar graph))
         )
        (else         
         (dibujarPorNodo dc (caar graph) (cadar graph))
         (dibujarLineasAux (cdr graph) ) ;Elimina el primer nodo del grafo
         )
    )
  )


;Funcion para dibujar lineas de un nodo especifico
;Parametros:
;   dc: Drawing Context
;   node: nodo inicial
;   connections: lista de nodos que finalizan la linea
(define (dibujarPorNodo dc node connections)
  (cond ( (null? connections )
          #t
         )
        (else
         (dibujarLinea dc node (caar connections) (verificarCaminos (caar connections) node) )
         (dibujarPorNodo dc node (cdr connections) )
         )
    )
  )


;Funcion para dibujar una linea
;Parametros:
;   ini: nodo inicial
;   fin: nodo final
;   way: cantidad de vias (1 o 2)
(define (dibujarLinea dc ini fin direccion)
  (cond ( (equal? direccion 1)
          ;Dibuja solo las flechas de los caminos de una sola via
          (dibujarFlechas (+ 0 (getCoord ini "x")) (+ 0 (getCoord ini "y"))
              (+ 0 (getCoord fin "x")) (+ 0 (getCoord fin "y")) )
          ;Cambia al pen para una via
          (send dc set-pen penCeleste)
         )
        ( (equal? direccion 2)
          ;Cambia al pen para dos vias
          (send dc set-pen penAzul)
         )
    )
  ;Manda a dibujar la linea
  (send dc draw-line
        (+ 0 (getCoord ini "x")) (+ 0 (getCoord ini "y"))
        (+ 0 (getCoord fin "x")) (+ 0 (getCoord fin "y")) )
        ; + 15 ya que el nodo tiene radio de 30 -> asi la linea queda en el medio
  )


;Funcion para verificar si el camino es one-way o two-way
;Parametros:
;   node: nodo por verificar
;   fin: nodo por encontrar camino
(define (verificarCaminos node fin)
  (verificarCaminosAux node fin mapa);CAMBIO
  )

;Funcion auxiliar para verificar si el camino es one-way o two-way
;   node: nodo por verificar
;   fin: nodo por encontrar camino
;   graph: graph
(define (verificarCaminosAux node fin graph)
  (cond ( (null? graph)
          1
         )
        ( (equal? node (caar graph))
          (cond ( (ruta? fin (cadar graph) )
                  2
                 )
                (else
                 (verificarCaminosAux node fin (cdr graph) )
                 )
                )
         )
        (else
         (verificarCaminosAux node fin (cdr graph) )
         )
  )
  )

;Funcion para verificar si existe un path a la inversa del ya encontrado
;Parametros:
;   fin: nodo final que pasa a ser inicial
;   connections: lista de nodos que finalizarian la linea
(define (ruta? fin connections)
  (cond ( (null? connections)
          #f
         )
        ( (equal? fin (caar connections) )
          #t
          )
        (else
         (ruta? fin (cdr connections) )
         )
    )
 )


;Funcion para dibujar una flecha cerca de la linea
(define (dibujarFlechas x1 y1 x2 y2)
  (send dc set-pen penNegro)
  (send dc set-brush brushFlechas)

   (cond ( (equal? #t  (equal? x1 x2) )  ;x1 == x2
          (draw-arrow dc (/ (+ x1 (/ (+ x1 x2) 2)) 2)
                 (/ (+ y1 (/ (+ y1 y2) 2)) 2)
                 (/ (+ (/ (+ x1 x2) 2) x2) 2)
                 (/ (+ (/ (+ y1 y2) 2) y2) 2)
                 10 0)
         )
        ( (equal? #t  (equal? y1 y2) )  ;y1 == y2
          (draw-arrow dc (/ (+ x1 (/ (+ x1 x2) 2)) 2)
                 (/ (+ y1 (/ (+ y1 y2) 2)) 2)
                 (/ (+ (/ (+ x1 x2) 2) x2) 2)
                 (/ (+ (/ (+ y1 y2) 2) y2) 2)
                 0 10)
         )
        ( (equal? #t (and (> x1 x2) (> y1 y2) ) )  ;1
          (draw-arrow dc (/ (+ x1 (/ (+ x1 x2) 2)) 2)
                 (/ (+ y1 (/ (+ y1 y2) 2)) 2)
                 (/ (+ (/ (+ x1 x2) 2) x2) 2)
                 (/ (+ (/ (+ y1 y2) 2) y2) 2)
                 -8 8)
         )
        ( (equal? #t (and (< x1 x2) (> y1 y2) ) )  ;2
          (draw-arrow dc (/ (+ x1 (/ (+ x1 x2) 2)) 2)
                 (/ (+ y1 (/ (+ y1 y2) 2)) 2)
                 (/ (+ (/ (+ x1 x2) 2) x2) 2)
                 (/ (+ (/ (+ y1 y2) 2) y2) 2)
                 8 8)
         )
        ( (equal? #t (and (> x1 x2) (< y1 y2) ) )  ;3
          (draw-arrow dc (/ (+ x1 (/ (+ x1 x2) 2)) 2)
                 (/ (+ y1 (/ (+ y1 y2) 2)) 2)
                 (/ (+ (/ (+ x1 x2) 2) x2) 2)
                 (/ (+ (/ (+ y1 y2) 2) y2) 2)
                 8 8)
         )
        ( (equal? #t (and (< x1 x2) (< y1 y2) ) )  ;4
          (draw-arrow dc (/ (+ x1 (/ (+ x1 x2) 2)) 2)
                 (/ (+ y1 (/ (+ y1 y2) 2)) 2)
                 (/ (+ (/ (+ x1 x2) 2) x2) 2)
                 (/ (+ (/ (+ y1 y2) 2) y2) 2)
                 -8 8)
         )
        (else
         (draw-arrow dc (/ (+ x1 (/ (+ x1 x2) 2)) 2)
                 (/ (+ y1 (/ (+ y1 y2) 2)) 2)
                 (/ (+ (/ (+ x1 x2) 2) x2) 2)
                 (/ (+ (/ (+ y1 y2) 2) y2) 2)
                 20 20)
         )))










;Funcion para graficar el peso del camino entre dos nodos
(define (dibujarPesos)
  (dibujarPesosAux pesos)
  )

;Funcion auxiliar para graficar el peso del camino entre dos nodos
(define (dibujarPesosAux pesoCaminos)
  (cond ( (null? pesoCaminos)
          #t
         )
        (else
         ;Envia a dibujar el peso actual
         (dibujarPeso
          (+ 0 (getCoord (caar pesoCaminos) "x"))
          (+ 0 (getCoord (caar pesoCaminos) "y"))
          (+ 0 (getCoord (cadar pesoCaminos) "x"))
          (+ 0 (getCoord (cadar pesoCaminos) "y"))
          (caddar pesoCaminos) )
         ;Llama a la funcion nuevamente recursivamente para recorrer toda la lista
         (dibujarPesosAux (cdr pesoCaminos) )
         )
    )
  )


;Funcion para mostrar el peso de la linea cerca de esta
(define (dibujarPeso x1 y1 x2 y2 w)
  
  (cond ( (equal? #t  (equal? x1 x2) )  ;x1 == x2
          ;Dibuja el texto
          (send dc draw-text (number->string w)
                ( + (/ (+ x1 x2) 2) 10)
                ( + (/ (+ y1 y2) 2) 0)
                )
         )
        ( (equal? #t  (equal? y1 y2) )  ;y1 == y2
          ;Dibuja el texto
          (send dc draw-text (number->string w)
                ( + (/ (+ x1 x2) 2) 0)
                ( + (/ (+ y1 y2) 2) 10)
                )
         )
        ( (equal? #t (and (> x1 x2) (> y1 y2) ) )  ;1
          ;Dibuja el texto
          (send dc draw-text (number->string w)
                ( + (/ (+ x1 x2) 2) -20)
                ( + (/ (+ y1 y2) 2) 10)
                )
         )
        ( (equal? #t (and (< x1 x2) (> y1 y2) ) )  ;2
          ;Dibuja el texto
          (send dc draw-text (number->string w)
                ( + (/ (+ x1 x2) 2) 10)
                ( + (/ (+ y1 y2) 2) 10)
                )
         )
        ( (equal? #t (and (> x1 x2) (< y1 y2) ) )  ;3
          ;Dibuja el texto
          (send dc draw-text (number->string w)
                ( + (/ (+ x1 x2) 2) 10)
                ( + (/ (+ y1 y2) 2) 10)
                )
         )
        ( (equal? #t (and (< x1 x2) (< y1 y2) ) )  ;4
          ;Dibuja el texto
          (send dc draw-text (number->string w)
                ( + (/ (+ x1 x2) 2) -20)
                ( + (/ (+ y1 y2) 2) 10)
                )
         )
        
        (else
         ;Dibuja el texto
          (send dc draw-text (number->string w)
                ( + (/ (+ x1 x2) 2) 0)
                ( + (/ (+ y1 y2) 2) 0)
                ))
    ))










;Funcion para iniciar la busqueda de los caminos
(define (buscar)
  (cond ( (equal? #f (equal? destinoInicio destinoFin))
      (cond ( (equal? #t (verificarTexto)) ;Si los campos de texto estan correctos
          ;Deshabilita botones
          (send botonAgregarCiudad enable #f)
          (send botonAgregarCamino enable #f)
          (send botonBuscar enable #f)
          (send botonRutas enable #f)
          ;Habilita el boton de finalizar la busqueda
          (send botonFinalizar enable #t)
          ;Se informa en el text-box sobre la busqueda
          (verificarInfo)
          ;Se inicia la busqueda de los caminos dependiendo del estado de seleccion del usuario
          (buscarEstado)
         )
        (else
         ;(send information-text-field set-value
         ;     "-> Error en busqueda II." )
         #t
         )
       )
     )
        (else
         ;Si el destino inicial y final es el mismo
          (send TextFieldInstrucciones set-value
              "-> Las ciudades destino deben ser diferentes.\n-> Ingrese nuevos destinos." )
         )
        
   )
 )


;Funcion para verificar los fields antes de buscar las rutas
(define (verificarTexto)
  (cond 
        ( (equal? #f (verificarDestino destinoInicio coordenadas) )
        ;Si el destino inicial no esta en los nodos
          (send TextFieldInstrucciones set-value
              "-> El destino inicial seleccionado no existe.\n-> Ingrese un nuevo destino." )
          ; Wait a second to let the window get ready
  ;(sleep/yield 0.1)
          #f )
        ( (equal? #f (verificarDestino destinoFin coordenadas) )
        ;Si el destino final no esta en los nodos
          (send TextFieldInstrucciones set-value
              "-> El destino final seleccionado no existe.\n-> Ingrese un nuevo destino." )
          ; Wait a second to let the window get ready
  ;(sleep/yield 0.1)
          #f )
        (else
         #t )
  )
)

;Funcion para verificar las escogencias de los text-fields de los destinos
;Parametros:
;   text: string escrito en text-box
;   list: nodes-list
(define (verificarDestino destiny list)
  (cond ( (null? list)
          #f )
        ( (equal? destiny ( number->string (caar list)) )
          #t )
        (else
         (verificarDestino destiny (cdr list))
         )
    )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Funcion para cambiar el texto de instructions-text-field al presionar "Search"
(define (verificarInfo)
  ;Se guarda en un string la seleccion de los radio buttons
(cond ( (equal? 0 rutaSeleccion)
        (set! busqueda "Ruta más Corta" )
       )
      ( (equal? 1 rutaSeleccion)
        (set! busqueda "Todas las Rutas" )
       )
      )

  ;Se cambia el texto del text-field
  (send TextFieldInstrucciones set-value (string-append
                                           "-> Busqueda: " busqueda
                                           "\n\n-> Destino Inicial: " destinoInicio
                                           "\n-> Destino Final: " destinoFin )
        )
  )


;Funcion para empezar la busqueda de caminos dependiendo de la seleccion del usuario
(define (buscarEstado)
  (cond ( (equal? 0 rutaSeleccion)
          ;Camino mas corto
          (buscarCorta (string->number destinoInicio) (string->number destinoFin) )
         )
        ( (equal? 1 rutaSeleccion)
          ;Todos los caminos
          (buscarRutas (string->number destinoInicio) (string->number destinoFin) )
         )
        (else
         (send TextFieldInstrucciones set-value "-> Tipo de Busqueda no seleccionado.")
         )
    )
  )


;Funcion para graficar el CAMINO MAS CORTO
(define (buscarCorta ini fin)
  ;Se llama a la funcion BuscaCaminos en el archivo de logica
  ;Se guarda la lista con el camino y el peso
  (set! caminoCorto (rutaCorta ini fin mapa) )

  ;Verificar si shortest-path esta vacio
  (cond ( (null? caminoCorto)
          ;Si no se encuentra una ruta
          (send TextFieldInstrucciones set-value "-> No se ha encontrado ninguna ruta.")
         )
        (else
         ;Si se encuentra una ruta
         ; Wait a second to let the window get ready
         (sleep/yield 0.1)
         ;Dibuja el grid
         (showGrid)
         ;Dibuja todas las lineas nuevamente por si hay algun camino en pantalla
         (dibujarLineas)
         ;Llama a dibujar el camino mas corto
         (dibujarCamino caminoCorto)
         ;Se dibujan nuevamente los nodos para que camino queda debajo de estos
         (dibujarNodos)
         ;Se dibujan nuevamente los pesos
         (dibujarPesos)

         ;Ingresa la informacion del camino al text-field
         (send textFieldInfo set-value (string-append "Información de Rutas:\n"
                                                        "Ruta más corta:\nPeso:"
                                                        (number->string (cadr caminoCorto))))
         )
      )
   )



;Funcion para graficar TODOS LOS CAMINOS
(define (buscarRutas ini fin)
  ;Se llama a la funcion BuscaCaminos en el archivo de logica
  ;Se guarda la lista con los caminos y los pesos
  (set! caminosTotales (buscaRutasTotales ini fin mapa) )

  ;Verificar si shortest-path esta vacio
  (cond ( (null? caminosTotales)
          ;Si no se encuentra una ruta
          (send TextFieldInstrucciones set-value "-> No se ha encontrado ninguna ruta.")
         )
        (else
         ;Ingresa las informaciones de los caminos al text-field
         (infoCamino caminosTotales)
         
         ;Crea los botones necesarios
         (setCamino (length caminosTotales) 1)
         ;Se guarda una lista con los botones actuales
         (set! botones (send vpanel-buttons get-children) )
  
         ; Wait a second to let the window get ready
         (sleep/yield 0.1)

         ;Dibuja el grid
         (showGrid)
         ;Dibuja todas las lineas nuevamente por si hay algun camino en pantalla
         (dibujarLineas)
         ;Llama a dibujar el camino mas corto
         (dibujarCamino (car caminosTotales) )
         ;Se dibujan nuevamente los nodos para que camino queda debajo de estos
         (dibujarNodos)
         ;Se dibujan nuevamente los pesos
         (dibujarPesos)
         )
     )
 )


;Funcion para graficar un camino
;Parametros:
;   list: lista con el camino y el peso
(define (dibujarCamino list)
  (dibujarCaminoAux (car list))
  )

;Funcion auxilir para graficar un camino 
;Parametros:
;   path-list: lista solo con el camino
(define (dibujarCaminoAux listaCaminos)
  (cond ( (null? (cddr listaCaminos) )
          ;Grafica una linea entre los ultimos dos nodos
          (crearCamino dc (car listaCaminos) (cadr listaCaminos) )
         )
        (else
          ;Grafica una linea entre dos nodos
          (crearCamino dc (car listaCaminos) (cadr listaCaminos) )
          (dibujarCaminoAux (cdr listaCaminos) ) ;Elimina el primer nodo de la lista
         )
    )
  )


;Funcion para dibujar una linea
;Parametros:
;   ini: nodo inicial
;   fin: nodo final
;   way: cantidad de vias (1 o 2)
(define (crearCamino dc ini fin)
  ;Se escoje un pen rojo para trazar el camino
  (send dc set-pen red-pen)
  ;Manda a dibujar la linea
  (send dc draw-line
        (+ 0 (getCoord ini "x")) (+ 0 (getCoord ini "y"))
        (+ 0 (getCoord fin "x")) (+ 0 (getCoord fin "y")) )
        ; + 15 ya que el nodo tiene radio de 30 -> asi la linea queda en el medio

  )


;Funcion para ingresar la informacion de los caminos
(define (infoCamino listaCaminos)
  ;Redefine el information-text-field
  (send textFieldInfo set-value (string-append "Información de Rutas:\n"
                                                        (infoCaminoAux listaCaminos 1) ) )
  
  )

;Funcion para ingresar la informacion de los caminos
(define (infoCaminoAux listaCaminos n)
  
  (cond ( (null? listaCaminos )
          (set! caminos n)
          " "
         )
        (else

         (cond ( (equal? (length listaCaminos) 1)
                 (string-append "-> Ruta " (number->string n)
                                ":\n     Distancia: " (number->string (cadar listaCaminos)) 
                                (infoCaminoAux (cdr listaCaminos) (+ 1 n) ) )
                )
               (else
                (string-append "-> Ruta " (number->string n)
                               ":\n     Distancia: " (number->string (cadar listaCaminos)) "\n" 
                               (infoCaminoAux (cdr listaCaminos) (+ 1 n) ) )
                )
           )
          )
        )
  )


;Funcion para agregar los botones de las rutas
(define (setCamino total n)
  (cond ( (equal? (+ total 1) n)
          #t)
        (else
         
         (cond ( (> 20 n)
                 (setCaminoAux total n vpanel-buttons)
                 )
               ;Si se sobrepasa de 21 botones
               (else
                (setCaminoAux total n vpanel-buttons2)
                )
               )
         
         (setCamino total (+ n 1))
         
         )
        )
  )


;Funcion auxiliar para agregar los botones de las rutas
(define (setCaminoAux total n panel)
  (cond ( (< (length botones) n )
          ;Boton Ruta N
          (new button% [parent vpanel-buttons] [label (string-append "Ruta " (number->string n) )]
               
               [callback (lambda (button event)
                           ;Dibuja todas las lineas nuevamente por si hay algun camino en pantalla
                           (dibujarLineas)
                           
                           (crearUnCamino n caminosTotales)

                           ;Se dibujan nuevamente los nodos para que camino queda debajo de estos
                           (dibujarNodos)
                           ;Se dibujan nuevamente los pesos
                           (dibujarPesos)
                           
                         )])
         )
        (else

         (send (getBoton n botones) show #t)

         )
        )
  )


;Funcion para obtener el boton necesario n
(define (getBoton n list)
  (cond ( (equal? 1 n)
          (car list)
         )
        (else
         (getBoton (- n 1) (cdr list))
         )
   )
  )


;Funcion para eliminar los botones luego de ser utilizados
(define (deleteButtons list)
  (cond ( (null? list)
          #t
          )
        (else
         (send (car list) show #f)

         (deleteButtons (cdr list) )
         
         )
    )
  )

;Funcion para graficar una ruta especifica
;Usada en los botones dinamicos
(define (crearUnCamino n paths)
  (cond ( (<= n (length paths))   
          (cond ( (equal? n 1)
                  (dibujarCamino (car paths))
                 )
                (else
                 (crearUnCamino (- n 1) (cdr paths) )
                 )
            )
         )
        (else
         (send TextFieldInstrucciones set-value "-> Esta ruta no existe")
         )

        )
  )


;Funcion que sera llamada al presionar "Nueva Busqueda"
;"Reiniciara" la aplicacion
(define (end)
  ;Re-draws
  (showGrid)
  (dibujarLineas)
  (dibujarNodos)
  (dibujarPesos)
  ;Redefinicion de instruction-text-field
  (send TextFieldInstrucciones set-value "                                  ¡Bienvenido a Wazitico!\n
-> Agregar nuevas ciudades y caminos.
-> Buscar rutas entre ciudades.")
  ;Borra las selecciones de los destinos en text-fields
  (send textFieldInicial set-value "")
  (send textFieldFinal set-value "")
  ;Redefinicion de information-text-field
  (send textFieldInfo set-value "Información de Rutas:" )
  ;Elimina los botones de las rutas
  (deleteButtons botones)
  ;Habilita botones
  (send botonAgregarCiudad enable #t)
  (send botonAgregarCamino enable #t)
  (send botonBuscar enable #t)
  (send botonRutas enable #t)
  ;Deshabilita el boton de finalizar la busqueda
  (send botonFinalizar enable #f)
  )










;Frame inicial
(define initial-frame (new frame% [label "Wazitico"]
                   [width 100]
                   [height 200]
                   [alignment '(center center)]))

;Panel vertical
;Incluye titulo y panelr vertical de info
(define vpanel-initial (new vertical-panel% [parent initial-frame]
                            [alignment '(center center)]))


;

;Dirección de archivo específico en documentos propios

(require racket/draw
         net/url)


;

;Panel horizontal
(define hpanel-initial (new horizontal-panel% [parent vpanel-initial]
                            [alignment '(center center)]))


;Boton Iniciar
(define initial-initialize-buton (new button% [parent hpanel-initial]
             [label "Ingresar"]
             [vert-margin 10]	 
             [horiz-margin 5]
             [callback (lambda (button event)
                         ;Cierra la ventana inicial
                         (send initial-frame show #f)
                         ;Abre la ventana principal
                         (send framePrincipal show #t)

                         ; Wait a second to let the window get ready
                         (sleep/yield 1)
                         (showGrid)

                         )]))







; Frame principal
(define framePrincipal (new frame% [label "Wazitico"]
                   [width 1500]
                   [height 800]
                   [alignment '(left top)]))


;Panel principal (horizontal)
(define panelPrincipal (new horizontal-panel% [parent framePrincipal] ))
 

;Panel vertical
;Incluye cavas(mapa) y panel horizontal secundario
(define vpanel (new vertical-panel% [parent panelPrincipal]))

;Boton Search
(define botonAgregarCiudad (new button% [parent vpanel]
             [label "Agregar Ciudad"]
             [vert-margin 10]	 
             [horiz-margin 5]
             [callback (lambda (button event)
                         (send frameCiudad show #t)
                         )]))

;Boton Agregar Camino
(define botonAgregarCamino (new button% [parent vpanel]
             [label "Agregar Ruta"]
             [enabled #f]
             [vert-margin 10]	 
             [horiz-margin 5]
             [callback (lambda (button event)
                         (send add-road-frame show #t)
                         )]))



;Panel horizontal secundario
;Incluye text-field (instrucciones) y vpanel2 (botones)
(define panelHor (new horizontal-panel% [parent vpanel]
                     [alignment '(center center)]))


;Text-field de instrucciones
(define TextFieldInstrucciones ( new text-field% [parent panelHor]
                                    [label #f]
                                    [init-value
                                     "¡Bienvenido a Wazitico!\n
-> A continuación podrá crear su propia mapa
-> Utilice los botones agregar ciudad y agregar ruta para formarlo
-> Inserte una ciudad para empezar" ]
                                    [vert-margin 10]	 
                                    [horiz-margin 10]
                                    [min-width 380]
                                    [min-height 120]))


;Radio Button Selection
(define botonRutas (new radio-box% [label ""]
     [enabled #f]                 
     [choices '("Ruta más Corta" "Todas las Rutas")]
     [parent vpanel]
))
     

;Boton Search
(define botonBuscar (new button% [parent vpanel]
             [label "Buscar"]
             [enabled #f]
             [callback (lambda (button event)
                         ;Ingresa el texto de los fields a sus respectivas variables
                         (set! destinoInicio (send textFieldInicial get-value))
                         (set! destinoFin (send textFieldFinal get-value))
                         (set! rutaSeleccion (send botonRutas get-selection) )
                         ;Comienza el proceso de busqueda
                         (buscar)
                         )]))


;Boton New Path
(define botonFinalizar (new button% [parent vpanel]
             [label "Finalizar"]
             [enabled #f]
             [vert-margin 10]	 
             [horiz-margin 5]
             [callback (lambda (button event)
                         (end)
                         )]))




;Panel vertical 
;Agregar Ciudades y Caminos
(define vpanel2 (new vertical-panel% [parent panelHor]
                     [alignment '(center center)]))

;Canvas donde se muestra el grafo
(define canvasMapa (new canvas% [parent vpanel2]
                       [style '(border)]
                       [label "MAP"] 
                       [vert-margin 10]	 
                       [horiz-margin 10]
                       [min-height 500]
                       [min-width 800]
                       ))


;Drawing Context de map-canvas
(define dc (send canvasMapa get-dc))


;Panel vertical IV
;Destinos
(define vpanel4 (new vertical-panel% [parent panelHor]
                     [alignment '(center center)]))


;Text-field de destino inicial
(define textFieldInicial ( new text-field% [parent vpanel4]
                                 [enabled #f]       
                                 [label #f]
                                 [min-width 10]
                                 ))

;Label de destino Inicial
(define intial-label (new message% [parent vpanel4]
                          [label "Ciudad Origen"]))


;Text-field de destino final
(define textFieldFinal ( new text-field% [parent vpanel4]
                               [enabled #f]       
                               [label #f]
                               [min-width 10]
                               ))

;Label de destino Final
(define final-label (new message% [parent vpanel4]
                          [label "Ciudad Destino"]))


;Panel vertical V
;Busqueda
(define vpanel5 (new vertical-panel% [parent panelHor]
                     [alignment '(center center)]
                     ))




;Text-field de informacion
(define textFieldInfo ( new text-field% [parent panelPrincipal]
                                    [label #f]
                                    [init-value "Información:"]
                                    [vert-margin 10]	 
                                    [horiz-margin 10]
                                    [min-width 160]
                                    [min-height 640]))


;Panel vertical de botones principal
;
(define vpanel-buttons (new vertical-panel% [parent panelPrincipal]
                            [horiz-margin 8]
                     [alignment '(center center)]
                     ))

;Panel vertical de botones secundario
;
(define vpanel-buttons2 (new vertical-panel% [parent panelPrincipal]
                             	;[enabled #f]
                             [horiz-margin 3]
                     [alignment '(center center)] ))                   





;Frame de Agregar Ciudad
(define frameCiudad (new frame% [label "Agregar Ciudad"]
                   [width 140]
                   [height 180]
                   [alignment '(center center)]))

;Panel vertical city
;Agregar Ciudad
(define panelCiudad (new vertical-panel% [parent frameCiudad]
                     [alignment '(center center)]))


;Text-field de agregar ciudad
(define agregarCiudadText ( new text-field% [parent panelCiudad]
                                    [label #f]
                                    [min-width 10]))


;Label de agregar ciudad
(define labelAgregarCiudad (new message% [parent panelCiudad]
                          [label "Nuevo Nodo"]))


;Text-field de agregar ciudad
(define agregarCiudadX ( new text-field% [parent panelCiudad]
                                    [label #f]
                                    [min-width 10] ))


;Label de agregar ciudad
(define labelCiudadX (new message% [parent panelCiudad]
                          [label "Posición en x"]))

;Text-field de agregar ciudad
(define agregarCiudadY ( new text-field% [parent panelCiudad]
                                    [label #f]
                                    [min-width 10] ))


;Label de agregar ciudad
(define labelCiudadY (new message% [parent panelCiudad]
                          [label "Posición en y"]))



;Boton Search
(define agregarCiudadBoton (new button% [parent panelCiudad]
             [label "Agregar Ciudad"]
             [callback (lambda (button event)
                         (agregarCiudad) )]))










;Frame de Agregar Camino
(define add-road-frame (new frame% [label "Agregar Camino"]
                   [width 140]
                   [height 180]
                   [alignment '(center center)]))


;Panel vertical III
;Agregar Camino
(define vpanel3 (new vertical-panel% [parent add-road-frame]
                     [alignment '(center center)]))


;Text-field de agregar ciudad
(define textFieldAgregarCam (new text-field% [parent vpanel3]
                                    [label #f]
                                    [min-width 10]
                                    ))

;Label de agregar ciudad
(define add-road-initial-label (new message% [parent vpanel3]
                          [label "Ciudad Inicial"]))

;Text-field de destino Final
(define agregarCaminoFinal ( new text-field% [parent vpanel3]
                                    [label #f]
                                    [min-width 10]
                                    ))

;Label de destino Final
(define add-road-final-label (new message% [parent vpanel3]
                          [label "Ciudad Final"]))

;Text-field de peso
(define agregarPesoCamino (new text-field% [parent vpanel3]
                                    [label #f]
                                    [min-width 10]
                                    ))

;Label de peso
(define labelAgregarRuta (new message% [parent vpanel3]
                          [label "Distancia"]))

;Boton Agregar Camino
(define botonAgregarRuta (new button% [parent vpanel3]
             [label "Agregar Camino"]
             [callback (lambda (button event)
                         (agregarCamino)
                         )]))




; Se muestra el frame inicial
(send initial-frame show #t)


#|
;Utilizado para graficar el grafo preestablecido antes de
;haber hecho el programa dinamico.
;Lista de Nodos con coordenadas
(define nodes-list '(
        (11 442 279) (12 343 309) (13 510 339) (14 638 378)
        (21 393 219) (22 432 104) (23 265 40)
        (31 589 279) (32 687 249)
        (41 521 169) (42 540 90)
        (51 88 189) (52 137 70)
        (61 274 189) (62 736 428)
        (71 678 159) 
     )
  )
|#
