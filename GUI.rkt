#lang racket/gui


; Define lo necesario para la la ventana del menu

(define menu (new frame%
                        [label "Wazetico"]
                        [width 200]
                        [height 120]
                        ))

(define panel_menu (new vertical-panel% [parent menu]
                                     [alignment '(left top)]))

; Define los botones del menu y que abran las demas ventanas

(new button% [parent menu] [label "Navegacion"]
     [callback (lambda (button event)
                 (send mainWindow show #t))])
(new button% [parent menu] [label "Nueva ubicacion"]
     [callback (lambda (button event)
                 (send placeWindow show #t))])
(new button% [parent menu] [label "Nueva ruta"]
     [callback (lambda (button event)
                 (send pathWindow show #t))])

; Define lo necesario para la ventana para buscar una ruta

(define mainWindow (new frame%
                        [label "Wazetico"]
                        [width 800]
                        [height 600]
                        ))

; Paneles de la ventana

(define panel_root (new vertical-panel% [parent mainWindow]
                                     [alignment '(left top)]))
(define panel_0 (new horizontal-panel% [parent panel_root]
                                     [alignment '(left top)]))
(define panel_1 (new vertical-panel% [parent panel_0]
                                     [alignment '(left top)]))
(define panel_2 (new vertical-panel% [parent panel_0]
                                     [alignment '(left top)]))

; Input para ingresar el punto de origen de la busqueda

(define input1 (new text-field%
                    [parent panel_1]
                    [label "Ingrese el origen"]
                    [vert-margin 5]
                    [horiz-margin 5]
                    ))

(define input2 (new text-field%
                    [parent panel_2]
                    [label "Ingrese el destino"]
                    [vert-margin 5]
                    [horiz-margin 5]
                    ))

(new button% [parent panel_2] [label "Aceptar"])

; Ventana para definir una nueva ubicacion

; Se define la ventana
(define placeWindow (new frame%
                        [label "Wazetico"]
                        [width 800]
                        [height 600]
                        ))

; Se define el nombre del lugar (Nodo) 
(define input3 (new text-field%
                    [parent placeWindow]
                    [label "Ingrese el nombre del lugar"]
                    [vert-margin 5]
                    [horiz-margin 5]
                    ))

(new button% [parent placeWindow] [label "Aceptar"])



; Ventana para agregar una nueva arista


; Define la ventana 
(define pathWindow (new frame%
                        [label "Wazetico"]
                        [width 200]
                        [height 150]
                        ))

; Define la ventana principal
(define panelPath (new vertical-panel% [parent pathWindow]
                                     [alignment '(left top)]))

; Define el input para ingresar el punto de origen
(define input4 (new text-field%
                    [parent panelPath]
                    [label "Ingrese el origen"]
                    [vert-margin 5]
                    [horiz-margin 5]
                    ))

; Define el input para ingresar el destino
(define input5 (new text-field%
                    [parent panelPath]
                    [label "Ingrese el destino"]
                    [vert-margin 5]
                    [horiz-margin 5]
                    ))
; Define la distacia (costo) de la arista 
(define input6 (new text-field%
                    [parent panelPath]
                    [label "Ingrese la distancia"]
                    [vert-margin 5]
                    [horiz-margin 5]
                    ))

; Panel secundario para ordenar los botones
(define panelPath1 (new horizontal-panel% [parent panelPath]
                                     [alignment '(center center)]))

; Botones de la ventana
(new button% [parent panelPath1] [label "Aceptar"])
(new button% [parent panelPath1] [label "Cancelar"])

; Funcion para mostrar la ventana cuando se corre el programa
(send menu show #t)
