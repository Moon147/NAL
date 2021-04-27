(in-package :nal)

(define-easy-handler (index :uri "/NAL-Reasoner/index.html"
                                :default-request-type :post)
      

      (conocimiento valorV relacion expresion debug (selectbc :parameter-type 'integer)
        (selectlog :parameter-type 'integer)
        ;comportamiento 
        (comportamiento :parameter-type 'string)
        (decimales :parameter-type 'integer)
        (kuser :parameter-type 'integer)
        (opcjoin :parameter-type 'string) 
        (opcadd :parameter-type 'string))


  ;------------------variables para subir archivos--------------------------
  (let (post-parameter-p)
  (when (post-parameter "file1")
    (handle-file (post-parameter "file1"))
    (setq post-parameter-p t))
  (when (post-parameter "clean")
    (clean-tmp-dir)
    (setq post-parameter-p t)))

    ;(log:config :info)

  (with-html
    (:html
     (:head 
      (:title "Inferencia No-axiomática")
      (:link :rel "stylesheet" :type "text/css" :href "pushbar.css")
      (:link :rel "stylesheet" :type "text/css" :href "estilo.css")
      (:script :src "https://ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js")
      (:meta :charset "UTF-8")
      (:link :rel "stylesheet" 
        :href "https://fonts.googleapis.com/css?family=Libre+Franklin&display=swap")
      (:script :src "https://kit.fontawesome.com/d0a92786ea.js"
        :crossorigin "anonymous"))

    ;Muestra BC cuando el usuario selecciona una
   (cond ((numberp selectbc) 
      (setf var-selectbc selectbc)
      (if opcjoin (setf opcjoin-selectbc 'T)) ;Bandera para agrega la BC seleccionada a la BC anterior trabajada
      (setf opcjoin 'nil)
      (manage-files)))
    (cond ((numberp decimales) (setf var-decimales decimales)))
    (if opcadd (setf var-addexp 'T))
    (if (numberp kuser) (setf k kuser))
    ;------------------------------------------- 
     (:body :onload "recuperarPolitica()"
      (:div :id "contenedor"
        (:header
          (:table :style "width:100%"
          (:tr
            (:th :id "logoIPN" :style "width:100px;vertical-align:top"
              (:img :src "logo_ipn3.png" :height "80"))
            (:th :id "titulo" :style "text-align:center;vertical-align:top; padding: 3px"
              (:h1 "Inferencia No-axiomática"))
            (:th :id "institucion" :style "text-align:right;width:260px;vertical-align:top;padding:10px"
              (:h3 "Instituto Politécnico Nacional")
              (:h3 "Escuela Superior de Cómputo"))
            (:th :id "logoESCOM" :style "text-align:right;width:70px;vertical-align:top;padding: 8px; padding-top: 5px"
              (:img :src "logo_escom.png" :height "50"))
            (:th :id "logoCIC" :style "text-align:right;width:70px;vertical-align:top;padding: 8px; padding-top: 0px"
              (:img :src "logo_cic3.png" :height "75")) )))
        (:aside 
          (:div :id "collapse" :class "aside-close"
            (:ul :class "tabs1"
                      (:li :class "tabs__item active" :onclick "openTab(event,'TABLA')" 
                        (:h4 "BC USUARIO"))
                      (:li :class "tabs__item" :onclick "openTab(event,'BC');"
                        (:h4 "BC AGENTE")))
                    (:div :id "aside"
                      (:div :class "spanBC" 
                        (:span :class "c1" "Base de Conocimiento")
                        (:div  :class "collapse" 
                          (:i :class "openCollapse fas fa-angle-double-right" :id "openCollapse" :onclick  "collapse()")
                          (:i :class "closeCollapse fas fa-window-close " :id "closeCollapse" :onclick "collapseOff()"))
                       )
                      (:table :id "TABLA" :class "tabcontent active"
                        (:tr 
                          (:th :class "num" "No")
                          (:th :class "exp" "Expresión")
                          (:th :class "vv" "Valor de verdad"))
                        
                        (cond ((search "?" conocimiento)
                                (if (parseq:parseq 'query conocimiento)  
                                  (query-NAL1 (parseq:parseq 'query conocimiento) var-decimales) 
                                  (insert2 (format 'nil "Error en: ~a. Revise la estructura de su consulta."  conocimiento)) )
                                (if opcadd (parser  (concatenate 'string "(" (string truthv) ")") ))       ;Se agrega el resultado de la consulta a BC si opcadd fue seleccionado 
                                (setq truthv 'nil)
                                )                 ;Se reinician las variables                    
                              ((parseq:parseq 'funciones conocimiento) 
                                  (if (inference-rules (parseq:parseq 'funciones conocimiento) var-decimales)
                                  (insert2 (format 'nil "Error en: ~a. Revise la estructura de las reglas de inferencia"  conocimiento)) )
                                (if opcadd (parser  (concatenate 'string "(" (string statement) ")")))        ;Se agrega el resultado de la consulta a BC si opcadd fue seleccionado 
                                (setq statement 'nil))
                            (T (parser conocimiento) ))       
          
                          (loop for i from 1 to (- *cont* 1)
                           do 
                            (setf expresion (first (obtiene-expresion (list i))))
                            (setf valorV (second (third expresion)))
                            (setf relacion (string-downcase (first (third expresion))))
                              (htm
                               (:tr 
                                (:td (print i))
                                (:td (format t "~a" relacion))
                                (:td (format t "~a" valorV))))) )
                      (:table :id "BC" :class "tabcontent"
                        (:tr 
                          (:th :class "num" "No")
                          (:th :class "exp" "Expresión")
                          (:th :class "vv" "Valor de verdad")) 
                        (loop for i from 1 to (- *cont4* 1)
                           do 
                            (setf expresion (first (obtiene-agente (list i))))
                            (cond 
                              ((third expresion)  ;-----------------------EXPRESIÓN CON CÓPULAS --> O-> ->O O->O
                                (setf valorV (second (third expresion)))
                                (setf relacion (string-downcase (first (third expresion)))) 
                                (htm
                                 (:tr 
                                  (:td (print i))
                                  (:td (format t "~a" relacion))
                                  (:td (format t "~a" valorV)))) ) 
                              (T                   ;-----------------------EXPRESIÓN CON CÓPULA <-> SE IMPREMEN LAS 2 VERSIONES DE LA BC DE AGENTE
                                (setf valorV (second (third (first expresion))))
                                (setf relacion (string-downcase (first (third (first expresion)))) )
                                (htm
                                 (:tr 
                                  (:td (print i))
                                  (:td (format t "~a" relacion))
                                  (:td (format t "~a" valorV))))
                                (setf valorV (second (third (second expresion))))
                                (setf relacion (string-downcase (first (third (second expresion)))) )
                                (htm
                                 (:tr 
                                  (:td (print i))
                                  (:td (format t "~a" relacion))
                                  (:td (format t "~a" valorV)))) )) )) ))
          );aside

        (:section :id "contenido"
          (:ul :class "tabs2"
            (:li :class "tabs__item2 active" :onclick "openTab2(event,'informacion')"
              (:h4 "SALIDA"))
            (:li :class "tabs__item2" :onclick "openTab2(event,'DEBUG')"
              (:h4 "RASTREO")))
          (:div :class "tabcontent2 active" :id "informacion"
	     
             ;(format t " comportamiento: ~A ~% decimales: ~A ~% kuser: ~A ~% opcadd: ~A " comportamiento decimales kuser opcadd)
            (loop for i from 1 to (- *cont2* 1) 
                   do 
                   (setf expresion (obtiene-mensaje (list i)))
                   (htm
                      (:p :class "parrafo-salida" "  "(print i) 
                         (format t "~a" (first expresion)) )) ) )

          (:div :class "tabcontent2" :id "DEBUG"              
            (loop for i from 1 to (- *cont3* 1)
                   do 
                   (setf debug (obtiene-debug (list i)))
                   (htm
                      (:pre :class "parrafo-salida" "  "(print i) 
                         (format t "~a" (first debug)) )) )
             )

          ;(print(parseq 'judgement conocimiento))
          (:button :class "simbolo" :onclick "simbolo('-->')" :style "margin-left: 10px" "-->")
          (:button :onclick "simbolo('<=>')"  :class "simbolo" "<->")
          (:button :onclick "simbolo('o->')"  :class "simbolo" "o->")
          (:button :onclick "simbolo('->o')"  :class "simbolo" "->o")
          (:button :onclick "simbolo('o->o')"  :class "simbolo" "o->o")
          (:button :onclick "simbolo('&')"  :class "simbolo" "&")
          (:button :onclick "simbolo('|')"  :class "simbolo" "|")
          (:button :onclick "simbolo('-')"  :class "simbolo" "-")
          (:button :onclick "simbolo('~')"  :class "simbolo" "~")
          (:button :onclick "simbolo('*')"  :class "simbolo" "*")
          (:p (:form :method :post 
              (htm  
		(:input :style "display:none;" :id "comportamiento" :name "comportamiento")
		(:input :style "display:none;" :id "decimales" :name "decimales")              
		(:input :style "display:none;" :id "kuser" :name "kuser")              
		(:input :style "display:none;" :id "opcadd" :name "opcadd")
		(:input :type :text :class "conocimiento" :id "conocimiento"  :name "conocimiento"  
                  :placeholder "Ingrese sus consultas" ))
		(:input :class "botonSubir" :onclick "scroll()" :type "submit" :value "Enviar" ))))

        (:div :class "pc" :data-pushbar-target "mypushbar1"
          (:i :class "fas fa-angle-double-left"))
        (:section :data-pushbar-id "mypushbar1" :class "pushbar from_right"
          (:button :class "cerrar" :data-pushbar-close :value"x")
          (:button :class "regresar" :onclick "back()" (:p "<"))
          (:section :class "menu"
            (:div :class "submenu" :onclick "display_pushbar('.politica')"  (:h5 "Política de Control"))
            (:div :class "submenu" :onclick "display_pushbar('.selecciona-BC')"  (:h5 "Seleccionar Base de Conocimiento"))
            (:div :class "submenu" :onclick "display_pushbar('.sube-BC')"  (:h5 "Subir Base de Conocimiento"))
            (:div :class "submenu" :onclick "display_pushbar('.reglas')"  (:h5 "Reglas de Inferencia"))
            ;(:button :data-pushbar-target "mypushbar2" :class "btn" (:p "Cerrar sesión"))
            )

          ;;---------------------------------------FORMULARIO POLIÍTICA DE CONTROL------------------------------
          (:section :class "politica sub-menu"
            (:span :class "c2" "Política de Control")
          (:form :method :post :style "margin-left: 15px" :id "politica" :name "politica"
          (:legend "Valor de verdad") :br
          (:p "Decimales: "
            (:select :id "lpcomportamiento" :name "lpcomportamiento"
             (loop for (value option) in '((:redondear "Redondear")
                                           (:truncar "Truncar"))
                   do (htm
                       (:option :value value
                        :selected (eq value comportamiento)
                        (str option)))) )) :br
          (:p "Cantidad de decimales: " :br :t 
            (:input 
              :id "lpdecimales"
              :name "lpdecimales"
              :value (or decimales 2))) :br
          (:p "Parámetro k: " :br :t 
            (:input 
              :id "lpkuser"
              :name "lpkuser"
              :value (or kuser 1))) :br
          (:p  (:input :type "checkbox"
                 :name "lpopcadd"
                 :value "off"
                 :id "lpopcadd"
                 (format t "Agregar resultado a la Base de Conocimiento")))
                             :br
          (:p 
            (:input :type "button" :id "save" :class "submit" :value "Guardar datos")
            (:input :type "reset" :value "Restaurar"))))

          ;---------------------------------------SUBIR BASE DE CONOCIMIENTO-----------------------------------------
          (:section :class "sube-BC sub-menu"
            (:span :class "c3" "Subir Base de Conocimiento")
          (no-cache)
            (:form :method :post :enctype "multipart/form-data" 
             (:p :class "parrafo-subir-bc" "Archivo: "
              (:input :type :file :accept ".txt" :id "files"
               :name "file1" :multiple))
             (:p :class "parrafo-subir-bc" (:input :type :submit))) )


            ;---------------------------------------SELECCIONAR BASE DE CONOCIMIENTO---------------------------------
          (:section :class "selecciona-BC sub-menu"
            (:span :class "c4" "Seleccionar Base de Conocimiento")
              (:form :method :post :style "margin-left: 15px"
                (:p     ;formulario para escoger BC
                  (:select :name "selectbc" :style "margin-bottom: 15px"
                   (loop for x from 1 to (length *files*)
                    do
                      (htm
                        (:option :value x  :selected (eq x selectbc) (print x)))) ))
                (:p
                  (:input :type "checkbox"
                             :name "opcjoin"
                             :value "agregar"
                             :checked (or (string= "agregar" opcjoin) var-addexp)
                             (format t "~a" "Unir bases de conocimiento")) )

                  ;Muestra archivos de BC
                  (when *files* 
                    (htm (:p
                      (:table :style "padding: 5px;" :border 1 :cellpadding 2 :cellspacing 0
                       (loop for (path file-name nil) in (reverse *files*)
                         for counter from 1
                         do (htm
                             (:tr (:td :align "right" (str counter))
                              (:td :onclick "almacena()" (esc file-name) )
                              (:td :align "right"
                               (str (ignore-errors
                                      (with-open-file (in path)
                                        (file-length in)) ))
                               "&nbsp;Bytes"))))))))   
            (:p (:input  :type "submit" :style "margin-top: 10px"  )) ) ) ;fin formulario- section


          ;;---------------------------------------REGLAS DE INFERENCIA-------------------------------------------
          (:section :class "reglas sub-menu"
            (:span :class "c2" "Reglas de Inferencia") 
            (:div :style "margin-left: 15px"
              (:u (:b :style "font-size: 18px; display: flex; justify-content: center" "NAL-1"))
              (:i :style "font-size: 13px;" "Las premisas a usar deben tener la siguiente estructura según cada regla (el orden en que se usen las premisas es indistinto):") :br :br
              (:b "-Locales:") :br
              (:i :style "font-size: 13px;" "El término M y P deben ser comunes en ambas premisas, siendo M el sujeto y P el predicado. Las premisas deben contener valores de verdad diferentes") :br
              (:p :style "margin-left: 5px" "Revisión:" :br 
                  (:p :style "margin-left: 10px" "(revisión No-exp1: (M --> P) No-exp2: (M --> P))")) :br 
              (:p :style "margin-left: 5px" "Selección:" :br 
                  (:p :style "margin-left: 10px" "(selección No-exp1: (M --> P) No-exp2: (M --> P))")) :br ;*(valor-verdad | evidencia acumulada)
              (:hr :style "margin-right: 10px") :br  
              (:b "-Hacia adelante:") :br
              (:i :style "font-size: 13px;" "M es el término común en ambas premisas, mienstras que P y S son los no comunes") :br
              (:p :style "margin-left: 5px" "Deducción:" :br 
                  (:p :style "margin-left: 10px" "(deducción No-exp1: (M --> P) No-exp2: (S --> M))")) :br 
              (:p :style "margin-left: 5px" "Inducción:" :br 
                  (:p :style "margin-left: 10px" "(inducción No-exp1: (M --> P) No-exp2: (M --> S))")) :br 
              (:p :style "margin-left: 5px" "Abducción:" :br 
                  (:p :style "margin-left: 10px" "(abducción No-exp1: (P --> M) No-exp2: (S --> M))")) :br 
              (:p :style "margin-left: 5px" "Ejemplificación:" :br 
                  (:p :style "margin-left: 10px" "(ejemplificación No-exp1: (M --> P) No-exp2: (S --> M))")) :br 
              (:p :style "margin-left: 5px" "Conversión:" :br 
                  (:p :style "margin-left: 10px" "(conversión No-exp1: (P --> S))")) ) )

        );/section data-pushbar-id 

      (:section :data-pushbar-id "mypushbar2" :class "pushbar from_center"
        (:button :class "cerrar" :data-pushbar-close :value"x")
        (:p :style "margin-left: 40vw" "SESIONES")
        (:form :method :post :style "margin-left: 10vw"
          (:p     ;formulario para escoger BC
            (:select :name "selectlog" :style "margin-bottom: 15px"
             (loop for x from 1 to (length *log*)
              do
                (htm
                  (:option :value x  :selected (eq x selectlog) (print x)))) ))

            ;Muestra archivos de BC
            (when *log* 
              (htm (:p
                (:table :style "padding: 5px;" :border 1 :cellpadding 2 :cellspacing 0
                 (loop for sesion in (reverse *log*)
                   for counter from 1
                   do (htm
                       (:tr (:td :align "right" (str counter))
                        (:td (esc sesion) ) )))))))   
            (:p (:input  :type "submit" :style "margin-top: 10px"  )) ))
          
        (:footer
          (:p "Instituto Politécnico Nacional")) )

      (:script :type "text/javascript" :src "funciones.js") ))))
