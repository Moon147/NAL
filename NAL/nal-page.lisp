(in-package :nal)

(define-easy-handler (index :uri "/NAL-Reasoner/index.html"
                                :default-request-type :post)

      (conocimiento valorV relacion expresion (selectbc :parameter-type 'integer)
        comportamiento (decimales :parameter-type 'integer)
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

  (with-html
    (:html
     (:head 
      (:title "NAL-Reasoner")
      (:link :rel "stylesheet" :type "text/css" :href "pushbar.css")
      (:link :rel "stylesheet" :type "text/css" :href "estilo.css")
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
    ;------------------------------------------- 
     (:body
      (:div :id "contenedor"
        (:header
          (:table :style "width:100%"
          (:tr
            (:th :id "logoIPN" :style "width:100px;vertical-align:top"
              (:img :src "logo_ipn3.png" :height "90"))
            (:th :id "titulo" :style "text-align:center;vertical-align:top; padding: 3px"
              (:h1 "NAL-Reasoner"))
            (:th :id "institucion" :style "text-align:right;width:365px;vertical-align:top;padding:10px"
              (:h3 "Centro de Investigación en Computación")
              (:h3 "Laboratorio de Inteligencia Artificial"))
            (:th :id "logoCIC" :style "text-align:right;width:100px;vertical-align:top;padding: 8px; padding-top: 0px"
              (:img :src "logo_cic3.png" :height "75")))))
        (:aside
          (:ul :class "tabs1"
            (:li :class "tabs__item active" :onclick "openTab(event,'TABLA')"
              (:h4 "BC"))
            (:li :class "tabs__item" :onclick "openTab(event,'BC');"
              (:h4 "TABLA")))

          (:div :id "aside"
            (:span :class "c1" "Base de Conocimiento")
            (:table :id "TABLA" :class "tabcontent active"
              (:tr 
                (:th :id "num" "No")
                (:th :id "exp" "Expresión")
                (:th :id "vv" "Valor de verdad"))
              
              (cond ((search "?" conocimiento) 
                      (truth-value (parseq:parseq 'query conocimiento) var-decimales) 
                      (if opcadd (parser  truthv))        ;Se agrega el resultado de la consulta a BC si opcadd fue seleccionado 
                      (setq truthv 'nil opcadd 'nil))     ;Se reinician las variables
                (T (parser conocimiento) ))       

                (loop for i from 1 to (- *cont* 1)
                 do 
                  (setf expresion (first (obtiene-expresion (list i))))
                  (setf valorV (second expresion))
                  (setf relacion (first (third expresion)))
                    (htm
                     (:tr 
                      (:td (print i))
                      (:td (print relacion))
                      (:td (print valorV))))) )
            (:div :id "BC" :class "tabcontent")))
        (:section :id "contenido"
          (:ul :class "tabs2"
            (:li :class "tabs__item2 active" :onclick "openTab2(event,'informacion')"
              (:h4 "OUTPUT"))
            (:li :class "tabs__item2" :onclick "openTab2(event,'DEBUG')"
              (:h4 "DEBUG")))
          (:div :class "tabcontent2 active" :id "informacion"
            (loop for i from cont-message to (- *cont2* 1) 
                   do 
                   (setf expresion (obtiene-mensaje (list i)))
                   (htm
                      (:p :class "parrafo-salida" "  "(print i) 
                         (print (first expresion)) )) ))

          (:div :class "tabcontent2" :id "DEBUG"
            (when intensionA
              (htm
                (:p :class "parrafo-salida" "  " (print intensionA) )
                (:p :class "parrafo-salida" "  " (print extensionA) )
                (:p :class "parrafo-salida" "  " (print intensionB) )
                (:p :class "parrafo-salida" "  " (print extensionB)) )
              (setq intensionA '() intensionB '() extensionA '() extensionB '())) )
          ;(print(parseq 'judgement conocimiento))
          (:button :class "simbolo" :onclick "simbolo('-->')" :style "margin-left: 10px" "-->")
          (:button :onclick "simbolo('<=>')"  :class "simbolo" "<=>")
          (:button :onclick "simbolo('o->')"  :class "simbolo" "o->")
          (:button :onclick "simbolo('->o')"  :class "simbolo" "->o")
          (:button :onclick "simbolo('o->o')"  :class "simbolo" "o->o")
          (:button :onclick "simbolo('==>')"  :class "simbolo" "==>")
          (:button :onclick "simbolo('--')"  :class "simbolo" "--")
          (:button :onclick "simbolo('||')"  :class "simbolo" "||")
          (:button :onclick "simbolo('&&')"  :class "simbolo" "&&")
          (:button :onclick "simbolo('+')"  :class "simbolo" "+")
          (:p (:form :method :post 
              (htm  
                (:input :type "checkbox"
                 :name "opcadd"
                 :value "agregar"
                 :checked (string= "agregar" opcadd)
                 (print "Agregar consulta a BC"))
                (:br)
                (:input :type :text :class "conocimiento" :id "conocimiento"  :name "conocimiento"  
                  :placeholder "Estructura: perro --> animal" ))
              (:input :class "botonSubir" :type "submit"  ))) )

        (:div :class "pc" :data-pushbar-target "mypushbar1"
          (:i :class "fas fa-angle-double-left"))
        (:section :data-pushbar-id "mypushbar1" :class "pushbar from_right"
          (:button :class "cerrar" :data-pushbar-close :value"x")
          (:button :class "regresar" :onclick "back()" (:p "<"))
          (:section :class "menu"
            (:div :class "submenu" :onclick "display_pushbar('.politica')"  (:h5 "Política de Control"))
            (:div :class "submenu" :onclick "display_pushbar('.selecciona-BC')"  (:h5 "Seleccionar Base de Conocimiento"))
            (:div :class "submenu" :onclick "display_pushbar('.sube-BC')"  (:h5 "Subir Base de Conocimiento")))

          ;;---------------------------------------FORMULARIO POLIÍTICA DE CONTROL------------------------------
          (:section :class "politica sub-menu"
            (:span :class "c2" "Política de Control")
        (print decimales)
          (:form :method :post :style "margin-left: 15px"
          (:legend "Valores de verdad") :br
          (:p "Decimales: "
            (:select :name "comportamiento"
             (loop for (value option) in '((:redondear "Redondear")
                                           (:truncar "Truncar"))
                   do (htm
                       (:option :value value
                        :selected (eq value comportamiento)
                        (str option)))) )) :br
          (:p "Cantidad de decimales: " :br :t 
            (:input 
              :name "decimales"
              :value (or decimales 2))) :br
          (:p 
            (:input :type "submit" :class "submit" :value "Enviar datos")
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


            ;---------------------------------------SELECCIONAR BASE DE CONOCIMIENTO-----------------------------------------
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
                             :checked (string= "agregar" opcjoin)
                             (print "Unir bases de conocimiento")) )

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

        );/section data-pushbar-id 
          
        (:footer
          (:p "Instituto Politécnico Nacional. Centro de Investigación en Computación.")) )

      (:script :type "text/javascript" :src "funciones.js") ))))
