;;================================================================================================= 
;;	Funciones para NAL-REASONER
;;
;;	ACTUALIZACIÓN:  Jenifer López corrigió los siguientes errores de las pruebas hechas el 17-abril-2020:
;;	(22-abril-2020) 2)	Error en consulta "swimmer --> feathered ?"
;;						 El error se encontraba en el cálculo de la intensión y extensión,
;;						 no se recorría toda la caché con un mismo término a comparar. Se logró
;;						 corregir con ayuda del archivo NXL.cl. Se agregó una llamada más a las
;;						 funciones recursivas de intensión y extensión:
;;								 "(extension|intension term (incf i))"
;;					4)  Error en la actualización automática de archivos
;;						 Al cargar un archivo (subido o seleccionado) se carga en el primer refresh
;;						 de la página, ya no hay necesidad de cargar de nuevo la página para
;;						 actualizar el contenido de los archivos.
;;						 Se modificó la función handle-file y se creó la función manage-files.
;;					6)	Error en pestaña DEBUG
;;						 Se corrigió en el archivo de estilos.css, al regresar a la pestaña
;;						 SALIDA, ya se muestran los mensajes uno debajo del otro.
;;					7)	Indicar cuando un término es nuevo
;;						 Se encuentra en la sección SALIDA, mostrándolo como mensaje.
;;					8)	Tamaño de letra en la sección de consultas
;;						 Se agrandó a 19px, antes 15px.
;;					9)	Actialización en sección SALIDA
;;						 Se muestran los cambios.
;;						 
;;
;;	ACTUALIZACIÓN:  Jenifer López terminó la correción de errores en los archivos:
;;  (17-abril-2020) *)	La nueva ruta en el servidor donde se guardan los archivos subidos es: 
;;							"/home/nalogic/NAL/BC/"
;;					*)	Cuando se sube un archivo, se guarda sin alterarlo en la carpeta BC, se lee 
;;						y se carga el contenido al proyecto (se parsean las expresiones y
;;						se agregan a la caché si son correctas). Una vez cargado en caché, se puede
;;						modificar, agregando más expresiones o consultas y se sobreescribe el 
;;						archivo actual con todo el contenido de la caché en las siguientes acciones:
;;							-Cuando se carga un nuevo archivo.
;;							-Cuando se selecciona un archivo existente.
;;						Ocurre lo mismo cuando se selecciona un archivo, se carga en memoria y se
;;						sobreescribe con el contenido en caché bajo las mismas acciones.
;;					*)	Cuando se agregan expresiones al proyecto manualmente antes de que se 
;;						suba algún archivo, se genera un archivo en automático: "BC0" que almacena
;;						todas estas expresiones.
;;
;;	ACTUALIZACIÓN:  Jenifer López adaptó el proyecto a la nueva estructura entregada
;;  (10-abril-2020) ("canario --> ave" (1.0 0.9))
;;					antes: (("canario" "-->" "ave") (1.0 0.9)) 
;;					cambiando las funciones para trabajarlas todas bajo el uso de strings y
;;					la estructura de almacenamiento de las expresiones en la caché:
;;					(("term" "cop" "term") (truth-value) ("expresion"  "truth-value"))
;;					usando la función "convierte" para pasar de "term cop term" a 
;;					("term" "cop" "term").
;;
;;	ACTUALIZACIÓN:  Jenifer López agregó las funciones para cálculo de valor de verdad: intensión
;;  (8-abril-2020)	extensión y calculo de w+, w para obtener la frecuencia y confianza.
;;
;;	ACTUALIZACIÓN:  Montserrat González
;;					Implementación de la nueva estructura de almacenamiento: caché
;;					
;;			
;;================================================================================================= 

(in-package :nal)

;;======================================================================================= 
;;  
;;  Caché para las expresiones válidas, analizadas con el parser
;;  
;;=======================================================================================

(defparameter *cont* 1)
(defparameter *exprcon* nil)

(defun proveedor1 (key)
	(values *exprcon*
            key))

(defparameter *my-cache* (cacle:make-cache 10000000 #'proveedor1 :policy :lru))

(defun insert(knowledge)
	(setf *exprcon* knowledge)
	(cacle:cache-fetch *my-cache* *cont*)
	(incf *cont*))

(defun obtiene-expresion(id)
	(mapcar #'(lambda (key)
              (cacle:cache-fetch *my-cache* key :only-if-cached t))
          id))

;;======================================================================================= 
;;  
;;  Caché para los mensajes de error
;;  
;;=======================================================================================

(defparameter *cont2* 1)
(defparameter *exprerr* nil)
(defparameter cont-message 1)

(defun proveedor2 (key)
	(values *exprerr*
            key))

(defparameter *mensajes-cache* (cacle:make-cache 10000000 #'proveedor2 :policy :lru))

(defun insert2(message)
	(setf *exprerr* message)
	(cacle:cache-fetch *mensajes-cache* *cont2*)
	(incf *cont2*))

(defun obtiene-mensaje(id)
	(mapcar #'(lambda (key)
              (cacle:cache-fetch *mensajes-cache* key :only-if-cached t))
          id))

;;======================================================================================= 
;;  
;;  Reiniciar cachés
;;  
;;=======================================================================================

(defun initialize-cache()
	(setf *cont* 1)
	(setf *exprcon* nil)
	(setf *cont2* 1)
	(setf *exprerr* nil)
	(setf *my-cache* (cacle:make-cache 10000000 #'proveedor1 :policy :lru))
	(setf *mensajes-cache* (cacle:make-cache 10000000 #'proveedor2 :policy :lru)))


;;======================================================================================= 
;;  
;;  Convierte un string a una lista con los elementos del string
;;  
;;=======================================================================================
(defun convierte (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

;;======================================================================================= 
;;  
;;  Función para usar el parser NAL-parser.lisp y filtrar
;;  
;;=======================================================================================
(defvar auxiliar2 '())
(defun parser (aux)  ;------------------Mandar expresión para parsearla
	(setf auxiliar2 (parseq 'judgement aux))
	(cond 
	  ((not (equal auxiliar2 'NIL)) ;----------Si la expresión es correcta:
	; Si la expresión no tiene valor de verdad se le asigna por default (1.0, 0.9)
	  (if (null (second auxiliar2)) (setf auxiliar2 (list (first auxiliar2) '(1.0 0.9)) ))
	    ;-------Agrega la estructura de la expresión a la cache de BC
	    ; Se agrega una lista con 3 elementos (("term" "cop" "term2") (vv) ("expresion"  "vv"))
	    (insert (list (convierte (first auxiliar2)) (second auxiliar2)  
	       (list (first auxiliar2) (format nil " <~{~a~^, ~}>" (second auxiliar2))) )) 
	    (insert2  (format nil "La expresión: ~a ha sido añadida" aux ) ) );------Agregar la expresión a la cache de mensajes
	  ((and (equal auxiliar2 'NIL) (not (null aux)) ); Si la expresión está mal:
	    (insert2 (format nil "Error en ~a" aux)) )  )) ;Agregar como mensaje de error a la cache de errores



;;======================================================================================= 
;;  
;;  Concatenar cadenas
;;  
;;=======================================================================================
(defun concatena (lista)
	(concatenate 'string (first lista) (second lista)))

;;======================================================================================= 
;;  
;;  CÁLCULO VALOR DE VERDAD
;;  
;;=======================================================================================

(defparameter aux 'nil)
(defparameter aux2 'nil)
(defparameter term1 'nil)
(defparameter term2 'nil)
(defparameter k 1)
(defparameter expresion '())

; Función para calcular intension con parámetros: término a buscar y posición de la expresión a comparar con
; elementos de la cache
(defun intension (term i)
	(setf aux (first (first (first (obtiene-expresion (list i))))) ) ; aux contiene el termino a comparar con term 
			; ( (((term1 cop term2)(t-v))) (((exp2)(t-v))) (((exp3)(t-v))) )
	(cond
		((or (null term) (= i *cont*)) '()) 
		((string= term  aux) 
			(setf aux2 (third (first (first (obtiene-expresion (list i)))))) ; se obtiene el segundo término de la expresión
			(append (list aux2) (intension aux2 1) (intension term (incf i))) ) 	;y se agrega a la lista de la intensión llamando recursivamente la
														;función mandando ahora el nuevo término y 1 para encontrar todas las
														; ocurrencias desde la primera posición en cache
		(T (intension term (incf i))) ) ) ;si term != aux entonces se llama a la función con el mismo término y la siguiente
											;posición en cache

(defun extension (term i )
	(setf aux (third (first (first (obtiene-expresion (list i))))) )
	(cond
		((or (null term) (= i *cont*)) '()) 
		((string= term  aux) 
			(setf aux2 (first  (first (first (obtiene-expresion (list i))))))
			(append (list aux2) (extension aux2 1) (extension term (incf i)) ) ) 
		(T (extension term (incf i))) ) 
	)


; Entrega la lista de la intersección de A con B
(defun intersec (A B)
	(cond ((> (length B) (length A))
	(loop for x in A
		if  (loop for y in B
				if (string= x y ) return T) 
		collect x))
		(T 
	(loop for x in B
		if  (loop for y in A
				if (string= x y ) return T) 
		collect x))) )

; Entrega la lista de la unión de A con B
(defun union-new (A B)
	(cond ((null A) B)
		  ((null B) A)
		  ((> (length A)  (length B))
	(append (loop for x in A
				if  (loop for y in B
						if (not (string= x y )) return T) 
				collect x)
	    B)) 
		  (T (append (loop for x in B
				if  (loop for y in A
						if (not (string= x y )) return T) 
				collect x)
	    A))) )

; Devuelve la expresión de consulta con el valor de verdad calculado "term1 --> term2 <f , c>"
(defun truth-value (query)
	(setq expresion (convierte query))
	
	(setq term1 (first expresion)
	      term2 (third expresion)) 
	(let ( (w+ '()) (w '()) (confidence '()) (frequency '())
		(Ai	(cons term1 (intension term1 1)) )
		(Bi	(cons term2 (intension term2 1)) )
		(Ae	(cons term1 (extension term1 1)) ) 
		(Be	(cons term2 (extension term2 1)) ) )
	;(print Ai) (print Bi) (print Ae) (print Be) 
	;(print (union-new (intersec Ae Be) (intersec Ai Bi)) )
	(when (and (equal Ai Ae) (= 1 (length Ai))) 
		(insert2 (format nil "El término ~a es nuevo en esta BC" (first Ai))) 
		(decf cont-message ))
	(when (and (equal Bi Be) (= 1 (length Bi)))
		(insert2 (format nil "El término ~a es nuevo en esta BC" (first Bi))) 
		(decf cont-message ))

	(setf w+ (length (union-new (intersec Ae Be) (intersec Ai Bi))) ) ; w+ = ||(aE n bE) u (aI n bI)||
	(setf w  (+ (length Ae) (length Bi))) 							  ; w  = ||aE|| + ||bI|| 
	(setf frequency (format nil "~,2f" (/ w+ w)) )					  ; frequency = w+ / w
	(setf confidence (format nil "~,2f" (/ w (+ w k)))  )			  ; confidence = w / (w + k)
	(concatenate 'string term1 " --> " term2 " <" frequency ", " confidence ">")) ) 


;;======================================================================================= 
;;  
;;  Envío de archivos
;;  
;;=======================================================================================
(defparameter flag-firstFile T)
(defparameter flag-files 2)
(defparameter flag-reset T)
(defparameter flag-selectbc '())
(defparameter flag-BC0 '())
(defvar path-selectbc '())
(defvar path '())
(defparameter var-selectbc '())
(defvar *files* nil)

(defvar *directory*
    #+(or :win32 :mswindows) #p"c:\\NAL-Reasoner\\"
    #-(or :win32 :mswindows) #p"/home/nalogic/NAL/BC/")

;------Escribe en el archivo con ruta path el contenido de la cache de expresiones
(defun writefile (path)
	(with-open-file  (stream  path :direction :output :if-exists :supersede)
      (loop for i from 1 to (- *cont* 1)
       do 
      (write-line (concatena (third (first (obtiene-expresion (list i)) ))) stream) )) )

(defun manage-files ()
  (when (and flag-firstFile (= (length *files*) 1)) 
    (setq flag-firstFile '())
    (cond ((> *cont* 1) 
      (setq path "BC/BC-0")
      (writefile path)
      (setq *files* (list (first *files*) 
        (list #P"/home/nalogic/NAL/BC/BC-0" "BC-0.txt" "text/plain") )
        flag-BC0 'T)
      (incf flag-files))) )

  (when (or (= (length *files*) flag-files) (numberp var-selectbc))
    (if flag-selectbc (writefile path-selectbc) (writefile path))
    (cond ((numberp var-selectbc)
            (setq path-selectbc (format nil "BC/BC-~A" (if flag-BC0 (- var-selectbc 1) var-selectbc))
                 flag-reset 'T flag-selectbc 'T)  ) 
          (T (setq path (first (second *files*) )
                   flag-selectbc '())
              (incf flag-files) 
              (writefile path)
              (setq cont-message 1))) )

  (when flag-reset
    (setf flag-reset '())
    (initialize-cache) ;----- reiniciar la bc de expresiones y mensajes de error antes de evaluar el nuevo archivo
    (if (numberp var-selectbc)
      (setq path  path-selectbc)              ;--- obtiene archivo seleccionado por usuario
      (setq path (first (first *files*) )) )  ;--- obtiene el último archivo subido
    ;(print (first *files*))
    (with-open-file (in path)
      (loop for line = (read-line in nil) ;------LEE ARCHIVO
           while line do (parser line)) )  ) ;parsea cada expresión 

  (setf var-selectbc '())) 


(let ((counter 0))
  (defun handle-file (post-parameter)
  	(setf flag-reset 'T)
    (when (and post-parameter
               (listp post-parameter))
      (destructuring-bind (path file-name content-type)
          post-parameter
        (let ((new-path (make-pathname :name (format nil "BC-~A"
                                                     (incf counter))
                                       :type nil
                                       :defaults *directory*)))
          ;; strip directory info sent by Windows browsers
          (when (search "Windows" (user-agent) :test 'char-equal)
            (setq file-name (cl-ppcre:regex-replace ".*\\\\" file-name "")))
          (rename-file path (ensure-directories-exist new-path))
          (push (list new-path file-name content-type) *files*)))) 
    (manage-files)) )

(defun clean-tmp-dir2 ()
  (loop for (path . nil) in *files*
        when (probe-file path)
        do (ignore-errors (delete-file path)))
  (setq *files* nil))

