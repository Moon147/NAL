;;================================================================================================= 
;;	Funciones para comunicación de NAL-REASONER
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
;;					Las respuestas de las consultas ahora se muestran en la sección SALIDA
;;					La sección salida ahora se llama OUTPUT
;;					En la sección DEBUG ahora se muestran las intensiones y extensiones de las consultas
;;					Adaptación del proyecto con la última versión del parser NAL-parser-17-abril-2020.cl
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
;;					Creación de toda la estructura del servidor
;;					Montse pon aquí tu parte jejeje
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

(defparameter *my-cache* (cacle:make-cache 1000 #'proveedor1 :policy :lru))

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
;;  Caché para agente
;;  
;;=======================================================================================

(defparameter *cont4* 1)
(defparameter *expragente* nil)

(defun proveedor4 (key)
  (values *expragente*
            key))

(defparameter *mensajes-agente* (cacle:make-cache 1000 #'proveedor4 :policy :lru))

(defun insert4 (agente)
  (setf *expragente* agente)
  (cacle:cache-fetch *mensajes-agente* *cont4*)
  (incf *cont4*))

(defun obtiene-agente(id)
  (mapcar #'(lambda (key)
              (cacle:cache-fetch *mensajes-agente* key :only-if-cached t)) id))

;;======================================================================================= 
;;  
;;  Caché para los mensajes de error
;;  
;;=======================================================================================

(defparameter *cont2* 1)
(defparameter *exprerr* nil)

(defun proveedor2 (key)
	(values *exprerr*
            key))

(defparameter *mensajes-cache* (cacle:make-cache 1000 #'proveedor2 :policy :lru))

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
;;  Caché para los mensajes en DEBUG
;;  
;;=======================================================================================

(defparameter *cont3* 1)
(defparameter *exprdebug* nil)

(defun proveedor3 (key)
  (values *exprdebug*
            key))

(defparameter *mensajes-debug* (cacle:make-cache 1000 #'proveedor3 :policy :lru))

(defun insert3 (debug)
  (setf *exprdebug* debug)
  (cacle:cache-fetch *mensajes-debug* *cont3*)
  (incf *cont3*))

(defun obtiene-debug(id)
  (mapcar #'(lambda (key)
              (cacle:cache-fetch *mensajes-debug* key :only-if-cached t))
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
  (setf *exprdebug* nil)
  (setf *cont3* 1)  
  (setf *cont4* 1)
	(setf *my-cache* (cacle:make-cache 10000 #'proveedor1 :policy :lru))
  (setf *mensajes-agente* (cacle:make-cache 10000 #'proveedor4 :policy :lru)) 
	(setf *mensajes-cache* (cacle:make-cache 10000 #'proveedor2 :policy :lru))
  (setf *mensajes-debug* (cacle:make-cache 10000 #'proveedor3 :policy :lru)) )


;;======================================================================================= 
;;  
;;  Función para usar el parser NAL-parser.lisp y filtrar
;;  
;;=======================================================================================
(defvar auxiliar2 '())
(defvar confidenceCero 0.5)
(defvar tv 0)
(defvar contPassParser 0)
(defvar expresionLista 'nil)

(defun parser (aux)  						
	(setf auxiliar2 (parseq:parseq 'judgement aux))
        
	(cond 
	  ( auxiliar2 	;Si la expresión es correcta:
					;Si la expresión no tiene valor de verdad se le asigna por default (1, 0.9)
	  	(if (null (second auxiliar2)) (setf auxiliar2 (list (first auxiliar2) '(1.0 0.9)) ))
	    ;Agrega la estructura de la expresión a la cache de BC
	    ;Se agrega una lista con 3 elementos ((term cop term2) (vv) ("expresion"  "vv"))
      (setf tv (second auxiliar2))      ;tv valor de verdad (frequency confidence)
	    (setf expresionLista (list (first auxiliar2) 
                    (if (= 0 (second tv)) (list (first tv) confidenceCero) tv)  ;Si la confianza es 0, se asigna 1/2 de confianza
	       (list (format nil "~(~a ~a ~a~)" (first (first auxiliar2)) (second (first auxiliar2)) (third (first auxiliar2)))
	       		 (if (= 0 (second tv)) 
                (format nil " <~{~a~^, ~}>" (list (first tv) confidenceCero))
                (format nil " <~{~a~^, ~}>" (second auxiliar2)) )) ))  
      (insert expresionLista)
      (insert4 expresionLista)
      ;(format nil " <~{~a~^, ~}>" (list (first tv) confidenceCero))
      ;Agregar a variable contPassParser los que fueron agregados a la caché
      (incf contPassParser) )
	  ((and (equal auxiliar2 'NIL) (not (null aux)) )  ;Si la expresión está mal...
	    (insert2 (format nil "Error en ~(~a~). Revise la estructura de su consulta." aux)) )  )) ;Agregar como mensaje de error a la cache de errores



;;======================================================================================= 
;;  
;;  Concatenar cadenas
;;  
;;=======================================================================================
(defun concatena (lista)
	(concatenate 'string (first lista) (second lista))) 


;;======================================================================================= 
;;  
;;  Envío de archivos
;;  
;;=======================================================================================
(defparameter flag-firstFile T)  ;Bandera que indica si ya se ingresó el primer archivo
(defparameter flag-files 2)		 ;Bandera usada para sobreescribir los archivos
(defparameter flag-reset T)		 ;Bandera que indica cuando se reinicializan las caché
(defparameter flag-selectbc '()) ;Bandera que indica cuando se ha seleccionado una BC
(defparameter flag-BC0 '())		 ;Bandera que indica que se creó el archivo BC0
(defvar path-selectbc '())		 ;Ruta de la BC seleccionada por el usuario
(defvar opcjoin-selectbc '())	 ;Bandera de opción para unir nueva BC seleccionada con la actual
(defvar path '())				 ;Ruta del archivo que se va a modificar
(defparameter var-selectbc '())  ;Contiene el número del archivo seleccionado por usuario
(defvar *files* nil)			 ;Variable donde se guardan todas las rutas y nombres de los archivos

(defvar logfecha (multiple-value-bind 
		(segundos minutos horas dia mes año) 
		(get-decoded-time) 
	 	(format 'nil "~d-~2,'0d-~d_~2,'0d:~2,'0d:~2,'0d" 
	 												año mes dia horas minutos segundos )))
(defvar *directory*
  (pathname 
	(format 'nil "/home/jenifer/Escritorio/GITNAL/NAL/Sesiones/~a/" logfecha)))

(defparameter *log* 'nil) 		;Variable para guardar las sesiones 

;Escribe en el archivo con ruta path el contenido de la cache de expresiones
(defun writefile (path)
	(with-open-file  (stream  path :direction :output :if-exists :supersede)
      (loop for i from 1 to (- *cont* 1)
       do 
      (write-line (concatena (third (first (obtiene-expresion (list i)) ))) stream) )) )

(defun manage-files ()
	;Manejo de expresiones cuando se suba el primer archivo
  (when (and flag-firstFile (= (length *files*) 1)) 
  	(ensure-directories-exist *directory*)
    (setq flag-firstFile '())
    (cond ((> *cont* 1) 
      (setq path (format 'nil "Sesiones/~a/BC-0" logfecha))
      (writefile path)
      (setq *files* (list (first *files*) 
        (list (pathname (format 'nil "home/jenifer/Escritorio/GITNAL/NAL/Sesiones/~a/BC-0" logfecha))
        	"BC-0.txt" "text/plain") )
        flag-BC0 'T)
      (incf flag-files))) )

  ;Manejo para integrar BC a otra
   (when (and opcjoin-selectbc (numberp var-selectbc))				
		(setq path-selectbc (format nil "Sesiones/~a/BC-~A" logfecha (if flag-BC0 (- var-selectbc 1) var-selectbc))
			flag-reset 'nil opcjoin-selectbc 'nil var-selectbc 'nil)
		(if (not (equal path path-selectbc))
			(with-open-file (in path-selectbc)
		      (loop for line = (read-line in nil) 	  ;LEE ARCHIVO
		           while line do (parser line))) 
			(insert2 "No se puede agregar el mismo archivo")) )

  	;Manejo de los archivos existentes
  (when (or (= (length *files*) flag-files) (numberp var-selectbc))
    (writefile path)
    		;Cuando el usuario ha selccionado un archivo existente
    (cond ((numberp var-selectbc)
            (setq path-selectbc (format nil "Sesiones/~a/BC-~A" logfecha (if flag-BC0 (- var-selectbc 1) var-selectbc))
                 flag-reset 'T flag-selectbc 'T )  ) 
    		;Cuando se ha subido uno nuevo
          (T (setq path (first (second *files*) )
                   flag-selectbc '())
              (incf flag-files) )) )

  	;Pasos para sobreescribir archivos y leer nuevos 
  (when flag-reset
    (setf flag-reset '())
    (initialize-cache) 			;reiniciar la bc de expresiones y mensajes de error antes de evaluar el nuevo archivo
    (if (numberp var-selectbc)
      (setq path  path-selectbc)              ;obtiene archivo seleccionado por usuario
      (setq path (first (first *files*) )) )  ;obtiene el último archivo subido
    ;(print (first *files*))
    (with-open-file (in path)
      (loop for line = (read-line in nil) 	  ;LEE ARCHIVO
           while line do (parser line)) )  )  ;parsea cada expresión 

  (setf var-selectbc '())) 


(let ((counter 0))
  (defun handle-file (post-parameter)
  	(ensure-directories-exist *directory*)
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

