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
			(setf aux2 (fifth (first (first (obtiene-expresion (list i)))))) ; se obtiene el segundo término de la expresión
			(append (list aux2) (intension aux2 1)) ) 	;y se agrega a la lista de la intensión llamando recursivamente la
														;función mandando ahora el nuevo término y 1 para encontrar todas las
														; ocurrencias desde la primera posición en cache
		(T (intension term (incf i))) ) ) ;si term != aux entonces se llama a la función con el mismo término y la siguiente
											;posición en cache

(defun extension (term i)
	(setf aux (fifth (first (first (obtiene-expresion (list i))))) )
	(cond
		((or (null term) (= i *cont*)) '()) 
		((string= term  aux) 
			(setf aux2 (first  (first (first (obtiene-expresion (list i))))))
			(append (list aux2) (extension aux2 1)) ) 
		(T (extension term (incf i))) ) )

; Entrega la lista de la intersección de A con B
(defun intersec (A B)
	(loop for x in A
		if  (loop for y in B
				if (string= x y ) return T) 
		collect x)) 

; Entrega la lista de la unión de A con B
(defun union-new (A B)
	(append (loop for x in A
				if  (loop for y in B
						if (not (string= x y )) return T) 
				collect x)
	    B)) 

(defun convierte (string)
    (loop for i = 0 then (1+ j)
          as j = (position #\Space string :start i)
          collect (subseq string i j)
          while j))

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

(defvar *files* nil)

(defvar *directory*
    #+(or :win32 :mswindows) #p"c:\\NAL-Reasoner\\"
    #-(or :win32 :mswindows) #p"/home/nalogic/NAL/BC/")


(let ((counter 0))
  (defun handle-file (post-parameter)
  	(setf flag-reset 'T)
  	(print counter)
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
          (push (list new-path file-name content-type) *files*))))))

(defun clean-tmp-dir2 ()
  (loop for (path . nil) in *files*
        when (probe-file path)
        do (ignore-errors (delete-file path)))
  (setq *files* nil))

(defun writefile (path)
	(with-open-file  (stream  path :direction :output :if-exists :supersede)
      (loop for i from 1 to (- *cont* 1)
       do 
      (write-line (concatena (third (first (obtiene-expresion (list i)) ))) stream) )) )
	
