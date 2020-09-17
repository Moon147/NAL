;;================================================================================================= 
;;	Funciones para  NAL-REASONER
;;
;;================================================================================================= 

(in-package :nal)

;Función para redondear decimales del Profesor Godoy...
(defun adjust-precision (number precision)  
    (let ((div-part (expt 10 precision)))
        (/ (round (* number div-part)) div-part)))

;;======================================================================================= 
;;  
;;  CÁLCULO VALOR DE VERDAD
;;  
;;=======================================================================================
(defparameter truthv 'nil)
(defparameter term1 'nil)
(defparameter term2 'nil)
(defparameter k 1)
(defparameter intensionA 'nil)
(defparameter extensionA 'nil)
(defparameter intensionB 'nil)
(defparameter extensionB 'nil)
(defvar var-decimales 2)
(defvar var-addexp 'nil)

; Función para calcular intension con parámetros: término a buscar y posición de la expresión a comparar con
; elementos de la cache
(defun intension (term i)
	;aux contiene el termino a comparar con term 
	(let ((aux (first (first (first (obtiene-expresion (list i))))) ) (aux2) )
	;( (((term1 cop term2)(t-v))) (((exp2)(t-v))) (((exp3)(t-v))) )
	(cond
		((or (null term) (= i *cont*)) '()) 
		((eql term  aux) 
			;se obtiene el segundo término de la expresión
			(setf aux2 (third (first (first (obtiene-expresion (list i)))))) 
			;y se agrega a la lista de la intensión llamando recursivamente la
			;función mandando ahora el nuevo término y 1 para encontrar todas las
			; ocurrencias desde la primera posición en cache
			(append (list aux2) (intension aux2 1) (intension term (incf i))) ) 	
		;si term != aux entonces se llama a la función con el mismo término y la siguiente
		;posición en cache																				
		(T (intension term (incf i))) ) ) )

(defun extension (term i )
	(let ((aux (third (first (first (obtiene-expresion (list i))))) )  (aux2) )
	(cond
		((or (null term) (= i *cont*)) '()) 
		((eql term  aux) 
			(setf aux2 (first  (first (first (obtiene-expresion (list i))))))
			(append (list aux2) (extension aux2 1) (extension term (incf i)) ) ) 
		(T (extension term (incf i))) )  ))

;Elimina elementos repetidos de una lista
(defun eliminarRepetidos (lista) 
	(let ((nuevaLista)) 
		(dolist (elemento lista)
			(if (not (member elemento nuevaLista)) 
				(setf nuevaLista (append nuevaLista (list elemento))) ) ) 
	(return-from eliminarRepetidos nuevaLista)))

; Devuelve la expresión de consulta con el valor de verdad calculado "term1 --> term2 <f , c>"
(defun truth-value (query decimales)
	(setf cont-message *cont2*)
	(setq term1 (first query)
	      term2 (third query)) 
	(let ( (w+ '()) (w '()) (confidence 0) (frequency 0)
		(Ai	(eliminarRepetidos (cons term1 (intension term1 1))) )
		(Bi	(eliminarRepetidos (cons term2 (intension term2 1))) )
		(Ae	(eliminarRepetidos (cons term1 (extension term1 1))) ) 
		(Be	(eliminarRepetidos (cons term2 (extension term2 1))) ) )
	;(print Ai) (print Bi) (print Ae) (print Be) 

	;Ingreso de mensaje en caché de manesajes si los término de consulta no se conocían
	(when (and (equal Ai Ae) (= 1 (length Ai))) 
		(insert2 (format nil "El término ~(~a~) es nuevo en esta BC" (first Ai)))  )
	(when (and (equal Bi Be) (= 1 (length Bi)))
		(insert2 (format nil "El término ~(~a~) es nuevo en esta BC" (first Bi)))  )

	(setq intensionA  (format 'nil "Intensión de ~(~a: ~a~) " term1 Ai)
		  extensionA  (format 'nil "Extensión de ~(~a: ~a~) " term1 Ae)
		  intensionB  (format 'nil "Intensión de ~(~a: ~a~) " term2 Bi)
		  extensionB  (format 'nil "Extensión de ~(~a: ~a~) " term2 Be)

		  w+ (length (union (intersection Ae Be) (intersection Ai Bi)))  	; w+ = ||(aE n bE) u (aI n bI)||
		  w  (+ (length Ae) (length Bi)) 							  	 	; w  = ||aE|| + ||bI|| 
		  frequency (float (adjust-precision (/ w+ w) decimales)) 			; frequency = w+ / w
		  confidence (float (adjust-precision (/ w (+ w k)) decimales)) ) 	; confidence = w / (w + k)

	(setq truthv (concatenate 'string (string term1) " --> " (string term2) 
							" <" (format nil "~f" frequency) ", " (format nil "~f" confidence) ">" ))
	(insert2 (format 'nil "El resultado de la consulta es: ~(~a --> ~a~) <~a, ~a>" 
												term1 term2 frequency confidence)) ))

;;======================================================================================= 
;;  
;;  Reglas de inferencia NAL1 locales
;;  
;;=======================================================================================
(defvar statement 'nil)
(defvar expresion 'nil)
(defvar e '())
(defvar e2 '())
(defparameter flag-ingresaBC 'nil)	;Bandera para eliminar las expresiones usadas en las consultas

(defun local-rules-NAL1 (rule exp1 exp2 &optional decimales formula) 
	(labels ((revision (vv1 vv2)       							;regla de inferencia NAL1 local: revisión
    (let ((f1 (first vv1)) (c1 (second vv1))
    	  (f2 (first vv2)) (c2 (second vv2)) (frev 'nil) (crev 'nil))

        (setq frev (float (adjust-precision (/ (+ (* f1 c1 (- 1 c2)) (* f2 c2 (- 1 c1))) (+ (* c1 (- 1 c2)) (* c2 (- 1 c1)))) decimales))
        	  crev (float (adjust-precision (/ (+ (* c1 (- 1 c2)) (* c2 (- 1 c1))) (+ (* c1 (- 1 c2)) (* c2 (- 1 c1)) (* (- 1 c1) (- 1 c2)))) decimales)) )
        (list frev crev))) 

		 (seleccion-confianza (vv1 vv2)       				;regla de inferencia NAL1 local: selección- mayor confianza 
		 	(let ((f1 (first vv1)) (c1 (second vv1))
            (f2 (first vv2)) (c2 (second vv2)) ) 
		 		(list (if (> c1 c2) f1 f2) (if (> c1 c2) c1 c2)) )) 

		 (seleccion-espectativa-ea (term1 term2 term3 term4) ;regla de inferencia NAL1 local: selección- espectativa
		 	(let ((w+ '()) (w '()) (w2+ '()) (w2 '()) 
				(Ai	(eliminarRepetidos (cons term1 (intension term1 1))) )
				(Bi	(eliminarRepetidos (cons term2 (intension term2 1))) )
				(Ae	(eliminarRepetidos (cons term1 (extension term1 1))) ) 
				(Be	(eliminarRepetidos (cons term2 (extension term2 1))) )
				(Ai2	(eliminarRepetidos (cons term3 (intension term3 1))) )
				(Bi2	(eliminarRepetidos (cons term4 (intension term4 1))) )
				(Ae2	(eliminarRepetidos (cons term3 (extension term3 1))) ) 
				(Be2	(eliminarRepetidos (cons term4 (extension term4 1))) ) )

	 		(setq w+ (length (union (intersection Ae Be) (intersection Ai Bi)))  		; w+ = ||(aE n bE) u (aI n bI)||
 				  w  (+ (length Ae) (length Bi))
 				  w2+ (length (union (intersection Ae2 Be2) (intersection Ai2 Bi2)))  	; w+ = ||(aE n bE) u (aI n bI)||
 				  w2  (+ (length Ae2) (length Bi2)) ) 
	 		(setq e (/ (+ w+ (/ k 2)) (+ w k)) 		e2 (/ (+ w2+ (/ k 2)) (+ w2 k))) 	;fórmula de expactativa
	 		
	 		(if (> e e2) 
	 			(setf truthv (second exp1))
	 			(setq exp1 exp2 truthv (second exp2)) ) ))

		 (seleccion-espectativa-vv (freq conf freq2 conf2)
		 	(setq e (+ (* conf (- freq 0.5)) 0.5 ) e2 (+ (* conf2 (- freq2 0.5)) 0.5 )) 
		 		(if (> e e2) 
		 			(setf truthv (second exp1))
		 			(setq exp1 exp2 truthv (second exp2))) )  )

	(cond ((string= (string rule) "REVISIÓN") 
					;(setf statement (list (first exp1) (revision (second exp1) (second exp2)) (third exp1))) 
					(setf truthv (revision (second exp1) (second exp2))) 
					;(parser statement) 
					)
			  ((string= (string rule) "SELECCIÓN")
			  		(cond ((equal (first exp1) (first exp2)) 
			  				(setf truthv (seleccion-confianza (second exp1) (second exp2))))
			  			  ((= formula 0) 
			  			  	(seleccion-espectativa-ea (first (first exp1)) (third (first exp1)) 
  																					(first (first exp2)) (third (first exp2))) )
			  (T
			  	(seleccion-espectativa-vv (first (second exp1)) (second (second exp1)) 
			  							  						(first (second exp2)) (second (second exp2)))) ) )) 

	(setf statement (concatenate 'string (string (first (first exp1))) " --> " (string (third (first exp1))) 
							" <" (format nil "~f" (first truthv)) ", " (format nil "~f" (second truthv)) ">" )) ))

;;======================================================================================= 
;;  
;;  Reglas de inferencia NAL1 forward (hacia adelante)
;;  
;;=======================================================================================

;Macros del profesor Godoy para extended Boolean operators
(defmacro ext-not (x) `(- 1 ,x))
(defmacro ext-and (&rest arguments) `(* ,@arguments))
(defmacro ext-or (&rest arguments) `(- 1 (reduce #'* (mapcar #'(lambda (x) (- 1 x)) ',arguments))) )

(defun forward-rules-N1 (rule exp1 exp2 &optional decimales)
	(labels ((deduction (vv1 vv2)
		(list (float (adjust-precision (ext-and (first vv1) (first vv2)) decimales))
			  (float (adjust-precision (ext-and (first vv1) (first vv2) (second vv1) (second vv2)) decimales))) ) 

	 (induction (vv1 vv2) 
	 	(let ((w+ 'nil) (w 'nil) ) 
	 	 (setq w+ (ext-and (first vv2) (second vv2) (first vv1) (second vv1)) 		;w+ = and(f2 , c2 , f1 , c1 )
	 	 	   w  (ext-and (first vv2) (second vv2) (second vv1)) )	 				;w = and(f2 , c2 , c1 )
	 	 (list (float (adjust-precision (/ w+ w) decimales)) 			
  			   (float (adjust-precision (/ w (+ w k)) decimales)) ) )) 

	 (abduction (vv1 vv2)
	 	(let ((w+ 'nil) (w 'nil) ) 
	 	 (setq w+ (ext-and (first vv1) (second vv1) (first vv2) (second vv2)) 		;w + = and(f1 ,c1 ,f2 ,c2)
	 	 	   w  (ext-and (first vv1) (second vv1) (second vv2)) )	 				;w = and(f1 ,c1 ,c2)
	 	 (list (float (adjust-precision (/ w+ w) decimales)) 			
  			   (float (adjust-precision (/ w (+ w k)) decimales))) ))
	 
	 (conversion (vv1)
	 	 (list (float 1) (float (adjust-precision (/ (* (first vv1) (second vv1)) 
	 	 											(+ (* (first vv1) (second vv1)) k))  decimales)) )) 

	 (exemplification (vv1 vv2)
	 	(let ((w+ 'nil) (w 'nil) ) 
	 	 (setq w+ (ext-and (first vv1) (second vv1) (first vv2) (second vv2)) 		;w + = and(f1 ,c1 ,f2 ,c2)
	 	 	   w  (ext-and (first vv1) (second vv1) (first vv2) (second vv2)) )	 	;w = and(f1 ,c1,f2 ,c2)
	 	 (list (float (adjust-precision (/ w+ w) decimales)) 			
  			   (float (adjust-precision (/ w (+ w k)) decimales))) ) )) 

		(cond ((and (equal (first (first exp1)) (third (first exp2))) (string= (string rule) "DEDUCCIÓN")) 
				 (setq truthv (deduction (second exp1) (second exp2)) 
				 		expresion (list (first (first exp2))  (third (first exp1))) ))

			  ((and (equal (first (first exp1)) (first (first exp2))) (string= (string rule) "INDUCCIÓN")) 
				 (setq truthv (induction (second exp1) (second exp2)) 
				 		expresion (list (third (first exp2))  (third (first exp1))) ))

			  ((and (equal (third (first exp1)) (third (first exp2))) (string= (string rule) "ABDUCCIÓN")) 
			  	(setq truthv (abduction (second exp1) (second exp2)) 
				 		expresion (list (first (first exp2))  (first (first exp1))) ) ) 

			  ((and (equal (third (first exp1)) (first (first exp2)) ) (string= (string rule) "EJEMPLIFICACIÓN")) 
				 (setq truthv (exemplification (second exp1) (second exp2)) 
				 		expresion (list (third (first exp2))  (first (first exp1))) ))

			  ((and (string= rule "CONVERSIÓN") (null exp2))
				 (setq truthv (conversion (second exp1)) 
				 		expresion (list (third (first exp1)) (first (first exp1))) ))

			  (T (insert2 "No es válida la consulta") (setf flag-ingresaBC 'nil))) 

		(setf statement (concatenate 'string (string (first expresion)) " --> " (string (second expresion)) 
							" <" (format nil "~f" (first truthv)) ", " (format nil "~f" (second truthv)) ">" )) ))


(defun inference-rules (solicitud decimales)
	(cond ((and (< (second solicitud) *cont*) (not (numberp (third solicitud))) )
		(let ((exp1 (first (obtiene-expresion (list (second solicitud)))) )) 
			(forward-rules-N1 (first solicitud) exp1 'nil decimales) ))

		 ((and (< (second solicitud) *cont*) (< (third solicitud) *cont*))
		 	
			(let ((exp1 (first (obtiene-expresion (list (second solicitud)))) ) 
			  	  (exp2 (first (obtiene-expresion (list (third  solicitud))))  ))

			(cond ((and (equal (first exp1) (first exp2))
						(not (equal (second exp1) (second exp2))) (not (numberp (fourth solicitud))) ) 
					(local-rules-NAL1 (first solicitud) exp1 exp2 decimales))

				((and (string= (string (first solicitud)) "SELECCIÓN") (numberp (fourth solicitud))) 
					(local-rules-NAL1 (first solicitud) exp1 exp2 nil (fourth solicitud)))

				((not (numberp (fourth solicitud)) )
					(forward-rules-N1 (first solicitud) exp1 exp2 decimales) )

				(T (insert2 "No es válida la consulta") (setf flag-ingresaBC 'nil))))  ))  

	(when flag-ingresaBC 
		(cacle:cache-remove *my-cache* (second solicitud))
		(cacle:cache-remove *my-cache* (third solicitud))
		(parser statement)) 
	)

;;======================================================================================= 
;;  
;;  Consultas tipo ? --> something
;;  
;;=======================================================================================
(defvar subjectOptions)
(defvar predicateOptions)

(defun espectativa-vv (freq conf)
	(setq e (+ (* conf (- freq 0.5)) 0.5 )) )

(defun seleccion-confianza (vv1 vv2)       				;regla de inferencia NAL1 local: selección- mayor confianza 
 	(let ((f1 (first vv1)) (c1 (second vv1))
    (f2 (first vv2)) (c2 (second vv2)) ) 
 		(list (if (> c1 c2) f1 f2) (if (> c1 c2) c1 c2)) )) 

(defun extensionDirecta (term i )
	(let ((aux (third (first (first (obtiene-expresion (list i))))) )  (expresion) )
	(cond
		((or (null term) (= i *cont*)) '()) 
		((eql term  aux) 
			(setf expresion (first (obtiene-expresion (list i))) )
			(append (list (list i expresion)) (extensionDirecta term (incf i)) ) ) 
		(T (extensionDirecta term (incf i))) )  ))


(defun subject? (solicitud)
	(let ((predicate (third solicitud))
		 (subject) (maxEspectativa 0))

		(setf subjectOptions (extensionDirecta predicate 1) )
		(cond ((null subjectOptions)
	
			(setq truthv (concatenate 'string (string predicate) " --> " (string predicate)) 
				subjectOptions (list predicate)) )

			(T
				(loop for expresion in subjectOptions
					do (let ((value (second (second expresion))) (espectativa) )
						(setf espectativa (espectativa-vv (first value) (second value)))
						(if (> espectativa maxEspectativa) 
							(setq subject (third (second expresion)) maxEspectativa espectativa) ) ))
				(setq truthv subject  subjectOptions (mapcar #'(lambda (x) (third (second x))) subjectOptions) )) )
))

;;======================================================================================= 
;;  
;;  Consultas tipo something --> ?
;;  
;;=======================================================================================
(defun intensionDirecta (term i)
	(let ((aux (first (first (first (obtiene-expresion (list i))))) ) (expresion) )
	(cond
		((or (null term) (= i *cont*)) '()) 
		((eql term  aux) 
			(setf expresion (first (obtiene-expresion (list i))) )
			(append (list (list i expresion)) (intensionDirecta term (incf i))) ) 													
		(T (intensionDirecta term (incf i))) ) ) )

(defun predicate? (solicitud)
	(let ((subject (first solicitud))
		(predicate) (maxEspectativa 0))

		(setf predicateOptions (intensionDirecta subject 1) )
		(cond ((null predicateOptions)
	
			(setq truthv (concatenate 'string (string subject) " --> " (string subject)) 
				predicateOptions (list subject)) )

			(T
				(loop for expresion in predicateOptions
					do (let ((value (second (second expresion))) (espectativa) )
						(setf espectativa (espectativa-vv (first value) (second value)))
						(if (> espectativa maxEspectativa) 
							(setq predicate (third (second expresion)) maxEspectativa espectativa) ) ))
				(setq truthv predicate predicateOptions (mapcar #'(lambda (x) (third (second x))) predicateOptions) )) )
	))

;;======================================================================================= 
;;  
;;  Función que recibe la salida del parcer de una consulta tipo:
;;     a --> b?, a --> ? y ? --> b
;;  
;;=======================================================================================
(defun query-NAL1 (query decimales)
	(if (not (null query))
		(cond ((listp (first query)) ; a --> b ?
			      (truth-value (first query) decimales))     ;Se agrega el resultado de la consulta a BC si opcadd fue seleccionado 
			    ((string= (first query) #\?)                ; ? --> b 
			      (subject? query))
			    (T                ; a --> ?)
			    	(predicate? query)) )) )