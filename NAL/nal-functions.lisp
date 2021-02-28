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
(defparameter positiveEvidence 'nil)
(defparameter negativeEvidence 'nil)
(defparameter evidence 'nil)
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

;Calcula intensión y extensión
(defun I-E (term1 term2)
	(let (
		(Ai	(eliminarRepetidos (cons term1 (intension term1 1))) )
		(Bi	(eliminarRepetidos (cons term2 (intension term2 1))) )
		(Ae	(eliminarRepetidos (cons term1 (extension term1 1))) ) 
		(Be	(eliminarRepetidos (cons term2 (extension term2 1))) ) 
		(wp 'nil ) 
		(w 'nil ) )

		(setq wp (length (union (intersection Ae Be) (intersection Ai Bi)))
					w (+ (length Ae) (length Bi)) )
		(insert3   (format 'nil "Intensión de ~(~a: ~a~) ~% Extensión de ~(~a: ~a~) ~% Intensión de ~(~a: ~a~) ~% Extensión de ~(~a: ~a~) ~% Evidencia positiva: ~(~a~) ~% Evidencia negativa: ~(~a~) ~% Evidencia total: ~(~a~)" 
			term1 Ai term1 Ae term2 Bi term2 Be wp (- w wp) w 
			
			))
		))

; Devuelve la expresión de consulta con el valor de verdad calculado "term1 --> term2 <f , c>"
(defun truth-value (query decimales)
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

		(I-E term1 term2)

	(setq 
			w+ (length (union (intersection Ae Be) (intersection Ai Bi)))  	; w+ = ||(aE n bE) u (aI n bI)||
		  w  (+ (length Ae) (length Bi)) 							  	 	; w  = ||aE|| + ||bI|| 
		  frequency (float (adjust-precision (/ w+ w) decimales)) 			; frequency = w+ / w
		  confidence (float (adjust-precision (/ w (+ w k)) decimales)) ) 	; confidence = w / (w + k)

	(if (equal term1 term2) 
		(setq frequency 1.0 confidence 0.9) )

	(setq truthv (concatenate 'string (string term1) " --> " (string term2) 
							" <" (format nil "~f" frequency) ", " (format nil "~f" confidence) ">" ))
	(insert2 (format 'nil "~(~a --> ~a~) <~a, ~a>" 
												(string-downcase term1) (string-downcase term2) frequency confidence)) ))	

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
(defvar errorSintax-local-n1 'nil)

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
				(Be2	(eliminarRepetidos (cons term4 (extension term4 1))) )  )

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

	(cond ( (and (equal (first exp1) (first exp2))
						(not (equal (second exp1) (second exp2))) )

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
				)
	
				(T (insert2 (format 'nil "Error reglas NAL-1. Revise el número de las expresiones que desea usar asi como la estructura para la regla de inferencia ~a con estructura en las premisas: No-exp1: (M --> P) <f1,c1> No-exp2: (M --> P) <f2,c2>"  
					rule )) (setf errorSintax-local-n1 'T)) ) 	

	(I-E (first (first exp1)) (third (first exp1)))
	(setf statement (concatenate 'string (string-downcase (string (first (first exp1)))) " --> " (string-downcase (string (third (first exp1)))) 
							" <" (format nil "~f" (first truthv)) ", " (format nil "~f" (second truthv)) ">" )) 

	(if (and statement (not (search "NIL" statement) ) (not errorSintax-local-n1))  (insert2 statement)) 
	(setq e 'nil e2 'nil errorSintax-local-n1 'nil) ))

;;======================================================================================= 
;;  
;;  Reglas de inferencia NAL1 forward (hacia adelante)
;;  
;;=======================================================================================

;Macros del profesor Godoy para extended Boolean operators
(defmacro ext-not (x) `(- 1 ,x))
(defmacro ext-and (&rest arguments) `(* ,@arguments))
(defun ext-or (&rest arguments) (- 1 (reduce #'* (mapcar #'(lambda (x) (- 1 x)) arguments))) )

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

  (let ((termA1 (first (first exp1)))
  			(termA2 (third (first exp1)))
  			(termB1 (first (first exp2)))
  			(termB2 (third (first exp2))) 
				(ruleSintax 'nil) (errorSintax 'nil) ) 

		(cond ((string= (string rule) "DEDUCCIÓN")
					(setf ruleSintax "(deducción No-exp1: (M --> P) No-exp2: (S --> M))")
					(cond 
						((equal termA1 termB2)
							(setq truthv (deduction (second exp1) (second exp2)) 
				 			expresion (list termB1  termA2)))
						((equal termB1 termA2) 
				 			(setq truthv (deduction (second exp2) (second exp1)) 
				 			expresion (list termA1  termB2 )))
						(T (setf errorSintax 'T)) ))

			  ((string= (string rule) "INDUCCIÓN")
					(setf ruleSintax "(inducción No-exp1: (M --> P) No-exp2: (M --> S))")
					(cond 
						((equal termA1 termB1)
							(setq truthv (induction (second exp1) (second exp2)) 
				 			expresion (list termA2  termB2)))
						(T (setf errorSintax 'T)) ))

			  ((string= (string rule) "ABDUCCIÓN")
					(setf ruleSintax "(abducción No-exp1: (P --> M) No-exp2: (S --> M))")
					(cond 
						((equal termA2 termB2)
							(setq truthv (abduction (second exp1) (second exp2)) 
				 			expresion (list termA1  termB1)))
						(T (setf errorSintax 'T)) )) 

			  ((string= (string rule) "EJEMPLIFICACIÓN")
					(setf ruleSintax "(ejemplificación No-exp1: (M --> P) No-exp2: (S --> M))")
					(cond 
						((equal termA1 termB2)
							(setq truthv (exemplification (second exp1) (second exp2)) 
				 			expresion (list termA2 termB1)))
						((equal termB1 termA2) 
				 			(setq truthv (exemplification (second exp2) (second exp1)) 
				 			expresion (list termB2 termA1)))
						(T (setf errorSintax 'T)) ))

			  ((and (string= rule "CONVERSIÓN") (null exp2))
			  	(setf ruleSintax "(conversión No-exp1: (P --> S))")
				 (setq truthv (conversion (second exp1)) 
				 		expresion (list termA2 termA1) ))

			  (T (insert2 (format 'nil "Error reglas NAL-1. Revise el nombre de la regla que desea usar")) (setf flag-ingresaBC 'nil))) 

			(if errorSintax
				(insert2 (format 'nil "Error reglas NAL-1. Revise el número de las expresiones que desea usar asi como el orden para la regla de inferencia ~a con estructura ~a"  rule ruleSintax)))
		
			(setf errorSintax 'nil)

		(I-E (first expresion) (second expresion))
		;(I-E termB1 termB1)
		(setf statement (concatenate 'string (string-downcase (string (first expresion))) " --> " (string-downcase (string (second expresion))) 
							" <" (format nil "~f" (first truthv)) ", " (format nil "~f" (second truthv)) ">" )) 

		(if (and statement (not (search "NIL" statement) ))  (insert2 statement))  
		(setq e 'nil e2 'nil) )) )

;;======================================================================================= 
;;  
;;  Reglas de inferencia NAL2 
;;  
;;=======================================================================================
(defun rules-N2 (rule exp1 exp2 &optional decimales)
	(labels (
		(comparasion (vv1 vv2)
		(let ((w+ 'nil) (w 'nil) ) 
	 	 (setq w+ (ext-and (first vv2) (second vv2) (first vv1) (second vv1)) 		;w+ = and(f2 , c2 , f1 , c1 )
	 	 	   w  (ext-and (ext-or (first vv1) (first vv2)) (second vv2) (second vv1)) )	 				;w = and(or(f1, f2) , c1 , c2 )
	 	 (list (float (adjust-precision (/ w+ w) decimales)) 			
  			   (float (adjust-precision (/ w (+ w k)) decimales)) ) ))

  	(analogy (vv1 vv2)
		(list (float (adjust-precision (ext-and (first vv1) (first vv2)) decimales))   ;f = and(f1, f2)
			  (float (adjust-precision (ext-and (first vv2) (second vv1) (second vv2)) decimales))) ) ;c = and(f2, c1, c2)

  	(resemblance (vv1 vv2)
		(list (float (adjust-precision (ext-and (first vv1) (first vv2)) decimales))   ;f = and(f1, f2)
			  (float (adjust-precision (ext-and (ext-or (first vv1) (first vv2)) (second vv1) (second vv2)) decimales))) ) ;c = and(or(f1, f2) , c1 , c2 )
    )

    (let ((termA1 (first (first exp1)))
  			(termA2 (third (first exp1)))
  			(copulaA (second (first exp1)))
  			(termB1 (first (first exp2)))
  			(termB2 (third (first exp2))) 
  			(copulaB (second (first exp2)))
				(ruleSintax 'nil) (errorSintax 'nil) )  
    
    (cond ((string= (string rule) "COMPARACIÓN")
					(setf ruleSintax "(comparación No-exp1: (M --> P) No-exp2: (M --> S))")
					(cond 
						((equal termA1 termB1)
							(setq truthv (comparasion (second exp1) (second exp2)) 
							 			expresion (list termB2  termA2)
							 			statement (concatenate 'string (string-downcase (string (first expresion))) " <-> " (string-downcase (string (second expresion))) 
										" <" (format nil "~f" (first truthv)) ", " (format nil "~f" (second truthv)) ">" )) )
						((equal termB2 termA2) 
				 			(setq truthv (comparasion (second exp2) (second exp1)) 
				 						expresion (list termB1  termA1 )
				 						statement (concatenate 'string (string-downcase (string (first expresion))) " <-> " (string-downcase (string (second expresion))) 
										" <" (format nil "~f" (first truthv)) ", " (format nil "~f" (second truthv)) ">" )) )
						(T (setf errorSintax 'T)) ))

    			((string= (string rule) "ANALOGÍA")
					(setf ruleSintax "(analogía No-exp1: (M --> P) No-exp2: (S <-> M))")
					(cond 
						((and (equal termA1 termB2) (or (string= (string copulaA) "<->") (string= (string copulaB) "<->")) 
									(not (equal copulaA copulaB)))    ;M --> P  S <-> M s->p  ;m <-> p  s --> m s->p
							(setq truthv (analogy (second exp1) (second exp2)) 
							 			expresion (list termB1  termA2)
							 			statement (concatenate 'string (string-downcase (string (first expresion))) " --> " (string-downcase (string (second expresion))) 
										" <" (format nil "~f" (first truthv)) ", " (format nil "~f" (second truthv)) ">" )) )
						((and (equal termA2 termB2) (string= (string copulaB) "<->"))  ;p->m s<->m = p->s 
							(setq truthv (analogy (second exp1) (second exp2)) 
							 			expresion (list termA1  termB1)
							 			statement (concatenate 'string (string-downcase (string (first expresion))) " --> " (string-downcase (string (second expresion))) 
										" <" (format nil "~f" (first truthv)) ", " (format nil "~f" (second truthv)) ">" )) )
						((and (equal termA1 termB1) (string= (string copulaA) "<->"))  ;m<->p m->s = p->s
							(setq truthv (analogy (second exp1) (second exp2)) 
							 			expresion (list termA2  termB2)
							 			statement (concatenate 'string (string-downcase (string (first expresion))) " --> " (string-downcase (string (second expresion))) 
										" <" (format nil "~f" (first truthv)) ", " (format nil "~f" (second truthv)) ">" )) )
						(T (setf errorSintax 'T)) ))

			  ((string= (string rule) "SEMEJANZA")
			  	(setf ruleSintax "(analogía No-exp1: (M <-> P) No-exp2: (S <-> M))") 
			  (cond ((and (equal termA1 termB2) (string= (string copulaA) "<->") (string= (string copulaB) "<->"))
			  			(setq truthv (resemblance (second exp1) (second exp2)) 
								 		expresion (list termB1  termA2)  
							  	  statement (concatenate 'string (string-downcase (string (first expresion))) " <-> " (string-downcase (string (second expresion))) 
											" <" (format nil "~f" (first truthv)) ", " (format nil "~f" (second truthv)) ">" )) ) 
			  		(T (setf errorSintax 'T)) )) 

			  (T (insert2 (format 'nil "Error reglas NAL-2. Revise el nombre de la regla que desea usar" )) (setf flag-ingresaBC 'nil))) 

    	(if errorSintax
				(insert2 (format 'nil "Error reglas NAL-2. Revise el número de las expresiones que desea usar asi como el orden para la regla de inferencia ~a con estructura ~a"  rule ruleSintax)))

			(if (and statement (not (search "NIL" statement) )) 
						(insert2 statement))
			(setq e 'nil e2 'nil errorSintax 'nil)
		(I-E (first expresion) (second expresion))
	)) )

(defun inference-rules (solicitud decimales)
	(let ( (noExp1 (parse-integer (second solicitud)) ) 
			  (noExp2 (if (string= ")" (third solicitud))  'nil (parse-integer (third solicitud)) )) )
	
	(cond ((and (< noExp1 *cont*) (not (numberp noExp2)) )
		(let ((exp1 (first (obtiene-expresion (list noExp1))) )) 
			(forward-rules-N1 (first solicitud) exp1 'nil decimales) ))

		 ((and (< noExp1 *cont*) (< noExp2 *cont*))
		 	
			(let ((exp1 (first (obtiene-expresion (list noExp1))) ) 
			  	  (exp2 (first (obtiene-expresion (list noExp2)))  ))


			(cond ((and (string= (string (first solicitud)) "SELECCIÓN") (numberp (fourth solicitud))) 
					(local-rules-NAL1 (first solicitud) exp1 exp2 nil (fourth solicitud)))

				((or (string= (string (first solicitud)) "SELECCIÓN") (string= (string (first solicitud)) "REVISIÓN"))
					;(and (equal (first exp1) (first exp2))
					;	(not (equal (second exp1) (second exp2))) (not (numberp (fourth solicitud))) ) 
					(local-rules-NAL1 (first solicitud) exp1 exp2 decimales))

				((and (not (numberp (fourth solicitud)) ) (or (string= (string (first solicitud)) "COMPARACIÓN") (string= (string (first solicitud)) "ANALOGÍA") (string= (string (first solicitud)) "SEMEJANZA")))
					(rules-N2 (first solicitud) exp1 exp2 decimales) )

				((not (numberp (fourth solicitud)) )
					(forward-rules-N1 (first solicitud) exp1 exp2 decimales) )

				(T (insert2 (format 'nil "Error en: ~a. Revise la estructura y el nombre de las reglas de inferencia"  solicitud)) (setf flag-ingresaBC 'nil))))  )

		 ((or (>= noExp1 *cont*) (>= noExp2 *cont*))  
		 	(insert2 (format 'nil "Error en: ~a. Números fuera de la base de conocimiento"  solicitud)) ))  
	)
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
(defvar subjectOptions 'nil)
(defvar predicateOptions 'nil)

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

		(insert2 (format 'nil "Lista de coincidencias: ~a" subjectOptions)) ))

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
	
	(insert2 (format 'nil "Lista de coincidencias: ~a" predicateOptions))	))

;;======================================================================================= 
;;  
;;  Función que recibe la salida del parcer de una consulta tipo:
;;     a --> b?, a --> ? y ? --> b
;;  
;;=======================================================================================
(defun query-NAL1 (query decimales)
	(if (not (null query))
		(cond ((listp (first query)) ; a --> b ?
			      (truth-value (first query) decimales) )     ;Se agrega el resultado de la consulta a BC si opcadd fue seleccionado 
			    ((string= (first query) #\?)                ; ? --> b 
			      (subject? query))
			    (T                ; a --> ?)
			    	(predicate? query)) )) )