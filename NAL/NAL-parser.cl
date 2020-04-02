;;======================================================================================= 
;;  Prueba de Parser para NAL
;;  usando el paquete :PARSEQ en
;;     github.com/mrossini-ethz/parseq
;;
;;    *Nota: en esta prueba asumo que no existe 'tokenizador' (lexer) para la
;;           expresión a parsear, así que la expresión completa de entrada se
;;           lee en una sola cadena.  
;;           Muy probablemente, agregar el tokenizador facilitaría las cosas
;;           y ayudaría a eliminar algunas limitaciones...
;;
;;  Dr. Salvador Godoy C. -  noviembre 2019.
;;=======================================================================================

;(ql:quickload :parseq)
;(use-package :parseq)
(in-package :nal)

(defrule sentence () (or judgement question))

(defrule judgement () (and statement sp (? truthvalue)) (:choose 0 2))

(defrule question () statement)

(defrule statement () (or (and term sp relation sp term)
			  compound-statement 
			  term))

(defrule term () (or anyword
		 variable
		 compound-term)
		(:string))

(defrule relation () (or "<->" ;;Similarity NAL-2
			 "<=>" ;;Equivalence NAL-5
			 "->o" ;;Property NAL-2
			 "-->" ;;Inheritance NAL-1
			 "o->o" ;;InstanceProperty NAL-2
			 "o->" ;;Instance NAL-1					 
			 "==>" ;;Implication NAL-5
			 ))

(defrule compound-statement () (or (and "(--" sp statement ")") ;;Negation NAL-5
				   (and "(||" sp statement sp+ statement "+)") ;;Disjunction NAL-5
				   (and "(&&" sp statement sp+ statement "+)"))) ;;Conjunction NAL-5

(defrule compound-term () (or (and "{" term "+}") ;;SetExt NAL-2
			  (and "[" term "+]") ;;SetInt NAL-2
			  (and "(&" sp+ term sp+ term "+)") ;;IntersectionExt NAL-3
			  (and "(|" sp+ term sp+ term "+)") ;;IntersectionInt NAL-3
			  (and "(-" sp+ term sp+ term ")") ;;DifferenceExt NAL-3
			  (and "(~" sp+ term sp+ term ")") ;;DifferenceInt NAL-3
			  (and "(*" sp+ term sp+ term "+)") ;;Product NAL-4
			  (and "(/" sp+ term "+" sp+ "_" sp+ term "*)") ;;ImageExt NAL-4
			  (and "(\\" sp+ term "+" sp+ "_" sp+ term "*)"))) ;; ImageInt NAL-4

(defrule variable () (or independent-var
			 dependent-var
			 query-var))

(defrule independent-var () (and "$[" anyword "]"))

(defrule dependent-var () (and "#" anyword)) ;;NAL-6

(defrule query-var () (and "?[" anyword "]")) ;;NAL-6

(defrule truthvalue() (and "<" sp frequency sp "," sp confidence sp ">") (:choose 2 6))

;; ======================================================================================
;;   En la sección que sigue, existen limitantes:
;;
;;      *) Como la expresión lambda que valida el número flotante tiene 3 argumentos,
;;         no se permite colocar sólo un dígito 0, se requiere 0.0...
;; ======================================================================================

(defrule frequency () normalized-number)
(defrule confidence () normalized-number)

(defrule normalized-number () (or (and "0" "." digit)(and "1" "." "0")) 
		(:flatten) 
		(:lambda (x y z) (float (read-from-string (concatenate 'string x y (string z))))) )


;; ======================================================================================
;;   Todas las siguientes reglas son "de servicio" con definiciones generales
;;   para cualquier otra regla.  Particularmente caracteres de espacio...
;;
;;   Defino dos reglas para espacios: espacio opcional (sp) y espacio obligatorio (sp+)
;;   La razón de ello es, fudamentalmente, para separar los términos de la cópula en 
;;   una relación. Si el term1 llegara a terminar en 'o', o el term2 a comenzar en 'o',
;;   o ambas situaciones, podría confundirse con las cópulas 'o->','->o' o 'o->o', por ello,
;;   el espacio que separa términos y cópula es obligatorio...
;; ======================================================================================

(defrule digit () (char "0-9"))
(defrule binary-digit () (char "0-1"))

(defrule anyword () (+ (char "a-zA-Z0-9_üáéíóúñÁÉÍÓÚÑ")) )

(defrule sp () (* (or #\space #\tab #\newline)))	;espacio opcional (cerradura transitiva)
(defrule sp+ () (+ (or #\space #\tab #\newline)))	;espacio obligatorio (cerrradura positiva)
