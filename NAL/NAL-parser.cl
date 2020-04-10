;;================================================================================================= 
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
;;
;;      ACTUALIZACIÓN:  Montserrat González Padilla actualizó la gramática para dejarla
;;      (01-abril,2020) "casi" como la gramática oficial de NARSESE ubicada en 
;;                      https://github.com/opennars/opennars/wiki/Narsese-Grammar-(Input-Output-Format)
;;                      Con excepción del formato del valor de verdad que quedó delimitado
;;                      por corchetes triangulares y separado por coma.
;;
;;      ACTUALIZACIÓN:  Salvador Godoy corrigió algunos errores menores en la gramática:
;;      (10-abril,2020) *) La aparición de espacios en la regla judgement se soluciónó con
;;                         especificar un (:string) en la regla statement.
;;                      *) En la regla relation(cópula) el último comentario debe ir
;;                         fuera de los paréntesis para no causar problemas...
;;                      *) Ahora se aceptan números normalizados con cualquier cantidad de
;;                         dígitos decimales, para ello se separó la parte decimal en la regla
;;                         decimal-part y se cambió la función de aceptación en la regla 
;;                         normalized-number...
;;                      *) En el comentario de la regla relation la cópula Instance es de nivel 2
;;                         en lugar de nivel 1 como estaba indicado...
;;=================================================================================================

(ql:quickload :parseq)
(use-package :parseq)
(in-package :nal)

(defrule sentence () (or judgement query))

(defrule judgement () (and statement sp (? truthvalue)) (:choose 0 2))

(defrule query () statement)

(defrule statement () (or (and term sp relation sp term)
                      compound-statement 
                      term) 
                      (:string))

(defrule term () (or anyword variable compound-term) (:string))

(defrule relation () (or "<->"      ;;Similarity NAL-2
                         "<=>"      ;;Equivalence NAL-5
                         "->o"      ;;Property NAL-2
                         "-->"      ;;Inheritance NAL-1
                         "o->o"     ;;InstanceProperty NAL-2
                         "o->"      ;;Instance NAL-2					 
                         "==>" ))   ;;Implication NAL-5

(defrule compound-statement () (or (and "(--" sp statement ")")     ;;Negation NAL-5
				   (and "(||" sp statement sp+ statement "+)")      ;;Disjunction NAL-5
				   (and "(&&" sp statement sp+ statement "+)")))    ;;Conjunction NAL-5

(defrule compound-term () (or (and "{" term "+}") ;;SetExt NAL-2
			  (and "[" term "+]") ;;SetInt NAL-2
			  (and "(&" sp+ term sp+ term "+)")      ;;IntersectionExt NAL-3
			  (and "(|" sp+ term sp+ term "+)")      ;;IntersectionInt NAL-3
			  (and "(-" sp+ term sp+ term ")")       ;;DifferenceExt NAL-3
			  (and "(~" sp+ term sp+ term ")")       ;;DifferenceInt NAL-3
			  (and "(*" sp+ term sp+ term "+)")      ;;Product NAL-4
			  (and "(/" sp+ term "+" sp+ "_" sp+ term "*)")      ;;ImageExt NAL-4
			  (and "(\\" sp+ term "+" sp+ "_" sp+ term "*)")))   ;; ImageInt NAL-4

(defrule variable () (or independent-var
                         dependent-var
                         query-var))

(defrule independent-var () (and "$[" anyword "]"))

(defrule dependent-var () (and "#" anyword))    ;;NAL-6

(defrule query-var () (and "?[" anyword "]"))   ;;NAL-6

(defrule truthvalue() (and "<" sp frequency sp "," sp confidence sp ">") (:choose 2 6))

;; ================================================================================================
;;   Ya no existen límites a la precisión de los números de la siguiente sección...
;; ================================================================================================

(defrule frequency () normalized-number)
(defrule confidence () normalized-number)

;; 

(defrule normalized-number () (or   (and "0" "." decimalpart)
                                    (and "1" "." (* "0")) ) 
                                    (:flatten)
            (:lambda (&rest arguments) 
                    (float (read-from-string (apply #'concatenate 'string  arguments))))   )

(defrule decimalpart () (* digit) (:string))


;; ================================================================================================
;;   Todas las siguientes reglas son "de servicio" con definiciones generales
;;   para cualquier otra regla.  Particularmente caracteres de espacio...
;;
;;   Defino dos reglas para espacios: espacio opcional (sp) y espacio obligatorio (sp+)
;;   La razón de ello es, fudamentalmente, para separar los términos de la cópula en 
;;   una relación. Si el term1 llegara a terminar en 'o', o el term2 a comenzar en 'o',
;;   o ambas situaciones, podría confundirse con las cópulas 'o->','->o' o 'o->o', por ello,
;;   el espacio que separa términos y cópula es obligatorio...
;; ================================================================================================

(defrule digit () (char "0-9"))
(defrule binary-digit () (char "0-1"))

(defrule anyword () (+ (char "a-zA-Z0-9_üáéíóúñÁÉÍÓÚÑ")) )

(defrule sp () (* (or #\space #\tab #\newline)))	;espacio opcional (cerradura transitiva)
(defrule sp+ () (+ (or #\space #\tab #\newline)))	;espacio obligatorio (cerrradura positiva)
