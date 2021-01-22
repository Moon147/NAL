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
;;      ACTUALIZACIÓN:  Salvador Godoy modificó las reglas STATEMENT, TERM y RELATION para que
;;      (17-abril-2020) entreguen la estructura de un juicio en forma de lista "pura" sin cadenas.
;;                      La expresión.
;;                                          canario --> ave <1.0,0.9>
;;                      antes se entregaba con la estructura:
;;                                        ("canario --> ave" (1.0 0.9))
;;                      Y ahora se enregan así:
;;                                      ((CANARIO --> AVE) (1.0 0.9))
;;                      Con la limitación de no distinguir mayúsculas y minúsculas...
;;                      También invertí el orden de estos comentarios para que el último siempre
;;                      sea el de hasta arriba y el más antiguo, el de hasta abajo...
;;                      Por último, modifiqué la regla NORMALIZED-NUMBER para que acepte también
;;                      un cero sin parte decimal, así como un uno sin parte decimal...
;;
;;      ACTUALIZACIÓN:  Jenifer López agregó el signo de interrogación para consulta de los valores
;;      (11-abril,2020) de verdad en la regla "query".
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
;;
;;      ACTUALIZACIÓN:  Montserrat González Padilla actualizó la gramática para dejarla
;;      (01-abril,2020) "casi" como la gramática oficial de NARSESE ubicada en 
;;                      https://github.com/opennars/opennars/wiki/Narsese-Grammar-(Input-Output-Format)
;;                      Con excepción del formato del valor de verdad que quedó delimitado
;;                      por corchetes triangulares y separado por coma.
;;=================================================================================================

(ql:quickload :parseq)
(in-package :nal)

(parseq:defrule sentence () (or judgement query))

(parseq:defrule funciones () (or (and "(" operacion sp decimalpart sp ")" )
                                      (and "(" operacion sp decimalpart sp decimalpart sp ")" ) 
                                      (and "(selección" sp decimalpart sp decimalpart sp formula sp ")" ))  (:choose 1 3 5 7) )


;(parseq:defrule funciones () (and "(" operacion sp int sp int sp formula ")") (:choose 1 3 5 7) )
                     
(parseq:defrule operacion () (or "revisión"
                                 "selección"
                                 "deducción"
                                 "inducción"
                                 "abducción"
                                 "conversión"
                                 "ejemplificación"
                                 "comparación"
                                 "analogía"
                                 "semejanza")
        (:lambda (op) (read-from-string op)))

(parseq:defrule formula () (or "0" "1")
        (:lambda (form) (read-from-string form)))

(parseq:defrule judgement () (or (and "(" sp statement sp (? truthvalue) sp ")")
                                 (and "(" sp "{" sp statement sp "}" sp "-->" sp statement sp (? truthvalue) sp ")")
                                 (and "(" sp statement sp "-->" sp "[" sp statement sp "]" sp (? truthvalue) sp ")")
                                 (and "(" sp "{" sp statement sp "}" sp "-->" sp "[" sp statement sp "]" sp (? truthvalue) sp ")")) (:choose 2 4 6 8 10 12 14 16))

(parseq:defrule query () (or (and "(" sp statement sp "?" sp ")") 
                             (and "(" sp term sp "-->" sp "?" sp ")")
                             (and "(" sp "?" sp "-->" sp term sp ")")) (:choose 2 4 6))

;; ===========================================================
;;  Agregé :choose en lugar de :string  
;;  para separar los elementos de la lista (17-abril-2020)
;; ===========================================================
(parseq:defrule statement () (or (and term sp relation sp term)
                      compound-statement 
                      term) 
                      (:choose 0 2 4))
;; ===========================================================
;;  Agregé :lambda después de :string
;;  para  entregar en forma de símbolo (17-abril-2020)
;; ===========================================================
(parseq:defrule term () (or anyword variable compound-term) 
                    (:string)
                    (:lambda (term) (read-from-string term)) )

(parseq:defrule relation () (or "<->"      ;;Similarity NAL-2
                         "<=>"      ;;Equivalence NAL-5
                         "->o"      ;;Property NAL-2
                         "-->"      ;;Inheritance NAL-1
                         "o->o"     ;;InstanceProperty NAL-2
                         "o->"      ;;Instance NAL-2           
                         "==>" )    ;;Implication NAL-5
                    (:lambda (copula) (read-from-string copula)) )
;; ===========================================================
;; ===========================================================                    

                    
(parseq:defrule compound-statement () (or (and "(--" sp statement ")")     ;;Negation NAL-5
           (and "(||" sp statement sp+ statement "+)")      ;;Disjunction NAL-5
           (and "(&&" sp statement sp+ statement "+)")))    ;;Conjunction NAL-5

(parseq:defrule compound-term () (or (and "{" term "+}") ;;SetExt NAL-2
        (and "[" term "+]") ;;SetInt NAL-2
        (and "(&" sp+ term sp+ term "+)")      ;;IntersectionExt NAL-3
        (and "(|" sp+ term sp+ term "+)")      ;;IntersectionInt NAL-3
        (and "(-" sp+ term sp+ term ")")       ;;DifferenceExt NAL-3
        (and "(~" sp+ term sp+ term ")")       ;;DifferenceInt NAL-3
        (and "(*" sp+ term sp+ term "+)")      ;;Product NAL-4
        (and "(/" sp+ term "+" sp+ "_" sp+ term "*)")      ;;ImageExt NAL-4
        (and "(\\" sp+ term "+" sp+ "_" sp+ term "*)")))   ;; ImageInt NAL-4

(parseq:defrule variable () (or independent-var
                         dependent-var
                         query-var))

(parseq:defrule independent-var () (and "$[" anyword "]"))

(parseq:defrule dependent-var () (and "#" anyword))    ;;NAL-6

(parseq:defrule query-var () (and "?[" anyword "]"))   ;;NAL-6

(parseq:defrule truthvalue() (and "<" sp frequency sp "," sp confidence sp ">") (:choose 2 6))

;; ================================================================================================
;;   Ya no existen límites a la precisión de los números de la siguiente sección...
;; ================================================================================================

(parseq:defrule frequency () normalized-number)
(parseq:defrule confidence () normalized-number)


(parseq:defrule normalized-number () (or   (and "0" "." decimalpart)
                                    "0"
                                    (and "1" "." (* "0")) 
                                    "1")
                                    (:flatten)
            (:lambda (&rest arguments) 
                    (float (read-from-string (apply #'concatenate 'string  arguments)))) ) 

(parseq:defrule decimalpart () (* digit) (:string))

(parseq:defrule int () (decimalpart)
            (:lambda (&rest arguments) 
             (read-from-string (apply #'concatenate 'string  arguments))))


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

(parseq:defrule digit () (char "0-9"))
(parseq:defrule binary-digit () (char "0-1"))

(parseq:defrule anyword () (+ (char "a-zA-Z0-9_üáéíóúñÁÉÍÓÚÑ-")) )

(parseq:defrule sp () (* (or #\space #\tab #\newline))) ;espacio opcional (cerradura transitiva)
(parseq:defrule sp+ () (+ (or #\space #\tab #\newline)))  ;espacio obligatorio (cerrradura positiva)
