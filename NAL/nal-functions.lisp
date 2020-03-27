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