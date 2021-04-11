;r;
;;; granjero.lisp
;;; Problema del granjero, el lobo, oveja y legumbre.
;;;
;;; Los estados que propongo para la solucion de este problema tienen la siguiente estructura.
;;; ((Lobo Obeja Legumbre Barca) (Lobo Obeja Legumbre Barca))
;;; 
;;; Una lista con 2 sublistas internas, donde cada una de ellas representa una orilla del rio/lago y los 
;;; actores que se encuentran en esa orilla, en este caso se asume que solo hay un lobo, una oveja, una legumbre y una barca.
;;; En la orilla donde esta la barca esta el granjero.
;;; 
;;; Estado inicial           Estado meta 
;;; ((1 1 1 1)(0 0 0 0))     ((0 0 0 0)(1 1 1 1))
;;;
;;; Las restricciones del problema son las siguientes.
;;; 1. En ninguna orilla se pueden quedar solos el lobo y la oveja, sin la presencia del granjero.
;;; 2. En ninguna orilla se pueden quedar solos la oveja y la legumbre, sin la presencia del granjero.
;;;
;;; Hernandez Escudero Luis Hugo.

(defparameter *frontera* nil)
(defparameter *memoria* nil)
(defparameter *solucion* nil)
(defparameter *ancestro_actual* nil)
(defparameter *id* -1)
(defparameter *operadores* '((:lobo (1 0 0))
			    (:oveja (0 1 0))
			    (:legumbre (0 0 1))
			    (:Ninguno (0 0 0))))

(defun crear-nodo (estado operador)
  (incf *id*)
  (list *id* estado (first operador) *ancestro_actual*))

(defun insertar-en-frontera (estado operador metodo)
  (let ((nodo (crear-nodo estado operador)))
    (cond ((eql metodo :busqueda-en-profundidad)
	 (push nodo *frontera*))
	((eql metodo :busqueda-en-anchura)
	 (setq *frontera* (append *frontera* (list nodo)))))))

(defun sacar-de-frontera ()
  (pop *frontera*))

(defun orilla-barca (estado)
  (if (= (fourth (first estado)) 1) 0 1))

(defun orilla-sin-granjero (estado)
  (if (= (fourth (first estado)) 1) 1 0))

(defun operador-valido? (operador estado)
  (let* ((orilla (orilla-barca estado))
	(lobo (first (nth orilla estado)))
	(oveja (second (nth orilla estado)))
	(legumbre (third (nth orilla estado))))
    (and (>= lobo (first (second operador)))
	 (>= oveja (second (second operador)))
	 (>= legumbre (third (second operador))))))

(defun estado-valido? (estado)
  (let* ((orilla (orilla-sin-granjero estado))
	(lobo (first (nth orilla estado)))
	(oveja (second (nth orilla estado)))
	(legumbre (third (nth orilla estado))))
    (and (or (> oveja lobo) (zerop oveja))
	 (or (> legumbre oveja) (zerop legumbre)))))

(defun cambiar-orilla (bit) (boole BOOLE-XOR bit 1))

(defun aplicar-operador (operador estado)
  (let* ((orilla_barca (orilla-barca estado))
	(lobo0 (first (first estado)))
	(oveja0 (second (first estado)))
	(legumbre0 (third (first estado)))
	(barca0 (fourth (first estado)))
	(lobo1 (first (second estado)))
	(oveja1 (second (second estado)))
	(legumbre1 (third (second estado)))
	(barca1 (fourth (second estado))))
    (case (first operador)
      (:lobo
       (if (= orilla_barca 0)
	   (list (list 0 oveja0 legumbre0 (cambiar-orilla barca0)) (list 1 oveja1 legumbre1 (cambiar-orilla barca1)))
	   (list (list 1 oveja0 legumbre0 (cambiar-orilla barca0)) (list 0 oveja1 legumbre1 (cambiar-orilla barca1)))))
      (:oveja 
       (if (= orilla_barca 0)
	   (list (list lobo0 0 legumbre0 (cambiar-orilla barca0)) (list lobo1 1 legumbre1 (cambiar-orilla barca1)))
	   (list (list lobo0 1 legumbre0 (cambiar-orilla barca0)) (list lobo1 0 legumbre1 (cambiar-orilla barca1)))))
      (:legumbre
       (if (= orilla_barca 0)
	   (list (list lobo0 oveja0 0 (cambiar-orilla barca0)) (list lobo1 oveja1 1 (cambiar-orilla barca1)))
	   (list (list lobo0 oveja0 0 (cambiar-orilla barca0)) (list lobo1 oveja1 1 (cambiar-orilla barca1)))))
      (:Ninguno
       (list (list lobo0 oveja0 legumbre0 (cambiar-orilla barca0)) (list lobo1 oveja1 legumbre1 (cambiar-orilla barca1)))))))

(defun expandir (estado)
  (let ((descendientes nil)
	(nuevo_estado nil))
    (dolist (operador *operadores* descendientes)
      (setq nuevo_estado (aplicar-operador operador estado))
      (when (and (operador-valido? operador estado)
		 (estado-valido? nuevo_estado))
	(setq descendientes (cons (list nuevo_estado operador) descendientes))))))

(defun estado-recorrido? (estado memoria)
  (cond ((null memoria) nil)
	((equal estado (second (first memoria))) T)
	(T (estado-recorrido? estado (rest memoria)))))

(defun filtrar-estados (estados_y_operadores)
  (cond ((null estados_y_operadores) nil)
	((estado-recorrido? (first (first estados_y_operadores)) *memoria*)
	 (filtrar-estados (rest estados_y_operadores)))
	(T (cons (first estados_y_operadores) (filtrar-estados (rest estados_y_operadores))))))

(defun extraer-solucion (nodo)
  (labels ((localizar-nodo (id lista)
	     (cond ((null lista) nil)
		   ((eql id (first (first lista))) (first lista))
		   (T (localizar-nodo id (rest lista))))))
    (let ((nodo_actual (localizar-nodo (first nodo) *memoria*)))
      (loop while (not (null nodo_actual)) do
	    (push nodo_actual *solucion*)
	    (setq nodo_actual (localizar-nodo (fourth nodo_actual) *memoria*))))
    *solucion*))

(defun desplegar-solucion (solucion)
  (format t "Solucion con ~A pasos:~%~%" (1- (length solucion)))
  (let ((nodo nil))
    (dotimes (i (length solucion))
      (setq nodo (nth i solucion))
      (if (= i 0)
	  (format t "Inicio en: ~A~%" (second nodo))
	  (format t "\(~2A\) aplicando ~20A se llega a ~A~%" i (third nodo) (second nodo))))))

(defun inicializar ()
  (setq *frontera* nil)
  (setq *memoria* nil)
  (setq *solucion* nil)
  (setq *ancestro_actual* nil)
  (setq *id* -1))

(defun buscar (estado_inicial estado_meta metodo)
  (inicializar)
  (let ((nodo nil)
	(estado nil)
	(sucesores nil)
	(operador nil)
	(meta_encontrada nil))
    (insertar-en-frontera estado_inicial nil metodo)
    (loop until (or meta_encontrada (null *frontera*)) do
	  (setq nodo (sacar-de-frontera))
	  (setq estado (second nodo))
	  (setq operador (third nodo))
	  (push nodo *memoria*)
	  (cond ((equal estado estado_meta) 
		 (format t "Exito. Meta encontrada en ~A intentos ~%~%" (first nodo))
		 (desplegar-solucion (extraer-solucion nodo))
		 (setq meta_encontrada t))
		(T (setq *ancestro_actual* (first nodo))
		   (setq sucesores (expandir estado))
		   (setq sucesores (filtrar-estados sucesores))
		   (loop for sucesor in sucesores do
			 (insertar-en-frontera (first sucesor) (second sucesor) metodo)))))))

(buscar '((1 1 1 1)(0 0 0 0)) '((0 0 0 0)(1 1 1 1)) ':busqueda-en-anchura)
(buscar '((1 1 1 1)(0 0 0 0)) '((0 0 0 0)(1 1 1 1)) ':busqueda-en-profundidad)
