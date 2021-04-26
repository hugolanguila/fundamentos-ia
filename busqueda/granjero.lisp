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

(defparameter *nodos_creados* 0)
(defparameter *nodos_expandidos* 0)
(defparameter *longitud_maxima_frontera* 0)
(defparameter *longitud_solucion* 0)
(defparameter *tiempo1* 0)
(defparameter *tiempo2* 0)
(defparameter *tiempo_requerido* 0)

;;; Crea un nuevo nodo con la siguiente estructura
;;; (id estado operador ancestro_actual)
;;; estado -> estado actual
;;; operador -> operador aplicado al ancestro para llegar al estado actual
(defun crear-nodo (estado operador)
  (incf *id*)
  (incf *nodos_creados*)
  (list *id* estado (first operador) *ancestro_actual*))

;;; Crea un nuevo nodo y lo inserta en la frontera de busqueda dependiendo del [metodo].
;;;
;;; estado -> estado del nuevo nodo
;;; operador -> operador aplicado para obtener el [estado]
;;; metodo -> forma de insertar los nodos (por el inicio o por el final)
(defun insertar-en-frontera (estado operador metodo)
  (let ((nodo (crear-nodo estado operador)))
    (incf *longitud_maxima_frontera*)
    (cond ((eql metodo :busqueda-en-profundidad)
	 (push nodo *frontera*))
	((eql metodo :busqueda-en-anchura)
	 (setq *frontera* (append *frontera* (list nodo)))))))

;;;
;;; Devuelve el siguiente nodo a analizar
(defun sacar-de-frontera ()
  (pop *frontera*))

;;; Devuelve la orilla en la que se encuentra la barca (0 -> origen) (1 -> destino)
(defun orilla-barca (estado)
  (if (= (fourth (first estado)) 1) 0 1))

;;; Devuelve la orilla en la que no se encuentra la barca o bien donde no esta el granjero (0 -> origen) (1 -> destino)
(defun orilla-sin-granjero (estado)
  (if (= (fourth (first estado)) 1) 1 0))

;;;
;;; Verifica si el [operador] es valido para aplicarse al [estado]
;;;
;;; En este caso verifica si el "agente" que se desea pasar a la otra orilla esta presente en la orilla de la que se parte.
(defun operador-valido? (operador estado)
  (let* ((orilla (orilla-barca estado))
	(lobo (first (nth orilla estado)))
	(oveja (second (nth orilla estado)))
	(legumbre (third (nth orilla estado))))
    (and (>= lobo (first (second operador)))
	 (>= oveja (second (second operador)))
	 (>= legumbre (third (second operador))))))

;;; Verifica si el [estado] cumple con las restricciones del problema
;;; El lobo no puede quedarse con la oveja sin la precencia del granjero
;;; La oveja no puede quedarse con la legumbre sin la precensia del granjero.
(defun estado-valido? (estado)
  (let* ((orilla (orilla-sin-granjero estado))
	(lobo (first (nth orilla estado)))
	(oveja (second (nth orilla estado)))
	(legumbre (third (nth orilla estado))))
    (and (or (> oveja lobo) (zerop oveja))
	 (or (> legumbre oveja) (zerop legumbre)))))

;;; Cambia de orilla la barca
(defun cambiar-orilla (bit) (boole BOOLE-XOR bit 1))

;;; Devuelve una lista con el nuevo estado, resultado de aplicar el [operador] al [estado]
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

;;; Obtiene los descendientes de un estado al aplicar todos los operadores posibles y validos al [estado].
(defun expandir (estado)
  (let ((descendientes nil)
	(nuevo_estado nil))
    (incf *nodos_expandidos*)
    (dolist (operador *operadores* descendientes)
      (setq nuevo_estado (aplicar-operador operador estado))
      (when (and (operador-valido? operador estado)
		 (estado-valido? nuevo_estado))
	(setq descendientes (cons (list nuevo_estado operador) descendientes))))))

;;; Verifica si ya se analizo el [estado] al verificar si ya se encuentra en la [memoria]
;;;
;;; estado -> estado a buscar en la lista memoria
;;; memoria -> lista que contiene todos los estados analizados.
(defun estado-recorrido? (estado memoria)
  (cond ((null memoria) nil)
	((equal estado (second (first memoria))) T)
	(T (estado-recorrido? estado (rest memoria)))))

;;; Devuelve solo aquellos elementos de la lista [estados-y-operadores] 
;;; cuyos estados no se hayan analizado con anterioridad.
;;;
;;; estados-y-operadores -> lista que contiene sublistas donde el primer elemento de la sublista es un estado y el 
;;; segundo es un operador.
(defun filtrar-estados (estados_y_operadores)
  (cond ((null estados_y_operadores) nil)
	((estado-recorrido? (first (first estados_y_operadores)) *memoria*)
	 (filtrar-estados (rest estados_y_operadores)))
	(T (cons (first estados_y_operadores) (filtrar-estados (rest estados_y_operadores))))))

;;; Devuelve una lista, cuyos elementos son el camino que se siguio hasta llegar a un [nodo]
;;;
;;; nodo -> nodo del cual se rastreara el camino recorrido hasta llegar a el. 
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

;;; Despliega la solucion de la [lista_nodos] de tal manera que pueda ser entendida por un humano. 
;;; 
;;; lista_nodos -> lista de nodos cuyo orden denota la forma en que se llego a una solucion.
(defun desplegar-solucion (solucion)
  (format t "Solucion con ~A pasos:~%~%" (1- (length solucion)))
  (let ((nodo nil))
    (dotimes (i (length solucion))
      (setq nodo (nth i solucion))
      (if (= i 0)
	  (format t "Inicio en: ~A~%" (second nodo))
	  (format t "\(~2A\) aplicando ~20A se llega a ~A~%" i (third nodo) (second nodo))))))

;;; Inicializa las variables a usar en la busqueda de la solucion 
(defun inicializar ()
  (setq *frontera* nil)
  (setq *memoria* nil)
  (setq *solucion* nil)
  (setq *ancestro_actual* nil)
  (setq *id* -1)
  (setq *nodos_creados* 0)
  (setq *nodos_expandidos* 0)
  (setq *longitud_maxima_frontera* 0)
  (setq *tiempo1* 0)
  (setq *tiempo2* 0)
  (setq *tiempo_requerido* 0))

;;; Busca la solucion del problema de manera [no informada]
;;; partiendo del [estado_inicial] y aplicando el [metodo] de busqueda,
;;; 
;;; estado_inicial -> estado del que parte la busqueda
;;; estado_meta -> estado al que se desea llegar
;;; metodo -> forma de buscar (:busqueda-en-profundidad, :busqueda-en-anchura)
(defun busqueda-ciega (estado_inicial estado_meta metodo)
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
		 (format t "~%~%Exito. Meta encontrada en ~A intentos ~%" (first nodo))
		 (desplegar-solucion (extraer-solucion nodo))
		 (setq meta_encontrada t))
		(T (setq *ancestro_actual* (first nodo))
		   (setq sucesores (expandir estado))
		   (setq sucesores (filtrar-estados sucesores))
		   (loop for sucesor in sucesores do
			 (insertar-en-frontera (first sucesor) (second sucesor) metodo)))))))

(defun mostrar-indicadores ()
  (format t "~%Nodos creados: ~A~%" *nodos_creados*)
  (format t "Nodos expandidos: ~A~%" *nodos_expandidos*)
  (format t "Longitud maxima de la frontera de busqueda: ~A~%" *longitud_maxima_frontera*)
  (format t "Longitud de la solucion: ~A operadores~%" (1- (length *solucion*)))
  (format t "Tiempo para encontrar la solucion: ~6$ segundos~%" *tiempo_requerido*))

(print "Busqueda en profundidad")
(setq *tiempo1* (get-internal-run-time))
(busqueda-ciega '((1 1 1 1)(0 0 0 0)) '((0 0 0 0)(1 1 1 1)) ':busqueda-en-anchura)
(setq *tiempo2* (get-internal-run-time))
(setq *tiempo_requerido* (/ (- *tiempo2* *tiempo1*) (get-internal-real-time)))
(mostrar-indicadores)

(print "Busqueda en anchura")
(setq *tiempo1* (get-internal-run-time))
(busqueda-ciega '((1 1 1 1)(0 0 0 0)) '((0 0 0 0)(1 1 1 1)) ':busqueda-en-profundidad)
(setq *tiempo2* (get-internal-run-time))
(setq *tiempo_requerido* (/ (- *tiempo2* *tiempo1*) (get-internal-real-time)))
(mostrar-indicadores)
