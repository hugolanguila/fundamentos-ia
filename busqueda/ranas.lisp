;;; ranas.lisp
;;;   
;;;   Problema de las ranas que quieren cruzar el estanque.
;;;   3 ranas de color verde, cada una posicionada sobre una roca de la orilla de un estanque,
;;;   quieren cruzar a la orilla opuesta, donde hay 3 ranas de color cafe, cada una posicionada
;;;   sobre una roca, estas ranas de color cafe quieren cruzar a la orilla donde estan las ranas de color verde.
;;;
;;; Los estados que se proponen para este problema, tienen la siguiente 
;;; estructura.
;;;
;;; ((v1 v2 v3)(c1 c2 c3)(rv tv tc))
;;;
;;; Una lista con 3 sublistas internas, la primer lista tiene como elementos los indices de las rocas del estanque 
;;; donde estan posicionadas las ranas de color verde, la segunda sublista tiene como elementos los indices de 
;;; las rocas del estanque donde estan posicionadas las ranas de color cafe, la tercer sublista tiene como elementos
;;; el indice de la roca vacia, la rana de color verde que saltara y la rana de color cafe que saltara.
;;;
;;; Estado inicial             Estado meta
;;; ((0 1 2)(4 5 6)(3 2 0))    ((4 5 6)(0 1 2)(3 X Y))
;;;
;;; Con este planteamiento, existe mas de un estado meta, basta con que los indices de roca de las ranas verde sean mayores
;;; o iguales que 4 y los indices de roca de las ranas cafe, sean menores o iguales que 2.
;;;
;;; Los operadores que propongo son:
;;; 1. moverRanaVerde: mueve la rana verde cuyo turno es [tv] a la roca vacia [rv].
;;; 2. moverRanaCafe: mueve la rana cafe cuyo turno es [tc] a la roca vacia [rv].
;;;
;;; Las restricciones asociadas son:
;;; 1. Las ranas de color verde no pueden saltar a una roca cuya posicion sea menor a la posicion donde se encuentran.
;;; 2. Las ranas de color cafe no pueden saltar a una roca cuya posicion sea mayor a la posicion donde se encuentran.
;;;
;;; Hernandez Escudero Luis Hugo

(defparameter *frontera* nil)
(defparameter *memoria* nil)
(defparameter *solucion* nil)
(defparameter *ancestro_actual* nil)
(defparameter *id* -1)

(defparameter *operadores* '(:ranaVerde
			     :ranaCafe))

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
  (list *id* estado operador *ancestro_actual*))

;;; Crea un nuevo nodo y lo inserta en la frontera de busqueda dependiendo del [metodo].
;;;
;;; estado -> estado del nuevo nodo
;;; operador -> operador aplicado para obtener el [estado]
;;; metodo -> forma de insertar los nodos (por el inicio o por el final)
(defun agregar-a-frontera (estado operador metodo)
  (let ((nodo (crear-nodo estado operador)))
    (incf *longitud_maxima_frontera*)
    (cond ((eql metodo ':busqueda-en-profundidad)
	   (push nodo *frontera*))
	  ((eql metodo ':busqueda-en-anchura)
	   (setq *frontera* (append *frontera* (list nodo))))
	  (T nil))))

;;; Devuelve el siguiente nodo al frente en la frontera de busqueda
(defun sacar-de-frontera ()
  (pop *frontera*))

;;; Verifica que la roca a la que vaya a saltar una rana sea adecuada
;;; Aunque es valido que una rana de color verde salte hacia atras, esto provocaria en multiples ocasiones
;;; un estado que ya fue analizado con anterioridad. Por lo tanto solo se consideran saltos que acerquen a una
;;; rana hacia la orilla que quiere cruzar.
(defun operador-valido? (operador estado)
  (let* ((rocaVacia (first (third estado)))
	 (turnoVerde (second (third estado)))
	 (turnoCafe (third (third estado)))
	 (ranaVerde (nth turnoVerde (first estado)))
	 (ranaCafe (nth turnoCafe (second estado))))
    (case operador
      (:ranaVerde
       (> rocaVacia ranaVerde))
      (:ranaCafe
       (< rocaVacia ranaCafe))
      (T nil))))

;;; estado-valido?
;;; Devuelve T en caso de que las posiciones de las ranas esten dentro del rango permitido.
;;; Esta funcion esta de mas, porque con el planteamiento de los estados no es posible que 
;;; se produzca un estado no valido.
(defun estado-valido? (estado)
  (let* ((ranasVerdes T)
	 (ranasCafes T))
    (dotimes (i (length (first estado)))
      (setq ranasVerdes (and ranasVerdes (>= (nth i (first estado)) 0) (< (nth i (first estado)) 8)))
      (setq ranasCafes (and ranasCafes (>= (nth i (second estado)) 0) (< (nth i (second estado)) 8))))
    (and ranasVerdes ranasCafes)))

;;; Devuelve una lista, resultado de cambiar a la rana en la [posicion] a la [rocaVacia] de la lista [ranas]

;;; ranas -> lista de ranas a modificar
;;; rocaVacia -> posicion de la roca vacia a la que saltara la siguiente rana.
;;; posicion -> indice de la lista [ranas], que denota a la rana que saltara a la roca vacia.
(defun cambiar-roca (ranas rocaVacia posicion)
  (let ((nuevasPosiciones nil))
    (dotimes (i (length ranas) nuevasPosiciones)
      (if (= i posicion)
	  (setq nuevasPosiciones (append nuevasPosiciones (list rocaVacia)))
	  (setq nuevasPosiciones (append nuevasPosiciones (list (nth i ranas))))))))

;;; Devuelve un estado nuevo, resultado de aplicar el [operador] al [estado]
;;;
;;; estado -> estado actual al que se aplicara el operador.
;;; operador -> operador a ser aplicado.
;;; El stado contiene la sublista con la rocaVacia, y los turnos de las ranas verde o cafes que saltaran.
;;; El operador decide el color de la rana que saltara

(defun aplicar-operador (operador estado)
  (let* ((rocaVacia (first (third estado)))
	 (turnoVerde (second (third estado)))
	 (turnoCafe (third (third estado)))
	 (ranasVerdes (first estado))
	 (ranasCafes (second estado))
	 (siguienteRocaVacia nil)
	 (siguienteTurno nil))
    (case operador
      (:ranaVerde
       (setq siguienteRocaVacia (nth turnoVerde ranasVerdes))
       (setq siguienteTurno (mod (1- turnoVerde) 3))
       (list (cambiar-roca ranasVerdes rocaVacia turnoVerde) 
	     (second estado) 
	     (list siguienteRocaVacia siguienteTurno turnoCafe)))
      (:ranaCafe
       (setq siguienteRocaVacia (nth turnoCafe ranasCafes))
       (setq siguienteTurno (mod (1+ turnoCafe) 3))
       (list (first estado) 
	     (cambiar-roca ranasCafes rocaVacia turnoCafe) 
	     (list siguienteRocaVacia turnoVerde siguienteTurno)))
      (T "error"))))


;;; Obtiene los descendientes de un estado al aplicar todos los operadores posibles.
(defun expandir-estado (estado)
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
(defun estado-recordado? (estado memoria)
  (cond ((null memoria) nil)
	((equal estado (first (second memoria))) T)
	(T (estado-recordado? estado (rest memoria)))))

;;; Devuelve solo aquellos elementos de la lista [estados-y-operadores] 
;;; cuyos estados no se hayan analizado con anterioridad.
;;;
;;; estados-y-operadores -> lista que contiene sublistas donde el primer elemento de la sublista es un estado y el 
;;; segundo es un operador.
(defun filtrar-estados (estados-y-operadores)
 (cond ((null estados-y-operadores) nil)
       ((estado-recordado? (first (first estados-y-operadores)) *memoria*) 
	(filtrar-estados (rest estados-y-operadores)))
       (T (cons (first estados-y-operadores) (filtrar-estados (rest estados-y-operadores))))))

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
;;;
;;; Despliega la solucion de la [lista_nodos] de tal manera que pueda ser entendida por un humano. 
;;; 
;;; lista_nodos -> lista de nodos cuyo orden denota la forma en que se llego a una solucion.
(defun desplegar-solucion (lista_nodos)
  (format t "Solucion con ~A pasos:~%~%" (1- (length lista_nodos)))
  (let ((nodo nil))
    (dotimes (i (length lista_nodos))
      (setq nodo (nth i lista_nodos))
      (if (= i 0)
	  (format t "Inicio en: ~A~%" (second nodo))
	  (format t "\(~2A\) Aplicando ~20A se llega a ~A~%" i (third nodo) (second nodo))))))

;;; Devuelve T si el [estado] contiene una de las soluciones al problema
;;; estado -> estado a verificar
(defun estado-meta? (estado)
  (let ((posicionesVerdes T)
	(posicionesCafes T))
    (dotimes (i (length (first estado)))
      (setq posicionesVerdes (and posicionesVerdes (>= (nth i (first estado)) 4)))
      (setq posicionesCafes (and posicionesCafes (<= (nth i (second estado)) 2))))
    (and posicionesVerdes posicionesCafes)))

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
;;; metodo -> forma de buscar (:busqueda-en-profundidad, :busqueda-en-anchura)
(defun busqueda-ciega (estado_inicial metodo)
  (inicializar)
  (let ((nodo nil)
	(estado nil)
	(sucesores nil)
	(operador nil)
	(meta_encontrada nil))
    (agregar-a-frontera estado_inicial nil metodo)
    (loop until (or meta_encontrada (null *frontera*)) do
	  (setq nodo (sacar-de-frontera))
	  (setq estado (second nodo))
	  (setq operador (third nodo))
	  (push nodo *memoria*)
	  (cond ((estado-meta? estado)
		 (format t "~%Exito. Meta encontrada en ~A intentos~%" (first nodo))
		 (desplegar-solucion (extraer-solucion nodo))
		 (setq meta_encontrada T))
		(T (setq *ancestro_actual* (first nodo))
		   (setq sucesores (expandir-estado estado))
		   (setq sucesores (filtrar-estados sucesores))
		   (loop for sucesor in sucesores do
			 (agregar-a-frontera (first sucesor) (second sucesor) metodo))))))) 

(defun mostrar-indicadores ()
  (format t "~%Nodos creados: ~A~%" *nodos_creados*)
  (format t "Nodos expandidos: ~A~%" *nodos_expandidos*)
  (format t "Longitud maxima de la frontera de busqueda: ~A~%" *longitud_maxima_frontera*)
  (format t "Longitud de la solucion: ~A operadores~%" (1- (length *solucion*)))
  (format t "Tiempo para encontrar la solucion: ~6$ segundos~%" *tiempo_requerido*))

(print "Busqueda en profundidad")
(setq *tiempo1* (get-internal-run-time))
(busqueda-ciega '((0 1 2)(4 5 6)(3 2 0)) ':busqueda-en-profundidad)
(setq *tiempo2* (get-internal-run-time))
(setq *tiempo_requerido* (/ (- *tiempo2* *tiempo1*) (get-internal-real-time)))
(mostrar-indicadores)

(print "Busqueda en anchura")
(setq *tiempo1* (get-internal-run-time))
(busqueda-ciega '((0 1 2)(4 5 6)(3 2 0)) ':busqueda-en-anchura)
(setq *tiempo2* (get-internal-run-time))
(setq *tiempo_requerido* (/ (- *tiempo2* *tiempo1*) (get-internal-real-time)))
(mostrar-indicadores)
