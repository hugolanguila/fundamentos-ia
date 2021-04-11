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

(defun crear-nodo (estado operador)
  (incf *id*)
  (list *id* estado operador *ancestro_actual*))

(defun agregar-a-frontera (estado operador metodo)
  (let ((nodo (crear-nodo estado operador)))
    (cond ((eql metodo ':busqueda-en-profundidad)
	   (push nodo *frontera*))
	  ((eql metodo ':busqueda-en-anchura)
	   (setq *frontera* (append *frontera* (list nodo))))
	  (T nil))))

(defun sacar-de-frontera ()
  (pop *frontera*))

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
;(or (> rocaVacia ranaVerde) (< rocaVacia ranaCafe))))

;;; estado-valido?
;;; Devuelve T en caso de que las posiciones de las ranas esten dentro del rango permitido.
(defun estado-valido? (estado)
  (let* ((ranasVerdes T)
	 (ranasCafes T))
    (dotimes (i (length (first estado)))
      (setq ranasVerdes (and ranasVerdes (>= (nth i (first estado)) 0) (< (nth i (first estado)) 8)))
      (setq ranasCafes (and ranasCafes (>= (nth i (second estado)) 0) (< (nth i (second estado)) 8))))
    (and ranasVerdes ranasCafes)))

(defun cambiar-roca (ranas rocaVacia posicion)
  (let ((nuevasPosiciones nil))
    (dotimes (i (length ranas) nuevasPosiciones)
      (if (= i posicion)
	  (setq nuevasPosiciones (append nuevasPosiciones (list rocaVacia)))
	  (setq nuevasPosiciones (append nuevasPosiciones (list (nth i ranas))))))))

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

(defun expandir-estado (estado)
  (let ((descendientes nil)
	(nuevo_estado nil))
    (dolist (operador *operadores* descendientes)
      (setq nuevo_estado (aplicar-operador operador estado))
      (when (and (operador-valido? operador estado)
		 (estado-valido? nuevo_estado))
	(setq descendientes (cons (list nuevo_estado operador) descendientes))))))

(defun estado-recordado? (estado memoria)
  (cond ((null memoria) nil)
	((equal estado (first (second memoria))) T)
	(T (estado-recordado? estado (rest memoria)))))

(defun filtrar-estados (estados-y-operadores)
 (cond ((null estados-y-operadores) nil)
       ((estado-recordado? (first (first estados-y-operadores)) *memoria*) 
	(filtrar-estados (rest estados-y-operadores)))
       (T (cons (first estados-y-operadores) (filtrar-estados (rest estados-y-operadores))))))

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

(defun desplegar-solucion (lista_nodos)
  (format t "Solucion con ~A pasos:~%~%" (1- (length lista_nodos)))
  (let ((nodo nil))
    (dotimes (i (length lista_nodos))
      (setq nodo (nth i lista_nodos))
      (if (= i 0)
	  (format t "Inicio en: ~A~%" (second nodo))
	  (format t "\(~2A\) Aplicando ~20A se llega a ~A~%" i (third nodo) (second nodo))))))

(defun estado-meta? (estado)
  (let ((posicionesVerdes T)
	(posicionesCafes T))
    (dotimes (i (length (first estado)))
      (setq posicionesVerdes (and posicionesVerdes (>= (nth i (first estado)) 4)))
      (setq posicionesCafes (and posicionesVerdes (<= (nth i (second estado)) 2))))
    (and posicionesVerdes posicionesCafes)))

(defun inicializar ()
  (setq *frontera* nil)
  (setq *memoria* nil)
  (setq *solucion* nil)
  (setq *ancestro_actual* nil)
  (setq *id* -1))

(defun buscar (estado_inicial metodo)
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
		 (format t "Exito. Meta encontrada en ~A intentos~%" (first nodo))
		 (desplegar-solucion (extraer-solucion nodo))
		 (setq meta_encontrada T))
		(T (setq *ancestro_actual* (first nodo))
		   (setq sucesores (expandir-estado estado))
		   (setq sucesores (filtrar-estados sucesores))
		   (loop for sucesor in sucesores do
			 (agregar-a-frontera (first sucesor) (second sucesor) metodo))))))) 

(buscar '((0 1 2)(4 5 6)(3 2 0)) ':busqueda-en-profundidad)
(buscar '((0 1 2)(4 5 6)(3 2 0)) ':busqueda-en-anchura)
