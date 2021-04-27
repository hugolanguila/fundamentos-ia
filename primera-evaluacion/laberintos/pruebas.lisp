;;; laberinto2d.lisp
;;; Resuelve el problema de encontrar la salida en un laberinto a partir del estado inicial
;;; 
;;; Representacion de los estados
;;;    Un estado se representa como un arreglo con 2 elementos #(X Y) que denota la posicion actual 
;;;    X -> fila
;;;    Y -> columna
;;; 
;;; Hernandez Escudero Luis Hugo
(load "maze_lib.lisp")
(quicklisp:quickload 'cl-heap)

(defparameter *frontera* nil)
(defparameter *frontera_con_prioridad* nil)
(defparameter *solucion* nil)
(defparameter *memoria* nil)
(defparameter *ancestro_actual* nil)
(defparameter *id* -1)
(defparameter *xMeta* nil)
(defparameter *yMeta* nil)

(defvar *operadores* '((:norte    0)
		       (:noreste  1)
		       (:este     2)
		       (:sureste  3)
		       (:sur      4)
		       (:suroeste 5)
		       (:oeste    6)
		       (:noroeste 7)))

;;; Crea un nodo
(defun crear-nodo (estado operador aptitud)
  (incf *id*)
  (list *id* estado operador *ancestro_actual* aptitud))

;;;
;;; agrega un nodo a la frontera de busqueda
(defun agregar-a-frontera (estado operador metodo &optional (aptitud))
  (let ((nodo (crear-nodo estado operador aptitud)))
    (cond ((eql metodo :busqueda-en-profundidad)
	   (cl-heap:enqueue *frontera_con_prioridad* nodo aptitud))
	  ((eql metodo :sin-orden)
	   (cl-heap:enqueue *frontera_con_prioridad* nodo aptitud)))))

(defun sacar-de-frontera ()
  (pop *frontera*))

(defun operador-valido? (operador estado)
  (let* ((fila (aref estado 0))
	 (columna (aref estado 1))
	 (celda_actual (get-cell-walls fila columna))
	 (celda_siguiente nil)
	 (tamanio_laberinto (array-dimensions (get-maze-data))))
    (case (first operador)
      (:norte
       (cond ((>= (1- fila) 0)
	      (setq celda_siguiente (get-cell-walls (1- fila) columna))
	      (and (evenp celda_actual) (not (logbitp 2 celda_siguiente))))
	     (T nil))) 
      (:sur
       (cond ((< (1+ fila) (nth 0 tamanio_laberinto))
	      (setq celda_siguiente (get-cell-walls (1+ fila) columna))
	      (and (not (logbitp 2 celda_actual)) (evenp celda_siguiente)))
	     (T nil)))
      (:este 
       (cond ((< (1+ columna) (nth 1 tamanio_laberinto))
	      (setq celda_siguiente (get-cell-walls fila (1+ columna)))
	      (and (not (logbitp 1 celda_actual)) (<= celda_siguiente 7)))
	     (T nil)))
      (:oeste 
       (cond ((>= (1- columna) 0)
	      (setq celda_siguiente (get-cell-walls fila (1- columna)))
	      (and (<= celda_actual 7) (not (logbitp 1 celda_siguiente))))
	     (T nil)))
      (:noroeste 
       (cond ((and (>= (1- fila) 0) (>= (1- columna) 0))
	      (setq celda_siguiente (get-cell-walls (1- fila) (1- columna)))
	      (and (not (eql (logbitp 0 celda_actual) (logbitp 2 celda_siguiente)))
		   (not (eql (logbitp 3 celda_actual) (logbitp 1 celda_siguiente)))
		   (not (eql (logbitp 0 celda_actual) (logbitp 3 celda_actual)))
		   (not (eql (logbitp 1 celda_siguiente) (logbitp 2 celda_siguiente)))))
	     (T nil)))
      (:noreste 
       (cond ((and (>= (1- fila) 0) (< (1+ columna) (nth 1 tamanio_laberinto)))
	      (setq celda_siguiente (get-cell-walls (1- fila) (1+ columna)))
	      (and (not (eql (logbitp 0 celda_actual) (logbitp 2 celda_siguiente)))
		   (not (eql (logbitp 1 celda_actual) (logbitp 3 celda_siguiente)))
		   (not (eql (logbitp 0 celda_actual) (logbitp 1 celda_actual)))
		   (not (eql (logbitp 2 celda_siguiente) (logbitp 3 celda_siguiente)))))
	     (T nil)))
      (:suroeste 
       (cond ((and (< (1+ fila) (nth 0 tamanio_laberinto)) (>= (1- columna) 0))
	      (setq celda_siguiente (get-cell-walls (1+ fila) (1- columna)))
	      (and (not (eql (logbitp 2 celda_actual) (logbitp 0 celda_siguiente)))
		   (not (eql (logbitp 3 celda_actual) (logbitp 1 celda_siguiente)))
		   (not (eql (logbitp 2 celda_actual) (logbitp 3 celda_actual)))
		   (not (eql (logbitp 0 celda_siguiente) (logbitp 1 celda_siguiente)))))
	     (T nil)))
      (:sureste 
       (cond ((and (< (1+ fila) (nth 0 tamanio_laberinto)) (< (1+ columna) (nth 1 tamanio_laberinto)))
	      (setq celda_siguiente (get-cell-walls (1+ fila) (1+ columna)))
	      (and (not (eql (logbitp 2 celda_actual) (logbitp 0 celda_siguiente)))
		   (not (eql (logbitp 1 celda_actual) (logbitp 3 celda_siguiente)))
		   (not (eql (logbitp 1 celda_actual) (logbitp 2 celda_actual)))
		   (not (eql (logbitp 0 celda_siguiente) (logbitp 3 celda_siguiente)))))
	     (T nil)))
      (T nil))))

;;; Evalua si un [estado] es valido
;;; al verificar que los indices de la celda actual se encuentran dentro de los rangos del laberinto
(defun aplicar-operador (operador estado)
  (let* ((fila (aref estado 0))
	 (columna (aref estado 1)))
    (case (first operador)
      (:norte
       (make-array '(2) :initial-contents (list (1- fila) columna)))
      (:noreste 
	(make-array '(2) :initial-contents (list (1- fila) (1+ columna))))
      (:este
       (make-array '(2) :initial-contents (list fila (1+ columna))))
      (:sureste 
	(make-array '(2) :initial-contents (list (1+ fila) (1+ columna))))
      (:sur
	(make-array '(2) :initial-contents (list (1+ fila) columna)))
      (:suroeste 
	(make-array '(2) :initial-contents (list (1+ fila) (1- columna))))
      (:oeste
	(make-array '(2) :initial-contents (list fila (1- columna))))
      (:noroeste 
	(make-array '(2) :initial-contents (list (1- fila) (1- columna))))
      (T nil))))

;;; Expande un estado al aplicarle todos los operadores 
;;; disponibles y validos
(defun expandir-estado ( estado )
  (let ((descendientes nil)
	(nuevo_estado nil))
    (dolist (operador *operadores* descendientes)
      (setq nuevo_estado (aplicar-operador operador estado))
      (when (and (operador-valido? operador estado))
	(setq descendientes (cons (list nuevo_estado operador) descendientes))))))

;;; Verifica si un [estado] ya se encuentra en la lista de [memoria]
;;; La lista de memoria contiene los nodos del arbol por los que ya se 
;;; ha pasado, en cada nodo hay un estado asociado, el cual se compara
;;; con el estado que se pasa como argumento a esta funcion
(defun estado-recordado? ( estado memoria )
  (cond ((null memoria) nil)
	((equalp estado (second (first memoria))) T)
	(T (estado-recordado? estado (rest memoria)))))

;;; Filtra los estados, al descartar aquellos por los que ya se ha pasado
(defun filtrar-estados (estados-y-operadores)
  (cond ((null estados-y-operadores) nil)
	((estado-recordado? (first (first estados-y-operadores)) *memoria*)
	 (filtrar-estados (rest estados-y-operadores)))
	(T 
	 (cons (first estados-y-operadores) (filtrar-estados (rest estados-y-operadores))))))

;;; Extrae la solucion del problema a partir del nodo con el estado meta
;;; rastreando los nodos ancestros.
(defun extraer-solucion (nodo)
  (labels ((localizar-nodo (id lista)
	     (cond ((null lista) nil)
		   ((eql id (first (first lista))) (first lista))
		   (T (localizar-nodo id (rest lista))))))
    (let ((nodo_actual (localizar-nodo  (first  nodo)  *memoria*)))
      (loop  while  (not (null  nodo_actual))  do
	     (push nodo_actual *solucion*)
	     (setq nodo_actual (localizar-nodo (fourth nodo_actual) *memoria*))))
    *solucion*))

(defun crear-lista-movimientos (lista_nodos)
  (let ((nodo nil)
	(solucion nil))
    (dotimes (i (length lista_nodos) solucion)
      (setq nodo (nth i lista_nodos))
      (if (/= i 0)
	  (setq solucion (append solucion (list (second (third nodo)))))))))

(defun inicializar ()
  (setq *frontera* nil)
  (setq *frontera_con_prioridad* (make-instance 'cl-heap:priority-queue))
  (setq *memoria* nil)
  (setq *id* 0)
  (setq *ancestro_actual* nil)
  (setq *solucion* nil)
  (setq *xMeta* nil)
  (setq *yMeta* nil))

(add-algorithm 'busqueda-en-profundidad)
(add-algorithm 'mejor-aptitud)
(add-algorithm 'a-estrella)

(defun busqueda-en-profundidad ()
  (inicializar)
  (let ((nodo nil)
	(estado nil)
	(sucesores nil)
	(operador nil)
	(meta_encontrada nil)
	(solucion nil))
    (agregar-a-frontera *start* nil ':busqueda-en-profundidad)
    (loop until (or meta_encontrada (null *frontera*)) do
	  (setq nodo (sacar-de-frontera))
	  (setq estado (second nodo))
	  (setq operador (third nodo))
	  (push nodo *memoria*)
	  (cond ((equalp estado *goal*) 
		 (setq solucion (crear-lista-movimientos (extraer-solucion nodo)))
		 (setq *solution* solucion)
		 (setq meta_encontrada t))
		(T (setq *ancestro_actual* (first nodo))
		   (setq sucesores (expandir-estado estado))
		   (setq sucesores (filtrar-estados sucesores))
		   (loop for sucesor in sucesores do
			 (agregar-a-frontera (first sucesor) (second sucesor) ':busqueda-en-profundidad)))))))

;;; Calcula la aptitud de un estado, considerando la distancia manhatan 
;;; entre la posicion del [estado] y la posicion del [estado_meta].
(defun calcular-aptitud (estado)
  (let ((xi (aref estado 0))
	(yi (aref estado 1)))
    (+ (abs (- xi *xMeta*)) (abs (- yi *yMeta*)))))

(defun mejor-aptitud ()
  (inicializar)
  (let ((nodo nil)
	(estado nil)
	(aptitud nil)
	(sucesores nil)
	(operador nil)
	(meta_encontrada nil)
	(solucion nil))
    (setq *xMeta* (aref *goal* 0))
    (setq *yMeta* (aref *goal* 1))
    (agregar-a-frontera *start* nil ':sin-orden 0)
    (loop until (or meta_encontrada (= (cl-heap:queue-size *frontera_con_prioridad*) 0)) do
	  (setq nodo (cl-heap:dequeue *frontera_con_prioridad*))
	  (setq estado (second nodo))
	  (setq operador (third nodo))
	  (push nodo *memoria*)
	  (cond ((equalp estado *goal*) 
		 (setq solucion (crear-lista-movimientos (extraer-solucion nodo)))
		 (setq *solution* solucion)
		 (print solucion)
		 (setq meta_encontrada t))
		(T (setq *ancestro_actual* (first nodo))
		   (setq sucesores (expandir-estado estado))
		   (setq sucesores (filtrar-estados sucesores))
		   (loop for sucesor in sucesores do
			 (setq aptitud (calcular-aptitud (first sucesor)))
			 (agregar-a-frontera (first sucesor) (second sucesor) ':sin-orden aptitud)))))))

(defparameter tiempo (get-internal-run-time))
(mejor-aptitud)
(setq tiempo (- (get-internal-run-time) tiempo))
(print tiempo)
(start-maze)
