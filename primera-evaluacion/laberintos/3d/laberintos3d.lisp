;;; laberinto3d.lisp
;;; Resuelve el problema de encontrar la salida en un laberinto a partir del estado inicial
;;; 
;;; Representacion de los estados
;;;    Un estado se representa como un arreglo con 2 elementos #(X Y) que denota la posicion actual 
;;;    X -> fila
;;;    Y -> columna
;;;    Nivel -> 0 cuando este debajo de un puente, 1 arriba de un puente
;;; 
;;; Hernandez Escudero Luis Hugo
(load "maze_lib.lisp")

(defparameter *frontera* nil)
(defparameter *solucion* nil)
(defparameter *memoria* nil)
(defparameter *ancestro_actual* nil)
(defparameter *id* -1)
(defparameter *xMeta* nil)
(defparameter *yMeta* nil)
(defparameter *nivel* 0)

(defvar *operadores* '((:norte    0)
		       (:este     2)
		       (:sur      4)
		       (:oeste    6)))

;;; Los nodos son listas con los siguientes elementos.
;;; Id -> identificador del nodo
;;; El [estado] denota la posicion en el laberinto
;;; El [operador] es el que fue aplicado al nodo ancestro para llegar a este estado
;;; El identificador del ancestro
;;; La [evaluacion] denota el costo_real o la aptitud asociada al [estado]
(defun crear-nodo (estado operador evaluacion)
  (incf *id*)
  (list *id* estado operador *ancestro_actual* evaluacion))

;;; Crea un nodo a partir del [estado] [operador] y [evaluacion] y lo agrega a la frontera de busqueda 
(defun agregar-a-frontera (estado operador metodo &optional (evaluacion 0))
  (let ((nodo (crear-nodo estado operador evaluacion)))
    (cond ((eql metodo :busqueda-en-profundidad)
	   (push nodo *frontera*))
	  ((eql metodo :sin-orden)
	   (push nodo *frontera*)))))

(defun sacar-de-frontera ()
  (pop *frontera*))

(defun operador-valido? (operador estado)
  (let* ((fila (aref estado 0))
	 (columna (aref estado 1))
	 (nivel (aref estado 2))
	 (celda_actual (get-cell-walls fila columna))
	 (celda_siguiente nil)
	 (tamanio_laberinto (array-dimensions (get-maze-data))))
    (case (first operador)
      (:norte
       (cond ((>= (1- fila) 0)
	      (setq celda_siguiente (get-cell-walls (1- fila) columna))
	      (cond ((= celda_actual 16)
		     (if (= nivel 1)
			 nil
			 (or (= celda_siguiente 17)(not (logbitp 2 celda_siguiente)))))
		    ((= celda_actual 17)
		     (if (= nivel 0)
			 nil
			 (or (= celda_siguiente 16) (not (logbitp 2 celda_siguiente)))))
		    (T (and (or (evenp celda_actual) (> celda_actual 15)) (not (logbitp 2 celda_siguiente))))))
	     (T nil))) 
      (:sur
       (cond ((< (1+ fila) (nth 0 tamanio_laberinto))
	      (setq celda_siguiente (get-cell-walls (1+ fila) columna))
	      (cond ((= celda_actual 16)
		     (if (= nivel 1)
			 nil
			 (or (= celda_siguiente 17) (not (logbitp 0 celda_siguiente)))))
		    ((= celda_actual 17)
		     (if (= nivel 0)
			 nil
			 (or (= celda_siguiente 16) (not (logbitp 0 celda_siguiente)))))
		    (T (and (not (logbitp 2 celda_actual)) (or (evenp celda_siguiente) (> celda_siguiente 15))))))
	     (T nil)))
      (:este 
       (cond ((< (1+ columna) (nth 1 tamanio_laberinto))
	      (setq celda_siguiente (get-cell-walls fila (1+ columna)))
	      (cond ((= celda_actual 16)
		     (if (= nivel 0)
			 nil
			 (or (= celda_siguiente 17) (not (logbitp 3 celda_siguiente)))))
		    ((= celda_actual 17)
		     (if (= nivel 1)
			 nil
			 (or (= celda_siguiente 16) (not (logbitp 3 celda_siguiente)))))
		    (T (and (not (logbitp 1 celda_actual)) (or (<= celda_siguiente 7) (> celda_siguiente 15)) ))))
	     (T nil)))
      (:oeste
       (cond ((>= (1- columna) 0)
	      (setq celda_siguiente (get-cell-walls fila (1- columna)))
	      (cond ((= celda_actual 16)
		     (if (= nivel 0)
			 nil
			 (or (> celda_siguiente 15) (not (logbitp 1 celda_siguiente)))))
		    ((= celda_actual 17)
		     (if (= nivel 1)
			 nil
			 (or (> celda_siguiente 15) (not (logbitp 1 celda_siguiente)))))
		    (T (and (or (<= celda_actual 7) (> celda_actual 15)) (not (logbitp 1 celda_siguiente))))))
	     (T nil)))
      (T nil))))

;;; Aplica el [operador] al [estado] y regresa el estado resultado
(defun aplicar-operador (operador estado)
  (let* ((fila (aref estado 0))
	 (columna (aref estado 1))
	 (siguiente_nivel nil)
	 (celda_siguiente nil)
 	 (tamanio_laberinto (array-dimensions (get-maze-data))))
    (case (first operador)
      (:norte
       (cond ((>= (1- fila) 0)
	      (setq celda_siguiente (get-cell-walls (1- fila) columna))
	      (cond ((= celda_siguiente 16)
		     (setq siguiente_nivel 0))
		    ((= celda_siguiente 17)
		     (setq siguiente_nivel 1))
		    (t (setq siguiente_nivel 0))))
	     (t (setq siguiente_nivel 0)))
       (make-array '(3) :initial-contents (list (1- fila) columna siguiente_nivel)))
      (:sur
       (cond ((< (1+ fila) (nth 0 tamanio_laberinto))
	      (setq celda_siguiente (get-cell-walls (1+ fila) columna))
	      (cond ((= celda_siguiente 16)
		     (setq siguiente_nivel 0))
		    ((= celda_siguiente 17)
		     (setq siguiente_nivel 1))
		    (t (setq siguiente_nivel 0))))
	     (t (setq siguiente_nivel 0)))
       (make-array '(3) :initial-contents (list (1+ fila) columna siguiente_nivel)))
      (:este
       (cond ((< (1+ columna) (nth 1 tamanio_laberinto))
	      (setq celda_siguiente (get-cell-walls fila (1+ columna)))
	      (cond ((= celda_siguiente 16)
		     (setq siguiente_nivel 1))
		    ((= celda_siguiente 17)
		     (setq siguiente_nivel 0))
		    (t (setq siguiente_nivel 0))))
	     (t (setq siguiente_nivel 0)))
       (make-array '(3) :initial-contents (list fila (1+ columna) siguiente_nivel)))
      (:oeste
	(cond ((>= (1- columna) 0)
	      (setq celda_siguiente (get-cell-walls fila (1- columna)))
	      (cond ((= celda_siguiente 16)
		     (setq siguiente_nivel 1))
		    ((= celda_siguiente 17)
		     (setq siguiente_nivel 0))
		    (t (setq siguiente_nivel 0))))
	     (t (setq siguiente_nivel 0)))
       (make-array '(3) :initial-contents (list fila (1- columna) siguiente_nivel)))
      (T nil))))

;;; Expande un estado al aplicarle todos los operadores 
;;; disponibles y validos
(defun expandir-estado ( estado )
  (let ((descendientes nil)
	(nuevo_estado nil))
    (dolist (operador *operadores* descendientes)
      (setq nuevo_estado (aplicar-operador operador estado))
;     (format t "Nuevo estado: ~A  n ~A~%" nuevo_estado (operador-valido? operador estado))
      (when (and (operador-valido? operador estado))
	(setq descendientes (cons (list nuevo_estado operador) descendientes))))))

;;; Verifica si un [estado] ya se encuentra en la lista de [memoria]
(defun estado-recordado? ( estado memoria )
  (cond ((null memoria) nil)
	((equalp estado (second (first memoria))) T)
	(T (estado-recordado? estado (rest memoria)))))

;;; Filtra los estados, al descartar aquellos que ya se encuentran en la lista de memoria.
(defun filtrar-estados-recordados (estados-y-operadores)
  (cond ((null estados-y-operadores) nil)
	((estado-recordado? (first (first estados-y-operadores)) *memoria*)
	 (filtrar-estados-recordados (rest estados-y-operadores)))
	(T 
	 (cons (first estados-y-operadores) (filtrar-estados-recordados (rest estados-y-operadores))))))

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
  (setq *memoria* nil)
  (setq *solucion* nil)
  (setq *ancestro_actual* nil)
  (setq *id* 0)
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
	(estado_inicial (make-array '(3) :initial-contents (list (aref *start* 0) (aref *start* 1) 0)))
	(estado_meta (make-array '(3) :initial-contents (list (aref *goal* 0) (aref *goal* 1) 0)))
	(meta_encontrada nil)
	(solucion nil)
	(i 0))
    (agregar-a-frontera estado_inicial nil ':busqueda-en-profundidad)
    (loop until (or meta_encontrada (null *frontera*)) do
	  (setq nodo (sacar-de-frontera))
	  (setq estado (second nodo))
	  (setq operador (third nodo))
	  (push nodo *memoria*)
	  (cond ((equalp estado estado_meta) 
		 (setq solucion (crear-lista-movimientos (extraer-solucion nodo)))
		 (setq *solution* solucion)
		 (print solucion)
		 (setq meta_encontrada t))
		(T (setq *ancestro_actual* (first nodo))
		   (setq sucesores (expandir-estado estado))
		   (setq sucesores (filtrar-estados-recordados sucesores))
		   (loop for sucesor in sucesores do
			 (agregar-a-frontera (first sucesor) (second sucesor) ':busqueda-en-profundidad)))))))

(start-maze)
