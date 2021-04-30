;;; laberinto3d.lisp
;;; Resuelve el problema de encontrar la salida en un laberinto a partir del estado inicial
;;; 
;;; Representacion de los estados
;;;    Un estado se representa como un arreglo con 2 elementos #(X Y) que denota la posicion actual 
;;;    X -> fila
;;;    Y -> columna
;;;    Nivel -> 0 cuando este debajo de un puente, 1 cuando se pasa por encima de un puente
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

;;; Crea un nodo a partir del [estado] [operador] y [evaluacion] y lo agrega a la frontera de busqueda.
;;; Como en laberintos 2D, los nodos se agregan a la frontera en cualquier orden, sin embargo, para los
;;; algoritmos bfs y a* deberian insertarse en la posicion correcta.
;;; Esto no afecta el funcionamiento de los algoritmos pues mas adelante esta escrita la funcion que retorna el nodo
;;; con mejor evaluacion.
(defun agregar-a-frontera (estado operador metodo &optional (evaluacion 0))
  (let ((nodo (crear-nodo estado operador evaluacion)))
    (cond ((eql metodo :busqueda-en-profundidad)
	   (push nodo *frontera*))
	  ((eql metodo :sin-orden)
	   (push nodo *frontera*)))))

;;; Saca de la frontera el siguiente nodo cuyo estado sera expandido, esta funcion solo 
;;; es utilizada en el algoritmo busqueda-en-profundidad
(defun sacar-de-frontera ()
  (pop *frontera*))

;;; Verifica si es posible aplicar el [operador] al [estado].
;;; No todos los operadores se pueden aplicar a todos los estados, solo bajo ciertas condiciones es posible
;;; realizarlo y esta funcion realiza esa evaluacion.
;;; Es muy similar al codigo de la funcion de laberintos 2d, solo que ahora se considera el caso donde se pasara 
;;; por encima o por debajo de un puente.
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
			(not (logbitp 2 celda_siguiente))))
		    ((= celda_actual 17)
		     (if (= nivel 0)
			 nil
			 (not (logbitp 2 celda_siguiente))))
		    (T (and (or (evenp celda_actual) (> celda_actual 15)) (not (logbitp 2 celda_siguiente))))))
	     (T nil))) 
      (:sur
       (cond ((< (1+ fila) (nth 0 tamanio_laberinto))
	      (setq celda_siguiente (get-cell-walls (1+ fila) columna))
	      (cond ((= celda_actual 16)
		     (if (= nivel 1)
			 nil
			 (not (logbitp 0 celda_siguiente))))
		    ((= celda_actual 17)
		     (if (= nivel 0)
			 nil
			 (not (logbitp 0 celda_siguiente))))
		    (T (and (not (logbitp 2 celda_actual)) (or (evenp celda_siguiente) (> celda_siguiente 15))))))
	     (T nil)))
      (:este 
       (cond ((< (1+ columna) (nth 1 tamanio_laberinto))
	      (setq celda_siguiente (get-cell-walls fila (1+ columna)))
	      (cond ((= celda_actual 16)
		     (if (= nivel 0)
			 nil
			 (not (logbitp 3 celda_siguiente))))
		    ((= celda_actual 17)
		     (if (= nivel 1)
			 nil
			 (not (logbitp 3 celda_siguiente))))
		    (T (and (not (logbitp 1 celda_actual)) (or (<= celda_siguiente 7) (> celda_siguiente 15)) ))))
	     (T nil)))
      (:oeste
       (cond ((>= (1- columna) 0)
	      (setq celda_siguiente (get-cell-walls fila (1- columna)))
	      (cond ((= celda_actual 16)
		     (if (= nivel 0)
			 nil
			 (not (logbitp 1 celda_siguiente))))
		    ((= celda_actual 17)
		     (if (= nivel 1)
			 nil
			 (not (logbitp 1 celda_siguiente))))
		    (T (and (or (<= celda_actual 7) (> celda_actual 15)) (not (logbitp 1 celda_siguiente))))))
	     (T nil)))
      (T nil))))

;;; Aplica el [operador] al [estado] y regresa el estado resultado.
;;; Esta funcion tambien dista en ciera manera de la de laberintos 2D, los estados ahora tienen un tercer elemento,
;;; por esta razon, hay que tener ciertas consideraciones.
;;; Los elementos X & Y del estado cambian de la misma manera.
;;; El elemento NIVEL solo cambia cuando la siguiente celda tiene un paso desnivel.
;;; Si se pasa por arriba NIVEL es 1 y si se pasa por debajo NIVEL es 0
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

;;; Expande un estado al aplicarle todos los operadores validos al [estado] 
(defun expandir-estado ( estado )
  (let ((descendientes nil)
	(nuevo_estado nil))
    (dolist (operador *operadores* descendientes)
      (setq nuevo_estado (aplicar-operador operador estado))
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
;;; Retorna la lista con los nodos que se siguieron hasta llegar al estado meta
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
  (setq *id* 0))

(add-algorithm 'busqueda-en-profundidad)
(add-algorithm 'mejor-aptitud)
(add-algorithm 'a-estrella)

;;; Algoritmo general de busqueda en profundidad
(defun busqueda-en-profundidad ()
  (inicializar)
  (let ((nodo nil)
	(estado nil)
	(sucesores nil)
	(operador nil)
	(estado_inicial (make-array '(3) :initial-contents (list (aref *start* 0) (aref *start* 1) 0)))
	(estado_meta (make-array '(3) :initial-contents (list (aref *goal* 0) (aref *goal* 1) 0)))
	(meta_encontrada nil)
	(solucion nil))
    (agregar-a-frontera estado_inicial nil ':busqueda-en-profundidad)
    (loop until (or meta_encontrada (null *frontera*)) do
	  (setq nodo (sacar-de-frontera))
	  (setq estado (second nodo))
	  (setq operador (third nodo))
	  (push nodo *memoria*)
	  (cond ((equalp estado estado_meta) 
		 (setq solucion (crear-lista-movimientos (extraer-solucion nodo)))
		 (setq *solution* solucion)
		 (setq meta_encontrada t))
		(T (setq *ancestro_actual* (first nodo))
		   (setq sucesores (expandir-estado estado))
		   (setq sucesores (filtrar-estados-recordados sucesores))
		   (loop for sucesor in sucesores do
			 (agregar-a-frontera (first sucesor) (second sucesor) ':busqueda-en-profundidad)))))))


;;; Esta funcion retorna el nodo cuyo estado tiene mejor evaluacion, ya sea aptitud para el algoritmo
;;; best-first-search o costo real para el algoritmo a-estrella.

;;; La mejor evaluacion para best-first-search es la menor aptitud, es decir, el nodo con 
;;; el estado mas proximo al estado meta.
;;;
;;; La mejor evaluacion para a-estrella es el menor costo real, es decir, el nodo con el estado
;;; mas proximo al estado meta y con un menor numero de operaciones necesarias para llegar a el.

;;; Cabe mencionar que esta funcion se podria sustituir si se usara una cola con prioridad
(defun sacar-nodo-con-mejor-evaluacion ()
  (let ((menor nil)
	(nodo nil))
    (dotimes (i (length *frontera*) menor)
      (setq nodo (nth i *frontera*))
      (cond ((= i 0)
	     (setq menor nodo))
	    ((<= (nth 4 nodo) (nth 4 menor))
	     (setq menor nodo))))))

;;; Busca el [estado] en la [frontera], en caso de encontrarlo regresa el nodo que
;;; lo contiene, si no lo encuentra devuelve nil.
;;; Esta funcion es utilizada por el filtro para el algoritmo de mejor-aptitud y
;;; por el filtro para el algoritmo a-estrella
(defun estado-en-frontera? ( estado frontera )
  (cond ((null frontera) nil)
	((equalp estado (second (first frontera))) 
	 (first frontera))
	(T
	 (estado-en-frontera? estado (rest frontera)))))

;;; Filtra los estados, al descartar aquellos que ya se encuentran en la frontera de busqueda o en la 
;;; lista de memoria
(defun filtro-mejor-aptitud ( estados-y-operadores )
  (cond ((null estados-y-operadores) nil)
	((or (estado-recordado? (first (first estados-y-operadores)) *memoria*) 
	     (not (null (estado-en-frontera? (first (first estados-y-operadores)) *frontera*))))
	 (filtro-mejor-aptitud (rest estados-y-operadores)))
	(T
	 (cons (first estados-y-operadores) (filtro-mejor-aptitud (rest estados-y-operadores))))))

;;; Calcula la aptitud de un estado, considerando la distancia manhatan 
;;; entre la posicion del [estado] y la posicion del [estado_meta].
(defun calcular-aptitud (estado estado_meta)
  (let ((xActual (aref estado 0))
	(yActual (aref estado 1))
	(xMeta (aref estado_meta 0))
	(yMeta (aref estado_meta 1)))
    (+ (abs (- xActual xMeta)) (abs (- yActual yMeta)))))

(defun mejor-aptitud()
  (inicializar)
  (let ((nodo nil)
	(estado nil)
	(aptitud nil)
	(sucesores nil)
	(operador nil)
	(meta_encontrada nil)
	(solucion nil)
	(estado_inicial (make-array '(3) :initial-contents (list (aref *start* 0) (aref *start* 1) 0)))
	(estado_meta (make-array '(3) :initial-contents (list (aref *goal* 0) (aref *goal* 1) 0))))
    (agregar-a-frontera estado_inicial nil ':sin-orden 0)
    (loop until (or meta_encontrada (null *frontera*)) do
	  (setq nodo (sacar-nodo-con-mejor-evaluacion))
	  (setq estado (second nodo))
	  (setq operador (third nodo))
	  (setq *frontera* (delete nodo *frontera*))
	  (push nodo *memoria*)
	  (cond ((equalp estado estado_meta) 
		 (setq solucion (crear-lista-movimientos (extraer-solucion nodo)))
		 (setq *solution* solucion)
		 (setq meta_encontrada t))
		(T (setq *ancestro_actual* (first nodo))
		   (setq sucesores (expandir-estado estado))
		   (setq sucesores (filtro-mejor-aptitud sucesores))
		   (loop for sucesor in sucesores do
			 (setq aptitud (calcular-aptitud (first sucesor) *goal*))
			 (agregar-a-frontera (first sucesor) (second sucesor) ':sin-orden aptitud)))))))

;(mejor-aptitud)
;;; Si existe un nodo en la frontera de busauqeda con el mismo estado que el [estado] argumento
;;; verifica cual tiene mejor costo_real.
;;;
;;; En caso de que no exista el nodo, se regresa t, que indica que es posible agregarlo a la frontera.

;;; En caso de que exista el nodo, se verifica cual tiene mejor costo, si el nuevo estado tiene mejor costo se elimina el
;;; nodo de la frontera y se devuelve t, que denota que el nuevo estado puede agregarse.

;;; En caso de que no tenga mejor costo, se deja el nodo de la frontera y se descarta el nuevo estado.
(defun pasa-filtro-a-estrella? ( estado costo_real )
  (let* ((nodo (estado-en-frontera? estado *frontera*)))
    (cond ((null nodo) t) 
	  ((< costo_real (fifth nodo))
	   (setq *frontera* (delete nodo *frontera*))
	   t)
	  (T nil))))

(defun a-estrella()
  (inicializar)
  (let ((nodo nil)
	(estado nil)
	(sucesores nil)
	(operador nil)
	(meta_encontrada nil)
	(solucion nil)
	(costo_real nil)
	(estado_inicial (make-array '(3) :initial-contents (list (aref *start* 0) (aref *start* 1) 0)))
	(estado_meta (make-array '(3) :initial-contents (list (aref *goal* 0) (aref *goal* 1) 0))))
    (agregar-a-frontera estado_inicial nil ':sin-orden 0)
    (loop until (or meta_encontrada (null *frontera*)) do
	  (setq nodo (sacar-nodo-con-mejor-evaluacion))
	  (setq estado (second nodo))
	  (setq operador (third nodo))
	  (push nodo *memoria*)
	  (format t "~%Nodo: ~A~%~%" nodo)
	  (setq *frontera* (delete nodo *frontera*))
	  (cond ((equalp estado estado_meta) 
		 (setq solucion (crear-lista-movimientos (extraer-solucion nodo)))
		 (setq *solution* solucion)
		 (setq meta_encontrada t))
		(T (setq *ancestro_actual* (first nodo))
		   (setq sucesores (expandir-estado estado))
		   (setq sucesores (filtrar-estados-recordados sucesores))
		   (format t "Sucesores filtrados: ~A~%" sucesores)
		   (loop for sucesor in sucesores do
			 (setq costo_real (+ *ancestro_actual* (calcular-aptitud (first sucesor) *goal*)))
			 (format t "Sucesor: ~A pasa filtros: ~A~%" sucesor (pasa-filtro-a-estrella? (first sucesor) costo_real))
			 (if (pasa-filtro-a-estrella? (first sucesor) costo_real)
			     (agregar-a-frontera (first sucesor) (second sucesor) ':sin-orden costo_real))))))))

(a-estrella)
(start-maze)
