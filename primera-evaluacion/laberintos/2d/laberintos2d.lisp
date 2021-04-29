;;; laberintos2d.lisp
;;; Resuelve el problema de encontrar la salida en un laberinto a partir del estado inicial
;;; 
;;; Representacion de los estados
;;;    Un estado se representa como un arreglo con 2 elementos #(X Y) que denota la posicion actual 
;;;    X -> fila
;;;    Y -> columna
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

(defvar *operadores* '((:norte    0)
		       (:noreste  1)
		       (:este     2)
		       (:sureste  3)
		       (:sur      4)
		       (:suroeste 5)
		       (:oeste    6)
		       (:noroeste 7)))

;;; Los nodos del programa son una lista cuyos elementos son los siguientes.
;;; id -> Identificador del nodo
;;; estado -> Arreglo con 2 numeros #(X Y), donde X es la fila & Y es la columna.
;;; operador -> El operador que se aplico al estado en el nodo ancestro para llegar a este estado
;;; *ancestro_actual* -> identificador del nodo ancestro a este
;;; evaluacion -> Puede corresponder a la aptitud del estado o al costo real del estado
(defun crear-nodo (estado operador evaluacion)
  (incf *id*)
  (list *id* estado operador *ancestro_actual* evaluacion))

;;; agrega un nodo a la frontera de busqueda
;;; 
;;; Para los 3 algoritmos, la forma de agregar un nodo a la frontera de busqueda es la misma, 
;;; sin embargo, para los algoritmos bfs y a* los nodos deberian insertarse en la posicion cuyo orden sea el correcto.
;;; 
;;; Dado que no se esta trabajando con una cola con prioridad en la estructura *frontera*,
;;; los nodos se insertan sin importar la posicion, pero mas adelante esta la funcion que extrae el nodo con mejor aptitud.
;;;
;;; El parametro opcional llamado evaluacion, puede corresponder a dos valores
;;; Aptitud -> Para el algoritmo best-first-search la aptitud es la distancia manhatan entre el [estado] y 
;;;            el estado meta
;;; Costo-real -> Para el algoritmo a*, el costo real denota la aptitud del [estado] + el costo MINIMO con el que
;;;               se llega a ese [estado]
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

;;; Crea un nuevo estado a partir del [estado] y el [operador]
;;; El operador determina la posicion del estado a crear.
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
  (setq *id* 0)
  (setq *ancestro_actual* nil)
  (setq *solucion* nil)
  (setq *xMeta* nil)
  (setq *yMeta* nil))

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
		   (setq sucesores (filtrar-estados-recordados sucesores))
		   (loop for sucesor in sucesores do
			 (agregar-a-frontera (first sucesor) (second sucesor) ':busqueda-en-profundidad)))))))


;;; Esta funcion retorna el nodo con mejor evaluacion, ya sea aptitud para el algoritmo
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
	    ((< (nth 4 nodo) (nth 4 menor))
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
(defun calcular-aptitud (estado)
  (let ((xi (aref estado 0))
	(yi (aref estado 1)))
    (+ (abs (- xi *xMeta*)) (abs (- yi *yMeta*)))))

(defun mejor-aptitud()
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
    (loop until (or meta_encontrada (null *frontera*)) do
	  (setq nodo (sacar-nodo-con-mejor-evaluacion))
	  (setq estado (second nodo))
	  (setq operador (third nodo))
	  (setq *frontera* (delete nodo *frontera*))
	  (push nodo *memoria*)
	  (cond ((equalp estado *goal*) 
		 (setq solucion (crear-lista-movimientos (extraer-solucion nodo)))
		 (setq *solution* solucion)
		 (print (length solucion))
		 (setq meta_encontrada t))
		(T (setq *ancestro_actual* (first nodo))
		   (setq sucesores (expandir-estado estado))
		   (setq sucesores (filtro-mejor-aptitud sucesores))
		   (loop for sucesor in sucesores do
			 (setq aptitud (calcular-aptitud (first sucesor)))
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
	(costo_real nil))
    (setq *xMeta* (aref *goal* 0))
    (setq *yMeta* (aref *goal* 1))
    (agregar-a-frontera *start* nil ':sin-orden 0)
    (loop until (or meta_encontrada (null *frontera*)) do
	  (setq nodo (sacar-nodo-con-mejor-evaluacion))
	  (setq estado (second nodo))
	  (setq operador (third nodo))
	  (setq *frontera* (delete nodo *frontera*))
	  (push nodo *memoria*)
	  (cond ((equalp estado *goal*) 
		 (setq solucion (crear-lista-movimientos (extraer-solucion nodo)))
		 (setq *solution* solucion)
		 (print (length solucion))
		 (setq meta_encontrada t))
		(T (setq *ancestro_actual* (first nodo))
		   (setq sucesores (expandir-estado estado))
		   (setq sucesores (filtrar-estados-recordados sucesores))
		   (loop for sucesor in sucesores do
			 (setq costo_real (+ *ancestro_actual* (calcular-aptitud (first sucesor))))
			 (if (pasa-filtro-a-estrella? (first sucesor) costo_real)
			     (agregar-a-frontera (first sucesor) (second sucesor) ':sin-orden costo_real))))))))

(start-maze)
