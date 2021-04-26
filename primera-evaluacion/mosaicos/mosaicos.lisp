(load "mosaic-lib.lisp")

(defparameter *frontera* nil)
(defparameter *memoria* nil)
(defparameter *ancestro_actual* nil)
(defparameter *id* -1)
(defparameter *soluciones_encontradas* 0)
(defparameter *mosaicos* (get-pieces))
(defparameter *tablero* (get-board))

(defparameter *operadores* '((:colocar-mosaico)
			     (:remover-mosaico)
			     (:0-grados     0)
			     (:60-grados   60)
			     (:120-grados 120)
			     (:180-grados 180)
			     (:240-grados 240)
			     (:300-grados 300)
			     (:360-grados 360)))

(defun crear-nodo ( estado operador )
  (incf *id*)
  (list *id* estado operador *ancestro_actual*))
 
(defun agregar-a-frontera( estado operador )
    (let ((nodo (crear-nodo estado operador)))
      (push nodo *frontera*)))

(defun sacar-de-frontera ()
  (pop *frontera*))

(defun rotar (mosaico grados)
  (append (nthcdr (- (length mosaico) grados) mosaico) (butlast mosaico grados)))

(defun rotar-mosaico (mosaico grados)
  (case grados
    (  0 mosaico)
    ( 60 (rotar mosaico 1))
    (120 (rotar mosaico 2))
    (180 (rotar mosaico 3))
    (240 (rotar mosaico 4))
    (300 (rotar mosaico 5))))(defun evaluar (sucesores))

;;;
;;;
;;; Piezas_colocadas, lista con sublistas cuya estructura es la siguiente
;;; ( posicion_tablero identificador_mosaico grados )
;;;
(defun estado-meta? ( piezas_colocadas tablero )
  (let ((estado_valido t)
	(lista_adyacencia nil)
	(mosaico nil)
	(patron nil))
    (cond ((/= (length piezas_colocadas) (length tablero)) nil)
	  (T
	   (dolist (pieza piezas_colocadas estado_valido)
	     (setq lista_adyacencia (get-proximity-info (first pieza)))
	     (setq mosaico (get-piece-info (second pieza)))
	     (setq mosaico (rotar-mosaico mosaico grados))
	     (dotimes (i (length lista_adyacencia))
	       (cond ((null (nth i lista_adyacencia)) nil)
		     (T
		      (setq patron (nth i mosaico))
		      ()))))))))

(defun extraer-identificadores ( mosaicos )
  (cond ((null mosaicos) nil)
	(T (cons (first (first mosaicos)) (extraer-identificadores (rest mosaicos))))))

(defun extraer-posiciones ( tablero )
  (let ((posiciones_tablero nil))
    (dotimes (i (array-total-size tablero) posiciones_tablero)
      (setq posiciones_tablero (append posiciones_tablero (list i))))))

(defun convertir-a-lista (tablero)
  (let ((lista nil))
    (dotimes (i (array-total-size tablero) lista)
      (setq lista (append lista (aref i tablero))))))

(defun inicializar () 
  (setq *frontera* nil)
  (setq *memoria* nil)
  (setq *ancestro_actual* nil)
  (setq *id* -1)
  (setq *soluciones_encontradas* 0)
  (setq *mosaicos_disponibles* (extraer-identificadores (get-pieces)))
  (setq *posiciones_libres* (extraer-posiciones (get-board)))
  (setq *tablero* (convertir-a-lista (get-board))))

(defun a-estrella (mosaicos tablero)
  (inicializar)
  (let ((nodo nil)
	(estado nil)
	(sucesores nil)
	(operador nil))
    (agregar-a-frontera (list mosaicos_disponibles posiciones_libres nil) nil)
    (loop until (or (= *soluciones_encontradas* 2) (null *frontera*)) do
	  (setq nodo (sacar-de-frontera))
	  (setq estado (second nodo))
	  (setq operador (third nodo))
	  (push nodo *memoria*)
	  (cond ((estado-meta? estado *tablero*)
		 (incf *soluciones_encontradas*))
		(T (setq *ancestro_actual* (first nodo))
		   (setq sucesores (expandir-estado estado))
		   (setq sucesores (filtrar-estados sucesores))
		   (setq sucesores (evaluar sucesores))
		   (if ()))))))

(a-estrella (get-pieces) (get-board))
