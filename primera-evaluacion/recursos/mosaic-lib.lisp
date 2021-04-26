;(load "/home/fia1/quicklisp/setup.lisp")
(defvar *encode* nil)

;(ql:quickload :cl-json)
(defclass table nil 
  (( data 
     :initarg 
     :data
     :initform (make-array '(1 1)))
   ( rows_Len 
     :initarg :rows_Len
     :initform '(2 2 2))
   ( rows_Num 
     :initarg :rows_Num
     :initform 3)))

(defclass pieces nil 
  (( data
     :initarg :data
     :initform '(1 1))
   ( Num
     :initarg :Num
     :initform 0)))

(load "table-2.lisp")
(load "pieces-total.lisp")

(defvar *algorithms-list* nil)
(defvar *swapped* nil)
(defvar *solution* nil)
(defvar *exec_time* 100000)
(defvar *num_algorithm* 0)
(defvar *num_Tablero* 0)
(defvar *num_Pieces* 0)
(defvar *table* table-2)
(defvar *pieces* pieces-total)
;(defvar pieces-total nil)
;(defvar table-2 nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun load-problem()
;	(load "pieces-total.lisp")
;	(setq *pieces* pieces-total)
;	(load "table-2.lisp")
;	(setq *table* table-2)
;)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;get-pieces
(defun get-pieces ()
  (slot-value *pieces* 'data))

(defun get-pieces-data ()
  (slot-value *pieces* 'data))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;get-board
(defun get-board ()
  ;Obtiene los datos del tablero
  (slot-value *table* 'data))

(defun get-table-data ()
  ;Obtiene los datos del tablero
  (slot-value *table* 'data))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-table-rows-len ()
;Regresa una lista con el munero de filas
  (slot-value *table* 'rows_Num))

(defun get-table-rows ()
;Regresa una lista con la dimencion de cada una de las filas
  (slot-value *table* 'rows_Len))
;get-piece-info
(defun get-piece-info (id) 
  (write (second(find id (get-pieces) :key #'first :test #'equalp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;get-proximity-info
(defun get-proximity-info (id) 
  (write (remove 0 (second (aref (get-table-data) (- id 1))))))

(defun get-adjacent (id)
	(write (remove 0 (second (aref (get-table-data) (- id 1))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;get-adjacent-side
(defun get-adjacent-side (id adj) 
  (write (nth (- adj 1) (second (aref (get-table-data) (- id 1))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-neighbors-piece (x y)
  (let ((RL (slot-value *table* 'rows_Len)) (RN (slot-value *table* 'rows_Num)))
    (cond ((and  (>= x 0) (< x RN))
	   (cond ((and (>= y 0) (< y (nth x RL)))
		  (aref (get-table-data) x y))
		 (t (error "Coordenadas fuera de las dimensiones del tablero."))))
	  (t (error "Coordenadas fuera de las dimensiones del tablero.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro add-algorithm (algoritmo)
  ;Añade un algoritmo a ejecutar.
  `(setq *algorithms-list* (append *algorithms-list* (list ,algoritmo))))

(defun get-pieces()
	(slot-value *pieces* 'data))

(defun get-piece-id(x)
	(find x (get-pieces) :key #'car :from-end t))

(defun get-piece-num(x)
	(nth x (slot-value *pieces* 'data)))

(defun exec_algorithm (num)
  ;Ejecuta el algoritmo y mide el tiempo que tardó en ejecutarse.
  (let ((t_inicio (get-internal-real-time)))
    ;(setq *num_algorithm* num)
    (funcall (read-from-string num));(funcall (nth num *algorithms-list*))  
    (setq *exec_time* (- (get-internal-real-time) t_inicio))))

(defun start-table ()
  ;Función para procesar la línea de comandos.
  (loop for k from 1 below (length *posix-argv*) do
	(eval (read-from-string (nth k *posix-argv*)))))
