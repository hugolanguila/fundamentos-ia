;1
(defun elem-in-pos (elemento lista posicion)
  (cond ((null lista) NIL)
	((= posicion 0)
	 (if (equal (first lista) elemento) T NIL))
	(T (elem-in-pos elemento (rest lista) (1- posicion)))))
(trace elem-in-pos)
(elem-in-pos '5 '(B A  D X -1 11) '3)
(elem-in-pos 'W '() '6)
(elem-in-pos '(A B) '(B A (7 8) -1 11) '4)
(elem-in-pos '(5 6) '(-1 3 2 (5 6) -7 12 33) '3)

;2
(defun inicio-en-aux (lista elemento resultado encontrado)
  (cond ((null lista) resultado)
	((and (not encontrado) (equal elemento (first lista)))
	 (setq resultado (append (list elemento) resultado))
	 (inicio-en-aux (rest lista) elemento resultado T))
	(T (setq resultado (append resultado (list (first lista))))
	   (inicio-en-aux (rest lista) elemento resultado NIL))))
(defun inicio-en (lista elemento)
  (inicio-en-aux lista elemento () nil))

(inicio-en '(A B C D #\G (1 2) NIL ()) '(1 2))

;3

;4
(defun primer-impar-aux (lista contador)
  (cond ((null lista) (list NIL NIL))
	((and (numberp (first lista)) (oddp (first lista)))
	 (list (first lista) contador))
	(T (primer-impar-aux (rest lista) (1+ contador)))))

(defun primer-impar (lista)
  (primer-impar-aux lista 0))

(primer-impar '(A B C D 10))
(primer-impar '(2 4 6 8 A  #\C 91 16 ))
(primer-impar '(2 4 3 8  #\W #\D #\X #\K ))
(primer-impar '())

;5
(defun ultimo-elemento-aux (lista elemento contador)
  (let ((primero (first lista)))
    (cond ((null lista) (list elemento contador))
	      ((= contador 0)
	       (if (and (numberp primero) (>= primero 0))
		   (ultimo-elemento-aux (rest lista) primero 1)
		   (ultimo-elemento-aux (rest lista) elemento 0)))
	      ((and (numberp primero) (= primero elemento))
	       (ultimo-elemento-aux (rest lista) elemento (1+ contador)))
	      (T (ultimo-elemento-aux (rest lista) elemento contador)))))

(defun ultimo-elemento (lista)
  (ultimo-elemento-aux (reverse lista) 'NoEncontrado 0))

(ultimo-elemento '(1 1 4 5 1 (#\A #\W) 3 -5 1 68 -1 3 A 2 7 -3))
(ultimo-elemento '(5 1 () A #\C NIL (X Y)))
(ultimo-elemento '(#\A 10 ()))
(ultimo-elemento '(A #\A))
(ultimo-elemento '())

;6
(defun conteo-aux (lista conteoElementos conteoSublistas)
  (cond ((null lista)
	 (cons conteoElementos conteoSublistas))
	((numberp (first lista))
	 (conteo-aux (rest lista) (1+ conteoElementos) conteoSublistas))
	((listp (first lista))
	 (conteo-aux (rest lista) conteoElementos (1+ conteoSublistas)))
	(T (conteo-aux (rest lista) conteoElementos conteoSublistas))))
(defun conteo (lista)
  (conteo-aux lista 0 0))

(conteo '(A B 6 -4 7 19 - 11 (C) (D E) 8 9 (#\F #\G)))
(conteo '( (A #\A) ((3)) #\w ()))
(conteo '(-6 98 16.2 -7.7 ))
(conteo '(A B C))

;7
(defun aplana-aux (lista resultado)
  (cond ((null lista) resultado)
	((listp (first lista))
	 (setq resultado (append resultado (aplana-aux (first lista) '())))
	 (aplana-aux (rest lista) resultado))
	(T
	 (setq resultado (append resultado (list (first lista))))
	 (aplana-aux (rest lista) resultado))))

(defun aplana (lista)
  (aplana-aux lista '()))

(trace aplana-aux)
(trace aplana)

(aplana '(A B C (D E) (F (0 -1)) H I (J K (L (M (7 -7))))))
(aplana '(A B C (D E) (#\1 #\Space) H T (J K (L (M (NIL -7))))))
(aplana '((T NIL) ((NIL '()))))
(length nil)
;8
(defun diagonal-aux (lista respuesta m)
  (cond ((null lista) respuesta)
	(T (setq respuesta (append respuesta (list (nth (ash m -1) (first lista)))))
	 (diagonal-aux (rest lista) respuesta m))))

(defun diagonal (lista)
  (diagonal-aux lista () (length lista)))

(diagonal '((A B C) (4 5 6) (NIL T NIL)))

;9
(defun tipos-aux (lista resultado)
  (cond ((null lista) resultado)
	((and (listp (first lista)) (not (null (first lista))))
	 (setq resultado (append resultado (list 'L)))
	 (tipos-aux (rest lista) resultado))
	((and (listp (first lista)) (null (first lista)))
	 (setq resultado (append resultado (list 'N)))
	 (tipos-aux (rest lista) resultado))
	((atom (first lista))
	 (setq resultado (append resultado (list 'A)))
	 (tipos-aux (rest lista) resultado))))

(defun tipos (lista)
  (tipos-aux lista '())) 

(tipos '((#\A #\B) C D E (7 -1) NIL ()))


;10
(defun suma-numerica-aux (lista suma)
  (cond ((null lista) suma)
	((null (rest lista)) suma)
	((numberp (first lista))
	 (setq suma (+ suma (first lista)))
	 (suma-numerica-aux (rest lista) suma))
	(T (suma-numerica-aux (rest lista) suma))))

(defun suma-numerica (lista)
  (suma-numerica-aux lista 0))
(suma-numerica '(5 -1 50 -20 8 7 A B C -1 3 5 9 10))

;11
(defparameter vocales '(#\A #\E #\I #\O #\U #\a #\e #\i #\o #\u))
(defun filtra-vocales-aux (lista acomulado)
  (cond ((null lista) acomulado)
	((listp (first lista))
	 (setq acomulado (append acomulado (list (filtra-vocales-aux (first lista) '()))))
	 (filtra-vocales-aux (rest lista) acomulado))
	((member (first lista) vocales)
	 (filtra-vocales-aux (rest lista) acomulado))
	(T (setq acomulado (append acomulado (list (first lista))))
	   (filtra-vocales-aux (rest lista) acomulado))))

(defun filtra-vocales (lista)
  (filtra-vocales-aux lista '()))

(filtra-vocales '(#\A (5 6) (B (#\E F)) (NIL (T (#\I)) #\O) (5 #\u ())))

;12
(defun filtra-multiplos (lista)
  )
