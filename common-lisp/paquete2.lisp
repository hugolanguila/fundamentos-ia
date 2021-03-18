;1
(defun ElemInPos (elemento lista posicion)
  (cond ((null lista) NIL)
	((>= posicion (length lista)) NIL)
	((equal (nth posicion lista) elemento))
	(T NIL)))
(ElemInPos '5 '(B A  D X -1 11) '3)
(ElemInPos 'W '() '6)
(ElemInPos '(A B) '(B A (7 8) -1 11) '4)
(ElemInPos '(5 6) '(-1 3 2 (5 6) -7 12 33) '3)

;2
(defun inicio-en (lista elemento)
  (let ((resultado '()))
    (dolist (i lista resultado)
      (if (equal elemento i)
	  (setq resultado (append (list i) resultado ))
	  (setq resultado (append resultado (list i)))))))
(inicio-en '(A B C D E F) 'F)
(inicio-en '(-1 2 3 0 -7 16) -7)
(inicio-en '(#\S #\T #\P #\Q 7 7 #\W #\Space 7) 7)
(inicio-en '(-1 2 3 (A C) (P Q 8)) '(A C))

(eq () NIL)

;3
(defun termina-en (lista elemento)
  (let ((resultado '())
	(invertida (reverse lista)))
    (dolist (i invertida resultado)
      (if (equal elemento i)
	  (setq resultado (append resultado (list i)))
	  (setq resultado (append (list i) resultado))))))
(termina-en '(A B C D E F) 'D)
(termina-en '(-1 2 3 0 -7 16) -7) 
(termina-en '(#\S #\T #\P #\Q 7 7 #\W #\Space 7) 7)
(termina-en '(-1 2 3 (A C) (P Q 8)) '(A C))

;4
(defun primer-impar (lista)
  (let ( (resultado '()) (encontrado NIL) )
    (do ((n 0 (+ n 1))
	 (elemento (first lista) (first lista)))
	((or encontrado (null lista)) resultado)
      (if (and (numberp elemento) (= (mod elemento 2) 1))
	  (progn (setq encontrado T)
		 (setq resultado (append resultado (list elemento n)))))
      (setq lista (rest lista))))) 
(primer-impar '(A B C D 10))
(primer-impar '(2 4 6 8 A  #\C 91 16 ))
(primer-impar '(2 4 3 8  #\W #\D #\X #\K ))
(primer-impar '())

;5
(defun ultimo-elemento (lista)
  (let ((resultado '()) 
	(conteo 0)
	(elemento)
	(enesimo))
    (do ((n (- (length lista) 1) (- n 1)))
	((<= n 0) resultado)
      (setq enesimo (nth n lista))
      (cond ((and (realp enesimo) (>= enesimo 0) (= conteo 0))
	    (setq elemento enesimo)
	    (setq conteo 1)
	    (setq resultado (list elemento conteo)))
	   ((and (realp enesimo) (> conteo 0) (eql enesimo elemento))
	    (setq conteo (+ conteo 1))
	    (setq resultado (list elemento conteo))))))) 
(ultimo-elemento '(1 1 4 5 1 (#\A #\W) 3 -5 1 68 -1 3 A 2 7 -3))
(ultimo-elemento '(5 1 () A #\C NIL (X Y)))
(ultimo-elemento '( #\A 10 ()))
(ultimo-elemento '( 2 4 6 8 PI #\A))
(ultimo-elemento '())



;6
(defun conteo (lista)
  (let ((resultado '()) 
	(conteoNumerico 0)
	(conteoSublistas 0))
    (dolist (elemento lista resultado)
      (cond ((numberp elemento)
	    (setq conteoNumerico (+ conteoNumerico elemento))
	    (setq resultado (cons conteoNumerico conteoSublistas)))
	   ((listp elemento)
	    (setq conteoSublistas (1+ conteoSublistas))
	    (setq resultado (cons conteoNumerico conteoSublistas))))))) 
(conteo '(A B 6 -4 7 19 - 11 (C) (D E) 8 9 (#\F #\G)))
(conteo '( (A #\A) ((3)) #\w ()))
(conteo '(-6 98 16.2 -7.7 ))
(conteo '(A B C))

;7

(defun aplana (lista)
  (let ((resultado '()))
    (dolist (elemento lista resultado)
      (cond ( (listp elemento )
	    (setq resultado (append resultado (aplana elemento))))
	    (T (setq resultado (append resultado (list elemento)))))))) 

(defun aplana (lista) 
  (let ((resultado '())
	(elemento))
    (do ((n 0 (1+ n)))
	((= n (length lista)) resultado)
      (setq elemento (nth n lista))
      (cond ((atom elemento)
	     (setq resultado (append resultado (list elemento))))
	    (T
	     (setq resultado (append resultado (aplana elemento))))))))

(trace aplana)
(aplana '(A B C (D E) (F (0 -1)) H I (J K (L (M (7 -7))))))
(aplana '(A B C (D E) (#\1 #\Space) H T (J K (L (M (NIL -7))))))
(aplana '((T NIL) ((NIL '()))))
(length nil)
;8
(defun diagonal (lista)
  (let ((diagonal '())
	(m (length lista))
	(elemento))
    (dolist (sublista lista diagonal)
      (setq elemento (nth (ash m -1) sublista))
      (setq diagonal (append diagonal (list elemento))))))

(diagonal '((A B C) (4 5 6) (NIL T NIL)))

;9---Checar

(primer-nivel '((#\A #\B) C D E (7 -1) NIL '()))

(atom ())
(null nil)
;10
(defun suma-numerica (lista)
  (let ((sumaNumerica 0))
    (dolist (elemento lista sumaNumerica)
      (if (numberp elemento)
	  (setq sumaNumerica (+ sumaNumerica elemento))))))

(suma-numerica '(5 -1 50 -20 8 7 A B C -1 3 5 9 10))

;11
(defun filtra-vocales (lista)
  (let ((listaSinVocales '())
	(vocales '(#\A #\a #\E #\e #\I #\i #\O #\o #\U #\u )))
    (dolist (elemento lista listaSinVocales)
      (cond ((listp elemento)
	     (setq listaSinVocales (append listaSinVocales (list (filtra-vocales elemento)))))
	    ((characterp elemento)
	     (if (not (numberp (position elemento vocales)))
		 (setq listaSinVocales (append listaSinVocales (list elemento)))))
	    (T (setq listaSinVocales (append listaSinVocales (list elemento))))))))

(filtra-vocales '(#\A (5 6) (B (#\E F)) (NIL (T (#\I)) #\O) (5 #\u ())))

;12
(defun filtra-multiplos (lista multiplo)
  (let ((listaSinMultiplos '()))
    (dolist (elemento lista listaSinMultiplos)
      (cond ((numberp elemento)
	     (if (/= (mod elemento multiplo) 0)
		 (setq listaSinMultiplos (append listaSinMultiplos (list elemento)))))))))

(filtra-multiplos '(10 5 2 4 6 8 -1 -6 -5 -1 0 50 11 13 2 7 4 -7) 2)

;13
(defun celdas (lista)
  (let ((numeroDeCeldas (length lista)))
    (dolist (elemento lista numeroDeCeldas)
      (if (listp elemento)
	  (setq numeroDeCeldas (+ numeroDeCeldas (celdas elemento)))))))

(celdas '( (ROJO AZUL) AMARILLO (VERDE GRIS) () (1 2 (3 4 (5 6 ())))))

;14 ?

;15
(defun Mult (A B)
  "Matriz A[m][n], Matriz B[p][q]"
  (let ((n (length (first A)))
	(p (length B))
	(resultado '()))
    (cond ((= n p)
	   (do ((x 0 (1+ x))
		(nuevoRenglon '() '())
		(renglonA))
	       ((= x (length A)) resultado)
	     (setq renglonA (nth x A))
	     (do ((y 0 (1+ y))
		  (suma 0 0))
		 ((= y (length (first B))))
	       (do ((k 0 (1+ k))
		    (renglonB))
		   ((= k (length B)))
		 (setq renglonB (nth k B))
		 (setq suma (+ suma (* (nth k renglonA) (nth y renglonB)))))
	       (setq nuevoRenglon (append nuevoRenglon (list suma))))
	     (setq resultado (append resultado (list nuevoRenglon)))))
	  (T NIL))))
(mult '((1 1) (1 1)) '((1 1 1) (1 1 1)))
(mult '((1 1) (1 1)) '((1 1 1) (1 1 1)))
(mult '((1 1) (1 1) (1 1) (1 1)) '((1 1 1 1) (1 1 1 1)))
(mult '((1 0 0)(0 1 0)(0 0 1)) '((1 1 1)(1 1 1)(1 1 1)))
(mult '((1 1) (1 1)) '((1 1)))