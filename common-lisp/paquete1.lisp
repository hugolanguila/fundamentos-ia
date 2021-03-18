;1
(first (rest (rest (rest (rest (list '((1 2) 3) '4 '(5 (6)) 'A '(B C) 'D '(E (F G))))))))
(defparameter x '10)
(defparameter y '50)
(and (not (= x 0)) (<= x y))
(* 366 86400)
(list (/ (+ -7 (sqrt (- (expt 7 2) (* 4 2 5)))) (* 2 2)) (/ (- -7 (sqrt (- (expt 7 2) (* 4 2 5)))) (* 2 2)))

;2
(+ (* 2 4) (- 6 8))
(/ (+ 5 (+ -3 4)) (+ 6 (/ 2 5)))
(sqrt (/ (+ (* -1 (- -4 (/ 3 8)) ) 1.4502) (expt -1 (expt (- 3 5 ) (/ 1 3)))))
(expt (/ (expt (/ 65.402 (sqrt -1)) (/ 1 5)) 0.17) (/ 1 7))

;3
(cdar '((one two) three four))
(append (cons '(eva lisa) '(karl sven)) '(eva lisa) '(karl sven))
(subst 'gitan 'birgitta '(eva birgitta lisa birgitta karin))
(remove 'sven '(eva sven lisa sven anna))
(butlast '(karl adam nilsson gregg alisson vilma) 3)
(nth 2 '(a b c d e))
(nthcdr 2 '(a b c d e))
(intersection '(a b c) '(x b z c))
(cdadar '(((((1 2 3) z) y) (x 4)) 7 8 (a b c (5 (6 7 8)))))

;4
(defparameter lista (list (cons 'A 'x) (cons 'B 'y) (cons 'C 'z)))
(defun recombina (lista )
 (list 
   (cons (list (rest (first lista)) (rest (second lista))) (first (first lista)))
   (cons (list (rest (second lista)) (rest (third lista))) (first (third lista)))
   (cons (list (rest (third lista)) (rest (second lista)) (rest (first lista ))) (first (second lista)))))
(recombina lista)

;5
(defun realNoCerop (dato)
  (if  (and (numberp dato) (not (= dato 0)))
      T
      NIL))
(realNoCerop ())
(realNoCerop -1)
(realNoCerop 0)

;6
(defun analiza (x)
  (let ((atomo (atom x))
	(numero (numberp x))
	(listaeval (listp x))
	(celda (consp x))
	(vacia (null x)))
    (list atomo numero listaeval celda vacia )))
(analiza '(hola mundo)) 
(analiza ())
(analiza 5)

;7
(defun intercala (lista1 lista2)
  (let ((resultado '()))
    (do ()
	((and (null lista1) (null lista2)) resultado)
      (if (not (null lista1))
	  (progn (setq resultado (append resultado (list (first lista1))))
		 (setq lista1 (rest lista1))))
      (if (not (null lista2))
	  (progn (setq resultado (append resultado (list (first lista2))))
		 (setq lista2 (rest lista2)))))))

(intercala '(A C E) '(B D F))
(intercala '(1) '(2 3 B))
(intercala '(4 5 6 7) '())

;8
(defun mismo-tipo (lista1 lista2)
  (let ((resultado T)
	(longitud (length lista1)))
      (dotimes (n longitud resultado) 
	(when (not (typep (nth n lista1) (type-of (nth n lista2))))
	  (return-from nil)))))
(mismo-tipo '(5 B C) '(#\c B w))
(mismo-tipo '(8 10 -1) '(8 10 -1))
(mismo-tipo '(A ZZ QB) '(5 A 78))
(mismo-tipo '(7 X Y) '(9 Z Y))


;9
(defun APalindromo (cadena)
  (let ((cadenaInvertida (reverse cadena)))
	(concatenate 'string cadena cadenaInvertida)))

(APalindromo "Hola")
(APalindromo "12345")
(APalindromo "Land-Of-Lisp")

;10
(defun biciestop ( numero )
  (or (and (= (mod numero 4) 0) (not (= (mod numero 100) 0)) ) (= (mod numero 400) 0)))
(biciestop 1992)
(biciestop 2000)
(biciestop 1900)
(biciestop 2007)
