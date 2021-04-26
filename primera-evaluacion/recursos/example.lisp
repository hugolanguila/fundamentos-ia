;;=======================================================================================
;;; Codigo de ejemplo. Mosaicos.
;;; Este código muestra cómo usar las funciones de la biblioteca "mosaic-lib.lisp".
;;; Sólo funciona para el laberinto Nivel 1, ya que la solución ya esta escrita
;;; dentro del código. Si se usa en los demas laberintos regresará la misma
;;; solucion.
;;=======================================================================================

;Primero debe cargarse la biblioteca de la siguiente forma:
(load "mosaic-lib.lisp")

; Para añadir un algoritmo al menú de la página, es necesario usar la función
; "add-algrithm"como se muestra a continuación. No importa en qué lugar
; del código se use pero, de preferencia, usar al inicio del código.
; Puede omitirse al realizar pruebas en su computadora.

(add-algorithm 'breadth-first)
(add-algorithm 'depth-first)
(add-algorithm 'skull)
(add-algorithm 'error-example)
(add-algorithm 'error-sleep)
(add-algorithm 'get-information)

;Funcion de muestra. Regresa el resultado de un algoritmo de búsqueda a lo
;ancho. Esta función no debe llevar argumentos.

(defun breadth-first ()
  (setq *solution* '(((1 F 120)
		      (2 H 180)
		      (3 E 60)
		      (4 A 0)
		      (5 B 300)
		      (6 D 300)
		      (7 C 0)))))
;; La solución debe ser expresada como una lista de todas las soluciones encontradas...

;; Función de muestra. Regresa el resultado de un algoritmo de búsqueda a lo
;; profundo. Esta función no debe llevar argumentos.

(defun depth-first ()
 (setq *solution* '(
	  (
        (1 J 60) (2 B 180) (3 C 240)
        (4 I 60) (5 H 60) (6 A 240) (7 D 180)
        (8 G 300) (9 F 0) (10 E 300)
      )
    )
  )
)

;; Función de muestra skull. Solución de un tablero con espacios restringidos,
;; los cuales poseen el Id -1, el cual también debe usarse al presentar la solución, 
;; como se muestra en este ejemplo. 

(defun skull()
 (setq *solution* '(
	  (
		(1 Y 240) (2 V 240) (3 M 0) (4 L 60)
        (5 X 120) (6 R 180) (7 C 180) (8 K 180)
        (9 Q 120) (10 P 120) (11 B 120) (12 A 180) (13 E 240)
        (14 ? 180) (15 S 0) (16 F 300) (17 @ 180)
        (18 Z 300) (19 I 0) (20 G 240)  
      )
    )
  )
)
;(find 'A '((a(111)) (b(222)) (c(333)) (d(444))) :key #'first :test #'equalp)
(defun get-information()
  ;La solucion o soluciones debe almacenarse en la variable global *solution*.
  (setq *solution* (list 'get-information 
      (get-pieces)
      (get-board)
      (get-piece-info 'b)
      (get-proximity-info 4)
      (get-adjacent-side 4 3)
    )
  )
)

;; Función retardada. Esta función genera un error de tiempo al tardar más de
;; lo recomendado

(defun error-example () (get-neighbors-piece 1000 1000))
(defun error-sleep ()
	(sleep 150)
	(setq *solution* '(
		(
			(1 A 30) (2 B 30)
			(3 C 30) (4 D 30) (5 Q 30)
			(6 N 30) (7 O 30)
		))))
;; La última línea ejecutable del código debe ser la siguiente. Es la que se
;; encarga de enviar la solución a la pagina Web para que pueda ser
;; dibujada. Si están haciendo pruebas en su computadora entonces pueden omitirla
;; o comentarla.

(print *solution*)

