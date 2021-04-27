 \rr(quicklisp:quickload 'cl-heap)

(defparameter queue (make-instance 'cl-heap:priority-queue))

(cl-heap:enqueue queue '(jola) 15)
(cl-heap:enqueue queue '(jola) 16)
(cl-heap:enqueue queue '(jola) 3)
(cl-heap:enqueue queue '(hla) 5)

(cl-heap:dequeue queue)

(or (estado-recordado? ) (not (null ('(a b c)))))
