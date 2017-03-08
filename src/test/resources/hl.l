((defun foo (x) (+ x 1))
 (print ((lambda (x) (* x (foo x))) 2)))
