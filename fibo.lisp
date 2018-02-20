(defun fibo (n)
	(if (= 0 n)
		0
		(if (= 1 n)
			1
			(+ (fibo (- n 1))(fibo (- n 2))))))
(fibo 7)