; Compilateur lisp vers lisp VM
; Groupe Florian NOVELLON et Fabien FERAUD

(setf indexIf 0)
(setf indexComp 0)

(defun compil-fichier (nomFichier &optional dest)
	(let ((fichier (open nomFichier)) (code '()) (bytecode '()))
		(loop for expr = (read fichier nil) while expr do
			(setf code (append code (list expr)))
		)
		(close fichier)
		;(print (append code '((HALT))))
		(setf bytecode (compil-list (append code '((HALT)))))
		(affiche-code bytecode)
		(if (not (null dest))
			(with-open-file (str (string-concat "./" dest)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
  			(format str (write-to-string bytecode)))
		)
		"Compilation termine !"
	)
)

(defun compil-list (listLisp)
  (if (null listLisp)
    NIL
    (append
      (compil-expr (car listLisp))
      (compil-list (cdr listLisp)))))

(defun affiche-code (code)
  (affiche-code-ligne code 1))

(defun affiche-code-ligne (code ligne)
  (if (null code)
    NIL
    (progn
      (print (string-concat (write-to-string ligne) " : " (write-to-string (car code))))
      (affiche-code-ligne (cdr code) (+ 1 ligne)))))

(defun compil-expr (expr &optional env)
  (cond 
  	((consp expr)
  		(case (car expr)
  			('if (compil-if expr env))
  			('defun (compil-defun expr env))
			('halt `((HALT)))
			('nop `((NOP)))
  			(t (compil-appel expr env))))
  	
  	((constantp expr) (compil-constant expr))
  	
  	((symbolp expr) (compil-var expr env))
  	
  	(t (error "Expression ~s n'est pas évaluable" expr))))


(defun compil-constant (constant)
	`((MOVE (LIT ,constant) R0)))

(defun compil-if (code  &optional env)
	(setf indexIf (+ indexIf 1))
	(let ((sinon (intern (string-concat (string "SINON") (write-to-string indexIf))))
		 (finSi (intern (string-concat (string "FINSI") (write-to-string indexIf)))))
		(append 
			(compil-expr (cadr code) env)
			`((CMP (LIT 0) R0))
			`((JEQ (LABEL ,sinon)))
			(compil-expr (caddr code) env)
			`((JMP (LABEL ,finSi)))
			`((LABEL ,sinon))
			(compil-expr (cadddr code) env)
			`((LABEL ,finSi))
		)
	)
)

(defun compil-var (var  &optional env)
	(let ((lib (assoc var env)))
		(if lib
			(append
				`((MOVE FP R0))
				`((SUB (LIT ,(cdr lib)) R0))
				`((LOAD R0 R0))
			)
			`((MOVE (VAR ,var) RO)) ; pour la gestion des let
		)
	)
)

(defun compil-defun (code &optional env) 
	(let ((positionPile 0))
		(progn
			(map
				'list
				(lambda (param)
					(progn 
						(setf positionPile (+ positionPile 1))
						(setf env (acons param positionPile env))
					)
				)
				(caddr code)
			)
			(append
				`((JMP (LABEL ,(intern (string-concat "END" (string (cadr code)))))))
				`((LABEL ,(cadr code)))

				(compil-expr (cadddr code) env)

				`((MOVE FP R1))
				`((ADD (LIT 4) R1))
				'((MOVE R1 SP))
				`((RTN))


				`((LABEL ,(intern (string-concat "END" (string (cadr code))))))
			)
		)
	)
)

(defun compil-appel (code  &optional env)
	(append 
		(apply 
			'append 
				(map 
					'list
					(lambda (param) 
						(append 
							(compil-expr param env) 
							`((PUSH R0))
						)
					)
					(reverse (cdr code))
				)
		)
		`((MOVE FP R1))
		`((MOVE SP FP))
		`((PUSH (LIT ,(list-length (cdr code)))))
		`((MOVE SP R2))
		`((SUB (LIT ,(+ (list-length (cdr code)) 1)) R2))
		`((PUSH R2))
		`((PUSH R1))

		(compil-appel-primitif (car code))

		`((POP R1))
		`((POP R2))

		`((MOVE R1 FP))
		`((MOVE R2 SP))
	)
)

(defun compil-appel-primitif (nomFonction)
	(cond 
		((member nomFonction '(+ - * /))
			(append 
				'(
					(MOVE FP R0)
					(SUB (LIT 1) R0)
					(LOAD R0 R0)
					(MOVE FP R1)
					(SUB (LIT 2) R1)
					(LOAD R1 R1)
				)
				(case nomFonction 
					('+ '((ADD R1 R0)))
					('- '((SUB R1 R0)))
					('* '((MULT R1 R0)))
					('/ '((DIV R1 R0)))
				)
			)
		)
		((member nomFonction '(= <= < > >=))
			(setf indexComp (+ indexComp 1))
			(let ((finCond (intern (string-concat (string "FINCOMP") (write-to-string indexComp)))))
				(append 
					'(
						(MOVE FP R0)
						(SUB (LIT 1) R0)
						(LOAD R0 R0)
						(MOVE FP R1)
						(SUB (LIT 2) R1)
						(LOAD R1 R1)
						(CMP R0 R1) 
						(MOVE (LIT 1) R0)
					)

					(case nomFonction
						('= `((JEQ (LABEL ,finCond))))
						('<= `((JPE (LABEL ,finCond))))
						('< `((JPG (LABEL ,finCond))))
						('> `((JPP (LABEL ,finCond))))
						('>= `((JGE (LABEL ,finCond))))
					)
					'((MOVE (LIT 0) R0))
					`((LABEL ,finCond))
				)
			)
		)
		(t `((JSR (LABEL ,nomFonction))))	
	)
)