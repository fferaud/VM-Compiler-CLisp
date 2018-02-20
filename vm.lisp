; Compilateur lisp vers lisp VM
; Groupe Florian NOVELLON et Fabien FERAUD


;; Creation vm et initialisation

(defun make-vm (name taille)
	(setMemoireSize name taille)
	
	(setf (get name 'symboleR) (make-hash-table)) ; table symboles resolus
	
	(setf (get name 'referenceNR) (make-hash-table)) ; table references non resolus
	
	(setf (get name 'exitVM) 0)
	(setf (get name 'maxStack) 100000)
	(setf (get name 'BP) 10)
	(setf (get name 'SP) 10)
	(setf (get name 'FP) 10)
	(setf (get name 'PC) 100001)
	(setf (get name 'R0) 0)
	(setf (get name 'R1) 0)
	(setf (get name 'R2) 0)
	
	;flag
	(setf (get name 'DEQ) 0)
	(setf (get name 'DPP) 0)
	(setf (get name 'DPG) 0)

	"Creation et initialisation de la vm : OK"
)

;; Gestion mémoire

(defun getMemoireSize (vm)
	(array-total-size (get vm 'memoire))
)

(defun setMemoireSize (vm taille)
	(setf (get vm 'memoire) (make-array taille))
	T
)

(defun getMemoireAt (vm adr)
	(if (>= adr (getMemoireSize vm))
		(error "getMemoireAt à l'adresse ~s hors des limites" adr)
		(let ((value (aref (get vm 'memoire) adr)))
			(if (null value)
				0
				value
			)
		)
	)
)

(defun setMemoireAt (vm adr src)
	(if (>= adr (getMemoireSize vm))
		(error "setMemoireAt à l'adresse ~s hors des limites" adr)
		(setf (aref (get vm 'memoire) adr) src)
	)
)


;; Gestion registres

(defun getRegistre (vm reg)
	(let ((value (get vm reg)))
		(if (null value)
			(error "getRegistre registre ~s inconnu" reg)
			value
		)
	)
)

(defun setRegistre (vm reg value)
	(if (null (get vm reg))
		(error "setRegistre registre ~s inconnu" reg)
		(setf (get vm reg) value)
	)
)

;; Gestion symboles resolus

(defun setSymbole (vm symb adr)
	(setf (gethash symb (get vm 'symboleR)) adr)
)

(defun getSymbole (vm symb)
	(gethash symb (get vm 'symboleR))
)

(defun isSymboleSet (vm symb)
	(if (getSymbole vm symb)
		t
		nil
	)
)

; Gestion reference non resolu 

(defun setReferenceNR (vm ref adr)
	(if (isReferenceSet vm ref)
		(setf (gethash ref (get vm 'referenceNR)) (cons adr (gethash ref (get vm 'referenceNR))))
		(setf (gethash ref (get vm 'referenceNR)) (list adr))
	)
)

(defun getReferenceNR (vm ref)
	(gethash ref (get vm 'referenceNR))
)

(defun isReferenceSet (vm ref)
	(if (getReferenceNR vm ref)
		t
		nil
	)
)

;; Fonctions de la vm

(defun isLIT (arg)
	(and (consp arg) (eql (car arg) 'LIT))
)

(defun vm_move (vm arg1 arg2)
	(if (isLIT arg1)
		(setRegistre vm arg2 (cadr arg1))
		(setRegistre vm arg2 (getRegistre vm arg1))
	)
)

(defun vm_load (vm adr arg)
	(if (isLIT adr)
		(vm_move vm `(LIT ,(getMemoireAt vm adr)) arg)
		(vm_move vm `(LIT ,(getMemoireAt vm (getRegistre vm adr))) arg)
	)
	
)

(defun vm_store (vm arg adr)
	(if (isLIT adr)
		(setMemoireAt vm adr (getRegistre vm arg))
		(setMemoireAt vm (getRegistre vm adr) (getRegistre vm arg))
	)
)

(defun vm_incr (vm arg)

	(setRegistre vm arg (+ (getRegistre vm arg) 1))
)

(defun vm_decr (vm arg)
	(setRegistre vm arg (- (getRegistre vm arg) 1))
)

(defun vm_add (vm arg1 arg2)
	(if (isLIT arg1)
		(setRegistre vm arg2 (+ (getRegistre vm arg2) (cadr arg1)))
		(setRegistre vm arg2 (+ (getRegistre vm arg2) (getRegistre vm arg1)))
	)
)

(defun vm_mult (vm arg1 arg2)
	(if (isLIT arg1)
		(setRegistre vm arg2 (* (getRegistre vm arg2) (cadr arg1)))
		(setRegistre vm arg2 (* (getRegistre vm arg2) (getRegistre vm arg1)))
	)
)

(defun vm_sub (vm arg1 arg2)
	(if (isLIT arg1)
		(setRegistre vm arg2 (- (getRegistre vm arg2) (cadr arg1)))
		(setRegistre vm arg2 (- (getRegistre vm arg2) (getRegistre vm arg1)))
	)
)

(defun vm_div (vm arg1 arg2)
	(if (isLIT arg1)
		(setRegistre vm arg2 (/ (getRegistre vm arg2) (cadr arg1)))
		(if (= 0 (getRegistre arg1))
			(error "vm_div division par 0 impossible")
			(setRegistre vm arg2 (/ (getRegistre vm arg2) (getRegistre vm arg1)))
		)
	)
)

(defun vm_push (vm arg)
	;(print (getRegistre vm 'SP))
	(if (> (getRegistre vm 'SP) (getRegistre vm 'maxStack))
		(error "vm_push depassement de pile")
		(progn
			(if (isLIT arg)
				(setMemoireAt vm (getRegistre vm 'SP) (cadr arg))
				(setMemoireAt vm (getRegistre vm 'SP) (getRegistre vm arg)))
			(setRegistre vm 'SP (+ (getRegistre vm 'SP) 1))
		)
	)
)

(defun vm_pop (vm arg)
	;(print (getRegistre vm 'SP))
	(if (<= (getRegistre vm 'SP) (getRegistre vm 'BP))
		(error "vm_pop pile vide")
		(progn 
			(setRegistre vm 'SP (- (getRegistre vm 'SP) 1))
			(setRegistre vm arg (getMemoireAt vm (getRegistre vm 'SP)))
		)
	)
)


(defun vm_cmp (vm arg1 arg2)
	(if (isLIT arg1)
		(setq tmpArg1 (cadr arg1))
		(setq tmpArg1 (getRegistre vm arg1))
	)
	(setq tmpArg2 (getRegistre vm arg2))
	(if (equal tmpArg1 tmpArg2)
		( progn
			(setRegistre vm 'DEQ 1)
			(setRegistre vm 'DPG 0)
			(setRegistre vm 'DPP 0)
		)
		(
			if (< tmpArg1 tmpArg2)
			( progn
				(setRegistre vm 'DEQ 0)
				(setRegistre vm 'DPG 0)
				(setRegistre vm 'DPP 1)
			)
			( progn
				(setRegistre vm 'DEQ 0)
				(setRegistre vm 'DPG 1)
				(setRegistre vm 'DPP 0)
			)
		)
	)
)


(defun isLABEL (arg)
	(and (consp arg) (eql (car arg) 'LABEL))
)

 ;gensym ??
(defun vm_jpg (vm etiq)
	(if (= (getRegistre vm 'DPG) 1)
		(vm_jmp vm etiq)
	)
)

(defun vm_jeq (vm etiq)
	(if (= (getRegistre vm 'DEQ) 1)
		(vm_jmp vm etiq)
	)
)
(defun vm_jpp (vm etiq)
	(if (= (getRegistre vm 'DPP) 1)
		(vm_jmp vm etiq)
	)
)

(defun vm_jge (vm etiq)
	(if (or (= (getRegistre vm 'DPG) 1) (= (getRegistre vm 'DEQ) 1))
		(vm_jmp vm etiq)
	)
)

(defun vm_jpe (vm etiq)
	(if (or (= (getRegistre vm 'DPP) 1) (= (getRegistre vm 'DEQ) 1))
		(vm_jmp vm etiq)
	)
)

(defun vm_jmp (vm etiq)
	(if (integerp etiq)
		(setRegistre vm 'PC etiq)
		(error "vm_jmp etiquette n'est pas une adresse : ~s" etiq)
	)
	
)

(defun vm_jsr (vm etiq)
	(setMemoireAt vm (getRegistre vm 'SP) (+ (getRegistre vm 'PC) 1))
	(setRegistre vm 'SP (+ (getRegistre vm 'SP) 1))
	(vm_jmp vm etiq)
)

(defun vm_rtn (vm)
	(setRegistre vm 'SP (- (getRegistre vm 'SP) 1))
	(vm_jmp vm (getMemoireAt vm (getRegistre vm 'SP)))
)

(defun vm_nop (vm))

(defun vm_halt (vm)
	(setRegistre vm 'exitVM 1)
)

(defun vm_cons (vm arg1 arg2)
	(setRegistre vm arg2 (cons (getRegistre vm arg1) (getRegistre vm arg2)))
)

(defun vm_car (vm arg)
	(setRegistre vm arg (car (getRegistre vm arg)))
)

(defun vm_cdr (vm arg)
	(setRegistre vm arg (cdr (getRegistre vm arg)))
)

; execution de l'instruction courante
(defun execInst (vm value)
	(case (car value)
		('MOVE	 (vm_MOVE vm (cadr value) (caddr value)))
		('LOAD	 (vm_LOAD vm (cadr value) (caddr value)))
		('STORE	 (vm_STORE vm (cadr value) (caddr value)))
		('INCR	 (vm_INCR vm (cadr value)))
		('DECR	 (vm_DECR vm (cadr value)))
		('ADD	 (vm_ADD vm (cadr value) (caddr value)))
		('MULT	 (vm_MULT vm (cadr value) (caddr value)))
		('SUB	 (vm_SUB vm (cadr value) (caddr value)))
		('DIV	 (vm_DIV vm (cadr value) (caddr value)))
		('PUSH	 (vm_PUSH vm (cadr value)))
		('POP	 (vm_POP vm (cadr value)))
		('CMP	 (vm_CMP vm (cadr value) (caddr value)))
		('JPG	 (vm_JPG vm (cadr value)))
		('JEQ	 (vm_JEQ vm (cadr value)))
		('JPP	 (vm_JPP vm (cadr value)))
		('JGE	 (vm_JGE vm (cadr value)))
		('JPE	 (vm_JPE vm (cadr value)))
		('JMP	 (vm_JMP vm (cadr value)))
		('JSR	 (vm_JSR vm (cadr value)))
		('RTN	 (vm_RTN vm))
		('NOP	 (vm_NOP vm))
		('HALT	 (vm_HALT vm))
		('CONS	 (vm_CONS vm (cadr value) (caddr value)))
		('CAR	 (vm_CAR vm (cadr value) ))
		('CDR	 (vm_CDR vm (cadr value) ))
		(t (error "execInst instruction inconnue ~s " (car value)))
	)
)

(defun vm_exec (vm)
	(loop while (= (get vm 'exitVM) 0) do
		(let* ((pc (getRegistre vm 'PC)) (instr (getMemoireAt vm pc)))
			(progn
				;(print instr)

				(execInst vm instr)
				(if (= (getRegistre vm 'PC) pc)
					(setRegistre vm 'PC (+ pc 1))
					nil
				)

				;(print (getRegistre vm 'R0))
				;(print (getRegistre vm 'R1))
				;(print (getRegistre vm 'R2))
				;(print (getRegistre vm 'SP))
				;(print (getRegistre vm 'FP))
			)
		)	
	)
	(print "Resultat de l'appel : ")
	(getRegistre vm 'R0)
)

(defun vm_lecture (vm nomfichier &optional (co 100001))
	(let ((fichier (open nomfichier)))
		(if fichier
			(prog1 
				(vm_chargeur vm (read fichier nil) co)
				(close fichier)
			)
		)
	)
	"Chargement du code termine !"
)

; On regarde chaque instruction, on effectue modification si nécessaire
; et on charge le code en mémoire si ce n'est pas un label
(defun vm_chargeur (vm fichier &optional (co 100001))
	;(print fichier)
	(loop while (not (null fichier)) do
		(let ((instr (car fichier)))
			;(print instr)

			(if (null instr)
				nil
				(if (eql 'LABEL (car instr))
					(vm_charger_symb vm (cadr instr) co)
					(progn
						(setMemoireAt vm co (vm_resoudre_symb vm instr co))
						(setf co (+ co 1))
					)
				)
			)
		)
		(setf fichier (cdr fichier))
	)
)

; On regarde si l'instruction est un jump/call sur une étiquette.
; Si oui on modifie le code si on connait la valeur de l'étiquette sinon
; on laisse comme cela.
; Si pas d'instruction de se type on retourne tel quel pour le charger en
; mémoire
(defun vm_resoudre_symb (vm instr co)
	(if 
		(or 
			(eql 'JMP (car instr))
			(eql 'JSR (car instr))
			(eql 'JPG (car instr))
			(eql 'JEQ (car instr))
			(eql 'JPP (car instr))
			(eql 'JGE (car instr))
			(eql 'JPE (car instr))
		)

		(if (isLABEL (cadr instr))
			(if (isSymboleSet vm (cadadr instr))
				(cons (car instr) (list (getSymbole vm (cadadr instr)))) ; met l'adresse comme integer directement ex : JSR 100152
				(progn
					(setReferenceNR vm (cadadr instr) co)
					instr
				)
			)
			instr
		)
		instr
	)
)

; Traitement du symbole après le LABEL
(defun vm_charger_symb (vm symb co)
	(if (isSymboleSet vm symb)
		(error "vm_chargeur_symb symbole existe deja")
		(progn
			(setSymbole vm symb co)
			(vm_resoudre_refNR vm symb)
		)
	)
)

; On vient de trouver un nouveau (LABEL symb). On regarde s'il n'y a
; pas eu avant un appel alors qu'on ne le connaissait pas. Si oui on modifie
; pour mettre l'adresse du jump maintenant qu'il est connu
(defun vm_resoudre_refNR (vm symb)
	(if (isReferenceSet vm symb)
		(map
			'list
			(lambda 
				(co)
				(setMemoireAt vm co `(,(car (getMemoireAt vm co)) ,(getSymbole vm symb)))
			)
			(getReferenceNR vm symb)
		)
	)
)
