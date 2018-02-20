
# VM and compiler CLISP by NOVELLON Florian and FERAUD Fabien

## Study project 
* Create CLISP compiler of factorial and fibonacci CLISP into pseudo code MIPS
* Create CLISP VM which can load pseudo code MIPS and execute him

## Manual
* Launch clisp in the project directory
* (load "compilateur") // load compiler functions
* (load "vm") // load VM functions

* (compil-fichier "fichierACompiler" "nomFichierDestination") 
// "nomFichierDestination" does not have to be created before and is optional. If there is no destination file name this will just display the compiled code instruction by instruction.
// You must use the files "fact.lisp" and "fibo.lisp" instead of "fichierACompiler"

* (make-vm 'vm taille) // taille must be greater than 100001 because the code starts from 100001. 200000 or higher recommended.
* (vm_lecture 'vm "nomFichierCompile") // loads the code into memory from the 100001 address in an increasing way
* (vm_exec 'vm) // Execute the code from the address 100001.

## Notes
* To load code after running the vm, you must first redo (make-vm 'vm taille)

* For the compilation of functions (defun) we add a jump before the beginning of the function which points the address after the RTN of the function in the compiled code. This allows the execution not to execute the body of functions because we start at the address 100001 (initial value of PC). So we can put a defun then a call and a defun it will execute the call first without further processing from the charger.