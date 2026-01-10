# Lisp implementation, loosely following Make a Lisp (mal)

# How to Use
- cargo run opens a REPL
- load a file using (loadf "path/to/file.qx")
- the prelude automatically tries to load the first argument as a file
- in REPL: use up-arrow for history and enter to run
- in File: use (export! sym1 sym2 ...) to move local values to the global environment that carries over into the REPL

# Basics
basic math: +, -, /, *
define a global variable with (val! ident val)
### Functions
create a closure with (fn* (arg1 arg2 ...) body)
use defun! or val! for defining a function
provide default values with (optarg defaultvalue) instead of the argument
keyword arguments with :arg1 val1
example:
```lisp
  ; returns a list with step +1 between 
  (defun! range 
    (to (from 0) (step 1) (acc '())) 
    (if (= from to) 
      (rev acc) 
      (range 
        :from (+ step from) 
        :to to 
        :acc (cons from acc))))
```

There is a clojure-like shortform for closures. use #(body), arguments are implicitly numbered %, %1, %2, ...
The arity is determined by the maximum argument number used inside the body
Example:
```lisp
  (#(+ % %1) 1 2) => 3
  ; long form
  ((fn* (a b) (+ a b)) 1 2) => 3
```
Some basic functions:
- (map f over): returns a new list with the result of applying f to each element in over
- (rev list): reverse a list
- (println (str "a" "b")): println prints, str concatenates all arguments to one string
- cons, car, cdr: lisp basics
- bye: quit the REPL. call at the end of a file to exit the interpreter.
- the threading macro (-> start_val expr1 expr2 ...)
  inserts the start_val as a first argument into the function calls expr1
  example:

```lisp
  (-> 50
    (+ 100) ; 150
    (* 2)   ; 300
    (/ 3))  ; => 100
```

# Project Structure
The most important modules are eval and read. 
read.rs contains the parsing logic, converting input streams into an `Expr`.
The basic Lisp `Expr` Type is defined in read/types.rs
eval.rs contains the evaluation logic. The REPL logic is in main.rs. The prelude is in init.qx and gets run before any user code is run.
The natively implemented Funcs are defined in env/ll_core.rs and are added into the lowest nested env within Env::core()
ll_core::core_map() returns a hashmap of all Func names to their Func object.
