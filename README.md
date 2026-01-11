# Lisp implementation, loosely following Make a Lisp (mal)

# How to Use
- cargo run opens a REPL
- load a file using (loadf "path/to/file.qx")
- the prelude automatically tries to load the first argument as a file
- in REPL: use up-arrow for history and enter to run
- in File: use (export! sym1 sym2 ...) to move local values to the global environment that carries over into the REPL

# Demos
Demos are available in demos/
There is binary_tree.qx, which implements a basic binary tree with immutable insertion and checking wether the tree contains a value.
aoc2025-01.qx contains the solution to advent of code 2025 day 1.
run them with `cargo run binary_tree.qx/aoc2025-01.qx`.
binary_tree exports some functions that can be used in the REPL afterwards

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
Some basic functionality:
- (map f over): returns a new list with the result of applying f to each element in over
- (rev list): reverse a list
- (println (str "a" "b")): println prints, str concatenates all arguments to one string
- cons, car, cdr: lisp basics
- bye: quit the REPL. call at the end of a file to exit the interpreter.
- the threading macro (-> start_val expr1 expr2 ...)
  inserts the start_val as a first argument into the function calls expr1
  example:
- quasiquoting and quoting is a way to make a lisp expression not be treated as a function call but as data
  quasiquoting means, you get to splice together things that will be evaluated at runtime and things that wont be
  example: 
  ```clojure
  (val! a 100)

  ; quoting
  '(1 2 3)      => (1 2 3)

  ; quasiquoting
  `(,a 200 300) => (100 200 300)
  
  ; splicing
  (val! l '(1 2 3))
  `(~l 4 5 6) => (1 2 3 4 5 6)
```

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
