# Lisp implementation
A Lisp implementation loosely following Make a Lisp https://github.com/kanaka/mal.
I have added a REPL and features like shortform closures and keyword arguments (more below).

## How to Use
- `cargo run` opens a REPL using reedline. it has coloring and history but no completions. 
- Call `(defsym)` to get a sorted list of all defined symbols.
- load a file in the REPL using `(loadf "path/to/file.qx")`
- the prelude `src/init.qx` automatically tries to load the first argument as a file
- in REPL: use up-arrow for history and enter to run
- in File: use `(export! sym1 sym2 ...)` to move local values to the global environment that carries over into the REPL

## Demos
- Demos are available in `demos/`
- There is `binary_tree.qx`, which implements a basic binary tree with immutable insertion and checking wether the tree contains a value.
- `aoc2025-01.qx` contains the solution to advent of code 2025 day 1.
- run them with `cargo run binary_tree.qx/aoc2025-01.qx`.
- binary_tree exports some functions that can be used in the REPL afterwards


## Project Structure

The core of the interpreter is split across two main modules: `read` and `eval`.

The `read` module is responsible for parsing. It converts input streams into the fundamental Lisp S-Expression, `Expr`. The definition of the type itself lives in `read/types.rs`.

The `eval` module contains the evaluation logic that executes expressions. The REPL implementation is located in `main.rs`. Before any user code is executed, the prelude defined in `init.qx` is loaded automatically.

Native functions implemented in Rust are defined in `env/ll_core.rs`. These functions are inserted into the base environment via `Env::core()`. The function `ll_core::core_map()` returns a `HashMap` that maps function names to their corresponding `Func` implementations.

## Basics
- basic math: +, -, /, *, rem, mod
- define a global variable with (val! ident val)

### Functions
- create a closure with `(fn* (arg1 arg2 ...) body)`
- use `defun!` or `val!` for defining a function
- provide default values with `(arg1 defaultvalue1)` instead of the argument
- keyword arguments with `(function :arg1 val1)`

  Example:
  ```clojure
  ; returns a list of numbers in a range
  ; default values for arguments
  (defun! range (to (from 0) (acc ()))
    (if (< from to)
      ; recursive
      (range to :from (+ from 1) :acc (cons from acc))
      ; reverse before returning
      (rev acc)))

  (range :to 10) ; => (0 1 2 3 4 5 6 7 8 9)
  ```

There is a clojure-like shortform for closures. use `#(body)`, arguments are implicitly numbered %, %1, %2, ...
The arity is determined by the maximum argument number used inside the body

Example:
```clojure
  (#(+ % %1) 1 2)   ; => 3
  ; long form
  ((fn* (a b) (+ a b)) 1 2); => 3
```

### Basic Functions
- (map f over): returns a new list with the result of applying f to each element in over
- (rev list): reverse a list
- (println (str "a" "b")): println prints, str concatenates all arguments to one string
- cons, car, cdr: lisp basics
- bye: quit the REPL. call at the end of a file to exit the interpreter.
- the threading macro (-> start_val expr1 expr2 ...)
  inserts the start_val as a first argument into the function calls expr1
  

  ```clojure
    (-> 50
      (+ 100) ;    => 150
      (* 2)   ;    => 300
      (/ 3))  ;    => 100
  ```

- quasiquoting and quoting is a way to make a lisp expression not be treated as a function call but as data.
  quasiquoting means, you get to splice together things that will be evaluated at runtime and things that are constant

  example: 
  ```clojure
    (val! a 100)

    ; quoting
    '(1 2 3)      ;  => (1 2 3)

    ; quasiquoting
    `(,a 200 300) ;  => (100 200 300)
    
    ; splicing
    (val! l '(1 2 3))
    `(~l 4 5 6)    ; => (1 2 3 4 5 6)
  ```
