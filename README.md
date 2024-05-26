# Lisp implementation, loosely following Make a Lisp (mal)

### Todo:
- [ ] Add Runtime Type Checking for Function Arguments
  - [ ] with generics?
  - [ ] recursive type definition accoring to typegrammar.bnf
  - [ ] custom types/structs
  - [ ] sum types? like Bool|Int|Nil (would make ?Type unnecessary)
form: 
```lisp
  (defun! myfun (foo=Fn bar={?String}) ; foo is a Closure, bar is a List of Nullable Strings
    .. )
```

- [ ] Add Keyword arguments
