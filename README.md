# top-down-hindley-milner

## Notes
* I original learned about Hindley-Milner / unification-based type checking
  in the context of *type inference*. Given a typing context and term as 
  input, produce a type (or error) as an output: `Context -> Term -> Either Error Type`.
  
* As a compiler user, I find type errors easiest to comprehend when they
  tell me the expected type and the actual type. Hindley-Milner-based
  algorithms usually ignore this detail. I use a discipline that keeps
  "expected" and "actual" types in a way that flows through to the user-facing
  errors. This discipline is inspired by bidirectional typing. Roughly:
  synthesis produces "actual" types, and checking consumes "expected"  types.

* I recently discovered that this discipline was reporting type errors that 
  weren't localised enough. The problem was that my algorithm wasn't 
  propagating known types deep enough while checking.

* Example

  Given `f : a -> (a -> Int) -> Int`, this expression:

  ```
  f "hello" (\x -> if x then 0 else 1)
  ```

  would result in this error:

  ```
  (test):x:y: error: expected type `String -> Int`, got type `Bool -> Int`
    |
  x | f "hello" (\x -> if x then 0 else 1)
    |            ^^^^^^^^^^^^^^^^^^^^^^^^ 
  ```

  Instead, it needs to report the following:

  ```
  (test):x:y: error: expected type `Bool`, got type `String`
    |
  x | f "hello" (\x -> if x then 0 else 1)
    |                     ^ 
  ```

* To solve this, I started from the perspective that
  "there is always an expected type" - that "checking" is the main mode
  of operation. From this perspective, synthesis is the act of checking that 
  a term has type `?`, where `?` is some fresh metavariable. This is in 
  contrast to the synthesis-first approach, where synthesis is the main
  mode and checking is done by unifying an expected type with a synthesised
  type.

* I call the approach "top-down" because it fundamentally assumes that 
  you have an expected type and then checks that your term inhabits this
  type. Inference is the special case of checking that your term inhabits
  a specific yet flexible unknown type (a metavariable). Perhaps you could
  call this "Hindley-Milner checking", in contrast to "Hindley-Milner
  inference".