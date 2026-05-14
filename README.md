# meeps

This is a reusable OCaml library implementing a bidirectional type checker with affine ownership tracking

## Examples

```ocaml
open Affine_typechecker

let expr =
  Syntax.Let
    ("x", `Owned, Some Type.TInt, Syntax.Int 42, Syntax.Move "x")

let result = Checker.infer expr
```

```ocaml
open Affine_typechecker

let session = Checker.Session.create ~config:Config.strict ()

let () =
  ignore
    (Checker.Session.add_binding session
       ~name:"global_counter"
       { Checker.ty = Type.TInt; mode = `Shared })

let ty_result = Checker.Session.infer_expr session (Syntax.Var "global_counter")
```

Use a session when checking multiple expressions/items in sequence while preserving context and ownership state
