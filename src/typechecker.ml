module Span = struct
  type t = {
    file : string;
    start_line : int;
    start_col : int;
    end_line : int;
    end_col : int;
  }

  let make ~file ~start_line ~start_col ~end_line ~end_col =
    { file; start_line; start_col; end_line; end_col }

  let pp t =
    t.file ^ ":" ^ string_of_int t.start_line ^ ":" ^ string_of_int t.start_col
    ^ "-" ^ string_of_int t.end_line ^ ":" ^ string_of_int t.end_col
end

module Diagnostic = struct
  type severity = [ `Error | `Warning | `Info ]

  type t = {
    code : string;
    severity : severity;
    message : string;
    span : Span.t option;
    notes : string list;
  }

  let make ?span ?(notes = []) ~code ~severity ~message () =
    { code; severity; message; span; notes }

  let pp_severity = function
    | `Error -> "error"
    | `Warning -> "warning"
    | `Info -> "info"

  let pp d =
    let loc =
      match d.span with
      | None -> ""
      | Some sp -> Span.pp sp ^ ": "
    in
    let head =
      loc ^ "[" ^ d.code ^ "] " ^ pp_severity d.severity ^ ": " ^ d.message
    in
    match d.notes with
    | [] -> head
    | _ ->
        head ^ "\n"
        ^ String.concat "\n" (List.map (fun n -> "  note: " ^ n) d.notes)
end

module Config = struct
  type profile = [ `Strict | `Research | `Legacy ]

  type validation_error =
    | Non_positive_max_nodes of int
    | Non_positive_max_depth of int

  type t = {
    profile : profile;
    max_nodes : int;
    max_depth : int;
  }

  let make ?(profile = `Strict) ~max_nodes ~max_depth () =
    if max_nodes <= 0 then Error (Non_positive_max_nodes max_nodes)
    else if max_depth <= 0 then Error (Non_positive_max_depth max_depth)
    else Ok { profile; max_nodes; max_depth }

  let strict =
    match make ~profile:`Strict ~max_nodes:250_000 ~max_depth:4_096 () with
    | Ok config -> config
    | Error _ -> failwith "invalid strict checker configuration"

  let research =
    match make ~profile:`Research ~max_nodes:1_000_000 ~max_depth:16_384 () with
    | Ok config -> config
    | Error _ -> failwith "invalid research checker configuration"

  let legacy =
    match make ~profile:`Legacy ~max_nodes:200_000 ~max_depth:2_048 () with
    | Ok config -> config
    | Error _ -> failwith "invalid legacy checker configuration"
end

module Type = struct
  type ownership = [ `Owned | `Borrowed | `Shared ]

  type t =
    | TInt
    | TBool
    | TUnit
    | TFun of t * t
    | TPair of t * t
    | TRef of ownership * t

  let rec equal a b =
    match (a, b) with
    | TInt, TInt
    | TBool, TBool
    | TUnit, TUnit ->
        true
    | TFun (a1, a2), TFun (b1, b2)
    | TPair (a1, a2), TPair (b1, b2) ->
        equal a1 b1 && equal a2 b2
    | TRef (k1, t1), TRef (k2, t2) -> k1 = k2 && equal t1 t2
    | _ -> false

  let rec pp = function
    | TInt -> "int"
    | TBool -> "bool"
    | TUnit -> "unit"
    | TFun (a, b) -> "(" ^ pp a ^ " -> " ^ pp b ^ ")"
    | TPair (a, b) -> "(" ^ pp a ^ " * " ^ pp b ^ ")"
    | TRef (`Owned, t) -> "owned<" ^ pp t ^ ">"
    | TRef (`Borrowed, t) -> "borrowed<" ^ pp t ^ ">"
    | TRef (`Shared, t) -> "shared<" ^ pp t ^ ">"
end

module Ownership = struct
  type owned = [ `Owned ]
  type borrowed = [ `Borrowed ]
  type shared = [ `Shared ]
  type any = [ owned | borrowed | shared ]

  type +'k reference = Reference of Type.t

  let owned t = Reference (Type.TRef (`Owned, t))
  let borrowed t = Reference (Type.TRef (`Borrowed, t))
  let shared t = Reference (Type.TRef (`Shared, t))
  let erase (Reference t) = t
end

module Syntax = struct
  type binder_mode = [ `Owned | `Shared ]

  type expr =
    | Var of string
    | Int of int
    | Bool of bool
    | Unit
    | Pair of expr * expr
    | Lam of string * binder_mode * Type.t option * expr
    | App of expr * expr
    | Let of string * binder_mode * Type.t option * expr * expr
    | If of expr * expr * expr
    | Ann of expr * Type.t
    | Move of string
    | Borrow of string
    | Share of string

  type item =
    | Def of string * binder_mode * Type.t option * expr
    | Expr of expr
end

module Elaborated = struct
  type expr = {
    ty : Type.t;
    node : node;
  }

  and node =
    | EVar of string
    | EInt of int
    | EBool of bool
    | EUnit
    | EPair of expr * expr
    | ELam of string * Syntax.binder_mode * Type.t * expr
    | EApp of expr * expr
    | ELet of string * Syntax.binder_mode * Type.t * expr * expr
    | EIf of expr * expr * expr
    | EAnn of expr * Type.t
    | EMove of string
    | EBorrow of string
    | EShare of string
end

module Checker = struct
  module StringMap = Map.Make (String)

  type error =
    | Unbound_variable of string
    | Annotation_required of string
    | Type_mismatch of { expected : Type.t; actual : Type.t }
    | Expected_function of Type.t
    | Ownership_violation of string
    | Branch_ownership_mismatch of string
    | Limit_exceeded of string

  type binding = {
    ty : Type.t;
    mode : [ `Owned | `Borrowed | `Shared ];
  }

  type context = binding StringMap.t

  type slot = {
    consumed : bool;
    mut_borrowed : bool;
    shared_borrows : int;
  }

  type ownership_state = {
    slots : slot StringMap.t;
    visited_nodes : int;
  }

  type env = {
    ctx : context;
    config : Config.t;
    depth : int;
  }

  type 'a tc = env -> ownership_state -> (('a * ownership_state), error) result

  let return x : 'a tc = fun _env st -> Ok (x, st)

  let bind (m : 'a tc) (f : 'a -> 'b tc) : 'b tc =
   fun env st ->
    match m env st with
    | Ok (x, st') -> f x env st'
    | Error e -> Error e

  let ( let* ) = bind
  let throw e : 'a tc = fun _env _st -> Error e
  let ask : env tc = fun env st -> Ok (env, st)
  let get : ownership_state tc = fun _env st -> Ok (st, st)
  let put st : unit tc = fun _env _ -> Ok ((), st)

  let run (m : 'a tc) ~(context : context) ~(config : Config.t) ~(state : ownership_state) =
    m { ctx = context; config; depth = 0 } state

  let empty_context = StringMap.empty
  let initial_slot = { consumed = false; mut_borrowed = false; shared_borrows = 0 }

  let bootstrap_state (ctx : context) =
    let slots =
      StringMap.fold
        (fun name binding acc ->
          match binding.mode with
          | `Owned -> StringMap.add name initial_slot acc
          | `Borrowed | `Shared -> acc)
        ctx StringMap.empty
    in
    { slots; visited_nodes = 0 }

  let slot_equal a b =
    a.consumed = b.consumed
    && a.mut_borrowed = b.mut_borrowed
    && a.shared_borrows = b.shared_borrows

  let state_equal a b = StringMap.equal slot_equal a.slots b.slots

  let bump_node : unit tc =
   fun env st ->
    let next = st.visited_nodes + 1 in
    if next > env.config.max_nodes then
      Error
        (Limit_exceeded
           ("node budget exceeded (" ^ string_of_int env.config.max_nodes ^ ")"))
    else Ok ((), { st with visited_nodes = next })

  let with_depth (m : 'a tc) : 'a tc =
   fun env st ->
    let next_depth = env.depth + 1 in
    if next_depth > env.config.max_depth then
      Error
        (Limit_exceeded
           ("depth budget exceeded (" ^ string_of_int env.config.max_depth ^ ")"))
    else m { env with depth = next_depth } st

  let add_binding ~name (binding : binding) (ctx : context) =
    if StringMap.mem name ctx then
      Error
        (Ownership_violation
           ("binding '" ^ name ^ "' already exists in the context"))
    else Ok (StringMap.add name binding ctx)

  let lookup_binding (name : string) : binding tc =
    let* env = ask in
    match StringMap.find_opt name env.ctx with
    | Some b -> return b
    | None -> throw (Unbound_variable name)

  let with_binding ~(name : string) ~(binding : binding) (m : 'a tc) : 'a tc =
   fun env st ->
    let ctx' = StringMap.add name binding env.ctx in
    let slots' =
      match binding.mode with
      | `Owned -> StringMap.add name initial_slot st.slots
      | `Borrowed | `Shared -> st.slots
    in
    let st' = { st with slots = slots' } in
    match m { env with ctx = ctx' } st' with
    | Error e -> Error e
    | Ok (x, out_state) ->
        let cleaned = { out_state with slots = StringMap.remove name out_state.slots } in
        Ok (x, cleaned)

  let update_slot (name : string) (f : slot -> (slot, error) result) : unit tc =
    let* st = get in
    let current =
      match StringMap.find_opt name st.slots with
      | Some slot -> slot
      | None -> initial_slot
    in
    match f current with
    | Error e -> throw e
    | Ok next ->
        let slots' = StringMap.add name next st.slots in
        put { st with slots = slots' }

  let consume_owned (name : string) : unit tc =
    update_slot name (fun s ->
        if s.consumed then
          Error (Ownership_violation ("use-after-move of '" ^ name ^ "'"))
        else if s.mut_borrowed || s.shared_borrows > 0 then
          Error
            (Ownership_violation
               ("cannot move '" ^ name ^ "' while references are active"))
        else Ok { s with consumed = true })

  let begin_borrow (name : string) : unit tc =
    update_slot name (fun s ->
        if s.consumed then
          Error (Ownership_violation ("cannot borrow moved value '" ^ name ^ "'"))
        else if s.mut_borrowed || s.shared_borrows > 0 then
          Error
            (Ownership_violation
               ("cannot mutably borrow '" ^ name ^ "' while aliased"))
        else Ok { s with mut_borrowed = true })

  let begin_share (name : string) : unit tc =
    update_slot name (fun s ->
        if s.consumed then
          Error (Ownership_violation ("cannot share moved value '" ^ name ^ "'"))
        else if s.mut_borrowed then
          Error
            (Ownership_violation
               ("cannot create shared refs while '" ^ name ^ "' is mutably borrowed"))
        else Ok { s with shared_borrows = s.shared_borrows + 1 })

  let ensure_type expected actual =
    if Type.equal expected actual then return ()
    else throw (Type_mismatch { expected; actual })

  let mode_of_binder :
      Syntax.binder_mode -> [ `Owned | `Borrowed | `Shared ] = function
    | `Owned -> `Owned
    | `Shared -> `Shared

  let mk ty node = { Elaborated.ty; node }

  let rec infer_typed_expr (expr : Syntax.expr) : Elaborated.expr tc =
    let* () = bump_node in
    match expr with
    | Syntax.Var name ->
        let* b = lookup_binding name in
        (match b.mode with
        | `Owned ->
            let* () = consume_owned name in
            return (mk b.ty (Elaborated.EVar name))
        | `Borrowed | `Shared -> return (mk b.ty (Elaborated.EVar name)))
    | Syntax.Move name ->
        let* b = lookup_binding name in
        (match b.mode with
        | `Owned ->
            let* () = consume_owned name in
            return (mk b.ty (Elaborated.EMove name))
        | `Borrowed | `Shared -> return (mk b.ty (Elaborated.EMove name)))
    | Syntax.Int n -> return (mk Type.TInt (Elaborated.EInt n))
    | Syntax.Bool b -> return (mk Type.TBool (Elaborated.EBool b))
    | Syntax.Unit -> return (mk Type.TUnit Elaborated.EUnit)
    | Syntax.Pair (a, b) ->
        let* ea = infer_once a in
        let* eb = infer_once b in
        return (mk (Type.TPair (ea.ty, eb.ty)) (Elaborated.EPair (ea, eb)))
    | Syntax.Ann (inner, ty) ->
        let* ein = infer_once inner in
        let* () = ensure_type ty ein.ty in
        return (mk ty (Elaborated.EAnn (ein, ty)))
    | Syntax.Lam (arg, mode, arg_ty_opt, body) ->
        let* arg_ty =
          match arg_ty_opt with
          | Some t -> return t
          | None ->
              throw
                (Annotation_required
                   ("lambda parameter '" ^ arg ^ "' requires an annotation"))
        in
        let arg_binding = { ty = arg_ty; mode = mode_of_binder mode } in
        let* body_typed =
          with_binding ~name:arg ~binding:arg_binding (infer_once body)
        in
        let ty = Type.TFun (arg_ty, body_typed.ty) in
        return (mk ty (Elaborated.ELam (arg, mode, arg_ty, body_typed)))
    | Syntax.App (f, arg) ->
        let* ef = infer_once f in
        (match ef.ty with
        | Type.TFun (in_ty, out_ty) ->
            let* ea = ensure_inferred_type arg in_ty in
            return (mk out_ty (Elaborated.EApp (ef, ea)))
        | not_fun -> throw (Expected_function not_fun))
    | Syntax.Let (name, mode, ann_opt, value, body) ->
        let* value_typed =
          match ann_opt with
          | Some expected ->
              let* ev = ensure_inferred_type value expected in
              return (mk expected ev.node)
          | None -> infer_once value
        in
        let binding = { ty = value_typed.ty; mode = mode_of_binder mode } in
        let* body_typed =
          with_binding ~name ~binding (infer_once body)
        in
        return
          (mk body_typed.ty
             (Elaborated.ELet (name, mode, value_typed.ty, value_typed, body_typed)))
    | Syntax.If (cond, then_e, else_e) ->
        let* cond_typed = infer_once cond in
        let* () = ensure_type Type.TBool cond_typed.ty in
        infer_if_branches cond_typed then_e else_e
    | Syntax.Borrow name ->
        let* b = lookup_binding name in
        (match b.mode with
        | `Owned ->
            let* () = begin_borrow name in
            let ty = Ownership.erase (Ownership.borrowed b.ty) in
            return (mk ty (Elaborated.EBorrow name))
        | `Borrowed ->
            throw
              (Ownership_violation
                 ("cannot borrow '" ^ name ^ "' because it is already borrowed"))
        | `Shared ->
            throw
              (Ownership_violation
                 ("cannot mutably borrow shared binding '" ^ name ^ "'")))
    | Syntax.Share name ->
        let* b = lookup_binding name in
        (match b.mode with
        | `Owned ->
            let* () = begin_share name in
            let ty = Ownership.erase (Ownership.shared b.ty) in
            return (mk ty (Elaborated.EShare name))
        | `Borrowed ->
            throw
              (Ownership_violation
                 ("cannot share borrowed binding '" ^ name ^ "'"))
        | `Shared ->
            let ty = Ownership.erase (Ownership.shared b.ty) in
            return (mk ty (Elaborated.EShare name)))

  and infer_once (expr : Syntax.expr) : Elaborated.expr tc = with_depth (infer_typed_expr expr)

  and ensure_inferred_type (expr : Syntax.expr) (expected : Type.t) : Elaborated.expr tc =
    let* actual = infer_once expr in
    let* () = ensure_type expected actual.ty in
    return actual

  and infer_if_branches
      (cond_typed : Elaborated.expr)
      (then_e : Syntax.expr)
      (else_e : Syntax.expr) : Elaborated.expr tc =
    fun env st0 ->
      match with_depth (infer_typed_expr then_e) env st0 with
      | Error e -> Error e
      | Ok (then_typed, then_st) ->
          begin
            match with_depth (infer_typed_expr else_e) env st0 with
            | Error e -> Error e
            | Ok (else_typed, else_st) ->
                if not (Type.equal then_typed.ty else_typed.ty) then
                  Error
                    (Type_mismatch
                       {
                         expected = then_typed.ty;
                         actual = else_typed.ty;
                       })
                else if not (state_equal then_st else_st) then
                  Error
                    (Branch_ownership_mismatch
                       "if branches must leave ownership in equivalent states")
                else
                  Ok
                    ( mk then_typed.ty
                        (Elaborated.EIf (cond_typed, then_typed, else_typed)),
                      then_st )
          end

  and check_expr (expr : Syntax.expr) (expected : Type.t) : unit tc =
    let* () = bump_node in
    match (expr, expected) with
    | Syntax.Lam (arg, mode, arg_ty_opt, body), Type.TFun (in_ty, out_ty) ->
        let* () =
          match arg_ty_opt with
          | Some annotated -> ensure_type in_ty annotated
          | None -> return ()
        in
        let arg_binding = { ty = in_ty; mode = mode_of_binder mode } in
        with_binding ~name:arg ~binding:arg_binding
          (check_expr body out_ty)
    | _ ->
        let* actual = infer_once expr in
        ensure_type expected actual.ty

  let infer ?(config = Config.strict) ?(context = empty_context) expr =
    let initial_state = bootstrap_state context in
    match run (infer_typed_expr expr) ~context ~config ~state:initial_state with
    | Ok (typed, st) -> Ok (typed.ty, st)
    | Error e -> Error e

  let infer_elaborated ?(config = Config.strict) ?(context = empty_context) expr =
    let initial_state = bootstrap_state context in
    run (infer_typed_expr expr) ~context ~config ~state:initial_state

  let check ?(config = Config.strict) ?(context = empty_context) expr ~expected =
    let initial_state = bootstrap_state context in
    match run (check_expr expr expected) ~context ~config ~state:initial_state with
    | Ok ((), st) -> Ok st
    | Error e -> Error e

  let infer_items_with_state
      ~(config : Config.t)
      ~(context : context)
      ~(state : ownership_state)
      (items : Syntax.item list) =
    let rec go ctx st acc = function
      | [] -> Ok (ctx, st, List.rev acc)
      | item :: rest -> (
          match item with
          | Syntax.Expr expr -> (
              match infer_typed_expr expr { ctx; config; depth = 0 } st with
              | Error e -> Error e
              | Ok (typed, st') -> go ctx st' (Some typed.ty :: acc) rest)
          | Syntax.Def (name, mode, ann_opt, expr) -> (
              match infer_typed_expr expr { ctx; config; depth = 0 } st with
              | Error e -> Error e
              | Ok (typed, st') ->
                  let final_ty =
                    match ann_opt with
                    | Some expected when Type.equal typed.ty expected -> Ok expected
                    | Some expected ->
                        Error (Type_mismatch { expected; actual = typed.ty })
                    | None -> Ok typed.ty
                  in
                  match final_ty with
                  | Error e -> Error e
                  | Ok ty -> (
                      let binding = { ty; mode = mode_of_binder mode } in
                      match add_binding ~name binding ctx with
                      | Error e -> Error e
                      | Ok ctx' ->
                          let slots' =
                            match binding.mode with
                            | `Owned -> StringMap.add name initial_slot st'.slots
                            | `Borrowed | `Shared -> st'.slots
                          in
                          let st'' = { st' with slots = slots' } in
                          go ctx' st'' (None :: acc) rest)))
    in
    go context state [] items

  let infer_items ?(config = Config.strict) ?(context = empty_context) items =
    let state0 = bootstrap_state context in
    match infer_items_with_state ~config ~context ~state:state0 items with
    | Ok (ctx, _st, out) -> Ok (ctx, out)
    | Error e -> Error e

  let error_code = function
    | Unbound_variable _ -> "TC1001"
    | Annotation_required _ -> "TC1002"
    | Type_mismatch _ -> "TC1003"
    | Expected_function _ -> "TC1004"
    | Ownership_violation _ -> "TC2001"
    | Branch_ownership_mismatch _ -> "TC2002"
    | Limit_exceeded _ -> "TC3001"

  let pp_error = function
    | Unbound_variable name -> "unbound variable: " ^ name
    | Annotation_required msg -> msg
    | Type_mismatch { expected; actual } ->
        "type mismatch: expected " ^ Type.pp expected ^ " but got " ^ Type.pp actual
    | Expected_function ty -> "expected a function type, got " ^ Type.pp ty
    | Ownership_violation msg -> "ownership violation: " ^ msg
    | Branch_ownership_mismatch msg -> "ownership mismatch across branches: " ^ msg
    | Limit_exceeded msg -> "limit exceeded: " ^ msg

  let to_diagnostic ?span err =
    Diagnostic.make ?span ~code:(error_code err) ~severity:`Error
      ~message:(pp_error err) ()

  module Session = struct
    type t = {
      config : Config.t;
      mutable ctx : context;
      mutable st : ownership_state;
    }

    let create ?(config = Config.strict) ?(context = empty_context) () =
      { config; ctx = context; st = bootstrap_state context }

    let context t = t.ctx
    let ownership_state t = t.st

    let add_binding t ~name binding =
      match add_binding ~name binding t.ctx with
      | Error e -> Error e
      | Ok ctx' ->
          t.ctx <- ctx';
          let slots' =
            match binding.mode with
            | `Owned -> StringMap.add name initial_slot t.st.slots
            | `Borrowed | `Shared -> t.st.slots
          in
          t.st <- { t.st with slots = slots' };
          Ok ()

    let infer_elaborated_expr t expr =
      match run (infer_typed_expr expr) ~context:t.ctx ~config:t.config ~state:t.st with
      | Error e -> Error e
      | Ok (typed, st') ->
          t.st <- st';
          Ok typed

    let infer_expr t expr =
      match infer_elaborated_expr t expr with
      | Error e -> Error e
      | Ok typed -> Ok typed.ty

    let check_expr t expr ~expected =
      match run (check_expr expr expected) ~context:t.ctx ~config:t.config ~state:t.st with
      | Error e -> Error e
      | Ok ((), st') ->
          t.st <- st';
          Ok ()

    let infer_items t items =
      match infer_items_with_state ~config:t.config ~context:t.ctx ~state:t.st items with
      | Error e -> Error e
      | Ok (ctx', st', out) ->
          t.ctx <- ctx';
          t.st <- st';
          Ok out
  end
end
