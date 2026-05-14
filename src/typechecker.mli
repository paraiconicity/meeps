module Span : sig
  type t = {
    file : string;
    start_line : int;
    start_col : int;
    end_line : int;
    end_col : int;
  }

  val make :
    file:string ->
    start_line:int ->
    start_col:int ->
    end_line:int ->
    end_col:int ->
    t

  val pp : t -> string
end

module Diagnostic : sig
  type severity = [ `Error | `Warning | `Info ]

  type t = {
    code : string;
    severity : severity;
    message : string;
    span : Span.t option;
    notes : string list;
  }

  val make :
    ?span:Span.t ->
    ?notes:string list ->
    code:string ->
    severity:severity ->
    message:string ->
    unit ->
    t

  val pp : t -> string
end

module Config : sig
  type profile = [ `Strict | `Research | `Legacy ]

  type t = {
    profile : profile;
    max_nodes : int;
    max_depth : int;
  }

  val strict : t
  val research : t
  val legacy : t
end

module Type : sig
  type ownership = [ `Owned | `Borrowed | `Shared ]

  type t =
    | TInt
    | TBool
    | TUnit
    | TFun of t * t
    | TPair of t * t
    | TRef of ownership * t

  val equal : t -> t -> bool
  val pp : t -> string
end

module Ownership : sig
  type owned = [ `Owned ]
  type borrowed = [ `Borrowed ]
  type shared = [ `Shared ]
  type any = [ owned | borrowed | shared ]

  type +'k reference

  val owned : Type.t -> owned reference
  val borrowed : Type.t -> borrowed reference
  val shared : Type.t -> shared reference
  val erase : _ reference -> Type.t
end

module Syntax : sig
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

module Elaborated : sig
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

module Checker : sig
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

  type context
  type ownership_state

  val empty_context : context

  val add_binding :
    name:string ->
    binding ->
    context ->
    (context, error) result

  val infer :
    ?config:Config.t ->
    ?context:context ->
    Syntax.expr ->
    (Type.t * ownership_state, error) result

  val infer_elaborated :
    ?config:Config.t ->
    ?context:context ->
    Syntax.expr ->
    (Elaborated.expr * ownership_state, error) result

  val check :
    ?config:Config.t ->
    ?context:context ->
    Syntax.expr ->
    expected:Type.t ->
    (ownership_state, error) result

  val infer_items :
    ?config:Config.t ->
    ?context:context ->
    Syntax.item list ->
    (context * (Type.t option list), error) result

  val error_code : error -> string
  val to_diagnostic : ?span:Span.t -> error -> Diagnostic.t
  val pp_error : error -> string

  module Session : sig
    type t

    val create : ?config:Config.t -> ?context:context -> unit -> t
    val context : t -> context
    val ownership_state : t -> ownership_state

    val add_binding :
      t ->
      name:string ->
      binding ->
      (unit, error) result

    val infer_expr : t -> Syntax.expr -> (Type.t, error) result
    val infer_elaborated_expr : t -> Syntax.expr -> (Elaborated.expr, error) result
    val check_expr : t -> Syntax.expr -> expected:Type.t -> (unit, error) result
    val infer_items : t -> Syntax.item list -> (Type.t option list, error) result
  end
end
