open Affine_typechecker.Typechecker

let fail msg = failwith msg

let expect_ok = function
  | Ok value -> value
  | Error err -> fail (Checker.pp_error err)

let add_binding ~name binding =
  Checker.add_binding ~name binding Checker.empty_context |> expect_ok

let owned_int name =
  add_binding ~name { Checker.ty = Type.TInt; mode = `Owned }

let owned_bool name =
  add_binding ~name { Checker.ty = Type.TBool; mode = `Owned }

let test_ann_single_eval () =
  let context = owned_int "x" in
  let expr = Syntax.Ann (Syntax.Move "x", Type.TInt) in
  match Checker.infer ~context expr with
  | Ok (ty, _) -> assert (Type.equal ty Type.TInt)
  | Error err -> fail (Checker.pp_error err)

let test_app_single_eval () =
  let context =
    Checker.add_binding ~name:"f"
      { Checker.ty = Type.TFun (Type.TInt, Type.TInt); mode = `Shared }
      (owned_int "y")
    |> expect_ok
  in
  let expr = Syntax.App (Syntax.Var "f", Syntax.Move "y") in
  match Checker.infer ~context expr with
  | Ok (ty, _) -> assert (Type.equal ty Type.TInt)
  | Error err -> fail (Checker.pp_error err)

let test_let_single_eval () =
  let context = owned_int "y" in
  let expr =
    Syntax.Let ("z", `Owned, Some Type.TInt, Syntax.Move "y", Syntax.Var "z")
  in
  match Checker.infer ~context expr with
  | Ok (ty, _) -> assert (Type.equal ty Type.TInt)
  | Error err -> fail (Checker.pp_error err)

let test_if_single_eval () =
  let context = owned_bool "b" in
  let expr = Syntax.If (Syntax.Var "b", Syntax.Unit, Syntax.Unit) in
  match Checker.infer ~context expr with
  | Ok (ty, _) -> assert (Type.equal ty Type.TUnit)
  | Error err -> fail (Checker.pp_error err)

let test_config_validation () =
  match Config.make ~max_nodes:0 ~max_depth:1 () with
  | Ok _ -> fail "expected config validation to fail"
  | Error (Config.Non_positive_max_nodes 0) -> ()
  | Error _ -> fail "unexpected config validation error"

let () =
  List.iter
    (fun f -> f ())
    [
      test_ann_single_eval;
      test_app_single_eval;
      test_let_single_eval;
      test_if_single_eval;
      test_config_validation;
    ]
