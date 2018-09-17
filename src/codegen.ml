open Base
open Asm
open Ast
open Codegen_rules
open Labels

type emitter = (Asm.t -> unit)

let rec codegen_exp emitter variables e = 
  let cg e = (ignore @@ codegen_exp emitter variables e; ()) in
  let emit_binary_op e1 e2 =
    cg e1;
    emitter (Push Rax);
    cg e2;
    emitter (Pop Rcx) 
  in
  let emit_comparison e1 e2 =
    cg e1;
    emitter (Push Rax);
    cg e2;
    emitter (Pop Rcx);
    emitter (Cmpl (R Eax, Ecx));
    emitter (Movl (I 0, Eax));
  in 
  match e with
  | Const x -> 
      emitter (Movl (I x, Eax));
      None
  | Complement e -> 
      cg e;
      emitter (Not Eax);
      None
  | Negation e -> 
      cg e;
      emitter (Neg Eax);
      None
  | Logical_Negation e -> 
      cg e;
      emitter (Cmpl (I 0, Eax));
      emitter (Movl (I 0, Eax));
      emitter (Sete Al);
      None
  | Addition (e1, e2) -> 
      emit_binary_op e1 e2;
      emitter (Addl (Ecx, Eax));
      None
  | Subtraction (e1, e2) -> 
      emit_binary_op e2 e1;
      emitter (Subl (Ecx, Eax));
      None
  | Multiplication (e1, e2) -> 
      emit_binary_op e1 e2;
      emitter (IMul (Ecx, Eax));
      None
  | Division (e1, e2) -> 
      emitter (Movl (I 0, Edx));
      cg e2;
      emitter (Push Rax);
      cg e1;
      emitter (Pop Rcx);
      emitter (IDivl (Ecx, Eax));
      None
  | Equality (e1, e2) ->
      emit_comparison e1 e2;
      emitter (Sete Al);
      None
  | Inequality (e1, e2) ->
      emit_comparison e1 e2;
      emitter (Setne Al);
      None
  | LessThan (e1, e2) ->
      emit_comparison e1 e2;
      emitter (Setl Al);
      None
  | LessThanOrEqual (e1, e2) ->
      emit_comparison e1 e2;
      emitter (Setle Al);
      None
  | GreaterThan (e1, e2) ->
      emit_comparison e1 e2;
      emitter (Setg Al);
      None
  | GreaterThanOrEqual (e1, e2) ->
      emit_comparison e1 e2;
      emitter (Setge Al);
      None
  | Logical_And (e1, e2) ->
      cg e1;
      emitter (Push Rax);
      cg e2;
      emitter (Pop Rcx);
      emitter (Cmpl (I 0, Ecx));
      emitter (Setne Cl);
      emitter (Cmpl (I 0, Eax));
      emitter (Movl (I 0, Eax));
      emitter (Setne Al);
      emitter (Andb (Cl, Al));
      None
  | Logical_Or (e1, e2) ->
      cg e1;
      emitter (Push Rax);
      cg e2;
      emitter (Pop Rcx);
      emitter (Orl (Ecx, Eax));
      emitter (Movl (I 0, Eax));
      emitter (Setne Al);
      None
  | Assign (name, exp) -> begin
      match Hashtbl.find variables name with
      | None -> Some (Printf.sprintf "%s is not declared" name)
      | Some stack_index -> begin
          cg exp;
          emitter (Movq (R Rax, O (stack_index, Rbp)));
          None
        end
    end
  | Var name -> begin
      match Hashtbl.find variables name with
      | None -> Some (Printf.sprintf "%s is not declared" name)
      | Some stack_index -> begin
          emitter (Movq (O (stack_index, Rbp), R Rax));
          None
        end
    end

let rec codegen_stmt emitter variables stack_index = function
| Return e -> 
    ignore @@ codegen_exp emitter variables e;
    None
| Declare {name; initial_value} ->  
      let write_declaration e =
        ignore @@ codegen_exp emitter variables e;
        emitter (Movq (R Rax, O (!stack_index, Rbp)));
        emitter (Push Rax) in
    if Option.is_some (Hashtbl.find variables name) 
    then Some (Printf.sprintf "Redeclaration of variable '%s'" name)
    else begin
      Option.iter initial_value ~f:write_declaration;
      Hashtbl.set variables ~key:name ~data:!stack_index;
      stack_index := !stack_index - 8;
      None
    end
| Exp e -> 
    codegen_exp emitter variables e
| Conditional (cond, if_true, if_false) ->
    let if_false_label = generate_label "if_false" in
    let post_conditional_label = generate_label "post_conditional" in
    ignore @@ codegen_exp emitter variables cond;
    emitter (Cmpl ((I 0),Eax));
    emitter (Je if_false_label);
    ignore @@ codegen_stmt emitter variables stack_index if_true;
    match if_false with
    | Some if_false ->
        emitter (Jmp post_conditional_label);
        emitter (Label if_false_label);
        ignore @@ codegen_stmt emitter variables stack_index if_false;
        emitter (Label post_conditional_label);
        None
    | None ->
        emitter (Label if_false_label);
        None
;;
let rec codegen_stmts cg = function 
| [] -> Ok ()
| (s :: rest) -> (
    match cg s with
    | None -> codegen_stmts cg rest
    | Some e -> Error e
)

let codegen_fundef emitter fundef =
  let {name; body} = generate_return fundef in
  let variables: (string, int) Hashtbl.t = Hashtbl.create (module String) in
  emitter (Globl name);
  emitter (Label name);
  (* Function prologue *)
  emitter (Push Rbp);
  emitter (Movq (R Rsp,R Rbp));
  let stack_index = ref 0 in
  let result = codegen_stmts (codegen_stmt emitter variables stack_index) body in
  (* Function epilogue *)
  emitter (Movq (R Rbp,R Rsp));
  emitter (Pop Rbp);
  emitter Ret;
  result

let codegen = codegen_fundef