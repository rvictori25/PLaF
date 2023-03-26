open Parser_plaf.Ast
open Parser_plaf.Parser
open Ds
(*Renny Victoria and Nerissa Lundquist*)
(*I pledge my honor that I have abided by the Stevens Honor System.*)
  
let rec lookup id l=
  match l with
  |[] -> error "Proj: field does not exist"
  | (st, e)::t ->
  if(st =id)
  then return e
  else lookup id t

let rec eval_expr : expr -> exp_val ea_result = fun e ->
  match e with
  | Int(n) ->
    return @@ NumVal n
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1+n2)
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1-n2)
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return @@ NumVal (n1*n2)
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return @@ NumVal (n1/n2)
  | Let(id,def,body) ->
    eval_expr def >>=
    extend_env id >>+
    eval_expr body
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return @@ BoolVal (n = 0)
  | Proc(id,_,e)  ->
    lookup_env >>= fun en ->
    return (ProcVal(id,e,en))
  | App(e1,e2)  -> 
    eval_expr e1 >>= 
    clos_of_procVal >>= fun (id,e,en) ->
    eval_expr e2 >>= fun ev ->
    return en >>+
    extend_env id ev >>+
    eval_expr e
  | Abs(e1)      ->
    eval_expr e1  >>=
    int_of_numVal >>= fun n ->
    return @@ NumVal (abs n)
  | Record(fs) ->
    failwith "implement me"
  | Proj(e,id) ->
    failwith "implement me"
  | Cons(e1, e2) ->
    eval_expr e1 >>= fun h ->
    eval_expr e2 >>= 
    list_of_listVal>>= fun t ->
    return (ListVal (List.cons h t))
  | Hd(e1) ->
    eval_expr e1 >>=
    list_of_listVal >>= fun l ->
    return (List.hd l)
  | Tl(e1) ->
    eval_expr e1 >>=
    list_of_listVal >>= fun t ->
    return (ListVal (List.tl t))
  | IsEmpty(e1)  ->
    eval_expr e1 >>=
    list_of_listVal >>= fun n ->
    if(n=[])
    then return (BoolVal true)
    else return (BoolVal false)
  | EmptyList ->
    return @@ ListVal([])
  | EmptyTree ->
    return @@ TreeVal(Empty)
  (*| Node(e1,lte,rte) ->
    failwith "implement me"
  | CaseT(target,emptycase,id1,id2,id3,nodecase) ->
    failwith "implement me" *)
  | Tuple(es) ->
    eval_exprs es >>= fun t ->
    return @@ TupleVal t
  | Untuple(ids,e1,e2) ->
    failwith "implement me"
and
  eval_exprs : expr list -> (exp_val list) ea_result =
  fun es ->
  match es with
  | [] -> return []
  | h::t -> eval_expr h >>= fun i ->
    eval_exprs t >>= fun l ->
    return (i::l)
  (*| Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str; 
    error "Debug called"
  | _ -> failwith "Not implemented yet!"*)

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e

(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c

