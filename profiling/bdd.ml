(** a tiny BDD implementation *)

type bdd_ptr = int

(** tiny bdd *)
type bdd =
  True
  | False
  | Node of { topvar: int; low: bdd_ptr; high: bdd_ptr }

type bdd_table = {
  (* backing table that stores all unique BDDs *)
  backing_table: bdd Array.t;

  (* memoizes BDDs to ensure there is only ever 1 unique BDD pointer for a
     particular structural BDD form *)
  compute_table: (bdd, int) Hashtbl.t;

  (* holds the next free location in the backing table*)
  next_free: int ref;

  (* number of variables that have been allocated so far *)
  num_vars: int ref;

  (* memoizes BDD conjunction operations *)
  memo_table: (bdd_ptr * bdd_ptr, bdd_ptr) Hashtbl.t
}

let true_ptr = 0

let false_ptr = 1

let fresh_bdd_table () : bdd_table =
  let arr = Array.make 10000 True in
  let compute_table = Hashtbl.create 100 in
  Hashtbl.add compute_table true_ptr True;
  Hashtbl.add compute_table false_ptr False;
  Array.set arr 1 False;
  Array.set arr 0 True;
  {
    backing_table=arr;
    next_free = ref 2;
    num_vars = ref 0;
    compute_table = Hashtbl.create 100;
    memo_table = Hashtbl.create 100;
  }

let deref_bdd (tbl:bdd_table) (ptr:bdd_ptr) : bdd =
  Array.get tbl.backing_table ptr

(** get or insert a fresh bdd into the table *)
let get_or_insert (table:bdd_table) (bdd:bdd) : bdd_ptr =
  match Hashtbl.find_opt table.compute_table bdd with
  | Some(v) -> v
  | None ->
    let new_idx = !(table.next_free) in
    table.next_free := new_idx + 1;
    Array.set table.backing_table new_idx bdd;
    Hashtbl.add table.compute_table bdd new_idx;
    new_idx

let rec string_of_bdd (tbl: bdd_table) (f:bdd_ptr) : string =
  match deref_bdd tbl f with
  | True -> "T"
  | False -> "F"
  | Node { topvar; low; high } ->
    Format.sprintf "(%d %s %s)" topvar (string_of_bdd tbl low) (string_of_bdd tbl high)

(** get a pointer to a fresh BDD variable that is true *)
let fresh_var (tbl: bdd_table) : bdd_ptr =
  let fresh_var = !(tbl.num_vars) in
  tbl.num_vars := fresh_var + 1;
  get_or_insert tbl (Node { topvar = fresh_var; low = true_ptr; high = false_ptr})

(** get a pointer to a bdd with topvariable `topvar` *)
let bdd_var (tbl: bdd_table) (topvar: int) : bdd_ptr =
  (* assert that this variable has been allocated via freshvar; this invariant
     is necessary to ensure that freshvar always produces a variable with no existing
     references
  *)
  assert (topvar < !(tbl.num_vars));
  get_or_insert tbl (Node { topvar = topvar; low = true_ptr; high = false_ptr})

let topvar (tbl: bdd_table) (f: bdd_ptr) =
  match deref_bdd tbl f with
  | Node { topvar; low=_; high=_ } -> topvar
  | _ -> failwith "Tried to call topvar on non-node"

(** negate a BDD *)
let rec bdd_neg (tbl:bdd_table) (f:bdd_ptr) : bdd_ptr =
  let rec neg_h memo tbl f =
    match Hashtbl.find_opt memo f with
    | Some(v) -> v
    | None ->
      let r = (match deref_bdd tbl f with
      | True -> false_ptr
      | False -> true_ptr
      | Node { topvar; low; high } ->
        get_or_insert tbl (Node { topvar; low = bdd_neg tbl low; high = bdd_neg tbl high})) in
      Hashtbl.add memo f r;
      r in
  neg_h (Hashtbl.create 100) tbl f

(** conjoin together two BDDs *)
let rec bdd_and (tbl:bdd_table) (f:bdd_ptr) (g:bdd_ptr) : bdd_ptr =
  (* check for cached BDD *)
  match Hashtbl.find_opt tbl.memo_table (f, g) with
  | Some(v) -> v
  | None ->
    (* no cached BDD, compute conjunction *)
    let r = (match (deref_bdd tbl f), (deref_bdd tbl g) with
        | _, False
        | False, _ -> false_ptr
        | True, True -> true_ptr
        | True, Node { topvar; high; low } -> g
        | Node { topvar; high; low }, True -> f
        | Node { topvar=f_topvar; low=f_low; high=f_high },
          Node { topvar=g_topvar; low=g_low; high=g_high } when f_topvar = g_topvar ->
          let l = bdd_and tbl f_low g_low in
          let h = bdd_and tbl f_high g_high in
          if l = h then l else get_or_insert tbl (Node { topvar = f_topvar; low = l; high = h})
        | Node { topvar=f_topvar; low=f_low; high=f_high },
          Node { topvar=g_topvar; low=g_low; high=g_high } ->
          if f_topvar < g_topvar then
            let l = bdd_and tbl f_low g in
            let h = bdd_and tbl f_high g in
            if l = h then l else get_or_insert tbl (Node { topvar = f_topvar; low = l; high = h})
          else
            let l = bdd_and tbl f g_low in
            let h = bdd_and tbl f g_high in
            if l = h then l else get_or_insert tbl (Node { topvar = g_topvar; low = l; high = h})) in

    (* cache the conjunction *)
    Hashtbl.add tbl.memo_table (f, g) r;
    r

let rec bdd_or (tbl: bdd_table) (f:bdd_ptr) (g:bdd_ptr) : bdd_ptr =
  (* compute disjunction via DeMorgan's law for or *)
  let negf = bdd_neg tbl f in
  let negg = bdd_neg tbl g in
  bdd_neg tbl (bdd_and tbl negf negg)

(* some basic tests *)
let () =
  (* test for canonicity of negation *)
  let tbl = fresh_bdd_table () in
  let b1 = fresh_var tbl in
  let b1n = bdd_neg tbl b1 in
  let b2n = bdd_neg tbl b1 in
  assert (b1n = b2n)

let () =
  (* test for canonicity of conjunction *)
  let tbl = fresh_bdd_table () in
  let a = fresh_var tbl in
  let b = fresh_var tbl in
  let a2 = bdd_var tbl 0 in
  let b2 = bdd_var tbl 1 in
  assert (bdd_and tbl a b = bdd_and tbl b2 a2)

let () =
  (* test for unsat case *)
  let tbl = fresh_bdd_table () in
  let a = fresh_var tbl in
  assert ((bdd_and tbl a (bdd_neg tbl a)) = false_ptr)

let () =
  (* test for valid case *)
  let tbl = fresh_bdd_table () in
  let a = fresh_var tbl in
  assert ((bdd_or tbl a (bdd_neg tbl a)) = true_ptr)

type weight = { low_w:float; high_w:float }

type wmc_params =
  {
    weights: (int, weight) Hashtbl.t;
    one: float;
    zero: float;
  }

(** perform an unsmoothed weighted model counting: weights must
    sum to unity *)
let wmc (tbl:bdd_table) (w:wmc_params) (f:bdd_ptr) : float =
  let rec wmc_h (memo: (bdd_ptr, float) Hashtbl.t) tbl w f =
    match Hashtbl.find_opt memo f with
    | Some(v) -> v
    | None ->
      let r = (match deref_bdd tbl f with
      | True -> w.one
      | False -> w.zero
      | Node { topvar; low; high } ->
        let { low_w; high_w } = Hashtbl.find (w.weights) topvar in
        let low_wmc = wmc_h memo tbl w low in
        let high_wmc = wmc_h memo tbl w high in
        (low_w *. low_wmc) +. (high_w *. high_wmc)
        ) in
      Hashtbl.add memo f r;
      r in
  wmc_h (Hashtbl.create 100) tbl w f

(* some testing *)
let within_epsilon a b = Float.abs (a -. b) < 0.0001

let () =
  let tbl = fresh_bdd_table () in
  let a = fresh_var tbl in
  let b = fresh_var tbl in
  let c = fresh_var tbl in
  let disj = bdd_or tbl a (bdd_or tbl b c) in
  let w = { low_w = 0.5; high_w = 0.5} in
  let params = {
    weights = Hashtbl.of_seq (List.to_seq [(0, w); (1, w); (2, w)]);
    one = 1.0;
    zero = 0.0
  } in
  assert (within_epsilon (wmc tbl params disj) 0.875)

(**********************************************************************************)
(* tiny dice *)

type pure_e =
  | And of pure_e * pure_e
  | Or of pure_e * pure_e
  | Not of pure_e
  | Ident of string
  | Ite of pure_e * pure_e * pure_e
  | True
  | False

(** tinyppl probabilistic expressions *)
type expr =
  | Flip       of float
  | Bind       of string * expr * expr
  | Return     of pure_e

module StringMap = Map.Make(String)

(* map identifiers to propositional variables *)
type env = bdd_ptr StringMap.t

let rec compile_p (tbl:bdd_table) (env:env) (e:pure_e) : bdd_ptr =
  match e with
  | True -> true_ptr
  | False -> false_ptr
  | Ident(s) -> StringMap.find s env
  | And(e1, e2) ->
    let c1 = compile_p tbl env e1 in
    let c2 = compile_p tbl env e2 in
    bdd_and tbl c1 c2
  | Or(e1, e2) ->
    let c1 = compile_p tbl env e1 in
    let c2 = compile_p tbl env e2 in
    bdd_or tbl c1 c2
  | Not(e) ->
    bdd_neg tbl (compile_p tbl env e)
  | Ite(g, thn, els) ->
    let cg = compile_p tbl env g in
    let cthn = compile_p tbl env thn in
    let cels = compile_p tbl env els in
    bdd_and tbl (bdd_or tbl cg cthn) (bdd_or tbl (bdd_neg tbl cg) cels)

let rec compile_e (tbl:bdd_table) (w:wmc_params) (env:env) (e:expr) : bdd_ptr =
  match e with
  | Flip(f) ->
    let v = fresh_var tbl in
    let weight = { low_w = f; high_w = 1.0 -. f } in
    Hashtbl.add (w.weights) (topvar tbl v) weight;
    v
  | Bind(x, e1, e2) ->
    let c1 = compile_e tbl w env e1 in
    let new_env = StringMap.add x c1 env in
    compile_e tbl w new_env e2
  | Return(e) -> compile_p tbl env e

let prob (e:expr) : float =
  let tbl = fresh_bdd_table () in
  let params = {
    weights = Hashtbl.create 100;
    one = 1.0;
    zero = 0.0
  } in
  let compiled = compile_e tbl params StringMap.empty e in
  wmc tbl params compiled

(* some small examples *)
let within_epsilon a b = Float.abs (a -. b) < 0.0001

let prog1 = Flip 0.5

let prog2 = Bind("x", Flip(0.5), Return(Ident("x")))

let prog3 = Bind("x", Flip(0.5),
                 Bind("y", Flip(0.3),
                      Bind("z", Flip(0.7),
                           Return(Ite(Ident("x"), Ident("y"), Ident("z"))))))

let () =
  assert (within_epsilon (prob prog1) 0.5);
  assert (within_epsilon (prob prog2) 0.5);
  assert (within_epsilon (prob prog3) 0.5)

(**********************************************************************************)
(** This module is a very simple parsing library for S-expressions. *)
(* Copyright (C) 2009  Florent Monnier, released under MIT license. *)

type sexpr = Atom of string | Expr of sexpr list

type state =
  | Parse_root of sexpr list
  | Parse_content of sexpr list
  | Parse_word of Buffer.t * sexpr list
  | Parse_string of bool * Buffer.t * sexpr list

let parse pop_char =
  let rec aux st =
    match pop_char() with
    | None ->
        begin match st with
        | Parse_root sl -> (List.rev sl)
        | Parse_content _
        | Parse_word _
        | Parse_string _ ->
            failwith "Parsing error: content not closed by parenthesis"
        end
    | Some c ->
        match c with
        | '(' ->
            begin match st with
            | Parse_root sl ->
                let this = aux(Parse_content []) in
                aux(Parse_root((Expr this)::sl))
            | Parse_content sl ->
                let this = aux(Parse_content []) in
                aux(Parse_content((Expr this)::sl))
            | Parse_word(w, sl) ->
                let this = aux(Parse_content []) in
                aux(Parse_content((Expr this)::Atom(Buffer.contents w)::sl))
            | Parse_string(_, s, sl) ->
                Buffer.add_char s c;
                aux(Parse_string(false, s, sl))
            end
        | ')' ->
            begin match st with
            | Parse_root sl ->
                failwith "Parsing error: closing parenthesis without openning"
            | Parse_content sl -> (List.rev sl)
            | Parse_word(w, sl) -> List.rev(Atom(Buffer.contents w)::sl)
            | Parse_string(_, s, sl) ->
                Buffer.add_char s c;
                aux(Parse_string(false, s, sl))
            end
        | ' ' | '\n' | '\r' | '\t' ->
            begin match st with
            | Parse_root sl -> aux(Parse_root sl)
            | Parse_content sl -> aux(Parse_content sl)
            | Parse_word(w, sl) -> aux(Parse_content(Atom(Buffer.contents w)::sl))
            | Parse_string(_, s, sl) ->
                Buffer.add_char s c;
                aux(Parse_string(false, s, sl))
            end
        | '"' ->
            (* '"' *)
            begin match st with
            | Parse_root _ -> failwith "Parse error: double quote at root level"
            | Parse_content sl ->
                let s = Buffer.create 74 in
                aux(Parse_string(false, s, sl))
            | Parse_word(w, sl) ->
                let s = Buffer.create 74 in
                aux(Parse_string(false, s, Atom(Buffer.contents w)::sl))
            | Parse_string(true, s, sl) ->
                Buffer.add_char s c;
                aux(Parse_string(false, s, sl))
            | Parse_string(false, s, sl) ->
                aux(Parse_content(Atom(Buffer.contents s)::sl))
            end
        | '\\' ->
            begin match st with
            | Parse_string(true, s, sl) ->
                Buffer.add_char s c;
                aux(Parse_string(false, s, sl))
            | Parse_string(false, s, sl) ->
                aux(Parse_string(true, s, sl))
            | _ ->
                failwith "Parsing error: escape character in wrong place"
            end
        | _ ->
            begin match st with
            | Parse_root _ ->
                failwith(Printf.sprintf "Parsing error: char '%c' at root level" c)
            | Parse_content sl ->
                let w = Buffer.create 16 in
                Buffer.add_char w c;
                aux(Parse_word(w, sl))
            | Parse_word(w, sl) ->
                Buffer.add_char w c;
                aux(Parse_word(w, sl))
            | Parse_string(_, s, sl) ->
                Buffer.add_char s c;
                aux(Parse_string(false, s, sl))
            end
  in
  aux (Parse_root [])


let string_pop_char str =
  let len = String.length str in
  let i = ref(-1) in
  (function () -> incr i; if !i >= len then None else Some(str.[!i]))


let parse_string str =
  parse (string_pop_char str)


let ic_pop_char ic =
  (function () ->
     try Some(input_char ic)
     with End_of_file -> (None))


let parse_ic ic =
  parse (ic_pop_char ic)


let parse_file filename =
  let ic = open_in filename in
  let res = parse_ic ic in
  close_in ic;
  (res)


let quote s =
  "\"" ^ s ^ "\""

let needs_quote s =
  List.exists (String.contains s) [' '; '\n'; '\r'; '\t'; '('; ')']

let protect s =
  let s = String.escaped s in
  if needs_quote s then quote s else s


let string_of_sexpr s =
  let rec aux acc = function
  | (Atom tag)::tl -> aux ((protect tag)::acc) tl
  | (Expr e)::tl ->
      let s =
        "(" ^
        (String.concat " " (aux [] e))
        ^ ")"
      in
      aux (s::acc) tl
  | [] -> (List.rev acc)
  in
  String.concat " " (aux [] s)


let print_sexpr s =
  print_endline (string_of_sexpr s)


let string_of_sexpr_indent s =
  let rec aux i acc = function
  | (Atom tag)::tl -> aux i ((protect tag)::acc) tl
  | (Expr e)::tl ->
      let s =
        "\n" ^ (String.make i ' ') ^ "(" ^
        (String.concat " " (aux (succ i) [] e))
        ^ ")"
      in
      aux i (s::acc) tl
  | [] -> (List.rev acc)
  in
  String.concat "\n" (aux 0 [] s)


let print_sexpr_indent s =
  print_endline (string_of_sexpr_indent s)

(**********************************************************************************)
(* s-expression parser for TinyPPL *)

exception Parse_error of string

(** parse an s-expression into a pure tinyppl program *)
let rec tinyppl_p_of_sexpr (s:sexpr) : pure_e =
  match s with
  | Atom(s) when s = "true" -> True
  | Atom(s) when s = "false" -> False
  | Atom(s) -> Ident(s)
  | Expr(Atom(s) :: snd :: []) when s = "not" -> Not(tinyppl_p_of_sexpr snd)
  | Expr(Atom(s) :: snd :: thrd :: []) when s = "and" ->
    And(tinyppl_p_of_sexpr snd, tinyppl_p_of_sexpr thrd)
  | Expr(Atom(s) :: snd :: thrd :: []) when s = "or" ->
    Or(tinyppl_p_of_sexpr snd, tinyppl_p_of_sexpr thrd)
  | Expr(Atom(s) :: g :: thn :: els :: []) when s = "if" ->
    Ite(tinyppl_p_of_sexpr g, tinyppl_p_of_sexpr thn, tinyppl_p_of_sexpr els)
  | _ -> raise (Parse_error (string_of_sexpr [s]))

(** parse a string into a pure tinyppl program *)
let tinyppl_p_of_string s : pure_e =
  let sexpr = parse_string s in
  tinyppl_p_of_sexpr (List.hd sexpr)

let rec tinyppl_e_of_sexpr (s:sexpr) : expr =
  match s with
  | Expr(Atom(f) :: Atom(r) :: []) when f = "flip" ->
    Flip(Float.of_string r)
  | Expr(Atom(r) :: e :: []) when r = "return" -> Return(tinyppl_p_of_sexpr e)
  | Expr(Atom(s) :: Atom(x) :: e1 :: e2 :: []) when s = "bind" ->
    Bind(x, tinyppl_e_of_sexpr e1, tinyppl_e_of_sexpr e2)
  | _ -> raise (Parse_error (string_of_sexpr [s]))

(** parse a string into a tinyppl expression *)
let tinyppl_e_of_string s : expr =
  let sexpr = parse_string s in
  tinyppl_e_of_sexpr (List.hd sexpr)

(* some examples *)
let p1 = tinyppl_e_of_string "(bind x (flip 0.5) (return x))"

let p2 = tinyppl_e_of_string "(bind x (flip 0.5)
                              (bind y (flip 0.4)
                              (bind z (flip 0.6)
                              (return (if x y z)))))"

let big_prog = tinyppl_e_of_string
    "(bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (bind x (flip 0.5)
     (return x)))))))))))))))))))))))))))"

let network = tinyppl_e_of_string
  "(bind r2forward (flip 0.5)
   (bind l1fail (flip 0.02)
   (bind l2fail (flip 0.02)
   (bind l3fail (flip 0.02)
   (bind l4fail (flip 0.02)
   (return (if r2forward (and (not l1fail) (not l4fail))
                         (and (not l2fail) (not l3fail)))))))))"

let () =
  assert (within_epsilon (prob p1) 0.5);
  assert (within_epsilon (prob p2) 0.5)