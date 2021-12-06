open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) [];;

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  List.fold_left (fun acc (x,y,z) -> if (elem x qs) && (y = s) then (insert z acc) else acc) [] nfa.delta ;;

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list = 
  let rec helper (delta_lst: ('q,'s) transition list) (lists: 'q list) (result: 'q list) = match lists with 
  |[] -> List.sort (fun x y -> if x = y then 0 else if x > y then 1 else -1) result
  |h::t -> let accumulator = (List.fold_left (fun acc (a,b,c) -> if b = None && a = h then 
  (if Sets.elem c result  = false then Sets.insert c acc else acc) else acc) [] delta_lst) in 
  helper (delta_lst) (accumulator @ t) (accumulator @ result) in
  helper (nfa.delta) (qs) (qs);;


(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let  new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = 
(List.fold_left(fun acc x -> (e_closure (nfa) (move (nfa) (qs) (Some x)))::acc) [] nfa.sigma);;

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
(List.fold_left(fun acc x -> ((qs), (Some x), List.sort (fun x y -> if x = y then 0 else if x > y then 1 else -1) (e_closure (nfa) (move (nfa) (qs) (Some x))))::acc) [] nfa.sigma);;

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =  
List.fold_left (fun a x -> if List.mem x qs = true then qs::a else a) [] nfa.fs;;



let rec nfa_to_dfa_helper  (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t) (work: 'q list list) = 
  match work with 
  []-> dfa
  |h::t-> 
  let states_to_add = new_states nfa h in
  let tail_to_return  = Sets.diff (Sets.union states_to_add dfa.qs) (dfa.qs) in
  let dfa_to_return  = {
    sigma = dfa.sigma; 
    qs = Sets.union states_to_add dfa.qs;
    q0 = dfa.q0; 
    fs = Sets.union (new_finals (nfa) (h)) dfa.fs;
    delta = Sets.union (new_trans nfa h) dfa.delta;
    } in
  nfa_to_dfa_helper nfa dfa_to_return (t@tail_to_return);;

  let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let start_state = e_closure (nfa) ([nfa.q0]) in 
  let dfa = {
    sigma = nfa.sigma;
    qs = [start_state];
    q0 = start_state;
    fs = [];
    delta = [];
} in 
nfa_to_dfa_helper (nfa) (dfa) [start_state];;



let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  failwith "unimplemented"

let contain (lst1: 'q list) (lst2: 'q list) : bool = 
List.fold_left (fun acc x -> if Sets.elem (x) (lst1) then  true else acc ) false lst2;;

let rec accept_helper (nfa: ('q,'s) nfa_t) (chars: 's list) (work: 'q list) = 
match chars with 
[] -> work
|h::t -> accept_helper (nfa) (t) (e_closure (nfa) (move(nfa) (work) (Some h)));;

let accept (nfa: ('q,char) nfa_t) (s: string) : bool = 
let curr_state = e_closure (nfa) ([nfa.q0]) in
let char_list  = explode s in
let x = accept_helper nfa char_list curr_state in
if contain nfa.fs x then true else false;;

let dfa_ex = {
    sigma = ['a'; 'b'];
    qs = [1; 2; 3; 4];
    q0 = 1;
    fs = [4];
    delta = [(1, Some 'a', 2); (1, None, 3); (2, Some 'b', 4); (3, Some 'b', 1);
    (3, Some 'b', 4); (4, None, 1)
    ]
};;

let rec accept_helper (nfa: ('q,'s) nfa_t) (chars: 's list) (work: 'q list) = 
match chars with 
[] -> work
|h::t -> accept_helper (nfa) (t) (e_closure (nfa) (move(nfa) (work) (Some h)));;

let accept (nfa: ('q,char) nfa_t) (s: string) : bool = 
let curr_state = e_closure (nfa) ([nfa.q0]) in
let char_list  = explode s in
let x = accept_helper nfa char_list curr_state in
if contain nfa.fs x then true else false;;



let nfa_ex = {
    sigma = ['a'; 'b'];
    qs = [0; 1; 2; 3];
    q0 = 0;
    fs = [3];
    delta = [(0, Some 'a', 1); (0, Some 'a', 3);  (1, Some 'b', 3); (1, Some 'b', 2);  (3, Some 'a', 2); 
     (2, Some 'a', 2)]
}