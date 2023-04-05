(*

   Quiz 5 - Mutable Data Structures in OCaml
   31 Mar 2023
   Names: Renny Victoria and Nerissa Lundquist
   Pledge: I pledge my honor that I have abided by the Stevens Honor System.
*)

type 'a node = {
  mutable data: 'a;
  mutable left: 'a node option;
  mutable right: 'a node option}


type 'a bt = {
  mutable root: 'a node option;
  mutable size: int}

(* Sample binary tree:

      7
     / \
    3   77
       /
      33
*)

let t1:int bt =
  { root = Some { data=7;
                  left = Some {data=3; left=None; right=None};
                  right = Some {data=77;
                                left=Some {data=33; left=None; right=None};
                                right=None} };
    size = 4}


(** [no_of_nodes t] returns the number of nodes of the binary tree [t] 
    NOTE: you CANNOT return the value of the size field. Youb MUST
    travarse the tree itself.
    Eg. 
    # no_of_nodes t1;;
    - : int = 4
*)
let no_of_nodes : 'a bt -> int =
  fun t ->
  let rec helper no =
    match no with 
    | None -> 0
    | Some n ->
      1+ helper (n.left) + helper (n.right)
    in helper t.root

(** [mirror t] swaps all the left and right children of [t] thus
    producing its mirror image.
    Eg. 
    # mirror t1;;
    - : unit = ()
    # t1;;
    - : int bt =
    {root = Some
             {data = 7;
              left = Some
                      {data = 77; left = None;
                       right = Some {data = 33; left = None; right = None}};
              right = Some {data = 3; left = None; right = None}};
     size = 4}
*)
let mirror : 'a bt -> unit =
  fun t ->
  let rec mirror_tree = function
    | None -> None
    | Some n -> let anode = mirror_tree n.right
                in n.right <- mirror_tree n.left;
                  n.left <- anode;
                  Some n
      in t.root <- mirror_tree t.root



(** [add e t] adds [e] to the binary search tree [t]. 
    Eg. 
    # add 44 t1;;
    - : unit = ()
    # t1;;
    - : int bt =
    {root = Some
             {data = 7; left = Some {data = 3; left = None; right = None};
              right = Some
                      {data = 77;
                       left = Some
                               {data = 33; left = None;
                                right = Some {data = 44; left = None; right = None}};
                       right = None}};
     size = 5}
*)
let add : 'a -> 'a bt ->  unit =
  (*fun e t ->
  | None -> ()
  | Some n -> if e.data > n.right
              then add n.right
              else e <- n.right

              if e.data < n.left
              then 
  |*)
  failwith "implement me"

