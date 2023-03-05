(* See interface about type ['a t]. *)

type mid = Mid and side = Side and other = Other

type ('a,_) tst =
  Nil : ('a,side) tst
| Def : 'a -> ('a,mid) tst
| Leg : char * ('a,side) tst * ('a,mid) tst * ('a,side) tst -> ('a,'b) tst
| Pre : 'a * ('a,other) tst -> ('a,mid) tst
| Cmp : string * ('a,mid) tst -> ('a,'b) tst

type 'a t = Make : ('a,'b) tst -> 'a t

(* The value of [expand pre dict] is a ternary search tree starting
   with a vertical path of $n$ [Leg] nodes, where $n$ is the length of
   the string [pre], followed by the ternary search tree [dict]. The
   created [Leg] nodes contain the characters of the string [pre]
   ("prefix"). In other words, [expand] allows us to get rid of a
   [Cmp] node by expressing it as [Leg] nodes. *)

let expand (pre: string) (dict: ('a,mid) tst) : ('a,mid) tst =
  let len = String.length pre in
  let rec expand' (i:int) =
    if i < len then Leg(pre.[i],Nil,expand'(i+1),Nil) else dict
in expand' 0

let rec next (c: char) (Make t as dict: 'a t) : 'a t = 
match t with
  Nil             -> dict
| Def _           -> Make Nil
| Cmp(p,t)        -> next c (Make(expand p t))
| Leg(x,t1,t2,t3) -> if c = x then Make t2 
                     else next c (Make(if c < x then t1 else t3))
| Pre(_,t)        -> next c (Make t)

(* See interface about [find] *)

let find (s: string) : 'a t -> 'a option =
  let len = String.length s in
  let rec search (i: int) (Make t as d: 'a t) : 'a option =
    if i = len then None
    else match next s.[i] d with
               Make Nil -> None
         | Make (Def k) -> Some k
         |           d' -> search (i+1) d'
in search 0

(* See interface about [def]. *)

let def : 'a t -> 'a list =
  let rec post : type b.'a list -> ('a,b) tst -> 'a list = 
    fun l -> function
    Nil             -> l
  | Def k           -> k::l
  | Cmp(_,t)        -> post l t
  | Leg(_,t1,t2,t3) -> post (post (post l t3) t2) t1
  | Pre(k,t)        -> k::(post l t)
in function Make t -> post [] t
