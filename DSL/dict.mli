(* Type ['a t] denotes a dictionary, that is, a mapping from words of
   type [string] to definitions of type ['a]. It is implemented as a
   \emph{ternary search tree}.

   Perhaps the most straightforward definition is the following type:

   [type 'a t = Nil
              | Leg of char * 'a t * 'a t * 'a t
              | Pre of 'a * 'a t]

   where
   \begin{enumerate}

     \item [Nil] is an empty tree;

     \item [Leg(c,l,m,r)] ("lower, equal, greater") is a tree whose
       root is the character [c], left subtree is [l], middle subtree
       is [m] and right subtree is [r]. The subtree [l] contains words
       starting with a letter lower than [c], in alphabetic order; [m]
       contains words starting with the letter [c] and [r] holds words
       starting with a letter greater than [c].

     \item [Pre(k,s)] ("prefix") denotes the dictionary where the
       empty word $\epsilon$ is mapped to the definition [k] and
       otherwise is the dictionary denoted by the subtree [s].

   \end{enumerate}
   To save some memory, we can add a couple of value constructors:
 
   [type 'a t = Nil
              | Leg of char * 'a t * 'a t * 'a t
              | Pre of 'a * 'a t
              | Def of 'a
              | Cmp of string * 'a t]

   where
   \begin{enumerate}

     \item [Def k] ("definition") is a tree containing only the
       definition [k], that is, it represents the mapping from the
       empty word $\epsilon$ to [k]. In fact, [Def k] is the same as
       [Pre(k,Nil)].

     \item [Cmp(p,s)] ("compressed path") is a tree containing words
       with the prefix [p] and whose suffixes are in the subtree [s],
       that is, it denotes a dictionary whose entries start with [p]
       and whose definitions are in [s]. It can be understood as the
       compression of a linear path in the tree, so [Comp(p,s)] is the
       same as [Leg(p.[0], Nil, Leg(p.[1], Nil, ..., Leg(p.[n], Nil,
       s, Nil), Nil)...), Nil)], where [n = String.length p].

  \end{enumerate}
  The constraints are as follows:
  \begin{itemize}

    \item [Def] and [Pre] can only occur as the second argument of
    [Cmp] or the third of [Leg];

    \item [Nil] can only occur as the second or fourth argument of
    [Leg]; it is also best, although not mandatory, to use [Cmp]
    instead if [Nil] occurs in both positions;

    \item the first argument of [Cmp] should not be [""];

    \item the second argument of [Pre] must either be [Leg] or [Cmp].

  \end{itemize}

  Since, for efficiency reasons, we wish to define a dictionary of all
  ASN.1 keywords by building directly a term of type ['a t], the only
  way to enforce the above mentioned constraints is to define a
  function to check them after the term has been constructed. The
  other option consists in making the previous definition of ['a t]
  private, but constructors cannot be applied directly and the expense
  is a function call for each. A third option strikes a balance
  between static and dynamic checks: we can use a Generalised
  Algebraic Data Type (GADT) in combination with singleton types, so
  we can have the type inference check the mandatory constraints
  listed above while allowing the type to remain concrete. Others can
  be checked with a lightweight function a posteriori.  *)

type mid = Mid and side = Side and other = Other

type ('a,_) tst =
  Nil : ('a,side) tst
| Def : 'a -> ('a,mid) tst
| Leg : char * ('a,side) tst * ('a,mid) tst * ('a,side) tst -> ('a,'b) tst
| Pre : 'a * ('a,other) tst -> ('a,mid) tst
| Cmp : string * ('a,mid) tst -> ('a,'b) tst

type 'a t = Make : ('a,'b) tst -> 'a t

(* Let [c] be a character and [t] a ternary search tree. If [t]
   contains words starting with [c], the value of [next c t] is the
   subtree of [t] containing those words without their first letter;
   otherwise, it is the empty tree. *)

val next : char -> 'a t -> 'a t

(* If the dictionary [dict] contains a definition [k] whose string
   representation is [str], then the value of [find str dict] is [Some 
  k], otherwise [None]. *)

val find : string -> 'a t -> 'a option

(* The value of [def t] is the list of definitions in the ternary
   search tree [t]. In other words, it is the fringe of the tree. *)

val def : 'a t -> 'a list
