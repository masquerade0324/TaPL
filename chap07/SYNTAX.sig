signature SYNTAX =
sig
  datatype term =
      TmVar of int * int
    | TmAbs of string * term
    | TmApp of term * term
  type binding
  type context

  val shift    : int -> term -> term
  val substTop : term -> term -> term

  val emptyCtx : context
  val addBind  : context -> string -> binding -> context
  val addName  : context -> string -> context
  val idxToName : context -> int -> string
  val nameToIdx : context -> string -> int
  val toString : context -> term -> string
end
