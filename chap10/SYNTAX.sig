signature SYNTAX =
sig
  datatype ty =
      TyBool
    | TyFun of ty * ty
  datatype term =
      TmVar of int * int
    | TmAbs of string * ty * term
    | TmApp of term * term
    | TmTrue
    | TmFalse
    | TmIf of term * term * term
  type binding
  type context

  val shift    : int -> term -> term
  val substTop : term -> term -> term

  val emptyCtx  : context
  val addBind   : context -> string -> binding -> context
  val addName   : context -> string -> context
  val addVar    : context -> string -> ty -> context
  val idxToName : context -> int -> string
  val nameToIdx : context -> string -> int
  val getBind   : context -> int -> binding
  val getTypeFromCtx : context -> int -> ty
  val tyToStr   : ty -> string
  val termToStr : context -> term -> string
end
