structure Syntax : SYNTAX =
struct 
  datatype term =
      TmVar of int * int
    | TmAbs of string * term
    | TmApp of term * term

  datatype binding = NameBind

  type context = (string * binding) list

  fun shift d t =
    let
      fun sft c (TmVar (x, n))   = if x >= c then TmVar (x+d, n+d)
                                   else TmVar (x, n+d)
        | sft c (TmAbs (id, t1)) = TmAbs (id, sft c t1)
        | sft c (TmApp (t1, t2)) = TmApp (sft c t1, sft c t2)
    in
      sft 0 t
    end

  fun subst j s t =
    let
      fun sst c (TmVar (x, n))   = if x = j + c then shift c s
                                   else TmVar (x, n)
        | sst c (TmAbs (id, t1)) = TmAbs (id, sst (c+1) t1)
        | sst c (TmApp (t1, t2)) = TmApp (sst c t1, sst c t2)
    in
      sst 0 t
    end

  fun substTop s t = shift (~1) (subst 0 (shift 1 s) t)

  val emptyCtx = []

  fun addBind ctx x bind = (x, bind)::ctx

  fun addName ctx x = addBind ctx x NameBind

  fun isNameBound ([]: context) _ = false
    | isNameBound ((y, _)::ctx) x = if x = y then true else isNameBound ctx x

  fun pickFreshName ctx x =
      if isNameBound ctx x then pickFreshName ctx (x ^ "'")
      else ((x, NameBind)::ctx, x)

  fun idxToName ctx x =
    let
      val (xn, _) = List.nth (ctx, x)
    in
      xn
    end

  fun nameToIdx ((y, _)::ctx) x = if x = y then 0 else 1 + nameToIdx ctx x

  fun toString ctx t =
    case t of
        TmVar (x, n) =>
          if length ctx = n then idxToName ctx x else "error"
      | TmAbs (id, t1) =>
          let
            val (ctx', id') = pickFreshName ctx id
          in
            "(fn " ^ id' ^ " => " ^ toString ctx' t1 ^ ")"
          end
      | TmApp (t1, t2) => toString ctx t1 ^ " " ^ toString ctx t2
end
