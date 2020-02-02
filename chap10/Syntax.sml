structure Syntax : SYNTAX =
struct
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

  datatype binding =
      NameBind
    | VarBind of ty

  type context = (string * binding) list

  fun shift d t =
    let
      fun sft c (TmVar (x, n))        = if x >= c then TmVar (x+d, n+d)
                                        else TmVar (x, n+d)
        | sft c (TmAbs (id, typ, t1)) = TmAbs (id, typ, sft c t1)
        | sft c (TmApp (t1, t2))      = TmApp (sft c t1, sft c t2)
        | sft c TmTrue                = TmTrue
        | sft _ TmFalse               = TmFalse
        | sft c (TmIf (t1, t2, t3))   = TmIf (sft c t1, sft c t2, sft c t3)
    in
      sft 0 t
    end

  fun subst j s t =
    let
      fun sst c (TmVar (x, n))        = if x = j + c then shift c s
                                        else TmVar (x, n)
        | sst c (TmAbs (id, typ, t1)) = TmAbs (id, typ, sst (c+1) t1)
        | sst c (TmApp (t1, t2))      = TmApp (sst c t1, sst c t2)
        | sst _ TmTrue                = TmTrue
        | sst _ TmFalse               = TmFalse
        | sst c (TmIf (t1, t2, t3))   = TmIf (sst c t1, sst c t2, sst c t3)
    in
      sst 0 t
    end

  fun substTop s t = shift (~1) (subst 0 (shift 1 s) t)

  val emptyCtx = []

  fun addBind ctx x bind = (x, bind)::ctx

  fun addName ctx x = addBind ctx x NameBind

  fun addVar ctx x typ = addBind ctx x (VarBind typ)

  fun isNameBound ([] : context) _ = false
    | isNameBound ((y, _)::ctx) x  = if x = y then true else isNameBound ctx x

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

  fun getBind ctx i = 
    let
      val (_, bind) = List.nth (ctx, i)
    in
      bind
    end

  fun getTypeFromCtx ctx i =
    case getBind ctx i of
        VarBind typ => typ

  fun tyToStr typ =
    case typ of
        TyBool             => "bool"
      | TyFun (typ1, typ2) => "(" ^ tyToStr typ1 ^ " -> " ^ tyToStr typ2 ^ ")"

  fun termToStr ctx t =
    case t of
        TmVar (x, n) =>
          if length ctx = n then idxToName ctx x else "error"
      | TmAbs (id, typ, t1) =>
          let
            val (ctx', id') = pickFreshName ctx id
          in
            "(fn " ^ id' ^ " : " ^ tyToStr typ ^ " => " ^
            termToStr ctx' t1 ^ ")"
          end
      | TmApp (t1, t2) => termToStr ctx t1 ^ " " ^ termToStr ctx t2
      | TmTrue => "true"
      | TmFalse => "false"
      | TmIf (t1, t2, t3) => "if " ^ termToStr ctx t1 ^
                             " then " ^ termToStr ctx t2 ^
                             " else " ^ termToStr ctx t3
end
